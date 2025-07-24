;;; claude-code-mcp.el --- Claude MCP over websockets implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; Integrate claude-code.el with claude processes using websockets + MCP.


;;; Code:
;;; Require dependencies
(require 'cl-lib)
(require 'diff) ; For diff-no-select
(require 'json)
(require 'project) ; For project-current and project-root
(require 'subr-x) ; For when-let*
(require 'websocket)

;; Declare external functions from flymake
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))

;; Declare external functions from flycheck
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(declare-function flycheck-error-checker "flycheck" (err))

;;; Session structure
(cl-defstruct claude-code-mcp--session
  "MCP session for a Claude instance."
  key
  server
  client
  port
  initialized      ; Whether handshake is complete
  auth-token       ; UUID auth token for validation
  opened-diffs     ; Hash table of active diff sessions keyed by tab-name - [TODO] I don't think we're using anymore, remove it and related code
  deferred-responses) ; Hash table of deferred responses keyed by unique-key

;;; Constants
(defconst claude-code-mcp--port-min 10000 "Minimum port number for WebSocket server.")
(defconst claude-code-mcp--port-max 65535 "Maximum port number for WebSocket server.")
(defconst claude-code-mcp--protocol-version "2024-11-05" "MCP protocol version supported.")
(defconst claude-code-mcp--selection-delay 0.05 "Delay in seconds before sending selection update.")
(defconst claude-code-mcp--initial-notification-delay 0.01 "Delay before sending initial notifications after connection.")

;;; Customization variables
(defcustom claude-code-mcp-enable-logging nil
  "Enable logging of MCP websocket messages."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-mcp-log-buffer-name "*claude-code mcp log*"
  "Name of the buffer for MCP logging."
  :type 'string
  :group 'claude-code)

;;; Logging functions
(defun claude-code-mcp--get-log-buffer ()
  "Get or create the MCP log buffer."
  (get-buffer-create claude-code-mcp-log-buffer-name))

(defun claude-code-mcp--log (direction type data &optional json-string)
  "Log MCP message to buffer.

DIRECTION is either `in' or `out'.
TYPE is a descriptive string like `request' or `response'.
DATA is the elisp data structure.
JSON-STRING is the actual JSON string if available."
  (when claude-code-mcp-enable-logging
    (with-current-buffer (claude-code-mcp--get-log-buffer)
      (goto-char (point-max))
      (insert (format "\n[%s] %s %s:\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S.%3N")
                      direction
                      type))
      (insert "Elisp: ")
      (pp data (current-buffer))
      (when json-string
        (insert "JSON: ")
        (insert json-string)
        (insert "\n"))
      (insert "\n" (make-string 60 ?-) "\n"))))

(defun claude-code-mcp--validate-json (json-string context)
  "Validate that JSON-STRING is well-formed JSON.
CONTEXT is a string describing where this JSON is being sent from.
If logging is enabled and JSON is malformed, logs error and signals.
Returns t if valid, signals error if invalid."
  (condition-case err
      (progn
        (json-read-from-string json-string)
        t)
    (json-error
     (when claude-code-mcp-enable-logging
       (claude-code-mcp--log 'out 'json-validation-error
                             `((context . ,context)
                               (error . ,(error-message-string err))
                               (json-length . ,(length json-string))
                               (json-preview . ,(substring json-string 0 (min 200 (length json-string)))))
                             json-string))
     (error "Malformed JSON in %s: %s" context (error-message-string err)))))

;;; Internal state variables
(defvar claude-code-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping claude code buffer names to ide websocket sessions.

Values are instances of the `claude-code-mcp--session' structure.

Keys can actually be any unique string, not just buffer names. Using a
key that is not a buffer name is useful for creating a websocket server
in Emacs to connect to an claude process running outside Emacs." )

(defvar claude-code-mcp--selection-timer nil
  "Timer for debouncing selection updates.")

;;; Util functions
(defun claude-code-mcp--generate-uuid ()
  "Generate a UUID v4 string."
  (let ((uuid (make-string 36 ?-)))
    ;; Generate random hex characters
    (dotimes (i 36)
      (unless (memq i '(8 13 18 23)) ; Skip dash positions
        (let ((char (if (= i 14)
                        4 ; Version 4 UUID
                      (if (= i 19)
                          ;; Variant bits: 10xx
                          (let ((val (random 16)))
                            (if (< val 4) (+ val 8)
                              (if (< val 8) (+ val 8)
                                (if (< val 12) val
                                  (- val 4)))))
                        (random 16)))))
          (aset uuid i (aref "0123456789abcdef" char)))))
    uuid))

(defun claude-code-mcp--shuffle-list (l)
  "Randomly shuffle list L."
  (let* ((v (apply #'vector l))
         (n (length v)))
    (dotimes (i (1- n))
      (cl-rotatef (aref v i) (aref v (+ i (random (- n i))))))
    (append v nil)))

(defun claude-code-mcp--port-available-p (port)
  "Check if PORT is available for binding."
  (condition-case nil
      (let ((test-process
             (make-network-process
              :name "test"
              :server t
              :host 'local
              :service port
              :family 'ipv4)))
        (delete-process test-process)
        t)
    (error nil)))

(defun claude-code-mcp--find-session-by-client (client)
  "Find the session that owns the CLIENT websocket."
  (seq-find (lambda (session)
              (eq (claude-code-mcp--session-client session) client))
            (hash-table-values claude-code-mcp--sessions)))

(defun claude-code-mcp--find-free-port ()
  "Find a free port in the allowed range."
  (let* ((ports (number-sequence claude-code-mcp--port-min claude-code-mcp--port-max))
         ;; Shuffle ports for randomness
         (shuffled-ports (claude-code-mcp--shuffle-list ports)))
    ;; Try ports until we find a free one
    (cl-loop for port in shuffled-ports
             when (claude-code-mcp--port-available-p port)
             return port
             finally (error "No free ports available"))))

(defun claude-code-mcp--create-lockfile (folder port auth-token)
  "Create lock file for claude running in FOLDER for PORT with AUTH-TOKEN."
  (condition-case err
      (let* ((dir (expand-file-name "~/.claude/ide/"))
             (file (expand-file-name (format "%d.lock" port) dir))
             (content (json-encode
                       `((pid . ,(emacs-pid))
                         (workspaceFolders . ,(vector folder))
                         (ideName . "Emacs") ;; [TODO] include directory or project in ide name
                         (transport . "ws")
                         (authToken . ,auth-token)))))
        ;; Ensure directory exists
        (make-directory dir t)
        ;; Write lock file
        (with-temp-file file
          (insert content))
        ;; Make it readable
        (set-file-modes file #o644)
        (claude-code-mcp--log 'out 'lockfile-created
                              `((file . ,file)
                                (port . ,port)
                                (authToken . ,auth-token))
                              content))
    (error
     (claude-code-mcp--log 'out 'lockfile-error
                           `((error . ,(error-message-string err))
                             (port . ,port))
                           nil)
     (signal (car err) (cdr err)))))

(defun claude-code-mcp--remove-lockfile (port)
  "Remove lock file for PORT."
  (when port
    (let* ((dir (expand-file-name "~/.claude/ide/"))
           (file (expand-file-name (format "%d.lock" port) dir)))
      (when (file-exists-p file)
        (condition-case err
            (progn
              (delete-file file)
              (claude-code-mcp--log 'out 'lockfile-removed
                                    `((file . ,file)
                                      (port . ,port))
                                    nil))
          (error
           (claude-code-mcp--log 'out 'lockfile-remove-error
                                 `((error . ,(error-message-string err))
                                   (file . ,file)
                                   (port . ,port))
                                 nil)))))))

(defun claude-code-mcp--send-response (ws id result)
  "Send successful response with ID and RESULT to WS."
  (let* ((data `((jsonrpc . "2.0")
                 (id . ,id)
                 (result . ,result)))
         (response (json-encode data)))
    (when claude-code-mcp-enable-logging
      (claude-code-mcp--validate-json response (format "send-response (id: %s)" id)))
    (claude-code-mcp--log 'out 'response data response)
    (websocket-send-text ws response)))

(defun claude-code-mcp--send-error (ws id code message &optional data)
  "Send error response with ID, CODE, MESSAGE and optional DATA to WS."
  (let* ((error-data `((jsonrpc . "2.0")
                       (id . ,id)
                       (error . ((code . ,code)
                                 (message . ,message)
                                 ,@(when data `((data . ,data)))))))
         (response (json-encode error-data)))
    (when claude-code-mcp-enable-logging
      (claude-code-mcp--validate-json response (format "send-error (id: %s, code: %s)" id code)))
    (claude-code-mcp--log 'out 'error error-data response)
    (websocket-send-text ws response)))

(defun claude-code-mcp--send-notification (client method &optional params)
  "Send notification with METHOD and PARAMS to CLIENT.
If PARAMS is not provided, uses an empty hash table."
  (when client
    (let* ((session (claude-code-mcp--find-session-by-client client)))
      ;; Only send if session is initialized (unless it's the initialization-related notification)
      (when (or (and session (claude-code-mcp--session-initialized session))
                (string-prefix-p "notifications/" method))
        (let* ((params (or params (make-hash-table :test 'equal)))
               (data `((jsonrpc . "2.0")
                       (method . ,method)
                       (params . ,params)))
               (notification (json-encode data)))
          (when claude-code-mcp-enable-logging
            (claude-code-mcp--validate-json notification (format "send-notification (method: %s)" method)))
          (claude-code-mcp--log 'out 'notification data notification)
          (condition-case err
              (websocket-send-text client notification)
            (error
             (claude-code-mcp--log 'out 'notification-error
                                   `((error . ,(error-message-string err))
                                     (method . ,method))
                                   nil))))))))

;;; Deferred Response Support
(defun claude-code-mcp--store-deferred-response (session unique-key id)
  "Store a deferred response for UNIQUE-KEY with request ID in SESSION."
  (when (and session unique-key id)
    (let ((deferred-responses (claude-code-mcp--session-deferred-responses session)))
      (puthash unique-key id deferred-responses))))

(defun claude-code-mcp--complete-deferred-response (unique-key result)
  "Complete a deferred response for UNIQUE-KEY with RESULT.
Searches all sessions for the deferred response."
  (let ((completed nil))
    (maphash (lambda (_key session)
               (unless completed
                 (let* ((deferred-responses (claude-code-mcp--session-deferred-responses session))
                        (request-id (gethash unique-key deferred-responses)))
                   (when request-id
                     ;; Found the deferred response
                     (let ((client (claude-code-mcp--session-client session)))
                       (when client
                         ;; Send the response
                         (claude-code-mcp--send-response client request-id result)
                         ;; Remove from deferred responses
                         (remhash unique-key deferred-responses)
                         (setq completed t)))))))
             claude-code-mcp--sessions)))

;;; Handlers
(defun claude-code-mcp--handle-initialize (session ws id _params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (when-let ((client (claude-code-mcp--session-client session)))
    ;; Send initialize response
    (claude-code-mcp--send-response
     ws id
     `((protocolVersion . ,claude-code-mcp--protocol-version)
       (capabilities . ((tools . ((listChanged . t)))
                        (prompts . ((listChanged . t)))
                        (resources . ((subscribe . :json-false)
                                      (listChanged . :json-false)))))
       (serverInfo . ((name . "claude-code.el")
                      (version . "0.1.0")))))

    ;; Mark session as initialized
    (setf (claude-code-mcp--session-initialized session) t)
    (claude-code-mcp--log 'out 'session-initialized
                          `((key . ,(claude-code-mcp--session-key session))
                            (port . ,(claude-code-mcp--session-port session)))
                          nil)

    ;; Send tools/list_changed notification immediately
    (claude-code-mcp--send-notification
     client
     "notifications/tools/list_changed")))

(defun claude-code-mcp--handle-tools-list (_session ws id _params)
  "Handle tools/list request with ID and PARAMS from WS for SESSION."
  (claude-code-mcp--send-response
   ws id
   `((tools . ,(claude-code-mcp--get-tools-list)))))

(defun claude-code-mcp--handle-tools-call (session ws id params)
  "Handle tools/call request with ID and PARAMS from WS for SESSION."
  (let* ((tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params))
         (handler (claude-code-mcp--get-tool-handler tool-name)))
    (if handler
        (condition-case err
            (let ((result (funcall handler arguments session)))
              ;; Check if this is a deferred response
              (if (and (listp result) (alist-get 'deferred result))
                  (let ((unique-key (alist-get 'unique-key result)))
                    ;; Store the deferred response
                    (claude-code-mcp--store-deferred-response session unique-key id)
                    ;; Don't send response yet - it will be sent later
                    nil)
                ;; Normal response - send immediately
                (claude-code-mcp--send-response ws id result)))
          (error
           (claude-code-mcp--send-error
            ws id -32603
            (format "Error in tool %s: %s" tool-name (error-message-string err)))))
      (claude-code-mcp--send-error
       ws id -32601
       (format "Tool not found: %s" tool-name)))))

(defun claude-code-mcp--handle-resources-list (_session ws id params)
  "Handle resources/list request with ID and PARAMS from WS for SESSION."
  (let* ((cursor (alist-get 'cursor params))
         (resources (claude-code-mcp--get-file-resources cursor)))
    (claude-code-mcp--send-response
     ws id
     `((resources . ,(vconcat resources))
       ,@(when cursor `((nextCursor . ,cursor)))))))

(defun claude-code-mcp--handle-resources-read (_session ws id params)
  "Handle resources/read request with ID and PARAMS from WS for SESSION."
  (let* ((uri (alist-get 'uri params))
         (file-path (when (string-prefix-p "file://" uri)
                      (substring uri 7))))
    (if (and file-path (file-exists-p file-path))
        (let ((content (with-temp-buffer
                         (insert-file-contents file-path)
                         (buffer-string))))
          (claude-code-mcp--send-response
           ws id
           `((contents . ,(vector `((uri . ,uri)
                                    (text . ,content)
                                    (mimeType . ,(claude-code-mcp--get-mime-type file-path))))))))
      (claude-code-mcp--send-error
       ws id -32602
       (format "Resource not found: %s" uri)))))

(defun claude-code-mcp--handle-ide-connected (session ws id _params)
  "Handle ide_connected notification from Claude Code.
This is sent when Claude Code has successfully connected to the IDE.
We use this opportunity to send the initial selection state."
  ;; Send acknowledgment response
  (claude-code-mcp--send-response ws id '())
  
  ;; Send initial selection state after a short delay
  (when-let ((client (claude-code-mcp--session-client session)))
    (run-with-timer claude-code-mcp--initial-notification-delay nil
                    (lambda ()
                      ;; Send current selection if we have a file buffer
                      (when (and buffer-file-name
                                 (buffer-live-p (current-buffer)))
                        (let ((selection (claude-code-mcp--get-selection)))
                          (when selection
                            (claude-code-mcp--send-notification
                             client
                             "selection_changed"
                             selection))))))))

;;; MCP Tools
(defun claude-code-mcp--get-tools-list ()
  "Return the list of available MCP tools."
  (vector
   `((name . "getCurrentSelection")
     (description . "Get the current text selection or cursor position in the active editor")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "openFile")
     (description . "Open a file in the editor")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string")
                                            (description . "The file URI or path to open")))))
                     (required . ["uri"]))))
   `((name . "openDiff")
     (description . "Open a diff view")
     (inputSchema . ((type . "object")
                     (properties . ((old_file_path . ((type . "string")))
                                    (new_file_path . ((type . "string")))
                                    (new_file_contents . ((type . "string")))
                                    (tab_name . ((type . "string")))))
                     (required . ["old_file_path" "new_file_path" "new_file_contents"]))))
   `((name . "closeAllDiffTabs")
     (description . "Close all diff tabs")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "close_tab")
     (description . "Close a tab")
     (inputSchema . ((type . "object")
                     (properties . ((tab_name . ((type . "string")))))
                     (required . ["tab_name"]))))
   `((name . "getDiagnostics")
     (description . "Get diagnostics for a file")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string"))))))))
   `((name . "getOpenEditors")
     (description . "Get the list of currently open files in the editor")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "getWorkspaceFolders")
     (description . "Get the list of workspace folders (project directories)")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "checkDocumentDirty")
     (description . "Check if a document has unsaved changes")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string")
                                            (description . "The file URI or path to check")))))
                     (required . ["uri"]))))
   `((name . "saveDocument")
     (description . "Save a document to disk")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string")
                                            (description . "The file URI or path to save")))))
                     (required . ["uri"]))))
   `((name . "getLatestSelection")
     (description . "Get the latest text selection from any file")
     (inputSchema . ((type . "object")
                     (properties . ()))))))

(defun claude-code-mcp--get-tool-handler (name)
  "Return the handler function for tool NAME."
  (pcase name
    ("getCurrentSelection" #'claude-code-mcp--tool-get-current-selection)
    ("openFile" #'claude-code-mcp--tool-open-file)
    ("openDiff" #'claude-code-mcp--tool-open-diff)
    ("closeAllDiffTabs" #'claude-code-mcp--tool-close-all-diff-tabs)
    ("close_tab" #'claude-code-mcp--tool-close-tab)
    ("getDiagnostics" #'claude-code-mcp--tool-get-diagnostics)
    ("getOpenEditors" #'claude-code-mcp--tool-get-open-editors)
    ("getWorkspaceFolders" #'claude-code-mcp--tool-get-workspace-folders)
    ("checkDocumentDirty" #'claude-code-mcp--tool-check-document-dirty)
    ("saveDocument" #'claude-code-mcp--tool-save-document)
    ("getLatestSelection" #'claude-code-mcp--tool-get-latest-selection)
    (_ nil)))

(defun claude-code-mcp--tool-get-current-selection (_params _session)
  "Implementation of getCurrentSelection tool.
_PARAMS is unused for this tool.
_SESSION is the MCP session (unused for this tool)."
  (let ((selection-data (claude-code-mcp--get-selection)))
    (if selection-data
        `((content . ,(vector (list (cons 'type "text")
                                      (cons 'text (json-encode selection-data))))))
      `((content . ,(vector (list (cons 'type "text")
                                    (cons 'text (json-encode '((text . "")
                                               (filePath . "")
                                               (selection . ((start . ((line . 0) (character . 0)))
                                                             (end . ((line . 0) (character . 0)))
                                                             (isEmpty . t)))))))))))))

(defun claude-code-mcp--tool-open-file (params _session)
  "Implementation of openFile tool.
PARAMS contains uri.
_SESSION is the MCP session (unused for this tool)."
  (condition-case err
      (let* ((uri (alist-get 'uri params))
             ;; Handle file:// URIs
             (file-path (if (string-prefix-p "file://" uri)
                            (substring uri 7)
                          uri)))
        ;; Expand and normalize the file path
        (setq file-path (expand-file-name file-path))

        ;; Check if file exists
        (unless (file-exists-p file-path)
          (error "File not found: %s" file-path))

        ;; Open the file and switch to its buffer
        (find-file file-path)

        ;; Make sure the buffer is displayed in a window
        (switch-to-buffer (current-buffer))

        ;; Return success with file information
        `((content . ,(vector (list (cons 'type "text")
                                    (cons 'text (format "Opened file: %s" file-path)))))))
    (error
     `((content . ,(vector (list (cons 'type "text")
                                 (cons 'text (format "Error opening file: %s" (error-message-string err))))))))))

(defun claude-code-mcp--tool-open-diff (params session)
  "Simplified diff tool that prompts user to accept/reject changes.
  PARAMS contains old_file_path, new_file_path, new_file_contents, tab_name.
  SESSION is the MCP session for this request."
  (let ((old-path (alist-get 'old_file_path params))
        (new-path (alist-get 'new_file_path params))
        (new-contents (alist-get 'new_file_contents params))
        (tab-name (alist-get 'tab_name params)))

    (unless (and old-path new-contents tab-name session)
      (error "Missing required parameters"))

    (let ((file-exists (file-exists-p old-path))
          (old-temp-buffer (generate-new-buffer
                            (format "*%s-old*" (file-name-nondirectory old-path))))
          (new-temp-buffer (generate-new-buffer
                            (format "*%s-new*" (file-name-nondirectory old-path)))))

      ;; Fill the old temp buffer
      (with-current-buffer old-temp-buffer
        (if file-exists
            (insert-file-contents old-path)
          ;; New file - leave empty
          (insert ""))
        ;; Set buffer-file-name to help with mode detection
        (setq-local buffer-file-name old-path)
        ;; Mark as not modified since this is a temporary buffer
        (set-buffer-modified-p nil))

      ;; Fill the new temp buffer
      (with-current-buffer new-temp-buffer
        (when new-contents
          (insert new-contents))
        ;; Set buffer-file-name to help with mode detection
        (setq-local buffer-file-name (or new-path old-path))
        ;; Mark as not modified since this is a temporary buffer
        (set-buffer-modified-p nil))

      ;; Create the diff
      ;; [TODO] make buffer name include with claude
      (let* ((diff-buffer-name (format "*Diff: %s*" tab-name))
             (diff-buffer (get-buffer-create diff-buffer-name t))
             (switches `("-u" "--label" ,old-path "--label" ,(or new-path old-path)))
             ;; syntax highlighting
             (diff-font-lock-syntax 'hunk-also))
        ;; Create diff via diff-no-select
        (diff-no-select old-temp-buffer new-temp-buffer switches t diff-buffer)
        ;; Configure the diff buffer
        (with-current-buffer diff-buffer
          ;; Set the default directory to help with file resolution
          (setq default-directory (file-name-directory old-path))
          ;; Store file paths for diff-mode to use
          (setq-local diff-vc-backend nil) ; Not using VC
          (setq-local diff-default-directory default-directory)
          ;; Set diff-font-lock-syntax to 'hunk-also BEFORE calling diff-mode
          (setq-local diff-font-lock-syntax 'hunk-also)
          ;; Re-initialize diff-mode with our settings
          (diff-mode))

        ;; Display the diff buffer in a pop up window without selecting it
        (display-buffer diff-buffer
                        '((display-buffer-pop-up-window)
                          ;; [TODO] do we need inhibit-switch-frame
                          (inhibit-switch-frame . t)))

        ;; Store diff info for cleanup later
        (let ((opened-diffs (claude-code-mcp--session-opened-diffs session)))
          (puthash tab-name
                   `((old-temp-buffer . ,old-temp-buffer)
                     (new-temp-buffer . ,new-temp-buffer)
                     (diff-buffer . ,diff-buffer)
                     (old-file-path . ,old-path)
                     (new-file-path . ,new-path)
                     (new-contents . ,new-contents)
                     (file-exists . ,file-exists)
                     (session . ,session)
                     (created-at . ,(current-time)))
                   opened-diffs))))

    ;; Return deferred response indicator
    `((deferred . t)
      (unique-key . ,tab-name))))

(defun claude-code-mcp--tool-close-tab (params session)
  "Close a tab/buffer.
PARAMS should contain `path' or `tab_name' of the file to close.
SESSION is the MCP session for tracking opened diffs."
  (let ((path (alist-get 'path params))
        (tab-name (alist-get 'tab_name params)))
    (cond
     ;; Handle closing by file path
     (path
      (let ((buffer (find-buffer-visiting path)))
        (if buffer
            (progn
              (kill-buffer buffer)
              (claude-code-mcp--log 'out 'close-tab-success
                                    `((path . ,path)
                                      (result . "TAB_CLOSED"))
                                    nil)
              ;; Return success response
              `((content . ,(vector (list (cons 'type "text")
                                           (cons 'text "TAB_CLOSED")))))
              ;; Buffer not found - log and return success anyway
              (progn
                (message "Claude Code: No buffer visiting %s, ignoring close request" path)
                (claude-code-mcp--log 'out 'close-tab-no-buffer
                                      `((path . ,path)
                                        (result . "Buffer not found, returning success"))
                                      nil)
                `((content . ,(vector (list (cons 'type "text")
                                             (cons 'text "TAB_CLOSED"))))))))))
     ;; Handle closing by tab name (buffer name or diff tab)
     (tab-name
      ;; First check if this is a diff tab
      (let* ((opened-diffs (when session
                             (claude-code-mcp--session-opened-diffs session)))
             (diff-info (when opened-diffs
                          (gethash tab-name opened-diffs))))
        (when diff-info
          (let ((new-contents (gethash 'new-contents)))
            ;; complete the deferred response to save the diff
            (message "xxx saving diff")
            (claude-code-mcp--complete-deferred-response
             tab-name
             `((content . ,(vector
                            (list (cons 'type "text")
                                  (cons 'text "FILE_SAVED"))
                            (list (cons 'type "text")
                                  (cons 'text new-contents)))))))

          ;; Clean up the diff
          (claude-code-mcp--cleanup-diff tab-name session)

          ;; Log and return success response
          (claude-code-mcp--log 'out 'close-diff-tab-success
                                `((tab-name . ,tab-name)
                                  (result . "TAB_CLOSED"))
                                nil)

          ;; Log and return response
          (claude-code-mcp--log 'out 'close-tab-no-buffer
                                `((tab-name . ,tab-name)
                                  (result . "TAB_CLOSED"))
                                nil)
          `((content . ,(vector (list (cons 'type "text")
                                      (cons 'text "TAB_CLOSED"))))))
        ;; Not a diff - treat tab_name as regular buffer name
        (let ((buffer (get-buffer tab-name)))
          (if buffer
              (progn
                (kill-buffer buffer)
                (claude-code-mcp--log 'out 'close-tab-success
                                      `((tab-name . ,tab-name)
                                        (result . "TAB_CLOSED"))
                                      nil)
                ;; Return success response
                `((content . ,(vector (list (cons 'type "text")
                                             (cons 'text "TAB_CLOSED")))))
                ;; Buffer not found - log and return success anyway
                (progn
                  (message "Claude Code: No buffer named %s, ignoring close request" tab-name)
                  (claude-code-mcp--log 'out 'close-tab-no-buffer
                                        `((tab-name . ,tab-name)
                                          (result . "Buffer not found, returning success"))
                                        nil)
                  `((content . ,(vector (list (cons 'type "text")
                                              (cons 'text "TAB_CLOSED")))))))))))
     ;; Neither path nor tab_name provided - this is still an error
     (t
      (error "Either 'path' or 'tab_name' must be provided")))))

(defun claude-code-mcp--tool-get-diagnostics (params _session)
  "Implementation of getDiagnostics tool.
PARAMS contains optional uri.
_SESSION is the MCP session (unused for this tool)."
  (let* ((uri (alist-get 'uri params))
         (file-path (when uri
                      (if (string-prefix-p "file://" uri)
                          (substring uri 7)
                        uri)))
         (diagnostics '()))
    ;; Get diagnostics for the specified file or all open files
    (dolist (buffer (buffer-list))
      (when (and (buffer-file-name buffer)
                 (or (null file-path)
                     (string= file-path (buffer-file-name buffer))))
        (with-current-buffer buffer
          ;; Check flymake if available
          (when (bound-and-true-p flymake-mode)
            (dolist (diag (flymake-diagnostics))
              (let* ((beg (flymake-diagnostic-beg diag))
                     (end (flymake-diagnostic-end diag))
                     (type (flymake-diagnostic-type diag))
                     (text (flymake-diagnostic-text diag))
                     (severity (cond
                                ((eq type :error) 1) ; Error
                                ((eq type :warning) 2) ; Warning
                                ((eq type :note) 3)    ; Information
                                (t 4)))                ; Hint
                     (start-line (1- (line-number-at-pos beg)))
                     (start-char (save-excursion
                                   (goto-char beg)
                                   (current-column)))
                     (end-line (1- (line-number-at-pos end)))
                     (end-char (save-excursion
                                 (goto-char end)
                                 (current-column))))
                (push `((uri . ,(concat "file://" (buffer-file-name)))
                        (range . ((start . ((line . ,start-line)
                                            (character . ,start-char)))
                                  (end . ((line . ,end-line)
                                          (character . ,end-char)))))
                        (severity . ,severity)
                        (message . ,text)
                        (source . "flymake"))
                      diagnostics))))
          ;; Check flycheck if available
          (when (and (bound-and-true-p flycheck-mode)
                     (boundp 'flycheck-current-errors)
                     flycheck-current-errors)
            (dolist (err flycheck-current-errors)
              (let* ((line (1- (or (flycheck-error-line err) 1)))
                     (col (or (flycheck-error-column err) 0))
                     (level (flycheck-error-level err))
                     (msg (flycheck-error-message err))
                     (checker (symbol-name (flycheck-error-checker err)))
                     (severity (cond
                                ((eq level 'error) 1)
                                ((eq level 'warning) 2)
                                ((eq level 'info) 3)
                                (t 4))))
                (push `((uri . ,(concat "file://" (buffer-file-name)))
                        (range . ((start . ((line . ,line)
                                            (character . ,col)))
                                  (end . ((line . ,line)
                                          (character . ,col)))))
                        (severity . ,severity)
                        (message . ,msg)
                        (source . ,checker))
                      diagnostics)))))))
    ;; Return the diagnostics
    `((content . ,(vector (list (cons 'type "text")
                                    (cons 'text (json-encode `((diagnostics . ,(vconcat (nreverse diagnostics))))))))))))

(defun claude-code-mcp--cleanup-diff (tab-name session)
  "Clean up diff session for TAB-NAME in SESSION."
  (let ((opened-diffs (claude-code-mcp--session-opened-diffs session)))
    (when-let ((diff-info (gethash tab-name opened-diffs)))
      (let ((old-temp-buffer (alist-get 'old-temp-buffer diff-info))
            (new-temp-buffer (alist-get 'new-temp-buffer diff-info))
            (diff-buffer (alist-get 'diff-buffer diff-info))
            (file-exists (alist-get 'file-exists diff-info))
            (kill-buffer-query-functions nil))
        ;; Kill the diff buffer and close its window
        (when (and diff-buffer (buffer-live-p diff-buffer))
          (let ((diff-window (get-buffer-window diff-buffer)))
            (with-current-buffer diff-buffer
              (set-buffer-modified-p nil))
            ;; [TODO] This window deletion might be problematic if the user configures
            ;; the diff window to appear in the same slot as the claude buffer.
            ;; We might need a special case or user option to handle that scenario.
            (when diff-window
              (delete-window diff-window)))
          (kill-buffer diff-buffer))
        ;; Kill the new temporary buffer
        (when (and new-temp-buffer (buffer-live-p new-temp-buffer))
          (with-current-buffer new-temp-buffer
            (set-buffer-modified-p nil))
          (kill-buffer new-temp-buffer))
        ;; Kill the old temporary buffer
        (when (and old-temp-buffer (buffer-live-p old-temp-buffer))
          (with-current-buffer old-temp-buffer
            (set-buffer-modified-p nil))
          (kill-buffer old-temp-buffer))
        ;; Remove from opened diffs
        (remhash tab-name opened-diffs)))))

(defun claude-code-mcp--tool-close-all-diff-tabs (_params session)
  "Close all diff tabs created by Claude.
_PARAMS is empty.
SESSION is the MCP session containing opened diffs."
  (let ((closed-count 0))
    (when session
      ;; Close all diffs for this session
      (let ((opened-diffs (claude-code-mcp--session-opened-diffs session)))
        (maphash (lambda (tab-name _diff-info)
                   (claude-code-mcp--cleanup-diff tab-name session)
                   (setq closed-count (1+ closed-count)))
                 opened-diffs)))
    ;; Return success with actual count
    `((content . ,(vector (list (cons 'type "text")
                                (cons 'text (format "CLOSED_%d_DIFF_TABS" closed-count))))))))

(defun claude-code-mcp--tool-get-open-editors (_params _session)
  "Implementation of getOpenEditors tool.
_PARAMS is unused for this tool.
_SESSION is the MCP session (unused for this tool)."
  (let ((editors '()))
    ;; Collect all file-visiting buffers
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (push `((uri . ,(concat "file://" file))
                (name . ,(file-name-nondirectory file))
                (path . ,file))
              editors)))
    ;; Return the list of open editors
    `((content . ,(vector (list (cons 'type "text")
                                    (cons 'text (json-encode `((editors . ,(vconcat (nreverse editors))))))))))))

(defun claude-code-mcp--tool-get-workspace-folders (_params _session)
  "Implementation of getWorkspaceFolders tool.
_PARAMS is unused for this tool.
_SESSION is the MCP session (unused for this tool)."
  (let ((folders '())
        (seen-dirs (make-hash-table :test 'equal)))
    ;; First, add current project if available
    (when-let* ((project (project-current)))
      (let ((root (project-root project)))
        (unless (gethash root seen-dirs)
          (puthash root t seen-dirs)
          (push `((uri . ,(concat "file://" root))
                  (name . ,(file-name-nondirectory (directory-file-name root))))
                folders))))
    ;; Then add unique directories from all file-visiting buffers
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer))
                  (dir (file-name-directory file)))
        ;; Find the project root for this file
        (let ((project-root (or (when-let ((proj (project-current nil dir)))
                                  (project-root proj))
                                dir)))
          (unless (gethash project-root seen-dirs)
            (puthash project-root t seen-dirs)
            (push `((uri . ,(concat "file://" project-root))
                    (name . ,(file-name-nondirectory (directory-file-name project-root))))
                  folders)))))
    ;; Return the list of workspace folders
    `((content . ,(vector (list (cons 'type "text")
                                    (cons 'text (json-encode `((folders . ,(vconcat (nreverse folders))))))))))))

(defun claude-code-mcp--tool-check-document-dirty (params _session)
  "Check if a document has unsaved change.
PARAMS contains uri.
_SESSION is the MCP session (unused for this tool)."
  (let* ((uri (alist-get 'uri params))
         (file-path (if (string-prefix-p "file://" uri)
                        (substring uri 7)
                      uri))
         (buffer (find-buffer-visiting file-path))
         (is-dirty (if buffer
                       (buffer-modified-p buffer)
                     nil)))
    `((content . ,(vector (list (cons 'type "text")
                                (cons 'text (json-encode `((isDirty . ,is-dirty))))))))))

(defun claude-code-mcp--tool-save-document (params _session)
  "Save a document to disk.
PARAMS contains uri.
_SESSION is the MCP session (unused for this tool)."
  (let* ((uri (alist-get 'uri params))
         (file-path (if (string-prefix-p "file://" uri)
                        (substring uri 7)
                      uri))
         (buffer (find-buffer-visiting file-path)))
    (if buffer
        (with-current-buffer buffer
          (save-buffer)
          `((content . ,(vector (list (cons 'type "text")
                                      (cons 'text (json-encode `((saved . t))))))))
      `((content . ,(vector (list (cons 'type "text")
                                  (cons 'text (json-encode `((saved . :json-false)
                                                             (error . "File not open")))))))))))

(defun claude-code-mcp--tool-get-latest-selection (_params _session)
  "Get the latest text selection from any file.
_PARAMS is unused for this tool.
_SESSION is the MCP session (unused for this tool)."
  ;; Find the most recently selected buffer with a selection
  (let ((selection-data nil)
        (latest-time 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (use-region-p)
                   (> (buffer-modified-tick) latest-time))
          (setq latest-time (buffer-modified-tick))
          (setq selection-data (claude-code-mcp--get-selection)))))
    ;; If no selection found, return empty
    (if selection-data
        `((content . ,(vector (list (cons 'type "text")
                                    (cons 'text (json-encode selection-data))))))
      `((content . ,(vector (list (cons 'type "text")
                                  (cons 'text (json-encode '((text . "")
                                                             (filePath . "")
                                                             (selection . ((start . ((line . 0) (character . 0)))
                                                                           (end . ((line . 0) (character . 0)))
                                                                           (isEmpty . t))))))))))))))

;;; Websocket server management functions
;; NOTE: Authentication validation limitation
;; The websocket.el library doesn't provide access to HTTP headers during the
;; handshake phase, so we cannot validate the x-claude-code-ide-authorization
;; header as required by the protocol. This means any client can connect to our
;; websocket server if they know the port. The auth token in the lockfile provides
;; some security through obscurity, but proper header validation would be better.
(defun claude-code-mcp--on-open-server (session ws)
  "Handle WebSocket WS open for SESSION.

Put client WS in SESSION structure, so we can use it to send messages to
claude later."
  ;; Add immediate message to see if connection happens
  (setf (claude-code-mcp--session-client session) ws)
  (claude-code-mcp--log 'in 'connection-opened
                        `((port . ,(claude-code-mcp--session-port session))
                          (key . ,(claude-code-mcp--session-key session)))
                        nil))

(defun claude-code-mcp--on-close-server (session _ws)
  "Handle WebSocket WS close for SESSION.

Remove SESSION from `claude-code-mcp--sessions'."
  ;; Add immediate message to track disconnections
  (let ((key (claude-code-mcp--session-key session))
        (port (claude-code-mcp--session-port session)))
    ;; Mark as not initialized
    (setf (claude-code-mcp--session-initialized session) nil)
    ;; Remove lockfile
    (claude-code-mcp--remove-lockfile port)
    (claude-code-mcp--log 'in 'connection-closed
                          `((port . ,port)
                            (key . ,key))
                          nil)
    (remhash key claude-code-mcp--sessions)
    ;; Cancel global selection timer if no more sessions
    (when (and claude-code-mcp--selection-timer
               (= 0 (hash-table-count claude-code-mcp--sessions)))
      (cancel-timer claude-code-mcp--selection-timer)
      (setq claude-code-mcp--selection-timer nil))))

(defun claude-code-mcp--on-error-server (session _ws action error)
  "Handle WebSocket error for SESSION with WS during ACTION with ERROR."
  ;; Add immediate error message
  (message "MCP ERROR: port %d, action '%s': %s"
           (claude-code-mcp--session-port session)
           action
           (error-message-string error))
  (claude-code-mcp--log 'in 'websocket-error
                        `((action . ,action)
                          (error . ,(error-message-string error))
                          (port . ,(claude-code-mcp--session-port session))
                          (key . ,(claude-code-mcp--session-key session)))
                        nil)
  (message "WebSocket error on port %d during %s: %s"
           (claude-code-mcp--session-port session)
           action
           (error-message-string error)))

(defun claude-code-mcp--on-message (session ws frame)
  "Handle JSON-RPC messsage from WS FRAME for SESSION."
  (condition-case err
      (let* ((payload (websocket-frame-text frame))
             (message (condition-case json-err
                          (json-read-from-string payload)
                        (error
                         (claude-code-mcp--log 'in 'json-parse-error
                                               `((error . ,(error-message-string json-err))
                                                 (payload-length . ,(length payload)))
                                               payload)
                         (error "JSON parse error: %s" (error-message-string json-err)))))
             (id (alist-get 'id message))
             (method (alist-get 'method message))
             (params (alist-get 'params message)))
        ;; Log incoming message
        (claude-code-mcp--log 'in (if method 'request 'response) message payload)

        (pcase method
          ;; Protocol initialization
          ("initialize"
           (claude-code-mcp--handle-initialize session ws id params))
          ;; Tool listing
          ("tools/list"
           (claude-code-mcp--handle-tools-list session ws id params))
          ;; Tool invocation
          ("tools/call"
           (claude-code-mcp--handle-tools-call session ws id params))
          ;; Prompts listing (empty for now)
          ("prompts/list"
           (claude-code-mcp--send-response ws id '((prompts . []))))
          ;; Resources listing
          ("resources/list"
           (claude-code-mcp--handle-resources-list session ws id params))
          ;; Resources read
          ("resources/read"
           (claude-code-mcp--handle-resources-read session ws id params))
          ;; IDE connected notification
          ("ide_connected"
           (claude-code-mcp--handle-ide-connected session ws id params))
          ;; Notifications initialized
          ("notifications/initialized"
           (claude-code-mcp--send-response ws id '()))
          ;; Unknown method
          (_
           (claude-code-mcp--send-error ws id -32601 (format "Method not found: %s" method))
           (message "Method not found: %s" method))))
    (error
     (claude-code-mcp--log 'in 'message-handler-error
                           `((error . ,(error-message-string err)))
                           nil)
     (message "Error handling MCP message: %s" (error-message-string err)))))

(defun claude-code-mcp--start-server (key dir)
  "Start websocker server for claude process KEY running in DIR.

Returns the session object."
  (let* ((port (claude-code-mcp--find-free-port))
         (auth-token (claude-code-mcp--generate-uuid))
         (session (make-claude-code-mcp--session
                   :key key
                   :port port
                   :initialized nil
                   :auth-token auth-token
                   :opened-diffs (make-hash-table :test 'equal)
                   :deferred-responses (make-hash-table :test 'equal))))
    (condition-case err
        (let ((server (websocket-server
                       port
                       :host 'local
                       :on-open (lambda (ws)
                                  (claude-code-mcp--on-open-server session ws))
                       :on-close (lambda (ws)
                                   (claude-code-mcp--on-close-server session ws))
                       :on-message (lambda (ws frame)
                                     (claude-code-mcp--on-message session ws frame))
                       :on-error (lambda (ws action error)
                                   (claude-code-mcp--on-error-server session ws action error))
                       :protocol '("mcp"))))
          ;; Put server in the session
          (setf (claude-code-mcp--session-server session) server)

          ;; Create lock file with auth token
          (claude-code-mcp--create-lockfile dir port (claude-code-mcp--session-auth-token session))

          ;; Store session
          (puthash key session claude-code-mcp--sessions)

          ;; Register hooks for selection tracking
          (claude-code-mcp-register-hooks)

          ;; Return session
          session)
      (error
       (message "Failed to start MCP server: %s" (error-message-string err))
       nil))))

;;; Resources helpers
(defun claude-code-mcp--get-file-resources (&optional cursor)
  "Get list of file resources.
CURSOR is for pagination support (not implemented yet).
Returns a list of resource objects."
  (let ((resources '()))
    ;; Add open file buffers as resources
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (push `((uri . ,(concat "file://" file))
                (name . ,(file-name-nondirectory file))
                (description . ,(format "Open file in buffer: %s" (buffer-name buffer)))
                (mimeType . ,(claude-code-mcp--get-mime-type file)))
              resources)))
    ;; Add recent files if available
    (when (boundp 'recentf-list)
      (dolist (file (seq-take recentf-list 20)) ; Limit to 20 recent files
        (when (and (file-exists-p file)
                   ;; Don't duplicate already open files
                   (not (find-buffer-visiting file)))
          (push `((uri . ,(concat "file://" file))
                  (name . ,(file-name-nondirectory file))
                  (description . "Recent file")
                  (mimeType . ,(claude-code-mcp--get-mime-type file)))
                resources))))
    ;; Add project files if available (limit for performance)
    (when-let* ((project (project-current))
                (root (project-root project)))
      ;; Get a sample of project files
      (let ((project-files (project-files project)))
        (dolist (file (seq-take project-files 50)) ; Limit to 50 project files
          (let ((full-path (expand-file-name file root)))
            (when (and (file-regular-p full-path)
                       ;; Don't duplicate already listed files
                       (not (cl-find-if (lambda (r)
                                          (string= (alist-get 'uri r)
                                                   (concat "file://" full-path)))
                                        resources)))
              (push `((uri . ,(concat "file://" full-path))
                      (name . ,(file-name-nondirectory full-path))
                      (description . "Project file")
                      (mimeType . ,(claude-code-mcp--get-mime-type full-path)))
                    resources))))))
    (nreverse resources)))

(defun claude-code-mcp--get-mime-type (file)
  "Get MIME type for FILE based on extension."
  (let ((ext (file-name-extension file)))
    (cond
     ;; Text files
     ((member ext '("el" "elc")) "text/x-elisp")
     ((member ext '("py" "pyw")) "text/x-python")
     ((member ext '("js" "mjs" "cjs")) "text/javascript")
     ((member ext '("ts" "tsx")) "text/typescript")
     ((member ext '("jsx")) "text/jsx")
     ((member ext '("json")) "application/json")
     ((member ext '("xml")) "application/xml")
     ((member ext '("html" "htm")) "text/html")
     ((member ext '("css")) "text/css")
     ((member ext '("scss" "sass")) "text/x-scss")
     ((member ext '("md" "markdown")) "text/markdown")
     ((member ext '("txt" "text")) "text/plain")
     ((member ext '("yaml" "yml")) "text/yaml")
     ((member ext '("toml")) "text/x-toml")
     ((member ext '("ini" "cfg" "conf")) "text/x-ini")
     ((member ext '("sh" "bash" "zsh")) "text/x-shellscript")
     ((member ext '("c" "h")) "text/x-c")
     ((member ext '("cpp" "cc" "cxx" "hpp" "hxx")) "text/x-c++")
     ((member ext '("java")) "text/x-java")
     ((member ext '("rs")) "text/x-rust")
     ((member ext '("go")) "text/x-go")
     ((member ext '("rb")) "text/x-ruby")
     ((member ext '("php")) "text/x-php")
     ((member ext '("swift")) "text/x-swift")
     ((member ext '("kt" "kts")) "text/x-kotlin")
     ((member ext '("scala")) "text/x-scala")
     ((member ext '("clj" "cljs" "cljc")) "text/x-clojure")
     ((member ext '("lisp" "lsp")) "text/x-lisp")
     ((member ext '("vim")) "text/x-vim")
     ((member ext '("lua")) "text/x-lua")
     ((member ext '("r" "R")) "text/x-r")
     ((member ext '("m" "mm")) "text/x-objc")
     ((member ext '("sql")) "text/x-sql")
     ((member ext '("graphql" "gql")) "text/x-graphql")
     ((member ext '("proto")) "text/x-protobuf")
     ((member ext '("dockerfile" "Dockerfile")) "text/x-dockerfile")
     ((member ext '("makefile" "Makefile" "mk")) "text/x-makefile")
     ((member ext '("cmake" "CMakeLists.txt")) "text/x-cmake")
     ;; Default
     (t "text/plain"))))

;;; Selection
(defun claude-code-mcp--get-selection ()
  "Return current selection information."
  (when buffer-file-name
    (let* ((file-path buffer-file-name)
           (point-pos (point))
           (has-region (use-region-p))
           (region-start (if has-region (region-beginning) point-pos))
           (region-end (if has-region (region-end) point-pos))
           (text (if has-region
                     (buffer-substring-no-properties region-start region-end)
                   ""))
           ;; Convert to line/column positions
           (start-line (1- (line-number-at-pos region-start)))
           (start-col (save-excursion
                        (goto-char region-start)
                        (current-column)))
           (end-line (1- (line-number-at-pos region-end)))
           (end-col (save-excursion
                      (goto-char region-end)
                      (current-column)))
           (selection `((start . ((line . ,start-line)
                                  (character . ,start-col)))
                        (end . ((line . ,end-line)
                                (character . ,end-col)))
                        (isEmpty . ,(not has-region))))
           (file-url (concat "file://" file-path)))
      `((text . ,text)
        (filePath . ,file-path)
        (fileUrl . ,file-url)
        (selection . ,selection)))))

(defun claude-code-mcp--track-selection-change ()
  "Track selection change in file buffers."
  (when (and buffer-file-name
             ;; Only track if we have initialized sessions
             (cl-some (lambda (session)
                        (and (claude-code-mcp--session-initialized session)
                             (claude-code-mcp--session-client session)))
                      (hash-table-values claude-code-mcp--sessions)))
    ;; Cancel any existing timer
    (when claude-code-mcp--selection-timer
      (cancel-timer claude-code-mcp--selection-timer))
    ;; Set new timer for debounced update
    (setq claude-code-mcp--selection-timer
          (run-with-timer claude-code-mcp--selection-delay nil
                          #'claude-code-mcp--send-selection-to-all))))

(defun claude-code-mcp--send-selection-to-all ()
  "Send selection update to all initialized sessions."
  (when buffer-file-name
    (let* ((cursor-pos (point))
           (current-state (if (use-region-p)
                              `(,cursor-pos ,(region-beginning) ,(region-end))
                            `(,cursor-pos ,cursor-pos ,cursor-pos)))
           (selection (claude-code-mcp--get-selection)))
      (maphash (lambda (_key session)
                 (when (and (claude-code-mcp--session-initialized session)
                            (claude-code-mcp--session-client session))
                   (claude-code-mcp--send-notification
                    (claude-code-mcp--session-client session)
                    "selection_changed"
                    selection)))
               claude-code-mcp--sessions))))

;;; Hooks
(defun claude-code-mcp-register-hooks ()
  "Register hooks for MCP functionality."
  (add-hook 'post-command-hook #'claude-code-mcp--track-selection-change))

;;; Interactive functions standalone terminal use
(defun claude-code-mcp-start-websocket-server ()
  "Start a websocket server for the current buffer."
  (interactive)
  (if (gethash (buffer-name) claude-code-mcp--sessions)
      (message "Websocket server already running for buffer %s" (buffer-name))
    (claude-code-mcp--start-server (buffer-name) (expand-file-name default-directory))))

(defun claude-code-mcp-stop-websocket-server ()
  "Stop the websocket server for the current buffer."
  (interactive)
  (if-let* ((session (gethash (buffer-name) claude-code-mcp--sessions))
            (server (claude-code-mcp--session-server session))
            (port (claude-code-mcp--session-port session)))
      (progn
        ;; Remove lockfile before closing server
        (claude-code-mcp--remove-lockfile port)
        (websocket-server-close server))
    (message "No websocket server running for buffer %s" (buffer-name))))

(defun claude-code-mcp-stop-all-servers ()
  "Stop all running MCP websocket servers."
  (interactive)
  (let ((count 0))
    (maphash (lambda (_key session)
               (when-let ((server (claude-code-mcp--session-server session))
                          (port (claude-code-mcp--session-port session)))
                 ;; Remove lockfile before closing server
                 (claude-code-mcp--remove-lockfile port)
                 (websocket-server-close server)
                 (cl-incf count)))
             claude-code-mcp--sessions)
    (clrhash claude-code-mcp--sessions)
    (when claude-code-mcp--selection-timer
      (cancel-timer claude-code-mcp--selection-timer)
      (setq claude-code-mcp--selection-timer nil))))

(defun claude-code-mcp-cleanup-orphaned-lockfiles ()
  "Clean up lockfiles for ports that are no longer in use."
  (interactive)
  (let* ((dir (expand-file-name "~/.claude/ide/"))
         (lockfiles (when (file-directory-p dir)
                      (directory-files dir t "\\.lock$")))
         (cleaned 0))
    (dolist (lockfile lockfiles)
      (let* ((port (string-to-number
                    (file-name-sans-extension
                     (file-name-nondirectory lockfile))))
             (in-use (cl-some (lambda (session)
                                (= (claude-code-mcp--session-port session) port))
                              (hash-table-values claude-code-mcp--sessions))))
        (unless in-use
          (condition-case nil
              (progn
                (delete-file lockfile)
                (cl-incf cleaned))
            (error nil)))))))

;;; Cleanup functions
(defun claude-code-mcp--cleanup-session (key)
  "Clean up the MCP session for KEY."
  (when-let* ((session (gethash key claude-code-mcp--sessions))
              (server (claude-code-mcp--session-server session))
              (port (claude-code-mcp--session-port session)))
    ;; Remove lockfile before closing server
    (claude-code-mcp--remove-lockfile port)
    ;; Close the websocket server
    (websocket-server-close server)
    ;; Remove from sessions hash table
    (remhash key claude-code-mcp--sessions)
    ;; Remove hooks if no more sessions
    (when (= 0 (hash-table-count claude-code-mcp--sessions))
      (remove-hook 'post-command-hook #'claude-code-mcp--track-selection-change))))

(defun claude-code-mcp--cleanup-on-exit ()
  "Clean up all MCP sessions and lockfiles on Emacs exit."
  (claude-code-mcp-stop-all-servers))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-mcp--cleanup-on-exit)

(provide 'claude-code-mcp)
;;; claude-code-mcp.el ends here
