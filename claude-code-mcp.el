;;; claude-code-mcp.el --- Claude MCP over websockets implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; Integrate claude-code.el with claude processes using websockets + MCP.


;;; Code:
;;; Require dependencies
(require 'json)
(require 'cl-lib)
(require 'subr-x) ; For when-let*
(require 'project) ; For project-current and project-root

;; Declare external functions and variables from websocket.el
(declare-function websocket-server "websocket" (port &rest args))
(declare-function websocket-server-close "websocket" (server))
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function websocket-frame-text "websocket" (frame))

;; Try to load websocket if available
(require 'websocket nil t)

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
  auth-token)      ; UUID auth token for validation

;;; Constants
(defconst claude-code-mcp--port-min 10000 "Minimum port number for WebSocket server.")
(defconst claude-code-mcp--port-max 65535 "Maximum port number for WebSocket server.")
(defconst claude-code-mcp--protocol-version "2024-11-05" "MCP protocol version supported.")
(defconst claude-code-mcp--selection-delay 0.05 "Delay in seconds before sending selection update.")
(defconst claude-code-mcp--initial-notification-delay 0.1 "Delay before sending initial notifications after connection.")

;;; Customization variables
(defcustom claude-code-mcp-enable-logging nil
  "Enable logging of MCP websocket messages."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-mcp-log-buffer-name "*Claude Code MCP Log*"
  "Name of the buffer for MCP logging."
  :type 'string
  :group 'claude-code)

;;; Logging functions
(defun claude-code-mcp--get-log-buffer ()
  "Return the MCP log buffer, creating it if necessary."
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

;;; Internal state variables
(defvar claude-code-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping claude code buffer names to ide websocket sessions.

Values are instances of the `claude-code-mcp--session' structure.

Keys can actually be any unique string, not just buffer names. Using a
key that is not a buffer name is useful for creating a websocket server
in Emacs to connect to an claude process running outside Emacs." )

(defvar claude-code-mcp--selection-timer nil
  "Timer for debouncing selection updates.")

(defvar claude-code-mcp--last-selection nil
  "Last selection state to avoid duplicate notifications.")

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
    (dotimes (i (1- n) v)
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
    (claude-code-mcp--log 'out 'error error-data response)
    (websocket-send-text ws response)))

(defun claude-code-mcp--send-notification (client method params)
  "Send notification with METHOD and PARAMS to CLIENT."
  (when client
    (let* ((session (claude-code-mcp--find-session-by-client client)))
      ;; Only send if session is initialized (unless it's the initialization-related notification)
      (when (or (and session (claude-code-mcp--session-initialized session))
                (string-prefix-p "notifications/" method))
        (let* ((data `((jsonrpc . "2.0")
                       (method . ,method)
                       (params . ,params)))
               (notification (json-encode data)))
          (claude-code-mcp--log 'out 'notification data notification)
          (condition-case err
              (websocket-send-text client notification)
            (error
             (claude-code-mcp--log 'out 'notification-error
                                   `((error . ,(error-message-string err))
                                     (method . ,method))
                                   nil))))))))

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

    ;; Send tools/list_changed notification after a delay [TODO] do we need the delay?
    (run-with-timer claude-code-mcp--initial-notification-delay nil
                    (lambda ()
                      (when-let ((s (gethash (claude-code-mcp--session-key session) claude-code-mcp--sessions)))
                        (claude-code-mcp--send-notification
                         (claude-code-mcp--session-client s)
                         "notifications/tools/list_changed"
                         (make-hash-table :test 'equal)))))))

(defun claude-code-mcp--handle-tools-list (_session ws id _params)
  "Handle tools/list request with ID and PARAMS from WS for SESSION."
  (claude-code-mcp--send-response
   ws id
   `((tools . ,(claude-code-mcp--get-tools-list)))))

(defun claude-code-mcp--handle-tools-call (_session ws id params)
  "Handle tools/call request with ID and PARAMS from WS for SESSION."
  (let* ((tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params))
         (handler (claude-code-mcp--get-tool-handler tool-name)))
    (if handler
        (condition-case err
            (let ((result (funcall handler arguments)))
              (claude-code-mcp--send-response ws id result))
          (error
           (claude-code-mcp--send-error
            ws id -32603
            (format "Error in tool %s: %s" tool-name (error-message-string err)))))
      (claude-code-mcp--send-error
       ws id -32601
       (format "Tool not found: %s" tool-name)))))

;;; MCP Tools
(defun claude-code-mcp--get-tools-list ()
  "Return the list of available MCP tools."
  (vector
   ;; Real tools
   `((name . "getCurrentSelection")
     (description . "Get the current text selection or cursor position in the active editor")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "openFile")
     (description . "Open a file in the editor")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string")
                                            (description . "The file URI or path to open")))))
                     (required . ,(vector "uri")))))
   ;; Stub tools to prevent crashes
   `((name . "openDiff")
     (description . "Open a diff view")
     (inputSchema . ((type . "object")
                     (properties . ((old_file_path . ((type . "string")))
                                    (new_file_path . ((type . "string")))
                                    (new_file_contents . ((type . "string")))
                                    (tab_name . ((type . "string")))))
                     (required . ,(vector "old_file_path" "new_file_path" "new_file_contents")))))
   `((name . "close_tab")
     (description . "Close a tab")
     (inputSchema . ((type . "object")
                     (properties . ((tab_name . ((type . "string")))))
                     (required . ,(vector "tab_name")))))
   `((name . "getDiagnostics")
     (description . "Get diagnostics for a file")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string"))))))))
   `((name . "closeAllDiffTabs")
     (description . "Close all diff tabs")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "getOpenEditors")
     (description . "Get the list of currently open files in the editor")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "getWorkspaceFolders")
     (description . "Get the list of workspace folders (project directories)")
     (inputSchema . ((type . "object")
                     (properties . ()))))))

(defun claude-code-mcp--get-tool-handler (name)
  "Return the handler function for tool NAME."
  (pcase name
    ("getCurrentSelection" #'claude-code-mcp--tool-get-current-selection)
    ("openFile" #'claude-code-mcp--tool-open-file)
    ("openDiff" #'claude-code-mcp--tool-open-diff)
    ("close_tab" #'claude-code-mcp--tool-close-tab)
    ("getDiagnostics" #'claude-code-mcp--tool-get-diagnostics)
    ("closeAllDiffTabs" #'claude-code-mcp--tool-close-all-diff-tabs)
    ("getOpenEditors" #'claude-code-mcp--tool-get-open-editors)
    ("getWorkspaceFolders" #'claude-code-mcp--tool-get-workspace-folders)
    (_ nil)))

;; Real tool implementations
(defun claude-code-mcp--tool-get-current-selection (_params)
  "Implementation of getCurrentSelection tool.
_PARAMS is unused for this tool."
  (let ((selection-data (claude-code-mcp--get-selection)))
    (if selection-data
        (list (cons 'content
                    (vector (list (cons 'type "text")
                                  (cons 'text (json-encode selection-data))))))
      (list (cons 'content
                  (vector (list (cons 'type "text")
                                (cons 'text (json-encode '((text . "")
                                                           (filePath . "")
                                                           (selection . ((start . ((line . 0) (character . 0)))
                                                                         (end . ((line . 0) (character . 0)))
                                                                         (isEmpty . t)))))))))))))

(defun claude-code-mcp--tool-open-file (params)
  "Implementation of openFile tool.
PARAMS contains uri."
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
        (list (cons 'content
                    (vector (list (cons 'type "text")
                                  (cons 'text (format "Opened file: %s" file-path)))))))
    (error
     (list (cons 'content
                 (vector (list (cons 'type "text")
                               (cons 'text (format "Error opening file: %s" (error-message-string err))))))))))

;; Stub tool implementations
(defun claude-code-mcp--tool-open-diff (params)
  "Open a diff view by creating a temporary buffer and showing both files.
PARAMS contains old_file_path, new_file_path, new_file_contents, tab_name."
  (let ((old-path (alist-get 'old_file_path params))
        (new-path (alist-get 'new_file_path params))
        (new-contents (alist-get 'new_file_contents params))
        (tab-name (alist-get 'tab_name params)))
    ;; Log the request
    (claude-code-mcp--log 'in 'openDiff-params params nil)
    
    ;; Ensure we have required parameters
    (unless (and old-path new-contents)
      (error "Missing required parameters: old_file_path and new_file_contents"))
    
    ;; Try to open the old file
    (let ((old-buffer (condition-case nil
                          (find-file-noselect old-path)
                        (error 
                         (message "Claude Code: Cannot open %s for diff, creating empty buffer" old-path)
                         (get-buffer-create (format "*Missing: %s*" (file-name-nondirectory old-path)))))))
      
      ;; Create a temporary buffer with the new contents
      ;; Use tab_name if provided, otherwise generate a name
      (let* ((temp-buffer-name (or tab-name
                                   (format "*Claude Diff: %s*" 
                                          (file-name-nondirectory (or new-path old-path)))))
             (temp-buffer (get-buffer-create temp-buffer-name)))
        
        ;; Set up the temporary buffer with new contents
        (with-current-buffer temp-buffer
          (erase-buffer)
          (insert new-contents)
          ;; Set the major mode based on the file extension
          (let ((mode (assoc-default (or new-path old-path) auto-mode-alist 'string-match)))
            (when mode (funcall mode)))
          ;; Mark as not modified to prevent save prompts
          (set-buffer-modified-p nil))
        
        ;; Display both buffers in a split window
        (delete-other-windows)
        (switch-to-buffer old-buffer)
        (split-window-horizontally)
        (other-window 1)
        (switch-to-buffer temp-buffer)
        (other-window 1)
        
        ;; Log success
        (claude-code-mcp--log 'out 'openDiff-success
                              `((old-path . ,old-path)
                                (new-path . ,new-path)
                                (tab-name . ,temp-buffer-name))
                              nil)
        
        ;; Return success response
        (list (cons 'content
                    (vector (list (cons 'type "text")
                                  (cons 'text "DIFF_VIEW_OPENED")))))))))

(defun claude-code-mcp--tool-close-tab (params)
  "Close a tab/buffer.
PARAMS should contain `path' or `tab_name' of the file to close."
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
              (list (cons 'content
                          (vector (list (cons 'type "text")
                                        (cons 'text "TAB_CLOSED"))))))
          ;; Buffer not found - log and return success anyway
          (progn
            (message "Claude Code: No buffer visiting %s, ignoring close request" path)
            (claude-code-mcp--log 'out 'close-tab-no-buffer
                                  `((path . ,path)
                                    (result . "Buffer not found, returning success"))
                                  nil)
            (list (cons 'content
                        (vector (list (cons 'type "text")
                                      (cons 'text "TAB_CLOSED")))))))))
     ;; Handle closing by tab name (buffer name)
     (tab-name
      (let ((buffer (get-buffer tab-name)))
        (if buffer
            (progn
              (kill-buffer buffer)
              (claude-code-mcp--log 'out 'close-tab-success
                                    `((tab-name . ,tab-name)
                                      (result . "TAB_CLOSED"))
                                    nil)
              ;; Return success response
              (list (cons 'content
                          (vector (list (cons 'type "text")
                                        (cons 'text "TAB_CLOSED"))))))
          ;; Buffer not found - log and return success anyway
          (progn
            (message "Claude Code: No buffer named %s, ignoring close request" tab-name)
            (claude-code-mcp--log 'out 'close-tab-no-buffer
                                  `((tab-name . ,tab-name)
                                    (result . "Buffer not found, returning success"))
                                  nil)
            (list (cons 'content
                        (vector (list (cons 'type "text")
                                      (cons 'text "TAB_CLOSED")))))))))
     ;; Neither path nor tab_name provided - this is still an error
     (t
      (error "Either 'path' or 'tab_name' must be provided")))))

(defun claude-code-mcp--tool-get-diagnostics (params)
  "Implementation of getDiagnostics tool.
PARAMS contains optional uri."
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
                               ((eq type :error) 1)     ; Error
                               ((eq type :warning) 2)   ; Warning
                               ((eq type :note) 3)      ; Information
                               (t 4)))                  ; Hint
                     (start-line (1- (line-number-at-pos beg)))
                     (start-char (save-excursion
                                  (goto-char beg)
                                  (current-column)))
                     (end-line (1- (line-number-at-pos end)))
                     (end-char (save-excursion
                                (goto-char end)
                                (current-column))))
                (push (list (cons 'uri (concat "file://" (buffer-file-name)))
                           (cons 'range (list (cons 'start (list (cons 'line start-line)
                                                               (cons 'character start-char)))
                                            (cons 'end (list (cons 'line end-line)
                                                           (cons 'character end-char)))))
                           (cons 'severity severity)
                           (cons 'message text)
                           (cons 'source "flymake"))
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
                (push (list (cons 'uri (concat "file://" (buffer-file-name)))
                           (cons 'range (list (cons 'start (list (cons 'line line)
                                                               (cons 'character col)))
                                            (cons 'end (list (cons 'line line)
                                                           (cons 'character col)))))
                           (cons 'severity severity)
                           (cons 'message msg)
                           (cons 'source checker))
                      diagnostics)))))))
    ;; Return the diagnostics
    (list (cons 'content
                (vector (list (cons 'type "text")
                              (cons 'text (json-encode (list (cons 'diagnostics (vconcat (nreverse diagnostics))))))))))))

(defun claude-code-mcp--tool-close-all-diff-tabs (_params)
  "Close all diff tabs created by Claude.
PARAMS is empty."
  (let ((closed-count 0))
    ;; Close all buffers that match our diff buffer naming pattern
    (dolist (buffer (buffer-list))
      (let ((buffer-name (buffer-name buffer)))
        (when (string-match "^\\*Claude Diff: .*\\*$" buffer-name)
          (claude-code-mcp--log 'out 'close-diff-tab
                                `((buffer . ,buffer-name))
                                nil)
          (kill-buffer buffer)
          (setq closed-count (1+ closed-count)))))
    ;; Also close any "*Missing: filename*" buffers we created
    (dolist (buffer (buffer-list))
      (when (string-match "^\\*Missing: .*\\*$" (buffer-name buffer))
        (kill-buffer buffer)
        (setq closed-count (1+ closed-count))))
    (message "Claude Code: Closed %d diff tabs" closed-count)
    ;; Return success with actual count
    (list (cons 'content
                (vector (list (cons 'type "text")
                              (cons 'text (format "CLOSED_%d_DIFF_TABS" closed-count))))))))

;; Real tool implementations for IDE features
(defun claude-code-mcp--tool-get-open-editors (_params)
  "Implementation of getOpenEditors tool.
_PARAMS is unused for this tool."
  (let ((editors '()))
    ;; Collect all file-visiting buffers
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (push (list (cons 'uri (concat "file://" file))
                    (cons 'name (file-name-nondirectory file))
                    (cons 'path file))
              editors)))
    ;; Return the list of open editors
    (list (cons 'content
                (vector (list (cons 'type "text")
                              (cons 'text (json-encode (list (cons 'editors (vconcat (nreverse editors))))))))))))

(defun claude-code-mcp--tool-get-workspace-folders (_params)
  "Implementation of getWorkspaceFolders tool.
_PARAMS is unused for this tool."
  (let ((folders '())
        (seen-dirs (make-hash-table :test 'equal)))
    ;; First, add current project if available
    (when-let* ((project (project-current)))
      (let ((root (project-root project)))
        (unless (gethash root seen-dirs)
          (puthash root t seen-dirs)
          (push (list (cons 'uri (concat "file://" root))
                      (cons 'name (file-name-nondirectory (directory-file-name root))))
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
            (push (list (cons 'uri (concat "file://" project-root))
                        (cons 'name (file-name-nondirectory (directory-file-name project-root))))
                  folders)))))
    ;; Return the list of workspace folders
    (list (cons 'content
                (vector (list (cons 'type "text")
                              (cons 'text (json-encode (list (cons 'folders (vconcat (nreverse folders))))))))))))

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
  (setf (claude-code-mcp--session-client session) ws)
  (claude-code-mcp--log 'in 'connection-opened
                        `((port . ,(claude-code-mcp--session-port session))
                          (key . ,(claude-code-mcp--session-key session)))
                        nil)
  (message "Claude Code connected to MCP server on port %d" (claude-code-mcp--session-port session)))

(defun claude-code-mcp--on-close-server (session _ws)
  "Handle WebSocket WS close for SESSION.

Remove SESSION from `claude-code-mcp--sessions'."
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
      (setq claude-code-mcp--selection-timer nil))
    (message "server running on port %d closed" port)))

(defun claude-code-mcp--on-error-server (session _ws action error)
  "Handle WebSocket error for SESSION with WS during ACTION with ERROR."
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
    ;; (if (gethash key claude-code-mcp--sessions)
    ;;   (error "Websocket server already started for key %s" key)
  (let* ((port (claude-code-mcp--find-free-port))
         (auth-token (claude-code-mcp--generate-uuid))
         (session (make-claude-code-mcp--session
                   :key key
                   :port port
                   :initialized nil
                   :auth-token auth-token)))
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
          (message "MCP server started on port %d" port)
          session)
      (error
       (message "Failed to start MCP server: %s" (error-message-string err))
       nil))))

;;; Selection
;;;; Selection functions
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

;;;; Selection global hooks
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
                              (list cursor-pos (region-beginning) (region-end))
                            (list cursor-pos cursor-pos cursor-pos)))
           (state-changed (not (equal current-state claude-code-mcp--last-selection))))
      ;; Only send if state actually changed
      (when state-changed
        (setq claude-code-mcp--last-selection current-state)
        (let ((selection (claude-code-mcp--get-selection)))
          ;; Send to all initialized sessions
          (maphash (lambda (_key session)
                     (when (and (claude-code-mcp--session-initialized session)
                                (claude-code-mcp--session-client session))
                       (claude-code-mcp--send-notification
                        (claude-code-mcp--session-client session)
                        "selection_changed"
                        selection)))
                   claude-code-mcp--sessions))))))

;;; Hooks
(defun claude-code-mcp-register-hooks ()
  "Register hooks for MCP functionality."
  (add-hook 'post-command-hook #'claude-code-mcp--track-selection-change))

;;; Interactive functions for testing

(defun claude-code-mcp-show-log ()
  "Show the MCP log buffer."
  (interactive)
  (switch-to-buffer (claude-code-mcp--get-log-buffer)))

(defun claude-code-mcp-clear-log ()
  "Clear the MCP log buffer."
  (interactive)
  (with-current-buffer (claude-code-mcp--get-log-buffer)
    (erase-buffer)
    (message "MCP log buffer cleared")))

(defun claude-code-mcp-toggle-logging ()
  "Toggle MCP logging on/off."
  (interactive)
  (setq claude-code-mcp-enable-logging (not claude-code-mcp-enable-logging))
  (message "MCP logging %s" (if claude-code-mcp-enable-logging "enabled" "disabled")))

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
        (websocket-server-close server)
        (message "Stopped websocket server for buffer %s" (buffer-name)))
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
      (setq claude-code-mcp--selection-timer nil))
    (message "Stopped %d MCP servers" count)))

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
            (error nil)))))
    (message "Cleaned up %d orphaned lockfiles" cleaned)))

(defun claude-code-mcp--cleanup-on-exit ()
  "Clean up all MCP sessions and lockfiles on Emacs exit."
  (claude-code-mcp-stop-all-servers))

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
      (remove-hook 'post-command-hook #'claude-code-mcp--track-selection-change))
    (message "Cleaned up MCP session for %s" key)))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-mcp--cleanup-on-exit)

(provide 'claude-code-mcp)
;;; claude-code-mcp.el ends here