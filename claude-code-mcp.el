;;; claude-code-mcp.el  --- Claude MCP over websockets implementation  -*- lexical-binding:t -*-

;;; Commentary:
;; Integrate claude-code.el with claude processes using websockets + MCP.


;;; Code:
;;; Require dependencies
(require 'websocket)

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
  "Get or create the MCP log buffer."
  (get-buffer-create claude-code-mcp-log-buffer-name))

(defun claude-code-mcp--log (direction type data &optional json-string)
  "Log MCP message to buffer.

DIRECTION is either 'in' or 'out'.
TYPE is a descriptive string like 'request' or 'response'.
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
  (let ((found-session nil))
    (maphash (lambda (_key session)
               (when (eq (claude-code-mcp--session-client session) client)
                 (setq found-session session)))
             claude-code-mcp--sessions)
    found-session))

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
                         (ideName . "Emacs")
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
(defun claude-code-mcp--handle-initialize (session ws id params)
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
    
    ;; Send tools/list_changed notification after a delay
    (run-with-timer claude-code-mcp--initial-notification-delay nil
                    (lambda ()
                      (when-let ((s (gethash (claude-code-mcp--session-key session) claude-code-mcp--sessions)))
                        (claude-code-mcp--send-notification
                         (claude-code-mcp--session-client s)
                         "notifications/tools/list_changed"
                         (make-hash-table :test 'equal)))))))

(defun claude-code-mcp--handle-tools-list (_session ws id params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (claude-code-mcp--send-response ws id `((tools . []))))

(defun claude-code-mcp--handle-tools-call (_session ws id params)
  "Handle tools/call request with ID and PARAMS from WS for SESSION."
  ;; [TODO]
  (claude-code-mcp--send-error ws id -32601 "No tools implemented yet"))

;;; Websocket server management functions
;; NOTE: Authentication validation limitation
;; The websocket.el library doesn't provide access to HTTP headers during the
;; handshake phase, so we cannot validate the x-claude-code-ide-authorization
;; header as required by the protocol. This means any client can connect to our
;; websocket server if they know the port. The auth token in the lockfile provides
;; some security through obscurity, but proper header validation would be better.
;; Consider implementing a custom handshake or switching to a different websocket
;; library if stricter security is needed.
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

(defun claude-code-mcp--on-close-server (session ws)
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

(defun claude-code-mcp--on-error-server (session ws action error)
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

          ;; Return session
          (message "MCP server started on port %d" port)
          session)
      (error
       (message "Failed to start MCP server: %s" (error-message-string err))
       nil))))

;;; Selection
;;;; Selection functions
(defun claude-code-mcp--get-selection ()
  "Get current selection information."
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

;; Removed claude-code-mcp--send-selection - we now use per-session sending

;;;; Selection global hooks
(defun claude-code-mcp--track-selection-change ()
  "Track selection changes in file buffers."
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

;; (add-hook 'post-command-hook #'claude-code-mcp--track-selection-change)
;; (remove-hook 'post-command-hook #'claude-code-mcp--selection-change)

;; (add-hook 'window-selection-change-functions #'claude-code-mcp--window-change)


;;; Hooks
(defun claude-code-mcp-register-hooks ()
  (add-hook 'post-command-hook #'claude-code-mcp--track-selection-change)
  ;; (add-hook 'window-selection-change-functions #'claude-code-mcp--window-change)
  )

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
    (maphash (lambda (key session)
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

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-mcp--cleanup-on-exit)

(provide 'claude-code-mcp)
;;; claude-code-mcp.el ends here
