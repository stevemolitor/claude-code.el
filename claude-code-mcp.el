;;; claude-code-mcp.el  --- Claude MCP over websockets implementation  -*- lexical-binding:t -*-

;;; Commentary:
;; Integrate claude-code.el with claude processes using websockets + MCP.


;;; Code:
;;; Require dependencies
(require 'websocket)
(require 'timeout)

;;; Session structure
(cl-defstruct claude-code-mcp--session
  "MCP session for a Claude instance."
  key
  server
  client
  port)

;;; Constants
(defconst claude-code-mcp--port-min 10000 "Minimum port number for WebSocket server.")
(defconst claude-code-mcp--port-max 65535 "Maximum port number for WebSocket server.")
(defconst claude-code-mcp--protocol-version "2024-11-05" "MCP protocol version supported.")
(defconst claude-code-mcp--selection-delay 0.05 "Delay in seconds before sending selection update.")

;;; Internal state variables
(defvar claude-code-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping claude code buffer names to ide websocket sessions.

Values are instances of the `claude-code-mcp--session' structure.

Keys can actually be any unique string, not just buffer names. Using a
key that is not a buffer name is useful for creating a websocket server
in Emacs to connect to an claude process running outside Emacs." )

;;; Util functions
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

(defun claude-code-mcp--create-lockfile (folder port)
  "Create lock file for claude running in FOLDER for SESSION."
  (let* ((port port)
         (dir (expand-file-name "~/.claude/ide/"))
         (file (expand-file-name (format "%d.lock" port) dir))
         (content (json-encode
                   `((pid . ,(emacs-pid))
                     (workspaceFolders . ,(vector folder))
                     (ideName . "Emacs")
                     (transport . "ws")))))
    ;; Ensure directory exists
    (make-directory dir t)
    ;; Write lock file
    (with-temp-file file
      (insert content))
    ;; Make it readable
    (set-file-modes file #o644)))

(defun claude-code-mcp--send-response (ws id result)
  "Send successful response with ID and RESULT to WS."
  (let ((response (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,id)
                     (result . ,result)))))
    (websocket-send-text ws response)))

(defun claude-code-mcp--send-error (ws id code message &optional data)
  "Send error response with ID, CODE, MESSAGE and optional DATA to WS."
  (let ((response (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,id)
                     (error . ((code . ,code)
                               (message . ,message)
                               ,@(when data `((data . ,data)))))))))
    (websocket-send-text ws response)))

(defun claude-code-mcp--send-notification (client method params)
  "Send notification with METHOD and PARAMS to CLIENT."
  (let ((notification (json-encode
                       `((jsonrpc . "2.0")
                         (method . ,method)
                         (params . ,params)))))
    (websocket-send-text client notification)))

;;; Handlers
(defun claude-code-mcp--handle-initialize (session ws id params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (when-let ((client (claude-code-mcp--session-client session)))
    (claude-code-mcp--send-response
     ws id
     `((protocolVersion . ,claude-code-mcp--protocol-version)
       (capabilities . ((tools . ((listChanged . t)))
                        (prompts . ((listChanged . t)))
                        (resources . ((subscribe . t)
                                      (listChanged . t)))))
       (serverInfo . ((name . "claude-code.el")
                      (version . "0.1.0")))))))

(defun claude-code-mcp--handle-tools-list (_session ws id params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (claude-code-mcp--send-response ws id `((tools . []))))

(defun claude-code-mcp--handle-tools-call (_session ws id params)
  "Handle tools/call request with ID and PARAMS from WS for SESSION."
  ;; [TODO]
  (claude-code-mcp--send-error ws id -32601 "No tools implemented yet"))

;;; Websocket server management functions
(defun claude-code-mcp--on-open-server (session ws)
  "Handle WebSocket WS open for SESSION.

Put client WS in SESSION structure, so we can use it to send messages to
claude later."
  (setf (claude-code-mcp--session-client session) ws)
  (message "Claude Code connected to MCP server on port %d" (claude-code-mcp--session-port session)))

(defun claude-code-mcp--on-close-server (session ws)
  "Handle WebSocket WS close for SESSION.

Remove SESSION from `claude-code-mcp--sessions'."
  (let ((key (claude-code-mcp--session-key session))
        (port (claude-code-mcp--session-port session)))
    (remhash key claude-code-mcp--sessions)
    (message "server running on port %d closed" port)))

(defun claude-code-mcp--on-message (session ws frame)
  "Handle JSON-RPC messsage from WS FRAME for SESSION."
  (let* ((payload (websocket-frame-text frame))
         (message (json-read-from-string payload))
         (id (alist-get 'id message))
         (method (alist-get 'method message))
         (params (alist-get 'params message)))
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
       (message "Method not found: %s" method)))))

(defun claude-code-mcp--start-server (key dir)
  "Start websocker server for claude process KEY running in DIR.

Returns the session object."
    ;; (if (gethash key claude-code-mcp--sessions)
    ;;   (error "Websocket server already started for key %s" key)
  (let* ((port (claude-code-mcp--find-free-port))
         (session (make-claude-code-mcp--session :key key :port port)))
    (condition-case err
        (let ((server (websocket-server
                       port
                       :host 'local
                       :on-open (lambda (ws)
                                  (claude-code-mcp--on-open-server session ws))
                       :on-close (lambda (ws)
                                   (claude-code-mcp--on-close-server session ws))
                       :on-message (lambda (ws frame)
                                     (message "HEY on message!")
                                     (claude-code-mcp--on-message session ws frame))

                       ;; [TODO] :on-error
                       :protocol '("mcp"))))
          ;; Put server in the session
          (setf (claude-code-mcp--session-server session) server)

          ;; Create lock file
          (claude-code-mcp--create-lockfile dir port)

          ;; Store session
          (puthash key session claude-code-mcp--sessions)

          ;; Create lockfile
          (claude-code-mcp--create-lockfile dir port)

          ;; Print message and return session
          ;; Return session
          (message "server started on %d" port)
          session)
      (error
       (message "Failed to start MCP server: %s" (error-message-string err))
       nil))))

;;; Selection
;;;; Selection functions
(defun claude-code-mcp--get-selection ()
  (let* ((file-path (buffer-file-name))
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
      (selection . ,selection))))

(defun claude-code-mcp--send-selection ()
  (maphash (lambda (_key session)
             (let ((client (claude-code-mcp--session-client session))
                   (selection (claude-code-mcp--get-selection)))
               (claude-code-mcp--send-notification client "selection_changed" selection)))
           claude-code-mcp--sessions))

(timeout-debounce! #'claude-code-mcp--send-selection claude-code-mcp--selection-delay)

;;;; Selection global hooks
(defun claude-code-mcp--track-selection-change ()
  (when buffer-file-name
    (claude-code-mcp--send-selection)))

;; (add-hook 'post-command-hook #'claude-code-mcp--track-selection-change)
;; (remove-hook 'post-command-hook #'claude-code-mcp--selection-change)

;; (add-hook 'window-selection-change-functions #'claude-code-mcp--window-change)


;;; Hooks
(defun claude-code-mcp-register-hooks ()
  (add-hook 'post-command-hook #'claude-code-mcp--track-selection-change)
  ;; (add-hook 'window-selection-change-functions #'claude-code-mcp--window-change)
  )

;;; Interactive functions for testing


(defun claude-code-mcp-start-websocket-server ()
  (interactive)
  (claude-code-mcp--start-server (buffer-name) (expand-file-name default-directory))) ;; [TODO] we may need to pass the directory

(defun claude-code-mcp-stop-websocket-server ()
  (interactive)
  (when-let* ((session (gethash (buffer-name) claude-code-mcp--sessions))
              (server (claude-code-mcp--session-server session)))
    (websocket-server-close server)))

(provide 'claude-code-mcp)
;;; claude-code-mcp.el ends here
