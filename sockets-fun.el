(require 'websocket)

(defvar claude-code--ws-server nil)
(defvar claude-code--ws-client nil)
(defvar claude-code--ws-protocol-version "2024-11-05" "MCP protocol version supported.")
(defconst claude-code--ws-port-min 10000 "Minimum port number for WebSocket server.")
(defconst claude-code--ws-port-max 65535 "Maximum port number for WebSocket server.")

(defun claude-code--shuffle-list (l)
  (let* ((v (apply #'vector l))
         (n (length v)))
    (dotimes (i (1- n) v)
      (cl-rotatef (aref v i) (aref v (+ i (random (- n i))))))
    (append v nil)))

(defun claude-code--ws-port-available-p (port)
  "Check if PORT is available for binding."
  (condition-case nil
      (let ((test-process
             (make-network-process
              :name "test"
              :server t
              ;; :host "127.0.0.1"
              :host 'local
              :service port
              :family 'ipv4)))
        (delete-process test-process)
        t)
    (error nil)))

(defun claude-code--ws-find-free-port ()
  "Find a free port in the allowed range."
  (let* ((ports (number-sequence claude-code--ws-port-min claude-code--ws-port-max))
         ;; Shuffle ports for randomness
         (shuffled-ports (claude-code--shuffle-list ports)))
    ;; Try ports until we find a free one
    (cl-loop for port in shuffled-ports
             when (claude-code--ws-port-available-p port)
             return port
             finally (error "No free ports available"))))

(defun claude-code--ws-create-lockfile (folder port)
  "Create lock file for SESSION."
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

(defun claude-code--ws-send-response (ws id result)
  "Send successful response with ID and RESULT to WS."
  (let ((response (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,id)
                     (result . ,result)))))
    (websocket-send-text ws response)))

(defun claude-code--ws-send-error (ws id code message &optional data)
  "Send error response with ID, CODE, MESSAGE and optional DATA to WS."
  (let ((response (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,id)
                     (error . ((code . ,code)
                               (message . ,message)
                               ,@(when data `((data . ,data)))))))))
    (websocket-send-text ws response)))

(defun claude-code--ws-handle-initialize (ws id params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (claude-code--ws-send-response
   ws id
   `((protocolVersion . ,claude-code--ws-protocol-version)
     (capabilities . ((tools . ((listChanged . t)))
                      (prompts . ((listChanged . t)))
                      (resources . ((subscribe . t)
                                    (listChanged . t)))))
     (serverInfo . ((name . "claude-code.el")
                    (version . "0.1.0"))))))

(defun claude-code--ws-handle-tools-list (ws id params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (claude-code--ws-send-response ws id `((tools . []))))

(defun claude-code--ws-handle-tools-call (session ws id params)
  "Handle tools/call request with ID and PARAMS from WS for SESSION."
  ;; Will be implemented when claude-code-tools.el is created
  (claude-code--ws-send-error ws id -32601 "No tools implemented yet"))

(defun claude-code--ws-on-open-server (ws)
  (message "server opened")
  (setq claude-code--ws-client ws))

(defun claude-code--ws-on-close-server (_ws)
  (setq claude-code--ws-client nil)
  (setq claude-code--ws-server nil)
  (message "server closed"))

(defun claude-code--ws-on-message (ws frame)
  (let* ((payload (websocket-frame-text frame))
         (message (json-read-from-string payload))
         (id (alist-get 'id message))
         (method (alist-get 'method message))
         (params (alist-get 'params message)))
    (message "id %s, method %s, params %S" id method params)
    (pcase method
      ;; Protocol initialization
      ("initialize"
       (claude-code--ws-handle-initialize ws id params))
      ;; Tool listing
      ("tools/list"
       (claude-code--ws-handle-tools-list ws id params))
      ;; Tool invocation
      ("tools/call"
       (claude-code--ws-handle-tools-call ws id params))
      ;; Prompts listing (empty for now)
      ("prompts/list"
       (claude-code--ws-send-response ws id '((prompts . []))))
      ;; Unknown method
      (_
       (claude-code--ws-send-error ws id -32601 (format "Method not found: %s" method))
       (message "Method not found: %s" method)))))

(defun claude-code--ws-start-server (dir)
  (let* ((port (claude-code--ws-find-free-port))
         (server (websocket-server
                  port
                  :host 'local
                  :on-open #'claude-code--ws-on-open-server
                  :on-close #'claude-code--ws-on-close-server
                  :on-message #'claude-code--ws-on-message
                  ;; TODO :on-error
                  :protocol '("mcp"))))
    (claude-code--ws-create-lockfile "/Users/stephenmolitor/scratch" port)
    (message "server started on port %d" port)
    server))

;; (setq claude-code--ws-server (claude-code--ws-start-server "~/scratch"))

;; (websocket-server-close claude-code--ws-server)
