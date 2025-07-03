;;; test-websocket-server.el --- Test the WebSocket server implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; Manual testing utilities for the Claude Code IDE WebSocket server.
;; Fixed version with proper Lisp syntax.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'claude-code-ws)

(defvar test-websocket-server nil "Test server instance.")
(defvar test-websocket-port 12345 "Default test port.")

(defun test-websocket-start-server (&optional port)
  "Start a test WebSocket server on PORT (default 12345)."
  (interactive (list (read-number "Port: " test-websocket-port)))
  (when test-websocket-server
    (test-websocket-stop-server))
  (setq test-websocket-port (or port test-websocket-port))
  (setq test-websocket-server
        (claude-code-ws-create-server
         :port test-websocket-port
         :on-open
         (lambda (client)
           (message "[WS Test] Client connected: %s" 
                    (process-contact (claude-code-ws-client-process client) :remote))
           ;; Send welcome message
           (claude-code-ws-send client
                                    (json-encode `((type . "welcome")
                                                   (message . "Connected to test WebSocket server")
                                                   (timestamp . ,(current-time-string))))))
         
         :on-message
         (lambda (client message)
           (message "[WS Test] Received: %s" message)
           ;; Try to parse as JSON
           (condition-case err
               (let ((json-msg (json-read-from-string message)))
                 (message "[WS Test] Parsed JSON: %S" json-msg)
                 ;; Echo back with metadata
                 (claude-code-ws-send
                  client
                  (json-encode `((type . "echo")
                                 (original . ,json-msg)
                                 (received-at . ,(current-time-string))
                                 (client . ,(process-contact 
                                             (claude-code-ws-client-process client) 
                                             :remote))))))
             (error
              ;; Not JSON, just echo the text
              (claude-code-ws-send
               client
               (json-encode `((type . "echo")
                              (text . ,message)
                              (received-at . ,(current-time-string))))))))
         
         :on-close
         (lambda (client code reason)
           (message "[WS Test] Client disconnected: code=%d reason=%s" code reason))
         
         :on-error
         (lambda (client error)
           (message "[WS Test] Error: %s" error))
         
         :on-ping
         (lambda (client data)
           (message "[WS Test] Ping received with %d bytes of data" (length data)))))
  
  (message "[WS Test] WebSocket server started on port %d" test-websocket-port)
  (message "[WS Test] Test with: websocat ws://127.0.0.1:%d" test-websocket-port)
  (message "[WS Test] Or test with: wscat -c ws://127.0.0.1:%d" test-websocket-port))

(defun test-websocket-stop-server ()
  "Stop the test WebSocket server."
  (interactive)
  (if test-websocket-server
      (progn
        (claude-code-ws-stop-server test-websocket-server)
        (setq test-websocket-server nil)
        (message "[WS Test] Server stopped"))
    (message "[WS Test] No server running")))

(defun test-websocket-server-status ()
  "Show status of test WebSocket server."
  (interactive)
  (if test-websocket-server
      (let ((clients (claude-code-ws-server-clients test-websocket-server)))
        (message "[WS Test] Server running on port %d with %d client(s)"
                 test-websocket-port
                 (length clients))
        (dolist (client clients)
          (message "[WS Test]   Client: %s" 
                   (process-contact (claude-code-ws-client-process client) :remote))))
    (message "[WS Test] No server running")))

(defun test-websocket-send-to-all (message)
  "Send MESSAGE to all connected clients."
  (interactive "sMessage to send: ")
  (if test-websocket-server
      (let ((clients (claude-code-ws-server-clients test-websocket-server)))
        (if clients
            (progn
              (dolist (client clients)
                (claude-code-ws-send 
                 client
                 (json-encode `((type . "broadcast")
                                (message . ,message)
                                (timestamp . ,(current-time-string))))))
              (message "[WS Test] Sent to %d client(s)" (length clients)))
          (message "[WS Test] No clients connected")))
    (message "[WS Test] No server running")))

(defun test-websocket-simulate-mcp ()
  "Start server that simulates MCP protocol responses."
  (interactive)
  (test-websocket-stop-server)
  (setq test-websocket-server
        (claude-code-ws-create-server
         :port test-websocket-port
         :on-open
         (lambda (client)
           (message "[MCP Test] Client connected"))
         
         :on-message
         (lambda (client message)
           (message "[MCP Test] Received: %s" message)
           (condition-case err
               (let* ((request (json-read-from-string message))
                      (method (alist-get 'method request))
                      (id (alist-get 'id request))
                      (params (alist-get 'params request)))
                 (pcase method
                   ("initialize"
                    (claude-code-ws-send
                     client
                     (json-encode
                      `((jsonrpc . "2.0")
                        (id . ,id)
                        (result . ((protocolVersion . "2024-11-05")
                                   (capabilities . ((tools . ((listChanged . t)))
                                                    (prompts . ((listChanged . t)))
                                                    (resources . ((subscribe . t)
                                                                  (listChanged . t)))))
                                   (serverInfo . ((name . "test-mcp-server")
                                                  (version . "0.1.0")))))))))
                   ("tools/list"
                    (claude-code-ws-send
                     client
                     (json-encode
                      `((jsonrpc . "2.0")
                        (id . ,id)
                        (result . ((tools . ,(vector
                                             `((name . "openFile")
                                               (description . "Open a file")
                                               (inputSchema . ((type . "object")
                                                               (properties . ((filePath . ((type . "string")))))
                                                               (required . ,(vector "filePath")))))
                                             `((name . "getCurrentSelection")
                                               (description . "Get current selection")
                                               (inputSchema . ((type . "object")
                                                               (properties . ())
                                                               (required . ,(vector)))))))))))))
                   (_
                    (claude-code-ws-send
                     client
                     (json-encode
                      `((jsonrpc . "2.0")
                        (id . ,id)
                        (error . ((code . -32601)
                                  (message . ,(format "Method not found: %s" method))))))))))
             (error
              (message "[MCP Test] Error handling message: %s" 
                       (error-message-string err)))))))
  
  (message "[MCP Test] MCP simulation server started on port %d" test-websocket-port))

;; Test utilities

(defun test-websocket-check-dependencies ()
  "Check if required dependencies are available."
  (interactive)
  (message "Checking WebSocket server dependencies...")
  (message "  - make-network-process: %s" 
           (if (fboundp 'make-network-process) "OK" "MISSING"))
  (message "  - json-encode: %s" 
           (if (fboundp 'json-encode) "OK" "MISSING"))
  (message "  - secure-hash: %s" 
           (if (fboundp 'secure-hash) "OK" "MISSING"))
  (message "  - base64-encode-string: %s" 
           (if (fboundp 'base64-encode-string) "OK" "MISSING"))
  (message "All dependencies OK!"))

;; Provide test menu
(defun test-websocket-menu ()
  "Show test menu for WebSocket server."
  (interactive)
  (let ((choice (read-char-choice
                 (concat "[WebSocket Test Menu]\n"
                         "1. Start test server\n"
                         "2. Stop server\n"
                         "3. Server status\n"
                         "4. Send broadcast message\n"
                         "5. Start MCP simulation\n"
                         "6. Check dependencies\n"
                         "q. Quit\n"
                         "Choice: ")
                 '(?1 ?2 ?3 ?4 ?5 ?6 ?q))))
    (pcase choice
      (?1 (call-interactively 'test-websocket-start-server))
      (?2 (test-websocket-stop-server))
      (?3 (test-websocket-server-status))
      (?4 (call-interactively 'test-websocket-send-to-all))
      (?5 (test-websocket-simulate-mcp))
      (?6 (test-websocket-check-dependencies))
      (?q nil))))

(provide 'test-websocket-server)

;; Quick start
(message "WebSocket test server loaded. Run M-x test-websocket-menu to start.")

;;; test-websocket-server.el ends here