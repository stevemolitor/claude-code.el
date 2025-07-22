;;; test-mcp-connection.el --- Test MCP connection and logging -*- lexical-binding: t; -*-

;; This file helps diagnose MCP connection and logging issues

(require 'claude-code-mcp)

(defun test-mcp-connection ()
  "Test MCP connection and logging."
  (interactive)
  
  ;; First, ensure logging is enabled
  (setq claude-code-mcp-enable-logging t)
  (message "MCP logging enabled: %s" claude-code-mcp-enable-logging)
  
  ;; Check if log buffer exists
  (let ((log-buffer (get-buffer claude-code-mcp-log-buffer-name)))
    (if log-buffer
        (progn
          (message "Log buffer exists with %d bytes" (buffer-size log-buffer))
          (switch-to-buffer-other-window log-buffer))
      (message "Log buffer does not exist - creating it")
      (switch-to-buffer-other-window (claude-code-mcp--get-log-buffer))))
  
  ;; Test logging directly
  (claude-code-mcp--log 'test 'manual-test 
                        '((message . "Testing logging functionality")
                          (timestamp . ,(current-time-string)))
                        nil)
  
  ;; Check active sessions
  (message "\nActive MCP sessions:")
  (if (= 0 (hash-table-count claude-code-mcp--sessions))
      (message "  No active sessions")
    (maphash (lambda (key session)
               (message "  Session '%s':" key)
               (message "    Port: %d" (claude-code-mcp--session-port session))
               (message "    Websocket: %s" 
                        (if (claude-code-mcp--session-websocket session) 
                            "connected" 
                          "not connected"))
               (message "    Client: %s"
                        (if (claude-code-mcp--session-client session)
                            "connected"
                          "not connected")))
             claude-code-mcp--sessions))
  
  ;; Check if Claude buffer has MCP session
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'claude-code--mcp-session)
                 claude-code--mcp-session)
        (message "\nBuffer '%s' has MCP session" (buffer-name))))))

;; Function to manually start MCP server for testing
(defun test-start-mcp-server ()
  "Manually start an MCP server for testing."
  (interactive)
  (let* ((buffer-name "*test-mcp*")
         (dir default-directory)
         (session (claude-code-mcp--start-server buffer-name dir)))
    (if session
        (message "Started MCP server on port %d" (claude-code-mcp--session-port session))
      (message "Failed to start MCP server"))))

;; Function to check websocket server
(defun test-check-websocket-servers ()
  "Check all websocket servers."
  (interactive)
  (message "Checking websocket servers...")
  (if (boundp 'websocket-server-websockets)
      (message "Active websocket servers: %s" websocket-server-websockets)
    (message "No websocket-server-websockets variable found")))

(provide 'test-mcp-connection)