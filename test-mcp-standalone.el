;;; test-mcp-standalone.el --- Test MCP server standalone -*- lexical-binding: t; -*-

;; Test MCP server without starting Claude in Emacs
;; This allows testing the connection from external Claude CLI

;;; Code:

(require 'claude-code-mcp)
(require 'claude-code-selection)

(defvar test-mcp-standalone-session nil
  "Current standalone MCP test session.")

(defvar test-mcp-standalone-buffer nil
  "Buffer for standalone MCP test output.")

(defun test-mcp-standalone-start ()
  "Start MCP server standalone for testing with external Claude CLI."
  (interactive)
  ;; Clean up any existing session
  (test-mcp-standalone-stop)
  
  ;; Create output buffer
  (setq test-mcp-standalone-buffer 
        (get-buffer-create "*MCP Standalone Test*"))
  
  ;; Start MCP server
  (let* ((project-dir (expand-file-name default-directory))
         (instance-name "standalone-test"))
    (setq test-mcp-standalone-session
          (claude-code--mcp-start project-dir instance-name test-mcp-standalone-buffer))
    
    (if test-mcp-standalone-session
        (let ((port (claude-code--mcp-session-port test-mcp-standalone-session)))
          (with-current-buffer test-mcp-standalone-buffer
            (erase-buffer)
            (insert "=== MCP Standalone Test Server Started ===\n\n")
            (insert (format "Port: %d\n" port))
            (insert (format "Project: %s\n" project-dir))
            (insert (format "Instance: %s\n\n" instance-name))
            (insert "To connect from Claude CLI, run this in your terminal:\n\n")
            (insert "```bash\n")
            (insert "# Set environment variables\n")
            (insert (format "export CLAUDE_CODE_SSE_PORT=%d\n" port))
            (insert "export ENABLE_IDE_INTEGRATION=1\n\n")
            (insert "# Start Claude\n")
            (insert "claude\n\n")
            (insert "# In Claude, connect to the MCP server:\n")
            (insert "/ide\n")
            (insert "```\n\n")
            (insert "=== Server Log ===\n")
            (goto-char (point-max)))
          ;; Set up logging
          (test-mcp-standalone-setup-logging)
          (display-buffer test-mcp-standalone-buffer)
          (message "✓ MCP server started on port %d. See *MCP Standalone Test* buffer for instructions." port))
      (message "✗ Failed to start MCP server"))))

(defun test-mcp-standalone-stop ()
  "Stop the standalone MCP test server."
  (interactive)
  ;; Remove advice
  (advice-remove 'claude-code--mcp-on-message 
                 (lambda (session ws frame) nil))
  (advice-remove 'websocket-send-text
                 (lambda (ws text) nil))
  (advice-remove 'claude-code--mcp-on-open
                 (lambda (session ws) nil))
  (advice-remove 'claude-code--mcp-on-close
                 (lambda (session ws) nil))
  
  (when test-mcp-standalone-session
    (claude-code--mcp-stop test-mcp-standalone-session)
    (setq test-mcp-standalone-session nil)
    (message "✓ MCP standalone server stopped"))
  (when (and test-mcp-standalone-buffer
             (buffer-live-p test-mcp-standalone-buffer))
    (with-current-buffer test-mcp-standalone-buffer
      (goto-char (point-max))
      (insert "\n=== Server Stopped ===\n"))))

(defun test-mcp-standalone-status ()
  "Check status of standalone MCP server."
  (interactive)
  (if test-mcp-standalone-session
      (let ((port (claude-code--mcp-session-port test-mcp-standalone-session))
            (lockfile (claude-code--mcp-lockfile-path test-mcp-standalone-session)))
        (message "MCP server running on port %d" port)
        (message "Lock file: %s" lockfile)
        (when (claude-code--mcp-session-client test-mcp-standalone-session)
          (message "✓ Client connected!")))
    (message "No standalone MCP server running")))

(defun test-mcp-standalone-send-test-notification ()
  "Send a test notification to connected client."
  (interactive)
  (if (and test-mcp-standalone-session
           (claude-code--mcp-session-client test-mcp-standalone-session))
      (let ((notification (json-encode
                          `((jsonrpc . "2.0")
                            (method . "notifications/message")
                            (params . ((level . "info")
                                      (message . "Test notification from Emacs MCP server")))))))
        (websocket-send-text 
         (claude-code--mcp-session-client test-mcp-standalone-session)
         notification)
        (message "Sent test notification"))
    (message "No connected client")))

;; Enhanced logging that intercepts all messages
(defun test-mcp-standalone-setup-logging ()
  "Set up comprehensive logging for the MCP session."
  (when test-mcp-standalone-session
    ;; Store original functions
    (setq test-mcp--original-on-message #'claude-code--mcp-on-message)
    (setq test-mcp--original-send-response #'claude-code--mcp-send-response)
    (setq test-mcp--original-send-notification #'claude-code--mcp-send-notification)
    
    ;; Override on-message with logging
    (advice-add 'claude-code--mcp-on-message :before
                (lambda (session ws frame)
                  (when (eq session test-mcp-standalone-session)
                    (let ((payload (websocket-frame-text frame)))
                      (with-current-buffer test-mcp-standalone-buffer
                        (goto-char (point-max))
                        (insert (format "\n[%s] → Received:\n" 
                                        (format-time-string "%H:%M:%S")))
                        (condition-case nil
                            (insert (pp-to-string (json-read-from-string payload)))
                          (error (insert payload)))
                        (insert "\n"))))))
    
    ;; Override send-response with logging
    (advice-add 'websocket-send-text :before
                (lambda (ws text)
                  (when (and test-mcp-standalone-session
                             (eq ws (claude-code--mcp-session-client test-mcp-standalone-session)))
                    (with-current-buffer test-mcp-standalone-buffer
                      (goto-char (point-max))
                      (insert (format "\n[%s] ← Sent:\n" 
                                      (format-time-string "%H:%M:%S")))
                      (condition-case nil
                          (insert (pp-to-string (json-read-from-string text)))
                        (error (insert text)))
                      (insert "\n"))))))
    
    ;; Override on-open with logging
    (advice-add 'claude-code--mcp-on-open :after
                (lambda (session ws)
                  (when (eq session test-mcp-standalone-session)
                    (with-current-buffer test-mcp-standalone-buffer
                      (goto-char (point-max))
                      (insert (format "\n[%s] ✓ Client connected\n" 
                                      (format-time-string "%H:%M:%S"))))))))
    
    ;; Override on-close with logging
    (advice-add 'claude-code--mcp-on-close :after
                (lambda (session ws)
                  (when (eq session test-mcp-standalone-session)
                    (with-current-buffer test-mcp-standalone-buffer
                      (goto-char (point-max))
                      (insert (format "\n[%s] ✗ Client disconnected\n" 
                                      (format-time-string "%H:%M:%S")))))))

(provide 'test-mcp-standalone)
;;; test-mcp-standalone.el ends here