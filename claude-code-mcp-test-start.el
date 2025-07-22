;;; claude-code-mcp-test-start.el --- Test MCP server startup -*- lexical-binding: t; -*-

(require 'claude-code-mcp)

(defun test-mcp-startup ()
  "Test MCP server startup directly."
  (interactive)
  (let* ((key "*test-claude*")
         (dir default-directory))
    (message "Testing MCP startup...")
    (message "Key: %s" key)
    (message "Dir: %s" dir)
    
    ;; Enable logging first
    (setq claude-code-mcp-enable-logging t)
    
    ;; Try to start server
    (condition-case err
        (let ((session (claude-code-mcp--start-server key dir)))
          (if session
              (progn
                (message "SUCCESS: MCP server started on port %d" 
                         (claude-code-mcp--session-port session))
                (message "Check lockfile at: ~/.claude/ide/%d.lock" 
                         (claude-code-mcp--session-port session))
                ;; Show the session info
                (with-output-to-temp-buffer "*MCP Test Result*"
                  (princ (format "MCP Session started successfully!\n"))
                  (princ (format "Port: %d\n" (claude-code-mcp--session-port session)))
                  (princ (format "Auth token: %s\n" (claude-code-mcp--session-auth-token session)))
                  (princ (format "Key: %s\n" (claude-code-mcp--session-key session)))))
            (message "FAILED: claude-code-mcp--start-server returned nil")))
      (error
       (message "ERROR starting MCP server: %s" (error-message-string err))
       (with-output-to-temp-buffer "*MCP Error*"
         (princ (format "Error: %s\n" (error-message-string err)))
         (princ (format "Error details: %S" err)))))))

(provide 'claude-code-mcp-test-start)