;;; claude-code-mcp-debug.el --- Debug helpers for MCP -*- lexical-binding: t; -*-

(require 'claude-code-mcp)

(defun claude-code-mcp-debug-status ()
  "Show MCP debugging status."
  (interactive)
  (with-output-to-temp-buffer "*MCP Debug Status*"
    (princ "=== MCP Debug Status ===\n\n")
    
    ;; Logging status
    (princ (format "Logging enabled: %s\n" claude-code-mcp-enable-logging))
    (princ (format "Log buffer: %s\n" claude-code-mcp-log-buffer-name))
    (if-let ((log-buf (get-buffer claude-code-mcp-log-buffer-name)))
        (princ (format "Log buffer size: %d bytes\n" (buffer-size log-buf)))
      (princ "Log buffer does not exist\n"))
    
    (princ "\n=== Active MCP Sessions ===\n")
    (if (zerop (hash-table-count claude-code-mcp--sessions))
        (princ "No active sessions\n")
      (maphash (lambda (key session)
                 (princ (format "\nSession: %s\n" key))
                 (princ (format "  Port: %d\n" (claude-code-mcp--session-port session)))
                 (princ (format "  Initialized: %s\n" 
                                (if (claude-code-mcp--session-initialized session) "yes" "no")))
                 (princ (format "  Websocket server: %s\n" 
                                (if (claude-code-mcp--session-websocket session) "active" "nil")))
                 (princ (format "  Client connected: %s\n"
                                (if (claude-code-mcp--session-client session) "yes" "no")))
                 (princ (format "  Auth token: %s\n" (claude-code-mcp--session-auth-token session))))
               claude-code-mcp--sessions))
    
    (princ "\n=== Lockfiles ===\n")
    (let ((lockfile-dir (expand-file-name "~/.claude/ide/")))
      (if (file-directory-p lockfile-dir)
          (dolist (file (directory-files lockfile-dir t "\\.lock$"))
            (princ (format "\n%s:\n" (file-name-nondirectory file)))
            (condition-case err
                (let* ((content (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string)))
                       (json (json-read-from-string content)))
                  (princ (format "  PID: %s\n" (alist-get 'pid json)))
                  (princ (format "  Workspace: %s\n" (elt (alist-get 'workspaceFolders json) 0)))
                  (princ (format "  IDE: %s\n" (alist-get 'ideName json))))
              (error (princ (format "  Error reading: %s\n" (error-message-string err))))))
        (princ "Lockfile directory does not exist\n")))
    
    (princ "\n=== Claude Buffers with MCP ===\n")
    (let ((found nil))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (boundp 'claude-code--mcp-session)
                     claude-code--mcp-session)
            (setq found t)
            (princ (format "  %s\n" (buffer-name))))))
    (unless found
      (princ "  No Claude buffers with MCP sessions\n")))))

(defun claude-code-mcp-debug-enable-logging ()
  "Enable MCP logging and show the log buffer."
  (interactive)
  (setq claude-code-mcp-enable-logging t)
  (switch-to-buffer-other-window (claude-code-mcp--get-log-buffer))
  (message "MCP logging enabled"))

(defun claude-code-mcp-debug-test-log ()
  "Test logging functionality."
  (interactive)
  (claude-code-mcp--log 'test 'manual-test 
                        `((message . "Manual test of logging")
                          (timestamp . ,(current-time-string))
                          (logging-enabled . ,claude-code-mcp-enable-logging))
                        nil)
  (message "Test log entry created - check %s" claude-code-mcp-log-buffer-name))

(provide 'claude-code-mcp-debug)