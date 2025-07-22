;;; claude-code-mcp-monitor.el --- Monitor MCP connections -*- lexical-binding: t; -*-

(require 'claude-code-mcp)

(defvar claude-code-mcp-monitor-timer nil
  "Timer for monitoring MCP connections.")

(defvar claude-code-mcp-monitor-buffer "*MCP Monitor*"
  "Buffer name for MCP monitoring.")

(defun claude-code-mcp-monitor-start ()
  "Start monitoring MCP connections."
  (interactive)
  (when claude-code-mcp-monitor-timer
    (cancel-timer claude-code-mcp-monitor-timer))
  (setq claude-code-mcp-monitor-timer
        (run-with-timer 0 2 #'claude-code-mcp-monitor-update))
  (switch-to-buffer-other-window (get-buffer-create claude-code-mcp-monitor-buffer))
  (message "MCP monitoring started - updates every 2 seconds"))

(defun claude-code-mcp-monitor-stop ()
  "Stop monitoring MCP connections."
  (interactive)
  (when claude-code-mcp-monitor-timer
    (cancel-timer claude-code-mcp-monitor-timer)
    (setq claude-code-mcp-monitor-timer nil))
  (message "MCP monitoring stopped"))

(defun claude-code-mcp-monitor-update ()
  "Update the monitor display."
  (with-current-buffer (get-buffer-create claude-code-mcp-monitor-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format-time-string "=== MCP Monitor - %Y-%m-%d %H:%M:%S ===\n\n"))
      
      ;; Check messages buffer for recent MCP messages
      (insert "Recent MCP Messages:\n")
      (with-current-buffer "*Messages*"
        (save-excursion
          (goto-char (point-max))
          (let ((lines '())
                (count 0))
            (while (and (< count 10) (not (bobp)))
              (forward-line -1)
              (when (looking-at ".*MCP.*")
                (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
                (setq count (1+ count))))
            (if lines
                (dolist (line lines)
                  (insert (format "  %s\n" line)))
              (insert "  (no recent MCP messages)\n")))))
      
      (insert "\n")
      
      ;; Active sessions
      (insert (format "Active Sessions: %d\n\n" (hash-table-count claude-code-mcp--sessions)))
      
      (if (zerop (hash-table-count claude-code-mcp--sessions))
          (insert "No active MCP sessions\n")
        (maphash
         (lambda (key session)
           (insert (format "Session: %s\n" key))
           (insert (format "  Port: %d\n" (claude-code-mcp--session-port session)))
           (insert (format "  Initialized: %s\n" 
                           (if (claude-code-mcp--session-initialized session) "YES" "NO")))
           (insert (format "  WebSocket: %s\n"
                           (if (claude-code-mcp--session-websocket session) 
                               "server running" 
                               "no server")))
           (insert (format "  Client: %s\n"
                           (if (claude-code-mcp--session-client session)
                               "CONNECTED"
                               "not connected")))
           (insert "\n"))
         claude-code-mcp--sessions))
      
      ;; Check lockfiles
      (insert "\nLockfiles:\n")
      (let ((lockfile-dir (expand-file-name "~/.claude/ide/")))
        (if (file-directory-p lockfile-dir)
            (let ((files (directory-files lockfile-dir t "\\.lock$")))
              (if files
                  (dolist (file files)
                    (insert (format "  %s\n" (file-name-nondirectory file))))
                (insert "  (no lockfiles)\n")))
          (insert "  (lockfile directory does not exist)\n")))
      
      ;; Log buffer status
      (insert "\nLog Buffer:\n")
      (if-let ((log-buf (get-buffer claude-code-mcp-log-buffer-name)))
          (insert (format "  Size: %d bytes\n" (buffer-size log-buf)))
        (insert "  Not created\n"))
      
      (goto-char (point-min)))))

(defun claude-code-mcp-show-log ()
  "Show the MCP log buffer."
  (interactive)
  (if-let ((buf (get-buffer claude-code-mcp-log-buffer-name)))
      (switch-to-buffer-other-window buf)
    (message "No MCP log buffer exists. Enable logging with: (setq claude-code-mcp-enable-logging t)")))

(provide 'claude-code-mcp-monitor)