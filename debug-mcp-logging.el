;;; Debug MCP logging

;; Check if logging is enabled
(message "MCP logging enabled: %s" claude-code-mcp-enable-logging)

;; Check log buffer name
(message "Log buffer name: %s" claude-code-mcp-log-buffer-name)

;; Check if log buffer exists
(message "Log buffer exists: %s" (get-buffer claude-code-mcp-log-buffer-name))

;; Try to create a test log entry
(claude-code-mcp--log 'test 'debug-message '((test . "manual log entry")) nil)

;; Check if log buffer has content
(when-let ((buf (get-buffer claude-code-mcp-log-buffer-name)))
  (message "Log buffer size: %d" (buffer-size buf))
  (with-current-buffer buf
    (message "First 200 chars of log: %s" 
             (substring (buffer-string) 0 (min 200 (point-max))))))

;; Check active sessions
(message "Active MCP sessions: %d" (hash-table-count claude-code-mcp--sessions))

;; List all sessions
(maphash (lambda (key session)
           (message "Session %s: port=%d, websocket=%s" 
                    key 
                    (claude-code-mcp--session-port session)
                    (if (claude-code-mcp--session-websocket session) "active" "nil")))
         claude-code-mcp--sessions)
EOF < /dev/null