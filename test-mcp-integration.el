;;; test-mcp-integration.el --- Test MCP integration with Claude -*- lexical-binding: t; -*-

;;; Commentary:
;; Quick test functions for MCP integration

;;; Code:

(defun test-mcp-setup ()
  "Set up MCP for testing."
  (interactive)
  ;; Enable logging
  (setq claude-code-mcp-enable-logging t)
  ;; Enable IDE integration
  (setq claude-code-enable-ide-integration t)
  (message "MCP test setup complete. Logging: %s, IDE integration: %s"
           claude-code-mcp-enable-logging
           claude-code-enable-ide-integration))

(defun test-mcp-start-claude ()
  "Start Claude with IDE integration in current directory."
  (interactive)
  (unless buffer-file-name
    (error "Please open a file first to test getCurrentSelection"))
  (claude-code)
  ;; Show the log buffer in another window
  (when claude-code-mcp-enable-logging
    (display-buffer (claude-code-mcp--get-log-buffer))))

(defun test-mcp-check-tools ()
  "Check if getCurrentSelection is in the tools list."
  (interactive)
  (let ((tools (claude-code-mcp--get-tools-list)))
    (if (cl-some (lambda (tool)
                   (string= (alist-get 'name tool) "getCurrentSelection"))
                 tools)
        (message "✓ getCurrentSelection found in tools list")
      (message "✗ getCurrentSelection NOT found in tools list"))
    ;; Also show all tool names
    (message "Available tools: %s"
             (mapconcat (lambda (tool)
                          (alist-get 'name tool))
                        tools
                        ", "))))

(defun test-mcp-selection ()
  "Test the selection function directly."
  (interactive)
  (let ((selection (claude-code-mcp--get-selection)))
    (if selection
        (message "Selection data: %s" (json-encode selection))
      (message "No selection data (no file open?)"))))

(defun test-mcp-call-get-current-selection ()
  "Test calling getCurrentSelection directly."
  (interactive)
  (let ((result (claude-code-mcp--tool-get-current-selection nil)))
    (message "getCurrentSelection result: %s" 
             (alist-get 'text (aref (alist-get 'content result) 0)))))

;;; Test checklist:
(defun test-mcp-checklist ()
  "Show MCP testing checklist."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Test Checklist*")
    (erase-buffer)
    (insert "MCP Integration Test Checklist\n")
    (insert "==============================\n\n")
    (insert "1. [ ] Run M-x test-mcp-setup\n")
    (insert "2. [ ] Open a file with some code\n")
    (insert "3. [ ] Run M-x test-mcp-check-tools\n")
    (insert "4. [ ] Run M-x test-mcp-selection\n")
    (insert "5. [ ] Run M-x test-mcp-start-claude\n")
    (insert "6. [ ] Check *Messages* for 'MCP server started on port'\n")
    (insert "7. [ ] Check MCP log for connection and tools/list\n")
    (insert "8. [ ] Ask Claude: 'What text is currently selected?'\n")
    (insert "9. [ ] Check MCP log for getCurrentSelection call\n")
    (insert "10. [ ] Select some text and ask again\n")
    (insert "\nLog Commands:\n")
    (insert "- M-x claude-code-mcp-show-log - View MCP log\n")
    (insert "- M-x claude-code-mcp-clear-log - Clear log\n")
    (insert "- M-x claude-code-mcp-toggle-logging - Toggle logging\n")
    (display-buffer (current-buffer))))

(provide 'test-mcp-integration)
;;; test-mcp-integration.el ends here