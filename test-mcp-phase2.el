;;; test-mcp-phase2.el --- Test Phase 2 MCP integration -*- lexical-binding: t; -*-

;; Test file for Phase 2 MCP integration with claude-code.el
;; Usage: M-x eval-buffer, then M-x claude-code-test-phase2

;;; Code:

(require 'claude-code)
(require 'claude-code-mcp)
(require 'claude-code-mcp-integration)

(defun claude-code-test-phase2 ()
  "Test Phase 2 MCP integration."
  (interactive)
  (message "=== Starting Phase 2 MCP Integration Test ===")
  
  ;; Check if integration is enabled
  (if claude-code-enable-ide-integration
      (message "✓ IDE integration is enabled")
    (message "✗ IDE integration is disabled"))
  
  ;; Show current hooks
  (message "\nCurrent claude-code-start-hook:")
  (if claude-code-start-hook
      (dolist (hook claude-code-start-hook)
        (message "  - %s" hook))
    (message "  (empty)"))
  
  ;; Test starting Claude with MCP
  (message "\nStarting Claude with MCP integration...")
  (message "When Claude starts:")
  (message "  1. Check *Messages* for MCP server startup")
  (message "  2. Check ~/.claude/ide/ for lock file")
  (message "  3. Run this in terminal to verify env vars:")
  (message "     ps aux | grep claude | grep SSE_PORT")
  (message "\nPress C-c C-o to start Claude...")
  
  ;; Show how to verify
  (message "\n=== Verification Steps ===")
  (message "1. Start Claude: M-x claude-code")
  (message "2. Check process environment:")
  (message "   M-: (process-environment)")
  (message "3. Look for CLAUDE_CODE_SSE_PORT and ENABLE_IDE_INTEGRATION")
  (message "4. Check MCP session:")
  (message "   M-: claude-code--mcp-session")
  (message "5. Move cursor in a file to test selection tracking"))

(defun claude-code-test-phase2-check-session ()
  "Check the MCP session in current Claude buffer."
  (interactive)
  (if (claude-code--buffer-p (current-buffer))
      (if claude-code--mcp-session
          (let ((session claude-code--mcp-session))
            (message "=== MCP Session Info ===")
            (message "Port: %d" (claude-code--mcp-session-port session))
            (message "Project: %s" (claude-code--mcp-session-project-dir session))
            (message "Instance: %s" (claude-code--mcp-session-instance-name session))
            (message "Client connected: %s" 
                     (if (claude-code--mcp-session-client session) "Yes" "No"))
            ;; Check lock file
            (let ((lockfile (claude-code--mcp-lockfile-path session)))
              (if (file-exists-p lockfile)
                  (message "Lock file: ✓ %s" lockfile)
                (message "Lock file: ✗ Not found"))))
        (message "No MCP session in this buffer"))
    (message "Not in a Claude buffer")))

(defun claude-code-test-phase2-check-env ()
  "Check environment variables in Claude process."
  (interactive)
  (if (claude-code--buffer-p (current-buffer))
      (let ((proc (get-buffer-process (current-buffer))))
        (if proc
            (let ((env (process-get proc 'env)))
              (message "=== Process Environment ===")
              (if env
                  (progn
                    (dolist (var env)
                      (when (or (string-match "CLAUDE_CODE_SSE_PORT" var)
                                (string-match "ENABLE_IDE_INTEGRATION" var))
                        (message "  %s" var))))
                (message "No process environment found"))
              ;; Alternative: check command line
              (message "\nProcess command: %s" (process-command proc)))
          (message "No process in this buffer")))
    (message "Not in a Claude buffer")))

(defun claude-code-test-phase2-toggle ()
  "Toggle MCP integration on/off."
  (interactive)
  (if claude-code-enable-ide-integration
      (progn
        (claude-code-mcp-integration-disable)
        (message "MCP integration disabled"))
    (progn
      (claude-code-mcp-integration-enable)
      (message "MCP integration enabled"))))

(provide 'test-mcp-phase2)
;;; test-mcp-phase2.el ends here