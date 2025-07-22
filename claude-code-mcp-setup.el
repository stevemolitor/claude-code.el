;;; claude-code-mcp-setup.el --- Easy setup for MCP integration -*- lexical-binding: t; -*-

;; Simple setup functions for testing MCP integration

(defun claude-code-mcp-enable-all ()
  "Enable MCP integration and logging."
  (interactive)
  (setq claude-code-enable-ide-integration t)
  (setq claude-code-mcp-enable-logging t)
  (message "MCP integration and logging enabled. Now start Claude with M-x claude-code"))

(defun claude-code-mcp-disable-all ()
  "Disable MCP integration and logging."
  (interactive)
  (setq claude-code-enable-ide-integration nil)
  (setq claude-code-mcp-enable-logging nil)
  (message "MCP integration and logging disabled"))

(defun claude-code-mcp-start-with-integration ()
  "Enable integration and start Claude."
  (interactive)
  (claude-code-mcp-enable-all)
  (call-interactively 'claude-code))

(provide 'claude-code-mcp-setup)