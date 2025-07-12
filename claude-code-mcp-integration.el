;;; claude-code-mcp-integration.el --- Integrate MCP with claude-code.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stephen Molitor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the Apache License, Version 2.0.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:
;; Integration between claude-code.el and MCP server.
;; This module handles starting/stopping MCP servers alongside Claude instances
;; and passing environment variables to enable IDE integration.

;;; Code:

(require 'claude-code-mcp)
(require 'claude-code-selection)

;;;; Customization

(defcustom claude-code-enable-ide-integration t
  "Whether to enable IDE integration via MCP protocol.

When enabled, Claude Code will start an MCP server that allows
bidirectional communication between Claude and Emacs."
  :type 'boolean
  :group 'claude-code)

;;;; Integration Variables

(defvar-local claude-code--mcp-session nil
  "MCP session for the current Claude buffer.")

;;;; Start/Stop Integration

(defun claude-code--mcp-integration-start ()
  "Start MCP server for current Claude buffer.

This function is added to `claude-code-start-hook' when IDE
integration is enabled."
  (when claude-code-enable-ide-integration
    (let* ((instance-name (claude-code--extract-instance-name-from-buffer-name
                           (buffer-name)))
           (project-dir (claude-code--directory))
           (session (claude-code--mcp-start project-dir 
                                            (or instance-name "default")
                                            (current-buffer))))
      (when session
        (setq claude-code--mcp-session session)
        ;; Add cleanup hook
        (add-hook 'kill-buffer-hook #'claude-code--mcp-integration-stop nil t)))))

(defun claude-code--mcp-integration-stop ()
  "Stop MCP server for current Claude buffer."
  (when claude-code--mcp-session
    (claude-code--mcp-stop claude-code--mcp-session)
    (setq claude-code--mcp-session nil)))

;;;; Environment Variable Integration

(defun claude-code--mcp-get-environment-variables ()
  "Get environment variables to pass to Claude process.

Returns a list of strings in the format \"VAR=value\" to be passed
as part of the command line switches."
  (when claude-code-enable-ide-integration
    (let ((port (and claude-code--mcp-session
                     (claude-code--mcp-session-port claude-code--mcp-session))))
      (when port
        (list (format "--env=CLAUDE_CODE_SSE_PORT=%d" port)
              "--env=ENABLE_IDE_INTEGRATION=1")))))

;;;; Advice for Process Creation

(defun claude-code--mcp-advise-start (orig-fun arg extra-switches &optional force-prompt force-switch-to-buffer)
  "Advice for `claude-code--start' to add MCP environment variables.

ORIG-FUN is the original function.
ARG, EXTRA-SWITCHES, FORCE-PROMPT, and FORCE-SWITCH-TO-BUFFER are
passed through to the original function."
  ;; First, we need to check if IDE integration is enabled
  (if claude-code-enable-ide-integration
      ;; We need to start the MCP server BEFORE creating the process
      ;; to get the port number for environment variables
      (let* ((dir (if (equal arg '(16))
                      (read-directory-name "Project directory: ")
                    (claude-code--directory)))
             (existing-buffers (claude-code--find-claude-buffers-for-directory dir))
             (existing-instance-names (mapcar (lambda (buf)
                                                (or (claude-code--extract-instance-name-from-buffer-name
                                                     (buffer-name buf))
                                                    "default"))
                                              existing-buffers))
             (instance-name (claude-code--prompt-for-instance-name dir existing-instance-names force-prompt))
             ;; Start MCP server early to get port
             (temp-buffer (generate-new-buffer " *claude-mcp-temp*"))
             (session (claude-code--mcp-start dir instance-name temp-buffer))
             (mcp-env-vars (when session
                             (list (format "--env=CLAUDE_CODE_SSE_PORT=%d" 
                                           (claude-code--mcp-session-port session))
                                   "--env=ENABLE_IDE_INTEGRATION=1")))
             (all-switches (append extra-switches mcp-env-vars)))
        ;; Stop the temporary session first - we'll start a proper one in the hook
        (when session
          (claude-code--mcp-stop session))
        ;; Then clean up temp buffer
        (when (buffer-live-p temp-buffer)
          (kill-buffer temp-buffer))
        ;; Call original function with additional switches
        (funcall orig-fun arg all-switches force-prompt force-switch-to-buffer))
    ;; IDE integration disabled, call original function as-is
    (funcall orig-fun arg extra-switches force-prompt force-switch-to-buffer)))

;;;; Setup and Teardown

(defun claude-code-mcp-integration-enable ()
  "Enable MCP integration with claude-code."
  (interactive)
  ;; Add to startup hook
  (add-hook 'claude-code-start-hook #'claude-code--mcp-integration-start)
  ;; Add advice to include environment variables
  (advice-add 'claude-code--start :around #'claude-code--mcp-advise-start)
  (setq claude-code-enable-ide-integration t)
  (message "Claude Code MCP integration enabled"))

(defun claude-code-mcp-integration-disable ()
  "Disable MCP integration with claude-code."
  (interactive)
  ;; Remove from startup hook
  (remove-hook 'claude-code-start-hook #'claude-code--mcp-integration-start)
  ;; Remove advice
  (advice-remove 'claude-code--start #'claude-code--mcp-advise-start)
  (setq claude-code-enable-ide-integration nil)
  ;; Stop all active MCP sessions
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when claude-code--mcp-session
        (claude-code--mcp-integration-stop))))
  (message "Claude Code MCP integration disabled"))

;;;; Initialize on Load

;; Enable integration by default when this file is loaded
(when claude-code-enable-ide-integration
  (claude-code-mcp-integration-enable))

(provide 'claude-code-mcp-integration)
;;; claude-code-mcp-integration.el ends here