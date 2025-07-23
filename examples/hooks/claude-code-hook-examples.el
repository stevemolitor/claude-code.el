;;; claude-code-hook-examples.el --- Example hook handlers for Claude Code -*- lexical-binding: t; -*-

;; Author: Example
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai

;;; Commentary:
;; This file provides examples of how to configure and use Claude Code hooks.
;; Copy and adapt these examples to your own configuration.

;;; Code:

;;;; Example Hook Handlers

;; Uses the new hook API where claude-code-handle-hook creates a plist message
;; with :type, :buffer-name, :json-data, and :args keys

(defun my-claude-notification-handler (message)
  "Handle Claude notification hooks with visual and audio feedback.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data))
        (args (plist-get message :args)))
    (cond
     ((eq hook-type 'notification)
      ;; Visual notification  
      (message "🤖 Claude is ready for input in %s! JSON: %s" buffer-name json-data)
      ;; Audio notification
      (ding)
      ;; Optional: switch to Claude buffer
      (when buffer-name
        (let ((claude-buffer (get-buffer buffer-name)))
          (when claude-buffer
            (display-buffer claude-buffer)))))
     ((eq hook-type 'stop)
      ;; Notification when Claude finishes
      (message "✅ Claude finished responding in %s! JSON: %s" buffer-name json-data)
      (ding)))))

(defun my-claude-tool-use-tracker (message)
  "Track Claude's tool usage for debugging/monitoring.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data)))
    (cond
     ((eq hook-type 'pre-tool-use)
      (message "🔧 Claude is about to use a tool in %s. JSON: %s" buffer-name json-data))
     ((eq hook-type 'post-tool-use)
      (message "✅ Claude finished using a tool in %s. JSON: %s" buffer-name json-data)))))

(defun my-claude-session-logger (message)
  "Log all Claude hook events to a file.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] %s: %s (JSON: %s)\n" timestamp hook-type buffer-name json-data))
      (append-to-file (point-min) (point-max) "~/claude-hooks.log"))))

(defun my-claude-org-task-manager (message)
  "Create org-mode entries for Claude sessions.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data)))
    (when (eq hook-type 'notification)
      ;; Create an org entry when Claude is ready
      (let ((org-file "~/claude-tasks-example.org")
            (task-title (format "Claude session in %s" buffer-name)))
        (when (file-exists-p org-file)
          (with-temp-buffer
            (insert (format "* TODO %s\n  SCHEDULED: <%s>\n  :PROPERTIES:\n  :CLAUDE_BUFFER: %s\n  :CLAUDE_JSON: %s\n  :END:\n\n"
                            task-title
                            (format-time-string "%Y-%m-%d %a %H:%M")
                            buffer-name
                            json-data))
            (append-to-file (point-min) (point-max) org-file)))))))

;;;; Hook Setup Examples

(defun setup-claude-hooks-basic ()
  "Set up basic Claude hook handling with notifications."
  (interactive)
  (add-hook 'claude-code-hook 'my-claude-notification-handler)
  (message "Basic Claude hooks configured"))

(defun setup-claude-hooks-advanced ()
  "Set up advanced Claude hook handling with multiple handlers."
  (interactive)
  ;; Add multiple handlers
  (add-hook 'claude-code-hook 'my-claude-notification-handler)
  (add-hook 'claude-code-hook 'my-claude-tool-use-tracker)
  (add-hook 'claude-code-hook 'my-claude-session-logger)
  (message "Advanced Claude hooks configured"))

(defun setup-claude-hooks-org-integration ()
  "Set up Claude hooks with org-mode integration."
  (interactive)
  (add-hook 'claude-code-hook 'my-claude-notification-handler)
  (add-hook 'claude-code-hook 'my-claude-org-task-manager)
  (message "Claude hooks with org-mode integration configured"))

;;;; Utility Functions

(defun remove-all-claude-hooks ()
  "Remove all Claude hook handlers."
  (interactive)
  (setq claude-code-hook nil)
  (message "All Claude hooks removed"))

(defun list-claude-hooks ()
  "Show currently configured Claude hook handlers."
  (interactive)
  (if claude-code-hook
      (message "Claude hooks: %s" 
               (mapconcat (lambda (f) (symbol-name f)) claude-code-hook ", "))
    (message "No Claude hooks configured")))

;;;; Usage Instructions
;;
;; To use these examples:
;;
;; 1. Load this file: (load-file "claude-code-hook-examples.el")
;; 2. Set up hooks: (setup-claude-hooks-basic)  ; or one of the other setup functions
;; 3. Configure Claude Code CLI hooks in .claude/settings.json:
;;
;;    {
;;      "hooks": {
;;        "Notification": [
;;          {
;;            "matcher": "",
;;            "hooks": [
;;              {
;;                "type": "command",
;;                "command": "tmpfile=$(mktemp); cat > \"$tmpfile\"; /opt/homebrew/bin/emacsclient --eval \"(claude-code-handle-hook 'notification \\\"$CLAUDE_BUFFER_NAME\\\" \\\"$tmpfile\\\")\"; rm \"$tmpfile\""
;;              }
;;            ]
;;          }
;;        ],
;;        "Stop": [
;;          {
;;            "matcher": "",
;;            "hooks": [
;;              {
;;                "type": "command",
;;                "command": "tmpfile=$(mktemp); cat > \"$tmpfile\"; /opt/homebrew/bin/emacsclient --eval \"(claude-code-handle-hook 'stop \\\"$CLAUDE_BUFFER_NAME\\\" \\\"$tmpfile\\\")\"; rm \"$tmpfile\""
;;              }
;;            ]
;;          }
;;        ]
;;      }
;;    }

(provide 'claude-code-hook-examples)

;;; claude-code-hook-examples.el ends here
