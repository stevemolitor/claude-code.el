;;; claude-code-hook-examples.el --- Example hook handlers for Claude Code -*- lexical-binding: t; -*-

;; Author: Example
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai

;;; Commentary:
;; This file provides examples of how to configure and use Claude Code hooks.
;; It includes both basic examples and enhanced examples showing how to pass
;; additional data beyond JSON using server-eval-args-left.
;; Copy and adapt these examples to your own configuration.

;;; Code:

;;;; Basic Hook Listeners

;; Uses the hook API where claude-code-handle-hook creates a plist message
;; with :type, :buffer-name, :json-data, and :args keys

(defun my-claude-notification-listener (message)
  "Handle Claude notification events with visual and audio feedback.
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

(defun my-claude-tool-use-listener (message)
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

(defun my-claude-session-listener (message)
  "Log all Claude hook events to a file.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] %s: %s (JSON: %s)\n" timestamp hook-type buffer-name json-data))
      (append-to-file (point-min) (point-max) "~/claude-hooks.log"))))

(defun my-claude-org-listener (message)
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

;;;; PreToolUse Interactive Permission Control Example
;;
;; This example shows how to intercept Claude's tool usage requests
;; and provide interactive permission control via minibuffer prompts.
;;
;; NOTE: Tools in Claude Code's permissions.allow list still trigger PreToolUse hooks.
;; This is a known limitation - see https://github.com/anthropics/claude-code/issues/4142
;; 
;; This handler works around the limitation by duplicating Claude Code's internal permission 
;; logic - reading the same settings files and applying the same pattern matching rules.
;; While this duplication is not ideal, it's necessary because:
;; 1. Claude Code doesn't expose its permission decisions to hooks
;; 2. The PreToolUse hook fires before Claude Code's internal permission check
;; 3. Without this workaround, users get redundant permission prompts for already-allowed tools
;;
;; LIMITATIONS: This workaround only checks file-based permissions and doesn't respect 
;; in-session permissions granted during the current Claude Code session.

(defcustom my-claude-check-allowlist t
  "Whether to check Claude Code settings files for allowed tools.
When non-nil, tools in the permissions.allow lists will be auto-approved.
When nil, all tools will prompt for permission."
  :type 'boolean
  :group 'claude-code)

(defun my-claude-pretooluse-handler (message)
  "Handle PreToolUse events with minibuffer permission prompts.
MESSAGE contains hook data including tool name and arguments.

If `my-claude-check-allowlist` is non-nil, checks Claude Code settings files
for allowed tools and auto-approves them. Otherwise, prompts for all tools."
  (when (eq (plist-get message :type) 'pre-tool-use)
    
    (let* ((json-data (plist-get message :json-data))
           (parsed-data (when json-data
                          (condition-case err
                              (json-read-from-string json-data)
                            (error nil))))
           (tool-name (or (when parsed-data 
                            (alist-get 'tool_name parsed-data))
                          "Unknown Tool"))
           (tool-input (or (when parsed-data 
                             (alist-get 'tool_input parsed-data))
                           "{}"))
           (tool-allowed-p (when my-claude-check-allowlist
                             (let ((all-allowed '())
                                   (files-to-check (list (expand-file-name "~/.claude/settings.json")
                                                        (expand-file-name ".claude/settings.json" default-directory)
                                                         (expand-file-name ".claude/settings.local.json" default-directory))))
                               (dolist (file files-to-check)
                                 (when (file-exists-p file)
                                   (condition-case err
                                       (let* ((settings-content (with-temp-buffer
                                                                  (insert-file-contents file)
                                                                  (buffer-string)))
                                              (settings-json (json-read-from-string settings-content))
                                              (permissions (alist-get 'permissions settings-json))
                                              (allow-list (alist-get 'allow permissions)))
                                         (when (arrayp allow-list)
                                           (setq all-allowed (append all-allowed (append allow-list nil)))))
                                     (error nil))))
                               ;; Check if current tool matches any allowed pattern
                               (cl-some (lambda (allowed-pattern)
                                          (or (string= tool-name allowed-pattern)
                                              (string-prefix-p (concat tool-name "(") allowed-pattern)
                                              (string-match-p (regexp-quote allowed-pattern) tool-name)))
                                        (delete-dups all-allowed))))))
      
      ;; Use the single allowlist check result
      (if tool-allowed-p
          ;; Tool is allowed, auto-approve
          (json-encode `((hookSpecificOutput . ((hookEventName . "PreToolUse")
                                               (permissionDecision . "allow")
                                               (permissionDecisionReason . "Tool in allowed list")))))
        ;; Tool not in allowed list or allowlist checking disabled, prompt user
        (let* ((formatted-input (if (stringp tool-input)
                                    tool-input
                                  (with-temp-buffer
                                    (insert (json-encode tool-input))
                                    (json-pretty-print-buffer)
                                    (buffer-string))))
               (prompt-text (format "Claude wants to use %s with args:\n%s\nAllow? (y/n/q): " 
                                   tool-name 
                                   formatted-input))
               (response (read-char-choice prompt-text '(?y ?n ?q ?Y ?N ?Q ?\e)))
               (decision (cond 
                          ((memq response '(?y ?Y)) "allow")
                          ((memq response '(?n ?N)) "deny")
                          ((memq response '(?q ?Q ?\e)) "ask")
                          (t "deny"))))
      ;; Clear the minibuffer
      (message "")
      ;; Return JSON response for Claude Code
      (json-encode `((hookSpecificOutput . ((hookEventName . "PreToolUse")
                                           (permissionDecision . ,decision)
                                           (permissionDecisionReason . "User decision via minibuffer"))))))))))




;;;; Hook Setup Examples

(defun setup-claude-hooks-basic ()
  "Set up basic Claude hook handling with notifications."
  (interactive)
  (add-hook 'claude-code-event-hook 'my-claude-notification-listener)
  (message "Basic Claude hooks configured"))

(defun setup-claude-hooks-advanced ()
  "Set up advanced Claude hook handling with multiple listeners."
  (interactive)
  ;; Add multiple listeners
  (add-hook 'claude-code-event-hook 'my-claude-notification-listener)
  (add-hook 'claude-code-event-hook 'my-claude-tool-use-listener)
  (add-hook 'claude-code-event-hook 'my-claude-session-listener)
  (message "Advanced Claude hooks configured"))

(defun setup-claude-hooks-org-integration ()
  "Set up Claude hooks with org-mode integration."
  (interactive)
  (add-hook 'claude-code-event-hook 'my-claude-notification-listener)
  (add-hook 'claude-code-event-hook 'my-claude-org-listener)
  (message "Claude hooks with org-mode integration configured"))

(defun setup-claude-pretooluse-control ()
  "Set up PreToolUse permission control via minibuffer.
Uses the single handler that respects `my-claude-check-allowlist` setting."
  (interactive)
  (remove-hook 'claude-code-event-hook 'my-claude-pretooluse-handler)
  (add-hook 'claude-code-event-hook 'my-claude-pretooluse-handler nil t)
  (message "Claude PreToolUse permission control configured"))


;;;; Utility Functions

(defun remove-all-claude-hooks ()
  "Remove all Claude hook handlers."
  (interactive)
  (setq claude-code-event-hook nil)
  (message "All Claude hooks removed"))

(defun list-claude-hooks ()
  "Show currently configured Claude hook handlers."
  (interactive)
  (if claude-code-event-hook
      (message "Claude hooks: %s" 
               (mapconcat (lambda (f) (symbol-name f)) claude-code-event-hook ", "))
    (message "No Claude hooks configured")))

;;;; Usage Instructions
;;
;; To use these examples:
;;
;; 1. Load this file: (load-file "claude-code-hook-examples.el")
;; 2. Set up hooks: (setup-claude-hooks-basic)  ; or one of the other setup functions
;; 3. Configure Claude Code CLI hooks in .claude/settings.json:

;;;; Basic Configuration (JSON data only):
;; Use this with the basic listeners (my-claude-notification-listener, etc.)
;;
;;    {
;;      "hooks": {
;;        "Notification": [
;;          {
;;            "matcher": "",
;;            "hooks": [
;;              {
;;                "type": "command",
;;                "command": "claude-code-hook-wrapper notification"
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
;;                "command": "claude-code-hook-wrapper stop"
;;              }
;;            ]
;;          }
;;        ]
;;      }
;;    }

;;;; Configuration with additional arguments:
;; Use this with my-claude-context-listener to access extra context data
;;
;;    {
;;      "hooks": {
;;        "Notification": [
;;          {
;;            "matcher": "",
;;            "hooks": [
;;              {
;;                "type": "command",
;;                "command": "claude-code-hook-wrapper notification \"$PWD\" \"$(date -Iseconds)\" \"$$\""
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
;;                "command": "claude-code-hook-wrapper stop \"$PWD\" \"$(date -Iseconds)\" \"$$\""
;;              }
;;            ]
;;          }
;;        ]
;;      }
;;    }
;;
;; This enhanced configuration passes:
;; - JSON data from stdin (always required)
;; - Current working directory ($PWD)  
;; - Timestamp ($(date -Iseconds))
;; - Process ID ($$)
;; 
;; The my-claude-context-listener function demonstrates how to extract and use this extra data.

(defun my-claude-context-listener (message)
  "Event listener that demonstrates using extra arguments passed from CLI.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys.
The :args field contains additional data like working directory, timestamp, and PID
when using the configuration with additional arguments."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data))
        (args (plist-get message :args)))
    (cond
     ((eq hook-type 'notification)
      ;; Extract additional arguments if they were passed
      (if args
          (let ((working-dir (nth 0 args))
                (timestamp (nth 1 args))
                (process-id (nth 2 args)))
            (message "🤖 Claude ready in %s! Working dir: %s, Time: %s, PID: %s"
                     buffer-name working-dir timestamp process-id)
            ;; Could log with more context
            (with-temp-buffer
              (insert (format "[%s] Claude ready in %s (dir: %s, PID: %s) - JSON: %s\n"
                              timestamp buffer-name working-dir process-id json-data))
              (append-to-file (point-min) (point-max) "~/claude-context.log")))
        ;; Fallback for basic configuration without extra args
        (message "🤖 Claude ready in %s! JSON: %s" buffer-name json-data)))
     ((eq hook-type 'stop)
      (if args
          (let ((working-dir (nth 0 args))
                (timestamp (nth 1 args))
                (process-id (nth 2 args)))
            (message "✅ Claude finished in %s! Working dir: %s, Time: %s, PID: %s"
                     buffer-name working-dir timestamp process-id))
        (message "✅ Claude finished in %s! JSON: %s" buffer-name json-data))))))

(defun setup-claude-hooks-with-context ()
  "Set up Claude hooks that use extra CLI arguments.
Use this with the configuration that passes additional arguments like $PWD, timestamp, and PID."
  (interactive)
  (add-hook 'claude-code-event-hook 'my-claude-context-listener)
  (message "Claude hooks with context awareness configured - use the configuration with additional arguments"))


(provide 'claude-code-hook-examples)

;;; claude-code-hook-examples.el ends here
