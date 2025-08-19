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

(defun my-claude-pretooluse-handler (message)
  "Handle PreToolUse events with minibuffer permission prompts.
MESSAGE contains hook data including tool name and arguments."
  (when (eq (plist-get message :type) 'pre-tool-use)
    (let* ((json-data (plist-get message :json-data))
           (parsed-data (when (and json-data (stringp json-data))
                          (condition-case err
                              (json-read-from-string json-data)
                            (error 
                             (message "Error parsing JSON: %s" err)
                             nil))))
           (tool-name (when parsed-data (alist-get 'tool_name parsed-data)))
           (tool-input (when parsed-data (alist-get 'tool_input parsed-data)))
           (prompt-text (format "Claude wants to use %s with args: %s - Allow? (y/n/q): " 
                               tool-name tool-input))
           (response (read-char-choice prompt-text '(?y ?n ?q ?Y ?N ?Q)))
           (decision (cond 
                      ((memq response '(?y ?Y)) "allow")
                      ((memq response '(?n ?N)) "deny") 
                      (t "ask"))))
      ;; Return JSON response for Claude Code
      (json-encode `((hookSpecificOutput . ((hookEventName . "PreToolUse")
                                           (permissionDecision . ,decision)
                                           (permissionDecisionReason . "User decision via minibuffer"))))))))



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
  "Set up PreToolUse permission control via minibuffer."
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
;;                "command": "emacsclient --eval \"(claude-code-handle-hook 'notification \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
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
;;                "command": "emacsclient --eval \"(claude-code-handle-hook 'stop \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
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
;;                "command": "emacsclient --eval \"(claude-code-handle-hook 'notification \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\" \"$PWD\" \"$(date -Iseconds)\" \"$$\""
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
;;                "command": "emacsclient --eval \"(claude-code-handle-hook 'stop \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\" \"$PWD\" \"$(date -Iseconds)\" \"$$\""
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
