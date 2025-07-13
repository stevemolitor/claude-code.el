;;; claude-code-org-notifications.el --- Org mode notification queue for Claude Code -*- lexical-binding: t; -*-

;; Author: Claude AI
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai, org

;;; Commentary:
;; This package extends claude-code.el with org mode notification queue functionality.
;; It provides persistent task tracking in ~/.claude/taskmaster.org with timestamps
;; and clickable buffer links, plus smart popup notifications that only appear when
;; the Claude buffer isn't currently visible.

;;; Code:

(require 'json)

;;;; Customization

(defcustom claude-code-taskmaster-org-file (expand-file-name "~/.claude/taskmaster.org")
  "Path to the org mode file for storing Claude task notifications.

This file will contain a queue of completed Claude tasks as TODO entries
with timestamps and links back to the original Claude buffers."
  :type 'file
  :group 'claude-code)

;;;; Org mode integration functions

(defun claude-code--ensure-claude-directory ()
  "Ensure the Claude directory exists for storing taskmaster.org."
  (let ((claude-dir (file-name-directory claude-code-taskmaster-org-file)))
    (unless (file-directory-p claude-dir)
      (make-directory claude-dir t))))

(defun claude-code--format-org-timestamp ()
  "Format current time as an org mode timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun claude-code--add-org-todo-entry (buffer-name message)
  "Add a TODO entry to the taskmaster org file.

BUFFER-NAME is the name of the Claude buffer that completed a task.
MESSAGE is the notification message to include in the TODO entry."
  (claude-code--ensure-claude-directory)
  (let ((timestamp (claude-code--format-org-timestamp))
        (buffer-link (if buffer-name
                         (format "[[elisp:(switch-to-buffer \"%s\")][%s]]" buffer-name buffer-name)
                       "Unknown buffer")))
    (with-temp-buffer
      (when (file-exists-p claude-code-taskmaster-org-file)
        (insert-file-contents claude-code-taskmaster-org-file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* TODO Claude task completed %s\n" timestamp))
      (insert (format "  Message: %s\n" (or message "Task completed")))
      (insert (format "  Buffer: %s\n" buffer-link))
      (insert "\n")
      (write-region (point-min) (point-max) claude-code-taskmaster-org-file))))

;;;; Enhanced notification system

;;;###autoload
(defun claude-code-handle-notification (message &optional buffer-name-override)
  "Handle notification with clickable link to Claude buffer and org queue entry.

MESSAGE is the notification message to display and log.
BUFFER-NAME-OVERRIDE allows specifying a different buffer name than the
environment variable.

Creates a notification buffer with a clickable button to switch to the
specified Claude buffer and adds an entry to the taskmaster org file.
This is intended to be called from Claude Code hooks via emacsclient."
  (let* (;; Handle backwards compatibility: if message looks like a buffer name, swap parameters
         (is-buffer-name (and message (string-match-p "^\\*claude:" message)))
         (actual-message (if is-buffer-name 
                             (or buffer-name-override "Task completed")
                           message))
         (actual-buffer-name (if is-buffer-name
                                 message
                               (or buffer-name-override
                                   (getenv "CLAUDE_BUFFER_NAME"))))
         (notification-buffer "*Claude Code Notification*")
         (target-buffer (when actual-buffer-name (get-buffer actual-buffer-name))))
    
    ;; Add entry to org file
    (claude-code--add-org-todo-entry actual-buffer-name actual-message)
    
    ;; Only show popup if the Claude buffer is not currently visible
    (unless (and target-buffer (get-buffer-window target-buffer))
      ;; Create and display notification buffer
      (with-current-buffer (get-buffer-create notification-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Claude notification: %s\n" (or actual-message "Task completed")))
          (insert (format "Buffer: %s\n\n" (or actual-buffer-name "unknown buffer")))
          
          (if (and target-buffer (buffer-live-p target-buffer))
              (insert-button "Switch to Claude buffer"
                             'action (lambda (_button)
                                       (when (buffer-live-p target-buffer)
                                         (switch-to-buffer target-buffer)
                                         (kill-buffer notification-buffer)))
                             'help-echo (format "Click to switch to %s" actual-buffer-name))
            (insert (format "Buffer '%s' not found or no longer exists." (or actual-buffer-name "unknown"))))
          
          (insert "\n\n")
          (insert-button "View Task Queue"
                         'action (lambda (_button)
                                   (find-file claude-code-taskmaster-org-file)
                                   (kill-buffer notification-buffer))
                         'help-echo "Click to view the org mode task queue")
          
          (goto-char (point-min))
          (setq buffer-read-only t))
        
        ;; Display the notification buffer
        (display-buffer notification-buffer)))))

;;;###autoload
(defun claude-code-test-notification ()
  "Test the notification system interactively."
  (interactive)
  (claude-code-handle-notification "Test notification message" (buffer-name)))

;;;; Settings.json configuration helper

;;;###autoload
(defun claude-code-setup-hooks ()
  "Add or update Claude Code notification hooks in ~/.claude/settings.json."
  (interactive)
  (let* ((claude-dir (expand-file-name "~/.claude"))
         (settings-file (expand-file-name "settings.json" claude-dir))
         (emacsclient-cmd (executable-find "emacsclient"))
         (hooks-config `((hooks . ((Notification . [((matcher . "")
                                                     (hooks . [((type . "command")
                                                                (command . ,(format "%s --eval \"(claude-code-handle-notification \\\"Claude task completed\\\" \\\"$CLAUDE_BUFFER_NAME\\\")\""
                                                                                    emacsclient-cmd)))]))])))))
         (existing-config (when (file-exists-p settings-file)
                            (condition-case err
                                (json-read-file settings-file)
                              (error
                               (message "Warning: Could not parse existing settings.json: %s" (error-message-string err))
                               nil))))
         (new-config (if existing-config
                         (let ((config-alist (if (hash-table-p existing-config)
                                                 (claude-code--hash-table-to-alist existing-config)
                                               existing-config)))
                           (claude-code--merge-hooks-config config-alist hooks-config))
                       hooks-config)))

    (unless emacsclient-cmd
      (error "emacsclient not found in PATH. Please ensure Emacs server is properly installed"))

    ;; Ensure Claude directory exists
    (unless (file-directory-p claude-dir)
      (make-directory claude-dir t))

    ;; Write updated config with pretty formatting
    (with-temp-file settings-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode new-config))))

    (message "Claude Code notification hooks added to %s" settings-file)))

(defun claude-code--hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to an alist."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    (nreverse result)))

(defun claude-code--merge-hooks-config (existing-config hooks-config)
  "Merge HOOKS-CONFIG into EXISTING-CONFIG, preserving other settings."
  (let ((config-copy (copy-alist existing-config))
        (hooks-entry (assoc 'hooks hooks-config)))
    (if (assoc 'hooks config-copy)
        ;; Hooks section exists, merge it
        (setcdr (assoc 'hooks config-copy) (cdr hooks-entry))
      ;; No hooks section, add it
      (push hooks-entry config-copy))
    config-copy))

;;;; Integration

;; Check if Doom Emacs is available and configure popup rule
(when (and (boundp 'doom-version) (fboundp 'set-popup-rule!))
  (set-popup-rule! "^\\*Claude Code Notification\\*$"
    :side 'bottom
    :size 0.3
    :select nil
    :quit t))

(provide 'claude-code-org-notifications)

;;; claude-code-org-notifications.el ends here
