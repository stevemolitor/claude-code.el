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
(require 'cl-lib)

;; Declare functions from Doom Emacs workspaces
(declare-function +workspace-list-names "workspaces")
(declare-function +workspace-get "workspaces")
(declare-function +workspace/switch-to "workspaces")
(declare-function +workspace/switch-to-0 "workspaces")
(declare-function +workspace-buffer-list "workspaces")
(declare-function persp-parameter "persp-mode")

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

(defun claude-code--get-workspace-from-buffer-name (buffer-name)
  "Extract workspace directory from Claude BUFFER-NAME.
For example, *claude:/path/to/project/* returns /path/to/project/."
  (when (and buffer-name (string-match "^\\*claude:\\([^:]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name))
    (match-string 1 buffer-name)))

(defun claude-code--add-org-todo-entry (buffer-name message)
  "Add a TODO entry to the taskmaster org file.

BUFFER-NAME is the name of the Claude buffer that completed a task.
MESSAGE is the notification message to include in the TODO entry."
  (claude-code--ensure-claude-directory)
  (let* ((timestamp (claude-code--format-org-timestamp))
         (buffer-link (if buffer-name
                          (format "[[elisp:(switch-to-buffer \"%s\")][%s]]" buffer-name buffer-name)
                        "Unknown buffer"))
         (workspace-dir (claude-code--get-workspace-from-buffer-name buffer-name))
         (workspace-link (if workspace-dir
                             (format "[[elisp:(claude-code--switch-to-workspace-for-buffer \"%s\")][%s]]" buffer-name (file-name-nondirectory (directory-file-name workspace-dir)))
                           "Unknown workspace")))
    (with-temp-buffer
      (when (file-exists-p claude-code-taskmaster-org-file)
        (insert-file-contents claude-code-taskmaster-org-file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* TODO Claude task completed %s\n" timestamp))
      (insert (format "  Message: %s\n" (or message "Task completed")))
      (insert (format "  Buffer: %s\n" buffer-link))
      (insert (format "  Workspace: %s\n" workspace-link))
      (insert "\n")
      (write-region (point-min) (point-max) claude-code-taskmaster-org-file))))

(defun claude-code--get-most-recent-workspace ()
  "Get the most recent workspace from the taskmaster org file."
  (when (file-exists-p claude-code-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-code-taskmaster-org-file)
      (goto-char (point-max))
      (when (re-search-backward "Workspace: .*elisp:(let ((default-directory \"\([^\"]+\)\"))" nil t)
        (match-string 1)))))

(defun claude-code--find-workspace-for-buffer (buffer-name)
  "Find the workspace that contains the specified BUFFER-NAME."
  (when (and (boundp 'doom-version) (fboundp '+workspace-list-names))
    (let ((target-buffer (get-buffer buffer-name)))
      (when target-buffer
        (cl-loop for workspace-name in (+workspace-list-names)
                 for workspace = (+workspace-get workspace-name)
                 when (and workspace
                           (member target-buffer (+workspace-buffer-list workspace)))
                 return workspace-name)))))

(defun claude-code--switch-to-workspace-for-buffer (buffer-name)
  "Switch to the workspace that contains the specified BUFFER-NAME."
  (if-let ((workspace-name (claude-code--find-workspace-for-buffer buffer-name)))
      (progn
        (+workspace/switch-to workspace-name)
        (message "Switched to workspace: %s" workspace-name)
        workspace-name)
    (error "No workspace found for buffer: %s" buffer-name)))

(defun claude-code--clear-most-recent-org-entry ()
  "Clear (mark as DONE) the most recent TODO entry in the taskmaster org file."
  (when (file-exists-p claude-code-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-code-taskmaster-org-file)
      (goto-char (point-max))
      (when (re-search-backward "^\* TODO Claude task completed" nil t)
        (replace-match "* DONE Claude task completed")
        (write-region (point-min) (point-max) claude-code-taskmaster-org-file)))))

;;;; Enhanced notification system

;;;###autoload
(defun claude-code-handle-notification (message &optional buffer-name-override)
  "Handle notification with clickable link to Claude buffer and org queue entry.

MESSAGE is the notification message to display and log.
BUFFER-NAME-OVERRIDE is the name of the Claude buffer.

Creates a notification buffer with a clickable button to switch to the
specified Claude buffer and adds an entry to the taskmaster org file.
This is intended to be called from Claude Code hooks via emacsclient."
  (let* ((notification-buffer "*Claude Code Notification*")
         (target-buffer (when buffer-name-override (get-buffer buffer-name-override)))
         (workspace-dir (claude-code--get-workspace-from-buffer-name buffer-name-override)))
    
    ;; Add entry to org file
    (claude-code--add-org-todo-entry buffer-name-override message)
    
    ;; Only show popup if the Claude buffer is not currently visible or focused
    (unless (and target-buffer 
                 (or (get-buffer-window target-buffer)
                     (eq (current-buffer) target-buffer)))
      ;; Create and display notification buffer
      (with-current-buffer (get-buffer-create notification-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Claude notification: %s\n" (or message "Task completed")))
          (insert (format "Buffer: %s\n\n" (or buffer-name-override "unknown buffer")))
          
          (if (and target-buffer (buffer-live-p target-buffer))
              (insert-button "Switch to Claude buffer"
                             'action `(lambda (_button)
                                        (when (buffer-live-p ,target-buffer)
                                          (switch-to-buffer ,target-buffer)
                                          (kill-buffer ,notification-buffer)))
                             'help-echo (format "Click to switch to %s" buffer-name-override))
            (insert (format "Buffer '%s' not found or no longer exists." (or buffer-name-override "unknown"))))
          
          (insert "\n")
          (when workspace-dir
            (insert-button "Open Workspace"
                           'action `(lambda (_button)
                                      (claude-code--switch-to-workspace-for-buffer ,buffer-name-override)
                                      (kill-buffer ,notification-buffer))
                           'help-echo (format "Click to switch to workspace for buffer: %s" buffer-name-override))
            (insert "   ")
            (insert-button "Open & Clear"
                           'action `(lambda (_button)
                                      (claude-code--switch-to-workspace-for-buffer ,buffer-name-override)
                                      (claude-code--clear-most-recent-org-entry)
                                      (kill-buffer ,notification-buffer))
                           'help-echo (format "Click to switch to workspace and clear org entry for buffer: %s" buffer-name-override))
            (insert "\n"))
          
          (insert "\n")
          (insert-button "View Task Queue"
                         'action `(lambda (_button)
                                    (find-file ,claude-code-taskmaster-org-file)
                                    (kill-buffer ,notification-buffer))
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
                                                                (command . ,(format "BUFFER=\"$CLAUDE_BUFFER_NAME\"; %s --eval \"(progn (require 'claude-code-org-notifications) (claude-code-handle-notification \\\"Claude task completed\\\" \\\"$BUFFER\\\"))\""
                                                                                    emacsclient-cmd)))]))])

                                   (Stop . [((matcher . "")
                                             (hooks . [((type . "command")
                                                        (command . ,(format "BUFFER=\"$CLAUDE_BUFFER_NAME\"; %s --eval \"(progn (require 'claude-code-org-notifications) (claude-code-handle-notification \\\"Claude session stopped\\\" \\\"$BUFFER\\\"))\""
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

;;;; Workspace Navigation Commands

;;;###autoload
(defun claude-code-goto-recent-workspace ()
  "Go to the most recent workspace from the taskmaster org file."
  (interactive)
  (if-let ((workspace-dir (claude-code--get-most-recent-workspace)))
      ;; Try to find a Claude buffer in the workspace directory to use for detection
      (let ((claude-buffers (cl-loop for buf in (buffer-list)
                                     when (and (buffer-name buf)
                                               (string-match-p "^\\*claude:" (buffer-name buf))
                                               (string-match-p (regexp-quote workspace-dir) (buffer-name buf)))
                                     return (buffer-name buf))))
        (if claude-buffers
            (claude-code--switch-to-workspace-for-buffer claude-buffers)
          (message "No Claude buffer found for workspace directory: %s" workspace-dir)))
    (message "No recent workspace found in taskmaster.org")))

;;;###autoload
(defun claude-code-goto-recent-workspace-and-clear ()
  "Go to the most recent workspace and clear the org entry."
  (interactive)
  (if-let ((workspace-dir (claude-code--get-most-recent-workspace)))
      ;; Try to find a Claude buffer in the workspace directory to use for detection
      (let ((claude-buffers (cl-loop for buf in (buffer-list)
                                     when (and (buffer-name buf)
                                               (string-match-p "^\\*claude:" (buffer-name buf))
                                               (string-match-p (regexp-quote workspace-dir) (buffer-name buf)))
                                     return (buffer-name buf))))
        (if claude-buffers
            (progn
              (claude-code--switch-to-workspace-for-buffer claude-buffers)
              (claude-code--clear-most-recent-org-entry)
              (message "Switched to workspace and cleared org entry for buffer: %s" claude-buffers))
          (message "No Claude buffer found for workspace directory: %s" workspace-dir)))
    (message "No recent workspace found in taskmaster.org")))

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
