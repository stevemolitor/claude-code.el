;;; claude-code-org-notifications.el --- Org mode notification queue for Claude Code -*- lexical-binding: t; -*-

;; Author: Claude AI
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0") (org "9.0"))
;; Keywords: tools, ai, org

;;; Commentary:
;; This package extends claude-code.el with org mode notification queue functionality.
;; It provides persistent task tracking in ~/.claude/taskmaster.org with timestamps
;; and clickable buffer links, plus smart popup notifications that only appear when
;; the Claude buffer isn't currently visible.

;;; Code:

(require 'json)
(require 'cl-lib)

;; Forward declarations for claude-code functions
(declare-function claude-code-handle-hook "claude-code")
(defvar claude-code-event-hook)

;; Declare functions from perspective.el
(declare-function persp-names "persp-mode")
(declare-function persp-get-by-name "persp-mode")
(declare-function persp-switch "persp-mode")
(declare-function persp-buffers "persp-mode")

;; Declare functions from org-mode
(declare-function org-back-to-heading "org")
(declare-function org-next-visible-heading "org")

;; Declare functions from evil (optional)
(declare-function evil-insert-state "evil-states")

;; Constants
(defconst claude-code-notification-buffer-name "*Claude Code Notification*"
  "Name of the notification buffer.")

(defconst claude-code-org-todo-pattern "^\* TODO Claude task completed"
  "Pattern to match Claude task entries in org file.")

;;;; Customization

(defcustom claude-code-taskmaster-org-file (expand-file-name "~/.claude/taskmaster.org")
  "Path to the org mode file for storing Claude task notifications.

This file will contain a queue of completed Claude tasks as TODO entries
with timestamps and links back to the original Claude buffers."
  :type 'file
  :group 'claude-code)

(defcustom claude-code-auto-advance-queue nil
  "Whether to automatically advance to the next queue entry after sending input.

When non-nil, pressing enter (or sending any input) in a Claude buffer will:
1. Clear the current buffer from the task queue
2. Automatically switch to the next Claude buffer in the queue

This provides a streamlined workflow for processing multiple completed tasks."
  :type 'boolean
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
MESSAGE is the notification message to include in the TODO entry.

If an entry for the same buffer already exists, it will be removed first
to prevent duplicate entries in the queue."
  (claude-code--ensure-claude-directory)
  ;; First, remove any existing entry for this buffer
  (when buffer-name
    (claude-code--delete-queue-entry-for-buffer buffer-name))
  
  (let* ((timestamp (claude-code--format-org-timestamp))
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
      (insert (format "  Actions: [[elisp:(claude-code--switch-to-workspace-for-buffer \"%s\")][Go to Workspace]] | [[elisp:(claude-code--clear-current-org-entry-and-switch \"%s\")][Clear and Go to Workspace]]\n" buffer-name buffer-name))
      (insert "\n")
      (write-region (point-min) (point-max) claude-code-taskmaster-org-file))))

(defun claude-code--get-most-recent-buffer ()
  "Get the most recent Claude buffer name from the taskmaster org file."
  (when (file-exists-p claude-code-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-code-taskmaster-org-file)
      (goto-char (point-max))
      (when (re-search-backward "Buffer: \\[\\[elisp:(switch-to-buffer \"\\([^\"]+\\)\")\\]\\[" nil t)
        (match-string 1)))))

(defun claude-code--find-workspace-for-buffer (buffer-name)
  "Find the perspective that contains the specified BUFFER-NAME."
  (when (featurep 'persp-mode)
    (let ((target-buffer (get-buffer buffer-name)))
      (when target-buffer
        (cl-loop for persp-name in (persp-names)
                 for persp = (persp-get-by-name persp-name)
                 when (and persp
                           (member target-buffer (persp-buffers persp)))
                 return persp-name)))))

(defun claude-code--switch-to-workspace-for-buffer (buffer-name)
  "Switch to the perspective that contains BUFFER-NAME and navigate to it."
  (if-let ((persp-name (claude-code--find-workspace-for-buffer buffer-name)))
      (progn
        (persp-switch persp-name)
        (when-let ((target-buffer (get-buffer buffer-name)))
          (if-let ((window (get-buffer-window target-buffer)))
              ;; Buffer is visible, just select the window
              (select-window window)
            ;; Buffer is not visible, display it
            (switch-to-buffer target-buffer))
          ;; If using evil mode and this is a Claude buffer, enter insert mode
          (when (and (boundp 'evil-mode) evil-mode
                     (string-match-p "^\\*claude:" buffer-name))
            (evil-insert-state)))
        (message "Switched to perspective: %s and navigated to buffer: %s" persp-name buffer-name)
        persp-name)
    (error "No perspective found for buffer: %s" buffer-name)))

(defun claude-code--clear-most-recent-org-entry ()
  "Clear (mark as DONE) the most recent TODO entry in the taskmaster org file."
  (when (file-exists-p claude-code-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-code-taskmaster-org-file)
      (goto-char (point-max))
      (when (re-search-backward claude-code-org-todo-pattern nil t)
        (replace-match "* DONE Claude task completed")
        (write-region (point-min) (point-max) claude-code-taskmaster-org-file)))))

(defun claude-code--clear-current-org-entry-and-switch (buffer-name)
  "Delete the current TODO entry and switch to workspace for BUFFER-NAME."
  (interactive)
  (when (and (buffer-file-name)
             (string= (file-name-nondirectory (buffer-file-name)) "taskmaster.org"))
    ;; We're in the taskmaster.org file, delete current entry
    (save-excursion
      (org-back-to-heading t)
      (when (looking-at claude-code-org-todo-pattern)
        ;; Delete the entire entry (from heading to next heading or end of buffer)
        (let ((start (point)))
          (if (org-next-visible-heading 1)
              (delete-region start (point))
            (delete-region start (point-max))))
        (save-buffer)
        (message "Deleted entry and switching to workspace...")
        ;; Switch to workspace
        (claude-code--switch-to-workspace-for-buffer buffer-name)))))

;;;; Notification dismissal system

(defvar claude-code--notification-dismiss-active nil
  "Whether notification dismiss mode is currently active.")

(defvar claude-code--notification-buffer-name nil
  "Name of the current notification buffer.")

(defun claude-code--enable-notification-dismiss (buffer-name)
  "Enable global notification dismissal for BUFFER-NAME."
  (unless claude-code--notification-dismiss-active
    (setq claude-code--notification-dismiss-active t
          claude-code--notification-buffer-name buffer-name)
    ;; Use overriding-local-map for higher precedence
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<escape>") 'claude-code--dismiss-notification-if-visible)
      (define-key map (kbd "q") 'claude-code--dismiss-notification-if-visible)
      (setq overriding-local-map map))))

(defun claude-code--disable-notification-dismiss ()
  "Disable global notification dismissal and restore original ESC binding."
  (when claude-code--notification-dismiss-active
    (setq claude-code--notification-dismiss-active nil
          claude-code--notification-buffer-name nil)
    ;; Clear the overriding map
    (setq overriding-local-map nil)))

(defun claude-code--dismiss-notification-if-visible ()
  "Dismiss notification if visible."
  (interactive)
  (when (and claude-code--notification-dismiss-active
             claude-code--notification-buffer-name
             (get-buffer-window claude-code--notification-buffer-name))
    ;; Notification is visible, dismiss it
    (kill-buffer claude-code--notification-buffer-name)
    (claude-code--disable-notification-dismiss)))

(defun claude-code--dismiss-and-kill-buffer (buffer-name)
  "Helper to dismiss notification and kill BUFFER-NAME."
  (claude-code--disable-notification-dismiss)
  (kill-buffer buffer-name))

;;;; Enhanced notification system

(defun claude-code--buffer-visible-in-current-perspective-p (buffer-name)
  "Check if BUFFER-NAME is currently visible in the active perspective.

Returns t if the buffer is visible in a window in the current perspective,
nil otherwise."
  (when-let ((target-buffer (get-buffer buffer-name)))
    (and 
     ;; Buffer exists and is live
     (buffer-live-p target-buffer)
     ;; Buffer has a visible window
     (get-buffer-window target-buffer)
     ;; If persp-mode is active, check if we're in the right perspective
     (or (not (featurep 'persp-mode))
         (let ((buffer-persp (claude-code--find-workspace-for-buffer buffer-name))
               (current-persp (when (fboundp 'get-current-persp)
                                (let ((cp (get-current-persp)))
                                  (when cp (persp-name cp))))))
           ;; Either buffer has no perspective (global) or we're in its perspective
           (or (null buffer-persp)
               (string= buffer-persp current-persp)))))))

;;;###autoload
(defun claude-code-org-notification-listener (message)
  "Handle Claude Code hook events for org-mode task tracking.

MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys.
This is designed to work with the new claude-code-event-hook system."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data)))
    (cond
     ((eq hook-type 'notification)
      (claude-code--handle-task-completion buffer-name "Claude task completed" json-data))
     ((eq hook-type 'stop)
      (claude-code--handle-task-completion buffer-name "Claude session stopped" json-data)))))

(defun claude-code--handle-task-completion (buffer-name message json-data)
  "Handle a Claude task completion event.

BUFFER-NAME is the name of the Claude buffer.
MESSAGE is the notification message to display and log.
JSON-DATA is the JSON payload from Claude CLI."
  (let* ((notification-buffer claude-code-notification-buffer-name)
         (target-buffer (when buffer-name (get-buffer buffer-name)))
         (has-workspace (and buffer-name 
                             (claude-code--get-workspace-from-buffer-name buffer-name)))
         (buffer-visible (claude-code--buffer-visible-in-current-perspective-p buffer-name)))

    ;; Always add entry to org file regardless of visibility
    (claude-code--add-org-todo-entry buffer-name message)
    
    ;; Only show popup notification if buffer is not currently visible
    (unless buffer-visible
      (let ((queue-total (length (claude-code--get-all-queue-entries))))
        (with-current-buffer (get-buffer-create notification-buffer)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "%s%s\nBuffer: %s" 
                            message
                            (if (> queue-total 0)
                                (format " - %d in queue" queue-total)
                              "")
                            (or buffer-name "unknown buffer")))
            (goto-char (point-min))
            (setq buffer-read-only t))
          
          ;; Display as small popup without stealing focus
          (display-buffer notification-buffer 
                          '((display-buffer-in-side-window)
                            (side . bottom)
                            (window-height . 1)
                            (select . nil)))
          
          ;; Auto-dismiss after 2 seconds
          (run-with-timer 2 nil `(lambda ()
                                   (when (get-buffer ,notification-buffer)
                                     (kill-buffer ,notification-buffer)))))))
    
    ;; Disabled complex popup - keeping code for potential future use
    (when nil  ;; Change to t to re-enable complex popups
      (unless (and target-buffer 
                   (or (get-buffer-window target-buffer)
                       (eq (current-buffer) target-buffer)))
        ;; Create and display notification buffer
        (with-current-buffer (get-buffer-create notification-buffer)
          (let ((inhibit-read-only t)
                (queue-total (length (claude-code--get-all-queue-entries))))
            (erase-buffer)
            (insert (format "Claude notification: %s\n" (or message "Task completed")))
            (insert (format "Buffer: %s\n" (or buffer-name-override "unknown buffer")))
            ;; Add queue position information
            (when (> queue-total 0)
              (insert (format "Queue: %d entries\n" queue-total)))
            (insert "\n")

            (if (and target-buffer (buffer-live-p target-buffer))
                (insert-button "Switch to Claude buffer"
                               'action `(lambda (_button)
                                          (when (buffer-live-p ,target-buffer)
                                            (switch-to-buffer ,target-buffer)
                                            ;; Enter insert mode if using evil
                                            (when (and (boundp 'evil-mode) evil-mode
                                                       (string-match-p "^\\*claude:" ,buffer-name-override))
                                              (evil-insert-state))
                                            (claude-code--dismiss-and-kill-buffer ,notification-buffer)))
                               'help-echo (format "Click to switch to %s" buffer-name-override))
              (insert (format "Buffer '%s' not found or no longer exists." (or buffer-name-override "unknown"))))

            (insert "\n")
            (when has-workspace
              (insert-button "Open Workspace"
                             'action `(lambda (_button)
                                        (claude-code--switch-to-workspace-for-buffer ,buffer-name-override)
                                        (claude-code--dismiss-and-kill-buffer ,notification-buffer))
                             'help-echo (format "Click to switch to workspace for buffer: %s" buffer-name-override))
              (insert "   ")
              (insert-button "Open & Clear"
                             'action `(lambda (_button)
                                        (claude-code--switch-to-workspace-for-buffer ,buffer-name-override)
                                        (claude-code--clear-most-recent-org-entry)
                                        (claude-code--dismiss-and-kill-buffer ,notification-buffer))
                             'help-echo (format "Click to switch to workspace and clear org entry for buffer: %s" buffer-name-override))
              (insert "\n"))

            (insert "\n")
            (insert-button "View Task Queue"
                           'action `(lambda (_button)
                                      (find-file ,claude-code-taskmaster-org-file)
                                      (claude-code--dismiss-and-kill-buffer ,notification-buffer))
                           'help-echo "Click to view the org mode task queue")
            (insert "   ")
            (insert-button "Skip Entry"
                           'action `(lambda (_button)
                                      (claude-code--delete-queue-entry-for-buffer ,buffer-name-override)
                                      (claude-code--dismiss-and-kill-buffer ,notification-buffer)
                                      (message "Skipped queue entry for %s" ,buffer-name-override))
                           'help-echo "Click to skip this queue entry")

            (goto-char (point-min)))
          
          ;; Display the notification buffer and set up dismissal
          (display-buffer notification-buffer
                          '((display-buffer-in-side-window)
                            (side . bottom)
                            (window-height . 0.3)
                            (select . nil)))
          (claude-code--enable-notification-dismiss notification-buffer)
          
          ;; Auto-dismiss timer
          (run-with-timer 10 nil `(lambda ()
                                    (when (buffer-live-p (get-buffer ,notification-buffer))
                                      (claude-code--dismiss-and-kill-buffer ,notification-buffer)))))))))

;;;###autoload
(defun claude-code-test-notification ()
  "Test the notification system interactively."
  (interactive)
  (claude-code-org-notification-listener 
   (list :type 'notification 
         :buffer-name (buffer-name)
         :json-data "{\"test\": true}"
         :args '())))

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
                                                                (command . ,(format "%s --eval \"(claude-code-handle-hook 'notification \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
                                                                                    emacsclient-cmd)))]))])

                                   (Stop . [((matcher . "")
                                             (hooks . [((type . "command")
                                                        (command . ,(format "%s --eval \"(claude-code-handle-hook 'stop \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
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
  "Go to the most recent perspective from the taskmaster org file."
  (interactive)
  (if-let ((buffer-name (claude-code--get-most-recent-buffer)))
      (claude-code--switch-to-workspace-for-buffer buffer-name)
    (message "No recent perspective found in taskmaster.org")))

;;;###autoload
(defun claude-code-goto-recent-workspace-and-clear ()
  "Go to the most recent perspective and clear the org entry."
  (interactive)
  (if-let ((buffer-name (claude-code--get-most-recent-buffer)))
      (progn
        (claude-code--switch-to-workspace-for-buffer buffer-name)
        (claude-code--clear-most-recent-org-entry)
        (message "Switched to perspective and cleared org entry for buffer: %s" buffer-name))
    (message "No recent perspective found in taskmaster.org")))

;;;; Queue Navigation System

(defvar claude-code--queue-position 0
  "Current position in the taskmaster.org queue.")

(defun claude-code--get-all-queue-entries ()
  "Get all TODO entries from taskmaster.org as a list of buffer names."
  (when (file-exists-p claude-code-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-code-taskmaster-org-file)
      (goto-char (point-min))
      (let (entries)
        (while (re-search-forward claude-code-org-todo-pattern nil t)
          (when (re-search-forward "Buffer: \\[\\[elisp:(switch-to-buffer \"\\([^\"]+\\)\")\\]\\[" nil t)
            (push (match-string 1) entries)))
        (nreverse entries)))))

(defun claude-code--get-queue-entry-at-position (position)
  "Get the queue entry at POSITION, or nil if out of bounds."
  (let ((entries (claude-code--get-all-queue-entries)))
    (when (and entries (>= position 0) (< position (length entries)))
      (nth position entries))))

(defun claude-code--delete-queue-entry-for-buffer (buffer-name)
  "Delete the queue entry corresponding to BUFFER-NAME from taskmaster.org."
  (when (file-exists-p claude-code-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-code-taskmaster-org-file)
      (goto-char (point-min))
      (let (found)
        (while (and (not found) (re-search-forward claude-code-org-todo-pattern nil t))
          (let ((entry-start (match-beginning 0)))
            (when (re-search-forward (format "Buffer: \\[\\[elisp:(switch-to-buffer \"%s\")" (regexp-quote buffer-name)) nil t)
              (goto-char entry-start)
              (if (org-next-visible-heading 1)
                  (delete-region entry-start (point))
                (delete-region entry-start (point-max)))
              (setq found t))))
        (when found
          (write-region (point-min) (point-max) claude-code-taskmaster-org-file)
          t)))))

;;;###autoload
(defun claude-code-queue-next ()
  "Navigate to the next entry in the taskmaster.org queue."
  (interactive)
  (let* ((entries (claude-code--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "No entries in queue")
      (setq claude-code--queue-position (mod (1+ claude-code--queue-position) total))
      (let ((buffer-name (nth claude-code--queue-position entries)))
        (claude-code--switch-to-workspace-for-buffer buffer-name)
        (message "Queue position %d/%d: %s" (1+ claude-code--queue-position) total buffer-name)))))

;;;###autoload
(defun claude-code-queue-previous ()
  "Navigate to the previous entry in the taskmaster.org queue."
  (interactive)
  (let* ((entries (claude-code--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "No entries in queue")
      (setq claude-code--queue-position (mod (1- claude-code--queue-position) total))
      (let ((buffer-name (nth claude-code--queue-position entries)))
        (claude-code--switch-to-workspace-for-buffer buffer-name)
        (message "Queue position %d/%d: %s" (1+ claude-code--queue-position) total buffer-name)))))

;;;###autoload
(defun claude-code-queue-skip ()
  "Skip the current queue entry (delete it) and advance to the next."
  (interactive)
  (let* ((entries (claude-code--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "No entries in queue")
      (let ((current-buffer (nth claude-code--queue-position entries)))
        (if (claude-code--delete-queue-entry-for-buffer current-buffer)
            (progn
              (message "Skipped entry for %s" current-buffer)
              ;; Adjust position if we're at the end
              (let ((new-total (length (claude-code--get-all-queue-entries))))
                (when (>= claude-code--queue-position new-total)
                  (setq claude-code--queue-position (max 0 (1- new-total))))
                (if (zerop new-total)
                    (message "Queue is now empty")
                  (claude-code-queue-next))))
          (message "Failed to skip entry for %s" current-buffer))))))

;;;###autoload
(defun claude-code-queue-status ()
  "Show the current queue status."
  (interactive)
  (let* ((entries (claude-code--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "Queue is empty")
      (message "Queue: %d/%d entries, current: %s" 
               (1+ claude-code--queue-position) total 
               (nth claude-code--queue-position entries)))))

;;;###autoload
(defun claude-code-queue-browse ()
  "Browse and select from the taskmaster.org queue using minibuffer completion."
  (interactive)
  (let ((entries (claude-code--get-all-queue-entries)))
    (if (null entries)
        (message "Queue is empty")
      (let* ((choices (cl-loop for entry in entries
                               for i from 0
                               collect (cons (format "%d. %s" (1+ i) entry) entry)))
             (selection (completing-read "Select queue entry: " choices nil t))
             (selected-buffer (cdr (assoc selection choices))))
        (when selected-buffer
          ;; Update queue position to match selection
          (setq claude-code--queue-position (cl-position selected-buffer entries :test #'string=))
          ;; Switch to the selected buffer
          (claude-code--switch-to-workspace-for-buffer selected-buffer)
          (message "Switched to queue entry: %s" selected-buffer))))))

;;;; Queue Cleanup on Buffer Kill

(defun claude-code--cleanup-queue-entries ()
  "Remove taskmaster.org entries when Claude buffer is killed.

This function is added to `kill-buffer-hook' in Claude buffers to automatically
clean up queue entries when the buffer is no longer available."
  (let ((buffer-name (buffer-name)))
    (when (and buffer-name (string-match-p "^\\*claude:" buffer-name))
      (claude-code--delete-queue-entry-for-buffer buffer-name))))

;;;; Automatic Entry Clearing on RET

(defun claude-code--auto-clear-on-ret ()
  "Auto-clear taskmaster.org entry when user sends input.

This function is added to the RET key in Claude buffers to provide
seamless queue progression."
  (let ((buffer-name (buffer-name)))
    (when (string-match-p "^\\*claude:" buffer-name)
      (when (claude-code--delete-queue-entry-for-buffer buffer-name)
        (message "Auto-cleared queue entry for %s" buffer-name)))))

(defun claude-code--auto-advance-to-next ()
  "Clear current buffer from queue and advance to the next queue entry.

This function clears the current Claude buffer from the task queue and
automatically switches to the next available queue entry. If no more
entries exist, it displays a message."
  (let ((buffer-name (buffer-name)))
    (when (and claude-code-auto-advance-queue 
               (string-match-p "^\\*claude:" buffer-name))
      ;; Clear current buffer from queue
      (when (claude-code--delete-queue-entry-for-buffer buffer-name)
        (message "Cleared queue entry for %s" buffer-name)
        ;; Get remaining entries after clearing current one
        (let* ((remaining-entries (claude-code--get-all-queue-entries))
               ;; Filter out the current buffer from remaining entries (in case it wasn't properly cleared)
               (other-entries (cl-remove-if (lambda (buf-name) 
                                              (string= buf-name buffer-name))
                                            remaining-entries)))
          (if other-entries
              (progn
                ;; Reset queue position to 0 and advance to first different entry
                (setq claude-code--queue-position 0)
                (let ((next-buffer (nth claude-code--queue-position other-entries)))
                  (claude-code--switch-to-workspace-for-buffer next-buffer)
                  (message "Auto-advanced to next queue entry: %s (%d remaining)" 
                           next-buffer (length other-entries))))
            (message "Queue is now empty - no more entries to process")))))))

(defun claude-code--setup-auto-clear-hook ()
  "Set up automatic entry clearing for Claude buffers.
This function is added to `claude-code-start-hook' to enable automatic
queue progression when users respond to Claude."
  (when (string-match-p "^\\*claude:" (buffer-name))
    ;; Use pre-command-hook to detect when user is about to send input
    ;; This works better with terminal emulators than trying to override RET
    (add-hook 'pre-command-hook #'claude-code--check-for-input nil t)))

(defun claude-code--check-for-input ()
  "Check if user is sending input and auto-clear queue entry.
This runs on pre-command-hook in Claude buffers."
  (when (and (string-match-p "^\\*claude:" (buffer-name))
             ;; Check if this is likely an input command (RET, sending text, etc.)
             (or (eq this-command 'self-insert-command)
                 (eq this-command 'newline)
                 (eq this-command 'electric-newline-and-maybe-indent)
                 (string-match-p "return\\|newline\\|send" (symbol-name (or this-command 'unknown)))))
    ;; If auto-advance mode is enabled, use the advance function, otherwise just clear
    (if claude-code-auto-advance-queue
        (claude-code--auto-advance-to-next)
      (claude-code--auto-clear-on-ret))))

;; Add the hook to set up auto-clearing in Claude buffers
(add-hook 'claude-code-start-hook #'claude-code--setup-auto-clear-hook)

;;;###autoload
(defun claude-code-toggle-auto-advance-queue ()
  "Toggle auto-advance queue mode on or off.

When enabled, pressing enter in a Claude buffer will clear it from the
queue and automatically advance to the next queue entry."
  (interactive)
  (setq claude-code-auto-advance-queue (not claude-code-auto-advance-queue))
  (message "Claude Code auto-advance queue mode %s" 
           (if claude-code-auto-advance-queue "enabled" "disabled")))

;;;; Hook Integration Setup

;;;###autoload
(defun claude-code-org-notifications-setup ()
  "Set up org-mode notifications using the claude-code-event-hook system."
  (interactive)
  (add-hook 'claude-code-event-hook 'claude-code-org-notification-listener)
  (message "Claude Code org-mode notifications configured"))

;;;###autoload  
(defun claude-code-org-notifications-remove ()
  "Remove org-mode notification listener from claude-code-event-hook."
  (interactive)
  (remove-hook 'claude-code-event-hook 'claude-code-org-notification-listener)
  (message "Claude Code org-mode notifications removed"))

;;;; Integration

;; Configure display rule for notification buffer
(add-to-list 'display-buffer-alist
             '("^\\*Claude Code Notification\\*$"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.1)
               (select . nil)
               (quit-window . kill)))

(provide 'claude-code-org-notifications)

;;; claude-code-org-notifications.el ends here
