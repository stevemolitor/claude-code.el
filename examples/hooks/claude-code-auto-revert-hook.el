;;; claude-code-auto-revert-hook.el --- Auto-revert buffers after Claude edits -*- lexical-binding: t; -*-

;; Author: Claude Code Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai

;;; Commentary:
;; This hook automatically reverts buffers when Claude makes changes to files.
;; It listens for post-tool-use events from Edit, Write, and MultiEdit tools
;; and automatically reverts the corresponding buffers if they're open.
;;
;; This solves the issue where changes made by Claude don't automatically
;; appear in Emacs buffers, requiring manual revert-buffer calls.

;;; Code:

(require 'json)

(defvar claude-code-auto-revert-debug nil
  "When non-nil, enable debug messages for auto-revert hook.")

(defun claude-code-auto-revert-listener (message)
  "Auto-revert buffers when Claude edits files.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data)))
    
    (when (eq hook-type 'post-tool-use)
      ;; Parse the JSON data to get tool information
      (condition-case err
          (let* ((json-object (json-read-from-string json-data))
                 (tool-name (cdr (assoc 'tool_name json-object)))
                 (params (cdr (assoc 'tool_input json-object))))
            
            (when claude-code-auto-revert-debug
              (message "[Claude Auto-Revert] Tool: %s, Params: %S" tool-name params))
            
            ;; Check if it's an editing tool
            (when (member tool-name '("Edit" "Write" "MultiEdit" "NotebookEdit"))
              (let ((file-path (or (cdr (assoc 'file_path params))
                                   (cdr (assoc 'notebook_path params)))))
                
                (when file-path
                  ;; Find buffer for this file
                  (let ((target-buffer (find-buffer-visiting file-path)))
                    (if target-buffer
                        (with-current-buffer target-buffer
                          ;; Check if buffer has unsaved changes
                          (if (buffer-modified-p)
                              (when claude-code-auto-revert-debug
                                (message "[Claude Auto-Revert] Buffer %s has unsaved changes, skipping revert" 
                                         (buffer-name)))
                            ;; Revert the buffer
                            (revert-buffer t t t)
                            (message "[Claude Auto-Revert] Reverted %s after Claude edit" 
                                     (buffer-name))))
                      (when claude-code-auto-revert-debug
                        (message "[Claude Auto-Revert] No buffer visiting %s" file-path))))))))
        
        (error
         (message "[Claude Auto-Revert] Error processing hook data: %s" err))))))

(defun claude-code-auto-revert-aggressive-listener (message)
  "Auto-revert buffers even with unsaved changes (asks for confirmation).
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys.
This is a more aggressive version that will prompt to revert even modified buffers."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data)))
    
    (when (eq hook-type 'post-tool-use)
      (condition-case err
          (let* ((json-object (json-read-from-string json-data))
                 (tool-name (cdr (assoc 'tool_name json-object)))
                 (params (cdr (assoc 'tool_input json-object))))
            
            (when (member tool-name '("Edit" "Write" "MultiEdit" "NotebookEdit"))
              (let ((file-path (or (cdr (assoc 'file_path params))
                                   (cdr (assoc 'notebook_path params)))))
                
                (when file-path
                  (let ((target-buffer (find-buffer-visiting file-path)))
                    (when target-buffer
                      (with-current-buffer target-buffer
                        (if (buffer-modified-p)
                            ;; Ask user if they want to revert modified buffer
                            (when (y-or-n-p (format "Buffer %s has unsaved changes. Revert to Claude's version? " 
                                                    (buffer-name)))
                              (revert-buffer t t t)
                              (message "[Claude Auto-Revert] Reverted %s (discarded local changes)" 
                                       (buffer-name)))
                          ;; No modifications, just revert
                          (revert-buffer t t t)
                          (message "[Claude Auto-Revert] Reverted %s after Claude edit" 
                                   (buffer-name))))))))))
        
        (error
         (message "[Claude Auto-Revert] Error: %s" err))))))

(defun claude-code-auto-revert-org-listener (message)
  "Special handler for org-mode files that preserves folding state.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (json-data (plist-get message :json-data)))
    
    (when (eq hook-type 'post-tool-use)
      (condition-case err
          (let* ((json-object (json-read-from-string json-data))
                 (tool-name (cdr (assoc 'tool_name json-object)))
                 (params (cdr (assoc 'tool_input json-object))))
            
            (when (member tool-name '("Edit" "Write" "MultiEdit"))
              (let ((file-path (cdr (assoc 'file_path params))))
                
                (when (and file-path 
                           (string-match-p "\\.org$" file-path))
                  (let ((target-buffer (find-buffer-visiting file-path)))
                    (when target-buffer
                      (with-current-buffer target-buffer
                        (unless (buffer-modified-p)
                          ;; Save org folding state
                          (let ((point-pos (point))
                                (window-start-pos (window-start)))
                            ;; Revert the buffer
                            (revert-buffer t t t)
                            ;; Restore position
                            (goto-char point-pos)
                            (set-window-start (selected-window) window-start-pos)
                            (message "[Claude Auto-Revert] Reverted org file %s" 
                                     (buffer-name)))))))))))
        
        (error
         (message "[Claude Auto-Revert] Error in org handler: %s" err))))))

;;;; Setup Functions

(defun setup-claude-auto-revert ()
  "Set up automatic buffer reverting after Claude edits.
This version skips buffers with unsaved changes."
  (interactive)
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-listener)
  (message "Claude auto-revert hook configured (safe mode)"))

(defun setup-claude-auto-revert-aggressive ()
  "Set up aggressive auto-revert that prompts for modified buffers."
  (interactive)
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-aggressive-listener)
  (message "Claude auto-revert hook configured (aggressive mode)"))

(defun setup-claude-auto-revert-org ()
  "Set up auto-revert specifically for org-mode files."
  (interactive)
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-org-listener)
  ;; Auto-save and redisplay images after org-babel execution
  (add-hook 'org-babel-after-execute-hook (lambda () (save-buffer) (org-redisplay-inline-images)))
  (message "Claude auto-revert hook configured for org files"))

(defun setup-claude-auto-revert-all ()
  "Set up both standard and org-specific auto-revert handlers."
  (interactive)
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-listener)
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-org-listener)
  (message "Claude auto-revert hooks configured for all file types"))

(defun remove-claude-auto-revert-hooks ()
  "Remove all auto-revert hook handlers."
  (interactive)
  (remove-hook 'claude-code-event-hook 'claude-code-auto-revert-listener)
  (remove-hook 'claude-code-event-hook 'claude-code-auto-revert-aggressive-listener)
  (remove-hook 'claude-code-event-hook 'claude-code-auto-revert-org-listener)
  (message "Claude auto-revert hooks removed"))

;;;; Usage Instructions
;;
;; To use this auto-revert hook:
;;
;; 1. Load this file: 
;;    (load-file "claude-code-auto-revert-hook.el")
;;
;; 2. Choose your setup:
;;    - Safe mode (skips modified buffers): (setup-claude-auto-revert)
;;    - Aggressive mode (prompts for modified): (setup-claude-auto-revert-aggressive)
;;    - Org-mode only: (setup-claude-auto-revert-org)
;;    - All modes: (setup-claude-auto-revert-all)
;;
;; 3. Configure Claude Code CLI hooks in ~/.claude/settings.json:
;;
;;    {
;;      "hooks": {
;;        "PostToolUse": [
;;          {
;;            "matcher": "",
;;            "hooks": [
;;              {
;;                "type": "command",
;;                "command": "emacsclient --eval \"(claude-code-handle-hook 'post-tool-use \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
;;              }
;;            ]
;;          }
;;        ]
;;      }
;;    }
;;
;; 4. Optional: Enable debug messages
;;    (setq claude-code-auto-revert-debug t)
;;
;; The hook will automatically revert buffers when Claude uses the Edit, Write,
;; MultiEdit, or NotebookEdit tools on files that are open in Emacs.

(provide 'claude-code-auto-revert-hook)

;;; claude-code-auto-revert-hook.el ends here