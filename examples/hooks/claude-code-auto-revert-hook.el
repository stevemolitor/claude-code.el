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
;;
;; Usage:
;;   (load-file "claude-code-auto-revert-hook.el")
;;   (setup-claude-auto-revert)
;;
;; Then configure PostToolUse hook in ~/.claude/settings.json:
;;   {
;;     "hooks": {
;;       "PostToolUse": [
;;         {
;;           "matcher": "",
;;           "hooks": [
;;             {
;;               "type": "command",
;;               "command": "emacsclient --eval \"(claude-code-handle-hook 'post-tool-use \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
;;             }
;;           ]
;;         }
;;       ]
;;     }
;;   }

;;; Code:

(require 'json)

(defvar claude-code--file-original-content (make-hash-table :test 'equal)
  "Hash table to store original file content for git merge.")

(defun claude-code-auto-revert-listener (message)
  "Auto-revert buffers when Claude edits files using git merge for conflict resolution.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (when (eq (plist-get message :type) 'post-tool-use)
    (condition-case err
        (let* ((json-object (json-read-from-string (plist-get message :json-data)))
               (tool-name (cdr (assoc 'tool_name json-object)))
               (params (cdr (assoc 'tool_input json-object))))
          
          (when (member tool-name '("Edit" "Write" "MultiEdit" "NotebookEdit"))
            (let ((file-path (or (cdr (assoc 'file_path params))
                                 (cdr (assoc 'notebook_path params)))))
              
              (when file-path
                (let ((target-buffer (find-buffer-visiting file-path)))
                  (when target-buffer
                    (with-current-buffer target-buffer
                      (cond
                       ;; Buffer not modified - safe to revert
                       ((not (buffer-modified-p))
                        (claude-code--store-original-content file-path)
                        (revert-buffer t t t)
                        (message "[Claude Auto-Revert] Reverted %s" (buffer-name)))
                       
                       ;; Buffer modified - use git merge (don't save first!)
                       (t
                        (claude-code--store-original-content file-path)
                        (claude-code--git-merge-and-update file-path))))))))))
      
      (error
       (message "[Claude Auto-Revert] Error: %s" err)))))

(defun claude-code--store-original-content (file-path)
  "Store the original content of file before Claude's changes."
  (unless (gethash file-path claude-code--file-original-content)
    (when-let ((buffer (find-buffer-visiting file-path)))
      (with-current-buffer buffer
        (puthash file-path (buffer-string) claude-code--file-original-content)))))

(defun claude-code--git-merge-and-update (file-path)
  "Use git merge-file to intelligently merge buffer and file changes."
  (let* ((buffer-content (buffer-string))
         (original-content (gethash file-path claude-code--file-original-content))
         (temp-buffer-file (make-temp-file "claude-buffer" nil ".tmp"))
         (temp-original-file (make-temp-file "claude-original" nil ".tmp"))
         (temp-result-file (make-temp-file "claude-result" nil ".tmp")))
    
    (unwind-protect
        (progn
          ;; Write buffer content to temp file
          (with-temp-file temp-buffer-file
            (insert buffer-content))
          
          ;; Write original content to temp file (or current file if no original stored)
          (with-temp-file temp-original-file
            (insert (or original-content buffer-content)))
          
          ;; Copy current file to result temp file
          (copy-file file-path temp-result-file t)
          
          ;; Try git merge-file: merge buffer changes with Claude's changes
          (let* ((exit-code (call-process "git" nil nil nil
                                        "merge-file" 
                                        temp-buffer-file
                                        temp-original-file  
                                        temp-result-file)))
            (cond
             ;; Clean merge (exit code 0) - use merged result
             ((= exit-code 0)
              (erase-buffer)
              (insert-file-contents temp-buffer-file)
              (set-buffer-modified-p nil)
              (message "[Claude Auto-Revert] Git merge successful in %s" (buffer-name)))
             
             ;; Merge with conflicts (exit code 1) - show merged result with conflict markers
             ((= exit-code 1)
              (erase-buffer)
              (insert-file-contents temp-buffer-file)
              (set-buffer-modified-p t)
              (message "[Claude Auto-Revert] Git merge with conflicts in %s - resolve manually" (buffer-name)))
             
             ;; Git merge failed - keep original buffer
             (t
              (message "[Claude Auto-Revert] Git merge failed for %s (exit code %d) - keeping your changes" 
                       (buffer-name) exit-code)))))
      
      ;; Cleanup temp files
      (dolist (temp-file (list temp-buffer-file temp-original-file temp-result-file))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(defun claude-code--has-conflict-markers ()
  "Check if buffer contains git conflict markers."
  (save-excursion
    (goto-char (point-min))
    (or (search-forward "<<<<<<< " nil t)
        (search-forward "=======" nil t)
        (search-forward ">>>>>>> " nil t))))

(defun setup-claude-auto-revert ()
  "Set up automatic buffer reverting after Claude edits."
  (interactive)
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-listener)
  (message "Claude auto-revert hook configured"))

(defun remove-claude-auto-revert ()
  "Remove the auto-revert hook."
  (interactive)
  (remove-hook 'claude-code-event-hook 'claude-code-auto-revert-listener)
  (message "Claude auto-revert hook removed"))

;; For Org babel files, where cells are running, you may also want to enable
;; (add-hook 'org-babel-after-execute-hook (lambda () (save-buffer) ))
;; To Auto-save and redisplay images after org-babel execution

(provide 'claude-code-auto-revert-hook)

;;; claude-code-auto-revert-hook.el ends here
