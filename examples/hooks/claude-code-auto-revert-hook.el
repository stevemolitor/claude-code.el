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

(defun claude-code-auto-revert-listener (message)
  "Auto-revert buffers when Claude edits files.
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
                  (when (and target-buffer (not (buffer-modified-p target-buffer)))
                    (with-current-buffer target-buffer
                      (revert-buffer t t t)
                      (message "[Claude Auto-Revert] Reverted %s" (buffer-name)))))))))
      
      (error
       (message "[Claude Auto-Revert] Error: %s" err)))))

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
