;;; claude-code-auto-revert-hook.el --- Git merge or simple revert hook -*- lexical-binding: t; -*-
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai

;;; Commentary:
;; Auto-revert hook with intelligent conflict resolution and auto-save.
;;
;; FEATURES:
;; - Auto-saves modified buffers before Claude edits them (prevents conflicts)
;; - Captures file state before Claude's changes for proper three-way merging
;; - Two revert strategies:
;;   - :git-merge - Use git merge to preserve both user and Claude changes  
;;   - :revert - Simple revert (lose user changes, preserve cursor position)
;;
;; USAGE:
;; (load-file "/path/to/claude-code-auto-revert-hook.el")
;; (setup-claude-auto-revert :git-merge t)  ; git-merge strategy, auto-save enabled
;;
;; The git-merge strategy performs a true three-way merge:
;; 1. Original file content (captured before any edits)
;; 2. User's changes (from Emacs buffer) 
;; 3. Claude's changes (written to file)
;; This allows preserving both sets of changes when possible.

;;; Code:

(require 'json)

(defvar claude-code--file-bases (make-hash-table :test 'equal)
  "Hash table storing original file contents before Claude edits.
Keys are file paths, values are the original contents.")

(defvar claude-code-revert-strategy :git-merge
  "Strategy for handling auto-revert.
Options:
  :git-merge - Use git merge to combine changes (may create conflicts)
  :revert    - Simple revert, preserve cursor, lose user changes")

(defvar claude-code-auto-save-before-edit t
  "Whether to automatically save modified buffers before Claude edits them.
When non-nil, any modified buffer for a file that Claude is about to edit
will be saved automatically. This prevents three-way merge conflicts by
ensuring the file on disk matches what's in the buffer before Claude's edit.

Set to nil if you prefer to handle saving manually or want to allow 
three-way merges between user changes, file state, and Claude's changes.")



(defun claude-code-auto-revert-pre-tool-listener (message)
  "Pre-tool-use hook to save file state before Claude edits.
Also auto-saves any open buffers for the target file to prevent conflicts.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (when (eq (plist-get message :type) 'pre-tool-use)
    (condition-case err
        (let* ((json-data (plist-get message :json-data))
               (parsed-data (when (and json-data (stringp json-data))
                              (condition-case parse-err
                                  (json-read-from-string json-data)
                                (error 
                                 (message "[Claude Pre-Hook] JSON parse error: %s" parse-err)
                                 nil))))
               (tool-name (when parsed-data (alist-get 'tool_name parsed-data)))
               (tool-input (when parsed-data (alist-get 'tool_input parsed-data))))
          
          (when (and tool-name (member tool-name '("Edit" "Write" "MultiEdit" "NotebookEdit" "Read" "Update")))
            (let ((file-path (or (alist-get 'file_path tool-input)
                                 (alist-get 'notebook_path tool-input))))
              
              
              (when (and file-path 
                         (not (string-match-p "hook" file-path)))
                
                ;; AUTO-SAVE: Save any open buffers for this file before Claude edits
                (when claude-code-auto-save-before-edit
                  (let ((target-buffer (find-buffer-visiting file-path)))
                    (when (and target-buffer (buffer-modified-p target-buffer))
                      (with-current-buffer target-buffer
                        (save-buffer)
                        (message "[Claude Auto-Save] Saved %s before Claude edit" file-path)))))
                
                ;; Store the base content (current file state after auto-save)
                (let ((base-content (if (file-exists-p file-path)
                                        (with-temp-buffer
                                          (insert-file-contents file-path)
                                          (buffer-string))
                                      ""))) ; Empty base for new files
                  (puthash file-path base-content claude-code--file-bases))))))
      
      (error
       (message "[Claude Pre-Hook] Error: %s" err)))))

(defun claude-code-auto-revert-post-tool-listener (message)
  "Auto-revert hook with git-merge or simple revert.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (when (eq (plist-get message :type) 'post-tool-use)
    (message "[Claude Post-Hook] Starting post-tool-use hook")
    (condition-case err
        (let* ((json-object (json-read-from-string (plist-get message :json-data)))
               (tool-name (cdr (assoc 'tool_name json-object)))
               (params (cdr (assoc 'tool_input json-object))))
          
          (message "[Claude Post-Hook] Tool: %s" tool-name)
          (when (member tool-name '("Edit" "Write" "MultiEdit" "NotebookEdit"))
            (let ((file-path (or (cdr (assoc 'file_path params))
                                 (cdr (assoc 'notebook_path params)))))
              
              (when (and file-path 
                         (not (string-match-p "hook" file-path)))
                
                (let ((target-buffer (find-buffer-visiting file-path)))
                  (when target-buffer
                    (message "[Claude Post-Hook] Found buffer for %s" file-path)
                    (with-current-buffer target-buffer
                      (let ((has-changes (buffer-modified-p)))
                        (message "[Claude Post-Hook] Buffer modified: %s" has-changes)
                        
                        (cond
                         ;; Use git-merge strategy when configured
                         ((equal claude-code-revert-strategy :git-merge)
                          (message "[Claude Post-Hook] Using git-merge strategy for %s" file-path)
                          (claude-code--auto-revert-git-merge file-path)
                          (message "[Claude Post-Hook] Git-merge completed and saved"))
                         
                         ;; Fall back to simple revert for :revert strategy
                         (t
                          (revert-buffer t t t)
                          (save-buffer)
                          (message "[Claude Revert] Simple revert and saved")))))))))))
      (error
       (message "[Claude Revert] Error: %s" err)))))

(defun claude-code--auto-revert-git-merge (file-path)
  "Perform git merge between buffer changes and file changes.
  
  ARCHITECTURE:
  1. User is editing buffer (may have unsaved changes)
  2. Claude edits the file on disk
  3. This function merges both sets of changes
  
  THE PROBLEM:
  - User's buffer: has user's changes
  - File on disk: has Claude's changes
  - We don't have the original state before either made changes
  
  THE SOLUTION:
  - Use git merge-file in a clever way
  - Treat user's buffer as the 'base' 
  - This makes git apply Claude's changes as a patch to user's buffer"
  
  (let* ((buffer-content (buffer-string))
         (base-content (gethash file-path claude-code--file-bases))
         (temp-base-file (make-temp-file "claude-base" nil ".tmp"))
         (temp-user-file (make-temp-file "claude-user" nil ".tmp"))
         (temp-claude-file (make-temp-file "claude-changes" nil ".tmp"))
         (temp-result-file (make-temp-file "claude-result" nil ".tmp")))
    
    
    (unwind-protect
        (progn
          ;; Write all three versions to temp files
          (with-temp-file temp-user-file
            (insert buffer-content))
          
          (with-temp-file temp-claude-file
            (insert-file-contents file-path))
          
          (if base-content
              (progn
                ;; We have the base! Do a proper three-way merge
                (with-temp-file temp-base-file
                  (insert base-content))
                
                ;; Copy user's file as starting point
                (copy-file temp-user-file temp-result-file t)
                
                (let ((exit-code (call-process "git" nil nil nil
                                               "merge-file"
                                               temp-result-file  ; Current (user's version)
                                               temp-base-file     ; Base (ORIGINAL!)
                                               temp-claude-file))) ; Other (Claude's version)

                  (process-merge-result temp-result-file file-path exit-code)))
            
            ;; No base content - fallback to simple revert
            (revert-buffer t t t)
            (save-buffer)
            (message "[Claude Revert] Simple revert (no base) and saved")))
      
      ;; Cleanup temp files
      (when (file-exists-p temp-base-file)
        (delete-file temp-base-file))
      (when (file-exists-p temp-user-file)
        (delete-file temp-user-file))
      (when (file-exists-p temp-claude-file)
        (delete-file temp-claude-file))
      (when (file-exists-p temp-result-file)
        (delete-file temp-result-file))
      ;; Clean up base after use
      (remhash file-path claude-code--file-bases))))

(defun process-merge-result (result-file file-path exit-code)
  "Process the merge result and update the buffer."
  ;; Create temp buffer with merge result
  (let ((temp-buffer (generate-new-buffer "*claude-merge-result*")))
    (unwind-protect
        (progn
          ;; Load merge result into temp buffer
          (with-current-buffer temp-buffer
            (insert-file-contents result-file))
          ;; Replace file buffer contents with temp buffer contents
          (with-current-buffer (find-buffer-visiting file-path)
            (message "[Claude Merge] About to replace-buffer-contents")
            ;; Update file modification time to prevent "changed on disc" warning
            (set-visited-file-modtime)
            (replace-buffer-contents temp-buffer)
            (message "[Claude Merge] Buffer contents replaced")
            ;; Mark as unmodified since we just synced with disk
            (save-buffer)
            (message "[Claude Merge] Buffer saved")))
      (kill-buffer temp-buffer)))
  

  ;; Report result
  (if (= exit-code 0)
      (message "[Claude Revert] Git merge successful and saved")
    (message "[Claude Revert] Git merge with conflicts and saved - exit code %d" exit-code)))

(defun setup-claude-auto-revert (&optional strategy auto-save)
  "Set up auto-revert hook with pre and post listeners.
STRATEGY can be :git-merge (default) or :revert.
AUTO-SAVE controls whether to auto-save buffers before Claude edits (default t).

Features:
- Auto-save modified buffers before Claude edits them (prevents conflicts)
- Store file state before Claude's changes for proper three-way merging
- Git merge user changes with Claude's changes using the original as base
- Preserve cursor position after reverting files"
  (interactive)
  (when strategy
    (setq claude-code-revert-strategy strategy))
  (when (not (eq auto-save 'unspecified))
    (setq claude-code-auto-save-before-edit (if auto-save auto-save t)))
  ;; Add both pre and post hooks
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-pre-tool-listener)
  (add-hook 'claude-code-event-hook 'claude-code-auto-revert-post-tool-listener)
  (message "Claude auto-revert configured: strategy=%s, auto-save=%s" 
           claude-code-revert-strategy claude-code-auto-save-before-edit))

(defun remove-claude-auto-revert ()
  "Remove both pre and post auto-revert hooks."
  (interactive)
  (remove-hook 'claude-code-event-hook 'claude-code-auto-revert-pre-tool-listener)
  (remove-hook 'claude-code-event-hook 'claude-code-auto-revert-post-tool-listener)
  (clrhash claude-code--file-bases)
  (message "Claude auto-revert hooks removed"))

(provide 'claude-code-auto-revert-hook)

;;; claude-code-auto-revert-hook.el ends here
