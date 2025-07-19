;;; test-auth.el --- Test authentication implementation -*- lexical-binding: t -*-

;; Load the MCP module
(add-to-list 'load-path default-directory)
(require 'claude-code-mcp)

;; Test UUID generation
(let ((uuid1 (claude-code-mcp--generate-uuid))
      (uuid2 (claude-code-mcp--generate-uuid)))
  (message "UUID 1: %s" uuid1)
  (message "UUID 2: %s" uuid2)
  (message "UUID format valid: %s" 
           (string-match-p "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}$" uuid1))
  (message "UUIDs are different: %s" (not (string= uuid1 uuid2))))

;; Test lockfile creation
(let* ((test-dir "/tmp/test-claude-ide")
       (test-port 12345)
       (auth-token (claude-code-mcp--generate-uuid)))
  ;; Create test directory
  (make-directory test-dir t)
  
  ;; Create lockfile
  (condition-case err
      (progn
        (claude-code-mcp--create-lockfile test-dir test-port auth-token)
        (let* ((lockfile-path (expand-file-name "~/.claude/ide/12345.lock"))
               (lockfile-exists (file-exists-p lockfile-path)))
          (message "Lockfile created: %s" lockfile-exists)
          (when lockfile-exists
            (let* ((content (with-temp-buffer
                              (insert-file-contents lockfile-path)
                              (buffer-string)))
                   (json-data (json-read-from-string content)))
              (message "Lockfile content: %S" json-data)
              (message "Has authToken: %s" (assoc 'authToken json-data))
              (message "Auth token matches: %s" 
                       (string= auth-token (cdr (assoc 'authToken json-data))))
              ;; Clean up
              (delete-file lockfile-path)))))
    (error (message "Error creating lockfile: %s" err))))

;;; test-auth.el ends here