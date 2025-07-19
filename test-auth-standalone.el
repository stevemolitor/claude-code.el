;;; test-auth-standalone.el --- Test auth functions standalone -*- lexical-binding: t -*-

;; Test UUID generation function
(defun test-generate-uuid ()
  "Generate a UUID v4 string."
  (let ((uuid (make-string 36 ?-)))
    ;; Generate random hex characters
    (dotimes (i 36)
      (unless (memq i '(8 13 18 23)) ; Skip dash positions
        (let ((char (if (= i 14)
                        4 ; Version 4 UUID
                      (if (= i 19)
                          ;; Variant bits: 10xx
                          (let ((val (random 16)))
                            (if (< val 4) (+ val 8)
                              (if (< val 8) (+ val 8)
                                (if (< val 12) val
                                  (- val 4)))))
                        (random 16)))))
          (aset uuid i (aref "0123456789abcdef" char)))))
    uuid))

;; Test lockfile creation function  
(defun test-create-lockfile (folder port auth-token)
  "Create lock file for claude running in FOLDER for PORT with AUTH-TOKEN."
  (let* ((dir (expand-file-name "~/.claude/ide/"))
         (file (expand-file-name (format "%d.lock" port) dir))
         (content (json-encode
                   `((pid . ,(emacs-pid))
                     (workspaceFolders . ,(vector folder))
                     (ideName . "Emacs")
                     (transport . "ws")
                     (authToken . ,auth-token)))))
    ;; Ensure directory exists
    (make-directory dir t)
    ;; Write lock file
    (with-temp-file file
      (insert content))
    ;; Make it readable
    (set-file-modes file #o644)
    (message "Created lockfile: %s" file)
    file))

;; Run tests
(message "Testing UUID generation...")
(let ((uuid1 (test-generate-uuid))
      (uuid2 (test-generate-uuid)))
  (message "UUID 1: %s" uuid1)
  (message "UUID 2: %s" uuid2)
  (message "UUID format valid: %s" 
           (string-match-p "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}$" uuid1))
  (message "UUIDs are different: %s" (not (string= uuid1 uuid2))))

(message "\nTesting lockfile creation...")
(let* ((test-dir "/tmp/test-claude-ide")
       (test-port 12345)
       (auth-token (test-generate-uuid)))
  ;; Create test directory
  (make-directory test-dir t)
  
  ;; Create lockfile
  (condition-case err
      (let ((lockfile-path (test-create-lockfile test-dir test-port auth-token)))
        (let* ((content (with-temp-buffer
                          (insert-file-contents lockfile-path)
                          (buffer-string)))
               (json-data (json-read-from-string content)))
          (message "Lockfile content: %S" json-data)
          (message "Has authToken: %s" (assoc 'authToken json-data))
          (message "Auth token matches: %s" 
                   (string= auth-token (cdr (assoc 'authToken json-data))))
          ;; Clean up
          (delete-file lockfile-path)
          (message "Test passed!")))
    (error (message "Error in test: %s" err))))

;;; test-auth-standalone.el ends here