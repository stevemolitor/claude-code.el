;;; test-auto-revert.el --- Test the auto-revert hook -*- lexical-binding: t; -*-

;;; Commentary:
;; Test file to demonstrate the auto-revert hook functionality.
;; Run this to set up and test the hook.

;;; Code:

(defun test-claude-auto-revert ()
  "Test the Claude auto-revert hook functionality."
  (interactive)
  
  ;; Load the auto-revert hook
  (load-file "claude-code-auto-revert-hook.el")
  
  ;; Set up the hook (safe mode)
  (setup-claude-auto-revert)
  
  ;; Enable debug messages
  (setq claude-code-auto-revert-debug t)
  
  ;; Create a test file
  (let ((test-file "/tmp/claude-auto-revert-test.txt"))
    (with-temp-file test-file
      (insert "Original content\n"))
    
    ;; Open the file in a buffer
    (find-file test-file)
    
    (message "Test file created and opened: %s" test-file)
    (message "Now ask Claude to edit this file and watch it auto-revert!")
    (message "Example: 'Please add a line saying Hello World to /tmp/claude-auto-revert-test.txt'")
    
    ;; Return the test file path for convenience
    test-file))

(defun simulate-claude-edit-event ()
  "Simulate a Claude edit event for testing."
  (interactive)
  
  ;; Create mock JSON data
  (let ((json-data "{\"name\": \"Edit\", \"params\": {\"file_path\": \"/tmp/claude-auto-revert-test.txt\"}}")
        (message-plist (list :type 'post-tool-use
                              :buffer-name "*claude-code*"
                              :json-data nil
                              :args nil)))
    
    ;; Update json-data in the plist
    (plist-put message-plist :json-data json-data)
    
    ;; Call the hook directly
    (claude-code-auto-revert-listener message-plist)
    
    (message "Simulated Claude edit event for /tmp/claude-auto-revert-test.txt")))

;;; test-auto-revert.el ends here