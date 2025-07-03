;;; test-ws-simple.el --- Simple test for WebSocket server -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal test to verify WebSocket server loads and starts.

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load the WebSocket server
(condition-case err
    (require 'claude-code-ws)
  (error (message "Error loading claude-code-ws: %s" (error-message-string err))))

;; Simple test function
(defun test-ws-basic ()
  "Basic WebSocket server test."
  (interactive)
  (message "Creating WebSocket server...")
  (condition-case err
      (let ((server (claude-code-ws-create-server
                     :port 12345
                     :on-open (lambda (client)
                                (message "Client connected!"))
                     :on-message (lambda (client msg)
                                   (message "Received: %s" msg)
                                   (claude-code-ws-send client (concat "Echo: " msg)))
                     :on-close (lambda (client code reason)
                                 (message "Client disconnected: %d %s" code reason))
                     :on-error (lambda (client error)
                                 (message "Error: %s" error)))))
        (if server
            (progn
              (message "Server created successfully on port 12345")
              (message "Test with: websocat ws://127.0.0.1:12345")
              server)
          (message "Failed to create server")))
    (error (message "Error creating server: %s" (error-message-string err)))))

;; Run the test
(test-ws-basic)

(provide 'test-ws-simple)
;;; test-ws-simple.el ends here