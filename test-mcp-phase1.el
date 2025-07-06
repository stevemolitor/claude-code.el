;;; test-mcp-phase1.el --- Test Phase 1 MCP implementation -*- lexical-binding: t; -*-

;; Test file for Phase 1 MCP implementation
;; Usage: M-x eval-buffer, then M-x claude-code-test-phase1

;;; Code:

(require 'claude-code-mcp)
(require 'claude-code-selection)
(require 'json)

(defvar claude-code-test-session nil
  "Test session for verification.")

(defvar claude-code-test-buffer nil
  "Test buffer for Claude terminal.")

(defun claude-code-test-phase1 ()
  "Test Phase 1 MCP implementation."
  (interactive)
  (message "=== Starting Phase 1 MCP Test ===")
  
  ;; Clean up any existing test
  (claude-code-test-cleanup)
  
  ;; Create test buffer
  (setq claude-code-test-buffer 
        (get-buffer-create "*claude-code-test*"))
  
  ;; Start MCP server
  (message "Starting MCP server...")
  (setq claude-code-test-session
        (claude-code--mcp-start 
         (expand-file-name default-directory)
         "test"
         claude-code-test-buffer))
  
  (if claude-code-test-session
      (progn
        (message "✓ MCP server started on port %d" 
                 (claude-code--mcp-session-port claude-code-test-session))
        
        ;; Check lock file
        (let ((lockfile (claude-code--mcp-lockfile-path claude-code-test-session)))
          (if (file-exists-p lockfile)
              (progn
                (message "✓ Lock file created: %s" lockfile)
                ;; Read and display lock file content
                (with-temp-buffer
                  (insert-file-contents lockfile)
                  (let ((content (json-read)))
                    (message "  PID: %s" (alist-get 'pid content))
                    (message "  IDE: %s" (alist-get 'ideName content))
                    (message "  Transport: %s" (alist-get 'transport content))
                    (message "  Workspace: %s" 
                             (aref (alist-get 'workspaceFolders content) 0)))))
            (message "✗ Lock file not created")))
        
        ;; Test selection tracking
        (message "\nTesting selection tracking...")
        (claude-code-test-selection-tracking)
        
        ;; Show connection instructions
        (message "\n=== WebSocket Connection Test ===")
        (message "To test WebSocket connection, run:")
        (message "  websocat -s mcp ws://127.0.0.1:%d" 
                 (claude-code--mcp-session-port claude-code-test-session))
        (message "\nThen send this initialize request:")
        (let ((json-str (json-encode 
                         '((jsonrpc . "2.0")
                           (id . 1)
                           (method . "initialize")
                           (params . ((protocolVersion . "2024-11-05")
                                      (capabilities . ((tools . ())))
                                      (clientInfo . ((name . "test")
                                                    (version . "1.0")))))))))
          (message "%s" json-str))
        (message "\nExpected response should include:")
        (message "  - protocolVersion: 2024-11-05")
        (message "  - serverInfo.name: claude-code-mcp")
        (message "\nRun M-x claude-code-test-cleanup when done testing"))
    (message "✗ Failed to start MCP server"))))

(defun claude-code-test-selection-tracking ()
  "Test selection tracking functionality."
  ;; Create a test file buffer
  (let ((test-file (expand-file-name "test-selection.txt" default-directory)))
    (with-current-buffer (find-file-noselect test-file)
      (erase-buffer)
      (insert "Line 1: Hello World\n")
      (insert "Line 2: Testing selection\n")
      (insert "Line 3: More content here\n")
      (save-buffer)
      
      ;; Test 1: Cursor position
      (goto-char (point-min))
      (message "  Moved cursor to beginning of buffer")
      
      ;; Test 2: Select some text
      (forward-word 2)
      (push-mark (point) t t)
      (forward-word 2)
      (message "  Selected text: %s" 
               (buffer-substring-no-properties (region-beginning) (region-end)))
      
      ;; Give time for debounced update
      (sit-for 0.1)
      
      ;; Clean up test file
      (kill-buffer)
      (delete-file test-file))))

(defun claude-code-test-cleanup ()
  "Clean up test session."
  (interactive)
  (when claude-code-test-session
    (message "Cleaning up test session...")
    (claude-code--mcp-stop claude-code-test-session)
    (setq claude-code-test-session nil)
    (message "✓ Test session cleaned up"))
  (when (and claude-code-test-buffer
             (buffer-live-p claude-code-test-buffer))
    (kill-buffer claude-code-test-buffer)
    (setq claude-code-test-buffer nil)))

(defun claude-code-test-send-json (json-string)
  "Send JSON-STRING to test session for debugging."
  (interactive "sJSON to send: ")
  (if (and claude-code-test-session
           (claude-code--mcp-session-client claude-code-test-session))
      (websocket-send-text 
       (claude-code--mcp-session-client claude-code-test-session)
       json-string)
    (message "No active WebSocket client connection")))

(provide 'test-mcp-phase1)
;;; test-mcp-phase1.el ends here