;;; mcp-tools.el --- Example MCP tools for claude-code.el -*- lexical-binding: t; -*-

;; Author: Claude Code Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai, mcp

;;; Commentary:
;; This file provides example MCP tools that can be exposed to Claude
;; through the claude-code.el MCP integration. These tools demonstrate
;; how to create functions that Claude can discover and execute.
;;
;; To use these tools:
;; 1. Load claude-code.el first to get the claude-code-defmcp macro
;; 2. Load this file: (load-file "examples/mcp-tools.el")  
;; 3. Start claude-code with MCP enabled
;; 4. Configure Claude Desktop to use the MCP server
;;
;; Functions defined with claude-code-defmcp are automatically discovered
;; and made available to Claude.

;;; Code:

;;;; Basic Examples

(claude-code-defmcp mcp-hello-world (&optional name)
  "Say hello to someone."
  :mcp-description "Greet someone with a friendly hello message"
  :mcp-schema '((name . ("string" "Name of person to greet")))
  (format "Hello, %s! 👋" (or name "World")))

;;;; Emacs Variable Access

(claude-code-defmcp mcp-get-variable-value (variable-names)
  "Get the current value of one or more Emacs variables."
  :mcp-description "Get the current value of one or more Emacs variables"
  :mcp-schema '((variable-names . ("array" "List of variable names to query")))
  (let ((results '()))
    (dolist (var-name variable-names)
      (let ((var-symbol (intern var-name)))
        (condition-case err
            (push (format "%s: %s" var-name (symbol-value var-symbol)) results)
          (error (push (format "%s: Error: %s" var-name (error-message-string err)) results)))))
    (mapconcat 'identity (reverse results) "\n")))

;;;; Org-Mode Integration

(claude-code-defmcp mcp-get-agenda (&optional agenda-type)
  "Get the org-agenda view and write it to /tmp/ClaudeWorkingFolder/agenda_<type>.txt."
  :mcp-description "Get org-agenda view and save to file for analysis"
  :mcp-schema '((agenda-type . ("string" "Agenda type (default: 'a')")))
  (let ((type (or agenda-type "a")))
    (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
      (make-directory "/tmp/ClaudeWorkingFolder" t))
    (let ((filename (format "/tmp/ClaudeWorkingFolder/agenda_%s.txt" type)))
      (save-window-excursion
        (let ((org-agenda-window-setup 'current-window))
          (org-agenda nil type)
          (with-current-buffer "*Org Agenda*"
            (write-file filename))))
      (format "Agenda content written to %s" filename))))

;;;; File Operations

(claude-code-defmcp mcp-open-file (file-paths)
  "Open files in Emacs and return buffer names."
  :mcp-description "Open one or more files in Emacs in the background"
  :mcp-schema '((file-paths . ("array" "List of file paths to open")))
  (let ((results '()))
    (dolist (file-path file-paths)
      (condition-case err
          (let ((buffer (find-file-noselect file-path)))
            (push (format "%s -> %s" file-path (buffer-name buffer)) results))
        (error (push (format "%s -> Error: %s" file-path (error-message-string err)) results))))
    (format "Files opened:\n%s" (mapconcat 'identity (reverse results) "\n"))))

;;;; Emacs Search and Introspection

(claude-code-defmcp mcp-emacs-search (pattern &optional type predicate)
  "Search for Emacs symbols, commands, or variables matching a pattern."
  :mcp-description "Search for Emacs symbols using apropos functions"
  :mcp-schema '((pattern . ("string" "Pattern to search for"))
                (type . ("string" "Type: all, commands, variables, functions"))
                (predicate . ("string" "Optional predicate for filtering")))
  (let ((search-type (or type "all"))
        (result ""))
    (cond
     ((string= search-type "commands")
      (setq result (format "=== COMMANDS MATCHING '%s' ===\n" pattern))
      (dolist (sym (apropos-internal pattern 'commandp))
        (when (commandp sym)
          (setq result (concat result (symbol-name sym) "\n")))))
     ((string= search-type "variables")
      (setq result (format "=== VARIABLES MATCHING '%s' ===\n" pattern))
      (dolist (sym (apropos-internal pattern))
        (when (boundp sym)
          (setq result (concat result (symbol-name sym) "\n")))))
     ((string= search-type "functions")
      (setq result (format "=== FUNCTIONS MATCHING '%s' ===\n" pattern))
      (dolist (sym (apropos-internal pattern 'fboundp))
        (when (fboundp sym)
          (setq result (concat result (symbol-name sym) "\n")))))
     (t
      (setq result (format "=== ALL SYMBOLS MATCHING '%s' ===\n" pattern))
      (let ((pred-func (when (and predicate (not (string-empty-p predicate)))
                        (intern predicate))))
        (dolist (sym (apropos-internal pattern pred-func))
          (let ((types '()))
            (when (fboundp sym) (push "function" types))
            (when (boundp sym) (push "variable" types))
            (when (commandp sym) (push "command" types))
            (setq result (concat result (symbol-name sym)
                               (if types (concat " (" (mapconcat 'identity types ", ") ")") "")
                               "\n")))))))
    result))

(claude-code-defmcp mcp-emacs-describe (symbol-names &optional type)
  "Get comprehensive documentation for Emacs symbols."
  :mcp-description "Get detailed documentation for one or more Emacs symbols"
  :mcp-schema '((symbol-names . ("array" "List of symbol names to describe"))
                (type . ("string" "Type: function, variable, or symbol")))
  (let ((desc-type (or type "symbol"))
        (results '()))
    (dolist (symbol-name symbol-names)
      (let ((sym (intern symbol-name))
            (desc ""))
        (setq desc (format "=== SYMBOL: %s ===\n" symbol-name))
        (cond
         ((string= desc-type "function")
          (if (fboundp sym)
              (setq desc (concat desc (save-window-excursion
                                       (with-temp-buffer
                                         (describe-function sym)
                                         (with-current-buffer "*Help*"
                                           (buffer-string))))))
            (setq desc (concat desc "Symbol is not a function"))))
         ((string= desc-type "variable")
          (if (boundp sym)
              (setq desc (concat desc (save-window-excursion
                                      (with-temp-buffer
                                        (describe-variable sym)
                                        (with-current-buffer "*Help*"
                                          (buffer-string))))))
            (setq desc (concat desc "Symbol is not a variable"))))
         (t
          (when (fboundp sym)
            (setq desc (concat desc "=== FUNCTION ===\n"))
            (setq desc (concat desc (save-window-excursion
                                    (with-temp-buffer
                                      (describe-function sym)
                                      (with-current-buffer "*Help*"
                                        (buffer-string)))) "\n\n")))
          (when (boundp sym)
            (setq desc (concat desc "=== VARIABLE ===\n"))
            (setq desc (concat desc (save-window-excursion
                                    (with-temp-buffer
                                      (describe-variable sym)
                                      (with-current-buffer "*Help*"
                                        (buffer-string)))))))
          (when (and (not (fboundp sym)) (not (boundp sym)))
            (setq desc (concat desc "Symbol not found as function or variable")))))
        (push desc results)))
    (mapconcat 'identity (reverse results) "\n\n")))

;;;; Buffer Operations

(claude-code-defmcp mcp-emacs-buffer-info (buffer-names &optional include-content include-variables)
  "Get comprehensive buffer information and write to files."
  :mcp-description "Get buffer info including content, mode details, and variables"
  :mcp-schema '((buffer-names . ("array" "List of buffer names to analyze"))
                (include-content . ("boolean" "Include buffer content"))
                (include-variables . ("boolean" "Include key variables")))
  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
    (make-directory "/tmp/ClaudeWorkingFolder" t))
  
  (let ((successful-files '())
        (with-content (if include-content include-content t))
        (with-vars (if include-variables include-variables t)))
    (dolist (buffer-name buffer-names)
      (condition-case err
          (with-current-buffer buffer-name
            (let ((info "")
                  (sanitized-name (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))
                  (filename (format "/tmp/ClaudeWorkingFolder/buffer_info_%s.txt" 
                                   (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))))
              
              (setq info (concat info (format "=== BUFFER INFO: %s ===\n" (buffer-name))))
              (setq info (concat info (format "File: %s\n" (or (buffer-file-name) "no file"))))
              (setq info (concat info (format "Major mode: %s\n" (symbol-name major-mode))))
              (setq info (concat info (format "Buffer size: %d characters\n" (buffer-size))))
              (setq info (concat info (format "Modified: %s\n\n" (if (buffer-modified-p) "yes" "no"))))
              
              (when with-vars
                (setq info (concat info "=== KEY VARIABLES ===\n"))
                (setq info (concat info (format "default-directory: %s\n" default-directory)))
                (setq info (concat info (format "tab-width: %s\n" tab-width)))
                (setq info (concat info (format "fill-column: %s\n" fill-column)))
                (setq info (concat info (format "buffer-read-only: %s\n\n" (if buffer-read-only "yes" "no")))))
              
              (when with-content
                (setq info (concat info "=== BUFFER CONTENT ===\n"))
                (let ((lines (split-string (buffer-string) "\n")))
                  (setq info (concat info (mapconcat 
                                          (lambda (line)
                                            (format "%4d→%s" (1+ (cl-position line lines :test 'equal)) line))
                                          lines "\n")))))
              
              (write-region info nil filename)
              (push filename successful-files)))
        (error
         (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))
    
    (format "Buffer info written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))

(claude-code-defmcp mcp-check-parens (file-paths)
  "Run check-parens on Lisp files to validate parentheses balance."
  :mcp-description "Validate parentheses balance in Lisp code files"
  :mcp-schema '((file-paths . ("array" "List of file paths to check")))
  (let ((results '()))
    (dolist (file-path file-paths)
      (let ((temp-buffer (find-file-noselect file-path)))
        (unwind-protect
            (with-current-buffer temp-buffer
              (condition-case err
                  (progn
                    (check-parens)
                    (push (format "%s: Parentheses are balanced correctly" file-path) results))
                (error 
                 (push (format "%s: Parentheses error at line %d, column %d: %s" 
                              file-path
                              (line-number-at-pos (point))
                              (current-column)
                              (error-message-string err)) results))))
          (kill-buffer temp-buffer))))
    (mapconcat 'identity (reverse results) "\n")))

(claude-code-defmcp mcp-get-buffer-list (&optional include-details)
  "Get a list of all live buffers in Emacs."
  :mcp-description "Get list of all buffers with optional details"
  :mcp-schema '((include-details . ("boolean" "Include buffer details like file, size, modification status")))
  (let ((buffers (buffer-list))
        (result "=== BUFFER LIST ===\n")
        (with-details (or include-details nil)))
    (if with-details
        (progn
          (setq result "=== BUFFER LIST WITH DETAILS ===\n")
          (dolist (buf buffers)
            (with-current-buffer buf
              (setq result (concat result
                                  (format "Buffer: %s\n" (buffer-name))
                                  (format "  File: %s\n" (or (buffer-file-name) "no file"))
                                  (format "  Size: %d characters\n" (buffer-size))
                                  (format "  Modified: %s\n" (if (buffer-modified-p) "yes" "no"))
                                  (format "  Major mode: %s\n\n" (symbol-name major-mode)))))))
      (dolist (buf buffers)
        (with-current-buffer buf
          (setq result (concat result
                              (format "%s%s\n"
                                     (buffer-name)
                                     (if (buffer-file-name)
                                         (format " (%s)" (buffer-file-name))
                                       "")))))))
    result))

;;;; Workspace Management

(claude-code-defmcp mcp-get-workspace-buffers (&optional workspace-name)
  "Get buffers in workspaces and write to file."
  :mcp-description "Get list of buffers in each workspace"
  :mcp-schema '((workspace-name . ("string" "Specific workspace name (optional)")))
  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
    (make-directory "/tmp/ClaudeWorkingFolder" t))
  
  (let ((filename "/tmp/ClaudeWorkingFolder/workspace_buffers.txt")
        (result ""))
    (cond
     ;; Check for workspace systems
     ((and (fboundp '+workspace-buffer-list) (fboundp '+workspace-list-names))
      (if workspace-name
          (condition-case err
              (let* ((workspace-names (+workspace-list-names))
                     (current-workspace (+workspace-current-name)))
                (if (member workspace-name workspace-names)
                    (progn
                      (+workspace-switch workspace-name t)
                      (setq result (format "=== WORKSPACE: %s ===\n" workspace-name))
                      (let ((buffers (mapcar 'buffer-name (+workspace-buffer-list))))
                        (dolist (buf buffers)
                          (setq result (concat result buf "\n"))))
                      (unless (string= current-workspace workspace-name)
                        (+workspace-switch current-workspace t)))
                  (setq result (format "Workspace '%s' not found. Available: %s\n" 
                                      workspace-name 
                                      (mapconcat 'identity workspace-names ", ")))))
            (error (setq result (format "Error accessing workspace: %s\n" (error-message-string err)))))
        ;; Get all workspaces
        (condition-case err
            (let* ((workspace-names (+workspace-list-names))
                   (current-workspace (+workspace-current-name)))
              (setq result "=== ALL WORKSPACE BUFFERS ===\n")
              (dolist (ws-name workspace-names)
                (setq result (concat result (format "\n=== WORKSPACE: %s ===\n" ws-name)))
                (+workspace-switch ws-name t)
                (let ((buffers (mapcar 'buffer-name (+workspace-buffer-list))))
                  (dolist (buf buffers)
                    (setq result (concat result buf "\n")))))
              (+workspace-switch current-workspace t))
          (error (setq result (format "Error accessing workspaces: %s\n" (error-message-string err)))))))
     (t
      (setq result "No supported workspace system found - showing current buffer list\n")
      (let ((buffers (mapcar 'buffer-name (buffer-list))))
        (dolist (buf buffers)
          (setq result (concat result buf "\n"))))))
    
    (write-region result nil filename)
    (format "Workspace buffers written to %s" filename)))

(claude-code-defmcp mcp-view-buffer (buffer-names)
  "Get buffer contents and write each to separate files."
  :mcp-description "View buffer contents by writing to individual files"
  :mcp-schema '((buffer-names . ("array" "List of buffer names to view")))
  (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
    (make-directory "/tmp/ClaudeWorkingFolder" t))
  
  (let ((successful-files '()))
    (dolist (buffer-name buffer-names)
      (condition-case err
          (with-current-buffer buffer-name
            (let* ((sanitized-name (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))
                   (filename (format "/tmp/ClaudeWorkingFolder/%s.txt" sanitized-name))
                   (content (buffer-string))
                   (lines (split-string content "\n")))
              (write-region (mapconcat (lambda (line)
                                        (format "%4d→%s" (1+ (cl-position line lines :test 'equal)) line))
                                      lines "\n")
                           nil filename)
              (push filename successful-files)))
        (error
         (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))
    
    (format "Buffer contents written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))

(provide 'mcp-tools)

;;; mcp-tools.el ends here