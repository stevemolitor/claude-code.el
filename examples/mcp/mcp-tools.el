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
;; 2. Load this file: (load-file "examples/mcp/mcp-tools.el")  
;; 3. Start claude-code with MCP enabled
;; 4. Configure Claude Desktop to use the MCP server
;;
;; Functions defined with claude-code-defmcp are automatically discovered
;; and made available to Claude.

;;; Code:

;;;; Configuration Variables

(defcustom claude-code-mcp-blocked-buffer-patterns
  '("password" ".pem" "secret" ".key" "token" "credential" "auth" ".ssh")
  "List of patterns that will block buffer access in MCP tools.
Buffer names or file paths containing these patterns will be blocked
from access through MCP tools for security."
  :type '(repeat string)
  :group 'claude-code)

;;;; Security Functions  

(defun claude-code-mcp-buffer-blocked-p (buffer-name)
  "Check if a buffer should be blocked based on name or file path.
Returns t if BUFFER-NAME or its associated file path contains any
pattern from `claude-code-mcp-blocked-buffer-patterns'."
  (when (and buffer-name claude-code-mcp-blocked-buffer-patterns)
    (let ((buffer-obj (get-buffer buffer-name))
          (blocked nil))
      ;; Check buffer name against patterns
      (dolist (pattern claude-code-mcp-blocked-buffer-patterns)
        (when (string-match-p (regexp-quote pattern) buffer-name)
          (setq blocked t)))
      ;; If buffer exists, also check its file path
      (when (and buffer-obj (not blocked))
        (let ((file-path (buffer-file-name buffer-obj)))
          (when file-path
            (dolist (pattern claude-code-mcp-blocked-buffer-patterns)
              (when (string-match-p (regexp-quote pattern) file-path)
                (setq blocked t))))))
      blocked)))

;;;; Basic Utilities

(claude-code-defmcp mcp-hello-world (name)
                    "Greet someone with a friendly hello message."
                    :mcp-description "Greet someone with a friendly hello message"
                    :mcp-schema '((name . ("string" "Name of person to greet")))
                    (format "Hello, %s! 👋" name))

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
                      (let ((filename "/tmp/ClaudeWorkingFolder/agenda.txt"))
                        (save-window-excursion
                          (let ((org-agenda-window-setup 'current-window))
                            (org-agenda nil type)
                            (with-current-buffer "*Org Agenda*"
                              (write-region (point-min) (point-max) filename)))
                          (format "Agenda content written to %s" filename)))))

(claude-code-defmcp mcp-org-agenda-todo-batch (batch-updates &optional agenda-type)
                    "Change the state of multiple agenda items in batch."
                    :mcp-description "Change the state of multiple agenda items in batch"
                    :mcp-schema '((batch-updates . ("array" "List of [line-number, new-state] pairs"))
                                  (agenda-type . ("string" "Agenda type to work with (default 'a')")))
                    (unless agenda-type (setq agenda-type "a"))
                    (save-window-excursion
                      (let ((org-agenda-window-setup 'current-window)
                            (processed-items '())
                            (failed-items '()))
                        (org-agenda nil agenda-type)
                        (with-current-buffer "*Org Agenda*"
                          ;; Process each update
                          (dolist (update batch-updates)
                            (let ((line-num (car update))
                                  (new-state (cadr update)))
                              (condition-case err
                                  (progn
                                    (goto-char (point-min))
                                    (forward-line (1- line-num))
                                    (if (org-agenda-check-type nil 'agenda 'todo 'tags 'search)
                                        (progn
                                          (if new-state
                                              (org-agenda-todo new-state)
                                            (org-agenda-todo))
                                          (push (format "Line %d: %s" line-num (or new-state "cycled")) processed-items))
                                      (push (format "Line %d: No valid agenda item" line-num) failed-items)))
                                (error
                                 (push (format "Line %d: %s" line-num (error-message-string err)) failed-items)))))
                          
                          ;; Save all changes at once
                          (org-save-all-org-buffers)
                          ;; Refresh the agenda to keep line numbers in sync
                          (org-agenda-redo t)
                          ;; Regenerate the single agenda file
                          (write-region (point-min) (point-max) "/tmp/ClaudeWorkingFolder/agenda.txt")
                          
                          ;; Return summary
                          (let ((success-count (length processed-items))
                                (failure-count (length failed-items)))
                            (format "Batch update complete: %d successful, %d failed\nSuccessful: %s\nFailed: %s"
                                    success-count failure-count
                                    (if processed-items (mapconcat 'identity (reverse processed-items) ", ") "none")
                                    (if failed-items (mapconcat 'identity (reverse failed-items) ", ") "none")))))))

(claude-code-defmcp mcp-org-schedule-todo (org-file heading-text schedule-date &optional remove-schedule)
                    "Schedule a TODO item by adding SCHEDULED property."
                    :mcp-description "Schedule a TODO item by adding SCHEDULED property"
                    :mcp-schema '((org-file . ("string" "Path to the org file containing the heading"))
                                  (heading-text . ("string" "Text of the heading to schedule"))
                                  (schedule-date . ("string" "Date/time to schedule (e.g., '2025-01-15', 'today', '+1d')"))
                                  (remove-schedule . ("boolean" "Remove existing schedule instead of setting one")))
                    (save-window-excursion
                      (find-file org-file)
                      (goto-char (point-min))
                      (if (search-forward heading-text nil t)
                          (progn
                            (org-back-to-heading t)
                            (if remove-schedule
                                (org-schedule '(4))
                              (org-schedule nil schedule-date))
                            (save-buffer)
                            (if remove-schedule
                                (format "Successfully removed schedule from heading '%s' in %s" heading-text org-file)
                              (format "Successfully scheduled heading '%s' for %s in %s" heading-text schedule-date org-file)))
                        (error "Heading '%s' not found in %s" heading-text org-file))))

(claude-code-defmcp mcp-org-archive-todo (org-file heading-text &optional archive-location)
                    "Archive a TODO item by moving it to the archive file."
                    :mcp-description "Archive a TODO item by moving it to the archive file"
                    :mcp-schema '((org-file . ("string" "Path to the org file containing the heading"))
                                  (heading-text . ("string" "Text of the heading to archive"))
                                  (archive-location . ("string" "Archive location (optional)")))
                    (save-window-excursion
                      (find-file org-file)
                      (goto-char (point-min))
                      (if (search-forward heading-text nil t)
                          (progn
                            (org-back-to-heading t)
                            (if (and archive-location (not (string-empty-p archive-location)))
                                (let ((org-archive-location archive-location))
                                  (org-archive-subtree))
                              (org-archive-subtree))
                            (save-buffer)
                            (if (and archive-location (not (string-empty-p archive-location)))
                                (format "Successfully archived heading '%s' from %s to %s" heading-text org-file archive-location)
                              (format "Successfully archived heading '%s' from %s to default archive" heading-text org-file)))
                        (error "Heading '%s' not found in %s" heading-text org-file))))

(claude-code-defmcp mcp-org-capture (&optional template-key content immediate-finish)
                    "Add a new agenda item via org-capture mechanism."
                    :mcp-description "Add a new agenda item via org-capture mechanism"
                    :mcp-schema '((template-key . ("string" "Capture template key (optional)"))
                                  (content . ("string" "Content to capture (optional)"))
                                  (immediate-finish . ("boolean" "Whether to immediately finish capture")))
                    (unless immediate-finish (setq immediate-finish t))
                    (cond
                     ((not template-key)
                      ;; Show available capture templates
                      (let ((templates org-capture-templates)
                            (result "=== AVAILABLE CAPTURE TEMPLATES ===\n"))
                        (if templates
                            (dolist (template templates)
                              (setq result (concat result
                                                   (format "%s: %s\n"
                                                           (car template)
                                                           (cadr template)))))
                          (setq result (concat result "No capture templates configured")))
                        result))
                     (content
                      ;; Capture with provided content
                      (condition-case err
                          (let ((org-capture-entry (assoc template-key org-capture-templates)))
                            (if org-capture-entry
                                (progn
                                  (org-capture-string content template-key)
                                  (when immediate-finish
                                    (org-capture-finalize))
                                  (format "Successfully captured item using template '%s': %s" template-key content))
                              (error "Capture template '%s' not found" template-key)))
                        (error (format "Capture failed: %s" (error-message-string err)))))
                     (t
                      ;; Interactive capture
                      (condition-case err
                          (let ((org-capture-entry (assoc template-key org-capture-templates)))
                            (if org-capture-entry
                                (progn
                                  (org-capture nil template-key)
                                  (format "Interactive capture started with template '%s'. Edit and press C-c C-c to finish." template-key))
                              (error "Capture template '%s' not found" template-key)))
                        (error (format "Capture failed: %s" (error-message-string err)))))))

(claude-code-defmcp mcp-org-get-all-todos (&optional include-done org-files)
                    "Get all TODO items from org files, including unscheduled ones."
                    :mcp-description "Get all TODO items from org files, including unscheduled ones"
                    :mcp-schema '((include-done . ("boolean" "Include DONE items in results"))
                                  (org-files . ("array" "Specific org files to search (defaults to org-agenda-files)")))
                    (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                      (make-directory "/tmp/ClaudeWorkingFolder" t))

                    (let ((filename "/tmp/ClaudeWorkingFolder/all_todos.txt")
                          (result "=== ALL TODO ITEMS ===\n")
                          (files (or org-files (org-agenda-files)))
                          (todo-keywords (if include-done
                                             '("TODO" "NEXT" "STARTED" "WAITING" "DONE" "CANCELLED")
                                           '("TODO" "NEXT" "STARTED" "WAITING"))))
                      (dolist (file files)
                        (when (file-exists-p file)
                          (with-temp-buffer
                            (insert-file-contents file)
                            (org-mode)
                            (goto-char (point-min))
                            (setq result (concat result (format "\n=== FILE: %s ===\n" file)))
                            (let ((keyword-regex (concat "^\\*+ \\(" (mapconcat 'identity todo-keywords "\\|") "\\) ")))
                              (while (re-search-forward keyword-regex nil t)
                                (let* ((heading-start (line-beginning-position))
                                       (heading-end (line-end-position))
                                       (heading-text (buffer-substring heading-start heading-end))
                                       (scheduled (org-entry-get (point) "SCHEDULED"))
                                       (deadline (org-entry-get (point) "DEADLINE"))
                                       (line-num (line-number-at-pos)))
                                  (setq result (concat result
                                                       (format "Line %d: %s\n" line-num heading-text)
                                                       (if scheduled (format "  SCHEDULED: %s\n" scheduled) "")
                                                       (if deadline (format "  DEADLINE: %s\n" deadline) "")
                                                       "\n"))))))))
                      (write-region result nil filename)
                      (format "All TODO items written to %s" filename)))

(claude-code-defmcp mcp-org-agenda-goto (target-type target &optional agenda-type context-lines)
                    "Go to the source location of an agenda item and return file path and content."
                    :mcp-description "Go to the source location of an agenda item and return file path and content"
                    :mcp-schema '((target-type . ("string" "Either 'agenda_line' or 'agenda_text'"))
                                  (target . ("string" "Either agenda line number (1-based) or agenda item text"))
                                  (agenda-type . ("string" "Agenda type to work with (default 'a')"))
                                  (context-lines . ("number" "Number of lines before/after to show for context")))
                    (unless agenda-type (setq agenda-type "a"))
                    (unless context-lines (setq context-lines 5))
                    (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                      (make-directory "/tmp/ClaudeWorkingFolder" t))

                    (let ((filename "/tmp/ClaudeWorkingFolder/agenda_goto_result.txt")
                          (result ""))
                      (save-window-excursion
                        (let ((org-agenda-window-setup 'current-window))
                          (org-agenda nil agenda-type)
                          (with-current-buffer "*Org Agenda*"
                            (goto-char (point-min))
                            (cond
                             ((string= target-type "agenda_line")
                              (forward-line (1- (string-to-number target)))
                              (condition-case err
                                  (progn
                                    (org-agenda-goto)
                                    (let* ((file-name (buffer-file-name))
                                           (line-num (line-number-at-pos))
                                           (heading (org-get-heading t t t t))
                                           (start-line (max 1 (- line-num context-lines)))
                                           (end-line (+ line-num context-lines))
                                           (content ""))
                                      (setq result (concat result "=== AGENDA ITEM SOURCE ===\n"))
                                      (setq result (concat result (format "File: %s\n" file-name)))
                                      (setq result (concat result (format "Line: %d\n" line-num)))
                                      (setq result (concat result (format "Heading: %s\n\n" heading)))
                                      (setq result (concat result (format "=== CONTEXT (lines %d-%d) ===\n" start-line end-line)))
                                      (save-excursion
                                        (goto-char (point-min))
                                        (forward-line (1- start-line))
                                        (let ((current-line start-line))
                                          (while (and (<= current-line end-line) (not (eobp)))
                                            (setq content (concat content
                                                                  (format "%4d%s %s\n"
                                                                          current-line
                                                                          (if (= current-line line-num) "→" " ")
                                                                          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                                            (forward-line 1)
                                            (setq current-line (1+ current-line)))))
                                      (setq result (concat result content))))
                                (error (setq result (format "Error going to agenda item at line %s: %s\n" target (error-message-string err))))))
                             ((string= target-type "agenda_text")
                              (if (search-forward target nil t)
                                  (condition-case err
                                      (progn
                                        (beginning-of-line)
                                        (org-agenda-goto)
                                        (let* ((file-name (buffer-file-name))
                                               (line-num (line-number-at-pos))
                                               (heading (org-get-heading t t t t))
                                               (start-line (max 1 (- line-num context-lines)))
                                               (end-line (+ line-num context-lines))
                                               (content ""))
                                          (setq result (concat result "=== AGENDA ITEM SOURCE ===\n"))
                                          (setq result (concat result (format "File: %s\n" file-name)))
                                          (setq result (concat result (format "Line: %d\n" line-num)))
                                          (setq result (concat result (format "Heading: %s\n\n" heading)))
                                          (setq result (concat result (format "=== CONTEXT (lines %d-%d) ===\n" start-line end-line)))
                                          (save-excursion
                                            (goto-char (point-min))
                                            (forward-line (1- start-line))
                                            (let ((current-line start-line))
                                              (while (and (<= current-line end-line) (not (eobp)))
                                                (setq content (concat content
                                                                      (format "%4d%s %s\n"
                                                                              current-line
                                                                              (if (= current-line line-num) "→" " ")
                                                                              (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                                                (forward-line 1)
                                                (setq current-line (1+ current-line)))))
                                          (setq result (concat result content))))
                                    (error (setq result (format "Error going to agenda item containing '%s': %s\n" target (error-message-string err)))))
                                (setq result (format "Agenda item containing '%s' not found\n" target))))
                             (t (error "target_type must be either 'agenda_line' or 'agenda_text'"))))))
                      (write-region result nil filename)
                      (format "Agenda goto result written to %s:\n%s" filename result)))

;;;; File Operations

(claude-code-defmcp mcp-open-file (file-paths)
                    "Open files in Emacs and return buffer names."
                    :mcp-description "Open one or more files in Emacs in the background"
                    :mcp-schema '((file-paths . ("array" "List of file paths to open")))
                    (let ((results '())
                          (blocked-files '()))
                      (dolist (file-path file-paths)
                        (cond
                         ((and (boundp 'claude-code-mcp-restrict-file-access)
                               claude-code-mcp-restrict-file-access
                               (fboundp 'claude-code-mcp-file-access-allowed-p)
                               (not (claude-code-mcp-file-access-allowed-p file-path)))
                          (push (format "%s -> BLOCKED (file access restricted)" file-path) blocked-files))
                         (t
                          (condition-case err
                              (let ((buffer (find-file-noselect file-path)))
                                (push (format "%s -> %s" file-path (buffer-name buffer)) results))
                            (error (push (format "%s -> Error: %s" file-path (error-message-string err)) results))))))
                      
                      (let ((result (format "Files opened:\n%s" (mapconcat 'identity (reverse results) "\n"))))
                        (if blocked-files
                            (concat result "\nBlocked files:\n" (mapconcat 'identity (reverse blocked-files) "\n"))
                          result))))

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

(claude-code-defmcp mcp-emacs-keymap-analysis (buffer-names &optional include-global)
                    "Dump keymaps for buffer contexts to files."
                    :mcp-description "Analyze keymaps for one or more buffer contexts"
                    :mcp-schema '((buffer-names . ("array" "List of buffer names to analyze"))
                                  (include-global . ("boolean" "Include global keymap analysis")))
                    (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                      (make-directory "/tmp/ClaudeWorkingFolder" t))

                    (let ((successful-files '())
                          (blocked-buffers '()))
                      (dolist (buffer-name buffer-names)
                        (cond
                         ((claude-code-mcp-buffer-blocked-p buffer-name)
                          (push (format "%s: BLOCKED (contains sensitive pattern)" buffer-name) blocked-buffers))
                         (t
                          (condition-case err
                              (let ((filename (format "/tmp/ClaudeWorkingFolder/keymap_analysis_%s.txt"
                                                      (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))))
                                (with-temp-buffer
                                  (insert (format "=== KEYMAP ANALYSIS FOR BUFFER: %s ===\n\n" buffer-name))

                                  ;; Get keymap data without creating popup windows
                                  (let ((target-buffer (get-buffer buffer-name)))
                                    (if target-buffer
                                        (progn
                                          (insert (format "Major mode: %s\n\n"
                                                          (buffer-local-value 'major-mode target-buffer)))
                                          (insert "=== BUFFER KEY BINDINGS ===\n")
                                          ;; Get keymaps directly without describe-buffer-bindings
                                          (with-current-buffer target-buffer
                                            (let ((major-map (current-local-map))
                                                  (minor-maps (current-minor-mode-maps)))
                                              (insert "=== MAJOR MODE KEYMAP ===\n")
                                              (when major-map
                                                (insert (format "Keymap: %s\n" major-map)))
                                              (insert "\n=== MINOR MODE KEYMAPS ===\n")
                                              (dolist (map minor-maps)
                                                (when map
                                                  (insert (format "Keymap: %s\n" map))))
                                              (when include-global
                                                (insert "\n=== GLOBAL KEYMAP ===\n")
                                                (insert (format "Global keymap: %s\n" (current-global-map))))))
                                          (when include-global
                                            (insert "\n=== NOTE: Global bindings included above ===\n")))
                                      (insert (format "Error: Buffer '%s' not found\n" buffer-name))))

                                  (write-region (point-min) (point-max) filename))
                                (push filename successful-files))
                            (error
                             (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))))

                      (let ((result (format "Keymap analysis written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))
                        (if blocked-buffers
                            (concat result "\nBlocked buffers: " (mapconcat 'identity (reverse blocked-buffers) ", "))
                          result))))

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
                          (blocked-buffers '())
                          (with-content (if include-content include-content t))
                          (with-vars (if include-variables include-variables t)))
                      (dolist (buffer-name buffer-names)
                        (cond
                         ((claude-code-mcp-buffer-blocked-p buffer-name)
                          (push (format "%s: BLOCKED (contains sensitive pattern)" buffer-name) blocked-buffers))
                         (t
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
                                    (let ((lines (split-string (buffer-string) "\n"))
                                          (line-num 0))
                                      (setq info (concat info (mapconcat
                                                               (lambda (line)
                                                                 (setq line-num (1+ line-num))
                                                                 (format "%4d→%s" line-num line))
                                                               lines "\n")))))

                                  (write-region info nil filename)
                                  (push filename successful-files)))
                            (error
                             (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))))

                      (let ((result (format "Buffer info written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))
                        (if blocked-buffers
                            (concat result "\nBlocked buffers: " (mapconcat 'identity (reverse blocked-buffers) ", "))
                          result))))

(claude-code-defmcp mcp-check-parens (file-paths)
                    "Run check-parens on Lisp files to validate parentheses balance."
                    :mcp-description "Validate parentheses balance in Lisp code files"
                    :mcp-schema '((file-paths . ("array" "List of file paths to check")))
                    (let ((results '()))
                      (dolist (file-path file-paths)
                        (let* ((existing-buffer (find-buffer-visiting file-path))
                               (temp-buffer (find-file-noselect file-path))
                               (buffer-was-created (not existing-buffer)))
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
                            (when buffer-was-created
                              (kill-buffer temp-buffer)))))
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
                        (setq result "No Doom workspace system found - showing current buffer list\n")
                        (let ((buffers (mapcar 'buffer-name (buffer-list))))
                          (dolist (buf buffers)
                            (setq result (concat result buf "\n"))))))

                      (write-region result nil filename)
                      (format "Workspace buffers written to %s" filename)))

(claude-code-defmcp mcp-rename-workspace (workspace-identifier new-name)
                    "Rename a workspace by its slot number or current name."
                    :mcp-description "Rename a workspace by its slot number or current name"
                    :mcp-schema '((workspace-identifier . ("string" "Current workspace name or slot number to rename"))
                                  (new-name . ("string" "New name for the workspace")))
                    (if (and (fboundp '+workspace/rename) (fboundp '+workspace-get))
                        (condition-case err
                            (let* ((current-workspace (+workspace-current))
                                   (workspace-names (+workspace-list-names))
                                   (target-workspace (or
                                                      ;; Try to find by name first
                                                      (cl-find workspace-identifier workspace-names :test 'string=)
                                                      ;; Try to find by number
                                                      (when (string-match-p "^[0-9]+$" workspace-identifier)
                                                        (let ((index (string-to-number workspace-identifier)))
                                                          (when (and (>= index 0) (< index (length workspace-names)))
                                                            (nth index workspace-names)))))))
                              (if target-workspace
                                  (let ((current-workspace (+workspace-current-name))
                                        (old-name nil))
                                    (condition-case rename-err
                                        (progn
                                          ;; Switch to target workspace
                                          (+workspace-switch target-workspace t)
                                          ;; Rename it (this operates on current workspace)
                                          (setq old-name (+workspace-rename (+workspace-current-name) new-name))
                                          ;; Switch back to original workspace
                                          (unless (string= current-workspace target-workspace)
                                            (+workspace-switch current-workspace t))
                                          (if old-name
                                              (format "Successfully renamed Doom workspace '%s' to '%s'" old-name new-name)
                                            (format "Failed to rename Doom workspace '%s'" workspace-identifier)))
                                      (error
                                       ;; Try to switch back on error
                                       (ignore-errors (+workspace-switch current-workspace t))
                                       (format "Error during rename: %s" (error-message-string rename-err)))))
                                (format "Doom workspace '%s' not found. Available: %s" workspace-identifier (mapconcat 'identity workspace-names ", "))))
                          (error (format "Error renaming Doom workspace: %s" (error-message-string err))))
                      "Doom workspace system not available"))

(claude-code-defmcp mcp-create-workspace (workspace-name)
                    "Create a new workspace with a given name."
                    :mcp-description "Create a new workspace with a given name"
                    :mcp-schema '((workspace-name . ("string" "Name for the new workspace")))
                    (if (fboundp '+workspace-new)
                        (condition-case err
                            (progn
                              (+workspace-new workspace-name)
                              (format "Successfully created Doom workspace '%s'" workspace-name))
                          (error (format "Error creating Doom workspace: %s" (error-message-string err))))
                      "Doom workspace system not available"))

(claude-code-defmcp mcp-delete-workspace (workspace-identifier)
                    "Delete a workspace by name or identifier."
                    :mcp-description "Delete a workspace by name or identifier"
                    :mcp-schema '((workspace-identifier . ("string" "Workspace name or identifier to delete")))
                    (if (and (fboundp 'persp-kill) (fboundp '+workspace-list-names))
                        (condition-case err
                            (let* ((workspace-names (+workspace-list-names))
                                   (target-workspace (or
                                                      (cl-find workspace-identifier workspace-names :test 'string=)
                                                      (when (string-match-p "^[0-9]+$" workspace-identifier)
                                                        (let ((index (string-to-number workspace-identifier)))
                                                          (when (and (>= index 0) (< index (length workspace-names)))
                                                            (nth index workspace-names)))))))
                              (if target-workspace
                                  ;; Check for active sessions and terminals before deleting
                                  (let* ((persp-obj (persp-get-by-name target-workspace))
                                         (workspace-buffers (when persp-obj (mapcar 'buffer-name (persp-buffers persp-obj))))
                                         (claude-buffers (cl-remove-if-not
                                                          (lambda (buf-name)
                                                            (string-match-p "^\\*claude:.*:\\*$" buf-name))
                                                          workspace-buffers))
                                         (terminal-buffers (cl-remove-if-not
                                                            (lambda (buf-name)
                                                              (or (string-match-p "^\\*vterm\\*" buf-name)
                                                                  (string-match-p "^\\*eat\\*" buf-name)
                                                                  (string-match-p "^\\*eshell\\*" buf-name)
                                                                  (string-match-p "^\\*shell\\*" buf-name)
                                                                  (string-match-p "^\\*term\\*" buf-name)
                                                                  (string-match-p "^\\*ansi-term\\*" buf-name)
                                                                  (string-match-p "^\\*mistty\\*" buf-name)))
                                                            workspace-buffers))
                                         (protected-buffers (append claude-buffers terminal-buffers))
                                         (workspace-has-protected (> (length protected-buffers) 0)))
                                    (if workspace-has-protected
                                        (format "Cannot delete workspace '%s' - contains active sessions/terminals: %s. Please close these or move them to another workspace first."
                                                target-workspace
                                                (mapconcat 'identity protected-buffers ", "))
                                      (progn
                                        (persp-kill target-workspace)
                                        (format "Successfully deleted Doom workspace '%s'" target-workspace))))
                                (format "Doom workspace '%s' not found. Available: %s" workspace-identifier (mapconcat 'identity workspace-names ", "))))
                          (error (format "Error deleting Doom workspace: %s" (error-message-string err))))
                      "Doom workspace system not available"))

(claude-code-defmcp mcp-move-protected-buffers-to-workspace (source-workspace target-workspace)
                    "Move all protected buffers from one workspace to another."
                    :mcp-description "Move all protected buffers (Claude Code sessions, terminals, etc.) from one workspace to another"
                    :mcp-schema '((source-workspace . ("string" "Workspace containing protected buffers to move"))
                                  (target-workspace . ("string" "Workspace to move protected buffers to")))
                    (if (and (fboundp '+workspace-list-names) (fboundp '+workspace-buffer-list))
                        (condition-case err
                            (let* ((workspace-names (+workspace-list-names)))
                              (if (and (cl-find source-workspace workspace-names :test 'string=)
                                       (cl-find target-workspace workspace-names :test 'string=))
                                  (let* ((source-buffers (mapcar 'buffer-name (+workspace-buffer-list source-workspace)))
                                         (claude-buffers (cl-remove-if-not
                                                          (lambda (buf-name)
                                                            (string-match-p "^\\*claude:.*:\\*$" buf-name))
                                                          source-buffers))
                                         (terminal-buffers (cl-remove-if-not
                                                            (lambda (buf-name)
                                                              (or (string-match-p "^\\*vterm\\*" buf-name)
                                                                  (string-match-p "^\\*eat\\*" buf-name)
                                                                  (string-match-p "^\\*eshell\\*" buf-name)
                                                                  (string-match-p "^\\*shell\\*" buf-name)
                                                                  (string-match-p "^\\*term\\*" buf-name)
                                                                  (string-match-p "^\\*ansi-term\\*" buf-name)
                                                                  (string-match-p "^\\*mistty\\*" buf-name)))
                                                            source-buffers))
                                         (protected-buffers (append claude-buffers terminal-buffers))
                                         (current-workspace (+workspace-current-name)))
                                    (if protected-buffers
                                        (progn
                                          ;; Switch to target workspace and add buffers
                                          (+workspace-switch target-workspace t)
                                          (dolist (buf-name protected-buffers)
                                            (when (get-buffer buf-name)
                                              (persp-add-buffer (get-buffer buf-name))))
                                          ;; Switch to source workspace and remove buffers
                                          (+workspace-switch source-workspace t)
                                          (dolist (buf-name protected-buffers)
                                            (when (get-buffer buf-name)
                                              (persp-remove-buffer (get-buffer buf-name))))
                                          ;; Switch back to original workspace
                                          (unless (or (string= current-workspace source-workspace)
                                                      (string= current-workspace target-workspace))
                                            (+workspace-switch current-workspace t))
                                          (format "Successfully moved %d protected buffers from '%s' to '%s': %s"
                                                  (length protected-buffers)
                                                  source-workspace
                                                  target-workspace
                                                  (mapconcat 'identity protected-buffers ", ")))
                                      (format "No protected buffers found in workspace '%s'" source-workspace)))
                                (format "Workspace not found. Source: %s, Target: %s. Available: %s"
                                        source-workspace
                                        target-workspace
                                        (mapconcat 'identity workspace-names ", "))))
                          (error (format "Error moving protected buffers: %s" (error-message-string err))))
                      "Doom workspace system not available"))

(claude-code-defmcp mcp-setup-workspace-layout (workspace-name layout)
                    "Set up window layout for a workspace without switching away from current workspace."
                    :mcp-description "Set up window layout for a workspace without switching away from current workspace"
                    :mcp-schema '((workspace-name . ("string" "Name of the workspace to configure"))
                                  (layout . ("object" "Layout configuration with primary_buffer, secondary_buffer, split_direction")))
                    (condition-case err
                        (let ((current-workspace (+workspace-current-name))
                              (target-workspace workspace-name)
                              (primary-buf (cdr (assoc 'primary_buffer layout)))
                              (secondary-buf (cdr (assoc 'secondary_buffer layout)))
                              (split-dir (or (cdr (assoc 'split_direction layout)) "horizontal")))
                          ;; Save current workspace state
                          (if (cl-find target-workspace (+workspace-list-names) :test 'string=)
                              (progn
                                ;; Switch to target workspace temporarily
                                (+workspace-switch target-workspace t)

                                ;; Set up primary buffer
                                (if (get-buffer primary-buf)
                                    (switch-to-buffer primary-buf)
                                  (error "Primary buffer '%s' not found" primary-buf))

                                ;; Set up secondary buffer if specified
                                (when (and secondary-buf (get-buffer secondary-buf))
                                  (if (string= split-dir "vertical")
                                      (split-window-below)
                                    (split-window-right))
                                  (other-window 1)
                                  (switch-to-buffer secondary-buf)
                                  (other-window 1))

                                ;; Switch back to original workspace
                                (unless (string= current-workspace target-workspace)
                                  (+workspace-switch current-workspace t))

                                (format "Successfully configured workspace '%s' with layout: %s%s"
                                        target-workspace
                                        primary-buf
                                        (if secondary-buf
                                            (format " + %s (%s split)" secondary-buf split-dir)
                                          "")))
                            (format "Workspace '%s' not found. Available: %s"
                                    target-workspace
                                    (mapconcat 'identity (+workspace-list-names) ", "))))
                      (error (format "Error setting up workspace layout: %s" (error-message-string err)))))

(claude-code-defmcp mcp-view-buffer (buffer-names)
                    "Get buffer contents and write each to separate files."
                    :mcp-description "View buffer contents by writing to individual files"
                    :mcp-schema '((buffer-names . ("array" "List of buffer names to view")))
                    (unless (file-directory-p "/tmp/ClaudeWorkingFolder")
                      (make-directory "/tmp/ClaudeWorkingFolder" t))

                    (let ((successful-files '())
                          (blocked-buffers '()))
                      (dolist (buffer-name buffer-names)
                        (cond
                         ((claude-code-mcp-buffer-blocked-p buffer-name)
                          (push (format "%s: BLOCKED (contains sensitive pattern)" buffer-name) blocked-buffers))
                         (t
                          (condition-case err
                              (with-current-buffer buffer-name
                                (let* ((sanitized-name (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" buffer-name))
                                       (filename (format "/tmp/ClaudeWorkingFolder/%s.txt" sanitized-name))
                                       (content (buffer-string))
                                       (lines (split-string content "\n"))
                                       (line-num 0))
                                  (write-region (mapconcat (lambda (line)
                                                             (setq line-num (1+ line-num))
                                                             (format "%4d→%s" line-num line))
                                                           lines "\n")
                                                nil filename)
                                  (push filename successful-files)))
                            (error
                             (message "Error processing buffer '%s': %s" buffer-name (error-message-string err)))))))

                      (let ((result (format "Buffer contents written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))
                        (if blocked-buffers
                            (concat result "\nBlocked buffers: " (mapconcat 'identity (reverse blocked-buffers) ", "))
                          result))))

;;;; Debugging Tools

(claude-code-defmcp mcp-count-parens (file-path start-line end-line)
                    "Count opening and closing parentheses between two lines in a file."
                    :mcp-description "Count opening and closing parentheses between two lines in a file"
                    :mcp-schema '((file-path . ("string" "Path to the file to analyze"))
                                  (start-line . ("number" "Starting line number (1-based)"))
                                  (end-line . ("number" "Ending line number (1-based)")))
                    (if (file-exists-p file-path)
                        (with-temp-buffer
                          (insert-file-contents file-path)
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (let ((start-pos (point))
                                (open-count 0)
                                (close-count 0)
                                (in-string nil)
                                (in-comment nil))
                            (forward-line (- end-line start-line -1))
                            (let ((end-pos (point)))
                              (goto-char start-pos)
                              (while (< (point) end-pos)
                                (let ((char (char-after)))
                                  (cond
                                   ;; Handle string state
                                   ((and (eq char ?\") (not in-comment))
                                    (unless (eq (char-before) ?\\)
                                      (setq in-string (not in-string))))
                                   ;; Handle comment state
                                   ((and (eq char ?\;) (not in-string))
                                    (setq in-comment t))
                                   ;; Count parentheses only if not in string or comment
                                   ((and (eq char ?\() (not in-string) (not in-comment))
                                    (setq open-count (1+ open-count)))
                                   ((and (eq char ?\)) (not in-string) (not in-comment))
                                    (setq close-count (1+ close-count)))
                                   ;; Reset comment state at newline
                                   ((eq char ?\n)
                                    (setq in-comment nil))))
                                (forward-char 1))
                              (format "Lines %d-%d: Opening parens: %d, Closing parens: %d, Net: %d"
                                      start-line end-line open-count close-count 
                                      (- open-count close-count)))))
                      (format "File not found: %s" file-path)))

(provide 'mcp-tools)


;;; mcp-tools.el ends here
