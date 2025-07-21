;;; test-diff-display.el --- Test diff display functionality -*- lexical-binding: t; -*-

;; Simple test to verify the diff display preserves the Claude window

(require 'claude-code-mcp)

(defun test-diff-display ()
  "Test the diff display functionality interactively."
  (interactive)
  ;; First ensure Claude is running and visible
  ;; (unless (get-buffer "*claude*")
  ;;   (error "Please start Claude first with M-x claude-code"))
  
  ;; Create a test scenario
  (let ((test-content ";;; Original file -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Example Corp
;; Author: Original Author
;; Version: 1.0

(require 'cl-lib)

(defcustom example-mode-hook nil
  \"Hook run after entering Example mode.\"
  :type 'hook
  :group 'example)

(defvar example-counter 0
  \"Global counter for examples.\")

(defun hello-world ()
  \"Say hello to the world.\"
  (message \"Hello, World!\"))

(defun factorial (n)
  \"Calculate factorial of N.\"
  (if (<= n 1)
      1
    (* n (factorial (1- n)))))

(defun process-list (items)
  \"Process a list of ITEMS.\"
  (dolist (item items)
    (when (stringp item)
      (message \"Processing: %s\" item))))

(defclass example-class ()
  ((name :initarg :name
         :type string
         :documentation \"Name of the example.\")
   (value :initarg :value
          :type number
          :documentation \"Numeric value.\"))
  \"A simple example class.\")

(defun example-helper (x y)
  \"Add X and Y together.\"
  (+ x y))

;; This function will be removed
(defun deprecated-function ()
  \"This function is deprecated.\"
  (message \"Please use new-function instead\"))

(provide 'example-mode)
;;; example-mode.el ends here
")
        (modified-content ";;; Modified file -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Example Corp
;; Author: Modified Author <modified@example.com>
;; Version: 2.0
;; Keywords: example, demo
;; Package-Requires: ((emacs \"27.1\"))

(require 'cl-lib)
(require 'seq)

(defgroup example nil
  \"Example mode configuration.\"
  :group 'tools)

(defcustom example-mode-hook nil
  \"Hook run after entering Example mode.\"
  :type 'hook
  :group 'example)

(defcustom example-default-name \"Example\"
  \"Default name for examples.\"
  :type 'string
  :group 'example)

(defvar example-counter 0
  \"Global counter for tracking created examples.\")

(defvar-local example-local-state nil
  \"Buffer-local state for example mode.\")

(defun hello-world (name &optional greeting)
  \"Say hello to NAME with optional GREETING.
If GREETING is not provided, use 'Hello'.\")
  (let ((greet (or greeting \"Hello\")))
    (message \"%s, %s!\" greet name)))

(defun factorial (n)
  \"Calculate the factorial of N recursively.
Signal an error if N is negative.\"
  (cond
   ((< n 0) (error \"Factorial undefined for negative numbers\"))
   ((<= n 1) 1)
   (t (* n (factorial (1- n))))))

(defun process-list (items &optional filter-fn)
  \"Process a list of ITEMS with optional FILTER-FN.
If FILTER-FN is provided, only process items that satisfy it.\"
  (dolist (item items)
    (when (and (or (not filter-fn) (funcall filter-fn item))
               (stringp item))
      (message \"Processing: %s (length: %d)\" item (length item)))))

(defclass example-class ()
  ((name :initarg :name
         :type string
         :documentation \"Name of the example.\")
   (value :initarg :value
          :type number
          :documentation \"Numeric value.\")
   (tags :initarg :tags
         :initform nil
         :type list
         :documentation \"List of tags.\")
   (created-at :initarg :created-at
               :initform (current-time)
               :documentation \"Creation timestamp.\"))
  \"An enhanced example class with more features.\")

(cl-defmethod example-describe ((obj example-class))
  \"Describe the example object OBJ.\"
  (format \"Example '%s' with value %d\"
          (slot-value obj 'name)
          (slot-value obj 'value)))

(defun example-helper (x y &rest args)
  \"Add X and Y together, plus any additional ARGS.\"
  (apply #'+ x y args))

(defun new-function (data)
  \"Process DATA in a new way.
This replaces the deprecated-function.\"
  (let ((result (mapcar (lambda (x) (* x 2)) data)))
    (message \"Processed %d items\" (length result))
    result))

(defmacro with-example-context (&rest body)
  \"Execute BODY with example context.\"
  `(let ((example-counter (1+ example-counter)))
     ,@body))

(define-minor-mode example-mode
  \"Toggle Example mode.\"
  :lighter \" Example\"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd \"C-c e\") #'example-command)
            map)
  (if example-mode
      (message \"Example mode enabled\")
    (message \"Example mode disabled\")))

(provide 'example-mode)
;;; example-mode.el ends here
"))
    
    ;; Find an active MCP session
    (let ((session (car (hash-table-values claude-code-mcp--sessions))))
      (if session
          (progn
            (message "Testing diff display with Claude window preservation...")
            (claude-code-mcp--tool-open-diff
             `((old_file_path . "original.el")
               (new_file_path . "modified.el")
               (new_file_contents . ,modified-content)
               (tab_name . "Test Diff")
               (old_file_contents . ,test-content))
             session)
            (message "Diff should now be displayed in a bottom window. Claude should still be visible."))
        (error "No active MCP session found. Make sure Claude is connected")))))

(provide 'test-diff-display)
;;; test-diff-display.el ends here
