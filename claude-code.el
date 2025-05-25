;;; claude-code.el --- Claude Code Emacs integration -*- lexical-binding: t; -*-

;; Author: Stephen Molitor <stevemolitor@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0") (transient "0.7.5") (eat "0.9.2"))
;; Keywords: tools, ai
;; URL: https://github.com/stevemolitor/claude-code.el

;;; Commentary:
;; An Emacs interface to Claude Code.  This package provides convenient
;; ways to interact with Claude from within Emacs, including sending
;; commands, toggling the Claude window, and accessing slash commands.

;;; Code:

(require 'transient)
(require 'project)
(require 'cl-lib)

;; Declare external variables and functions from eat package
(defvar eat--semi-char-mode)
(defvar eat-terminal)
(defvar eat--synchronize-scroll-function)
(declare-function eat-term-reset "eat" (terminal))
(declare-function eat-term-redisplay "eat" (terminal))
(declare-function eat--set-cursor "eat" (terminal &rest args))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat-term-display-beginning "eat" (terminal))

;;;; Customization options
(defgroup claude-code nil
  "Claude AI interface for Emacs."
  :group 'tools)

(defface claude-code-repl-face
  nil
  "Face for Claude REPL."
  :group 'claude-code)

(defcustom claude-code-term-name "xterm-256color"
  "Terminal type to use for Claude REPL."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-start-hook nil
  "Hook run after Claude is started."
  :type 'hook
  :group 'claude-code)

(defcustom claude-code-startup-delay 0.1
  "Delay in seconds after starting Claude before displaying buffer.

This helps fix terminal layout issues that can occur if the buffer
is displayed before Claude is fully initialized."
  :type 'number
  :group 'claude-code)

(defcustom claude-code-large-buffer-threshold 1000
  "Size threshold in characters above which buffers are considered \"large\".

When sending a buffer to Claude with `claude-code-send-region` and no
region is active, prompt for confirmation if buffer size exceeds this value."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-program "claude"
  "Program to run when starting Claude.
This is passed as the PROGRAM parameter to `eat-make`."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-program-switches nil
  "List of command line switches to pass to the Claude program.
These are passed as SWITCHES parameters to `eat-make`."
  :type '(repeat string)
  :group 'claude-code)

(defcustom claude-code-read-only-mode-cursor-type '(box nil nil)
  "Type of cursor to use as invisible cursor in Claude Code terminal buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking.

Valid cursor types for CURSOR-ON and CURSOR-OFF:
- t: Frame default cursor
- box: Filled box cursor
- (box . N): Box cursor with specified size N
- hollow: Hollow cursor
- bar: Vertical bar cursor
- (bar . N): Vertical bar with specified height N
- hbar: Horizontal bar cursor
- (hbar . N): Horizontal bar with specified width N
- nil: No cursor

BLINKING-FREQUENCY can be nil (no blinking) or a number."
  :type '(list
          (choice
           (const :tag "Frame default" t)
           (const :tag "Filled box" box)
           (cons :tag "Box with specified size" (const box) integer)
           (const :tag "Hollow cursor" hollow)
           (const :tag "Vertical bar" bar)
           (cons :tag "Vertical bar with specified height" (const bar)
                 integer)
           (const :tag "Horizontal bar" hbar)
           (cons :tag "Horizontal bar with specified width"
                 (const hbar) integer)
           (const :tag "None" nil))
          (choice
           (const :tag "No blinking" nil)
           (number :tag "Blinking frequency"))
          (choice
           (const :tag "Frame default" t)
           (const :tag "Filled box" box)
           (cons :tag "Box with specified size" (const box) integer)
           (const :tag "Hollow cursor" hollow)
           (const :tag "Vertical bar" bar)
           (cons :tag "Vertical bar with specified height" (const bar)
                 integer)
           (const :tag "Horizontal bar" hbar)
           (cons :tag "Horizontal bar with specified width"
                 (const hbar) integer)
           (const :tag "None" nil)))
  :group 'claude-code)

(defvar claude-code--claude-buffer "*claude*"
  "Name of the Claude buffer.")

;; Forward declare variables to avoid compilation warnings
(defvar eat-terminal)
(defvar eat-term-name)
(defvar eat-invisible-cursor-type)
(declare-function eat-term-send-string "eat")
(declare-function eat-kill-process "eat")
(declare-function eat-make "eat")
(declare-function eat-emacs-mode "eat")
(declare-function eat-semi-char-mode "eat")

;; Forward declare flycheck functions
(declare-function flycheck-overlay-errors-at "flycheck")
(declare-function flycheck-error-filename "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-message "flycheck")

;;;; Key bindings
;;;###autoload (autoload 'claude-code-command-map "claude-code")
(defvar claude-code-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "/" 'claude-code-slash-commands)
    (define-key map "b" 'claude-code-switch-to-buffer)
    (define-key map "c" 'claude-code)
    (define-key map "d" 'claude-code-current-directory)
    (define-key map "e" 'claude-code-fix-error-at-point)
    (define-key map "k" 'claude-code-kill)
    (define-key map "m" 'claude-code-transient)
    (define-key map "n" 'claude-code-send-escape)
    (define-key map "r" 'claude-code-send-region)
    (define-key map "s" 'claude-code-send-command)
    (define-key map "t" 'claude-code-toggle)
    (define-key map "x" 'claude-code-send-command-with-context)
    (define-key map "y" 'claude-code-send-return)
    (define-key map "z" 'claude-code-toggle-read-only-mode)
    map)
  "Keymap for Claude commands.")

;;;; Transient Menus
;;;###autoload (autoload 'claude-code-transient "claude-code" nil t)
(transient-define-prefix claude-code-transient ()
  "Claude command menu."
  ["Claude Commands"
   ["Manage Claude" ("c" "Start Claude" claude-code)
    ("d" "Start Claude in current directory" claude-code-current-directory)
    ("t" "Toggle claude window" claude-code-toggle)
    ("b" "Switch to Claude buffer" claude-code-switch-to-buffer)
    ("k" "Kill Claude" claude-code-kill)
    ("z" "Toggle read-only mode" claude-code-toggle-read-only-mode)]
   ["Send Commands to Claude" ("s" "Send command" claude-code-send-command)
    ("x" "Send command with context" claude-code-send-command-with-context)
    ("r" "Send region or buffer" claude-code-send-region)
    ("e" "Fix error at point" claude-code-fix-error-at-point)
    ("y" "Send <return> to Claude (\"Yes\")" claude-code-send-return)
    ("n" "Send <escape> to Claude (\"No\")" claude-code-send-escape)
    ("/" "Slash Commands" claude-code-slash-commands)]])

;;;###autoload (autoload 'claude-code-slash-commands "claude-code" nil t)
(transient-define-prefix claude-code-slash-commands ()
  "Claude slash commands menu."
  ["Slash Commands"
   ["Basic Commands"
    ("c" "Clear" (lambda () (interactive) (claude-code--do-send-command "/clear")))
    ("o" "Compact" (lambda () (interactive) (claude-code--do-send-command "/compact")))
    ("f" "Config" (lambda () (interactive) (claude-code--do-send-command "/config")))
    ("t" "Cost" (lambda () (interactive) (claude-code--do-send-command "/cost")))
    ("d" "Doctor" (lambda () (interactive) (claude-code--do-send-command "/doctor")))
    ("x" "Exit" (lambda () (interactive) (claude-code--do-send-command "/exit")))
    ("h" "Help" (lambda () (interactive) (claude-code--do-send-command "/help")))]
   
   ["Special Commands"
    ("i" "Init" (lambda () (interactive) (claude-code--do-send-command "/init")))
    ("p" "PR" (lambda () (interactive) (claude-code--do-send-command "/pr")))
    ("r" "Release" (lambda () (interactive) (claude-code--do-send-command "/release")))
    ("b" "Bug" (lambda () (interactive) (claude-code--do-send-command "/bug")))
    ("v" "Review" (lambda () (interactive) (claude-code--do-send-command "/review")))]
   
   ["Additional Commands"
    ("e" "Terminal" (lambda () (interactive) (claude-code--do-send-command "/terminal")))
    ("m" "Theme" (lambda () (interactive) (claude-code--do-send-command "/theme")))
    ("v" "Vim" (lambda () (interactive) (claude-code--do-send-command "/vim")))
    ("a" "Approved" (lambda () (interactive) (claude-code--do-send-command "/approved")))
    ("l" "Logout" (lambda () (interactive) (claude-code--do-send-command "/logout")))
    ("g" "Login" (lambda () (interactive) (claude-code--do-send-command "/login")))]
   ])

;;;; Private util functions
(defun claude-code--get-claude-buffer ()
  "Return the Claude buffer if it exists, nil otherwise."
  (get-buffer claude-code--claude-buffer))

(defun claude-code--switch-to-claude-buffer ()
  "Switch to the Claude buffer."
  (switch-to-buffer claude-code--claude-buffer))

(defun claude-code--do-send-command (cmd)
  "Send a command CMD to Claude if Claude buffer exists.

After sending the command, move point to the end of the buffer."
  (if-let ((claude-code-buffer (claude-code--get-claude-buffer)))
      (with-current-buffer claude-code-buffer
        (eat-term-send-string eat-terminal cmd)
        (eat-term-send-string eat-terminal (kbd "RET"))
        (display-buffer claude-code-buffer))
    (error "Claude is not running")))

(defun claude-code--setup-repl-faces ()
  "Setup faces for the Claude REPL buffer.

Applies the `claude-code-repl-face' to all terminal-related faces
for consistent appearance."
  (dolist (face '(eat-shell-prompt-annotation-running
                  eat-shell-prompt-annotation-success
                  eat-shell-prompt-annotation-failure
                  eat-term-bold
                  eat-term-faint
                  eat-term-italic
                  eat-term-slow-blink
                  eat-term-fast-blink))
    (funcall 'face-remap-add-relative face :inherit 'claude-code-repl-face))
  (dotimes (i 10)
    (let ((face (intern (format "eat-term-font-%d" i))))
      (funcall 'face-remap-add-relative face :inherit 'claude-code-repl-face)))
  (dotimes (i 10)
    (let ((face (intern (format "eat-term-font-%d" i))))
      (funcall 'face-remap-add-relative face :inherit 'claude-code-repl-face)))
  (buffer-face-set :inherit 'claude-code-repl-face)
  (face-remap-add-relative 'nobreak-space :underline nil)
  (face-remap-add-relative 'eat-term-faint :foreground "#999999" :weight 'light))

(defun claude-code--synchronize-scroll (windows)
  "Synchronize scrolling and point between terminal and WINDOWS.

WINDOWS is a list of windows.  WINDOWS may also contain the special
symbol `buffer', in which case the point of current buffer is set.

This custom version keeps the prompt at the bottom of the window when
possible, preventing the scrolling up issue when editing other buffers."
  (dolist (window windows)
    (if (eq window 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      ;; Instead of always setting window-start to the beginning,
      ;; keep the prompt at the bottom of the window when possible
      (let ((cursor-pos (eat-term-display-cursor eat-terminal))
            (term-beginning (eat-term-display-beginning eat-terminal)))
        ;; Set point first
        (set-window-point window cursor-pos)
        ;; Check if we should keep the prompt at the bottom
        (when (and (>= cursor-pos (- (point-max) 2))
                   (not (pos-visible-in-window-p cursor-pos window)))
          ;; Recenter with point at bottom of window
          (with-selected-window window
            (save-excursion
              (goto-char cursor-pos)
              (recenter -1))))
        ;; Otherwise, only adjust window-start if cursor is not visible
        (unless (pos-visible-in-window-p cursor-pos window)
          (set-window-start window term-beginning))))))

(defun claude-code--on-window-configuration-change ()
  "Handle window configuration changes for Claude buffer.

Ensures the Claude buffer stays scrolled to the bottom when window all
configuration changes (e.g., when minibuffer opens/closes)."
  (when-let ((claude-buffer (claude-code--get-claude-buffer)))
    (with-current-buffer claude-buffer
      ;; Get all windows showing the Claude buffer
      (if-let ((windows (get-buffer-window-list claude-buffer nil t)))
        (claude-code--synchronize-scroll windows)))))

(defun claude-code--start (dir &optional arg continue)
  "Start Claude in directory DIR.

With non-nil ARG, switch to the Claude buffer after starting. With
non-nil CONTINUE, start Claude with --continue flag to continue previous
conversation."
  ;; Forward declare variables to avoid compilation warnings
  (require 'eat)
  
  (let* ((default-directory dir)
         (buffer (get-buffer-create claude-code--claude-buffer))
         (program-switches (if continue
                              (append claude-code-program-switches '("--continue"))
                            claude-code-program-switches)))
    (with-current-buffer buffer
      (cd dir)
      ;; (setq-local eat-invisible-cursor-type claude-code-read-only-mode-cursor-type)
      (setq-local eat-term-name claude-code-term-name)
      (let ((process-adaptive-read-buffering nil))
        (apply #'eat-make "claude" claude-code-program nil program-switches))
      (claude-code--setup-repl-faces)
      ;; Set our custom synchronize scroll function
      (setq-local eat--synchronize-scroll-function #'claude-code--synchronize-scroll)
      ;; Add window configuration change hook to keep buffer scrolled to bottom
      (add-hook 'window-configuration-change-hook #'claude-code--on-window-configuration-change nil t)
      (run-hooks 'claude-code-start-hook)

      ;; fix wonky initial terminal layout that happens sometimes if we show the buffer before claude is ready
      (sleep-for claude-code-startup-delay)
      (display-buffer buffer))
    (when arg
      (switch-to-buffer buffer))))

(defun claude-code--format-errors-at-point ()
  "Format errors at point as a string with file and line numbers.
First tries flycheck errors if flycheck is enabled, then falls back
to help-at-pt (used by flymake and other systems).
Returns a string with the errors or a message if no errors found."
  (interactive)
  (cond
   ;; Try flycheck first if available and enabled
   ((and (featurep 'flycheck) (bound-and-true-p flycheck-mode))
    (let ((errors (flycheck-overlay-errors-at (point)))
          (result ""))
      (if (not errors)
          "No flycheck errors at point"
        (dolist (err errors)
          (let ((file (flycheck-error-filename err))
                (line (flycheck-error-line err))
                (msg (flycheck-error-message err)))
            (setq result (concat result
                                 (format "%s:%d: %s\n"
                                         file
                                         line
                                         msg)))))
        (string-trim-right result))))
   ;; Fall back to help-at-pt-kbd-string (works with flymake and other sources)
   ((help-at-pt-kbd-string)
    (let ((help-str (help-at-pt-kbd-string)))
      (if (not (null help-str))
          (substring-no-properties help-str)
        "No help string available at point")))
   ;; No errors found by any method
   (t "No errors at point")))

;;;; Interactive Commands
;;;###autoload
(defun claude-code (&optional arg)
  "Start Claude in an eat terminal and enable `claude-code-mode'.

With single prefix ARG (C-u), prompt for the project directory.
With double prefix ARG (C-u C-u), continue previous conversation."
  (interactive "P")
  (let* ((dir (if (and arg (not (equal arg '(16))))
                  (read-directory-name "Project directory: ")
                (funcall #'project-root (project-current t))))
         (continue (equal arg '(16))))
    (claude-code--start dir arg continue)))

;;;###autoload
(defun claude-code-send-region (&optional arg)
  "Send the current region to Claude.

If no region is active, send the entire buffer if it's not too large.
For large buffers, ask for confirmation first.

With prefix ARG, prompt for instructions to add to the text before
sending. With two prefix ARGs (C-u C-u), both add instructions and
switch to Claude buffer."
  (interactive "P")
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (if (> (buffer-size) claude-code-large-buffer-threshold)
                     (when (yes-or-no-p "Buffer is large.  Send anyway? ")
                       (buffer-substring-no-properties (point-min) (point-max)))
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (prompt (cond
                  ((equal arg '(4)) ; C-u
                   (read-string "Instructions for Claude: "))
                  ((equal arg '(16)) ; C-u C-u
                   (read-string "Instructions for Claude: "))
                  (t nil)))
         (full-text (if prompt
                        (format "%s\n\n%s" prompt text)
                      text)))
    (when full-text
      (claude-code--do-send-command full-text)
      (when (equal arg '(16)) ; Only switch buffer with C-u C-u
        (claude-code--switch-to-claude-buffer)))))

;;;###autoload
(defun claude-code-current-directory (&optional arg)
  "Start Claude in the current directory.

With single prefix ARG (C-u), switch to the Claude buffer after starting.
With double prefix ARG (C-u C-u), continue previous conversation."
  (interactive "P")
  (let ((continue (equal arg '(16))))
    (claude-code--start default-directory arg continue)))

;;;###autoload
(defun claude-code-toggle ()
  "Show or hide the Claude window.

If the Claude buffer doesn't exist, create it."
  (interactive)
  (let ((claude-code-buffer (claude-code--get-claude-buffer)))
    (if claude-code-buffer
        (if (get-buffer-window claude-code-buffer)
            (delete-window (get-buffer-window claude-code-buffer))
          (display-buffer claude-code-buffer))
      (error "Claude is not running"))))

;;;###autoload
(defun claude-code-switch-to-buffer ()
  "Switch to the Claude buffer if it exists."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-claude-buffer)))
      (claude-code--switch-to-claude-buffer)
    (error "Claude is not running")))

;;;###autoload
(defun claude-code-kill ()
  "Kill Claude process and close its window."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-claude-buffer)))
      (progn (with-current-buffer claude-code-buffer
               (eat-kill-process)
               (kill-buffer claude-code-buffer))
             (message "Claude killed"))
    (error "Claude is not running")))

;;;###autoload
(defun claude-code-send-command (cmd &optional arg)
  "Read a Claude command from the minibuffer and send it.

With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (claude-code--do-send-command cmd)
  (when arg
    (claude-code--switch-to-claude-buffer)))

;;;###autoload
(defun claude-code-send-command-with-context (cmd &optional arg)
  "Read a Claude command and send it with current file and line context.

If region is active, include region line numbers.
With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (let* ((file-name (when (buffer-file-name)
                        (file-relative-name (buffer-file-name) (project-root (project-current t)))))
         (line-info (if (use-region-p)
                        (format "Lines: %d-%d"
                                (line-number-at-pos (region-beginning))
                                (line-number-at-pos (region-end)))
                      (format "Line: %d" (line-number-at-pos))))
         (cmd-with-context (if file-name
                               (format "%s\nContext: File: %s, %s"
                                       cmd
                                       file-name
                                       line-info)
                             cmd)))
    (claude-code--do-send-command cmd-with-context)
    (when arg
      (claude-code--switch-to-claude-buffer))))

;;;###autoload
(defun claude-code-send-return ()
  "Send <return> to the Claude Code REPL.

This is useful for saying Yes when Claude asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (claude-code--do-send-command ""))

;;;###autoload
(defun claude-code-send-escape ()
  "Send <escape> to the Claude Code REPL.

This is useful for saying \"No\" when Claude asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-claude-buffer)))
      (with-current-buffer claude-code-buffer
        (eat-term-send-string eat-terminal (kbd "ESC"))
        (display-buffer claude-code-buffer))
    (error "Claude is not running")))

;;;###autoload
(defun claude-code-fix-error-at-point (&optional arg)
  "Ask Claude to fix the error at point.

Gets the error message, file name, and line number, and instructs Claude
to fix the error. Supports both flycheck and flymake error systems, as well
as any system that implements help-at-pt.

With prefix ARG, switch to the Claude buffer after sending."
  (interactive "P")
  ;; (unless (bound-and-true-p flycheck-mode)
  ;;   (message "Flycheck mode is not enabled in this buffer.")
  ;;   (cl-return-from claude-code-fix-error-at-point))
  
  (let* ((error-text (claude-code--format-errors-at-point))
         (file-name (when (buffer-file-name)
                      (file-relative-name (buffer-file-name) (project-root (project-current t))))))
    (if (string= error-text "No errors at point")
        (message "No errors found at point")
      (let ((command (format "Fix this error in %s:\nDo not run any external linter or other program, just fix the error at point using the context provided in the error message: <%s>"
                             file-name error-text)))
        (claude-code--do-send-command command)
        (when arg
          (claude-code--switch-to-claude-buffer))))))

;;;###autoload
(defun claude-code-read-only-mode ()
  "Enter read-only mode in Claude buffer with visible cursor.

In this mode, you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Claude
buffer. However, you are not allowed to change the buffer contents or
enter Claude commands.

Use `claude-code-exit-read-only-mode' to switch back to normal mode."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-claude-buffer)))
      (with-current-buffer claude-code-buffer
        (eat-emacs-mode)
        (setq-local eat-invisible-cursor-type claude-code-read-only-mode-cursor-type)
        (eat--set-cursor nil :invisible)
        (message "Claude read-only mode enabled"))
    (error "Claude is not running")))

;;;###autoload
(defun claude-code-exit-read-only-mode ()
  "Exit read-only mode and return to normal mode (eat semi-char mode)."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-claude-buffer)))
      (with-current-buffer claude-code-buffer
        (eat-semi-char-mode)
        (setq-local eat-invisible-cursor-type nil)
        (eat--set-cursor nil :invisible)
        (message "Claude semi-char mode enabled"))
    (error "Claude is not running")))

;;;###autoload
(defun claude-code-toggle-read-only-mode ()
  "Toggle between read-only mode and normal mode.

In read-only mode you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Claude
buffer. However, you are not allowed to change the buffer contents or
enter Claude commands."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-claude-buffer)))
      (with-current-buffer claude-code-buffer
        (if eat--semi-char-mode
            (claude-code-read-only-mode)
          (claude-code-exit-read-only-mode)))
    (error "Claude is not running")))

;;;; Mode definition
;;;###autoload
(define-minor-mode claude-code-mode
  "Minor mode for interacting with Claude AI CLI.

When enabled, provides functionality for starting, sending commands to,
and managing Claude sessions."
  :init-value nil
  :lighter " Claude"
  :global t
  :group 'claude-code)

;;;; Provide the feature
(provide 'claude-code)

;;; claude-code.el ends here
