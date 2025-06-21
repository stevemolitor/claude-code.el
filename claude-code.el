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
;;
;; This package supports both eat and vterm terminal backends.  The eat
;; backend is the default and is required.  The vterm backend is optional
;; and requires the vterm package to be installed separately.

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
(declare-function eat-term-live-p "eat" (terminal))

;;;; Customization options
(defgroup claude-code nil
  "Claude AI interface for Emacs."
  :group 'tools)

(defface claude-code-repl-face
  nil
  "Face for Claude REPL."
  :group 'claude-code)

(defcustom claude-code-terminal-backend 'eat
  "Terminal backend to use for Claude Code.

Can be either \\='eat or \\='vterm.  The eat terminal is the default
and original backend.  The vterm backend requires the vterm
package to be installed."
  :type '(choice (const :tag "Eat terminal" eat)
                 (const :tag "Vterm terminal" vterm))
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

(defcustom claude-code-large-buffer-threshold 100000
  "Size threshold in characters above which buffers are considered \"large\".

When sending a buffer to Claude with `claude-code-send-region` and no
region is active, prompt for confirmation if buffer size exceeds this value."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-program "claude"
  "Program to run when starting Claude.

For eat backend: passed as the PROGRAM parameter to `eat-make`.
For vterm backend: set as the shell to run in vterm."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-program-switches nil
  "List of command line switches to pass to the Claude program.

For eat backend: passed as SWITCHES parameters to `eat-make`.
For vterm backend: handled differently as vterm doesn't support
switches in the same way."
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

(defcustom claude-code-never-truncate-claude-buffer nil
  "When non-nil, disable truncation of Claude output buffer.

By default, Eat will truncate the terminal scrollback buffer when it
reaches a certain size.  This can cause Claude's output to be cut off
when dealing with large responses.  Setting this to non-nil disables
the scrollback size limit, allowing Claude to output unlimited content
without truncation.

Note: Disabling truncation may consume more memory for very large
outputs."
  :type 'boolean
  :group 'claude-code)

;; Forward declare variables to avoid compilation warnings
(defvar eat-terminal)
(defvar eat-term-name)
(defvar eat-invisible-cursor-type)
(declare-function eat-term-send-string "eat")
(declare-function eat-kill-process "eat")
(declare-function eat-make "eat")
(declare-function eat-emacs-mode "eat")
(declare-function eat-semi-char-mode "eat")

;; Forward declare vterm functions
(defvar vterm-shell)
(defvar vterm-max-scrollback)
(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-send-escape "vterm")
(declare-function vterm-send-tab "vterm")
(declare-function vterm-send-backspace "vterm")
(declare-function vterm-send-key "vterm")
(declare-function vterm-copy-mode "vterm")

;; Forward declare flycheck functions
(declare-function flycheck-overlay-errors-at "flycheck")
(declare-function flycheck-error-filename "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-message "flycheck")

;;;; Internal state variables
(defvar claude-code--directory-buffer-map (make-hash-table :test 'equal)
  "Hash table mapping directories to user-selected Claude buffers.
Keys are directory paths, values are buffer objects.
This allows remembering which Claude instance the user selected
for each directory across multiple invocations.")

;;;; Key bindings
;;;###autoload (autoload 'claude-code-command-map "claude-code")
(defvar claude-code-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "/" 'claude-code-slash-commands)
    (define-key map "b" 'claude-code-switch-to-buffer)
    (define-key map "c" 'claude-code)
    (define-key map "e" 'claude-code-fix-error-at-point)
    (define-key map "k" 'claude-code-kill)
    (define-key map "m" 'claude-code-transient)
    (define-key map "n" 'claude-code-send-escape)
    (define-key map "f" 'claude-code-fork)
    (define-key map "r" 'claude-code-send-region)
    (define-key map "s" 'claude-code-send-command)
    (define-key map "t" 'claude-code-toggle)
    (define-key map "x" 'claude-code-send-command-with-context)
    (define-key map "y" 'claude-code-send-return)
    (define-key map "z" 'claude-code-toggle-read-only-mode)
    (define-key map "1" 'claude-code-send-1)
    (define-key map "2" 'claude-code-send-2)
    (define-key map "3" 'claude-code-send-3)
    (define-key map [tab] 'claude-code-cycle-mode)
    map)
  "Keymap for Claude commands.")

;;;; Transient Menus
;;;###autoload (autoload 'claude-code-transient "claude-code" nil t)
(transient-define-prefix claude-code-transient ()
  "Claude command menu."
  ["Claude Commands"
   ["Manage Claude" ("c" "Start Claude" claude-code)
    ("t" "Toggle claude window" claude-code-toggle)
    ("b" "Switch to Claude buffer" claude-code-switch-to-buffer)
    ("k" "Kill Claude" claude-code-kill)
    ("z" "Toggle read-only mode" claude-code-toggle-read-only-mode)]
   ["Send Commands to Claude" ("s" "Send command" claude-code-send-command)
    ("x" "Send command with context" claude-code-send-command-with-context)
    ("r" "Send region or buffer" claude-code-send-region)
    ("e" "Fix error at point" claude-code-fix-error-at-point)
    ("f" "Fork (jump to previous conversation" claude-code-fork)
    ("/" "Slash Commands" claude-code-slash-commands)]
   ["Quick Responses" ("y" "Send <return> (\"Yes\")" claude-code-send-return)
    ("n" "Send <escape> (\"No\")" claude-code-send-escape)
    ("1" "Send \"1\"" claude-code-send-1)
    ("2" "Send \"2\"" claude-code-send-2)
    ("3" "Send \"3\"" claude-code-send-3)
    ("TAB" "Cycle Claude mode" claude-code-cycle-mode)]])


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
(defun claude-code--validate-terminal-backend ()
  "Validate that the selected terminal backend is available.

Returns t if the backend is available, nil otherwise.
For eat backend, checks if eat is available.
For vterm backend, checks if vterm is available."
  (pcase claude-code-terminal-backend
    ('eat (featurep 'eat))
    ('vterm (featurep 'vterm))
    (_ (error "Invalid terminal backend: %s" claude-code-terminal-backend))))

(defun claude-code--ensure-terminal-backend ()
  "Ensure the selected terminal backend is available.

Attempts to load the selected backend if not yet loaded.
Raises an error if the backend cannot be loaded."
  (pcase claude-code-terminal-backend
    ('eat
     (unless (featurep 'eat)
       (condition-case nil
           (require 'eat)
         (error "Failed to load eat terminal backend"))))
    ('vterm
     (unless (featurep 'vterm)
       (condition-case nil
           (require 'vterm)
         (error
          (error "Vterm backend selected but vterm package is not installed. Please install vterm or switch to eat backend")))))
    (_ (error "Invalid terminal backend: %s" claude-code-terminal-backend))))

;;;; Terminal Backend Abstraction Layer

(defun claude-code--term-in-read-only-mode-p ()
  "Check if the terminal is currently in read-only mode.

This function checks the backend-specific state to determine
if the terminal is in read-only/copy mode."
  (pcase claude-code-terminal-backend
    ('eat (not eat--semi-char-mode))
    ('vterm (and (boundp 'vterm-copy-mode) vterm-copy-mode))))

(defun claude-code--term-setup-buffer ()
  "Setup terminal buffer based on the configured backend.

This function dispatches to the appropriate backend-specific
setup function."
  (pcase claude-code-terminal-backend
    ('eat (claude-code--eat-setup-buffer))
    ('vterm (claude-code--vterm-setup-buffer))))

(defun claude-code--term-make (buffer-name program switches)
  "Create a terminal using the configured backend.

BUFFER-NAME is the name for the terminal buffer.
PROGRAM is the program to run.
SWITCHES are command line arguments for the program.

This function dispatches to the appropriate backend-specific
implementation based on `claude-code-terminal-backend'."
  (claude-code--ensure-terminal-backend)
  (pcase claude-code-terminal-backend
    ('eat (claude-code--eat-make buffer-name program switches))
    ('vterm (claude-code--vterm-make buffer-name program switches))))

(defun claude-code--term-send-string (string)
  "Send STRING to the terminal.

This function dispatches to the appropriate backend-specific
implementation based on `claude-code-terminal-backend'."
  (pcase claude-code-terminal-backend
    ('eat (claude-code--eat-send-string string))
    ('vterm (claude-code--vterm-send-string string))))

(defun claude-code--term-send-key (key)
  "Send KEY to the terminal.

KEY should be a string like \"<escape>\" or \"<return>\".

This function dispatches to the appropriate backend-specific
implementation based on `claude-code-terminal-backend'."
  (pcase claude-code-terminal-backend
    ('eat (claude-code--eat-send-key key))
    ('vterm (claude-code--vterm-send-key key))))

(defun claude-code--term-kill-process ()
  "Kill the terminal process.

This function dispatches to the appropriate backend-specific
implementation based on `claude-code-terminal-backend'."
  (pcase claude-code-terminal-backend
    ('eat (claude-code--eat-kill-process))
    ('vterm (claude-code--vterm-kill-process))))

(defun claude-code--term-enter-read-only-mode ()
  "Enter read-only/copy mode in the terminal.

This function dispatches to the appropriate backend-specific
implementation based on `claude-code-terminal-backend'."
  (pcase claude-code-terminal-backend
    ('eat (claude-code--eat-enter-read-only-mode))
    ('vterm (claude-code--vterm-enter-read-only-mode))))

(defun claude-code--term-exit-read-only-mode ()
  "Exit read-only/copy mode in the terminal.

This function dispatches to the appropriate backend-specific
implementation based on `claude-code-terminal-backend'."
  (pcase claude-code-terminal-backend
    ('eat (claude-code--eat-exit-read-only-mode))
    ('vterm (claude-code--vterm-exit-read-only-mode))))

;;;; Eat Backend Implementation

(defun claude-code--eat-setup-buffer ()
  "Setup eat-specific buffer settings."
  (setq-local eat-term-name claude-code-term-name)
  ;; Turn off shell integration, as we don't need it for Claude
  (setq-local eat-enable-directory-tracking t
              eat-enable-shell-command-history nil
              eat-enable-shell-prompt-annotation nil)
  ;; Conditionally disable scrollback truncation
  (when claude-code-never-truncate-claude-buffer
    (setq-local eat-term-scrollback-size nil))
  ;; Add advice to only notify claude on window width changes
  (advice-add 'eat--adjust-process-window-size :around #'claude-code--eat-adjust-process-window-size-advice)
  ;; Set our custom synchronize scroll function
  (setq-local eat--synchronize-scroll-function #'claude-code--synchronize-scroll))

(defun claude-code--eat-make (buffer-name program switches)
  "Create an eat terminal.

BUFFER-NAME is the name for the terminal buffer.
PROGRAM is the program to run.
SWITCHES are command line arguments for the program."
  (apply #'eat-make buffer-name program nil switches))

(defun claude-code--eat-send-string (string)
  "Send STRING to the eat terminal."
  (when eat-terminal
    (eat-term-send-string eat-terminal string)
    (eat-term-send-string eat-terminal (kbd "RET"))))

(defun claude-code--eat-send-key (key)
  "Send KEY to the eat terminal."
  (when eat-terminal
    (eat-term-send-string eat-terminal (kbd key))))

(defun claude-code--eat-kill-process ()
  "Kill the eat terminal process."
  (eat-kill-process))

(defun claude-code--eat-enter-read-only-mode ()
  "Enter read-only mode in eat terminal."
  (eat-emacs-mode)
  (setq-local eat-invisible-cursor-type claude-code-read-only-mode-cursor-type)
  (eat--set-cursor nil :invisible))

(defun claude-code--eat-exit-read-only-mode ()
  "Exit read-only mode in eat terminal."
  (eat-semi-char-mode)
  (setq-local eat-invisible-cursor-type nil)
  (eat--set-cursor nil :invisible))

;;;; Vterm Backend Implementation

(defun claude-code--vterm-window-size-change (_frame)
  "Handle window size changes for vterm buffers.
  
This function is called when window size changes. It ensures vterm
properly adjusts to the new window dimensions. The _FRAME argument
is ignored as we operate on the current buffer."
  (when (and (eq claude-code-terminal-backend 'vterm)
             (derived-mode-p 'vterm-mode))
    ;; Force vterm to recalculate window size
    ;; vterm has its own window size adjustment function
    (when (fboundp 'vterm--window-adjust-process-window-size)
      (vterm--window-adjust-process-window-size))))

(defun claude-code--vterm-setup-buffer ()
  "Setup vterm-specific buffer settings."
  ;; Vterm doesn't need as much setup as eat
  ;; Most terminal behavior is handled internally by vterm
  (setq-local vterm-max-scrollback (if claude-code-never-truncate-claude-buffer
                                        1000000  ; Very large scrollback
                                      vterm-max-scrollback))
  ;; Disable line wrapping to prevent terminal UI corruption in narrow windows
  (setq-local truncate-lines t)
  (setq-local truncate-partial-width-windows nil)
  ;; Prevent vterm from automatically renaming the buffer
  (setq-local vterm-buffer-name-string nil)
  ;; Add window size change hook for vterm to handle resizing
  (add-hook 'window-size-change-functions #'claude-code--vterm-window-size-change nil t))

(defun claude-code--vterm-make (buffer-name program switches)
  "Create a vterm terminal.

BUFFER-NAME is the name for the terminal buffer.
PROGRAM is the program to run.
SWITCHES are command line arguments for the program."
  ;; Store the current buffer (which has our desired name)
  (let ((original-buffer-name (buffer-name)))
    ;; vterm starts a shell, so we need to construct a command to run claude
    (vterm-mode)
    ;; Restore our desired buffer name (vterm-mode may have changed it)
    (rename-buffer original-buffer-name t)
    ;; Run the program with switches
    (let ((command (if switches
                       (concat program " " (mapconcat #'identity switches " "))
                     program)))
      (vterm-send-string command)
      (vterm-send-return))))

(defun claude-code--vterm-send-string (string)
  "Send STRING to the vterm terminal."
  (vterm-send-string string)
  (vterm-send-return))

(defun claude-code--vterm-send-key (key)
  "Send KEY to the vterm terminal."
  (pcase key
    ("<escape>" (vterm-send-escape))
    ("<return>" (vterm-send-return))
    ("<tab>" (vterm-send-tab))
    ("<backspace>" (vterm-send-backspace))
    (_ (vterm-send-key key))))

(defun claude-code--vterm-kill-process ()
  "Kill the vterm terminal process."
  ;; vterm doesn't have a direct kill-process function
  ;; We need to kill the buffer's process
  (when-let ((proc (get-buffer-process (current-buffer))))
    (kill-process proc)))

(defun claude-code--vterm-enter-read-only-mode ()
  "Enter read-only mode in vterm terminal."
  (vterm-copy-mode 1))

(defun claude-code--vterm-exit-read-only-mode ()
  "Exit read-only mode in vterm terminal."
  (vterm-copy-mode -1))

(defun claude-code--directory ()
  "Get get the root Claude directory for the current buffer.

If not in a project and no buffer file return `default-directory'."
  (let* ((project (project-current))
         (current-file (buffer-file-name)))
    (cond
     ;; Case 1: In a project
     (project (project-root project))
     ;; Case 2: Has buffer file (when not in VC repo)
     (current-file (file-name-directory current-file))
     ;; Case 3: No project and no buffer file
     (t default-directory))))

(defun claude-code--find-all-claude-buffers ()
  "Find all active Claude buffers across all directories.

Returns a list of buffer objects."
  (cl-remove-if-not
   (lambda (buf)
     (string-match-p "^\\*claude:" (buffer-name buf)))
   (buffer-list)))

(defun claude-code--find-claude-buffers-for-directory (directory)
  "Find all active Claude buffers for a specific DIRECTORY.

Returns a list of buffer objects."
  (cl-remove-if-not
   (lambda (buf)
     (let ((buf-dir (claude-code--extract-directory-from-buffer-name (buffer-name buf))))
       (and buf-dir
            (string= (file-truename (abbreviate-file-name directory))
                     (file-truename buf-dir)))))
   (claude-code--find-all-claude-buffers)))

(defun claude-code--extract-directory-from-buffer-name (buffer-name)
  "Extract the directory path from a Claude BUFFER-NAME.

For example, *claude:/path/to/project/* returns /path/to/project/.
For example, *claude:/path/to/project/:tests* returns /path/to/project/."
  (when (string-match "^\\*claude:\\([^:]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (match-string 1 buffer-name)))

(defun claude-code--extract-instance-name-from-buffer-name (buffer-name)
  "Extract the instance name from a Claude BUFFER-NAME.

For example, *claude:/path/to/project/:tests* returns \"tests\".
For example, *claude:/path/to/project/* returns nil."
  (when (string-match "^\\*claude:\\([^:]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (match-string 2 buffer-name)))

(defun claude-code--buffer-display-name (buffer)
  "Create a display name for Claude BUFFER.

Returns a formatted string like `project:instance (directory)' or
`project (directory)'."
  (let* ((name (buffer-name buffer))
         (dir (claude-code--extract-directory-from-buffer-name name))
         (instance-name (claude-code--extract-instance-name-from-buffer-name name)))
    (if instance-name
        (format "%s:%s (%s)"
                (file-name-nondirectory (directory-file-name dir))
                instance-name
                dir)
      (format "%s (%s)"
              (file-name-nondirectory (directory-file-name dir))
              dir))))

(defun claude-code--buffers-to-choices (buffers &optional simple-format)
  "Convert BUFFERS list to an alist of (display-name . buffer) pairs.

If SIMPLE-FORMAT is non-nil, use just the instance name as display name."
  (mapcar (lambda (buf)
            (let ((display-name (if simple-format
                                    (or (claude-code--extract-instance-name-from-buffer-name
                                         (buffer-name buf))
                                        "default")
                                  (claude-code--buffer-display-name buf))))
              (cons display-name buf)))
          buffers))

(defun claude-code--select-buffer-from-choices (prompt buffers &optional simple-format)
  "Prompt user to select a buffer from BUFFERS list using PROMPT.

If SIMPLE-FORMAT is non-nil, use simplified display names.
Returns the selected buffer or nil."
  (when buffers
    (let* ((choices (claude-code--buffers-to-choices buffers simple-format))
           (selection (completing-read prompt
                                       (mapcar #'car choices)
                                       nil t)))
      (cdr (assoc selection choices)))))

(defun claude-code--prompt-for-claude-buffer ()
  "Prompt user to select from available Claude buffers.

Returns the selected buffer or nil if canceled. If a buffer is selected,
it's remembered for the current directory."
  (let* ((current-dir (claude-code--directory))
         (claude-buffers (claude-code--find-all-claude-buffers)))
    (when claude-buffers
      (let* ((prompt (substitute-command-keys
                      (format "No Claude instance running in %s. Cancel (\\[keyboard-quit]), or select Claude instance: "
                              (abbreviate-file-name current-dir))))
             (selected-buffer (claude-code--select-buffer-from-choices prompt claude-buffers)))
        ;; Remember the selection for this directory
        (when selected-buffer
          (puthash current-dir selected-buffer claude-code--directory-buffer-map))
        selected-buffer))))

(defun claude-code--get-or-prompt-for-buffer ()
  "Get Claude buffer for current directory or prompt for selection.

First checks for Claude buffers in the current directory. If there are
multiple, prompts the user to select one. If there are none, checks if
there's a remembered selection for this directory. If not, and there are
other Claude buffers running, prompts the user to select one. Returns
the buffer or nil."
  (let* ((current-dir (claude-code--directory))
         (dir-buffers (claude-code--find-claude-buffers-for-directory current-dir)))
    (cond
     ;; Multiple buffers for this directory - prompt for selection
     ((> (length dir-buffers) 1)
      (claude-code--select-buffer-from-choices
       (format "Select Claude instance for %s: "
               (abbreviate-file-name current-dir))
       dir-buffers
       t))  ; Use simple format (just instance names)
     ;; Single buffer for this directory - use it
     ((= (length dir-buffers) 1)
      (car dir-buffers))
     ;; No buffers for this directory - check remembered or prompt for other directories
     (t
      ;; Check for remembered selection for this directory
      (let ((remembered-buffer (gethash current-dir claude-code--directory-buffer-map)))
        (if (and remembered-buffer (buffer-live-p remembered-buffer))
            remembered-buffer
          ;; No valid remembered buffer, check for other Claude instances
          (let ((other-buffers (claude-code--find-all-claude-buffers)))
            (when other-buffers
              (claude-code--prompt-for-claude-buffer)))))))))

(defun claude-code--switch-to-selected-buffer (selected-buffer)
  "Switch to SELECTED-BUFFER if it's not the current buffer.

This is used after command functions to ensure we switch to the
selected Claude buffer when the user chose a different instance."
  (when (and selected-buffer
             (not (eq selected-buffer (current-buffer))))
    (switch-to-buffer selected-buffer)))

(defun claude-code--buffer-name (&optional instance-name)
  "Generate the Claude buffer name based on project or current buffer file.

If INSTANCE-NAME is provided, include it in the buffer name.
If not in a project and no buffer file, raise an error."
  (let ((dir (claude-code--directory)))
    (if dir
        (if instance-name
            (format "*claude:%s:%s*" (abbreviate-file-name (file-truename dir)) instance-name)
          (format "*claude:%s*" (abbreviate-file-name (file-truename dir))))
      (error "Cannot determine Claude directory - no `default-directory'!"))))

(defun claude-code--show-not-running-message ()
  "Show a message that Claude is not running in any directory."
  (message "Claude is not running"))

(defun claude-code--kill-buffer (buffer)
  "Kill a Claude BUFFER by cleaning up hooks and processes.

This function handles the proper cleanup sequence for a Claude buffer:
1. Remove the window configuration change hook
2. Kill the terminal process
3. Kill the buffer without prompting"
  (with-current-buffer buffer
    (remove-hook 'window-configuration-change-hook #'claude-code--on-window-configuration-change t)
    ;; Kill the process first
    (claude-code--term-kill-process)
    ;; Set buffer-modified-p to nil to avoid save prompts
    (set-buffer-modified-p nil)
    ;; Let kill-buffer-query-functions skip process check
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun claude-code--cleanup-directory-mapping ()
  "Remove entries from directory-buffer map when this buffer is killed.

This function is added to `kill-buffer-hook' in Claude buffers to clean up
the remembered directory->buffer associations."
  (let ((dying-buffer (current-buffer)))
    (maphash (lambda (dir buffer)
               (when (eq buffer dying-buffer)
                 (remhash dir claude-code--directory-buffer-map)))
             claude-code--directory-buffer-map)))

(defun claude-code--get-buffer-file-name ()
  "Get the file name associated with the current buffer."
  (when buffer-file-name
    (file-truename buffer-file-name)))

(defun claude-code--do-send-command (cmd)
  "Send a command CMD to Claude if Claude buffer exists.

After sending the command, move point to the end of the buffer.
Returns the selected Claude buffer or nil."
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (progn
        (with-current-buffer claude-code-buffer
          (claude-code--term-send-string cmd)
          (display-buffer claude-code-buffer))
        claude-code-buffer)
    (claude-code--show-not-running-message)
    nil))

(defun claude-code--setup-repl-faces ()
  "Setup faces for the Claude REPL buffer.

Applies the `claude-code-repl-face' to all terminal-related faces
for consistent appearance."
  (pcase claude-code-terminal-backend
    ('eat
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
     (face-remap-add-relative 'eat-term-faint :foreground "#999999" :weight 'light))
    ('vterm
     ;; vterm uses standard Emacs faces for colors
     (dolist (face '(vterm-color-black
                     vterm-color-red
                     vterm-color-green
                     vterm-color-yellow
                     vterm-color-blue
                     vterm-color-magenta
                     vterm-color-cyan
                     vterm-color-white))
       (when (facep face)
         (funcall 'face-remap-add-relative face :inherit 'claude-code-repl-face)))))
  ;; Common face setup for both backends
  (buffer-face-set :inherit 'claude-code-repl-face)
  (face-remap-add-relative 'nobreak-space :underline nil))

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
      ;; keep the prompt at the bottom of the window when possible.
      ;; Don't move the cursor around though when in eat-emacs-mode
      (when (not buffer-read-only)
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
            (set-window-start window term-beginning)))))))

(defun claude-code--on-window-configuration-change ()
  "Handle window configuration change for Claude buffers.

Ensure all Claude buffers stay scrolled to the bottom when window
configuration changes (e.g., when minibuffer opens/closes)."
  (dolist (claude-buffer (claude-code--find-all-claude-buffers))
    (with-current-buffer claude-buffer
      ;; Get all windows showing this Claude buffer
      (when-let ((windows (get-buffer-window-list claude-buffer nil t)))
        (claude-code--synchronize-scroll windows)))))

(defvar claude-code--window-widths (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping windows to their last known widths.")

(defun claude-code--eat-adjust-process-window-size-advice (orig-fun &rest args)
  "Advice for `eat--adjust-process-window-size' to only signal on width change.

Returns the size returned by ORIG-FUN only when the width of any Claude
window has changed, not when only the height has changed. This prevents
unnecessary terminal reflows when only vertical space changes.

ARGS is passed to ORIG-FUN unchanged."
  (when (and eat-terminal (eat-term-live-p eat-terminal))
      ;; Call the original function first
      (let ((result (apply orig-fun args)))
        ;; Check all windows for Claude buffers
        (let ((width-changed nil))
          (dolist (window (window-list))
            (let ((buffer (window-buffer window)))
              (when (and buffer (string-match-p "^\\*claude" (buffer-name buffer)))
                (let ((current-width (window-width window))
                      (stored-width (gethash window claude-code--window-widths)))
                  ;; Check if this is a new window or if width changed
                  (when (or (not stored-width) (/= current-width stored-width))
                    (setq width-changed t)
                    ;; Update stored width
                    (puthash window current-width claude-code--window-widths))))))
          ;; Return result only if a Claude window width changed, otherwise nil
          (if width-changed result nil)))))

(defun claude-code (&optional arg)
  "Start Claude in a terminal and enable `claude-code-mode'.

If current buffer belongs to a project start Claude in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
buffer file.

With single prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), continue previous conversation.
With triple prefix ARG (\\[universal-argument] \\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (interactive "P")

  ;; Ensure terminal backend is available
  (claude-code--ensure-terminal-backend)

  (let* ((dir (if (equal arg '(64))  ; Triple prefix
                  (read-directory-name "Project directory: ")
                (claude-code--directory)))
         (abbreviated-dir (abbreviate-file-name dir))
         (continue (equal arg '(16))) ; Double prefix
         (switch-after (equal arg '(4))) ; Single prefix
         (default-directory dir)
         ;; Check for existing Claude instances in this directory
         (existing-buffers (claude-code--find-claude-buffers-for-directory dir))
         ;; Determine instance name
         (instance-name (if existing-buffers
                            (read-string (format "Instances already running for %s, new instance name (existing: %s): "
                                                 abbreviated-dir
                                                 (mapconcat (lambda (buf)
                                                              (or (claude-code--extract-instance-name-from-buffer-name
                                                                   (buffer-name buf))
                                                                  "default"))
                                                            existing-buffers ", ")))
                          "default"))
         (buffer-name (claude-code--buffer-name instance-name))
         (trimmed-buffer-name (string-trim-right (string-trim buffer-name "\\*") "\\*"))
         (buffer (get-buffer-create buffer-name))
         (program-switches (if continue
                               (append claude-code-program-switches '("--continue"))
                             claude-code-program-switches)))
    ;; Start the terminal process
    (with-current-buffer buffer
      (cd dir)
      
      ;; Setup backend-specific buffer settings
      (claude-code--term-setup-buffer)

      (let ((process-adaptive-read-buffering nil))
        (condition-case nil
            (claude-code--term-make trimmed-buffer-name claude-code-program program-switches)
          (error
           (error "error starting claude")
           (signal 'claude-start-error "error starting claude"))))

      ;; Set terminal faces to inherit from claude-code-repl-face
      (claude-code--setup-repl-faces)

      ;; Add window configuration change hook to keep buffer scrolled to bottom
      (add-hook 'window-configuration-change-hook #'claude-code--on-window-configuration-change nil t)

      ;; fix wonky initial terminal layout that happens sometimes if we show the buffer before claude is ready
      ;; Only needed for eat terminal backend
      (when (eq claude-code-terminal-backend 'eat)
        (sleep-for claude-code-startup-delay))

      ;; Add cleanup hook to remove directory mappings when buffer is killed
      (add-hook 'kill-buffer-hook #'claude-code--cleanup-directory-mapping nil t)

      ;; run start hooks and show the claude buffer
      (run-hooks 'claude-code-start-hook)
      (display-buffer buffer))
    (when switch-after
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
                  ((equal arg '(4))     ; C-u
                   (read-string "Instructions for Claude: "))
                  ((equal arg '(16))    ; C-u C-u
                   (read-string "Instructions for Claude: "))
                  (t nil)))
         (full-text (if prompt
                        (format "%s\n\n%s" prompt text)
                      text)))
    (when full-text
      (let ((selected-buffer (claude-code--do-send-command full-text)))
        (when (and (equal arg '(16)) selected-buffer)  ; Only switch buffer with C-u C-u
          (switch-to-buffer selected-buffer))))))

;;;###autoload
(defun claude-code-toggle ()
  "Show or hide the Claude window.

If the Claude buffer doesn't exist, create it."
  (interactive)
  (let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
    (if claude-code-buffer
        (if (get-buffer-window claude-code-buffer)
            (delete-window (get-buffer-window claude-code-buffer))
          (display-buffer claude-code-buffer))
      (claude-code--show-not-running-message))))

;;;###autoload
(defun claude-code-switch-to-buffer (&optional arg)
  "Switch to the Claude buffer if it exists.

With prefix ARG, show all Claude instances across all directories."
  (interactive "P")
  (if arg
      ;; With prefix arg, show all Claude instances
      (let ((all-buffers (claude-code--find-all-claude-buffers)))
        (cond
         ((null all-buffers)
          (claude-code--show-not-running-message))
         ((= (length all-buffers) 1)
          ;; Only one buffer, just switch to it
          (switch-to-buffer (car all-buffers)))
         (t
          ;; Multiple buffers, let user choose
          (let ((selected-buffer (claude-code--select-buffer-from-choices
                                  "Select Claude instance: "
                                  all-buffers)))
            (when selected-buffer
              (switch-to-buffer selected-buffer))))))
    ;; Without prefix arg, use normal behavior
    (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
        (switch-to-buffer claude-code-buffer)
      (claude-code--show-not-running-message))))

;;;###autoload
(defun claude-code-kill (&optional arg)
  "Kill Claude process and close its window.

With prefix ARG, kill ALL Claude processes across all directories."
  (interactive "P")
  (if arg
      ;; Kill all Claude instances
      (let ((all-buffers (claude-code--find-all-claude-buffers)))
        (if all-buffers
            (let* ((buffer-count (length all-buffers))
                   (plural-suffix (if (= buffer-count 1) "" "s")))
              (when (yes-or-no-p (format "Kill %d Claude instance%s? " buffer-count plural-suffix))
                (dolist (buffer all-buffers)
                  (claude-code--kill-buffer buffer))
                (message "%d Claude instance%s killed" buffer-count plural-suffix)))
          (claude-code--show-not-running-message)))
    ;; Kill single instance
    (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
        (when (yes-or-no-p "Kill Claude instance? ")
          (claude-code--kill-buffer claude-code-buffer)
          (message "Claude instance killed"))
      (claude-code--show-not-running-message))))

;;;###autoload
(defun claude-code-send-command (cmd &optional arg)
  "Read a Claude command from the minibuffer and send it.

With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (let ((selected-buffer (claude-code--do-send-command cmd)))
    (when (and arg selected-buffer)
      (switch-to-buffer selected-buffer))))

;;;###autoload
(defun claude-code-send-command-with-context (cmd &optional arg)
  "Read a Claude command and send it with current file and line context.

If region is active, include region line numbers.
With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (let* ((file-name (claude-code--get-buffer-file-name))
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
    (let ((selected-buffer (claude-code--do-send-command cmd-with-context)))
      (when (and arg selected-buffer)
        (switch-to-buffer selected-buffer)))))

;;;###autoload
(defun claude-code-send-return ()
  "Send <return> to the Claude Code REPL.

This is useful for saying Yes when Claude asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (claude-code--do-send-command ""))

;;;###autoload
(defun claude-code-send-1 ()
  "Send \"1\" to the Claude Code REPL.

This selects the first option when Claude presents a numbered menu."
  (interactive)
  (claude-code--do-send-command "1"))

;;;###autoload
(defun claude-code-send-2 ()
  "Send \"2\" to the Claude Code REPL.

This selects the second option when Claude presents a numbered menu."
  (interactive)
  (claude-code--do-send-command "2"))

;;;###autoload
(defun claude-code-send-3 ()
  "Send \"3\" to the Claude Code REPL.

This selects the third option when Claude presents a numbered menu."
  (interactive)
  (claude-code--do-send-command "3"))

;;;###autoload
(defun claude-code-send-escape ()
  "Send <escape> to the Claude Code REPL.

This is useful for saying \"No\" when Claude asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (claude-code--term-send-key "<escape>")
        (display-buffer claude-code-buffer))
    (claude-code--show-not-running-message)))

;;;###autoload
(defun claude-code-cycle-mode ()
  "Send Shift-Tab to Claude to cycle between modes.

Claude uses Shift-Tab to cycle through:
- Default mode
- Auto-accept edits mode
- Plan mode"
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (claude-code--term-send-key "\e[Z")
        (display-buffer claude-code-buffer))
    (claude-code--show-not-running-message)))

(defun claude-code-fork ()
  "Jump to a previous conversation by invoking the Claude fork command.

Sends <escape><escape> to the Claude Code REPL."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (claude-code--send-key "")
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
  (let* ((error-text (claude-code--format-errors-at-point))
         (file-name (claude-code--get-buffer-file-name)))
    (if (string= error-text "No errors at point")
        (message "No errors found at point")
      (let ((command (format "Fix this error in %s:\nDo not run any external linter or other program, just fix the error at point using the context provided in the error message: <%s>"
                             file-name error-text)))
        (let ((selected-buffer (claude-code--do-send-command command)))
          (when (and arg selected-buffer)
            (switch-to-buffer selected-buffer)))))))

;;;###autoload
(defun claude-code-read-only-mode ()
  "Enter read-only mode in Claude buffer with visible cursor.

In this mode, you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Claude
buffer. However, you are not allowed to change the buffer contents or
enter Claude commands.

Use `claude-code-exit-read-only-mode' to switch back to normal mode."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (claude-code--term-enter-read-only-mode)
        (message "Claude read-only mode enabled"))
    (claude-code--show-not-running-message)))

;;;###autoload
(defun claude-code-exit-read-only-mode ()
  "Exit read-only mode and return to normal mode (eat semi-char mode)."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (claude-code--term-exit-read-only-mode)
        (message "Claude semi-char mode enabled"))
    (claude-code--show-not-running-message)))

;;;###autoload
(defun claude-code-toggle-read-only-mode ()
  "Toggle between read-only mode and normal mode.

In read-only mode you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Claude
buffer. However, you are not allowed to change the buffer contents or
enter Claude commands."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (if (claude-code--term-in-read-only-mode-p)
            (claude-code-exit-read-only-mode)
          (claude-code-read-only-mode)))
    (claude-code--show-not-running-message)))

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
