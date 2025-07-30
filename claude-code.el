;;; claude-code.el --- Claude Code Emacs integration -*- lexical-binding: t; -*-

;; Author: Stephen Molitor <stevemolitor@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0") (transient "0.9.3"))
;; Keywords: tools, ai
;; URL: https://github.com/stevemolitor/claude-code.el

;;; Commentary:
;; An Emacs interface to Claude Code.  This package provides convenient
;; ways to interact with Claude from within Emacs, including sending
;; commands, toggling the Claude window, and accessing slash commands.

;;; Code:
;;;; Require dependencies
(require 'transient)
(require 'project)
(require 'cl-lib)
(require 'json)

;;;; Customization options
(defgroup claude-code nil
  "Claude AI interface for Emacs."
  :group 'tools)

(defgroup claude-code-eat nil
  "Eat terminal backend specific settings for Claude Code."
  :group 'claude-code)

(defgroup claude-code-vterm nil
  "Vterm terminal backend specific settings for Claude Code."
  :group 'claude-code)

(defgroup claude-code-window nil
  "Window management settings for Claude Code."
  :group 'claude-code)

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

(defvar claude-code-event-hook nil
  "Hook run when Claude Code CLI triggers events.
Functions in this hook are called with one argument: a plist with :type and
:buffer-name keys.  Use `add-hook' and `remove-hook' to manage this hook.")

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
This is passed as the PROGRAM parameter to `eat-make`."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-program-switches nil
  "List of command line switches to pass to the Claude program.
These are passed as SWITCHES parameters to `eat-make`."
  :type '(repeat string)
  :group 'claude-code)

(defcustom claude-code-newline-keybinding-style 'newline-on-shift-return
  "Key binding style for entering newlines and sending messages.

This controls how the return key and its modifiers behave in Claude buffers:
- \\='newline-on-shift-return: S-return enters a line break, RET sends the
  command (default)
- \\='newline-on-alt-return: M-return enters a line break, RET sends the command
- \\='shift-return-to-send: RET enters a line break, S-return sends the command
- \\='super-return-to-send: RET enters a line break, s-return sends the command

`\"S\"' is the shift key.
`\"s\"' is the hyper key, which is the COMMAND key on macOS."
  :type '(choice (const :tag "Newline on shift-return (s-return for newline, RET to send)" newline-on-shift-return)
                 (const :tag "Newline on alt-return (M-return for newline, RET to send)" newline-on-alt-return)
                 (const :tag "Shift-return to send (RET for newline, S-return to send)" shift-return-to-send)
                 (const :tag "Super-return to send (RET for newline, s-return to send)" super-return-to-send))
  :group 'claude-code)

(defcustom claude-code-enable-notifications t
  "Whether to show notifications when Claude finishes and awaits input."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-notification-function 'claude-code-default-notification
  "Function to call for notifications.

The function is called with two arguments:
- TITLE: Title of the notification
- MESSAGE: Body of the notification

You can set this to your own custom notification function.
The default function displays a message and pulses the modeline
to provide visual feedback when Claude is ready for input."
  :type 'function
  :group 'claude-code)

(defcustom claude-code-confirm-kill t
  "Whether to ask for confirmation before killing Claude instances.

When non-nil, claude-code-kill will prompt for confirmation.
When nil, Claude instances will be killed without confirmation."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-optimize-window-resize t
  "Whether to optimize terminal window resizing to prevent unnecessary reflows.

When non-nil, terminal reflows are only triggered when the window width
changes, not when only the height changes. This prevents unnecessary
terminal redraws when windows are split or resized vertically, improving
performance and reducing visual artifacts.

Set to nil if you experience issues with terminal display after window
resizing."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-terminal-backend 'eat
  "Terminal backend to use for Claude Code.
Choose between \\='eat (default) and \\='vterm terminal emulators."
  :type '(radio (const :tag "Eat terminal emulator" eat)
                (const :tag "Vterm terminal emulator" vterm))
  :group 'claude-code)

(defcustom claude-code-no-delete-other-windows nil
  "Whether to prevent Claude Code windows from being deleted.

When non-nil, claude-code will have the `no-delete-other-windows'
parameter.  This parameter prevents the claude-code window from
closing when calling `delete-other-windows' or any command that would
launch a new full-screen buffer."
  :type 'boolean
  :group 'claude-code-window)

;;;;; Eat terminal customizations
;; Eat-specific terminal faces
(defface claude-code-eat-prompt-annotation-running-face
  '((t :inherit eat-shell-prompt-annotation-running))
  "Face for running prompt annotations in Claude eat terminal."
  :group 'claude-code-eat)

(defface claude-code-eat-prompt-annotation-success-face
  '((t :inherit eat-shell-prompt-annotation-success))
  "Face for successful prompt annotations in Claude eat terminal."
  :group 'claude-code-eat)

(defface claude-code-eat-prompt-annotation-failure-face
  '((t :inherit eat-shell-prompt-annotation-failure))
  "Face for failed prompt annotations in Claude eat terminal."
  :group 'claude-code-eat)

(defface claude-code-eat-term-bold-face
  '((t :inherit eat-term-bold))
  "Face for bold text in Claude eat terminal."
  :group 'claude-code-eat)

(defface claude-code-eat-term-faint-face
  '((t :inherit eat-term-faint))
  "Face for faint text in Claude eat terminal."
  :group 'claude-code-eat)

(defface claude-code-eat-term-italic-face
  '((t :inherit eat-term-italic))
  "Face for italic text in Claude eat terminal."
  :group 'claude-code-eat)

(defface claude-code-eat-term-slow-blink-face
  '((t :inherit eat-term-slow-blink))
  "Face for slow blinking text in Claude eat terminal."
  :group 'claude-code-eat)

(defface claude-code-eat-term-fast-blink-face
  '((t :inherit eat-term-fast-blink))
  "Face for fast blinking text in Claude eat terminal."
  :group 'claude-code-eat)

(dotimes (i 10)
  (let ((face-name (intern (format "claude-code-eat-term-font-%d-face" i)))
        (eat-face (intern (format "eat-term-font-%d" i))))
    (eval `(defface ,face-name
             '((t :inherit ,eat-face))
             ,(format "Face for font %d in Claude eat terminal." i)
             :group 'claude-code-eat))))

(defcustom claude-code-eat-read-only-mode-cursor-type '(box nil nil)
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
  :group 'claude-code-eat)

(defcustom claude-code-eat-never-truncate-claude-buffer nil
  "When non-nil, disable truncation of Claude output buffer.

By default, Eat will truncate the terminal scrollback buffer when it
reaches a certain size.  This can cause Claude's output to be cut off
when dealing with large responses.  Setting this to non-nil disables
the scrollback size limit, allowing Claude to output unlimited content
without truncation.

Note: Disabling truncation may consume more memory for very large
outputs."
  :type 'boolean
  :group 'claude-code-eat)

(make-obsolete-variable 'claude-code-eat-never-truncate-claude-buffer
                        "Setting it to t can consume more memory for very large outputs and can cause performance issues with long Claude sessions"
                        "0.4.0")

;;;;; Vterm terminal customizations
(defcustom claude-code-vterm-buffer-multiline-output t
  "Whether to buffer vterm output to prevent flickering on multi-line input.

When non-nil, vterm output that appears to be redrawing multi-line
input boxes will be buffered briefly and processed in a single
batch. This prevents the flickering that can occur when Claude redraws
its input box as it expands to multiple lines.

This only affects the vterm backend."
  :type 'boolean
  :group 'claude-code-vterm)

(defcustom claude-code-vterm-multiline-delay 0.01
  "Delay in seconds before processing buffered vterm output.

This controls how long vterm waits to collect output before processing
it when `claude-code-vterm-buffer-multiline-output' is enabled.
The delay should be long enough to collect bursts of updates but short
enough to not be noticeable to the user.

The default value of 0.01 seconds (10ms) provides a good balance
between reducing flickering and maintaining responsiveness."
  :type 'number
  :group 'claude-code-vterm)

;;;; Forward declarations for flycheck
(declare-function flycheck-overlay-errors-at "flycheck")
(declare-function flycheck-error-filename "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-message "flycheck")

;;;; Forward declarations for server
(defvar server-eval-args-left)

;;;; Internal state variables
(defvar claude-code--directory-buffer-map (make-hash-table :test 'equal)
  "Hash table mapping directories to user-selected Claude buffers.
Keys are directory paths, values are buffer objects.
This allows remembering which Claude instance the user selected
for each directory across multiple invocations.")

(defvar claude-code--window-widths nil
  "Hash table mapping windows to their last known widths for eat terminals.")

;;;; Key bindings
;;;###autoload (autoload 'claude-code-command-map "claude-code")
(defvar claude-code-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "/") 'claude-code-slash-commands)
    (define-key map (kbd "b") 'claude-code-switch-to-buffer)
    (define-key map (kbd "B") 'claude-code-select-buffer)
    (define-key map (kbd "c") 'claude-code)
    (define-key map (kbd "C") 'claude-code-continue)
    (define-key map (kbd "R") 'claude-code-resume)
    (define-key map (kbd "i") 'claude-code-new-instance)
    (define-key map (kbd "d") 'claude-code-start-in-directory)
    (define-key map (kbd "e") 'claude-code-fix-error-at-point)
    (define-key map (kbd "k") 'claude-code-kill)
    (define-key map (kbd "K") 'claude-code-kill-all)
    (define-key map (kbd "m") 'claude-code-transient)
    (define-key map (kbd "n") 'claude-code-send-escape)
    (define-key map (kbd "f") 'claude-code-fork)
    (define-key map (kbd "r") 'claude-code-send-region)
    (define-key map (kbd "s") 'claude-code-send-command)
    (define-key map (kbd "t") 'claude-code-toggle)
    (define-key map (kbd "x") 'claude-code-send-command-with-context)
    (define-key map (kbd "y") 'claude-code-send-return)
    (define-key map (kbd "z") 'claude-code-toggle-read-only-mode)
    (define-key map (kbd "1") 'claude-code-send-1)
    (define-key map (kbd "2") 'claude-code-send-2)
    (define-key map (kbd "3") 'claude-code-send-3)
    (define-key map (kbd "M") 'claude-code-cycle-mode)
    (define-key map (kbd "o") 'claude-code-send-buffer-file)
    map)
  "Keymap for Claude commands.")

;;;; Transient Menus
;;;###autoload (autoload 'claude-code-transient "claude-code" nil t)
(transient-define-prefix claude-code-transient ()
  "Claude command menu."
  ["Claude Commands"
   ["Start/Stop Claude"
    ("c" "Start Claude" claude-code)
    ("d" "Start in directory" claude-code-start-in-directory)
    ("C" "Continue conversation" claude-code-continue)
    ("R" "Resume session" claude-code-resume)
    ("i" "New instance" claude-code-new-instance)
    ("k" "Kill Claude" claude-code-kill)
    ("K" "Kill all Claude instances" claude-code-kill-all)
    ]
   ["Send Commands to Claude"
    ("s" "Send command" claude-code-send-command)
    ("x" "Send command with context" claude-code-send-command-with-context)
    ("r" "Send region or buffer" claude-code-send-region)
    ("o" "Send buffer file" claude-code-send-buffer-file)
    ("e" "Fix error at point" claude-code-fix-error-at-point)
    ("f" "Fork conversation" claude-code-fork)
    ("/" "Slash Commands" claude-code-slash-commands)]
   ["Manage Claude"
    ("t" "Toggle claude window" claude-code-toggle)
    ("b" "Switch to Claude buffer" claude-code-switch-to-buffer)
    ("B" "Select from all Claude buffers" claude-code-select-buffer)
    ("z" "Toggle read-only mode" claude-code-toggle-read-only-mode)
    ("M" "Cycle Claude mode" claude-code-cycle-mode :transient t)
    ]
   ["Quick Responses"
    ("y" "Send <return>" claude-code-send-return)
    ("n" "Send <escape>" claude-code-send-escape)
    ("1" "Send \"1\"" claude-code-send-1)
    ("2" "Send \"2\"" claude-code-send-2)
    ("3" "Send \"3\"" claude-code-send-3)
    ]])

;;;###autoload (autoload 'claude-code-slash-commands "claude-code" nil t)
(transient-define-prefix claude-code-slash-commands ()
  "Claude slash commands menu."
  ["Slash Commands"
   ["Core Commands"
    ("h" "Help" (lambda () (interactive) (claude-code--do-send-command "/help")))
    ("c" "Clear" (lambda () (interactive) (claude-code--do-send-command "/clear")))
    ("C" "Compact" (lambda () (interactive) (claude-code--do-send-command "/compact")))
    ("s" "Status" (lambda () (interactive) (claude-code--do-send-command "/status")))
    ("d" "Doctor" (lambda () (interactive) (claude-code--do-send-command "/doctor")))]

   ["Configuration & Setup"
    ("f" "Config" (lambda () (interactive) (claude-code--do-send-command "/config")))
    ("i" "Init" (lambda () (interactive) (claude-code--do-send-command "/init")))
    ("m" "Memory" (lambda () (interactive) (claude-code--do-send-command "/memory")))
    ("a" "Add-dir" (lambda () (interactive) (claude-code--do-send-command "/add-dir")))
    ("t" "Terminal-setup" (lambda () (interactive) (claude-code--do-send-command "/terminal-setup")))]

   ["Account & Model"
    ("l" "Login" (lambda () (interactive) (claude-code--do-send-command "/login")))
    ("L" "Logout" (lambda () (interactive) (claude-code--do-send-command "/logout")))
    ("M" "Model" (lambda () (interactive) (claude-code--do-send-command "/model")))
    ("p" "Permissions" (lambda () (interactive) (claude-code--do-send-command "/permissions")))
    ("$" "Cost" (lambda () (interactive) (claude-code--do-send-command "/cost")))]

   ["Development Tools"
    ("r" "Review" (lambda () (interactive) (claude-code--do-send-command "/review")))
    ("P" "PR comments" (lambda () (interactive) (claude-code--do-send-command "/pr_comments")))
    ("A" "Agents" (lambda () (interactive) (claude-code--do-send-command "/agents")))
    ("v" "Vim" (lambda () (interactive) (claude-code--do-send-command "/vim")))
    ("S" "MCP" (lambda () (interactive) (claude-code--do-send-command "/mcp")))]

   ["Support"
    ("b" "Bug" (lambda () (interactive) (claude-code--do-send-command "/bug")))]
   ])

;;;; Terminal abstraction layer
;; This layer abstracts terminal operations to support multiple backends (eat, vterm, etc.)
;;;;; Generic function definitions

(cl-defgeneric claude-code--term-make (backend buffer-name program &optional switches)
  "Create a terminal using BACKEND in BUFFER-NAME running PROGRAM.
Optional SWITCHES are command-line arguments to PROGRAM.
Returns the buffer containing the terminal.")

(cl-defgeneric claude-code--term-send-string (backend terminal string)
  "Send STRING to TERMINAL using BACKEND.")

(cl-defgeneric claude-code--term-kill-process (backend buffer)
  "Kill the terminal process in BUFFER using BACKEND.")

(cl-defgeneric claude-code--term-read-only-mode (backend)
  "Switch current terminal to read-only mode using BACKEND.")

(cl-defgeneric claude-code--term-interactive-mode (backend)
  "Switch current terminal to interactive mode using BACKEND.")

(cl-defgeneric claude-code--term-in-read-only-p (backend)
  "Check if current terminal is in read-only mode using BACKEND.")

(cl-defgeneric claude-code--term-configure (backend)
  "Configure terminal in current buffer with BACKEND specific settings.")

(cl-defgeneric claude-code--term-customize-faces (backend)
  "Apply face customizations for the terminal using BACKEND.")

(cl-defgeneric claude-code--term-setup-keymap (backend)
  "Set up the local keymap for Claude Code buffers using BACKEND.")

(cl-defgeneric claude-code--term-get-adjust-process-window-size-fn (backend)
  "Get the BACKEND specific function that adjusts window size.")

;;;;; eat backend implementations

;; Declare external variables and functions from eat package
(defvar eat--semi-char-mode)
(defvar eat--synchronize-scroll-function)
(defvar eat-invisible-cursor-type)
(defvar eat-term-name)
(defvar eat-terminal)
(declare-function eat--adjust-process-window-size "eat" (&rest args))
(declare-function eat--set-cursor "eat" (terminal &rest args))
(declare-function eat-emacs-mode "eat")
(declare-function eat-kill-process "eat" (&optional buffer))
(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat-semi-char-mode "eat")
(declare-function eat-term-display-beginning "eat" (terminal))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat-term-live-p "eat" (terminal))
(declare-function eat-term-parameter "eat" (terminal parameter) t)
(declare-function eat-term-redisplay "eat" (terminal))
(declare-function eat-term-reset "eat" (terminal))
(declare-function eat-term-send-string "eat" (terminal string))

;; Helper to ensure eat is loaded
(defun claude-code--ensure-eat ()
  "Ensure eat package is loaded."
  (unless (featurep 'eat)
    (unless (require 'eat nil t)
      (error "The eat package is required for eat terminal backend. Please install it"))))

(cl-defmethod claude-code--term-make ((_backend (eql eat)) buffer-name program &optional switches)
  "Create an eat terminal for BACKEND.

_BACKEND is the terminal backend type (should be \\='eat).
BUFFER-NAME is the name for the new terminal buffer.
PROGRAM is the program to run in the terminal.
SWITCHES are optional command-line arguments for PROGRAM."
  (claude-code--ensure-eat)

  (let* ((trimmed-buffer-name (string-trim-right (string-trim buffer-name "\\*") "\\*"))
         (process-environment (cons (format "CLAUDE_BUFFER_NAME=%s" buffer-name)
                                    process-environment)))
    (apply #'eat-make trimmed-buffer-name program nil switches)))

(cl-defmethod claude-code--term-send-string ((_backend (eql eat)) string)
  "Send STRING to eat terminal.

_BACKEND is the terminal backend type (should be \\='eat).
STRING is the text to send to the terminal."
  (eat-term-send-string eat-terminal string))

(cl-defmethod claude-code--term-kill-process ((_backend (eql eat)) buffer)
  "Kill the eat terminal process in BUFFER.

_BACKEND is the terminal backend type (should be \\='eat).
BUFFER is the terminal buffer containing the process to kill."
  (with-current-buffer buffer
    (eat-kill-process)
    (kill-buffer buffer)))

(cl-defmethod claude-code--term-read-only-mode ((_backend (eql eat)))
  "Switch eat terminal to read-only mode.

_BACKEND is the terminal backend type (should be \\'eat)."
  (claude-code--ensure-eat)
  (eat-emacs-mode)
  (setq-local eat-invisible-cursor-type claude-code-eat-read-only-mode-cursor-type)
  (eat--set-cursor nil :invisible))

(cl-defmethod claude-code--term-interactive-mode ((_backend (eql eat)))
  "Switch eat terminal to interactive mode.

_BACKEND is the terminal backend type (should be \\='eat)."
  (claude-code--ensure-eat)
  (eat-semi-char-mode)
  (setq-local eat-invisible-cursor-type nil)
  (eat--set-cursor nil :invisible))

(cl-defmethod claude-code--term-in-read-only-p ((_backend (eql eat)))
  "Check if eat terminal is in read-only mode.

_BACKEND is the terminal backend type (should be \\='eat)."
  (not eat--semi-char-mode))

(defun claude-code--eat-synchronize-scroll (windows)
  "Synchronize scrolling and point between terminal and WINDOWS.

WINDOWS is a list of windows.  WINDOWS may also contain the special
symbol `buffer', in which case the point of current buffer is set.

This custom version keeps the prompt at the bottom of the window when
possible, preventing the scrolling up issue when editing other buffers."
  (dolist (window windows)
    (if (eq window 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      ;; Don't move the cursor around when in eat-emacs-mode
      (when (not buffer-read-only)
        (let ((cursor-pos (eat-term-display-cursor eat-terminal)))
          ;; Always set point to cursor position
          (set-window-point window cursor-pos)
          ;; Try to keep cursor visible with minimal scrolling
          (cond
           ;; If cursor is at/near end, keep at bottom
           ((>= cursor-pos (- (point-max) 2))
            (with-selected-window window
              (goto-char cursor-pos)
              (recenter -1)))
           ;; If cursor not visible, scroll minimally to show it
           ((not (pos-visible-in-window-p cursor-pos window))
            (with-selected-window window
              (goto-char cursor-pos)
              ;; Center cursor in window instead of jumping to term beginning
              (recenter)))))))))

(cl-defmethod claude-code--term-configure ((_backend (eql eat)))
  "Configure eat terminal in current buffer.

_BACKEND is the terminal backend type (should be \\='eat)."
  (claude-code--ensure-eat)
  ;; Configure eat-specific settings
  (setq-local eat-term-name claude-code-term-name)
  (setq-local eat-enable-directory-tracking nil)
  (setq-local eat-enable-shell-command-history nil)
  (setq-local eat-enable-shell-prompt-annotation nil)
  (when claude-code-eat-never-truncate-claude-buffer
    (setq-local eat-term-scrollback-size nil))

  ;; Set up custom scroll function to stop eat from scrolling to the top
  (setq-local eat--synchronize-scroll-function #'claude-code--eat-synchronize-scroll)

  ;; Configure bell handler - ensure eat-terminal exists
  (when (bound-and-true-p eat-terminal)
    (eval '(setf (eat-term-parameter eat-terminal 'ring-bell-function) #'claude-code--notify)))

  ;; fix wonky initial terminal layout that happens sometimes if we show the buffer before claude is ready
  (sleep-for claude-code-startup-delay))

(cl-defmethod claude-code--term-customize-faces ((_backend (eql eat)))
  "Apply face customizations for eat terminal.

_BACKEND is the terminal backend type (should be \\='eat)."
  ;; Remap eat faces to Claude-specific faces
  (face-remap-add-relative 'eat-shell-prompt-annotation-running 'claude-code-eat-prompt-annotation-running-face)
  (face-remap-add-relative 'eat-shell-prompt-annotation-success 'claude-code-eat-prompt-annotation-success-face)
  (face-remap-add-relative 'eat-shell-prompt-annotation-failure 'claude-code-eat-prompt-annotation-failure-face)
  (face-remap-add-relative 'eat-term-bold 'claude-code-eat-term-bold-face)
  (face-remap-add-relative 'eat-term-faint 'claude-code-eat-term-faint-face)
  (face-remap-add-relative 'eat-term-italic 'claude-code-eat-term-italic-face)
  (face-remap-add-relative 'eat-term-slow-blink 'claude-code-eat-term-slow-blink-face)
  (face-remap-add-relative 'eat-term-fast-blink 'claude-code-eat-term-fast-blink-face)
  (dolist (i (number-sequence 0 9))
    (let ((eat-face (intern (format "eat-term-font-%d" i)))
          (claude-face (intern (format "claude-code-eat-term-font-%d-face" i))))
      (face-remap-add-relative eat-face claude-face))))

(cl-defmethod claude-code--term-setup-keymap ((_backend (eql eat)))
  "Set up the local keymap for Claude Code buffers.

_BACKEND is the terminal backend type (should be \\='eat)."
  (let ((map (make-sparse-keymap)))
    ;; Inherit parent eat keymap
    (set-keymap-parent map (current-local-map))

    ;; C-g for escape
    (define-key map (kbd "C-g") #'claude-code-send-escape)

    ;; Configure key bindings based on user preference
    (pcase claude-code-newline-keybinding-style
      ('newline-on-shift-return
       ;; S-return enters a line break, RET sends the command
       (define-key map (kbd "<S-return>") #'claude-code--eat-send-alt-return)
       (define-key map (kbd "<return>") #'claude-code--eat-send-return))
      ('newline-on-alt-return
       ;; M-return enters a line break, RET sends the command
       (define-key map (kbd "<M-return>") #'claude-code--eat-send-alt-return)
       (define-key map (kbd "<return>") #'claude-code--eat-send-return))
      ('shift-return-to-send
       ;; RET enters a line break, S-return sends the command
       (define-key map (kbd "<return>") #'claude-code--eat-send-alt-return)
       (define-key map (kbd "<S-return>") #'claude-code--eat-send-return))
      ('super-return-to-send
       ;; RET enters a line break, s-return sends the command.
       (define-key map (kbd "<return>") #'claude-code--eat-send-alt-return)
       (define-key map (kbd "<s-return>") #'claude-code--eat-send-return)))
    (use-local-map map)))

(defun claude-code--eat-send-alt-return ()
  "Send <alt>-<return> to eat."
  (interactive)
  (eat-term-send-string eat-terminal "\e\C-m"))

(defun claude-code--eat-send-return ()
  "Send <return> to eat."
  (interactive)
  (eat-term-send-string eat-terminal (kbd "RET")))

(cl-defgeneric claude-code--term-get-adjust-process-window-size-fn (backend)
  "Get the BACKEND specific function that adjusts window size.")

(cl-defmethod claude-code--term-get-adjust-process-window-size-fn ((_backend (eql eat)))
  "Get the BACKEND specific function that adjusts window size."
  #'eat--adjust-process-window-size)

;;;;; vterm backend implementations

;; Declare external variables and functions from vterm package
(defvar vterm-buffer-name)
(defvar vterm-copy-mode)
(defvar vterm-environment)
(defvar vterm-shell)
(defvar vterm-term-environment-variable)
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm--window-adjust-process-window-size "vterm" (process window))
(declare-function vterm-copy-mode "vterm" (&optional arg))
(declare-function vterm-mode "vterm")
(declare-function vterm-send-key "vterm" key &optional shift meta ctrl accept-proc-output)
(declare-function vterm-send-string "vterm" (string &optional paste-p))

;; Helper to ensure vterm is loaded
(cl-defmethod claude-code--term-make ((_backend (eql vterm)) buffer-name program &optional switches)
  "Create a vterm terminal.

_BACKEND is the terminal backend type (should be \\='vterm).
BUFFER-NAME is the name for the new terminal buffer.
PROGRAM is the program to run in the terminal.
SWITCHES are optional command-line arguments for PROGRAM."
  (claude-code--ensure-vterm)
  (let* ((vterm-shell (if switches
                          (concat program " " (mapconcat #'identity switches " "))
                        program))
         (process-environment (cons (format "CLAUDE_BUFFER_NAME=%s" buffer-name)
                                    process-environment))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; vterm needs to have an open window before starting the claude
      ;; process; otherwise Claude doesn't seem to know how wide its
      ;; terminal window is and it draws the input box too wide. But
      ;; the user may not want to pop to the buffer. For some reason
      ;; `display-buffer' also leads to wonky results, it has to be
      ;; `pop-to-buffer'. So, show the buffer, start vterm-mode (which
      ;; starts the vterm-shell claude process), and then hide the
      ;; buffer. We'll optionally re-open it later.
      ;;
      ;; [TODO] see if there's a cleaner way to do this.
      (pop-to-buffer buffer)
      (vterm-mode)
      (delete-window (get-buffer-window buffer))
      buffer)))

(defun claude-code--ensure-vterm ()
  "Ensure vterm package is loaded."
  (unless (featurep 'vterm)
    (unless (require 'vterm nil t)
      (error "The vterm package is required for vterm terminal backend. Please install it"))))

(cl-defmethod claude-code--term-send-string ((_backend (eql vterm)) string)
  "Send STRING to vterm terminal.

_BACKEND is the terminal backend type (should be \\='vterm).
_TERMINAL is unused for vterm backend.
STRING is the text to send to the terminal."
  (vterm-send-string string))

(cl-defmethod claude-code--term-kill-process ((_backend (eql vterm)) buffer)
  "Kill the vterm terminal process in BUFFER.

_BACKEND is the terminal backend type (should be \\='vterm).
BUFFER is the terminal buffer containing the process to kill."
  (kill-process (get-buffer-process buffer)))

;; Mode operations
(cl-defmethod claude-code--term-read-only-mode ((_backend (eql vterm)))
  "Switch vterm terminal to read-only mode.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (claude-code--ensure-vterm)
  (vterm-copy-mode 1)
  (setq-local cursor-type t))

(cl-defmethod claude-code--term-interactive-mode ((_backend (eql vterm)))
  "Switch vterm terminal to interactive mode.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (claude-code--ensure-vterm)
  (vterm-copy-mode -1)
  (setq-local cursor-type nil)
  ;; Restore keybindings that were lost when vterm-copy-mode reset the keymap
  (claude-code--term-setup-keymap 'vterm))

(cl-defmethod claude-code--term-in-read-only-p ((_backend (eql vterm)))
  "Check if vterm terminal is in read-only mode.

_BACKEND is the terminal backend type (should be \\='vterm)."
  vterm-copy-mode)

(cl-defmethod claude-code--term-configure ((_backend (eql vterm)))
  "Configure vterm terminal in current buffer.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (claude-code--ensure-vterm)
  ;; set TERM
  (setq vterm-term-environment-variable claude-code-term-name)
  ;; Prevent vterm from automatically renaming the buffer
  (setq-local vterm-buffer-name-string nil)
  ;; Disable automatic scrolling to bottom on output to prevent flickering
  (setq-local vterm-scroll-to-bottom-on-output nil)
  ;; Disable immediate redraw to batch updates and reduce flickering
  (setq-local vterm--redraw-immididately nil)
  ;; Try to prevent cursor flickering by disabling Emacs' own cursor management
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local blink-cursor-mode nil)
  (setq-local cursor-type nil)  ; Let vterm handle the cursor entirely
  ;; Set timer delay to nil for faster updates (reduces visible flicker duration)
  ;; (setq-local vterm-timer-delay nil)
  ;; Increase process read buffering to batch more updates together
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)
    ;; Try to make vterm read larger chunks at once
    (process-put proc 'read-output-max 4096))
  ;; Set up bell detection advice
  (advice-add 'vterm--filter :around #'claude-code--vterm-bell-detector)
  ;; Set up multi-line buffering to prevent flickering
  (advice-add 'vterm--filter :around #'claude-code--vterm-multiline-buffer-filter))

(cl-defmethod claude-code--term-customize-faces ((_backend (eql vterm)))
  "Apply face customizations for vterm terminal.

_BACKEND is the terminal backend type (should be \\='vterm)."
  ;; no faces to customize yet (this could change)
  )

(defun claude-code--vterm-send-escape ()
  "Send escape key to vterm."
  (interactive)
  (vterm-send-key ""))

(defun claude-code--vterm-send-return ()
  "Send escape key to vterm."
  (interactive)
  (vterm-send-key ""))

(defun claude-code--vterm-send-alt-return ()
  "Send <alt>-<return> to vterm."
  (interactive)
  (vterm-send-key "" nil t))

(defun claude-code--vterm-send-shift-return ()
  "Send shift return to vterm."
  (interactive)
  (vterm-send-key "" t))

(defun claude-code--vterm-send-super-return ()
  "Send escape key to vterm."
  (interactive)
  ;; (vterm-send-key " " t)
  (vterm-send-key (kbd "s-<return>") t))

;; (defun claude-code--vterm-send-alt-return ()
;;   "Send alt-return to vterm for newline without submitting."
;;   (message "claude-code--vterm-send-alt-return invoked")
;;   (interactive)
;;   (vterm-send-key "" nil t))

(cl-defmethod claude-code--term-setup-keymap ((_backend (eql vterm)))
  "Set up the local keymap for Claude Code buffers.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (let ((map (make-sparse-keymap)))
    ;; Inherit parent eat keymap
    (set-keymap-parent map (current-local-map))

    ;; C-g for escape
    (define-key map (kbd "C-g") #'claude-code--vterm-send-escape)

    (pcase claude-code-newline-keybinding-style
      ('newline-on-shift-return
       ;; S-return enters a line break, RET sends the command
       (define-key map (kbd "<S-return>") #'claude-code--vterm-send-alt-return)
       (define-key map (kbd "<return>") #'claude-code--vterm-send-return))
      ('newline-on-alt-return
       ;; M-return enters a line break, RET sends the command
       (define-key map (kbd "<M-return>") #'claude-code--vterm-send-alt-return)
       (define-key map (kbd "<return>") #'claude-code--vterm-send-return))
      ('shift-return-to-send
       ;; RET enters a line break, S-return sends the command
       (define-key map (kbd "<return>") #'claude-code--vterm-send-alt-return)
       (define-key map (kbd "<S-return>") #'claude-code--vterm-send-return))
      ('super-return-to-send
       ;; RET enters a line break, s-return sends the command.
       (define-key map (kbd "<return>") #'claude-code--vterm-send-alt-return)
       (define-key map (kbd "<s-return>") #'claude-code--vterm-send-return)))

    (use-local-map map)))

(cl-defmethod claude-code--term-get-adjust-process-window-size-fn ((_backend (eql vterm)))
  "Get the BACKEND specific function that adjusts window size."
  #'vterm--window-adjust-process-window-size)

;;;; Private util functions
(defmacro claude-code--with-buffer (&rest body)
  "Execute BODY with the Claude buffer, handling buffer selection and display.

Gets or prompts for the Claude buffer, executes BODY within that buffer's
context, displays the buffer, and shows not-running message if no buffer
is found."
  `(if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
       (with-current-buffer claude-code-buffer
         ,@body
         (display-buffer claude-code-buffer))
     (claude-code--show-not-running-message)))

(defun claude-code--buffer-p (buffer)
  "Return non-nil if BUFFER is a Claude buffer.

BUFFER can be either a buffer object or a buffer name string."
  (let ((name (if (stringp buffer)
                  buffer
                (buffer-name buffer))))
    (and name (string-match-p "^\\*claude:" name))))

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
   #'claude-code--buffer-p
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
    (pop-to-buffer selected-buffer)))

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

(defun claude-code--prompt-for-instance-name (dir existing-instance-names &optional force-prompt)
  "Prompt user for a new instance name for directory DIR.

EXISTING-INSTANCE-NAMES is a list of existing instance names.
If FORCE-PROMPT is non-nil, always prompt even if no instances exist."
  (if (or existing-instance-names force-prompt)
      (let ((proposed-name ""))
        (while (or (string-empty-p proposed-name)
                   (member proposed-name existing-instance-names))
          (setq proposed-name
                (read-string (if (and existing-instance-names (not force-prompt))
                                 (format "Instances already running for %s (existing: %s), new instance name: "
                                         (abbreviate-file-name dir)
                                         (mapconcat #'identity existing-instance-names ", "))
                               (format "Instance name for %s: " (abbreviate-file-name dir)))
                             nil nil proposed-name))
          (cond
           ((string-empty-p proposed-name)
            (message "Instance name cannot be empty. Please enter a name.")
            (sit-for 1))
           ((member proposed-name existing-instance-names)
            (message "Instance name '%s' already exists. Please choose a different name." proposed-name)
            (sit-for 1))))
        proposed-name)
    "default"))

(defun claude-code--show-not-running-message ()
  "Show a message that Claude is not running in any directory."
  (message "Claude is not running"))

(defun claude-code--kill-buffer (buffer)
  "Kill a Claude BUFFER by cleaning up hooks and processes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Remove the adjust window size advice if it was added
      (when claude-code-optimize-window-resize
        (advice-remove (claude-code--term-get-adjust-process-window-size-fn claude-code-terminal-backend) #'claude-code--adjust-window-size-advice))
      ;; Remove vterm advice if using vterm backend
      (when (eq claude-code-terminal-backend 'vterm)
        (advice-remove 'vterm--filter #'claude-code--vterm-bell-detector)
        (advice-remove 'vterm--filter #'claude-code--vterm-multiline-buffer-filter))
      ;; Clean the window widths hash table
      (when claude-code--window-widths
        (clrhash claude-code--window-widths))
      ;; Kill the process
      (claude-code--term-kill-process claude-code-terminal-backend buffer))))

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
    (file-local-name (file-truename buffer-file-name))))

(defun claude-code--format-file-reference (&optional file-name line-start line-end)
  "Format a file reference in the @file:line style.

FILE-NAME is the file path.  If nil, get from current buffer.
LINE-START is the starting line number.  If nil, use current line.
LINE-END is the ending line number for a range.  If nil, format single line."
  (let ((file (or file-name (claude-code--get-buffer-file-name)))
        (start (or line-start (line-number-at-pos)))
        (end line-end))
    (when file
      (if end
          (format "@%s:%d-%d" file start end)
        (format "@%s:%d" file start)))))

(defun claude-code--do-send-command (cmd)
  "Send a command CMD to Claude if Claude buffer exists.

After sending the command, move point to the end of the buffer.
Returns the selected Claude buffer or nil."
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (progn
        (with-current-buffer claude-code-buffer
          (claude-code--term-send-string claude-code-terminal-backend cmd)
          (claude-code--term-send-string claude-code-terminal-backend (kbd "RET"))
          (display-buffer claude-code-buffer))
        claude-code-buffer)
    (claude-code--show-not-running-message)
    nil))

(defun claude-code--start (arg extra-switches &optional force-prompt force-switch-to-buffer)
  "Start Claude with given command-line EXTRA-SWITCHES.

ARG is the prefix argument controlling directory and buffer switching.
EXTRA-SWITCHES is a list of additional command-line switches to pass to Claude.
If FORCE-PROMPT is non-nil, always prompt for instance name.
If FORCE-SWITCH-TO-BUFFER is non-nil, always switch to the Claude buffer.

With single prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (let* ((dir (if (equal arg '(16))     ; Double prefix
                  (read-directory-name "Project directory: ")
                (claude-code--directory)))
         (switch-after (or (equal arg '(4)) force-switch-to-buffer)) ; Single prefix or force-switch-to-buffer
         (default-directory dir)
         ;; Check for existing Claude instances in this directory
         (existing-buffers (claude-code--find-claude-buffers-for-directory dir))
         ;; Get existing instance names
         (existing-instance-names (mapcar (lambda (buf)
                                            (or (claude-code--extract-instance-name-from-buffer-name
                                                 (buffer-name buf))
                                                "default"))
                                          existing-buffers))
         ;; Prompt for instance name (only if instances exist, or force-prompt is true)
         (instance-name (claude-code--prompt-for-instance-name dir existing-instance-names force-prompt))
         (buffer-name (claude-code--buffer-name instance-name))
         (program-switches (if extra-switches
                               (append claude-code-program-switches extra-switches)
                             claude-code-program-switches))

         ;; Set process-adaptive-read-buffering to nil to avoid flickering while Claude is processing
         (process-adaptive-read-buffering nil)

         ;; Start the terminal process
         (buffer (claude-code--term-make claude-code-terminal-backend buffer-name claude-code-program program-switches)))

    ;; Check if the claude program is available
    (unless (executable-find claude-code-program)
      (error "Claude Code program '%s' not found in PATH" claude-code-program))

    ;; Check if buffer was successfully created
    (unless (buffer-live-p buffer)
      (error "Failed to create Claude Code buffer"))

    ;; setup claude buffer
    (with-current-buffer buffer

      ;; Configure terminal with backend-specific settings
      (claude-code--term-configure claude-code-terminal-backend)

      ;; Initialize the window widths hash table
      (setq claude-code--window-widths (make-hash-table :test 'eq :weakness 'key))

      ;; Set up window width tracking if optimization is enabled
      (when claude-code-optimize-window-resize
        (advice-add (claude-code--term-get-adjust-process-window-size-fn claude-code-terminal-backend) :around #'claude-code--adjust-window-size-advice))

      ;; Setup our custom key bindings
      (claude-code--term-setup-keymap claude-code-terminal-backend)

      ;; Customize terminal faces
      (claude-code--term-customize-faces claude-code-terminal-backend)

      ;; remove underlines from _>_
      (face-remap-add-relative 'nobreak-space :underline nil)

      ;; set buffer face
      (buffer-face-set :inherit 'claude-code-repl-face)

      ;; disable scroll bar, fringes
      (setq-local vertical-scroll-bar nil)
      (setq-local fringe-mode 0)

      ;; Add cleanup hook to remove directory mappings when buffer is killed
      (add-hook 'kill-buffer-hook #'claude-code--cleanup-directory-mapping nil t)

      ;; run start hooks
      (run-hooks 'claude-code-start-hook)
      
      ;; Start MCP server if enabled
      (when claude-code-mcp-enabled
        (claude-code--start-mcp-server)
        (claude-code--start-mcp-bridge-process))

      ;; Disable vertical scroll bar in claude buffer
      (setq-local vertical-scroll-bar nil)

      ;; Display buffer, setting window parameters
      (let ((window (display-buffer buffer '((display-buffer-below-selected)))))
        (when window
          ;; turn off fringes and margins in the Claude buffer
          (set-window-parameter window 'left-margin-width 0)
          (set-window-parameter window 'right-margin-width 0)
          (set-window-parameter window 'left-fringe-width 0)
          (set-window-parameter window 'right-fringe-width 0)
          ;; set no-delete-other-windows parameter for claude-code window
          (set-window-parameter window 'no-delete-other-windows claude-code-no-delete-other-windows))))

    ;; switch to the Claude buffer if asked to
    (when switch-after
      (pop-to-buffer buffer))))

;;;###autoload
(defun claude-code (&optional arg)
  "Start Claude in an eat terminal and enable `claude-code-mode'.

If current buffer belongs to a project start Claude in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
buffer file.

With single prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (interactive "P")
  (claude-code--start arg nil))

;;;###autoload
(defun claude-code-start-in-directory (&optional arg)
  "Prompt for a directory and start Claude there.

This is a convenience command equivalent to using `claude-code` with
double prefix arg (\\[universal-argument] \\[universal-argument]).

With prefix ARG (\\[universal-argument]), switch to buffer after creating."
  (interactive "P")
  ;; Always prompt for directory (like double prefix)
  ;; If user gave us a prefix arg, also switch to buffer after creating
  (let ((dir (read-directory-name "Project directory: ")))
    ;; We need to temporarily override claude-code--directory to return our chosen dir
    (cl-letf (((symbol-function 'claude-code--directory) (lambda () dir)))
      (claude-code (when arg '(4))))))

;;;###autoload
(defun claude-code-continue (&optional arg)
  "Start Claude and continue the previous conversation.

This command starts Claude with the --continue flag to resume
where you left off in your last session.

If current buffer belongs to a project start Claude in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
buffer file.

With prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (interactive "P")
  (claude-code--start arg '("--continue")))

;;;###autoload
(defun claude-code-resume (arg)
  "Resume a specific Claude session.

This command starts Claude with the --resume flag to resume a specific
past session. Claude will present an interactive list of past sessions
to choose from.

If current buffer belongs to a project start Claude in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
 buffer file.

With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (interactive "P")

  (let ((extra-switches '("--resume")))
    (claude-code--start arg extra-switches nil t))
  (claude-code--term-send-string claude-code-terminal-backend "")
  (goto-char (point-min)))

;;;###autoload
(defun claude-code-new-instance (&optional arg)
  "Create a new Claude instance, prompting for instance name.

This command always prompts for an instance name, unlike `claude-code'
which uses \"default\" when no instances exist.

If current buffer belongs to a project start Claude in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
buffer file.

With single prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt
for the project directory."
  (interactive "P")

  ;; Call claude-code--start with force-prompt=t
  (claude-code--start arg nil t))

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

(defun claude-code--pulse-modeline ()
  "Pulse the modeline to provide visual notification."
  ;; First pulse - invert
  (invert-face 'mode-line)
  (run-at-time 0.1 nil
               (lambda ()
                 ;; Return to normal
                 (invert-face 'mode-line)
                 ;; Second pulse
                 (run-at-time 0.1 nil
                              (lambda ()
                                (invert-face 'mode-line)
                                ;; Final return to normal
                                (run-at-time 0.1 nil
                                             (lambda ()
                                               (invert-face 'mode-line))))))))

(defun claude-code-default-notification (title message)
  "Default notification function that displays a message and pulses the modeline.

TITLE is the notification title.
MESSAGE is the notification body."
  ;; Display the message
  (message "%s: %s" title message)
  ;; Pulse the modeline for visual feedback
  (claude-code--pulse-modeline)
  (message "%s: %s" title message))

(defun claude-code-handle-hook (type buffer-name &rest args)
  "Handle hook of TYPE for BUFFER-NAME with JSON data and additional ARGS.
This is the unified entry point for all Claude Code CLI hooks.
ARGS can contain additional arguments passed from the CLI."
  ;; Must consume ALL arguments from server-eval-args-left to prevent Emacs
  ;; from trying to evaluate leftover arguments as Lisp expressions
  (let ((json-data (when server-eval-args-left (pop server-eval-args-left)))
        (extra-args (prog1 server-eval-args-left (setq server-eval-args-left nil))))
    (let ((message (list :type type 
                         :buffer-name buffer-name 
                         :json-data json-data 
                         :args (append args extra-args))))
      (run-hook-with-args 'claude-code-event-hook message))))

(defun claude-code--notify (_terminal)
  "Notify the user that Claude has finished and is awaiting input.

TERMINAL is the eat terminal parameter (not used)."
  (when claude-code-enable-notifications
    (funcall claude-code-notification-function
             "Claude Ready"
             "Waiting for your response")))

(defun claude-code--vterm-bell-detector (orig-fun process input)
  "Detect bell characters in vterm output and trigger notifications.

ORIG-FUN is the original vterm--filter function.
PROCESS is the vterm process.
INPUT is the terminal output string."
  (when (and (string-match-p "\007" input)
             (claude-code--buffer-p (process-buffer process))
             ;; Ignore bells in OSC sequences (terminal title updates)
             (not (string-match-p "]0;.*\007" input)))
    (claude-code--notify nil))

  (funcall orig-fun process input))

(defvar-local claude-code--vterm-multiline-buffer nil
  "Buffer for accumulating multi-line vterm output.")

(defvar-local claude-code--vterm-multiline-buffer-timer nil
  "Timer for processing buffered multi-line vterm output.")

(defun claude-code--vterm-multiline-buffer-filter (orig-fun process input)
  "Buffer vterm output when it appears to be redrawing multi-line input.
This prevents flickering when Claude redraws its input box as it expands
to multiple lines. We detect this by looking for escape sequences that
indicate cursor positioning and line clearing operations.

ORIG-FUN is the original vterm--filter function.
PROCESS is the vterm process.
INPUT is the terminal output string."
  (if (or (not claude-code-vterm-buffer-multiline-output)
          (not (claude-code--buffer-p (process-buffer process))))
      ;; Feature disabled or not a Claude buffer, pass through normally
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      ;; Check if this looks like multi-line input box redraw
      ;; Common patterns when redrawing multi-line input:
      ;; - ESC[K (clear to end of line)
      ;; - ESC[<n>;<m>H (cursor positioning)
      ;; - ESC[<n>A/B/C/D (cursor movement)
      ;; - Multiple of these in sequence
      (let ((has-clear-line (string-match-p "\033\\[K" input))
            (has-cursor-pos (string-match-p "\033\\[[0-9]+;[0-9]+H" input))
            (has-cursor-move (string-match-p "\033\\[[0-9]*[ABCD]" input))
            (escape-count (cl-count ?\033 input)))

        ;; If we see multiple escape sequences that look like redrawing,
        ;; or we're already buffering, add to buffer
        (if (or (and (>= escape-count 3)
                     (or has-clear-line has-cursor-pos has-cursor-move))
                claude-code--vterm-multiline-buffer)
            (progn
              ;; Add to buffer
              (setq claude-code--vterm-multiline-buffer
                    (concat claude-code--vterm-multiline-buffer input))
              ;; Cancel existing timer
              (when claude-code--vterm-multiline-buffer-timer
                (cancel-timer claude-code--vterm-multiline-buffer-timer))
              ;; Set timer with configurable delay
              ;; This is enough to collect a burst of updates but not noticeable to user
              (setq claude-code--vterm-multiline-buffer-timer
                    (run-at-time claude-code-vterm-multiline-delay nil
                                 (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (when claude-code--vterm-multiline-buffer
                                         (let ((inhibit-redisplay t)
                                               (data claude-code--vterm-multiline-buffer))
                                           ;; Clear buffer first to prevent recursion
                                           (setq claude-code--vterm-multiline-buffer nil
                                                 claude-code--vterm-multiline-buffer-timer nil)
                                           ;; Process all buffered data at once
                                           (funcall orig-fun
                                                    (get-buffer-process buf)
                                                    data))))))
                                 (current-buffer))))
          ;; Not multi-line redraw, process normally
          (funcall orig-fun process input))))))

(defun claude-code--adjust-window-size-advice (orig-fun &rest args)
  "Advice to only signal on width change.

Works with `eat--adjust-process-window-size' or
`vterm--adjust-process-window-size' to prevent unnecessary reflows.

Returns the size returned by ORIG-FUN only when the width of any Claude
window has changed, not when only the height has changed. This prevents
unnecessary terminal reflows when only vertical space changes.

ARGS is passed to ORIG-FUN unchanged."
  (let ((result (apply orig-fun args)))
    ;; Check all windows for Claude buffers
    (let ((width-changed nil))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (and buffer (claude-code--buffer-p buffer))
            (let ((current-width (window-width window))
                  (stored-width (gethash window claude-code--window-widths)))
              ;; Check if this is a new window or if width changed
              (when (or (not stored-width) (/= current-width stored-width))
                (setq width-changed t)
                ;; Update stored width
                (puthash window current-width claude-code--window-widths))))))
      ;; If current buffer is not a Claude buffer, just pass through normally
      (if (not (claude-code--buffer-p (current-buffer)))
          result
        ;; For Claude buffers: Return result only if a Claude window width changed and
        ;; we're not in read-only mode. otherwise nil. Nil means do
        ;; not send a window size changed event to the Claude process.
        (if (and width-changed (not (claude-code--term-in-read-only-p claude-code-terminal-backend)))
            result
          nil)))))

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
        (when (and (equal arg '(16)) selected-buffer) ; Only switch buffer with C-u C-u
          (pop-to-buffer selected-buffer))))))

;;;###autoload
(defun claude-code-toggle ()
  "Show or hide the Claude window.

If the Claude buffer doesn't exist, create it."
  (interactive)
  (let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
    (if claude-code-buffer
        (if (get-buffer-window claude-code-buffer)
            (delete-window (get-buffer-window claude-code-buffer))
          (let ((window (display-buffer claude-code-buffer '((display-buffer-below-selected)))))
            ;; set no-delete-other-windows parameter for claude-code window
            (set-window-parameter window 'no-delete-other-windows claude-code-no-delete-other-windows)))
      (claude-code--show-not-running-message))))

;;;###autoload
(defun claude-code--switch-to-all-instances-helper ()
  "Helper function to switch to a Claude buffer from all available instances.

Returns t if a buffer was selected and switched to, nil otherwise."
  (let ((all-buffers (claude-code--find-all-claude-buffers)))
    (cond
     ((null all-buffers)
      (claude-code--show-not-running-message)
      nil)
     ((= (length all-buffers) 1)
      ;; Only one buffer, just switch to it
      (pop-to-buffer (car all-buffers))
      t)
     (t
      ;; Multiple buffers, let user choose
      (let ((selected-buffer (claude-code--select-buffer-from-choices
                              "Select Claude instance: "
                              all-buffers)))
        (when selected-buffer
          (pop-to-buffer selected-buffer)
          t))))))

(defun claude-code-switch-to-buffer (&optional arg)
  "Switch to the Claude buffer if it exists.

With prefix ARG, show all Claude instances across all directories."
  (interactive "P")
  (if arg
      ;; With prefix arg, show all Claude instances
      (claude-code--switch-to-all-instances-helper)
    ;; Without prefix arg, use normal behavior
    (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
        (pop-to-buffer claude-code-buffer)
      (claude-code--show-not-running-message))))

;;;###autoload
(defun claude-code-select-buffer ()
  "Select and switch to a Claude buffer from all running instances.

This command shows all Claude instances across all projects and
directories, allowing you to choose which one to switch to."
  (interactive)
  (claude-code--switch-to-all-instances-helper))

(defun claude-code--kill-all-instances ()
  "Kill all Claude instances across all directories."
  (let ((all-buffers (claude-code--find-all-claude-buffers)))
    (if all-buffers
        (let* ((buffer-count (length all-buffers))
               (plural-suffix (if (= buffer-count 1) "" "s")))
          (if claude-code-confirm-kill
              (when (yes-or-no-p (format "Kill %d Claude instance%s? " buffer-count plural-suffix))
                (dolist (buffer all-buffers)
                  (claude-code--kill-buffer buffer))
                (message "%d Claude instance%s killed" buffer-count plural-suffix))
            (dolist (buffer all-buffers)
              (claude-code--kill-buffer buffer))
            (message "%d Claude instance%s killed" buffer-count plural-suffix)))
      (claude-code--show-not-running-message))))

;;;###autoload
(defun claude-code-kill ()
  "Kill Claude process and close its window."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (if claude-code-confirm-kill
          (when (yes-or-no-p "Kill Claude instance? ")
            (claude-code--kill-buffer claude-code-buffer)
            (message "Claude instance killed"))
        (claude-code--kill-buffer claude-code-buffer)
        (message "Claude instance killed"))
    (claude-code--show-not-running-message)))

;;;###autoload
(defun claude-code-kill-all ()
  "Kill ALL Claude processes across all directories."
  (interactive)
  (claude-code--kill-all-instances))

;;;###autoload
(defun claude-code-send-command (cmd &optional arg)
  "Read a Claude command from the minibuffer and send it.

With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (let ((selected-buffer (claude-code--do-send-command cmd)))
    (when (and arg selected-buffer)
      (pop-to-buffer selected-buffer))))

;;;###autoload
(defun claude-code-send-command-with-context (cmd &optional arg)
  "Read a Claude command and send it with current file and line context.

If region is active, include region line numbers.
With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (let* ((file-ref (if (use-region-p)
                       (claude-code--format-file-reference
                        nil
                        (line-number-at-pos (region-beginning))
                        (line-number-at-pos (region-end)))
                     (claude-code--format-file-reference)))
         (cmd-with-context (if file-ref
                               (format "%s\n%s" cmd file-ref)
                             cmd)))
    (let ((selected-buffer (claude-code--do-send-command cmd-with-context)))
      (when (and arg selected-buffer)
        (pop-to-buffer selected-buffer)))))

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
  (claude-code--with-buffer
   (claude-code--term-send-string claude-code-terminal-backend (kbd "ESC"))))

;;;###autoload
(defun claude-code-send-file (file-path)
  "Send the specified FILE-PATH to Claude prefixed with `@'.

FILE-PATH should be an absolute path to the file to send."
  (interactive "fFile to send to Claude: ")
  (let ((command (format "@%s" (expand-file-name file-path))))
    (claude-code--do-send-command command)))

;;;###autoload
(defun claude-code-send-buffer-file (&optional arg)
  "Send the file associated with current buffer to Claude prefixed with `@'.

With prefix ARG, prompt for instructions to add to the file before sending.
With two prefix ARGs, both add instructions and switch to Claude buffer."
  (interactive "P")
  (let ((file-path (claude-code--get-buffer-file-name)))
    (if file-path
        (let* ((prompt (when arg
                         (read-string "Instructions for Claude: ")))
               (command (if prompt
                            (format "%s\n\n@%s" prompt file-path)
                          (format "@%s" file-path))))
          (let ((selected-buffer (claude-code--do-send-command command)))
            (when (and (equal arg '(16)) selected-buffer) ; Only switch buffer with C-u C-u
              (pop-to-buffer selected-buffer))))
      (error "Current buffer is not associated with a file"))))

(defun claude-code--send-meta-return ()
  "Send Meta-Return key sequence to the terminal."
  (interactive)
  (claude-code--term-send-string claude-code-terminal-backend "\e\C-m"))

(defun claude-code--send-return ()
  "Send Return key to the terminal."
  (interactive)
  (claude-code--term-send-string claude-code-terminal-backend (kbd "RET")))

;;;###autoload
(defun claude-code-cycle-mode ()
  "Send Shift-Tab to Claude to cycle between modes.

Claude uses Shift-Tab to cycle through:
- Default mode
- Auto-accept edits mode
- Plan mode"
  (interactive)
  (claude-code--with-buffer
   (claude-code--term-send-string claude-code-terminal-backend "\e[Z")))

;; (define-key key-translation-map (kbd "ESC") "")

;;;###autoload
(defun claude-code-fork ()
  "Jump to a previous conversation by invoking the Claude fork command.

Sends <escape><escape> to the Claude Code REPL."
  (interactive)
  (if-let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (claude-code--term-send-string claude-code-terminal-backend "")
        ;; (display-buffer claude-code-buffer)
        (pop-to-buffer claude-code-buffer))
    (claude-code--show-not-running-message)))

;;;###autoload
(defun claude-code-fix-error-at-point (&optional arg)
  "Ask Claude to fix the error at point.

Gets the error message, file name, and line number, and instructs Claude
to fix the error. Supports both flycheck and flymake error systems, as well
as any system that implements help-at-pt.

With prefix ARG, switch to the Claude buffer after sending."
  (interactive "P")
  (let* ((error-text (claude-code--format-errors-at-point))
         (file-ref (claude-code--format-file-reference)))
    (if (string= error-text "No errors at point")
        (message "No errors found at point")
      (let ((command (format "Fix this error at %s:\nDo not run any external linter or other program, just fix the error at point using the context provided in the error message: <%s>"
                             (or file-ref "current position") error-text)))
        (let ((selected-buffer (claude-code--do-send-command command)))
          (when (and arg selected-buffer)
            (pop-to-buffer selected-buffer)))))))

;;;###autoload
(defun claude-code-read-only-mode ()
  "Enter read-only mode in Claude buffer with visible cursor.

In this mode, you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Claude
buffer. However, you are not allowed to change the buffer contents or
enter Claude commands.

Use `claude-code-exit-read-only-mode' to switch back to normal mode."
  (interactive)
  (claude-code--with-buffer
   (claude-code--term-read-only-mode claude-code-terminal-backend)
   (message "Claude read-only mode enabled")))

;;;###autoload
(defun claude-code-exit-read-only-mode ()
  "Exit read-only mode and return to normal mode (eat semi-char mode)."
  (interactive)
  (claude-code--with-buffer
   (claude-code--term-interactive-mode claude-code-terminal-backend)
   (message "Claude read-only disabled")))

;;;###autoload
(defun claude-code-toggle-read-only-mode ()
  "Toggle between read-only mode and normal mode.

In read-only mode you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Claude
buffer. However, you are not allowed to change the buffer contents or
enter Claude commands."
  (interactive)
  (claude-code--with-buffer
   (if (not (claude-code--term-in-read-only-p claude-code-terminal-backend))
       (claude-code-read-only-mode)
     (claude-code-exit-read-only-mode))))

;;;; MCP Integration
(defcustom claude-code-mcp-enabled t
  "Whether to enable MCP (Model Context Protocol) integration."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-mcp-port 8765
  "Port for the MCP TCP server."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-mcp-blocked-buffer-patterns
  '("password" ".pem" "secret" ".key" "token" "credential" "auth" ".ssh")
  "List of patterns to block buffer access from MCP tools.
Buffers containing any of these strings (case-insensitive) in their 
name or associated file path will be blocked from MCP tool access.
This helps prevent accidental exposure of sensitive information."
  :type '(repeat string)
  :group 'claude-code)

(defcustom claude-code-mcp-restrict-file-access t
  "Whether to restrict MCP file access to current directory and /tmp/ClaudeWorkingFolder/.
When non-nil, absolute file paths outside /tmp/ClaudeWorkingFolder/ are blocked,
and directory traversal (../) is prevented. When nil, file path restrictions
are relaxed, but buffer blocking patterns still apply for security."
  :type 'boolean
  :group 'claude-code)

(defvar claude-code-mcp-server-process nil
  "Process for the MCP TCP server.")

(defvar claude-code-mcp-bridge-process nil
  "Process for the MCP bridge server.")

(defvar claude-code-mcp-client-connections nil
  "List of active MCP client connections.")

(defvar claude-code-mcp-bridge-restart-count 0
  "Number of times the MCP bridge has been restarted.")

(defvar claude-code-mcp-bridge-last-restart-time nil
  "Time of the last MCP bridge restart attempt.")

(defvar claude-code-mcp-bridge-max-restart-attempts 5
  "Maximum number of restart attempts before giving up.")

(defvar claude-code-mcp-bridge-restart-interval 10
  "Minimum seconds between restart attempts.")

(defun claude-code-mcp-buffer-blocked-p (buffer-name)
  "Check if BUFFER-NAME should be blocked from MCP tool access.
Returns t if the buffer name or associated file path contains any
pattern from `claude-code-mcp-blocked-buffer-patterns'."
  (when buffer-name
    (let ((buffer (get-buffer buffer-name))
          (blocked nil))
      (when buffer
        (let ((name (buffer-name buffer))
              (file-path (buffer-file-name buffer)))
          (dolist (pattern claude-code-mcp-blocked-buffer-patterns)
            (when (or (and name (string-match-p (regexp-quote pattern) name t))
                      (and file-path (string-match-p (regexp-quote pattern) file-path t)))
              (setq blocked t))))))))

(defun claude-code-mcp-file-access-allowed-p (file-path)
  "Check if FILE-PATH should be allowed for MCP tool access.
Returns t if access is allowed, nil if blocked.
Respects `claude-code-mcp-restrict-file-access' setting."
  (when file-path
    (if claude-code-mcp-restrict-file-access
        ;; Apply restrictions when enabled
        (and 
         ;; Block directory traversal
         (not (or (string-match-p "\\.\\./\\|\\.\\.\\\\", file-path)))
         ;; Block absolute paths outside allowed directories
         (or (not (string-match-p "^/" file-path))
             (string-match-p "^/tmp/ClaudeWorkingFolder/" file-path)))
      ;; When restrictions disabled, allow all paths
      t)))

(defmacro claude-code-defmcp (name args docstring &rest body-and-properties)
  "Define an MCP tool function with embedded properties.
NAME is the function name, ARGS is the argument list, DOCSTRING is the documentation.
The remaining BODY-AND-PROPERTIES can contain :mcp-description, :mcp-schema, and function body."
  (let ((description nil)
        (schema nil)
        (body '()))
    
    ;; Parse body and properties
    (while body-and-properties
      (let ((item (car body-and-properties)))
        (cond
         ((eq item :mcp-description)
          (setq description (cadr body-and-properties))
          (setq body-and-properties (cddr body-and-properties)))
         ((eq item :mcp-schema)
          (setq schema (cadr body-and-properties))
          (setq body-and-properties (cddr body-and-properties)))
         (t
          (push item body)
          (setq body-and-properties (cdr body-and-properties))))))
    
    (setq body (nreverse body))
    
    `(progn
       (defun ,name ,args
         ,docstring
         ,@body)
       (put ',name :mcp-tool t)
       ,(when description
          `(put ',name :mcp-description ,description))
       ,(when schema
          `(put ',name :mcp-schema ',schema))
       ',name)))

(defun claude-code--start-mcp-server ()
  "Start the MCP TCP server."
  (when (and claude-code-mcp-enabled
             (not (and claude-code-mcp-server-process
                       (process-live-p claude-code-mcp-server-process))))
    (setq claude-code-mcp-server-process
          (make-network-process
           :name "claude-code-mcp"
           :service claude-code-mcp-port
           :server t
           :family 'ipv4
           :host 'local
           :filter 'claude-code--mcp-filter
           :sentinel 'claude-code--mcp-sentinel))
    (message "Claude Code MCP server started on port %d" claude-code-mcp-port)))

(defun claude-code--mcp-bridge-running-p ()
  "Check if an MCP bridge process is already running."
  (and claude-code-mcp-bridge-process
       (process-live-p claude-code-mcp-bridge-process)))

(defun claude-code--start-mcp-bridge-process ()
  "Start the TypeScript MCP bridge server process."
  (when (and claude-code-mcp-enabled
             (not (claude-code--mcp-bridge-running-p)))
    (let* ((package-dir (file-name-directory (or load-file-name 
                                                 (buffer-file-name)
                                                 default-directory)))
           (mcp-server-path (expand-file-name "mcp-server/dist/index.js" package-dir)))
      (if (file-exists-p mcp-server-path)
          (progn
            (message "Starting MCP bridge process...")
            ;; Clear any stale process reference
            (setq claude-code-mcp-bridge-process nil)
            ;; Wait briefly to ensure any previous process has fully terminated
            (sit-for 0.5)
            (setq claude-code-mcp-bridge-process
                  (start-process "claude-code-mcp-bridge"
                                 "*claude-code-mcp-bridge*"
                                 "node"
                                 mcp-server-path))
            ;; Add sentinel to monitor process state
            (when claude-code-mcp-bridge-process
              (set-process-sentinel claude-code-mcp-bridge-process 
                                    'claude-code--mcp-bridge-sentinel)
              (message "MCP bridge process started (PID: %s)" 
                       (process-id claude-code-mcp-bridge-process))))
        (message "MCP server script not found at: %s" mcp-server-path)))))

(defun claude-code--stop-mcp-server ()
  "Stop the MCP TCP server."
  (when (and claude-code-mcp-server-process
             (process-live-p claude-code-mcp-server-process))
    (delete-process claude-code-mcp-server-process)
    (setq claude-code-mcp-server-process nil))
  
  (dolist (conn claude-code-mcp-client-connections)
    (when (process-live-p conn)
      (delete-process conn)))
  (setq claude-code-mcp-client-connections nil))

(defun claude-code--stop-mcp-bridge-process ()
  "Stop the TypeScript MCP bridge server process."
  (when (and claude-code-mcp-bridge-process
             (process-live-p claude-code-mcp-bridge-process))
    (delete-process claude-code-mcp-bridge-process)
    (setq claude-code-mcp-bridge-process nil)))

(defun claude-code--discover-mcp-tools ()
  "Discover all available MCP tools."
  (let ((tools '()))
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (get symbol :mcp-tool))
         (let* ((name (symbol-name symbol))
                (description (or (get symbol :mcp-description) 
                                (format "MCP tool: %s" name)))
                (schema (or (get symbol :mcp-schema) '())))
           (push `((name . ,name)
                   (description . ,description)
                   (inputSchema . ,(claude-code--build-json-schema schema)))
                 tools)))))
    tools))

(defun claude-code--build-json-schema (schema)
  "Build JSON Schema from simplified schema format."
  ;; Handle quoted schemas
  (when (and (consp schema) (eq (car schema) 'quote))
    (setq schema (cadr schema)))
  
  (cond
   ;; Empty or nil schema - no parameters
   ((or (null schema) (equal schema '()))
    `((type . "object") 
      (properties . ,(make-hash-table :test 'equal))
      (required . ,(vector))
      (additionalProperties . :json-false)))
   
   ;; Simplified format - list of (name type description) tuples
   ((listp schema)
    (let ((properties (make-hash-table :test 'equal))
          (required '()))
      (dolist (item schema)
        (cond
         ;; (name "type" "description") list format  
         ((and (listp item) (>= (length item) 2))
          (let ((param-name (symbol-name (car item)))
                (param-type (cadr item))
                (param-desc (caddr item)))
            (push param-name required)
            (if param-desc
                (puthash param-name `((type . ,param-type)
                                     (description . ,param-desc)) properties)
              (puthash param-name `((type . ,param-type)) properties))))))
      
      `((type . "object")
        (properties . ,properties)
        (required . ,(vconcat (nreverse required)))
        (additionalProperties . :json-false))))
   
   ;; Fallback
   (t `((type . "object")
        (properties . ,(make-hash-table :test 'equal))
        (required . ,(vector))
        (additionalProperties . :json-false)))))

(defun claude-code--execute-mcp-tool (tool-name params)
  "Execute an MCP tool with given parameters."
  (let ((symbol (intern tool-name)))
    (if (and (fboundp symbol) (get symbol :mcp-tool))
        (condition-case err
            (let* ((func-args (help-function-arglist symbol))
                   (ordered-params (claude-code--map-params-to-args params func-args)))
              (format "%s" (apply symbol ordered-params)))
          (error (format "Error executing %s: %s" tool-name (error-message-string err))))
      (format "Tool not found: %s" tool-name))))

(defun claude-code--map-params-to-args (params-alist func-args)
  "Map parameters from alist to function arguments in correct order."
  (let ((mapped-args '()))
    (dolist (arg-spec func-args)
      (unless (memq arg-spec '(&optional &rest &key))
        (let* ((arg-name (if (listp arg-spec) (car arg-spec) arg-spec))
               (arg-key (symbol-name arg-name))
               (param-value (alist-get (intern arg-key) params-alist)))
          ;; Convert JSON arrays (vectors) to Lisp lists
          (when (vectorp param-value)
            (setq param-value (append param-value nil)))
          (push param-value mapped-args))))
    (nreverse mapped-args)))

(defun claude-code--mcp-filter (proc string)
  "Filter function for MCP TCP connections.
PROC is the process and STRING is the received data."
  (condition-case err
      (let* ((request (json-parse-string string :object-type 'alist))
             (method (alist-get 'method request))
             (id (alist-get 'id request))
             (params (alist-get 'params request))
             response)
        
        (setq response
              (cond
               ((string= method "discover_tools")
                `((jsonrpc . "2.0")
                  (id . ,id)
                  (result . ,(claude-code--discover-mcp-tools))))
               
               ((string= method "call_tool")
                (let* ((tool-name (alist-get 'name params))
                       (tool-params (alist-get 'arguments params))
                       (result (claude-code--execute-mcp-tool tool-name tool-params)))
                  `((jsonrpc . "2.0")
                    (id . ,id)
                    (result . ,result))))
               
               (t
                `((jsonrpc . "2.0")
                  (id . ,id)
                  (error . ((code . -32601)
                           (message . ,(format "Method not found: %s" method))))))))
        
        (process-send-string proc (concat (json-encode response) "\n")))
    
    (error
     (let ((error-response `((jsonrpc . "2.0")
                            (id . ,(or (ignore-errors (alist-get 'id (json-parse-string string :object-type 'alist))) 0))
                            (error . ((code . -32603)
                                     (message . ,(format "Internal error: %s" (error-message-string err))))))))
       (process-send-string proc (concat (json-encode error-response) "\n"))))))

(defun claude-code--mcp-sentinel (proc event)
  "Sentinel function for MCP TCP connections.
PROC is the process and EVENT is the event string."
  (cond
   ((string-match "open" event)
    (push proc claude-code-mcp-client-connections)
    (message "MCP client connected"))
   ((string-match "closed\\|finished" event)
    (setq claude-code-mcp-client-connections
          (delq proc claude-code-mcp-client-connections))
    (message "MCP client disconnected"))))

(defun claude-code--mcp-bridge-sentinel (proc event)
  "Sentinel function for MCP bridge process.
PROC is the process and EVENT is the event string."
  (cond
   ((string-match "finished\\|exited" event)
    (let ((exit-status (process-exit-status proc)))
      (if (= exit-status 0)
          (progn
            (message "MCP bridge process exited normally")
            ;; Reset restart count on clean exit
            (setq claude-code-mcp-bridge-restart-count 0))
        (message "MCP bridge process exited with status %d" exit-status)
        (claude-code--mcp-bridge-handle-crash)))
    (setq claude-code-mcp-bridge-process nil))
   ((string-match "killed\\|terminated" event)
    (message "MCP bridge process was killed")
    (claude-code--mcp-bridge-handle-crash)
    (setq claude-code-mcp-bridge-process nil))))

(defun claude-code--mcp-bridge-handle-crash ()
  "Handle MCP bridge process crash with backoff logic."
  (let ((current-time (float-time)))
    (setq claude-code-mcp-bridge-restart-count (1+ claude-code-mcp-bridge-restart-count))
    
    (if (> claude-code-mcp-bridge-restart-count claude-code-mcp-bridge-max-restart-attempts)
        (message "MCP bridge crashed %d times, giving up. Use `M-x claude-code-restart-mcp-server` to try again."
                 claude-code-mcp-bridge-restart-count)
      
      ;; Check if enough time has passed since last restart
      (let ((time-since-last-restart 
             (if claude-code-mcp-bridge-last-restart-time
                 (- current-time claude-code-mcp-bridge-last-restart-time)
               claude-code-mcp-bridge-restart-interval)))
        
        (if (>= time-since-last-restart claude-code-mcp-bridge-restart-interval)
            (progn
              (message "MCP bridge crashed (attempt %d/%d), restarting in background..."
                       claude-code-mcp-bridge-restart-count 
                       claude-code-mcp-bridge-max-restart-attempts)
              (setq claude-code-mcp-bridge-last-restart-time current-time)
              ;; Restart after a brief delay
              (run-at-time 2 nil 'claude-code--start-mcp-bridge-process))
          
          (let ((wait-time (- claude-code-mcp-bridge-restart-interval time-since-last-restart)))
            (message "MCP bridge crashed too soon, waiting %.1f seconds before restart..."
                     wait-time)
            (run-at-time wait-time nil 'claude-code--mcp-bridge-handle-crash)))))))

;;;###autoload
(defun claude-code-start-mcp-server ()
  "Start the MCP server for Claude integration."
  (interactive)
  (claude-code--start-mcp-server)
  (claude-code--start-mcp-bridge-process))

;;;###autoload
(defun claude-code-stop-mcp-server ()
  "Stop the MCP server."
  (interactive)
  (claude-code--stop-mcp-server)
  (claude-code--stop-mcp-bridge-process)
  (message "Claude Code MCP server stopped"))

;;;###autoload
(defun claude-code-restart-mcp-server ()
  "Restart the MCP server."
  (interactive)
  (claude-code-stop-mcp-server)
  ;; Reset restart count when manually restarting
  (setq claude-code-mcp-bridge-restart-count 0
        claude-code-mcp-bridge-last-restart-time nil)
  (sit-for 1)
  (claude-code-start-mcp-server))

;;;###autoload
(defun claude-code-mcp-status ()
  "Show the status of the MCP server."
  (interactive)
  (let ((tcp-status (if (and claude-code-mcp-server-process
                             (process-live-p claude-code-mcp-server-process))
                        "Running" "Stopped"))
        (bridge-status (if (and claude-code-mcp-bridge-process
                                (process-live-p claude-code-mcp-bridge-process))
                           "Running" "Stopped"))
        (connections (length claude-code-mcp-client-connections)))
    (message "MCP TCP Server: %s, Bridge: %s, Connections: %d"
             tcp-status bridge-status connections)))

;;;###autoload
(defun claude-code-install-mcp-server ()
  "Install the Emacs MCP server in Claude Code CLI configuration."
  (interactive)
  (let* ((package-dir (file-name-directory (or load-file-name 
                                               (buffer-file-name)
                                               default-directory)))
         (mcp-server-path (expand-file-name "mcp-server/dist/index.js" package-dir))
         (server-name "emacs")
         (install-command (format "claude mcp add %s node %s" server-name mcp-server-path)))
    
    (if (file-exists-p mcp-server-path)
        (progn
          (message "Installing Emacs MCP server...")
          (message "Running: %s" install-command)
          
          (let ((result (shell-command install-command)))
            (if (= result 0)
                (progn
                  (message "✅ Emacs MCP server installed successfully!")
                  (message "🔄 Restart Claude Code CLI to use the new MCP tools")
                  (message "📚 Load tools with: (load-file \"examples/mcp-tools.el\")")
                  (when (y-or-n-p "Load example MCP tools now? ")
                    (load-file (expand-file-name "examples/mcp-tools.el" package-dir))
                    (message "📦 Example MCP tools loaded!")))
              (message "❌ Failed to install MCP server. Exit code: %d" result))))
      
      (message "❌ MCP server not found at: %s" mcp-server-path)
      (message "💡 Build it first with: cd mcp-server && npm install && npm run build"))))

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
