# claude-code.el

An Emacs interface for [Claude Code CLI](https://github.com/anthropics/claude-code), providing integration between Emacs and Claude AI for coding assistance.

## Features

- Start, stop, and toggle Claude Code sessions directly from Emacs
- Continue and resume previous Claude Code conversations
- Support for multiple Claude instances across different projects and directories
- Send commands or region to Claude from normal Emacs buffers or the minibuffer, with or without
  file/line context 
- Send quick responses to Claude from Emacs minibuffer - cycle accept edits / plan mode, quick commands to send yes, no, 1, 2, 3, etc to Claude.
- Quick access to all Claude slash commands via transient menus
- Fix error at point - Send flycheck or flymake error at point to Claude, have Claude fix it.
- Commands to show, hide, switch to Claude terminal buffers
- Claude terminal niceties - `C-g` to quit/escape, 3 options for entering newlines and submitting.
- Desktop notifications when Claude finishes processing and awaits input
- Customizable key bindings and appearance settings

## Installation

### Prerequisites

- Emacs 30.0 or higher
- [Claude Code CLI](https://github.com/anthropics/claude-code) installed and configured
- Required Emacs packages: transient (0.4.0+), eat (0.9.2+)

### Using builtin use-package (Emacs 30+)

```elisp
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)) ;; or your preferred key
```

### Using straight.el

```elisp
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  (claude-code-mode))
```

## Usage

You need to set your own key binding for the Claude Code command map. The examples in this README use `C-c c` as the prefix key.

### Basic Commands

- `claude-code` (`C-c c c`) - Start Claude. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-continue` (`C-c c C`) - Start Claude and continue the previous conversation. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-resume` (`C-c c R`) - Resume a specific Claude session by ID or choose interactively. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-new-instance` (`C-c c i`) - Create a new Claude instance with a custom name. Always prompts for instance name, unlike `claude-code` which uses "default" when no instances exist. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-start-in-directory` (`C-c c d`) - Prompt for a directory and start Claude there. With prefix arg (`C-u`), switches to the Claude buffer after creating
- `claude-code-toggle` (`C-c c t`) - Toggle Claude window
- `claude-code-switch-to-buffer` (`C-c c b`) - Switch to the Claude buffer. With prefix arg (`C-u`), shows all Claude instances across all directories
- `claude-code-select-buffer` (`C-c c B`) - Select and switch to a Claude buffer from all running instances across all projects and directories
- `claude-code-kill` (`C-c c k`) - Kill Claude session
- `claude-code-kill-all` (`C-c c K`) - Kill ALL Claude instances across all directories
- `claude-code-send-command` (`C-c c s`) - Send command to Claude
- `claude-code-send-command-with-context` (`C-c c x`) - Send command with current file and line context
- `claude-code-send-region` (`C-c c r`) - Send the current region or buffer to Claude. With prefix arg (`C-u`), prompts for instructions to add to the text. With double prefix (`C-u C-u`), adds instructions and switches to Claude buffer
- `claude-code-fix-error-at-point` (`C-c c e`) - Ask Claude to fix the error at the current point (works with flycheck, flymake, and any system that implements help-at-pt)
- `claude-code-slash-commands` (`C-c c /`) - Access Claude slash commands menu
- `claude-code-transient` (`C-c c m`) - Show all commands (transient menu)
- `claude-code-send-return` (`C-c c y`) - Send return key to Claude (useful for confirming with Claude without switching to the Claude REPL buffer)
- `claude-code-send-escape` (`C-c c n`) - Send escape key to Claude (useful for saying "No" when Claude asks for confirmation without switching to the Claude REPL buffer)
- `claude-code-fork` (`C-c c f`) - Fork conversation (jump to previous conversation by sending escape-escape to Claude)
- `claude-code-send-1` (`C-c c 1`) - Send "1" to Claude (useful for selecting the first option when Claude presents a numbered menu)
- `claude-code-send-2` (`C-c c 2`) - Send "2" to Claude (useful for selecting the second option when Claude presents a numbered menu)
- `claude-code-send-3` (`C-c c 3`) - Send "3" to Claude (useful for selecting the third option when Claude presents a numbered menu)
- `claude-code-cycle-mode` (`C-c c TAB`) - Send Shift-Tab to Claude to cycle between default mode, auto-accept edits mode, and plan mode

With a single prefix arg, `claude-code`, `claude-code-send-command` and
`claude-code-send-command-with-context` will switch to the Claude terminal buffer after sending the
command.

### Keybindings in Claude Buffers

In Claude buffers, the ESC key now works as expected - a single press sends an escape character to Claude. This is useful for canceling operations or saying "No" to Claude prompts. You can also use `C-g` as an alternative way to send escape/cancel.

The behavior of the return key and its modifiers can be customized using the `claude-code-newline-keybinding-style` variable. By default, pressing RET sends your message to Claude while M-return (Meta/Alt + Return) inserts a newline. This can be changed to match your preferences - for example, you can configure it so that RET inserts newlines and M-return sends the message, or use Shift-return or Super-return for newlines. See the Customization section for available options.

### Read-Only Mode Toggle

The `claude-code-toggle-read-only-mode` command provides a convenient way to switch between normal terminal mode and read-only mode in the Claude buffer:

- `claude-code-toggle-read-only-mode` - Toggle between read-only mode and normal mode

In read-only mode, you can interact with the terminal buffer just like a regular Emacs buffer, making it easy to select and copy text. However, you cannot change the buffer contents or enter Claude commands in this mode. This is particularly useful when you need to copy output from Claude without accidentally modifying the terminal.

The command automatically detects the current mode and switches to the other:
- If in normal terminal mode (semi-char mode), it switches to read-only mode
- If in read-only mode (emacs mode), it switches back to normal terminal mode

### Continuing and Resuming Conversations

Claude Code provides two ways to restore previous conversations:

#### Continue Most Recent Conversation

Use the `claude-code-continue` command (`C-c c C`) to resume where you left off in your most recent Claude session in the current directory. This command uses Claude's `--continue` flag.

- `claude-code-continue` - Continue the most recent conversation in the current directory
- With prefix arg (`C-u`) - Continue conversation and switch to buffer
- With double prefix arg (`C-u C-u`) - Continue conversation in a specific directory (prompts for directory)

#### Resume Specific Session

Use the `claude-code-resume` command (`C-c c R`) to resume a specific past session. This command uses Claude's `--resume` flag and allows you to:

- Resume any past session by selecting from an interactive list
- Resume a specific session if you know its ID (can be passed programmatically)
- With prefix arg (`C-u`) - Resume session and switch to buffer
- With double prefix arg (`C-u C-u`) - Resume session in a specific directory (prompts for directory)

### Transient Menus

Access all commands through the transient menu with `C-c c m`.

#### Slash Commands Menu

For quick access to Claude slash commands like `/help`, `/clear`, or `/compact`, use `C-c c /` to open the slash commands menu.

### Read-Only Mode for Text Selection

In the Claude terminal, you can switch to a read-only mode to select and copy text:

- `C-c C-e` (`eat-emacs-mode`) - Switch to read-only mode with normal Emacs cursor for text selection
- `C-c C-j` (`semi-char-mode`) - Return to normal terminal mode

The cursor appearance in read-only mode can be customized via the `claude-code-read-only-mode-cursor-type` variable. This variable uses the format `(CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF)`. For more information, run `M-x describe-variable RET claude-code-read-only-mode-cursor-type RET`.

```elisp
;; Customize cursor type in read-only mode (default is '(box nil nil))
;; Cursor type options: 'box, 'hollow, 'bar, 'hbar, or nil
(setq claude-code-read-only-mode-cursor-type '(bar nil nil))
```

### Multiple Claude Instances

`claude-code.el` supports running multiple Claude instances across different projects and directories. Each Claude instance is associated with a specific directory (project root, file directory, or current directory).

#### Instance Management

- When you start Claude with `claude-code`, it creates an instance for the current directory
- If a Claude instance already exists for the directory, you'll be prompted to name the new instance (e.g., "tests", "docs")
- You can also use `claude-code-new-instance` to explicitly create a new instance with a custom name
- Buffer names follow the format:
  - `*claude:/path/to/directory*` for the default instance
  - `*claude:/path/to/directory:instance-name*` for named instances (e.g., `*claude:/home/user/project:tests*`)
- If you're in a directory without a Claude instance but have instances running in other directories, you'll be prompted to select one
- Your selection is remembered for that directory, so you won't be prompted again
- To start a new instance instead of selecting an existing one, cancel the prompt with `C-g`

#### Instance Selection

Commands that operate on an instance (`claude-send-command`, `claude-code-switch-to-buffer`, `claude-code-kill`, etc.) will prompt you for the Claude instance if there is more than one instance associated with the current buffer's project.

If the buffer file is not associated with a running Claude instance, you can select an instance running in a different project. This is useful when you want Claude to analyze dependent projects or files that you have checked out in sibling directories.

Claude-code.el remembers which buffers are associated with which Claude instances, so you won't be repeatedly prompted. This association also helps claude-code.el "do the right thing" when killing a Claude process and deleting its associated buffer.

#### Multiple Instances Per Directory

You can run multiple Claude instances for the same directory to support different workflows:

- The first instance in a directory is the "default" instance
- Additional instances require a name when created (e.g., "tests", "docs", "refactor")
- When multiple instances exist for a directory, commands that interact with Claude will prompt you to select which instance to use
- Use `C-u claude-code-switch-to-buffer` to see all Claude instances across all directories (not just the current directory)
- Use `claude-code-select-buffer` as a dedicated command to always show all Claude instances across all directories

This allows you to have separate Claude conversations for different aspects of your work within the same project, such as one instance for writing code and another for writing tests.

## Customization

```elisp
;; Set your key binding for the command map
(global-set-key (kbd "C-c C-a") claude-code-command-map)

;; Set terminal type for the Claude terminal emulation (default is "xterm-256color")
;; This determines terminal capabilities like color support
;; See the documentation for eat-term-name for more information
(setq claude-code-term-name "xterm-256color")

;; Change the path to the Claude executable (default is "claude")
;; Useful if Claude is not in your PATH or you want to use a specific version
(setq claude-code-program "/usr/local/bin/claude")

;; Set command line arguments for Claude
;; For example, to enable verbose output
(setq claude-code-program-switches '("--verbose"))

;; Add hooks to run after Claude is started
(add-hook 'claude-code-start-hook 'my-claude-setup-function)

;; Adjust initialization delay (default is 0.1 seconds)
;; This helps prevent terminal layout issues if the buffer is displayed before Claude is fully ready
(setq claude-code-startup-delay 0.2)

;; Configure the buffer size threshold for confirmation prompt (default is 100000 characters)
;; If a buffer is larger than this threshold, claude-code-send-region will ask for confirmation
;; before sending the entire buffer to Claude
(setq claude-code-large-buffer-threshold 100000)

;; Disable truncation of Claude output buffer (default is nil)
;; When set to t, claude-code.el can output display content without truncation
;; This is useful when working with large Claude buffers
(setq claude-code-never-truncate-claude-buffer t)

;; Configure key binding style for entering newlines and sending messages in Claude buffers
;; Available styles:
;;   'default              - M-return inserts newline, RET sends message (default)
;;   'newline-on-return    - RET inserts newline, M-return sends message
;;   'newline-on-shift-return - RET sends message, S-return inserts newline
;;   'super-return-to-send - RET inserts newline, s-return sends message
(setq claude-code-newline-keybinding-style 'default)

;; Enable or disable notifications when Claude finishes and awaits input (default is t)
(setq claude-code-enable-notifications t)

;; Customize the notification function (default is claude-code--default-notification)
;; The function should accept two arguments: title and message
;; The default function displays a message and pulses the modeline for visual feedback
(setq claude-code-notification-function 'claude-code--default-notification)

;; Example: Use your own notification function
(defun my-claude-notification (title message)
  "Custom notification function for Claude Code."
  ;; Your custom notification logic here
  (message "[%s] %s" title message))
(setq claude-code-notification-function 'my-claude-notification)

;; Configure kill confirmation behavior (default is t)
;; When t, claude-code-kill prompts for confirmation before killing instances
;; When nil, kills Claude instances without confirmation
(setq claude-code-confirm-kill t)
```

### Customizing Window Position

You can control how the Claude Code window appears using Emacs' `display-buffer-alist`. For example, to make the Claude window appear in a persistent side window on the right side of your screen with 33% width:

```elisp
(add-to-list 'display-buffer-alist
                 '("^\\*claude"
                   (display-buffer-in-side-window)
                   (side . right)
                   (window-width . ,width)
                   (window-parameters . ((no-delete-other-windows . t)))))
```

This layout works best on wide screens.

### Font Setup

Claude Code uses a lot of special unicode characters, and most common programming fonts don't include them all. To ensure that Claude renders special characters correctly in Emacs, you need to either use a font with really good unicode support, or set up fallback fonts for Emacs to use when your preferred font does not have a character. 

### Using System Fonts as Fallbacks

If you don't want to install any new fonts, you can use fonts already on your system as fallbacks. Here's a good setup for macOS, assuming your default, preferred font is "Maple Mono".  Substitute "Maple Mono" with whatever your default font is, and add this to your `init.el` file:

```elisp
;; important - tell emacs to use our fontset settings
(setq use-default-font-for-symbols nil)

;; add least preferred fonts first, most preferred last
(set-fontset-font t 'symbol "STIX Two Math" nil 'prepend)
(set-fontset-font t 'symbol "Zapf Dingbats" nil 'prepend)
(set-fontset-font t 'symbol "Menlo" nil 'prepend)

;; add your default, preferred font last
(set-fontset-font t 'symbol "Maple Mono" nil 'prepend)
```

The configuration on Linux or Windows will depend on the fonts available on your system. To test if
your system has a certain font, evaluate this expression:

```elisp
(find-font (font-spec :family "DejaVu Sans Mono"))
```

On Linux it might look like this:

```elisp
(setq use-default-font-for-symbols nil)
(set-fontset-font t 'symbol "DejaVu Sans Mono" nil 'prepend)

;; your preferred, default font:
(set-fontset-font t 'symbol "Maple Mono" nil 'prepend)
```

### Using JuliaMono as Fallback

A cross-platform approach is to install a fixed-width font with really good unicode symbols support. 
[JuliaMono](https://juliamono.netlify.app/) has excellent Unicode symbols support. To let the Claude Code buffer use Julia Mono for rendering Unicode characters while still using your default font for ASCII characters add this elisp code:

```elisp
(setq use-default-font-for-symbols nil)
(set-fontset-font t 'unicode (font-spec :family "JuliaMono"))

;; your preferred, default font:
(set-fontset-font t 'symbol "Maple Mono" nil 'prepend)
```

### Using a Custom Claude Code Font

If instead you want to use a particular font just for the Claude Code REPL but use a different font
everywhere else you can customize the `claude-code-repl-face`:

```elisp
(custom-set-faces
   '(claude-code-repl-face ((t (:family "JuliaMono")))))
```

(If you set the Claude Code font to "JuliaMono", you can skip all the fontset fallback configurations above.)

### Reducing Flickering on Window Configuration Changes

To reduce flickering in the Claude buffer on window configuration changes, you can adjust eat latency variables in a hook. This reduces flickering at the cost of some increased latency:

```elisp
  ;; reduce flickering
  (add-hook 'claude-code-start-hook
            (lambda ()
              (setq-local eat-minimum-latency 0.033
                          eat-maximum-latency 0.1)))
```

_Note_: Recent changes to claude-code.el have fixed flickering issues, making customization of these latency values less necessary. 

## Demo

### GIF Demo

![Claude Code Emacs Demo](./demo.gif)

This [demo](./demo.gif) shows claude-code.el in action, including accessing the transient menu, sending commands with file context, and fixing errors.

### Video Demo

[![The Emacs Claude Code Package](https://img.youtube.com/vi/K8sCVLmFyyU/0.jpg)](https://www.youtube.com/watch?v=K8sCVLmFyyU)

Check out this [video demo](https://www.youtube.com/watch?v=K8sCVLmFyyU) demonstrating the claude-code.el package. This video was kindly created and shared by a user of the package.

## Limitations

- `claude-code.el` only supports using [eat](https://codeberg.org/akib/emacs-eat) for the Claude Code terminal window. Eat provides better rendering with less flickering and visual artifacts compared to other terminal emulators like ansi-term and vterm in testing.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the Apache License 2.0 - see the LICENSE file for details.
