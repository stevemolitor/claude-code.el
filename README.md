# claude-code.el

An Emacs interface for [Claude Code CLI](https://github.com/anthropics/claude-code), providing integration between Emacs and Claude AI for coding assistance.

## Features

- **Seamless Emacs Integration** - Start, manage, and interact with Claude without leaving Emacs
- **Stay in Your Buffer** - Send code, regions, or commands to Claude while keeping your focus
- **Fix Errors Instantly** - Point at a flycheck/flymake error and ask Claude to fix it
- **Multiple Instances** - Run separate Claude sessions for different projects or tasks
- **Quick Responses** - Answer Claude with a keystroke (<return>/<escape>/1/2/3) without switching buffers
- **Smart Context** - Optionally include file paths and line numbers when sending commands to Claude
- **Transient Menu** - Access all commands and slash commands through a transient menu
- **Continue Conversations** - Resume previous sessions or fork to earlier points
- **Read-Only Mode** - Toggle to select and copy text with normal Emacs commands and keybindings
- **Mode Cycling** - Quick switch between default, auto-accept edits, and plan modes
- **Desktop Notifications** - Get notified when Claude finishes processing
- **Terminal Choice** - Works with both eat and vterm backends
- **Fully Customizable** - Configure keybindings, notifications, and display preferences

## Installation

### Prerequisites

- Emacs 30.0 or higher
- [Claude Code CLI](https://github.com/anthropics/claude-code) installed and configured
- Required: transient (0.7.5+) inheritenv (0.2)
- Optional: eat (0.9.2+) for eat backend, vterm for vterm backend
  - Note: If not using a `:vc` install, the `eat` package requires NonGNU ELPA:
    ```elisp
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
    ```
- Optional but recommended: [Monet](https://github.com/stevemolitor/monet) for IDE integration

### Using builtin use-package (Emacs 30+)

```elisp
;; add melpa to package archives, as vterm is on melpa:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; install required inheritenv dependency:
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for eat terminal backend:
(use-package eat :ensure t)

;; for vterm terminal backend:
(use-package vterm :ensure t)

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
```

### Using straight.el

```elisp
;; install required inheritenv dependency:
(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

;; for eat terminal backend:
(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

;; for vterm terminal backend:
(use-package vterm :straight t)

;; install claude-code.el, using :depth 1 to reduce download size:
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode))
```

## Basic Usage

### Setting Prefix Key
You need to set your own key binding for the Claude Code command map, as described in the [Installation](#installation) section. The examples in this README use `C-c c` as the prefix key.

### Picking Eat or Vterm

By default claude-code.el uses the `eat` backend. If you prefer vterm customize
`claude-code-terminal-backend`:

```elisp
(setq claude-code-terminal-backend 'vterm)
```

### Transient Menu

You can see a menu of the important commands by invoking the transient, `claude-code-transient` (`C-c c m`):

![](./images/transient.png)

### Starting and Stopping Claude

To start Claude, run `claude-code` (`C-c c c`). This will start a new Claude instance in the root
project directory of the buffer file, or the current directory if outside of a project.
Claude-code.el uses Emacs built-in
[project.el](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html) which works
with most version control systems.

To start Claude in a specific directory use `claude-code-start-in-directory` (`C-c c d`). It will
prompt you for the directory.

The `claude-code-continue` command will continue the previous conversation, and `claude-code-resume` will let you pick from a list of previous sessions.

To kill the Claude process and close its window use `claude-code-kill` (`C-c c k`).

### Sending Commands to Claude

Once Claude has started, you can switch to the Claude buffer and start entering prompts.
Alternately, you can send prompts to Claude using the minibuffer via `claude-code-send-command`
(`C-c c s`). `claude-code-send-command-with-context` (`C-c c x`) will also send the current file name and line
number to Claude. This is useful for asking things like "what does this code do?", or "fix the bug
in this code".

Use the `claude-code-send-region` (`C-c c r`) command to send the selected region to Claude, or the entire buffer if no region is selected. This command is useful for writing a prompt in a regular Emacs buffer and sending it to Claude. With a single prefix arg (`C-u C-c c r`) it will prompt for extra context before sending the region to Claude.

You can also send files directly to Claude using `claude-code-send-file` to send any file by path, or `claude-code-send-buffer-file` (`C-c c o`) to send the file associated with the current buffer. The `claude-code-send-buffer-file` command supports prefix arguments similar to `claude-code-send-region` - with a single prefix arg it prompts for instructions, and with double prefix it also switches to the Claude buffer.

If you put your cursor over a flymake or flycheck error, you can ask Claude to fix it via `claude-code-fix-error-at-point` (`C-c c e`).

To show and hide the Claude buffer use `claude-code-toggle` (`C-c c t`).  To jump to the Claude buffer use `claude-code-switch-to-buffer` (`C-c c b`). This will open the buffer if hidden.

### Managing Claude Windows

The `claude-code-toggle` (`C-c c t`) will show and hide the Claude window. Use the `claude-code-switch-to-buffer` (`C-c c b`) command to switch to the Claude window even if it is hidden. 

To enter read-only mode in the Claude buffer use `claude-code-toggle-read-only-mode` (`C-c c z`). In this mode you can select and copy text, and use regular Emacs keybindings. To exit read-only mode invoke `claude-code-toggle-read-only-mode` again.

### Quick Responses

Sometimes you want to send a quick response to Claude without switching to the Claude buffer. The following commands let you answer a query from Claude without leaving your current editing buffer:

- `claude-code-send-return` (`C-c c y`) - send the return or enter key to Claude, commonly used to respond with "Yes" to Claude queriesy
- `claude-code-send-escape` (`C-c c n`) - send the escape key, to say "No" to Claude or to cancel a running Claude action
- `claude-code-send-1` (`C-c c 1`) - send "1" to Claude, to choose option "1" in response to a Claude query
- `claude-code-send-2` (`C-c c 2`) - send "2" to Claude
- `claude-code-send-3` (`C-c c 3`) - send "3" to Claude

## IDE Integration with [Monet](https://github.com/stevemolitor/monet)
You can optionally use [Monet](https://github.com/stevemolitor/monet) for IDE integration. To integrate Monet with Claude do this (or the equivalent `use-package` declaration shown above):

```elisp
(add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
(monet-mode 1)
```

When Claude starts a new instance it will automatically start a Monet websocket server to listen to and send IDE comments to/from Claude. Current selection will automatically be sent to Claude, and Claude will show diffs in Emacs, use Emacs Monet tools to open files, get diagnostics, etc. See the [Monet](https://github.com/stevemolitor/monet) documentation for more details.

## Working with Multiple Claude Instances

`claude-code.el` supports running multiple Claude instances across different projects and directories. Each Claude instance is associated with a specific directory (project root, file directory, or current directory).

#### Instance Management

- When you start Claude with `claude-code`, it creates an instance for the current directory
- If a Claude instance already exists for the directory, you'll be prompted to name the new instance (e.g., "tests", "docs")
- You can also use `claude-code-new-instance` to explicitly create a new instance with a custom name
- Buffer names follow the format:
  - `*claude:/path/to/directory:instance-name*` (e.g., `*claude:/home/user/project:tests*`)
- If you're in a directory without a Claude instance but have instances running in other directories, you'll be prompted to select one
- Your selection is remembered for that directory, so you won't be prompted again

### Instance Selection

Commands that operate on an instance (`claude-send-command`, `claude-code-switch-to-buffer`, `claude-code-kill`, etc.) will prompt you for the Claude instance if there is more than one instance associated with the current buffer's project.

If the buffer file is not associated with a running Claude instance, you can select an instance running in a different project. This is useful when you want Claude to analyze dependent projects or files that you have checked out in sibling directories.

Claude-code.el remembers which buffers are associated with which Claude instances, so you won't be repeatedly prompted. This association also helps claude-code.el "do the right thing" when killing a Claude process and deleting its associated buffer.

### Multiple Instances Per Directory

You can run multiple Claude instances for the same directory to support different workflows:

- The first instance in a directory is the "default" instance
- Additional instances require a name when created (e.g., "tests", "docs", "refactor")
- When multiple instances exist for a directory, commands that interact with Claude will prompt you to select which instance to use
- Use `C-u claude-code-switch-to-buffer` to see all Claude instances across all directories (not just the current directory)
- Use `claude-code-select-buffer` as a dedicated command to always show all Claude instances across all directories

This allows you to have separate Claude conversations for different aspects of your work within the same project, such as one instance for writing code and another for writing tests.

## Working in the Claude Buffer

claude-code.el is designed to support using Claude Code in Emacs using the minibuffer and regular Emacs buffers, with normal keybindings and full Emacs editing facilities. However, claude-code.el also adds a few niceties for working in the Claude Code terminal buffer:

You can type `C-g` as an alternative to escape. Also claude-code.el supports several options for
entering newlines in the Claude Code session:

- **Default (newline-on-shift-return)**: Press `Shift-Return` to insert a newline, `Return` to send your message
- **Alt-return style**: Press `Alt-Return` to insert a newline, `Return` to send
- **Shift-return to send**: Press `Return` to insert a newline, `Shift-Return` to send
- **Super-return to send**: Press `Return` to insert a newline, `Command-Return` (macOS) to send

You can change this behavior by customizing `claude-code-newline-keybinding-style` (see [Customization](#customization)).

### Command Reference

- `claude-code-transient` (`C-c c m`) - Show all commands (transient menu)
- `claude-code` (`C-c c c`) - Start Claude. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-start-in-directory` (`C-c c d`) - Prompt for a directory and start Claude there. With prefix arg (`C-u`), switches to the Claude buffer after creating
- `claude-code-continue` (`C-c c C`) - Start Claude and continue the previous conversation. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-resume` (`C-c c R`) - Resume a specific Claude session from an interactive list. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-new-instance` (`C-c c i`) - Create a new Claude instance with a custom name. Always prompts for instance name, unlike `claude-code` which uses "default" when no instances exist. With prefix arg (`C-u`), switches to the Claude buffer after creating. With double prefix (`C-u C-u`), prompts for the project directory
- `claude-code-kill` (`C-c c k`) - Kill Claude session
- `claude-code-kill-all` (`C-c c K`) - Kill ALL Claude instances across all directories
- `claude-code-send-command` (`C-c c s`) - Send command to Claude. With prefix arg (`C-u`), switches to the Claude buffer after sending
- `claude-code-send-command-with-context` (`C-c c x`) - Send command with current file and line context. With prefix arg (`C-u`), switches to the Claude buffer after sending
- `claude-code-send-region` (`C-c c r`) - Send the current region or buffer to Claude. With prefix arg (`C-u`), prompts for instructions to add to the text. With double prefix (`C-u C-u`), adds instructions and switches to Claude buffer
- `claude-code-send-file` - Send a specified file to Claude. Prompts for file path
- `claude-code-send-buffer-file` (`C-c c o`) - Send the file associated with current buffer to Claude. With prefix arg (`C-u`), prompts for instructions to add to the file. With double prefix (`C-u C-u`), adds instructions and switches to Claude buffer
- `claude-code-fix-error-at-point` (`C-c c e`) - Ask Claude to fix the error at the current point (works with flycheck, flymake, and any system that implements help-at-pt). With prefix arg (`C-u`), switches to the Claude buffer after sending
- `claude-code-fork` (`C-c c f`) - Fork conversation (jump to previous conversation by sending escape-escape to Claude)
- `claude-code-slash-commands` (`C-c c /`) - Access Claude slash commands menu
- `claude-code-toggle` (`C-c c t`) - Toggle Claude window
- `claude-code-switch-to-buffer` (`C-c c b`) - Switch to the Claude buffer. With prefix arg (`C-u`), shows all Claude instances across all directories
- `claude-code-select-buffer` (`C-c c B`) - Select and switch to a Claude buffer from all running instances across all projects and directories
- `claude-code-toggle-read-only-mode` (`C-c c z`) - Toggle between read-only mode and normal mode in Claude buffer (useful for selecting and copying text)
- `claude-code-cycle-mode` (`C-c c M`) - Send Shift-Tab to Claude to cycle between default mode, auto-accept edits mode, and plan mode. See the installation section above to configure a repeat map so that you can cycle thru the modes with "M" after the initial invocation.

- `claude-code-send-return` (`C-c c y`) - Send return key to Claude (useful for confirming with Claude without switching to the Claude REPL buffer) (useful for responding with "Yes"  to Claude)
- `claude-code-send-escape` (`C-c c n`) - Send escape key to Claude (useful for saying "No" when Claude asks for confirmation without switching to the Claude REPL buffer)
- `claude-code-send-1` (`C-c c 1`) - Send "1" to Claude (useful for selecting the first option when Claude presents a numbered menu)
- `claude-code-send-2` (`C-c c 2`) - Send "2" to Claude (useful for selecting the second option when Claude presents a numbered menu)
- `claude-code-send-3` (`C-c c 3`) - Send "3" to Claude (useful for selecting the third option when Claude presents a numbered menu)

## Desktop Notifications

claude-code.el notifies you when Claude finishes processing and is waiting for input. By default, it displays a message in the minibuffer and pulses the modeline for visual feedback.

### macOS Native Notifications

To use macOS native notifications with sound, add this to your configuration:

```elisp
(defun my-claude-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

(setq claude-code-notification-function #'my-claude-notify)
```

This will display a system notification with a "Glass" sound effect when Claude is ready. You can change the sound name to any system sound (e.g., "Ping", "Hero", "Morse", etc.) or remove the `sound name` part for silent notifications.

### Linux Native Notifications

For Linux desktop notifications, you can use `notify-send` (GNOME/Unity) or `kdialog` (KDE):

```elisp
;; For GNOME/Unity desktops
(defun my-claude-notify (title message)
  "Display a Linux notification using notify-send."
  (if (executable-find "notify-send")
      (call-process "notify-send" nil nil nil title message)
    (message "%s: %s" title message)))

(setq claude-code-notification-function #'my-claude-notify)
```

To add sound on Linux:

```elisp
(defun my-claude-notify-with-sound (title message)
  "Display a Linux notification with sound."
  (when (executable-find "notify-send")
    (call-process "notify-send" nil nil nil title message))
  ;; Play sound if paplay is available
  (when (executable-find "paplay")
    (call-process "paplay" nil nil nil "/usr/share/sounds/freedesktop/stereo/message.oga")))

(setq claude-code-notification-function #'my-claude-notify-with-sound)
```

### Windows Native Notifications

For Windows, you can use PowerShell to create toast notifications:

```elisp
(defun my-claude-notify (title message)
  "Display a Windows notification using PowerShell."
  (call-process "powershell" nil nil nil
                "-NoProfile" "-Command"
                (concat "[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null; "
                        "$template = '<toast><visual><binding template=\"ToastGeneric\"><text>" title "</text><text>" message "</text></binding></visual></toast>'; "
                        "$xml = New-Object Windows.Data.Xml.Dom.XmlDocument; "
                        "$xml.LoadXml($template); "
                        "$toast = [Windows.UI.Notifications.ToastNotification]::new($xml); "
                        "[Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier('Emacs').Show($toast)")))

(setq claude-code-notification-function #'my-claude-notify)
```

*Note: Linux and Windows examples are untested. Feedback and improvements are welcome!*

### Claude Code Hooks Integration

claude-code.el provides integration to **receive** hook events from Claude Code CLI via emacsclient. 

See [`examples/hooks/claude-code-hook-examples.el`](examples/hooks/claude-code-hook-examples.el) for comprehensive examples of hook listeners and setup functions.

#### Hook API

- `claude-code-event-hook` - Emacs hook run when Claude Code CLI triggers events
- `claude-code-handle-hook` - **Unified entry point** for all Claude Code CLI hooks. Call this from your CLI hooks with `(type buffer-name &rest args)` and JSON data as additional emacsclient arguments

#### JSON Response System

Hooks can return structured JSON data to control Claude Code behavior using `run-hook-with-args-until-success`:

1. **Multiple handlers**: Register multiple functions on `claude-code-event-hook`
2. **Sequential execution**: Functions are called in order with the message data  
3. **First response wins**: Execution stops when a function returns non-nil JSON
4. **Bidirectional communication**: The JSON response is sent back to Claude Code CLI

This enables interactive workflows like permission prompts where hooks can influence Claude's behavior.

#### Setup

1. **Add the bin directory to your PATH** (required for hook wrapper script):
   ```bash
   export PATH="/path/to/claude-code.el/bin:$PATH"
   ```
   Add this to your bash configuration file (~/.bashrc, ~/.bash_profile, etc.) since Claude Code needs it in the bash environment.

2. **Start the Emacs server** so that `emacsclient` can communicate with your Emacs instance:
   ```elisp
   ;; Start the Emacs server (add this to your init.el)
   (start-server)

   ;; Add your hook listeners using standard Emacs functions
   (add-hook 'claude-code-event-hook 'my-claude-hook-listener)
   ```

#### Custom Hook Listener

Hook listeners receive a message plist with these keys:
- `:type` - Hook type (e.g., `'notification`, `'stop`, `'pre-tool-use`, `'post-tool-use`)
- `:buffer-name` - Claude buffer name from `$CLAUDE_BUFFER_NAME`
- `:json-data` - JSON payload from Claude CLI
- `:args` - List of additional arguments (when using extended configuration)

```elisp
;; Define your own hook listener function
(defun my-claude-hook-listener (message)
  "Custom listener for Claude Code hooks.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data))
        (args (plist-get message :args)))
    (cond 
     ((eq hook-type 'notification)
      (message "Claude is ready in %s! JSON: %s" buffer-name json-data))
     ((eq hook-type 'stop)  
      (message "Claude finished in %s! JSON: %s" buffer-name json-data))
     (t
      (message "Claude hook: %s with JSON: %s" hook-type json-data)))))

;; Add the hook listener using standard Emacs hook functions
(add-hook 'claude-code-event-hook 'my-claude-hook-listener)
```

See the examples file for complete listeners that demonstrate notifications, logging, org-mode integration, and using extra arguments from the `:args` field.

#### Claude Code CLI Configuration

Configure Claude Code CLI hooks to call `claude-code-handle-hook` via emacsclient by passing JSON data as an additional argument:

```json
{
  "hooks": {
    "Notification": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "emacsclient --eval \"(claude-code-handle-hook 'notification \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
          }
        ]
      }
    ],
    "Stop": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "emacsclient --eval \"(claude-code-handle-hook 'stop \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
          }
        ]
      }
    ]
  }
}
```

The command pattern:  
```bash
emacsclient --eval "(claude-code-handle-hook 'notification \"$CLAUDE_BUFFER_NAME\")" "$(cat)" "ARG1" "ARG2" "ARG3"
```

Where:
- `"$(cat)"` - JSON data from stdin (always required)
- `ARG1` is `"$PWD"` - current working directory  
- `ARG2` is `"$(date -Iseconds)"` - timestamp
- `ARG3` is `"$$"` - process ID

`claude-code-handle-hook` creates a message plist sent to listeners:
```elisp
(list :type 'notification 
      :buffer-name "$CLAUDE_BUFFER_NAME"
      :json-data "$(cat)" 
      :args '("ARG1" "ARG2" "ARG3"))
```

See the [Claude Code hooks documentation](https://docs.anthropic.com/en/docs/claude-code/hooks) for details on setting up CLI hooks.

## Tips and Tricks

- **Paste images**: Use `C-v` to paste images into the Claude window. Note that on macOS, this is `Control-v`, not `Command-v`.
- **Paste text**: Use `C-y` (`yank`) to paste text into the Claude window. 
- **Save files before sending commands**: Claude reads files directly from disk, not from Emacs buffers. Always save your files (`C-x C-s`) before sending commands that reference file content. Consider enabling `global-auto-revert-mode` to automatically sync Emacs buffers with file changes made by Claude:
  ```elisp
  (global-auto-revert-mode 1)
  ;; If files aren't reliably auto-reverting after Claude makes changes,
  ;; disable file notification and use polling instead:
  (setq auto-revert-use-notify nil)
  ```
- **Auto-revert with hooks**: For more control over buffer reverting, use the auto-revert hook example that listens for Claude's file edits:
  ```elisp
  ;; Load the auto-revert hook
  (load-file "examples/hooks/claude-code-auto-revert-hook.el")
  ;; Set up auto-revert (choose one):
  (setup-claude-auto-revert)           ; Safe mode - skips modified buffers
  (setup-claude-auto-revert-aggressive) ; Prompts to revert modified buffers
  (setup-claude-auto-revert-org)       ; Special handling for org files
  ```
  Then configure the PostToolUse hook in your `~/.claude/settings.json` (see `examples/hooks/auto-revert-settings.json`) 

## Customization

```elisp
;; Set your key binding for the command map.
(global-set-key (kbd "C-c C-a") claude-code-command-map)

;; Set terminal type for the Claude terminal emulation (default is "xterm-256color").
;; This determines terminal capabilities like color support.
;; See the documentation for eat-term-name for more information.
(setq claude-code-term-name "xterm-256color")

;; Change the path to the Claude executable (default is "claude").
;; Useful if Claude is not in your PATH or you want to use a specific version.
(setq claude-code-program "/usr/local/bin/claude")

;; Set command line arguments for Claude
;; For example, to enable verbose output
(setq claude-code-program-switches '("--verbose"))

;; Add hooks to run after Claude is started
(add-hook 'claude-code-start-hook 'my-claude-setup-function)

;; Adjust initialization delay (default is 0.1 seconds)
;; This helps prevent terminal layout issues if the buffer is displayed before Claude is fully ready.
(setq claude-code-startup-delay 0.2)

;; Configure the buffer size threshold for confirmation prompt (default is 100000 characters)
;; If a buffer is larger than this threshold, claude-code-send-region will ask for confirmation
;; before sending the entire buffer to Claude.
(setq claude-code-large-buffer-threshold 100000)

;; Configure key binding style for entering newlines and sending messages in Claude buffers.
;; Available styles:
;;   'newline-on-shift-return - S-return inserts newline, RET sends message (default)
;;   'newline-on-alt-return   - M-return inserts newline, RET sends message
;;   'shift-return-to-send    - RET inserts newline, S-return sends message
;;   'super-return-to-send    - RET inserts newline, s-return sends message (Command+Return on macOS)
(setq claude-code-newline-keybinding-style 'newline-on-shift-return)

;; Enable or disable notifications when Claude finishes and awaits input (default is t).
(setq claude-code-enable-notifications t)

;; Customize the notification function (default is claude-code--default-notification).
;; The function should accept two arguments: title and message.
;; The default function displays a message and pulses the modeline for visual feedback.
(setq claude-code-notification-function 'claude-code--default-notification)

;; Example: Use your own notification function
(defun my-claude-notification (title message)
  "Custom notification function for Claude Code."
  ;; Your custom notification logic here
  (message "[%s] %s" title message))
(setq claude-code-notification-function 'my-claude-notification)

;; Configure kill confirmation behavior (default is t).
;; When t, claude-code-kill prompts for confirmation before killing instances.
;; When nil, kills Claude instances without confirmation.
(setq claude-code-confirm-kill t)

;; Enable/disable window resize optimization (default is t)
;; When enabled, terminal reflows are only triggered when window width changes,
;; not when only height changes. This prevents unnecessary redraws when splitting
;; windows vertically, improving performance and reducing visual artifacts.
;; Set to nil if you experience issues with terminal display after resizing.
(setq claude-code-optimize-window-resize t)

;; Enable/disable no-delete-other-windows parameter (default is nil)
;; When enabled, Claude Code windows have the no-delete-other-windows
;; parameter set. This prevents the Claude window from being closed
;; when you run delete-other-windows or similar commands, keeping the
;; Claude buffer visible and accessible.
(setq claude-code-no-delete-other-windows t)

;; Automatically select the Claude buffer when toggling it open (default is nil)
;; When set to t, claude-code-toggle will switch focus to the Claude buffer
;; after displaying it. When nil, the buffer is displayed but focus remains
;; in the current buffer.
(setq claude-code-toggle-auto-select t)
```

### Customizing Window Position

#### Using the Display Window Function

You can customize how Claude Code windows are displayed by setting `claude-code-display-window-fn`. This function is called with the Claude buffer and should display it appropriately:

```elisp
;; Use display-buffer with custom configuration
(setq claude-code-display-window-fn #'display-buffer)

;; Example: Display in a side window using popwin
(setq claude-code-display-window-fn #'display-buffer)
(let ((buffer-regexp "^\\*claude:.+:.+\\*$"))
  (push `(,buffer-regexp :regexp t :width 78 :position left :stick t :noselect nil :dedicated nil)
        popwin:special-display-config))

;; Example: Always display in a side window on the right
(defun my-claude-display-right (buffer)
  "Display Claude buffer in right side window."
  (display-buffer buffer '((display-buffer-in-side-window)
                           (side . right)
                           (window-width . 90))))
(setq claude-code-display-window-fn #'my-claude-display-right)
```

#### Using display-buffer-alist

You can also control how the Claude Code window appears using Emacs' `display-buffer-alist`. For example, to make the Claude window appear in a persistent side window on the right side of your screen that is 90 characters wide:

```elisp
(add-to-list 'display-buffer-alist
                 '("^\\*claude"
                   (display-buffer-in-side-window)
                   (side . right)
                   (window-width . 90)))
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

*Note*: Recent changes to claude-code.el have fixed flickering issues, making customization of these latency values less necessary. 

### Fixing Spaces Between Vertical Bars

If you see spaces between vertical bars in Claude's output, you can fix this by adjusting the `line-spacing` value. For example:

```elisp
;; Set line spacing to reduce gaps between vertical bars
(setq line-spacing 0.1)
```

Or to apply it only to Claude buffers:

```elisp
(add-hook 'claude-code-start-hook
          (lambda ()
            ;; Reduce line spacing to fix vertical bar gaps
            (setq-local line-spacing 0.1))) 
```

## Demo

### GIF Demo

![Claude Code Emacs Demo](./images/demo.gif)

This [demo](./demo.gif) shows claude-code.el in action, including accessing the transient menu, sending commands with file context, and fixing errors.

### Video Demo

[![The Emacs Claude Code Package](https://img.youtube.com/vi/K8sCVLmFyyU/0.jpg)](https://www.youtube.com/watch?v=K8sCVLmFyyU)

Check out this [video demo](https://www.youtube.com/watch?v=K8sCVLmFyyU) demonstrating the claude-code.el package. This video was kindly created and shared by a user of the package.

### Eat-specific Customization

When using the eat terminal backend, there are additional customization options available:

```elisp
;; Customize cursor type in read-only mode (default is '(box nil nil))
;; The format is (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF)
;; Cursor type options: 'box, 'hollow, 'bar, 'hbar, or nil
(setq claude-code-eat-read-only-mode-cursor-type '(bar nil nil))

;; Control eat scrollback size for longer conversations
;; The default is 131072 characters, which is usually sufficient
;; For very long Claude sessions, you may want to increase it
;; WARNING: Setting to nil (unlimited) is NOT recommended with Claude Code
;; as it can cause severe performance issues with long sessions
(setq eat-term-scrollback-size 500000)  ; Increase to 500k characters
```

### Vterm-specific Customization

When using the vterm terminal backend, there are additional customization options available:

```elisp
;; Enable/disable buffering to prevent flickering on multi-line input (default is t)
;; When enabled, vterm output that appears to be redrawing multi-line input boxes
;; will be buffered briefly and processed in a single batch
;; This prevents flickering when Claude redraws its input box as it expands
(setq claude-code-vterm-buffer-multiline-output t)

;; Control the delay before processing buffered vterm output (default is 0.01)
;; This is the time in seconds that vterm waits to collect output bursts
;; A longer delay may reduce flickering more but could feel less responsive
;; The default of 0.01 seconds (10ms) provides a good balance
(setq claude-code-vterm-multiline-delay 0.01)
```

#### Vterm Scrollback Configuration

Vterm has its own scrollback limit that is separate from claude-code.el settings. By default, vterm limits scrollback to 1000 lines. To allow scrolling back to the top of long Claude conversations, you can increase `vterm-max-scrollback`:

```elisp
;; Increase vterm scrollback to 100000 lines (the maximum allowed)
;; Note: This increases memory usage
(setq vterm-max-scrollback 100000)
```

If you prefer not to set this globally, you can set it only for Claude buffers using a hook:

```elisp
(add-hook 'claude-code-start-hook
          (lambda ()
            ;; Only increase scrollback for vterm backend
            (when (eq claude-code-terminal-backend 'vterm)
              (setq-local vterm-max-scrollback 100000))))
```

This ensures that only Claude buffers have increased scrollback, while other vterm buffers maintain the default limit.

#### Vterm Window Width Configuration

Vterm has a minimum window width setting that affects how text wraps. By default, `vterm-min-window-width` is set to 80 columns. If you resize the Claude window to be narrower than this limit, the Claude input box may wrap incorrectly, causing display issues.

If you prefer to use Claude in a narrow window (for example, in a side window), you can adjust `vterm-min-window-width`. Note that this must be set as a custom variable, either via `custom-set-variables` or `setop`, `setq` won't work:

```elisp
;; Allow vterm windows to be as narrow as 40 columns
(setopt vterm-min-window-width 40)
```

This is particularly useful if you like to keep Claude in a narrow side window while coding in your main window.

#### Vterm Timer Delay

The `vterm-timer-delay` variable controls how often vterm refreshes its buffer when receiving data. This delay (in seconds) helps manage performance when processing large amounts of output. Setting it to `nil` disables the delay entirely.

The default value of `0.1` seconds works well with Claude Code. Since Claude often sends large bursts of data when generating code or explanations, reducing this delay or disabling it (`nil`) can significantly degrade performance. Stick with the default, or use a slightly higher value  unless you experience specific display issues. 

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the Apache License 2.0 - see the LICENSE file for details.
