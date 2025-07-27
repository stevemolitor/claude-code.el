# Changelog

All notable changes to claude-code.el will be documented in this file.

## [0.4.4]

### Added
- Claude Code hooks integration support for executing shell commands in response to tool calls (#70)
  - Allows custom automation via hook configurations in settings

### Changed
- Reorganized slash commands menu for better usability (#71)
  - Grouped commands by functionality instead of alphabetically
  - Added missing slash commands (memory, add-dir, model, permissions, etc.)
  - Improved key bindings with consistent uppercase for less common commands

### Fixed
- Fixed vterm advice persistence issue that could affect other vterm buffers (#74)
  - Ensures vterm advice only applies to Claude Code buffers

### [0.4.3]

### Added
- New `claude-code-vterm-multiline-delay` customization variable to control the delay before processing buffered vterm output
  - Default value changed from 0.001 to 0.01 seconds (10ms) to better reduce flickering
  - Allows fine-tuning the balance between flickering reduction and responsiveness
  
### Fixed
- Fix bug in eat keybindings 

## [0.4.2]

### Changed
- File references now use `@file:line` format instead of verbose context format

## [0.4.1]

### Changed
- upgrade to the latest transient release

## [0.4.0]

### Changed
- `claude-code-eat-never-truncate-claude-buffer` is now obsolete
  - Setting it to t can consume excessive memory and cause performance issues with long sessions
  - The variable will be removed in a future release

### Added

- New `claude-code-eat` customization group for eat backend specific settings
  - All eat-specific faces moved to this group with `claude-code-eat-` prefix
  - Faces can be customized via `M-x customize-group RET claude-code-eat RET`
- vterm support
  - New `claude-code-terminal-backend` customization variable to choose between eat (default) and vterm
  - New `claude-code-vterm` customization group for vterm-specific settings
  - `claude-code-vterm-buffer-multiline-output` prevents flickering when Claude redraws multi-line input boxes
- New `claude-code-newline-keybinding-style` customization variable to configure how return and modifier keys behave in Claude buffers
  - `'default` (default): M-return inserts newline, RET sends message
  - `'newline-on-return`: RET inserts newline, M-return sends message
  - `'newline-on-shift-return`: RET sends message, S-return inserts newline
  - `'super-return-to-send`: RET inserts newline, s-return sends message
- Single ESC key now works as expected in Claude buffers for canceling operations
- C-g can be used as an alternative to ESC for canceling in Claude buffers
- New `claude-code-confirm-kill` customization variable to control kill confirmation prompts
  - When `t` (default), prompts for confirmation before killing Claude instances
  - When `nil`, kills Claude instances without confirmation
- New `claude-code-continue` command to explicitly continue previous conversations
  - Bound to `C-c c C` in the command map
  - Supports same prefix arguments as `claude-code` command
- New `claude-code-resume` command to resume specific past sessions
  - Bound to `C-c c R` in the command map
  - Allows resuming any past session from an interactive list
  - Can programmatically resume a specific session by ID
  - Supports same prefix arguments as `claude-code` command
- New `claude-code-start-in-directory` command for convenience
  - Bound to `C-c c d` in the command map
  - Always prompts for directory (equivalent to `C-u C-u claude-code`)
  - With prefix arg, switches to buffer after creating
- New `claude-code-new-instance` command to create a new Claude instance with a custom name
  - Bound to `C-c c i` in the command map
  - Always prompts for instance name (unlike `claude-code` which uses "default" for the first instance)
  - Supports same prefix arguments as `claude-code` command
- New `claude-code-select-buffer` command to select from all Claude instances
  - Bound to `C-c c B` in the command map
  - Shows all Claude instances across all projects and directories
  - Provides a dedicated command for global instance selection (similar to `C-u claude-code-switch-to-buffer`)
- New `claude-code-kill-all` command to kill all Claude instances
  - Bound to `C-c c K` in the command map
  - Kills all Claude instances across all projects and directories
  - Provides dedicated functionality previously available via `C-u claude-code-kill`
- New notification system for when Claude finishes processing and awaits input
  - `claude-code-enable-notifications` customization variable to toggle notifications (default: t)
  - `claude-code-notification-function` customization variable to set custom notification behavior
  - Default notification displays a message and pulses the modeline for visual feedback
- New `claude-code-optimize-window-resize` customization variable to prevent unnecessary terminal reflows
  - When enabled (default), terminal only reflows when window width changes, not height
  - Improves performance and reduces visual artifacts when splitting windows vertically

### Changed

- Renamed internal variable from `claude-code-key-binding-style` to `claude-code-newline-keybinding-style` for clarity
- Simplified `claude-code` command prefix arguments:
  - Single prefix (`C-u`) now switches to buffer after creating
  - Double prefix (`C-u C-u`) now prompts for project directory
  - Removed support for continuing conversations (use `claude-code-continue` instead)
- `claude-code-kill` no longer accepts prefix arguments
  - Use the new `claude-code-kill-all` command to kill all instances

### Fixed

- Fixed startup error "Symbol's function definition is void: (setf eat-term-parameter)" that occurred when starting claude-code for the first time
  - Added proper compile-time handling of eat package dependencies

## [0.3.8]

### Added

- New `claude-code-never-truncate-claude-buffer` customization variable to disable truncation of Claude output buffer
  - When set to `t`, disables Eat's scrollback size limit, allowing Claude to output unlimited content without truncation
  - Useful when working with large Claude responses
  - Defaults to `nil` to maintain backward compatibility

## [0.3.7]

### Added

- New quick response commands for numbered menu selection:
  - `claude-code-send-1` (`C-c c 1`) - Send "1" to select first option in Claude menus
  - `claude-code-send-2` (`C-c c 2`) - Send "2" to select second option
  - `claude-code-send-3` (`C-c c 3`) - Send "3" to select third option
- New `claude-code-cycle-mode` command (`C-c c TAB`) to send Shift-Tab to Claude for cycling between default mode, auto-accept edits mode, and plan mode
- Added "Quick Responses" section to transient menu grouping numbered and yes/no responses

### Changed

- Excluded experimental `sockets-mcp/` directory from version control via .gitignore

## [0.3.6]

### Changed

- Added confirmation prompts before killing Claude instances to prevent accidental termination (thanks to [microamp](https://github.com/microamp))
  - `claude-code-kill` now asks "Kill Claude instance?" before terminating
  - With prefix arg (`C-u`), asks for confirmation before killing all instances

## [0.3.5]

### Fixed

- Potential fix for issue #29: check if eat process is still running before adjusting claude buffer window size

## [0.3.4]

### Fixed

- Do not move to end of buffer when in eat-emacs-mode (read-only mode)

## [0.3.3]

### Fixed 

- Fixed `claude-code-send-command-with-context` and `claude-code-fix-error-at-point` to use full absolute paths for files outside of projects, ensuring commands work correctly with non-project files.

## [0.3.2]

### Fixed

- Further reduce flickering by only telling the Claude process about window resize events when the _width_ of the Claude window has changed. When the width has changed, Claude needs to redraw the prompt input box. But when only the height has changed, Claude does not have to re-create everything. This greatly reduces flickering that can occur when editing while a Claude window is open in Emacs.

## [0.3.1]

### Fixed

- Fixed bug using `claude-code-send-command-with-context` and `claude-code-fix-error-at-point` when invoked outside of a project, where it incorrectly prompted for a project.

## [0.3.0]

### Added

- **New feature**: Launch repository-specific Claude sessions - work on multiple projects simultaneously with separate Claude instances
- **New feature**: Support for multiple named Claude instances per directory (e.g., one for coding, another for tests)
  - Prompts for instance name when creating additional instances in the same directory
  - Buffer names now include instance names: `*claude:/path/to/project:instance-name*`
- Intelligent instance selection: When switching between directories, claude-code.el prompts to select from existing Claude instances or start a new one
- Instance memory: Your Claude instance selections are remembered per directory during the current Emacs session
- Simplified startup behavior: `claude-code` now automatically detects the appropriate directory (project root, current file directory, or default directory)
- Added prefix arg support to `claude-code-switch-to-buffer` - use `C-u` to see all Claude instances across all directories
- Added prefix arg support to `claude-code-kill` - use `C-u` to kill ALL Claude instances across all directories

### Changed

- Improved performance by reducing terminal reflows - Claude windows now only trigger terminal resizing when width changes, not height

- Claude buffer names now use abbreviated file paths for better readability (e.g., `*claude:~/projects/myapp*`)
- Reorganized prefix arguments for `claude-code` command:
  - Single prefix (`C-u`) now switches to buffer after creating (more commonly used)
  - Double prefix (`C-u C-u`) continues previous conversation (unchanged)
  - Triple prefix (`C-u C-u C-u`) prompts for project directory (previously single prefix)

### Removed

- Removed `claude-code-current-directory` command - its functionality is now integrated into the main `claude-code` command
- Removed limitation of only supporting one Claude process at a time

## [0.2.5] - 2025-06-06

### Added
- New `claude-code-fork` command to jump to previous conversations by sending escape-escape to Claude
  - Bound to `C-c c f` in the command map
  - Available in the transient menu

### Fixed
- Disabled unnecessary shell integration features (command history and prompt annotation) to improve performance

### Changed

## [0.2.4] - 2025-06-05

### Added

- New `claude-code-fork` command to jump to previous conversations by sending escape-escape to Claude
  - Bound to `C-c c f` in the command map
  - Available in the transient menu

### Changed
- `claude-code-kill` now shows a message instead of throwing an error when Claude is not running

### Changed
-  `claude-code-kill` now shows a message instead of throwing an error when Claude is not running

## [0.2.3] - 2025-05-23

### Fixed
- Fixed Claude buffer jumping to top when editing in other windows (#8)

## [0.2.2] - 2025-05-22

### Added
- Support for continuing previous conversations with double prefix arg (`C-u C-u`) in `claude-code` and `claude-code-current-directory` commands
    - Uses Claude's `--continue` flag to resume previous sessions
- Read-only mode for text selection in Claude terminal. Toggle with `claude-code-toggle-read-only-mode`.
- Customizable cursor appearance in read-only mode via `claude-code-read-only-mode-cursor-type`

## [0.2.1] - 2025-05-01

### Added
- Extended `claude-code-fix-error-at-point` to support flymake and any system implementing help-at-pt

### Changed
- Fixed compiler warnings (thanks to [ncaq](https://github.com/ncaq))

## [0.2.0] - 2025-04-22

### Added
- New `claude-code-fix-error-at-point` function to help fix flycheck errors. A flymake version will come later.
- Adding option to prompt for extra input in `claude-code-send-region`.
- Confirm before sending large regions or buffers in `claude-code-send-region`. The customization variable `claude-code-large-buffer-threshold` determines what is "large". 
- Added gitleaks pre-commit hook for security

### Changed
- Renamed functions to follow elisp naming conventions:
  - `claude-fix-error-at-point` → `claude-code-fix-error-at-point`
  - `claude--format-flycheck-errors-at-point` → `claude-code--format-flycheck-errors-at-point`
- Removed prefix key customization in favor of manual key binding, fixes #2. 
- Updated Makefile to disable sentence-end-double-space
- Enhanced documentation for flycheck integration and build process


