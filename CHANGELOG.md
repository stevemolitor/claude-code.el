# Changelog

All notable changes to claude-code.el will be documented in this file.

## [Unreleased]

### Added

- **New feature**: Launch repository-specific Claude sessions - work on multiple projects simultaneously with separate Claude instances
- Intelligent instance selection: When switching between directories, claude-code.el prompts to select from existing Claude instances or start a new one
- Instance memory: Your Claude instance selections are remembered per directory during the current Emacs session
- Simplified startup behavior: `claude-code` now automatically detects the appropriate directory (project root, current file directory, or default directory)

### Changed

- Claude buffer names now use abbreviated file paths for better readability (e.g., `*claude:~/projects/myapp*`)

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


