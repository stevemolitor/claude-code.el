# Claude-code.el Vterm Support Implementation Plan

## Executive Summary
This document outlines a comprehensive plan for adding vterm support to claude-code.el as an alternative to the current eat terminal emulator. Users will be able to choose between eat and vterm through a customization variable.

## Analysis Summary

### Current eat Implementation
- **Terminal Creation**: Uses `eat-make` with program name and switches
- **Command Sending**: Uses `eat-term-send-string` with eat-terminal object
- **Mode Switching**: Uses `eat-semi-char-mode` and `eat-emacs-mode`
- **Process Management**: Uses `eat-kill-process`
- **Terminal State**: eat.el provides and sets `eat-terminal` buffer-local variable
- **Scrolling**: Custom synchronization function via `eat--synchronize-scroll-function` (workaround for eat scrolling issues)
- **Window Resizing**: Custom advice on `eat--adjust-process-window-size` (prevents unnecessary reflows)
- **Scrollback**: Option to disable truncation via `eat-term-scrollback-size`

### vterm Differences
- **Terminal Creation**: Uses `vterm-mode` which internally creates the process
- **Command Sending**: Uses `vterm-send-string` and `vterm-send-key`
- **Mode Switching**: Uses `vterm-copy-mode` for read-only mode
- **Process Management**: Process stored in `vterm--process`
- **Terminal State**: Internal terminal object in `vterm--term`
- **Different API**: No direct equivalent to some eat functions

## Implementation Strategy

### 1. Add Terminal Backend Selection
```elisp
(defcustom claude-code-terminal-backend 'eat
  "Terminal backend to use for Claude Code.
Can be either 'eat or 'vterm."
  :type '(choice (const :tag "Eat terminal" eat)
                 (const :tag "Vterm terminal" vterm))
  :group 'claude-code)
```

### 2. Create Abstraction Layer
Create terminal-agnostic functions that dispatch to the appropriate backend:

```elisp
;; Generic terminal interface functions
(defun claude-code--term-make (buffer-name program switches)
  "Create a terminal using the configured backend.")

(defun claude-code--term-send-string (string)
  "Send STRING to the terminal.")

(defun claude-code--term-send-key (key)
  "Send KEY to the terminal.")

(defun claude-code--term-kill-process ()
  "Kill the terminal process.")

(defun claude-code--term-enter-read-only-mode ()
  "Enter read-only/copy mode.")

(defun claude-code--term-exit-read-only-mode ()
  "Exit read-only/copy mode.")
```

### 3. Implementation Details

#### 3.1 Terminal Creation
- **eat**: Current implementation with `eat-make`
- **vterm**: 
  ```elisp
  (with-current-buffer buffer
    (vterm-mode)
    ;; vterm automatically starts the process
    ;; Configure vterm-specific settings
    (setq vterm-shell program)
    ;; Handle program switches differently as vterm doesn't 
    ;; directly support switches like eat does
    )
  ```

#### 3.2 Sending Commands
- **eat**: `(eat-term-send-string eat-terminal string)`
- **vterm**: `(vterm-send-string string)`

#### 3.3 Sending Special Keys
- **eat**: `(eat-term-send-string eat-terminal (kbd "ESC"))`
- **vterm**: `(vterm-send-key "<escape>")`

#### 3.4 Read-only Mode
- **eat**: `eat-emacs-mode` / `eat-semi-char-mode`
- **vterm**: `vterm-copy-mode`

### 4. Challenges and Solutions

**Note**: Some complexity in the current implementation exists to work around eat.el issues (scrolling, window resizing, scrollback truncation). Vterm may not need equivalent workarounds, which could simplify the implementation.

#### 4.1 Program Switches
**Challenge**: vterm doesn't support program switches directly in the same way eat does.
**Solution**: 
- For simple cases, append switches to the shell command
- For `--continue`, might need to send as a command after terminal starts
- May need custom handling per switch

#### 4.2 Terminal Object Access
**Challenge**: eat requires passing `eat-terminal` to many functions, vterm's API needs investigation
**Solution**: 
- Investigate if vterm functions require terminal object access
- Many vterm functions appear to work on the current buffer without explicit terminal object
- Store backend type in buffer-local variable if needed
- Abstract terminal object access only if vterm requires it

#### 4.3 Window Synchronization
**Challenge**: Custom scroll synchronization in eat vs vterm's built-in handling
**Solution**: 
- The custom scroll sync in claude-code.el is a workaround for eat.el issues
- For vterm, we may be able to rely on its built-in window handling
- Test if vterm needs any custom synchronization at all

#### 4.4 Cursor Management
**Challenge**: Different cursor handling between eat and vterm
**Solution**: 
- Abstract cursor operations
- Use vterm's `vterm-reset-cursor-point` equivalent functionality

### 5. Implementation Phases

#### Phase 1: Core Abstraction (Priority: High)
1. Add `claude-code-terminal-backend` customization variable
2. Create abstraction functions for basic operations
3. Refactor existing code to use abstractions
4. Test with eat to ensure no regression

#### Phase 2: Basic vterm Support (Priority: High)
1. Implement vterm backend for core functions:
   - Terminal creation
   - Sending strings/commands
   - Process termination
2. Handle buffer naming and directory setup
3. Basic testing with vterm

#### Phase 3: Advanced Features (Priority: Medium)
1. Read-only mode support for vterm
2. Special key handling (escape, return, etc.)
3. Window management and synchronization
4. Handle program switches appropriately

#### Phase 4: Polish and Edge Cases (Priority: Low)
1. Terminal resizing handling
2. Face/theme support
3. Performance optimizations
4. Comprehensive testing

### 6. Code Structure

```
claude-code.el
├── Customization
│   └── claude-code-terminal-backend
├── Terminal Abstraction Layer
│   ├── claude-code--term-make
│   ├── claude-code--term-send-string
│   ├── claude-code--term-send-key
│   ├── claude-code--term-kill-process
│   └── claude-code--term-*-mode
├── Backend Implementations
│   ├── eat backend functions
│   └── vterm backend functions
└── Core Functions (refactored to use abstraction)
```

### 7. Testing Strategy

1. **Compatibility Testing**: Ensure eat backend works exactly as before
2. **Feature Parity**: Test all features work with both backends
3. **Edge Cases**: 
   - Multiple instances
   - Directory switching
   - Special characters
   - Large output handling
4. **Performance**: Compare performance between eat and vterm

### 8. Migration Guide for Users

```elisp
;; To use vterm instead of eat:
(setq claude-code-terminal-backend 'vterm)

;; Ensure vterm is installed:
;; M-x package-install RET vterm RET
```

### 9. Future Considerations

1. **Dynamic Backend Switching**: Allow switching backends without restarting Emacs
2. **Backend Feature Detection**: Automatically detect available backends
3. **Backend-Specific Optimizations**: Take advantage of unique features in each backend
4. **Additional Backends**: Structure allows for future terminal emulators

## Conclusion

This implementation plan provides a clean abstraction layer that allows claude-code.el to support multiple terminal backends while maintaining backward compatibility. The phased approach ensures that basic functionality can be delivered quickly while more complex features are implemented incrementally.