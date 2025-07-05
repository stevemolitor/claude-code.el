# Claude Code Ide Integration

## Links 
- protocol - /Users/stephenmolitor/repos/claudecode.nvim/PROTOCOL.md
- claude-code-ide - /Users/stephenmolitor/.emacs.d/straight/repos/claude-code-ide.el

## Context

I would like to add integration into Claude Code and claude-code.el, so that claude-code.el can send messages to the Claude Code process telling it what the current file and selection is, errors, etc, and conversely so that Claude Code can tell claude-code.el in Emacs to do things like show a diff, open a file, etc. 

The communication protocol between Claude Code and editors ("ide") is described here: /Users/stephenmolitor/repos/claudecode.nvim/PROTOCOL.md. This PROTOCOL.md is from a neovim plugin that reverse engineered and implemented the protocol for neovim. You can look at the **.lua files in that directory to learn more about the neovim plugin implementation.

The Emacs claude-code-ide package has also implemented the protocol for Emacs. I would like to implement something similar for claude-code.el. 

Please:

- Read the PROTOCOL.md file and think about how the protocol works.
- Read the *.el files /Users/stephenmolitor/.emacs.d/straight/repos/claude-code-ide.el and think harder about its implementation to understand how claude-code-ide has implemented the protocol, with an eye towards implemented it in claude-code.el.
- Read ./claude-code.el to understand my claude-code.el package and think about how it communicates with vterm and eat.
- Ultrathink about how to create an implementation plan for implementing the ide protocol in claude-code.el. 
- Don't make any code changes yet, just ultrathink about an implementation plan. Append your plan to this ide-integration.md file.


## Implementation Plan (Revised - Using websocket.el)

### Overview

The implementation will add IDE protocol support to claude-code.el by leveraging the existing websocket.el package. This enables bidirectional communication between Claude Code CLI and Emacs, allowing Claude to understand the current editor context (selections, open files, errors) and perform editor actions (open files, show diffs, etc.).

### Key Design Decisions

1. **Use websocket.el package** instead of implementing WebSocket protocol from scratch
2. **All MCP functions use private naming convention** (`claude-code--mcp-*`) since they're internal implementation details
3. **Selection tracking moved to Phase 1** for immediate user value
4. **Follow claude-code-ide architecture** while maintaining our unique features

### Architecture

The implementation will follow a modular architecture:

```
claude-code.el (main package)
├── claude-code-mcp.el (MCP protocol server using websocket.el)
├── claude-code-selection.el (Selection tracking - implemented early!)
└── claude-code-tools.el (Tool implementations)
```

### Phase 1: Core Infrastructure with Selection Tracking

#### 1.1 Prerequisites

- Add websocket.el dependency
- Update Package-Requires: `((emacs "30.0") (transient "0.7.5") (websocket "1.13"))`
- Users must install websocket from MELPA

#### 1.2 MCP Server Module (`claude-code-mcp.el`)

**Purpose**: Implement MCP protocol server using websocket.el

**Session Structure**:
```elisp
(cl-defstruct claude-code--mcp-session
  server          ; websocket server process
  client          ; connected websocket client
  port            ; server port
  project-dir     ; project directory
  instance-name   ; e.g., "default", "tests"
  buffer          ; associated Claude terminal buffer
  deferred        ; hash table for async responses
  selection-timer ; debounced selection tracking
  last-selection) ; cache to avoid duplicates
```

**Core Private Functions**:
```elisp
;; Start MCP server for a Claude instance
(defun claude-code--mcp-start (project-dir instance-name buffer)
  "Start MCP server for PROJECT-DIR with INSTANCE-NAME in BUFFER."
  ...)

;; Stop server and cleanup
(defun claude-code--mcp-stop (session)
  "Stop MCP server and clean up SESSION."
  ...)

;; Send notification to Claude
(defun claude-code--mcp-send-notification (session method params)
  "Send METHOD notification with PARAMS to Claude."
  ...)
```

**Lock File Management**:
- Location: `~/.claude/ide/[port].lock`
- Content: `{"pid": 12345, "workspaceFolders": [...], "ideName": "Emacs", "transport": "ws"}`
- Created on server start, removed on stop

#### 1.3 Selection Tracking Module (`claude-code-selection.el`)

**Purpose**: Track cursor/selection changes and notify Claude immediately

**Private Functions**:
```elisp
;; Initialize tracking for a session
(defun claude-code--selection-start (session)
  "Start selection tracking for SESSION."
  ...)

;; Post-command hook handler
(defun claude-code--selection-track-change ()
  "Track selection changes in current buffer."
  ...)

;; Send update to Claude
(defun claude-code--selection-send-notification (session)
  "Send selection update for SESSION."
  ...)
```

**Implementation Details**:
- Hook into `post-command-hook` locally
- Debounce updates (50ms default)
- Only track files within project directory
- Send `selection_changed` notifications
- Cache last selection to avoid duplicates

### Phase 2: Integration with claude-code.el

#### 2.1 Startup Integration
Modify `claude-code--start` to:
```elisp
;; Add to claude-code--start:
(when claude-code-enable-ide-integration
  (require 'claude-code-mcp)
  (require 'claude-code-selection)
  (let ((session (claude-code--mcp-start dir instance-name buffer)))
    (setq-local claude-code--mcp-session session)
    ;; Set environment variables
    (setq process-environment
          (append `(,(format "CLAUDE_CODE_SSE_PORT=%d" 
                            (claude-code--mcp-session-port session))
                    "ENABLE_IDE_INTEGRATION=true")
                  process-environment))))
```
#### 2.2 Shutdown Integration

Modify `claude-code--kill-buffer` to:
```elisp
;; Add to cleanup:
(when (and (boundp 'claude-code--mcp-session) 
           claude-code--mcp-session)
  (claude-code--mcp-stop claude-code--mcp-session))
```
#### 2.3 Configuration

Add new customization options:
```elisp
(defcustom claude-code-enable-ide-integration t
  "Enable IDE integration for Claude Code."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-selection-delay 0.05
  "Delay in seconds before sending selection updates."
  :type 'number
  :group 'claude-code)
```

### Phase 3: Tool Implementations (`claude-code-tools.el`)

All tool functions use private naming convention:

#### 3.1 Basic Tools (Priority 1)
```elisp
;; Return list of available tools
(defun claude-code--tool-list ()
  "Return MCP tools list.")

;; Query tools
(defun claude-code--tool-get-current-selection ()
  "Return current text selection.")

(defun claude-code--tool-get-open-editors ()
  "Return list of open file buffers.")

(defun claude-code--tool-get-workspace-folders ()
  "Return project directories.")

(defun claude-code--tool-check-document-dirty (params)
  "Check if document at PATH is modified.")
```

#### 3.2 File Operations (Priority 2)
```elisp
(defun claude-code--tool-open-file (params)
  "Open file with optional line/text selection.")

(defun claude-code--tool-save-document (params)
  "Save the specified buffer.")

(defun claude-code--tool-close-tab (params)
  "Close buffer by name.")
```

#### 3.3 Advanced Tools (Priority 3)
```elisp
(defun claude-code--tool-open-diff (params)
  "Open ediff session - returns deferred response.")

(defun claude-code--tool-get-diagnostics (params)
  "Return Flycheck/Flymake errors for file.")
```

### Implementation Timeline

#### Week 1: Core Infrastructure with Selection Tracking
- Create claude-code-mcp.el with websocket.el integration
- Implement session management
- Add lock file creation/cleanup
- **Implement selection tracking (claude-code-selection.el)**
- Basic initialize handler
- Test with websocat that selection notifications work

#### Week 2: Integration with claude-code.el
- Modify startup/shutdown flows
- Add configuration options
- Test with actual Claude CLI
- Verify selection tracking in real usage

#### Week 3: Basic Tools
- Create claude-code-tools.el
- Implement tools/list handler
- Add query tools (getCurrentSelection, etc.)
- Add file operations (openFile, saveDocument)

#### Week 4: Advanced Features
- Implement openDiff with ediff
- Add getDiagnostics support
- Polish and documentation

### Testing Strategy

#### Phase 1 Testing (Week 1)
- Use websocat to verify WebSocket server starts:
  ```bash
  websocat ws://127.0.0.1:[port]
  ```
- Verify lock file created in `~/.claude/ide/`
- Check selection_changed notifications sent as cursor moves
- Test debouncing works (rapid movements = single update)

#### Integration Testing (Week 2)
- Test with actual Claude CLI
- Verify environment variables set correctly
- Check Claude receives selection updates
- Test session cleanup on buffer kill

#### Tool Testing (Week 3-4)
- Test each tool with Claude commands
- Verify error handling for invalid params
- Test deferred responses (openDiff)
- Check all private functions are inaccessible

### Key Benefits of This Approach

1. **Immediate Value**: Selection tracking works from day one
2. **Clean API**: All implementation details are private
3. **Less Code**: websocket.el handles protocol complexity
4. **Better Maintainability**: Clear separation of concerns
5. **Proven Technology**: websocket.el is well-tested

### Summary

This revised plan leverages the websocket.el package to provide a cleaner, more maintainable implementation. By moving selection tracking to Phase 1 and using private function naming throughout, we get immediate user value while maintaining a clean API surface.
