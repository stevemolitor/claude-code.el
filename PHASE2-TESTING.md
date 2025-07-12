# Phase 2 Testing Guide

## Overview
Phase 2 integrates MCP with claude-code.el:
- Automatic MCP server startup when Claude is launched
- Environment variables passed to Claude process
- Proper cleanup on buffer kill
- Support for multiple Claude instances

## Testing Instructions

### 1. Load Phase 2 Implementation

```elisp
;; First, clean up any existing test sessions
M-x claude-code-test-cleanup

;; Load all components
(load-file "claude-code-mcp.el")
(load-file "claude-code-selection.el")
(load-file "claude-code-mcp-integration.el")
(load-file "claude-code.el")  ; Reload to pick up integration
(load-file "test-mcp-phase2.el")

;; Run the test info
M-x claude-code-test-phase2
```

### 2. Start Claude with MCP Integration

```elisp
;; Start Claude normally
M-x claude-code

;; Or with C-u prefix to switch to buffer
C-u M-x claude-code
```

### 3. Verify MCP Integration

In the Claude buffer, run:

```elisp
;; Check MCP session
M-x claude-code-test-phase2-check-session

;; Check environment variables
M-x claude-code-test-phase2-check-env
```

Expected results:
- MCP session should show port number
- Lock file should exist in ~/.claude/ide/
- Environment should show CLAUDE_CODE_SSE_PORT and ENABLE_IDE_INTEGRATION

### 4. Test Multiple Instances

```elisp
;; Start another instance in same project
M-x claude-code
;; Choose a different instance name (e.g., "tests")

;; Verify both have different MCP ports
;; Switch between buffers and run:
M-x claude-code-test-phase2-check-session
```

### 5. Test Selection Tracking

With Claude running:
1. Open a source file in the same project
2. Move cursor around
3. Select some text
4. Check *Messages* buffer for selection notifications

### 6. Test WebSocket Connection

Find the port from the session info, then:

```bash
# Test the connection
./test-websocket.sh <port>
```

### 7. Test Cleanup

```elisp
;; Kill a Claude buffer
M-x claude-code-kill

;; Verify:
;; - Lock file is removed
;; - MCP server is stopped
ls ~/.claude/ide/
```

### 8. Test Toggling Integration

```elisp
;; Disable integration
M-x claude-code-test-phase2-toggle

;; Start Claude - should start without MCP
M-x claude-code

;; Re-enable integration  
M-x claude-code-test-phase2-toggle
```

## Troubleshooting

1. **No MCP session**: Check if `claude-code-enable-ide-integration` is t
2. **Environment variables not set**: The advice might not be working - check with `(advice-member-p 'claude-code--mcp-advise-start 'claude-code--start)`
3. **Port conflicts**: The system automatically finds free ports
4. **Selection not tracking**: Ensure you're in a file within the Claude project directory

## Integration Points

The integration works by:
1. Advising `claude-code--start` to add environment variables
2. Using `claude-code-start-hook` to start MCP server
3. Using `kill-buffer-hook` to stop MCP server
4. Tracking sessions per buffer with `claude-code--mcp-session`

## Next Steps

Once Phase 2 is verified:
- Phase 3: Implement tool functions (openFile, showDiff, etc.)
- Phase 4: Advanced features (diagnostics, resources)