# Phase 1 Testing Guide

## Overview
Phase 1 implements the core MCP infrastructure:
- WebSocket server using websocket.el
- Lock file creation for Claude CLI discovery
- Selection tracking with debounced updates

## Testing Instructions

### 1. Load and Test in Emacs

```elisp
;; Load the implementation
(load-file "claude-code-mcp.el")
(load-file "claude-code-selection.el")
(load-file "test-mcp-phase1.el")

;; Run the test
M-x claude-code-test-phase1
```

This will:
- Start an MCP server on a random port
- Create a lock file in ~/.claude/ide/[port].lock
- Test selection tracking
- Display connection instructions

### 2. Test WebSocket Connection

After running the Emacs test, note the port number and run:

```bash
./test-websocket.sh <port>
```

Expected output:
- Initialize response with protocol version 2024-11-05
- Empty tools array (not implemented yet)
- Empty prompts array (not implemented yet)

### 3. Manual WebSocket Testing

For interactive testing:

```bash
# Connect with websocat
websocat -s mcp ws://127.0.0.1:<port>

# Send initialize request
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}

# Expected response
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05","capabilities":{"tools":{"listChanged":true},"prompts":{"listChanged":true},"resources":{"subscribe":true,"listChanged":true}},"serverInfo":{"name":"claude-code-mcp","version":"0.1.0"}}}
```

### 4. Selection Tracking Test

With the MCP server running:
1. Open a file in Emacs
2. Move cursor around
3. Select text regions
4. Watch the *Messages* buffer for selection notifications

### 5. Lock File Verification

Check the lock file:

```bash
cat ~/.claude/ide/<port>.lock | jq .
```

Should show:
- pid: Emacs process ID
- workspaceFolders: Current project directory
- ideName: "Emacs"
- transport: "ws"

### 6. Cleanup

```elisp
M-x claude-code-test-cleanup
```

This will:
- Stop the WebSocket server
- Remove the lock file
- Clean up test buffers

## Troubleshooting

1. **Port already in use**: The implementation finds a free port automatically
2. **websocat not found**: Install with `brew install websocat` or `cargo install websocat`
3. **Connection refused**: Ensure the MCP server is still running in Emacs
4. **No selection updates**: Check that you're in a file-visiting buffer

## Next Steps

Once Phase 1 is verified:
- Phase 2: Integrate with claude-code.el startup
- Phase 3: Implement tool functions
- Phase 4: Advanced features