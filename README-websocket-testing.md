# WebSocket Server Testing Guide

This guide explains how to test the WebSocket server implementation for Claude Code IDE integration.

## Files Created

1. **claude-code-ws.el** - The WebSocket server implementation
2. **test-websocket-server.el** - Test utilities for manual testing
3. **test-ws-client.py** - Python WebSocket client for testing

## Testing Steps

### 1. Load and Start the Test Server

In Emacs:

```elisp
;; Load the test file
(load-file "test-websocket-server.el")

;; Start the test menu
M-x test-websocket-menu

;; Or start server directly
M-x test-websocket-start-server
```

### 2. Verify Server is Running

```bash
# Check if server is listening
netstat -an | grep 12345

# Should show:
# tcp4  0  0  127.0.0.1.12345  *.*  LISTEN
```

### 3. Test with Command Line Tools

#### Using websocat (recommended):

```bash
# Install websocat
brew install websocat  # macOS
# or
cargo install websocat  # with Rust

# Connect to server
websocat ws://127.0.0.1:12345

# Type messages and press Enter
{"type": "test", "message": "Hello"}
```

#### Using wscat:

```bash
# Install wscat
npm install -g wscat

# Connect
wscat -c ws://127.0.0.1:12345

# Send messages
> {"type": "test", "message": "Hello"}
```

### 4. Test with Python Client

```bash
# Install websocket-client
pip install websocket-client

# Run the test client
python3 test-ws-client.py 12345
```

### 5. Test MCP Protocol Simulation

In Emacs:

```elisp
;; Start MCP simulation server
M-x test-websocket-simulate-mcp
```

Then connect with a client and send MCP messages:

```json
{"jsonrpc": "2.0", "method": "initialize", "id": 1, "params": {}}
{"jsonrpc": "2.0", "method": "tools/list", "id": 2, "params": {}}
```

## What to Look For

### In Emacs *Messages* Buffer

You should see:

```
[WS Test] WebSocket server started on port 12345
[WS Test] Client connected: (127.0.0.1 . 54321)
[WS Test] Received: {"type": "test", "message": "Hello"}
[WS Test] Client disconnected: code=1000 reason=
```

### Expected Behavior

1. **Connection**: Client connects successfully with WebSocket handshake
2. **Echo**: Server echoes messages back with metadata
3. **JSON Parsing**: Server correctly parses JSON messages
4. **Frame Handling**: Proper handling of WebSocket frames
5. **Disconnection**: Clean disconnection with proper close frames

## Debugging

Enable debug output by evaluating:

```elisp
(setq debug-on-error t)
```

Check server status:

```elisp
M-x test-websocket-server-status
```

## Common Issues

1. **Port Already in Use**: Stop the server and try a different port
2. **Connection Refused**: Make sure server is running
3. **Invalid Handshake**: Check client is using WebSocket protocol
4. **Frame Parsing Errors**: Check *Messages* buffer for error details

## Next Steps

Once basic WebSocket communication is verified:

1. Test with actual Claude Code CLI
2. Implement JSON-RPC message routing
3. Add MCP protocol handlers
4. Integrate with claude-code.el