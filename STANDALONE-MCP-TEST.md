# Standalone MCP Server Testing Guide

This guide tests the MCP server independently from the Claude Emacs integration, allowing you to verify the protocol handshake with the actual Claude CLI.

## Setup

### 1. In Emacs, load the required files:

```elisp
;; Load MCP components
(load-file "claude-code-mcp.el")
(load-file "claude-code-selection.el")
(load-file "test-mcp-standalone.el")
```

### 2. Start the standalone MCP server:

```elisp
M-x test-mcp-standalone-start
```

This will:
- Start an MCP WebSocket server on a random port
- Create a lock file in ~/.claude/ide/
- Display instructions in the `*MCP Standalone Test*` buffer

### 3. Note the port number from the output

The buffer will show something like:
```
Port: 35456
```

## Connect from Claude CLI

### 1. Open a new terminal (outside Emacs)

### 2. Set the required environment variables:

```bash
export CLAUDE_CODE_SSE_PORT=35456  # Use the actual port from step 3
export ENABLE_IDE_INTEGRATION=1
```

### 3. Start Claude:

```bash
claude
```

### 4. In Claude, connect to the MCP server:

```
/ide
```

## What to Expect

### Success Scenario:
1. Claude sends an initialize request
2. MCP server responds with capabilities
3. Claude may request tools/list, prompts/list, etc.
4. Connection is established

### In the Emacs buffer, you should see:
- Connection accepted
- Initialize request received
- Response sent
- Any follow-up requests

## Debugging

### 1. Enable enhanced logging:

In Emacs:
```elisp
M-x test-mcp-standalone-enhanced-logging
```

This will show detailed message exchanges.

### 2. Check the lock file:

```bash
cat ~/.claude/ide/*.lock | jq .
```

Should show:
```json
{
  "pid": <emacs-pid>,
  "ideName": "Emacs", 
  "transport": "ws",
  "workspaceFolders": ["/your/project/path"]
}
```

### 3. Test with websocat first:

Before trying with Claude, test the basic connection:

```bash
./test-websocket.sh <port>
```

### 4. Monitor the connection:

In Emacs:
```elisp
M-x test-mcp-standalone-status
```

### 5. Send a test notification:

Once connected:
```elisp
M-x test-mcp-standalone-send-test-notification
```

## Common Issues

### "Connection refused"
- Ensure the MCP server is still running
- Check the port number is correct
- Verify no firewall is blocking localhost connections

### "No /ide command"
- Ensure you exported the environment variables BEFORE starting claude
- The variables must be set in the same shell where you run claude

### "Handshake failed"
- Check the `*MCP Standalone Test*` buffer for error details
- Enable enhanced logging to see the exact messages

### "Selection tracking not working"
- Open a file in the project directory
- Ensure the file buffer is visible
- Move cursor/make selections

## Cleanup

When done testing:
```elisp
M-x test-mcp-standalone-stop
```

This will:
- Stop the WebSocket server
- Remove the lock file
- Update the test buffer

## Advanced Testing

### Test multiple connections:
1. Keep the first terminal connected
2. Open another terminal, set env vars, start another claude
3. Try `/ide` - it should fail (already connected)

### Test reconnection:
1. Exit claude in the terminal
2. Start claude again with same env vars
3. Use `/ide` - should reconnect

### Test protocol compliance:
Watch the message exchange to ensure:
- All requests have proper id fields
- Responses match request ids
- JSON-RPC 2.0 format is correct
- Methods are properly namespaced