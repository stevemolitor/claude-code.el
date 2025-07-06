#!/bin/bash
# Test WebSocket connection to MCP server

# Check if port is provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <port>"
    echo "Get the port from the Emacs test output"
    exit 1
fi

PORT=$1
echo "Testing MCP WebSocket server on port $PORT..."

# Check if websocat is installed
if ! command -v websocat &> /dev/null; then
    echo "websocat is not installed. Install it with:"
    echo "  brew install websocat  # macOS"
    echo "  cargo install websocat # via Rust"
    exit 1
fi

# Create temporary file for test messages
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}},"clientInfo":{"name":"test-client","version":"1.0"}}}
{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
{"jsonrpc":"2.0","id":3,"method":"prompts/list","params":{}}
EOF

echo "Connecting to ws://127.0.0.1:$PORT with MCP subprotocol..."
echo "Sending test messages:"
echo "1. initialize"
echo "2. tools/list" 
echo "3. prompts/list"
echo ""

# Connect and send messages  
cat "$TMPFILE" | websocat --protocol=mcp "ws://127.0.0.1:$PORT"

# Clean up
rm "$TMPFILE"

echo ""
echo "Test complete. Check Emacs for any error messages."