# MCP Initialization Handshake
⏺ MCP Initialization Handshake Explained

  1. Connection Setup

  When Claude CLI starts with the environment variables:
  - CLAUDE_CODE_SSE_PORT=<port> - tells Claude which port to connect to
  - ENABLE_IDE_INTEGRATION=1 - enables the /ide command

  2. WebSocket Connection

  - Claude connects to ws://127.0.0.1:<port> with subprotocol "mcp"
  - The claude-code--mcp-on-open handler sets the client connection in the session

  3. Initialize Request (Claude → MCP Server)

  Claude sends:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "protocolVersion": "2024-11-05",
      "capabilities": {},
      "clientInfo": {
        "name": "claude-desktop",
        "version": "0.7.10"
      }
    }
  }

  4. Initialize Response (MCP Server → Claude)

  The server responds with its capabilities:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
      "protocolVersion": "2024-11-05",
      "capabilities": {
        "tools": {
          "listChanged": true      // Server can notify when tools change
        },
        "prompts": {
          "listChanged": true      // Server can notify when prompts change
        },
        "resources": {
          "subscribe": true,       // Client can subscribe to resources
          "listChanged": true      // Server can notify when resources change
        }
      },
      "serverInfo": {
        "name": "claude-code-mcp",
        "version": "0.1.0"
      }
    }
  }

  5. Follow-up Requests

  After successful initialization, Claude typically requests:
  - tools/list - to get available tools
  - prompts/list - to get available prompts
  - May subscribe to resources

  Key Protocol Rules:

  1. JSON-RPC 2.0: All messages follow JSON-RPC 2.0 format
  2. Request/Response: Requests have id, responses must echo the same id
  3. Notifications: No id field, fire-and-forget messages
  4. Protocol Version: Must match exactly ("2024-11-05")
  5. Capabilities: Server declares what it supports, client respects those limits

  Error Handling:

  - -32700: Parse error (invalid JSON)
  - -32601: Method not found
  - -32603: Internal error
  - Custom codes for domain-specific errors

  The handshake establishes:
  - Protocol compatibility
  - Server capabilities
  - Client identity
  - Available features (tools, prompts, resources)

  Once complete, the connection is ready for bidirectional communication where
  Claude can call tools and the server can send notifications (like selection
  changes).

> Explain 5. Folllow-up requests

⏺ Let me show you the follow-up requests in detail by examining what happens after
   the initialize handshake:

⏺ Read(claude-code-mcp.el)
  ⎿  Read 25 lines (ctrl+r to expand)

⏺ Follow-up Requests Explained

  After the initialization handshake completes successfully, Claude automatically
  sends follow-up requests to discover what capabilities the MCP server actually
  provides. Here's what happens:

  1. tools/list Request

  Claude asks: "What tools do you have available?"

  Request from Claude:
  {
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/list",
    "params": {}
  }

  Response from MCP Server (currently empty):
  {
    "jsonrpc": "2.0",
    "id": 2,
    "result": {
      "tools": []
    }
  }

  When implemented (Phase 3), this would return tools like:
  {
    "tools": [
      {
        "name": "openFile",
        "description": "Open a file in Emacs",
        "inputSchema": {
          "type": "object",
          "properties": {
            "path": {"type": "string"}
          }
        }
      },
      {
        "name": "getCurrentSelection",
        "description": "Get the currently selected text",
        "inputSchema": {"type": "object"}
      }
    ]
  }

  2. prompts/list Request

  Claude asks: "What prompt templates do you have?"

  Request from Claude:
  {
    "jsonrpc": "2.0",
    "id": 3,
    "method": "prompts/list",
    "params": {}
  }

  Response from MCP Server (currently empty):
  {
    "jsonrpc": "2.0",
    "id": 3,
    "result": {
      "prompts": []
    }
  }

  Prompts are pre-defined templates that could be filled with context. For
  example:
  {
    "prompts": [
      {
        "name": "explain-code",
        "description": "Explain the selected code",
        "arguments": [
          {
            "name": "code",
            "description": "The code to explain",
            "required": true
          }
        ]
      }
    ]
  }

  3. Resource Subscriptions (if implemented)

  Since we declared resources.subscribe: true, Claude might also:

  Subscribe to a resource:
  {
    "jsonrpc": "2.0",
    "id": 4,
    "method": "resources/subscribe",
    "params": {
      "uri": "file:///path/to/project"
    }
  }

  Why These Follow-ups Matter:

  1. Dynamic Discovery: Claude doesn't assume what tools exist - it asks
  2. Version Compatibility: Different MCP servers may offer different tools
  3. User Interface: Claude uses this info to show available commands to users
  4. Validation: Claude can validate tool calls against the schemas

  The Flow:

  1. Initialize → "I speak protocol version X"
  2. tools/list → "What can I ask you to do?"
  3. prompts/list → "What templates do you offer?"
  4. (Optional) resources/subscribe → "Notify me about changes to X"

  This discovery mechanism makes MCP flexible - servers can implement whatever
  tools make sense for their environment without Claude needing to know about them
   in advance.
