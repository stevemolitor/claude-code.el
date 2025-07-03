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


## Implementation Plan

### Overview

The implementation will add IDE protocol support to claude-code.el, enabling bidirectional communication between Claude Code CLI and Emacs. This will allow Claude to understand the current editor context (selections, open files, errors) and perform editor actions (open files, show diffs, etc.).

### Architecture

The implementation will follow a modular architecture similar to claude-code-ide.el:

```
claude-code.el (main package)
├── claude-code-ide-ws.el (WebSocket server implementation)
├── claude-code-ide-mcp.el (MCP protocol handler)
├── claude-code-ide-tools.el (Tool implementations)
└── claude-code-ide-selection.el (Selection tracking)
```

### Key Components

#### 1. WebSocket Server (`claude-code-ide-ws.el`)

**Purpose**: Create and manage WebSocket servers for Claude Code connections.

**Implementation**:
- Use `make-network-process` with `:server t` to create TCP server
- Implement RFC 6455 WebSocket protocol:
  - HTTP upgrade handshake
  - Frame parsing (opcode, payload length, masking)
  - Frame creation for sending messages
  - Ping/pong handling
  - Connection management
- Find free port in range 10000-65535
- Bind to localhost (127.0.0.1) only for security

**Key Functions**:
```elisp
;; Create WebSocket server
(cl-defun claude-code-ide-ws-create-server (&key port on-open on-message on-close on-error)
  ...)

;; Send message to client
(cl-defun claude-code-ide-ws-send (client message)
  ...)

;; Close client connection
(cl-defun claude-code-ide-ws-close (client &optional code reason)
  ...)
```

**Manual Testing**:
1. **Server Creation Test**:
   - Start the WebSocket server on a specific port using Emacs Lisp:
     ```elisp
     ;; In *scratch* buffer or M-x eval-expression
     ;; First, load the WebSocket module (once implemented)
     (require 'claude-code-ide-ws)
     
     ;; Create a test server on port 12345
     (setq test-server 
       (claude-code-ide-ws-create-server
         :port 12345
         :on-open (lambda (client)
                    (message "Client connected: %s" client))
         :on-message (lambda (client message)
                       (message "Received: %s" message)
                       ;; Echo the message back
                       (claude-code-ide-ws-send client 
                         (format "Echo: %s" message)))
         :on-close (lambda (client code reason)
                     (message "Client disconnected: %s %s" code reason))
         :on-error (lambda (client error)
                     (message "Error: %s" error))))
     
     ;; Verify server is running
     (message "Server started on port %d" (claude-code-ide-ws-server-port test-server))
     ```
   
   - Alternatively, create a test helper function:
     ```elisp
     (defun claude-code-test-websocket-server ()
       "Start a test WebSocket server for manual testing."
       (interactive)
       (let ((port (read-number "Port: " 12345)))
         (setq claude-code-test-server
           (claude-code-ide-ws-create-server
             :port port
             :on-open (lambda (client)
                        (message "[WS] Client connected from %s" 
                                (process-contact client :remote)))
             :on-message (lambda (client message)
                           (message "[WS] Received: %s" message)
                           (claude-code-ide-ws-send client 
                             (json-encode `((echo . ,message)
                                          (timestamp . ,(current-time-string))))))
             :on-close (lambda (client code reason)
                         (message "[WS] Client disconnected: code=%s reason=%s" 
                                  code reason))
             :on-error (lambda (client error)
                         (message "[WS] Error: %s" error))))
         (message "Test WebSocket server started on port %d" port)))
     
     ;; Run the test
     (claude-code-test-websocket-server)
     ```
   
   - Use `netstat -an | grep <port>` to verify it's listening on 127.0.0.1:
     ```bash
     netstat -an | grep 12345
     # Should show: tcp4  0  0  127.0.0.1.12345  *.*  LISTEN
     ```
   
   - Use a WebSocket client tool (e.g., `websocat`) to test connection:
     ```bash
     # Install websocat first: brew install websocat (macOS)
     websocat ws://127.0.0.1:12345
     
     # Type a message and press Enter
     Hello from client
     # Should receive: {"echo":"Hello from client","timestamp":"..."}
     ```
   
   - Monitor the *Messages* buffer in Emacs for server logs
   
   - To stop the test server:
     ```elisp
     (claude-code-ide-ws-stop-server test-server)
     ;; or
     (claude-code-ide-ws-stop-server claude-code-test-server)
     ```

2. **Handshake Test**:
   - Connect with proper WebSocket headers
   - Verify server responds with correct upgrade response
   - Check that invalid handshakes are rejected

3. **Frame Parsing Test**:
   - Send various frame types (text, ping, close)
   - Verify correct parsing and responses
   - Test with different payload sizes

4. **Debug Output**:
   - Enable debug logging to see all incoming/outgoing frames
   - Check *Messages* buffer for WebSocket activity

#### 2. MCP Protocol Handler (`claude-code-ide-mcp.el`)

**Purpose**: Implement the Model Context Protocol for Claude Code.

**Implementation**:
- JSON-RPC 2.0 message handling
- Method routing for:
  - `initialize`: Protocol handshake
  - `tools/list`: Return available tools
  - `tools/call`: Execute tool handlers
  - `prompts/list`: Return prompts (empty initially)
- Response and error formatting
- Session state management

**Key Functions**:
```elisp
;; Start MCP server for a Claude instance
(cl-defun claude-code-ide-mcp-start (&key project-dir instance-name buffer)
  ...)

;; Handle incoming JSON-RPC message
(cl-defun claude-code-ide-mcp-handle-message (session client message)
  ...)

;; Send notification to Claude
(cl-defun claude-code-ide-mcp-notify (session method params)
  ...)
```

**Session Structure**:
```elisp
(cl-defstruct claude-code-ide-session
  server           ; WebSocket server
  client           ; Connected client (if any)
  port             ; Server port
  project-dir      ; Project directory
  instance-name    ; Claude instance name
  buffer           ; Associated Claude buffer
  deferred         ; Hash table for deferred responses
  selection-timer  ; Timer for debounced selection updates
  )
```

**Manual Testing**:
1. **Initialize Handshake Test**:
   - Start Claude Code with IDE integration enabled
   - Check lock file exists in `~/.claude/ide/`
   - Verify Claude connects and sends `initialize` request
   - Check response contains correct capabilities

2. **Message Routing Test**:
   - Send test JSON-RPC messages via WebSocket
   - Verify correct method handlers are called
   - Test invalid methods return proper errors

3. **Session State Test**:
   - Create multiple Claude instances
   - Verify each has separate session
   - Test session cleanup on disconnect

4. **Debug Output**:
   - Enable MCP debug mode
   - Monitor *Claude IDE Debug* buffer for all messages
   - Verify JSON-RPC request/response pairs match

#### 3. Tool Implementations (`claude-code-ide-tools.el`)

**Purpose**: Implement MCP tools that Claude can call.

**Tools to Implement**:

1. **openFile**
   - Open file in Emacs
   - Support line/column positioning
   - Support text selection (startText/endText)

2. **openDiff** (blocking operation)
   - Create ediff session
   - Wait for user accept/reject
   - Return FILE_SAVED or DIFF_REJECTED

3. **getCurrentSelection**
   - Return current region text and position
   - Include file path and line/column info

4. **getOpenEditors**
   - List all file-visiting buffers
   - Include modification status

5. **getWorkspaceFolders**
   - Return project directories
   - Support multiple projects

6. **getDiagnostics**
   - Integrate with Flycheck errors
   - Support Flymake as fallback
   - Format errors with file/line/message

7. **saveDocument**
   - Save specified buffer
   - Return success/failure

8. **close_tab**
   - Close buffer by name
   - Handle unsaved changes

9. **checkDocumentDirty**
   - Check if buffer has unsaved changes

**Tool Registration**:
```elisp
(defvar claude-code-ide-tools
  '((openFile . claude-code-ide-tool-open-file)
    (openDiff . claude-code-ide-tool-open-diff)
    (getCurrentSelection . claude-code-ide-tool-get-current-selection)
    ;; ... etc
    ))
```

**Manual Testing**:
1. **Individual Tool Tests**:
   - **openFile**: Ask Claude to "open file.txt line 10"
     - Verify file opens in Emacs
     - Check cursor is at line 10
   - **openDiff**: Ask Claude to show a diff
     - Verify ediff session starts
     - Test accepting/rejecting changes
   - **getCurrentSelection**: Select text and ask Claude "what is selected?"
     - Verify Claude sees the correct text
   - **getOpenEditors**: Ask Claude "what files are open?"
     - Verify list matches open buffers
   - **getDiagnostics**: Create syntax error and ask Claude "what errors exist?"
     - Verify Claude sees flycheck/flymake errors

2. **Tool Error Handling**:
   - Test tools with invalid parameters
   - Verify proper error responses
   - Check non-existent files return errors

3. **Blocking Tool Test**:
   - Test openDiff waits for user interaction
   - Verify timeout handling
   - Test concurrent tool calls

4. **Debug Output**:
   - Enable tool debug logging
   - Monitor tool invocations and responses
   - Verify parameters are correctly parsed

#### 4. Selection Tracking (`claude-code-ide-selection.el`)

**Purpose**: Track editor state changes and notify Claude.

**Implementation**:
- Hook into `post-command-hook` for cursor/selection changes
- Track active buffer changes
- Debounce updates (50ms delay)
- Send `selection_changed` notifications
- Only track files within project directory

**Key Functions**:
```elisp
;; Start tracking for a session
(cl-defun claude-code-ide-selection-start (session)
  ...)

;; Send selection update
(cl-defun claude-code-ide-selection-notify (session)
  ...)
```

**Manual Testing**:
1. **Selection Change Test**:
   - Move cursor in a file within project
   - Verify `selection_changed` notifications sent
   - Check debouncing works (rapid movements = single update)

2. **Buffer Switch Test**:
   - Switch between buffers
   - Verify active editor notifications sent
   - Check only project files trigger updates

3. **Region Selection Test**:
   - Select text regions
   - Verify selection info is accurate
   - Test visual and non-visual selections

4. **Performance Test**:
   - Make rapid cursor movements
   - Verify no lag in editor
   - Check message frequency is reasonable

5. **Debug Output**:
   - Enable selection tracking debug
   - Monitor notification frequency
   - Verify selection coordinates are correct

### Integration with claude-code.el

#### 1. Startup Integration

Modify `claude-code--start` to:
- Start WebSocket server before launching Claude
- Create lock file in `~/.claude/ide/[port].lock`
- Set environment variables:
  - `CLAUDE_CODE_SSE_PORT`: WebSocket server port
  - `ENABLE_IDE_INTEGRATION=true`
- Store session in buffer-local variable

#### 2. Lock File Management

**Location**: `~/.claude/ide/[port].lock`

**Content**:
```json
{
  "pid": 12345,
  "workspaceFolders": ["/path/to/project"],
  "ideName": "Emacs",
  "transport": "ws"
}
```

**Cleanup**: Remove on server shutdown or buffer kill

#### 3. Buffer-Local Variables

Add to Claude buffers:
```elisp
(defvar-local claude-code-ide-session nil
  "IDE protocol session for this Claude instance.")
```

#### 4. Cleanup Integration

Modify `claude-code--kill-buffer` to:
- Stop WebSocket server
- Remove lock file
- Clean up session resources

### Configuration Options

Add new customization options:

```elisp
(defcustom claude-code-enable-ide-integration t
  "Enable IDE integration protocol for Claude Code."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-ide-port-min 10000
  "Minimum port number for IDE WebSocket server."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-ide-port-max 65535
  "Maximum port number for IDE WebSocket server."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-ide-selection-delay 0.05
  "Delay in seconds before sending selection updates."
  :type 'float
  :group 'claude-code)
```

### Implementation Phases

#### Phase 1: Core Infrastructure
1. Implement WebSocket server with basic frame handling
2. Implement JSON-RPC message routing
3. Create session management structure
4. Add lock file creation/cleanup
5. Integrate server startup with claude-code--start

#### Phase 2: Basic Tools
1. Implement initialize handshake
2. Implement tools/list response
3. Implement simple tools:
   - getCurrentSelection
   - getOpenEditors
   - getWorkspaceFolders
   - checkDocumentDirty

#### Phase 3: File Operations
1. Implement openFile with selection support
2. Implement saveDocument
3. Implement close_tab
4. Add proper error handling

#### Phase 4: Advanced Features
1. Implement openDiff with ediff integration
2. Implement getDiagnostics with Flycheck/Flymake
3. Add selection change tracking
4. Implement at_mentioned notifications

#### Phase 5: Polish and Testing
1. Add comprehensive error handling
2. Add debug logging capabilities
3. Test with multiple Claude instances
4. Ensure proper cleanup on all exit paths
5. Add documentation

### Technical Considerations

#### WebSocket Implementation
- No external dependencies (implement RFC 6455 directly)
- Support text frames only (binary not needed)
- Handle connection errors gracefully
- Implement proper frame masking for client messages

#### Async Operations
- Use timers for debouncing
- Consider `make-thread` for blocking operations
- Ensure UI remains responsive

#### Error Handling
- Return proper JSON-RPC error responses
- Log errors for debugging
- Graceful degradation if IDE integration fails

#### Performance
- Efficient frame parsing
- Minimal overhead for selection tracking
- Lazy initialization of features

### Testing Strategy

1. **Unit Tests**:
   - WebSocket frame parsing/creation
   - JSON-RPC message handling
   - Individual tool functions

2. **Integration Tests**:
   - Full handshake flow
   - Tool invocation round-trip
   - Multi-instance scenarios

3. **Manual Testing**:
   - Test with actual Claude Code CLI
   - Verify all tools work correctly
   - Test error scenarios

#### End-to-End Manual Testing

1. **Basic Integration Test**:
   ```bash
   # Start Emacs with claude-code
   # M-x claude-code
   # Verify in terminal:
   echo $CLAUDE_CODE_SSE_PORT
   echo $ENABLE_IDE_INTEGRATION
   # Check lock file:
   ls ~/.claude/ide/*.lock
   cat ~/.claude/ide/*.lock
   ```

2. **Tool Integration Test**:
   - Ask Claude: "What file am I looking at?"
   - Ask Claude: "Open test.txt and go to line 5"
   - Ask Claude: "Show me the errors in this file"
   - Ask Claude: "What files are currently open?"

3. **Multi-Instance Test**:
   - Start multiple Claude instances
   - Verify each has separate WebSocket server
   - Test cross-instance isolation

4. **Cleanup Test**:
   - Kill Claude process
   - Verify WebSocket server stops
   - Check lock file is removed
   - Ensure no orphaned processes

### Documentation

1. **User Documentation**:
   - How to enable/disable IDE integration
   - Troubleshooting common issues
   - Feature overview

2. **Developer Documentation**:
   - Architecture overview
   - Adding new tools
   - Protocol details

### Future Enhancements

1. **Additional Tools**:
   - Code formatting
   - Refactoring support
   - Test running

2. **Enhanced Notifications**:
   - File change notifications
   - Build status updates
   - VCS status changes

3. **Performance Optimizations**:
   - Connection pooling
   - Message batching
   - Compression support
