# Claude Code Development Guide

## Build and Test Commands
- Install package: `M-x package-install-file RET /path/to/claude-code.el`
- Use project Makefile targets (preferred methods):
  - Byte compile: `make compile`
  - Lint with checkdoc: `make checkdoc`
  - Run both checkdoc and compile: `make all`
- Alternative direct commands:
  - Byte compile: `emacs -Q --batch -f batch-byte-compile claude-code.el`
  - Check package: `emacs -Q --batch -l package-lint.el -f package-lint-batch-and-exit claude-code.el`
  - Lint with checkdoc: `emacs -Q --batch -l checkdoc -f checkdoc-file claude-code.el`

## Code Style Guidelines
- Prefix all functions/variables with `claude-code-` (public) or `claude-code--` (private)
- Follow standard Emacs Lisp naming conventions (kebab-case)
- Use lexical binding (include `lexical-binding: t` in header)
- Organize with section headers: `;;;; Section Name`
- Maintain proper package headers and autoload declarations
- Docstring for all functions and variables
  - Add a blank line after the first line of docstrings for multi-line descriptions
  - First line should be a complete sentence ending with a period
- Maintain dependency requirements: Emacs 30.1+, transient 0.4.0+, eat 0.8+

## Error Handling
- Use `if-let` for conditional execution with potential null values
- Provide clear error messages with `error` function
- Check for running Claude instance before sending commands

## Project Structure
- Single file package with clear module organization
- Licensed under Apache License 2.0

## MCP Server Usage Guide

This project includes an MCP (Model Context Protocol) server that exposes Emacs functionality to MCP-compatible clients.

### Server Architecture
- **TCP Server**: Emacs-based TCP server (port 8765) handles MCP protocol
- **Bridge Process**: Node.js process bridges between Emacs and external MCP clients
- **Tool Discovery**: Dynamic discovery of functions marked with `claude-code-defmcp`

### Setting Up the MCP Server

1. **Build the Node.js bridge** (required):
   ```bash
   cd mcp-server
   npm install && npm run build
   ```

2. **Load example tools** (the server provides no tools by default):
   ```elisp
   (load-file "examples/mcp/mcp-tools.el")
   ```

3. **Install in Claude Code CLI**:
   ```elisp
   M-x claude-code-install-mcp-server
   ```

4. **Configure directory permissions** in `~/.claude/settings.json`:
   ```json
   {
     "permissions": {
       "additionalDirectories": ["/tmp/ClaudeWorkingFolder"]
     }
   }
   ```

### Available Tools (from examples/mcp/mcp-tools.el)

#### Basic Utilities

**`mcp-hello-world`** - Basic greeting functionality for testing
- **Parameters**: `name` (string) - Name of person to greet
- **Returns**: Greeting message with emoji
- **Usage**: Test MCP connectivity and basic functionality

#### Variable Access

**`mcp-get-variable-value`** - Get values of Emacs variables
- **Parameters**: `variable-names` (array) - List of variable names to query
- **Returns**: Formatted string with variable names and their current values
- **Usage**: Inspect Emacs configuration, debug settings, check customizations
- **Example**: Query `org-directory`, `user-full-name`, `emacs-version`

#### File Operations

**`mcp-open-file`** - Open files in Emacs buffers
- **Parameters**: `file-paths` (array) - List of file paths to open (relative to current directory)
- **Returns**: Mapping of file paths to buffer names
- **Usage**: Load files for analysis, prepare buffers for other operations
- **Security**: Restricted to current working directory and `/tmp/ClaudeWorkingFolder`

**`mcp-check-parens`** - Validate parentheses balance in Lisp files
- **Parameters**: `file-paths` (array) - List of Lisp file paths to check
- **Returns**: Status of parentheses balance for each file with error locations
- **Usage**: Debug Emacs Lisp syntax errors, validate code before loading

**`mcp-count-parens`** - Count parentheses between two lines in a file
- **Parameters**: 
  - `file-path` (string) - Path to the file to analyze
  - `start-line` (number) - Starting line number (1-based)
  - `end-line` (number) - Ending line number (1-based)
- **Returns**: Count of opening and closing parentheses with net balance
- **Usage**: Debug parentheses balance issues, analyze specific code sections

#### Emacs Introspection

**`mcp-emacs-search`** - Search for symbols, commands, variables, functions
- **Parameters**: 
  - `pattern` (string) - Search pattern/regex
  - `type` (string, optional) - "all", "commands", "variables", "functions" (default: "all")
  - `predicate` (string, optional) - Additional filter predicate
- **Returns**: List of matching symbols with their types (function, variable, command)
- **Usage**: Explore Emacs functionality, find functions by name pattern, discover commands

**`mcp-emacs-describe`** - Get documentation for Emacs symbols
- **Parameters**: 
  - `symbol-names` (array) - List of symbol names to describe
  - `type` (string, optional) - "function", "variable", "symbol" (default: "symbol")
- **Returns**: Complete documentation including key bindings for functions
- **Usage**: Get help documentation, understand function signatures, see key bindings

**`mcp-emacs-keymap-analysis`** - Analyze keymaps for buffer contexts
- **Parameters**: 
  - `buffer-names` (array) - List of buffer names to analyze
  - `include-global` (boolean, optional) - Include global keymap (default: false)
- **Returns**: File paths containing detailed keymap analysis
- **Usage**: Debug key binding conflicts, understand mode-specific shortcuts, analyze keymap hierarchy

**`mcp-get-buffer-list`** - List all live buffers
- **Parameters**: `include-details` (boolean, optional) - Include detailed buffer information
- **Returns**: List of buffer names with optional details (file, size, modification status, major mode)
- **Usage**: Overview of current Emacs session, find specific buffers, monitor buffer states

#### Buffer Management

**`mcp-emacs-buffer-info`** - Get comprehensive buffer information
- **Parameters**: 
  - `buffer-names` (array) - List of buffer names to analyze
  - `include-content` (boolean, optional) - Include full buffer content (default: true)
  - `include-variables` (boolean, optional) - Include key variables (default: true)
- **Returns**: File paths containing detailed buffer analysis including content, mode info, variables
- **Usage**: Deep buffer inspection, debug buffer states, extract buffer content with line numbers

**`mcp-view-buffer`** - Get buffer contents with line numbers
- **Parameters**: `buffer-names` (array) - List of buffer names to view
- **Returns**: File paths containing buffer contents with line numbers
- **Usage**: Quick buffer content extraction, prepare content for analysis, view specific buffers

#### Org-Mode Integration

**`mcp-get-agenda`** - Get org-agenda view
- **Parameters**: `agenda-type` (string, optional) - Agenda type to generate (default: "a")
- **Returns**: File path containing formatted agenda view
- **Usage**: Access current agenda, analyze scheduled items, export agenda data

**`mcp-org-agenda-todo`** - Change TODO item states
- **Parameters**: 
  - `target-type` (string) - "agenda_line" or "org_heading"
  - `target` (string) - Line number (1-based) or heading text to find
  - `new-state` (string, optional) - Specific state or cycle through states
  - `agenda-type` (string, optional) - Agenda type (default: "a")
  - `org-file` (string, optional) - Required for "org_heading" type
- **Returns**: Success confirmation with details
- **Usage**: Mark tasks complete, change TODO states, manage task progression

**`mcp-org-schedule-todo`** - Schedule TODO items with dates
- **Parameters**: 
  - `org-file` (string) - Path to org file containing the heading
  - `heading-text` (string) - Text of heading to schedule
  - `schedule-date` (string) - Date to schedule ("2025-01-15", "today", "+1d", etc.)
  - `remove-schedule` (boolean, optional) - Remove existing schedule
- **Returns**: Success confirmation with scheduling details
- **Usage**: Add deadlines, schedule tasks, manage time-based TODO items

**`mcp-org-archive-todo`** - Archive TODO items to archive files
- **Parameters**: 
  - `org-file` (string) - Path to org file containing the heading
  - `heading-text` (string) - Text of heading to archive
  - `archive-location` (string, optional) - Custom archive location
- **Returns**: Success confirmation with archive location
- **Usage**: Clean up completed tasks, organize old TODO items, maintain file structure

**`mcp-org-capture`** - Add new agenda items via org-capture templates
- **Parameters**: 
  - `template-key` (string, optional) - Capture template key, shows available if not provided
  - `content` (string, optional) - Content to capture
  - `immediate-finish` (boolean, optional) - Auto-finish capture (default: true)
- **Returns**: Available templates or capture success confirmation
- **Usage**: Quick task creation, add notes with templates, capture ideas and tasks

**`mcp-org-get-all-todos`** - Get all TODO items from org files
- **Parameters**: 
  - `include-done` (boolean, optional) - Include completed items (default: false)
  - `org-files` (array, optional) - Specific files to search (defaults to org-agenda-files)
- **Returns**: File path containing comprehensive TODO listing with scheduling info
- **Usage**: Overview of all tasks, find unscheduled items, audit TODO states across files

**`mcp-org-agenda-goto`** - Navigate to agenda item source locations
- **Parameters**: 
  - `target-type` (string) - "agenda_line" or "agenda_text"
  - `target` (string) - Line number or text to search for
  - `agenda-type` (string, optional) - Agenda type (default: "a")
  - `context-lines` (number, optional) - Lines of context to show (default: 5)
- **Returns**: File path containing source location with context
- **Usage**: Jump to TODO source, see task context, locate specific agenda items

#### Doom Workspace Management

**`mcp-get-workspace-buffers`** - List buffers in Doom workspaces
- **Parameters**: `workspace-name` (string, optional) - Specific workspace or all workspaces
- **Returns**: File path containing workspace buffer listings
- **Usage**: Understand workspace organization, find buffers in specific workspaces

**`mcp-rename-workspace`** - Rename Doom workspaces
- **Parameters**: 
  - `workspace-identifier` (string) - Current workspace name or slot number
  - `new-name` (string) - New name for the workspace
- **Returns**: Success confirmation with old and new names
- **Usage**: Organize workspaces with meaningful names, improve workspace navigation

**`mcp-create-workspace`** - Create new Doom workspaces
- **Parameters**: `workspace-name` (string) - Name for the new workspace
- **Returns**: Success confirmation with workspace name
- **Usage**: Set up project-specific workspaces, organize work contexts

**`mcp-delete-workspace`** - Delete Doom workspaces with protection checks
- **Parameters**: `workspace-identifier` (string) - Workspace name or identifier
- **Returns**: Success confirmation or protection warning
- **Usage**: Clean up unused workspaces (prevents deletion of workspaces with active Claude/terminal sessions)

**`mcp-move-protected-buffers-to-workspace`** - Move Claude/terminal buffers safely
- **Parameters**: 
  - `source-workspace` (string) - Workspace containing protected buffers
  - `target-workspace` (string) - Workspace to move buffers to
- **Returns**: Success confirmation with moved buffer list
- **Usage**: Reorganize protected buffers before workspace deletion, maintain session continuity

**`mcp-setup-workspace-layout`** - Configure Doom workspace window layouts
- **Parameters**: 
  - `workspace-name` (string) - Workspace to configure
  - `layout` (object) - Layout config with `primary_buffer`, `secondary_buffer`, `split_direction`
- **Returns**: Success confirmation with layout details
- **Usage**: Set up optimal workspace layouts, prepare workspaces with specific buffer arrangements

### Creating Custom MCP Tools

Use the `claude-code-defmcp` macro to define new tools:

```elisp
(claude-code-defmcp my-tool-name (param1 param2)
  "Function documentation string."
  :mcp-description "Brief description for MCP clients"
  :mcp-schema '((param1 . ("string" "Description of param1"))
                (param2 . ("array" "Description of param2")))
  ;; Function body
  (format "Result: %s, %s" param1 param2))
```

#### Schema Format
The `:mcp-schema` uses a simplified format:
- `'((name . ("type" "description")))` for each parameter
- Types: "string", "number", "boolean", "array", "object"
- Empty schema `'()` for functions with no parameters

### Server Management Commands

- `claude-code-start-mcp-server` - Start MCP server
- `claude-code-stop-mcp-server` - Stop MCP server
- `claude-code-restart-mcp-server` - Restart MCP server
- `claude-code-mcp-status` - Show server status
- `claude-code-install-mcp-server` - Install in Claude Code CLI

### Configuration Variables

```elisp
;; Enable/disable MCP server (default: t)  
(setq claude-code-mcp-enabled t)

;; MCP server port (default: 8765)
(setq claude-code-mcp-port 8765)

;; Restrict file access to current directory and /tmp/ClaudeWorkingFolder/ (default: t)
(setq claude-code-mcp-restrict-file-access nil)  ; Allow access to any file

;; Customize buffer blocking patterns (default includes common sensitive patterns)
(setq claude-code-mcp-blocked-buffer-patterns 
      '("password" ".pem" "secret" ".key" "token" "credential" "auth" ".ssh"))
```

### Troubleshooting

1. **No tools available**: Load `examples/mcp/mcp-tools.el` or define your own tools
2. **Bridge process crashes**: Check `*claude-code-mcp-bridge*` buffer for errors
3. **Permission denied**: Add `/tmp/ClaudeWorkingFolder` to Claude Code settings
4. **Port conflicts**: Change `claude-code-mcp-port` to an available port
5. **Security validation errors**: Check parameter format - ensure file paths, symbols, and content follow Emacs conventions

### Security

The MCP server includes comprehensive input validation to prevent injection attacks:

#### Blocked Patterns
- **Elisp injection**: `"); (shell-command`, `eval`, `load-file`, etc.
- **Function definitions**: `defun`, `setq`, `defvar`, `lambda`
- **File operations**: `delete-file`, `delete-directory`, `write-file`
- **Directory traversal**: `../` sequences in file paths
- **Shell commands**: `;`, `|`, `&`, backticks, `${}`, `$()`

#### Parameter Validation
- **File paths**: Restricted to `/tmp/ClaudeWorkingFolder/` for absolute paths
- **Buffer names**: Length limits and injection pattern detection
- **Symbol names**: Validates proper Emacs symbol format (`[a-zA-Z][a-zA-Z0-9\-_:+*/?<>=!]*`)
- **Search patterns**: Length limits and dangerous pattern detection
- **Org content**: Length limits and injection pattern validation

#### Legitimate Usage Preserved
- Emacs variables with special characters (`c++-mode-hook`, `find/grep-command`)
- System buffers (`*Messages*`, `*scratch*`)
- Standard file operations within allowed directories
- Regular org-mode content and search operations

Security validation occurs at the TypeScript layer before parameters reach Emacs, providing defense-in-depth protection.

#### Security Considerations

**Important**: The MCP tools provide Claude with broad access to your Emacs environment and filesystem:

**Buffer Access**: Claude can read any buffer open in Emacs, including:
- Source code and configuration files
- Personal notes and org-mode files with sensitive data
- Password files, SSH keys, or API credentials you may have opened
- Any content in your Emacs session

**File Access**: The `mcp-open-file` tool access depends on `claude-code-mcp-restrict-file-access`:
- **When enabled (default)**: Only current working directory, subdirectories, and `/tmp/ClaudeWorkingFolder/`
- **When disabled**: Can access any file on the system (blocks only injection patterns)
- **Always blocked**: Directory traversal (`../`) when restrictions enabled

**Emacs Users Often Have Sensitive Data**: Many Emacs users keep sensitive information in their editor environment - configuration files with tokens, personal notes, password managers, SSH configs, etc. **Assume Claude has access to any sensitive information in your Emacs session.**

**Data Exfiltration Risk**: Combining file/buffer reading with web access creates potential for data exfiltration attacks (see [Lethal Trifecta](https://simonwillison.net/tags/lethal-trifecta/)). Consider:
- Limiting Claude's web access when using MCP tools
- Closing sensitive buffers before enabling MCP integration
- Using dedicated Emacs sessions for Claude interactions
- Being cautious about which directories you run Claude from
- Using Claude Code's directory restrictions in settings

### File Locations

- MCP server source: `mcp-server/src/index.ts`
- Security validation: `mcp-server/src/security.ts`
- Example tools: `examples/mcp/mcp-tools.el`
- Temp file directory: `/tmp/ClaudeWorkingFolder/`
- Bridge process logs: `*claude-code-mcp-bridge*` buffer

### Uninstalling

Remove from Claude Code CLI:
```bash
claude mcp remove emacs
```

## Testing MCP Integration

Test the server manually:
1. Start Emacs with claude-code loaded
2. Load example tools: `(load-file "examples/mcp/mcp-tools.el")`
3. Check server status: `M-x claude-code-mcp-status`
4. View available tools via MCP client or Claude Code CLI

## Debugging Elisp Code

When debugging parentheses balance issues in Elisp code, use these MCP tools:

### Parentheses Debugging Tools

**`mcp-check-parens`** - Comprehensive parentheses validation
- Validates entire file for balanced parentheses, quotes, and brackets
- Reports exact line and column of first unmatched bracket
- Essential for debugging syntax errors before loading Elisp code

**`mcp-count-parens`** - Granular parentheses analysis  
- Counts opening and closing parentheses between specific lines
- Shows net balance (should be 0 for complete expressions)
- Useful for isolating problematic code sections
- Note: Individual code sections may have non-zero net balance, but complete functions should balance

### Debugging Workflow

1. **Full file check**: Use `mcp-check-parens` to identify if there are balance issues
2. **Section analysis**: Use `mcp-count-parens` to analyze specific functions or code blocks
3. **Iterative fixing**: Fix one unmatched bracket at a time, re-checking after each fix
4. **Function boundaries**: Ensure each complete function definition has net balance of 0

### Example Usage

```elisp
;; Check entire file
mcp-check-parens(["examples/mcp/mcp-tools.el"])

;; Analyze specific function (lines 34-44)  
mcp-count-parens("examples/mcp/mcp-tools.el", 34, 44)
```

This approach helps systematically identify and fix parentheses mismatches in complex Elisp code.