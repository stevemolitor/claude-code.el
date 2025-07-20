# Claude Code MCP Implementation - Iterative Plan

## Approach: Build, Integrate, Test, Repeat

Instead of implementing all 12 tools first, we'll take an iterative approach where we integrate with claude-code.el immediately and add tools one by one, testing each as we go.

## CRITICAL DISCOVERY: Claude Crashes Without Essential Tools!

**When Claude tries to use tools like `openDiff` and receives errors, it immediately disconnects and exits.** This means we cannot enable IDE integration without implementing at least stub versions of critical tools.

## Phase 0: URGENT - Implement Stub Tools to Prevent Crashes

### Step 0.1: Add minimal stub implementations
Before we can even test integration, we MUST implement these tools as stubs:
- `openDiff` - Return success without actually opening diff
- `close_tab` - Return success without action
- `getDiagnostics` - Return empty diagnostics array
- `closeAllDiffTabs` - Return success without action

These prevent Claude from crashing when it tries to use them.

## Phase 1: Basic Integration (Day 1)

### Step 1.1: Wire up MCP server to claude-code.el [COMPLETE]
- ✅ Added `claude-code-enable-ide-integration` customization variable
- ✅ Modified `claude-code--start` to automatically start MCP server when integration is enabled
- ✅ Set `CLAUDE_CODE_SSE_PORT` and `ENABLE_IDE_INTEGRATION=true` environment variables
- ✅ Store MCP session in buffer-local variable `claude-code--mcp-session`
- ✅ Added cleanup in `claude-code--kill-buffer` via `claude-code-mcp--cleanup-session`

### Step 1.2: Implement first real tool - getCurrentSelection [COMPLETE]
- ✅ Added getCurrentSelection to tools list
- ✅ Added handler mapping in claude-code-mcp--get-tool-handler
- ✅ Implemented claude-code-mcp--tool-get-current-selection
- ✅ Returns current selection data or empty selection if no file open
- ✅ Uses existing claude-code-mcp--get-selection function

### Step 1.3: Test with Claude
- Launch Claude with integration enabled
- Verify websocket connection succeeds
- Test that Claude can call getCurrentSelection
- Debug any protocol issues

## Phase 2: Core Tools (Day 2)

### Step 2.1: Implement openFile [COMPLETE]
- ✅ Added openFile to tools list with proper schema
- ✅ Added handler mapping in claude-code-mcp--get-tool-handler
- ✅ Implemented claude-code-mcp--tool-open-file function
- ✅ Handles both file:// URIs and regular paths
- ✅ Opens files with find-file
- ✅ Supports optional text selection with start/end positions
- ✅ Converts 0-based line numbers to Emacs 1-based

### Step 2.2: Implement getOpenEditors & getWorkspaceFolders
- Provide workspace context to Claude
- These are read-only, low risk
- Test Claude's understanding of project structure

### Step 2.3: Live testing
- Use Claude for actual coding tasks
- Identify any issues with the core tools
- Refine based on real usage

## Phase 3: Advanced Tools (Day 3)

### Step 3.1: Implement openDiff
- More complex - requires ediff integration
- Blocking operation that waits for user
- Critical for code review workflow

### Step 3.2: Document tools
- checkDocumentDirty
- saveDocument
- close_tab

### Step 3.3: Diagnostic tools
- getDiagnostics (flycheck/flymake integration)
- getLatestSelection
- closeAllDiffTabs

## Implementation Order & Rationale

1. **Stub implementations** - Prevent crashes (openDiff, close_tab, getDiagnostics, closeAllDiffTabs)
2. **getCurrentSelection** - Simplest, proves the system works
3. **openFile** - Most useful, enables core workflows  
4. **getOpenEditors/getWorkspaceFolders** - Provides context
5. **openDiff** (real implementation) - Enables code review workflow
6. **Document management** - saveDocument, checkDocumentDirty
7. **Cleanup tools** (real implementation) - close_tab, closeAllDiffTabs
8. **Advanced** - getDiagnostics (real implementation), getLatestSelection

## Benefits of Iterative Approach

1. **Immediate feedback** - Test with real Claude after each tool
2. **Early validation** - Catch protocol issues immediately
3. **User testing** - Can use partially complete system
4. **Prioritized development** - Most important tools first
5. **Reduced risk** - Problems found early, not after implementing everything

## Success Metrics Per Iteration

### After Phase 0:
- Claude doesn't crash when using unimplemented tools
- Can maintain stable connection

### After Phase 1:
- Claude connects successfully
- Can query current selection
- No authentication errors

### After Phase 2:
- Claude can open and navigate files
- Understands project structure
- Core editing workflow functional

### After Phase 3:
- Full diff review workflow
- All tools implemented
- Production ready

## Quick Start Implementation

```elisp
;; In claude-code.el, add to claude-code--start:
(when claude-code-enable-ide-integration
  (require 'claude-code-mcp)
  (let* ((session (claude-code-mcp--start-server 
                   (buffer-name) 
                   default-directory))
         (port (claude-code-mcp--session-port session)))
    (setq-local claude-code--mcp-session session)
    ;; Add to process environment
    (setq claude-code--process-environment
          (append `(,(format "CLAUDE_CODE_SSE_PORT=%d" port)
                    "ENABLE_IDE_INTEGRATION=true")
                  process-environment))))

;; First tool implementation:
(defun claude-code-mcp--tool-get-current-selection (params)
  "Get current selection or cursor position."
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties 
                    (region-beginning) (region-end))
                 ""))
         (result (json-encode
                  `((success . t)
                    (text . ,text)
                    (filePath . ,(or (buffer-file-name) ""))
                    (selection . ,(claude-code-mcp--get-selection))))))
    `((content . [((type . "text") (text . ,result))]))))
```

## Next Immediate Steps

1. Implement stub tools to prevent crashes
2. Create a minimal integration in claude-code.el
3. Implement getCurrentSelection tool
4. Test with Claude
5. Fix any issues
6. Move to next tool

This approach ensures we have a working system at each step and can course-correct based on actual usage.