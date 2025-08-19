# Claude Code Hooks Examples

This directory contains examples for setting up Claude Code hooks with Emacs integration.

## Files

- `../bin/claude-code-hook-wrapper.sh` - Generic wrapper script for emacsclient hook integration (moved to bin directory)
- `example_settings.json` - Example settings.json with proper hook configuration
- `claude-code-hook-examples.el` - Emacs Lisp hook handler examples

## Quick Setup

1. **Add the claude-code.el/bin directory to your PATH:**
   ```bash
   export PATH="/path/to/claude-code.el/bin:$PATH"
   ```
   Or add this line to your shell configuration file (~/.bashrc, ~/.zshrc, etc.)

2. **Update your `~/.claude/settings.json` based on `example_settings.json`:**
   ```json
   {
     "hooks": {
       "PreToolUse": [{
         "matcher": "",
         "hooks": [{
           "type": "command", 
           "command": "claude-code-hook-wrapper.sh pre-tool-use"
         }]
       }]
     }
   }
   ```

3. **Load the hook handlers in Emacs:**
   ```elisp
   (load-file "/path/to/claude-code-hook-examples.el")
   ```

## How It Works

### The Wrapper Script Problem

When using `emacsclient --eval` with hooks that return JSON responses, emacsclient wraps the output in quotes like `"{\\"json\\": \\"data\\"}"`. Claude Code expects clean JSON, so the wrapper script:

1. Calls emacsclient with the hook
2. Detects if the response is a quoted JSON string
3. Strips the outer quotes and unescapes inner quotes
4. Returns clean JSON to Claude Code

### Hook Types

- **PreToolUse**: Shows minibuffer prompts for tool permissions (y/n/q responses)
- **PostToolUse**: Handles completion notifications and logging
- **Notification**: Processes Claude Code notifications
- **Stop**: Handles session stop events

### JSON Response Format

Hooks can return JSON to control Claude Code behavior:

```json
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "allow",
    "permissionDecisionReason": "User approved via minibuffer"
  }
}
```

### PreToolUse Minibuffer Integration

When Claude wants to use a tool, you'll see a minibuffer prompt:
```
Claude wants to use WebFetch with args: {"url": "https://example.com"} - Allow? (y/n/q):
```

- **y/Y**: Allow the tool use
- **n/N**: Deny the tool use  
- **q/Q**: Request UI confirmation (falls back to Claude's permission dialog)

## Customization

Edit the `.el` files to customize hook behavior:
- Add custom tool permission logic
- Implement project-specific notifications  
- Create custom logging and monitoring
- Add integration with other Emacs packages

## Troubleshooting

1. **Hook not triggering**: Check that the wrapper script is executable and the path is correct
2. **JSON errors**: Ensure the wrapper script properly handles emacsclient output
3. **Minibuffer not appearing**: Verify that claude-command.el is loaded and the handler functions are available

For more details, see the main claude-code.el documentation.