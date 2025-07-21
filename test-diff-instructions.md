# Testing Diff Display with Claude Window Preservation

## Setup

1. Make sure you have Claude running:
   ```
   M-x claude-code
   ```

2. Load the test file:
   ```
   M-x load-file RET test-diff-display.el RET
   ```

## Running the Test

1. Execute the test function:
   ```
   M-x test-diff-display
   ```

2. You should see:
   - A diff buffer appear in a bottom side window (taking up about 30% of the frame height)
   - The Claude window should remain visible in its original position
   - The diff shows changes between "original.el" and "modified.el"

## Expected Behavior

The diff display should:
- Show in a dedicated bottom window
- Preserve the Claude window layout
- Allow you to navigate the diff with standard diff-mode commands
- Allow accepting/rejecting changes when prompted

## Manual Testing

You can also test through the existing test function in claude-code-mcp.el:
```
M-x claude-code-mcp--test-window-preservation
```

This requires a test-example.el file to exist.

## Troubleshooting

If the Claude window gets hidden:
- Check that you're using the latest code
- Ensure Claude is running before testing
- Try restarting Emacs and Claude

The implementation uses `display-buffer-in-side-window` to ensure the diff doesn't interfere with Claude's window.