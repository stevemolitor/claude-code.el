#!/bin/bash

# Generic wrapper script for Claude Code hooks using emacsclient
# This script handles the JSON output cleaning that emacsclient requires
#
# Usage: claude-code-hook-wrapper.sh <hook-type>
# The hook type is passed as the first argument (e.g., 'notification', 'pre-tool-use')

hook_type="$1"

# Read the JSON input from stdin
json_input=$(cat)

# Debug: Log the raw JSON to a file
echo "Raw JSON for $hook_type:" >> /tmp/claude-hook-debug.log
echo "$json_input" >> /tmp/claude-hook-debug.log
echo "---" >> /tmp/claude-hook-debug.log

# Call emacsclient and get the response
# Pass the JSON as an additional string argument that will be picked up by server-eval-args-left
# Don't escape the JSON - let emacsclient handle it as a literal string
response=$(emacsclient --eval "(claude-code-handle-hook '$hook_type \"$CLAUDE_BUFFER_NAME\")" "$json_input" 2>/dev/null)

# Check if the response looks like a JSON string (starts and ends with quotes)
if [[ "$response" =~ ^\".*\"$ ]]; then
    # Remove the surrounding quotes that emacsclient adds
    # emacsclient returns strings with quotes like "\"json content\""
    cleaned_response=$(echo "$response" | sed 's/^"//;s/"$//' | sed 's/\\"/"/g')
    echo "$cleaned_response"
elif [[ "$response" == "nil" || -z "$response" ]]; then
    # If response is nil or empty, don't output anything
    :
else
    # For non-JSON responses, output as-is
    echo "$response"
fi