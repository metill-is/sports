#!/bin/bash
# PostToolUse hook: syntax-check Stan files after edits
# Uses stanc (fast frontend-only check, no C++ compilation)

FILE_PATH=$(cat | jq -r '.tool_input.file_path // empty')

if [ -z "$FILE_PATH" ]; then
  exit 0
fi

if echo "$FILE_PATH" | grep -q '\.stan$'; then
  STANC=$(ls -d "$HOME"/.cmdstan/cmdstan-*/bin/stanc 2>/dev/null | sort -V | tail -1)
  if [ -n "$STANC" ] && [ -x "$STANC" ]; then
    OUTPUT=$("$STANC" --warn-pedantic "$FILE_PATH" 2>&1)
    if [ $? -ne 0 ]; then
      echo "Stan syntax check failed:" >&2
      echo "$OUTPUT" >&2
      exit 2
    fi
  fi
fi

exit 0
