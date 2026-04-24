#!/usr/bin/env bash
# PostToolUse hook: validates output after Sports pipeline runs.
# Checks for failures and stale posteriors.
# Reads JSON from stdin (Claude hooks protocol).

INPUT_JSON=$(cat)

# Only process if the command was a Sports pipeline run
COMMAND=$(echo "$INPUT_JSON" | jq -r '.tool_input.command // empty')
if ! echo "$COMMAND" | grep -q "run.R"; then
  exit 0
fi

# Read the tool output (stdout + stderr)
TOOL_OUTPUT=$(echo "$INPUT_JSON" | jq -r '(.tool_response.stdout // "") + "\n" + (.tool_response.stderr // "")')

# Check for failure indicators
FAILURES=$(echo "$TOOL_OUTPUT" | grep -c "\[FAIL\]" 2>/dev/null || echo "0")
ERRORS=$(echo "$TOOL_OUTPUT" | grep -c "\[ERROR\]" 2>/dev/null || echo "0")

WARNINGS=""

if [ "$FAILURES" -gt 0 ]; then
  WARNINGS="${WARNINGS}⚠️ ${FAILURES} pipeline step(s) reported [FAIL]\n"
fi

if [ "$ERRORS" -gt 0 ]; then
  WARNINGS="${WARNINGS}⚠️ ${ERRORS} pipeline step(s) reported [ERROR]\n"
fi

# Check for stale posterior warnings in output
STALE=$(echo "$TOOL_OUTPUT" | grep -ci "stale\|old posterior\|posterior.*ago" 2>/dev/null || echo "0")
if [ "$STALE" -gt 0 ]; then
  WARNINGS="${WARNINGS}⚠️ Stale posteriors detected — consider running --step fit to refresh models\n"
fi

if [ -n "$WARNINGS" ]; then
  echo -e "Pipeline validation:\n${WARNINGS}"
fi
