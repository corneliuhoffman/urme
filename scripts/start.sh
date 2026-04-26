#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== urme - Start ==="
echo ""

# --- Build binary if needed ---
BINARY="$PROJECT_DIR/bin/experience-agent"
if [ ! -f "$BINARY" ]; then
  echo "Binary not found. Building..."
  make -C "$PROJECT_DIR" build || {
    echo "ERROR: Build failed. Try 'make setup' first."
    exit 1
  }
fi

# --- Offer to install MCP server globally ---
echo ""
if command -v claude >/dev/null 2>&1; then
  read -p "Register urme as a global MCP server in Claude Code? [y/N] " ans
  case "$ans" in
    [yY]*)
      claude mcp add -s user experience "$BINARY" -- --project-dir .
      echo "MCP server registered globally. Restart Claude Code to activate."
      ;;
    *)
      echo "Skipped. To register manually later, run:"
      echo "  claude mcp add -s user experience $BINARY -- --project-dir ."
      echo ""
      echo "Or add to a project's .mcp.json:"
      echo '  {'
      echo '    "mcpServers": {'
      echo '      "experience": {'
      echo '        "type": "stdio",'
      echo "        \"command\": \"$BINARY\","
      echo '        "args": ["--project-dir", "."]'
      echo '      }'
      echo '    }'
      echo '  }'
      ;;
  esac
else
  echo "Claude Code CLI not found. To register the MCP server later:"
  echo "  claude mcp add -s user experience $BINARY -- --project-dir ."
fi

echo ""
echo "=== Ready ==="
echo ""
echo "First run in a project:"
echo "  $BINARY --project-dir . init"
echo ""
echo "Then launch the TUI:"
echo "  $BINARY --project-dir ."
