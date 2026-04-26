#!/bin/bash

echo "=== urme - Stop ==="
echo ""

# urme has no long-running services to stop — the TUI, MCP server, and
# index jobs are all in-process with the single urme binary. This script
# is kept for parity with the old V1 workflow (Ollama + ChromaDB).

# Stop any stray urme processes (optional).
URME_PIDS=$(pgrep -f 'experience-agent|urme' 2>/dev/null || true)
if [ -n "$URME_PIDS" ]; then
  echo "Running urme processes: $URME_PIDS"
  echo "(use 'pkill -f urme' to terminate)"
else
  echo "No urme processes running."
fi

echo ""
echo "=== Done ==="
