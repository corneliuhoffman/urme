#!/bin/bash

echo "=== Experience Agent - Stop ==="
echo ""

# Stop Ollama
if pgrep -x ollama >/dev/null 2>&1; then
  echo "Stopping Ollama (pid $(pgrep -x ollama))..."
  pkill -x ollama
  echo "Ollama stopped."
else
  echo "Ollama is not running."
fi

# Stop ChromaDB
CHROMA_PID=$(pgrep -f "chroma run" 2>/dev/null || true)
if [ -n "$CHROMA_PID" ]; then
  echo "Stopping ChromaDB (pid $CHROMA_PID)..."
  kill $CHROMA_PID
  echo "ChromaDB stopped."
else
  echo "ChromaDB is not running."
fi

echo ""
echo "=== Stopped ==="
