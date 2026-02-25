#!/bin/bash
set -e

echo "=== Experience Agent Setup ==="

# Check dependencies
command -v git >/dev/null 2>&1 || { echo "Error: git not found"; exit 1; }
command -v opam >/dev/null 2>&1 || { echo "Error: opam not found"; exit 1; }

# Install ChromaDB via pipx
if ! command -v chroma >/dev/null 2>&1; then
  echo "Installing ChromaDB via pipx..."
  command -v pipx >/dev/null 2>&1 || { echo "Installing pipx..."; brew install pipx; pipx ensurepath; }
  pipx install chromadb --python python3.12
fi

# Install Ollama for embeddings
if ! command -v ollama >/dev/null 2>&1; then
  echo "Installing Ollama..."
  brew install ollama
fi

# Pull embedding model
echo "Pulling embedding model..."
ollama pull nomic-embed-text

# Install OCaml dependencies
echo "Installing OCaml dependencies..."
opam install yojson cohttp-lwt-unix lwt uuidm -y

# Build
echo "Building experience-agent..."
cd "$(dirname "$0")/.."
dune build

BINARY_PATH="$(pwd)/_build/default/bin/main.exe"
echo ""
echo "=== Setup complete ==="
echo ""
echo "Binary: $BINARY_PATH"
echo ""
echo "Before using, start both services:"
echo "  ollama serve &"
echo "  chroma run --port 8000 &"
echo ""
echo "Then restart Claude Code."
