#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CHROMADB_PORT=8000

echo "=== Experience Agent - Start ==="
echo ""

# Detect OS
OS="$(uname -s)"
case "$OS" in
  Darwin)  PKG_MGR="brew" ;;
  Linux)
    if command -v apt-get >/dev/null 2>&1; then
      PKG_MGR="apt"
    elif command -v dnf >/dev/null 2>&1; then
      PKG_MGR="dnf"
    elif command -v pacman >/dev/null 2>&1; then
      PKG_MGR="pacman"
    else
      PKG_MGR="unknown"
    fi
    ;;
  *) PKG_MGR="unknown" ;;
esac

install_ollama() {
  case "$PKG_MGR" in
    brew)   brew install ollama ;;
    apt)    curl -fsSL https://ollama.com/install.sh | sh ;;
    dnf)    curl -fsSL https://ollama.com/install.sh | sh ;;
    pacman) curl -fsSL https://ollama.com/install.sh | sh ;;
    *)
      echo "Unsupported OS. Install Ollama manually: https://ollama.com/download"
      return 1
      ;;
  esac
}

install_chromadb() {
  if ! command -v pipx >/dev/null 2>&1; then
    echo "pipx is required to install ChromaDB."
    read -p "Install pipx? [y/N] " ans
    case "$ans" in
      [yY]*)
        case "$PKG_MGR" in
          brew)   brew install pipx && pipx ensurepath ;;
          apt)    sudo apt-get install -y pipx && pipx ensurepath ;;
          dnf)    sudo dnf install -y pipx && pipx ensurepath ;;
          pacman) sudo pacman -S --noconfirm python-pipx && pipx ensurepath ;;
          *)
            echo "Unsupported OS. Install pipx manually: https://pipx.pypa.io"
            return 1
            ;;
        esac
        ;;
      *) echo "Cannot install ChromaDB without pipx."; return 1 ;;
    esac
  fi
  pipx install chromadb
}

# --- Check Ollama ---
MISSING=0
if ! command -v ollama >/dev/null 2>&1; then
  echo "Ollama is not installed."
  read -p "Install Ollama now? [y/N] " ans
  case "$ans" in
    [yY]*)
      install_ollama || { echo "ERROR: Failed to install Ollama."; exit 1; }
      ;;
    *)
      echo ""
      echo "ERROR: Ollama is required but not installed."
      echo "Install it manually:"
      echo "  macOS:  brew install ollama"
      echo "  Linux:  curl -fsSL https://ollama.com/install.sh | sh"
      echo ""
      MISSING=1
      ;;
  esac
fi

# --- Check ChromaDB ---
if ! command -v chroma >/dev/null 2>&1; then
  echo "ChromaDB is not installed."
  read -p "Install ChromaDB now? [y/N] " ans
  case "$ans" in
    [yY]*)
      install_chromadb || { echo "ERROR: Failed to install ChromaDB."; exit 1; }
      ;;
    *)
      echo ""
      echo "ERROR: ChromaDB is required but not installed."
      echo "Install it manually:"
      echo "  pipx install chromadb"
      echo "  (needs pipx: brew install pipx / apt install pipx)"
      echo ""
      MISSING=1
      ;;
  esac
fi

if [ "$MISSING" -eq 1 ]; then
  echo "Aborting: missing prerequisites. Install them and re-run."
  exit 1
fi

# --- Start services ---
echo ""

# Ollama
if pgrep -x ollama >/dev/null 2>&1; then
  echo "Ollama is already running."
else
  echo "Starting Ollama..."
  if [ "$OS" = "Darwin" ]; then
    GGML_METAL_TENSOR_DISABLE=1 ollama serve > /tmp/ollama.log 2>&1 &
  else
    ollama serve > /tmp/ollama.log 2>&1 &
  fi
  sleep 2
  if pgrep -x ollama >/dev/null 2>&1; then
    echo "Ollama started (pid $(pgrep -x ollama), log: /tmp/ollama.log)"
  else
    echo "WARNING: Ollama may not have started. Check /tmp/ollama.log"
  fi
fi

# --- Pull embedding model if needed (after Ollama is running) ---
if ! ollama list 2>/dev/null | grep -q "nomic-embed-text"; then
  echo "Pulling nomic-embed-text model..."
  ollama pull nomic-embed-text
fi

# ChromaDB
if lsof -i :"$CHROMADB_PORT" >/dev/null 2>&1; then
  echo "ChromaDB is already running on port $CHROMADB_PORT."
else
  echo "Starting ChromaDB on port $CHROMADB_PORT..."
  chroma run --port "$CHROMADB_PORT" > /tmp/chromadb.log 2>&1 &
  sleep 2
  if lsof -i :"$CHROMADB_PORT" >/dev/null 2>&1; then
    echo "ChromaDB started (port $CHROMADB_PORT, log: /tmp/chromadb.log)"
  else
    echo "WARNING: ChromaDB may not have started. Check /tmp/chromadb.log"
  fi
fi

# --- Build binary if needed ---
BINARY="$PROJECT_DIR/bin/experience-agent"
if [ ! -f "$BINARY" ]; then
  echo ""
  echo "Binary not found. Building..."
  make -C "$PROJECT_DIR" build || { echo "ERROR: Build failed. Do you have opam and dune installed? Try 'make setup' first."; exit 1; }
fi

# --- Offer to install MCP server globally ---
echo ""
if command -v claude >/dev/null 2>&1; then
  read -p "Register experience-agent as a global MCP server in Claude Code? [y/N] " ans
  case "$ans" in
    [yY]*)
      claude mcp add -s user experience "$BINARY" -- --chromadb-port "$CHROMADB_PORT" --project-dir .
      echo "MCP server registered globally. Restart Claude Code to activate."
      ;;
    *)
      echo "Skipped. To register manually later, run:"
      echo "  claude mcp add -s user experience $BINARY -- --chromadb-port $CHROMADB_PORT --project-dir ."
      echo ""
      echo "Or add to a project's .mcp.json:"
      echo '  {'
      echo '    "mcpServers": {'
      echo '      "experience": {'
      echo '        "type": "stdio",'
      echo "        \"command\": \"$BINARY\","
      echo '        "args": ["--chromadb-port", "'"$CHROMADB_PORT"'", "--project-dir", "."]'
      echo '      }'
      echo '    }'
      echo '  }'
      ;;
  esac
else
  echo "Claude Code CLI not found. To register the MCP server later:"
  echo "  claude mcp add -s user experience $BINARY -- --chromadb-port $CHROMADB_PORT --project-dir ."
fi

echo ""
echo "=== Ready ==="
echo ""
echo "Start Claude Code inside a git repository to use the experience agent:"
echo "  cd /path/to/your/project"
echo "  git init   # if not already a git repo"
echo "  claude"
echo ""
echo "Commands:"
echo "  save experience          - save current work as an experience checkpoint"
echo "  search experience        - find relevant past experiences"
echo "  go back to ...           - revert to a previous experience"
echo "  show experience steps    - list all experience steps in this session"
echo "  commit experience        - squash experience micro-commits into one commit"
echo "  list experiences         - show all saved experiences"
echo "  delete experience <id>   - remove a saved experience"
