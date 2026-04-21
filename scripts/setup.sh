#!/bin/bash
set -e

echo "=== urme Setup ==="

command -v git >/dev/null 2>&1 || { echo "Error: git not found"; exit 1; }
command -v opam >/dev/null 2>&1 || { echo "Error: opam not found"; exit 1; }

echo "Installing OCaml dependencies..."
opam install --yes \
  yojson cohttp-lwt-unix lwt uuidm irmin-git cmdliner \
  notty patch domainslib conf-libev sqlite3 ocamlgraph

echo "Building urme..."
cd "$(dirname "$0")/.."
dune build

BINARY_PATH="$(pwd)/_build/default/bin/main.exe"
echo ""
echo "=== Setup complete ==="
echo ""
echo "Binary: $BINARY_PATH"
echo ""
echo "First-time index in a project:"
echo "  $BINARY_PATH --project-dir /path/to/project init"
echo ""
echo "Then launch the TUI:"
echo "  $BINARY_PATH --project-dir /path/to/project"
