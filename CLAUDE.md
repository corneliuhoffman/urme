# Experience Agent

OCaml MCP server that adds experience replay to Claude Code.

## Build

```
opam install yojson cohttp-lwt-unix lwt uuidm -y
dune build
```

## Setup

### 1. Start Ollama and ChromaDB

Both must be running before using the experience agent:

```
GGML_METAL_TENSOR_DISABLE=1 ollama serve & chroma run --port 8000 &
```

`GGML_METAL_TENSOR_DISABLE=1` is required for Apple M5 chips (Metal tensor ops crash without it).

Install if needed:
```
brew install ollama pipx
ollama pull nomic-embed-text
pipx ensurepath
pipx install chromadb
```

### 2. Add MCP Server to Claude Code

The binary is at `_build/default/bin/main.exe` after building.

**Option A: Global via CLI (recommended)**

```
claude mcp add -s user experience /path/to/experience-agent/_build/default/bin/main.exe -- --chromadb-port 8000 --project-dir .
```

This writes to `~/.claude.json` and works in all projects.

**Option B: Per-project via `.mcp.json`**

Create a `.mcp.json` file in the root of a specific project:
```json
{
  "mcpServers": {
    "experience": {
      "type": "stdio",
      "command": "/path/to/experience-agent/_build/default/bin/main.exe",
      "args": ["--chromadb-port", "8000", "--project-dir", "."]
    }
  }
}
```

**Option C: Project-scoped via CLI**

```
claude mcp add -s project experience /path/to/experience-agent/_build/default/bin/main.exe -- --chromadb-port 8000 --project-dir .
```

This writes to the project's `.mcp.json`.

After adding, restart Claude Code (or start a new session). Verify with `/mcp` to see the server status — all 7 tools should appear.

## System Prompt for Claude Code

Add this to the CLAUDE.md of any project using the experience agent:

```
You have access to an experience memory system via MCP tools (experience server).

At the START of every task:
- Call mcp__experience__search_experience with the user's request to find relevant past work
- If results are found, use them to inform your approach

When the user says "save" / "save this":
- Call mcp__experience__save_step with:
  - label: short description of what was accomplished
  - intent: the original user request
  - conversation: the FULL conversation transcript since the last save point (or session start). Include all user messages, your reasoning, tool calls with inputs/outputs, and the outcome.

When the user says "go back" / "undo to ..." / "revert to when ...":
- Call mcp__experience__go_back with the user's description as the query
- Show the user the match found and ask for confirmation
- Call go_back again with confirm=true to apply

When the user says "steps" / "show steps":
- Call mcp__experience__show_steps

When the user says "commit":
- Call mcp__experience__commit with a commit message

Other commands:
- "list experiences" → mcp__experience__list_experiences
- "delete experience <id>" → mcp__experience__delete_experience
```
