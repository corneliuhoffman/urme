# Experience Agent

OCaml MCP server that adds experience replay to Claude Code.

## Build

```
make setup   # install OCaml dependencies
make build   # build and copy binary to bin/experience-agent
make clean   # remove build artifacts
```

## Quick Start

```
./scripts/start.sh   # check prerequisites, install if needed, start services
./scripts/stop.sh    # stop Ollama and ChromaDB
```

`start.sh` will:
1. Check for Ollama and ChromaDB, offer to install if missing (supports macOS/apt/dnf/pacman)
2. Pull the `nomic-embed-text` embedding model
3. Start both services in the background
4. Offer to register the MCP server globally in Claude Code

## Manual Setup

### 1. Start Ollama and ChromaDB

Both must be running before using the experience agent:

```
ollama serve &
chroma run --port 8000 &
```

On macOS with Apple M5, prefix Ollama with:
```
GGML_METAL_TENSOR_DISABLE=1 ollama serve &
```

### 2. Add MCP Server to Claude Code

The binary is at `bin/experience-agent` after building.

**Option A: Global via CLI (recommended)**

```
claude mcp add -s user experience /path/to/experience-agent/bin/experience-agent -- --chromadb-port 8000 --project-dir .
```

This writes to `~/.claude.json` and works in all projects.

**Option B: Per-project via `.mcp.json`**

Create a `.mcp.json` file in the root of a specific project:
```json
{
  "mcpServers": {
    "experience": {
      "type": "stdio",
      "command": "/path/to/experience-agent/bin/experience-agent",
      "args": ["--chromadb-port", "8000", "--project-dir", "."]
    }
  }
}
```

**Option C: Project-scoped via CLI**

```
claude mcp add -s project experience /path/to/experience-agent/bin/experience-agent -- --chromadb-port 8000 --project-dir .
```

This writes to the project's `.mcp.json`.

After adding, restart Claude Code (or start a new session). Verify with `/mcp` to see the server status — all 7 tools should appear.

## Usage

Once running, Claude Code gains experience memory — it can save, search, and replay past work across sessions.

User commands:

| Command | What it does |
|---|---|
| `save experience` | Save current work as an experience checkpoint (git micro-commit + vector DB) |
| `search experience` | Find relevant past experiences to inform the current task |
| `go back to ...` | Revert to a previous experience by description |
| `show experience steps` | List all experience steps in the current session |
| `commit experience` | Squash all experience micro-commits into one clean commit |
| `list experiences` | Show all saved experiences for the project |
| `delete experience <id>` | Remove a saved experience by ID |

### CLAUDE.md Setup

Add this to the `CLAUDE.md` of any project where you want experience memory:

```
You have access to an experience memory system via MCP tools (experience server).

When working on tasks, search for relevant past experiences using
mcp__experience__search_experience to draw on previous solutions and patterns.

When the user says "save experience":
- Call mcp__experience__save_step with:
  - label: short description of what was accomplished
  - intent: the original user request
  - conversation: the FULL conversation transcript since the last save point (or session start).
    Include all user messages, your reasoning, tool calls with inputs/outputs, and the outcome.

When the user says "search experience":
- Call mcp__experience__search_experience with the user's query to find relevant past experiences
- Use the results to inform your approach

When the user says "go back to ..." / "undo to ..." / "revert to ...":
- Call mcp__experience__go_back with the user's description as the query
- Show the matched experience and ask for confirmation
- Call go_back again with confirm=true to apply

When the user says "show experience steps":
- Call mcp__experience__show_steps

When the user says "commit experience":
- Call mcp__experience__commit with a commit message

When the user says "list experiences":
- Call mcp__experience__list_experiences

When the user says "delete experience <id>":
- Call mcp__experience__delete_experience with the given ID
```
