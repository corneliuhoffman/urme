# Experience Agent

OCaml MCP server that gives Claude Code persistent experience memory — save, search, replay, and undo past work across sessions.

## How It Works

Every time you save a step, the agent:
1. Creates a **git micro-commit** capturing the current file state
2. Computes the **diff** and list of changed files
3. Stores the full **conversation transcript**, intent, and metadata in ChromaDB (vector DB)
4. Indexes by both intent and conversation for **three-stage semantic search**

This means Claude can later search for similar past work, replay step-by-step solutions, or semantically undo to a previous state.

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

On macOS with Apple Silicon, prefix Ollama with:
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

After adding, restart Claude Code (or start a new session). Verify with `/mcp` to see the server status — all 11 tools should appear.

## Usage

Once running, Claude Code gains experience memory — it can save, search, and replay past work across sessions.

### Manual Mode

Save individual steps on demand:

| Command | What it does |
|---|---|
| `save experience` | Save current work as a checkpoint (git micro-commit + vector DB) |
| `search experience` | Find relevant past experiences to inform the current task |
| `go back to ...` | Semantic undo — revert to a previous state by description |
| `show experience steps` | List all steps in the current session |
| `commit experience` | Squash all micro-commits into one clean commit |
| `list experiences` | Show all saved experiences for the project |
| `delete experience <id>` | Remove a saved experience by ID |
| `replay experience ...` | Replay a past experience step-by-step with full conversation data |

### Auto-Recording Mode

Auto-recording tracks every conversation turn as a step automatically. This is useful for capturing a full workflow end-to-end.

| Command | What it does |
|---|---|
| `start experience` | Enter auto-recording mode |
| *(work normally)* | Each turn is saved as a step automatically |
| `no` / reject | Roll back the last step if it wasn't useful |
| `save experience` | Finalize the group — squashes micro-commits, saves as a searchable experience group |

In auto-recording mode:
- After each conversation turn, Claude calls `save_step` with the full transcript
- If you say **yes** or continue working, the step is kept
- If you say **no** or express disapproval, Claude calls `reject_step` to undo the last step (rolls back git + removes from vector DB)
- When you're done, say `save experience` to finalize — this squashes all micro-commits into one clean commit and saves the group as a single searchable entity
- Auto-recording stays on after saving, so you can start another group immediately

### Replay

Search for a past experience and get back the full step-by-step conversation data:

```
replay experience "adding authentication to the API"
```

This returns either:
- An **experience group** (from auto-recording) with all individual steps and their conversations in order
- A **single experience** with its full conversation transcript

Use this to re-follow a previous solution or adapt it to a new context.

### Semantic Undo

`go back` uses semantic search to find the state you want to revert to:

```
go back to before the auth changes
go back to when the tests were passing
go back to step 3
```

It searches both the current session's steps and past saved experiences. You'll see the match and confirm before it applies the rollback.

## CLAUDE.md Setup

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

When the user says "start experience":
- Call mcp__experience__start_experiences to enter auto-recording mode
- After each conversation turn, call mcp__experience__save_step with the full transcript
- If the user rejects (says "no", expresses disapproval), call mcp__experience__reject_step
- When the user says "save experience" during auto-recording, call mcp__experience__save_experience
  with a label and intent to finalize the group

When the user says "replay experience ...":
- Call mcp__experience__replay_experience with the user's description
- Present the returned steps and conversations for reference
```

## Author

Corneliu Hoffman, 2026
