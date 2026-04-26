# urme

OCaml TUI + MCP server for linking git history to Claude Code conversations.
Browse commits, see which Claude edits explain each diff, detect human
modifications, and search past interactions.

## How It Works

urme V2 is a single-binary tool backed by a local SQLite store
(`.urme/db.sqlite` at the project root). There are no external services —
no ChromaDB, no Ollama, no vector embeddings.

1. **Indexing**: reads the Claude Code session logs under
   `~/.claude/projects/<encoded-project-path>/*.jsonl` and writes one
   `steps` row per turn, with deterministic metadata (files touched,
   commands run, tokens, git commit_before/after).
2. **Summarisation**: runs the `claude` CLI (Haiku 4.5, one spawn per
   batch of 8 turns) to produce a one-sentence summary + 3–8 tags for
   each step. Written back into the step row; FTS5 indexes them
   automatically.
3. **Git linkage**: runs the branch-aware `Git_walk` algorithm against
   the Edit/Write tool_use history and populates the `edit_links` table
   with per-edit → commit linkage. Human edits (content in a commit
   that no Claude edit explains) are detected by reconciliation.
4. **Search**: FTS5 + BM25 over `summary`, `tags`, `prompt_text`.
   Optional `--smart` mode has Claude rewrite sparse queries and
   rerank the shortlist with a one-sentence synthesis answer.

Claude access goes exclusively through the `claude` CLI subprocess — no
`ANTHROPIC_API_KEY`, no direct API calls. Uses your Max subscription.

## Build

```
make setup   # install OCaml dependencies
make build   # build + copy binary to bin/experience-agent
make clean   # remove build artifacts
```

Dependencies: `ocaml >= 4.14`, `opam`. See `scripts/setup.sh` for the
full package list.

## First run

In any project directory:

```
bin/experience-agent --project-dir . init
```

This:
1. Creates `.urme/db.sqlite`.
2. Indexes every Claude Code session JSONL under the project's
   `~/.claude/projects/` path.
3. Builds per-edit git linkage (runs the walker).
4. Runs the Haiku summarisation pass on every turn (~seconds per batch
   of 8 turns; costs against your Max subscription, not tokens billed
   to a key).

Add `--skip-summaries` to defer the LLM pass.

## TUI

```
bin/experience-agent --project-dir .
```

Modes:
- **History** — list sessions, browse turns, search (FTS5 BM25).
- **Git** — browse branches/commits/files with per-file Claude-edit
  attribution and human-edit highlighting.

## CLI search

```
bin/experience-agent search "why did tests fail on new files"
bin/experience-agent search "how did we detect human edits" --smart
```

`--smart` adds Claude query rewrite (on sparse hits) and rerank.
Adds latency; useful when lexical search returns nothing relevant.

## Export / import

```
bin/experience-agent export /tmp/backup.sqlite
bin/experience-agent import /tmp/backup.sqlite --project-dir /other/repo --force
```

Uses SQLite's `VACUUM INTO` under the hood — produces a standard
`.sqlite` file anyone can open. WAL-state-safe; can run alongside the
summariser.

## MCP server

```
bin/experience-agent serve --project-dir .
```

Speaks JSON-RPC 2.0 over stdio. Six tools:
`search_history`, `file_history`, `region_blame`, `explain_change`,
`commit_links`, `search_by_file`.

Register with Claude Code:

```
claude mcp add -s user experience /path/to/bin/experience-agent -- --project-dir .
```

Or per-project `.mcp.json`:
```json
{
  "mcpServers": {
    "experience": {
      "type": "stdio",
      "command": "/path/to/bin/experience-agent",
      "args": ["--project-dir", ".", "serve"]
    }
  }
}
```

## Author

Corneliu Hoffman, 2026
