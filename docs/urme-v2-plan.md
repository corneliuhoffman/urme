# Urme V2 — Design

Drop ChromaDB + Ollama embeddings. Move to SQLite (FTS5) + a graph layer over
OCamlGraph. Keep the existing `git_walk` / `diff_match` / `edit_extract` engine.

## Claude access — hard constraint

**All Claude calls go through the `claude` CLI as a subprocess.** No
`ANTHROPIC_API_KEY`, no direct API client, no SDK. We piggyback on the user's
Claude Max subscription.

Use the existing pattern in `lib/claude/process.ml`:
- `spawn_oneshot` for one-shot prompts (summarise a turn batch, dedupe, rewrite a query, rerank a shortlist).
- Persistent daemon (`claude --print --input-format stream-json --output-format stream-json`) if we ever need a single multi-turn reasoning stream.

Every "Claude call" below means: spawn the CLI with a prompt, collect the
`Result` stream event, parse JSON out of it. No network client in our process.

## 1. Schema

SQLite tables:

- `steps` — one row per prompt→response turn, with structured metadata
  (session_id, turn_index, timestamp, prompt_text, files_touched, commands_run,
  tokens_in/out, commit_before, commit_after, summary, tags)
- `steps_fts` — FTS5 virtual table over `summary`, `tags`, `prompt_text`
  (porter stemmer, external-content linked to `steps`)
- `entities` — named concepts, files, decisions, bugs, patterns
  (id, kind, name, aliases, description, created_at)
- `relationships` — edges between entities, with `step_id` provenance
  (src_entity_id, dst_entity_id, kind, step_id, weight, created_at)
- `sessions` — metadata about each Claude Code session
  (id, started_at, jsonl_path, turn_count, last_indexed_at)
- `meta` — single-row table for `graph_version`, schema version, etc.

Human edits from git_walk are stored as `steps` with `session_id = NULL`.

WAL mode on from day one.

## 2. Indexing Pipeline

When a session is processed:

1. Parse stream-json → extract turns.
2. For each turn:
   - Extract structured metadata deterministically (files, commands, tokens, timestamps).
   - Correlate with git walk (commit_before/after, human edits).
   - Spawn `claude` CLI one-shot: generate summary + tags + extract
     entities/relationships. Batch 5–10 consecutive turns per spawn to amortise
     startup cost.
   - Deduplicate entities: pre-filter locally (trigram / name-alias similarity,
     top 30 candidates), then spawn `claude` CLI to merge.
   - Write step to SQLite + FTS5.
   - Write new/merged entities and relationships to graph tables.
3. Human edits from git walk → stored as steps with `session_id = NULL`.
   Summarisation deferred (see §7).
4. Bump `meta.graph_version` after each session commit.

## 3. Search Pipeline

1. User enters natural language query in the TUI.
2. Load graph from SQLite into an in-memory `Graph.t`. Cache it; invalidate
   when `meta.graph_version` bumps.
3. Local FTS5 first: tokenize the query, run `MATCH` directly. No `claude` spawn.
4. If initial hits are poor (count < threshold, or low relevance), spawn
   `claude` CLI to rewrite — expect structured JSON:
   ```json
   { "queries": ["taint propagator ocaml", "taint analysis dataflow"],
     "repo": "opengrep", "after": null, "has_commit": null }
   ```
   then re-run FTS5 with the alternatives.
5. Entity resolution: if the query mentions known entities, join their
   step provenance into the result set.
6. Graph traversal: for matched steps, fetch related entities via OCamlGraph
   (1–2 hops).
7. Spawn `claude` CLI once more to re-rank shortlist + synthesise. Keep the
   prompt compact — shortlist is already ≤ ~20 rows.
8. Write-back (gated): if Claude flags a new connection with confidence ≥
   threshold, write the edge.
9. Display in TUI.

## 4. OCaml Module Structure

This is a branch on the existing codebase — new modules land in the existing
libs, and ChromaDB code is removed as it's replaced.

- `lib/store/schema.ml` — SQLite schema, migrations, WAL setup *(new)*
- `lib/store/db.ml` — thin sqlite3 open/prepare/bind helpers *(new)*
- `lib/search/fts.ml` — FTS5 query construction, tokenization *(new)*
- `lib/search/search.ml` — search pipeline orchestration *(new, replaces `chromadb.ml` over time)*
- `lib/search/graph.ml` — OCamlGraph wrapper, load/traverse, version cache *(new)*
- `lib/engine/indexer.ml` — turn → steps row, git correlation glue *(new; reuses `git_walk`, `edit_extract`, `diff_match`)*
- `lib/claude/prompts.ml` — wrappers over `Process.spawn_oneshot` for
  summarise_batch / dedupe_entities / rewrite_query / rerank / propose_edges *(new)*
- `lib/tui/` — adapt search view to new pipeline; no new module
- `lib/search/chromadb.ml` — deleted once `search.ml` is the default

Reuse as-is: `engine/git_walk.ml`, `engine/diff_match.ml`,
`engine/edit_extract.ml`, `claude/process.ml`.

## 5. Claude Call Inventory (all via `claude` CLI subprocess)

| When                  | Purpose                                     | Size   |
|-----------------------|---------------------------------------------|--------|
| Index, per turn-batch | Summary + tags + entity extraction          | Medium |
| Index, per session    | Entity deduplication against graph          | Small  |
| Search (on miss)      | FTS5 query rewrite                          | Tiny   |
| Search                | Re-rank shortlist + synthesis               | Medium |
| Search (gated)        | Write back new edges                        | Tiny   |

## 6. Build Order

1. Schema + migrations (+ WAL).
2. Stream-json parser + git correlation — mostly exists, wire into new schema.
3. `claude` CLI summarisation + FTS5 indexing.
4. Basic search — FTS5 only, no graph yet.
5. Export / import utility (makes reset-and-replay trivial for later work).
6. Entity extraction + graph storage.
7. Graph-aware search + OCamlGraph traversal.
8. `claude` CLI re-ranking + write-back.
9. TUI polish.

## 7. Deferred

- Multi-machine merge.
- Daemon mode.
- Human-edit summarisation (stored but not summarised initially — only
  searchable by file/commit metadata until later).
- Migration from existing ChromaDB data. Clean reset; reindex from JSONL.
