# Plan

Build a Haskell GraphQL service that reads org-mode files directly from `org/` on each request, exposes queries for files/headlines/details, and adds a small auth gate plus minimal mutations for live modification. Use Nix dev shells (flake + shell) and a Stack-based project setup to keep builds reproducible.

## Scope
- In: Stack/Nix project scaffold, org file parsing (headlines/todo/tags/scheduled/properties), GraphQL schema/resolvers, light auth, and basic tests/docs.
- Out: Full-text search/indexing, caching, frontend UI, advanced Org features, and deployment/CI/CD.

## Action items
[ ] Add Stack project files (`stack.yaml`, package/cabal config, `app/Main.hs`, `src/`), wiring to the Nix dev shell.
[ ] Define core data types for org files and headlines (id, level, title, todo, tags, scheduled, body, children).
[ ] Implement live file reader + parser over `org/`, with deterministic ids and error handling for malformed entries.
[ ] Design GraphQL schema for queries (files, headlines, headline) and mutations for edits (create/update/delete headline or file content).
[ ] Add a light auth layer (e.g., Bearer token via `ORG_BACKEND_TOKEN`) and enforce it on mutations (and optionally all requests).
[ ] Add basic tests for parsing and GraphQL resolvers plus a simple README with setup and example queries.
[ ] Run tests/build via Stack and document expected commands.

## Open questions
- What mutations are required: only create/update/delete headlines, or full raw file edits?
- Preferred auth behavior: required for all requests or only for mutations?
- Should headline ids be stable across edits (e.g., via generated property) or computed from path/line?
