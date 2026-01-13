# Repository Guidelines

## General guidelines
- Break work down into atomic steps.
- Follow TDD/BDD strictly (red-green-refactor). Write the minimal failing test, then minimal passing code, then refactor.
- Commit after each atomic step; keep commits small and sequential.

## Project Structure & Module Organization
- `app/` holds the executable entry point (GraphQL server).
- `src/` contains library modules (Org parser, storage, GraphQL schema/resolvers).
- `test/` is for automated tests.
- `org/` is the live data directory containing `.org` files (read/write at runtime).
- `flake.nix` and `shell.nix` define the Nix dev environments.
- `plan.md` documents the project roadmap and open questions.

## Build, Test, and Development Commands
- `nix develop`: enter the flake-based dev shell (GHC, cabal, tools). Use this shell to run project commands so `stack` is available.
- `nix-shell`: enter the legacy Nix shell with the same toolchain.
- `stack build`: build the library and server (from within `nix develop`).
- `stack run`: start the GraphQL API locally (from within `nix develop`).
- `stack test`: run the test suite (from within `nix develop`).

## Coding Style & Naming Conventions
- Language: Haskell.
- Indentation: 2 spaces; align `where`/`let` blocks consistently.
- Module names: `Org.*`, `Api.*`, `Storage.*` (e.g., `Org.Parser`).
- Avoid non-ASCII in source unless required by org content.
- Formatting: use `ormolu` (`ormolu -m inplace src/ app/ test/`).
- Linting: use `hlint` for quick feedback.

## Testing Guidelines
- Framework: Hspec (expected).
- Naming: `Spec` modules (e.g., `Org.ParserSpec`) under `test/`.
- Focus on parser correctness, headline IDs, and GraphQL resolver behavior.
- Run tests with `stack test`; keep fixtures in `test/fixtures/` if added.

## Current GraphQL surface
- Queries: `hello`, `parseOrg(text)`, `orgFile(path)`, `orgFiles(...)`.
- `orgFiles` supports: `recursive`, `includeHidden`, `prefix`, `offset`, `limit`, `sort` (NAME|MTIME), `sortDirection` (ASC|DESC), `filterTags`, `filterTodo`, and returns `{ total, items }`.
- Headline properties are exposed via `propertiesJson` (JSON string).
- Mutations: `writeOrgFile(path, content)`, `deleteOrgFile(path)`, `updateHeadlineTitle(path, id, title)`, `updateHeadlineTodo(path, id, todo)`, `updateHeadlineTags(path, id, tags)`, `updateHeadlineScheduled(path, id, scheduled)`, `updateHeadlineProperties(path, id, properties)`, `insertHeadlineAfter(path, afterId, title)`, `deleteHeadline(path, id)`.
  - HTTP server (warp) injects `Authorization` header into the request body before GraphQL execution.

## Commit & Pull Request Guidelines
- Use clear, imperative summaries (e.g., “Add GraphQL schema for headlines”).
- PRs should include: scope summary, testing performed, and any config changes.

## Security & Configuration Tips
- Prefer environment variables for auth tokens (e.g., `ORG_BACKEND_TOKEN`).
- When `ORG_BACKEND_TOKEN` is set, every GraphQL request must include an auth token (Bearer or raw token in the JSON body).
- Keep `org/` data local; avoid committing sensitive files.
