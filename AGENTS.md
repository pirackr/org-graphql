# Repository Guidelines

## General guidelines 
- Break it down to atomic level of detailed ## General guidelines 
- Follow TDD/BDD strictly, red-green-refactor cycle. Write minimal test just failed and minimal code just passed only - don't try to write test to cover everything from beginning.

- Follow TDD/BDD strictly, red-green-refactor cycle. Write minimal test just failed and minimal code just passed only - don't try to write test to cover everything from beginning.

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

## Commit & Pull Request Guidelines
- No commit history yet, so no established convention.
- Use clear, imperative summaries (e.g., “Add GraphQL schema for headlines”).
- PRs should include: scope summary, testing performed, and any config changes.

## Security & Configuration Tips
- Prefer environment variables for auth tokens (e.g., `ORG_BACKEND_TOKEN`).
- Keep `org/` data local; avoid committing sensitive files.
