# org-backend

GraphQL backend for Org-mode data.

## Development

- `nix develop` to enter the dev shell
- `stack build` to build
- `stack test` to run tests
- `stack run` to start the server

## Configuration

- `ORG_BACKEND_ORG_DIR`: directory containing `.org` files. Defaults to `org/`.

## GraphQL queries

- `hello`: basic health check
- `parseOrg(text: String!)`: parse an Org document from input text
- `orgFile(path: String!)`: parse a file from `ORG_BACKEND_ORG_DIR` (path must be relative, no `..`)
- `orgFiles`: list all `.org` files under `ORG_BACKEND_ORG_DIR` (recursive, hidden paths skipped)

Errors are surfaced as GraphQL errors with the prefix `ORG_BACKEND:`.

### Example

```graphql
query {
  orgFile(path: "notes.org") {
    headlines {
      title
      properties { key value }
    }
  }
}
```
