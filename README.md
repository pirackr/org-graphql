# org-backend

GraphQL backend for Org-mode data.

## Development

- `nix develop` to enter the dev shell
- `stack build` to build
- `stack test` to run tests
- `stack run` to start the server

## Configuration

- `ORG_BACKEND_ORG_DIR`: directory containing `.org` files. Defaults to `org/`.
- `ORG_BACKEND_TOKEN`: if set, every GraphQL request must include an auth token.

## GraphQL queries

- `hello`: basic health check
- `parseOrg(text: String!)`: parse an Org document from input text
- `orgFile(path: String!)`: parse a file from `ORG_BACKEND_ORG_DIR` (path must be relative, no `..`)
- `orgFiles(recursive: Boolean = true, includeHidden: Boolean = false, prefix: String, offset: Int, limit: Int, sort: NAME | MTIME, sortDirection: ASC | DESC, filterTags: [String!], filterTodo: String)`: list `.org` files under `ORG_BACKEND_ORG_DIR` with optional filtering/pagination; returns `{ total, items }`

Errors are surfaced as GraphQL errors with the prefix `ORG_BACKEND:`.

## GraphQL mutations

- `writeOrgFile(path: String!, content: String!): Boolean`: write raw Org content to `ORG_BACKEND_ORG_DIR` (path must be relative, no `..`).
- `deleteOrgFile(path: String!): Boolean`: delete a file under `ORG_BACKEND_ORG_DIR` (path must be relative, no `..`).
- `updateHeadlineTitle(path: String!, id: String!, title: String!): Boolean`: update a headline title by id and rewrite the file.

### Examples

```graphql
query {
  orgFile(path: "notes.org") {
    headlines {
      title
      propertiesJson
    }
  }
}
```

```graphql
query {
  parseOrg(text: "* TODO Hello :work:\n:PROPERTIES:\n:ID: 123\n:END:\n") {
    headlines { title todo tags propertiesJson }
  }
}
```

```graphql
query {
  orgFiles(recursive: false, includeHidden: true) {
    total
    items
  }
}
```

```graphql
query {
  orgFiles(prefix: "subdir/", offset: 0, limit: 10) {
    total
    items
  }
}
```

```graphql
query {
  orgFiles(prefix: "subdir", sort: MTIME, sortDirection: DESC) {
    total
    items
  }
}
```

```graphql
query {
  orgFiles(prefix: "tagged.org", filterTags: ["work"], filterTodo: "TODO") {
    total
    items
  }
}
```

```graphql
mutation {
  writeOrgFile(path: "notes.org", content: "* Hello\n")
}
```

```graphql
mutation {
  deleteOrgFile(path: "notes.org")
}
```

```graphql
mutation {
  updateHeadlineTitle(path: "notes.org", id: "hello", title: "Updated")
}
```

Notes:
- `prefix: "subdir"` matches files under `subdir/`.
- `sort: MTIME` orders newest first by default, `sortDirection` can flip it.
- `filterTags`/`filterTodo` skip files that fail to parse.
- When `ORG_BACKEND_TOKEN` is set, include `"authorization": "Bearer <token>"` (or the raw token) in the JSON request body, or `"extensions": { "authorization": "Bearer <token>" }`.

Example authenticated request payload:

```json
{
  "authorization": "Bearer my-token",
  "query": "{ hello }"
}
```
