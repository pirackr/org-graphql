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
- `PORT`: port for the HTTP server (default: 8080).

## GraphQL queries

- `hello`: basic health check
- `parseOrg(text: String!)`: parse an Org document from input text
- `orgFile(path: String!)`: parse a file from `ORG_BACKEND_ORG_DIR` (path must be relative, no `..`)
- `orgFiles(recursive: Boolean = true, includeHidden: Boolean = false, prefix: String, offset: Int, limit: Int, sort: NAME | MTIME, sortDirection: ASC | DESC, filterTags: [String!], filterTodo: String)`: list `.org` files under `ORG_BACKEND_ORG_DIR` with optional filtering/pagination; returns `{ total, items }`

Errors are surfaced as GraphQL errors with the prefix `ORG_BACKEND:`.

## HTTP usage

Send POST requests to `http://localhost:8080/` with a JSON body containing `query` (and optional `variables`).
If `ORG_BACKEND_TOKEN` is set, pass `Authorization: Bearer <token>` and the server will inject it into the GraphQL request body.

## GraphQL mutations

- `writeOrgFile(path: String!, content: String!): Boolean`: write raw Org content to `ORG_BACKEND_ORG_DIR` (path must be relative, no `..`).
- `deleteOrgFile(path: String!): Boolean`: delete a file under `ORG_BACKEND_ORG_DIR` (path must be relative, no `..`).
- `updateHeadlineTitle(path: String!, id: String!, title: String!): Boolean`: update a headline title by id and rewrite the file.
- `updateHeadlineTodo(path: String!, id: String!, todo: String!): Boolean`: update a headline TODO keyword by id and rewrite the file.
- `updateHeadlineTags(path: String!, id: String!, tags: [String!]!): Boolean`: update headline tags by id and rewrite the file.
- `updateHeadlineScheduled(path: String!, id: String!, scheduled: String!): Boolean`: update a headline scheduled timestamp by id and rewrite the file.
- `updateHeadlineProperties(path: String!, id: String!, properties: [PropertyInput!]!): Boolean`: replace a headline's properties drawer by id and rewrite the file.
- `insertHeadlineAfter(path: String!, afterId: String!, title: String!): Boolean`: insert a sibling headline after the target id.
- `deleteHeadline(path: String!, id: String!): Boolean`: delete a headline (and its subtree) by id.

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

```graphql
mutation {
  updateHeadlineTodo(path: "notes.org", id: "hello", todo: "DONE")
}
```

```graphql
mutation {
  updateHeadlineTags(path: "notes.org", id: "hello", tags: ["work", "later"])
}
```

```graphql
mutation {
  updateHeadlineScheduled(path: "notes.org", id: "hello", scheduled: "2024-01-02 Tue")
}
```

```graphql
mutation {
  updateHeadlineProperties(
    path: "notes.org",
    id: "hello",
    properties: [{name: "A", value: "1"}, {name: "Z", value: "9"}]
  )
}
```

```graphql
mutation {
  insertHeadlineAfter(path: "notes.org", afterId: "hello", title: "Inserted")
}
```

```graphql
mutation {
  deleteHeadline(path: "notes.org", id: "hello")
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
