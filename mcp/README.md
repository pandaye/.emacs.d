# Org Roam Todo MCP

This directory contains a small MCP server for managing Org TODOs and
Org-roam notes.  It supports both stdio and HTTP JSON-RPC transports.

## Server

Run with stdio:

```sh
python3 org_roam_todo_mcp.py
```

Run as an HTTP service:

```sh
python3 org_roam_todo_mcp.py \
  --transport http \
  --host 0.0.0.0 \
  --port 8765
```

HTTP endpoints:

- `POST /mcp`: JSON-RPC MCP requests.
- `GET /health`: health check.

Example HTTP call:

```sh
curl -s http://127.0.0.1:8765/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

Default configuration is read from `mcp/config.json` next to the server script:

Copy the example and customize it:

```sh
cp mcp/config.json.example mcp/config.json
```

Example content:

```json
{
  "org_base": "~/.org-journal",
  "issue_file": "~/.org-journal/issue.org",
  "roam_dir": "~/.org-journal/roam",
  "trash_dir": "~/.org-journal/.mcp-trash",
  "private_todo_files": ["magic.org"],
  "private_roam_dirs": ["private"]
}
```

Use another config file with:

```sh
python3 org_roam_todo_mcp.py \
  --config /path/to/org-roam-todo-mcp.json
```

Individual fields can still be overridden by command-line flags or environment
variables:

- `ORG_MCP_CONFIG`
- `ORG_BASE_PATH`
- `ORG_ISSUE_FILE`
- `ORG_ROAM_DIRECTORY`
- `ORG_MCP_TRASH_DIR`
- `ORG_MCP_PRIVATE_TODO_FILES` as a comma-separated list
- `ORG_MCP_PRIVATE_ROAM_DIRS` as a comma-separated list

## Tools

- `todo_list`: list top-level Org TODO headings under the Org base directory.
  Compact by default, with `limit=20`; supports `state`, `states`,
  `include_done`, `query`, `with_time`, `time_types`, `limit`, and `verbose`.
  Set `with_time=true` to return only items with `SCHEDULED` or `DEADLINE`;
  narrow with `time_types=["scheduled"]` or `["deadline"]`.  Returns a `ref`
  for each TODO; no TODO `ID` is required.
- `todo_get`: read one complete Org TODO subtree by `ref`.
- `todo_due_today`: list active TODO items scheduled or due today for reminder
  jobs.  Includes overdue `SCHEDULED`/`DEADLINE` items by default; set
  `include_overdue=false` for today only.
- `todo_updated_last_week`: list top-level TODO items whose latest state change
  happened during last week.  Weeks start on Monday.
- `todo_closed_this_week`: list top-level `DONE`/`CANCEL` TODO items closed
  during this week.  Weeks start on Monday.
- `todo_create`: append a TODO to the issue inbox without adding an `ID`.
- `todo_update`: replace one complete Org TODO subtree by `ref`.  Call
  `todo_get`, edit the returned `node`, then submit the whole `node`.
- `todo_delete`: remove a TODO subtree by `ref` and save a copy under trash.
- `roam_list`: list Org-roam file-level notes.  Compact by default, with
  `limit=20`; supports `query`, `limit`, and `verbose`.
- `roam_get`: read a note by `#+ID`.
- `roam_create`: create an Org-roam file-level note with UUID `#+ID`.
- `roam_update`: update note title and/or body by `#+ID`.
- `roam_delete`: move a note file to trash.

## Notes

List tools intentionally return compact data by default to keep LLM input token
usage small.  Use `verbose=true` only when the client needs absolute file paths,
heading levels, or mtimes.  Use `query`, `state`, and smaller `limit` values
before calling detail tools such as `todo_get` or `roam_get`.

TODO operations use a positional `ref` such as `project:97` rather than Org
`ID` properties.  If a file is edited heavily between `todo_list`/`todo_get` and
`todo_update`, list again to get a fresh `ref`.  Org-roam operations still use
file-level `#+ID`.

The server edits plain Org files directly.  It does not require Emacs to be
running and does not update `org-roam.db`; Emacs' existing
`org-roam-db-autosync-mode` will refresh the database when files are opened or
changed.  Run `org-roam-db-sync` manually if a client needs the database updated
immediately after MCP writes.

For LAN access, bind to `0.0.0.0` and configure the remote MCP client, such as
Hermes, to use `http://<this-host-ip>:8765/mcp`.  Restrict this port to trusted
hosts because the tools can modify and delete Org files.
