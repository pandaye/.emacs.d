#!/usr/bin/env python3
"""MCP server for Org TODOs and Org-roam notes.

The server intentionally uses only the Python standard library.  It speaks the
JSON-RPC based MCP stdio and HTTP transports, and edits plain Org files directly.
"""

from __future__ import annotations

import argparse
import datetime as dt
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
import re
import shutil
import sys
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable


TODO_STATES = {"TODO", "PROCESSING", "BLOCK", "LATER", "REVIEWING", "DONE", "CANCEL"}
DONE_STATES = {"REVIEWING", "DONE", "CANCEL"}
ACTIVE_STATES = TODO_STATES - DONE_STATES
CLOSED_STATES = {"DONE", "CANCEL"}
TIME_TYPES = {"scheduled", "deadline"}
SUPPORTED_PROTOCOL_VERSIONS = ["2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05"]


class ToolError(Exception):
    """User-facing MCP tool error."""


def default_config_path() -> Path:
    return Path(__file__).with_name("config.json")


def load_config_file(config_path: str | None, use_default: bool = True) -> dict[str, Any]:
    path_value = config_path or os.environ.get("ORG_MCP_CONFIG")
    if path_value:
        path = Path(path_value).expanduser()
    elif use_default:
        path = default_config_path()
    else:
        return {}
    if not path.exists():
        return {}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise ToolError(f"invalid config file: {path}") from exc
    if not isinstance(data, dict):
        raise ToolError(f"config file must contain a JSON object: {path}")
    return data


def config_value(args: argparse.Namespace, file_config: dict[str, Any], name: str, env_name: str, default: Any = None) -> Any:
    arg_value = getattr(args, name, None)
    if arg_value is not None:
        return arg_value
    if env_name in os.environ:
        return os.environ[env_name]
    if name in file_config:
        return file_config[name]
    return default


def split_env_list(value: str) -> list[str]:
    return [item.strip() for item in value.split(",") if item.strip()]


def config_list(args: argparse.Namespace, file_config: dict[str, Any], name: str, env_name: str) -> list[str]:
    arg_value = getattr(args, name, None)
    if arg_value:
        return list(arg_value)
    if env_name in os.environ:
        return split_env_list(os.environ[env_name])
    value = file_config.get(name, [])
    if isinstance(value, str):
        return split_env_list(value)
    if isinstance(value, list) and all(isinstance(item, str) for item in value):
        return value
    raise ToolError(f"config field {name} must be a string list")


@dataclass
class Config:
    org_base: Path
    issue_file: Path
    roam_dir: Path
    trash_dir: Path
    private_todo_files: set[str]
    private_roam_dirs: set[str]

    @classmethod
    def from_args(cls, args: argparse.Namespace) -> "Config":
        has_overrides = any(
            getattr(args, name, None)
            for name in (
                "org_base",
                "issue_file",
                "roam_dir",
                "trash_dir",
                "private_todo_files",
                "private_roam_dirs",
            )
        )
        file_config = load_config_file(getattr(args, "config", None), use_default=not has_overrides)
        org_base_value = config_value(args, file_config, "org_base", "ORG_BASE_PATH")
        if not org_base_value:
            raise ToolError("org_base is required. Set --org-base, ORG_BASE_PATH, or provide a config file.")
        org_base = Path(org_base_value).expanduser()
        issue_file = Path(config_value(args, file_config, "issue_file", "ORG_ISSUE_FILE", org_base / "issue.org")).expanduser()
        roam_dir = Path(config_value(args, file_config, "roam_dir", "ORG_ROAM_DIRECTORY", org_base / "roam")).expanduser()
        trash_dir = Path(config_value(args, file_config, "trash_dir", "ORG_MCP_TRASH_DIR", org_base / ".mcp-trash")).expanduser()
        private_todo_files = set(config_list(args, file_config, "private_todo_files", "ORG_MCP_PRIVATE_TODO_FILES"))
        private_roam_dirs = set(config_list(args, file_config, "private_roam_dirs", "ORG_MCP_PRIVATE_ROAM_DIRS"))
        return cls(
            org_base=org_base,
            issue_file=issue_file,
            roam_dir=roam_dir,
            trash_dir=trash_dir,
            private_todo_files=private_todo_files,
            private_roam_dirs=private_roam_dirs,
        )


@dataclass
class Heading:
    file: Path
    start: int
    end: int
    level: int
    state: str
    title: str
    id: str | None
    body: str


def utc_now_org() -> str:
    return dt.datetime.now().strftime("[%Y-%m-%d %a %H:%M]")


def start_of_week(now: dt.datetime | None = None) -> dt.datetime:
    current = now or dt.datetime.now()
    midnight = current.replace(hour=0, minute=0, second=0, microsecond=0)
    return midnight - dt.timedelta(days=midnight.weekday())


def week_range(offset: int = 0, now: dt.datetime | None = None) -> tuple[dt.datetime, dt.datetime]:
    start = start_of_week(now) + dt.timedelta(days=offset * 7)
    return start, start + dt.timedelta(days=7)


def datetime_in_range(value: dt.datetime | None, start: dt.datetime, end: dt.datetime) -> bool:
    return value is not None and start <= value < end


def day_range(offset: int = 0, now: dt.datetime | None = None) -> tuple[dt.datetime, dt.datetime]:
    current = now or dt.datetime.now()
    start = current.replace(hour=0, minute=0, second=0, microsecond=0) + dt.timedelta(days=offset)
    return start, start + dt.timedelta(days=1)


def slugify(title: str) -> str:
    slug = re.sub(r"[^0-9A-Za-z\u4e00-\u9fff]+", "-", title.strip().lower())
    slug = re.sub(r"-+", "-", slug).strip("-")
    return slug or "note"


def ensure_inside(path: Path, root: Path) -> Path:
    resolved = path.expanduser().resolve()
    root_resolved = root.expanduser().resolve()
    if resolved != root_resolved and root_resolved not in resolved.parents:
        raise ToolError(f"path outside configured root: {resolved}")
    return resolved


def read_text(path: Path) -> str:
    if not path.exists():
        return ""
    return path.read_text(encoding="utf-8")


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def org_files(cfg: Config) -> list[Path]:
    if not cfg.org_base.exists():
        return []
    return sorted(path for path in cfg.org_base.glob("*.org") if path.is_file() and path.name not in cfg.private_todo_files)


def is_private_roam_file(path: Path, cfg: Config) -> bool:
    try:
        relative = path.relative_to(cfg.roam_dir)
    except ValueError:
        return True
    return bool(relative.parts and relative.parts[0] in cfg.private_roam_dirs)


def public_roam_files(cfg: Config) -> list[Path]:
    if not cfg.roam_dir.exists():
        return []
    return sorted(path for path in cfg.roam_dir.rglob("*.org") if path.is_file() and not is_private_roam_file(path, cfg))


def todo_source_name(path: Path, cfg: Config) -> str:
    try:
        relative = path.relative_to(cfg.org_base)
    except ValueError:
        relative = path
    return str(relative.with_suffix(""))


def todo_ref(heading: Heading, cfg: Config) -> str:
    return f"{todo_source_name(heading.file, cfg)}:{heading.start}"


def parse_todo_ref(cfg: Config, ref: str) -> tuple[Path, int]:
    source, sep, offset_text = ref.rpartition(":")
    if not sep or not source:
        raise ToolError(f"invalid todo ref: {ref}")
    try:
        offset = int(offset_text)
    except ValueError as exc:
        raise ToolError(f"invalid todo ref offset: {ref}") from exc
    if offset < 0:
        raise ToolError(f"invalid todo ref offset: {ref}")
    path = cfg.org_base / f"{source}.org"
    ensure_inside(path, cfg.org_base)
    if path.parent != cfg.org_base:
        raise ToolError(f"todo ref must point to a top-level org file: {ref}")
    return path, offset


def parse_properties(block: str) -> dict[str, str]:
    props: dict[str, str] = {}
    match = re.search(r"(?ms)^[ \t]*:PROPERTIES:[ \t]*\n(.*?)^[ \t]*:END:[ \t]*$", block)
    if not match:
        return props
    for line in match.group(1).splitlines():
        prop = re.match(r"^[ \t]*:([^:]+):[ \t]*(.*)$", line)
        if prop:
            props[prop.group(1).upper()] = prop.group(2).strip()
    return props


def parse_org_datetime(value: str) -> dt.datetime | None:
    match = re.search(r"(\d{4})-(\d{2})-(\d{2})(?:[^\d\n]+(\d{1,2}):(\d{2}))?", value)
    if not match:
        return None
    hour = int(match.group(4) or 0)
    minute = int(match.group(5) or 0)
    return dt.datetime(int(match.group(1)), int(match.group(2)), int(match.group(3)), hour, minute)


def org_datetimes(text: str) -> list[dt.datetime]:
    values = []
    for match in re.finditer(r"[\[<](\d{4}-\d{2}-\d{2}[^\]>]*)[\]>]", text):
        parsed = parse_org_datetime(match.group(1))
        if parsed is not None:
            values.append(parsed)
    return values


def planning_times(heading: Heading, time_types: set[str] | None = None) -> list[dict[str, str]]:
    selected = time_types or TIME_TYPES
    times = []
    for match in re.finditer(r"(?m)\b(SCHEDULED|DEADLINE):[ \t]*([<\[][^>\]]+[>\]])", heading.body):
        kind = match.group(1).lower()
        if kind not in selected:
            continue
        parsed = parse_org_datetime(match.group(2))
        if parsed is None:
            continue
        times.append({"type": kind, "time": parsed.isoformat(timespec="minutes")})
    return times


def normalize_time_types(time_types: list[str] | None) -> set[str]:
    if not time_types:
        return set(TIME_TYPES)
    selected = {item.lower() for item in time_types}
    invalid = sorted(selected - TIME_TYPES)
    if invalid:
        raise ToolError(f"invalid time type(s): {', '.join(invalid)}")
    return selected


def last_state_change_time(heading: Heading) -> dt.datetime | None:
    latest = None
    todo_regexp = "|".join(re.escape(state) for state in sorted(TODO_STATES, key=len, reverse=True))
    for match in re.finditer(rf"(?m)^[ \t]*- State \"(?:{todo_regexp})\".*?(\[[^\]]+\])", heading.body):
        parsed = parse_org_datetime(match.group(1))
        if parsed is not None:
            latest = parsed
    if latest is not None:
        return latest
    closed_match = re.search(r"(?m)^[ \t]*CLOSED:[ \t]*(\[[^\]]+\])", heading.body)
    if closed_match:
        parsed = parse_org_datetime(closed_match.group(1))
        if parsed is not None:
            return parsed
    props = parse_properties(read_text(heading.file)[heading.start : heading.end])
    for key in ("CLOSED", "ARCHIVE_TIME"):
        parsed = parse_org_datetime(props.get(key, ""))
        if parsed is not None:
            return parsed
    return None


def closed_time(heading: Heading) -> dt.datetime | None:
    props = parse_properties(read_text(heading.file)[heading.start : heading.end])
    parsed = parse_org_datetime(props.get("CLOSED", ""))
    if parsed is not None:
        return parsed
    closed_match = re.search(r"(?m)^[ \t]*CLOSED:[ \t]*(\[[^\]]+\])", heading.body)
    if closed_match:
        parsed = parse_org_datetime(closed_match.group(1))
        if parsed is not None:
            return parsed
    done_regexp = "|".join(re.escape(state) for state in sorted(CLOSED_STATES, key=len, reverse=True))
    latest = None
    for match in re.finditer(rf"(?m)^[ \t]*- State \"(?:{done_regexp})\".*?(\[[^\]]+\])", heading.body):
        parsed = parse_org_datetime(match.group(1))
        if parsed is not None:
            latest = parsed
    if latest is not None:
        return latest
    return parse_org_datetime(props.get("ARCHIVE_TIME", ""))


def parse_todo_headings(path: Path) -> list[Heading]:
    text = read_text(path)
    matches = list(re.finditer(r"(?m)^(\*+)\s+([A-Z]+)\s+(.*)$", text))
    headings: list[Heading] = []
    for index, match in enumerate(matches):
        state = match.group(2)
        if state not in TODO_STATES:
            continue
        level = len(match.group(1))
        end = len(text)
        for later in matches[index + 1 :]:
            if len(later.group(1)) <= level:
                end = later.start()
                break
        block = text[match.start() : end]
        title = match.group(3).strip()
        props = parse_properties(block)
        body = block[match.end() - match.start() :].strip("\n")
        headings.append(
            Heading(
                file=path,
                start=match.start(),
                end=end,
                level=level,
                state=state,
                title=title,
                id=props.get("ID"),
                body=body,
            )
        )
    return headings


def heading_to_dict(
    heading: Heading,
    cfg: Config,
    include_body: bool = True,
    verbose: bool = True,
) -> dict[str, Any]:
    data: dict[str, Any] = {
        "ref": todo_ref(heading, cfg),
        "id": heading.id,
        "editable": True,
        "title": heading.title,
        "state": heading.state,
        "relative_file": todo_source_name(heading.file, cfg),
    }
    if verbose:
        data["file"] = str(heading.file)
        data["level"] = heading.level
    if include_body:
        data["body"] = heading.body
    return data


def all_todos(cfg: Config) -> list[Heading]:
    headings: list[Heading] = []
    for path in org_files(cfg):
        headings.extend(parse_todo_headings(path))
    return headings


def find_todo(cfg: Config, todo_id: str) -> Heading:
    for heading in all_todos(cfg):
        if heading.id == todo_id:
            return heading
    raise ToolError(f"todo not found: {todo_id}")


def find_todo_by_ref(cfg: Config, ref: str) -> Heading:
    path, offset = parse_todo_ref(cfg, ref)
    for heading in parse_todo_headings(path):
        if heading.start == offset:
            return heading
    raise ToolError(f"todo not found for ref: {ref}")


def normalize_todo_states(state: str | None = None, states: list[str] | None = None) -> set[str] | None:
    selected: set[str] = set()
    if state:
        selected.add(state)
    if states:
        selected.update(states)
    if not selected:
        return None
    invalid = sorted(selected - TODO_STATES)
    if invalid:
        raise ToolError(f"invalid state(s): {', '.join(invalid)}")
    return selected


def tool_todo_list(
    cfg: Config,
    state: str | None = None,
    states: list[str] | None = None,
    include_done: bool = False,
    active_only: bool = False,
    query: str | None = None,
    search_body: bool = False,
    with_time: bool = False,
    time_types: list[str] | None = None,
    limit: int = 20,
    verbose: bool = False,
) -> dict[str, Any]:
    query_text = query.strip() if query else None
    if query_text and not state and not states and query_text.upper() in TODO_STATES:
        state = query_text.upper()
        query_text = None
    selected_states = normalize_todo_states(state=state, states=states)
    selected_time_types = normalize_time_types(time_types)
    if limit < 1:
        raise ToolError("limit must be >= 1")
    rows = []
    for heading in all_todos(cfg):
        if selected_states and heading.state not in selected_states:
            continue
        if active_only and heading.state in DONE_STATES:
            continue
        if not selected_states and not include_done and heading.state in DONE_STATES:
            continue
        haystack = heading.title if not search_body else f"{heading.title}\n{heading.body}"
        if query_text and query_text.lower() not in haystack.lower():
            continue
        times = planning_times(heading, selected_time_types)
        if with_time and not times:
            continue
        row = heading_to_dict(heading, cfg, include_body=False, verbose=verbose)
        if times:
            row["times"] = times
        rows.append(row)
    return {"todos": rows[:limit], "limit": limit, "total": len(rows), "truncated": len(rows) > limit}


def tool_todo_due_today(cfg: Config, include_overdue: bool = True, limit: int = 20, verbose: bool = False) -> dict[str, Any]:
    if limit < 1:
        raise ToolError("limit must be >= 1")
    start, end = day_range()
    rows = []
    for heading in all_todos(cfg):
        if heading.state in DONE_STATES:
            continue
        due_times = []
        for item in planning_times(heading):
            parsed = parse_org_datetime(item["time"])
            if parsed is None:
                continue
            is_today = datetime_in_range(parsed, start, end)
            is_overdue = parsed < start
            if is_today or (include_overdue and is_overdue):
                due_times.append({**item, "overdue": is_overdue})
        if not due_times:
            continue
        row = heading_to_dict(heading, cfg, include_body=False, verbose=verbose)
        row["due_times"] = due_times
        row["overdue"] = any(item["overdue"] for item in due_times)
        rows.append(row)
    rows.sort(key=lambda item: (not item["overdue"], item["due_times"][0]["time"], item["title"]))
    return {
        "date": start.date().isoformat(),
        "todos": rows[:limit],
        "limit": limit,
        "total": len(rows),
        "truncated": len(rows) > limit,
    }


def tool_todo_get(cfg: Config, ref: str) -> dict[str, Any]:
    heading = find_todo_by_ref(cfg, ref)
    data = heading_to_dict(heading, cfg, include_body=False, verbose=True)
    data["node"] = read_text(heading.file)[heading.start : heading.end].rstrip("\n")
    return {"todo": data}


def tool_todo_updated_last_week(cfg: Config, limit: int = 50, verbose: bool = False) -> dict[str, Any]:
    if limit < 1:
        raise ToolError("limit must be >= 1")
    start, end = week_range(offset=-1)
    rows = []
    for heading in all_todos(cfg):
        updated_at = last_state_change_time(heading)
        if not datetime_in_range(updated_at, start, end):
            continue
        row = heading_to_dict(heading, cfg, include_body=False, verbose=verbose)
        row["updated_at"] = updated_at.isoformat(timespec="minutes") if updated_at else None
        rows.append(row)
    rows.sort(key=lambda item: item.get("updated_at") or "", reverse=True)
    return {
        "todos": rows[:limit],
        "limit": limit,
        "total": len(rows),
        "truncated": len(rows) > limit,
        "range": {"start": start.isoformat(), "end": end.isoformat()},
    }


def tool_todo_closed_this_week(cfg: Config, limit: int = 50, verbose: bool = False) -> dict[str, Any]:
    if limit < 1:
        raise ToolError("limit must be >= 1")
    start, end = week_range(offset=0)
    rows = []
    for heading in all_todos(cfg):
        if heading.state not in CLOSED_STATES:
            continue
        closed_at = closed_time(heading)
        if not datetime_in_range(closed_at, start, end):
            continue
        row = heading_to_dict(heading, cfg, include_body=False, verbose=verbose)
        row["closed_at"] = closed_at.isoformat(timespec="minutes") if closed_at else None
        rows.append(row)
    rows.sort(key=lambda item: item.get("closed_at") or "", reverse=True)
    return {
        "todos": rows[:limit],
        "limit": limit,
        "total": len(rows),
        "truncated": len(rows) > limit,
        "range": {"start": start.isoformat(), "end": end.isoformat()},
    }


def tool_todo_completed_last_week(cfg: Config, limit: int = 50, verbose: bool = False) -> dict[str, Any]:
    return tool_todo_updated_last_week(cfg, limit=limit, verbose=verbose)


def tool_todo_this_week(cfg: Config, limit: int = 50, verbose: bool = False) -> dict[str, Any]:
    return tool_todo_closed_this_week(cfg, limit=limit, verbose=verbose)


def tool_todo_create(cfg: Config, title: str, body: str = "", state: str = "TODO") -> dict[str, Any]:
    if state not in TODO_STATES:
        raise ToolError(f"invalid state: {state}")
    title = title.strip()
    if not title:
        raise ToolError("title is required")
    body = body.strip("\n")
    entry = f"* {state} {title}\n  {utc_now_org()}\n"
    if body:
        entry += f"{body}\n"
    existing = read_text(cfg.issue_file)
    prefix = "" if not existing or existing.endswith("\n") else "\n"
    write_text(cfg.issue_file, existing + prefix + entry + "\n")
    created = parse_todo_headings(cfg.issue_file)[-1]
    return {"todo": heading_to_dict(created, cfg)}


def replace_heading(heading: Heading, new_block: str) -> None:
    text = read_text(heading.file)
    write_text(heading.file, text[: heading.start] + new_block.rstrip("\n") + "\n" + text[heading.end :].lstrip("\n"))


def tool_todo_update(
    cfg: Config,
    ref: str,
    node: str,
) -> dict[str, Any]:
    heading = find_todo_by_ref(cfg, ref)
    node = node.strip("\n")
    if not node:
        raise ToolError("node cannot be empty")
    first_line = node.splitlines()[0]
    match = re.match(r"^(\*+)\s+([A-Z]+)\s+.+$", first_line)
    if not match:
        raise ToolError("node must start with an Org TODO heading")
    if match.group(2) not in TODO_STATES:
        raise ToolError(f"invalid state: {match.group(2)}")
    level = len(match.group(1))
    if level != heading.level:
        raise ToolError(f"node heading level must remain {heading.level}")
    for later in re.finditer(r"(?m)^(\*+)\s+", node):
        if later.start() == 0:
            continue
        if len(later.group(1)) <= level:
            raise ToolError("node must contain exactly one subtree; sibling headings are not allowed")
    replace_heading(heading, node)
    updated = parse_todo_headings(heading.file)
    for candidate in updated:
        if candidate.start == heading.start:
            return {"todo": heading_to_dict(candidate, cfg)}
    raise ToolError("updated todo moved; list todos again to get a fresh ref")


def trash_path(cfg: Config, path: Path) -> Path:
    stamp = dt.datetime.now().strftime("%Y%m%d%H%M%S")
    cfg.trash_dir.mkdir(parents=True, exist_ok=True)
    return cfg.trash_dir / f"{stamp}-{path.name}"


def tool_todo_delete(cfg: Config, ref: str) -> dict[str, Any]:
    heading = find_todo_by_ref(cfg, ref)
    text = read_text(heading.file)
    removed = text[heading.start : heading.end].strip("\n")
    write_text(heading.file, text[: heading.start] + text[heading.end :].lstrip("\n"))
    deleted_file = cfg.trash_dir / "deleted-todos.org"
    existing = read_text(deleted_file)
    write_text(deleted_file, existing + f"\n* Deleted todo {ref}\n#+deleted_at: {utc_now_org()}\n{removed}\n")
    return {"deleted": True, "ref": ref, "trash_file": str(deleted_file)}


def parse_keywords(text: str) -> dict[str, str]:
    keywords: dict[str, str] = {}
    for line in text.splitlines():
        match = re.match(r"^#\+([^:]+):\s*(.*)$", line, re.IGNORECASE)
        if match:
            keywords[match.group(1).upper()] = match.group(2).strip()
    return keywords


def roam_note_id(text: str) -> str | None:
    props = parse_properties(text)
    if props.get("ID"):
        return props["ID"]
    keywords = parse_keywords(text)
    return keywords.get("ID")


def roam_note_info(
    path: Path,
    cfg: Config,
    include_content: bool = False,
    verbose: bool = True,
) -> dict[str, Any]:
    text = read_text(path)
    keywords = parse_keywords(text)
    note_id = roam_note_id(text)
    data: dict[str, Any] = {
        "id": note_id,
        "title": keywords.get("TITLE") or path.stem,
        "relative_file": str(path.relative_to(cfg.roam_dir)) if cfg.roam_dir in path.parents else str(path),
    }
    if verbose:
        data["file"] = str(path)
        data["mtime"] = dt.datetime.fromtimestamp(path.stat().st_mtime).isoformat(timespec="seconds")
    if include_content:
        data["content"] = text
    return data


def all_roam_notes(cfg: Config) -> list[dict[str, Any]]:
    notes = [roam_note_info(path, cfg, verbose=True) for path in public_roam_files(cfg)]
    return [note for note in notes if note.get("id")]


def find_roam_path(cfg: Config, id: str) -> Path:
    for path in public_roam_files(cfg):
        if roam_note_info(path, cfg).get("id") == id:
            return path
    raise ToolError(f"roam note not found: {id}")


def tool_roam_list(cfg: Config, query: str | None = None, limit: int = 20, verbose: bool = False) -> dict[str, Any]:
    if limit < 1:
        raise ToolError("limit must be >= 1")
    rows = all_roam_notes(cfg)
    if query:
        query_lower = query.lower()
        rows = [row for row in rows if query_lower in row["title"].lower() or query_lower in read_text(Path(row["file"])).lower()]
    rows.sort(key=lambda row: row["mtime"], reverse=True)
    selected = rows[:limit]
    if not verbose:
        selected = [
            {
                "id": row["id"],
                "title": row["title"],
                "relative_file": row["relative_file"],
            }
            for row in selected
        ]
    return {"notes": selected, "limit": limit, "total": len(rows), "truncated": len(rows) > limit}


def tool_roam_get(cfg: Config, id: str) -> dict[str, Any]:
    return {"note": roam_note_info(find_roam_path(cfg, id), cfg, include_content=True)}


def tool_roam_create(cfg: Config, title: str, content: str = "", tags: list[str] | None = None) -> dict[str, Any]:
    title = title.strip()
    if not title:
        raise ToolError("title is required")
    note_id = str(uuid.uuid4())
    timestamp = dt.datetime.now().strftime("%Y%m%d%H%M%S")
    path = cfg.roam_dir / f"{timestamp}-{slugify(title)}.org"
    tag_line = f"#+filetags: {' '.join(':' + tag.strip(':') + ':' for tag in tags)}\n" if tags else ""
    text = f":PROPERTIES:\n:ID:       {note_id}\n:END:\n#+title: {title}\n#+date: {utc_now_org()}\n{tag_line}\n{content.strip()}\n"
    write_text(path, text)
    return {"note": roam_note_info(path, cfg, include_content=True)}


def tool_roam_update(cfg: Config, id: str, title: str | None = None, content: str | None = None) -> dict[str, Any]:
    path = find_roam_path(cfg, id)
    text = read_text(path)
    if title is not None:
        title = title.strip()
        if not title:
            raise ToolError("title cannot be empty")
        if re.search(r"(?im)^#\+title:", text):
            text = re.sub(r"(?im)^#\+title:.*$", f"#+title: {title}", text, count=1)
        else:
            text = f"#+title: {title}\n{text}"
    if content is not None:
        lines = text.splitlines()
        split_at = 0
        in_properties = False
        while split_at < len(lines):
            line = lines[split_at]
            stripped = line.strip()
            if stripped == ":PROPERTIES:":
                in_properties = True
                split_at += 1
                continue
            if in_properties:
                split_at += 1
                if stripped == ":END:":
                    in_properties = False
                continue
            if line.startswith("#+") or not stripped:
                split_at += 1
                continue
            break
        header = "\n".join(lines[:split_at]).rstrip()
        text = f"{header}\n\n{content.strip()}\n"
    write_text(path, text)
    return {"note": roam_note_info(path, cfg, include_content=True)}


def tool_roam_delete(cfg: Config, id: str) -> dict[str, Any]:
    path = find_roam_path(cfg, id)
    target = trash_path(cfg, path)
    shutil.move(str(path), str(target))
    return {"deleted": True, "id": id, "trash_file": str(target)}


def tool_specs() -> list[dict[str, Any]]:
    string = {"type": "string"}
    boolean = {"type": "boolean"}
    integer = {"type": "integer"}
    string_array = {"type": "array", "items": string}
    return [
        {"name": "todo_list", "description": "List active/open Org TODO headings by default. Default excludes REVIEWING, DONE, and CANCEL. Set include_done=true only when closed/reviewing items are explicitly requested. Use state or states for exact status filtering, for example state='PROCESSING' or states=['TODO','PROCESSING']. Set with_time=true to return only items with SCHEDULED or DEADLINE planning timestamps; use time_types=['scheduled'] or ['deadline'] to narrow it. query searches titles only unless search_body=true. Compact by default; set verbose=true for absolute paths and level.", "inputSchema": {"type": "object", "properties": {"state": string, "states": string_array, "include_done": boolean, "active_only": boolean, "query": string, "search_body": boolean, "with_time": boolean, "time_types": string_array, "limit": integer, "verbose": boolean}}},
        {"name": "todo_get", "description": "Get a complete Org TODO subtree by ref. Use the ref returned by todo_list.", "inputSchema": {"type": "object", "properties": {"ref": string}, "required": ["ref"]}},
        {"name": "todo_due_today", "description": "List active TODO items scheduled or due today. Includes overdue scheduled/deadline items by default; set include_overdue=false for today only. Intended for daily reminder jobs.", "inputSchema": {"type": "object", "properties": {"include_overdue": boolean, "limit": integer, "verbose": boolean}}},
        {"name": "todo_updated_last_week", "description": "List TODO items whose latest state change happened during last week. Includes all current states. Weeks start on Monday.", "inputSchema": {"type": "object", "properties": {"limit": integer, "verbose": boolean}}},
        {"name": "todo_closed_this_week", "description": "List DONE/CANCEL TODO items closed during this week. Weeks start on Monday.", "inputSchema": {"type": "object", "properties": {"limit": integer, "verbose": boolean}}},
        {"name": "todo_create", "description": "Create a TODO in the issue inbox. Does not add an ID property.", "inputSchema": {"type": "object", "properties": {"title": string, "body": string, "state": string}, "required": ["title"]}},
        {"name": "todo_update", "description": "Replace a complete Org TODO subtree by ref. Call todo_get first, edit the returned node text, then submit the whole node.", "inputSchema": {"type": "object", "properties": {"ref": string, "node": string}, "required": ["ref", "node"]}},
        {"name": "todo_delete", "description": "Delete a TODO subtree by ref, preserving it in trash. Use the ref returned by todo_list.", "inputSchema": {"type": "object", "properties": {"ref": string}, "required": ["ref"]}},
        {"name": "roam_list", "description": "List Org-roam notes. Compact by default; set verbose=true for absolute paths and mtime.", "inputSchema": {"type": "object", "properties": {"query": string, "limit": integer, "verbose": boolean}}},
        {"name": "roam_get", "description": "Get one Org-roam note by ID.", "inputSchema": {"type": "object", "properties": {"id": string}, "required": ["id"]}},
        {"name": "roam_create", "description": "Create an Org-roam file-level note.", "inputSchema": {"type": "object", "properties": {"title": string, "content": string, "tags": {"type": "array", "items": string}}, "required": ["title"]}},
        {"name": "roam_update", "description": "Update title and/or content of an Org-roam note by ID.", "inputSchema": {"type": "object", "properties": {"id": string, "title": string, "content": string}, "required": ["id"]}},
        {"name": "roam_delete", "description": "Move an Org-roam note to trash by ID.", "inputSchema": {"type": "object", "properties": {"id": string}, "required": ["id"]}},
    ]


def dispatch_tool(cfg: Config, name: str, args: dict[str, Any]) -> dict[str, Any]:
    tools: dict[str, Callable[..., dict[str, Any]]] = {
        "todo_list": lambda **kw: tool_todo_list(cfg, **kw),
        "todo_get": lambda **kw: tool_todo_get(cfg, **kw),
        "todo_due_today": lambda **kw: tool_todo_due_today(cfg, **kw),
        "todo_updated_last_week": lambda **kw: tool_todo_updated_last_week(cfg, **kw),
        "todo_closed_this_week": lambda **kw: tool_todo_closed_this_week(cfg, **kw),
        "todo_completed_last_week": lambda **kw: tool_todo_completed_last_week(cfg, **kw),
        "todo_this_week": lambda **kw: tool_todo_this_week(cfg, **kw),
        "todo_create": lambda **kw: tool_todo_create(cfg, **kw),
        "todo_update": lambda **kw: tool_todo_update(cfg, **kw),
        "todo_delete": lambda **kw: tool_todo_delete(cfg, **kw),
        "roam_list": lambda **kw: tool_roam_list(cfg, **kw),
        "roam_get": lambda **kw: tool_roam_get(cfg, **kw),
        "roam_create": lambda **kw: tool_roam_create(cfg, **kw),
        "roam_update": lambda **kw: tool_roam_update(cfg, **kw),
        "roam_delete": lambda **kw: tool_roam_delete(cfg, **kw),
    }
    if name not in tools:
        raise ToolError(f"unknown tool: {name}")
    return tools[name](**args)


def result_summary(result: dict[str, Any]) -> str:
    if "todos" in result:
        states: dict[str, int] = {}
        for todo in result["todos"]:
            state = todo.get("state", "?")
            states[state] = states.get(state, 0) + 1
        return f"todos={len(result['todos'])} total={result.get('total')} states={states}"
    if "notes" in result:
        return f"notes={len(result['notes'])} total={result.get('total')}"
    if "todo" in result:
        todo = result["todo"]
        return f"todo id={todo.get('id')} state={todo.get('state')}"
    if "note" in result:
        note = result["note"]
        return f"note id={note.get('id')} title={note.get('title')!r}"
    return ",".join(sorted(result.keys()))


def log_json(value: Any, max_chars: int = 4000) -> str:
    text = json.dumps(value, ensure_ascii=False, sort_keys=True)
    if len(text) > max_chars:
        return text[:max_chars] + f"...<truncated {len(text) - max_chars} chars>"
    return text


def log_tool_call(name: str, args: dict[str, Any], result: dict[str, Any] | None = None, error: Exception | None = None) -> None:
    timestamp = dt.datetime.now().isoformat(timespec="seconds")
    args_text = log_json(args)
    if error is not None:
        print(f"[{timestamp}] tool={name} args={args_text} error={type(error).__name__}: {error}", file=sys.stderr, flush=True)
        return
    summary = result_summary(result or {})
    print(f"[{timestamp}] tool={name} args={args_text} result={summary}", file=sys.stderr, flush=True)


def log_rpc(method: str | None, request_id: Any = None) -> None:
    timestamp = dt.datetime.now().isoformat(timespec="seconds")
    print(f"[{timestamp}] rpc method={method} id={request_id}", file=sys.stderr, flush=True)


def log_rpc_detail(request: dict[str, Any]) -> None:
    timestamp = dt.datetime.now().isoformat(timespec="seconds")
    method = request.get("method")
    request_id = request.get("id")
    params = request.get("params")
    print(f"[{timestamp}] rpc-detail method={method} id={request_id} params={log_json(params)}", file=sys.stderr, flush=True)


def log_http_request(peer: str, path: str, request: dict[str, Any]) -> None:
    timestamp = dt.datetime.now().isoformat(timespec="seconds")
    print(f"[{timestamp}] http peer={peer} path={path} body={log_json(request)}", file=sys.stderr, flush=True)


def log_tools_list(tools: list[dict[str, Any]]) -> None:
    timestamp = dt.datetime.now().isoformat(timespec="seconds")
    names = [tool.get("name") for tool in tools]
    print(f"[{timestamp}] tools/list count={len(tools)} names={names}", file=sys.stderr, flush=True)


def make_tool_content(payload: dict[str, Any]) -> dict[str, Any]:
    return {"content": [{"type": "text", "text": json.dumps(payload, ensure_ascii=False, indent=2)}]}


def handle_request(cfg: Config, request: dict[str, Any]) -> dict[str, Any] | None:
    method = request.get("method")
    request_id = request.get("id")
    log_rpc(method, request_id)
    log_rpc_detail(request)
    try:
        if method == "initialize":
            requested_version = (request.get("params") or {}).get("protocolVersion")
            protocol_version = requested_version if requested_version in SUPPORTED_PROTOCOL_VERSIONS else SUPPORTED_PROTOCOL_VERSIONS[0]
            result = {
                "protocolVersion": protocol_version,
                "capabilities": {"tools": {}},
                "serverInfo": {"name": "org-roam-todo-mcp", "version": "0.1.0"},
            }
        elif method == "notifications/initialized":
            return None
        elif method == "tools/list":
            tools = tool_specs()
            log_tools_list(tools)
            result = {"tools": tools}
        elif method == "tools/call":
            params = request.get("params") or {}
            tool_name = params.get("name")
            tool_args = params.get("arguments") or {}
            try:
                tool_result = dispatch_tool(cfg, tool_name, tool_args)
                log_tool_call(tool_name, tool_args, result=tool_result)
            except Exception as exc:
                log_tool_call(tool_name, tool_args, error=exc)
                raise
            result = make_tool_content(tool_result)
        else:
            raise ToolError(f"unsupported method: {method}")
        return {"jsonrpc": "2.0", "id": request_id, "result": result}
    except Exception as exc:
        code = -32000 if isinstance(exc, ToolError) else -32603
        return {"jsonrpc": "2.0", "id": request_id, "error": {"code": code, "message": str(exc)}}


def serve_stdio(cfg: Config) -> None:
    for line in sys.stdin:
        if not line.strip():
            continue
        response = handle_request(cfg, json.loads(line))
        if response is not None:
            print(json.dumps(response, ensure_ascii=False), flush=True)


def make_http_handler(cfg: Config) -> type[BaseHTTPRequestHandler]:
    class OrgRoamTodoMcpHandler(BaseHTTPRequestHandler):
        server_version = "OrgRoamTodoMCP/0.1.0"

        def log_message(self, format: str, *args: Any) -> None:
            print(f"{self.address_string()} - {format % args}", file=sys.stderr)

        def send_json(self, status: HTTPStatus, payload: dict[str, Any]) -> None:
            data = json.dumps(payload, ensure_ascii=False).encode("utf-8")
            self.send_response(status)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_GET(self) -> None:
            if self.path == "/health":
                self.send_json(HTTPStatus.OK, {"ok": True, "server": "org-roam-todo-mcp"})
                return
            self.send_json(HTTPStatus.NOT_FOUND, {"error": "not found"})

        def do_POST(self) -> None:
            if self.path != "/mcp":
                self.send_json(HTTPStatus.NOT_FOUND, {"error": "not found"})
                return
            try:
                length = int(self.headers.get("Content-Length", "0"))
                request = json.loads(self.rfile.read(length).decode("utf-8"))
                log_http_request(self.client_address[0], self.path, request)
                response = handle_request(cfg, request)
            except Exception as exc:
                response = {"jsonrpc": "2.0", "id": None, "error": {"code": -32700, "message": str(exc)}}
            if response is None:
                response = {"jsonrpc": "2.0", "result": None}
            self.send_json(HTTPStatus.OK, response)

    return OrgRoamTodoMcpHandler


def serve_http(cfg: Config, host: str, port: int) -> None:
    server = ThreadingHTTPServer((host, port), make_http_handler(cfg))
    print(f"org-roam-todo-mcp listening on http://{host}:{port}/mcp", file=sys.stderr, flush=True)
    server.serve_forever()


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="MCP server for Org TODOs and Org-roam notes")
    parser.add_argument("--config", help="JSON config file, default: ORG_MCP_CONFIG or mcp/config.json")
    parser.add_argument("--transport", choices=("stdio", "http"), default="stdio", help="Transport to use, default: stdio")
    parser.add_argument("--host", default="127.0.0.1", help="HTTP bind host, default: 127.0.0.1")
    parser.add_argument("--port", type=int, default=8765, help="HTTP bind port, default: 8765")
    parser.add_argument("--org-base", help="Org base directory")
    parser.add_argument("--issue-file", help="TODO inbox file, default: ORG_ISSUE_FILE or <org-base>/issue.org")
    parser.add_argument("--roam-dir", help="Org-roam directory, default: ORG_ROAM_DIRECTORY or <org-base>/roam")
    parser.add_argument("--trash-dir", help="Trash directory for deleted items, default: ORG_MCP_TRASH_DIR or <org-base>/.mcp-trash")
    parser.add_argument("--private-todo-files", action="append", help="Top-level Org file to exclude from TODO tools; repeatable")
    parser.add_argument("--private-roam-dirs", action="append", help="Top-level Org-roam subdirectory to exclude; repeatable")
    return parser


def main() -> int:
    args = build_parser().parse_args()
    cfg = Config.from_args(args)
    if args.transport == "http":
        serve_http(cfg, args.host, args.port)
    else:
        serve_stdio(cfg)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
