#!/usr/bin/env python3
"""Tests for org_roam_todo_mcp.py."""

from __future__ import annotations

import json
import sys
import tempfile
import threading
import unittest
from argparse import Namespace
from contextlib import redirect_stderr
from http.server import ThreadingHTTPServer
from io import StringIO
from pathlib import Path
from urllib import request as urlrequest

sys.path.insert(0, str(Path(__file__).resolve().parent))

import org_roam_todo_mcp as server


class OrgRoamTodoMcpTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        base = Path(self.tmp.name)
        self.cfg = server.Config.from_args(
            Namespace(
                config=None,
                org_base=str(base),
                issue_file=None,
                roam_dir=None,
                trash_dir=None,
                private_todo_files=["magic.org"],
                private_roam_dirs=["private"],
            )
        )

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def test_config_file_controls_paths_and_privacy(self) -> None:
        base = Path(self.tmp.name) / "configured"
        config_path = Path(self.tmp.name) / "mcp-config.json"
        config_path.write_text(
            json.dumps(
                {
                    "org_base": str(base),
                    "issue_file": str(base / "inbox.org"),
                    "roam_dir": str(base / "notes"),
                    "trash_dir": str(base / "trash"),
                    "private_todo_files": ["secret.org"],
                    "private_roam_dirs": ["hidden"],
                }
            ),
            encoding="utf-8",
        )

        cfg = server.Config.from_args(
            Namespace(
                config=str(config_path),
                org_base=None,
                issue_file=None,
                roam_dir=None,
                trash_dir=None,
                private_todo_files=None,
                private_roam_dirs=None,
            )
        )

        self.assertEqual(cfg.issue_file, base / "inbox.org")
        self.assertEqual(cfg.roam_dir, base / "notes")
        self.assertEqual(cfg.private_todo_files, {"secret.org"})
        self.assertEqual(cfg.private_roam_dirs, {"hidden"})

    def test_todo_crud(self) -> None:
        created = server.tool_todo_create(self.cfg, title="Write MCP design", body="Initial body")
        todo = created["todo"]
        self.assertEqual(todo["state"], "TODO")
        self.assertEqual(todo["title"], "Write MCP design")
        self.assertIsNone(todo["id"])
        self.assertNotIn("ref", todo)

        listed = server.tool_todo_list(self.cfg, include_done=False)
        self.assertEqual(len(listed["todos"]), 1)

        got = server.tool_todo_get(self.cfg, title="Write MCP design")
        self.assertIn("* TODO Write MCP design", got["todo"]["node"])
        self.assertIn("Initial body", got["todo"]["node"])

        updated = server.tool_todo_update(
            self.cfg,
            title="Write MCP design",
            node="* PROCESSING Write MCP implementation\nUpdated body\n",
        )
        self.assertEqual(updated["todo"]["state"], "PROCESSING")
        self.assertEqual(updated["todo"]["title"], "Write MCP implementation")

        deleted = server.tool_todo_delete(self.cfg, title="Write MCP implementation")
        self.assertTrue(deleted["deleted"])
        self.assertEqual(server.tool_todo_list(self.cfg)["todos"], [])
        self.assertTrue(Path(deleted["trash_file"]).exists())

    def test_todo_list_is_compact_by_default(self) -> None:
        for index in range(3):
            server.tool_todo_create(self.cfg, title=f"Task {index}")

        compact = server.tool_todo_list(self.cfg, limit=2)
        self.assertEqual(len(compact["todos"]), 2)
        self.assertEqual(compact["total"], 3)
        self.assertTrue(compact["truncated"])
        self.assertNotIn("file", compact["todos"][0])
        self.assertNotIn("level", compact["todos"][0])

        verbose = server.tool_todo_list(self.cfg, limit=1, verbose=True)
        self.assertIn("file", verbose["todos"][0])
        self.assertIn("level", verbose["todos"][0])

    def test_todo_list_defaults_to_active_items(self) -> None:
        for state in ["TODO", "PROCESSING", "REVIEWING", "DONE", "CANCEL", "LATER"]:
            server.tool_todo_create(self.cfg, title=f"{state} task", state=state)

        default = server.tool_todo_list(self.cfg, limit=10)
        self.assertEqual(
            [todo["state"] for todo in default["todos"]],
            ["TODO", "PROCESSING", "LATER"],
        )

        with_done = server.tool_todo_list(self.cfg, include_done=True, limit=10)
        self.assertEqual(
            [todo["state"] for todo in with_done["todos"]],
            ["TODO", "PROCESSING", "REVIEWING", "DONE", "CANCEL", "LATER"],
        )

        explicit_reviewing = server.tool_todo_list(self.cfg, state="REVIEWING", limit=10)
        self.assertEqual([todo["state"] for todo in explicit_reviewing["todos"]], ["REVIEWING"])

    def test_todo_list_supports_multiple_states_and_done_filter(self) -> None:
        for state in ["TODO", "PROCESSING", "REVIEWING", "DONE", "CANCEL", "LATER"]:
            server.tool_todo_create(self.cfg, title=f"{state} task", state=state)

        selected = server.tool_todo_list(self.cfg, states=["TODO", "PROCESSING"], limit=10)
        self.assertEqual([todo["state"] for todo in selected["todos"]], ["TODO", "PROCESSING"])

        active = server.tool_todo_list(self.cfg, include_done=False, limit=10)
        self.assertEqual(
            [todo["state"] for todo in active["todos"]],
            ["TODO", "PROCESSING", "LATER"],
        )

    def test_todo_list_active_only(self) -> None:
        for state in ["TODO", "PROCESSING", "REVIEWING", "DONE", "CANCEL", "LATER"]:
            server.tool_todo_create(self.cfg, title=f"{state} task", state=state)

        active = server.tool_todo_list(self.cfg, active_only=True, limit=10)
        self.assertEqual(
            [todo["state"] for todo in active["todos"]],
            ["TODO", "PROCESSING", "LATER"],
        )

    def test_todo_list_can_filter_planning_times(self) -> None:
        project = self.cfg.org_base / "project.org"
        project.write_text(
            "\n".join(
                [
                    "* TODO Scheduled task",
                    "SCHEDULED: <2026-06-01 Mon 10:30>",
                    "* TODO Deadline task",
                    "DEADLINE: <2026-06-02 Tue>",
                    "* TODO No time task",
                    "* DONE Done scheduled task",
                    "SCHEDULED: <2026-06-03 Wed>",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        any_time = server.tool_todo_list(self.cfg, with_time=True, limit=10)
        scheduled = server.tool_todo_list(self.cfg, with_time=True, time_types=["scheduled"], limit=10)
        deadline = server.tool_todo_list(self.cfg, with_time=True, time_types=["deadline"], limit=10)
        include_done = server.tool_todo_list(self.cfg, with_time=True, include_done=True, limit=10)

        self.assertEqual([todo["title"] for todo in any_time["todos"]], ["Scheduled task", "Deadline task"])
        self.assertEqual([todo["title"] for todo in scheduled["todos"]], ["Scheduled task"])
        self.assertEqual([todo["title"] for todo in deadline["todos"]], ["Deadline task"])
        self.assertEqual(
            [todo["title"] for todo in include_done["todos"]],
            ["Scheduled task", "Deadline task", "Done scheduled task"],
        )
        self.assertEqual(any_time["todos"][0]["times"][0]["type"], "scheduled")

    def test_todo_due_today(self) -> None:
        today_start, _ = server.day_range()
        yesterday = today_start - server.dt.timedelta(days=1)
        today = today_start + server.dt.timedelta(hours=9)
        tomorrow = today_start + server.dt.timedelta(days=1, hours=9)
        project = self.cfg.org_base / "project.org"
        project.write_text(
            "\n".join(
                [
                    "* TODO Overdue task",
                    f"DEADLINE: <{yesterday.strftime('%Y-%m-%d %a')}>",
                    "* PROCESSING Today scheduled task",
                    f"SCHEDULED: <{today.strftime('%Y-%m-%d %a %H:%M')}>",
                    "* TODO Future task",
                    f"SCHEDULED: <{tomorrow.strftime('%Y-%m-%d %a')}>",
                    "* DONE Done today task",
                    f"SCHEDULED: <{today.strftime('%Y-%m-%d %a')}>",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        with_overdue = server.tool_todo_due_today(self.cfg, include_overdue=True, limit=10)
        today_only = server.tool_todo_due_today(self.cfg, include_overdue=False, limit=10)

        self.assertEqual([todo["title"] for todo in with_overdue["todos"]], ["Overdue task", "Today scheduled task"])
        self.assertTrue(with_overdue["todos"][0]["overdue"])
        self.assertEqual(with_overdue["todos"][0]["due_times"][0]["type"], "deadline")
        self.assertEqual([todo["title"] for todo in today_only["todos"]], ["Today scheduled task"])

    def test_todo_query_state_name_filters_state_not_logbook(self) -> None:
        project = self.cfg.org_base / "project.org"
        project.parent.mkdir(parents=True, exist_ok=True)
        project.write_text(
            "\n".join(
                [
                    "* REVIEWING Reviewed task",
                    ":LOGBOOK:",
                    "- State \"REVIEWING\"  from \"PROCESSING\" [2026-05-28 Thu 10:00]",
                    ":END:",
                    "* PROCESSING Current task",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        by_query = server.tool_todo_list(self.cfg, query="PROCESSING", limit=10)
        self.assertEqual([todo["state"] for todo in by_query["todos"]], ["PROCESSING"])

        title_only = server.tool_todo_list(self.cfg, query="Reviewed", include_done=True, limit=10)
        self.assertEqual([todo["state"] for todo in title_only["todos"]], ["REVIEWING"])

        no_body_search = server.tool_todo_list(self.cfg, query="2026-05-28", limit=10)
        self.assertEqual(no_body_search["todos"], [])

        body_search = server.tool_todo_list(self.cfg, query="2026-05-28", search_body=True, include_done=True, limit=10)
        self.assertEqual([todo["state"] for todo in body_search["todos"]], ["REVIEWING"])

    def test_todo_list_does_not_write_missing_ids(self) -> None:
        legacy_file = self.cfg.org_base / "legacy.org"
        legacy_file.parent.mkdir(parents=True, exist_ok=True)
        original = "* TODO Legacy task without ID\nbody\n"
        legacy_file.write_text(original, encoding="utf-8")

        listed = server.tool_todo_list(self.cfg)

        self.assertEqual(legacy_file.read_text(encoding="utf-8"), original)
        self.assertEqual(len(listed["todos"]), 1)
        self.assertIsNone(listed["todos"][0]["id"])
        self.assertTrue(listed["todos"][0]["editable"])

    def test_todo_list_scans_only_top_level_org_files_and_strips_suffix(self) -> None:
        project_file = self.cfg.org_base / "project.org"
        nested_file = self.cfg.org_base / "daily" / "today.org"
        nested_file.parent.mkdir(parents=True, exist_ok=True)
        project_file.write_text("* TODO Top level task\n", encoding="utf-8")
        nested_file.write_text("* TODO Nested task\n", encoding="utf-8")

        listed = server.tool_todo_list(self.cfg)

        self.assertEqual([todo["title"] for todo in listed["todos"]], ["Top level task"])
        self.assertEqual(listed["todos"][0]["relative_file"], "project")

    def test_todo_queries_exclude_magic_file(self) -> None:
        project_file = self.cfg.org_base / "project.org"
        magic_file = self.cfg.org_base / "magic.org"
        last_start, _ = server.week_range(offset=-1)
        last_week = last_start + server.dt.timedelta(days=1, hours=9)
        project_file.write_text("* TODO Public task\n", encoding="utf-8")
        magic_file.write_text(
            "\n".join(
                [
                    "* TODO Private task",
                    "* DONE Private done last week",
                    f"CLOSED: [{last_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        listed = server.tool_todo_list(self.cfg, include_done=True)
        updated = server.tool_todo_updated_last_week(self.cfg)

        self.assertEqual([todo["title"] for todo in listed["todos"]], ["Public task"])
        self.assertEqual(updated["todos"], [])

    def test_todo_update_rejects_sibling_headings(self) -> None:
        created = server.tool_todo_create(self.cfg, title="One subtree")
        with self.assertRaises(server.ToolError):
            server.tool_todo_update(
                self.cfg,
                title=created["todo"]["title"],
                node="* TODO One subtree\n* TODO Accidental sibling\n",
            )

    def test_todo_update_can_rename_title(self) -> None:
        server.tool_todo_create(self.cfg, title="Old title", body="body")
        updated = server.tool_todo_update(
            self.cfg,
            title="Old title",
            node="* TODO New title\nbody\n",
        )
        self.assertEqual(updated["todo"]["title"], "New title")
        got = server.tool_todo_get(self.cfg, title="New title")
        self.assertIn("* TODO New title", got["todo"]["node"])
        with self.assertRaises(server.ToolError):
            server.tool_todo_get(self.cfg, title="Old title")

    def test_todo_update_multiple_items_keep_titles_stable(self) -> None:
        server.tool_todo_create(self.cfg, title="First")
        server.tool_todo_create(self.cfg, title="Second")
        server.tool_todo_create(self.cfg, title="Third")

        server.tool_todo_update(self.cfg, title="First", node="* DONE First\n")
        server.tool_todo_update(self.cfg, title="Second", node="* PROCESSING Second\n")
        server.tool_todo_update(self.cfg, title="Third", node="* DONE Third\n")

        listed = server.tool_todo_list(self.cfg, include_done=True, limit=10)
        by_title = {todo["title"]: todo["state"] for todo in listed["todos"]}
        self.assertEqual(
            by_title,
            {"First": "DONE", "Second": "PROCESSING", "Third": "DONE"},
        )

    def test_todo_create_rejects_duplicate_title(self) -> None:
        server.tool_todo_create(self.cfg, title="Unique")
        with self.assertRaises(server.ToolError):
            server.tool_todo_create(self.cfg, title="Unique")

    def test_todo_duplicate_title_errors_on_get_and_delete(self) -> None:
        project = self.cfg.org_base / "project.org"
        project.write_text(
            "* TODO Same title\n* TODO Same title\n",
            encoding="utf-8",
        )
        with self.assertRaises(server.ToolError):
            server.tool_todo_get(self.cfg, title="Same title")
        with self.assertRaises(server.ToolError):
            server.tool_todo_delete(self.cfg, title="Same title")

    def test_todo_unknown_title_errors(self) -> None:
        with self.assertRaises(server.ToolError):
            server.tool_todo_get(self.cfg, title="No such todo")
        with self.assertRaises(server.ToolError):
            server.tool_todo_delete(self.cfg, title="No such todo")
        with self.assertRaises(server.ToolError):
            server.tool_todo_update(self.cfg, title="No such todo", node="* TODO x\n")

    def test_todo_updated_last_week(self) -> None:
        last_start, _ = server.week_range(offset=-1)
        this_start, _ = server.week_range(offset=0)
        last_week = last_start + server.dt.timedelta(days=2, hours=9)
        this_week = this_start + server.dt.timedelta(hours=9)
        project = self.cfg.org_base / "project.org"
        project.write_text(
            "\n".join(
                [
                    "* DONE Finished last week",
                    f"CLOSED: [{last_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    "* PROCESSING Updated last week",
                    ":LOGBOOK:",
                    f"- State \"PROCESSING\" from \"TODO\"       [{last_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    ":END:",
                    "* DONE Finished this week",
                    f"CLOSED: [{this_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    "* REVIEWING Not closed",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        listed = server.tool_todo_updated_last_week(self.cfg, limit=10)

        self.assertEqual(listed["total"], 2)
        self.assertEqual({todo["title"] for todo in listed["todos"]}, {"Finished last week", "Updated last week"})
        self.assertTrue(all("updated_at" in todo for todo in listed["todos"]))

    def test_todo_closed_this_week(self) -> None:
        last_start, _ = server.week_range(offset=-1)
        this_start, _ = server.week_range(offset=0)
        last_week = last_start + server.dt.timedelta(days=2, hours=9)
        this_week = this_start + server.dt.timedelta(days=1, hours=10)
        project = self.cfg.org_base / "project.org"
        project.write_text(
            "\n".join(
                [
                    "* DONE Finished this week",
                    f"CLOSED: [{this_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    "* CANCEL Cancelled this week",
                    ":LOGBOOK:",
                    f"- State \"CANCEL\"     from \"TODO\"       [{this_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    ":END:",
                    "* DONE Finished last week",
                    f"CLOSED: [{last_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    "* REVIEWING Review this week",
                    f"CLOSED: [{this_week.strftime('%Y-%m-%d %a %H:%M')}]",
                    "",
                ]
            ),
            encoding="utf-8",
        )

        listed = server.tool_todo_closed_this_week(self.cfg, limit=10)

        self.assertEqual(listed["total"], 2)
        self.assertEqual({todo["title"] for todo in listed["todos"]}, {"Finished this week", "Cancelled this week"})
        self.assertTrue(all("closed_at" in todo for todo in listed["todos"]))

    def test_roam_crud(self) -> None:
        created = server.tool_roam_create(
            self.cfg,
            title="MCP note",
            content="A note body",
            tags=["mcp", "org"],
        )
        note = created["note"]
        self.assertEqual(note["title"], "MCP note")
        self.assertIn("A note body", note["content"])
        self.assertTrue(note["content"].startswith(":PROPERTIES:\n:ID:"))
        self.assertNotIn("#+ID:", note["content"])

        listed = server.tool_roam_list(self.cfg, query="body")
        self.assertEqual(len(listed["notes"]), 1)

        updated = server.tool_roam_update(
            self.cfg,
            id=note["id"],
            title="Updated MCP note",
            content="Replacement body",
        )
        self.assertEqual(updated["note"]["title"], "Updated MCP note")
        self.assertIn("Replacement body", updated["note"]["content"])

        deleted = server.tool_roam_delete(self.cfg, id=note["id"])
        self.assertTrue(deleted["deleted"])
        self.assertEqual(server.tool_roam_list(self.cfg)["notes"], [])
        self.assertTrue(Path(deleted["trash_file"]).exists())

    def test_roam_reads_legacy_keyword_id(self) -> None:
        legacy_id = "22222222-2222-4222-8222-222222222222"
        path = self.cfg.roam_dir / "legacy.org"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(
            f"#+title: Legacy note\n#+ID: {legacy_id}\n\nlegacy content\n",
            encoding="utf-8",
        )

        got = server.tool_roam_get(self.cfg, id=legacy_id)

        self.assertEqual(got["note"]["title"], "Legacy note")
        self.assertEqual(got["note"]["id"], legacy_id)

    def test_roam_list_is_compact_by_default(self) -> None:
        first = server.tool_roam_create(self.cfg, title="First note", content="one")
        second = server.tool_roam_create(self.cfg, title="Second note", content="two")
        self.assertNotEqual(first["note"]["id"], second["note"]["id"])

        compact = server.tool_roam_list(self.cfg, limit=1)
        self.assertEqual(len(compact["notes"]), 1)
        self.assertEqual(compact["total"], 2)
        self.assertTrue(compact["truncated"])
        self.assertNotIn("file", compact["notes"][0])
        self.assertNotIn("mtime", compact["notes"][0])

        verbose = server.tool_roam_list(self.cfg, limit=1, verbose=True)
        self.assertIn("file", verbose["notes"][0])
        self.assertIn("mtime", verbose["notes"][0])

    def test_roam_queries_exclude_private_directory(self) -> None:
        public = server.tool_roam_create(self.cfg, title="Public note", content="visible")
        private_dir = self.cfg.roam_dir / "private"
        private_dir.mkdir(parents=True, exist_ok=True)
        private_id = "11111111-1111-4111-8111-111111111111"
        (private_dir / "private-note.org").write_text(
            f":PROPERTIES:\n:ID:       {private_id}\n:END:\n#+title: Private note\n\nsecret\n",
            encoding="utf-8",
        )

        listed = server.tool_roam_list(self.cfg, query="note", limit=10)

        self.assertEqual([note["id"] for note in listed["notes"]], [public["note"]["id"]])
        with self.assertRaises(server.ToolError):
            server.tool_roam_get(self.cfg, id=private_id)

    def test_mcp_tools_call_shape(self) -> None:
        log = StringIO()
        with redirect_stderr(log):
            response = server.handle_request(
                self.cfg,
                {
                    "jsonrpc": "2.0",
                    "id": 1,
                    "method": "tools/call",
                    "params": {
                        "name": "todo_create",
                        "arguments": {"title": "Protocol test"},
                    },
                },
            )
        self.assertIsNotNone(response)
        assert response is not None
        self.assertEqual(response["id"], 1)
        payload = json.loads(response["result"]["content"][0]["text"])
        self.assertEqual(payload["todo"]["title"], "Protocol test")
        self.assertIn("tool=todo_create", log.getvalue())
        self.assertIn("Protocol test", log.getvalue())
        self.assertIn("rpc-detail method=tools/call", log.getvalue())

    def test_tools_list_logs_returned_tool_names(self) -> None:
        log = StringIO()
        with redirect_stderr(log):
            response = server.handle_request(
                self.cfg,
                {
                    "jsonrpc": "2.0",
                    "id": 1,
                    "method": "tools/list",
                    "params": {},
                },
            )
        self.assertIsNotNone(response)
        assert response is not None
        self.assertIn("tools/list count=", log.getvalue())
        self.assertIn("todo_list", log.getvalue())
        self.assertIn("roam_get", log.getvalue())

    def test_http_transport(self) -> None:
        httpd = ThreadingHTTPServer(("127.0.0.1", 0), server.make_http_handler(self.cfg))
        thread = threading.Thread(target=httpd.serve_forever, daemon=True)
        thread.start()
        self.addCleanup(httpd.server_close)
        self.addCleanup(httpd.shutdown)

        url = f"http://127.0.0.1:{httpd.server_port}/mcp"
        payload = json.dumps(
            {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {
                    "name": "todo_create",
                    "arguments": {"title": "HTTP transport test"},
                },
            }
        ).encode("utf-8")
        req = urlrequest.Request(url, data=payload, headers={"Content-Type": "application/json"})
        with urlrequest.urlopen(req, timeout=5) as resp:
            response = json.loads(resp.read().decode("utf-8"))

        result = json.loads(response["result"]["content"][0]["text"])
        self.assertEqual(result["todo"]["title"], "HTTP transport test")


if __name__ == "__main__":
    unittest.main()
