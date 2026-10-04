"""Tests for gathering Quick Switcher candidates from a mindspace."""
# pylint: disable=missing-class-docstring, missing-function-docstring

import os

import pytest

from context.context_info import ContextInfo
from desktop.quick_switcher.quick_switcher_entries import build_quick_switcher_entries


def make_context(context_id: str, context_type: str, path: str, title: str) -> ContextInfo:
    return ContextInfo(
        context_id=context_id,
        context_type=context_type,
        path=path,
        title=title,
        is_modified=False,
    )


@pytest.fixture
def mindspace(tmp_path):
    """A mindspace tree with a couple of files and one stored conversation."""
    root = tmp_path / "ms"
    conversations = root / ".humbug" / "conversations"
    conversations.mkdir(parents=True)

    (root / "main.py").write_text("x = 1", encoding="utf-8")
    (root / "notes.md").write_text("notes", encoding="utf-8")
    (conversations / "plan.conv").write_text("{}", encoding="utf-8")

    return root


def gather(mindspace, contexts=None, max_files=100):
    return build_quick_switcher_entries(
        contexts=contexts or [],
        mindspace_path=str(mindspace),
        conversations_dir=str(mindspace / ".humbug" / "conversations"),
        max_files=max_files,
    )


class TestGathering:
    def test_files_and_conversations_are_listed(self, mindspace):
        entries, truncated = gather(mindspace)

        titles = {entry.title for entry in entries}
        assert titles == {"main.py", "notes.md", "plan"}
        assert not truncated

    def test_ignored_directories_are_skipped(self, mindspace):
        (mindspace / "node_modules").mkdir()
        (mindspace / "node_modules" / "junk.js").write_text("junk", encoding="utf-8")
        (mindspace / ".git").mkdir()
        (mindspace / ".git" / "config").write_text("cfg", encoding="utf-8")

        entries, _truncated = gather(mindspace)

        titles = {entry.title for entry in entries}
        assert "junk.js" not in titles
        assert "config" not in titles

    def test_conversations_are_not_reached_by_the_file_walk(self, mindspace):
        entries, _truncated = gather(mindspace)

        conversation_entries = [entry for entry in entries if entry.title == "plan"]
        assert len(conversation_entries) == 1
        assert conversation_entries[0].kind == "conversation"


class TestOpenTabDeduplication:
    def test_an_open_file_is_listed_once_as_a_tab(self, mindspace):
        open_file = str(mindspace / "main.py")
        contexts = [make_context("c1", "editor", open_file, "main.py")]

        entries, _truncated = gather(mindspace, contexts)

        matching = [entry for entry in entries if entry.title == "main.py"]
        assert len(matching) == 1
        assert matching[0].kind == "tab"
        assert matching[0].entry_id == "tab:c1"

    def test_an_open_conversation_is_listed_once_as_a_tab(self, mindspace):
        open_conversation = str(mindspace / ".humbug" / "conversations" / "plan.conv")
        contexts = [make_context("c2", "conversation", open_conversation, "plan")]

        entries, _truncated = gather(mindspace, contexts)

        matching = [entry for entry in entries if entry.title == "plan"]
        assert len(matching) == 1
        assert matching[0].kind == "tab"

    def test_unopened_files_are_still_listed(self, mindspace):
        contexts = [make_context("c1", "editor", str(mindspace / "main.py"), "main.py")]

        entries, _truncated = gather(mindspace, contexts)

        assert {entry.title for entry in entries} == {"main.py", "notes.md", "plan"}

    def test_pathless_contexts_are_skipped(self, mindspace):
        contexts = [make_context("c3", "terminal", "", "Terminal")]

        entries, _truncated = gather(mindspace, contexts)

        assert all(entry.kind != "tab" for entry in entries)


class TestFileLimit:
    def test_walk_stops_at_the_limit_and_reports_truncation(self, tmp_path):
        root = tmp_path / "big"
        (root / ".humbug" / "conversations").mkdir(parents=True)
        for i in range(25):
            (root / f"file{i}.txt").write_text(str(i), encoding="utf-8")

        entries, truncated = gather(root, max_files=10)

        file_entries = [entry for entry in entries if entry.kind == "file"]
        assert len(file_entries) == 10
        assert truncated

    def test_no_truncation_when_under_the_limit(self, mindspace):
        _entries, truncated = gather(mindspace, max_files=100)
        assert not truncated

    def test_open_tabs_survive_the_file_limit(self, tmp_path):
        root = tmp_path / "big"
        (root / ".humbug" / "conversations").mkdir(parents=True)
        for i in range(25):
            (root / f"file{i}.txt").write_text(str(i), encoding="utf-8")

        open_file = str(root / "file24.txt")
        contexts = [make_context("c1", "editor", open_file, "file24.txt")]

        entries, truncated = gather(root, contexts, max_files=2)

        assert truncated
        tab_entries = [entry for entry in entries if entry.kind == "tab"]
        assert len(tab_entries) == 1
        assert tab_entries[0].subtitle == os.path.basename(open_file)
