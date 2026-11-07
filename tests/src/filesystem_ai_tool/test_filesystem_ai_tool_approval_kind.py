"""Tests for how the filesystem tool classifies its authorization requests."""

import asyncio
from pathlib import Path
from typing import Any
from unittest.mock import MagicMock

from ai_tool import AIToolApprovalKind, AIToolCall
from filesystem_ai_tool.filesystem_access_settings import FilesystemAccessSettings
from filesystem_ai_tool.filesystem_ai_tool import FileSystemAITool


class _JournallingMindspace:
    """
    A mindspace whose journal records every path outside a .humbug/ directory.

    The scope rule is the real one: a path under .humbug/ is Humbug's own state and
    the journal never covers it, so the tool must not offer it as a covered change.
    """

    def __init__(self, root: Path) -> None:
        self._root = root
        self.created: list[str] = []
        self.journal = MagicMock()
        self.journal.record_creation.side_effect = self._record_creation

    def _record_creation(
        self,
        _path: str,
        relative_path: str,
        _actor: Any,
        _conversation: str,
        _group: str = "",
        is_directory: bool = False
    ) -> MagicMock:
        """Record a creation, remembering the folders a call made."""
        if is_directory:
            self.created.append(relative_path)

        return MagicMock()

    def has_mindspace(self) -> bool:
        """Report that a mindspace is open."""
        return True

    def mindspace_path(self) -> str:
        """Return the mindspace root, which bounds how far a parent walk may go."""
        return str(self._root)

    def add_interaction(self, *_args: Any, **_kwargs: Any) -> None:
        """Discard an interaction log entry."""

    def journalled_path(self, path: str) -> str | None:
        """Return the relative path when the journal records a change to it."""
        relative = Path(path).relative_to(self._root)
        if ".humbug" in relative.parts:
            return None

        return str(relative)

    def file_journal(self) -> MagicMock:
        """Return the journal the tool records through."""
        return self.journal

    def conversation_identifier(self, _requester: Any) -> str:
        """Report no conversation, as a tool call made outside one would."""
        return ""

    def conversation_log_context(self, _requester: Any) -> str | None:
        """Attach no conversation context to an interaction."""
        return None

    def remove_path(self, _path: str, _actor: Any, _conversation: str) -> bool:
        """Report a removal as done, which the journal would have recorded."""
        return True


def _make_tool(root: Path, mindspace: _JournallingMindspace) -> FileSystemAITool:
    """Create a filesystem tool rooted at a directory, recording into the given mindspace."""
    settings = FilesystemAccessSettings(
        allow_external_access=False,
        external_allowlist=[],
        external_denylist=[]
    )

    def resolve(path: str) -> tuple[Path, str]:
        """Model the real resolver, whose display path is relative to the mindspace."""
        candidate = Path(path)
        if candidate.is_absolute():
            return candidate, str(candidate.relative_to(root))

        return root / path, path

    return FileSystemAITool(
        resolve_path=resolve,
        get_access_settings=lambda: settings,
        mindspace=mindspace  # type: ignore[arg-type]
    )


def _run(tool: FileSystemAITool, operation: str, **kwargs: Any) -> list[AIToolApprovalKind]:
    """Run an operation and return the classifications of its authorization requests."""
    recorded: list[AIToolApprovalKind] = []

    async def authorize(
        _tool_name: str,
        _arguments: dict[str, Any],
        _reason: str,
        _context: str | None,
        _destructive: bool,
        kind: AIToolApprovalKind
    ) -> bool:
        recorded.append(kind)
        return True

    arguments: dict[str, Any] = {"operation": operation}
    arguments.update(kwargs)
    call = AIToolCall(id="test-id", name="filesystem", arguments=arguments)

    async def execute() -> None:
        await tool.execute(call, None, authorize)

    asyncio.run(execute())

    return recorded


def _execute(root: Path, operation: str, **kwargs: Any) -> list[AIToolApprovalKind]:
    """Run an operation in a fresh mindspace and return its classifications."""
    return _run(_make_tool(root, _JournallingMindspace(root)), operation, **kwargs)


class TestApprovalKind:
    """Tests for which requests the tool offers as changes the journal records."""

    def test_writing_a_file_is_a_journalled_change(self, tmp_path) -> None:
        """A file write inside the mindspace is a change the journal reverses."""
        assert _execute(tmp_path, "write_file", path="note.txt", content="hello") == [
            AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        ]

    def test_deleting_a_file_is_a_journalled_change(self, tmp_path) -> None:
        """A deletion inside the mindspace is recorded with the content it held."""
        (tmp_path / "note.txt").write_text("hello", encoding="utf-8")

        assert _execute(tmp_path, "delete_file", path="note.txt") == [
            AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        ]

    def test_creating_a_directory_is_a_journalled_change(self, tmp_path) -> None:
        """A created directory is recorded so that reversal removes it."""
        assert _execute(tmp_path, "create_directory", path="notes") == [
            AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        ]

    def test_a_write_the_journal_does_not_cover_is_standard(self, tmp_path) -> None:
        """A path outside the journal's scope is offered to the human, not assumed."""
        (tmp_path / ".humbug").mkdir()

        assert _execute(tmp_path, "write_file", path=".humbug/state.json", content="{}") == [
            AIToolApprovalKind.STANDARD
        ]

    def test_moving_a_file_is_a_journalled_change(self, tmp_path) -> None:
        """A file move is recorded at both ends, so it is reversible."""
        (tmp_path / "note.txt").write_text("hello", encoding="utf-8")

        assert _execute(tmp_path, "move", path="note.txt", destination="moved.txt") == [
            AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        ]

    def test_moving_an_empty_directory_is_a_journalled_change(self, tmp_path) -> None:
        """An empty directory is recorded at both ends, so its move is reversible."""
        (tmp_path / "notes").mkdir()

        assert _execute(tmp_path, "move", path="notes", destination="archive") == [
            AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        ]

    def test_moving_a_populated_directory_is_standard(self, tmp_path) -> None:
        """A directory whose contents are not captured is not offered as reversible."""
        (tmp_path / "notes").mkdir()
        (tmp_path / "notes" / "note.txt").write_text("hello", encoding="utf-8")

        assert _execute(tmp_path, "move", path="notes", destination="archive") == [
            AIToolApprovalKind.STANDARD
        ]


class TestDirectoryCreationRecording:
    """Tests for the folders a creation records, including the ones it makes on the way."""

    def test_the_folders_a_creation_makes_are_all_recorded(self, tmp_path) -> None:
        """A creation records each missing parent as well as the folder it was asked for."""
        mindspace = _JournallingMindspace(tmp_path)

        _run(_make_tool(tmp_path, mindspace), "create_directory", path="one/two/three")

        assert mindspace.created == [
            "one",
            str(Path("one") / "two"),
            str(Path("one") / "two" / "three"),
        ]

    def test_the_walk_stops_at_the_mindspace_root(self, tmp_path) -> None:
        """A root that does not exist yet does not send the walk looking above the mindspace."""
        missing_root = tmp_path / "not-yet-there"
        mindspace = _JournallingMindspace(missing_root)

        _run(_make_tool(missing_root, mindspace), "create_directory", path="notes")

        assert mindspace.created == ["notes"]
