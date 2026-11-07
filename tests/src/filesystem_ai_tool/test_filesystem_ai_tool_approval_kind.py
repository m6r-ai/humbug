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

    def has_mindspace(self) -> bool:
        """Report that a mindspace is open."""
        return True

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
        return MagicMock()

    def conversation_identifier(self, _requester: Any) -> str:
        """Report no conversation, as a tool call made outside one would."""
        return ""

    def conversation_log_context(self, _requester: Any) -> str | None:
        """Attach no conversation context to an interaction."""
        return None

    def remove_path(self, _path: str, _actor: Any, _conversation: str) -> bool:
        """Report a removal as done, which the journal would have recorded."""
        return True


def _execute(root: Path, operation: str, **kwargs: Any) -> list[AIToolApprovalKind]:
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

    def resolve(path: str) -> tuple[Path, str]:
        return root / path, path

    settings = FilesystemAccessSettings(
        allow_external_access=False,
        external_allowlist=[],
        external_denylist=[]
    )
    tool = FileSystemAITool(
        resolve_path=resolve,
        get_access_settings=lambda: settings,
        mindspace=_JournallingMindspace(root)  # type: ignore[arg-type]
    )

    arguments: dict[str, Any] = {"operation": operation}
    arguments.update(kwargs)
    call = AIToolCall(id="test-id", name="filesystem", arguments=arguments)

    async def run() -> None:
        await tool.execute(call, None, authorize)

    asyncio.run(run())

    return recorded


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

    def test_creating_a_directory_is_not_a_journalled_change(self, tmp_path) -> None:
        """A created directory leaves no pre-image, so the journal cannot reverse it."""
        assert _execute(tmp_path, "create_directory", path="notes") == [AIToolApprovalKind.STANDARD]

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

    def test_moving_a_directory_is_standard(self, tmp_path) -> None:
        """A moved directory is not journalled, so its move is not offered as reversible."""
        (tmp_path / "notes").mkdir()

        assert _execute(tmp_path, "move", path="notes", destination="archive") == [
            AIToolApprovalKind.STANDARD
        ]
