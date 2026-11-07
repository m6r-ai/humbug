"""Tests for how the document converter records and classifies its writes."""

from pathlib import Path
from typing import Any
from unittest.mock import MagicMock

import pytest

from ai_tool import AIToolApprovalKind
from document_converter_ai_tool.document_converter_ai_tool import DocumentConverterAITool


class _JournallingMindspace:
    """A mindspace whose journal records every path outside a .humbug/ directory."""

    def __init__(self, root: Path) -> None:
        self._root = root
        self.journal = MagicMock()
        self.journal.record_write.return_value = MagicMock()
        self.journal.record_creation.return_value = MagicMock()
        self.logged: list[str] = []

    def has_mindspace(self) -> bool:
        """Report that a mindspace is open."""
        return True

    def add_interaction(self, _level: Any, content: str, context: Any = None) -> None:
        """Record an interaction log entry."""
        del context
        self.logged.append(content)

    def conversation_log_context(self, _requester: Any) -> str | None:
        """Attach no conversation context to an interaction."""
        return None

    def conversation_identifier(self, _requester: Any) -> str:
        """Name the conversation the write belongs to."""
        return "conv"

    def journalled_path(self, path: str) -> str | None:
        """Return the relative path when the journal records a change to it."""
        relative = Path(path).relative_to(self._root)
        if ".humbug" in relative.parts:
            return None

        return str(relative)

    def file_journal(self) -> MagicMock:
        """Return the journal the tool records through."""
        return self.journal


@pytest.fixture
def tool(tmp_path) -> tuple[DocumentConverterAITool, _JournallingMindspace]:
    """Create a converter wired to a real directory and a journalling mindspace."""
    mindspace = _JournallingMindspace(tmp_path)
    converter = DocumentConverterAITool(
        resolve_path=lambda path: (tmp_path / path, path),
        mindspace=mindspace,  # type: ignore[arg-type]
    )

    return converter, mindspace


class TestApprovalKind:
    """Tests for which conversions the approval policy may resolve without asking."""

    def test_a_conversion_into_the_mindspace_is_a_journalled_change(self, tool, tmp_path) -> None:
        """The output and its image folder are recorded, so the conversion is covered."""
        converter, _ = tool

        assert converter._approval_kind(tmp_path / "notes.md") == (  # pylint: disable=protected-access
            AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        )

    def test_a_conversion_outside_the_journal_is_standard(self, tool, tmp_path) -> None:
        """A path the journal cannot record is offered to the human instead."""
        converter, _ = tool

        assert converter._approval_kind(tmp_path / ".humbug" / "notes.md") == (  # pylint: disable=protected-access
            AIToolApprovalKind.STANDARD
        )


class TestSidecarRecording:
    """Tests for recording the image folder and images a conversion writes."""

    def test_the_folder_is_recorded_as_a_creation(self, tool, tmp_path) -> None:
        """The folder a conversion creates is recorded so that reversal removes it."""
        converter, mindspace = tool
        sidecar = tmp_path / "notes_files"

        converter._record_sidecar_write(sidecar, None, "")  # pylint: disable=protected-access

        mindspace.journal.record_creation.assert_called_once()
        arguments = mindspace.journal.record_creation.call_args
        assert arguments.args[0] == str(sidecar)
        assert arguments.args[1] == "notes_files"
        assert arguments.kwargs["is_directory"] is True

    def test_an_image_is_recorded_as_a_write(self, tool, tmp_path) -> None:
        """An image is recorded with the bytes about to be written, before they are."""
        converter, mindspace = tool
        image = tmp_path / "notes_files" / "image1.png"

        converter._record_sidecar_write(image, b"png", "")  # pylint: disable=protected-access

        mindspace.journal.record_write.assert_called_once()
        arguments = mindspace.journal.record_write.call_args
        assert arguments.args[0] == str(image)
        assert arguments.args[1] == str(Path("notes_files") / "image1.png")
        assert arguments.args[2] == b"png"
        assert arguments.args[4] == "conv"

    def test_a_failed_recording_is_reported_rather_than_silent(self, tool, tmp_path) -> None:
        """A write the journal cannot record is logged so the file is not shown as revertable."""
        converter, mindspace = tool
        mindspace.journal.record_write.return_value = None

        converter._record_sidecar_write(tmp_path / "notes_files" / "image1.png", b"png", "")  # pylint: disable=protected-access

        assert any("for reversal" in entry for entry in mindspace.logged)

    def test_a_path_outside_the_journal_is_not_recorded(self, tool, tmp_path) -> None:
        """Nothing is recorded for a path the journal does not cover."""
        converter, mindspace = tool

        converter._record_sidecar_write(tmp_path / ".humbug" / "image1.png", b"png", "")  # pylint: disable=protected-access

        mindspace.journal.record_write.assert_not_called()
