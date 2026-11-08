"""Shared fixtures for editor tests requiring Qt.

The QT_QPA_PLATFORM variable must be set before the QApplication is created.
"""
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

# pylint: disable=wrong-import-position
from pathlib import Path
from typing import Any
from unittest.mock import MagicMock

import pytest

from PySide6.QtWidgets import QApplication

from ai_tool import AIToolApprovalKind
from editor_ai_tool.editor_ai_tool import EditorAITool
from editor_context.editor_context import EditorContext
from editor_context.editor_context_document import EditorContextDocument


class RecordingAuthorizer:
    """Authorization callback that records each request and grants or refuses it."""

    def __init__(self, grant: bool = True) -> None:
        self.grant = grant
        self.requests: list[tuple[str, dict[str, Any], str, str | None, bool, AIToolApprovalKind]] = []

    async def __call__(
        self,
        tool_name: str,
        arguments: dict[str, Any],
        context: str,
        diff: str | None,
        is_write: bool,
        kind: AIToolApprovalKind
    ) -> bool:
        """Record the request and answer it."""
        self.requests.append((tool_name, arguments, context, diff, is_write, kind))
        return self.grant


class Editor:
    """A real editor context backed by a file in a test's temporary directory."""

    def __init__(self, path: str, context: EditorContext) -> None:
        self.path = path
        self.context = context

    def buffer(self) -> str:
        """Return the current buffer content."""
        return self.context.get_text_range(None, None)

    def file_text(self) -> str:
        """Return the file content on disk, or an empty string when it has none."""
        if not self.path or not Path(self.path).exists():
            return ''

        return Path(self.path).read_text(encoding='utf-8')


def make_editor(
    tmp_path: Path,
    mindspace: MagicMock,
    content: str,
    read_only: bool = False,
    untitled: bool = False
) -> Editor:
    """
    Create an editor context whose document is a file holding the given content.

    Args:
        tmp_path: Directory the file is created in.
        mindspace: The mock mindspace whose registry will hold the context.
        content: Content written to the file and loaded into the buffer.
        read_only: True for a view of content that must not be written back.
        untitled: True for an editor with no file path.

    Returns:
        The editor holding the context.
    """
    path = "" if untitled else str(tmp_path / "file.txt")

    if path:
        Path(path).write_text(content, encoding='utf-8')

    document = EditorContextDocument(path=path, content=content, read_only=read_only)
    context = EditorContext(context_id="test-tab-id", document=document)

    registry = MagicMock()
    registry.get_model.return_value = context
    mindspace.contexts.return_value = registry

    return Editor(path, context)


@pytest.fixture(scope="session")
def qapp():
    """Session-wide QApplication running on the offscreen platform."""
    app = QApplication.instance() or QApplication([])
    return app


@pytest.fixture
def mock_mindspace(tmp_path):
    """Fixture providing a minimal mock Mindspace."""
    mindspace = MagicMock()
    mindspace.add_interaction = MagicMock()
    mindspace.mindspace_path.return_value = str(tmp_path)
    mindspace.contexts.return_value = MagicMock()
    mindspace.conversation_identifier.return_value = "conversation-id"
    return mindspace


@pytest.fixture
def mock_authorization():
    """Fixture that always grants authorization."""
    async def _auth(*_args, **_kwargs):
        return True
    return _auth


@pytest.fixture
def editor_tool(mock_mindspace):
    """Fixture providing an EditorAITool and the mock mindspace it uses."""
    return EditorAITool(mindspace=mock_mindspace), mock_mindspace
