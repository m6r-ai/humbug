"""Shared fixtures for document converter AI tool tests."""

from pathlib import Path
from unittest.mock import MagicMock

import pytest

from document_converter_ai_tool.document_converter_ai_tool import DocumentConverterAITool


@pytest.fixture
def mock_mindspace():
    """Mock mindspace that silently accepts add_interaction calls."""
    mock = MagicMock()
    mock.add_interaction.return_value = None
    return mock


@pytest.fixture
def mock_path_resolver():
    """Path resolver that maps paths into a fake /test/mindspace/ sandbox."""
    def resolver(path: str) -> tuple[Path, str]:
        if not path:
            raise ValueError("Path cannot be empty")

        # Reject traversal attempts
        if ".." in path:
            raise ValueError(f"Path is outside the mindspace: {path}")

        # Absolute paths outside the sandbox are rejected
        if path.startswith("/") and not path.startswith("/test/mindspace/"):
            raise ValueError(f"Path is outside the mindspace: {path}")

        if path.startswith("/test/mindspace/"):
            abs_path = Path(path)
            display = path[len("/test/mindspace/"):]
            return abs_path, display

        abs_path = Path(f"/test/mindspace/{path}")
        return abs_path, path

    return resolver


@pytest.fixture
def converter_tool(mock_path_resolver, mock_mindspace):
    """DocumentConverterAITool instance wired to the mock resolver and mindspace."""
    return DocumentConverterAITool(
        resolve_path=mock_path_resolver,
        mindspace=mock_mindspace,
    )


@pytest.fixture
def mock_authorization():
    """Authorization callback that always approves."""
    mock = MagicMock()

    async def _approve(_tool_name, _arguments, _context, _requester_ref, _destructive):
        return True

    mock.side_effect = _approve
    return mock


@pytest.fixture
def mock_authorization_denied():
    """Authorization callback that always denies."""
    mock = MagicMock()

    async def _deny(_tool_name, _arguments, _context, _requester_ref, _destructive):
        return False

    mock.side_effect = _deny
    return mock
