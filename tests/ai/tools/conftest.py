"""
Shared fixtures and utilities for tool tests.
"""
import tempfile
from pathlib import Path
from unittest.mock import MagicMock

import pytest

from ai.tools.ai_tool_filesystem import AIToolFileSystem


@pytest.fixture
def mock_mindspace_manager():
    """Fixture providing a mocked mindspace manager."""
    mock_manager = MagicMock()

    # Default behavior - mindspace is available
    mock_manager.has_mindspace.return_value = True
    mock_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
    mock_manager.get_mindspace_relative_path.return_value = "file.txt"
    mock_manager.get_relative_path.return_value = "file.txt"

    return mock_manager


@pytest.fixture
def filesystem_tool(mock_mindspace_manager):
    """Fixture providing a filesystem tool instance with mocked mindspace manager."""
    tool = AIToolFileSystem()
    tool._mindspace_manager = mock_mindspace_manager
    return tool


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    mock = MagicMock()

    async def mock_auth_callback(_tool_name, _arguments, _context, _destructive):
        return True  # Default to authorized

    mock.side_effect = mock_auth_callback
    return mock


@pytest.fixture
def mock_authorization_denied():
    """Fixture providing a mocked authorization callback that denies requests."""
    mock = MagicMock()

    async def mock_auth_callback(_tool_name, _arguments, _context, _destructive):
        return False  # Always deny

    mock.side_effect = mock_auth_callback
    return mock


@pytest.fixture
def temp_file():
    """Fixture providing a temporary file for testing."""
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
        f.write("test content")
        temp_path = Path(f.name)

    yield temp_path

    # Cleanup
    if temp_path.exists():
        temp_path.unlink()


@pytest.fixture
def mock_path_factory():
    """Factory for creating mock Path objects with common behaviors."""
    def _create_mock_path(path_str, exists=True, is_file=True, is_dir=False, size=100, items=None):
        """
        Create a mock Path object with specified behaviors.

        Args:
            path_str: String representation of the path
            exists: Whether the path exists
            is_file: Whether the path is a file
            is_dir: Whether the path is a directory
            size: File size in bytes (for files)
            items: List of items in directory (for directories)

        Returns:
            Mock Path object configured with the specified behaviors
        """
        mock_path = MagicMock()
        mock_path.__str__ = lambda: path_str
        mock_path.exists.return_value = exists
        mock_path.is_file.return_value = is_file
        mock_path.is_dir.return_value = is_dir

        if is_file and exists:
            mock_stat = MagicMock()
            mock_stat.st_size = size
            mock_stat.st_mtime = 1640995200.0  # 2022-01-01 00:00:00
            mock_stat.st_mode = 0o100644  # Regular file permissions
            mock_path.stat.return_value = mock_stat
            mock_path.suffix = Path(path_str).suffix

        elif is_dir and exists:
            mock_stat = MagicMock()
            mock_stat.st_mtime = 1640995200.0  # 2022-01-01 00:00:00
            mock_stat.st_mode = 0o040755  # Directory permissions
            mock_path.stat.return_value = mock_stat

            if items is not None:
                mock_path.iterdir.return_value = items

            else:
                mock_path.iterdir.return_value = []

        return mock_path

    return _create_mock_path
