"""
Shared fixtures and utilities for tool tests.
"""
import tempfile
from pathlib import Path
from typing import Tuple, Dict, Any
from unittest.mock import MagicMock

import pytest

from ai_tool import AIToolCall
from ai_tool.filesystem.filesystem_ai_tool import FileSystemAITool
from humbug.user.user_settings import UserSettings


@pytest.fixture
def mock_path_resolver():
    """Fixture providing a simple path resolver for testing."""
    def resolver(path: str) -> Tuple[Path, str]:
        """
        Simple test path resolver that maps paths to a test sandbox.

        Args:
            path: Input path string

        Returns:
            Tuple of (absolute_path, display_path)

        Raises:
            ValueError: If path is invalid
        """
        if not path:
            raise ValueError("Path cannot be empty")

        # Handle leading separator (treat as sandbox root)
        if path.startswith('/'):
            path = path[1:]

        # Simple validation - reject paths that go outside boundaries
        if '..' in path or path.startswith('/'):
            raise ValueError(f"Path is outside allowed boundaries: {path}")

        # Create absolute path for operations
        abs_path = Path(f"/test/sandbox/{path}")

        # Use the input path as display path (after removing leading slash)
        display_path = path

        return abs_path, display_path

    return resolver


@pytest.fixture
def mock_user_settings():
    """Fixture providing mock user settings for testing."""
    def get_settings() -> UserSettings:
        """Return test user settings with external file access disabled by default."""
        return UserSettings(
            ai_backends={},
            allow_external_file_access=False,  # Disabled by default for tests
            external_file_allowlist="",
            external_file_denylist=""
        )
    return get_settings


@pytest.fixture
def mock_user_settings_with_external_access():
    """Fixture providing mock user settings with external access enabled."""
    def get_settings() -> UserSettings:
        """Return test user settings with external file access enabled."""
        return UserSettings(
            ai_backends={},
            allow_external_file_access=True,
            external_file_allowlist="/usr/include/**\n/usr/share/**",
            external_file_denylist="~/.ssh/**\n~/.aws/**"
        )
    return get_settings


@pytest.fixture
def filesystem_tool(mock_path_resolver, mock_user_settings):
    """Fixture providing a filesystem tool instance with mocked path resolver."""
    return FileSystemAITool(resolve_path=mock_path_resolver, get_user_settings=mock_user_settings)


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    mock = MagicMock()

    async def mock_auth_callback(_tool_name, _arguments, _context, _requester_ref, _destructive):
        return True  # Default to authorized

    mock.side_effect = mock_auth_callback
    return mock


@pytest.fixture
def mock_authorization_denied():
    """Fixture providing a mocked authorization callback that denies requests."""
    mock = MagicMock()

    async def mock_auth_callback(_tool_name, _arguments, _context, _requester_ref, _destructive):
        return False  # Always deny

    mock.side_effect = mock_auth_callback
    return mock


@pytest.fixture
def make_tool_call():
    """Factory for creating AIToolCall objects for testing."""
    counter = [0]

    def _make_call(tool_name: str, arguments: Dict[str, Any]) -> AIToolCall:
        counter[0] += 1
        return AIToolCall(
            id=f"test_call_{counter[0]}",
            name=tool_name,
            arguments=arguments
        )

    return _make_call


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


@pytest.fixture
def custom_path_resolver():
    """Factory for creating custom path resolvers for specific test scenarios."""
    def _create_resolver(path_mapping=None, validation_func=None):
        """
        Create a custom path resolver with specific behaviors.

        Args:
            path_mapping: Dict mapping input paths to (abs_path, display_path) tuples
            validation_func: Function to validate paths, should raise ValueError if invalid

        Returns:
            Path resolver function
        """
        def resolver(path: str) -> Tuple[Path, str]:
            if validation_func:
                validation_func(path)

            if path_mapping and path in path_mapping:
                abs_path_str, display_path = path_mapping[path]
                return Path(abs_path_str), display_path

            # Default behavior
            if not path:
                raise ValueError("Path cannot be empty")

            if path.startswith('/'):
                path = path[1:]

            abs_path = Path(f"/test/sandbox/{path}")
            return abs_path, path

        return resolver

    return _create_resolver
