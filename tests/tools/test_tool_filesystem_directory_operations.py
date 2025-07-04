"""
Tests for filesystem tool directory operations: list_directory, create_directory, remove_directory.
"""
import asyncio
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

from humbug.ai.ai_tool_manager import AIToolExecutionError, AIToolAuthorizationDenied


class TestToolFileSystemListDirectory:
    """Test the list_directory operation."""

    def test_list_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory listing."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"
        mock_mindspace_manager.get_relative_path.return_value = "dir"

        # Mock directory items
        mock_file = MagicMock()
        mock_file.name = "file.txt"
        mock_file.is_file.return_value = True
        mock_file.is_dir.return_value = False
        mock_file.stat.return_value.st_size = 100

        mock_subdir = MagicMock()
        mock_subdir.name = "subdir"
        mock_subdir.is_file.return_value = False
        mock_subdir.is_dir.return_value = True

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file, mock_subdir]

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "list_directory", "path": "dir"},
                mock_authorization
            ))

            assert "Directory: dir" in result
            assert "Items: 2" in result
            assert "📄 file.txt (100 bytes)" in result
            assert "📁 subdir/" in result

    def test_list_directory_empty(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing empty directory."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/empty_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "empty_dir"
        mock_mindspace_manager.get_relative_path.return_value = "empty_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/empty_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "list_directory", "path": "empty_dir"},
                mock_authorization
            ))

            assert "Directory: empty_dir" in result
            assert "Items: 0" in result

    def test_list_directory_with_other_types(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing directory with various item types."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"
        mock_mindspace_manager.get_relative_path.return_value = "dir"

        # Mock different types of items
        mock_file = MagicMock()
        mock_file.name = "file.txt"
        mock_file.is_file.return_value = True
        mock_file.is_dir.return_value = False
        mock_file.stat.return_value.st_size = 100

        mock_symlink = MagicMock()
        mock_symlink.name = "symlink"
        mock_symlink.is_file.return_value = False
        mock_symlink.is_dir.return_value = False

        mock_unknown = MagicMock()
        mock_unknown.name = "unknown"
        mock_unknown.is_file.side_effect = PermissionError("Access denied")
        mock_unknown.is_dir.return_value = False

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file, mock_symlink, mock_unknown]

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "list_directory", "path": "dir"},
                mock_authorization
            ))

            assert "Directory: dir" in result
            assert "Items: 3" in result
            assert "📄 file.txt (100 bytes)" in result
            assert "📄 symlink" in result
            assert "📄 unknown" in result

    def test_list_directory_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing non-existent directory."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "list_directory", "path": "dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory does not exist: dir" in str(error)

    def test_list_directory_not_directory(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing when path is not a directory."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "list_directory", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is not a directory: file.txt" in str(error)

    def test_list_directory_permission_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing directory with permission error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "list_directory", "path": "dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied listing directory" in str(error)

    def test_list_directory_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing directory with OS error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = OSError("I/O error")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "list_directory", "path": "dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to list directory" in str(error)


class TestToolFileSystemCreateDirectory:
    """Test the create_directory operation."""

    def test_create_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory creation."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/new_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "new_dir"
        mock_mindspace_manager.get_relative_path.return_value = "new_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_path = Path("/test/mindspace/new_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "create_directory", "path": "new_dir"},
                mock_authorization
            ))

            assert "Directory created successfully: new_dir" in result
            # Verify mkdir was called with default create_parents=True
            mock_mkdir.assert_called_once_with(parents=True, exist_ok=False)
            # Verify authorization was called with destructive=False
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is False  # destructive parameter

    def test_create_directory_with_create_parents_false(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test directory creation with create_parents=False."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/new_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "new_dir"
        mock_mindspace_manager.get_relative_path.return_value = "new_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_path = Path("/test/mindspace/new_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "create_directory", "path": "new_dir", "create_parents": False},
                mock_authorization
            ))

            assert "Directory created successfully: new_dir" in result
            # Verify mkdir was called with create_parents=False
            mock_mkdir.assert_called_once_with(parents=False, exist_ok=False)

    def test_create_directory_already_exists_as_directory(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test creating directory that already exists as a directory."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/existing_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "existing_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_path = Path("/test/mindspace/existing_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "create_directory", "path": "existing_dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory already exists: existing_dir" in str(error)

    def test_create_directory_already_exists_as_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test creating directory where a file already exists."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/existing_file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "existing_file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_path = Path("/test/mindspace/existing_file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "create_directory", "path": "existing_file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path exists but is not a directory: existing_file.txt" in str(error)

    def test_create_directory_authorization_denied(self, filesystem_tool, mock_mindspace_manager, mock_authorization_denied):
        """Test creating directory when authorization is denied."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/new_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "new_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/new_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "create_directory", "path": "new_dir"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to create directory: new_dir" in str(error)

    def test_create_directory_file_exists_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test creating directory with FileExistsError from mkdir."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/new_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "new_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_path = Path("/test/mindspace/new_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False
            mock_mkdir.side_effect = FileExistsError("Directory already exists")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "create_directory", "path": "new_dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory already exists" in str(error)

    def test_create_directory_permission_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test creating directory with permission error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/new_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "new_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_path = Path("/test/mindspace/new_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False
            mock_mkdir.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "create_directory", "path": "new_dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied creating directory" in str(error)

    def test_create_directory_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test creating directory with OS error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/new_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "new_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_path = Path("/test/mindspace/new_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False
            mock_mkdir.side_effect = OSError("Disk full")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "create_directory", "path": "new_dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to create directory" in str(error)


class TestToolFileSystemRemoveDirectory:
    """Test the remove_directory operation."""

    def test_remove_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory removal."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/empty_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "empty_dir"
        mock_mindspace_manager.get_relative_path.return_value = "empty_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('pathlib.Path.rmdir') as mock_rmdir:

            mock_path = Path("/test/mindspace/empty_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []  # Empty directory

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "remove_directory", "path": "empty_dir"},
                mock_authorization
            ))

            assert "Directory removed successfully: empty_dir" in result
            mock_rmdir.assert_called_once()
            # Verify authorization was called with destructive=True
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is True  # destructive parameter

    def test_remove_directory_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test removing non-existent directory."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/nonexistent"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "nonexistent"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/nonexistent")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "remove_directory", "path": "nonexistent"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory does not exist: nonexistent" in str(error)

    def test_remove_directory_not_directory(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test removing when path is not a directory."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "remove_directory", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is not a directory: file.txt" in str(error)

    def test_remove_directory_not_empty(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test removing directory that is not empty."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/full_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "full_dir"

        # Mock directory items
        mock_file = MagicMock()
        mock_file.name = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/full_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file]  # Non-empty directory

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "remove_directory", "path": "full_dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory is not empty (contains 1 items): full_dir" in str(error)

    def test_remove_directory_permission_error_checking_contents(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test removing directory with permission error when checking contents."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "remove_directory", "path": "dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied checking directory contents" in str(error)

    def test_remove_directory_authorization_denied(self, filesystem_tool, mock_mindspace_manager, mock_authorization_denied):
        """Test removing directory when authorization is denied."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/empty_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "empty_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_path = Path("/test/mindspace/empty_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "remove_directory", "path": "empty_dir"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to remove directory: empty_dir" in str(error)

    def test_remove_directory_already_removed(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test removing directory that was already removed (race condition)."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/empty_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "empty_dir"
        mock_mindspace_manager.get_relative_path.return_value = "empty_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('pathlib.Path.rmdir') as mock_rmdir:

            mock_path = Path("/test/mindspace/empty_dir")
            mock_resolve.return_value = mock_path

            # First exists() call returns True, second returns False (after rmdir fails)
            mock_exists.side_effect = [True, False]
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []
            mock_rmdir.side_effect = OSError("Directory not found")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "remove_directory", "path": "empty_dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to remove directory" in str(error)

    def test_remove_directory_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test removing directory with OS error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/empty_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "empty_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('pathlib.Path.rmdir') as mock_rmdir:

            mock_path = Path("/test/mindspace/empty_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []
            mock_rmdir.side_effect = OSError("I/O error")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "remove_directory", "path": "empty_dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to remove directory" in str(error)

    def test_remove_directory_authorization_context_permission_error_on_directory_listing(
        self,
        filesystem_tool,
        mock_mindspace_manager,
        mock_authorization
    ):
        """Test authorization context building with permission error when listing directory contents."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/protected_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "protected_dir"
        mock_mindspace_manager.get_relative_path.return_value = "protected_dir"

        call_count = [0]
        def mock_iterdir_side_effect():
            call_count[0] += 1
            if call_count[0] == 1:
                # First call: checking if directory is empty
                return []

            # Subsequent calls: during authorization context building
            raise PermissionError("Permission denied")

        with patch('pathlib.Path.resolve') as mock_resolve, \
            patch('pathlib.Path.exists') as mock_exists, \
            patch('pathlib.Path.is_dir') as mock_is_dir, \
            patch('pathlib.Path.iterdir') as mock_iterdir, \
            patch('pathlib.Path.rmdir') as mock_rmdir:

            mock_path = Path("/test/mindspace/protected_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = mock_iterdir_side_effect

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "remove_directory", "path": "protected_dir"},
                mock_authorization
            ))

            assert "Directory removed successfully: protected_dir" in result

            # Verify that authorization was called, which means
            # _build_authorization_context completed successfully despite the PermissionError
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            # The context should include the permission denied message
            assert "Operation: remove_directory" in context
            assert "Path: protected_dir" in context
            assert "Directory items: Permission denied" in context
