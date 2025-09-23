"""
Tests for filesystem tool directory operations: list_directory, create_directory, remove_directory.
"""
import asyncio
from unittest.mock import patch, MagicMock

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied


class TestFileSystemAIToolListDirectory:
    """Test the list_directory operation."""

    def test_list_directory_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful directory listing."""
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

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file, mock_subdir]

            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Directory: dir" in result.content
            assert "Items: 2" in result.content
            assert "📄 file.txt (100 bytes)" in result.content
            assert "📁 subdir/" in result.content

    def test_list_directory_empty(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test listing empty directory."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []

            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "empty_dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Directory: empty_dir" in result.content
            assert "Items: 0" in result.content

    def test_list_directory_with_other_types(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test listing directory with various item types."""
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

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file, mock_symlink, mock_unknown]

            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Directory: dir" in result.content
            assert "Items: 3" in result.content
            assert "📄 file.txt (100 bytes)" in result.content
            assert "📄 symlink" in result.content
            assert "📄 unknown" in result.content

    def test_list_directory_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test listing non-existent directory."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Directory does not exist: dir" in str(error)

    def test_list_directory_not_directory(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test listing when path is not a directory."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_exists.return_value = True
            mock_is_dir.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Path is not a directory: file.txt" in str(error)

    def test_list_directory_permission_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test listing directory with permission error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied listing directory" in str(error)

    def test_list_directory_os_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test listing directory with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = OSError("I/O error")

            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to list directory" in str(error)


class TestFileSystemAIToolCreateDirectory:
    """Test the create_directory operation."""

    def test_create_directory_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful directory creation."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "new_dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Directory created successfully: new_dir" in result.content
            # Verify mkdir was called with default create_parents=True
            mock_mkdir.assert_called_once_with(parents=True, exist_ok=False)
            # Verify authorization was called with destructive=False
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is False  # destructive parameter

    def test_create_directory_with_create_parents_false(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test directory creation with create_parents=False."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "new_dir", "create_parents": False})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Directory created successfully: new_dir" in result.content
            # Verify mkdir was called with create_parents=False
            mock_mkdir.assert_called_once_with(parents=False, exist_ok=False)

    def test_create_directory_already_exists_as_directory(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test creating directory that already exists as a directory."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "existing_dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Directory already exists: existing_dir" in str(error)

    def test_create_directory_already_exists_as_file(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test creating directory where a file already exists."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_exists.return_value = True
            mock_is_dir.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "existing_file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Path exists but is not a directory: existing_file.txt" in str(error)

    def test_create_directory_authorization_denied(self, filesystem_tool, mock_authorization_denied, make_tool_call):
        """Test creating directory when authorization is denied."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "new_dir"})
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization_denied))

            error = exc_info.value
            assert "User denied permission to create directory: new_dir" in str(error)

    def test_create_directory_file_exists_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test creating directory with FileExistsError from mkdir."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_exists.return_value = False
            mock_mkdir.side_effect = FileExistsError("Directory already exists")

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "new_dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Directory already exists" in str(error)

    def test_create_directory_permission_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test creating directory with permission error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_exists.return_value = False
            mock_mkdir.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "new_dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied creating directory" in str(error)

    def test_create_directory_os_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test creating directory with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir:

            mock_exists.return_value = False
            mock_mkdir.side_effect = OSError("Disk full")

            tool_call = make_tool_call("filesystem", {"operation": "create_directory", "path": "new_dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to create directory" in str(error)


class TestFileSystemAIToolRemoveDirectory:
    """Test the remove_directory operation."""

    def test_remove_directory_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful directory removal."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('pathlib.Path.rmdir') as mock_rmdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []  # Empty directory

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "empty_dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Directory removed successfully: empty_dir" in result.content
            mock_rmdir.assert_called_once()
            # Verify authorization was called with destructive=True
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is True  # destructive parameter

    def test_remove_directory_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test removing non-existent directory."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "nonexistent"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Directory does not exist: nonexistent" in str(error)

    def test_remove_directory_not_directory(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test removing when path is not a directory."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir:

            mock_exists.return_value = True
            mock_is_dir.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Path is not a directory: file.txt" in str(error)

    def test_remove_directory_not_empty(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test removing directory that is not empty."""
        # Mock directory items
        mock_file = MagicMock()
        mock_file.name = "file.txt"

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file]  # Non-empty directory

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "full_dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Directory is not empty (contains 1 items): full_dir" in str(error)

    def test_remove_directory_permission_error_checking_contents(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test removing directory with permission error when checking contents."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied checking directory contents" in str(error)

    def test_remove_directory_authorization_denied(self, filesystem_tool, mock_authorization_denied, make_tool_call):
        """Test removing directory when authorization is denied."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "empty_dir"})
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization_denied))

            error = exc_info.value
            assert "User denied permission to remove directory: empty_dir" in str(error)

    def test_remove_directory_already_removed(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test removing directory that was already removed (race condition)."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('pathlib.Path.rmdir') as mock_rmdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []
            mock_rmdir.side_effect = OSError("Directory not found")

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "empty_dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to remove directory" in str(error)

    def test_remove_directory_os_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test removing directory with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('pathlib.Path.rmdir') as mock_rmdir:

            mock_exists.return_value = True
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []
            mock_rmdir.side_effect = OSError("I/O error")

            tool_call = make_tool_call("filesystem", {"operation": "remove_directory", "path": "empty_dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to remove directory" in str(error)
