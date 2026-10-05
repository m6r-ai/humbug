"""
Tests for filesystem tool directory operations: list_directory, create_directory, remove_directory.
"""
import json
import asyncio
import os
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied
from filesystem_ai_tool.filesystem_ai_tool import FileSystemAITool
from filesystem_ai_tool.filesystem_access_settings import FilesystemAccessSettings


def _make_tool_for_sandbox(sandbox: str) -> FileSystemAITool:
    """
    Build a filesystem tool whose resolver maps paths into a real directory.

    Directory listing inspects the real filesystem, so the tests exercise it
    against a temporary directory rather than mocked path objects.

    Args:
        sandbox: Absolute path to the directory to treat as the mindspace root.

    Returns:
        A FileSystemAITool rooted at the sandbox.
    """
    def resolver(path: str) -> tuple[Path, str]:
        if path.startswith("/"):
            path = path[1:]

        return Path(os.path.join(sandbox, path)), path

    def get_settings() -> FilesystemAccessSettings:
        return FilesystemAccessSettings(
            allow_external_access=False,
            external_allowlist=[],
            external_denylist=[]
        )

    mindspace = MagicMock()
    mindspace.has_mindspace.return_value = True
    mindspace.add_interaction.return_value = None
    mindspace.mindspace_path.return_value = sandbox

    return FileSystemAITool(
        resolve_path=resolver,
        get_access_settings=get_settings,
        mindspace=mindspace
    )


class TestFileSystemAIToolListDirectory:
    """Test the list_directory operation."""

    def test_list_directory_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful directory listing."""
        with tempfile.TemporaryDirectory() as sandbox:
            target = os.path.join(sandbox, "dir")
            os.mkdir(target)

            with open(os.path.join(target, "file.txt"), "w", encoding="utf-8") as f:
                f.write("x" * 100)

            os.mkdir(os.path.join(target, "subdir"))

            tool = _make_tool_for_sandbox(sandbox)
            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            result = asyncio.run(tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            listing = json.loads(result.content)
            assert listing["directory"] == "dir"
            assert listing["total_items"] == 2
            assert result.context == "json"
            # Check items
            items = {item["name"]: item for item in listing["items"]}
            assert items["file.txt"]["type"] == "file"
            assert items["file.txt"]["size"] == 100
            assert items["subdir"]["type"] == "directory"

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

            # New format returns JSON
            listing = json.loads(result.content)
            assert listing["directory"] == "empty_dir"
            assert listing["total_items"] == 0
            assert listing["items"] == []

    def test_list_directory_with_other_types(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test listing directory with various item types."""
        with tempfile.TemporaryDirectory() as sandbox:
            target = os.path.join(sandbox, "dir")
            os.mkdir(target)

            with open(os.path.join(target, "file.txt"), "w", encoding="utf-8") as f:
                f.write("x" * 100)

            os.mkdir(os.path.join(target, "realdir"))

            # A FIFO is neither a regular file nor a directory, so it is reported as "other".
            os.mkfifo(os.path.join(target, "pipe"))

            tool = _make_tool_for_sandbox(sandbox)
            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            result = asyncio.run(tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            listing = json.loads(result.content)
            assert listing["directory"] == "dir"
            assert listing["total_items"] == 3
            # Check items
            items = {item["name"]: item for item in listing["items"]}
            assert items["file.txt"]["type"] == "file"
            assert items["realdir"]["type"] == "directory"
            assert items["pipe"]["type"] == "other"

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
            assert args[4] is False  # destructive parameter

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
            assert args[4] is True  # destructive parameter

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


class TestListDirectorySymlinks:
    """Tests that list_directory does not disclose symlinked targets."""

    def test_symlink_escaping_directory_is_omitted(self, mock_authorization, make_tool_call):
        """A symlink pointing outside the listed directory is not reported."""
        with tempfile.TemporaryDirectory() as sandbox:
            target = os.path.join(sandbox, "dir")
            os.mkdir(target)

            with open(os.path.join(target, "normal.txt"), "w", encoding="utf-8") as f:
                f.write("x")

            outside = os.path.join(sandbox, "outside")
            os.mkdir(outside)

            with open(os.path.join(outside, "secret.txt"), "w", encoding="utf-8") as f:
                f.write("secret")

            os.symlink(outside, os.path.join(target, "escape"))

            tool = _make_tool_for_sandbox(sandbox)
            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
            listing = json.loads(result.content)

            names = {item["name"] for item in listing["items"]}
            assert "normal.txt" in names
            assert "escape" not in names

    def test_symlink_into_humbug_is_omitted(self, mock_authorization, make_tool_call):
        """A symlink pointing into .humbug/ is not reported."""
        with tempfile.TemporaryDirectory() as sandbox:
            target = os.path.join(sandbox, "dir")
            os.mkdir(target)

            humbug = os.path.join(sandbox, ".humbug")
            os.mkdir(humbug)

            with open(os.path.join(humbug, "system.json"), "w", encoding="utf-8") as f:
                f.write("audit")

            os.symlink(humbug, os.path.join(target, "hlink"))

            tool = _make_tool_for_sandbox(sandbox)
            tool_call = make_tool_call("filesystem", {"operation": "list_directory", "path": "dir"})
            result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
            listing = json.loads(result.content)

            assert listing["total_items"] == 0
