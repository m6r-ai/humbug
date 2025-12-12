"""
Tests for filesystem tool file management operations: delete_file, copy_file, move.
"""
import asyncio
from unittest.mock import patch, MagicMock

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied
from ai_tool.filesystem.filesystem_ai_tool import FileSystemAITool


class TestFileSystemAIToolDeleteFile:
    """Test the delete_file operation."""

    def test_delete_file_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful file deletion."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "file.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File deleted successfully: file.txt" in result.content
            mock_unlink.assert_called_once()
            # Verify authorization was called with destructive=True
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[4] is True  # destructive parameter

    def test_delete_file_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test deleting non-existent file."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "nonexistent.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "File does not exist: nonexistent.txt" in str(error)

    def test_delete_file_not_file(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test deleting when path is not a file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Path is not a file: dir" in str(error)

    def test_delete_file_authorization_denied(self, filesystem_tool, mock_authorization_denied, make_tool_call):
        """Test deleting file when authorization is denied."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "file.txt"})
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization_denied))

            error = exc_info.value
            assert "User denied permission to delete file: file.txt" in str(error)

    def test_delete_file_already_deleted(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test deleting file that was already deleted (race condition)."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_unlink.side_effect = OSError("File not found")

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to delete file" in str(error)

    def test_delete_file_permission_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test deleting file with permission error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_unlink.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied deleting file" in str(error)

    def test_delete_file_os_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test deleting file with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_unlink.side_effect = OSError("I/O error")

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to delete file" in str(error)


class TestFileSystemAIToolCopyFile:
    """Test the copy_file operation."""

    def test_copy_file_success_new_destination(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test successful file copying to new destination."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

            # Mock exists to return True for source, False for destination
            # Track which call we're on - first call is source, second is destination
            call_count = [0]
            def exists_side_effect():
                call_count[0] += 1
                # First call is for source (should exist), second is for destination (should not exist)
                return call_count[0] == 1

            mock_exists.side_effect = exists_side_effect
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File copied successfully: source.txt -> dest.txt" in result.content
            mock_copy2.assert_called_once()
            # Verify authorization was called with destructive=False for new destination
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[4] is False  # destructive parameter

    def test_copy_file_success_overwrite_destination(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test successful file copying with overwrite."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

            mock_exists.return_value = True  # Both source and destination exist
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File copied successfully: source.txt -> dest.txt" in result.content
            # Verify authorization was called with destructive=True for existing destination
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[4] is True  # destructive parameter

    def test_copy_file_source_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test copying non-existent source file."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "nonexistent.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Source file does not exist: nonexistent.txt" in str(error)

    def test_copy_file_source_not_file(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test copying when source is not a file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "dir", "destination": "dest.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Source path is not a file: dir" in str(error)

    def test_copy_file_no_destination(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test copying without destination parameter."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:
            mock_exists.return_value = True
            mock_is_file.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "source.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "destination" in str(error).lower() and "missing" in str(error).lower()

    def test_copy_file_source_too_large(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test copying file that exceeds size limit."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 11 * 1024 * 1024  # 11MB, exceeds 10MB limit
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "large.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Source file too large: 11.0MB (max: 10.0MB)" in str(error)

    def test_copy_file_authorization_denied(self, custom_path_resolver, mock_authorization_denied, make_tool_call):
        """Test copying file when authorization is denied."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            # Mock exists to return True for source, False for destination
            # We need to track which path object is being checked
            call_count = [0]
            def exists_side_effect():
                call_count[0] += 1
                # First call is for source (should exist), second is for destination (should not exist)
                return call_count[0] == 1

            mock_exists.side_effect = exists_side_effect
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization_denied))

            error = exc_info.value
            assert "User denied permission to copy file: source.txt -> dest.txt" in str(error)

    def test_copy_file_permission_error(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test copying file with permission error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

            # Mock exists to return True for source, False for destination
            call_count = [0]
            def exists_side_effect():
                call_count[0] += 1
                # First call is for source (should exist), second is for destination (should not exist)
                return call_count[0] == 1

            mock_exists.side_effect = exists_side_effect
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            mock_copy2.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied copying file" in str(error)

    def test_copy_file_os_error(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test copying file with OS error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

            # Mock exists to return True for source, False for destination
            call_count = [0]
            def exists_side_effect():
                call_count[0] += 1
                # First call is for source (should exist), second is for destination (should not exist)
                return call_count[0] == 1

            mock_exists.side_effect = exists_side_effect
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            mock_copy2.side_effect = OSError("Disk full")

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to copy file" in str(error)


class TestFileSystemAIToolMove:
    """Test the move operation."""

    def test_move_file_success(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test successful file moving."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "source.txt", "destination": "dest.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Moved successfully: source.txt -> dest.txt" in result.content
            mock_rename.assert_called_once()
            # Verify authorization was called with destructive=True
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[4] is True  # destructive parameter

    def test_move_directory_success(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test successful directory moving."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source_dir": ("/test/sandbox/source_dir", "source_dir"),
            "dest_dir": ("/test/sandbox/dest_dir", "dest_dir")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "source_dir", "destination": "dest_dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Moved successfully: source_dir -> dest_dir" in result.content
            mock_rename.assert_called_once()

    def test_move_source_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test moving non-existent source."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "nonexistent", "destination": "dest"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Source path does not exist: nonexistent" in str(error)

    def test_move_no_destination(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test moving without destination parameter."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "source.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "destination" in str(error).lower() and "missing" in str(error).lower()

    def test_move_authorization_denied(self, custom_path_resolver, mock_authorization_denied, make_tool_call):
        """Test moving when authorization is denied."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "source.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization_denied))

            error = exc_info.value
            assert "User denied permission to move: source.txt -> dest.txt" in str(error)

    def test_move_with_parent_creation(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test moving with parent directory creation."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "new_dir/dest.txt": ("/test/sandbox/new_dir/dest.txt", "new_dir/dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir, \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "source.txt", "destination": "new_dir/dest.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Moved successfully: source.txt -> new_dir/dest.txt" in result.content
            # Verify parent directory creation was attempted
            mock_mkdir.assert_called_once_with(parents=True, exist_ok=True)

    def test_move_permission_error(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test moving with permission error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_rename.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "source.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied moving" in str(error)

    def test_move_os_error(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test moving with OS error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_rename.side_effect = OSError("Cross-device link")

            tool_call = make_tool_call("filesystem", {"operation": "move", "path": "source.txt", "destination": "dest.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to move" in str(error)
