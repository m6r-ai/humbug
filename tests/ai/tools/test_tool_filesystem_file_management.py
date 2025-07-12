"""
Tests for filesystem tool file management operations: delete_file, copy_file, move.
"""
import asyncio
from unittest.mock import patch, MagicMock

import pytest

from ai.ai_tool_manager import AIToolExecutionError, AIToolAuthorizationDenied
from ai.tools.ai_tool_filesystem import AIToolFileSystem


class TestAIToolFileSystemDeleteFile:
    """Test the delete_file operation."""

    def test_delete_file_success(self, filesystem_tool, mock_authorization):
        """Test successful file deletion."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "delete_file", "path": "file.txt"},
                mock_authorization
            ))

            assert "File deleted successfully: file.txt" in result
            mock_unlink.assert_called_once()
            # Verify authorization was called with destructive=True
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is True  # destructive parameter

    def test_delete_file_not_exists(self, filesystem_tool, mock_authorization):
        """Test deleting non-existent file."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "nonexistent.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "File does not exist: nonexistent.txt" in str(error)

    def test_delete_file_not_file(self, filesystem_tool, mock_authorization):
        """Test deleting when path is not a file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is not a file: dir" in str(error)

    def test_delete_file_authorization_denied(self, filesystem_tool, mock_authorization_denied):
        """Test deleting file when authorization is denied."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "file.txt"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to delete file: file.txt" in str(error)

    def test_delete_file_already_deleted(self, filesystem_tool, mock_authorization):
        """Test deleting file that was already deleted (race condition)."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_unlink.side_effect = OSError("File not found")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to delete file" in str(error)

    def test_delete_file_permission_error(self, filesystem_tool, mock_authorization):
        """Test deleting file with permission error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_unlink.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied deleting file" in str(error)

    def test_delete_file_os_error(self, filesystem_tool, mock_authorization):
        """Test deleting file with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_unlink.side_effect = OSError("I/O error")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to delete file" in str(error)


class TestAIToolFileSystemCopyFile:
    """Test the copy_file operation."""

    def test_copy_file_success_new_destination(self, custom_path_resolver, mock_authorization):
        """Test successful file copying to new destination."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

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

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"},
                mock_authorization
            ))

            assert "File copied successfully: source.txt -> dest.txt" in result
            mock_copy2.assert_called_once()
            # Verify authorization was called with destructive=False for new destination
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is False  # destructive parameter

    def test_copy_file_success_overwrite_destination(self, custom_path_resolver, mock_authorization):
        """Test successful file copying with overwrite."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

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

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"},
                mock_authorization
            ))

            assert "File copied successfully: source.txt -> dest.txt" in result
            # Verify authorization was called with destructive=True for existing destination
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is True  # destructive parameter

    def test_copy_file_source_not_exists(self, filesystem_tool, mock_authorization):
        """Test copying non-existent source file."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "nonexistent.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source file does not exist: nonexistent.txt" in str(error)

    def test_copy_file_source_not_file(self, filesystem_tool, mock_authorization):
        """Test copying when source is not a file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "dir", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source path is not a file: dir" in str(error)

    def test_copy_file_no_destination(self, filesystem_tool, mock_authorization):
        """Test copying without destination parameter."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:
            mock_exists.return_value = True
            mock_is_file.return_value = True

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "source.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "No 'destination' argument provided" in str(error)

    def test_copy_file_source_too_large(self, filesystem_tool, mock_authorization):
        """Test copying file that exceeds size limit."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 11 * 1024 * 1024  # 11MB, exceeds 10MB limit
            mock_stat.return_value = mock_stat_result

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "large.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source file too large: 11.0MB (max: 10.0MB)" in str(error)

    def test_copy_file_authorization_denied(self, custom_path_resolver, mock_authorization_denied):
        """Test copying file when authorization is denied."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

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

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to copy file: source.txt -> dest.txt" in str(error)

    def test_copy_file_permission_error(self, custom_path_resolver, mock_authorization):
        """Test copying file with permission error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

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

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied copying file" in str(error)

    def test_copy_file_os_error(self, custom_path_resolver, mock_authorization):
        """Test copying file with OS error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

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

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to copy file" in str(error)


class TestAIToolFileSystemMove:
    """Test the move operation."""

    def test_move_file_success(self, custom_path_resolver, mock_authorization):
        """Test successful file moving."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "move", "path": "source.txt", "destination": "dest.txt"},
                mock_authorization
            ))

            assert "Moved successfully: source.txt -> dest.txt" in result
            mock_rename.assert_called_once()
            # Verify authorization was called with destructive=True
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is True  # destructive parameter

    def test_move_directory_success(self, custom_path_resolver, mock_authorization):
        """Test successful directory moving."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source_dir": ("/test/mindspace/source_dir", "source_dir"),
            "dest_dir": ("/test/mindspace/dest_dir", "dest_dir")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "move", "path": "source_dir", "destination": "dest_dir"},
                mock_authorization
            ))

            assert "Moved successfully: source_dir -> dest_dir" in result
            mock_rename.assert_called_once()

    def test_move_source_not_exists(self, filesystem_tool, mock_authorization):
        """Test moving non-existent source."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "nonexistent", "destination": "dest"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source path does not exist: nonexistent" in str(error)

    def test_move_no_destination(self, filesystem_tool, mock_authorization):
        """Test moving without destination parameter."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = True

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "No 'destination' argument provided" in str(error)

    def test_move_authorization_denied(self, custom_path_resolver, mock_authorization_denied):
        """Test moving when authorization is denied."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = True

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to move: source.txt -> dest.txt" in str(error)

    def test_move_with_parent_creation(self, custom_path_resolver, mock_authorization):
        """Test moving with parent directory creation."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "new_dir/dest.txt": ("/test/mindspace/new_dir/dest.txt", "new_dir/dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir, \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "move", "path": "source.txt", "destination": "new_dir/dest.txt"},
                mock_authorization
            ))

            assert "Moved successfully: source.txt -> new_dir/dest.txt" in result
            # Verify parent directory creation was attempted
            mock_mkdir.assert_called_once_with(parents=True, exist_ok=True)

    def test_move_permission_error(self, custom_path_resolver, mock_authorization):
        """Test moving with permission error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_rename.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied moving" in str(error)

    def test_move_os_error(self, custom_path_resolver, mock_authorization):
        """Test moving with OS error."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/mindspace/source.txt", "source.txt"),
            "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_exists.return_value = True
            mock_rename.side_effect = OSError("Cross-device link")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to move" in str(error)
