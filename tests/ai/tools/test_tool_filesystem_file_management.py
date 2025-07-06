"""
Tests for filesystem tool file management operations: delete_file, copy_file, move.
"""
import asyncio
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

from humbug.ai.ai_tool_manager import AIToolExecutionError, AIToolAuthorizationDenied


class TestAIToolFileSystemDeleteFile:
    """Test the delete_file operation."""

    def test_delete_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file deletion."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

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

    def test_delete_file_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test deleting non-existent file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/nonexistent.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "nonexistent.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/nonexistent.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "nonexistent.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "File does not exist: nonexistent.txt" in str(error)

    def test_delete_file_not_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test deleting when path is not a file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "dir"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is not a file: dir" in str(error)

    def test_delete_file_authorization_denied(self, filesystem_tool, mock_mindspace_manager, mock_authorization_denied):
        """Test deleting file when authorization is denied."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "file.txt"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to delete file: file.txt" in str(error)

    def test_delete_file_already_deleted(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test deleting file that was already deleted (race condition)."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path

            # First exists() call returns True, second returns False (after unlink fails)
            mock_exists.side_effect = [True, False]
            mock_is_file.return_value = True
            mock_unlink.side_effect = OSError("File not found")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to delete file" in str(error)

    def test_delete_file_permission_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test deleting file with permission error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            mock_unlink.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "delete_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied deleting file" in str(error)

    def test_delete_file_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test deleting file with OS error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

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

    def test_copy_file_success_new_destination(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file copying to new destination."""
        call_count = [0]  # Use list to make it mutable in nested function

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_get_relative_path(path):
            if "source.txt" in str(path):
                return "source.txt"

            if "dest.txt" in str(path):
                return "dest.txt"

            return str(path).split("/")[-1]

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        def mock_exists():
            # Return True for source, False for destination
            return call_count[0] == 1

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]
        mock_mindspace_manager.get_relative_path.side_effect = mock_get_relative_path

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', side_effect=mock_exists), \
             patch('pathlib.Path.is_file', return_value=True), \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

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

    def test_copy_file_success_overwrite_destination(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file copying with overwrite."""
        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_get_relative_path(path):
            if "source.txt" in str(path):
                return "source.txt"

            if "dest.txt" in str(path):
                return "dest.txt"

            return str(path).split("/")[-1]

        call_count = [0]
        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]
        mock_mindspace_manager.get_relative_path.side_effect = mock_get_relative_path

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', return_value=True), \
             patch('pathlib.Path.is_file', return_value=True), \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

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

    def test_copy_file_source_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying non-existent source file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/nonexistent.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "nonexistent.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/nonexistent.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "nonexistent.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source file does not exist: nonexistent.txt" in str(error)

    def test_copy_file_source_not_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying when source is not a file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "dir", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source path is not a file: dir" in str(error)

    def test_copy_file_no_destination(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying without destination parameter."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/source.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "source.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:
            mock_path = Path("/test/mindspace/source.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "copy_file", "path": "source.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "No 'destination' argument provided" in str(error)

    def test_copy_file_source_too_large(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying file that exceeds size limit."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/large.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "large.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_path = Path("/test/mindspace/large.txt")
            mock_resolve.return_value = mock_path
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

    def test_copy_file_authorization_denied(self, filesystem_tool, mock_mindspace_manager, mock_authorization_denied):
        """Test copying file when authorization is denied."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        def mock_exists():
            # Return True for source, False for destination
            return call_count[0] == 1

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', side_effect=mock_exists), \
             patch('pathlib.Path.is_file', return_value=True), \
             patch('pathlib.Path.stat') as mock_stat:

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

    def test_copy_file_permission_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying file with permission error."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        def mock_exists():
            # Return True for source, False for destination
            return call_count[0] == 1

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', side_effect=mock_exists), \
             patch('pathlib.Path.is_file', return_value=True), \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

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

    def test_copy_file_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying file with OS error."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        def mock_exists():
            # Return True for source, False for destination
            return call_count[0] == 1

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', side_effect=mock_exists), \
             patch('pathlib.Path.is_file', return_value=True), \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.mkdir'), \
             patch('shutil.copy2') as mock_copy2:

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

    def test_move_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file moving."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_get_relative_path(path):
            if "source.txt" in str(path):
                return "source.txt"

            if "dest.txt" in str(path):
                return "dest.txt"

            return str(path).split("/")[-1]

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]
        mock_mindspace_manager.get_relative_path.side_effect = mock_get_relative_path

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', return_value=True), \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

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

    def test_move_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory moving."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source_dir":
                return "/test/mindspace/source_dir"

            if path == "dest_dir":
                return "/test/mindspace/dest_dir"

            return f"/test/mindspace/{path}"

        def mock_get_relative_path(path):
            if "source_dir" in str(path):
                return "source_dir"

            if "dest_dir" in str(path):
                return "dest_dir"

            return str(path).split("/")[-1]

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source_dir")

            return Path("/test/mindspace/dest_dir")

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source_dir", "dest_dir"]
        mock_mindspace_manager.get_relative_path.side_effect = mock_get_relative_path

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', return_value=True), \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "move", "path": "source_dir", "destination": "dest_dir"},
                mock_authorization
            ))

            assert "Moved successfully: source_dir -> dest_dir" in result
            mock_rename.assert_called_once()

    def test_move_source_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test moving non-existent source."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/nonexistent"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "nonexistent"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/nonexistent")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "nonexistent", "destination": "dest"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source path does not exist: nonexistent" in str(error)

    def test_move_no_destination(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test moving without destination parameter."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/source.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "source.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:
            mock_path = Path("/test/mindspace/source.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "No 'destination' argument provided" in str(error)

    def test_move_authorization_denied(self, filesystem_tool, mock_mindspace_manager, mock_authorization_denied):
        """Test moving when authorization is denied."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', return_value=True):

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to move: source.txt -> dest.txt" in str(error)

    def test_move_with_parent_creation(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test moving with parent directory creation."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "new_dir/dest.txt":
                return "/test/mindspace/new_dir/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_get_relative_path(path):
            if "source.txt" in str(path):
                return "source.txt"

            if "new_dir/dest.txt" in str(path):
                return "new_dir/dest.txt"

            return str(path).split("/")[-1]

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/new_dir/dest.txt")

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "new_dir/dest.txt"]
        mock_mindspace_manager.get_relative_path.side_effect = mock_get_relative_path

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', return_value=True), \
             patch('pathlib.Path.mkdir') as mock_mkdir, \
             patch('pathlib.Path.rename') as mock_rename:

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "move", "path": "source.txt", "destination": "new_dir/dest.txt"},
                mock_authorization
            ))

            assert "Moved successfully: source.txt -> new_dir/dest.txt" in result
            # Verify parent directory creation was attempted
            mock_mkdir.assert_called_once_with(parents=True, exist_ok=True)

    def test_move_permission_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test moving with permission error."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', return_value=True), \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_rename.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied moving" in str(error)

    def test_move_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test moving with OS error."""
        call_count = [0]

        def mock_get_absolute_path(path):
            if path == "source.txt":
                return "/test/mindspace/source.txt"

            if path == "dest.txt":
                return "/test/mindspace/dest.txt"

            return f"/test/mindspace/{path}"

        def mock_resolve():
            call_count[0] += 1
            if call_count[0] == 1:  # First call for source
                return Path("/test/mindspace/source.txt")

            return Path("/test/mindspace/dest.txt")

        mock_mindspace_manager.get_absolute_path.side_effect = mock_get_absolute_path
        mock_mindspace_manager.get_mindspace_relative_path.side_effect = ["source.txt", "dest.txt"]

        with patch('pathlib.Path.resolve', side_effect=mock_resolve), \
             patch('pathlib.Path.exists', return_value=True), \
             patch('pathlib.Path.mkdir'), \
             patch('pathlib.Path.rename') as mock_rename:

            mock_rename.side_effect = OSError("Cross-device link")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "move", "path": "source.txt", "destination": "dest.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to move" in str(error)
