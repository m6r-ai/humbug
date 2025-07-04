"""
Tests for filesystem tool info operations: get_info.
"""
import asyncio
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

from humbug.ai.ai_tool_manager import AIToolExecutionError


class TestToolFileSystemGetInfo:
    """Test the get_info operation."""

    def test_get_info_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file info retrieval."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 1024
            mock_stat_result.st_mtime = 1640995200.0  # 2022-01-01 00:00:00
            mock_stat_result.st_mode = 0o100644  # Regular file with 644 permissions
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "file.txt"},
                mock_authorization
            ))

            assert "File: file.txt" in result
            assert "Type: File" in result
            assert "Size: 1,024 bytes" in result
            assert "Modified: 2022-01-01T00:00:00" in result
            assert "Permissions: 644" in result
            assert "Extension: .txt" in result

    def test_get_info_file_no_extension(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test file info retrieval for file without extension."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/README"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "README"
        mock_mindspace_manager.get_relative_path.return_value = "README"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/README")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 512
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o100644
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "README"},
                mock_authorization
            ))

            assert "File: README" in result
            assert "Extension: None" in result

    def test_get_info_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory info retrieval."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir"
        mock_mindspace_manager.get_relative_path.return_value = "dir"

        # Mock directory items
        mock_file = MagicMock()
        mock_file.is_file.return_value = True
        mock_file.is_dir.return_value = False

        mock_subdir = MagicMock()
        mock_subdir.is_file.return_value = False
        mock_subdir.is_dir.return_value = True

        mock_other = MagicMock()
        mock_other.is_file.return_value = False
        mock_other.is_dir.return_value = False

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file, mock_subdir, mock_other]

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o040755  # Directory with 755 permissions
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "dir"},
                mock_authorization
            ))

            assert "Directory: dir" in result
            assert "Type: Directory" in result
            assert "Items: 3 total (1 files, 1 directories)" in result
            assert "Modified: 2022-01-01T00:00:00" in result
            assert "Permissions: 755" in result

    def test_get_info_directory_empty(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test directory info retrieval for empty directory."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/empty_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "empty_dir"
        mock_mindspace_manager.get_relative_path.return_value = "empty_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/empty_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o040755
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "empty_dir"},
                mock_authorization
            ))

            assert "Items: 0 total (0 files, 0 directories)" in result

    def test_get_info_directory_permission_denied_listing(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test directory info retrieval with permission denied for listing."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/protected_dir"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "protected_dir"
        mock_mindspace_manager.get_relative_path.return_value = "protected_dir"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/protected_dir")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = PermissionError("Access denied")

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o040755
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "protected_dir"},
                mock_authorization
            ))

            assert "Items: Permission denied" in result

    def test_get_info_other_type(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test info retrieval for other types (neither file nor directory)."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/special"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "special"
        mock_mindspace_manager.get_relative_path.return_value = "special"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/special")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o020666  # Character device
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "special"},
                mock_authorization
            ))

            assert "Path: special" in result
            assert "Type: Other (neither file nor directory)" in result
            assert "Modified: 2022-01-01T00:00:00" in result
            assert "Permissions: 666" in result

    def test_get_info_path_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test info retrieval for non-existent path."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/nonexistent"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "nonexistent"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/nonexistent")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "get_info", "path": "nonexistent"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path does not exist: nonexistent" in str(error)

    def test_get_info_permission_error_stat(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test info retrieval with permission error on stat."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/protected"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "protected"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_path = Path("/test/mindspace/protected")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_stat.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "get_info", "path": "protected"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied getting info" in str(error)

    def test_get_info_os_error_stat(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test info retrieval with OS error on stat."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/broken"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "broken"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_path = Path("/test/mindspace/broken")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_stat.side_effect = OSError("I/O error")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "get_info", "path": "broken"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to get info" in str(error)

    def test_get_info_large_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test info retrieval for large file with proper formatting."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/large.bin"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "large.bin"
        mock_mindspace_manager.get_relative_path.return_value = "large.bin"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/large.bin")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 1234567890  # Large file size
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o100644
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "large.bin"},
                mock_authorization
            ))

            # Check that large numbers are formatted with commas
            assert "Size: 1,234,567,890 bytes" in result

    def test_get_info_different_permissions(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test info retrieval with different permission modes."""
        test_cases = [
            (0o100755, "755"),  # Executable file
            (0o100600, "600"),  # Read-write only for owner
            (0o040700, "700"),  # Directory with owner-only access
            (0o100444, "444"),  # Read-only file
        ]

        for mode, expected_perms in test_cases:
            mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/test_file"
            mock_mindspace_manager.get_mindspace_relative_path.return_value = "test_file"
            mock_mindspace_manager.get_relative_path.return_value = "test_file"

            with patch('pathlib.Path.resolve') as mock_resolve, \
                 patch('pathlib.Path.exists') as mock_exists, \
                 patch('pathlib.Path.is_file') as mock_is_file, \
                 patch('pathlib.Path.is_dir') as mock_is_dir, \
                 patch('pathlib.Path.stat') as mock_stat, \
                 patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

                mock_path = Path("/test/mindspace/test_file")
                mock_resolve.return_value = mock_path
                mock_exists.return_value = True
                mock_is_file.return_value = True
                mock_is_dir.return_value = False

                mock_stat_result = MagicMock()
                mock_stat_result.st_size = 100
                mock_stat_result.st_mtime = 1640995200.0
                mock_stat_result.st_mode = mode
                mock_stat.return_value = mock_stat_result

                mock_datetime = MagicMock()
                mock_datetime.isoformat.return_value = "2022-01-01T00:00:00"
                mock_fromtimestamp.return_value = mock_datetime

                result = asyncio.run(filesystem_tool.execute(
                    {"operation": "get_info", "path": "test_file"},
                    mock_authorization
                ))

                assert f"Permissions: {expected_perms}" in result

    def test_get_info_datetime_formatting(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test that datetime formatting works correctly."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('datetime.datetime.fromtimestamp') as mock_fromtimestamp:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat_result.st_mtime = 1672531200.0  # 2023-01-01 00:00:00
            mock_stat_result.st_mode = 0o100644
            mock_stat.return_value = mock_stat_result

            mock_datetime = MagicMock()
            mock_datetime.isoformat.return_value = "2023-01-01T00:00:00"
            mock_fromtimestamp.return_value = mock_datetime

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "get_info", "path": "file.txt"},
                mock_authorization
            ))

            # Verify that fromtimestamp was called with the correct timestamp
            mock_fromtimestamp.assert_called_with(1672531200.0)
            assert "Modified: 2023-01-01T00:00:00" in result
