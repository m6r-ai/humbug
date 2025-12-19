"""
Tests for filesystem tool info operations: get_info.
"""
import json
import asyncio
from datetime import timezone
from unittest.mock import patch, MagicMock

import pytest

from ai_tool import AIToolExecutionError


class TestFileSystemAIToolGetInfo:
    """Test the get_info operation."""

    def test_get_info_file_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful file info retrieval."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 1024
            mock_stat_result.st_mtime = 1640995200.0  # 2022-01-01 00:00:00
            mock_stat_result.st_mode = 0o100644  # Regular file with 644 permissions
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "file.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            info = json.loads(result.content)
            assert info["path"] == "file.txt"
            assert info["type"] == "file"
            assert info["size_bytes"] == 1024
            assert info["modified"] == "2022-01-01T00:00:00"
            assert info["permissions"] == "644"
            assert info["extension"] == ".txt"
            assert result.context == "json"

    def test_get_info_file_no_extension(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test file info retrieval for file without extension."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.suffix', new_callable=lambda: ""), \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 512
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o100644
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "README"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            info = json.loads(result.content)
            assert info["path"] == "README"
            assert info["type"] == "file"
            assert info["extension"] is None

    def test_get_info_directory_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful directory info retrieval."""
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

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True
            mock_iterdir.return_value = [mock_file, mock_subdir, mock_other]

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o040755  # Directory with 755 permissions
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            info = json.loads(result.content)
            assert info["path"] == "dir"
            assert info["type"] == "directory"
            assert info["items"]["total"] == 3
            assert info["items"]["files"] == 1
            assert info["items"]["directories"] == 1
            assert info["permissions"] == "755"

    def test_get_info_directory_empty(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test directory info retrieval for empty directory."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True
            mock_iterdir.return_value = []

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o040755
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "empty_dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            info = json.loads(result.content)
            assert info["path"] == "empty_dir"
            assert info["items"]["total"] == 0
            assert info["items"]["files"] == 0
            assert info["items"]["directories"] == 0

    def test_get_info_directory_permission_denied_listing(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test directory info retrieval with permission denied for listing."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('pathlib.Path.iterdir') as mock_iterdir, \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = True
            mock_iterdir.side_effect = PermissionError("Access denied")

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o040755
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "protected_dir"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            info = json.loads(result.content)
            assert info["path"] == "protected_dir"
            assert info["type"] == "directory"
            assert info["items"]["error"] == "Permission denied"

    def test_get_info_other_type(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test info retrieval for other types (neither file nor directory)."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = False
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o020666  # Character device
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "special"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON
            info = json.loads(result.content)
            assert info["path"] == "special"
            assert info["type"] == "other"
            assert info["permissions"] == "666"

    def test_get_info_path_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test info retrieval for non-existent path."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "nonexistent"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Path does not exist: nonexistent" in str(error)

    def test_get_info_permission_error_stat(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test info retrieval with permission error on stat."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_exists.return_value = True
            mock_stat.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "protected"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied getting info" in str(error)

    def test_get_info_os_error_stat(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test info retrieval with OS error on stat."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_exists.return_value = True
            mock_stat.side_effect = OSError("I/O error")

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "broken"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to get info" in str(error)

    def test_get_info_large_file(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test info retrieval for large file with proper formatting."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 1234567890  # Large file size
            mock_stat_result.st_mtime = 1640995200.0
            mock_stat_result.st_mode = 0o100644
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "large.bin"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # New format returns JSON with size_bytes (no comma formatting)
            info = json.loads(result.content)
            assert info["path"] == "large.bin"
            assert info["type"] == "file"
            assert info["size_bytes"] == 1234567890

    def test_get_info_different_permissions(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test info retrieval with different permission modes."""
        test_cases = [
            (0o100755, "755"),  # Executable file
            (0o100600, "600"),  # Read-write only for owner
            (0o040700, "700"),  # Directory with owner-only access
            (0o100444, "444"),  # Read-only file
        ]

        for mode, expected_perms in test_cases:
            with patch('pathlib.Path.exists') as mock_exists, \
                 patch('pathlib.Path.is_file') as mock_is_file, \
                 patch('pathlib.Path.is_dir') as mock_is_dir, \
                 patch('pathlib.Path.stat') as mock_stat, \
                 patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

                mock_exists.return_value = True
                mock_is_file.return_value = True
                mock_is_dir.return_value = False

                mock_stat_result = MagicMock()
                mock_stat_result.st_size = 100
                mock_stat_result.st_mtime = 1640995200.0
                mock_stat_result.st_mode = mode
                mock_stat.return_value = mock_stat_result

                mock_datetime_instance = MagicMock()
                mock_datetime_instance.isoformat.return_value = "2022-01-01T00:00:00"
                mock_datetime.fromtimestamp.return_value = mock_datetime_instance

                tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "test_file"})
                result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

                # New format returns JSON
                info = json.loads(result.content)
                assert info["permissions"] == expected_perms

    def test_get_info_datetime_formatting(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test that datetime formatting works correctly."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.is_dir') as mock_is_dir, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('ai_tool.filesystem.filesystem_ai_tool.datetime') as mock_datetime:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_is_dir.return_value = False

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat_result.st_mtime = 1672531200.0  # 2023-01-01 00:00:00
            mock_stat_result.st_mode = 0o100644
            mock_stat.return_value = mock_stat_result

            mock_datetime_instance = MagicMock()
            mock_datetime_instance.isoformat.return_value = "2023-01-01T00:00:00"
            mock_datetime.fromtimestamp.return_value = mock_datetime_instance

            tool_call = make_tool_call("filesystem", {"operation": "get_info", "path": "file.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # Verify that fromtimestamp was called with the correct timestamp
            mock_datetime.fromtimestamp.assert_called_with(1672531200.0, tz=timezone.utc)

            # New format returns JSON
            info = json.loads(result.content)
            assert info["modified"] == "2023-01-01T00:00:00"
