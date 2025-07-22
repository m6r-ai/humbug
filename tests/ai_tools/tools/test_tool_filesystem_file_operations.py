"""
Tests for filesystem tool file operations: read_file, write_file, append_to_file.
"""
import asyncio
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied


class TestFileSystemAIToolReadFile:
    """Test the read_file operation."""

    def test_read_file_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful file reading."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "file.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File: file.txt" in result.content
            assert "Size: 12 bytes" in result.content
            assert "Encoding: utf-8" in result.content
            assert "test content" in result.content

    def test_read_file_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test reading non-existent file."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "nonexistent.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "File does not exist: nonexistent.txt" in str(error)

    def test_read_file_not_file(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test reading when path is not a file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "dir"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Path is not a file: dir" in str(error)

    def test_read_file_too_large(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test reading file that exceeds size limit."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 11 * 1024 * 1024  # 11MB, exceeds 10MB limit
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "large.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "File too large: 11.0MB (max: 10.0MB)" in str(error)

    def test_read_file_custom_encoding(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test reading file with custom encoding."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "file.txt", "encoding": "utf-16"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Encoding: utf-16" in result.content
            assert "test content" in result.content
            # Verify the encoding was passed to open
            mock_file.assert_called_with(Path("/test/sandbox/file.txt"), 'r', encoding='utf-16')

    def test_read_file_unicode_decode_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test reading file with unicode decode error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise UnicodeDecodeError
            mock_file.side_effect = UnicodeDecodeError('utf-8', b'', 0, 1, 'invalid start byte')

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to decode file with encoding 'utf-8'" in str(error)

    def test_read_file_permission_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test reading file with permission error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise PermissionError
            mock_file.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied reading file" in str(error)

    def test_read_file_os_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test reading file with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise OSError
            mock_file.side_effect = OSError("Disk error")

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to read file" in str(error)


class TestFileSystemAIToolWriteFile:
    """Test the write_file operation."""

    def test_write_file_success_new_file(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful writing to new file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_exists.return_value = False  # New file

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (12 bytes)" in result.content
            # Verify authorization was called with destructive=False for new file
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is False  # destructive parameter

    def test_write_file_success_overwrite(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful writing to existing file (overwrite)."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_exists.return_value = True  # Existing file

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (12 bytes)" in result.content
            # Verify authorization was called with destructive=True for existing file
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is True  # destructive parameter

    def test_write_file_no_content(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file without content parameter."""
        tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "No 'content' argument provided" in str(error)

    def test_write_file_non_string_content(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with non-string content."""
        tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": 123})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "'content' must be a string" in str(error)

    def test_write_file_content_too_large(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with content that exceeds size limit."""
        # Create content larger than 10MB
        large_content = "x" * (11 * 1024 * 1024)

        tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": large_content})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "'content' too large: 11.0MB (max: 10.0MB)" in str(error)

    def test_write_file_custom_encoding(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with custom encoding."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_exists.return_value = False

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content", "encoding": "utf-16"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully" in result.content
            # Verify the encoding was passed to NamedTemporaryFile
            mock_temp_file.assert_called_once()
            kwargs = mock_temp_file.call_args[1]
            assert kwargs['encoding'] == 'utf-16'

    def test_write_file_create_parents(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with create_parents option."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_exists.return_value = False

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "dir/file.txt", "content": "test content", "create_parents": True})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully" in result.content
            # Verify mkdir was called with parents=True
            mock_mkdir.assert_called_once_with(parents=True, exist_ok=True)

    def test_write_file_authorization_denied(self, filesystem_tool, mock_authorization_denied, make_tool_call):
        """Test writing file when authorization is denied."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization_denied))

            error = exc_info.value
            assert "User denied permission to write file: file.txt" in str(error)

    def test_write_file_permission_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with permission error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file:

            mock_exists.return_value = False

            # Mock tempfile to raise PermissionError
            mock_temp_file.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied writing file" in str(error)

    def test_write_file_os_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file:

            mock_exists.return_value = False

            # Mock tempfile to raise OSError
            mock_temp_file.side_effect = OSError("Disk full")

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to write file" in str(error)


class TestFileSystemAIToolAppendFile:
    """Test the append_to_file operation."""

    def test_append_file_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful appending to file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open()) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100  # Current file size
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": "new content"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Content appended successfully: file.txt (+11 bytes)" in result.content
            # Verify the file was opened in append mode
            mock_file.assert_called_with(Path("/test/sandbox/file.txt"), 'a', encoding='utf-8')

    def test_append_file_not_exists(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test appending to non-existent file."""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": "content"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "File does not exist: file.txt" in str(error)

    def test_append_file_not_file(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test appending to path that is not a file."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_exists.return_value = True
            mock_is_file.return_value = False

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "dir", "content": "content"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Path is not a file: dir" in str(error)

    def test_append_file_too_large_after_append(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test appending when result would exceed size limit."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 9 * 1024 * 1024  # 9MB current
            mock_stat.return_value = mock_stat_result

            # Content that would make total > 10MB
            large_content = "x" * (2 * 1024 * 1024)  # 2MB

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": large_content})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "File would be too large after append" in str(error)

    def test_append_file_custom_encoding(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test appending with custom encoding."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open()) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": "content", "encoding": "utf-16"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Content appended successfully" in result.content
            # Verify the encoding was passed to open
            mock_file.assert_called_with(Path("/test/sandbox/file.txt"), 'a', encoding='utf-16')

    def test_append_file_no_content(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test appending without content parameter."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:
            mock_exists.return_value = True
            mock_is_file.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "No 'content' argument provided" in str(error)

    def test_append_file_authorization_denied(self, filesystem_tool, mock_authorization_denied, make_tool_call):
        """Test appending when authorization is denied."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": "content"})
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization_denied))

            error = exc_info.value
            assert "User denied permission to append to file: file.txt" in str(error)

    def test_append_file_permission_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test appending with permission error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise PermissionError
            mock_file.side_effect = PermissionError("Access denied")

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": "content"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Permission denied appending to file" in str(error)

    def test_append_file_os_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test appending with OS error."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise OSError
            mock_file.side_effect = OSError("Disk error")

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": "content"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to append to file" in str(error)
