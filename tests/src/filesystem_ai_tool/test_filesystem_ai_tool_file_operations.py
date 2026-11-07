"""
Tests for filesystem tool file operations: read_file, write_file, append_to_file.
"""
import asyncio
import json
from pathlib import Path
from typing import Any
from unittest.mock import patch, MagicMock, mock_open

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied
from filesystem_ai_tool.filesystem_access_settings import FilesystemAccessSettings
from filesystem_ai_tool.filesystem_ai_tool import FileSystemAITool
from mindspace.mindspace_error import MindspaceHumbugPathError


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

            # New format returns just the content
            assert result.content == "test content"
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

    def test_read_file_too_large(self, bounded_filesystem_tool, mock_authorization, make_tool_call):
        """Test reading a file that would hold more content in memory than the load limit allows."""
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
                asyncio.run(bounded_filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Operation would hold 11.0MB of content in memory (limit: 10.0MB)" in str(error)
            assert "read_file_lines" in str(error)

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

            # New format returns just the content (encoding info not in output)
            assert result.content == "test content"
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
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

            mock_exists.return_value = False  # New file

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (13 bytes)" in result.content
            # Verify authorization was called with destructive=False for new file
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[4] is False  # destructive parameter

            # Verify chmod was called
            mock_chmod.assert_called_once()

    def test_write_file_success_overwrite(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test successful writing to existing file (overwrite)."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

            mock_exists.return_value = True  # Existing file

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (13 bytes)" in result.content
            # Verify authorization was called with destructive=True for existing file
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[4] is True  # destructive parameter

            # Verify chmod was called
            mock_chmod.assert_called_once()

    def test_write_file_no_content(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file without content parameter."""
        tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "content" in str(error).lower() and "missing" in str(error).lower()

    def test_write_file_non_string_content(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with non-string content."""
        tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": 123})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "'content' must be a string" in str(error)

    def test_write_file_content_too_large(self, bounded_filesystem_tool, mock_authorization, make_tool_call):
        """Test writing content that would hold more in memory than the load limit allows."""
        # Create content larger than 10MB
        large_content = "x" * (11 * 1024 * 1024)

        tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": large_content})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(bounded_filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "Operation would hold 11.0MB of content in memory (limit: 10.0MB)" in str(error)

    def test_write_file_custom_encoding(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with custom encoding."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

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

            # Verify chmod was called
            mock_chmod.assert_called_once()

    def test_write_file_create_parents(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test writing file with create_parents option."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

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

            # Verify chmod was called
            mock_chmod.assert_called_once()

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

            assert "Content appended successfully: file.txt (+12 bytes)" in result.content
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

    def test_append_content_too_large(self, bounded_filesystem_tool, mock_authorization, make_tool_call):
        """Appending content that would exceed the load limit is refused."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open()):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=0)

            large_content = "x" * (11 * 1024 * 1024)

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": large_content})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(bounded_filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Operation would hold 11.0MB of content in memory" in str(error)

    def test_append_to_file_larger_than_load_limit(self, filesystem_tool, mock_authorization, make_tool_call):
        """Appending is bounded by the content supplied, not by the size of the file it lands in."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open()) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=9 * 1024 * 1024)

            tool_call = make_tool_call("filesystem", {"operation": "append_to_file", "path": "file.txt", "content": "one more line"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Content appended successfully: file.txt" in result.content
            assert any(call.args[1] == 'a' for call in mock_file.call_args_list)

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
            assert "content" in str(error).lower() and "missing" in str(error).lower()

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

class TestFileSystemAIToolTrailingNewline:
    """Tests for trailing newline enforcement in write and append operations."""

    def test_write_file_adds_trailing_newline(self, filesystem_tool, mock_authorization, make_tool_call):
        """write_file appends a newline when content lacks one."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

            mock_exists.return_value = False

            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {
                "operation": "write_file",
                "path": "file.txt",
                "content": "hello"
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (6 bytes)" in result.content
            mock_temp_instance.write.assert_called_with("hello\n")

    def test_write_file_preserves_existing_trailing_newline(self, filesystem_tool, mock_authorization, make_tool_call):
        """write_file does not add an extra newline when content already ends with one."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

            mock_exists.return_value = False

            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {
                "operation": "write_file",
                "path": "file.txt",
                "content": "hello\n"
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (6 bytes)" in result.content
            mock_temp_instance.write.assert_called_with("hello\n")

    def test_write_file_crlf_content_gets_crlf_termination(self, filesystem_tool, mock_authorization, make_tool_call):
        """write_file appends \\r\\n when content uses Windows line endings."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

            mock_exists.return_value = False

            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {
                "operation": "write_file",
                "path": "file.txt",
                "content": "line1\r\nline2"
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            mock_temp_instance.write.assert_called_with("line1\r\nline2\r\n")

    def test_write_file_empty_content_unchanged(self, filesystem_tool, mock_authorization, make_tool_call):
        """write_file does not add a newline to empty content."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace, \
             patch('pathlib.Path.chmod') as mock_chmod:

            mock_exists.return_value = False

            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            tool_call = make_tool_call("filesystem", {
                "operation": "write_file",
                "path": "file.txt",
                "content": ""
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (0 bytes)" in result.content
            mock_temp_instance.write.assert_called_with("")

    def test_append_prepends_newline_when_file_lacks_one(self, filesystem_tool, mock_authorization, make_tool_call):
        """append_to_file prepends a newline when the existing file does not end with one."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=b'x')) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 5
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {
                "operation": "append_to_file",
                "path": "file.txt",
                "content": "new line"
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Content appended successfully: file.txt (+9 bytes)" in result.content
            file_handle = mock_file()
            file_handle.write.assert_called_with("\nnew line")

    def test_append_no_prepended_newline_when_file_ends_with_newline(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """append_to_file does not prepend a newline when the file already ends with one."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=b'\n')) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 5
            mock_stat.return_value = mock_stat_result

            tool_call = make_tool_call("filesystem", {
                "operation": "append_to_file",
                "path": "file.txt",
                "content": "new line"
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "Content appended successfully: file.txt (+8 bytes)" in result.content
            file_handle = mock_file()
            file_handle.write.assert_called_with("new line")


class TestFileSystemAIToolHumbugPath:
    """Tests that .humbug/ paths are rejected outright, never offered for approval."""

    def _make_tool_with_humbug_resolver(self) -> Any:
        """Build a filesystem tool whose resolver rejects .humbug/ paths."""
        def resolver(path: str) -> tuple[Path, str]:
            raise MindspaceHumbugPathError(
                f"Path is inside the .humbug/ directory, which is managed by Humbug internally: {path}"
            )

        settings = FilesystemAccessSettings(
            allow_external_access=True,
            external_allowlist=[],
            external_denylist=[]
        )
        return FileSystemAITool(
            resolve_path=resolver,
            get_access_settings=lambda: settings,
            mindspace=MagicMock()
        )

    def test_read_humbug_path_is_rejected_without_authorization(self, mock_authorization, make_tool_call) -> None:
        """Reading a .humbug/ path raises an execution error and never requests approval."""
        tool = self._make_tool_with_humbug_resolver()
        tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": ".humbug/settings.json"})

        with pytest.raises(AIToolExecutionError, match="humbug"):
            asyncio.run(tool.execute(tool_call, "", mock_authorization))

        mock_authorization.assert_not_called()

    def test_write_humbug_path_is_rejected_without_authorization(self, mock_authorization, make_tool_call) -> None:
        """Writing a .humbug/ path raises an execution error and never requests approval."""
        tool = self._make_tool_with_humbug_resolver()
        tool_call = make_tool_call("filesystem", {
            "operation": "write_file",
            "path": ".humbug/settings.json",
            "content": "{}"
        })

        with pytest.raises(AIToolExecutionError, match="humbug"):
            asyncio.run(tool.execute(tool_call, "", mock_authorization))

        mock_authorization.assert_not_called()


class TestFileSystemAIToolReadFileResponseLimit:
    """Tests that read_file bounds the text it returns into the conversation."""

    def test_read_file_content_exceeding_limit_is_rejected(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """Content exceeding 64 KB is rejected with a hint naming how to narrow the read."""
        content = "x" * (70 * 1024)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=content)):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=len(content))

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "big.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        assert "exceeds the 64 KB response size limit" in str(exc_info.value)
        assert "read_file_lines" in str(exc_info.value)

    def test_read_file_content_within_limit_succeeds(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """Content just under 64 KB is returned whole."""
        content = "x" * ((64 * 1024) - 1)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=content)):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=len(content))

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "big.txt"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        assert result.content == content

    def test_read_file_pdf_text_exceeding_limit_is_rejected(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """A PDF whose extracted text exceeds 64 KB is rejected even though the PDF is small."""
        extracted_text = "x" * (70 * 1024)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=b"%PDF-1.4")), \
             patch('filesystem_ai_tool.filesystem_ai_tool.parse_pdf'), \
             patch('filesystem_ai_tool.filesystem_ai_tool.extract_text',
                   return_value=extracted_text):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=2048)

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "big.pdf"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        assert "exceeds the 64 KB response size limit" in str(exc_info.value)
        assert "cannot be read in parts" in str(exc_info.value)

    def test_read_file_docx_text_exceeding_limit_is_rejected(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """A DOCX whose extracted text exceeds 64 KB is rejected even though the DOCX is small."""
        extracted_text = "x" * (70 * 1024)

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=b"PK\x03\x04")), \
             patch('filesystem_ai_tool.filesystem_ai_tool.extract_docx_text',
                   return_value=extracted_text):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=4096)

            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "big.docx"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        assert "exceeds the 64 KB response size limit" in str(exc_info.value)
        assert "cannot be read in parts" in str(exc_info.value)


class TestFileSystemAIToolReadFileLinesResponseLimit:
    """Tests that read_file_lines bounds the lines it returns into the conversation."""

    def _large_file_content(self) -> str:
        """Build 1000 lines of 100 characters, about 101 KB."""
        return "\n".join("x" * 100 for _ in range(1000))

    def test_read_file_lines_exceeding_limit_is_rejected(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """Reading every line of a file whose lines exceed 64 KB is rejected."""
        content = self._large_file_content()

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=content)):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=len(content))

            tool_call = make_tool_call("filesystem", {"operation": "read_file_lines", "path": "big.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        assert "exceeds the 64 KB response size limit" in str(exc_info.value)
        assert "start_line and end_line" in str(exc_info.value)

    def test_read_file_lines_narrow_range_within_limit_succeeds(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """A range small enough to fit the limit is returned from a file too large to read whole."""
        content = self._large_file_content()

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=content)):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=len(content))

            tool_call = make_tool_call("filesystem", {
                "operation": "read_file_lines",
                "path": "big.txt",
                "start_line": 1,
                "end_line": 3
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        payload = json.loads(result.content)
        assert payload["range"] == "1-3"
        assert len(payload["lines"]) == 3
        assert payload["lines"]["1"] == "x" * 100

    def test_read_file_lines_range_from_file_larger_than_load_limit(
        self, filesystem_tool, mock_authorization, make_tool_call
    ):
        """A line range is read from a file larger than the loaded-content limit."""
        content = "".join(f"line {n}\n" for n in range(1, 1001))

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data=content)):

            mock_exists.return_value = True
            mock_is_file.return_value = True
            mock_stat.return_value = MagicMock(st_size=50 * 1024 * 1024)

            tool_call = make_tool_call("filesystem", {
                "operation": "read_file_lines",
                "path": "big.txt",
                "start_line": 500,
                "end_line": 501
            })
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        payload = json.loads(result.content)
        assert payload["lines"] == {"500": "line 500", "501": "line 501"}
