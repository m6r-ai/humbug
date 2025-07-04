"""
Tests for the filesystem tool
"""
import asyncio
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open

import pytest

from humbug.tools.tool_filesystem import ToolFileSystem
from humbug.ai.ai_tool_manager import (
    AITool, AIToolDefinition, AIToolParameter, AIToolExecutionError,
    AIToolAuthorizationDenied
)
from humbug.mindspace.mindspace_error import MindspaceNotFoundError


@pytest.fixture
def mock_mindspace_manager():
    """Fixture providing a mocked mindspace manager."""
    mock_manager = MagicMock()

    # Default behavior - mindspace is available
    mock_manager.has_mindspace.return_value = True
    mock_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
    mock_manager.get_mindspace_relative_path.return_value = "file.txt"
    mock_manager.get_relative_path.return_value = "file.txt"

    return mock_manager


@pytest.fixture
def filesystem_tool(mock_mindspace_manager):
    """Fixture providing a filesystem tool instance with mocked mindspace manager."""
    tool = ToolFileSystem()
    tool._mindspace_manager = mock_mindspace_manager
    return tool


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    mock = MagicMock()

    async def mock_auth_callback(_tool_name, _arguments, _context, _destructive):
        return True  # Default to authorized

    mock.side_effect = mock_auth_callback
    return mock


@pytest.fixture
def mock_authorization_denied():
    """Fixture providing a mocked authorization callback that denies requests."""
    mock = MagicMock()

    async def mock_auth_callback(_tool_name, _arguments, _context, _destructive):
        return False  # Always deny

    mock.side_effect = mock_auth_callback
    return mock


@pytest.fixture
def temp_file():
    """Fixture providing a temporary file for testing."""
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
        f.write("test content")
        temp_path = Path(f.name)

    yield temp_path

    # Cleanup
    if temp_path.exists():
        temp_path.unlink()


class TestToolFileSystemDefinition:
    """Test the filesystem tool definition."""

    def test_get_definition_returns_correct_structure(self):
        """Test that get_definition returns the correct tool definition structure."""
        filesystem_tool = ToolFileSystem()
        definition = filesystem_tool.get_definition()

        assert isinstance(definition, AIToolDefinition)
        assert definition.name == "filesystem"
        assert "Perform filesystem operations within the current mindspace" in definition.description
        assert "Maximum file size: 10MB" in definition.description
        assert len(definition.parameters) == 6

    def test_operation_parameter_definition(self):
        """Test the operation parameter definition."""
        filesystem_tool = ToolFileSystem()
        definition = filesystem_tool.get_definition()
        operation_param = definition.parameters[0]

        assert isinstance(operation_param, AIToolParameter)
        assert operation_param.name == "operation"
        assert operation_param.type == "string"
        assert operation_param.description == "Filesystem operation to perform"
        assert operation_param.required is True
        assert operation_param.enum == [
            "read_file", "write_file", "append_to_file",
            "list_directory", "create_directory", "remove_directory",
            "delete_file", "copy_file", "move", "get_info"
        ]

    def test_path_parameter_definition(self):
        """Test the path parameter definition."""
        filesystem_tool = ToolFileSystem()
        definition = filesystem_tool.get_definition()
        path_param = definition.parameters[1]

        assert isinstance(path_param, AIToolParameter)
        assert path_param.name == "path"
        assert path_param.type == "string"
        assert "Path to file or directory" in path_param.description
        assert path_param.required is True
        assert path_param.enum is None

    def test_optional_parameters_definition(self):
        """Test the optional parameters definitions."""
        filesystem_tool = ToolFileSystem()
        definition = filesystem_tool.get_definition()
        param_names = [p.name for p in definition.parameters]

        assert "content" in param_names
        assert "destination" in param_names
        assert "encoding" in param_names
        assert "create_parents" in param_names

        # Check encoding parameter has correct enum
        encoding_param = next(p for p in definition.parameters if p.name == "encoding")
        assert encoding_param.enum == ["utf-8", "utf-16", "ascii", "latin-1"]
        assert encoding_param.required is False

    def test_custom_max_file_size_in_definition(self):
        """Test that custom max file size is reflected in definition."""
        custom_tool = ToolFileSystem(max_file_size_mb=5)
        definition = custom_tool.get_definition()

        assert "Maximum file size: 5MB" in definition.description


class TestToolFileSystemValidation:
    """Test validation through public interface."""

    def test_no_mindspace_error(self, mock_mindspace_manager, mock_authorization):
        """Test error when no mindspace is open."""
        mock_mindspace_manager.has_mindspace.return_value = False
        filesystem_tool = ToolFileSystem()
        filesystem_tool._mindspace_manager = mock_mindspace_manager

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "file.txt"},
                mock_authorization
            ))

        error = exc_info.value
        assert "No mindspace is currently open" in str(error)
        assert error.tool_name == "filesystem"

    def test_missing_path_parameter(self, filesystem_tool, mock_authorization):
        """Test error when path parameter is missing."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file"},
                mock_authorization
            ))

        error = exc_info.value
        assert "No 'path' argument provided" in str(error)

    def test_non_string_path_parameter(self, filesystem_tool, mock_authorization):
        """Test error when path parameter is not a string."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": 123},
                mock_authorization
            ))

        error = exc_info.value
        assert "'path' must be a string" in str(error)

    def test_empty_path_parameter(self, filesystem_tool, mock_authorization):
        """Test error when path parameter is empty."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": ""},
                mock_authorization
            ))

        error = exc_info.value
        assert "Path parameter is required" in str(error)

    def test_path_outside_mindspace(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test error when path is outside mindspace boundaries."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = None  # Outside mindspace

        with patch('pathlib.Path.resolve') as mock_resolve:
            mock_resolve.return_value = Path("/outside/mindspace/file.txt")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "../../../outside/file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is outside mindspace boundaries" in str(error)

    def test_mindspace_manager_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test error when mindspace manager raises an error."""
        mock_mindspace_manager.get_absolute_path.side_effect = MindspaceNotFoundError("No mindspace")

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "file.txt"},
                mock_authorization
            ))

        error = exc_info.value
        assert "Mindspace error: No mindspace" in str(error)
        assert isinstance(error.__cause__, MindspaceNotFoundError)


class TestToolFileSystemReadFile:
    """Test the read_file operation."""

    def test_read_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file reading."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "file.txt"},
                mock_authorization
            ))

            assert "File: file.txt" in result
            assert "Size: 12 bytes" in result
            assert "Encoding: utf-8" in result
            assert "test content" in result

    def test_read_file_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading non-existent file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "File does not exist: file.txt" in str(error)

    def test_read_file_not_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading when path is not a file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is not a file: file.txt" in str(error)

    def test_read_file_too_large(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading file that exceeds size limit."""
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
            mock_stat_result.st_size = 11 * 1024 * 1024  # 11MB, exceeds 10MB limit
            mock_stat.return_value = mock_stat_result

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "File too large: 11.0MB (max: 10.0MB)" in str(error)

    def test_read_file_custom_encoding(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading file with custom encoding."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "file.txt", "encoding": "utf-16"},
                mock_authorization
            ))

            assert "Encoding: utf-16" in result
            assert "test content" in result
            # Verify the encoding was passed to open
            mock_file.assert_called_with(mock_path, 'r', encoding='utf-16')

    def test_read_file_unicode_decode_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading file with unicode decode error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise UnicodeDecodeError
            mock_file.side_effect = UnicodeDecodeError('utf-8', b'', 0, 1, 'invalid start byte')

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to decode file with encoding 'utf-8'" in str(error)

    def test_read_file_permission_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading file with permission error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise PermissionError
            mock_file.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied reading file" in str(error)

    def test_read_file_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading file with OS error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open') as mock_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result

            # Mock open to raise OSError
            mock_file.side_effect = OSError("Disk error")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to read file" in str(error)


class TestToolFileSystemWriteFile:
    """Test the write_file operation."""

    def test_write_file_success_new_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful writing to new file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False  # New file

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": "test content"},
                mock_authorization
            ))

            assert "File written successfully: file.txt (12 bytes)" in result
            # Verify authorization was called with destructive=False for new file
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is False  # destructive parameter

    def test_write_file_success_overwrite(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful writing to existing file (overwrite)."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True  # Existing file

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": "test content"},
                mock_authorization
            ))

            assert "File written successfully: file.txt (12 bytes)" in result
            # Verify authorization was called with destructive=True for existing file
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            assert args[3] is True  # destructive parameter

    def test_write_file_no_content(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file without content parameter."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve:
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "write_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "No 'content' argument provided" in str(error)

    def test_write_file_non_string_content(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with non-string content."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve:
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "write_file", "path": "file.txt", "content": 123},
                    mock_authorization
                ))

            error = exc_info.value
            assert "'content' must be a string" in str(error)

    def test_write_file_content_too_large(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with content that exceeds size limit."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve:
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path

            # Create content larger than 10MB
            large_content = "x" * (11 * 1024 * 1024)

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "write_file", "path": "file.txt", "content": large_content},
                    mock_authorization
                ))

            error = exc_info.value
            assert "'content' too large: 11.0MB (max: 10.0MB)" in str(error)

    def test_write_file_custom_encoding(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with custom encoding."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": "test content", "encoding": "utf-16"},
                mock_authorization
            ))

            assert "File written successfully" in result
            # Verify the encoding was passed to NamedTemporaryFile
            mock_temp_file.assert_called_once()
            kwargs = mock_temp_file.call_args[1]
            assert kwargs['encoding'] == 'utf-16'

    def test_write_file_create_parents(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with create_parents option."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir/file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "dir/file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.mkdir') as mock_mkdir, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_path = Path("/test/mindspace/dir/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "dir/file.txt", "content": "test content", "create_parents": True},
                mock_authorization
            ))

            assert "File written successfully" in result
            # Verify mkdir was called with parents=True
            mock_mkdir.assert_called_once_with(parents=True, exist_ok=True)

    def test_write_file_authorization_denied(self, filesystem_tool, mock_mindspace_manager, mock_authorization_denied):
        """Test writing file when authorization is denied."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "write_file", "path": "file.txt", "content": "test content"},
                    mock_authorization_denied
                ))

            error = exc_info.value
            assert "User denied permission to write file: file.txt" in str(error)

    def test_write_file_permission_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with permission error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            # Mock tempfile to raise PermissionError
            mock_temp_file.side_effect = PermissionError("Access denied")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "write_file", "path": "file.txt", "content": "test content"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Permission denied writing file" in str(error)

    def test_write_file_os_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with OS error."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            # Mock tempfile to raise OSError
            mock_temp_file.side_effect = OSError("Disk full")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "write_file", "path": "file.txt", "content": "test content"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Failed to write file" in str(error)


class TestToolFileSystemAppendFile:
    """Test the append_to_file operation."""

    def test_append_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful appending to file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open()) as mock_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100  # Current file size
            mock_stat.return_value = mock_stat_result

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "append_to_file", "path": "file.txt", "content": "new content"},
                mock_authorization
            ))

            assert "Content appended successfully: file.txt (+11 bytes)" in result
            # Verify the file was opened in append mode
            mock_file.assert_called_with(mock_path, 'a', encoding='utf-8')

    def test_append_file_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test appending to non-existent file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "append_to_file", "path": "file.txt", "content": "content"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "File does not exist: file.txt" in str(error)

    def test_append_file_too_large_after_append(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test appending when result would exceed size limit."""
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
            mock_stat_result.st_size = 9 * 1024 * 1024  # 9MB current
            mock_stat.return_value = mock_stat_result

            # Content that would make total > 10MB
            large_content = "x" * (2 * 1024 * 1024)  # 2MB

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "append_to_file", "path": "file.txt", "content": large_content},
                    mock_authorization
                ))

            error = exc_info.value
            assert "File would be too large after append" in str(error)


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


class TestToolFileSystemExecute:
    """Test the main execute method."""

    def test_execute_missing_operation(self, filesystem_tool, mock_authorization):
        """Test execute without operation parameter."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute({"path": "file.txt"}, mock_authorization))

        error = exc_info.value
        assert "No 'path' argument provided" in str(error)

    def test_execute_invalid_operation(self, filesystem_tool, mock_authorization):
        """Test execute with invalid operation."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute({"operation": "invalid_op", "path": "file.txt"}, mock_authorization))

        error = exc_info.value
        assert "Unsupported operation: invalid_op" in str(error)

    def test_execute_read_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execute with read_file operation."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "file.txt"},
                mock_authorization
            ))

            assert "File: file.txt" in result
            assert "test content" in result

    def test_execute_write_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execute with write_file operation."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            result = asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": "test content"},
                mock_authorization
            ))

            assert "File written successfully: file.txt (12 bytes)" in result

    def test_execute_unexpected_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execute with unexpected error."""
        # Patch one of the operation handlers to raise an unexpected error
        with patch.object(filesystem_tool, '_read_file', side_effect=RuntimeError("Unexpected error")):
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": "file.txt"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "Filesystem operation failed: Unexpected error" in str(error)
            assert isinstance(error.__cause__, RuntimeError)


class TestToolFileSystemIntegration:
    """Integration tests for the filesystem tool."""

    def test_tool_inheritance(self):
        """Test that ToolFileSystem properly inherits from AITool."""
        filesystem_tool = ToolFileSystem()
        assert isinstance(filesystem_tool, AITool)
        assert hasattr(filesystem_tool, 'get_definition')
        assert hasattr(filesystem_tool, 'execute')
        assert callable(filesystem_tool.get_definition)
        assert callable(filesystem_tool.execute)

    def test_authorization_context_includes_operation_details(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test that authorization context includes relevant operation details."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": "test content"},
                mock_authorization
            ))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "Operation: write_file" in context
            assert "Path: file.txt" in context
            assert "Content size: 12 bytes" in context

    def test_authorization_context_with_large_content(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test authorization context with large content doesn't include preview."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('tempfile.NamedTemporaryFile') as mock_temp_file, \
             patch('pathlib.Path.replace') as mock_replace:

            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False

            # Mock temporary file
            mock_temp_instance = MagicMock()
            mock_temp_instance.name = "/tmp/temp_file"
            mock_temp_instance.__enter__.return_value = mock_temp_instance
            mock_temp_instance.__exit__.return_value = None
            mock_temp_file.return_value = mock_temp_instance

            large_content = "x" * 1000
            asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": large_content},
                mock_authorization
            ))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "Operation: write_file" in context
            assert "Path: file.txt" in context
            assert "Content size: 1,000 bytes" in context
            # Should not include preview for large content
            assert "Content preview:" not in context


class TestToolFileSystemParametrized:
    """Parametrized tests for the filesystem tool."""

    @pytest.mark.parametrize("operation", [
        "read_file", "write_file", "append_to_file", "list_directory",
        "create_directory", "remove_directory", "delete_file",
        "copy_file", "move", "get_info"
    ])
    def test_supported_operations_in_definition(self, operation):
        """Test that all supported operations are included in definition."""
        filesystem_tool = ToolFileSystem()
        definition = filesystem_tool.get_definition()
        operation_param = definition.parameters[0]

        assert operation in operation_param.enum

    @pytest.mark.parametrize("encoding", ["utf-8", "utf-16", "ascii", "latin-1"])
    def test_supported_encodings_in_definition(self, encoding):
        """Test that all supported encodings are included in definition."""
        filesystem_tool = ToolFileSystem()
        definition = filesystem_tool.get_definition()
        encoding_param = next(p for p in definition.parameters if p.name == "encoding")

        assert encoding in encoding_param.enum

    @pytest.mark.parametrize("max_size_mb,expected_bytes", [
        (1, 1024 * 1024),
        (5, 5 * 1024 * 1024),
        (10, 10 * 1024 * 1024),
        (50, 50 * 1024 * 1024),
    ])
    def test_custom_max_file_sizes(self, max_size_mb, expected_bytes):
        """Test filesystem tool with different max file sizes."""
        tool = ToolFileSystem(max_file_size_mb=max_size_mb)

        assert tool._max_file_size_bytes == expected_bytes

        definition = tool.get_definition()
        assert f"Maximum file size: {max_size_mb}MB" in definition.description

    @pytest.mark.parametrize("path_input,expected_error", [
        ("", "Path parameter is required"),
        (123, "'path' must be a string"),
    ])
    def test_invalid_path_inputs(self, filesystem_tool, mock_authorization, path_input, expected_error):
        """Test various invalid path inputs through public interface."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": path_input},
                mock_authorization
            ))

        error = exc_info.value
        assert expected_error in str(error)

    @pytest.mark.parametrize("falsy_value,expected_error", [
        (None, "'path' must be a string"),
        ([], "'path' must be a string"),
        ({}, "'path' must be a string"),
        (0, "'path' must be a string"),
        (False, "'path' must be a string"),
    ])
    def test_invalid_path_inputs_falsy_values(self, filesystem_tool, mock_authorization, falsy_value, expected_error):
        """Test falsy path inputs and their specific error messages."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": falsy_value},
                mock_authorization
            ))

        error = exc_info.value
        assert expected_error in str(error)

    def test_empty_string_path(self, filesystem_tool, mock_authorization):
        """Test empty string path specifically."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": ""},
                mock_authorization
            ))

        error = exc_info.value
        assert "Path parameter is required" in str(error)
