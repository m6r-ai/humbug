"""
Tests for the filesystem tool
"""
import asyncio
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock, AsyncMock

import pytest

from humbug.tools.tool_filesystem import ToolFileSystem
from humbug.ai.ai_tool_manager import (
    AITool, AIToolDefinition, AIToolParameter, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolTimeoutError
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
    async def mock_auth_callback(tool_name, arguments, context, destructive):
        return True  # Default to authorized

    return mock_auth_callback


@pytest.fixture
def mock_authorization_denied():
    """Fixture providing a mocked authorization callback that denies requests."""
    async def mock_auth_callback(tool_name, arguments, context, destructive):
        return False  # Always deny

    return mock_auth_callback


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
    """Test validation methods in the filesystem tool."""

    def test_validate_mindspace_access_success(self, filesystem_tool, mock_mindspace_manager):
        """Test successful mindspace access validation."""
        mock_mindspace_manager.has_mindspace.return_value = True
        
        # Should not raise an exception
        filesystem_tool._validate_mindspace_access()

    def test_validate_mindspace_access_no_mindspace(self, filesystem_tool, mock_mindspace_manager):
        """Test mindspace access validation when no mindspace is open."""
        mock_mindspace_manager.has_mindspace.return_value = False
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_mindspace_access()
        
        error = exc_info.value
        assert "No mindspace is currently open" in str(error)
        assert error.tool_name == "filesystem"

    def test_get_str_value_from_key_success(self, filesystem_tool):
        """Test successful string value extraction."""
        arguments = {"path": "/test/file.txt", "content": "test"}
        
        path_value = filesystem_tool._get_str_value_from_key("path", arguments)
        assert path_value == "/test/file.txt"
        
        content_value = filesystem_tool._get_str_value_from_key("content", arguments)
        assert content_value == "test"

    def test_get_str_value_from_key_missing(self, filesystem_tool):
        """Test string value extraction with missing key."""
        arguments = {"content": "test"}
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._get_str_value_from_key("path", arguments)
        
        error = exc_info.value
        assert "No 'path' argument provided" in str(error)

    def test_get_str_value_from_key_not_string(self, filesystem_tool):
        """Test string value extraction with non-string value."""
        arguments = {"path": 123}
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._get_str_value_from_key("path", arguments)
        
        error = exc_info.value
        assert "'path' must be a string" in str(error)

    def test_validate_and_resolve_path_success(self, filesystem_tool, mock_mindspace_manager):
        """Test successful path validation and resolution."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        
        with patch('pathlib.Path.resolve') as mock_resolve:
            mock_resolve.return_value = Path("/test/mindspace/file.txt")
            
            result = filesystem_tool._validate_and_resolve_path("file.txt", "read_file")
            assert result == Path("/test/mindspace/file.txt")

    def test_validate_and_resolve_path_empty(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation with empty path."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path("", "read_file")
        
        error = exc_info.value
        assert "Path parameter is required" in str(error)

    def test_validate_and_resolve_path_not_string(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation with non-string path."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path(123, "read_file")
        
        error = exc_info.value
        assert "Path must be a string" in str(error)

    def test_validate_and_resolve_path_outside_mindspace(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation when resolved path is outside mindspace."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = None  # Outside mindspace
        
        with patch('pathlib.Path.resolve') as mock_resolve:
            mock_resolve.return_value = Path("/outside/mindspace/file.txt")
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                filesystem_tool._validate_and_resolve_path("../../../outside/file.txt", "read_file")
            
            error = exc_info.value
            assert "Path is outside mindspace boundaries" in str(error)

    def test_validate_and_resolve_path_mindspace_error(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation when mindspace manager raises error."""
        mock_mindspace_manager.get_absolute_path.side_effect = MindspaceNotFoundError("No mindspace")
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path("file.txt", "read_file")
        
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
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            
            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result
            
            mock_to_thread.return_value = ("test content", 100)
            
            result = asyncio.run(filesystem_tool._read_file(
                {"path": "file.txt"}, 
                mock_authorization
            ))
            
            assert "File: file.txt" in result
            assert "Size: 100 bytes" in result
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
                asyncio.run(filesystem_tool._read_file(
                    {"path": "file.txt"}, 
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
                asyncio.run(filesystem_tool._read_file(
                    {"path": "file.txt"}, 
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
                asyncio.run(filesystem_tool._read_file(
                    {"path": "file.txt"}, 
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
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            
            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result
            
            mock_to_thread.return_value = ("test content", 100)
            
            result = asyncio.run(filesystem_tool._read_file(
                {"path": "file.txt", "encoding": "utf-16"}, 
                mock_authorization
            ))
            
            assert "Encoding: utf-16" in result
            # Verify the encoding was passed to the thread function
            mock_to_thread.assert_called_once()
            args = mock_to_thread.call_args[0]
            assert args[2] == "utf-16"  # Third argument should be encoding

    def test_read_file_timeout(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading file with timeout."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        
        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('asyncio.wait_for') as mock_wait_for:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            
            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result
            
            mock_wait_for.side_effect = asyncio.TimeoutError()
            
            with pytest.raises(AIToolTimeoutError) as exc_info:
                asyncio.run(filesystem_tool._read_file(
                    {"path": "file.txt"}, 
                    mock_authorization
                ))
            
            error = exc_info.value
            assert "File reading timed out" in str(error)
            assert error.timeout_seconds == 30.0

    def test_read_file_content_unicode_error(self, filesystem_tool):
        """Test _read_file_content with unicode decode error."""
        with patch('builtins.open', side_effect=UnicodeDecodeError('utf-8', b'', 0, 1, 'invalid')):
            with pytest.raises(AIToolExecutionError) as exc_info:
                filesystem_tool._read_file_content(Path("/test/file.txt"), "utf-8")
            
            error = exc_info.value
            assert "Failed to decode file with encoding 'utf-8'" in str(error)

    def test_read_file_content_permission_error(self, filesystem_tool):
        """Test _read_file_content with permission error."""
        with patch('builtins.open', side_effect=PermissionError("Access denied")):
            with pytest.raises(AIToolExecutionError) as exc_info:
                filesystem_tool._read_file_content(Path("/test/file.txt"), "utf-8")
            
            error = exc_info.value
            assert "Permission denied reading file" in str(error)


class TestToolFileSystemWriteFile:
    """Test the write_file operation."""

    def test_write_file_success_new_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful writing to new file."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"
        
        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False  # New file
            
            mock_to_thread.return_value = None
            
            result = asyncio.run(filesystem_tool._write_file(
                {"path": "file.txt", "content": "test content"}, 
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
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True  # Existing file
            
            mock_to_thread.return_value = None
            
            result = asyncio.run(filesystem_tool._write_file(
                {"path": "file.txt", "content": "test content"}, 
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
                asyncio.run(filesystem_tool._write_file(
                    {"path": "file.txt"}, 
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
                asyncio.run(filesystem_tool._write_file(
                    {"path": "file.txt", "content": 123}, 
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
                asyncio.run(filesystem_tool._write_file(
                    {"path": "file.txt", "content": large_content}, 
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
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False
            
            mock_to_thread.return_value = None
            
            result = asyncio.run(filesystem_tool._write_file(
                {"path": "file.txt", "content": "test content", "encoding": "utf-16"}, 
                mock_authorization
            ))
            
            assert "File written successfully" in result
            # Verify the encoding was passed to the thread function
            mock_to_thread.assert_called_once()
            args = mock_to_thread.call_args[0]
            assert args[2] == "utf-16"  # Third argument should be encoding

    def test_write_file_create_parents(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with create_parents option."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/dir/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "dir/file.txt"
        mock_mindspace_manager.get_relative_path.return_value = "dir/file.txt"
        
        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/dir/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False
            
            mock_to_thread.return_value = None
            
            result = asyncio.run(filesystem_tool._write_file(
                {"path": "dir/file.txt", "content": "test content", "create_parents": True}, 
                mock_authorization
            ))
            
            assert "File written successfully" in result
            # Verify create_parents was passed to the thread function
            mock_to_thread.assert_called_once()
            args = mock_to_thread.call_args[0]
            assert args[3] is True  # Fourth argument should be create_parents

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
                asyncio.run(filesystem_tool._write_file(
                    {"path": "file.txt", "content": "test content"}, 
                    mock_authorization_denied
                ))
            
            error = exc_info.value
            assert "User denied permission to write file: file.txt" in str(error)

    def test_write_file_timeout(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with timeout."""
        mock_mindspace_manager.get_absolute_path.return_value = "/test/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "file.txt"
        
        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('asyncio.wait_for') as mock_wait_for:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False
            
            mock_wait_for.side_effect = asyncio.TimeoutError()
            
            with pytest.raises(AIToolTimeoutError) as exc_info:
                asyncio.run(filesystem_tool._write_file(
                    {"path": "file.txt", "content": "test content"}, 
                    mock_authorization
                ))
            
            error = exc_info.value
            assert "File writing timed out" in str(error)
            assert error.timeout_seconds == 30.0

    def test_write_file_content_permission_error(self, filesystem_tool):
        """Test _write_file_content with permission error."""
        with patch('tempfile.NamedTemporaryFile', side_effect=PermissionError("Access denied")):
            with pytest.raises(AIToolExecutionError) as exc_info:
                filesystem_tool._write_file_content(Path("/test/file.txt"), "content", "utf-8", False)
            
            error = exc_info.value
            assert "Permission denied writing file" in str(error)

    def test_write_file_content_os_error(self, filesystem_tool):
        """Test _write_file_content with OS error."""
        with patch('tempfile.NamedTemporaryFile', side_effect=OSError("Disk full")):
            with pytest.raises(AIToolExecutionError) as exc_info:
                filesystem_tool._write_file_content(Path("/test/file.txt"), "content", "utf-8", False)
            
            error = exc_info.value
            assert "Failed to write file" in str(error)


class TestToolFileSystemExecute:
    """Test the main execute method."""

    def test_execute_no_mindspace(self, mock_mindspace_manager, mock_authorization):
        """Test execute when no mindspace is open."""
        # Create tool with a mindspace manager that has no mindspace
        mock_mindspace_manager.has_mindspace.return_value = False
        filesystem_tool = ToolFileSystem()
        filesystem_tool._mindspace_manager = mock_mindspace_manager
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute({"operation": "read_file", "path": "file.txt"}, mock_authorization))
        
        error = exc_info.value
        assert "No mindspace is currently open" in str(error)

    def test_execute_missing_operation(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execute without operation parameter."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute({"path": "file.txt"}, mock_authorization))
        
        error = exc_info.value
        assert "No 'path' argument provided" in str(error)

    def test_execute_invalid_operation(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
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
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True
            
            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 100
            mock_stat.return_value = mock_stat_result
            
            mock_to_thread.return_value = ("test content", 100)
            
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
             patch('asyncio.to_thread') as mock_to_thread:
            
            mock_path = Path("/test/mindspace/file.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = False
            
            mock_to_thread.return_value = None
            
            result = asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": "test content"}, 
                mock_authorization
            ))
            
            assert "File written successfully: file.txt (12 bytes)" in result

    def test_execute_unexpected_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execute with unexpected error."""
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

    def test_build_authorization_context(self, filesystem_tool, mock_mindspace_manager):
        """Test building authorization context."""
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"
        
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False
            
            context = filesystem_tool._build_authorization_context(
                "write_file", 
                Path("/test/mindspace/file.txt"),
                content="test content"
            )
            
            assert "Operation: write_file" in context
            assert "Path: file.txt" in context
            assert "Content size: 12 bytes" in context
            assert "Content preview: 'test content'" in context

    def test_build_authorization_context_large_content(self, filesystem_tool, mock_mindspace_manager):
        """Test building authorization context with large content."""
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"
        
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False
            
            large_content = "x" * 1000
            context = filesystem_tool._build_authorization_context(
                "write_file", 
                Path("/test/mindspace/file.txt"),
                content=large_content
            )
            
            assert "Operation: write_file" in context
            assert "Path: file.txt" in context
            assert "Content size: 1,000 bytes" in context
            # Should not include preview for large content
            assert "Content preview:" not in context

    def test_build_authorization_context_existing_file(self, filesystem_tool, mock_mindspace_manager):
        """Test building authorization context for existing file."""
        mock_mindspace_manager.get_relative_path.return_value = "file.txt"
        
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat:
            
            mock_exists.return_value = True
            mock_is_file.return_value = True
            
            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 500
            mock_stat.return_value = mock_stat_result
            
            context = filesystem_tool._build_authorization_context(
                "read_file", 
                Path("/test/mindspace/file.txt")
            )
            
            assert "Operation: read_file" in context
            assert "Path: file.txt" in context
            assert "Current size: 500 bytes" in context


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
        (123, "Path must be a string"),
    ])
    def test_invalid_path_inputs(self, filesystem_tool, mock_mindspace_manager, path_input, expected_error):
        """Test various invalid path inputs."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path(path_input, "test_operation")
        
        error = exc_info.value
        assert expected_error in str(error)

    def test_invalid_path_inputs_falsy_values(self, filesystem_tool, mock_mindspace_manager):
        """Test falsy path inputs that are handled by the empty string check."""
        falsy_values = [None, [], {}, 0, False]
        
        for falsy_value in falsy_values:
            with pytest.raises(AIToolExecutionError) as exc_info:
                filesystem_tool._validate_and_resolve_path(falsy_value, "test_operation")
            
            error = exc_info.value
            # All falsy values are caught by the empty string check first
            assert "Path parameter is required" in str(error)