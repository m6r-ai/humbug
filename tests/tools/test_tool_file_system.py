"""
Tests for the filesystem tool
"""
import asyncio
import tempfile
import shutil
from pathlib import Path
from unittest.mock import patch, MagicMock, AsyncMock
from datetime import datetime

import pytest

from humbug.tools.tool_file_system import ToolFileSystem
from humbug.ai.ai_tool_manager import (
    AITool, AIToolDefinition, AIToolParameter, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolTimeoutError
)
from humbug.mindspace.mindspace_error import MindspaceNotFoundError


@pytest.fixture
def mock_mindspace_manager():
    """Fixture providing a mocked mindspace manager."""
    with patch('humbug.tools.tool_file_system.MindspaceManager') as mock_manager_class:
        mock_manager = MagicMock()
        mock_manager_class.return_value = mock_manager
        
        # Default behavior - mindspace is open
        mock_manager.has_mindspace.return_value = True
        mock_manager.get_absolute_path.side_effect = lambda path: f"/mindspace/{path}"
        mock_manager.get_mindspace_relative_path.side_effect = lambda path: path.replace("/mindspace/", "") if "/mindspace/" in path else path
        mock_manager.get_relative_path.side_effect = lambda path: path.replace("/mindspace/", "") if "/mindspace/" in path else path
        
        yield mock_manager


@pytest.fixture
def filesystem_tool(mock_mindspace_manager):
    """Fixture providing a filesystem tool instance with mocked mindspace manager."""
    return ToolFileSystem(max_file_size_mb=1)  # 1MB for testing


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    async def mock_auth_callback(tool_name, arguments, context, destructive):
        return True  # Default to authorized
    
    return mock_auth_callback


@pytest.fixture
def temp_directory():
    """Fixture providing a temporary directory for real filesystem tests."""
    temp_dir = tempfile.mkdtemp()
    # Resolve symlinks to get the real path (important on macOS)
    resolved_temp_dir = Path(temp_dir).resolve()
    yield resolved_temp_dir
    shutil.rmtree(str(resolved_temp_dir), ignore_errors=True)


class TestToolFileSystemDefinition:
    """Test the filesystem tool definition."""

    def test_get_definition_returns_correct_structure(self, filesystem_tool):
        """Test that get_definition returns the correct tool definition structure."""
        definition = filesystem_tool.get_definition()

        assert isinstance(definition, AIToolDefinition)
        assert definition.name == "filesystem"
        assert "Perform filesystem operations within the current mindspace" in definition.description
        assert "Maximum file size: 1MB" in definition.description
        assert len(definition.parameters) == 6

    def test_operation_parameter_definition(self, filesystem_tool):
        """Test the operation parameter definition."""
        definition = filesystem_tool.get_definition()
        operation_param = definition.parameters[0]

        assert isinstance(operation_param, AIToolParameter)
        assert operation_param.name == "operation"
        assert operation_param.type == "string"
        assert operation_param.description == "Filesystem operation to perform"
        assert operation_param.required is True
        
        expected_operations = [
            "read_file", "write_file", "append_to_file",
            "list_directory", "create_directory", "remove_directory",
            "delete_file", "copy_file", "move", "get_info"
        ]
        assert operation_param.enum == expected_operations

    def test_path_parameter_definition(self, filesystem_tool):
        """Test the path parameter definition."""
        definition = filesystem_tool.get_definition()
        path_param = definition.parameters[1]

        assert isinstance(path_param, AIToolParameter)
        assert path_param.name == "path"
        assert path_param.type == "string"
        assert "relative to mindspace root or absolute" in path_param.description
        assert path_param.required is True
        assert path_param.enum is None

    def test_content_parameter_definition(self, filesystem_tool):
        """Test the content parameter definition."""
        definition = filesystem_tool.get_definition()
        content_param = definition.parameters[2]

        assert isinstance(content_param, AIToolParameter)
        assert content_param.name == "content"
        assert content_param.type == "string"
        assert "write_file and append_to_file operations" in content_param.description
        assert content_param.required is False

    def test_destination_parameter_definition(self, filesystem_tool):
        """Test the destination parameter definition."""
        definition = filesystem_tool.get_definition()
        dest_param = definition.parameters[3]

        assert isinstance(dest_param, AIToolParameter)
        assert dest_param.name == "destination"
        assert dest_param.type == "string"
        assert "copy_file and move operations" in dest_param.description
        assert dest_param.required is False

    def test_encoding_parameter_definition(self, filesystem_tool):
        """Test the encoding parameter definition."""
        definition = filesystem_tool.get_definition()
        encoding_param = definition.parameters[4]

        assert isinstance(encoding_param, AIToolParameter)
        assert encoding_param.name == "encoding"
        assert encoding_param.type == "string"
        assert "Text encoding to use for file operations" in encoding_param.description
        assert encoding_param.required is False
        assert encoding_param.enum == ["utf-8", "utf-16", "ascii", "latin-1"]

    def test_create_parents_parameter_definition(self, filesystem_tool):
        """Test the create_parents parameter definition."""
        definition = filesystem_tool.get_definition()
        create_parents_param = definition.parameters[5]

        assert isinstance(create_parents_param, AIToolParameter)
        assert create_parents_param.name == "create_parents"
        assert create_parents_param.type == "boolean"
        assert "Create parent directories if they don't exist" in create_parents_param.description
        assert create_parents_param.required is False


class TestToolFileSystemValidation:
    """Test filesystem tool validation methods."""

    def test_validate_mindspace_access_success(self, filesystem_tool, mock_mindspace_manager):
        """Test successful mindspace access validation."""
        mock_mindspace_manager.has_mindspace.return_value = True
        
        # Should not raise an exception
        filesystem_tool._validate_mindspace_access()

    def test_validate_mindspace_access_failure(self, filesystem_tool, mock_mindspace_manager):
        """Test failed mindspace access validation."""
        mock_mindspace_manager.has_mindspace.return_value = False
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_mindspace_access()

        error = exc_info.value
        assert "No mindspace is currently open" in str(error)
        assert error.tool_name == "filesystem"

    def test_validate_and_resolve_path_success(self, filesystem_tool, mock_mindspace_manager):
        """Test successful path validation and resolution."""
        mock_mindspace_manager.get_absolute_path.return_value = "/mindspace/test/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = "test/file.txt"
        
        result = filesystem_tool._validate_and_resolve_path("test/file.txt", "read_file")
        
        assert isinstance(result, Path)
        assert str(result) == "/mindspace/test/file.txt"

    def test_validate_and_resolve_path_empty(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation with empty path."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path("", "read_file")

        error = exc_info.value
        assert "Path parameter is required" in str(error)

    def test_validate_and_resolve_path_non_string(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation with non-string path."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path(123, "read_file")

        error = exc_info.value
        assert "Path must be a string" in str(error)

    def test_validate_and_resolve_path_outside_mindspace(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation for path outside mindspace."""
        mock_mindspace_manager.get_absolute_path.return_value = "/outside/mindspace/file.txt"
        mock_mindspace_manager.get_mindspace_relative_path.return_value = None
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path("../../../etc/passwd", "read_file")

        error = exc_info.value
        assert "Path is outside mindspace boundaries" in str(error)

    def test_validate_and_resolve_path_mindspace_error(self, filesystem_tool, mock_mindspace_manager):
        """Test path validation with mindspace error."""
        mock_mindspace_manager.get_absolute_path.side_effect = MindspaceNotFoundError("No mindspace")
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            filesystem_tool._validate_and_resolve_path("test.txt", "read_file")

        error = exc_info.value
        assert "Mindspace error" in str(error)

    def test_build_authorization_context_basic(self, filesystem_tool, mock_mindspace_manager):
        """Test basic authorization context building."""
        mock_mindspace_manager.get_relative_path.return_value = "test/file.txt"
        
        context = filesystem_tool._build_authorization_context(
            "read_file", 
            Path("/mindspace/test/file.txt")
        )
        
        assert "Operation: read_file" in context
        assert "Path: test/file.txt" in context

    def test_build_authorization_context_with_content(self, filesystem_tool, mock_mindspace_manager):
        """Test authorization context building with content."""
        mock_mindspace_manager.get_relative_path.return_value = "test/file.txt"
        
        content = "Hello, world!"
        context = filesystem_tool._build_authorization_context(
            "write_file",
            Path("/mindspace/test/file.txt"),
            content=content
        )
        
        assert "Operation: write_file" in context
        assert "Path: test/file.txt" in context
        assert "Content size: 13 bytes" in context
        assert "Content preview: 'Hello, world!'" in context

    def test_build_authorization_context_large_content(self, filesystem_tool, mock_mindspace_manager):
        """Test authorization context building with large content."""
        mock_mindspace_manager.get_relative_path.return_value = "test/file.txt"
        
        content = "x" * 1000
        context = filesystem_tool._build_authorization_context(
            "write_file",
            Path("/mindspace/test/file.txt"),
            content=content
        )
        
        assert "Content size: 1,000 bytes" in context
        assert "Content preview:" not in context  # Too large for preview

    def test_build_authorization_context_with_destination(self, filesystem_tool, mock_mindspace_manager):
        """Test authorization context building with destination."""
        mock_mindspace_manager.get_relative_path.side_effect = lambda path: path.replace("/mindspace/", "")
        
        context = filesystem_tool._build_authorization_context(
            "copy_file",
            Path("/mindspace/source.txt"),
            destination="/mindspace/dest.txt"
        )
        
        assert "Operation: copy_file" in context
        assert "Path: source.txt" in context
        assert "Destination: dest.txt" in context


class TestToolFileSystemExecuteRouting:
    """Test the main execute method and operation routing."""

    def test_execute_missing_operation(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execution without operation parameter."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute({}, mock_authorization))

        error = exc_info.value
        assert "Operation parameter is required" in str(error)

    def test_execute_unsupported_operation(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execution with unsupported operation."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "invalid_operation", "path": "test.txt"}, 
                mock_authorization
            ))

        error = exc_info.value
        assert "Unsupported operation: invalid_operation" in str(error)

    def test_execute_no_mindspace(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test execution when no mindspace is open."""
        mock_mindspace_manager.has_mindspace.return_value = False
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "test.txt"}, 
                mock_authorization
            ))

        error = exc_info.value
        assert "No mindspace is currently open" in str(error)


class TestToolFileSystemReadOperations:
    """Test file reading operations."""

    def test_read_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file reading."""
        test_content = "Hello, world!"
        
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            # Mock path resolution
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            
            # Mock file existence and properties
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            mock_stat = MagicMock()
            mock_stat.st_size = len(test_content)
            mock_path.stat.return_value = mock_stat
            
            # Mock file reading
            with patch('asyncio.to_thread') as mock_to_thread:
                mock_to_thread.return_value = (test_content, len(test_content))
                
                with patch('asyncio.wait_for') as mock_wait_for:
                    mock_wait_for.return_value = (test_content, len(test_content))
                    
                    mock_mindspace_manager.get_relative_path.return_value = "test.txt"
                    
                    result = asyncio.run(filesystem_tool._read_file(
                        {"path": "test.txt", "encoding": "utf-8"}, 
                        mock_authorization
                    ))
                    
                    assert "File: test.txt" in result
                    assert "Size: 13 bytes" in result
                    assert "Encoding: utf-8" in result
                    assert test_content in result

    def test_read_file_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading non-existent file."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = False
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._read_file(
                    {"path": "nonexistent.txt"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "File does not exist" in str(error)

    def test_read_file_is_directory(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading a directory instead of file."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = False
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._read_file(
                    {"path": "directory"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is not a file" in str(error)

    def test_read_file_too_large(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test reading file that exceeds size limit."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            
            # File larger than 1MB limit
            mock_stat = MagicMock()
            mock_stat.st_size = 2 * 1024 * 1024  # 2MB
            mock_path.stat.return_value = mock_stat
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._read_file(
                    {"path": "large_file.txt"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "File too large" in str(error)
            assert "2.0MB" in str(error)

    def test_read_file_timeout(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test file reading timeout."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            mock_stat = MagicMock()
            mock_stat.st_size = 100
            mock_path.stat.return_value = mock_stat
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.side_effect = asyncio.TimeoutError()
                
                with pytest.raises(AIToolTimeoutError) as exc_info:
                    asyncio.run(filesystem_tool._read_file(
                        {"path": "test.txt"}, 
                        mock_authorization
                    ))

                error = exc_info.value
                assert "File reading timed out" in str(error)
                assert error.timeout_seconds == 30.0

    def test_read_file_encoding_error(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test file reading with encoding error."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            mock_stat = MagicMock()
            mock_stat.st_size = 100
            mock_path.stat.return_value = mock_stat
            
            # Mock encoding error in sync helper
            filesystem_tool._read_file_content = MagicMock(
                side_effect=AIToolExecutionError(
                    "Failed to decode file with encoding 'ascii'",
                    "filesystem",
                    {}
                )
            )
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.side_effect = filesystem_tool._read_file_content.side_effect
                
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(filesystem_tool._read_file(
                        {"path": "test.txt", "encoding": "ascii"}, 
                        mock_authorization
                    ))

                error = exc_info.value
                assert "Failed to decode file" in str(error)


class TestToolFileSystemWriteOperations:
    """Test file writing operations."""

    def test_write_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file writing."""
        test_content = "Hello, world!"
        
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = None
                
                mock_mindspace_manager.get_relative_path.return_value = "test.txt"
                
                result = asyncio.run(filesystem_tool._write_file(
                    {"path": "test.txt", "content": test_content, "encoding": "utf-8"}, 
                    mock_authorization
                ))
                
                assert "File written successfully: test.txt" in result
                assert "13 bytes" in result

    def test_write_file_non_string_content(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with non-string content."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._write_file(
                    {"path": "test.txt", "content": 123}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Content must be a string" in str(error)

    def test_write_file_content_too_large(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test writing file with content exceeding size limit."""
        large_content = "x" * (2 * 1024 * 1024)  # 2MB
        
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._write_file(
                    {"path": "test.txt", "content": large_content}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Content too large" in str(error)

    def test_write_file_authorization_denied(self, filesystem_tool, mock_mindspace_manager):
        """Test file writing with authorization denied."""
        async def deny_authorization(tool_name, arguments, context, destructive):
            return False
        
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            
            with pytest.raises(AIToolAuthorizationDenied) as exc_info:
                asyncio.run(filesystem_tool._write_file(
                    {"path": "test.txt", "content": "test"}, 
                    deny_authorization
                ))

            error = exc_info.value
            assert "User denied permission to write file" in str(error)

    def test_write_file_timeout(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test file writing timeout."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.side_effect = asyncio.TimeoutError()
                
                with pytest.raises(AIToolTimeoutError) as exc_info:
                    asyncio.run(filesystem_tool._write_file(
                        {"path": "test.txt", "content": "test"}, 
                        mock_authorization
                    ))

                error = exc_info.value
                assert "File writing timed out" in str(error)

    def test_append_to_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file appending."""
        test_content = "Appended content"
        
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            mock_stat = MagicMock()
            mock_stat.st_size = 100  # Existing file size
            mock_path.stat.return_value = mock_stat
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = None
                
                mock_mindspace_manager.get_relative_path.return_value = "test.txt"
                
                result = asyncio.run(filesystem_tool._append_to_file(
                    {"path": "test.txt", "content": test_content}, 
                    mock_authorization
                ))
                
                assert "Content appended successfully: test.txt" in result
                assert "+16 bytes" in result

    def test_append_to_file_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test appending to non-existent file."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = False
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._append_to_file(
                    {"path": "nonexistent.txt", "content": "test"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "File does not exist" in str(error)

    def test_append_to_file_would_exceed_size(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test appending content that would exceed size limit."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            
            # Existing file is already near limit
            mock_stat = MagicMock()
            mock_stat.st_size = 1024 * 1024 - 100  # Just under 1MB
            mock_path.stat.return_value = mock_stat
            
            large_content = "x" * 200  # Would exceed limit
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._append_to_file(
                    {"path": "test.txt", "content": large_content}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "File would be too large after append" in str(error)


class TestToolFileSystemDirectoryOperations:
    """Test directory operations."""

    def test_list_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory listing."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_dir.return_value = True
            
            # Mock directory items
            mock_items = [
                {'name': 'file1.txt', 'type': 'file', 'size': 100},
                {'name': 'file2.txt', 'type': 'file', 'size': 200},
                {'name': 'subdir', 'type': 'directory', 'size': None}
            ]
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = mock_items
                
                mock_mindspace_manager.get_relative_path.return_value = "test_dir"
                
                result = asyncio.run(filesystem_tool._list_directory(
                    {"path": "test_dir"}, 
                    mock_authorization
                ))
                
                assert "Directory: test_dir" in result
                assert "Items: 3" in result
                assert "📁 subdir/" in result
                assert "📄 file1.txt (100 bytes)" in result
                assert "📄 file2.txt (200 bytes)" in result

    def test_list_directory_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing non-existent directory."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = False
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._list_directory(
                    {"path": "nonexistent"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory does not exist" in str(error)

    def test_list_directory_is_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test listing a file instead of directory."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_dir.return_value = False
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._list_directory(
                    {"path": "file.txt"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is not a directory" in str(error)

    def test_create_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory creation."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            mock_path.exists.return_value = False
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = None
                
                mock_mindspace_manager.get_relative_path.return_value = "new_dir"
                
                result = asyncio.run(filesystem_tool._create_directory(
                    {"path": "new_dir"}, 
                    mock_authorization
                ))
                
                assert "Directory created successfully: new_dir" in result

    def test_create_directory_already_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test creating directory that already exists."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_dir.return_value = True
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._create_directory(
                    {"path": "existing_dir"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory already exists" in str(error)

    def test_remove_directory_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful directory removal."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            mock_path.exists.return_value = True
            mock_path.is_dir.return_value = True
            mock_path.iterdir.return_value = []  # Empty directory
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = None
                
                mock_mindspace_manager.get_relative_path.return_value = "empty_dir"
                
                result = asyncio.run(filesystem_tool._remove_directory(
                    {"path": "empty_dir"}, 
                    mock_authorization
                ))
                
                assert "Directory removed successfully: empty_dir" in result

    def test_remove_directory_not_empty(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test removing non-empty directory."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            mock_path.is_dir.return_value = True
            
            # Mock non-empty directory
            mock_file = MagicMock()
            mock_path.iterdir.return_value = [mock_file]
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._remove_directory(
                    {"path": "nonempty_dir"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Directory is not empty" in str(error)


class TestToolFileSystemFileManagement:
    """Test file management operations."""

    def test_delete_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file deletion."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = None
                
                mock_mindspace_manager.get_relative_path.return_value = "test.txt"
                
                result = asyncio.run(filesystem_tool._delete_file(
                    {"path": "test.txt"}, 
                    mock_authorization
                ))
                
                assert "File deleted successfully: test.txt" in result

    def test_delete_file_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test deleting non-existent file."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = False
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._delete_file(
                    {"path": "nonexistent.txt"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "File does not exist" in str(error)

    def test_copy_file_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file copying."""
        with patch('pathlib.Path') as mock_path_class:
            mock_source = MagicMock()
            mock_dest = MagicMock()
            
            def mock_validate_path(path, operation):
                if "source" in path:
                    return mock_source
                return mock_dest
            
            filesystem_tool._validate_and_resolve_path = mock_validate_path
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            
            mock_source.exists.return_value = True
            mock_source.is_file.return_value = True
            mock_stat = MagicMock()
            mock_stat.st_size = 100
            mock_source.stat.return_value = mock_stat
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = None
                
                mock_mindspace_manager.get_relative_path.side_effect = lambda p: p.replace("/mindspace/", "")
                
                result = asyncio.run(filesystem_tool._copy_file(
                    {"path": "source.txt", "destination": "dest.txt"}, 
                    mock_authorization
                ))
                
                assert "File copied successfully" in result

    def test_copy_file_missing_destination(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying file without destination."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool._copy_file(
                {"path": "source.txt"}, 
                mock_authorization
            ))

        error = exc_info.value
        assert "Destination parameter is required" in str(error)

    def test_copy_file_source_too_large(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test copying file that exceeds size limit."""
        with patch('pathlib.Path') as mock_path_class:
            mock_source = MagicMock()
            mock_dest = MagicMock()
            
            def mock_validate_path(path, operation):
                if "source" in path:
                    return mock_source
                return mock_dest
            
            filesystem_tool._validate_and_resolve_path = mock_validate_path
            
            mock_source.exists.return_value = True
            mock_source.is_file.return_value = True
            mock_stat = MagicMock()
            mock_stat.st_size = 2 * 1024 * 1024  # 2MB
            mock_source.stat.return_value = mock_stat
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._copy_file(
                    {"path": "large_source.txt", "destination": "dest.txt"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Source file too large" in str(error)

    def test_move_success(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test successful file/directory move."""
        with patch('pathlib.Path') as mock_path_class:
            mock_source = MagicMock()
            mock_dest = MagicMock()
            
            def mock_validate_path(path, operation):
                if "source" in path:
                    return mock_source
                return mock_dest
            
            filesystem_tool._validate_and_resolve_path = mock_validate_path
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            mock_source.exists.return_value = True
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = None
                
                mock_mindspace_manager.get_relative_path.side_effect = lambda p: p.replace("/mindspace/", "")
                
                result = asyncio.run(filesystem_tool._move(
                    {"path": "source.txt", "destination": "dest.txt"}, 
                    mock_authorization
                ))
                
                assert "Moved successfully" in result

    def test_move_missing_destination(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test moving without destination."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool._move(
                {"path": "source.txt"}, 
                mock_authorization
            ))

        error = exc_info.value
        assert "Destination parameter is required" in str(error)

    def test_get_info_file(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test getting file information."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = True
            
            mock_info = "File: test.txt\nType: File\nSize: 100 bytes"
            
            with patch('asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = mock_info
                
                result = asyncio.run(filesystem_tool._get_info(
                    {"path": "test.txt"}, 
                    mock_authorization
                ))
                
                assert result == mock_info

    def test_get_info_not_exists(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test getting info for non-existent path."""
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            
            filesystem_tool._validate_and_resolve_path = MagicMock(return_value=mock_path)
            mock_path.exists.return_value = False
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool._get_info(
                    {"path": "nonexistent"}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path does not exist" in str(error)


class TestToolFileSystemParametrized:
    """Parametrized tests for various scenarios."""

    @pytest.mark.parametrize("operation", [
        "read_file", "write_file", "append_to_file", "list_directory",
        "create_directory", "remove_directory", "delete_file", 
        "copy_file", "move", "get_info"
    ])
    def test_all_operations_require_path(self, filesystem_tool, mock_mindspace_manager, mock_authorization, operation):
        """Test that all operations require a path parameter."""
        arguments = {"operation": operation}
        
        # Add required parameters for specific operations
        if operation in ("write_file", "append_to_file"):
            arguments["content"] = "test"
        if operation in ("copy_file", "move"):
            arguments["destination"] = "dest"
        
        # Should fail due to missing path
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(arguments, mock_authorization))

        error = exc_info.value
        assert "Path parameter is required" in str(error)

    @pytest.mark.parametrize("encoding", ["utf-8", "utf-16", "ascii", "latin-1"])
    def test_supported_encodings(self, filesystem_tool, encoding):
        """Test that all supported encodings are accepted."""
        definition = filesystem_tool.get_definition()
        encoding_param = next(p for p in definition.parameters if p.name == "encoding")
        
        assert encoding in encoding_param.enum

    @pytest.mark.parametrize("destructive_operation", [
        "write_file", "append_to_file", "create_directory", 
        "remove_directory", "delete_file", "copy_file", "move"
    ])
    def test_destructive_operations_require_authorization(self, filesystem_tool, mock_mindspace_manager, destructive_operation):
        """Test that destructive operations require authorization."""
        async def deny_authorization(tool_name, arguments, context, destructive):
            assert destructive is True  # Should be marked as destructive
            return False
        
        arguments = {"operation": destructive_operation, "path": "test"}
        
        # Add required parameters
        if destructive_operation in ("write_file", "append_to_file"):
            arguments["content"] = "test"
        if destructive_operation in ("copy_file", "move"):
            arguments["destination"] = "dest"
        
        # Mock path validation to pass
        filesystem_tool._validate_and_resolve_path = MagicMock(return_value=Path("/mindspace/test"))
        
        # Mock file/directory existence as needed
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            mock_path.exists.return_value = True
            mock_path.is_file.return_value = True
            mock_path.is_dir.return_value = False
            
            if destructive_operation == "append_to_file":
                mock_stat = MagicMock()
                mock_stat.st_size = 100
                mock_path.stat.return_value = mock_stat
            elif destructive_operation == "remove_directory":
                mock_path.is_dir.return_value = True
                mock_path.iterdir.return_value = []
            elif destructive_operation == "copy_file":
                mock_stat = MagicMock()
                mock_stat.st_size = 100
                mock_path.stat.return_value = mock_stat
            
            filesystem_tool._build_authorization_context = MagicMock(return_value="context")
            
            with pytest.raises(AIToolAuthorizationDenied):
                asyncio.run(filesystem_tool.execute(arguments, deny_authorization))

    @pytest.mark.parametrize("non_destructive_operation", [
        "read_file", "list_directory", "get_info"
    ])
    def test_non_destructive_operations_no_authorization(self, filesystem_tool, mock_mindspace_manager, non_destructive_operation):
        """Test that non-destructive operations don't require authorization."""
        authorization_called = False
        
        async def track_authorization(tool_name, arguments, context, destructive):
            nonlocal authorization_called
            authorization_called = True
            return True
        
        arguments = {"operation": non_destructive_operation, "path": "test"}
        
        # Mock successful operation
        filesystem_tool._validate_and_resolve_path = MagicMock(return_value=Path("/mindspace/test"))
        
        with patch('pathlib.Path') as mock_path_class:
            mock_path = MagicMock()
            mock_path_class.return_value = mock_path
            mock_path.exists.return_value = True
            
            if non_destructive_operation == "read_file":
                mock_path.is_file.return_value = True
                mock_stat = MagicMock()
                mock_stat.st_size = 100
                mock_path.stat.return_value = mock_stat
                
                with patch('asyncio.wait_for') as mock_wait_for:
                    mock_wait_for.return_value = ("content", 100)
                    mock_mindspace_manager.get_relative_path.return_value = "test"
                    
                    result = asyncio.run(filesystem_tool.execute(arguments, track_authorization))
                    assert "File: test" in result
                    
            elif non_destructive_operation == "list_directory":
                mock_path.is_dir.return_value = True
                
                with patch('asyncio.wait_for') as mock_wait_for:
                    mock_wait_for.return_value = []
                    mock_mindspace_manager.get_relative_path.return_value = "test"
                    
                    result = asyncio.run(filesystem_tool.execute(arguments, track_authorization))
                    assert "Directory: test" in result
                    
            elif non_destructive_operation == "get_info":
                with patch('asyncio.wait_for') as mock_wait_for:
                    mock_wait_for.return_value = "File info"
                    
                    result = asyncio.run(filesystem_tool.execute(arguments, track_authorization))
                    assert result == "File info"
        
        # Authorization should not have been called for non-destructive operations
        assert not authorization_called


class TestToolFileSystemSecurity:
    """Test security aspects of the filesystem tool."""

    def test_path_traversal_prevention(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test that path traversal attacks are prevented."""
        malicious_paths = [
            "../../../etc/passwd",
            "..\\..\\..\\windows\\system32",
            "/etc/passwd",
            "C:\\Windows\\System32",
            "~/../../etc/passwd",
            "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",  # URL encoded
        ]
        
        for malicious_path in malicious_paths:
            mock_mindspace_manager.get_absolute_path.return_value = "/outside/mindspace/file"
            mock_mindspace_manager.get_mindspace_relative_path.return_value = None
            
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": "read_file", "path": malicious_path}, 
                    mock_authorization
                ))

            error = exc_info.value
            assert "Path is outside mindspace boundaries" in str(error)

    def test_file_size_limits_enforced(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test that file size limits are enforced."""
        large_content = "x" * (2 * 1024 * 1024)  # 2MB, exceeds 1MB limit
        
        filesystem_tool._validate_and_resolve_path = MagicMock(return_value=Path("/mindspace/test.txt"))
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "test.txt", "content": large_content}, 
                mock_authorization
            ))

        error = exc_info.value
        assert "Content too large" in str(error)

    def test_mindspace_boundary_enforcement(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test that operations are restricted to mindspace."""
        mock_mindspace_manager.has_mindspace.return_value = False
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "test.txt"}, 
                mock_authorization
            ))

        error = exc_info.value
        assert "No mindspace is currently open" in str(error)

    @pytest.mark.parametrize("invalid_path", [
        None,
        123,
        [],
        {},
        "",
        "   ",
    ])
    def test_invalid_path_types_rejected(self, filesystem_tool, mock_mindspace_manager, mock_authorization, invalid_path):
        """Test that invalid path types are rejected."""
        if invalid_path == "":
            expected_error = "Path parameter is required"
        elif invalid_path == "   ":
            expected_error = "Path parameter is required" 
        else:
            expected_error = "Path must be a string"
        
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": invalid_path}, 
                mock_authorization
            ))

        error = exc_info.value
        assert expected_error in str(error)


class TestToolFileSystemIntegration:
    """Integration tests for the filesystem tool."""

    def test_tool_inheritance(self, filesystem_tool):
        """Test that ToolFileSystem properly inherits from AITool."""
        assert isinstance(filesystem_tool, AITool)
        assert hasattr(filesystem_tool, 'get_definition')
        assert hasattr(filesystem_tool, 'execute')
        assert callable(filesystem_tool.get_definition)
        assert callable(filesystem_tool.execute)

    def test_end_to_end_file_workflow(self, temp_directory):
        """Test end-to-end file operations with real filesystem."""
        # Create a fresh filesystem tool with separate mindspace manager mock for this test
        with patch('humbug.tools.tool_file_system.MindspaceManager') as mock_manager_class:
            mock_manager = MagicMock()
            mock_manager_class.return_value = mock_manager
            mock_manager.has_mindspace.return_value = True
            
            filesystem_tool = ToolFileSystem(max_file_size_mb=1)
            
            test_file = temp_directory / "test.txt"
            test_content = "Hello, world!"
            
            # Mock path resolution to use temp directory - ensure consistent path resolution
            def mock_get_absolute_path(path):
                return str(temp_directory / path)
            
            def mock_get_relative_path(path):
                # Handle both resolved and unresolved paths
                path_obj = Path(path).resolve()
                try:
                    return str(path_obj.relative_to(temp_directory))
                except ValueError:
                    # If relative_to fails, try with the original path
                    return str(Path(path).relative_to(temp_directory))
            
            mock_manager.get_absolute_path.side_effect = mock_get_absolute_path
            mock_manager.get_mindspace_relative_path.side_effect = lambda p: mock_get_relative_path(p)
            mock_manager.get_relative_path.side_effect = mock_get_relative_path
            
            async def always_authorize(tool_name, arguments, context, destructive):
                return True
            
            # Write file
            result = asyncio.run(filesystem_tool.execute({
                "operation": "write_file",
                "path": "test.txt",
                "content": test_content
            }, always_authorize))
            
            assert "File written successfully" in result
            assert test_file.exists()
            assert test_file.read_text() == test_content
            
            # Read file
            result = asyncio.run(filesystem_tool.execute({
                "operation": "read_file",
                "path": "test.txt"
            }, always_authorize))
            
            assert "File: test.txt" in result
            assert test_content in result
            
            # Append to file
            append_content = "\nAppended line"
            result = asyncio.run(filesystem_tool.execute({
                "operation": "append_to_file",
                "path": "test.txt",
                "content": append_content
            }, always_authorize))
            
            assert "Content appended successfully" in result
            assert test_file.read_text() == test_content + append_content
            
            # Get file info
            result = asyncio.run(filesystem_tool.execute({
                "operation": "get_info",
                "path": "test.txt"
            }, always_authorize))
            
            assert "File: test.txt" in result
            assert "Type: File" in result
            
            # Copy file
            result = asyncio.run(filesystem_tool.execute({
                "operation": "copy_file",
                "path": "test.txt",
                "destination": "test_copy.txt"
            }, always_authorize))
            
            assert "File copied successfully" in result
            test_copy = temp_directory / "test_copy.txt"
            assert test_copy.exists()
            assert test_copy.read_text() == test_file.read_text()
            
            # Delete original file
            result = asyncio.run(filesystem_tool.execute({
                "operation": "delete_file",
                "path": "test.txt"
            }, always_authorize))
            
            assert "File deleted successfully" in result
            assert not test_file.exists()
            assert test_copy.exists()  # Copy should still exist

    def test_end_to_end_directory_workflow(self, temp_directory):
        """Test end-to-end directory operations with real filesystem."""
        with patch('humbug.tools.tool_file_system.MindspaceManager') as mock_manager_class:
            mock_manager = MagicMock()
            mock_manager_class.return_value = mock_manager
            mock_manager.has_mindspace.return_value = True
            
            filesystem_tool = ToolFileSystem(max_file_size_mb=1)
            
            def mock_get_absolute_path(path):
                return str(temp_directory / path)
            
            def mock_get_relative_path(path):
                # Handle both resolved and unresolved paths
                path_obj = Path(path).resolve()
                try:
                    return str(path_obj.relative_to(temp_directory))
                except ValueError:
                    # If relative_to fails, try with the original path
                    return str(Path(path).relative_to(temp_directory))
            
            mock_manager.get_absolute_path.side_effect = mock_get_absolute_path
            mock_manager.get_mindspace_relative_path.side_effect = lambda p: mock_get_relative_path(p)
            mock_manager.get_relative_path.side_effect = mock_get_relative_path
            
            async def always_authorize(tool_name, arguments, context, destructive):
                return True
            
            # Create directory
            result = asyncio.run(filesystem_tool.execute({
                "operation": "create_directory",
                "path": "test_dir"
            }, always_authorize))
            
            assert "Directory created successfully" in result
            test_dir = temp_directory / "test_dir"
            assert test_dir.exists()
            assert test_dir.is_dir()
            
            # List empty directory
            result = asyncio.run(filesystem_tool.execute({
                "operation": "list_directory",
                "path": "test_dir"
            }, always_authorize))
            
            assert "Directory: test_dir" in result
            assert "Items: 0" in result
            
            # Create file in directory
            test_file = test_dir / "file.txt"
            test_file.write_text("test content")
            
            # List directory with file
            result = asyncio.run(filesystem_tool.execute({
                "operation": "list_directory",
                "path": "test_dir"
            }, always_authorize))
            
            assert "Items: 1" in result
            assert "📄 file.txt" in result
            
            # Remove file first
            result = asyncio.run(filesystem_tool.execute({
                "operation": "delete_file",
                "path": "test_dir/file.txt"
            }, always_authorize))
            
            assert "File deleted successfully" in result
            
            # Remove empty directory
            result = asyncio.run(filesystem_tool.execute({
                "operation": "remove_directory",
                "path": "test_dir"
            }, always_authorize))
            
            assert "Directory removed successfully" in result
            assert not test_dir.exists()

    def test_multiple_operations_independence(self, temp_directory):
        """Test that multiple operations are independent."""
        with patch('humbug.tools.tool_file_system.MindspaceManager') as mock_manager_class:
            mock_manager = MagicMock()
            mock_manager_class.return_value = mock_manager
            mock_manager.has_mindspace.return_value = True
            
            filesystem_tool = ToolFileSystem(max_file_size_mb=1)
            
            def mock_get_absolute_path(path):
                return str(temp_directory / path)
            
            def mock_get_relative_path(path):
                # Handle both resolved and unresolved paths
                path_obj = Path(path).resolve()
                try:
                    return str(path_obj.relative_to(temp_directory))
                except ValueError:
                    # If relative_to fails, try with the original path
                    return str(Path(path).relative_to(temp_directory))
            
            mock_manager.get_absolute_path.side_effect = mock_get_absolute_path
            mock_manager.get_mindspace_relative_path.side_effect = lambda p: mock_get_relative_path(p)
            mock_manager.get_relative_path.side_effect = mock_get_relative_path
            
            async def always_authorize(tool_name, arguments, context, destructive):
                return True
            
            # Create multiple files independently
            files = ["file1.txt", "file2.txt", "file3.txt"]
            
            for i, filename in enumerate(files):
                result = asyncio.run(filesystem_tool.execute({
                    "operation": "write_file",
                    "path": filename,
                    "content": f"Content of file {i+1}"
                }, always_authorize))
                
                assert "File written successfully" in result
            
            # Verify all files exist and have correct content
            for i, filename in enumerate(files):
                file_path = temp_directory / filename
                assert file_path.exists()
                assert file_path.read_text() == f"Content of file {i+1}"
            
            # Operations on one file shouldn't affect others
            result = asyncio.run(filesystem_tool.execute({
                "operation": "delete_file",
                "path": "file2.txt"
            }, always_authorize))
            
            assert "File deleted successfully" in result
            assert not (temp_directory / "file2.txt").exists()
            assert (temp_directory / "file1.txt").exists()
            assert (temp_directory / "file3.txt").exists()