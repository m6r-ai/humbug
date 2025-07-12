"""
Tests for the filesystem tool definition and basic validation.
"""
import asyncio
from pathlib import Path
from typing import Tuple

import pytest

from ai.ai_tool_manager import (
    AITool, AIToolDefinition, AIToolParameter, AIToolExecutionError
)
from ai.tools.ai_tool_filesystem import AIToolFileSystem


class TestAIToolFileSystemDefinition:
    """Test the filesystem tool definition."""

    def test_get_definition_returns_correct_structure(self, mock_path_resolver):
        """Test that get_definition returns the correct tool definition structure."""
        filesystem_tool = AIToolFileSystem(resolve_path=mock_path_resolver)
        definition = filesystem_tool.get_definition()

        assert isinstance(definition, AIToolDefinition)
        assert definition.name == "filesystem"
        assert "Perform filesystem operations within the current mindspace" in definition.description
        assert "Maximum file size: 10MB" in definition.description
        assert len(definition.parameters) == 6

    def test_operation_parameter_definition(self, mock_path_resolver):
        """Test the operation parameter definition."""
        filesystem_tool = AIToolFileSystem(resolve_path=mock_path_resolver)
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

    def test_path_parameter_definition(self, mock_path_resolver):
        """Test the path parameter definition."""
        filesystem_tool = AIToolFileSystem(resolve_path=mock_path_resolver)
        definition = filesystem_tool.get_definition()
        path_param = definition.parameters[1]

        assert isinstance(path_param, AIToolParameter)
        assert path_param.name == "path"
        assert path_param.type == "string"
        assert "Path to file or directory" in path_param.description
        assert path_param.required is True
        assert path_param.enum is None

    def test_optional_parameters_definition(self, mock_path_resolver):
        """Test the optional parameters definitions."""
        filesystem_tool = AIToolFileSystem(resolve_path=mock_path_resolver)
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

    def test_custom_max_file_size_in_definition(self, mock_path_resolver):
        """Test that custom max file size is reflected in definition."""
        custom_tool = AIToolFileSystem(resolve_path=mock_path_resolver, max_file_size_mb=5)
        definition = custom_tool.get_definition()

        assert "Maximum file size: 5MB" in definition.description


class TestAIToolFileSystemValidation:
    """Test validation through public interface."""

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

    def test_path_outside_boundaries(self, custom_path_resolver, mock_authorization):
        """Test error when path is outside allowed boundaries."""
        def validation_func(path: str):
            if '..' in path:
                raise ValueError(f"Path is outside allowed boundaries: {path}")

        resolver = custom_path_resolver(validation_func=validation_func)
        filesystem_tool = AIToolFileSystem(resolve_path=resolver)

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "../../../outside/file.txt"},
                mock_authorization
            ))

        error = exc_info.value
        assert "Path is outside allowed boundaries" in str(error)

    def test_path_resolver_error(self, mock_authorization):
        """Test error when path resolver raises an error."""
        def failing_resolver(path: str) -> Tuple[Path, str]:
            raise ValueError("Custom resolver error")

        filesystem_tool = AIToolFileSystem(resolve_path=failing_resolver)

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(
                {"operation": "read_file", "path": "file.txt"},
                mock_authorization
            ))

        error = exc_info.value
        assert "Custom resolver error" in str(error)

    def test_execute_missing_operation(self, filesystem_tool, mock_authorization):
        """Test execute without operation parameter."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute({"path": "file.txt"}, mock_authorization))

        error = exc_info.value
        assert "No 'operation' argument provided" in str(error)

    def test_execute_invalid_operation(self, filesystem_tool, mock_authorization):
        """Test execute with invalid operation."""
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute({"operation": "invalid_op", "path": "file.txt"}, mock_authorization))

        error = exc_info.value
        assert "Unsupported operation: invalid_op" in str(error)


class TestAIToolFileSystemInheritance:
    """Test tool inheritance and interface compliance."""

    def test_tool_inheritance(self, mock_path_resolver):
        """Test that AIToolFileSystem properly inherits from AITool."""
        filesystem_tool = AIToolFileSystem(resolve_path=mock_path_resolver)
        assert isinstance(filesystem_tool, AITool)
        assert hasattr(filesystem_tool, 'get_definition')
        assert hasattr(filesystem_tool, 'execute')
        assert callable(filesystem_tool.get_definition)
        assert callable(filesystem_tool.execute)

    def test_custom_max_file_sizes(self, mock_path_resolver):
        """Test filesystem tool with different max file sizes."""
        test_cases = [
            (1, 1024 * 1024),
            (5, 5 * 1024 * 1024),
            (10, 10 * 1024 * 1024),
            (50, 50 * 1024 * 1024),
        ]

        for max_size_mb, expected_bytes in test_cases:
            tool = AIToolFileSystem(resolve_path=mock_path_resolver, max_file_size_mb=max_size_mb)
            assert tool._max_file_size_bytes == expected_bytes

            definition = tool.get_definition()
            assert f"Maximum file size: {max_size_mb}MB" in definition.description
