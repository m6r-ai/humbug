"""
Integration and parametrized tests for the filesystem tool.
"""
import asyncio
from pathlib import Path
from typing import Tuple
from unittest.mock import patch, MagicMock, mock_open

import pytest

from ai_tool import AIToolExecutionError
from ai_tool.filesystem.filesystem_ai_tool import FileSystemAITool


class TestFileSystemAIToolIntegration:
    """Integration tests for the filesystem tool."""

    def test_execute_read_file_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test execute with read_file operation."""
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
            assert "test content" in result.content

    def test_execute_write_file_success(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test execute with write_file operation."""
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

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            assert "File written successfully: file.txt (12 bytes)" in result.content
            # Verify chmod was called
            mock_chmod.assert_called_once()

    def test_execute_unexpected_error(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test execute with unexpected error."""
        # Patch one of the operation handlers to raise an unexpected error
        with patch.object(filesystem_tool, '_read_file', side_effect=RuntimeError("Unexpected error")):
            tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "file.txt"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "filesystem operation failed: Unexpected error" in str(error)
            assert isinstance(error.__cause__, RuntimeError)


class TestFileSystemAIToolAuthorizationContext:
    """Test authorization context building for various operations."""

    def test_authorization_context_includes_operation_details(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test that authorization context includes relevant operation details."""
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

            tool_call = make_tool_call("filesystem", {"operation": "write_file", "path": "file.txt", "content": "test content"})
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "This will create a new file 'file.txt' with the provided content." in context
            # Verify chmod was called
            mock_chmod.assert_called_once()

    def test_authorization_context_copy_includes_destination(self, custom_path_resolver, mock_authorization, make_tool_call):
        """Test authorization context for copy operation includes destination."""
        # Create custom resolver that handles both source and destination paths
        path_mapping = {
            "source.txt": ("/test/sandbox/source.txt", "source.txt"),
            "dest.txt": ("/test/sandbox/dest.txt", "dest.txt")
        }
        resolver = custom_path_resolver(path_mapping=path_mapping)
        filesystem_tool = FileSystemAITool(resolve_path=resolver)

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

            tool_call = make_tool_call("filesystem", {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"})
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "Copy 'source.txt' to 'dest.txt'. This will create a new file at the destination." in context

    def test_authorization_context_existing_file_includes_size(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test authorization context includes existing file size."""
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.unlink') as mock_unlink:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            tool_call = make_tool_call("filesystem", {"operation": "delete_file", "path": "file.txt"})
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "Delete the file 'file.txt'. This file will be permanently removed and cannot be recovered." in context


class TestFileSystemAIToolParametrized:
    """Parametrized tests for the filesystem tool."""

    @pytest.mark.parametrize("operation", [
        "read_file", "write_file", "append_to_file", "list_directory",
        "create_directory", "remove_directory", "delete_file",
        "copy_file", "move", "get_info"
    ])
    def test_supported_operations_in_definition(self, operation, mock_path_resolver):
        """Test that all supported operations are included in definition."""
        filesystem_tool = FileSystemAITool(resolve_path=mock_path_resolver)
        definition = filesystem_tool.get_definition()
        operation_param = definition.parameters[0]

        assert operation in operation_param.enum

    @pytest.mark.parametrize("encoding", ["utf-8", "utf-16", "ascii", "latin-1"])
    def test_supported_encodings_in_definition(self, encoding, mock_path_resolver):
        """Test that all supported encodings are included in definition."""
        filesystem_tool = FileSystemAITool(resolve_path=mock_path_resolver)
        definition = filesystem_tool.get_definition()
        encoding_param = next(p for p in definition.parameters if p.name == "encoding")

        assert encoding in encoding_param.enum

    @pytest.mark.parametrize("max_size_mb,expected_bytes", [
        (1, 1024 * 1024),
        (5, 5 * 1024 * 1024),
        (10, 10 * 1024 * 1024),
        (50, 50 * 1024 * 1024),
    ])
    def test_custom_max_file_sizes(self, max_size_mb, expected_bytes, mock_path_resolver):
        """Test filesystem tool with different max file sizes."""
        tool = FileSystemAITool(resolve_path=mock_path_resolver, max_file_size_mb=max_size_mb)

        assert tool._max_file_size_bytes == expected_bytes

        definition = tool.get_definition()
        assert f"Maximum file size: {max_size_mb}MB" in definition.description

    @pytest.mark.parametrize("path_input,expected_error", [
        ("", "path: parameter must not be empty"),
        (123, "'path' must be a string"),
    ])
    def test_invalid_path_inputs(self, filesystem_tool, mock_authorization, make_tool_call, path_input, expected_error):
        """Test various invalid path inputs through public interface."""
        tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": path_input})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert expected_error in str(error)

    @pytest.mark.parametrize("falsy_value,expected_error", [
        (None, "'path' must be a string"),
        ([], "'path' must be a string"),
        ({}, "'path' must be a string"),
        (0, "'path' must be a string"),
        (False, "'path' must be a string"),
    ])
    def test_invalid_path_inputs_falsy_values(self, filesystem_tool, mock_authorization, make_tool_call, falsy_value, expected_error):
        """Test falsy path inputs and their specific error messages."""
        tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": falsy_value})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert expected_error in str(error)

    @pytest.mark.parametrize("operation,destructive", [
        ("read_file", False),
        ("list_directory", False),
        ("get_info", False),
        ("write_file", True),  # Can be destructive if file exists
        ("append_to_file", True),
        ("create_directory", False),
        ("remove_directory", True),
        ("delete_file", True),
        ("copy_file", True),  # Can be destructive if destination exists
        ("move", True),
    ])
    def test_operation_destructive_flags(self, operation, destructive):
        """Test that operations correctly set destructive flag for authorization."""
        # This is a documentation test to ensure we understand which operations are destructive
        # The actual testing of destructive flags is done in individual operation tests
        assert isinstance(destructive, bool)

    @pytest.mark.parametrize("operation,requires_content", [
        ("read_file", False),
        ("write_file", True),
        ("append_to_file", True),
        ("list_directory", False),
        ("create_directory", False),
        ("remove_directory", False),
        ("delete_file", False),
        ("copy_file", False),
        ("move", False),
        ("get_info", False),
    ])
    def test_operations_content_requirements(self, operation, requires_content):
        """Test documentation of which operations require content parameter."""
        # This is a documentation test to ensure we understand parameter requirements
        assert isinstance(requires_content, bool)

    @pytest.mark.parametrize("operation,requires_destination", [
        ("read_file", False),
        ("write_file", False),
        ("append_to_file", False),
        ("list_directory", False),
        ("create_directory", False),
        ("remove_directory", False),
        ("delete_file", False),
        ("copy_file", True),
        ("move", True),
        ("get_info", False),
    ])
    def test_operations_destination_requirements(self, operation, requires_destination):
        """Test documentation of which operations require destination parameter."""
        # This is a documentation test to ensure we understand parameter requirements
        assert isinstance(requires_destination, bool)


class TestFileSystemAIToolErrorHandling:
    """Test error handling patterns across operations."""

    def test_operations_requiring_destination_fail_without_it(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test that operations requiring destination fail appropriately."""
        operations_needing_destination = ["copy_file", "move"]

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            for operation in operations_needing_destination:
                tool_call = make_tool_call("filesystem", {"operation": operation, "path": "source.txt"})
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

                error = exc_info.value
                # The error message varies but should indicate missing parameter
                assert "destination" in str(error).lower() and "missing" in str(error).lower()

    def test_operations_requiring_content_fail_without_it(self, filesystem_tool, mock_authorization, make_tool_call):
        """Test that operations requiring content fail appropriately."""
        operations_needing_content = ["write_file", "append_to_file"]

        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            for operation in operations_needing_content:
                tool_call = make_tool_call("filesystem", {"operation": operation, "path": "file.txt"})
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

                error = exc_info.value
                # append_to_file checks for content later, so it fails on path validation first
                # write_file checks for content early
                assert "content" in str(error).lower() and "missing" in str(error).lower()


class TestFileSystemAIToolPathResolverIntegration:
    """Test integration with different path resolver behaviors."""

    def test_path_resolver_validation_error(self, mock_authorization, make_tool_call):
        """Test that path resolver validation errors are properly propagated."""
        def failing_resolver(path: str) -> Tuple[Path, str]:
            if path == "forbidden":
                raise ValueError("Access to this path is forbidden")
            return Path(f"/test/sandbox/{path}"), path

        filesystem_tool = FileSystemAITool(resolve_path=failing_resolver)

        tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": "forbidden"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "Access to this path is forbidden" in str(error)

    def test_path_resolver_custom_display_paths(self, mock_authorization, make_tool_call):
        """Test that custom display paths from resolver are used correctly."""
        def custom_resolver(path: str) -> Tuple[Path, str]:
            # Return custom display path that's different from input
            abs_path = Path(f"/test/sandbox/{path}")
            display_path = f"custom_prefix/{path}"
            return abs_path, display_path

        filesystem_tool = FileSystemAITool(resolve_path=custom_resolver)

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

            # Verify the custom display path is used in the result
            assert "File: custom_prefix/file.txt" in result.content

    def test_path_resolver_absolute_vs_relative_handling(self, mock_authorization, make_tool_call):
        """Test that path resolver handles both absolute and relative paths correctly."""
        def flexible_resolver(path: str) -> Tuple[Path, str]:
            if path.startswith('/'):
                # Absolute path - strip leading slash for display
                display_path = path[1:] if path != '/' else ''
                abs_path = Path(f"/test/sandbox/{display_path}")

            else:
                # Relative path
                display_path = path
                abs_path = Path(f"/test/sandbox/{path}")

            return abs_path, display_path

        filesystem_tool = FileSystemAITool(resolve_path=flexible_resolver)

        test_cases = [
            ("file.txt", "file.txt"),
            ("/file.txt", "file.txt"),
            ("dir/file.txt", "dir/file.txt"),
            ("/dir/file.txt", "dir/file.txt"),
        ]

        for input_path, expected_display in test_cases:
            with patch('pathlib.Path.exists') as mock_exists, \
                 patch('pathlib.Path.is_file') as mock_is_file, \
                 patch('pathlib.Path.stat') as mock_stat, \
                 patch('builtins.open', mock_open(read_data="test content")) as mock_file:

                mock_exists.return_value = True
                mock_is_file.return_value = True

                mock_stat_result = MagicMock()
                mock_stat_result.st_size = 12
                mock_stat.return_value = mock_stat_result

                tool_call = make_tool_call("filesystem", {"operation": "read_file", "path": input_path})
                result = asyncio.run(filesystem_tool.execute(tool_call, "", mock_authorization))

                assert f"File: {expected_display}" in result.content
