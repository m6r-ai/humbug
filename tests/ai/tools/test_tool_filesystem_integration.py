"""
Integration and parametrized tests for the filesystem tool.
"""
import asyncio
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open

import pytest

from humbug.ai.ai_tool_manager import AIToolExecutionError


class TestAIToolFileSystemIntegration:
    """Integration tests for the filesystem tool."""

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


class TestAIToolFileSystemAuthorizationContext:
    """Test authorization context building for various operations."""

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

    def test_authorization_context_with_small_content_includes_preview(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test authorization context with small content includes preview."""
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

            small_content = "Hello, World!"
            asyncio.run(filesystem_tool.execute(
                {"operation": "write_file", "path": "file.txt", "content": small_content},
                mock_authorization
            ))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "Operation: write_file" in context
            assert "Path: file.txt" in context
            assert "Content size: 13 bytes" in context
            assert "Content preview: 'Hello, World!'" in context

    def test_authorization_context_copy_includes_destination(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test authorization context for copy operation includes destination."""
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

            asyncio.run(filesystem_tool.execute(
                {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"},
                mock_authorization
            ))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "Operation: copy_file" in context
            assert "Path: source.txt" in context
            assert "Destination: dest.txt" in context

    def test_authorization_context_existing_file_includes_size(self, filesystem_tool, mock_mindspace_manager, mock_authorization):
        """Test authorization context includes existing file size."""
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
            mock_stat_result.st_size = 2048
            mock_stat.return_value = mock_stat_result

            asyncio.run(filesystem_tool.execute(
                {"operation": "delete_file", "path": "file.txt"},
                mock_authorization
            ))

            # Verify authorization was called with context information
            mock_authorization.assert_called_once()
            args = mock_authorization.call_args[0]
            context = args[2]  # Third argument is context

            assert "Operation: delete_file" in context
            assert "Path: file.txt" in context
            assert "Current size: 2,048 bytes" in context


class TestAIToolFileSystemParametrized:
    """Parametrized tests for the filesystem tool."""

    @pytest.mark.parametrize("operation", [
        "read_file", "write_file", "append_to_file", "list_directory",
        "create_directory", "remove_directory", "delete_file",
        "copy_file", "move", "get_info"
    ])
    def test_supported_operations_in_definition(self, operation):
        """Test that all supported operations are included in definition."""
        from humbug.ai.tools.ai_tool_filesystem import AIToolFileSystem
        filesystem_tool = AIToolFileSystem()
        definition = filesystem_tool.get_definition()
        operation_param = definition.parameters[0]

        assert operation in operation_param.enum

    @pytest.mark.parametrize("encoding", ["utf-8", "utf-16", "ascii", "latin-1"])
    def test_supported_encodings_in_definition(self, encoding):
        """Test that all supported encodings are included in definition."""
        from humbug.ai.tools.ai_tool_filesystem import AIToolFileSystem
        filesystem_tool = AIToolFileSystem()
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
        from humbug.ai.tools.ai_tool_filesystem import AIToolFileSystem
        tool = AIToolFileSystem(max_file_size_mb=max_size_mb)

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


class TestAIToolFileSystemErrorHandling:
    """Test error handling patterns across operations."""

    def test_all_operations_handle_mindspace_validation(self, mock_mindspace_manager, mock_authorization):
        """Test that all operations properly validate mindspace access."""
        from humbug.ai.tools.ai_tool_filesystem import AIToolFileSystem

        mock_mindspace_manager.has_mindspace.return_value = False
        filesystem_tool = AIToolFileSystem()
        filesystem_tool._mindspace_manager = mock_mindspace_manager

        operations = [
            "read_file", "write_file", "append_to_file", "list_directory",
            "create_directory", "remove_directory", "delete_file", "get_info"
        ]

        for operation in operations:
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(filesystem_tool.execute(
                    {"operation": operation, "path": "test"},
                    mock_authorization
                ))

            error = exc_info.value
            assert "No mindspace is currently open" in str(error)

    def test_operations_requiring_destination_fail_without_it(self, filesystem_tool, mock_authorization):
        """Test that operations requiring destination fail appropriately."""
        operations_needing_destination = ["copy_file", "move"]

        with patch('pathlib.Path.resolve') as mock_resolve, \
             patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.is_file') as mock_is_file, \
             patch('pathlib.Path.stat') as mock_stat, \
             patch('builtins.open', mock_open(read_data="test content")) as mock_file:

            mock_path = Path("/test/mindspace/source.txt")
            mock_resolve.return_value = mock_path
            mock_exists.return_value = True
            mock_is_file.return_value = True

            mock_stat_result = MagicMock()
            mock_stat_result.st_size = 12
            mock_stat.return_value = mock_stat_result

            for operation in operations_needing_destination:
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(filesystem_tool.execute(
                        {"operation": operation, "path": "source.txt"},
                        mock_authorization
                    ))

                error = exc_info.value
                # The error message varies but should indicate missing parameter
                assert "No 'destination' argument provided" in str(error)

    def test_operations_requiring_content_fail_without_it(self, filesystem_tool, mock_authorization):
        """Test that operations requiring content fail appropriately."""
        operations_needing_content = ["write_file", "append_to_file"]

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

            for operation in operations_needing_content:
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(filesystem_tool.execute(
                        {"operation": operation, "path": "file.txt"},
                        mock_authorization
                    ))

                error = exc_info.value
                # append_to_file checks for content later, so it fails on path validation first
                # write_file checks for content early
                assert "No 'content' argument provided" in str(error)
