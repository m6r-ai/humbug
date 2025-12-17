import json
import logging
import os
import shutil
import tempfile
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Any, List, Callable, Tuple, cast

from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback, AIToolOperationDefinition,
    AIToolResult, AIToolCall
)
from ai_tool.filesystem.filesystem_diff_applier import FilesystemDiffApplier
from diff import DiffParseError, DiffMatchError, DiffValidationError, DiffApplicationError
from syntax.programming_language_utils import ProgrammingLanguageUtils


class FileSystemAITool(AITool):
    """
    Comprehensive filesystem tool.

    All operations require user authorization and are restricted to filesystem boundaries.
    Provides secure file and directory operations with proper error handling and logging.
    """

    def __init__(self, resolve_path: Callable[[str], Tuple[Path, str]], max_file_size_mb: int = 10):
        """
        Initialize the filesystem tool.

        Args:
            resolve_path: Callback to resolve and validate paths, returns (absolute_path, display_path)
            max_file_size_mb: Maximum file size in MB for read/write operations
        """
        self._resolve_path = resolve_path
        self._max_file_size_bytes = max_file_size_mb * 1024 * 1024
        self._logger = logging.getLogger("FileSystemAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        operations = self.get_operation_definitions()
        operation_names: List[str] = list(operations.keys())

        # Build description from operations
        base_description = (
            f"The filesystem tool lets you (the AI) perform various file and directory operations within the current "
            f"filesystem sandbox environment (this sandbox is called the project mindspace). "
            f"All write operations require user authorization before proceeding. "
            f"Maximum file size: {self._max_file_size_bytes // (1024 * 1024)}MB."
        )

        # Generate operations list
        operation_list = []
        for name, op_def in operations.items():
            operation_list.append(f"- {name}: {op_def.description}")

        description = f"{base_description}\nAvailable operations are:\n" + "\n".join(operation_list)

        return AIToolDefinition(
            name="filesystem",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Filesystem operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="path",
                    type="string",
                    description="Path to file or directory (relative to sandbox root or absolute)",
                    required=True
                ),
                AIToolParameter(
                    name="content",
                    type="string",
                    description="Content to write (for write_file and append_to_file operations)",
                    required=False
                ),
                AIToolParameter(
                    name="destination",
                    type="string",
                    description="Destination path (for copy_file and move operations)",
                    required=False
                ),
                AIToolParameter(
                    name="encoding",
                    type="string",
                    description="Text encoding to use for file operations",
                    required=False,
                    enum=["utf-8", "utf-16", "ascii", "latin-1"]
                ),
                AIToolParameter(
                    name="format",
                    type="string",
                    description="Time format for timestamps ('iso' or 'timestamp')",
                    required=False,
                    enum=["iso", "timestamp"]
                ),
                AIToolParameter(
                    name="create_parents",
                    type="boolean",
                    description="Create parent directories if they don't exist (for write operations)",
                    required=False
                ),
                AIToolParameter(
                    name="start_line",
                    type="integer",
                    description="Start line number (1-indexed) for read_file_lines operation",
                    required=False
                ),
                AIToolParameter(
                    name="end_line",
                    type="integer",
                    description="End line number (1-indexed) for read_file_lines operation",
                    required=False
                ),
                AIToolParameter(
                    name="diff_content",
                    type="string",
                    description="Unified diff content to apply (for apply_diff operation)",
                    required=False
                ),
                AIToolParameter(
                    name="dry_run",
                    type="boolean",
                    description="If True, validate diff without applying changes (for apply_diff operation)",
                    required=False
                )
            ]
        )

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "read_file": AIToolOperationDefinition(
                name="read_file",
                handler=self._read_file,
                extract_context=None,
                allowed_parameters={"path", "encoding"},
                required_parameters={"path"},
                description="Read file contents"
            ),
            "read_file_lines": AIToolOperationDefinition(
                name="read_file_lines",
                handler=self._read_file_lines,
                extract_context=None,
                allowed_parameters={"path", "encoding", "start_line", "end_line"},
                required_parameters={"path"},
                description="Read file contents with line numbers. Optionally specify start_line and end_line "
                    "(1-indexed, inclusive) to read a specific range of lines. Returns a dictionary with integer "
                    "line numbers as keys and line content as string values. Use this when you only need specific "
                    "lines or when line numbers are important"
            ),
            "write_file": AIToolOperationDefinition(
                name="write_file",
                handler=self._write_file,
                extract_context=self._write_file_context,
                allowed_parameters={"path", "content", "encoding", "create_parents"},
                required_parameters={"path", "content"},
                description="Write content to file (create or overwrite)"
            ),
            "append_to_file": AIToolOperationDefinition(
                name="append_to_file",
                handler=self._append_to_file,
                extract_context=self._append_to_file_context,
                allowed_parameters={"path", "content", "encoding"},
                required_parameters={"path", "content"},
                description="Append content to existing file"
            ),
            "apply_diff_to_file": AIToolOperationDefinition(
                name="apply_diff_to_file",
                handler=self._apply_diff_to_file,
                extract_context=self._apply_diff_to_file_context,
                allowed_parameters={"path", "diff_content", "dry_run", "encoding"},
                required_parameters={"path", "diff_content"},
                description="Apply a unified diff to a file. The diff is applied with fuzzy matching to handle "
                    "minor line movements. If dry_run is True, validates the diff without applying changes. "
            ),
            "delete_file": AIToolOperationDefinition(
                name="delete_file",
                handler=self._delete_file,
                extract_context=None,
                allowed_parameters={"path"},
                required_parameters={"path"},
                description="Delete file"
            ),
            "copy_file": AIToolOperationDefinition(
                name="copy_file",
                handler=self._copy_file,
                extract_context=None,
                allowed_parameters={"path", "destination"},
                required_parameters={"path", "destination"},
                description="Copy file to destination"
            ),
            "list_directory": AIToolOperationDefinition(
                name="list_directory",
                handler=self._list_directory,
                extract_context=None,
                allowed_parameters={"path"},
                required_parameters={"path"},
                description="List directory contents"
            ),
            "create_directory": AIToolOperationDefinition(
                name="create_directory",
                handler=self._create_directory,
                extract_context=None,
                allowed_parameters={"path", "create_parents"},
                required_parameters={"path"},
                description="Create directory"
            ),
            "remove_directory": AIToolOperationDefinition(
                name="remove_directory",
                handler=self._remove_directory,
                extract_context=None,
                allowed_parameters={"path"},
                required_parameters={"path"},
                description="Remove empty directory"
            ),
            "move": AIToolOperationDefinition(
                name="move",
                handler=self._move,
                extract_context=None,
                allowed_parameters={"path", "destination"},
                required_parameters={"path", "destination"},
                description="Move/rename file or directory"
            ),
            "get_info": AIToolOperationDefinition(
                name="get_info",
                handler=self._get_info,
                extract_context=None,
                allowed_parameters={"path", "format"},
                required_parameters={"path"},
                description="Get detailed information about file or directory. "
                    "If format is not specified timestamps are in ISO format."
            )
        }

    def _validate_and_resolve_path(self, key: str, path_str: str) -> Tuple[Path, str]:
        """
        Validate path and resolve to absolute path with display path.

        Args:
            path_str: String path to validate and resolve
            operation: Operation being performed (for error context)

        Returns:
            Tuple of (resolved absolute Path, display path string)

        Raises:
            AIToolExecutionError: If path is invalid or outside boundaries
        """
        if not path_str:
            raise AIToolExecutionError(f"{key}: parameter must not be empty")

        try:
            return self._resolve_path(path_str)

        except ValueError as e:
            raise AIToolExecutionError(f"{key}: invalid path '{path_str}': {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"{key}: failed to resolve path '{path_str}': {str(e)}") from e

    def _format_time(self, dt: datetime, format_type: str | None) -> str:
        """
        Format datetime according to the specified format.

        Args:
            dt: Datetime to format
            format_type: Format type ('iso' or 'timestamp'), defaults to 'iso' if None

        Returns:
            Formatted time string
        """
        # Default to ISO format if not specified
        if format_type is None or format_type == "iso":
            return dt.isoformat()[:26] + "Z" if dt.tzinfo == timezone.utc else dt.isoformat()

        if format_type == "timestamp":
            return str(int(dt.timestamp()))

        # Fallback to ISO format for unknown types
        return dt.isoformat()[:26] + "Z" if dt.tzinfo == timezone.utc else dt.isoformat()

    def extract_context(self, tool_call: AIToolCall) -> str | None:
        """
        Extract context from the tool call.

        Args:
            tool_call: The tool call object

        Returns:
            Context string if applicable, otherwise None
        """
        arguments = tool_call.arguments
        operation = arguments.get("operation")
        if not operation:
            return None

        if not isinstance(operation, str):
            return None

        # Get operation definition
        operation_definitions = self.get_operation_definitions()
        if operation not in operation_definitions:
            return None

        operation_def = operation_definitions[operation]
        extract_context = operation_def.extract_context
        if extract_context is None:
            return None

        try:
            return extract_context(arguments)

        except AIToolExecutionError:
            # Ignore errors during context extraction
            return None

    async def _read_file(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read file contents."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        # Validate file exists and is readable
        if not path.exists():
            raise AIToolExecutionError(f"File does not exist: {arguments['path']}")

        if not path.is_file():
            raise AIToolExecutionError(f"Path is not a file: {arguments['path']}")

        # Check file size
        file_size = path.stat().st_size
        if file_size > self._max_file_size_bytes:
            size_mb = file_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(f"File too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)")

        encoding = self._get_optional_str_value("encoding", arguments, "utf-8")

        # Read file content
        try:
            with open(path, 'r', encoding=encoding) as f:
                content = f.read()

            actual_size = path.stat().st_size

        except UnicodeDecodeError as e:
            raise AIToolExecutionError(
                f"Failed to decode file with encoding '{encoding}': {str(e)}. Try a different encoding."
            ) from e

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied reading file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to read file: {str(e)}") from e

        language = ProgrammingLanguageUtils.from_file_extension(path.name)
        language_str = ProgrammingLanguageUtils.get_name(language)

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"File: {display_path}\nSize: {actual_size:,} bytes\nEncoding: {encoding}\n\n{content}",
            context=f"`content` is:\n```{language_str}\n{content}\n```"
        )

    async def _read_file_lines(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read file contents with line numbers."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        # Validate file exists and is readable
        if not path.exists():
            raise AIToolExecutionError(f"File does not exist: {arguments['path']}")

        if not path.is_file():
            raise AIToolExecutionError(f"Path is not a file: {arguments['path']}")

        # Check file size
        file_size = path.stat().st_size
        if file_size > self._max_file_size_bytes:
            size_mb = file_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(f"File too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)")

        encoding = self._get_optional_str_value("encoding", arguments, "utf-8")
        start_line = self._get_optional_int_value("start_line", arguments)
        end_line = self._get_optional_int_value("end_line", arguments)

        # Read file content
        try:
            with open(path, 'r', encoding=encoding) as f:
                content = f.read()

            actual_size = path.stat().st_size

        except UnicodeDecodeError as e:
            raise AIToolExecutionError(
                f"Failed to decode file with encoding '{encoding}': {str(e)}. Try a different encoding."
            ) from e

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied reading file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to read file: {str(e)}") from e

        # Build line-numbered content
        context_object = {}
        if not content:
            context_object[1] = ""

        else:
            content_lines = content.splitlines()
            total_lines = len(content_lines)

            # Apply line range if specified
            actual_start = (start_line if start_line is not None else 1)
            actual_end = (end_line if end_line is not None else total_lines)

            # Validate range
            if actual_start < 1:
                raise AIToolExecutionError(f"'start_line' must be >= 1, got {actual_start}")

            if actual_end < actual_start:
                raise AIToolExecutionError(f"'end_line' ({actual_end}) must be >= 'start_line' ({actual_start})")

            if actual_start > total_lines:
                raise AIToolExecutionError(f"'start_line' ({actual_start}) exceeds file length ({total_lines} lines)")

            # Build result (end_line can exceed total_lines, just truncate)
            last_line = min(actual_end, total_lines)
            for line_num in range(actual_start, last_line + 1):
                context_object[line_num] = content_lines[line_num - 1]

        # Build header with optional range description
        range_desc = ""
        if start_line is not None or end_line is not None:
            range_desc = f" (lines {start_line or 1}-{end_line or 'end'})"

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"File: {display_path}{range_desc}\nSize: {actual_size:,} bytes\nEncoding: {encoding}\n\n{str(context_object)}",
            context=f"`content` is:\n```json\n{json.dumps(context_object, indent=2)}\n```"
        )

    def _write_file_context(self, arguments: Dict[str, Any]) -> str | None:
        """Extract context for write_file operation."""
        path_arg = self._get_required_str_value("path", arguments)
        context = self._get_required_str_value("content", arguments)
        language = ProgrammingLanguageUtils.from_file_extension(path_arg)
        language_str = ProgrammingLanguageUtils.get_name(language)
        return f"`content` is:\n```{language_str}\n{context}\n```"

    async def _write_file(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Write content to file (create or overwrite)."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        content = self._get_required_str_value("content", arguments)
        encoding = cast(str, self._get_optional_str_value("encoding", arguments, "utf-8"))
        create_parents = self._get_optional_bool_value("create_parents", arguments, False)

        # Check content size
        content_size = len(content.encode(encoding))
        if content_size > self._max_file_size_bytes:
            size_mb = content_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(f"'content' too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)")

        # Build authorization context
        if path.exists():
            context = f"This will write content to '{display_path}'. " \
                "It will overwrite the existing file and the previous contents will be lost."
            destructive = True

        else:
            context = f"This will create a new file '{display_path}' with the provided content."
            if create_parents and not path.parent.exists():
                # Get display path for parent directory
                try:
                    _, parent_display_path = self._resolve_path(str(path.parent))
                    context += f" It will also create the parent directory '{parent_display_path}'."

                except Exception:
                    # If we can't resolve parent for display, just mention it generically
                    context += " It will also create the parent directory."

            destructive = False

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, None, destructive)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to write file: {arguments['path']}")

        # Write file content
        try:
            # Create parent directories if requested
            if create_parents:
                path.parent.mkdir(parents=True, exist_ok=True)

            # Write to temporary file first, then rename for atomicity
            with tempfile.NamedTemporaryFile(
                mode='w',
                encoding=encoding,
                dir=path.parent,
                delete=False,
                suffix='.tmp'
            ) as tmp_file:
                tmp_file.write(content)
                tmp_path = Path(tmp_file.name)

            # Atomic rename
            tmp_path.replace(path)

            # Set proper permissions based on umask
            umask = os.umask(0)
            os.umask(umask)
            desired_mode = 0o666 & ~umask
            path.chmod(desired_mode)

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied writing file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to write file: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"File written successfully: {display_path} ({content_size:,} bytes)"
        )

    def _append_to_file_context(self, arguments: Dict[str, Any]) -> str | None:
        """Extract context for append_to_file operation."""
        path_arg = self._get_required_str_value("path", arguments)
        if not path_arg:
            return None

        context = self._get_required_str_value("content", arguments)
        language = ProgrammingLanguageUtils.from_file_extension(path_arg)
        language_str = ProgrammingLanguageUtils.get_name(language)
        return f"`content` is:\n```{language_str}\n{context}\n```"

    async def _append_to_file(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Append content to existing file."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        # File must exist for append
        if not path.exists():
            raise AIToolExecutionError(f"File does not exist: {arguments['path']}")

        if not path.is_file():
            raise AIToolExecutionError(f"Path is not a file: {arguments['path']}")

        content = self._get_required_str_value("content", arguments)
        encoding = cast(str, self._get_optional_str_value("encoding", arguments, "utf-8"))

        # Check total size after append
        current_size = path.stat().st_size
        content_size = len(content.encode(encoding))
        total_size = current_size + content_size

        if total_size > self._max_file_size_bytes:
            total_mb = total_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(
                f"File would be too large after append: {total_mb:.1f}MB (max: {max_mb:.1f}MB)"
            )

        # Build authorization context
        context = f"This will append content to the end of '{display_path}'. It will modify the existing file."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, None, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to append to file: {arguments['path']}")

        # Append to file
        try:
            with open(path, 'a', encoding=encoding) as f:
                f.write(content)

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied appending to file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to append to file: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"Content appended successfully: {display_path} (+{content_size:,} bytes)"
        )

    async def _list_directory(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """List directory contents."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        if not path.exists():
            raise AIToolExecutionError(f"Directory does not exist: {arguments['path']}")

        if not path.is_dir():
            raise AIToolExecutionError(f"Path is not a directory: {arguments['path']}")

        # List directory contents
        try:
            items: List[Dict[str, Any]] = []
            for path_item in path.iterdir():
                try:
                    if path_item.is_file():
                        size = path_item.stat().st_size
                        items.append({
                            'name': path_item.name,
                            'type': 'file',
                            'size': size
                        })

                    elif path_item.is_dir():
                        items.append({
                            'name': path_item.name,
                            'type': 'directory',
                            'size': None
                        })

                    else:
                        items.append({
                            'name': path_item.name,
                            'type': 'other',
                            'size': None
                        })

                except (PermissionError, OSError):
                    # Skip items we can't access
                    items.append({
                        'name': path_item.name,
                        'type': 'unknown',
                        'size': None
                    })

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied listing directory: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to list directory: {str(e)}") from e

        result_lines = [f"Directory: {display_path}", f"Items: {len(items)}", ""]

        for item in sorted(items, key=lambda x: (x['type'], x['name'])):
            if item['type'] == 'directory':
                result_lines.append(f"📁 {item['name']}/")

            else:
                size_str = f" ({item['size']:,} bytes)" if item['size'] is not None else ""
                result_lines.append(f"📄 {item['name']}{size_str}")

        result_str = "\n".join(result_lines)

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=result_str,
            context=f"`content` is:\n```text\n{result_str}\n```"
        )

    async def _create_directory(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Create directory (with parents if needed)."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)
        create_parents = self._get_optional_bool_value("create_parents", arguments, True)

        if path.exists():
            if path.is_dir():
                raise AIToolExecutionError(f"Directory already exists: {arguments['path']}")

            raise AIToolExecutionError(f"Path exists but is not a directory: {arguments['path']}")

        # Build authorization context
        context = f"Create a new directory '{display_path}'."

        if create_parents and not path.parent.exists():
            context += " This will also create any missing parent directories."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, None, False)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to create directory: {arguments['path']}")

        # Create directory
        try:
            path.mkdir(parents=create_parents, exist_ok=False)

        except FileExistsError as e:
            raise AIToolExecutionError(f"Directory already exists: {str(path)}") from e

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied creating directory: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to create directory: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"Directory created successfully: {display_path}"
        )

    async def _remove_directory(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Remove empty directory."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        if not path.exists():
            raise AIToolExecutionError(f"Directory does not exist: {arguments['path']}")

        if not path.is_dir():
            raise AIToolExecutionError(f"Path is not a directory: {arguments['path']}")

        # Check if directory is empty
        try:
            items = list(path.iterdir())
            if items:
                raise AIToolExecutionError(
                    f"Directory is not empty (contains {len(items)} items): {arguments['path']}"
                )

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied checking directory contents: {str(e)}") from e

        # Build authorization context
        context = f"Remove the empty directory '{display_path}'. This directory will be permanently deleted."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, None, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to remove directory: {arguments['path']}")

        # Remove directory
        try:
            path.rmdir()

        except OSError as e:
            raise AIToolExecutionError(f"Failed to remove directory: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"Directory removed successfully: {display_path}"
        )

    async def _delete_file(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Delete file."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        if not path.exists():
            raise AIToolExecutionError(f"File does not exist: {arguments['path']}")

        if not path.is_file():
            raise AIToolExecutionError(f"Path is not a file: {arguments['path']}")

        # Build authorization context
        context = f"Delete the file '{display_path}'. This file will be permanently removed and cannot be recovered."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, None, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to delete file: {arguments['path']}")

        # Delete file
        try:
            path.unlink()

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied deleting file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to delete file: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"File deleted successfully: {display_path}"
        )

    async def _copy_file(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Copy file to destination."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        source_path, source_display_path = self._validate_and_resolve_path("path", path_arg)

        if not source_path.exists():
            raise AIToolExecutionError(f"Source file does not exist: {arguments['path']}")

        if not source_path.is_file():
            raise AIToolExecutionError(f"Source path is not a file: {arguments['path']}")

        destination_arg = self._get_required_str_value("destination", arguments)
        destination_path, dest_display_path = self._validate_and_resolve_path("destination", destination_arg)

        # Check source file size
        source_size = source_path.stat().st_size
        if source_size > self._max_file_size_bytes:
            size_mb = source_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(f"Source file too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)")

        # Build authorization context
        if destination_path.exists():
            context = f"Copy '{source_display_path}' to '{dest_display_path}'. " \
                "This will overwrite the existing destination file and its contents will be lost."
            destructive = True

        else:
            context = f"Copy '{source_display_path}' to '{dest_display_path}'. This will create a new file at the destination."
            destructive = False

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, None, destructive)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to copy file: {arguments['path']} -> {destination_arg}")

        # Copy file
        try:
            # Create parent directories if needed
            destination_path.parent.mkdir(parents=True, exist_ok=True)

            # Copy file
            shutil.copy2(source_path, destination_path)

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied copying file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to copy file: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"File copied successfully: {source_display_path} -> {dest_display_path}"
        )

    async def _move(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Move/rename file or directory."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        source_path, source_display_path = self._validate_and_resolve_path("path", path_arg)

        if not source_path.exists():
            raise AIToolExecutionError(f"Source path does not exist: {arguments['path']}")

        destination_arg = self._get_required_str_value("destination", arguments)
        destination_path, dest_display_path = self._validate_and_resolve_path("destination", destination_arg)

        # Build authorization context
        if source_path.is_file():
            item_type = "file"

        elif source_path.is_dir():
            item_type = "directory"

        else:
            item_type = "item"

        if destination_path.exists():
            context = f"Move the {item_type} '{source_display_path}' to '{dest_display_path}'. " \
                "This will overwrite the existing destination and its contents will be lost."

        else:
            context = f"Move the {item_type} '{source_display_path}' to '{dest_display_path}'."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, None, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to move: {arguments['path']} -> {destination_arg}")

        # Move file or directory
        try:
            # Create parent directories if needed
            destination_path.parent.mkdir(parents=True, exist_ok=True)

            # Move/rename
            source_path.rename(destination_path)

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied moving: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to move: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"Moved successfully: {source_display_path} -> {dest_display_path}"
        )

    async def _get_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get detailed information about file or directory."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        # Get format parameter (defaults to None, which will become 'iso' in _format_time)
        format_type = self._get_optional_str_value("format", arguments, None)

        if not path.exists():
            raise AIToolExecutionError(f"Path does not exist: {arguments['path']}")

        # Get file or directory information
        try:
            stat_info = path.stat()
            modified_dt = datetime.fromtimestamp(stat_info.st_mtime, tz=timezone.utc)
            modified_time = self._format_time(modified_dt, format_type)

            if path.is_file():
                result = f"""File: {display_path}
Type: File
Size: {stat_info.st_size:,} bytes
Modified: {modified_time}
Permissions: {oct(stat_info.st_mode)[-3:]}
Extension: {path.suffix or 'None'}"""

            elif path.is_dir():
                try:
                    items = list(path.iterdir())
                    file_count = sum(1 for item in items if item.is_file())
                    dir_count = sum(1 for item in items if item.is_dir())
                    items_info = f"{len(items)} total ({file_count} files, {dir_count} directories)"

                except PermissionError:
                    items_info = "Permission denied"

                result = f"""Directory: {display_path}
Type: Directory
Items: {items_info}
Modified: {modified_time}
Permissions: {oct(stat_info.st_mode)[-3:]}"""

            else:
                result = f"""Path: {display_path}
Type: Other (neither file nor directory)
Modified: {modified_time}
Permissions: {oct(stat_info.st_mode)[-3:]}"""

            return AIToolResult(
                id=tool_call.id,
                name="filesystem",
                content=result,
                context=f"`content` is:\n```text\n{result}\n```"
            )

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied getting info: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to get info: {str(e)}") from e

    def _apply_diff_to_file_context(self, arguments: Dict[str, Any]) -> str | None:
        """Extract context for append_to_file operation."""
        context = self._get_required_str_value("diff_content", arguments)
        return f"`diff_content` is:\n```diff\n{context}\n```"

    async def _apply_diff_to_file(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Apply a unified diff to a file."""
        arguments = tool_call.arguments
        path_arg = self._get_required_str_value("path", arguments)
        path, display_path = self._validate_and_resolve_path("path", path_arg)

        # Validate file exists and is readable
        if not path.exists():
            raise AIToolExecutionError(f"File does not exist: {arguments['path']}")

        if not path.is_file():
            raise AIToolExecutionError(f"Path is not a file: {arguments['path']}")

        diff_content = self._get_required_str_value("diff_content", arguments)
        dry_run = self._get_optional_bool_value("dry_run", arguments, False)

        # Read current file content
        encoding = self._get_optional_str_value("encoding", arguments, "utf-8")

        try:
            with open(path, 'r', encoding=encoding) as f:
                file_content = f.read()

        except UnicodeDecodeError as e:
            raise AIToolExecutionError(
                f"Failed to decode file with encoding '{encoding}': {str(e)}"
            ) from e

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied reading file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to read file: {str(e)}") from e

        # Apply diff
        applier = FilesystemDiffApplier(confidence_threshold=0.75, search_window=50)

        try:
            result, modified_content = applier.apply_diff_to_file(
                diff_content,
                file_content,
                dry_run=dry_run
            )

        except DiffParseError as e:
            raise AIToolExecutionError(f"Failed to parse diff: {str(e)}") from e

        except DiffMatchError as e:
            error_details = getattr(e, 'error_details', None)
            error_msg = f"Failed to match diff hunks: {str(e)}"
            if error_details:
                error_msg += f"\n\nError details:\n{error_details}"

            raise AIToolExecutionError(error_msg) from e

        except DiffValidationError as e:
            error_details = getattr(e, 'error_details', None)
            error_msg = f"Diff validation failed: {str(e)}"
            if error_details:
                error_msg += f"\n\nError details:\n{error_details}"

            raise AIToolExecutionError(error_msg) from e

        except DiffApplicationError as e:
            raise AIToolExecutionError(f"Failed to apply diff: {str(e)}") from e

        if not result.success:
            error_msg = f"Failed to apply diff: {result.message}"
            if result.error_details:
                error_msg += f"\n\nError details:\n{result.error_details}"

            raise AIToolExecutionError(error_msg)

        # If dry run, return validation result
        if dry_run:
            return AIToolResult(
                id=tool_call.id,
                name="filesystem",
                content=f"Diff validation successful for '{display_path}': {result.hunks_applied} hunk(s) can be applied"
            )

        # Request authorization to save modified file
        context = f"This will apply a diff to file '{display_path}' ({result.hunks_applied} hunk(s)). " \
            "This will overwrite the existing file."

        authorized = await request_authorization("filesystem", arguments, context, None, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to apply diff to file: {arguments['path']}")

        # Write modified content to file
        try:
            # Write to temporary file first, then rename for atomicity
            with tempfile.NamedTemporaryFile(
                mode='w',
                encoding=encoding,
                dir=path.parent,
                delete=False,
                suffix='.tmp'
            ) as tmp_file:
                tmp_file.write(cast(str,modified_content))
                tmp_path = Path(tmp_file.name)

            # Atomic rename
            tmp_path.replace(path)

            # Set proper permissions based on umask
            umask = os.umask(0)
            os.umask(umask)
            desired_mode = 0o666 & ~umask
            path.chmod(desired_mode)

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied writing file: {str(e)}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to write file: {str(e)}") from e

        return AIToolResult(
            id=tool_call.id,
            name="filesystem",
            content=f"Diff applied successfully to '{display_path}': {result.hunks_applied} hunk(s) applied"
        )
