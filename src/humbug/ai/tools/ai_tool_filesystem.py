import logging
import shutil
import tempfile
from datetime import datetime
import os
from pathlib import Path
from typing import Dict, Any, List

from humbug.ai.ai_tool_manager import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback
)
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_error import MindspaceNotFoundError


class AIToolFileSystem(AITool):
    """
    Comprehensive filesystem tool.

    All operations require user authorization and are restricted to the current mindspace.
    Provides secure file and directory operations with proper error handling and logging.
    """

    def __init__(self, max_file_size_mb: int = 10):
        """
        Initialize the filesystem tool.

        Args:
            max_file_size_mb: Maximum file size in MB for read/write operations
        """
        self._mindspace_manager = MindspaceManager()
        self._max_file_size_bytes = max_file_size_mb * 1024 * 1024
        self._logger = logging.getLogger("AIToolFileSystem")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return AIToolDefinition(
            name="filesystem",
            description=(
                f"Perform filesystem operations within the current mindspace. "
                f"All operations require user authorization and are restricted to mindspace boundaries. "
                f"Maximum file size: {self._max_file_size_bytes // (1024 * 1024)}MB."
            ),
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Filesystem operation to perform",
                    required=True,
                    enum=[
                        "read_file", "write_file", "append_to_file",
                        "list_directory", "create_directory", "remove_directory",
                        "delete_file", "copy_file", "move", "get_info"
                    ]
                ),
                AIToolParameter(
                    name="path",
                    type="string",
                    description="Path to file or directory (relative to mindspace root or absolute)",
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
                    name="create_parents",
                    type="boolean",
                    description="Create parent directories if they don't exist (for write operations)",
                    required=False
                )
            ]
        )

    def _validate_mindspace_access(self) -> None:
        """
        Validate that a mindspace is currently open.

        Raises:
            AIToolExecutionError: If no mindspace is open
        """
        if not self._mindspace_manager.has_mindspace():
            raise AIToolExecutionError(
                "No mindspace is currently open. Filesystem operations require an active mindspace.",
                "filesystem",
                {}
            )

    def _get_value_from_key(self, key: str, arguments: Dict[str, Any]) -> Any:
        """
        Extract value from arguments dictionary.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters

        Returns:
            Value for the given key

        Raises:
            AIToolExecutionError: If key is missing or value is not valid
        """
        if key not in arguments:
            raise AIToolExecutionError(
                f"No '{key}' argument provided",
                "filesystem",
                arguments
            )

        return arguments[key]

    def _get_str_value_from_key(self, key: str, arguments: Dict[str, Any]) -> str:
        """
        Extract string value from arguments dictionary.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters

        Returns:
            String value for the given key

        Raises:
            AIToolExecutionError: If key is missing or value is not a string
        """
        value = self._get_value_from_key(key, arguments)
        if not isinstance(value, str):
            raise AIToolExecutionError(
                f"'{key}' must be a string",
                "filesystem",
                arguments
            )

        return value

    def _validate_and_resolve_path(self, path_str: str, operation: str) -> Path:
        """
        Validate path is within mindspace and resolve to absolute path.

        Args:
            path_str: String path to validate and resolve
            operation: Operation being performed (for error context)

        Returns:
            Resolved Path object within mindspace

        Raises:
            AIToolExecutionError: If path is invalid or outside mindspace
        """
        if not path_str:
            raise AIToolExecutionError(
                "Path parameter is required",
                "filesystem",
                {"operation": operation, "path": path_str}
            )

        try:
            # Check if our path starts with a separator.  If it does we'll assume it's for the root of the mindspace.
            if path_str.startswith(os.sep):
                path_str = path_str[1:]

            # Convert to absolute path via mindspace manager
            abs_path = self._mindspace_manager.get_absolute_path(path_str)
            resolved_path = Path(abs_path).resolve()

            # Verify the resolved path is still within mindspace
            relative_path = self._mindspace_manager.get_mindspace_relative_path(str(resolved_path))
            if relative_path is None:
                raise AIToolExecutionError(
                    f"Path is outside mindspace boundaries: {path_str}",
                    "filesystem",
                    {"operation": operation, "path": path_str}
                )

            return resolved_path

        except MindspaceNotFoundError as e:
            raise AIToolExecutionError(
                f"Mindspace error: {str(e)}",
                "filesystem",
                {"operation": operation, "path": path_str}
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Invalid path '{path_str}': {str(e)}",
                "filesystem",
                {"operation": operation, "path": path_str}
            ) from e

    async def execute(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """
        Execute the filesystem operation with proper validation and authorization.

        Args:
            arguments: Dictionary containing operation parameters
            request_authorization: Function to call for user authorization

        Returns:
            String result of the operation

        Raises:
            AIToolExecutionError: If operation fails
            AIToolAuthorizationDenied: If user denies authorization
        """
        # Validate mindspace is open
        self._validate_mindspace_access()

        # Extract and validate operation
        operation = self._get_str_value_from_key("operation", arguments)

        # Route to specific operation handler
        handlers = {
            "read_file": self._read_file,
            "write_file": self._write_file,
            "append_to_file": self._append_to_file,
            "list_directory": self._list_directory,
            "create_directory": self._create_directory,
            "remove_directory": self._remove_directory,
            "delete_file": self._delete_file,
            "copy_file": self._copy_file,
            "move": self._move,
            "get_info": self._get_info,
        }

        if operation not in handlers:
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}",
                "filesystem",
                arguments
            )

        self._logger.debug("Filesystem operation requested: %s", operation)

        try:
            result = await handlers[operation](arguments, request_authorization)
            self._logger.info("Filesystem operation completed successfully: %s", operation)
            return result

        except (AIToolExecutionError, AIToolAuthorizationDenied):
            # Re-raise our own errors
            raise

        except Exception as e:
            self._logger.error("Unexpected error in filesystem operation '%s': %s", operation, str(e), exc_info=True)
            raise AIToolExecutionError(
                f"Filesystem operation failed: {str(e)}",
                "filesystem",
                arguments
            ) from e

    async def _read_file(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Read file contents."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "read_file")

        # Validate file exists and is readable
        if not path.exists():
            raise AIToolExecutionError(
                f"File does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        if not path.is_file():
            raise AIToolExecutionError(
                f"Path is not a file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Check file size
        file_size = path.stat().st_size
        if file_size > self._max_file_size_bytes:
            size_mb = file_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(
                f"File too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)",
                "filesystem",
                arguments
            )

        encoding = arguments.get("encoding", "utf-8")

        # Read file content
        try:
            with open(path, 'r', encoding=encoding) as f:
                content = f.read()

            actual_size = path.stat().st_size

        except UnicodeDecodeError as e:
            raise AIToolExecutionError(
                f"Failed to decode file with encoding '{encoding}': {str(e)}. Try a different encoding.",
                "filesystem",
                {"path": str(path), "encoding": encoding}
            ) from e

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied reading file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to read file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"File: {relative_path}\nSize: {actual_size:,} bytes\nEncoding: {encoding}\n\n{content}"

    async def _write_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Write content to file (create or overwrite)."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "write_file")

        if "content" not in arguments:
            raise AIToolExecutionError(
                "No 'content' argument provided",
                "filesystem",
                arguments
            )

        content = arguments["content"]
        if not isinstance(content, str):
            raise AIToolExecutionError(
                "'content' must be a string",
                "filesystem",
                arguments
            )

        encoding = arguments.get("encoding", "utf-8")
        create_parents = arguments.get("create_parents", False)

        # Check content size
        content_size = len(content.encode(encoding))
        if content_size > self._max_file_size_bytes:
            size_mb = content_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(
                f"'content' too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)",
                "filesystem",
                arguments
            )

        # Build authorization context
        relative_path = self._mindspace_manager.get_relative_path(str(path))

        if path.exists():
            context = f"Write content to '{relative_path}'. " \
                "This will overwrite the existing file and the previous contents will be lost."
            destructive = True

        else:
            context = f"Create a new file '{relative_path}' with the provided content."
            if create_parents and not path.parent.exists():
                parent_relative = self._mindspace_manager.get_relative_path(str(path.parent))
                context += f" This will also create the parent directory '{parent_relative}'."

            destructive = False

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, destructive)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to write file: {arguments['path']}",
                "filesystem",
                arguments
            )

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

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied writing file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to write file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"File written successfully: {relative_path} ({content_size:,} bytes)"

    async def _append_to_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Append content to existing file."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "append_to_file")

        # File must exist for append
        if not path.exists():
            raise AIToolExecutionError(
                f"File does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        if not path.is_file():
            raise AIToolExecutionError(
                f"Path is not a file: {arguments['path']}",
                "filesystem",
                arguments
            )

        content = self._get_str_value_from_key("content", arguments)
        encoding = arguments.get("encoding", "utf-8")

        # Check total size after append
        current_size = path.stat().st_size
        content_size = len(content.encode(encoding))
        total_size = current_size + content_size

        if total_size > self._max_file_size_bytes:
            total_mb = total_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(
                f"File would be too large after append: {total_mb:.1f}MB (max: {max_mb:.1f}MB)",
                "filesystem",
                arguments
            )

        # Build authorization context
        relative_path = self._mindspace_manager.get_relative_path(str(path))
        context = f"Append content to the end of '{relative_path}'. This will modify the existing file."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to append to file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Append to file
        try:
            with open(path, 'a', encoding=encoding) as f:
                f.write(content)

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied appending to file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to append to file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"Content appended successfully: {relative_path} (+{content_size:,} bytes)"

    async def _list_directory(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """List directory contents."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "list_directory")

        if not path.exists():
            raise AIToolExecutionError(
                f"Directory does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        if not path.is_dir():
            raise AIToolExecutionError(
                f"Path is not a directory: {arguments['path']}",
                "filesystem",
                arguments
            )

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
            raise AIToolExecutionError(
                f"Permission denied listing directory: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to list directory: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        result_lines = [f"Directory: {relative_path}", f"Items: {len(items)}", ""]

        for item in sorted(items, key=lambda x: (x['type'], x['name'])):
            if item['type'] == 'directory':
                result_lines.append(f"📁 {item['name']}/")

            else:
                size_str = f" ({item['size']:,} bytes)" if item['size'] is not None else ""
                result_lines.append(f"📄 {item['name']}{size_str}")

        return "\n".join(result_lines)

    async def _create_directory(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Create directory (with parents if needed)."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "create_directory")
        create_parents = arguments.get("create_parents", True)

        if path.exists():
            if path.is_dir():
                raise AIToolExecutionError(
                    f"Directory already exists: {arguments['path']}",
                    "filesystem",
                    arguments
                )

            raise AIToolExecutionError(
                f"Path exists but is not a directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Build authorization context
        relative_path = self._mindspace_manager.get_relative_path(str(path))
        context = f"Create a new directory '{relative_path}'."

        if create_parents and not path.parent.exists():
            context += " This will also create any missing parent directories."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, False)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to create directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Create directory
        try:
            path.mkdir(parents=create_parents, exist_ok=False)

        except FileExistsError as e:
            raise AIToolExecutionError(
                f"Directory already exists: {str(path)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied creating directory: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to create directory: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"Directory created successfully: {relative_path}"

    async def _remove_directory(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Remove empty directory."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "remove_directory")

        if not path.exists():
            raise AIToolExecutionError(
                f"Directory does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        if not path.is_dir():
            raise AIToolExecutionError(
                f"Path is not a directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Check if directory is empty
        try:
            items = list(path.iterdir())
            if items:
                raise AIToolExecutionError(
                    f"Directory is not empty (contains {len(items)} items): {arguments['path']}",
                    "filesystem",
                    arguments
                )

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied checking directory contents: {str(e)}",
                "filesystem",
                arguments
            ) from e

        # Build authorization context
        relative_path = self._mindspace_manager.get_relative_path(str(path))
        context = f"Remove the empty directory '{relative_path}'. This directory will be permanently deleted."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to remove directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Remove directory
        try:
            path.rmdir()

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to remove directory: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"Directory removed successfully: {relative_path}"

    async def _delete_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Delete file."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "delete_file")

        if not path.exists():
            raise AIToolExecutionError(
                f"File does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        if not path.is_file():
            raise AIToolExecutionError(
                f"Path is not a file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Build authorization context
        relative_path = self._mindspace_manager.get_relative_path(str(path))
        context = f"Delete the file '{relative_path}'. This file will be permanently removed and cannot be recovered."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to delete file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Delete file
        try:
            path.unlink()

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied deleting file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to delete file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"File deleted successfully: {relative_path}"

    async def _copy_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Copy file to destination."""
        path_arg = self._get_str_value_from_key("path", arguments)
        source_path = self._validate_and_resolve_path(path_arg, "copy_file")

        if not source_path.exists():
            raise AIToolExecutionError(
                f"Source file does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        if not source_path.is_file():
            raise AIToolExecutionError(
                f"Source path is not a file: {arguments['path']}",
                "filesystem",
                arguments
            )

        destination_arg = self._get_str_value_from_key("destination", arguments)
        destination_path = self._validate_and_resolve_path(destination_arg, "copy_file")

        # Check source file size
        source_size = source_path.stat().st_size
        if source_size > self._max_file_size_bytes:
            size_mb = source_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(
                f"Source file too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)",
                "filesystem",
                arguments
            )

        # Build authorization context
        source_relative = self._mindspace_manager.get_relative_path(str(source_path))
        dest_relative = self._mindspace_manager.get_relative_path(str(destination_path))

        if destination_path.exists():
            context = f"Copy '{source_relative}' to '{dest_relative}'. " \
                "This will overwrite the existing destination file and its contents will be lost."
            destructive = True

        else:
            context = f"Copy '{source_relative}' to '{dest_relative}'. This will create a new file at the destination."
            destructive = False

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, destructive)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to copy file: {arguments['path']} -> {destination_arg}",
                "filesystem",
                arguments
            )

        # Copy file
        try:
            # Create parent directories if needed
            destination_path.parent.mkdir(parents=True, exist_ok=True)

            # Copy file
            shutil.copy2(source_path, destination_path)

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied copying file: {str(e)}",
                "filesystem",
                {"source": str(source_path), "destination": str(destination_path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to copy file: {str(e)}",
                "filesystem",
                {"source": str(source_path), "destination": str(destination_path)}
            ) from e

        source_relative = self._mindspace_manager.get_relative_path(str(source_path))
        dest_relative = self._mindspace_manager.get_relative_path(str(destination_path))
        return f"File copied successfully: {source_relative} -> {dest_relative}"

    async def _move(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Move/rename file or directory."""
        path_arg = self._get_str_value_from_key("path", arguments)
        source_path = self._validate_and_resolve_path(path_arg, "move")

        if not source_path.exists():
            raise AIToolExecutionError(
                f"Source path does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        destination_arg = self._get_str_value_from_key("destination", arguments)
        destination_path = self._validate_and_resolve_path(destination_arg, "move")

        # Build authorization context
        source_relative = self._mindspace_manager.get_relative_path(str(source_path))
        dest_relative = self._mindspace_manager.get_relative_path(str(destination_path))

        if source_path.is_file():
            item_type = "file"

        elif source_path.is_dir():
            item_type = "directory"

        else:
            item_type = "item"

        if destination_path.exists():
            context = f"Move the {item_type} '{source_relative}' to '{dest_relative}'. " \
                "This will overwrite the existing destination and its contents will be lost."

        else:
            context = f"Move the {item_type} '{source_relative}' to '{dest_relative}'."

        # Request authorization
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to move: {arguments['path']} -> {destination_arg}",
                "filesystem",
                arguments
            )

        # Move file or directory
        try:
            # Create parent directories if needed
            destination_path.parent.mkdir(parents=True, exist_ok=True)

            # Move/rename
            source_path.rename(destination_path)

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied moving: {str(e)}",
                "filesystem",
                {"source": str(source_path), "destination": str(destination_path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to move: {str(e)}",
                "filesystem",
                {"source": str(source_path), "destination": str(destination_path)}
            ) from e

        source_relative = self._mindspace_manager.get_relative_path(str(source_path))
        dest_relative = self._mindspace_manager.get_relative_path(str(destination_path))
        return f"Moved successfully: {source_relative} -> {dest_relative}"

    async def _get_info(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Get detailed information about file or directory."""
        path_arg = self._get_str_value_from_key("path", arguments)
        path = self._validate_and_resolve_path(path_arg, "get_info")

        if not path.exists():
            raise AIToolExecutionError(
                f"Path does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Get file or directory information
        try:
            stat_info = path.stat()
            relative_path = self._mindspace_manager.get_relative_path(str(path))
            modified_time = datetime.fromtimestamp(stat_info.st_mtime).isoformat()

            if path.is_file():
                return f"""File: {relative_path}
Type: File
Size: {stat_info.st_size:,} bytes
Modified: {modified_time}
Permissions: {oct(stat_info.st_mode)[-3:]}
Extension: {path.suffix or 'None'}"""

            if path.is_dir():
                try:
                    items = list(path.iterdir())
                    file_count = sum(1 for item in items if item.is_file())
                    dir_count = sum(1 for item in items if item.is_dir())
                    items_info = f"{len(items)} total ({file_count} files, {dir_count} directories)"

                except PermissionError:
                    items_info = "Permission denied"

                return f"""Directory: {relative_path}
Type: Directory
Items: {items_info}
Modified: {modified_time}
Permissions: {oct(stat_info.st_mode)[-3:]}"""

            return f"""Path: {relative_path}
Type: Other (neither file nor directory)
Modified: {modified_time}
Permissions: {oct(stat_info.st_mode)[-3:]}"""

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied getting info: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to get info: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e
