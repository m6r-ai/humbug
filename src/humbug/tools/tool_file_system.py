import asyncio
import logging
import shutil
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List

from humbug.ai.ai_tool_manager import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback, AIToolTimeoutError
)
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_error import MindspaceNotFoundError




class ToolFileSystem(AITool):
    """
    Comprehensive filesystem tool.

    All operations require user authorization and are restricted to the current mindspace.
    Provides secure file and directory operations with proper error handling and logging.
    """

    def __init__(self, max_file_size_mb: int = 10):
        """
        Initialize the filesystem tool.

        Args:
            mindspace_manager: Manager for the current mindspace
            max_file_size_mb: Maximum file size in MB for read/write operations
        """
        self._mindspace_manager = MindspaceManager()
        self._max_file_size_bytes = max_file_size_mb * 1024 * 1024
        self._logger = logging.getLogger("ToolFileSystem")

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

        if not isinstance(path_str, str):
            raise AIToolExecutionError(
                "Path must be a string",
                "filesystem",
                {"operation": operation, "path": path_str}
            )

        try:
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

    def _build_authorization_context(self, operation: str, path: Path, **kwargs: Any) -> str:
        """
        Build rich context information for user authorization.

        Args:
            operation: Operation being performed
            path: Path being operated on
            **kwargs: Additional context information

        Returns:
            Formatted authorization context string
        """
        relative_path = self._mindspace_manager.get_relative_path(str(path))
        context_lines = [f"Operation: {operation}", f"Path: {relative_path}"]

        # Add operation-specific context
        if operation in ("write_file", "append_to_file") and "content" in kwargs:
            content = kwargs["content"]
            content_size = len(content.encode('utf-8'))
            context_lines.append(f"Content size: {content_size:,} bytes")

            # Show content preview for small files
            if content_size <= 500:
                preview = content[:200] + ("..." if len(content) > 200 else "")
                context_lines.append(f"Content preview: {repr(preview)}")

        elif operation in ("copy_file", "move") and "destination" in kwargs:
            dest_relative = self._mindspace_manager.get_relative_path(kwargs["destination"])
            context_lines.append(f"Destination: {dest_relative}")

        # Add file/directory information if path exists
        if path.exists():
            if path.is_file():
                size = path.stat().st_size
                context_lines.append(f"Current size: {size:,} bytes")

            elif path.is_dir():
                try:
                    items = list(path.iterdir())
                    context_lines.append(f"Directory items: {len(items)}")

                except PermissionError:
                    context_lines.append("Directory items: Permission denied")

        return "\n".join(context_lines)

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
            AIToolTimeoutError: If operation times out
        """
        # Validate mindspace is open
        self._validate_mindspace_access()

        # Extract and validate operation
        operation = arguments.get("operation", "")
        if not operation:
            raise AIToolExecutionError(
                "Operation parameter is required",
                "filesystem",
                arguments
            )

        if operation not in [
            "read_file", "write_file", "append_to_file",
            "list_directory", "create_directory", "remove_directory", 
            "delete_file", "copy_file", "move", "get_info"
        ]:
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}",
                "filesystem",
                arguments
            )

        self._logger.debug("Filesystem operation requested: %s", operation)

        try:
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

            result = await handlers[operation](arguments, request_authorization)
            self._logger.info("Filesystem operation completed successfully: %s", operation)
            return result

        except (AIToolExecutionError, AIToolAuthorizationDenied, AIToolTimeoutError):
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
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Read file contents."""
        path = self._validate_and_resolve_path(arguments["path"], "read_file")
        encoding = arguments.get("encoding", "utf-8")

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

        # Request authorization
        context = self._build_authorization_context("read_file", path, encoding=encoding)
        authorized = await request_authorization("filesystem", arguments, context, False)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to read file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Read file with timeout
        try:
            content, actual_size = await asyncio.wait_for(
                asyncio.to_thread(self._read_file_content, path, encoding),
                timeout=30.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "File reading timed out",
                "filesystem",
                arguments,
                30.0
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"File: {relative_path}\nSize: {actual_size:,} bytes\nEncoding: {encoding}\n\n{content}"

    def _read_file_content(self, path: Path, encoding: str) -> tuple[str, int]:
        """Synchronous helper for file reading."""
        try:
            with open(path, 'r', encoding=encoding) as f:
                content = f.read()

            file_size = path.stat().st_size
            return content, file_size

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

    async def _write_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Write content to file (create or overwrite)."""
        path = self._validate_and_resolve_path(arguments["path"], "write_file")
        content = arguments.get("content", "")
        encoding = arguments.get("encoding", "utf-8")
        create_parents = arguments.get("create_parents", False)

        if not isinstance(content, str):
            raise AIToolExecutionError(
                "Content must be a string",
                "filesystem",
                arguments
            )

        # Check content size
        content_size = len(content.encode(encoding))
        if content_size > self._max_file_size_bytes:
            size_mb = content_size / (1024 * 1024)
            max_mb = self._max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(
                f"Content too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)",
                "filesystem",
                arguments
            )

        # Check if we need to create parent directories
        if create_parents and not path.parent.exists():
            # Validate parent path is also within mindspace
            parent_relative = self._mindspace_manager.get_mindspace_relative_path(str(path.parent))
            if parent_relative is None:
                raise AIToolExecutionError(
                    f"Parent directory would be outside mindspace: {path.parent}",
                    "filesystem",
                    arguments
                )

        # Request authorization
        context = self._build_authorization_context(
            "write_file",
            path,
            content=content,
            encoding=encoding,
            create_parents=create_parents
        )
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to write file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Write file with timeout
        try:
            await asyncio.wait_for(
                asyncio.to_thread(self._write_file_content, path, content, encoding, create_parents),
                timeout=30.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "File writing timed out",
                "filesystem",
                arguments,
                30.0
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"File written successfully: {relative_path} ({content_size:,} bytes)"

    def _write_file_content(self, path: Path, content: str, encoding: str, create_parents: bool) -> None:
        """Synchronous helper for file writing."""
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

    async def _append_to_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Append content to existing file."""
        path = self._validate_and_resolve_path(arguments["path"], "append_to_file")
        content = arguments.get("content", "")
        encoding = arguments.get("encoding", "utf-8")

        if not isinstance(content, str):
            raise AIToolExecutionError(
                "Content must be a string",
                "filesystem",
                arguments
            )

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

        # Request authorization
        context = self._build_authorization_context(
            "append_to_file", path, 
            content=content, encoding=encoding
        )
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to append to file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Append to file with timeout
        try:
            await asyncio.wait_for(
                asyncio.to_thread(self._append_file_content, path, content, encoding),
                timeout=30.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "File append timed out",
                "filesystem",
                arguments,
                30.0
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"Content appended successfully: {relative_path} (+{content_size:,} bytes)"

    def _append_file_content(self, path: Path, content: str, encoding: str) -> None:
        """Synchronous helper for file appending."""
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

    async def _list_directory(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """List directory contents."""
        path = self._validate_and_resolve_path(arguments["path"], "list_directory")

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

        # Request authorization
        context = self._build_authorization_context("list_directory", path)
        authorized = await request_authorization("filesystem", arguments, context, False)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to list directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # List directory with timeout
        try:
            items = await asyncio.wait_for(
                asyncio.to_thread(self._list_directory_contents, path),
                timeout=15.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "Directory listing timed out",
                "filesystem",
                arguments,
                15.0
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

    def _list_directory_contents(self, path: Path) -> List[Dict[str, Any]]:
        """Synchronous helper for directory listing."""
        try:
            items = []
            for item in path.iterdir():
                try:
                    if item.is_file():
                        size = item.stat().st_size
                        items.append({
                            'name': item.name,
                            'type': 'file',
                            'size': size
                        })

                    elif item.is_dir():
                        items.append({
                            'name': item.name,
                            'type': 'directory',
                            'size': None
                        })

                    else:
                        items.append({
                            'name': item.name,
                            'type': 'other',
                            'size': None
                        })

                except (PermissionError, OSError):
                    # Skip items we can't access
                    items.append({
                        'name': item.name,
                        'type': 'unknown',
                        'size': None
                    })

            return items

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

    async def _create_directory(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Create directory (with parents if needed)."""
        path = self._validate_and_resolve_path(arguments["path"], "create_directory")
        create_parents = arguments.get("create_parents", True)

        if path.exists():
            if path.is_dir():
                relative_path = self._mindspace_manager.get_relative_path(str(path))
                return f"Directory already exists: {relative_path}"

            raise AIToolExecutionError(
                f"Path exists but is not a directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Request authorization
        context = self._build_authorization_context(
            "create_directory", path, 
            create_parents=create_parents
        )
        authorized = await request_authorization("filesystem", arguments, context, False)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to create directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Create directory with timeout
        try:
            await asyncio.wait_for(
                asyncio.to_thread(self._create_directory_sync, path, create_parents),
                timeout=10.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "Directory creation timed out",
                "filesystem",
                arguments,
                10.0
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"Directory created successfully: {relative_path}"

    def _create_directory_sync(self, path: Path, create_parents: bool) -> None:
        """Synchronous helper for directory creation."""
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

    async def _remove_directory(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Remove empty directory."""
        path = self._validate_and_resolve_path(arguments["path"], "remove_directory")

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

        # Request authorization
        context = self._build_authorization_context("remove_directory", path)
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to remove directory: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Remove directory with timeout
        try:
            await asyncio.wait_for(
                asyncio.to_thread(self._remove_directory_sync, path),
                timeout=10.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "Directory removal timed out",
                "filesystem",
                arguments,
                10.0
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"Directory removed successfully: {relative_path}"

    def _remove_directory_sync(self, path: Path) -> None:
        """Synchronous helper for directory removal."""
        try:
            path.rmdir()

        except OSError as e:
            if not path.exists():
                # Directory was already removed
                return

            raise AIToolExecutionError(
                f"Failed to remove directory (may not be empty): {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

    async def _delete_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Delete file."""
        path = self._validate_and_resolve_path(arguments["path"], "delete_file")

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

        # Request authorization
        context = self._build_authorization_context("delete_file", path)
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to delete file: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Delete file with timeout
        try:
            await asyncio.wait_for(
                asyncio.to_thread(self._delete_file_sync, path),
                timeout=10.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "File deletion timed out",
                "filesystem",
                arguments,
                10.0
            ) from e

        relative_path = self._mindspace_manager.get_relative_path(str(path))
        return f"File deleted successfully: {relative_path}"

    def _delete_file_sync(self, path: Path) -> None:
        """Synchronous helper for file deletion."""
        try:
            path.unlink()

        except PermissionError as e:
            raise AIToolExecutionError(
                f"Permission denied deleting file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

        except OSError as e:
            if not path.exists():
                # File was already deleted
                return

            raise AIToolExecutionError(
                f"Failed to delete file: {str(e)}",
                "filesystem",
                {"path": str(path)}
            ) from e

    async def _copy_file(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Copy file to destination."""
        source_path = self._validate_and_resolve_path(arguments["path"], "copy_file")
        destination_str = arguments.get("destination", "")

        if not destination_str:
            raise AIToolExecutionError(
                "Destination parameter is required for copy_file operation",
                "filesystem",
                arguments
            )

        destination_path = self._validate_and_resolve_path(destination_str, "copy_file")

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

        # Request authorization
        context = self._build_authorization_context(
            "copy_file", source_path, 
            destination=str(destination_path)
        )
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to copy file: {arguments['path']} -> {destination_str}",
                "filesystem",
                arguments
            )

        # Copy file with timeout
        try:
            await asyncio.wait_for(
                asyncio.to_thread(self._copy_file_sync, source_path, destination_path),
                timeout=30.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "File copy timed out",
                "filesystem",
                arguments,
                30.0
            ) from e

        source_relative = self._mindspace_manager.get_relative_path(str(source_path))
        dest_relative = self._mindspace_manager.get_relative_path(str(destination_path))
        return f"File copied successfully: {source_relative} -> {dest_relative}"

    def _copy_file_sync(self, source_path: Path, destination_path: Path) -> None:
        """Synchronous helper for file copying."""
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

    async def _move(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Move/rename file or directory."""
        source_path = self._validate_and_resolve_path(arguments["path"], "move")
        destination_str = arguments.get("destination", "")

        if not destination_str:
            raise AIToolExecutionError(
                "Destination parameter is required for move operation",
                "filesystem",
                arguments
            )

        destination_path = self._validate_and_resolve_path(destination_str, "move")

        if not source_path.exists():
            raise AIToolExecutionError(
                f"Source path does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Request authorization
        context = self._build_authorization_context(
            "move", source_path, 
            destination=str(destination_path)
        )
        authorized = await request_authorization("filesystem", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to move: {arguments['path']} -> {destination_str}",
                "filesystem",
                arguments
            )

        # Move with timeout
        try:
            await asyncio.wait_for(
                asyncio.to_thread(self._move_sync, source_path, destination_path),
                timeout=30.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "Move operation timed out",
                "filesystem",
                arguments,
                30.0
            ) from e

        source_relative = self._mindspace_manager.get_relative_path(str(source_path))
        dest_relative = self._mindspace_manager.get_relative_path(str(destination_path))
        return f"Moved successfully: {source_relative} -> {dest_relative}"

    def _move_sync(self, source_path: Path, destination_path: Path) -> None:
        """Synchronous helper for moving files/directories."""
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

    async def _get_info(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Get detailed information about file or directory."""
        path = self._validate_and_resolve_path(arguments["path"], "get_info")

        if not path.exists():
            raise AIToolExecutionError(
                f"Path does not exist: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Request authorization
        context = self._build_authorization_context("get_info", path)
        authorized = await request_authorization("filesystem", arguments, context, False)
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to get info: {arguments['path']}",
                "filesystem",
                arguments
            )

        # Get info with timeout
        try:
            info = await asyncio.wait_for(
                asyncio.to_thread(self._get_info_sync, path),
                timeout=10.0
            )

        except asyncio.TimeoutError as e:
            raise AIToolTimeoutError(
                "Get info operation timed out",
                "filesystem",
                arguments,
                10.0
            ) from e

        return info

    def _get_info_sync(self, path: Path) -> str:
        """Synchronous helper for getting file/directory information."""
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
