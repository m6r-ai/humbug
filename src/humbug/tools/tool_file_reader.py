import asyncio
from pathlib import Path
from typing import Dict, Any

from humbug.ai.ai_tool_manager import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback, AIToolTimeoutError
)


class ToolFileReader(AITool):
    """File reading tool that requires user authorization for each file access."""

    def __init__(self, max_file_size_mb: int = 10):
        """
        Initialize the file reader tool.

        Args:
            max_file_size_mb: Maximum file size in MB to read
        """
        self.max_file_size_bytes = max_file_size_mb * 1024 * 1024

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return AIToolDefinition(
            name="read_file",
            description=(
                f"Read the contents of a text file from the filesystem. "
                f"Each file read requires user authorization for security. "
                f"Maximum file size: {self.max_file_size_bytes // (1024 * 1024)}MB."
            ),
            parameters=[
                AIToolParameter(
                    name="file_path",
                    type="string",
                    description="Path to the file to read (absolute or relative to current working directory)",
                    required=True
                ),
                AIToolParameter(
                    name="encoding",
                    type="string",
                    description="Text encoding to use when reading the file",
                    required=False,
                    enum=["utf-8", "utf-16", "ascii", "latin-1"]
                )
            ]
        )

    def _resolve_and_validate_path(self, file_path_str: str) -> Path:
        """
        Synchronous helper for path resolution and validation.

        Args:
            file_path_str: String path to resolve and validate

        Returns:
            Resolved Path object

        Raises:
            AIToolExecutionError: If path is invalid or file doesn't meet requirements
        """
        # Resolve and validate the file path
        file_path = Path(file_path_str).resolve()

        # Security checks
        if not file_path.exists():
            raise AIToolExecutionError(
                f"File does not exist: {file_path}",
                "read_file",
                {"file_path": file_path_str}
            )

        if not file_path.is_file():
            raise AIToolExecutionError(
                f"Path is not a file: {file_path}",
                "read_file",
                {"file_path": file_path_str}
            )

        # Check file size
        file_size = file_path.stat().st_size
        if file_size > self.max_file_size_bytes:
            size_mb = file_size / (1024 * 1024)
            max_mb = self.max_file_size_bytes / (1024 * 1024)
            raise AIToolExecutionError(
                f"File too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)",
                "read_file",
                {"file_path": file_path_str}
            )

        return file_path

    def _read_file_content(self, file_path: Path, encoding: str) -> tuple[str, int]:
        """
        Synchronous helper for file reading.

        Args:
            file_path: Path object to read from
            encoding: Text encoding to use

        Returns:
            Tuple of (file_content, file_size)

        Raises:
            AIToolExecutionError: If file reading fails
        """
        try:
            with open(file_path, 'r', encoding=encoding) as f:
                content = f.read()
            file_size = file_path.stat().st_size
            return content, file_size

        except UnicodeDecodeError as e:
            raise AIToolExecutionError(
                f"Failed to decode file with encoding '{encoding}': {str(e)}. "
                f"Try a different encoding.",
                "read_file",
                {"file_path": str(file_path), "encoding": encoding}
            ) from e

    async def execute(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback | None = None
    ) -> str:
        """Execute the file reading tool with proper timeout handling."""
        file_path_str = arguments.get("file_path", "")
        encoding = arguments.get("encoding", "utf-8")

        # Validate inputs (no timeout needed)
        if not file_path_str:
            raise AIToolExecutionError(
                "file_path parameter is required",
                "read_file",
                arguments
            )

        if not isinstance(file_path_str, str):
            raise AIToolExecutionError(
                "file_path must be a string",
                "read_file",
                arguments
            )

        try:
            # File system operations with timeout
            try:
                file_path = await asyncio.wait_for(
                    asyncio.to_thread(self._resolve_and_validate_path, file_path_str),
                    timeout=10.0  # 10 seconds for file system operations
                )
            except asyncio.TimeoutError as e:
                raise AIToolTimeoutError(
                    "File path resolution timed out",
                    "read_file",
                    arguments,
                    10.0
                ) from e

            # Request authorization (NO timeout - user can take as long as needed)
            if request_authorization:
                file_size = file_path.stat().st_size
                file_info = (
                    f"Size: {file_size:,} bytes\n"
                    f"Extension: {file_path.suffix}\n"
                    f"Encoding: {encoding}"
                )

                reason = f"Read file '{file_path}'. {file_info}"

                authorized = await request_authorization("read_file", arguments, reason)

                if not authorized:
                    raise AIToolAuthorizationDenied(
                        f"User denied permission to read file: {file_path}",
                        "read_file",
                        arguments
                    )

            # File reading with timeout
            try:
                content, file_size = await asyncio.wait_for(
                    asyncio.to_thread(self._read_file_content, file_path, encoding),
                    timeout=30.0  # 30 seconds for reading
                )
            except asyncio.TimeoutError as e:
                raise AIToolTimeoutError(
                    "File reading timed out",
                    "read_file",
                    arguments,
                    30.0
                ) from e

            return f"File: {file_path}\nSize: {file_size:,} bytes\nEncoding: {encoding}\n\n{content}"

        except (AIToolExecutionError, AIToolAuthorizationDenied, AIToolTimeoutError):
            # Re-raise our own errors
            raise

        except Exception as e:
            raise AIToolExecutionError(
                f"Unexpected error reading file: {str(e)}",
                "read_file",
                arguments
            ) from e
