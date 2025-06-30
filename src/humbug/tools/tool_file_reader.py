import os
from pathlib import Path
from typing import Dict, Any

from humbug.ai.ai_tool_manager import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError, 
    AIToolAuthorizationDenied, AIToolAuthorizationCallback
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

    def get_timeout(self) -> float | None:
        """Get the preferred timeout for this tool."""
        return 60.0  # 60 seconds should be enough for file reading + user authorization

    async def execute(
        self, 
        arguments: Dict[str, Any], 
        request_authorization: AIToolAuthorizationCallback | None = None
    ) -> str:
        """Execute the file reading tool."""
        file_path_str = arguments.get("file_path", "")
        encoding = arguments.get("encoding", "utf-8")

        # Validate inputs
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
            # Resolve and validate the file path
            file_path = Path(file_path_str).resolve()
            
            # Security checks
            if not file_path.exists():
                raise AIToolExecutionError(
                    f"File does not exist: {file_path}",
                    "read_file",
                    arguments
                )

            if not file_path.is_file():
                raise AIToolExecutionError(
                    f"Path is not a file: {file_path}",
                    "read_file",
                    arguments
                )

            # Check file size
            file_size = file_path.stat().st_size
            if file_size > self.max_file_size_bytes:
                size_mb = file_size / (1024 * 1024)
                max_mb = self.max_file_size_bytes / (1024 * 1024)
                raise AIToolExecutionError(
                    f"File too large: {size_mb:.1f}MB (max: {max_mb:.1f}MB)",
                    "read_file",
                    arguments
                )

            # Request authorization from user
            if request_authorization:
                file_info = (
                    f"Size: {file_size:,} bytes\n"
                    f"Extension: {file_path.suffix}\n"
                    f"Encoding: {encoding}"
                )
                
                reason = f"Read file '{file_path}'. {file_info}"
                
                authorized = await request_authorization(
                    tool_name="read_file",
                    arguments=arguments,
                    reason=reason
                )
                
                if not authorized:
                    raise AIToolAuthorizationDenied(
                        f"User denied permission to read file: {file_path}",
                        "read_file",
                        arguments
                    )

            # Read the file
            try:
                with open(file_path, 'r', encoding=encoding) as f:
                    content = f.read()

                # Return file content with metadata
                return f"File: {file_path}\nSize: {file_size:,} bytes\nEncoding: {encoding}\n\n{content}"

            except UnicodeDecodeError as e:
                raise AIToolExecutionError(
                    f"Failed to decode file with encoding '{encoding}': {str(e)}. "
                    f"Try a different encoding.",
                    "read_file",
                    arguments
                ) from e

        except AIToolExecutionError:
            # Re-raise our own errors
            raise

        except AIToolAuthorizationDenied:
            # Re-raise authorization errors
            raise

        except Exception as e:
            raise AIToolExecutionError(
                f"Unexpected error reading file: {str(e)}",
                "read_file",
                arguments
            ) from e