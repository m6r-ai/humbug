"""
Standalone tool implementations for the pipeline engine.

Each tool mirrors the interface of its Humbug counterpart exactly, so that
the pipeline engine can be dropped into Humbug later with minimal changes.
Authorization in the standalone runner is handled by prompting the user on
stdin/stdout; in Humbug it will be handled by the existing authorization flow.
"""

import json
import os
import shutil
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Optional
import zoneinfo


class PipelineToolError(Exception):
    """Raised when a tool operation fails."""


class PipelineAuthorizationDenied(Exception):
    """Raised when the user denies authorization for a write operation."""


def _prompt_authorization(description: str) -> bool:
    """
    Prompt the user to authorize a write operation on stdin/stdout.

    Args:
        description: Human-readable description of the operation to authorize

    Returns:
        True if authorized, False if denied
    """
    print(f"\n  Authorization required: {description}")
    print("  Authorize? [y/N] ", end="", flush=True)
    try:
        response = input().strip().lower()
        return response in ("y", "yes")

    except EOFError:
        return False


class FilesystemTool:
    """
    Filesystem tool matching the Humbug FileSystemAITool interface.

    Read operations work on any accessible path.  Write operations require
    user authorization via stdin/stdout prompt.
    """

    MAX_FILE_SIZE_BYTES = 10 * 1024 * 1024

    def execute(self, operation: str, arguments: Dict[str, Any]) -> str:
        """
        Execute a filesystem operation.

        Args:
            operation: Operation name
            arguments: Operation arguments

        Returns:
            String result of the operation

        Raises:
            PipelineToolError: If the operation fails
            PipelineAuthorizationDenied: If the user denies a write operation
        """
        handlers = {
            "read_file": self._read_file,
            "read_file_lines": self._read_file_lines,
            "write_file": self._write_file,
            "append_to_file": self._append_to_file,
            "delete_file": self._delete_file,
            "copy_file": self._copy_file,
            "list_directory": self._list_directory,
            "create_directory": self._create_directory,
            "remove_directory": self._remove_directory,
            "move": self._move,
            "get_info": self._get_info,
        }

        handler = handlers.get(operation)
        if handler is None:
            raise PipelineToolError(f"Unknown filesystem operation: '{operation}'")

        return handler(arguments)

    def _resolve_path(self, key: str, arguments: Dict[str, Any]) -> Path:
        """Resolve and validate a path argument."""
        path_str = arguments.get(key)
        if not isinstance(path_str, str) or not path_str:
            raise PipelineToolError(f"'{key}' must be a non-empty string")

        return Path(path_str).expanduser().resolve()

    def _check_size(self, path: Path) -> None:
        """Raise if file exceeds the size limit."""
        size = path.stat().st_size
        if size > self.MAX_FILE_SIZE_BYTES:
            mb = size / (1024 * 1024)
            max_mb = self.MAX_FILE_SIZE_BYTES / (1024 * 1024)
            raise PipelineToolError(f"File too large: {mb:.1f}MB (max: {max_mb:.1f}MB)")

    def _format_time(self, dt: datetime, format_type: Optional[str]) -> str:
        """Format a datetime as ISO or Unix timestamp string."""
        if format_type == "timestamp":
            return str(int(dt.timestamp()))

        return dt.isoformat()[:26] + "Z" if dt.tzinfo == timezone.utc else dt.isoformat()

    def _read_file(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)

        if not path.exists():
            raise PipelineToolError(f"File does not exist: {path}")

        if not path.is_file():
            raise PipelineToolError(f"Path is not a file: {path}")

        self._check_size(path)
        encoding = arguments.get("encoding", "utf-8") or "utf-8"

        try:
            return path.read_text(encoding=encoding)

        except UnicodeDecodeError as e:
            raise PipelineToolError(
                f"Failed to decode file with encoding '{encoding}': {e}"
            ) from e

        except OSError as e:
            raise PipelineToolError(f"Failed to read file: {e}") from e

    def _read_file_lines(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)

        if not path.exists():
            raise PipelineToolError(f"File does not exist: {path}")

        if not path.is_file():
            raise PipelineToolError(f"Path is not a file: {path}")

        self._check_size(path)
        encoding = arguments.get("encoding", "utf-8") or "utf-8"

        try:
            content = path.read_text(encoding=encoding)

        except UnicodeDecodeError as e:
            raise PipelineToolError(
                f"Failed to decode file with encoding '{encoding}': {e}"
            ) from e

        except OSError as e:
            raise PipelineToolError(f"Failed to read file: {e}") from e

        start_line: Optional[int] = arguments.get("start_line")
        end_line: Optional[int] = arguments.get("end_line")

        lines = content.splitlines() if content else [""]
        total = len(lines)

        actual_start = start_line if start_line is not None else 1
        actual_end = end_line if end_line is not None else total

        if actual_start < 1:
            raise PipelineToolError(f"'start_line' must be >= 1, got {actual_start}")

        if actual_end < actual_start:
            raise PipelineToolError(
                f"'end_line' ({actual_end}) must be >= 'start_line' ({actual_start})"
            )

        if actual_start > total:
            raise PipelineToolError(
                f"'start_line' ({actual_start}) exceeds file length ({total} lines)"
            )

        last = min(actual_end, total)
        line_dict = {str(n): lines[n - 1] for n in range(actual_start, last + 1)}

        range_value = "all lines" if start_line is None and end_line is None else \
            f"{actual_start}-{end_line or 'end'}"

        return json.dumps({"range": range_value, "lines": line_dict}, indent=2)

    def _write_file(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)

        content = arguments.get("content")
        if not isinstance(content, str):
            raise PipelineToolError("'content' must be a string")

        encoding = arguments.get("encoding", "utf-8") or "utf-8"
        create_parents = arguments.get("create_parents", False)

        content_size = len(content.encode(encoding))
        if content_size > self.MAX_FILE_SIZE_BYTES:
            mb = content_size / (1024 * 1024)
            max_mb = self.MAX_FILE_SIZE_BYTES / (1024 * 1024)
            raise PipelineToolError(f"Content too large: {mb:.1f}MB (max: {max_mb:.1f}MB)")

        if path.exists():
            desc = f"Overwrite existing file '{path}' ({content_size:,} bytes)"

        else:
            desc = f"Create new file '{path}' ({content_size:,} bytes)"

        if not _prompt_authorization(desc):
            raise PipelineAuthorizationDenied(f"User denied permission to write '{path}'")

        try:
            if create_parents:
                path.parent.mkdir(parents=True, exist_ok=True)

            with tempfile.NamedTemporaryFile(
                mode="w", encoding=encoding, dir=path.parent,
                delete=False, suffix=".tmp"
            ) as tmp:
                tmp.write(content)
                tmp_path = Path(tmp.name)

            tmp_path.replace(path)
            umask = os.umask(0)
            os.umask(umask)
            path.chmod(0o666 & ~umask)

        except OSError as e:
            raise PipelineToolError(f"Failed to write file: {e}") from e

        return f"File written successfully: {path} ({content_size:,} bytes)"

    def _append_to_file(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)

        if not path.exists():
            raise PipelineToolError(f"File does not exist: {path}")

        if not path.is_file():
            raise PipelineToolError(f"Path is not a file: {path}")

        content = arguments.get("content")
        if not isinstance(content, str):
            raise PipelineToolError("'content' must be a string")

        encoding = arguments.get("encoding", "utf-8") or "utf-8"
        content_size = len(content.encode(encoding))

        if not _prompt_authorization(f"Append {content_size:,} bytes to '{path}'"):
            raise PipelineAuthorizationDenied(f"User denied permission to append to '{path}'")

        try:
            with open(path, "a", encoding=encoding) as f:
                f.write(content)

        except OSError as e:
            raise PipelineToolError(f"Failed to append to file: {e}") from e

        return f"Content appended successfully: {path} (+{content_size:,} bytes)"

    def _delete_file(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)

        if not path.exists():
            raise PipelineToolError(f"File does not exist: {path}")

        if not path.is_file():
            raise PipelineToolError(f"Path is not a file: {path}")

        if not _prompt_authorization(f"Permanently delete file '{path}'"):
            raise PipelineAuthorizationDenied(f"User denied permission to delete '{path}'")

        try:
            path.unlink()

        except OSError as e:
            raise PipelineToolError(f"Failed to delete file: {e}") from e

        return f"File deleted successfully: {path}"

    def _copy_file(self, arguments: Dict[str, Any]) -> str:
        src = self._resolve_path("path", arguments)
        dst_str = arguments.get("destination")
        if not isinstance(dst_str, str) or not dst_str:
            raise PipelineToolError("'destination' must be a non-empty string")

        dst = Path(dst_str).expanduser().resolve()

        if not src.exists():
            raise PipelineToolError(f"Source file does not exist: {src}")

        if not src.is_file():
            raise PipelineToolError(f"Source path is not a file: {src}")

        self._check_size(src)

        if dst.exists():
            desc = f"Copy '{src}' to '{dst}' (overwrites existing file)"
        else:
            desc = f"Copy '{src}' to '{dst}'"

        if not _prompt_authorization(desc):
            raise PipelineAuthorizationDenied(f"User denied permission to copy '{src}' to '{dst}'")

        try:
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, dst)

        except OSError as e:
            raise PipelineToolError(f"Failed to copy file: {e}") from e

        return f"File copied successfully: {src} -> {dst}"

    def _list_directory(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)

        if not path.exists():
            raise PipelineToolError(f"Directory does not exist: {path}")

        if not path.is_dir():
            raise PipelineToolError(f"Path is not a directory: {path}")

        try:
            items = []
            for item in path.iterdir():
                try:
                    if item.is_file():
                        items.append({"name": item.name, "type": "file", "size": item.stat().st_size})

                    elif item.is_dir():
                        items.append({"name": item.name, "type": "directory", "size": None})

                    else:
                        items.append({"name": item.name, "type": "other", "size": None})

                except OSError:
                    items.append({"name": item.name, "type": "unknown", "size": None})

        except OSError as e:
            raise PipelineToolError(f"Failed to list directory: {e}") from e

        result = {
            "directory": str(path),
            "total_items": len(items),
            "items": sorted(items, key=lambda x: (x["type"], x["name"]))
        }

        return json.dumps(result, indent=2)

    def _create_directory(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)
        create_parents = arguments.get("create_parents", True)

        if path.exists():
            if path.is_dir():
                raise PipelineToolError(f"Directory already exists: {path}")

            raise PipelineToolError(f"Path exists but is not a directory: {path}")

        if not _prompt_authorization(f"Create directory '{path}'"):
            raise PipelineAuthorizationDenied(f"User denied permission to create '{path}'")

        try:
            path.mkdir(parents=create_parents, exist_ok=False)

        except OSError as e:
            raise PipelineToolError(f"Failed to create directory: {e}") from e

        return f"Directory created successfully: {path}"

    def _remove_directory(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)

        if not path.exists():
            raise PipelineToolError(f"Directory does not exist: {path}")

        if not path.is_dir():
            raise PipelineToolError(f"Path is not a directory: {path}")

        items = list(path.iterdir())
        if items:
            raise PipelineToolError(
                f"Directory is not empty ({len(items)} items): {path}"
            )

        if not _prompt_authorization(f"Permanently remove empty directory '{path}'"):
            raise PipelineAuthorizationDenied(f"User denied permission to remove '{path}'")

        try:
            path.rmdir()

        except OSError as e:
            raise PipelineToolError(f"Failed to remove directory: {e}") from e

        return f"Directory removed successfully: {path}"

    def _move(self, arguments: Dict[str, Any]) -> str:
        src = self._resolve_path("path", arguments)
        dst_str = arguments.get("destination")
        if not isinstance(dst_str, str) or not dst_str:
            raise PipelineToolError("'destination' must be a non-empty string")

        dst = Path(dst_str).expanduser().resolve()

        if not src.exists():
            raise PipelineToolError(f"Source path does not exist: {src}")

        if not _prompt_authorization(f"Move '{src}' to '{dst}'"):
            raise PipelineAuthorizationDenied(f"User denied permission to move '{src}' to '{dst}'")

        try:
            dst.parent.mkdir(parents=True, exist_ok=True)
            src.rename(dst)

        except OSError as e:
            raise PipelineToolError(f"Failed to move: {e}") from e

        return f"Moved successfully: {src} -> {dst}"

    def _get_info(self, arguments: Dict[str, Any]) -> str:
        path = self._resolve_path("path", arguments)
        format_type = arguments.get("format")

        if not path.exists():
            raise PipelineToolError(f"Path does not exist: {path}")

        try:
            stat = path.stat()
            modified = self._format_time(
                datetime.fromtimestamp(stat.st_mtime, tz=timezone.utc), format_type
            )

            if path.is_file():
                result: Dict[str, Any] = {
                    "path": str(path),
                    "type": "file",
                    "size_bytes": stat.st_size,
                    "modified": modified,
                    "permissions": oct(stat.st_mode)[-3:],
                    "extension": path.suffix or None
                }

            elif path.is_dir():
                try:
                    items = list(path.iterdir())
                    items_info: Dict[str, Any] = {
                        "total": len(items),
                        "files": sum(1 for i in items if i.is_file()),
                        "directories": sum(1 for i in items if i.is_dir())
                    }
                except PermissionError:
                    items_info = {"error": "Permission denied"}

                result = {
                    "path": str(path),
                    "type": "directory",
                    "items": items_info,
                    "modified": modified,
                    "permissions": oct(stat.st_mode)[-3:]
                }

            else:
                result = {
                    "path": str(path),
                    "type": "other",
                    "modified": modified,
                    "permissions": oct(stat.st_mode)[-3:]
                }

        except OSError as e:
            raise PipelineToolError(f"Failed to get info: {e}") from e

        return json.dumps(result, indent=2)


class ClockTool:
    """
    Clock tool matching the Humbug ClockAITool interface.

    Provides get_time, sleep, and alarm operations.  In the pipeline
    context, sleep and alarm are available but rarely useful; they are
    included for interface parity.
    """

    def execute(self, operation: str, arguments: Dict[str, Any]) -> str:
        """
        Execute a clock operation.

        Args:
            operation: Operation name
            arguments: Operation arguments

        Returns:
            String result of the operation

        Raises:
            PipelineToolError: If the operation fails
        """
        handlers = {
            "get_time": self._get_time,
            "sleep": self._sleep,
            "alarm": self._alarm,
        }

        handler = handlers.get(operation)
        if handler is None:
            raise PipelineToolError(f"Unknown clock operation: '{operation}'")

        return handler(arguments)

    def _get_current_time(self, timezone_str: Optional[str]) -> datetime:
        """Get current time in the specified timezone."""
        try:
            if timezone_str is None or timezone_str.upper() == "UTC":
                return datetime.now(timezone.utc)

            return datetime.now(zoneinfo.ZoneInfo(timezone_str))

        except Exception as e:
            raise PipelineToolError(f"Invalid timezone '{timezone_str}': {e}") from e

    def _format_time(self, dt: datetime, format_type: Optional[str]) -> str:
        """Format a datetime as ISO or Unix timestamp string."""
        if format_type == "timestamp":
            return str(int(dt.timestamp()))

        return dt.isoformat()[:26] + "Z" if dt.tzinfo == timezone.utc else dt.isoformat()

    def _parse_alarm_time(self, time_str: str, timezone_str: Optional[str]) -> datetime:
        """Parse alarm time from ISO or Unix timestamp string."""
        time_str = time_str.strip()

        try:
            return datetime.fromtimestamp(float(time_str), tz=timezone.utc)

        except ValueError:
            pass

        try:
            parsed = datetime.fromisoformat(time_str.replace("Z", "+00:00"))
            if parsed.tzinfo is None:
                if timezone_str is None or timezone_str.upper() == "UTC":
                    parsed = parsed.replace(tzinfo=timezone.utc)

                else:
                    parsed = parsed.replace(tzinfo=zoneinfo.ZoneInfo(timezone_str))

            return parsed

        except ValueError:
            pass

        raise PipelineToolError(
            f"Unable to parse time '{time_str}'. "
            "Supported formats: ISO (2024-01-15T14:30:00), timestamp (1705339800)"
        )

    def _get_time(self, arguments: Dict[str, Any]) -> str:
        fmt = arguments.get("format", "iso")
        tz = arguments.get("timezone")
        now = self._get_current_time(tz)
        return self._format_time(now, fmt)

    def _sleep(self, arguments: Dict[str, Any]) -> str:
        import time as time_module

        duration = arguments.get("duration")
        if duration is None:
            raise PipelineToolError("'duration' is required for sleep operation")

        if not isinstance(duration, (int, float)):
            raise PipelineToolError("'duration' must be a number")

        if duration < 0:
            raise PipelineToolError("'duration' cannot be negative")

        time_module.sleep(duration)
        fmt = arguments.get("format", "iso")
        tz = arguments.get("timezone")
        return self._format_time(self._get_current_time(tz), fmt)

    def _alarm(self, arguments: Dict[str, Any]) -> str:
        import time as time_module

        time_str = arguments.get("time")
        if not isinstance(time_str, str):
            raise PipelineToolError("'time' must be a string")

        fmt = arguments.get("format", "iso")
        tz = arguments.get("timezone")

        target = self._parse_alarm_time(time_str, tz)
        now = self._get_current_time(tz)
        delay = (target - now).total_seconds()

        if delay > 0:
            time_module.sleep(delay)

        return self._format_time(self._get_current_time(tz), fmt)


class ConsoleTool:
    """
    Console tool for writing pipeline output to stdout or stderr.

    This tool has no Humbug counterpart yet; it exists to support
    pipeline output without requiring a filesystem write.
    """

    def execute(self, operation: str, arguments: Dict[str, Any]) -> str:
        """
        Execute a console operation.

        Args:
            operation: Operation name ('write_stdout' or 'write_stderr')
            arguments: Operation arguments

        Returns:
            Confirmation string

        Raises:
            PipelineToolError: If the operation fails
        """
        handlers = {
            "write_stdout": self._write_stdout,
            "write_stderr": self._write_stderr,
        }

        handler = handlers.get(operation)
        if handler is None:
            raise PipelineToolError(f"Unknown console operation: '{operation}'")

        return handler(arguments)

    def _write_stdout(self, arguments: Dict[str, Any]) -> str:
        content = arguments.get("content")
        if not isinstance(content, str):
            raise PipelineToolError("'content' must be a string")

        print(content, end="")
        return "Written to stdout"

    def _write_stderr(self, arguments: Dict[str, Any]) -> str:
        content = arguments.get("content")
        if not isinstance(content, str):
            raise PipelineToolError("'content' must be a string")

        print(content, end="", file=sys.stderr)
        return "Written to stderr"
