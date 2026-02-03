"""AIFPL trace watcher implementations.

This module provides standard trace watcher implementations for debugging
and profiling AIFPL programs.
"""

from typing import List, Any


class AIFPLStdoutTraceWatcher:
    """Watcher that prints trace messages to stdout."""

    def on_trace(self, message: str) -> None:
        """
        Print trace message to stdout.

        Args:
            message: The trace message as a string (AIFPL formatted)
        """
        print(message)


class AIFPLFileTraceWatcher:
    """Watcher that writes trace messages to a file."""

    def __init__(self, filepath: str):
        """
        Initialize file trace watcher.

        Args:
            filepath: Path to the file to write traces to
        """
        try:
            self.file = open(filepath, 'w', encoding='utf-8')

        except IOError as e:
            raise RuntimeError(f"Failed to open trace file '{filepath}': {e}") from e

    def on_trace(self, message: str) -> None:
        """
        Write trace message to file.

        Args:
            message: The trace message as a string (AIFPL formatted)
        """
        try:
            self.file.write(message + '\n')
            self.file.flush()

        except IOError as e:
            raise RuntimeError(f"Failed to write to trace file: {e}") from e

    def close(self) -> None:
        """Close the trace file."""
        self.file.close()

    def __enter__(self) -> 'AIFPLFileTraceWatcher':
        """Context manager entry."""
        return self

    def __exit__(self, *args: Any) -> None:
        """Context manager exit."""
        self.close()


class AIFPLBufferingTraceWatcher:
    """Watcher that buffers trace messages for programmatic access."""

    def __init__(self) -> None:
        """Initialize buffering trace watcher."""
        self.traces: List[str] = []

    def on_trace(self, message: str) -> None:
        """
        Buffer trace message.

        Args:
            message: The trace message as a string (AIFPL formatted)
        """
        self.traces.append(message)

    def get_traces(self) -> List[str]:
        """
        Get all buffered traces.

        Returns:
            List of trace messages
        """
        return self.traces.copy()

    def clear(self) -> None:
        """Clear all buffered traces."""
        self.traces.clear()
