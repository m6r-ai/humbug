"""Menai trace watcher implementations.

This module provides standard trace watcher implementations for debugging
and profiling Menai programs.
"""

from typing import List, Any


class MenaiStdoutTraceWatcher:
    """Watcher that prints trace messages to stdout."""

    def on_trace(self, message: str) -> None:
        """
        Print trace message to stdout.

        Args:
            message: The trace message as a string (Menai formatted)
        """
        print(message)


class MenaiFileTraceWatcher:
    """Watcher that writes trace messages to a file."""

    def __init__(self, filepath: str):
        """
        Initialize file trace watcher.

        Args:
            filepath: Path to the file to write traces to
        """
        try:
            self.file = open(filepath, 'w', encoding='utf-8')  # pylint: disable=consider-using-with

        except IOError as e:
            raise RuntimeError(f"Failed to open trace file '{filepath}': {e}") from e

    def on_trace(self, message: str) -> None:
        """
        Write trace message to file.

        Args:
            message: The trace message as a string (Menai formatted)
        """
        try:
            self.file.write(message + '\n')
            self.file.flush()

        except IOError as e:
            raise RuntimeError(f"Failed to write to trace file: {e}") from e

    def close(self) -> None:
        """Close the trace file."""
        self.file.close()

    def __enter__(self) -> 'MenaiFileTraceWatcher':
        """Context manager entry."""
        return self

    def __exit__(self, *args: Any) -> None:
        """Context manager exit."""
        self.close()


class MenaiBufferingTraceWatcher:
    """
    Watcher that buffers trace messages for programmatic access.

    Includes a configurable limit to prevent unbounded memory growth
    in case of infinite loops or excessive tracing.
    """

    def __init__(self, max_traces: int = 10000) -> None:
        """
        Initialize buffering trace watcher.

        Args:
            max_traces: Maximum number of traces to buffer (default: 10000).
                       When limit is reached, oldest traces are discarded.
        """
        self.traces: List[str] = []
        self.max_traces = max_traces
        self.total_traces = 0  # Total number of traces received (including discarded)
        self.clipped = False  # Whether traces have been clipped

    def on_trace(self, message: str) -> None:
        """
        Buffer trace message.

        If the buffer is full, the oldest trace is removed before adding the new one.

        Args:
            message: The trace message as a string (Menai formatted)
        """
        self.total_traces += 1

        if len(self.traces) >= self.max_traces:
            self.traces.pop(0)  # Remove oldest trace
            self.clipped = True

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
        self.total_traces = 0
        self.clipped = False

    def is_clipped(self) -> bool:
        """
        Check if traces have been clipped due to buffer limit.

        Returns:
            True if some traces were discarded, False otherwise
        """
        return self.clipped

    def get_total_count(self) -> int:
        """
        Get total number of traces received (including discarded).

        Returns:
            Total trace count
        """
        return self.total_traces
