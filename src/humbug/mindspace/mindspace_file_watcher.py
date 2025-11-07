"""File change detection service for mindspace preview content."""

import logging
import os
from typing import Dict, Set, Callable
from dataclasses import dataclass

from PySide6.QtCore import QObject, QTimer


@dataclass
class FileInfo:
    """Information about a watched file."""
    mtime: float
    exists: bool
    is_dir: bool
    dir_contents: Set[str] | None = None  # For directories, track contents


class MindspaceFileWatcher(QObject):
    """
    Polls files for changes and notifies registered callbacks.

    Uses file modification time and directory contents to detect changes.
    Designed for use with remote filesystems where inotify may not work reliably.

    Implements singleton pattern to provide global access to file watching state.
    """

    _instance = None
    _logger = logging.getLogger("MindspaceFileWatcher")

    def __new__(cls) -> 'MindspaceFileWatcher':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(MindspaceFileWatcher, cls).__new__(cls)

        return cls._instance

    def __init__(self, poll_interval: int = 1000) -> None:
        """
        Initialize the file watcher if not already initialized.

        Args:
            poll_interval: Time in milliseconds between polling cycles
        """
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._poll_interval = poll_interval
            self._watched_files: Dict[str, FileInfo] = {}  # path -> file info
            self._callbacks: Dict[str, Set[Callable[[str], None]]] = {}  # path -> callbacks
            self._timer = QTimer(self)
            self._timer.timeout.connect(self._poll_files)
            self._timer.setSingleShot(False)
            self._running = False
            self._initialized = True

    def watch_file(self, file_path: str, callback: Callable[[str], None]) -> None:
        """
        Register a file for watching with a callback.

        Args:
            file_path: Absolute path to the file or directory to watch
            callback: Function to call when the file changes, receives the changed path
        """
        # Normalize path
        normalized_path = os.path.abspath(file_path)

        # Add callback
        if normalized_path not in self._callbacks:
            self._callbacks[normalized_path] = set()

        self._callbacks[normalized_path].add(callback)

        # Initialize file info if not already watched
        if normalized_path not in self._watched_files:
            try:
                self._watched_files[normalized_path] = self._get_file_info(normalized_path)
                self._logger.debug("Started watching file: %s", normalized_path)

            except OSError as e:
                self._logger.warning("Failed to get initial info for %s: %s", normalized_path, str(e))
                # Still add to watched files, but mark as non-existent
                self._watched_files[normalized_path] = FileInfo(
                    mtime=0.0,
                    exists=False,
                    is_dir=False
                )

        # Start polling if not already running
        if not self._running:
            self.start()

    def unwatch_file(self, file_path: str, callback: Callable[[str], None]) -> None:
        """
        Unregister a callback for a file.

        Args:
            file_path: Path that was being watched
            callback: Callback function to remove
        """
        normalized_path = os.path.abspath(file_path)

        if normalized_path in self._callbacks:
            self._callbacks[normalized_path].discard(callback)

            # If no more callbacks for this file, stop watching it
            if not self._callbacks[normalized_path]:
                del self._callbacks[normalized_path]
                if normalized_path in self._watched_files:
                    del self._watched_files[normalized_path]

                self._logger.debug("Stopped watching file: %s", normalized_path)

        # Stop polling if no files are being watched
        if not self._callbacks and self._running:
            self.stop()

    def _get_file_info(self, file_path: str) -> FileInfo:
        """
        Get current information about a file or directory.

        Args:
            file_path: Path to examine

        Returns:
            FileInfo object with current state

        Raises:
            OSError: If file cannot be accessed
        """
        if not os.path.exists(file_path):
            return FileInfo(mtime=0.0, exists=False, is_dir=False)

        stat_info = os.stat(file_path)
        is_dir = os.path.isdir(file_path)

        dir_contents = None
        if is_dir:
            try:
                dir_contents = set(os.listdir(file_path))

            except OSError as e:
                self._logger.warning("Failed to list directory contents for %s: %s", file_path, str(e))
                dir_contents = set()

        return FileInfo(
            mtime=stat_info.st_mtime,
            exists=True,
            is_dir=is_dir,
            dir_contents=dir_contents
        )

    def _poll_files(self) -> None:
        """Check all watched files for changes."""
        try:
            # Create a copy of the paths to avoid issues if the dict changes during iteration
            paths_to_check = list(self._watched_files.keys())

            for file_path in paths_to_check:
                try:
                    # Get current file info
                    current_info = self._get_file_info(file_path)

                    # Skip if file is no longer being watched (edge case)
                    if file_path not in self._watched_files:
                        continue

                    old_info = self._watched_files[file_path]
                    changed = False

                    # Check if file existence changed
                    if current_info.exists != old_info.exists:
                        changed = True
                        if current_info.exists:
                            self._logger.debug("File appeared: %s", file_path)

                        else:
                            self._logger.debug("File disappeared: %s", file_path)

                    # Check if modification time changed (only for existing files)
                    elif current_info.exists and current_info.mtime != old_info.mtime:
                        changed = True
                        self._logger.debug("File modified: %s", file_path)

                    # Check if directory contents changed (for directories)
                    elif (current_info.exists and current_info.is_dir and
                          current_info.dir_contents != old_info.dir_contents):
                        changed = True
                        self._logger.debug("Directory contents changed: %s", file_path)

                    if changed:
                        # Update stored info
                        self._watched_files[file_path] = current_info

                        # Notify all callbacks for this file
                        callbacks = self._callbacks.get(file_path, set()).copy()

                        # Call callbacks
                        for callback in callbacks:
                            try:
                                callback(file_path)

                            except Exception as e:
                                self._logger.error("Error in file change callback for %s: %s",
                                                 file_path, str(e), exc_info=True)

                except OSError as e:
                    self._logger.warning("Error checking file %s: %s", file_path, str(e))

                except Exception as e:
                    self._logger.error("Unexpected error checking file %s: %s",
                                     file_path, str(e), exc_info=True)

        except Exception as e:
            self._logger.error("Error in file polling: %s", str(e), exc_info=True)

    def start(self) -> None:
        """Start the polling timer."""
        if not self._running:
            self._running = True
            self._timer.start(self._poll_interval)
            self._logger.debug("File watcher started")

    def stop(self) -> None:
        """Stop the polling timer."""
        if self._running:
            self._running = False
            self._timer.stop()
            self._logger.debug("File watcher stopped")

    def is_running(self) -> bool:
        """Check if the file watcher is currently running."""
        return self._running

    def get_watched_files(self) -> Set[str]:
        """Get a copy of all currently watched file paths."""
        return set(self._watched_files.keys())
