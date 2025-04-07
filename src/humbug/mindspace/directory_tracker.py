"""Directory tracking for mindspaces."""

import json
import logging
import os


from humbug.mindspace.directory_tracking import DirectoryTracking


class DirectoryTracker:
    """Tracks and persists last used directories per mindspace."""

    TRACKING_FILE = "directories.json"

    def __init__(self) -> None:
        """Initialize the directory tracker."""
        self._logger = logging.getLogger("DirectoryTracker")
        self._tracking: DirectoryTracking | None = None

    def _get_tracking_path(self, mindspace_path: str) -> str:
        """Get path to tracking file in mindspace."""
        return os.path.join(mindspace_path, ".humbug", self.TRACKING_FILE)

    def load_tracking(self, mindspace_path: str) -> None:
        """Load directory tracking for mindspace."""
        tracking_path = self._get_tracking_path(mindspace_path)

        try:
            if os.path.exists(tracking_path):
                with open(tracking_path, encoding='utf-8') as f:
                    data = json.load(f)

                # Validate stored paths - fall back to defaults if invalid
                self._tracking = DirectoryTracking.from_dict(data, mindspace_path)

                # Verify paths still exist, reset to defaults if not
                if not os.path.exists(self._tracking.file_dialog):
                    self._tracking.file_dialog = mindspace_path

                if not os.path.exists(self._tracking.conversations):
                    self._tracking.conversations = os.path.join(mindspace_path, "conversations")

            else:
                self._tracking = DirectoryTracking.create_default(mindspace_path)

        except (json.JSONDecodeError, OSError) as e:
            self._logger.warning("Failed to load directory tracking: %s", str(e))
            self._tracking = DirectoryTracking.create_default(mindspace_path)

    def save_tracking(self, mindspace_path: str) -> None:
        """Save current directory tracking to mindspace."""
        if not self._tracking:
            return

        tracking_path = self._get_tracking_path(mindspace_path)

        try:
            with open(tracking_path, 'w', encoding='utf-8') as f:
                json.dump(self._tracking.to_dict(), f, indent=2)
        except OSError as e:
            self._logger.error("Failed to save directory tracking: %s", str(e))

    def clear_tracking(self) -> None:
        """Clear current tracking state."""
        self._tracking = None

    def update_file_dialog_directory(self, path: str) -> None:
        """Update the last used file dialog directory."""
        if self._tracking:
            self._tracking.file_dialog = os.path.dirname(path)

    def update_conversations_directory(self, path: str) -> None:
        """Update the last used conversations directory."""
        if self._tracking:
            self._tracking.conversations = os.path.dirname(path)

    @property
    def file_dialog_directory(self) -> str | None:
        """Get the last used file dialog directory."""
        return self._tracking.file_dialog if self._tracking else None

    @property
    def conversations_directory(self) -> str | None:
        """Get the last used conversations directory."""
        return self._tracking.conversations if self._tracking else None
