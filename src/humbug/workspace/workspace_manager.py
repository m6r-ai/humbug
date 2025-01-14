"""
Manages Humbug application workspaces.

This module provides functionality for creating, opening, and managing Humbug workspaces.
A workspace contains project-specific settings, recent files, and conversation history.
"""

import json
import logging
import os
import shutil
from typing import Dict, Optional

from PySide6.QtCore import QObject, Signal

from humbug.workspace.workspace_error import WorkspaceError, WorkspaceExistsError, WorkspaceNotFoundError
from humbug.workspace.workspace_settings import WorkspaceSettings


class WorkspaceManager(QObject):
    """
    Manages Humbug application workspaces.

    A workspace is a directory containing project-specific settings and data. Each workspace
    has a .humbug subdirectory containing configuration files and conversation history.

    Implements singleton pattern to provide global access to workspace state.

    Attributes:
        WORKSPACE_DIR: Name of the workspace configuration directory
        SETTINGS_FILE: Name of the workspace settings file
        SESSION_FILE: Name of the file storing recent tabs
    """

    # Signal emitted when workspace settings change
    settings_changed = Signal()

    WORKSPACE_DIR = ".humbug"
    SETTINGS_FILE = "settings.json"
    SESSION_FILE = "session.json"

    _instance = None
    _logger = logging.getLogger("WorkspaceManager")

    def __new__(cls):
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(WorkspaceManager, cls).__new__(cls)
            # Note: Don't initialize here - wait for __init__
        return cls._instance

    def __init__(self):
        """Initialize workspace manager if not already initialized."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._workspace_path: Optional[str] = None
            self._settings: Optional[WorkspaceSettings] = None
            self._home_config = os.path.expanduser("~/.humbug/workspace.json")
            self._initialized = True

    @property
    def workspace_path(self) -> Optional[str]:
        """
        Get the current workspace path.

        Returns:
            The absolute path to the current workspace, or None if no workspace is open.
        """
        return self._workspace_path

    @property
    def settings(self) -> Optional[WorkspaceSettings]:
        """
        Get the current workspace settings.

        Returns:
            The WorkspaceSettings object for the current workspace, or None if no workspace is open.
        """
        return self._settings

    def update_settings(self, new_settings: WorkspaceSettings) -> None:
        """
        Update workspace settings and notify listeners.

        Args:
            new_settings: New settings to apply

        Raises:
            WorkspaceError: If settings cannot be saved
            WorkspaceNotFoundError: If no workspace is open
        """
        if not self.has_workspace:
            raise WorkspaceNotFoundError("No workspace is currently open")

        # Save settings to file
        settings_path = os.path.join(
            self._workspace_path,
            self.WORKSPACE_DIR,
            self.SETTINGS_FILE
        )
        try:
            new_settings.save(settings_path)
            self._settings = new_settings
            self.settings_changed.emit()
        except OSError as e:
            raise WorkspaceError(f"Failed to save workspace settings: {str(e)}") from e

    @property
    def has_workspace(self) -> bool:
        """
        Check if a workspace is currently open.

        Returns:
            True if a workspace is open, False otherwise.
        """
        return self._workspace_path is not None

    def create_workspace(self, path: str) -> None:
        """
        Create a new workspace at the specified path.

        Args:
            path: Directory path where the workspace should be created.

        Raises:
            WorkspaceExistsError: If a workspace already exists at the specified path.
            OSError: If there are filesystem errors creating the workspace.
        """
        workspace_dir = os.path.join(path, self.WORKSPACE_DIR)
        if os.path.exists(workspace_dir):
            raise WorkspaceExistsError(f"Workspace already exists at {path}")

        try:
            # Create workspace directory structure
            os.makedirs(workspace_dir)

            # Create and save default settings
            settings = WorkspaceSettings()
            settings.save(os.path.join(workspace_dir, self.SETTINGS_FILE))

            # Create empty session file
            session_path = os.path.join(workspace_dir, self.SESSION_FILE)
            with open(session_path, 'w', encoding='utf-8') as f:
                json.dump({"tabs": []}, f, indent=2)

        except OSError as e:
            self._logger.error("Failed to create workspace at %s: %s", path, str(e))
            # Clean up any partially created workspace
            if os.path.exists(workspace_dir):
                try:
                    shutil.rmtree(workspace_dir)
                except OSError:
                    pass  # Ignore cleanup errors
            raise

    def open_workspace(self, path: str) -> None:
        """
        Open an existing workspace.

        Args:
            path: Path to the workspace directory.

        Returns:
            The WorkspaceSettings object for the opened workspace.

        Raises:
            WorkspaceNotFoundError: If no workspace exists at the specified path.
            WorkspaceError: If there are errors loading the workspace settings.
        """
        workspace_dir = os.path.join(path, self.WORKSPACE_DIR)
        if not os.path.exists(workspace_dir):
            raise WorkspaceNotFoundError(f"No workspace found at {path}")

        try:
            settings_path = os.path.join(workspace_dir, self.SETTINGS_FILE)
            settings = WorkspaceSettings.load(settings_path)
            self._workspace_path = path
            self._settings = settings
            self._update_home_tracking()
            self.settings_changed.emit()

        except Exception as e:
            self._logger.error("Failed to open workspace at %s: %s", path, str(e))
            raise WorkspaceError(f"Failed to open workspace: {str(e)}") from e

    def close_workspace(self) -> None:
        """Close the current workspace."""
        if self.has_workspace:
            self._workspace_path = None
            self._settings = None
            self._update_home_tracking()
            self.settings_changed.emit()

    def save_workspace_state(self, state: Dict) -> None:
        """
        Save workspace state to disk.

        Args:
            state: Dictionary containing tabs and layout state

        Raises:
            WorkspaceError: If saving state fails
        """
        if not self.has_workspace:
            raise WorkspaceError("No workspace is active")

        try:
            # Ensure .humbug directory exists
            self.ensure_workspace_dir(".humbug")

            for tab_state in state.get('tabs', []):
                if 'path' in tab_state and os.path.isabs(tab_state['path']):
                    try:
                        tab_state['path'] = os.path.relpath(
                            tab_state['path'],
                            self.workspace_path
                        )
                    except ValueError:
                        # Path is outside workspace, keep as absolute
                        pass

            # Write session file
            session_file = os.path.join(self.workspace_path, self.WORKSPACE_DIR, self.SESSION_FILE)
            with open(session_file, 'w', encoding='utf-8') as f:
                json.dump(state, f)

        except OSError as e:
            raise WorkspaceError(f"Failed to save workspace state: {str(e)}") from e

    def load_workspace_state(self) -> Dict:
        """
        Load workspace state from disk.

        Returns:
            Dictionary containing tabs and layout state

        Raises:
            WorkspaceError: If loading state fails
        """
        if not self.has_workspace:
            raise WorkspaceError("No workspace is active")

        try:
            session_file = os.path.join(self.workspace_path, self.WORKSPACE_DIR, self.SESSION_FILE)
            if not os.path.exists(session_file):
                return []

            with open(session_file, encoding='utf-8') as f:
                state = json.load(f)

            for tab_state in state.get('tabs', []):
                if 'path' in tab_state and not os.path.isabs(tab_state['path']):
                    tab_state['path'] = os.path.join(
                        self.workspace_path,
                        tab_state['path']
                    )

            return state

        except json.JSONDecodeError as e:
            raise WorkspaceError(f"Failed to parse workspace state: {str(e)}") from e
        except OSError as e:
            raise WorkspaceError(f"Failed to load workspace state: {str(e)}") from e

    def get_last_workspace(self) -> Optional[str]:
        """
        Get the path of the last opened workspace.

        Returns:
            Path to the last workspace that was opened, or None if no workspace has been opened
            or the last workspace no longer exists.
        """
        try:
            with open(self._home_config, encoding='utf-8') as f:
                data = json.load(f)
                workspace_path = data.get("lastWorkspace")
                if workspace_path and os.path.exists(workspace_path):
                    return workspace_path
        except (FileNotFoundError, json.JSONDecodeError):
            pass
        return None

    def get_workspace_path(self, relative_path: str) -> str:
        """
        Convert a workspace-relative path to an absolute path.

        Args:
            relative_path: Path relative to the workspace root.

        Returns:
            Absolute path within the workspace.

        Raises:
            WorkspaceNotFoundError: If no workspace is currently open.
        """
        if not self.has_workspace:
            raise WorkspaceNotFoundError("No workspace is currently open")
        return os.path.join(self._workspace_path, relative_path)

    def make_relative_path(self, path: str) -> Optional[str]:
        """
        Convert an absolute path to a workspace-relative path if possible.

        Args:
            path: Absolute path to convert.

        Returns:
            Path relative to workspace root, or None if path is outside workspace.
        """
        if not self.has_workspace:
            return None

        try:
            # Normalize both paths for comparison
            abs_path = os.path.abspath(path)
            workspace_path = os.path.abspath(self._workspace_path)

            # Check if the path is actually within the workspace
            # by comparing the normalized path beginnings
            common_path = os.path.commonpath([abs_path, workspace_path])
            if common_path != workspace_path:
                return None  # Path is outside workspace

            # If we get here, the path is within the workspace, so make it relative
            return os.path.relpath(abs_path, workspace_path)
        except ValueError:
            return None  # Path is on different

    def ensure_workspace_dir(self, dir_path: str) -> str:
        """
        Ensure a directory exists within the workspace.

        Args:
            dir_path: Directory path relative to workspace root.

        Returns:
            Absolute path to the created directory.

        Raises:
            WorkspaceNotFoundError: If no workspace is currently open.
            OSError: If directory cannot be created.
        """
        if not self.has_workspace:
            raise WorkspaceNotFoundError("No workspace is currently open")

        abs_path = self.get_workspace_path(dir_path)
        try:
            os.makedirs(abs_path, exist_ok=True)
            return abs_path
        except OSError as e:
            raise WorkspaceError(f"Failed to create directory '{dir_path}' in workspace: {e}") from e

    def _update_home_tracking(self) -> None:
        """Update the home directory tracking file with current workspace path."""
        try:
            os.makedirs(os.path.dirname(self._home_config), exist_ok=True)
            with open(self._home_config, 'w', encoding='utf-8') as f:
                json.dump({"lastWorkspace": self._workspace_path}, f, indent=2)
        except OSError as e:
            self._logger.error("Failed to update home tracking: %s", str(e))
            # Non-critical error, don't raise
