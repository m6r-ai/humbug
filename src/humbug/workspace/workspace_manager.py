"""Manages Humbug application workspaces.

This module provides functionality for creating, opening, and managing Humbug workspaces.
A workspace contains project-specific settings, recent files, and conversation history.
"""

import json
import logging
import os
import shutil
from typing import Dict, List, Optional

from humbug.workspace.workspace_settings import WorkspaceSettings


logger = logging.getLogger(__name__)


class WorkspaceError(Exception):
    """Base exception for workspace-related errors."""


class WorkspaceNotFoundError(WorkspaceError):
    """Raised when attempting to access a non-existent workspace."""


class WorkspaceExistsError(WorkspaceError):
    """Raised when attempting to create a workspace that already exists."""


class WorkspaceManager:
    """
    Manages Humbug application workspaces.

    A workspace is a directory containing project-specific settings and data. Each workspace
    has a .humbug subdirectory containing configuration files and conversation history.

    Attributes:
        WORKSPACE_DIR: Name of the workspace configuration directory
        SETTINGS_FILE: Name of the workspace settings file
        RECENTS_FILE: Name of the file storing recent tabs
    """

    WORKSPACE_DIR = ".humbug"
    SETTINGS_FILE = "settings.json"
    RECENTS_FILE = "recents.json"

    def __init__(self):
        """Initialize the workspace manager."""
        self._workspace_path: Optional[str] = None
        self._settings: Optional[WorkspaceSettings] = None
        self._home_config = os.path.expanduser("~/.humbug/workspace.json")

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

            # Create empty recents file
            recents_path = os.path.join(workspace_dir, self.RECENTS_FILE)
            with open(recents_path, 'w', encoding='utf-8') as f:
                json.dump({"tabs": []}, f, indent=2)

        except OSError as e:
            logger.error("Failed to create workspace at %s: %s", path, str(e))
            # Clean up any partially created workspace
            if os.path.exists(workspace_dir):
                try:
                    shutil.rmtree(workspace_dir)
                except OSError:
                    pass  # Ignore cleanup errors
            raise

    def open_workspace(self, path: str) -> WorkspaceSettings:
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
            return settings

        except Exception as e:
            logger.error("Failed to open workspace at %s: %s", path, str(e))
            raise WorkspaceError(f"Failed to open workspace: {str(e)}") from e

    def close_workspace(self) -> None:
        """Close the current workspace."""
        if self.has_workspace:
            self._workspace_path = None
            self._settings = None
            self._update_home_tracking()

    def save_workspace_state(self, tabs: List[Dict]) -> None:
        """
        Save the current workspace state including open tabs.

        Args:
            tabs: List of dictionaries containing tab information to save.

        Raises:
            WorkspaceNotFoundError: If no workspace is currently open.
            OSError: If there are errors writing the state file.
        """
        if not self.has_workspace:
            raise WorkspaceNotFoundError("No workspace is currently open")

        # Convert paths that are within the workspace to relative paths
        # Keep paths outside the workspace as absolute
        for tab in tabs:
            if tab.get('path') and os.path.isabs(tab['path']):
                relative_path = self.make_relative_path(tab['path'])
                if relative_path:  # Path is inside workspace
                    tab['path'] = relative_path
                # If relative_path is None, path is outside workspace - keep it absolute

        recents_path = os.path.join(self._workspace_path, self.WORKSPACE_DIR, self.RECENTS_FILE)
        try:
            with open(recents_path, "w", encoding='utf-8') as f:
                json.dump({"tabs": tabs}, f, indent=2)
        except OSError as e:
            logger.error("Failed to save workspace state: %s", str(e))
            raise

    def load_workspace_state(self) -> Optional[List[Dict]]:
        """
        Load the workspace state including previously open tabs.

        Returns:
            List of dictionaries containing tab information, or None if state cannot be loaded.

        Raises:
            WorkspaceNotFoundError: If no workspace is currently open.
        """
        if not self.has_workspace:
            raise WorkspaceNotFoundError("No workspace is currently open")

        try:
            recents_path = os.path.join(self._workspace_path, self.WORKSPACE_DIR, self.RECENTS_FILE)
            with open(recents_path, encoding='utf-8') as f:
                data = json.load(f)
                tabs = data.get("tabs", [])

                # Handle paths based on whether they are relative or absolute
                for tab in tabs:
                    if tab.get('path'):
                        if not os.path.isabs(tab['path']):
                            # Convert relative paths to absolute within the workspace
                            tab['path'] = os.path.join(self._workspace_path, tab['path'])

                return tabs
        except (FileNotFoundError, json.JSONDecodeError) as e:
            logger.error("Error loading workspace state: %s", str(e))
            return None

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
        os.makedirs(abs_path, exist_ok=True)
        return abs_path

    def _update_home_tracking(self) -> None:
        """Update the home directory tracking file with current workspace path."""
        try:
            os.makedirs(os.path.dirname(self._home_config), exist_ok=True)
            with open(self._home_config, 'w', encoding='utf-8') as f:
                json.dump({"lastWorkspace": self._workspace_path}, f, indent=2)
        except OSError as e:
            logger.error("Failed to update home tracking: %s", str(e))
            # Non-critical error, don't raise
