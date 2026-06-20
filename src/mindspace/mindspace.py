import json
import logging
import os
import shutil
from typing import Callable, Dict, List

from ai_tool import AIToolManager
from context.context_registry import ContextRegistry

from mindspace.mindspace_error import MindspaceError, MindspaceExistsError, MindspaceNotFoundError
from mindspace.mindspace_interactions import MindspaceInteractions
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace_message import MindspaceMessage
from mindspace.mindspace_settings import MindspaceSettings
from mindspace.mindspace_usage import MindspaceUsage


class Mindspace:
    """
    Core mindspace model.

    Owns settings, the interaction log, path resolution, and session persistence.
    Has no Qt dependency — notifies observers via plain callbacks.
    """

    MINDSPACE_DIR     = ".humbug"
    SETTINGS_FILE     = "settings.json"
    SESSION_FILE      = "session.json"
    INTERACTIONS_FILE = "system.json"
    USAGE_FILE        = "usage.json"

    def __init__(
        self,
        on_settings_changed: Callable[[], None],
        on_interactions_updated: Callable[[], None],
        on_usage_updated: Callable[[], None] | None = None,
    ) -> None:
        """
        Initialise the mindspace model.

        Args:
            on_settings_changed: Called whenever settings change or a mindspace
                is opened or closed.
            on_interactions_updated: Called whenever a new interaction is added.
            on_usage_updated: Called whenever usage stats are updated.
        """
        self._on_settings_changed     = on_settings_changed
        self._on_interactions_updated = on_interactions_updated
        self._on_usage_updated        = on_usage_updated
        self._path: str = ""
        self._settings: MindspaceSettings | None = None
        self._interactions = MindspaceInteractions()
        self._usage = MindspaceUsage()
        self._context_registry = ContextRegistry()
        self._tool_manager = AIToolManager()
        self._logger = logging.getLogger("Mindspace")

    def mindspace_path(self) -> str:
        """Return the absolute path to the open mindspace, or empty string."""
        return self._path

    def has_mindspace(self) -> bool:
        """Return True if a mindspace is currently open."""
        return bool(self._path)

    def settings(self) -> MindspaceSettings | None:
        """Return current mindspace settings, or None if no mindspace is open."""
        return self._settings

    def contexts(self) -> ContextRegistry:
        """
        Return the context registry for this mindspace.

        The registry tracks all open contexts and emits events when they change.
        """
        return self._context_registry

    def is_already_mindspace(self, path: str) -> bool:
        """Return True if a mindspace already exists at path."""
        return os.path.exists(os.path.join(path, self.MINDSPACE_DIR))

    def check_mindspace(self, path: str) -> bool:
        """Return True if a mindspace exists at path."""
        return os.path.exists(os.path.join(path, self.MINDSPACE_DIR))

    def create_mindspace(self, path: str, folders: List[str]) -> None:
        """
        Create a new mindspace at path.

        Args:
            path: Directory in which to create the mindspace.
            folders: Subdirectories to create inside the mindspace root.

        Raises:
            MindspaceExistsError: A mindspace already exists at path.
            MindspaceError: Filesystem error during creation.
        """
        mindspace_dir = os.path.join(path, self.MINDSPACE_DIR)
        if os.path.exists(mindspace_dir):
            raise MindspaceExistsError(f"Mindspace already exists at {path}")

        try:
            os.makedirs(mindspace_dir)

            for folder in folders:
                os.makedirs(os.path.join(path, folder), exist_ok=True)

            settings = MindspaceSettings(self._tool_manager.get_default_enabled_tools())
            settings.save(os.path.join(mindspace_dir, self.SETTINGS_FILE))

            with open(os.path.join(mindspace_dir, self.SESSION_FILE), 'w', encoding='utf-8') as f:
                json.dump({"tabs": []}, f, indent=4)

        except OSError as e:
            self._logger.error("Failed to create mindspace at %s: %s", path, str(e))
            if os.path.exists(mindspace_dir):
                shutil.rmtree(mindspace_dir, ignore_errors=True)

            for folder in folders:
                folder_path = os.path.join(path, folder)
                if os.path.exists(folder_path):
                    shutil.rmtree(folder_path, ignore_errors=True)

            raise MindspaceError(f"Failed to create mindspace: {str(e)}") from e

    def open_mindspace(self, path: str) -> None:
        """
        Open an existing mindspace.

        Args:
            path: Path to the mindspace directory.

        Raises:
            MindspaceNotFoundError: No mindspace exists at path.
            MindspaceError: Error loading settings.
        """
        mindspace_dir = os.path.join(path, self.MINDSPACE_DIR)
        if not os.path.exists(mindspace_dir):
            raise MindspaceNotFoundError(f"No mindspace found at {path}")

        try:
            settings = MindspaceSettings.load(os.path.join(mindspace_dir, self.SETTINGS_FILE))
            self._path = path
            self._settings = settings
            self._load_interactions()
            self._load_usage()
            self._apply_tool_settings(settings)
            self._on_settings_changed()

        except Exception as e:
            self._logger.error("Failed to open mindspace at %s: %s", path, str(e))
            raise MindspaceError(f"Failed to open mindspace: {str(e)}") from e

    def close_mindspace(self) -> None:
        """Close the current mindspace and reset all state."""
        if self.has_mindspace():
            self._path = ""
            self._settings = None
            self._interactions.clear()
            self._usage = MindspaceUsage()
            self._context_registry.clear()
            self._reset_tool_manager()
            self._on_settings_changed()

    def update_settings(self, new_settings: MindspaceSettings) -> None:
        """
        Persist and apply updated settings.

        Args:
            new_settings: New settings to apply.

        Raises:
            MindspaceError: Settings could not be saved.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        settings_path = os.path.join(self._path, self.MINDSPACE_DIR, self.SETTINGS_FILE)
        try:
            new_settings.save(settings_path)
            self._settings = new_settings
            self._apply_tool_settings(new_settings)
            self._on_settings_changed()

        except OSError as e:
            raise MindspaceError(f"Failed to save mindspace settings: {str(e)}") from e

    def get_absolute_path(self, path: str) -> str:
        """
        Convert a mindspace-relative path to an absolute path.

        Args:
            path: Absolute path or path relative to the mindspace root.

        Returns:
            Absolute path.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        path = os.path.expanduser(path)
        if os.path.isabs(path):
            return os.path.abspath(path)

        return os.path.abspath(os.path.join(self._path, path))

    def get_relative_path(self, path: str) -> str:
        """
        Convert an absolute path to a mindspace-relative path if possible.

        Args:
            path: Absolute path to convert.

        Returns:
            Path relative to mindspace root, or the absolute path if outside.
        """
        abs_path = os.path.abspath(os.path.expanduser(path))
        if not self.has_mindspace():
            return abs_path

        try:
            mindspace_abs = os.path.abspath(self._path)
            if os.path.commonpath([abs_path, mindspace_abs]) != mindspace_abs:
                return abs_path

            return os.path.relpath(abs_path, self._path)

        except ValueError:
            self._logger.warning(
                "Failed to make path '%s' relative to mindspace '%s'",
                path, self._path
            )
            raise

    def get_mindspace_relative_path(self, path: str) -> str | None:
        """
        Convert an absolute path to a mindspace-relative path, or None if outside.

        Args:
            path: Absolute path to convert.

        Returns:
            Relative path if within mindspace, None otherwise.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        abs_path = os.path.abspath(os.path.expanduser(path))

        try:
            mindspace_abs = os.path.abspath(self._path)
            if os.path.commonpath([abs_path, mindspace_abs]) != mindspace_abs:
                return None

            return os.path.relpath(abs_path, self._path)

        except ValueError:
            return None

    def ensure_mindspace_dir(self, dir_path: str) -> str:
        """
        Ensure a directory exists within the mindspace, creating it if needed.

        Args:
            dir_path: Directory path relative to mindspace root.

        Returns:
            Absolute path to the directory.

        Raises:
            MindspaceError: Directory could not be created.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        abs_path = self.get_absolute_path(dir_path)
        try:
            os.makedirs(abs_path, exist_ok=True)
            return abs_path

        except OSError as e:
            raise MindspaceError(f"Failed to create directory '{dir_path}': {e}") from e

    def add_interaction(self, level: MindspaceLogLevel, content: str) -> MindspaceMessage:
        """
        Append a message to the interaction log and persist it.

        Args:
            level: Severity level of the message.
            content: Message text.

        Returns:
            The created MindspaceMessage.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        message = MindspaceMessage.create(level, content)
        self._interactions.add_message(message)
        self._save_interactions()
        self._on_interactions_updated()
        return message

    def get_interactions(self) -> List[MindspaceMessage]:
        """
        Return all interaction log messages.

        Returns:
            List of MindspaceMessage objects.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._interactions.get_messages()

    def save_mindspace_state(self, state: Dict) -> None:
        """
        Persist session state (open tabs, layout) to disk.

        Paths stored in the state dict are converted to mindspace-relative form
        before writing so the session is portable.

        Args:
            state: Dictionary containing tabs and layout state.

        Raises:
            MindspaceError: State could not be saved.
        """
        if not self.has_mindspace():
            raise MindspaceError("No mindspace is active")

        try:
            self.ensure_mindspace_dir(self.MINDSPACE_DIR)

            for tab_state in state.get('tabs', []):
                if 'path' in tab_state and os.path.isabs(tab_state['path']):
                    try:
                        tab_state['path'] = os.path.relpath(tab_state['path'], self._path)
                    except ValueError:
                        pass  # Path outside mindspace — keep absolute

            session_file = os.path.join(self._path, self.MINDSPACE_DIR, self.SESSION_FILE)
            with open(session_file, 'w', encoding='utf-8') as f:
                json.dump(state, f, indent=4)

        except OSError as e:
            raise MindspaceError(f"Failed to save mindspace state: {str(e)}") from e

    def load_mindspace_state(self) -> Dict:
        """
        Load session state from disk.

        Relative paths in the returned state are resolved to absolute paths.

        Returns:
            Dictionary containing tabs and layout state.

        Raises:
            MindspaceError: State could not be loaded.
        """
        if not self.has_mindspace():
            raise MindspaceError("No mindspace is active")

        session_file = os.path.join(self._path, self.MINDSPACE_DIR, self.SESSION_FILE)
        if not os.path.exists(session_file):
            return {}

        try:
            with open(session_file, encoding='utf-8') as f:
                state = json.load(f)

            for tab_state in state.get('tabs', []):
                if 'path' in tab_state and not os.path.isabs(tab_state['path']):
                    tab_state['path'] = os.path.join(self._path, tab_state['path'])

            return state

        except json.JSONDecodeError as e:
            raise MindspaceError(f"Failed to parse mindspace state: {str(e)}") from e

        except OSError as e:
            raise MindspaceError(f"Failed to load mindspace state: {str(e)}") from e

    def usage(self) -> MindspaceUsage:
        """Return the current mindspace usage stats."""
        return self._usage

    def update_usage(
        self,
        provider: str,
        model: str,
        input_tokens: int,
        output_tokens: int,
        cache_write_tokens: int = 0,
        cache_read_tokens: int = 0,
    ) -> None:
        """
        Record usage from a completed AI response and persist to disk.

        Args:
            provider: Provider identifier (e.g. 'anthropic').
            model: Model name (e.g. 'claude-sonnet-4-6').
            input_tokens: Number of prompt tokens consumed.
            output_tokens: Number of completion tokens generated.
            cache_write_tokens: Tokens written to provider cache.
            cache_read_tokens: Tokens read from provider cache.
        """
        self._usage.record(
            provider, model, input_tokens, output_tokens,
            cache_write_tokens=cache_write_tokens,
            cache_read_tokens=cache_read_tokens,
        )
        self._save_usage()
        if self._on_usage_updated:
            self._on_usage_updated()

    def reset_usage(self) -> None:
        """Clear all accumulated usage stats and persist the empty state."""
        self._usage.reset()
        self._save_usage()
        if self._on_usage_updated:
            self._on_usage_updated()

    def _apply_tool_settings(self, settings: MindspaceSettings) -> None:
        """Apply tool enabled states from settings to the tool manager."""
        try:
            self._tool_manager.set_tool_enabled_states(settings.enabled_tools)

        except Exception as e:
            self._logger.error("Failed to apply tool settings: %s", e)

    def _reset_tool_manager(self) -> None:
        """Reset tool manager to defaults when no mindspace is open."""
        try:
            self._tool_manager.set_tool_enabled_states(
                self._tool_manager.get_default_enabled_tools()
            )

        except Exception as e:
            self._logger.error("Failed to reset tool manager: %s", e)

    def _save_usage(self) -> None:
        """Persist usage stats to disk."""
        try:
            mindspace_dir = os.path.join(self._path, self.MINDSPACE_DIR)
            os.makedirs(mindspace_dir, exist_ok=True)
            usage_path = os.path.join(mindspace_dir, self.USAGE_FILE)
            with open(usage_path, 'w', encoding='utf-8') as f:
                json.dump(self._usage.to_dict(), f, indent=4)

        except OSError as e:
            self._logger.error("Failed to save usage stats: %s", e)

    def _load_usage(self) -> None:
        """Load usage stats from disk, silently ignoring missing files."""
        try:
            usage_path = os.path.join(self._path, self.MINDSPACE_DIR, self.USAGE_FILE)
            if not os.path.exists(usage_path):
                self._usage = MindspaceUsage()
                return

            with open(usage_path, encoding='utf-8') as f:
                data = json.load(f)

            self._usage = MindspaceUsage.from_dict(data)

        except Exception as e:
            self._logger.info("Failed to load usage stats: %s", e)
            self._usage = MindspaceUsage()

    def _save_interactions(self) -> None:
        """Persist the interaction log to disk."""
        try:
            mindspace_dir = os.path.join(self._path, self.MINDSPACE_DIR)
            os.makedirs(mindspace_dir, exist_ok=True)
            self._interactions.save(os.path.join(mindspace_dir, self.INTERACTIONS_FILE))

        except OSError as e:
            self._logger.error("Failed to save interactions: %s", e)

    def _load_interactions(self) -> None:
        """Load the interaction log from disk."""
        try:
            interactions_path = os.path.join(
                self._path, self.MINDSPACE_DIR, self.INTERACTIONS_FILE
            )
            self._interactions.load(interactions_path)

        except Exception as e:
            self._logger.info("Failed to load interactions: %s", e)
