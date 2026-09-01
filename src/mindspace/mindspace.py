from collections.abc import Callable
import json
import logging
import os
import shutil

from ai import AIConversationSettings
from ai_tool import AIToolManager
from context.context_registry import ContextRegistry
from conversation_context.conversation_context import ConversationContext

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
    """

    MINDSPACE_DIR = ".humbug"
    SETTINGS_FILE = "settings.json"
    SESSION_FILE = "session.json"
    INTERACTIONS_FILE = "system.json"
    USAGE_FILE = "usage.json"
    CONVERSATIONS_DIR = "conversations"
    TRASH_DIR = "trash"

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
        self._on_settings_changed = on_settings_changed
        self._on_interactions_updated = on_interactions_updated
        self._on_usage_updated = on_usage_updated
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

    def create_mindspace(self, path: str, folders: list[str]) -> None:
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
            self._migrate_conversations()
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

    def apply_ai_settings_to_all(self, settings: AIConversationSettings) -> None:
        """
        Persist AI settings as the mindspace default and apply them to every open conversation.

        Updates the mindspace default's AI fields, then broadcasts the settings to
        each open conversation context.  The UI reacts through each context's
        on_settings_applied callback, so the mindspace remains the source of truth.

        Args:
            settings: The AIConversationSettings to apply to all conversations.
        """
        current = self._settings
        if current is not None:
            current.model = settings.model
            current.provider = settings.provider
            current.temperature = settings.temperature
            current.reasoning = settings.reasoning
            current.reasoning_effort = settings.reasoning_effort
            self.update_settings(current)

        for info in self._context_registry.list_all():
            model = self._context_registry.get_model(info.context_id, ConversationContext)
            if model is not None:
                model.update_conversation_settings(settings)

    def conversations_dir(self) -> str:
        """
        Return the absolute path to the conversations directory.

        The conversations directory lives inside the .humbug directory so that
        conversations do not clutter the user's mindspace content.  The directory
        is created if it does not already exist.

        Returns:
            Absolute path to the conversations directory.

        Raises:
            MindspaceError: No mindspace is open or directory could not be created.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        path = os.path.join(self._path, self.MINDSPACE_DIR, self.CONVERSATIONS_DIR)
        try:
            os.makedirs(path, exist_ok=True)

        except OSError as e:
            raise MindspaceError(f"Failed to create conversations directory: {str(e)}") from e

        return path

    def conversations_rel_path(self) -> str:
        """
        Return the mindspace-relative path to the conversations directory.

        Returns:
            Path relative to the mindspace root, e.g. '.humbug/conversations'.
        """
        return os.path.join(self.MINDSPACE_DIR, self.CONVERSATIONS_DIR)

    def trash_dir(self) -> str:
        """
        Return the absolute path to the trash directory, creating it if needed.

        The trash directory lives inside .humbug, alongside but outside of
        conversations/, so trashed items never appear in the conversations
        tree.  Used to support undoing a delete.

        Returns:
            Absolute path to the trash directory.

        Raises:
            MindspaceError: No mindspace is open or directory could not be created.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        path = os.path.join(self._path, self.MINDSPACE_DIR, self.TRASH_DIR)
        try:
            os.makedirs(path, exist_ok=True)

        except OSError as e:
            raise MindspaceError(f"Failed to create trash directory: {str(e)}") from e

        return path

    def can_pin_path(self, abs_path: str) -> bool:
        """
        Return True if abs_path is allowed to be pinned.

        Folders and root conversations can be pinned at any depth — pinning
        a folder shows it in the sidebar's Pinned section as well as its
        normal location.  Delegate/fork children are excluded: pulling a
        single fork branch out of its parent conversation while leaving the
        rest behind would be a structural oddity.

        Args:
            abs_path: Absolute filesystem path to check.

        Returns:
            True if the path may be pinned.
        """
        if os.path.isdir(abs_path):
            return True

        return not self._has_delegate_parent(abs_path)

    def _has_delegate_parent(self, abs_path: str) -> bool:
        """
        Return True if the conversation file at abs_path is a fork/delegate
        child (its metadata.parent field is set).

        Args:
            abs_path: Absolute path to a .conv file.

        Returns:
            True if the file has a delegate parent.
        """
        try:
            with open(abs_path, 'r', encoding='utf-8') as f:
                data = json.load(f)

        except (OSError, json.JSONDecodeError):
            return False

        if not isinstance(data, dict):
            return False

        metadata = data.get('metadata')
        return isinstance(metadata, dict) and metadata.get('parent') is not None

    def is_path_pinned(self, abs_path: str) -> bool:
        """
        Return True if the given absolute path is directly pinned.

        This is an exact match against pinned_paths — it does not consider
        whether an ancestor folder is pinned.  Used internally by tree
        building and root de-duplication, which need exact semantics.  For
        UI decisions (e.g. a context-menu label), use is_effectively_pinned().

        Args:
            abs_path: Absolute filesystem path to check.

        Returns:
            True if the path is pinned, False otherwise.
        """
        if not self.has_mindspace() or self._settings is None:
            return False

        rel_path = self.get_mindspace_relative_path(abs_path)
        return rel_path is not None and rel_path in self._settings.pinned_paths

    def is_effectively_pinned(self, abs_path: str) -> bool:
        """
        Return True if abs_path is pinned, either directly or via an ancestor folder.

        A conversation inside a pinned folder is effectively pinned even
        though its own path was never added to pinned_paths — the folder's
        pin covers it.  Intended for UI decisions, such as whether a
        context menu should offer "Pin" or "Unpin".

        Args:
            abs_path: Absolute filesystem path to check.

        Returns:
            True if the path is pinned directly or through an ancestor.
        """
        if self.is_path_pinned(abs_path):
            return True

        if not self.has_mindspace() or self._settings is None:
            return False

        return self._find_pinned_ancestor_dir(abs_path, self._settings.pinned_paths) is not None

    def folder_has_pinned_content(self, folder_abs_path: str) -> bool:
        """
        Return True if folder_abs_path is pinned, or has any pinned conversation inside it.

        Covers both the fully-pinned case (the folder's own path is in
        pinned_paths) and the partially-pinned case (some conversation
        nested inside it is individually pinned).  Used to decide whether
        the folder's natural-location copy should show a pin indicator.

        Args:
            folder_abs_path: Absolute path to the folder to check.

        Returns:
            True if the folder or anything inside it is pinned.
        """
        if not self.has_mindspace() or self._settings is None:
            return False

        folder_rel = self.get_mindspace_relative_path(folder_abs_path)
        if folder_rel is None:
            return False

        prefix = folder_rel + os.sep
        return any(p == folder_rel or p.startswith(prefix) for p in self._settings.pinned_paths)

    def set_path_pinned(self, abs_path: str, pinned: bool) -> None:
        """
        Pin or unpin an absolute path, persisting the change.

        Pinning a conversation that completes the set of pinned conversations
        in its folder — either because it's the only conversation there, or
        because every other conversation in that folder is already pinned —
        promotes the pin to the folder itself, and any now-redundant
        individual pins for its conversations are dropped.

        Unpinning is symmetric in both directions: unpinning a folder also
        unpins any conversations inside it that are still individually
        pinned, so a folder never leaves stale or contradictory pin state
        behind.  Unpinning a conversation that is only *effectively* pinned
        (its own path isn't in pinned_paths, but an ancestor folder's is) is
        the inverse of promotion: the ancestor's pin is replaced with
        individual pins for every other conversation it covered, so the rest
        of the folder stays pinned.  If there's nothing left to cover, the
        ancestor simply ends up unpinned — so unpinning every conversation
        in a folder one at a time ends with the folder itself unpinned.

        Args:
            abs_path: Absolute filesystem path to pin or unpin.
            pinned: True to pin, False to unpin.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        assert self._settings is not None

        target = abs_path
        if pinned and not os.path.isdir(abs_path):
            promoted = self._folder_pin_promotion(abs_path)
            if promoted is not None:
                target = promoted

        if pinned and not self.can_pin_path(target):
            return

        rel_path = self.get_mindspace_relative_path(target)
        if rel_path is None:
            return

        pinned_paths = self._settings.pinned_paths
        changed = False

        if pinned and rel_path not in pinned_paths:
            pinned_paths.append(rel_path)
            changed = True

        elif not pinned and rel_path in pinned_paths:
            pinned_paths.remove(rel_path)
            changed = True

        elif not pinned and not os.path.isdir(target) and self._depromote_ancestor(target, pinned_paths):
            changed = True

        if os.path.isdir(target) and self._prune_pinned_descendants(target, pinned_paths):
            changed = True

        if not changed:
            return

        self.update_settings(self._settings)

    def _find_pinned_ancestor_dir(self, abs_path: str, pinned_paths: list[str]) -> str | None:
        """
        Find the nearest ancestor folder of abs_path that is directly pinned.

        Args:
            abs_path: Absolute path to start searching upward from.
            pinned_paths: The pinned_paths list to search against.

        Returns:
            The pinned ancestor folder's absolute path, or None if there
            isn't one (including if abs_path itself is outside the mindspace).
        """
        pinned_set = set(pinned_paths)
        conversations_root = os.path.normpath(self.conversations_dir())
        parent_dir = os.path.normpath(os.path.dirname(abs_path))

        while True:
            parent_rel = self.get_mindspace_relative_path(parent_dir)
            if parent_rel is not None and parent_rel in pinned_set:
                return parent_dir

            if parent_dir == conversations_root:
                return None

            next_parent = os.path.normpath(os.path.dirname(parent_dir))
            if next_parent == parent_dir:
                return None

            parent_dir = next_parent

    def _collect_conv_files(self, dir_path: str) -> list[str]:
        """
        Recursively collect every .conv file under dir_path.

        Args:
            dir_path: Absolute path to the directory to walk.

        Returns:
            Absolute paths of all .conv files found, in no particular order.
        """
        found = []
        for root, _dirs, files in os.walk(dir_path):
            for filename in files:
                if filename.lower().endswith('.conv'):
                    found.append(os.path.normpath(os.path.join(root, filename)))

        return found

    def _depromote_ancestor(self, abs_path: str, pinned_paths: list[str]) -> bool:
        """
        Undo a pinned ancestor folder's coverage of abs_path, in place.

        Removes the pinned ancestor's own entry and replaces it with
        individual entries for every other conversation it covered (skipping
        delegate children, which can never be individually pinned).  If
        abs_path was the only conversation the ancestor covered, nothing is
        added back and the ancestor ends up simply unpinned.

        Args:
            abs_path: Absolute path to the conversation being unpinned.
            pinned_paths: The pinned_paths list to mutate in place.

        Returns:
            True if a pinned ancestor was found and de-promoted.
        """
        ancestor_dir = self._find_pinned_ancestor_dir(abs_path, pinned_paths)
        if ancestor_dir is None:
            return False

        ancestor_rel = self.get_mindspace_relative_path(ancestor_dir)
        if ancestor_rel is None:
            return False

        pinned_paths.remove(ancestor_rel)

        abs_path_norm = os.path.normpath(abs_path)
        for conv_path in self._collect_conv_files(ancestor_dir):
            if conv_path == abs_path_norm:
                continue

            if self._has_delegate_parent(conv_path):
                continue

            conv_rel = self.get_mindspace_relative_path(conv_path)
            if conv_rel is not None and conv_rel not in pinned_paths:
                pinned_paths.append(conv_rel)

        return True

    def _folder_pin_promotion(self, abs_path: str) -> str | None:
        """
        Return abs_path's parent folder if pinning abs_path completes that folder.

        "Completes" means every pinnable conversation in the folder —
        including abs_path itself — is now individually pinned, whether
        because it's the only one or because pinning this one was the last
        of several.  Delegate/fork children are ignored for this check since
        they can never be individually pinned in the first place.

        Args:
            abs_path: Absolute path to a conversation file about to be pinned.

        Returns:
            The parent folder's absolute path, or None if the parent is the
            conversations root, or some other conversation there isn't pinned.
        """
        assert self._settings is not None

        parent_dir = os.path.dirname(abs_path)
        if os.path.normpath(parent_dir) == os.path.normpath(self.conversations_dir()):
            return None

        try:
            entries = os.listdir(parent_dir)

        except OSError:
            return None

        pinned_set = set(self._settings.pinned_paths)
        abs_path_norm = os.path.normpath(abs_path)
        for entry in entries:
            if not entry.lower().endswith('.conv'):
                continue

            sibling = os.path.normpath(os.path.join(parent_dir, entry))
            if sibling == abs_path_norm:
                continue

            if self._has_delegate_parent(sibling):
                continue

            sibling_rel = self.get_mindspace_relative_path(sibling)
            if sibling_rel is None or sibling_rel not in pinned_set:
                return None

        return parent_dir

    def _prune_pinned_descendants(self, folder_abs_path: str, pinned_paths: list[str]) -> bool:
        """
        Remove pinned entries nested under folder_abs_path, in place.

        Leaves the folder's own entry (if present) untouched — only entries
        strictly inside it are considered redundant or stale.

        Args:
            folder_abs_path: Absolute path to the folder whose pinned or
                unpinned state is being applied.
            pinned_paths: The pinned_paths list to prune, mutated in place.

        Returns:
            True if any entries were removed.
        """
        folder_rel = self.get_mindspace_relative_path(folder_abs_path)
        if folder_rel is None:
            return False

        prefix = folder_rel + os.sep
        remaining = [p for p in pinned_paths if p == folder_rel or not p.startswith(prefix)]
        if len(remaining) == len(pinned_paths):
            return False

        pinned_paths[:] = remaining
        return True

    def migrate_pinned_path(self, old_abs_path: str, new_abs_path: str) -> None:
        """
        Update pinned entries after a path is renamed or moved.

        Rewrites any pinned entry equal to, or nested under, old_abs_path so
        it points at new_abs_path instead.  No-op if nothing pinned is affected.

        Args:
            old_abs_path: Absolute path before the rename/move.
            new_abs_path: Absolute path after the rename/move.
        """
        if not self.has_mindspace() or self._settings is None:
            return

        old_rel = self.get_mindspace_relative_path(old_abs_path)
        new_rel = self.get_mindspace_relative_path(new_abs_path)
        if old_rel is None or new_rel is None:
            return

        old_prefix = old_rel + os.sep
        changed = False
        updated_paths = []
        for pinned_path in self._settings.pinned_paths:
            if pinned_path == old_rel:
                updated_paths.append(new_rel)
                changed = True

            elif pinned_path.startswith(old_prefix):
                updated_paths.append(new_rel + os.sep + pinned_path[len(old_prefix):])
                changed = True

            else:
                updated_paths.append(pinned_path)

        if not changed:
            return

        self._settings.pinned_paths = updated_paths
        self.update_settings(self._settings)

    def unpin_path_tree(self, abs_path: str) -> None:
        """
        Remove pinned entries for a path and anything nested under it.

        Intended to be called after a file or folder is deleted, to avoid
        leaving stale entries in settings.  No-op if nothing pinned is affected.

        Args:
            abs_path: Absolute path that was deleted.
        """
        if not self.has_mindspace() or self._settings is None:
            return

        rel_path = self.get_mindspace_relative_path(abs_path)
        if rel_path is None:
            return

        prefix = rel_path + os.sep
        remaining = [
            p for p in self._settings.pinned_paths
            if p != rel_path and not p.startswith(prefix)
        ]
        if len(remaining) == len(self._settings.pinned_paths):
            return

        self._settings.pinned_paths = remaining
        self.update_settings(self._settings)

    def pinned_root_paths(self) -> list[str]:
        """
        Return absolute paths for pinned entries that should be lifted into
        the sidebar's Pinned section.

        A pinned entry nested under another pinned entry is not a root of
        its own — it comes along for free as part of its ancestor's subtree.
        Entries that no longer exist on disk are silently skipped.

        Returns:
            Absolute paths of pinned roots, shallowest first.
        """
        if not self.has_mindspace() or self._settings is None:
            return []

        candidates = sorted(self._settings.pinned_paths, key=lambda p: p.count(os.sep))
        roots: list[str] = []
        for rel_path in candidates:
            abs_path = self.get_absolute_path(rel_path)
            if not os.path.exists(abs_path):
                continue

            if any(
                abs_path == root or abs_path.startswith(root + os.sep)
                for root in roots
            ):
                continue

            roots.append(abs_path)

        return roots

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

    def get_interactions(self) -> list[MindspaceMessage]:
        """
        Return all interaction log messages.

        Returns:
            List of MindspaceMessage objects.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._interactions.get_messages()

    def save_mindspace_state(self, state: dict) -> None:
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

    def load_mindspace_state(self) -> dict:
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

    def _migrate_conversations(self) -> None:
        """
        Migrate conversations from the legacy mindspace-root location to .humbug/.

        If a 'conversations' directory exists at the mindspace root (the legacy
        location) and no conversations directory exists inside .humbug, move it.
        This is a one-time migration that runs when a mindspace is opened.
        """
        old_path = os.path.join(self._path, self.CONVERSATIONS_DIR)
        new_path = os.path.join(self._path, self.MINDSPACE_DIR, self.CONVERSATIONS_DIR)

        if not os.path.isdir(old_path):
            return

        if os.path.isdir(new_path):
            # Both exist — log and leave the old one in place rather than risk data loss.
            self._logger.warning(
                "Conversations directory exists at both '%s' and '%s'; "
                "leaving legacy directory in place",
                old_path, new_path
            )
            return

        try:
            shutil.move(old_path, new_path)
            self._migrate_session_paths(
                self.CONVERSATIONS_DIR,
                os.path.join(self.MINDSPACE_DIR, self.CONVERSATIONS_DIR))
            self._logger.info("Migrated conversations from '%s' to '%s'", old_path, new_path)

        except OSError as e:
            self._logger.error("Failed to migrate conversations from '%s' to '%s': %s",
                               old_path, new_path, str(e))

    def _migrate_session_paths(self, old_dir: str, new_dir: str) -> None:
        """
        Rewrite conversation paths in the session file after migration.

        Session paths are stored relative to the mindspace root.  This updates
        any path that starts with the old conversations prefix to use the new
        .humbug/conversations prefix.

        Args:
            old_dir: The old conversations directory name relative to mindspace root.
            new_dir: The new conversations directory path relative to mindspace root.
        """
        session_file = os.path.join(self._path, self.MINDSPACE_DIR, self.SESSION_FILE)
        if not os.path.exists(session_file):
            return

        try:
            with open(session_file, encoding='utf-8') as f:
                state = json.load(f)

            old_prefix = old_dir + os.sep
            new_prefix = new_dir + os.sep
            changed = False
            for tab_state in state.get('tabs', []):
                path = tab_state.get('path')
                if path and path.startswith(old_prefix):
                    tab_state['path'] = new_prefix + path[len(old_prefix):]
                    changed = True

            if changed:
                with open(session_file, 'w', encoding='utf-8') as f:
                    json.dump(state, f, indent=4)

        except (json.JSONDecodeError, OSError) as e:
            self._logger.warning("Failed to migrate session paths: %s", str(e))

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
