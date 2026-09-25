"""Live conversation DAG index for a mindspace conversations directory."""

import logging
import os

from PySide6.QtCore import QObject, Signal

from conversation_dag import ConversationDag, ConversationNode, ForkEdge
from desktop.file_watcher import FileWatcher


class ConversationSidebarIndex(QObject):
    """
    Maintains a live DAG index of all conversation files in a mindspace.

    The DAG model itself lives in the frontend-agnostic ConversationDag.  This
    class adds the desktop concerns on top: it watches the conversations
    directory with the FileWatcher and emits signals so that views can refresh.

    Scans the conversations directory on load, then stays current via the
    FileWatcher.  Emits changed() whenever the index is updated so that views
    can refresh.

    Two signals are emitted to allow consumers to distinguish between changes
    that affect the DAG structure (files added/removed, parent linkage changed)
    and changes that only affect conversation content (new messages appended to
    an existing conversation whose parentage is unchanged).

    The watcher is registered on the root conversations directory and every
    subdirectory found within it.  When subdirectories are added or removed
    the watched set is updated accordingly.
    """

    changed = Signal()
    structure_changed = Signal()

    def __init__(self, parent: QObject | None = None) -> None:
        """Initialize the index."""
        super().__init__(parent)
        self._logger = logging.getLogger("ConversationSidebarIndex")
        self._file_watcher = FileWatcher()

        self._conversations_dir: str = ""
        self._dag = ConversationDag("")

        # Set of directories currently registered with the file watcher
        self._watched_dirs: set[str] = set()

    def set_conversations_dir(self, conversations_dir: str) -> None:
        """
        Set the conversations directory and perform initial scan.

        Unregisters any previous directories from the file watcher, scans all
        existing .conv files, then registers all discovered directories for
        live updates.

        Args:
            conversations_dir: Absolute path to the conversations directory,
                or empty string to clear the index.
        """
        self._unwatch_all()
        self._conversations_dir = conversations_dir
        self._dag = ConversationDag(conversations_dir)

        if not conversations_dir:
            self.changed.emit()
            self.structure_changed.emit()
            return

        self._initial_scan()
        self.changed.emit()
        self.structure_changed.emit()

    def conversations_dir(self) -> str:
        """
        Get the current conversations directory.

        Returns:
            Absolute path to the conversations directory, or empty string if not set.
        """
        return self._conversations_dir

    def get_node(self, path: str) -> ConversationNode | None:
        """
        Get the index node for a conversation file.

        Args:
            path: Absolute path to the conversation file.

        Returns:
            ConversationNode if indexed, None otherwise.
        """
        return self._dag.get_node(path)

    def get_all_paths(self) -> list[str]:
        """
        Get all indexed conversation file paths.

        Returns:
            List of absolute paths.
        """
        return self._dag.get_all_paths()

    def get_children(self, path: str) -> list[str]:
        """
        Get all direct delegation children of a conversation.

        A child is a conversation whose parent_message_id appears in the
        given conversation's message_ids.

        Args:
            path: Absolute path to the parent conversation file.

        Returns:
            List of absolute paths of child conversations.
        """
        return self._dag.get_children(path)

    def get_parent_paths(self, path: str) -> list[str]:
        """
        Resolve all parent file paths for a delegated child conversation.

        A child may have multiple parents when it was spawned from a message
        that exists in more than one conversation (i.e. the parent was forked
        after the child was created).

        Args:
            path: Absolute path to the child conversation file.

        Returns:
            List of absolute paths of parent conversation files.  Empty if
            this is a root conversation or the parent cannot be resolved.
        """
        return self._dag.get_parent_paths(path)

    def get_fork_edges(self) -> list[ForkEdge]:
        """
        Get all detected fork relationships.

        Returns:
            List of ForkEdge objects describing fork relationships.
        """
        return self._dag.get_fork_edges()

    def get_roots(self) -> list[str]:
        """
        Get all root conversations (those with no delegation parent).

        Returns:
            List of absolute paths of root conversations.
        """
        return self._dag.get_roots()

    def compute_operation_scope(self, paths: set[str]) -> tuple[set[str], set[str]]:
        """
        Compute which conversations can safely be moved or deleted as part of an operation.

        Given a set of root paths being operated on, walks the DAG to find all
        descendants.  A descendant is included in the operation if every one of
        its parents is also included.  A descendant is excluded (left behind) if
        any of its parents falls outside the included set — typically because it
        is a shared child referenced by a conversation not being operated on.

        Args:
            paths: Absolute paths of the conversations being directly operated on.

        Returns:
            Tuple of (included, excluded) where:
            - included: paths that will be moved/deleted, including the originals
              and any exclusively-owned descendants.
            - excluded: descendant paths that must be left behind because they
              are shared with conversations outside the operation scope.
        """
        return self._dag.compute_operation_scope(paths)

    def _unwatch_all(self) -> None:
        """Unregister all currently watched directories from the file watcher."""
        for watched_dir in self._watched_dirs:
            self._file_watcher.unwatch_file(watched_dir, self._on_directory_changed)

        self._watched_dirs.clear()

    def _watch_dir(self, dir_path: str) -> None:
        """
        Register a directory with the file watcher if not already watched.

        Args:
            dir_path: Absolute path to the directory to watch.
        """
        norm = os.path.normpath(dir_path)
        if norm not in self._watched_dirs:
            self._file_watcher.watch_file(norm, self._on_directory_changed)
            self._watched_dirs.add(norm)

    def _unwatch_dir(self, dir_path: str) -> None:
        """
        Unregister a directory from the file watcher.

        Args:
            dir_path: Absolute path to the directory to stop watching.
        """
        norm = os.path.normpath(dir_path)
        if norm in self._watched_dirs:
            self._file_watcher.unwatch_file(norm, self._on_directory_changed)
            self._watched_dirs.discard(norm)

    def _initial_scan(self) -> None:
        """Scan all .conv files in the conversations directory on startup."""
        if not os.path.isdir(self._conversations_dir):
            return

        for dirpath, _dirnames, _filenames in os.walk(self._conversations_dir):
            self._watch_dir(dirpath)

    def _reconcile_watched_dirs(self, current_dirs: set[str]) -> bool:
        """
        Reconcile the set of watched directories against the current filesystem state.

        Registers newly discovered subdirectories and unregisters those that
        no longer exist.

        Args:
            current_dirs: Set of normalised directory paths currently on disk.

        Returns:
            True if the set of watched directories changed, False otherwise.
        """
        dirs_changed = False
        new_dirs = current_dirs - self._watched_dirs
        gone_dirs = self._watched_dirs - current_dirs

        for new_dir in new_dirs:
            self._watch_dir(new_dir)
            dirs_changed = True

        for gone_dir in gone_dirs:
            self._unwatch_dir(gone_dir)
            dirs_changed = True

        return dirs_changed

    def _on_directory_changed(self, _path: str) -> None:
        """
        Handle a change notification from the file watcher.

        The watcher fires when a watched directory's contents change.  We
        reconcile the full filesystem state against the index, including
        updating the set of watched subdirectories.

        Args:
            _path: The watched path that changed (unused — we reconcile all).
        """
        if not os.path.isdir(self._conversations_dir):
            self._unwatch_all()
            self._dag = ConversationDag("")
            self.changed.emit()
            self.structure_changed.emit()
            return

        current_paths: set[str] = set()
        current_dirs: set[str] = set()

        for dirpath, _dirnames, filenames in os.walk(self._conversations_dir):
            current_dirs.add(os.path.normpath(dirpath))
            for filename in filenames:
                if filename.lower().endswith('.conv'):
                    current_paths.add(os.path.normpath(os.path.join(dirpath, filename)))

        # Reconcile watched directories before processing file changes so that
        # newly added subdirectories are watched immediately.
        dirs_changed = self._reconcile_watched_dirs(current_dirs)

        indexed_paths = set(self._dag.get_all_paths())
        added = current_paths - indexed_paths
        removed = indexed_paths - current_paths
        possibly_modified = current_paths & indexed_paths

        structure_changed = bool(added or removed or dirs_changed)
        content_changed = False

        for path in removed:
            self._dag.remove_file(path)

        for path in added:
            self._dag.add_file(path)

        for path in possibly_modified:
            result = self._dag.refresh_file_if_changed(path)
            if result == "structure":
                structure_changed = True

            elif result == "content":
                content_changed = True

        if structure_changed:
            self._dag.recompute_fork_edges()
            self.structure_changed.emit()
            self.changed.emit()

        elif content_changed:
            self.changed.emit()
