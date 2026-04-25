"""Conversation DAG index for a mindspace conversations directory."""

import json
import logging
import os
from dataclasses import dataclass
from typing import Dict, List, Set

from PySide6.QtCore import QObject, Signal

from humbug.mindspace.mindspace_file_watcher import MindspaceFileWatcher


@dataclass
class ConversationNode:
    """All index data for a single conversation file."""
    path: str
    message_ids: List[str]
    parent_message_id: str | None
    parent_tool_call_id: str | None


@dataclass
class ForkEdge:
    """A fork relationship between two conversation files."""
    path_a: str
    path_b: str
    fork_message_id: str


class MindspaceConversationsIndex(QObject):
    """
    Maintains a live DAG index of all conversation files in a mindspace.

    Scans the conversations directory on load, then stays current via the
    MindspaceFileWatcher.  Emits changed() whenever the index is updated so
    that views can refresh.

    Two kinds of edges are tracked:

    - Delegation edges: recorded explicitly in each child file's metadata
      (parent_message_id + parent_tool_call_id).
    - Fork edges: inferred by finding message IDs shared across multiple files.
      The last shared message ID between any two files is the fork point.

    Both are recomputed incrementally when individual files change.

    The watcher is registered on the root conversations directory and every
    subdirectory found within it.  When subdirectories are added or removed
    the watched set is updated accordingly.
    """

    changed = Signal()

    def __init__(self, parent: QObject | None = None) -> None:
        """Initialize the index."""
        super().__init__(parent)
        self._logger = logging.getLogger("MindspaceConversationsIndex")
        self._file_watcher = MindspaceFileWatcher()

        self._conversations_dir: str = ""

        # path -> ConversationNode
        self._nodes: Dict[str, ConversationNode] = {}

        # message_id -> set of paths that contain it
        self._message_id_index: Dict[str, Set[str]] = {}

        # Computed fork edges (rebuilt incrementally)
        self._fork_edges: List[ForkEdge] = []

        # Set of directories currently registered with the file watcher
        self._watched_dirs: Set[str] = set()

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
        self._clear()
        self._conversations_dir = conversations_dir

        if not conversations_dir:
            return

        self._initial_scan()
        self.changed.emit()

    def get_node(self, path: str) -> ConversationNode | None:
        """
        Get the index node for a conversation file.

        Args:
            path: Absolute path to the conversation file.

        Returns:
            ConversationNode if indexed, None otherwise.
        """
        return self._nodes.get(os.path.normpath(path))

    def get_all_paths(self) -> List[str]:
        """
        Get all indexed conversation file paths.

        Returns:
            List of absolute paths.
        """
        return list(self._nodes.keys())

    def get_children(self, path: str) -> List[str]:
        """
        Get all direct delegation children of a conversation.

        A child is a conversation whose parent_message_id appears in the
        given conversation's message_ids.

        Args:
            path: Absolute path to the parent conversation file.

        Returns:
            List of absolute paths of child conversations.
        """
        node = self._nodes.get(os.path.normpath(path))
        if node is None:
            return []

        message_id_set = set(node.message_ids)
        children = []
        for candidate_path, candidate_node in self._nodes.items():
            if candidate_node.parent_message_id in message_id_set:
                children.append(candidate_path)

        return children

    def get_parent_paths(self, path: str) -> List[str]:
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
        node = self._nodes.get(os.path.normpath(path))
        if node is None or node.parent_message_id is None:
            return []

        paths = self._message_id_index.get(node.parent_message_id, set())
        norm_path = os.path.normpath(path)
        return [p for p in paths if p != norm_path]

    def get_fork_edges(self) -> List[ForkEdge]:
        """
        Get all detected fork relationships.

        Returns:
            List of ForkEdge objects describing fork relationships.
        """
        return list(self._fork_edges)

    def get_roots(self) -> List[str]:
        """
        Get all root conversations (those with no delegation parent).

        Returns:
            List of absolute paths of root conversations.
        """
        return [
            path for path, node in self._nodes.items()
            if not self.get_parent_paths(path)
        ]

    def compute_operation_scope(self, paths: Set[str]) -> tuple[Set[str], Set[str]]:
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
        included: Set[str] = set()
        excluded: Set[str] = set()

        # Normalise the seed paths
        queue: List[str] = [os.path.normpath(p) for p in paths]
        for p in queue:
            included.add(p)

        # BFS over descendants
        visited: Set[str] = set(included)
        while queue:
            current = queue.pop(0)
            for child in self.get_children(current):
                child = os.path.normpath(child)
                if child in visited:
                    continue

                visited.add(child)

                # Check whether all parents of this child are within included
                parent_paths = self.get_parent_paths(child)
                all_parents_included = all(
                    os.path.normpath(p) in included for p in parent_paths
                )

                if all_parents_included:
                    included.add(child)
                    queue.append(child)

                else:
                    excluded.add(child)

        return included, excluded

    def _clear(self) -> None:
        """Clear all index state."""
        self._nodes.clear()
        self._message_id_index.clear()
        self._fork_edges.clear()

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

        for dirpath, _dirnames, filenames in os.walk(self._conversations_dir):
            self._watch_dir(dirpath)
            for filename in filenames:
                if filename.lower().endswith('.conv'):
                    self._add_file(os.path.join(dirpath, filename))

        self._recompute_fork_edges()

    def _reconcile_watched_dirs(self, current_dirs: Set[str]) -> bool:
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
            self._clear()
            self.changed.emit()
            return

        current_paths: Set[str] = set()
        current_dirs: Set[str] = set()

        for dirpath, _dirnames, filenames in os.walk(self._conversations_dir):
            current_dirs.add(os.path.normpath(dirpath))
            for filename in filenames:
                if filename.lower().endswith('.conv'):
                    current_paths.add(os.path.normpath(os.path.join(dirpath, filename)))

        # Reconcile watched directories before processing file changes so that
        # newly added subdirectories are watched immediately.
        dirs_changed = self._reconcile_watched_dirs(current_dirs)

        indexed_paths = set(self._nodes.keys())
        added = current_paths - indexed_paths
        removed = indexed_paths - current_paths
        possibly_modified = current_paths & indexed_paths

        changed = bool(added or removed or dirs_changed)

        for path in removed:
            self._remove_file(path)

        for path in added:
            self._add_file(path)

        for path in possibly_modified:
            if self._refresh_file_if_changed(path):
                changed = True

        if changed:
            self._recompute_fork_edges()
            self.changed.emit()

    def _read_conv_file(self, path: str) -> tuple[List[str], str | None, str | None] | None:
        """
        Read only the fields needed for indexing from a .conv file.

        Extracts message IDs and parent metadata without constructing full
        AIMessage objects.

        Args:
            path: Absolute path to the .conv file.

        Returns:
            Tuple of (message_ids, parent_message_id, parent_tool_call_id),
            or None if the file cannot be read or is not a valid conversation.
        """
        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)

            if not isinstance(data, dict):
                return None

            conversation = data.get('conversation')
            if not isinstance(conversation, list):
                return None

            message_ids = []
            for msg in conversation:
                if isinstance(msg, dict):
                    msg_id = msg.get('id')
                    if isinstance(msg_id, str):
                        message_ids.append(msg_id)

            parent_message_id = None
            parent_tool_call_id = None
            metadata = data.get('metadata')
            if isinstance(metadata, dict):
                parent = metadata.get('parent')
                if isinstance(parent, dict):
                    mid = parent.get('message_id')
                    tid = parent.get('tool_call_id')
                    if isinstance(mid, str):
                        parent_message_id = mid

                    if isinstance(tid, str):
                        parent_tool_call_id = tid

            return message_ids, parent_message_id, parent_tool_call_id

        except (OSError, json.JSONDecodeError, ValueError) as e:
            self._logger.debug("Could not index conversation file %s: %s", path, str(e))
            return None

    def _add_file(self, path: str) -> None:
        """
        Add a single conversation file to the index.

        Args:
            path: Absolute path to the .conv file.
        """
        norm_path = os.path.normpath(path)
        result = self._read_conv_file(norm_path)
        if result is None:
            return

        message_ids, parent_message_id, parent_tool_call_id = result

        node = ConversationNode(
            path=norm_path,
            message_ids=message_ids,
            parent_message_id=parent_message_id,
            parent_tool_call_id=parent_tool_call_id
        )
        self._nodes[norm_path] = node

        for msg_id in message_ids:
            if msg_id not in self._message_id_index:
                self._message_id_index[msg_id] = set()

            self._message_id_index[msg_id].add(norm_path)

    def _remove_file(self, path: str) -> None:
        """
        Remove a conversation file from the index.

        Args:
            path: Normalised absolute path to the .conv file.
        """
        node = self._nodes.pop(path, None)
        if node is None:
            return

        for msg_id in node.message_ids:
            paths = self._message_id_index.get(msg_id)
            if paths:
                paths.discard(path)
                if not paths:
                    del self._message_id_index[msg_id]

    def _refresh_file_if_changed(self, path: str) -> bool:
        """
        Re-index a file if its content has changed.

        Args:
            path: Normalised absolute path to the .conv file.

        Returns:
            True if the file was re-indexed, False if unchanged.
        """
        result = self._read_conv_file(path)
        if result is None:
            return False

        message_ids, parent_message_id, parent_tool_call_id = result
        existing = self._nodes.get(path)
        if existing and existing.message_ids == message_ids \
                and existing.parent_message_id == parent_message_id \
                and existing.parent_tool_call_id == parent_tool_call_id:
            return False

        self._remove_file(path)
        self._add_file(path)
        return True

    def _recompute_fork_edges(self) -> None:
        """
        Recompute all fork edges from the current message ID index.

        A fork edge exists between two files A and B when they share at least
        one message ID.  The fork point is the last message ID (by position in
        A's message list) that both files share.
        """
        self._fork_edges = []

        # Find all message IDs shared by more than one file
        shared: Dict[str, Set[str]] = {
            msg_id: paths
            for msg_id, paths in self._message_id_index.items()
            if len(paths) > 1
        }

        if not shared:
            return

        # For each pair of files that share at least one message ID, find the
        # last shared message ID (the fork point).
        processed_pairs: Set[frozenset] = set()

        for paths in shared.values():
            path_list = sorted(paths)  # Deterministic ordering
            for i in range(len(path_list)):
                for j in range(i + 1, len(path_list)):
                    pair = frozenset([path_list[i], path_list[j]])
                    if pair in processed_pairs:
                        continue

                    processed_pairs.add(pair)
                    fork_msg_id = self._find_fork_point(path_list[i], path_list[j])
                    if fork_msg_id:
                        self._fork_edges.append(ForkEdge(
                            path_a=path_list[i],
                            path_b=path_list[j],
                            fork_message_id=fork_msg_id
                        ))

    def _find_fork_point(self, path_a: str, path_b: str) -> str | None:
        """
        Find the last shared message ID between two conversation files.

        Args:
            path_a: Normalised path to first conversation file.
            path_b: Normalised path to second conversation file.

        Returns:
            The last shared message ID, or None if no shared messages.
        """
        node_a = self._nodes.get(path_a)
        node_b = self._nodes.get(path_b)
        if node_a is None or node_b is None:
            return None

        ids_b = set(node_b.message_ids)
        last_shared = None
        for msg_id in node_a.message_ids:
            if msg_id in ids_b:
                last_shared = msg_id

        return last_shared
