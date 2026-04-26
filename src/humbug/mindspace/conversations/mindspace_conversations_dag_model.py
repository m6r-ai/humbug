"""Custom QAbstractItemModel backed by the conversation DAG index."""

import logging
import json
import os
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, overload

from PySide6.QtCore import QAbstractItemModel, QModelIndex, Qt, QPersistentModelIndex
from PySide6.QtCore import QObject, Signal
from PySide6.QtCore import QFileInfo
from humbug.mindspace.conversations.mindspace_conversations_index import MindspaceConversationsIndex
from humbug.mindspace.mindspace_tree_icon_provider import MindspaceTreeIconProvider
from humbug.user.user_file_sort_order import UserFileSortOrder
from humbug.user.user_manager import UserManager


@dataclass
class _DirNode:
    """A filesystem directory node in the tree."""
    path: str
    parent_key: int | None


@dataclass
class _ConvNode:
    """A conversation file node in the tree."""
    path: str
    parent_key: int | None
    is_shared: bool  # True when this file appears under more than one parent


class MindspaceConversationsDAGModel(QAbstractItemModel):
    """
    Tree model for the conversations panel, driven by MindspaceConversationsIndex.

    Tree structure:
    - Top level: filesystem subdirectories of the conversations root, plus
      root conversations (those with no delegation parent) in their filesystem
      directory.
    - Under each root conversation: its delegate children, recursively.
    - A child conversation that has multiple parents (because its parent was
      forked) appears under each parent.  is_shared is set on those nodes so
      the view can render a distinct icon.

    The model rebuilds completely whenever the index emits changed().  Because
    the index is already incremental, rebuilds are fast.
    """

    # Custom roles
    PathRole = Qt.ItemDataRole.UserRole + 1
    IsSharedRole = Qt.ItemDataRole.UserRole + 2
    IsDirRole = Qt.ItemDataRole.UserRole + 3

    about_to_rebuild = Signal()
    rebuilt = Signal()

    def __init__(
        self,
        index: MindspaceConversationsIndex,
        icon_provider: MindspaceTreeIconProvider,
        parent: Any = None
    ) -> None:
        """
        Initialise the model.

        Args:
            index: Live conversation DAG index to drive the model.
            icon_provider: Icon provider for folder and conversation icons.
            parent: Optional Qt parent object.
        """
        super().__init__(parent)
        self._logger = logging.getLogger("MindspaceConversationsDAGModel")
        self._index = index
        self._icon_provider = icon_provider
        self._index.changed.connect(self._on_index_changed)
        self._user_manager = UserManager()
        self._user_manager.settings_changed.connect(self._rebuild)
        self._timestamp_cache: Dict[str, float] = {}

        # Node registry: int key -> _DirNode | _ConvNode
        self._nodes: Dict[int, _DirNode | _ConvNode] = {}
        self._next_key: int = 0

        # Lookup tables rebuilt on each reset
        # (parent_key, child_path) -> child_key  for ConvNodes
        # parent_key -> [child_key, ...]  ordered child list
        self._children: Dict[int | None, List[int]] = {}

        self._build()

    def set_index(self, index: MindspaceConversationsIndex) -> None:
        """
        Replace the backing index.

        Args:
            index: New index to use.
        """
        self._index.changed.disconnect(self._on_index_changed)
        self._index = index
        self._index.changed.connect(self._on_index_changed)
        self._rebuild()

    def _alloc_key(self) -> int:
        key = self._next_key
        self._next_key += 1
        return key

    def _get_conversation_timestamp(self, path: str) -> float:
        """
        Get the creation timestamp for a conversation file.

        Uses the first message timestamp from the file content, falling back
        to filesystem timestamps.  Results are cached by path.

        Args:
            path: Absolute path to the .conv file.

        Returns:
            Timestamp as float, or 0.0 if unable to determine.
        """
        if path in self._timestamp_cache:
            return self._timestamp_cache[path]

        timestamp = 0.0
        try:
            stat_info = os.stat(path)
            # Try first message timestamp from content
            try:
                with open(path, 'r', encoding='utf-8') as f:
                    data = json.load(f)

                messages = data.get('conversation', []) if isinstance(data, dict) else []
                for msg in messages:
                    if isinstance(msg, dict) and 'timestamp' in msg:
                        ts = msg['timestamp']
                        if ts.endswith('Z'):
                            ts = ts[:-1] + '+00:00'

                        try:
                            timestamp = datetime.fromisoformat(ts).timestamp()
                            break
                        except (ValueError, AttributeError):
                            continue

            except Exception:
                pass

            if timestamp == 0.0:
                # Fall back to filesystem timestamps
                if hasattr(stat_info, 'st_birthtime') and stat_info.st_birthtime > 0:  # type: ignore[attr-defined]
                    timestamp = stat_info.st_birthtime  # type: ignore[attr-defined]
                elif hasattr(stat_info, 'st_ctime'):
                    timestamp = stat_info.st_ctime
                else:
                    timestamp = stat_info.st_mtime

        except OSError:
            pass

        self._timestamp_cache[path] = timestamp
        return timestamp

    def _sort_key(self, path: str, is_dir: bool) -> tuple:
        """
        Return a sort key for a filesystem entry.

        Respects the user's file_sort_order setting.  Within each group,
        sorts by creation time descending (newest first) by default.

        Args:
            path: Absolute path to the entry.
            is_dir: Whether the entry is a directory.

        Returns:
            Tuple suitable for use as a sort key.
        """
        sort_order = self._user_manager.settings().file_sort_order
        name = os.path.basename(path).lower()

        if sort_order == UserFileSortOrder.ALPHABETICAL:
            # No dirs-first grouping; sort by creation time descending, then name
            timestamp = self._get_conversation_timestamp(path) if not is_dir else 0.0
            return (-timestamp, name)

        # DIRECTORIES_FIRST: dirs before files, then creation time descending, then name
        dir_group = 0 if is_dir else 1
        timestamp = self._get_conversation_timestamp(path) if not is_dir else 0.0
        return (dir_group, -timestamp, name)

    def _build(self) -> None:
        """Build the node registry and child lists from the current index state."""
        self._nodes.clear()
        self._children.clear()
        self._next_key = 0
        self._timestamp_cache.clear()

        conversations_dir = self._index._conversations_dir
        if not conversations_dir or not os.path.isdir(conversations_dir):
            return

        # Determine which conversation files are shared (multiple parents)
        all_paths = self._index.get_all_paths()
        shared_paths = {
            path for path in all_paths
            if len(self._index.get_parent_paths(path)) > 1
        }

        # Build the directory tree first, then attach conversations
        # Add the root sentinel node displayed as "." as the first top-level
        # sibling.  Existing drag/drop and branch-drawing logic identifies it
        # by basename.  Everything else is a sibling, not a child of it.
        root_sentinel_path = os.path.join(conversations_dir, ".")
        root_key = self._alloc_key()
        self._nodes[root_key] = _DirNode(path=root_sentinel_path, parent_key=None)
        self._children[None] = [root_key]

        self._build_dir_subtree(conversations_dir, None, shared_paths)

    def _build_dir_subtree(
        self,
        dir_path: str,
        parent_key: int | None,
        shared_paths: set
    ) -> None:
        """
        Recursively build directory and root-conversation nodes.

        Args:
            dir_path: Absolute path of the directory to process.
            parent_key: Key of the parent node, or None for top level.
            shared_paths: Set of conversation paths that appear under multiple parents.
        """
        if parent_key not in self._children:
            self._children[parent_key] = []

        try:
            raw_entries = os.listdir(dir_path)
        except OSError as e:
            self._logger.warning("Cannot list directory %s: %s", dir_path, e)
            return

        # Build list of (path, is_dir) for all relevant entries
        entry_infos = []
        for entry in raw_entries:
            entry_path = os.path.normpath(os.path.join(dir_path, entry))
            is_dir = os.path.isdir(entry_path)
            if is_dir:
                entry_infos.append((entry_path, True))
            elif entry.lower().endswith('.conv'):
                entry_infos.append((entry_path, False))

        # Sort using user-preference-aware key
        entry_infos.sort(key=lambda e: self._sort_key(e[0], e[1]))

        for entry_path, is_dir in entry_infos:
            if is_dir:
                dir_key = self._alloc_key()
                self._nodes[dir_key] = _DirNode(path=entry_path, parent_key=parent_key)
                self._children[parent_key].append(dir_key)
                self._build_dir_subtree(entry_path, dir_key, shared_paths)
            else:
                node = self._index.get_node(entry_path)
                if node is None:
                    continue

                if self._index.get_parent_paths(entry_path):
                    # Delegate child — attached under its parent conv node, not here
                    continue

                conv_key = self._alloc_key()
                is_shared = entry_path in shared_paths
                self._nodes[conv_key] = _ConvNode(
                    path=entry_path,
                    parent_key=parent_key,
                    is_shared=is_shared
                )
                self._children[parent_key].append(conv_key)
                self._build_conv_children(conv_key, entry_path, shared_paths)

    def _build_conv_children(
        self,
        parent_conv_key: int,
        parent_conv_path: str,
        shared_paths: set
    ) -> None:
        """
        Recursively attach delegate children under a conversation node.

        Args:
            parent_conv_key: Key of the parent conversation node.
            parent_conv_path: Absolute path of the parent conversation file.
            shared_paths: Set of conversation paths that appear under multiple parents.
        """
        if parent_conv_key not in self._children:
            self._children[parent_conv_key] = []

        children = sorted(
            self._index.get_children(parent_conv_path),
            key=lambda p: self._sort_key(p, False)
        )
        for child_path in children:
            child_key = self._alloc_key()
            is_shared = child_path in shared_paths
            self._nodes[child_key] = _ConvNode(
                path=child_path,
                parent_key=parent_conv_key,
                is_shared=is_shared
            )
            self._children[parent_conv_key].append(child_key)
            self._build_conv_children(child_key, child_path, shared_paths)

    def _rebuild(self) -> None:
        """Rebuild the model, notifying views."""
        self.about_to_rebuild.emit()
        self.beginResetModel()
        self._build()
        self.endResetModel()
        self.rebuilt.emit()

    def _on_index_changed(self) -> None:
        """Handle index change notification."""
        self._rebuild()

    def index(
        self,
        row: int,
        column: int,
        parent: QModelIndex | QPersistentModelIndex = QModelIndex()
    ) -> QModelIndex:
        """Return the model index for the given row, column, and parent."""
        if not self.hasIndex(row, column, parent):
            return QModelIndex()

        parent_key = None if not parent.isValid() else parent.internalId()
        children = self._children.get(parent_key, [])

        if row >= len(children):
            return QModelIndex()

        child_key = children[row]
        return self.createIndex(row, column, child_key)

    @overload
    def parent(self, index: QModelIndex | QPersistentModelIndex) -> QModelIndex: ...

    @overload
    def parent(self) -> QObject: ...

    def parent(self, index: QModelIndex | QPersistentModelIndex | None = None) -> QObject | QModelIndex:
        """Return the parent of the given model index, or the QObject parent if no index given."""
        if index is None:
            return super().parent()

        if not index.isValid():
            return QModelIndex()

        node_key = index.internalId()
        node = self._nodes.get(node_key)
        if node is None or node.parent_key is None:
            return QModelIndex()

        parent_node = self._nodes.get(node.parent_key)
        if parent_node is None:
            return QModelIndex()

        # Find the row of the parent within its own parent's children
        grandparent_key = parent_node.parent_key
        siblings = self._children.get(grandparent_key, [])
        try:
            row = siblings.index(node.parent_key)
        except ValueError:
            return QModelIndex()

        return self.createIndex(row, 0, node.parent_key)

    def rowCount(self, parent: QModelIndex | QPersistentModelIndex = QModelIndex()) -> int:
        """Return the number of rows under the given parent."""
        if parent.column() > 0:
            return 0

        parent_key = None if not parent.isValid() else parent.internalId()
        return len(self._children.get(parent_key, []))

    def columnCount(self, _parent: QModelIndex | QPersistentModelIndex = QModelIndex()) -> int:
        """Return the number of columns (always 1)."""
        return 1

    def data(
        self,
        index: QModelIndex | QPersistentModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole
    ) -> Any:
        """Return data for the given index and role."""
        if not index.isValid():
            return None

        node_key = index.internalId()
        node = self._nodes.get(node_key)
        if node is None:
            return None

        if role == Qt.ItemDataRole.DisplayRole:
            name = os.path.basename(node.path)
            if isinstance(node, _ConvNode) and name.lower().endswith('.conv'):
                name = name[:-5]

            return name

        if role == Qt.ItemDataRole.DecorationRole:
            file_info = QFileInfo(node.path)
            return self._icon_provider.icon(file_info)

        if role == self.PathRole:
            return node.path

        if role == self.IsSharedRole:
            return isinstance(node, _ConvNode) and node.is_shared

        if role == self.IsDirRole:
            return isinstance(node, _DirNode)

        return None

    def flags(self, index: QModelIndex | QPersistentModelIndex) -> Qt.ItemFlag:
        """Return item flags."""
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags

        return Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEditable

    def path_for_index(self, index: QModelIndex | QPersistentModelIndex) -> str | None:
        """
        Get the filesystem path for a model index.

        Args:
            index: Model index to look up.

        Returns:
            Absolute filesystem path, or None if index is invalid.
        """
        if not index.isValid():
            return None

        node = self._nodes.get(index.internalId())
        return node.path if node else None

    def index_for_path(self, path: str) -> QModelIndex:
        """
        Find the first model index for a given filesystem path.

        If the path appears multiple times (shared child), returns the first
        occurrence found.

        Args:
            path: Absolute filesystem path to find.

        Returns:
            Model index, or invalid QModelIndex if not found.
        """
        norm = os.path.normpath(path)
        for key, node in self._nodes.items():
            if os.path.normpath(node.path) == norm:
                parent_key = node.parent_key
                siblings = self._children.get(parent_key, [])
                try:
                    row = siblings.index(key)
                    parent_index = QModelIndex()
                    if parent_key is not None:
                        parent_index = self.index_for_key(parent_key)

                    return self.createIndex(row, 0, key)

                except ValueError:
                    continue

        return QModelIndex()

    def index_for_key(self, key: int) -> QModelIndex:
        """
        Get the model index for a node key.

        Args:
            key: Internal node key.

        Returns:
            Model index, or invalid QModelIndex if key not found.
        """
        node = self._nodes.get(key)
        if node is None:
            return QModelIndex()

        siblings = self._children.get(node.parent_key, [])
        try:
            row = siblings.index(key)
            return self.createIndex(row, 0, key)
        except ValueError:
            return QModelIndex()
