"""Custom QAbstractItemModel backed by the conversation DAG index."""

import logging
import json
import os
from dataclasses import dataclass
from datetime import datetime
from typing import Any, overload

from PySide6.QtCore import QAbstractItemModel, QModelIndex, Qt, QPersistentModelIndex
from PySide6.QtCore import QObject, Signal
from PySide6.QtCore import QFileInfo

from desktop.conversation_sidebar.conversation_sidebar_index import ConversationSidebarIndex
from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.user.user_file_sort_order import UserFileSortOrder
from desktop.user.user_manager import UserManager


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


@dataclass
class _PinnedSectionNode:
    """Synthetic header node for the Pinned section — not a real filesystem entry."""
    parent_key: int | None
    path: str = ""


@dataclass
class _PinnedGroupNode:
    """
    Virtual folder label inside the Pinned section.

    Not a real move — the underlying folder (node.path) stays where it is,
    with its other, unpinned contents.  Exists purely so a pinned
    conversation still shows which folder it came from.  Grouped by real
    folder path so multiple pinned conversations from the same folder share
    one label.
    """
    path: str
    parent_key: int | None


class ConversationSidebarDAGModel(QAbstractItemModel):
    """
    Tree model for the conversations panel, driven by ConversationSidebarIndex.

    Tree structure:
    - If anything is pinned, a synthetic Pinned section is the first entry at
      the top level, containing each pinned root (folder or root conversation)
      and its full subtree. A fully pinned folder is removed from its natural
      location; a folder with only some chats pinned remains there.
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
        index: ConversationSidebarIndex,
        icon_provider: SidebarTreeIconProvider,
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
        self._logger = logging.getLogger("ConversationSidebarDAGModel")
        self._index = index
        self._icon_provider = icon_provider
        self._index.structure_changed.connect(self._on_index_changed)
        self._user_manager = UserManager()
        self._user_manager.settings_changed.connect(self._rebuild)
        self._mindspace_manager = MindspaceManager()
        self._mindspace_manager.settings_changed.connect(self._rebuild)
        self._language_manager = LanguageManager()
        self._timestamp_cache: dict[str, float] = {}

        # Node registry: int key -> _DirNode | _ConvNode | _PinnedSectionNode | _PinnedGroupNode
        self._nodes: dict[int, _DirNode | _ConvNode | _PinnedSectionNode | _PinnedGroupNode] = {}
        self._next_key: int = 0

        # Lookup tables rebuilt on each reset
        # (parent_key, child_path) -> child_key  for ConvNodes
        # parent_key -> [child_key, ...]  ordered child list
        self._children: dict[int | None, list[int]] = {}

        self._build()

    def set_index(self, index: ConversationSidebarIndex) -> None:
        """
        Replace the backing index.

        Args:
            index: New index to use.
        """
        self._index.structure_changed.disconnect(self._on_index_changed)
        self._index = index
        self._index.structure_changed.connect(self._on_index_changed)
        self._rebuild()

    def _alloc_key(self) -> int:
        """Allocate and return the next unique key for a DAG node."""
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

        conversations_dir = self._index.conversations_dir()
        if not conversations_dir or not os.path.isdir(conversations_dir):
            return

        # Determine which conversation files are shared (multiple parents)
        all_paths = self._index.get_all_paths()
        shared_paths = {
            path for path in all_paths
            if len(self._index.get_parent_paths(path)) > 1
        }

        self._children[None] = []

        pinned_roots = self._mindspace_manager.pinned_root_paths()
        if pinned_roots:
            pinned_key = self._alloc_key()
            self._nodes[pinned_key] = _PinnedSectionNode(parent_key=None)
            self._children[None].append(pinned_key)
            self._children[pinned_key] = []
            self._attach_pinned_roots(pinned_key, pinned_roots, conversations_dir, shared_paths)

        self._build_dir_subtree(
            conversations_dir, None, shared_paths, exclude=set(pinned_roots), show_pinned=False
        )

    def _attach_pinned_roots(
        self,
        pinned_key: int,
        pinned_roots: list[str],
        conversations_dir: str,
        shared_paths: set
    ) -> None:
        """
        Attach pinned roots under the Pinned section, grouping stray conversations by folder.

        A pinned folder or a pinned conversation directly in the conversations
        root attaches directly.  A pinned conversation nested inside some
        other folder (one that was not itself promoted to a pinned root,
        because it still has other conversations) is grouped under a virtual
        _PinnedGroupNode labelled with that folder's name, so it stays clear
        which folder it came from without moving the folder's other contents.

        Args:
            pinned_key: Key of the Pinned section header node.
            pinned_roots: Absolute paths returned by pinned_root_paths().
            conversations_dir: Absolute path to the conversations root.
            shared_paths: Set of conversation paths that appear under multiple parents.
        """
        conversations_root_norm = os.path.normpath(conversations_dir)
        groups: dict[str, list[str]] = {}
        direct: list[tuple[str, bool]] = []

        for root_path in pinned_roots:
            is_dir = os.path.isdir(root_path)
            if is_dir:
                direct.append((root_path, True))
                continue

            parent_dir = os.path.normpath(os.path.dirname(root_path))
            if parent_dir == conversations_root_norm:
                direct.append((root_path, False))

            else:
                groups.setdefault(parent_dir, []).append(root_path)

        sort_targets = direct + [(folder, True) for folder in groups]
        sort_targets.sort(key=lambda e: self._sort_key(e[0], e[1]))

        for path, is_dir in sort_targets:
            if path in groups:
                group_key = self._alloc_key()
                self._nodes[group_key] = _PinnedGroupNode(path=path, parent_key=pinned_key)
                self._children[pinned_key].append(group_key)
                self._children[group_key] = []

                grouped_convs = sorted(groups[path], key=lambda p: self._sort_key(p, False))
                for conv_path in grouped_convs:
                    self._attach_entry(conv_path, False, group_key, shared_paths, exclude=set())

            else:
                self._attach_entry(path, is_dir, pinned_key, shared_paths, exclude=set())

    def _attach_entry(
        self,
        entry_path: str,
        is_dir: bool,
        parent_key: int | None,
        shared_paths: set,
        exclude: set,
        show_pinned: bool = True
    ) -> None:
        """
        Create a node for entry_path under parent_key and recurse into its subtree.

        Args:
            entry_path: Absolute path of the entry to attach.
            is_dir: Whether the entry is a directory.
            parent_key: Key of the parent node to attach under.
            shared_paths: Set of conversation paths that appear under multiple parents.
            exclude: Paths to skip while descending.
            show_pinned: Whether individually pinned conversations belong in this copy.
        """
        if is_dir:
            dir_key = self._alloc_key()
            self._nodes[dir_key] = _DirNode(path=entry_path, parent_key=parent_key)
            self._children[parent_key].append(dir_key)
            self._build_dir_subtree(entry_path, dir_key, shared_paths, exclude, show_pinned)
            return

        node = self._index.get_node(entry_path)
        if node is None:
            return

        conv_key = self._alloc_key()
        is_shared = entry_path in shared_paths
        self._nodes[conv_key] = _ConvNode(
            path=entry_path,
            parent_key=parent_key,
            is_shared=is_shared
        )
        self._children[parent_key].append(conv_key)
        self._build_conv_children(conv_key, entry_path, shared_paths)

    def _build_dir_subtree(
        self,
        dir_path: str,
        parent_key: int | None,
        shared_paths: set,
        exclude: set,
        show_pinned: bool = True
    ) -> None:
        """
        Recursively build directory and root-conversation nodes.

        Args:
            dir_path: Absolute path of the directory to process.
            parent_key: Key of the parent node, or None for top level.
            shared_paths: Set of conversation paths that appear under multiple parents.
            exclude: Paths to skip while building a pinned subtree.
            show_pinned: Whether individually pinned conversations belong in this copy.
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
            if entry_path in exclude:
                continue

            is_dir = os.path.isdir(entry_path)
            if is_dir:
                entry_infos.append((entry_path, True))

            elif (
                entry.lower().endswith('.conv')
                and (show_pinned or not self._mindspace_manager.is_path_pinned(entry_path))
            ):
                entry_infos.append((entry_path, False))

        # Sort using user-preference-aware key
        entry_infos.sort(key=lambda e: self._sort_key(e[0], e[1]))

        for entry_path, is_dir in entry_infos:
            if not is_dir and self._index.get_parent_paths(entry_path):
                # Delegate child — attached under its parent conv node, not here
                continue

            self._attach_entry(entry_path, is_dir, parent_key, shared_paths, exclude, show_pinned)

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
            if isinstance(node, _PinnedSectionNode):
                return self._language_manager.strings().pinned_section

            name = os.path.basename(node.path)
            if isinstance(node, _ConvNode) and name.lower().endswith('.conv'):
                name = name[:-5]

            return name

        if role == Qt.ItemDataRole.DecorationRole:
            if isinstance(node, _PinnedSectionNode):
                return self._icon_provider.pinned_section_icon()

            if (
                isinstance(node, _DirNode)
                and not self._node_is_under_pinned_section(node)
                and self._mindspace_manager.folder_has_pinned_content(node.path)
            ):
                # Natural-location copy of a folder that's pinned, in full or
                # in part — a marker so it's clear part of it lives under
                # Pinned too.  The Pinned-section copy keeps its normal
                # open/closed folder icon; being under "Pinned" already says so.
                return self._icon_provider.pinned_section_icon()

            file_info = QFileInfo(node.path)
            return self._icon_provider.icon(file_info)

        if role == self.PathRole:
            if isinstance(node, (_PinnedSectionNode, _PinnedGroupNode)):
                return None

            return node.path

        if role == self.IsSharedRole:
            return isinstance(node, _ConvNode) and node.is_shared

        if role == self.IsDirRole:
            return isinstance(node, (_DirNode, _PinnedGroupNode))

        return None

    def flags(self, index: QModelIndex | QPersistentModelIndex) -> Qt.ItemFlag:
        """Return item flags."""
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags

        node = self._nodes.get(index.internalId())
        if isinstance(node, (_PinnedSectionNode, _PinnedGroupNode)):
            # Group header / folder label — not selectable or editable, just an
            # expand/collapse target.
            return Qt.ItemFlag.ItemIsEnabled

        return Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEditable

    def path_for_index(self, index: QModelIndex | QPersistentModelIndex) -> str | None:
        """
        Get the filesystem path for a model index.

        Args:
            index: Model index to look up.

        Returns:
            Absolute filesystem path, or None if index is invalid, is the
            synthetic Pinned section header, or is a virtual folder-group label.
        """
        if not index.isValid():
            return None

        node = self._nodes.get(index.internalId())
        if node is None or isinstance(node, (_PinnedSectionNode, _PinnedGroupNode)):
            return None

        return node.path

    def group_folder_path(self, index: QModelIndex | QPersistentModelIndex) -> str | None:
        """
        Get the real folder path behind a pinned folder-group label.

        Deliberately separate from path_for_index(), which returns None for a
        group label to keep it non-interactive (no rename/delete/drag); this
        accessor exists only so the delegate can still show the correct
        open/closed folder icon for it.

        Args:
            index: Model index to look up.

        Returns:
            The real folder's absolute path if index is a pinned group label,
            else None.
        """
        if not index.isValid():
            return None

        node = self._nodes.get(index.internalId())
        return node.path if isinstance(node, _PinnedGroupNode) else None

    def _node_is_under_pinned_section(self, node: _DirNode | _ConvNode | _PinnedSectionNode | _PinnedGroupNode) -> bool:
        """
        Walk node's parent chain to see if it sits inside the Pinned section.

        Args:
            node: Node to check.

        Returns:
            True if an ancestor of node is the Pinned section header or a
            folder-group label.
        """
        parent_key = node.parent_key
        while parent_key is not None:
            parent_node = self._nodes.get(parent_key)
            if parent_node is None:
                return False

            if isinstance(parent_node, (_PinnedSectionNode, _PinnedGroupNode)):
                return True

            parent_key = parent_node.parent_key

        return False

    def is_pinned_section_copy(self, index: QModelIndex | QPersistentModelIndex) -> bool:
        """
        Return True if index is the Pinned-section copy of a node that may be duplicated.

        A pinned folder appears both under Pinned and at its natural
        location, sharing the same real path — this distinguishes which
        instance index refers to, since path_for_index() can't (both
        instances return the same path).

        Args:
            index: Model index to check.

        Returns:
            True if index is inside the Pinned section.
        """
        if not index.isValid():
            return False

        node = self._nodes.get(index.internalId())
        return node is not None and self._node_is_under_pinned_section(node)

    def natural_index_for_path(self, path: str) -> QModelIndex:
        """
        Find the model index for path at its natural (non-Pinned-section) location.

        Args:
            path: Absolute filesystem path to find.

        Returns:
            Model index, or invalid QModelIndex if not found there.
        """
        return self._index_for_path_in_section(path, want_pinned_copy=False)

    def pinned_index_for_path(self, path: str) -> QModelIndex:
        """
        Find the model index for path inside the Pinned section, if it has a copy there.

        Args:
            path: Absolute filesystem path to find.

        Returns:
            Model index, or invalid QModelIndex if not found there.
        """
        return self._index_for_path_in_section(path, want_pinned_copy=True)

    def _index_for_path_in_section(self, path: str, want_pinned_copy: bool) -> QModelIndex:
        """
        Find the model index for path in a specific copy (Pinned or natural).

        Args:
            path: Absolute filesystem path to find.
            want_pinned_copy: True to look inside the Pinned section, False
                to look at the natural (top-level) location.

        Returns:
            Model index, or invalid QModelIndex if no matching copy is found.
        """
        norm = os.path.normpath(path)
        for key, node in self._nodes.items():
            if isinstance(node, (_PinnedSectionNode, _PinnedGroupNode)):
                continue

            if os.path.normpath(node.path) != norm:
                continue

            if self._node_is_under_pinned_section(node) != want_pinned_copy:
                continue

            parent_key = node.parent_key
            siblings = self._children.get(parent_key, [])
            try:
                row = siblings.index(key)
                return self.createIndex(row, 0, key)

            except ValueError:
                continue

        return QModelIndex()

    def pinned_section_index(self) -> QModelIndex:
        """
        Get the model index of the Pinned section header, if present.

        Returns:
            The header's index, or an invalid QModelIndex if nothing is pinned.
        """
        for key in self._children.get(None, []):
            if isinstance(self._nodes.get(key), _PinnedSectionNode):
                return self.index_for_key(key)

        return QModelIndex()

    def is_first_row_after_pinned_section(self, index: QModelIndex | QPersistentModelIndex) -> bool:
        """
        Return True if index is the first top-level row after the Pinned section.

        Qt always paints a parent's children, then its next sibling, so row 1
        at the root is visually positioned right after the Pinned section's
        header and all of its (possibly expanded, possibly nested) contents —
        no need to work out exactly how many pinned rows are currently
        visible.  Shared by the delegate (to reserve space for the divider)
        and the tree view (to keep every part of that row's own painting,
        including Qt's branch/selection background, out of that space).

        Args:
            index: Model index to check.

        Returns:
            True if index is that boundary row.
        """
        if index.parent().isValid() or index.row() != 1:
            return False

        return self.pinned_section_index().isValid()

    def pinned_group_paths(self) -> list[str]:
        """
        Get the real folder paths of all currently-built pinned folder-group labels.

        Returns:
            Absolute folder paths, one per _PinnedGroupNode currently in the tree.
        """
        return [node.path for node in self._nodes.values() if isinstance(node, _PinnedGroupNode)]

    def pinned_group_index_for_path(self, path: str) -> QModelIndex:
        """
        Get the model index of the pinned folder-group label for path, if present.

        Distinct from index_for_path(), which always resolves to the real
        folder node — this looks specifically for the group label sharing
        that folder's path inside the Pinned section.

        Args:
            path: Absolute folder path to look up.

        Returns:
            The group label's index, or an invalid QModelIndex if not found.
        """
        norm = os.path.normpath(path)
        for key, node in self._nodes.items():
            if isinstance(node, _PinnedGroupNode) and os.path.normpath(node.path) == norm:
                return self.index_for_key(key)

        return QModelIndex()

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
            if isinstance(node, (_PinnedSectionNode, _PinnedGroupNode)):
                # The Pinned header has no real path; a group label shares its
                # path with the real folder node elsewhere, which must win.
                continue

            if os.path.normpath(node.path) == norm:
                parent_key = node.parent_key
                siblings = self._children.get(parent_key, [])
                try:
                    row = siblings.index(key)
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
