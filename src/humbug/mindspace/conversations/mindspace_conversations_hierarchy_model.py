"""Hierarchy model for conversations based on parent-child relationships stored in .conv metadata."""

import json
import logging
import os
from datetime import datetime
from enum import Enum
from typing import Any

from PySide6.QtCore import Qt, QModelIndex, QPersistentModelIndex, QSortFilterProxyModel, Signal, QFileInfo, QFileSystemWatcher, QTimer
from PySide6.QtGui import QStandardItemModel, QStandardItem

from humbug.mindspace.mindspace_tree_icon_provider import MindspaceTreeIconProvider


FILE_PATH_ROLE = Qt.ItemDataRole.UserRole
IS_DIR_ROLE = Qt.ItemDataRole.UserRole + 1
CREATION_TIME_ROLE = Qt.ItemDataRole.UserRole + 2


class MindspaceConversationsHierarchyModel(QStandardItemModel):
    """
    QStandardItemModel that builds a conversation tree from parent references
    stored in each .conv file's metadata.  Children are nested under their
    parent conversation item; all other conversations appear at the root level.
    Subdirectories are shown as expandable folder nodes.
    """

    class SortMode(Enum):
        NAME = "name"
        CREATION_TIME = "creation_time"

    conversations_changed = Signal()

    def __init__(self, icon_provider: MindspaceTreeIconProvider, parent: Any = None) -> None:
        super().__init__(parent)
        self._logger = logging.getLogger("MindspaceConversationsHierarchyModel")
        self._icon_provider = icon_provider
        self._conversations_dir = ""
        self._mindspace_root = ""
        self._sort_mode = self.SortMode.CREATION_TIME

        # Map normalised absolute path → QStandardItem for O(1) parent lookup
        self._path_to_item: dict[str, QStandardItem] = {}

        # Debounce rapid filesystem events so we don't rebuild on every file write
        self._debounce_timer = QTimer()
        self._debounce_timer.setSingleShot(True)
        self._debounce_timer.setInterval(150)
        self._debounce_timer.timeout.connect(self._do_rebuild)

        self._fs_watcher = QFileSystemWatcher()
        self._fs_watcher.directoryChanged.connect(self._on_fs_changed)
        self._fs_watcher.fileChanged.connect(self._on_fs_changed)

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def file_path(self, index: QModelIndex | QPersistentModelIndex) -> str:
        """Return the absolute file-system path stored at *index*, or ''."""
        item = self.itemFromIndex(index)
        if item is None:
            return ""
        path = item.data(FILE_PATH_ROLE)
        return path if isinstance(path, str) else ""

    def index_for_path(self, path: str) -> QModelIndex:
        """Return the QModelIndex for *path*, or an invalid index."""
        normalized = os.path.normpath(path)
        item = self._path_to_item.get(normalized)
        if item is not None:
            return self.indexFromItem(item)
        return QModelIndex()

    def get_sort_mode(self) -> "MindspaceConversationsHierarchyModel.SortMode":
        return self._sort_mode

    def set_sort_mode(self, mode: "MindspaceConversationsHierarchyModel.SortMode") -> None:
        if self._sort_mode != mode:
            self._sort_mode = mode
            self._do_rebuild()

    def refresh(self, conversations_dir: str, mindspace_root: str = "") -> None:
        """Re-scan *conversations_dir* and rebuild the tree immediately."""
        new_root = mindspace_root or (os.path.dirname(conversations_dir) if conversations_dir else "")
        self._debounce_timer.stop()
        if conversations_dir != self._conversations_dir:
            # Switching to a different conversations directory: full clear
            self._conversations_dir = conversations_dir
            self._mindspace_root = new_root
            self.clear()
            self._path_to_item.clear()
        else:
            self._conversations_dir = conversations_dir
            self._mindspace_root = new_root
        self._do_rebuild()

    def update_icons(self, icon_provider: MindspaceTreeIconProvider) -> None:
        """Refresh icons on all items after a theme/zoom change (in-place, no rebuild)."""
        self._icon_provider = icon_provider

        def _update(item: QStandardItem) -> None:
            path = item.data(FILE_PATH_ROLE)
            if path:
                item.setIcon(icon_provider.icon(QFileInfo(path)))
            for row in range(item.rowCount()):
                child = item.child(row)
                if child is not None:
                    _update(child)

        root = self.invisibleRootItem()
        for row in range(root.rowCount()):
            child = root.child(row)
            if child is not None:
                _update(child)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _evict_qt_descendants(self, item: "QStandardItem", new_paths: set, rescued: set) -> None:
        """Remove Qt children of item from _path_to_item before their C++ objects are deleted."""
        for row in range(item.rowCount()):
            child = item.child(row)
            if child is None:
                continue
            child_path = child.data(FILE_PATH_ROLE)
            if child_path and child_path in self._path_to_item:
                self._path_to_item.pop(child_path)
                if child_path in new_paths:
                    rescued.add(child_path)
            self._evict_qt_descendants(child, new_paths, rescued)

    def _do_rebuild(self) -> None:
        """Incrementally update the model so tree expansion state is preserved."""
        conv_dir = self._conversations_dir
        if not conv_dir or not os.path.exists(conv_dir):
            if self._path_to_item:
                self.clear()
                self._path_to_item.clear()
            self.conversations_changed.emit()
            return

        prev_watched_dirs = set(self._fs_watcher.directories())

        # Scan current filesystem state
        new_entry_info: dict[str, tuple[str, str | None, float, bool]] = {}
        self._scan_directory(conv_dir, new_entry_info)

        # Prune stale watched paths
        new_watched = set(self._fs_watcher.directories())
        stale = prev_watched_dirs - new_watched
        if stale:
            self._fs_watcher.removePaths(list(stale))

        old_paths = set(self._path_to_item.keys())
        new_paths = set(new_entry_info.keys())

        # Step 1: remove items that no longer exist (deepest first).
        # Removing a Qt item with removeRow also deletes all its Qt children.
        # Rescue any children whose paths are still valid (in new_paths) by
        # evicting them from _path_to_item before the deletion so Step 2
        # recreates them with a fresh QStandardItem.
        rescued_paths: set[str] = set()
        for path in sorted(old_paths - new_paths, key=lambda p: p.count(os.sep), reverse=True):
            item = self._path_to_item.pop(path, None)
            if item is None:
                continue
            # Evict any Qt descendants that are still alive in _path_to_item
            # so they get fresh items in Step 2 instead of crashing in Step 4.
            self._evict_qt_descendants(item, new_paths, rescued_paths)
            parent = item.parent()
            if parent is None:
                parent = self.invisibleRootItem()
            row = item.row()
            if row >= 0:
                parent.removeRow(row)

        # Step 2: create QStandardItems for newly added paths (and rescued ones).
        added_paths = (new_paths - old_paths) | rescued_paths
        for path in added_paths:
            if path not in new_entry_info:
                continue
            display_name, _, creation_time, is_dir = new_entry_info[path]
            item = self._make_item(path, display_name, creation_time, is_dir)
            self._path_to_item[path] = item

        # Step 3: compute desired parent for every path in the new state
        norm_conv_dir = os.path.normpath(conv_dir)
        desired_parent: dict[str, str | None] = {}
        for path, (_, parent_rel, _, is_dir) in new_entry_info.items():
            resolved: str | None = None
            if not is_dir and parent_rel:
                candidate = os.path.normpath(os.path.join(self._mindspace_root, parent_rel))
                if candidate in self._path_to_item:
                    resolved = candidate
            if resolved is None:
                fs_parent = os.path.normpath(os.path.dirname(path))
                if fs_parent != norm_conv_dir and fs_parent in self._path_to_item:
                    resolved = fs_parent
            desired_parent[path] = resolved

        # Step 4: update display names and detect items that need (re-)placement
        need_placement: set[str] = set(added_paths) | rescued_paths

        for path in old_paths & new_paths:
            item = self._path_to_item.get(path)
            if item is None:
                continue

            # Update display name if changed
            new_dn = new_entry_info[path][0]
            if item.text() != new_dn:
                item.setText(new_dn)

            # Detect parent change
            cur_parent_item = item.parent()
            cur_parent_path: str | None = None
            if cur_parent_item is not None:
                cur_parent_path = cur_parent_item.data(FILE_PATH_ROLE)

            if cur_parent_path != desired_parent.get(path):
                if cur_parent_item is not None:
                    cur_parent_item.takeRow(item.row())
                else:
                    self.invisibleRootItem().takeRow(item.row())
                need_placement.add(path)

        # Step 5: place items that need placement (topological order: parents before children)
        placed: set[str] = (new_paths - need_placement)
        unplaced = list(need_placement)

        for _ in range(len(unplaced) + 1):
            if not unplaced:
                break
            still_unplaced = []
            for path in unplaced:
                parent_path = desired_parent.get(path)
                if parent_path is not None and parent_path not in placed:
                    still_unplaced.append(path)
                    continue
                item = self._path_to_item[path]
                if parent_path is None:
                    self.invisibleRootItem().appendRow(item)
                else:
                    parent_item = self._path_to_item.get(parent_path)
                    (parent_item if parent_item is not None else self.invisibleRootItem()).appendRow(item)
                placed.add(path)
            unplaced = still_unplaced

        # Fallback: any remaining unplaced go to root
        for path in unplaced:
            item = self._path_to_item.get(path)
            if item is not None:
                self.invisibleRootItem().appendRow(item)

        self.conversations_changed.emit()

    def _sort_entries(
        self, entries: list[tuple[str, tuple]]
    ) -> list[tuple[str, tuple]]:
        """Sort entries: directories first, then by sort mode."""

        def key(entry: tuple[str, tuple]) -> tuple:
            abs_path, (_, _, creation_time, is_dir) = entry
            display = os.path.basename(abs_path)
            dir_key = 0 if is_dir else 1
            if self._sort_mode == self.SortMode.CREATION_TIME:
                return (dir_key, -creation_time, display.lower())
            return (dir_key, display.lower())

        return sorted(entries, key=key)

    def _make_item(
        self, abs_path: str, display_name: str, creation_time: float, is_dir: bool
    ) -> QStandardItem:
        item = QStandardItem(display_name)
        item.setData(abs_path, FILE_PATH_ROLE)
        item.setData(is_dir, IS_DIR_ROLE)
        item.setData(creation_time, CREATION_TIME_ROLE)
        item.setEditable(True)
        item.setIcon(self._icon_provider.icon(QFileInfo(abs_path)))
        return item

    def _scan_directory(
        self,
        directory: str,
        entry_info: dict,
        depth: int = 0,
    ) -> None:
        """Recursively scan *directory* for .conv files and sub-directories."""
        if depth > 8:
            return

        self._fs_watcher.addPath(directory)

        try:
            entries = list(os.scandir(directory))
        except OSError:
            return

        for entry in entries:
            if entry.name.startswith("."):
                continue

            abs_path = os.path.normpath(entry.path)

            if entry.is_dir(follow_symlinks=False):
                try:
                    stat = entry.stat()
                    ctime = float(getattr(stat, "st_birthtime", stat.st_ctime))
                except OSError:
                    ctime = 0.0
                entry_info[abs_path] = (entry.name, None, ctime, True)
                self._scan_directory(abs_path, entry_info, depth + 1)

            elif entry.is_file() and entry.name.lower().endswith(".conv"):
                display_name = entry.name[:-5]  # strip ".conv"
                parent_rel, ctime = self._read_conv_metadata(abs_path)
                entry_info[abs_path] = (display_name, parent_rel, ctime, False)

    def _read_conv_metadata(self, file_path: str) -> tuple[str | None, float]:
        """Return (parent_relative_path, creation_time) from a .conv file."""
        parent_rel: str | None = None
        ctime: float = 0.0

        try:
            with open(file_path, "r", encoding="utf-8") as f:
                data = json.load(f)

            metadata = data.get("metadata", {})
            parent_rel = metadata.get("parent")  # None or relative path string

            messages = data.get("conversation", [])
            for msg in messages:
                if isinstance(msg, dict) and "timestamp" in msg:
                    ts = msg["timestamp"]
                    try:
                        if ts.endswith("Z"):
                            ts = ts[:-1] + "+00:00"
                        ctime = datetime.fromisoformat(ts).timestamp()
                    except (ValueError, AttributeError):
                        pass
                    break

        except (OSError, json.JSONDecodeError, KeyError):
            pass

        if not ctime:
            try:
                stat = os.stat(file_path)
                ctime = float(getattr(stat, "st_birthtime", stat.st_ctime))
            except OSError:
                pass

        return parent_rel, ctime

    def _on_fs_changed(self, _path: str) -> None:
        # Debounce: restart the timer so rapid-fire events collapse into one rebuild
        self._debounce_timer.start()


# ---------------------------------------------------------------------------
# Sort proxy
# ---------------------------------------------------------------------------

class MindspaceConversationsSortProxy(QSortFilterProxyModel):
    """
    Thin proxy that delegates sorting to the hierarchy model's current sort
    mode so that QTreeView.setSortingEnabled(True) works correctly.
    """

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self.setSortCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)

    def lessThan(
        self,
        source_left: QModelIndex | QPersistentModelIndex,
        source_right: QModelIndex | QPersistentModelIndex,
    ) -> bool:
        source = self.sourceModel()
        if not isinstance(source, MindspaceConversationsHierarchyModel):
            return super().lessThan(source_left, source_right)

        left_is_dir = source_left.data(IS_DIR_ROLE)
        right_is_dir = source_right.data(IS_DIR_ROLE)

        # Directories always sort before files
        if left_is_dir and not right_is_dir:
            return True
        if not left_is_dir and right_is_dir:
            return False

        if source.get_sort_mode() == MindspaceConversationsHierarchyModel.SortMode.CREATION_TIME:
            left_time = source_left.data(CREATION_TIME_ROLE) or 0.0
            right_time = source_right.data(CREATION_TIME_ROLE) or 0.0
            return left_time > right_time  # newest first

        left_name = (source_left.data(Qt.ItemDataRole.DisplayRole) or "").lower()
        right_name = (source_right.data(Qt.ItemDataRole.DisplayRole) or "").lower()
        return left_name < right_name

    def set_conversation_sort_mode(
        self, mode: MindspaceConversationsHierarchyModel.SortMode
    ) -> None:
        source = self.sourceModel()
        if isinstance(source, MindspaceConversationsHierarchyModel):
            source.set_sort_mode(mode)
        self.invalidate()

    def get_conversation_sort_mode(
        self,
    ) -> MindspaceConversationsHierarchyModel.SortMode:
        source = self.sourceModel()
        if isinstance(source, MindspaceConversationsHierarchyModel):
            return source.get_sort_mode()
        return MindspaceConversationsHierarchyModel.SortMode.CREATION_TIME
