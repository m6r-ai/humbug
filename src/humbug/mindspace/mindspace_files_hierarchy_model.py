"""Hierarchy model for all mindspace files with logical .conv parent-child nesting."""

import json
import logging
import os
from datetime import datetime
from typing import Any

from PySide6.QtCore import Qt, QModelIndex, QPersistentModelIndex, QSortFilterProxyModel, Signal, QFileInfo, QFileSystemWatcher, QTimer
from PySide6.QtGui import QStandardItemModel, QStandardItem

from humbug.mindspace.mindspace_tree_icon_provider import MindspaceTreeIconProvider


FILE_PATH_ROLE = Qt.ItemDataRole.UserRole
IS_DIR_ROLE = Qt.ItemDataRole.UserRole + 1


class MindspaceFilesHierarchyModel(QStandardItemModel):
    """
    QStandardItemModel that shows all mindspace files using filesystem hierarchy,
    but nests .conv files under their logical parents based on the 'parent' metadata field.
    The .humbug directory is excluded.
    """

    files_changed = Signal()

    def __init__(self, icon_provider: MindspaceTreeIconProvider, parent: Any = None) -> None:
        super().__init__(parent)
        self._logger = logging.getLogger("MindspaceFilesHierarchyModel")
        self._icon_provider = icon_provider
        self._mindspace_root = ""
        self._path_to_item: dict[str, QStandardItem] = {}

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
        """Return the absolute path stored at index, or ''."""
        item = self.itemFromIndex(index)
        if item is None:
            return ""
        path = item.data(FILE_PATH_ROLE)
        return path if isinstance(path, str) else ""

    def index_for_path(self, path: str) -> QModelIndex:
        """Return the QModelIndex for path, or an invalid index."""
        normalized = os.path.normpath(path)
        item = self._path_to_item.get(normalized)
        if item is not None:
            return self.indexFromItem(item)
        return QModelIndex()

    def refresh(self, mindspace_root: str) -> None:
        """Re-scan mindspace_root and rebuild the tree immediately."""
        self._debounce_timer.stop()
        if mindspace_root != self._mindspace_root:
            # Switching mindspace: full clear so stale items don't appear
            self._mindspace_root = mindspace_root
            self.clear()
            self._path_to_item.clear()
        self._do_rebuild()

    def update_icons(self, icon_provider: MindspaceTreeIconProvider) -> None:
        """Refresh icons on all items after a theme/zoom change."""
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

    def _do_rebuild(self) -> None:
        """Incrementally update the model so tree expansion state is preserved."""
        if not self._mindspace_root or not os.path.exists(self._mindspace_root):
            if self._path_to_item:
                self.clear()
                self._path_to_item.clear()
            self.files_changed.emit()
            return

        prev_watched_dirs = set(self._fs_watcher.directories())

        # Scan current filesystem state
        new_entry_info: dict[str, tuple[str, str | None, float, bool]] = {}
        self._scan_directory(self._mindspace_root, new_entry_info)

        # Prune stale watched paths
        new_watched = set(self._fs_watcher.directories())
        stale = prev_watched_dirs - new_watched
        if stale:
            self._fs_watcher.removePaths(list(stale))

        old_paths = set(self._path_to_item.keys())
        new_paths = set(new_entry_info.keys())

        # Step 1: remove items that no longer exist (deepest first so parents outlive children)
        for path in sorted(old_paths - new_paths, key=lambda p: p.count(os.sep), reverse=True):
            item = self._path_to_item.pop(path, None)
            if item is None:
                continue
            parent = item.parent()
            if parent is None:
                parent = self.invisibleRootItem()
            row = item.row()
            if row >= 0:
                parent.removeRow(row)

        # Step 2: create QStandardItems for newly added paths
        added_paths = new_paths - old_paths
        for path in added_paths:
            display_name, _parent_rel, creation_time, is_dir = new_entry_info[path]
            item = self._make_item(path, display_name, creation_time, is_dir)
            self._path_to_item[path] = item

        # Step 3: compute desired parent for every path in the new state
        norm_root = os.path.normpath(self._mindspace_root)
        desired_parent: dict[str, str | None] = {}
        for path, (_dn, parent_rel, _ct, is_dir) in new_entry_info.items():
            resolved: str | None = None
            if not is_dir and parent_rel:
                candidate = os.path.normpath(os.path.join(self._mindspace_root, parent_rel))
                if candidate in self._path_to_item:
                    resolved = candidate
            if resolved is None:
                fs_parent = os.path.normpath(os.path.dirname(path))
                if fs_parent != norm_root and fs_parent in self._path_to_item:
                    resolved = fs_parent
            desired_parent[path] = resolved

        # Step 4: update display names and detect items that need (re-)placement
        need_placement: set[str] = set(added_paths)

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
                # Detach from current parent before re-placing
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

        self.files_changed.emit()

    def _sort_entries(self, entries: list[tuple[str, tuple]]) -> list[tuple[str, tuple]]:
        """Sort: directories first, then alphabetical by display name."""
        def key(entry: tuple[str, tuple]) -> tuple:
            _abs_path, (display_name, _, _, is_dir) = entry
            return (0 if is_dir else 1, display_name.lower())

        return sorted(entries, key=key)

    def _make_item(self, abs_path: str, display_name: str, creation_time: float, is_dir: bool) -> QStandardItem:
        item = QStandardItem(display_name)
        item.setData(abs_path, FILE_PATH_ROLE)
        item.setData(is_dir, IS_DIR_ROLE)
        item.setData(creation_time, Qt.ItemDataRole.UserRole + 2)
        item.setEditable(True)
        item.setIcon(self._icon_provider.icon(QFileInfo(abs_path)))
        return item

    def _scan_directory(self, directory: str, entry_info: dict, depth: int = 0) -> None:
        """Recursively scan directory for all files and subdirectories."""
        if depth > 8:
            return

        self._fs_watcher.addPath(directory)

        try:
            entries = list(os.scandir(directory))
        except OSError:
            return

        for entry in entries:
            # Skip hidden files/dirs and .humbug
            if entry.name.startswith(".") or entry.name == ".humbug":
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

            elif entry.is_file():
                try:
                    stat = entry.stat()
                    ctime = float(getattr(stat, "st_birthtime", stat.st_ctime))
                except OSError:
                    ctime = 0.0

                parent_rel: str | None = None
                if entry.name.lower().endswith(".conv"):
                    parent_rel, conv_ctime = self._read_conv_metadata(abs_path)
                    if conv_ctime:
                        ctime = conv_ctime

                entry_info[abs_path] = (entry.name, parent_rel, ctime, False)

    def _read_conv_metadata(self, file_path: str) -> tuple[str | None, float]:
        """Return (parent_relative_path, creation_time) from a .conv file."""
        parent_rel: str | None = None
        ctime: float = 0.0

        try:
            with open(file_path, "r", encoding="utf-8") as f:
                data = json.load(f)

            metadata = data.get("metadata", {})
            parent_rel = metadata.get("parent")

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

        return parent_rel, ctime

    def _on_fs_changed(self, _path: str) -> None:
        """Debounce rapid filesystem events into a single rebuild."""
        self._debounce_timer.start()


# ---------------------------------------------------------------------------
# Sort proxy
# ---------------------------------------------------------------------------

class MindspaceFilesSortProxy(QSortFilterProxyModel):
    """Thin proxy that keeps directories before files and sorts alphabetically."""

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self.setSortCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)

    def lessThan(
        self,
        source_left: QModelIndex | QPersistentModelIndex,
        source_right: QModelIndex | QPersistentModelIndex,
    ) -> bool:
        left_is_dir = source_left.data(IS_DIR_ROLE)
        right_is_dir = source_right.data(IS_DIR_ROLE)

        if left_is_dir and not right_is_dir:
            return True
        if not left_is_dir and right_is_dir:
            return False

        left_name = (source_left.data(Qt.ItemDataRole.DisplayRole) or "").lower()
        right_name = (source_right.data(Qt.ItemDataRole.DisplayRole) or "").lower()
        return left_name < right_name
