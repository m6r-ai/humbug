"""Commit-history view: commit list, changed files, and per-file diff."""

import logging
import os

from PySide6.QtCore import (
    QFileInfo, QModelIndex, QPersistentModelIndex, QPoint, QPointF, QRect, QSize, Qt, Signal
)
from PySide6.QtGui import QBrush, QColor, QPainter, QPen
from PySide6.QtWidgets import (
    QApplication, QInputDialog, QListWidget, QListWidgetItem, QPlainTextEdit, QSplitter,
    QStyle, QStyledItemDelegate, QStyleOptionViewItem, QVBoxLayout, QWidget
)

from git import (
    CommitFileChange, CommitInfo, GitError, VCSStatusCode,
    get_commit_file_diff, get_commit_files, get_log, get_ref_diff_files, get_ref_file_diff
)

from desktop.color_role import ColorRole
from desktop.git_history_tab.git_graph import GraphRow, compute_graph
from desktop.git_ui_helpers import DiffHighlighter, relative_time
from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType
from desktop.mindspace.mindspace_vcs_poller import MindspaceVCSPoller
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.style_manager import StyleManager
from desktop.vcs_sidebar.git_operations_mediator import GitOperationsMediator


_COMMIT_ROLE = Qt.ItemDataRole.UserRole
_FILE_PATH_ROLE = Qt.ItemDataRole.UserRole + 1  # relative path stored on file items

_STATUS_LABELS = {
    VCSStatusCode.MODIFIED: "M",
    VCSStatusCode.ADDED: "A",
    VCSStatusCode.DELETED: "D",
    VCSStatusCode.RENAMED: "R",
    VCSStatusCode.COPIED: "C",
    VCSStatusCode.UNKNOWN: "!",
}


# Fixed, theme-independent palette for graph lanes.
_LANE_COLORS = [
    "#e06c75", "#61afef", "#98c379", "#e5c07b",
    "#c678dd", "#56b6c2", "#d19a66", "#7f848e",
]


class _CommitDelegate(QStyledItemDelegate):
    """Renders each commit as a graph gutter, a subject line, and a dim meta line."""

    def __init__(self, style_manager: StyleManager, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = style_manager
        self._graph_rows: list[GraphRow] = []
        self._max_col = 0

    def set_graph(self, rows: list[GraphRow]) -> None:
        """Provide the per-row lane geometry to draw."""
        self._graph_rows = rows
        self._max_col = max((r.max_col for r in rows), default=0)

    def _lane_width(self) -> int:
        """Return the pixel width of one graph lane at the current zoom."""
        return round(14 * self._style_manager.zoom_factor())

    def _gutter_width(self) -> int:
        """Return the total width reserved for the graph gutter."""
        return (self._max_col + 2) * self._lane_width()

    def paint(
        self,
        painter: QPainter,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> None:
        commit: CommitInfo | None = index.data(_COMMIT_ROLE)
        if commit is None:
            super().paint(painter, option, index)
            return

        painter.save()
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        rect: QRect = option.rect  # type: ignore
        state = QStyle.StateFlag(option.state)  # type: ignore
        if state & QStyle.StateFlag.State_Selected:
            painter.fillRect(rect, self._style_manager.get_color(ColorRole.TEXT_SELECTED))

        elif state & QStyle.StateFlag.State_MouseOver:
            painter.fillRect(rect, self._style_manager.get_color(ColorRole.BACKGROUND_TERTIARY_HOVER))

        pad = round(6 * self._style_manager.zoom_factor())

        row = self._graph_rows[index.row()] if 0 <= index.row() < len(self._graph_rows) else None
        gutter = self._gutter_width() if row is not None else pad
        if row is not None:
            self._paint_graph(painter, rect, row)

        text_rect = rect.adjusted(gutter, 2, -pad, -2)

        font = option.font  # type: ignore
        subject_font = painter.font()
        subject_font.setPointSizeF(font.pointSizeF())
        painter.setFont(subject_font)
        painter.setPen(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))
        fm = painter.fontMetrics()
        half = text_rect.height() // 2
        subject_rect = QRect(text_rect.left(), text_rect.top(), text_rect.width(), half)
        painter.drawText(
            subject_rect,
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            fm.elidedText(commit.subject, Qt.TextElideMode.ElideRight, subject_rect.width()),
        )

        meta_font = painter.font()
        meta_font.setPointSizeF(font.pointSizeF() * 0.85)
        painter.setFont(meta_font)
        painter.setPen(self._style_manager.get_color(ColorRole.TEXT_INACTIVE))
        meta = f"{commit.short_hash}  ·  {commit.author_name}  ·  {relative_time(commit.timestamp)}"
        meta_rect = QRect(text_rect.left(), text_rect.top() + half, text_rect.width(), half)
        mfm = painter.fontMetrics()
        painter.drawText(
            meta_rect,
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            mfm.elidedText(meta, Qt.TextElideMode.ElideRight, meta_rect.width()),
        )
        painter.restore()

    def _paint_graph(self, painter: QPainter, rect: QRect, row: GraphRow) -> None:
        """Draw the branch/merge lanes and the commit node in the left gutter."""
        lane_w = self._lane_width()
        zoom = self._style_manager.zoom_factor()
        radius = max(2.5, 3.2 * zoom)

        def lane_x(col: int) -> float:
            return rect.left() + (col + 0.7) * lane_w

        mid = rect.center().y() + 0.5
        top, bot = float(rect.top()), float(rect.bottom())
        node_x = lane_x(row.node_col)

        def pen(color_idx: int) -> QPen:
            p = QPen(QColor(_LANE_COLORS[color_idx % len(_LANE_COLORS)]))
            p.setWidthF(max(1.4, 2.0 * zoom))
            p.setCapStyle(Qt.PenCapStyle.RoundCap)
            return p

        for col, color in row.top_lanes:
            cx = lane_x(col)
            painter.setPen(pen(color))
            if col in row.merge_cols:
                painter.drawLine(QPointF(cx, top), QPointF(node_x, mid))

            else:
                painter.drawLine(QPointF(cx, top), QPointF(cx, mid))

        for col, color in row.bottom_lanes:
            cx = lane_x(col)
            painter.setPen(pen(color))
            if col in row.branch_cols:
                painter.drawLine(QPointF(node_x, mid), QPointF(cx, bot))

            else:
                painter.drawLine(QPointF(cx, mid), QPointF(cx, bot))

        painter.setPen(pen(row.node_color))
        painter.setBrush(QBrush(QColor(_LANE_COLORS[row.node_color % len(_LANE_COLORS)])))
        painter.drawEllipse(QPointF(node_x, mid), radius, radius)

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        return QSize(super().sizeHint(option, index).width(), round(fm.height() * 2.2 + 8 * zoom))


class GitHistoryWidget(QWidget):
    """Displays the commit log with a changed-files list and per-file diff."""

    status_updated = Signal()

    def __init__(self, repo_root: str, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._logger = logging.getLogger("GitHistoryWidget")
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._icon_provider = SidebarTreeIconProvider()
        self._repo_root = repo_root
        self._current_commit: CommitInfo | None = None
        self._compare_ref: str | None = None  # when set, panes show ref..HEAD diff

        self._mediator = GitOperationsMediator(self)
        self._mediator.set_repo_root(repo_root)
        self._mediator.operation_succeeded.connect(self._on_operation_succeeded)
        self._mediator.operation_failed.connect(self._on_operation_failed)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._outer_split = QSplitter(Qt.Orientation.Vertical, self)

        self._commit_list = QListWidget(self._outer_split)
        self._commit_list.setObjectName("_commit_list")
        self._commit_delegate = _CommitDelegate(self._style_manager, self._commit_list)
        self._commit_list.setItemDelegate(self._commit_delegate)
        self._commit_list.currentItemChanged.connect(self._on_commit_selected)
        self._commit_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._commit_list.customContextMenuRequested.connect(self._show_commit_menu)
        self._outer_split.addWidget(self._commit_list)

        self._lower_split = QSplitter(Qt.Orientation.Horizontal, self._outer_split)

        self._files_list = QListWidget(self._lower_split)
        self._files_list.setObjectName("_files_list")
        self._files_list.currentItemChanged.connect(self._on_file_selected)
        self._lower_split.addWidget(self._files_list)

        self._diff_view = QPlainTextEdit(self._lower_split)
        self._diff_view.setObjectName("_diff_view")
        self._diff_view.setReadOnly(True)
        self._diff_view.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        self._highlighter = DiffHighlighter(self._diff_view.document(), self._style_manager)
        self._lower_split.addWidget(self._diff_view)
        self._lower_split.setStretchFactor(0, 1)
        self._lower_split.setStretchFactor(1, 2)

        self._outer_split.addWidget(self._lower_split)
        self._outer_split.setStretchFactor(0, 2)
        self._outer_split.setStretchFactor(1, 3)

        layout.addWidget(self._outer_split)

    def set_repo_root(self, repo_root: str) -> None:
        """Point the view at a different repository and reload."""
        self._repo_root = repo_root
        self._mediator.set_repo_root(repo_root)
        self.reload()

    def _show_commit_menu(self, position: QPoint) -> None:
        """Context menu for a commit: revert, reset, copy hash."""
        item = self._commit_list.itemAt(position)
        if item is None:
            return

        commit: CommitInfo = item.data(_COMMIT_ROLE)
        if commit is None:
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        menu.addAction(strings.git_revert).triggered.connect(
            lambda: self._mediator.revert(commit.commit_hash)
        )

        menu.addAction(strings.git_cherry_pick).triggered.connect(
            lambda: self._mediator.cherry_pick(commit.commit_hash)
        )

        reset_menu = self._style_manager.add_submenu(menu, strings.git_reset)
        reset_menu.addAction(strings.git_reset_soft).triggered.connect(
            lambda: self._mediator.reset("soft", commit.commit_hash)
        )
        reset_menu.addAction(strings.git_reset_mixed).triggered.connect(
            lambda: self._mediator.reset("mixed", commit.commit_hash)
        )
        reset_menu.addAction(strings.git_reset_hard).triggered.connect(
            lambda: self._confirm_hard_reset(commit)
        )

        menu.addSeparator()
        menu.addAction(strings.git_compare_head).triggered.connect(
            lambda: self._compare_with_head(commit)
        )
        menu.addAction(strings.git_create_tag_here).triggered.connect(
            lambda: self._handle_create_tag(commit)
        )
        menu.addAction(strings.git_copy_hash).triggered.connect(
            lambda: QApplication.clipboard().setText(commit.commit_hash)
        )

        menu.exec_(self._commit_list.viewport().mapToGlobal(position))

    def _handle_create_tag(self, commit: CommitInfo) -> None:
        """Prompt for a tag name and create it at the given commit."""
        strings = self._language_manager.strings()
        name, ok = QInputDialog.getText(self, strings.git_new_tag_title, strings.git_new_tag_prompt)
        if ok and name.strip():
            self._mediator.create_tag(name.strip(), target=commit.commit_hash)

    def _confirm_hard_reset(self, commit: CommitInfo) -> None:
        """Confirm before a destructive hard reset."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING,
            strings.git_confirm_reset_hard_title,
            strings.git_confirm_reset_hard_message.format(commit.short_hash),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )
        if result == MessageBoxButton.YES:
            self._mediator.reset("hard", commit.commit_hash)

    def _on_operation_succeeded(self, _op_name: str) -> None:
        """Reload the log and nudge the sidebar poller after a mutation."""
        self.reload()
        MindspaceVCSPoller().force_refresh()

    def _on_operation_failed(self, _op_name: str, message: str) -> None:
        """Surface a git operation failure."""
        strings = self._language_manager.strings()
        MessageBox.show_message(
            self, MessageBoxType.CRITICAL,
            strings.git_error_title, message,
            [MessageBoxButton.OK]
        )

    def reload(self) -> None:
        """Reload the commit log from the repository."""
        self._commit_list.clear()
        self._files_list.clear()
        self._diff_view.clear()

        if not self._repo_root:
            return

        try:
            commits = get_log(self._repo_root)

        except GitError as e:
            self._logger.debug("Failed to load git log: %s", e)
            return

        self._commit_delegate.set_graph(compute_graph(commits))

        for commit in commits:
            item = QListWidgetItem(commit.subject)
            item.setData(_COMMIT_ROLE, commit)
            item.setToolTip(f"{commit.commit_hash}\n{commit.author_email}")
            self._commit_list.addItem(item)

        if self._commit_list.count():
            self._commit_list.setCurrentRow(0)

        self.status_updated.emit()

    def commit_count(self) -> int:
        """Return the number of commits currently listed."""
        return self._commit_list.count()

    def _compare_with_head(self, commit: CommitInfo) -> None:
        """Show the diff between a commit and HEAD in the file/diff panes."""
        self._compare_ref = commit.commit_hash
        self._files_list.clear()
        self._diff_view.clear()

        try:
            files = get_ref_diff_files(self._repo_root, commit.commit_hash, "HEAD")

        except GitError as e:
            self._logger.debug("Failed to compare: %s", e)
            return

        self._populate_files(files)

    def _on_commit_selected(
        self, current: QListWidgetItem | None, _prev: QListWidgetItem | None = None
    ) -> None:
        """Load the changed files for the newly selected commit."""
        self._files_list.clear()
        self._diff_view.clear()
        self._compare_ref = None  # selecting a commit leaves compare mode
        self._current_commit = current.data(_COMMIT_ROLE) if current is not None else None
        if self._current_commit is None:
            return

        try:
            files = get_commit_files(self._repo_root, self._current_commit.commit_hash)

        except GitError as e:
            self._logger.debug("Failed to load commit files: %s", e)
            return

        self._populate_files(files)

    def _populate_files(self, files: list[CommitFileChange]) -> None:
        """Fill the changed-files list from a list of CommitFileChange."""
        for change in files:
            abs_path = os.path.join(self._repo_root, change.path)
            item = QListWidgetItem(f"{_STATUS_LABELS.get(change.code, '?')}  {change.path}")
            item.setIcon(self._icon_provider.icon(QFileInfo(abs_path)))
            item.setData(_FILE_PATH_ROLE, change.path)
            item.setForeground(self._color_for_code(change.code))
            item.setToolTip(change.path)
            self._files_list.addItem(item)

        if self._files_list.count():
            self._files_list.setCurrentRow(0)

    def _on_file_selected(
        self, current: QListWidgetItem | None, _prev: QListWidgetItem | None = None
    ) -> None:
        """Show the diff for the selected file (commit diff, or compare diff)."""
        self._diff_view.clear()
        if current is None:
            return

        rel_path = current.data(_FILE_PATH_ROLE)
        try:
            if self._compare_ref is not None:
                diff = get_ref_file_diff(self._repo_root, self._compare_ref, "HEAD", rel_path)

            elif self._current_commit is not None:
                diff = get_commit_file_diff(self._repo_root, self._current_commit.commit_hash, rel_path)

            else:
                return

        except GitError as e:
            self._logger.debug("Failed to load diff: %s", e)
            return

        self._diff_view.setPlainText(diff)

    def _color_for_code(self, code: VCSStatusCode) -> QColor:
        """Return the themed foreground colour for a status code."""
        if code in (VCSStatusCode.ADDED, VCSStatusCode.UNTRACKED):
            return self._style_manager.get_color(ColorRole.VCS_ADDED)

        if code == VCSStatusCode.DELETED:
            return self._style_manager.get_color(ColorRole.VCS_DELETED)

        if code in (VCSStatusCode.RENAMED, VCSStatusCode.COPIED):
            return self._style_manager.get_color(ColorRole.VCS_RENAMED)

        return self._style_manager.get_color(ColorRole.VCS_MODIFIED)

    def apply_style(self) -> None:
        """Reapply theme and zoom-dependent styling."""
        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size()
        font = self.font()
        font.setPointSizeF(base * zoom)
        self.setFont(font)
        self._commit_list.setFont(font)
        self._files_list.setFont(font)

        mono = self._style_manager.make_monospace_font()
        mono.setPointSizeF(base * zoom)
        self._diff_view.setFont(mono)

        self._icon_provider.update_icons()
        self._highlighter.refresh()

        sm = self._style_manager
        bg = sm.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)
        panel = sm.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        text = sm.get_color_str(ColorRole.TEXT_PRIMARY)
        selected = sm.get_color_str(ColorRole.TEXT_SELECTED)
        hover = sm.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        border = sm.get_color_str(ColorRole.MESSAGE_BORDER)

        self.setStyleSheet(f"""
            QWidget {{ background-color: {bg}; color: {text}; }}
            QListWidget#_commit_list, QListWidget#_files_list {{
                background-color: {bg};
                border: none;
                border-bottom: 1px solid {border};
                outline: none;
            }}
            QListWidget#_files_list::item {{ padding: 2px 4px; }}
            QListWidget#_files_list::item:selected {{ background-color: {selected}; }}
            QListWidget#_files_list::item:hover {{ background-color: {hover}; }}
            QPlainTextEdit#_diff_view {{
                background-color: {panel};
                border: none;
                color: {text};
            }}
            {sm.get_scrollbar_stylesheet("QPlainTextEdit#_diff_view QScrollBar")}
        """)
