"""Source-control view for the mindspace sidebar."""

import json
import os
import logging
from collections.abc import Callable

from PySide6.QtCore import (
    Qt, QEvent, QFileInfo, QMimeData, QPoint, QPointF, QSize, QTimer, Signal
)
from PySide6.QtGui import QColor, QDrag, QHelpEvent, QIcon, QMouseEvent, QPainter, QPen, QPixmap
from PySide6.QtWidgets import (
    QApplication, QFrame, QHBoxLayout, QInputDialog, QLabel, QLineEdit, QMenu, QPlainTextEdit,
    QSizePolicy, QToolButton, QToolTip, QTreeWidget, QTreeWidgetItem,
    QVBoxLayout, QWidget
)

from git import (
    BranchInfo, GitError, MergeState, UpstreamStatus, VCSFileStatus, VCSStatusCode,
    get_branch_info, get_current_branch, get_head_message, get_identity,
    get_staged_file_diff, get_unstaged_file_diff, list_remote_branches,
    list_remotes, list_tags, split_file_diff, stash_list
)

from mindspace.mindspace_log_level import MindspaceLogLevel

from desktop.color_role import ColorRole
from desktop.file_utils import is_binary_image_file
from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.mindspace.mindspace_vcs_poller import MindspaceVCSPoller
from desktop.settings.settings_combo import SettingsCombo
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.style_manager import StyleManager
from desktop.vcs_sidebar.git_operations_mediator import GitOperationsMediator
from desktop.vcs_sidebar.vcs_action_delegate import (
    VCSActionDelegate, BADGE_ROLE, BADGE_COLOR_ROLE, KIND_ROLE
)
from desktop.vcs_sidebar.vcs_hunk_dialog import HunkSelectionDialog
from desktop.vcs_sidebar.vcs_file_dialogs import BlameDialog, FileHistoryDialog


_PATH_ROLE = Qt.ItemDataRole.UserRole              # Absolute path (file or folder)

_STATUS_LABELS: dict[VCSStatusCode, str] = {
    VCSStatusCode.MODIFIED: "M",
    VCSStatusCode.ADDED: "A",
    VCSStatusCode.DELETED: "D",
    VCSStatusCode.RENAMED: "R",
    VCSStatusCode.COPIED: "C",
    VCSStatusCode.UNTRACKED: "?",
    VCSStatusCode.CONFLICTED: "!",
    VCSStatusCode.UNKNOWN: "!",
}


class _VCSTree(QTreeWidget):
    """Single-column tree with Humbug path drags and inline hover actions."""

    action_triggered = Signal(str, str, str)  # action_id, path, kind

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setHeaderHidden(True)
        self.setColumnCount(1)
        self.setRootIsDecorated(True)
        self.setExpandsOnDoubleClick(False)
        self.setMouseTracking(True)
        self.setToolTipDuration(10000)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._drag_start_pos: QPoint | None = None
        # Localised tooltip text per inline action id, populated by the sidebar.
        self.action_tooltips: dict[str, str] = {}

    def viewportEvent(self, e: QEvent) -> bool:
        """
        Show the action name (not the file path) when hovering an inline glyph.

        Item-view tooltips are delivered to the viewport, so this is the correct
        hook.  Over an action glyph we show the action's tooltip; elsewhere we
        fall back to the default (the item's file-path tooltip).
        """
        if e.type() == QEvent.Type.ToolTip and isinstance(e, QHelpEvent):
            hit = self._action_at(e.pos())
            if hit is not None:
                tip = self.action_tooltips.get(hit[0])
                if tip:
                    QToolTip.showText(e.globalPos(), tip, self.viewport())
                    return True

        return super().viewportEvent(e)

    def _action_at(self, pos: QPoint) -> tuple[str, QTreeWidgetItem] | None:
        """Return the (action_id, item) whose inline glyph is at *pos*, or None."""
        index = self.indexAt(pos)
        if not index.isValid():
            return None

        item = self.itemFromIndex(index)
        delegate = self.itemDelegate()
        if item is None or not isinstance(delegate, VCSActionDelegate):
            return None

        kind = item.data(0, KIND_ROLE) or "file"
        rect = self.visualRect(index)
        for action, arect in delegate.action_rects(rect, kind):
            if arect.contains(pos):
                return action, item

        return None

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            hit = self._action_at(event.pos())
            if hit is not None:
                action, item = hit
                path = item.data(0, _PATH_ROLE) or ""
                kind = item.data(0, KIND_ROLE) or "file"
                self.action_triggered.emit(action, path, kind)
                event.accept()
                return

            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        if event.buttons() & Qt.MouseButton.LeftButton and self._drag_start_pos is not None:
            if (event.pos() - self._drag_start_pos).manhattanLength() >= QApplication.startDragDistance():
                self._maybe_start_drag()

            super().mouseMoveEvent(event)
            return

        # Hover: point the cursor at inline actions so they read as clickable.
        self.setCursor(
            Qt.CursorShape.PointingHandCursor if self._action_at(event.pos())
            else Qt.CursorShape.ArrowCursor
        )
        super().mouseMoveEvent(event)

    def _maybe_start_drag(self) -> None:
        """Begin a Humbug path drag for the file item under the drag origin."""
        if self._drag_start_pos is None:
            return

        item = self.itemAt(self._drag_start_pos)
        self._drag_start_pos = None
        if item is None or item.data(0, KIND_ROLE) != "file":
            return

        path = item.data(0, _PATH_ROLE)
        if not path:
            return

        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-path", path.encode())
        mime_data.setData("application/x-humbug-source", b"vcs")

        drag = QDrag(self)
        drag.setMimeData(mime_data)
        drag.exec_(Qt.DropAction.CopyAction)


class VCSSidebar(SidebarBase):
    """Sidebar panel providing a source-control workflow for the current mindspace."""

    file_clicked = Signal(str, str, bool)                  # panel_id, path, ephemeral
    file_opened_in_editor = Signal(str, bool)              # path, ephemeral
    file_opened_in_preview = Signal(str)                   # path
    file_deleted = Signal(str)                             # path
    file_opened_in_diff = Signal(str, bool)                # path, ephemeral
    repo_available = Signal(bool)                          # True = repo found, False = hidden
    history_requested = Signal(str)                        # repo_root

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the source-control view."""
        super().__init__(parent)

        self.setObjectName("VCSSidebar")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        self._style_manager = StyleManager()
        self._logger = logging.getLogger("VCSSidebar")
        self._mindspace_manager = MindspaceManager()
        self._icon_provider = SidebarTreeIconProvider()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._mindspace_path: str = ""
        self._current_status: list[VCSFileStatus] = []
        self._upstream: UpstreamStatus | None = None
        self._branch_info: BranchInfo = BranchInfo(current="", branches=[])
        self._staged_collapsed = False
        self._changes_collapsed = False
        self._preferred_repo = ""
        self._preferred_applied = False

        # Created before the toolbar so button handlers can reference it.
        self._mediator = GitOperationsMediator(self)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        strings = self._language_manager.strings()

        self._header = SidebarSectionHeader(strings.mindspace_vcs, self)
        layout.addWidget(self._header)

        # All source-control widgets live in a container that is swapped out for
        # the no-repository empty state when the mindspace has no git repo.
        self._repo_content = QWidget(self)
        content_layout = QVBoxLayout(self._repo_content)
        content_layout.setContentsMargins(0, 0, 0, 0)
        content_layout.setSpacing(0)

        # Active-repository selector (custom-popup combo matching the app).
        combo_container = QWidget(self)
        combo_layout = QHBoxLayout(combo_container)
        combo_layout.setContentsMargins(6, 4, 6, 0)
        self._repo_combo = SettingsCombo("", parent=combo_container)
        self._repo_combo._label.hide()
        self._repo_combo.setToolTip(strings.git_repo_tooltip)
        self._repo_combo.value_changed.connect(self._on_repo_selected)
        combo_layout.addWidget(self._repo_combo)
        self._combo_container = combo_container
        content_layout.addWidget(combo_container)

        # Branch + remote toolbar.
        self._toolbar = QWidget(self)
        toolbar_layout = QHBoxLayout(self._toolbar)
        toolbar_layout.setContentsMargins(6, 4, 6, 4)
        toolbar_layout.setSpacing(4)

        # Branch pill (icon + name), expands to fill the toolbar width.
        self._branch_button = QToolButton(self._toolbar)
        self._branch_button.setObjectName("_branch_btn")
        self._branch_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        self._branch_button.setToolTip(strings.git_branch_tooltip)
        self._branch_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._branch_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._branch_button.clicked.connect(self._show_branch_menu)
        toolbar_layout.addWidget(self._branch_button, 1)

        # Remote action icons — flat, icon-only (pull/push also show a count).
        self._fetch_button = self._make_tool_button(strings.git_fetch)
        self._fetch_button.clicked.connect(self._mediator.fetch)
        toolbar_layout.addWidget(self._fetch_button)

        self._pull_button = self._make_tool_button(strings.git_pull, with_text=True)
        self._pull_button.clicked.connect(self._mediator.pull)
        toolbar_layout.addWidget(self._pull_button)

        self._push_button = self._make_tool_button(strings.git_push, with_text=True)
        self._push_button.clicked.connect(self._on_push_clicked)
        toolbar_layout.addWidget(self._push_button)

        self._history_button = self._make_tool_button(strings.git_history)
        self._history_button.clicked.connect(self._on_history_clicked)
        toolbar_layout.addWidget(self._history_button)

        self._more_button = self._make_tool_button(strings.git_more_actions)
        self._more_button.clicked.connect(self._show_more_menu)
        toolbar_layout.addWidget(self._more_button)

        content_layout.addWidget(self._toolbar)

        # In-flight status label (hidden unless an operation is running).
        self._status_label = QLabel("", self)
        self._status_label.setObjectName("_status_label")
        self._status_label.setContentsMargins(8, 0, 8, 2)
        self._status_label.setVisible(False)
        content_layout.addWidget(self._status_label)

        # Only reveal the "Working…" label if an operation runs long enough to
        # matter; fast ops (stage/unstage) never show it, so the layout can't
        # jump in and out for near-instant work.
        self._status_timer = QTimer(self)
        self._status_timer.setSingleShot(True)
        self._status_timer.setInterval(400)
        self._status_timer.timeout.connect(lambda: self._status_label.setVisible(True))

        # Merge/rebase banner (hidden unless a merge or rebase is in progress).
        self._merge_state = MergeState.NONE
        self._merge_banner = QWidget(self)
        self._merge_banner.setObjectName("_merge_banner")
        self._merge_banner.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        banner_layout = QHBoxLayout(self._merge_banner)
        banner_layout.setContentsMargins(8, 4, 6, 4)
        banner_layout.setSpacing(6)
        self._merge_label = QLabel("", self._merge_banner)
        self._merge_label.setObjectName("_merge_label")
        self._merge_label.setWordWrap(True)
        banner_layout.addWidget(self._merge_label, 1)
        self._continue_button = QToolButton(self._merge_banner)
        self._continue_button.setObjectName("_continue_button")
        self._continue_button.setText(strings.git_rebase_continue)
        self._continue_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._continue_button.clicked.connect(self._mediator.rebase_continue)
        self._continue_button.setVisible(False)
        banner_layout.addWidget(self._continue_button)

        self._abort_button = QToolButton(self._merge_banner)
        self._abort_button.setObjectName("_abort_button")
        self._abort_button.setText(strings.git_abort)
        self._abort_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._abort_button.clicked.connect(self._on_abort_clicked)
        banner_layout.addWidget(self._abort_button)
        self._merge_banner.setVisible(False)
        content_layout.addWidget(self._merge_banner)

        # Merge-conflicts section (shown only while conflicts exist).
        self._conflicts_header = SidebarSectionHeader(strings.git_conflicts, self)
        content_layout.addWidget(self._conflicts_header)
        self._conflicts_tree = self._make_tree("_conflicts_tree", is_staged=False, show_actions=False)
        content_layout.addWidget(self._conflicts_tree, 1)
        self._conflicts_header.setVisible(False)
        self._conflicts_tree.setVisible(False)

        # Staged section (header carries an "unstage all" button).
        self._staged_header = SidebarSectionHeader(strings.git_staged_changes, self)
        self._staged_header.clicked.connect(self._toggle_staged)
        self._unstage_all_button = self._make_header_button(strings.git_unstage_all_tooltip)
        self._unstage_all_button.clicked.connect(self._on_unstage_all)
        self._staged_header.add_trailing_widget(self._unstage_all_button)
        content_layout.addWidget(self._staged_header)

        self._staged_tree = self._make_tree("_staged_tree", is_staged=True)
        content_layout.addWidget(self._staged_tree, 1)

        # Divider between the staged and changes sections.
        self._divider = QFrame(self)
        self._divider.setObjectName("_divider")
        self._divider.setFrameShape(QFrame.Shape.HLine)
        self._divider.setFixedHeight(1)
        content_layout.addWidget(self._divider)

        # Changes section (header carries "stage all" + refresh buttons).
        self._changes_header = SidebarSectionHeader(strings.git_changes, self)
        self._changes_header.clicked.connect(self._toggle_changes)
        self._stage_all_button = self._make_header_button(strings.git_stage_all_tooltip)
        self._stage_all_button.clicked.connect(self._on_stage_all)
        self._changes_header.add_trailing_widget(self._stage_all_button)
        self._refresh_button = self._make_header_button(strings.git_refresh)
        self._refresh_button.clicked.connect(self._on_refresh_clicked)
        self._changes_header.add_trailing_widget(self._refresh_button)
        content_layout.addWidget(self._changes_header)

        self._changes_tree = self._make_tree("_changes_tree", is_staged=False)
        content_layout.addWidget(self._changes_tree, 1)

        # Commit area: multi-line message + split commit button.
        commit_widget = QWidget(self)
        commit_layout = QVBoxLayout(commit_widget)
        commit_layout.setContentsMargins(6, 4, 6, 6)
        commit_layout.setSpacing(4)

        self._commit_message = QPlainTextEdit(commit_widget)
        self._commit_message.setObjectName("_commit_message")
        self._commit_message.setPlaceholderText(strings.git_commit_placeholder)
        self._commit_message.setTabChangesFocus(True)
        commit_layout.addWidget(self._commit_message)

        self._commit_button = QToolButton(commit_widget)
        self._commit_button.setObjectName("_commit_button")
        self._commit_button.setText(strings.git_commit_button)
        self._commit_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
        self._commit_button.setPopupMode(QToolButton.ToolButtonPopupMode.MenuButtonPopup)
        self._commit_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._commit_button.clicked.connect(self._do_commit)
        self._commit_menu = self._style_manager.create_menu(self)
        self._commit_and_push_action = self._commit_menu.addAction(strings.git_commit_and_push)
        self._commit_and_push_action.triggered.connect(lambda: self._do_commit(push=True))
        self._amend_action = self._commit_menu.addAction(strings.git_commit_amend)
        self._amend_action.triggered.connect(lambda: self._do_commit(amend=True))
        self._commit_menu.addSeparator()
        self._undo_action = self._commit_menu.addAction(strings.git_undo_last_commit)
        self._undo_action.triggered.connect(self._mediator.undo_last_commit)
        self._commit_button.setMenu(self._commit_menu)
        commit_layout.addWidget(self._commit_button)

        content_layout.addWidget(commit_widget)
        layout.addWidget(self._repo_content, 1)

        # No-repository empty state (Initialize / Clone), shown when the
        # mindspace is open but contains no git repository.
        self._empty_state = self._build_empty_state()
        layout.addWidget(self._empty_state, 1)
        self._empty_state.setVisible(False)

        # Widgets disabled while a git operation is in flight.
        self._action_widgets = [
            self._branch_button, self._fetch_button, self._pull_button, self._push_button,
            self._history_button, self._more_button, self._stage_all_button,
            self._unstage_all_button, self._refresh_button, self._commit_button,
            self._init_repo_button, self._clone_repo_button,
        ]

        # Wire the mediator (created earlier) and status poller.
        self._mediator.operation_started.connect(self._on_operation_started)
        self._mediator.operation_succeeded.connect(self._on_operation_succeeded)
        self._mediator.operation_failed.connect(self._on_operation_failed)
        self._mediator.repo_info_changed.connect(self._on_repo_info_changed)

        self._poller = MindspaceVCSPoller()
        self._poller.repo_state_changed.connect(self._on_repo_state_changed)
        self._poller.status_changed.connect(self._on_status_changed)
        self._poller.repositories_changed.connect(self._on_repositories_changed)

    def _make_tool_button(self, tooltip: str, with_text: bool = False) -> QToolButton:
        """Create a flat icon tool button for the toolbar."""
        button = QToolButton(self._toolbar)
        button.setObjectName("_toolbtn")
        button.setToolTip(tooltip)
        button.setCursor(Qt.CursorShape.PointingHandCursor)
        button.setToolButtonStyle(
            Qt.ToolButtonStyle.ToolButtonTextBesideIcon if with_text
            else Qt.ToolButtonStyle.ToolButtonIconOnly
        )
        return button

    def _make_header_button(self, tooltip: str) -> QToolButton:
        """Create a flat, fixed-size icon button for a section header."""
        button = QToolButton(self)
        button.setObjectName("_hdrbtn")
        button.setToolTip(tooltip)
        button.setCursor(Qt.CursorShape.PointingHandCursor)
        button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        return button

    def _glyph_icon(self, kind: str, size: int) -> QIcon:
        """Paint a crisp themed arrow/plus/minus icon (no chevron)."""
        pixmap = QPixmap(size, size)
        pixmap.fill(Qt.GlobalColor.transparent)
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        pen = QPen(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))
        pen.setWidthF(max(1.4, size * 0.11))
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
        painter.setPen(pen)

        c = size / 2.0
        if kind in ("up", "down"):
            top, bot, head = size * 0.22, size * 0.78, size * 0.26
            painter.drawLine(QPointF(c, top), QPointF(c, bot))
            if kind == "up":
                painter.drawLine(QPointF(c, top), QPointF(c - head, top + head))
                painter.drawLine(QPointF(c, top), QPointF(c + head, top + head))

            else:
                painter.drawLine(QPointF(c, bot), QPointF(c - head, bot - head))
                painter.drawLine(QPointF(c, bot), QPointF(c + head, bot - head))

        elif kind == "plus":
            m = size * 0.24
            painter.drawLine(QPointF(c, m), QPointF(c, size - m))
            painter.drawLine(QPointF(m, c), QPointF(size - m, c))

        elif kind == "minus":
            m = size * 0.24
            painter.drawLine(QPointF(m, c), QPointF(size - m, c))

        elif kind == "dots":
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(self._style_manager.get_color(ColorRole.TEXT_PRIMARY))
            r = max(1.0, size * 0.07)
            for fx in (0.28, 0.5, 0.72):
                painter.drawEllipse(QPointF(size * fx, c), r, r)

        painter.end()
        return QIcon(pixmap)

    def _refresh_button_icons(self) -> None:
        """(Re)build themed icons and sizes for all icon buttons at current zoom."""
        zoom = self._style_manager.zoom_factor()
        s = round(16 * zoom)
        gi = self._style_manager.get_icon_path

        self._branch_button.setIcon(QIcon(gi("fork")))
        self._fetch_button.setIcon(QIcon(gi("update")))
        self._history_button.setIcon(QIcon(gi("clock")))
        self._refresh_button.setIcon(QIcon(gi("update")))
        self._more_button.setIcon(self._glyph_icon("dots", s))
        self._pull_button.setIcon(self._glyph_icon("down", s))
        self._push_button.setIcon(self._glyph_icon("up", s))
        self._stage_all_button.setIcon(self._glyph_icon("plus", s))
        self._unstage_all_button.setIcon(self._glyph_icon("minus", s))

        isize = QSize(s, s)
        toolbar_buttons = (
            self._branch_button, self._fetch_button, self._pull_button,
            self._push_button, self._history_button, self._more_button,
        )
        header_buttons = (self._refresh_button, self._stage_all_button, self._unstage_all_button)
        for button in toolbar_buttons + header_buttons:
            button.setIconSize(isize)

        # Toolbar action icons share a uniform minimum square; pull/push grow to
        # fit their count text (responsive).  Branch expands via its layout stretch.
        square = round(28 * zoom)
        for button in (self._fetch_button, self._pull_button, self._push_button,
                       self._history_button, self._more_button):
            button.setMinimumSize(QSize(square, square))

        # Header buttons are fixed squares so they align perfectly.
        header_square = round(24 * zoom)
        for button in header_buttons:
            button.setFixedSize(QSize(header_square, header_square))

    def _make_tree(self, object_name: str, is_staged: bool, show_actions: bool = True) -> _VCSTree:
        """Create a configured VCS tree widget with its hover-action delegate."""
        tree = _VCSTree()
        tree.setObjectName(object_name)
        tree.setItemDelegate(
            VCSActionDelegate(is_staged, self._style_manager, tree, show_actions=show_actions)
        )
        tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tree.customContextMenuRequested.connect(
            lambda pos, t=tree: self._show_context_menu(t, pos)
        )
        tree.itemClicked.connect(self._on_item_clicked)
        tree.itemActivated.connect(self._on_item_activated)
        tree.itemExpanded.connect(self._on_item_expanded)
        tree.itemCollapsed.connect(self._on_item_collapsed)
        tree.action_triggered.connect(self._apply_row_action)
        self._apply_action_tooltips(tree)
        return tree

    def _apply_action_tooltips(self, tree: "_VCSTree") -> None:
        """Set localised tooltips for the tree's inline action glyphs."""
        strings = self._language_manager.strings()
        tree.action_tooltips = {
            "stage": strings.git_stage,
            "unstage": strings.git_unstage,
            "discard": strings.git_discard,
        }

    # -- Mindspace / repo selection ------------------------------------------

    def set_mindspace(self, path: str) -> None:
        """Set the mindspace root to track."""
        self._mindspace_path = path
        self._current_status = []
        self._staged_tree.clear()
        self._changes_tree.clear()
        self._repo_combo.set_items([])
        self._preferred_repo = self._load_preferred_repo()
        self._preferred_applied = False
        self._poller.set_mindspace(path)
        # The panel is reachable whenever a mindspace is open; when it has no
        # git repository the panel shows the Initialize / Clone empty state
        # (chosen in _on_repo_state_changed) rather than hiding entirely.
        self.repo_available.emit(bool(path))

    def set_active_repo(self, repo_root: str) -> None:
        """Focus the panel on a specific repository (used by "Manage Git")."""
        if not repo_root:
            return

        self._preferred_applied = True
        self._repo_combo.set_value(os.path.normpath(repo_root))
        self._poller.set_active_repo(repo_root)
        self._save_preferred_repo(repo_root)

    def reveal_and_select_file(self, file_path: str) -> None:
        """Select the given file in whichever tree it appears in."""
        if not self._mindspace_path:
            return

        normalized_path = os.path.normpath(file_path)
        for tree in (self._staged_tree, self._changes_tree):
            item = self._find_file_item(tree, normalized_path)
            if item is not None:
                tree.clearSelection()
                tree.setCurrentItem(item)
                tree.scrollToItem(item, QTreeWidget.ScrollHint.EnsureVisible)
                return

    def _find_file_item(self, tree: _VCSTree, normalized_path: str) -> QTreeWidgetItem | None:
        """Return the file item with the given normalized path, expanding ancestors."""
        def walk(item: QTreeWidgetItem) -> QTreeWidgetItem | None:
            if item.data(0, KIND_ROLE) == "file":
                path = item.data(0, _PATH_ROLE)
                if path and os.path.normpath(path) == normalized_path:
                    return item

            for i in range(item.childCount()):
                found = walk(item.child(i))
                if found is not None:
                    return found

            return None

        for i in range(tree.topLevelItemCount()):
            top = tree.topLevelItem(i)
            match = walk(top) if top is not None else None
            if match is not None:
                parent = match.parent()
                while parent is not None:
                    parent.setExpanded(True)
                    parent = parent.parent()

                return match

        return None

    # -- Styling -------------------------------------------------------------

    def apply_style(self) -> None:
        """Reapply theme and zoom-dependent styling."""
        self._header.apply_style()
        self._staged_header.apply_style()
        self._changes_header.apply_style()
        self._conflicts_header.apply_style()

        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size()
        font = self.font()
        font.setPointSizeF(base * zoom)
        self.setFont(font)
        for widget in (
            self._staged_tree, self._changes_tree, self._conflicts_tree, self._branch_button,
            self._commit_message, self._commit_button, self._status_label,
            self._merge_label, self._abort_button, self._continue_button,
            self._empty_title, self._empty_message, self._init_repo_button, self._clone_repo_button,
        ):
            widget.setFont(font)

        # Give the commit box room for a few lines.
        self._commit_message.setFixedHeight(round(self._commit_message.fontMetrics().height() * 3 + 12))

        self._refresh_button_icons()
        self._icon_provider.update_icons()
        self._rebuild_trees()
        self._apply_stylesheet()

    def _apply_stylesheet(self) -> None:
        """Build and apply the widget stylesheet."""
        sm = self._style_manager
        background = sm.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        text = sm.get_color_str(ColorRole.TEXT_PRIMARY)
        text_dim = sm.get_color_str(ColorRole.TEXT_INACTIVE)
        divider = sm.get_color_str(ColorRole.SPLITTER)
        button_bg = sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND)
        button_hover = sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER)
        button_pressed = sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED)
        button_disabled = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)
        commit_bg = sm.get_color_str(ColorRole.BUTTON_BACKGROUND)
        commit_hover = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        commit_pressed = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)
        edit_bg = sm.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)
        edit_border = sm.get_color_str(ColorRole.EDIT_BOX_BORDER)
        tertiary = sm.get_color_str(ColorRole.BACKGROUND_TERTIARY)
        conflict = sm.get_color_str(ColorRole.VCS_DELETED)
        destructive = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)
        destructive_hover = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER)
        arrow_right = sm.get_icon_path("arrow-right")
        arrow_down = sm.get_icon_path("arrow-down")

        self.setStyleSheet(f"""
            QWidget#VCSSidebar {{
                background-color: {background};
                color: {text};
            }}
            QLabel#_status_label {{ color: {text_dim}; }}
            QFrame#_divider {{ background-color: {divider}; border: none; }}
            QWidget#_merge_banner {{
                background-color: {tertiary};
                border-left: 3px solid {conflict};
            }}
            QLabel#_merge_label {{ color: {text}; background: transparent; }}
            QToolButton#_abort_button {{
                background-color: {destructive};
                color: {text};
                border: none;
                border-radius: 4px;
                padding: 3px 10px;
            }}
            QToolButton#_abort_button:hover {{ background-color: {destructive_hover}; }}
            QToolButton#_continue_button {{
                background-color: {button_bg};
                color: {text};
                border: none;
                border-radius: 4px;
                padding: 3px 10px;
            }}
            QToolButton#_continue_button:hover {{ background-color: {button_hover}; }}
            QTreeWidget#_staged_tree, QTreeWidget#_changes_tree {{
                background-color: {background};
                color: {text};
                border: none;
                outline: none;
            }}
            QTreeWidget#_staged_tree::branch, QTreeWidget#_changes_tree::branch {{
                background-color: {background};
            }}
            QTreeWidget#_staged_tree::branch:closed:has-children,
            QTreeWidget#_changes_tree::branch:closed:has-children {{
                image: url("{arrow_right}");
            }}
            QTreeWidget#_staged_tree::branch:open:has-children,
            QTreeWidget#_changes_tree::branch:open:has-children {{
                image: url("{arrow_down}");
            }}
            QToolButton#_hdrbtn {{
                background-color: transparent;
                border: none;
                border-radius: 4px;
            }}
            QToolButton#_hdrbtn:hover {{ background-color: {button_hover}; }}
            QToolButton#_hdrbtn:pressed {{ background-color: {button_pressed}; }}
            QToolButton#_toolbtn {{
                background-color: transparent;
                border: none;
                border-radius: 4px;
                padding: 4px 6px;
                color: {text};
            }}
            QToolButton#_toolbtn:hover {{ background-color: {button_hover}; }}
            QToolButton#_toolbtn:pressed {{ background-color: {button_pressed}; }}
            QToolButton#_toolbtn:disabled {{ color: {text_dim}; }}
            QToolButton#_branch_btn {{
                background-color: {button_bg};
                border: 1px solid {edit_border};
                border-radius: 4px;
                padding: 4px 8px;
                color: {text};
                text-align: left;
            }}
            QToolButton#_branch_btn:hover {{ background-color: {button_hover}; }}
            QToolButton#_branch_btn:disabled {{ color: {text_dim}; }}
            QToolButton#_commit_button {{
                background-color: {commit_bg};
                color: {text};
                border: none;
                padding: 5px 8px;
                border-radius: 4px;
            }}
            QToolButton#_commit_button:hover {{ background-color: {commit_hover}; }}
            QToolButton#_commit_button:pressed {{ background-color: {commit_pressed}; }}
            QToolButton#_commit_button:disabled {{ background-color: {button_disabled}; }}
            QToolButton#_commit_button::menu-button {{
                border: none;
                width: 16px;
            }}
            QPlainTextEdit#_commit_message {{
                background-color: {edit_bg};
                border: 1px solid {edit_border};
                border-radius: 4px;
                padding: 4px;
                color: {text};
            }}
            QLabel#_empty_title {{ color: {text}; font-weight: bold; background: transparent; }}
            QLabel#_empty_message {{ color: {text_dim}; background: transparent; }}
            QToolButton#_empty_button {{
                background-color: {button_bg};
                color: {text};
                border: 1px solid {edit_border};
                border-radius: 4px;
                padding: 6px 10px;
            }}
            QToolButton#_empty_button:hover {{ background-color: {button_hover}; }}
            QToolButton#_empty_button:pressed {{ background-color: {button_pressed}; }}
        """)

    # -- Poller callbacks ----------------------------------------------------

    def _on_repo_state_changed(self, has_repo: bool) -> None:
        """Swap between the source-control view and the no-repo empty state."""
        if not has_repo:
            self._current_status = []
            self._upstream = None
            self._staged_tree.clear()
            self._changes_tree.clear()
            self._conflicts_tree.clear()
            self._conflicts_header.setVisible(False)
            self._conflicts_tree.setVisible(False)
            self._merge_banner.setVisible(False)
            self._mediator.set_repo_root("")

        else:
            self._mediator.set_repo_root(self._poller.repo_root())
            self._mediator.refresh_info()

        # The panel stays reachable whenever a mindspace is open (driven by
        # set_mindspace); here we only choose which view fills it.
        self._repo_content.setVisible(has_repo)
        self._empty_state.setVisible(not has_repo)

    def _build_empty_state(self) -> QWidget:
        """Create the no-repository placeholder offering Initialize / Clone."""
        strings = self._language_manager.strings()
        container = QWidget(self)
        box = QVBoxLayout(container)
        box.setContentsMargins(16, 24, 16, 24)
        box.setSpacing(10)
        box.addStretch(1)

        self._empty_title = QLabel(strings.git_no_repo_title, container)
        self._empty_title.setObjectName("_empty_title")
        self._empty_title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._empty_title.setWordWrap(True)
        box.addWidget(self._empty_title)

        self._empty_message = QLabel(strings.git_no_repo_message, container)
        self._empty_message.setObjectName("_empty_message")
        self._empty_message.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._empty_message.setWordWrap(True)
        box.addWidget(self._empty_message)

        self._init_repo_button = QToolButton(container)
        self._init_repo_button.setObjectName("_empty_button")
        self._init_repo_button.setText(strings.git_init_repo)
        self._init_repo_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._init_repo_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._init_repo_button.clicked.connect(self._on_init_repo_clicked)
        box.addWidget(self._init_repo_button)

        self._clone_repo_button = QToolButton(container)
        self._clone_repo_button.setObjectName("_empty_button")
        self._clone_repo_button.setText(strings.git_clone_repo)
        self._clone_repo_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._clone_repo_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._clone_repo_button.clicked.connect(self._on_clone_repo_clicked)
        box.addWidget(self._clone_repo_button)

        box.addStretch(2)
        return container

    def _on_init_repo_clicked(self) -> None:
        """Initialise a git repository in the current mindspace folder."""
        if self._mindspace_path:
            self._mediator.init_repository(self._mindspace_path)

    def _on_clone_repo_clicked(self) -> None:
        """Prompt for a URL and clone it into the current mindspace folder."""
        if not self._mindspace_path:
            return

        strings = self._language_manager.strings()
        url, ok = QInputDialog.getText(self, strings.git_clone_title, strings.git_clone_url_prompt)
        if ok and url.strip():
            self._mediator.clone_repository(url.strip(), self._mindspace_path)

    def _on_status_changed(self, status: list[VCSFileStatus]) -> None:
        """Update the trees when git status changes."""
        self._mediator.set_repo_root(self._poller.repo_root())
        self._current_status = status
        self._rebuild_trees()
        self._update_headers()
        self._update_merge_banner()
        self._mediator.refresh_info()

    def _update_merge_banner(self) -> None:
        """Show or hide the merge/rebase banner based on repository state."""
        repo = self._poller.repo_root()
        self._merge_state = self._poller.merge_state(repo) if repo else MergeState.NONE
        strings = self._language_manager.strings()

        if self._merge_state == MergeState.MERGE:
            self._merge_label.setText(strings.git_merge_in_progress)

        elif self._merge_state == MergeState.REBASE:
            self._merge_label.setText(strings.git_rebase_in_progress)

        # "Continue" only applies to a rebase; a merge completes with a commit.
        self._continue_button.setVisible(self._merge_state == MergeState.REBASE)
        self._merge_banner.setVisible(self._merge_state != MergeState.NONE)

    def _on_abort_clicked(self) -> None:
        """Confirm and abort the in-progress merge or rebase."""
        if self._merge_state == MergeState.NONE:
            return

        strings = self._language_manager.strings()
        kind = "merge" if self._merge_state == MergeState.MERGE else "rebase"
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING,
            strings.git_confirm_abort_title,
            strings.git_confirm_abort_message.format(kind),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )
        if result != MessageBoxButton.YES:
            return

        if self._merge_state == MergeState.MERGE:
            self._mediator.abort_merge()

        else:
            self._mediator.abort_rebase()

    def _on_repositories_changed(self, repos: list[str]) -> None:
        """Repopulate the repository selector when the discovered set changes."""
        # Apply a remembered repository selection once per mindspace.
        active = self._poller.active_repo()
        if not self._preferred_applied and self._preferred_repo in repos:
            self._preferred_applied = True
            active = self._preferred_repo
            if active != self._poller.active_repo():
                self._poller.set_active_repo(active)

        self._repo_combo.blockSignals(True)
        self._repo_combo.set_items([(self._repo_display(repo), repo) for repo in repos])
        if active:
            self._repo_combo.set_value(active)

        self._repo_combo.blockSignals(False)
        self._combo_container.setVisible(len(repos) > 1)

    def _on_repo_selected(self) -> None:
        """Handle the user picking a repository from the selector."""
        repo = self._repo_combo.get_value()
        if repo:
            self._preferred_applied = True
            self._poller.set_active_repo(repo)
            self._save_preferred_repo(repo)

    def _repo_display(self, repo_root: str) -> str:
        """Return a mindspace-relative label for a repository root."""
        if self._mindspace_path:
            try:
                rel = os.path.relpath(repo_root, self._mindspace_path)
                return rel if rel != "." else os.path.basename(repo_root)

            except ValueError:
                pass

        return os.path.basename(repo_root) or repo_root

    # -- Mediator callbacks --------------------------------------------------

    def _on_operation_started(self, _op_name: str) -> None:
        """Lock controls while an operation runs; reveal progress only if slow."""
        self._status_label.setText(self._language_manager.strings().git_working)
        self._status_timer.start()
        for widget in self._action_widgets:
            widget.setEnabled(False)

    def _set_idle(self) -> None:
        """Hide progress and re-enable controls."""
        self._status_timer.stop()
        self._status_label.setVisible(False)
        for widget in self._action_widgets:
            widget.setEnabled(True)

    def _on_operation_succeeded(self, op_name: str) -> None:
        """Refresh state after a successful git operation."""
        self._set_idle()
        if op_name in ("commit", "commit_push"):
            self._commit_message.clear()

        self._poller.force_refresh()
        self._mediator.refresh_info()

    def _on_operation_failed(self, _op_name: str, message: str) -> None:
        """Surface a git operation failure to the user."""
        self._set_idle()
        strings = self._language_manager.strings()

        if message == "Another git operation is already running":
            MessageBox.show_message(
                self, MessageBoxType.INFORMATION,
                strings.git_busy_title, strings.git_busy_message,
                [MessageBoxButton.OK]
            )
            return

        if self._looks_like_auth_error(message):
            MessageBox.show_message(
                self, MessageBoxType.CRITICAL,
                strings.git_auth_error_title,
                f"{strings.git_auth_error_hint}\n\n{message}",
                [MessageBoxButton.OK]
            )
            return

        MessageBox.show_message(
            self, MessageBoxType.CRITICAL,
            strings.git_error_title, message,
            [MessageBoxButton.OK]
        )

    @staticmethod
    def _looks_like_auth_error(message: str) -> bool:
        """Heuristically detect a remote-authentication failure from git's stderr."""
        lowered = message.lower()
        markers = (
            "authentication failed", "permission denied", "publickey",
            "could not read username", "could not read password",
            "host key verification failed", "invalid username or password",
            "terminal prompts disabled", "access denied", "authentication required",
        )
        return any(marker in lowered for marker in markers)

    def _on_repo_info_changed(self, branch_info: BranchInfo, upstream: UpstreamStatus | None) -> None:
        """Update the branch button and push/pull affordances."""
        self._branch_info = branch_info
        self._upstream = upstream
        self._update_toolbar(branch_info, upstream)

    def _update_toolbar(self, branch_info: BranchInfo, upstream: UpstreamStatus | None) -> None:
        """Refresh the branch label and ahead/behind counts on the toolbar."""
        strings = self._language_manager.strings()
        ahead = upstream.ahead if upstream else 0
        behind = upstream.behind if upstream else 0

        self._branch_button.setText(branch_info.current or "detached")

        # Pull/push are icon buttons; the ahead/behind counts ride beside the icon.
        self._pull_button.setText(str(behind) if behind else "")
        self._push_button.setText(str(ahead) if ahead else "")
        self._pull_button.setToolTip(
            f"{strings.git_pull} ({behind})" if behind else strings.git_pull
        )
        self._push_button.setToolTip(
            f"{strings.git_push} ({ahead})" if ahead else strings.git_push
        )

    # -- Tree rendering ------------------------------------------------------

    def _rebuild_trees(self) -> None:
        """Rebuild the conflict/staged/changes trees from the current status."""
        conflicts: list[tuple[str, VCSStatusCode]] = []
        staged: list[tuple[str, VCSStatusCode]] = []
        changes: list[tuple[str, VCSStatusCode]] = []

        for entry in self._current_status:
            if entry.is_conflicted:
                conflicts.append((entry.path, VCSStatusCode.CONFLICTED))
                continue

            if entry.is_staged() and entry.index_code is not None:
                staged.append((entry.path, entry.index_code))

            if entry.is_unstaged() and entry.worktree_code is not None:
                changes.append((entry.path, entry.worktree_code))

        self._populate_tree(self._conflicts_tree, conflicts)
        self._populate_tree(self._staged_tree, staged)
        self._populate_tree(self._changes_tree, changes)

        has_conflicts = bool(conflicts)
        self._conflicts_header.setVisible(has_conflicts)
        self._conflicts_tree.setVisible(has_conflicts)

    def _repo_base(self) -> str:
        """Return the base path used to compute display-relative paths."""
        return self._poller.repo_root() or self._mindspace_path

    def _populate_tree(self, tree: _VCSTree, entries: list[tuple[str, VCSStatusCode]]) -> None:
        """Rebuild *tree* as a folder hierarchy from (abs_path, code) entries."""
        tree.clear()
        base = self._repo_base()
        folder_nodes: dict[str, QTreeWidgetItem] = {}

        for abs_path, code in sorted(entries, key=lambda e: self._rel_path(e[0]).lower()):
            rel = self._rel_path(abs_path)
            parts = rel.split(os.sep)

            parent_item: QTreeWidgetItem | None = None
            cumulative = ""
            for part in parts[:-1]:
                cumulative = os.path.join(cumulative, part) if cumulative else part
                node = folder_nodes.get(cumulative)
                if node is None:
                    node = self._make_folder_item(os.path.join(base, cumulative), part)
                    if parent_item is None:
                        tree.addTopLevelItem(node)

                    else:
                        parent_item.addChild(node)

                    folder_nodes[cumulative] = node

                parent_item = node

            leaf = self._make_file_item(abs_path, parts[-1], code)
            if parent_item is None:
                tree.addTopLevelItem(leaf)

            else:
                parent_item.addChild(leaf)

        tree.expandAll()

    def _make_folder_item(self, abs_dir: str, name: str) -> QTreeWidgetItem:
        """Create a collapsible folder node with a themed folder icon."""
        item = QTreeWidgetItem([name])
        item.setIcon(0, self._folder_icon(abs_dir, expanded=False))
        item.setData(0, _PATH_ROLE, abs_dir)
        item.setData(0, KIND_ROLE, "folder")
        item.setToolTip(0, abs_dir)
        return item

    def _make_file_item(self, abs_path: str, name: str, code: VCSStatusCode) -> QTreeWidgetItem:
        """Create a file leaf with a themed file icon and status badge."""
        item = QTreeWidgetItem([name])
        item.setIcon(0, self._icon_provider.icon(QFileInfo(abs_path)))
        item.setData(0, _PATH_ROLE, abs_path)
        item.setData(0, KIND_ROLE, "file")
        item.setToolTip(0, abs_path)

        color = self._color_for_code(code)
        item.setForeground(0, color)
        item.setData(0, BADGE_ROLE, _STATUS_LABELS.get(code, "?"))
        item.setData(0, BADGE_COLOR_ROLE, color)
        return item

    def _folder_icon(self, abs_dir: str, expanded: bool) -> QIcon:
        """Return the open or closed themed folder icon."""
        if expanded:
            return self._icon_provider.open_folder_icon()

        if os.path.isdir(abs_dir):
            return self._icon_provider.icon(QFileInfo(abs_dir))

        return self._icon_provider.open_folder_icon()

    def _on_item_expanded(self, item: QTreeWidgetItem) -> None:
        """Swap to the open-folder icon when a folder expands."""
        if item.data(0, KIND_ROLE) == "folder":
            item.setIcon(0, self._folder_icon(item.data(0, _PATH_ROLE), expanded=True))

    def _on_item_collapsed(self, item: QTreeWidgetItem) -> None:
        """Swap to the closed-folder icon when a folder collapses."""
        if item.data(0, KIND_ROLE) == "folder":
            item.setIcon(0, self._folder_icon(item.data(0, _PATH_ROLE), expanded=False))

    # -- Section headers -----------------------------------------------------

    def _update_headers(self) -> None:
        """Update section titles with a collapse chevron and file counts."""
        strings = self._language_manager.strings()
        staged_n = sum(1 for e in self._current_status if e.is_staged())
        changes_n = sum(1 for e in self._current_status if e.is_unstaged())

        staged_chevron = "▸" if self._staged_collapsed else "▾"
        changes_chevron = "▸" if self._changes_collapsed else "▾"
        self._staged_header.set_title(f"{staged_chevron}  {strings.git_staged_changes}  ({staged_n})")
        self._changes_header.set_title(f"{changes_chevron}  {strings.git_changes}  ({changes_n})")

    def _toggle_staged(self) -> None:
        """Collapse or expand the staged section."""
        self._staged_collapsed = not self._staged_collapsed
        self._staged_tree.setVisible(not self._staged_collapsed)
        self._update_headers()

    def _toggle_changes(self) -> None:
        """Collapse or expand the changes section."""
        self._changes_collapsed = not self._changes_collapsed
        self._changes_tree.setVisible(not self._changes_collapsed)
        self._update_headers()

    # -- Row / bulk actions --------------------------------------------------

    def _apply_row_action(self, action: str, path: str, kind: str) -> None:
        """Handle an inline hover-action click on a tree row."""
        if not path:
            return

        if kind == "folder":
            paths = self._files_under(path, staged=action == "unstage")

        else:
            paths = [path]

        if action == "stage":
            self._mediator.stage(paths)

        elif action == "unstage":
            self._mediator.unstage(paths)

        elif action == "discard":
            self._handle_discard(paths)

    def _on_stage_all(self) -> None:
        """Stage every unstaged file."""
        paths = [e.path for e in self._current_status if e.is_unstaged()]
        if paths:
            self._mediator.stage(paths)

    def _on_unstage_all(self) -> None:
        """Unstage every staged file."""
        paths = [e.path for e in self._current_status if e.is_staged()]
        if paths:
            self._mediator.unstage(paths)

    def _files_under(self, dir_path: str, staged: bool) -> list[str]:
        """Return changed file paths beneath *dir_path* in the given section."""
        dir_norm = os.path.normpath(dir_path)
        result: list[str] = []
        for entry in self._current_status:
            path = os.path.normpath(entry.path)
            if path == dir_norm or path.startswith(dir_norm + os.sep):
                if (staged and entry.is_staged()) or (not staged and entry.is_unstaged()):
                    result.append(entry.path)

        return result

    # -- Context menu --------------------------------------------------------

    def _show_context_menu(self, tree: _VCSTree, position: QPoint) -> None:
        """Show a context menu for the item under the cursor."""
        item = tree.itemAt(position)
        if item is None:
            return

        kind = item.data(0, KIND_ROLE)

        if tree is self._conflicts_tree:
            self._show_conflict_menu(tree, position, item, kind)
            return

        is_staged_tree = tree is self._staged_tree

        if kind == "folder":
            paths = self._files_under(item.data(0, _PATH_ROLE), staged=is_staged_tree)
            self._show_group_menu(tree, position, paths, is_staged_tree)
            return

        path = item.data(0, _PATH_ROLE)
        if path:
            self._show_file_menu(tree, position, path, is_staged_tree)

    def _show_file_menu(self, tree: _VCSTree, position: QPoint, path: str, is_staged_tree: bool) -> None:
        """Context menu for a single changed file."""
        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        entry = self._status_for(path)

        if is_staged_tree:
            menu.addAction(strings.git_unstage).triggered.connect(lambda: self._mediator.unstage([path]))
            if entry is not None and entry.index_code == VCSStatusCode.MODIFIED:
                menu.addAction(strings.git_unstage_hunks).triggered.connect(
                    lambda: self._unstage_hunks(path)
                )

        else:
            menu.addAction(strings.git_stage).triggered.connect(lambda: self._mediator.stage([path]))
            if entry is not None and entry.worktree_code == VCSStatusCode.MODIFIED:
                menu.addAction(strings.git_stage_hunks).triggered.connect(
                    lambda: self._stage_hunks(path)
                )
                menu.addAction(strings.git_discard_hunks).triggered.connect(
                    lambda: self._discard_hunks(path)
                )

            menu.addAction(strings.git_discard).triggered.connect(lambda: self._handle_discard([path]))

        menu.addSeparator()
        menu.addAction(strings.open_in_diff).triggered.connect(
            lambda: self.file_opened_in_diff.emit(path, False)
        )

        if os.path.exists(path):
            edit_action = menu.addAction(strings.open_in_editor)
            edit_action.setEnabled(not is_binary_image_file(path))
            edit_action.triggered.connect(lambda: self.file_opened_in_editor.emit(path, False))
            menu.addAction(strings.open_in_preview).triggered.connect(
                lambda: self.file_opened_in_preview.emit(path)
            )

        # History / blame apply to tracked files only.
        if entry is not None and entry.worktree_code != VCSStatusCode.UNTRACKED:
            menu.addSeparator()
            menu.addAction(strings.git_file_history).triggered.connect(
                lambda: self._open_file_history(path)
            )
            menu.addAction(strings.git_blame).triggered.connect(lambda: self._open_blame(path))

        # Ignore is only meaningful for untracked files.
        if entry is not None and entry.worktree_code == VCSStatusCode.UNTRACKED:
            menu.addSeparator()
            menu.addAction(strings.git_add_gitignore).triggered.connect(
                lambda: self._mediator.add_to_gitignore(self._rel_path(path))
            )

        menu.exec_(tree.viewport().mapToGlobal(position))

    def _open_file_history(self, path: str) -> None:
        """Open the single-file history dialog."""
        FileHistoryDialog(self._poller.repo_root(), path, self._style_manager, self).exec()

    def _open_blame(self, path: str) -> None:
        """Open the blame dialog for a file."""
        BlameDialog(self._poller.repo_root(), path, self._style_manager, self).exec()

    def _show_group_menu(
        self, tree: _VCSTree, position: QPoint, paths: list[str], is_staged_tree: bool
    ) -> None:
        """Context menu for a folder, acting on all files beneath it."""
        if not paths:
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        if is_staged_tree:
            menu.addAction(strings.git_unstage_all).triggered.connect(
                lambda: self._mediator.unstage(paths)
            )

        else:
            menu.addAction(strings.git_stage_all).triggered.connect(
                lambda: self._mediator.stage(paths)
            )
            menu.addAction(strings.git_discard).triggered.connect(
                lambda: self._handle_discard(paths)
            )

        menu.exec_(tree.viewport().mapToGlobal(position))

    def _show_conflict_menu(
        self, tree: _VCSTree, position: QPoint, item: QTreeWidgetItem, kind: str
    ) -> None:
        """Context menu for a conflicted file or folder."""
        if kind == "folder":
            paths = self._conflicted_under(item.data(0, _PATH_ROLE))

        else:
            path = item.data(0, _PATH_ROLE)
            paths = [path] if path else []

        if not paths:
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        menu.addAction(strings.git_accept_ours).triggered.connect(
            lambda: self._mediator.accept_ours(paths)
        )
        menu.addAction(strings.git_accept_theirs).triggered.connect(
            lambda: self._mediator.accept_theirs(paths)
        )
        menu.addAction(strings.git_mark_resolved).triggered.connect(
            lambda: self._mediator.mark_resolved(paths)
        )

        if kind == "file" and os.path.exists(paths[0]):
            menu.addSeparator()
            edit_action = menu.addAction(strings.open_in_editor)
            edit_action.setEnabled(not is_binary_image_file(paths[0]))
            edit_action.triggered.connect(lambda: self.file_opened_in_editor.emit(paths[0], False))
            menu.addAction(strings.open_in_diff).triggered.connect(
                lambda: self.file_opened_in_diff.emit(paths[0], False)
            )

        menu.exec_(tree.viewport().mapToGlobal(position))

    def _conflicted_under(self, dir_path: str) -> list[str]:
        """Return conflicted file paths beneath *dir_path*."""
        dir_norm = os.path.normpath(dir_path)
        result: list[str] = []
        for entry in self._current_status:
            if not entry.is_conflicted:
                continue

            path = os.path.normpath(entry.path)
            if path == dir_norm or path.startswith(dir_norm + os.sep):
                result.append(entry.path)

        return result

    def _stage_hunks(self, path: str) -> None:
        """Open the hunk picker and stage the selected hunks of a file."""
        self._pick_and_apply_hunks(
            path, staged=False,
            title=self._language_manager.strings().git_stage_hunks_title,
            apply_fn=self._mediator.stage_hunk,
        )

    def _unstage_hunks(self, path: str) -> None:
        """Open the hunk picker and unstage the selected hunks of a file."""
        self._pick_and_apply_hunks(
            path, staged=True,
            title=self._language_manager.strings().git_unstage_hunks_title,
            apply_fn=self._mediator.unstage_hunk,
        )

    def _discard_hunks(self, path: str) -> None:
        """Open the hunk picker and discard the selected hunks from the working tree."""
        self._pick_and_apply_hunks(
            path, staged=False,
            title=self._language_manager.strings().git_discard_hunks_title,
            apply_fn=self._mediator.discard_hunk,
        )

    def _pick_and_apply_hunks(self, path: str, staged: bool, title: str, apply_fn: Callable[[str], None]) -> None:
        """Fetch a file's hunks, let the user choose, then apply the patch."""
        repo = self._poller.repo_root()
        if not repo:
            return

        try:
            diff = (get_staged_file_diff if staged else get_unstaged_file_diff)(repo, path)
            header, hunks = split_file_diff(diff)

        except Exception as e:  # pylint: disable=broad-except
            self._logger.debug("Failed to load hunks for '%s': %s", path, e)
            return

        if not hunks:
            return

        indices = HunkSelectionDialog.choose(title, hunks, self._style_manager, self)
        if not indices:
            return

        patch = header + "".join(hunks[i] for i in indices)
        apply_fn(patch)

    def _handle_discard(self, paths: list[str]) -> None:
        """Confirm and discard changes to one or more files."""
        strings = self._language_manager.strings()
        detail = os.path.basename(paths[0]) if len(paths) == 1 else f"{len(paths)} files"

        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.git_confirm_discard_title,
            strings.git_confirm_discard_message.format(detail),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )

        if result != MessageBoxButton.YES:
            return

        tracked: list[str] = []
        for path in paths:
            entry = self._status_for(path)
            if entry is not None and entry.worktree_code == VCSStatusCode.UNTRACKED:
                self._delete_untracked(path)

            else:
                tracked.append(path)

        if tracked:
            self._mediator.discard(tracked)

    def _delete_untracked(self, path: str) -> None:
        """Delete an untracked file from disk and notify listeners."""
        strings = self._language_manager.strings()
        try:
            os.remove(path)
            # Notify listeners only after the file is actually gone, so a
            # listener (e.g. closing an editor tab) can't re-save it or react to
            # a deletion that failed.
            self.file_deleted.emit(path)
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO, f"User discarded untracked file '{path}'"
            )
            self._poller.force_refresh()

        except FileNotFoundError:
            self._poller.force_refresh()

        except OSError as e:
            self._logger.error("Failed to delete file '%s': %s", path, str(e))
            MessageBox.show_message(
                self, MessageBoxType.CRITICAL,
                strings.git_error_title, str(e),
                [MessageBoxButton.OK]
            )

    # -- Toolbar / commit actions --------------------------------------------

    def _show_branch_menu(self) -> None:
        """Show the branch switch / create menu."""
        if not self._mindspace_path or not self._poller.has_repo():
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        try:
            info = get_branch_info(self._poller.repo_root())

        except Exception as e:  # pylint: disable=broad-except
            self._logger.debug("Failed to list branches: %s", e)
            info = self._branch_info

        for name in info.branches:
            is_current = name == info.current
            submenu = self._style_manager.add_submenu(menu, ("● " if is_current else "    ") + name)

            switch_action = submenu.addAction(strings.git_switch)
            switch_action.setEnabled(not is_current)
            switch_action.triggered.connect(lambda _c=False, n=name: self._mediator.switch_branch(n))

            merge_action = submenu.addAction(strings.git_merge_branch)
            merge_action.setEnabled(not is_current)
            merge_action.triggered.connect(lambda _c=False, n=name: self._mediator.merge_branch(n))

            rebase_action = submenu.addAction(strings.git_rebase_onto)
            rebase_action.setEnabled(not is_current)
            rebase_action.triggered.connect(lambda _c=False, n=name: self._mediator.rebase(n))

            submenu.addAction(strings.git_rename_branch).triggered.connect(
                lambda _c=False, n=name: self._handle_rename_branch(n)
            )

            delete_action = submenu.addAction(strings.git_delete_branch)
            delete_action.setEnabled(not is_current)
            delete_action.triggered.connect(lambda _c=False, n=name: self._handle_delete_branch(n))

        # Remote branches submenu (checkout as a local tracking branch).
        remote_menu = self._style_manager.add_submenu(menu, strings.git_remote_branches)
        try:
            remote_branches = list_remote_branches(self._poller.repo_root())

        except Exception as e:  # pylint: disable=broad-except
            self._logger.debug("Failed to list remote branches: %s", e)
            remote_branches = []

        if not remote_branches:
            remote_menu.addAction(strings.git_no_remote_branches).setEnabled(False)

        else:
            for rb in remote_branches:
                action = remote_menu.addAction(rb)
                action.triggered.connect(
                    lambda _c=False, r=rb: self._mediator.checkout_remote_branch(r)
                )

        menu.addSeparator()
        menu.addAction(strings.git_create_branch).triggered.connect(self._handle_create_branch)

        menu.exec_(self._branch_button.mapToGlobal(self._branch_button.rect().bottomLeft()))

    def _handle_rename_branch(self, name: str) -> None:
        """Prompt for a new name and rename a branch."""
        strings = self._language_manager.strings()
        new_name, ok = QInputDialog.getText(
            self, strings.git_rename_branch_title, strings.git_rename_branch_prompt, text=name
        )
        if ok and new_name.strip():
            self._mediator.rename_branch(name, new_name.strip())

    def _handle_delete_branch(self, name: str) -> None:
        """Confirm and delete a branch."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING,
            strings.git_confirm_delete_branch_title,
            strings.git_confirm_delete_branch_message.format(name),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )
        if result == MessageBoxButton.YES:
            self._mediator.delete_branch(name)

    def _handle_create_branch(self) -> None:
        """Prompt for a new branch name and create it."""
        strings = self._language_manager.strings()
        name, ok = QInputDialog.getText(
            self, strings.git_create_branch_title, strings.git_create_branch_prompt
        )
        if ok and name.strip():
            self._mediator.create_branch(name.strip())

    def _on_push_clicked(self) -> None:
        """Push, prompting to set upstream when the branch has none."""
        if self._upstream is not None and self._upstream.upstream is None:
            strings = self._language_manager.strings()
            branch = self._branch_info.current
            if not branch and self._poller.has_repo():
                try:
                    branch = get_current_branch(self._poller.repo_root())

                except Exception:  # pylint: disable=broad-except
                    branch = ""

            result = MessageBox.show_message(
                self, MessageBoxType.QUESTION,
                strings.git_set_upstream_title,
                strings.git_set_upstream_message.format(branch),
                [MessageBoxButton.YES, MessageBoxButton.NO],
                True
            )
            if result == MessageBoxButton.YES:
                self._mediator.push(set_upstream=True)

            return

        self._mediator.push()

    def _on_refresh_clicked(self) -> None:
        """Poll git immediately for new changes and refresh branch/upstream info."""
        self._poller.force_refresh()
        self._mediator.refresh_info()

    def _on_history_clicked(self) -> None:
        """Request the commit-history view for the active repository."""
        repo = self._poller.repo_root()
        if repo:
            self.history_requested.emit(repo)

    def _show_more_menu(self) -> None:
        """Show the overflow menu with stash actions."""
        if not self._poller.has_repo():
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        menu.addAction(strings.git_stash_changes).triggered.connect(
            lambda _checked=False: self._mediator.stash_push()
        )
        menu.addAction(strings.git_stash_with_message).triggered.connect(
            lambda _checked=False: self._handle_stash_with_message()
        )

        try:
            stashes = stash_list(self._poller.repo_root())

        except Exception as e:  # pylint: disable=broad-except
            self._logger.debug("Failed to list stashes: %s", e)
            stashes = []

        menu.addSeparator()
        if not stashes:
            empty = menu.addAction(strings.git_no_stashes)
            empty.setEnabled(False)

        else:
            for entry in stashes:
                submenu = self._style_manager.add_submenu(menu, f"{entry.ref}: {entry.message}")
                submenu.addAction(strings.git_stash_pop).triggered.connect(
                    lambda _c=False, r=entry.ref: self._mediator.stash_pop(r)
                )
                submenu.addAction(strings.git_stash_apply).triggered.connect(
                    lambda _c=False, r=entry.ref: self._mediator.stash_apply(r)
                )
                submenu.addAction(strings.git_stash_drop).triggered.connect(
                    lambda _c=False, r=entry.ref, m=entry.message: self._handle_stash_drop(r, m)
                )

        menu.addSeparator()
        self._build_tags_menu(self._style_manager.add_submenu(menu, strings.git_tags))
        self._build_remotes_menu(self._style_manager.add_submenu(menu, strings.git_remotes))

        menu.addSeparator()
        menu.addAction(strings.git_pull_rebase).triggered.connect(
            lambda: self._mediator.pull(use_rebase=True)
        )
        menu.addAction(strings.git_push_tags).triggered.connect(self._mediator.push_tags)
        menu.addAction(strings.git_force_push).triggered.connect(self._handle_force_push)

        menu.addSeparator()
        menu.addAction(strings.git_set_identity).triggered.connect(self._handle_set_identity)
        menu.addAction(strings.git_set_token).triggered.connect(self._handle_set_token)

        menu.addSeparator()
        menu.addAction(strings.git_discard_all).triggered.connect(self._handle_discard_all)
        menu.addAction(strings.git_clean_untracked).triggered.connect(self._handle_clean_untracked)

        menu.exec_(self._more_button.mapToGlobal(self._more_button.rect().bottomLeft()))

    def _handle_force_push(self) -> None:
        """Confirm and force-push (with lease)."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING, strings.git_force_push,
            strings.git_confirm_force_push_message,
            [MessageBoxButton.YES, MessageBoxButton.NO], True
        )
        if result == MessageBoxButton.YES:
            self._mediator.push(force=True)

    def _handle_discard_all(self) -> None:
        """Confirm and discard all tracked changes."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING, strings.git_discard_all,
            strings.git_confirm_discard_all_message,
            [MessageBoxButton.YES, MessageBoxButton.NO], True
        )
        if result == MessageBoxButton.YES:
            self._mediator.discard_all_changes()

    def _handle_clean_untracked(self) -> None:
        """Confirm and remove all untracked files."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING, strings.git_clean_untracked,
            strings.git_confirm_clean_message,
            [MessageBoxButton.YES, MessageBoxButton.NO], True
        )
        if result == MessageBoxButton.YES:
            self._mediator.clean_untracked()

    def _handle_set_token(self) -> None:
        """Prompt for a username and access token, then store it securely."""
        strings = self._language_manager.strings()
        if not self._poller.repo_root():
            return

        username, ok = QInputDialog.getText(
            self, strings.git_token_title, strings.git_token_username_prompt
        )
        if not ok or not username.strip():
            return

        token, ok = QInputDialog.getText(
            self, strings.git_token_title, strings.git_token_prompt,
            echo=QLineEdit.EchoMode.Password
        )
        if not ok or not token.strip():
            return

        self._mediator.set_credential(username.strip(), token.strip())

    def _handle_set_identity(self) -> None:
        """Prompt for and set the repository-local commit identity."""
        strings = self._language_manager.strings()
        repo = self._poller.repo_root()
        if not repo:
            return

        try:
            name, email = get_identity(repo)

        except Exception as e:  # pylint: disable=broad-except
            self._logger.debug("Failed to read identity: %s", e)
            name, email = "", ""

        new_name, ok = QInputDialog.getText(
            self, strings.git_identity_title, strings.git_identity_name_prompt, text=name
        )
        if not ok:
            return

        new_email, ok = QInputDialog.getText(
            self, strings.git_identity_title, strings.git_identity_email_prompt, text=email
        )
        if not ok:
            return

        self._mediator.set_identity(new_name.strip(), new_email.strip())

    def _build_tags_menu(self, menu: QMenu) -> None:
        """Populate the Tags submenu (create + list/delete)."""
        strings = self._language_manager.strings()

        menu.addAction(strings.git_new_tag).triggered.connect(self._handle_create_tag)
        menu.addSeparator()

        try:
            tags = list_tags(self._poller.repo_root())

        except Exception as e:  # pylint: disable=broad-except
            self._logger.debug("Failed to list tags: %s", e)
            tags = []

        if not tags:
            menu.addAction(strings.git_no_tags).setEnabled(False)
            return

        for tag in tags:
            submenu = self._style_manager.add_submenu(menu, tag)
            submenu.addAction(strings.git_delete_tag).triggered.connect(
                lambda _c=False, t=tag: self._handle_delete_tag(t)
            )

    def _build_remotes_menu(self, menu: QMenu) -> None:
        """Populate the Remotes submenu (add + list/remove)."""
        strings = self._language_manager.strings()

        menu.addAction(strings.git_add_remote).triggered.connect(self._handle_add_remote)
        menu.addSeparator()

        try:
            remotes = list_remotes(self._poller.repo_root())

        except Exception as e:  # pylint: disable=broad-except
            self._logger.debug("Failed to list remotes: %s", e)
            remotes = []

        if not remotes:
            menu.addAction(strings.git_no_remotes).setEnabled(False)
            return

        for remote in remotes:
            submenu = self._style_manager.add_submenu(menu, f"{remote.name}  ({remote.url})")
            submenu.addAction(strings.git_edit_remote_url).triggered.connect(
                lambda _c=False, n=remote.name, u=remote.url: self._handle_edit_remote_url(n, u)
            )
            submenu.addAction(strings.git_remove_remote).triggered.connect(
                lambda _c=False, n=remote.name: self._handle_remove_remote(n)
            )

    def _handle_create_tag(self) -> None:
        """Prompt for a tag name and create it on HEAD."""
        strings = self._language_manager.strings()
        name, ok = QInputDialog.getText(self, strings.git_new_tag_title, strings.git_new_tag_prompt)
        if ok and name.strip():
            self._mediator.create_tag(name.strip())

    def _handle_delete_tag(self, tag: str) -> None:
        """Confirm and delete a tag."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING,
            strings.git_delete_tag, strings.git_confirm_delete_tag_message.format(tag),
            [MessageBoxButton.YES, MessageBoxButton.NO], True
        )
        if result == MessageBoxButton.YES:
            self._mediator.delete_tag(tag)

    def _handle_add_remote(self) -> None:
        """Prompt for a remote name and URL, then add it."""
        strings = self._language_manager.strings()
        name, ok = QInputDialog.getText(
            self, strings.git_add_remote_title, strings.git_add_remote_name_prompt
        )
        if not ok or not name.strip():
            return

        url, ok = QInputDialog.getText(
            self, strings.git_add_remote_title, strings.git_add_remote_url_prompt
        )
        if ok and url.strip():
            self._mediator.add_remote(name.strip(), url.strip())

    def _handle_edit_remote_url(self, name: str, current_url: str) -> None:
        """Prompt for and set a new URL for a remote."""
        strings = self._language_manager.strings()
        url, ok = QInputDialog.getText(
            self, strings.git_edit_remote_title, strings.git_add_remote_url_prompt, text=current_url
        )
        if ok and url.strip():
            self._mediator.set_remote_url(name, url.strip())

    def _handle_remove_remote(self, name: str) -> None:
        """Confirm and remove a remote."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING,
            strings.git_remove_remote, strings.git_confirm_remove_remote_message.format(name),
            [MessageBoxButton.YES, MessageBoxButton.NO], True
        )
        if result == MessageBoxButton.YES:
            self._mediator.remove_remote(name)

    def _handle_stash_drop(self, ref: str, message: str) -> None:
        """Confirm and drop a stash entry."""
        strings = self._language_manager.strings()
        result = MessageBox.show_message(
            self, MessageBoxType.WARNING,
            strings.git_confirm_drop_title,
            strings.git_confirm_drop_message.format(message or ref),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True
        )
        if result == MessageBoxButton.YES:
            self._mediator.stash_drop(ref)

    def _handle_stash_with_message(self) -> None:
        """Prompt for a description, then stash the working tree under it."""
        strings = self._language_manager.strings()
        message, ok = QInputDialog.getText(
            self, strings.git_stash_changes, strings.git_stash_message_prompt
        )
        if ok and message.strip():
            self._mediator.stash_push(message.strip())

    def _head_commit_message(self) -> str:
        """Return HEAD's full commit message, or "" if unavailable."""
        repo = self._poller.repo_root()
        if not repo:
            return ""

        try:
            return get_head_message(repo)

        except GitError as e:
            self._logger.debug("Failed to read HEAD message: %s", e)
            return ""

    def _do_commit(self, push: bool = False, amend: bool = False) -> None:
        """Commit staged changes; optionally push, or amend the last commit."""
        message = self._commit_message.toPlainText().strip()
        strings = self._language_manager.strings()

        if amend and not message:
            # Amending with an empty box keeps the previous commit's message
            # rather than forcing the user to retype it.
            message = self._head_commit_message()

        if not amend and not any(e.is_staged() for e in self._current_status):
            MessageBox.show_message(
                self, MessageBoxType.INFORMATION,
                strings.git_no_staged_title, strings.git_no_staged_message,
                [MessageBoxButton.OK]
            )
            return

        if not message:
            self._commit_message.setFocus()
            return

        if push:
            self._mediator.commit_and_push(message, amend=amend)

        else:
            self._mediator.commit(message, amend=amend)

    # -- Item activation -----------------------------------------------------

    def _on_item_clicked(self, item: QTreeWidgetItem, _column: int) -> None:
        """Open an ephemeral diff tab when a file is single-clicked."""
        if item.data(0, KIND_ROLE) == "file":
            path = item.data(0, _PATH_ROLE)
            if path:
                self.file_opened_in_diff.emit(path, True)

    def _on_item_activated(self, item: QTreeWidgetItem, _column: int) -> None:
        """Open a persistent diff tab on double-click / keyboard activation."""
        if item.data(0, KIND_ROLE) == "file":
            path = item.data(0, _PATH_ROLE)
            if path:
                self.file_opened_in_diff.emit(path, False)

    # -- Persistence ---------------------------------------------------------

    def _state_path(self) -> str:
        """Path to the per-mindspace VCS state file."""
        return os.path.join(self._mindspace_path, ".humbug", "vcs_state.json")

    def _load_preferred_repo(self) -> str:
        """Load the last-selected repository for this mindspace, or ""."""
        if not self._mindspace_path:
            return ""

        try:
            with open(self._state_path(), encoding="utf-8") as f:
                return json.load(f).get("activeRepo", "")

        except (OSError, ValueError):
            return ""

    def _save_preferred_repo(self, repo_root: str) -> None:
        """Persist the selected repository for this mindspace."""
        if not self._mindspace_path:
            return

        try:
            os.makedirs(os.path.dirname(self._state_path()), exist_ok=True)
            with open(self._state_path(), "w", encoding="utf-8") as f:
                json.dump({"activeRepo": os.path.normpath(repo_root)}, f)

        except OSError as e:
            self._logger.debug("Failed to persist active repo: %s", e)

    # -- Helpers -------------------------------------------------------------

    def _status_for(self, path: str) -> VCSFileStatus | None:
        """Return the status entry for a path, or None."""
        normalized = os.path.normpath(path)
        for entry in self._current_status:
            if os.path.normpath(entry.path) == normalized:
                return entry

        return None

    def _rel_path(self, abs_path: str) -> str:
        """Convert an absolute path to a base-relative form for tree grouping."""
        base = self._repo_base()
        if base:
            try:
                return os.path.relpath(abs_path, base)

            except ValueError:
                pass

        return os.path.basename(abs_path)

    def _color_for_code(self, code: VCSStatusCode) -> QColor:
        """Return the theme-appropriate foreground colour for a status code."""
        if code in (VCSStatusCode.ADDED, VCSStatusCode.UNTRACKED):
            return self._style_manager.get_color(ColorRole.VCS_ADDED)

        if code in (VCSStatusCode.DELETED, VCSStatusCode.CONFLICTED):
            return self._style_manager.get_color(ColorRole.VCS_DELETED)

        if code in (VCSStatusCode.RENAMED, VCSStatusCode.COPIED):
            return self._style_manager.get_color(ColorRole.VCS_RENAMED)

        return self._style_manager.get_color(ColorRole.VCS_MODIFIED)

    def _on_language_changed(self) -> None:
        """Update localised strings when the UI language changes."""
        strings = self._language_manager.strings()
        self._header.set_title(strings.mindspace_vcs)
        self._conflicts_header.set_title(strings.git_conflicts)
        self._abort_button.setText(strings.git_abort)
        self._continue_button.setText(strings.git_rebase_continue)
        self._update_merge_banner()
        self._fetch_button.setToolTip(strings.git_fetch)
        self._branch_button.setToolTip(strings.git_branch_tooltip)
        self._apply_action_tooltips(self._staged_tree)
        self._apply_action_tooltips(self._changes_tree)
        self._refresh_button.setToolTip(strings.git_refresh)
        self._history_button.setToolTip(strings.git_history)
        self._more_button.setToolTip(strings.git_more_actions)
        self._stage_all_button.setToolTip(strings.git_stage_all_tooltip)
        self._unstage_all_button.setToolTip(strings.git_unstage_all_tooltip)
        self._repo_combo.setToolTip(strings.git_repo_tooltip)
        self._commit_message.setPlaceholderText(strings.git_commit_placeholder)
        self._commit_button.setText(strings.git_commit_button)
        self._commit_and_push_action.setText(strings.git_commit_and_push)
        self._amend_action.setText(strings.git_commit_amend)
        self._undo_action.setText(strings.git_undo_last_commit)
        self._empty_title.setText(strings.git_no_repo_title)
        self._empty_message.setText(strings.git_no_repo_message)
        self._init_repo_button.setText(strings.git_init_repo)
        self._clone_repo_button.setText(strings.git_clone_repo)
        self._update_toolbar(self._branch_info, self._upstream)
        self._update_headers()
        self.apply_style()
