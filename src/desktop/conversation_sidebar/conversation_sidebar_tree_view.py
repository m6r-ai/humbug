"""Conversations tree view implementation for mindspace conversations with drag and drop support and inline editing."""

from collections.abc import Callable
import os

from PySide6.QtCore import QModelIndex, QPersistentModelIndex, Qt, QTimer
from PySide6.QtGui import QBrush, QColor, QLinearGradient, QPaintEvent, QPainter, QPen
from PySide6.QtWidgets import QStyleOptionViewItem, QWidget

from desktop.color_role import ColorRole
from desktop.conversation_sidebar.conversation_sidebar_dag_model import ConversationSidebarDAGModel
from desktop.conversation_sidebar.conversation_sidebar_tree_delegate import ConversationSidebarTreeDelegate
from desktop.sidebar.sidebar_tree_view import SidebarTreeView
from desktop.style_manager import StyleManager


class ConversationSidebarTreeView(SidebarTreeView):
    """Custom tree view for conversations with drag and drop support, auto-scroll, and inline editing."""

    def __init__(self, parent: QWidget | None = None):
        """Initialize the conversations tree view."""
        super().__init__(parent)
        self._conversations_path: str = ""
        self._style_manager = StyleManager()

        self._deferred_scroll_timer = QTimer(self)
        self._deferred_scroll_timer.setSingleShot(True)
        self._deferred_scroll_timer.timeout.connect(self._on_deferred_scroll)
        self._deferred_scroll_index: QModelIndex = QModelIndex()
        self._deferred_scroll_callback: Callable | None = None

    def drawRow(
        self,
        painter: QPainter,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex
    ) -> None:
        """
        Shrink the boundary row's paint rect so nothing paints into the space
        reserved for the Pinned-section divider.

        drawRow() is the single place Qt paints an entire tree row — branch
        / expand-arrow background included, not just the delegate's own
        icon/text area.  ConversationSidebarTreeDelegate.sizeHint() reserves
        extra height on this row for the divider, but shrinking the rect
        only inside the delegate's paint() (as an earlier version of this
        did) left Qt's own branch/selection background painting the full,
        unshrunk row — which is why a selected or hovered boundary row could
        still show a highlight bleeding into the divider's space.  Shrinking
        here instead covers the whole row in one place.
        """
        model = self.model()
        if not isinstance(model, ConversationSidebarDAGModel) or not model.is_first_row_after_pinned_section(index):
            super().drawRow(painter, option, index)
            return

        zoom = self._style_manager.zoom_factor()
        pad = round(ConversationSidebarTreeDelegate.DIVIDER_PADDING * zoom)
        shrunk_option = QStyleOptionViewItem(option)
        shrunk_option.rect = option.rect.adjusted(0, pad, 0, 0)  # type: ignore
        super().drawRow(painter, shrunk_option, index)

    def paintEvent(self, event: QPaintEvent) -> None:
        """Paint the tree, then an independent divider marking the end of the Pinned section."""
        super().paintEvent(event)
        self._paint_pinned_divider()

    def _paint_pinned_divider(self) -> None:
        """
        Draw a line marking the boundary between the Pinned section and the
        rest of the tree, directly on the viewport.

        Deliberately painted here — at the view level, after every row has
        already been painted — rather than by the delegate for one specific
        row.  Combined with drawRow() shrinking that row's entire paint area
        (see above), the space this line is drawn into is always genuinely
        empty, regardless of hover or selection state on either neighbour.
        """
        model = self.model()
        if not isinstance(model, ConversationSidebarDAGModel):
            return

        if not model.pinned_section_index().isValid():
            return

        # Row 1 at the root is always the first item after the Pinned
        # section, however many rows are currently visible inside it.
        boundary_index = model.index(1, 0)
        if not boundary_index.isValid():
            return

        rect = self.visualRect(boundary_index)
        if rect.isEmpty():
            return

        viewport = self.viewport()
        if rect.top() < 0 or rect.top() > viewport.height():
            return

        zoom = self._style_manager.zoom_factor()
        pad = round(ConversationSidebarTreeDelegate.DIVIDER_PADDING * zoom)

        painter = QPainter(viewport)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        right_inset = round(10 * zoom)
        y = rect.top() + pad // 2
        x_start = 0
        x_end = viewport.width() - right_inset

        # Fade out at both ends instead of a flat line — a soft accent
        # rather than a hard rule, peaking a little past full-line opacity
        # right at the centre for a subtle "glow".
        color = self._style_manager.get_color(ColorRole.EDIT_BOX_BORDER)
        gradient = QLinearGradient(x_start, y, x_end, y)
        transparent = QColor(color)
        transparent.setAlpha(0)
        peak = QColor(color)
        peak.setAlpha(150)
        gradient.setColorAt(0.0, transparent)
        gradient.setColorAt(0.5, peak)
        gradient.setColorAt(1.0, transparent)

        pen = QPen(QBrush(gradient), max(1.0, 1.5 * zoom))
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        painter.setPen(pen)

        painter.drawLine(x_start, y, x_end, y)
        painter.end()

    def get_root_path(self) -> str:
        """
        Get the root path for this tree view.

        Returns:
            Conversations root path string, or empty string if no conversations path is configured
        """
        return self._conversations_path

    def is_valid_drag_source(self, path: str) -> bool:
        """
        Check if a path can be dragged from this tree view.

        Conversations tree view allows all items to be dragged.

        Args:
            path: Path to check for drag validity

        Returns:
            True if the path can be dragged, False otherwise
        """
        if os.path.basename(path) == ".":
            return False

        return True

    def configure_for_path(self, path: str) -> None:
        """
        Configure the tree view for the given conversations path.

        Args:
            path: Conversations path to configure the tree view for
        """
        self._conversations_path = path

    def get_view_type(self) -> str:
        """
        Get the type identifier for this view.

        Returns:
            String identifying this as the conversations view
        """
        return "conversations"

    def get_path_from_index(self, index: QModelIndex) -> str | None:
        """
        Get the file system path from a model index.

        Args:
            index: The model index to get the path for

        Returns:
            File system path if valid, None otherwise
        """
        if not index.isValid():
            return None

        dag_model = self.model()
        if not isinstance(dag_model, ConversationSidebarDAGModel):
            return None

        return dag_model.path_for_index(index)

    def collapse_path(self, path: str) -> QModelIndex:
        """
        Collapse the tree node corresponding to the given filesystem path.

        Args:
            path: Absolute filesystem path of the folder to collapse.

        Returns:
            The model index of the collapsed item, or an invalid index if not found.
        """
        index = self.index_for_path(path)
        if index.isValid():
            self.collapse(index)

        return index

    def index_for_path(self, path: str) -> QModelIndex:
        """
        Return the DAG model index for the given file system path without side effects.

        Args:
            path: Absolute filesystem path to look up.

        Returns:
            The model index, or an invalid index if not found.
        """
        dag_model = self.model()
        if not isinstance(dag_model, ConversationSidebarDAGModel):
            return QModelIndex()

        return dag_model.index_for_path(path)

    def ensure_path_visible_for_editing(self, file_path: str, callback: Callable) -> None:
        """
        Ensure the specified file path is visible and optimally positioned for editing.

        Args:
            file_path: Absolute path to the file to make visible
            callback: Callback to execute after the item is visible
        """
        dag_model = self.model()
        if not isinstance(dag_model, ConversationSidebarDAGModel):
            return

        index = dag_model.index_for_path(file_path)
        if not index.isValid():
            return

        # Expand all parents
        parent = index.parent()
        parents = []
        while parent.isValid():
            parents.append(parent)
            parent = parent.parent()

        for p in reversed(parents):
            if not self.isExpanded(p):
                self.expand(p)

        self._deferred_scroll_index = index
        self._deferred_scroll_callback = callback
        self._deferred_scroll_timer.setInterval(200)
        self._deferred_scroll_timer.start()

    def _scroll_to_and_edit(self, index: QModelIndex, callback: Callable) -> None:
        """
        Scroll to index and invoke callback.

        Args:
            index: Model index to scroll to
            file_path: Path for viewport position check
            callback: Callback to invoke after scrolling
        """
        viewport_rect = self.viewport().rect()
        item_rect = self.visualRect(index)
        margin = 40
        is_visible = (
            item_rect.top() >= margin and
            item_rect.bottom() <= viewport_rect.height() - margin
        )
        if not is_visible:
            self.scrollTo(index, self.ScrollHint.PositionAtCenter)
            self._deferred_scroll_callback = callback
            self._deferred_scroll_timer.setInterval(100)
            self._deferred_scroll_timer.start()

        else:
            callback()

    def _on_deferred_scroll(self) -> None:
        """Fire the stored deferred scroll callback."""
        if self._deferred_scroll_callback:
            if self._deferred_scroll_index.isValid():
                index = self._deferred_scroll_index
                self._deferred_scroll_index = QModelIndex()
                self._scroll_to_and_edit(index, self._deferred_scroll_callback)

            else:
                self._deferred_scroll_callback()
