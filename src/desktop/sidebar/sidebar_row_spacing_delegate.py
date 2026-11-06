"""Row-spacing delegate shared by the simple tree-based sidebar panels."""

from PySide6.QtCore import QModelIndex, QPersistentModelIndex, QSize
from PySide6.QtWidgets import QStyledItemDelegate, QStyleOptionViewItem, QTreeWidget

from desktop.style_manager import StyleManager


class SidebarRowSpacingDelegate(QStyledItemDelegate):
    """
    Item delegate giving a tree widget zoom-scaled row heights.

    A plain QTreeWidget left to its own sizing renders rows noticeably tighter
    than the panels built on SidebarTreeView, so this keeps the simple panels
    looking like the rest of the sidebar.
    """

    def __init__(self, tree: QTreeWidget, style_manager: StyleManager) -> None:
        """
        Initialise the delegate for a tree widget.

        Args:
            tree: The tree widget the delegate is attached to.
            style_manager: Style manager supplying the current zoom factor.
        """
        super().__init__(tree)
        self._style_manager = style_manager

    def sizeHint(self, option: QStyleOptionViewItem, _index: QModelIndex | QPersistentModelIndex) -> QSize:
        """Return a consistent, zoom-scaled row height matching the other sidebars."""
        zoom = self._style_manager.zoom_factor()
        font_metrics = option.fontMetrics  # type: ignore[attr-defined]
        row_height = max(font_metrics.height() + round(10 * zoom), round(24 * zoom))
        return QSize(super().sizeHint(option, _index).width(), row_height)
