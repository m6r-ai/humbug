"""Root drop target widget for mindspace tree views."""

import os
from typing import Callable

from PySide6.QtCore import Qt, QPoint, Signal
from PySide6.QtGui import QDragEnterEvent, QDragMoveEvent, QDropEvent, QDragLeaveEvent
from PySide6.QtWidgets import QWidget, QMenu

from humbug.style_manager import StyleManager


class MindspaceRootDropWidget(QWidget):
    """Drop target widget that appears below tree views to accept root-level drops."""

    file_dropped = Signal(str, str)  # source_path, target_path (root directory)

    def __init__(self, root_path_provider: Callable[[], str], parent: QWidget | None = None) -> None:
        """
        Initialize the root drop widget.

        Args:
            root_path_provider: Function that returns the root directory path for drops
            parent: Parent widget
        """
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._root_path_provider = root_path_provider
        self._context_menu_provider: Callable[[], QMenu | None] | None = None
        self._is_drop_target = False

        # Enable drops
        self.setAcceptDrops(True)

        # Enable context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_context_menu)

        # Set initial styling
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self.apply_style()

    def set_context_menu_provider(self, provider: Callable[[], QMenu | None]) -> None:
        """
        Set the context menu provider function.

        Args:
            provider: Function that returns a QMenu or None
        """
        self._context_menu_provider = provider

    def apply_style(self) -> None:
        """Apply styling based on zoom factor."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Calculate height to match tree view text line height
        # This matches the height of a tree view item
        line_height = int((base_font_size * zoom_factor) * 1.75)  # Font size + some padding
        self.setMinimumHeight(line_height)

    def _show_context_menu(self, position: QPoint) -> None:
        """
        Show context menu at the specified position.

        Args:
            position: Position where the context menu was requested
        """
        if not self._context_menu_provider:
            return

        menu = self._context_menu_provider()
        if not menu:
            return

        menu.exec_(self.mapToGlobal(position))

    def _is_valid_drop(self, event: QDragEnterEvent | QDragMoveEvent | QDropEvent) -> bool:
        """
        Check if the current drag operation is valid for this widget.

        Args:
            event: The drag event to validate

        Returns:
            True if this is a valid drop operation
        """
        # Check if we have the correct mime type
        if not event.mimeData().hasFormat("application/x-humbug-path"):
            return False

        # Get the root path
        root_path = self._root_path_provider()
        if not root_path:
            return False

        # Get the dragged path
        mime_data = event.mimeData().data("application/x-humbug-path").data()
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Can't drop a directory into itself or its parent
        if os.path.dirname(dragged_path) == root_path:
            return False

        # Can't drop a parent directory into one of its children
        if root_path.startswith(dragged_path + os.sep):
            return False

        return True

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Handle drag enter events."""
        if not self._is_valid_drop(event):
            event.ignore()
            return

        self._is_drop_target = True
        self.setProperty("is_drop_target", True)
        self.style().unpolish(self)
        self.style().polish(self)
        self.apply_style()
        event.acceptProposedAction()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Handle drag move events."""
        if not self._is_valid_drop(event):
            event.ignore()
            return

        event.acceptProposedAction()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Handle drag leave events."""
        self._is_drop_target = False
        self.setProperty("is_drop_target", False)
        self.style().unpolish(self)
        self.style().polish(self)
        self.apply_style()
        super().dragLeaveEvent(event)

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle drop events."""
        # Clear drop target state first
        self._is_drop_target = False
        self.setProperty("is_drop_target", False)
        self.style().unpolish(self)
        self.style().polish(self)
        self.apply_style()

        if not self._is_valid_drop(event):
            event.ignore()
            return

        # Get the root path
        root_path = self._root_path_provider()
        if not root_path:
            event.ignore()
            return

        # Get the dragged path
        mime_data = event.mimeData().data("application/x-humbug-path").data()
        if not isinstance(mime_data, bytes):
            mime_data = bytes(mime_data)

        dragged_path = mime_data.decode()

        # Emit the drop signal
        self.file_dropped.emit(dragged_path, root_path)
        event.acceptProposedAction()
