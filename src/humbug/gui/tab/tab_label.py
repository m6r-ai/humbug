"""Tab label management for the Humbug application."""

from typing import Optional

from PySide6.QtWidgets import (
    QWidget, QLabel, QToolButton, QHBoxLayout, QSizePolicy, QApplication
)
from PySide6.QtCore import Signal, QSize, Qt, QMimeData, QPoint
from PySide6.QtGui import QIcon, QPixmap, QDrag, QMouseEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class TabLabel(QWidget):
    """Custom widget for tab labels with close button and drag support."""

    close_clicked = Signal()
    drag_started = Signal()

    def __init__(self, text: str, tab_id: str, parent=None):
        """
        Initialize the tab label widget.

        Args:
            text: The text to display in the tab
            tab_id: Unique identifier for the associated tab
            parent: Optional parent widget
        """
        super().__init__(parent)

        self._tab_id = tab_id
        self._is_current = False
        self._is_active_column = False
        self._is_hovered = False
        self._style_manager = StyleManager()
        self._drag_start_pos: Optional[QPoint] = None

        self.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Minimum)

        self._layout = QHBoxLayout(self)
        self.setLayout(self._layout)

        # Add label with size policy
        self._label = QLabel(text)
        self._label.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Minimum)
        self._layout.addWidget(self._label)

        self._close_button = QToolButton()
        self._close_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._close_button.clicked.connect(self.close_clicked)
        self._close_button.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Minimum)
        self._layout.addWidget(self._close_button)

        self.handle_style_changed(False)

        self.setMouseTracking(True)

    def _create_visible_close_icon(self) -> QIcon:
        icon = QIcon()
        icon_path = self._style_manager.get_icon_path("close")
        pixmap = self._style_manager.scale_icon(icon_path, 16)  # 16px base size
        icon.addPixmap(pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        icon.addPixmap(pixmap, QIcon.Mode.Active, QIcon.State.Off)
        return icon

    def _create_visible_disabled_close_icon(self) -> QIcon:
        icon = QIcon()
        icon_path = self._style_manager.get_icon_path("disabled-close")
        pixmap = self._style_manager.scale_icon(icon_path, 16)  # 16px base size
        icon.addPixmap(pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        icon.addPixmap(pixmap, QIcon.Mode.Active, QIcon.State.Off)
        return icon

    def _create_invisible_close_icon(self) -> QIcon:
        """Create a transparent icon for the inactive state."""
        size = self._style_manager.get_scaled_size(16)
        transparent_pixmap = QPixmap(size, size)
        transparent_pixmap.fill(Qt.transparent)
        return QIcon(transparent_pixmap)

    def handle_style_changed(self, is_active: bool):
        """
        Handle style changes from StyleManager.

        Args:
            is_active: Is this an active tab's label?
        """
        colour = ColorRole.TEXT_PRIMARY if is_active else ColorRole.TAB_INACTIVE
        self._label.setStyleSheet(f"color: {self._style_manager.get_color_str(colour)}")

        self._update_font_size()

        # Update close button size
        factor = self._style_manager.zoom_factor
        button_size = 16 * factor
        self._close_button.setFixedSize(button_size, button_size)

        # Update icon size
        icon_size = 16 * factor
        self._close_button.setIconSize(QSize(icon_size, icon_size))

        # Recreate icons at new size
        self._visible_close_icon = self._create_visible_close_icon()
        self._visible_disabled_close_icon = self._create_visible_disabled_close_icon()
        self._invisible_close_icon = self._create_invisible_close_icon()

        # Update layout margins and spacing
        self._layout.setSpacing(6 * factor)
        margins = 8 * factor
        v_margins = 4 * factor
        self._layout.setContentsMargins(margins, v_margins, margins, v_margins)

        self._update_close_button()

        self.adjustSize()

    def _update_font_size(self):
        """Update the label font size based on current zoom factor."""
        font = self._label.font()
        base_size = self._style_manager.base_font_size
        scaled_size = self._style_manager.get_scaled_size(base_size)
        font.setPointSizeF(scaled_size)
        self._label.setFont(font)

    def mousePressEvent(self, event: QMouseEvent):
        """Handle mouse press events for drag initiation."""
        if event.button() == Qt.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent):
        """Handle mouse move events for drag operations."""
        if not self._drag_start_pos:
            return

        # Check if we've moved far enough to start a drag
        if (event.pos() - self._drag_start_pos).manhattanLength() < QApplication.startDragDistance():
            return

        # Create drag object
        drag = QDrag(self)
        mime_data = QMimeData()
        # Store the tab ID for identification
        mime_data.setData("application/x-humbug-tab", self._tab_id.encode())
        drag.setMimeData(mime_data)

        # Create pixmap for drag visual feedback
        pixmap = QPixmap(self.size())

        # Save original styles
        original_widget_style = self.styleSheet()
        original_label_style = self._label.styleSheet()
        original_button_style = self._close_button.styleSheet()

        # Set temporary style for drag visual
        self.setStyleSheet(f"""
            QWidget {{
                background: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)
        self.render(pixmap)

        # Restore original styles
        self.setStyleSheet(original_widget_style)
        self._label.setStyleSheet(original_label_style)
        self._close_button.setStyleSheet(original_button_style)
        drag.setPixmap(pixmap)
        drag.setHotSpot(self._drag_start_pos)

        # Clear drag tracking
        self._drag_start_pos = None

        # Emit signal before starting drag
        self.drag_started.emit()

        # Execute drag operation
        drag.exec_(Qt.MoveAction)

    def mouseReleaseEvent(self, event: QMouseEvent):
        """Handle mouse release events."""
        self._drag_start_pos = None
        super().mouseReleaseEvent(event)

    def update_hover_state(self, is_hovered: bool):
        """Handle updates to the hover state for the label."""
        self._is_hovered = is_hovered
        self._update_close_button()

    def _update_close_button(self):
        """Update close button appearance based on current state."""
        visible = self._is_current or self._is_hovered

        style_manager = StyleManager()
        if visible:
            base_color = (ColorRole.TAB_BACKGROUND_ACTIVE if self._is_current and not self._is_hovered
                        else ColorRole.TAB_BACKGROUND_HOVER)
            style = f"""
                QToolButton {{
                    border: none;
                    outline: none;
                    padding: 0px;
                    background: {style_manager.get_color_str(base_color)};
                }}
                QToolButton:hover {{
                    background: {style_manager.get_color_str(ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER)};
                }}
            """
            icon = self._visible_close_icon if self._is_active_column else self._visible_disabled_close_icon
            self._close_button.setIcon(icon)
            self._close_button.setCursor(Qt.PointingHandCursor)
            self._close_button.setToolTip("Close Tab")
        else:
            style = f"""
                QToolButton {{
                    border: none;
                    outline: none;
                    padding: 0px;
                    background: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_INACTIVE)};
                }}
            """
            self._close_button.setIcon(self._invisible_close_icon)
            self._close_button.setCursor(Qt.ArrowCursor)
            self._close_button.setToolTip("")

        self._close_button.setStyleSheet(style)

    def set_current(self, is_current: bool, is_active_column: bool):
        """Update the current state of the tab."""
        self._is_current = is_current
        self._is_active_column = is_active_column
        self._update_close_button()

    @property
    def tab_id(self) -> str:
        """Get the tab's unique identifier."""
        return self._tab_id

    def text(self) -> str:
        """
        Get the current text of the tab label.

        Returns:
            str: The current text displayed in the label
        """
        return self._label.text()

    def update_text(self, text: str) -> None:
        """
        Update the text displayed in the tab label.

        Args:
            text: New text to display
        """
        self._label.setText(text)
        self.adjustSize()

    def update_id(self, tab_id: str, text: str) -> None:
        """
        Update the ID and the text displayed in the tab label.

        Args:
            text: New text to display
        """
        self._tab_id = tab_id
        self.update_text(text)
