"""Tab label management for the Humbug application."""

from PySide6.QtWidgets import (
    QWidget, QLabel, QToolButton, QHBoxLayout, QSizePolicy, QApplication
)
from PySide6.QtCore import Signal, QSize, Qt, QMimeData, QPoint
from PySide6.QtGui import QIcon, QPixmap, QDrag, QMouseEvent

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class TabLabel(QWidget):
    """Custom widget for tab labels with close button and drag support."""

    close_clicked = Signal()
    drag_started = Signal()

    def __init__(self, tab_id: str, icon_name: str, text: str, tool_tip: str, parent: QWidget | None = None) -> None:
        """
        Initialize the tab label widget.

        Args:
            tab_id: Unique identifier for the associated tab
            icon_name: Name of the icon to display
            text: The text to display in the tab
            tool_tip: Tooltip text for the tab
            parent: Optional parent widget
        """
        super().__init__(parent)

        self.setToolTip(tool_tip)
        self._tab_id = tab_id
        self._icon_name = icon_name
        self._is_current = False
        self._is_active_column = False
        self._is_updated = False
        self._is_hovered = False
        self._is_ephemeral = False
        self._style_manager = StyleManager()
        self._drag_start_pos: QPoint | None = None

        self.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Minimum)

        self._layout = QHBoxLayout(self)
        self.setLayout(self._layout)

        self._type_button = QToolButton()
        self._type_button.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Minimum)
        self._layout.addWidget(self._type_button)

        self._label = QLabel(text)
        self._label.setIndent(0)
        self._layout.addWidget(self._label)

        self._close_button = QToolButton()
        self._close_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._close_button.clicked.connect(self.close_clicked)
        self._close_button.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Minimum)
        self._layout.addWidget(self._close_button)

        self.handle_style_changed()
        self.setMouseTracking(True)

    def _create_type_icon(self) -> QIcon:
        """Create an icon for the tab type."""
        icon = QIcon()
        icon_path = self._style_manager.get_icon_path(self._icon_name)
        pixmap = self._style_manager.scale_icon(icon_path, 16)
        icon.addPixmap(pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        icon.addPixmap(pixmap, QIcon.Mode.Active, QIcon.State.Off)
        return icon

    def _create_inactive_type_icon(self) -> QIcon:
        """Create an icon for the tab type."""
        icon = QIcon()
        icon_path = self._style_manager.get_icon_path(f"inactive-{self._icon_name}")
        pixmap = self._style_manager.scale_icon(icon_path, 16)
        icon.addPixmap(pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        icon.addPixmap(pixmap, QIcon.Mode.Active, QIcon.State.Off)
        return icon

    def _create_visible_close_icon(self) -> QIcon:
        icon = QIcon()
        icon_path = self._style_manager.get_icon_path("close")
        pixmap = self._style_manager.scale_icon(icon_path, 16)  # 16px base size
        icon.addPixmap(pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        icon.addPixmap(pixmap, QIcon.Mode.Active, QIcon.State.Off)
        return icon

    def _create_visible_inactive_close_icon(self) -> QIcon:
        icon = QIcon()
        icon_path = self._style_manager.get_icon_path("inactive-close")
        pixmap = self._style_manager.scale_icon(icon_path, 16)  # 16px base size
        icon.addPixmap(pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        icon.addPixmap(pixmap, QIcon.Mode.Active, QIcon.State.Off)
        return icon

    def _create_invisible_close_icon(self) -> QIcon:
        """Create a transparent icon for the inactive state."""
        size = int(16 * self._style_manager.zoom_factor())
        transparent_pixmap = QPixmap(size, size)
        transparent_pixmap.fill(Qt.GlobalColor.transparent)
        return QIcon(transparent_pixmap)

    def handle_style_changed(self) -> None:
        """
        Handle style changes from StyleManager.
        """
        self._update_label()

        # Update close button size
        factor = self._style_manager.zoom_factor()
        button_size = int(16 * factor)
        self._type_button.setFixedSize(button_size, button_size)
        self._close_button.setFixedSize(button_size, button_size)

        # Update icon size
        icon_size = int(16 * factor)
        self._type_button.setIconSize(QSize(icon_size, icon_size))
        self._close_button.setIconSize(QSize(icon_size, icon_size))

        # Recreate icons at new size
        self._type_icon = self._create_type_icon()
        self._inactive_type_icon = self._create_inactive_type_icon()

        self._visible_close_icon = self._create_visible_close_icon()
        self._visible_inactive_close_icon = self._create_visible_inactive_close_icon()
        self._invisible_close_icon = self._create_invisible_close_icon()

        # Update layout margins and spacing
        self._layout.setSpacing(int(6 * factor))
        margins = int(8 * factor)
        v_margins = int(4 * factor)
        self._layout.setContentsMargins(margins, v_margins, margins, v_margins)

        self._update_buttons()

        self.adjustSize()

    def _update_label(self) -> None:
        """Update the label based on current zoom factor and whether it's for an ephemeral tab."""
        show_active = self._is_current and self._is_active_column
        if self._is_ephemeral:
            colour = ColorRole.TEXT_EPHEMERAL if show_active else ColorRole.TEXT_EPHEMERAL_INACTIVE

        else:
            colour = ColorRole.TEXT_PRIMARY if show_active else ColorRole.TEXT_INACTIVE

        self._label.setStyleSheet(f"color: {self._style_manager.get_color_str(colour)}")
        font = self._label.font()
        base_size = self._style_manager.base_font_size()
        scaled_size = base_size * self._style_manager.zoom_factor()
        font.setPointSizeF(scaled_size)
        font.setItalic(self._is_ephemeral)
        self._label.setFont(font)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press events for drag initiation."""
        if event.button() & Qt.MouseButton.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
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
        drag.exec_(Qt.DropAction.MoveAction)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """Handle mouse release events."""
        self._drag_start_pos = None
        super().mouseReleaseEvent(event)

    def update_hover_state(self, is_hovered: bool) -> None:
        """Handle updates to the hover state for the label."""
        self._is_hovered = is_hovered
        self._update_buttons()
        self._update_label()

    def _get_background_color(self) -> ColorRole:
        """Get the appropriate background color based on current state."""
        if self._is_hovered and (not self._is_current or not self._is_active_column):
            return ColorRole.TAB_BACKGROUND_HOVER

        if self._is_current:
            return ColorRole.TAB_BACKGROUND_ACTIVE

        if self._is_updated:
            return ColorRole.TAB_BACKGROUND_UPDATED

        return ColorRole.TAB_BACKGROUND_INACTIVE

    def _update_buttons(self) -> None:
        """Update tab type and close button appearance based on current state."""
        visible = self._is_current or self._is_hovered

        style_manager = StyleManager()
        base_color = self._get_background_color()

        type_style = f"""
            QToolButton {{
                border: none;
                outline: none;
                padding: 0px;
                background: {style_manager.get_color_str(base_color)};
            }}
        """

        if visible:
            close_style = f"""
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
            show_active = self._is_current and self._is_active_column
            type_icon = self._type_icon if show_active else self._inactive_type_icon
            self._type_button.setIcon(type_icon)
            close_icon = self._visible_close_icon if show_active else self._visible_inactive_close_icon
            self._close_button.setIcon(close_icon)
            self._close_button.setCursor(Qt.CursorShape.PointingHandCursor)
            self._close_button.setToolTip("Close Tab")

        else:
            close_style = type_style
            self._type_button.setIcon(self._inactive_type_icon)
            self._close_button.setIcon(self._invisible_close_icon)
            self._close_button.setCursor(Qt.CursorShape.ArrowCursor)
            self._close_button.setToolTip("")

        self._type_button.setStyleSheet(type_style)
        self._close_button.setStyleSheet(close_style)

    def set_current(self, is_current: bool, is_active_column: bool, is_updated: bool = False) -> None:
        """
        Update the current state of the tab.

        Args:
            is_current: Whether this tab is currently selected
            is_active_column: Whether this tab is in the active column
            is_updated: Whether this tab has updated content
        """
        self._is_current = is_current
        self._is_active_column = is_active_column
        self._is_updated = is_updated

        # If tab becomes current, clear the updated state
        if is_current:
            self._is_updated = False

        self._update_buttons()

    def set_updated(self, is_updated: bool) -> None:
        """
        Set the updated state of the tab.

        Args:
            is_updated: Whether this tab has updated content
        """
        # Don't show updated state if tab is currently selected
        if not self._is_current:
            self._is_updated = is_updated
            self._update_buttons()

    def set_ephemeral(self, is_ephemeral: bool) -> None:
        """
        Set the ephemeral state and update font styling.

        Args:
            is_ephemeral: Whether this tab is ephemeral
        """
        self._is_ephemeral = is_ephemeral
        self._update_label()

    def text(self) -> str:
        """
        Get the current text of the tab label.

        Returns:
            str: The current text displayed in the label
        """
        return self._label.text()

    def set_text(self, text: str) -> None:
        """
        Set the text displayed in the tab label.

        Args:
            text: New text to display
        """
        self._label.setText(text)
        self.adjustSize()
