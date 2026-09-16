"""Section header widget for sidebar panes."""

from collections.abc import Callable

from PySide6.QtCore import QSize, Qt
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QHBoxLayout, QLabel, QToolButton, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


class SidebarSectionHeader(QWidget):
    """A titled header for sidebar panes, with optional trailing icon action buttons."""

    def __init__(self, title: str, parent: QWidget | None = None) -> None:
        """Initialize the section header."""
        super().__init__(parent)
        self.setObjectName("SidebarSectionHeader")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._style_manager = StyleManager()
        self._action_buttons: list[QToolButton] = []

        self._layout = QHBoxLayout(self)
        spacing = 5
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(spacing + 3, spacing, spacing, spacing)

        self._title_label = QLabel(title, self)
        self._title_label.setIndent(0)
        self._layout.addWidget(self._title_label)
        self._layout.addStretch()

    def set_title(self, title: str) -> None:
        """Set the header title text."""
        self._title_label.setText(title)

    def add_action_button(self, icon_name: str, tooltip: str, callback: Callable[[], None]) -> QToolButton:
        """
        Add a small icon button to the right edge of the header.

        Args:
            icon_name: Name of the icon to show, from the theme icon pack.
            tooltip: Tooltip text for the button.
            callback: Called (with no arguments) when the button is clicked.

        Returns:
            The created button, for further configuration (e.g. enabling/disabling it).
        """
        button = QToolButton(self)
        button.setObjectName("_action_button")
        button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        button.setCursor(Qt.CursorShape.PointingHandCursor)
        button.setToolTip(tooltip)
        button.setProperty("icon_name", icon_name)
        button.clicked.connect(callback)
        self._layout.addWidget(button)
        self._action_buttons.append(button)
        self.apply_style()
        return button

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        sm = self._style_manager
        zoom_factor = sm.zoom_factor()
        base_font_size = sm.base_font_size()
        background = sm.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        text = sm.get_color_str(ColorRole.TEXT_INACTIVE)
        button_hover = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        button_pressed = sm.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)
        radius = sm.radius()

        font = self._title_label.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._title_label.setFont(font)

        icon_size = sm.scale(14)
        button_size = sm.scale(22)
        for button in self._action_buttons:
            icon_name = button.property("icon_name")
            button.setIcon(QIcon(sm.scale_icon(icon_name, 14)))
            button.setIconSize(QSize(icon_size, icon_size))
            button.setFixedSize(button_size, button_size)

        self.setStyleSheet(f"""
            QWidget#SidebarSectionHeader {{
                background-color: {background};
                border: none;
            }}
            QWidget#SidebarSectionHeader QLabel {{
                color: {text};
                background: transparent;
            }}
            QWidget#SidebarSectionHeader QToolButton#_action_button {{
                background-color: transparent;
                border: none;
                border-radius: {radius}px;
            }}
            QWidget#SidebarSectionHeader QToolButton#_action_button:hover {{
                background-color: {button_hover};
            }}
            QWidget#SidebarSectionHeader QToolButton#_action_button:pressed {{
                background-color: {button_pressed};
            }}
        """)
