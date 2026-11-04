"""Section header widget for sidebar panes."""

from collections.abc import Callable

from PySide6.QtCore import QSize, Qt
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QHBoxLayout, QLabel, QToolButton, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


class SidebarSectionHeader(QWidget):
    """A titled header for sidebar panes, with optional trailing icon action buttons."""

    TITLE_INSET = 10
    VERTICAL_INSET = 4

    def __init__(self, title: str, parent: QWidget | None = None) -> None:
        """Initialize the section header."""
        super().__init__(parent)
        self.setObjectName("SidebarSectionHeader")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._style_manager = StyleManager()
        self._action_buttons: list[QToolButton] = []

        self._layout = QHBoxLayout(self)
        self._layout.setSpacing(self.VERTICAL_INSET)
        self._apply_insets()

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
        self._style_button(button)
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

        font = self._title_label.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._title_label.setFont(font)

        self._apply_insets()

        for button in self._action_buttons:
            self._style_button(button)

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
                padding: 0px;
                margin: 0px;
            }}
            QWidget#SidebarSectionHeader QToolButton#_action_button:hover {{
                background-color: {button_hover};
            }}
            QWidget#SidebarSectionHeader QToolButton#_action_button:pressed {{
                background-color: {button_pressed};
            }}
            QWidget#SidebarSectionHeader QToolButton#_action_button:disabled {{
                background-color: transparent;
            }}
        """)

    def _apply_insets(self) -> None:
        """
        Set the header's margins to the zoom-scaled title inset.

        The left inset is what lines the title up with the pane's rows and status
        text, so it has to follow the zoom factor like they do.
        """
        inset = self._style_manager.scale(self.TITLE_INSET)
        vertical = self._style_manager.scale(self.VERTICAL_INSET)
        self._layout.setContentsMargins(inset, vertical, inset, vertical)

    def _style_button(self, button: QToolButton) -> None:
        """Set one action button's icon (normal and disabled variants) and size."""
        icon_name = button.property("icon_name")
        if not isinstance(icon_name, str):
            return

        sm = self._style_manager
        icon_base_size = 14
        icon_scaled_size = sm.scale(icon_base_size)
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        icon = QIcon()
        icon.addPixmap(sm.scale_icon(icon_name, icon_base_size), QIcon.Mode.Normal)
        icon.addPixmap(sm.scale_icon(f"inactive-{icon_name}", icon_base_size), QIcon.Mode.Disabled)
        button.setIcon(icon)
        button.setIconSize(icon_size)
