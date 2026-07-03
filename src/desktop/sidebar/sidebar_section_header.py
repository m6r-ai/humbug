"""Section header widget for sidebar panes."""

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QMouseEvent
from PySide6.QtWidgets import QHBoxLayout, QLabel, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


class SidebarSectionHeader(QWidget):
    """A simple titled header for sidebar panes."""

    clicked = Signal()  # Emitted when the header is clicked (e.g. to collapse a section)

    def __init__(self, title: str, parent: QWidget | None = None) -> None:
        """Initialize the section header."""
        super().__init__(parent)
        self.setObjectName("SidebarSectionHeader")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._style_manager = StyleManager()

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

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Emit ``clicked`` for a left-button press that isn't on a child widget."""
        if event.button() == Qt.MouseButton.LeftButton:
            child = self.childAt(event.position().toPoint())
            # Ignore clicks that land on an interactive trailing widget (e.g. a button).
            if child is None or child is self._title_label:
                self.clicked.emit()

        super().mousePressEvent(event)

    def add_trailing_widget(self, widget: QWidget) -> None:
        """
        Add a widget aligned to the trailing edge of the header.

        Useful for small action buttons (e.g. a refresh button) that belong
        alongside a section title.

        Args:
            widget: The widget to append after the title's stretch.
        """
        widget.setParent(self)
        self._layout.addWidget(widget)

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()
        background = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)

        font = self._title_label.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._title_label.setFont(font)

        self.setStyleSheet(f"""
            QWidget#SidebarSectionHeader {{
                background-color: {background};
                border: none;
            }}
            QWidget#SidebarSectionHeader QLabel {{
                color: {text};
                background: transparent;
            }}
        """)
