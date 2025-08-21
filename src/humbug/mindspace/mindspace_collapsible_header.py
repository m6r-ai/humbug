"""Collapsible header widget for mindspace sections."""

from PySide6.QtCore import Signal, Qt, QSize, QEvent
from PySide6.QtGui import QIcon, QMouseEvent, QEnterEvent
from PySide6.QtWidgets import QWidget, QHBoxLayout, QLabel, QToolButton

from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager


class MindspaceCollapsibleHeader(QWidget):
    """Header widget with expand/collapse functionality for mindspace sections."""

    toggled = Signal(bool)  # Emitted when expand/collapse state changes (expanded state)

    def __init__(self, title: str, parent: QWidget | None = None) -> None:
        """
        Initialize the collapsible header.

        Args:
            title: The title text to display
            parent: Parent widget
        """
        super().__init__(parent)
        self.setObjectName("MindspaceCollapsibleHeader")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._is_expanded = True  # Default to expanded

        # Create layout
        layout = QHBoxLayout(self)
        spacing = 5
        layout.setSpacing(spacing)
        layout.setContentsMargins(spacing, spacing, spacing, spacing)

        # Create expand/collapse button
        self._expand_button = QToolButton(self)
        self._expand_button.setObjectName("_expand_button")
        self._expand_button.clicked.connect(self._toggle_expanded)
        layout.addWidget(self._expand_button)

        # Create title label
        self._title_label = QLabel(title, self)
        self._title_label.setIndent(0)
        layout.addWidget(self._title_label)

        # Add stretch to push everything to the left
        layout.addStretch()
        self.setLayout(layout)

        # Update button appearance
        self._update_expand_button()

        # Make the entire header clickable
        self.setCursor(Qt.CursorShape.PointingHandCursor)

    def set_title(self, title: str) -> None:
        """
        Set the header title text.

        Args:
            title: New title text
        """
        self._title_label.setText(title)

    def is_expanded(self) -> bool:
        """
        Check if the section is expanded.

        Returns:
            True if expanded, False if collapsed
        """
        return self._is_expanded

    def set_expanded(self, expanded: bool, emit_signal: bool = True) -> None:
        """
        Set the expanded state.

        Args:
            expanded: Whether the section should be expanded
            emit_signal: Whether to emit the toggled signal
        """
        if self._is_expanded == expanded:
            return

        self._is_expanded = expanded
        self._update_expand_button()

        if emit_signal:
            self.toggled.emit(expanded)

    def _toggle_expanded(self) -> None:
        """Toggle the expanded state."""
        self.set_expanded(not self._is_expanded)

    def _update_expand_button(self) -> None:
        """Update the expand button icon and tooltip based on current state."""
        strings = self._language_manager.strings()

        if self._is_expanded:
            # Show down arrow when expanded
            icon_name = "arrow-down"
            tooltip = strings.tooltip_collapse_message

        else:
            # Show right arrow when collapsed (with RTL support)
            icon_name = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"
            tooltip = strings.tooltip_expand_message

        # Set icon size based on zoom factor
        icon_base_size = 16
        self._expand_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path(icon_name), icon_base_size
        )))
        icon_scaled_size = round(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)
        self._expand_button.setIconSize(icon_size)

        # Update tooltip
        self.setToolTip(tooltip)

    def enterEvent(self, event: QEnterEvent) -> None:
        """Handle mouse enter events for hover effect."""
        self.setProperty("hovered", True)
        self.style().unpolish(self)
        self.style().polish(self)
        super().enterEvent(event)

    def leaveEvent(self, event: QEvent) -> None:
        """Handle mouse leave events for hover effect."""
        self.setProperty("hovered", False)
        self.style().unpolish(self)
        self.style().polish(self)
        super().leaveEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Handle mouse press events to make entire header clickable."""
        if event.button() == Qt.MouseButton.LeftButton:
            self._toggle_expanded()
            event.accept()
            return

        super().mousePressEvent(event)

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Update font size for title label
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._title_label.setFont(font)

        # Update expand button
        self._update_expand_button()
