"""Widget for handling find operations in editor."""

from typing import Any, Callable, Dict

from PySide6.QtWidgets import (
    QWidget, QHBoxLayout, QLineEdit, QToolButton, QLabel
)
from PySide6.QtCore import Signal, Qt, QSize
from PySide6.QtGui import QIcon, QFocusEvent, QKeyEvent, QCloseEvent, QResizeEvent

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager


class FindWidget(QWidget):
    """Non-modal widget for text find operations."""

    closed = Signal()
    find_next = Signal()
    find_previous = Signal()

    def __init__(self, parent: QWidget | None = None):
        """Initialize the find widget."""
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._preferred_width_fn: Callable[[], int | None] | None = None

        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(4, 4, 4, 4)
        self._layout.setSpacing(4)

        self._search_input = QLineEdit()
        self._search_input.textChanged.connect(self._on_text_changed)
        self._search_input.returnPressed.connect(self.find_next)
        self._layout.addWidget(self._search_input)

        self._match_case_button = QToolButton()
        self._match_case_button.setCheckable(True)
        self._match_case_button.setChecked(False)
        self._match_case_button.clicked.connect(self._on_mode_changed)
        self._layout.addWidget(self._match_case_button)

        self._regexp_button = QToolButton()
        self._regexp_button.setCheckable(True)
        self._regexp_button.setChecked(False)
        self._regexp_button.clicked.connect(self._on_mode_changed)
        self._layout.addWidget(self._regexp_button)

        self._status_label = QLabel()
        self._layout.addWidget(self._status_label)

        self._prev_button = QToolButton()
        self._prev_button.clicked.connect(self.find_previous)
        self._layout.addWidget(self._prev_button)

        self._next_button = QToolButton()
        self._next_button.clicked.connect(self.find_next)
        self._layout.addWidget(self._next_button)

        self._close_button = QToolButton()
        self._close_button.clicked.connect(self.close)
        self._layout.addWidget(self._close_button)

        # Track current state
        self._matches = 0
        self._current_match = 0

        self._on_language_changed()

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._language_manager.language_changed.connect(self._on_language_changed)

    def _on_language_changed(self) -> None:
        strings = self._language_manager.strings()
        self._search_input.setPlaceholderText(strings.find_placeholder)
        self._match_case_button.setToolTip(strings.find_match_case)
        self._regexp_button.setToolTip(strings.find_use_regexp)
        self._update_match_status()
        self._on_style_changed()

    def _on_style_changed(self) -> None:
        """Update styling when application style changes."""
        self._update_margins()
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font_size = base_font_size * factor
        font.setPointSizeF(font_size)
        self.setFont(font)

        prev_icon = "arrow-left" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-right"
        self._prev_button.setIcon(QIcon(self._style_manager.scale_icon(prev_icon, 15)))
        next_icon = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"
        self._next_button.setIcon(QIcon(self._style_manager.scale_icon(next_icon, 15)))
        self._close_button.setIcon(QIcon(self._style_manager.scale_icon("close", 15)))

        scaled_size = int(15 * factor)
        icon_size = QSize(scaled_size, scaled_size)
        self._prev_button.setIconSize(icon_size)
        self._next_button.setIconSize(icon_size)
        self._close_button.setIconSize(icon_size)

        button_bg = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)
        button_hover = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        button_pressed = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)
        button_checked = self._style_manager.get_color_str(ColorRole.TEXT_FOUND_DIM)
        button_disabled = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)
        text_primary = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        text_disabled = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        tab_bg = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)
        text_selected = self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {tab_bg};
                color: {text_primary};
                font-size: {font_size}pt;
            }}
            QLineEdit {{
                background-color: {button_bg};
                color: {text_primary};
                border: none;
                padding: 2px;
                selection-background-color: {text_selected};
            }}
            QToolButton {{
                background-color: {button_bg};
                color: {text_primary};
                border: none;
                padding: 2px;
            }}
            QToolButton:hover {{
                background-color: {button_hover};
            }}
            QToolButton:pressed {{
                background-color: {button_pressed};
            }}
            QToolButton:checked {{
                background-color: {button_checked};
            }}
            QToolButton:checked:hover {{
                background-color: {button_hover};
            }}
            QToolButton:disabled {{
                background-color: {button_disabled};
                color: {text_disabled};
            }}
            QLabel {{
                color: {text_primary};
                border: none;
                padding: 2px;
            }}
        """)

    def _update_margins(self) -> None:
        """Adjust layout margins to centre the controls when constrained."""
        if self._preferred_width_fn is None:
            self._layout.setContentsMargins(4, 4, 4, 4)
            return

        preferred = self._preferred_width_fn()
        if preferred is None:
            self._layout.setContentsMargins(4, 4, 4, 4)
            return

        available = self.width()
        side = max(4, (available - preferred) // 2)
        self._layout.setContentsMargins(side, 4, side, 4)

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Recompute centring margins whenever the widget is resized."""
        super().resizeEvent(event)
        self._update_margins()

    def set_preferred_width(self, fn: Callable[[], int | None] | None) -> None:
        """
        Set a callable returning the preferred content width, or None for full-width layout.

        Args:
            fn: Callable returning pixel width to centre controls within, or None for full-width.
        """
        self._preferred_width_fn = fn
        self._update_margins()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle key events."""
        if event.key() == Qt.Key.Key_Escape:
            self.close()
            event.accept()
            return

        super().keyPressEvent(event)

    def closeEvent(self, event: QCloseEvent) -> None:
        """Emit closed signal when closing."""
        super().closeEvent(event)
        self.closed.emit()

    def focusInEvent(self, _event: QFocusEvent) -> None:  # type: ignore[override]
        """Handle focus events."""
        self._search_input.setFocus()
        self._search_input.selectAll()

    def _update_match_status(self) -> None:
        """Update the match status display."""
        strings = self._language_manager.strings()
        if self._matches > 0:
            self._status_label.setText(strings.find_match_count.format(
                current=self._current_match,
                total=self._matches
            ))
        else:
            self._status_label.setText(strings.find_no_matches)

        self._status_label.show()

        # Update button states
        self._prev_button.setEnabled(self._matches > 0)
        self._next_button.setEnabled(self._matches > 0)

    def set_match_status(self, current: int, total: int) -> None:
        """Set the match status display.

        Args:
            current: Current match index (1-based)
            total: Total number of matches
        """
        self._matches = total
        self._current_match = current
        self._update_match_status()

    def set_invalid_regexp(self) -> None:
        """Show an invalid regexp indicator in the status label."""
        strings = self._language_manager.strings()
        self._status_label.setText(strings.find_invalid_regexp)
        self._status_label.show()
        self._prev_button.setEnabled(False)
        self._next_button.setEnabled(False)

    def _on_text_changed(self) -> None:
        """Handle changes to search text."""
        self.find_next.emit()

    def _on_mode_changed(self) -> None:
        """Handle changes to match-case or regexp toggle."""
        self.find_next.emit()

    def get_search_text(self) -> str:
        """Get the current search text."""
        return self._search_input.text()

    def set_search_text(self, text: str) -> None:
        """Set the search text and trigger a search."""
        self._search_input.setText(text)
        self._search_input.selectAll()

    def is_case_sensitive(self) -> bool:
        """Return True if match-case mode is active."""
        return self._match_case_button.isChecked()

    def is_regexp(self) -> bool:
        """Return True if regular-expression mode is active."""
        return self._regexp_button.isChecked()

    def create_state_metadata(self) -> Dict[str, Any]:
        """
        Create state metadata for persistence during tab moves.

        Returns:
            Dictionary containing find widget state
        """
        return {
            'search_text': self.get_search_text(),
            'is_visible': not self.isHidden(),
            'match_case': self.is_case_sensitive(),
            'regexp': self.is_regexp(),
        }

    def restore_from_metadata(self, metadata: Dict[str, Any]) -> None:
        """
        Restore state from metadata after tab moves.

        Args:
            metadata: Dictionary containing find widget state
        """
        try:
            if 'search_text' in metadata:
                self.set_search_text(metadata['search_text'])

            if 'match_case' in metadata:
                self._match_case_button.setChecked(bool(metadata['match_case']))

            if 'regexp' in metadata:
                self._regexp_button.setChecked(bool(metadata['regexp']))

            # Reset match status since we're not re-executing searches
            self.set_match_status(0, 0)

            if metadata.get('is_visible', False):
                self.show()
            else:
                self.hide()

        except Exception:
            # Fail silently if metadata is malformed
            pass
