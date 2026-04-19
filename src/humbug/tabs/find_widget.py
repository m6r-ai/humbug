"""Widget for handling find operations in editor."""

from typing import Any, Callable, Dict

from PySide6.QtWidgets import (
    QWidget, QGridLayout, QHBoxLayout, QLineEdit, QToolButton, QLabel, QPushButton
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
    replace_current = Signal(str)   # replace text
    replace_all = Signal(str)       # replace text

    def __init__(self, parent: QWidget | None = None):
        """Initialize the find widget."""
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._preferred_width_fn: Callable[[], int | None] | None = None

        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        self._replace_row_enabled = False
        self._replace_row_expanded = False

        # Grid layout: col 0 = expand button, col 1 = inputs (stretches), col 2+ = controls
        self._grid = QGridLayout(self)
        self._grid.setContentsMargins(4, 4, 4, 4)
        self._grid.setSpacing(4)
        self._grid.setColumnStretch(1, 1)

        self._expand_button = QToolButton()
        self._expand_button.clicked.connect(self._toggle_replace_row)
        self._expand_button.hide()
        self._grid.addWidget(self._expand_button, 0, 0)

        self._search_input = QLineEdit()
        self._search_input.textChanged.connect(self._on_text_changed)
        self._search_input.returnPressed.connect(self.find_next)
        self._grid.addWidget(self._search_input, 0, 1)

        self._replace_input = QLineEdit()
        self._replace_input.returnPressed.connect(self._on_replace_current)
        self._replace_input.hide()
        self._grid.addWidget(self._replace_input, 1, 1)

        self._match_case_button = QToolButton()
        self._match_case_button.setCheckable(True)
        self._match_case_button.setChecked(False)
        self._match_case_button.setObjectName("toggleButton")
        self._match_case_button.clicked.connect(self._on_mode_changed)

        self._regexp_button = QToolButton()
        self._regexp_button.setCheckable(True)
        self._regexp_button.setChecked(False)
        self._regexp_button.setObjectName("toggleButton")
        self._regexp_button.clicked.connect(self._on_mode_changed)

        self._status_label = QLabel()

        self._prev_button = QToolButton()
        self._prev_button.clicked.connect(self.find_previous)

        self._next_button = QToolButton()
        self._next_button.clicked.connect(self.find_next)

        self._close_button = QToolButton()
        self._close_button.clicked.connect(self.close)

        # Pack the find-row controls into a single HBox in col 2, row 0
        find_controls = QHBoxLayout()
        find_controls.setContentsMargins(0, 0, 0, 0)
        find_controls.setSpacing(4)
        find_controls.addWidget(self._match_case_button)
        find_controls.addWidget(self._regexp_button)
        find_controls.addWidget(self._status_label)
        find_controls.addWidget(self._prev_button)
        find_controls.addWidget(self._next_button)
        find_controls.addWidget(self._close_button)
        self._grid.addLayout(find_controls, 0, 2)

        self._replace_button = QPushButton()
        self._replace_button.clicked.connect(self._on_replace_current)

        self._replace_all_button = QPushButton()
        self._replace_all_button.clicked.connect(self._on_replace_all)

        # Pack the replace-row controls into a single HBox in col 2, row 1
        replace_controls = QHBoxLayout()
        replace_controls.setContentsMargins(0, 0, 0, 0)
        replace_controls.setSpacing(4)
        replace_controls.addWidget(self._replace_button)
        replace_controls.addWidget(self._replace_all_button)
        self._replace_controls_widget = QWidget()
        self._replace_controls_widget.setLayout(replace_controls)
        self._replace_controls_widget.hide()
        self._grid.addWidget(self._replace_controls_widget, 1, 2)

        # Track current state
        self._matches = 0
        self._current_match = 0

        self._on_language_changed()

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._language_manager.language_changed.connect(self._on_language_changed)

    def enable_replace_row(self) -> None:
        """
        Enable the expand/collapse chevron and replace row for this widget.

        Should be called by tabs that support replace (i.e. editor tabs).
        Has no effect if called more than once.
        """
        if self._replace_row_enabled:
            return

        self._replace_row_enabled = True
        self._expand_button.show()
        self._on_style_changed()

    def _toggle_replace_row(self) -> None:
        """Toggle the replace row visibility."""
        self._replace_row_expanded = not self._replace_row_expanded
        self._replace_input.setVisible(self._replace_row_expanded)
        self._replace_controls_widget.setVisible(self._replace_row_expanded)
        self._update_expand_icon()
        if self._replace_row_expanded:
            self._replace_input.setFocus()

    def _update_expand_icon(self) -> None:
        """Update the expand button icon to reflect current expanded state."""
        if self._replace_row_expanded:
            icon_name = "arrow-down"

        else:
            icon_name = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"

        self._expand_button.setIcon(QIcon(self._style_manager.scale_icon(icon_name, 15)))

    def _on_replace_current(self) -> None:
        """Emit replace_current with current replace text."""
        self.replace_current.emit(self._replace_input.text())

    def _on_replace_all(self) -> None:
        """Emit replace_all with current replace text."""
        self.replace_all.emit(self._replace_input.text())

    def _on_language_changed(self) -> None:
        strings = self._language_manager.strings()
        self._search_input.setPlaceholderText(strings.find_placeholder)
        self._match_case_button.setToolTip(strings.find_match_case)
        self._regexp_button.setToolTip(strings.find_use_regexp)
        self._replace_input.setPlaceholderText(strings.replace_placeholder)
        self._replace_button.setText(strings.replace_button)
        self._replace_all_button.setText(strings.replace_all_button)
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

        self._update_expand_icon()
        prev_icon = "arrow-left" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-right"
        self._prev_button.setIcon(QIcon(self._style_manager.scale_icon(prev_icon, 15)))
        next_icon = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"
        self._next_button.setIcon(QIcon(self._style_manager.scale_icon(next_icon, 15)))
        self._close_button.setIcon(QIcon(self._style_manager.scale_icon("close", 15)))

        self._match_case_button.setIcon(QIcon(self._style_manager.scale_icon("find-match-case", 15)))
        self._regexp_button.setIcon(QIcon(self._style_manager.scale_icon("find-regexp", 15)))

        scaled_size = int(15 * factor)
        icon_size = QSize(scaled_size, scaled_size)
        self._prev_button.setIconSize(icon_size)
        self._next_button.setIconSize(icon_size)
        self._close_button.setIconSize(icon_size)
        self._match_case_button.setIconSize(icon_size)
        self._regexp_button.setIconSize(icon_size)

        self._expand_button.setIconSize(icon_size)

        button_bg = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)
        button_hover = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        button_pressed = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)
        button_checked = self._style_manager.get_color_str(ColorRole.TEXT_FOUND)
        button_checked_hover = self._style_manager.get_color_str(ColorRole.TEXT_FOUND_DIM)
        button_disabled = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)
        text_primary = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        text_disabled = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        tab_bg = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)
        text_selected = self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)

        stylesheet = f"""
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
            QToolButton#toggleButton:hover {{
                background-color: {button_checked_hover};
            }}
            QToolButton#toggleButton:pressed {{
                background-color: {button_checked};
            }}
            QToolButton#toggleButton:checked {{
                background-color: {button_checked};
            }}
            QToolButton#toggleButton:checked:hover {{
                background-color: {button_checked_hover};
            }}
            QToolButton#toggleButton:checked:pressed {{
                background-color: {button_bg};
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
            QPushButton {{
                background-color: {button_bg};
                color: {text_primary};
                border: none;
                padding: 2px 6px;
            }}
            QPushButton:hover {{
                background-color: {button_hover};
            }}
            QPushButton:pressed {{
                background-color: {button_pressed};
            }}
        """
        self.setStyleSheet(stylesheet)

    def _update_margins(self) -> None:
        """Adjust layout margins to centre the controls when constrained."""
        if self._preferred_width_fn is None:
            self._grid.setContentsMargins(4, 4, 4, 4)
            return

        preferred = self._preferred_width_fn()
        if preferred is None:
            self._grid.setContentsMargins(4, 4, 4, 4)
            return

        available = self.width()
        side = max(4, (available - preferred) // 2)
        self._grid.setContentsMargins(side, 4, side, 4)

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

    def get_replace_text(self) -> str:
        """Get the current replace text."""
        return self._replace_input.text()

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

        has_matches = self._matches > 0
        self._replace_button.setEnabled(has_matches)
        self._replace_all_button.setEnabled(has_matches)

    def set_replace_enabled(self, enabled: bool) -> None:
        """Enable or disable the replace buttons independently of match count."""
        self._replace_button.setEnabled(enabled)
        self._replace_all_button.setEnabled(enabled)

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

    def set_status_text(self, text: str) -> None:
        """
        Display an arbitrary status string in the status label.

        Args:
            text: Text to display (e.g. "3 replaced")
        """
        self._status_label.setText(text)
        self._status_label.show()

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
            'replace_text': self.get_replace_text(),
            'replace_expanded': self._replace_row_expanded,
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

            if 'replace_text' in metadata:
                self._replace_input.setText(metadata['replace_text'])

            if metadata.get('replace_expanded', False) and self._replace_row_enabled:
                self._replace_row_expanded = True
                self._replace_input.show()
                self._replace_controls_widget.show()
                self._update_expand_icon()

            # Reset match status since we're not re-executing searches
            self.set_match_status(0, 0)

            if metadata.get('is_visible', False):
                self.show()
            else:
                self.hide()

        except Exception:
            # Fail silently if metadata is malformed
            pass
