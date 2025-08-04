"""Widget for displaying a single message in the system history with header."""

from datetime import datetime
import logging
from typing import List, Tuple

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QTextEdit
)
from PySide6.QtCore import Signal, QPoint, Qt
from PySide6.QtGui import (
    QResizeEvent, QColor, QCursor, QMouseEvent, QTextCursor,
    QTextCharFormat
)

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager
from humbug.tabs.shell.shell_message_source import ShellMessageSource
from humbug.tabs.shell.shell_text_edit import ShellTextEdit


class ShellMessageWidget(QFrame):
    """Widget for displaying a single message in the system history with header."""

    selection_changed = Signal(bool)
    scroll_requested = Signal(QPoint)
    mouse_released = Signal()

    def __init__(self, parent: QWidget | None = None, is_input: bool = False) -> None:
        """
        Initialize the message widget.

        Args:
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        # Set object name for QSS targeting
        self.setObjectName("ShellMessageWidget")

        self._is_input = is_input

        self._logger = logging.getLogger("ShellMessageWidget")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._style_manager = StyleManager()

        # Will store the actual message source
        self._message_id: str | None = None
        self._message_source: ShellMessageSource | None = None
        self._message_timestamp: datetime | None = None
        self._message_content = ""

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        spacing = int(self._style_manager.message_bubble_spacing())
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header.setObjectName("_header")
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(4)

        # Create role and timestamp labels
        self._role_label = QLabel(self)
        self._role_label.setObjectName("_role_label")
        self._role_label.setIndent(0)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addStretch()

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Create text area
        self._text_area = ShellTextEdit(is_input, self)
        self._text_area.setObjectName("_text_area")

        # Disable the standard context menu as our parent widget will handle that
        self._text_area.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)

        # Connect signals from text area
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mouse_pressed.connect(self._on_mouse_pressed)
        self._text_area.mouse_released.connect(self._on_mouse_released)

        self._layout.addWidget(self._text_area)

        self._is_focused = False
        self._mouse_left_button_pressed = False

        # Set input property for QSS targeting
        self.setProperty("isInput", is_input)

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()
        self._on_language_changed()

    def message_id(self) -> str | None:
        """Get the message ID."""
        return self._message_id

    def is_focused(self) -> bool:
        """Check if this message is focused."""
        return self._is_focused

    def set_focused(self, focused: bool) -> None:
        """Set the focused state of this message."""
        if self._is_focused == focused:
            return

        self._is_focused = focused
        if focused:
            self.setFocus()

        self._apply_shared_stylesheet()

    def _on_language_changed(self) -> None:
        """Update text when language changes."""
        if not self._is_input:
            # Don't update input widget headers
            self._update_role_text()

    def _update_role_text(self) -> None:
        """Update the role text based on current language."""
        strings = self._language_manager.strings()

        # Map from message source to display text
        if self._message_source == ShellMessageSource.USER:
            role_text = strings.role_you

        else:
            role_text = strings.role_system

        # Format with timestamp
        if self._message_timestamp is not None:
            timestamp_str = self._message_timestamp.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            self._role_label.setText(f"{role_text} @ {timestamp_str}")
        else:
            self._role_label.setText(role_text)

    def _on_mouse_pressed(self, event: QMouseEvent) -> None:
        """Handle mouse press from text area."""
        if event.buttons() == Qt.MouseButton.LeftButton:
            self._mouse_left_button_pressed = True

    def _on_mouse_released(self, _event: QMouseEvent) -> None:
        """Handle mouse release from text area."""
        self._mouse_left_button_pressed = False
        self.mouse_released.emit()

    def _on_selection_changed(self) -> None:
        """Handle selection changes in the text area."""
        cursor = self._text_area.textCursor()
        has_selection = cursor.hasSelection()

        if has_selection and self._mouse_left_button_pressed:
            # Emit global mouse position for accurate scroll calculations
            self.scroll_requested.emit(QCursor.pos())

        self.selection_changed.emit(has_selection)

    def set_content(self, text: str, source: ShellMessageSource, timestamp: datetime, message_id: str) -> None:
        """
        Set content with style, handling incremental updates for AI responses.

        Args:
            text: The message text content
            source: The source of the message
            timestamp: datetime object for the message timestamp
        """
        self._message_id = message_id
        self._message_source = source
        self._message_timestamp = timestamp
        self._message_content = text

        # Set message source property for QSS targeting
        source_name = {
            ShellMessageSource.USER: "user",
            ShellMessageSource.SUCCESS: "success",
            ShellMessageSource.ERROR: "error"
        }.get(source, "user")

        self._role_label.setProperty("message_source", source_name)
        self.setProperty("message_source", source_name)

        # Set the content in the text area
        self._text_area.set_text(text)
        if source == ShellMessageSource.USER:
            self._text_area.enable_highlighter()

        # Update the header
        self._update_role_text()
        self._apply_shared_stylesheet()

    def has_selection(self) -> bool:
        """Check if any section has selected text."""
        return self._text_area.textCursor().hasSelection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this message.

        Returns:
            Currently selected text or empty string
        """
        cursor = self._text_area.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            # Convert Qt's special line break character
            return text.replace('\u2029', '\n')

        return ""

    def copy_selection(self) -> None:
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def clear_selection(self) -> None:
        """Clear any text selection in this message."""
        cursor = self._text_area.textCursor()
        cursor.clearSelection()
        self._text_area.setTextCursor(cursor)

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

    def _get_border_color(self) -> str:
        """Get the border color based on current state."""
        if self._is_focused and self.hasFocus():
            return self._style_manager.get_color_str(ColorRole.MESSAGE_FOCUSED)

        current_source = self._message_source or ShellMessageSource.USER
        if current_source == ShellMessageSource.USER:
            return self._style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)

        return self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)

    def _build_message_frame_styles(self) -> str:
        """Build styles for the main message frame."""
        style_manager = self._style_manager
        border_color = self._get_border_color()
        border_radius = int(self._style_manager.message_bubble_spacing())

        return f"""
            QFrame#ShellMessageWidget {{
                margin: 0;
                border-radius: {border_radius}px;
                border: 2px solid {border_color};
            }}
            QFrame#ShellMessageWidget[message_source="user"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
            }}
            QFrame#ShellMessageWidget[message_source="success"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
            QFrame#ShellMessageWidget[message_source="error"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            #ShellMessageWidget QWidget#_header {{
                background-color: transparent;
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}
        """

    def _build_header_styles(self) -> str:
        """Build styles for the header area and role label."""
        style_manager = self._style_manager

        return f"""
            #ShellMessageWidget QLabel#_role_label {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                margin: 0;
                padding: 0;
                background-color: transparent;
            }}
            #ShellMessageWidget QLabel#_role_label[message_source="user"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_USER)};
            }}
            #ShellMessageWidget QLabel#_role_label[message_source="success"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_SUCCESS)};
            }}
            #ShellMessageWidget QLabel#_role_label[message_source="error"] {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_ERROR)};
            }}
        """

    def _build_text_area_styles(self) -> str:
        """Build styles for the text area and scrollbars."""
        style_manager = self._style_manager

        return f"""
            #ShellMessageWidget QTextEdit#_text_area {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
                background-color: transparent;
            }}

            #ShellMessageWidget #_text_area QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #ShellMessageWidget #_text_area QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            #ShellMessageWidget #_text_area QScrollBar::add-page:horizontal,
            #ShellMessageWidget #_text_area QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            #ShellMessageWidget #_text_area QScrollBar::add-line:horizontal,
            #ShellMessageWidget #_text_area QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """

    def _apply_shared_stylesheet(self) -> None:
        """Apply the shared stylesheet to this message."""
        # Build sections: message frame, header, text area
        stylesheet_parts = [
            self._build_message_frame_styles(),
            self._build_header_styles(),
            self._build_text_area_styles()
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

    def _on_style_changed(self) -> None:
        """Handle the style changing"""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Apply font to components
        self._role_label.setFont(font)

        self._apply_shared_stylesheet()

    def find_text(self, text: str) -> List[Tuple[int, int]]:
        """
        Find all instances of text in this message.

        Args:
            text: Text to search for

        Returns:
            List of (start_position, end_position) tuples for each match
        """
        document = self._text_area.document()
        matches = []
        cursor = QTextCursor(document)

        while True:
            cursor = document.find(text, cursor)
            if cursor.isNull():
                break

            matches.append((cursor.selectionStart(), cursor.selectionEnd()))

        return matches

    def highlight_matches(
        self,
        matches: List[Tuple[int, int]],
        current_match_index: int = -1,
        highlight_color: QColor | None = None,
        dim_highlight_color: QColor | None = None
    ) -> None:
        """
        Highlight matches in this message.

        Args:
            matches: List of (start, end) tuples to highlight
            current_match_index: Index of current match to highlight differently, or -1 for none
            highlight_color: QColor for current match, defaults to system highlight color
            dim_highlight_color: QColor for other matches, defaults to dimmer highlight color
        """
        # Default colors if not provided
        if not highlight_color:
            highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND)

        if not dim_highlight_color:
            dim_highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM)

        # Create format for current match
        current_format = QTextCharFormat()
        current_format.setBackground(highlight_color)

        # Create format for other matches
        other_format = QTextCharFormat()
        other_format.setBackground(dim_highlight_color)

        # Create selections
        selections = []
        for i, (start, end) in enumerate(matches):
            cursor = QTextCursor(self._text_area.document())
            cursor.setPosition(start)
            cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)

            extra_selection = QTextEdit.ExtraSelection()
            extra_selection.cursor = cursor  # type: ignore
            extra_selection.format = current_format if i == current_match_index else other_format  # type: ignore

            selections.append(extra_selection)

        self._text_area.setExtraSelections(selections)

    def clear_highlights(self) -> None:
        """Clear all highlights from the message."""
        self._text_area.setExtraSelections([])

    def select_and_scroll_to_position(self, position: int) -> QPoint:
        """
        Select text at a specific position and return the cursor position relative to this widget.

        Args:
            position: Text position to scroll to

        Returns:
            QPoint: Position of the cursor relative to this widget
        """
        cursor = QTextCursor(self._text_area.document())
        cursor.setPosition(position)
        self._text_area.setTextCursor(cursor)

        # Get cursor rectangle in text area coordinates
        cursor_rect = self._text_area.cursorRect(cursor)

        # Convert to position relative to this widget
        local_pos = self._text_area.mapTo(self, cursor_rect.topLeft())

        return local_pos

    def text_area(self) -> ShellTextEdit:
        """Get the text area widget."""
        return self._text_area
