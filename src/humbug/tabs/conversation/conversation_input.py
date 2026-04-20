"""Input widget that matches history message styling."""

import sys
from typing import Dict, List, Tuple, cast

from PySide6.QtCore import Signal, Qt, QRect, QSize, QObject, QEvent
from PySide6.QtGui import QTextCursor, QTextDocument, QIcon, QKeyEvent
from PySide6.QtWidgets import QWidget, QToolButton, QHBoxLayout, QLabel

from ai import AIMessageSource

from humbug.tabs.conversation.conversation_message import ConversationMessage


class ConversationInput(ConversationMessage):
    """Widget for conversation message input that matches history message styling."""

    # Forward text cursor signals from the input area
    cursor_position_changed = Signal()
    page_key_scroll_requested = Signal()
    submit_requested = Signal()
    stop_requested = Signal()
    settings_requested = Signal()
    attach_requested = Signal()
    modified = Signal()

    def __init__(self, style: AIMessageSource, parent: QWidget | None = None) -> None:
        """Initialize the conversation input widget."""
        self._is_streaming = False
        self._current_model = ""
        self._submit_button: QToolButton | None = None
        self._stop_button: QToolButton | None = None
        self._settings_button: QToolButton | None = None
        self._attach_button: QToolButton | None = None
        self._attachments: List[Tuple[str, str]] = []  # (filename, content)
        self._attachments_bar: QWidget | None = None
        self._attachments_layout: QHBoxLayout | None = None

        super().__init__(style, parent=parent, is_input=True)

        # Connect text cursor signals
        self._text_area = self._sections[0].text_area()

        self._text_area.cursorPositionChanged.connect(self.cursor_position_changed)
        self._text_area.page_key_scroll_requested.connect(self.page_key_scroll_requested)
        self._text_area.installEventFilter(self)  # Install event filter for Ctrl+Enter

        # Attachments bar (hidden until files are attached)
        self._attachments_bar = QWidget(self)
        self._attachments_bar.setObjectName("_attachments_bar")
        self._attachments_layout = QHBoxLayout(self._attachments_bar)
        self._attachments_layout.setContentsMargins(0, 2, 0, 2)
        self._attachments_layout.setSpacing(4)
        self._attachments_layout.addStretch()
        self._attachments_bar.hide()

        # Insert the attachments bar between the sections container and the banner.
        # The layout order for input messages is: spacing(1), sections_container, banner.
        # We want: spacing(1), sections_container, attachments_bar, banner.
        banner_index = self._layout.indexOf(self._banner)
        self._layout.insertWidget(banner_index, self._attachments_bar)

        # Create attach button (leftmost in banner, before stop/submit/settings)
        self._attach_button = QToolButton(self)
        self._attach_button.setObjectName("_attach_button")
        self._attach_button.clicked.connect(self._on_attach_button_clicked)
        self._banner_layout.insertWidget(0, self._attach_button)

        # Create stop button (initially hidden)
        self._stop_button = QToolButton(self)
        self._stop_button.setObjectName("_stop_button")
        self._stop_button.clicked.connect(self._on_stop_button_clicked)
        self._stop_button.hide()
        self._banner_layout.addWidget(self._stop_button)

        # Create submit button
        self._submit_button = QToolButton(self)
        self._submit_button.setObjectName("_submit_button")
        self._submit_button.clicked.connect(self._on_submit_button_clicked)
        self._banner_layout.addWidget(self._submit_button)

        # Create settings button
        self._settings_button = QToolButton(self)
        self._settings_button.setObjectName("_settings_button")
        self._settings_button.clicked.connect(self._on_settings_button_clicked)
        self._banner_layout.addWidget(self._settings_button)

        # Connect text changes to update button state
        self._text_area.textChanged.connect(self._on_text_changed)

        self._on_language_changed()

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """Intercept Ctrl+Enter in the inline editor to confirm."""
        if obj is self._text_area and event.type() == QEvent.Type.KeyPress:
            key_event = cast(QKeyEvent, event)
            if (key_event.key() == Qt.Key.Key_Return and key_event.modifiers() & Qt.KeyboardModifier.ControlModifier):
                self.submit_requested.emit()
                return True

        return super().eventFilter(obj, event)

    def set_model(self, model: str) -> None:
        """Set the model name for the input prompt."""
        self._current_model = model
        self._update_banner_text()

    def _on_language_changed(self) -> None:
        """Handle language change event."""
        self._update_banner_text()

        strings = self._language_manager.strings()

        if self._attach_button:
            self._attach_button.setToolTip(strings.tooltip_attach_file)

        if self._settings_button:
            self._settings_button.setToolTip(strings.tooltip_settings_message)

        if self._stop_button:
            self._stop_button.setToolTip(strings.tooltip_stop_message)

        if self._submit_button:
            self._submit_button.setToolTip(strings.tooltip_submit_message)

    def set_streaming(self, streaming: bool) -> None:
        """Update the streaming state and header text."""
        self._is_streaming = streaming
        self._update_banner_text()
        self._update_button_states()

    def _get_submit_key_text(self) -> str:
        """Get the appropriate submit key text based on the platform."""
        if sys.platform == "darwin":
            return "⌘ Enter"

        return "Ctrl+Enter"

    def apply_style(self) -> None:
        """Apply style changes."""
        super().apply_style()

        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        if self._attach_button:
            self._attach_button.setIcon(QIcon(self._style_manager.scale_icon("paperclip", icon_base_size)))
            self._attach_button.setIconSize(icon_size)

        if self._submit_button:
            self._submit_button.setIcon(QIcon(self._style_manager.scale_icon("submit", icon_base_size)))
            self._submit_button.setIconSize(icon_size)

        if self._settings_button:
            self._settings_button.setIcon(QIcon(self._style_manager.scale_icon("cog", icon_base_size)))
            self._settings_button.setIconSize(icon_size)

        if self._stop_button:
            self._stop_button.setIcon(QIcon(self._style_manager.scale_icon("stop", icon_base_size)))
            self._stop_button.setIconSize(icon_size)

        self._rescale_attachment_icons(icon_base_size, icon_size)

    def _update_banner_text(self) -> None:
        """Update the header text based on current state."""
        strings = self._language_manager.strings()
        submit_key = self._get_submit_key_text()
        if self._is_streaming:
            self._role_label.setText(strings.processing_message.format(model=self._current_model, key=submit_key))
            self.setProperty("message_source", "ai_streaming")

        else:
            self._role_label.setText(strings.input_prompt.format(model=self._current_model, key=submit_key))
            self.setProperty("message_source", "user_input")

        self._role_label.style().unpolish(self._role_label)
        self._role_label.style().polish(self._role_label)

    def _on_text_changed(self) -> None:
        """Handle text changes in the input area."""
        self._update_button_states()
        self.modified.emit()

    def _update_button_states(self) -> None:
        """Update button enabled states and visibility based on content and streaming status."""
        has_content = bool(self.to_plain_text().strip()) or bool(self._attachments)

        if self._is_streaming:
            if self._stop_button:
                self._stop_button.show()
                self._stop_button.setEnabled(True)

            if self._submit_button:
                self._submit_button.setEnabled(has_content)

        else:
            if self._stop_button:
                self._stop_button.hide()

            if self._submit_button:
                self._submit_button.setEnabled(has_content)

    def _on_submit_button_clicked(self) -> None:
        """Handle submit button click."""
        self.submit_requested.emit()

    def _on_stop_button_clicked(self) -> None:
        """Stop the current message processing via button click."""
        self.stop_requested.emit()

    def _on_settings_button_clicked(self) -> None:
        """Handle settings button click."""
        self.settings_requested.emit()

    def _on_attach_button_clicked(self) -> None:
        """Handle attach button click."""
        self.attach_requested.emit()

    def add_attachment(self, filename: str, content: str) -> None:
        """Add a file attachment and show it in the attachments bar."""
        self._attachments.append((filename, content))
        self._rebuild_attachments_bar()
        self._update_button_states()

    def get_attachments(self) -> List[Tuple[str, str]]:
        """Return the list of (filename, content) attachments."""
        return list(self._attachments)

    def clear_attachments(self) -> None:
        """Remove all attachments and hide the attachments bar."""
        self._attachments.clear()
        self._rebuild_attachments_bar()
        self._update_button_states()

    def _rebuild_attachments_bar(self) -> None:
        """Rebuild the attachments bar from the current attachments list."""
        if self._attachments_layout is None or self._attachments_bar is None:
            return

        # Remove all existing attachment widgets, leaving the trailing stretch.
        while self._attachments_layout.count() > 1:
            item = self._attachments_layout.takeAt(0)
            widget = item.widget() if item else None
            if widget:
                widget.deleteLater()

        for index, (filename, _) in enumerate(self._attachments):
            attachment_widget = QWidget()
            attachment_widget.setObjectName("_attachment_widget")
            attachment_widget.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
            attachment_layout = QHBoxLayout(attachment_widget)
            attachment_layout.setContentsMargins(8, 4, 4, 4)
            attachment_layout.setSpacing(4)

            name_label = QLabel(filename)
            name_label.setObjectName("_attachment_label")
            attachment_layout.addWidget(name_label)

            icon_base_size = 14
            icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
            icon_size = QSize(icon_scaled_size, icon_scaled_size)
            remove_button = QToolButton()
            remove_button.setObjectName("_attachment_remove")
            remove_button.setIcon(QIcon(self._style_manager.scale_icon("close", icon_base_size)))
            remove_button.setIconSize(icon_size)
            remove_button.clicked.connect(lambda _checked, i=index: self._remove_attachment(i))
            attachment_layout.addWidget(remove_button)

            self._attachments_layout.insertWidget(self._attachments_layout.count() - 1, attachment_widget)

        self._attachments_bar.setVisible(bool(self._attachments))

    def _remove_attachment(self, index: int) -> None:
        """Remove the attachment at the given index."""
        if 0 <= index < len(self._attachments):
            self._attachments.pop(index)
            self._rebuild_attachments_bar()
            self._update_button_states()

    def _rescale_attachment_icons(self, icon_base_size: int, icon_size: QSize) -> None:
        """Update close icons on all current attachment remove buttons after a zoom change."""
        if self._attachments_layout is None:
            return

        close_icon = QIcon(self._style_manager.scale_icon("close", icon_base_size))
        for i in range(self._attachments_layout.count() - 1):
            item = self._attachments_layout.itemAt(i)
            if item is None:
                continue

            attachment_widget = item.widget()
            if attachment_widget is None:
                continue

            remove_button = attachment_widget.findChild(QToolButton, "_attachment_remove")
            if remove_button is not None:
                remove_button.setIcon(close_icon)
                remove_button.setIconSize(icon_size)

    def clear(self) -> None:
        """Clear the input area."""
        self._text_area.clear()

    def to_plain_text(self) -> str:
        """Get the current input text."""
        return self._text_area.toPlainText()

    def set_plain_text(self, text: str) -> None:
        """Set the input text."""
        self._text_area.setPlainText(text)

    def cursor_rect(self) -> QRect:
        """Get the cursor rectangle from the input area."""
        text_cursor = self._text_area.cursorRect()
        offset = self._banner.height()
        cursor = QRect(text_cursor.x(), offset + text_cursor.y(), text_cursor.width(), text_cursor.height())
        return cursor

    def setFocus(self, reason: Qt.FocusReason | None = None) -> None:
        """Set focus to the input area."""
        if reason is None:
            self._text_area.setFocus()
            return

        self._text_area.setFocus(reason)

    def hasFocus(self) -> bool:
        """Check if the input area has focus."""
        return self._text_area.hasFocus()

    def document(self) -> QTextDocument:
        """Get the document from the input area."""
        return self._text_area.document()

    def text_cursor(self) -> QTextCursor:
        """Get the text cursor from the input area."""
        return self._text_area.textCursor()

    def undo(self) -> None:
        """Undo the last edit operation."""
        self._text_area.undo()

    def redo(self) -> None:
        """Redo the last undone edit operation."""
        self._text_area.redo()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._text_area.cut()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._text_area.paste()

    def set_cursor_position(self, position: Dict[str, int]) -> None:
        """
        Set cursor position.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        cursor = self._text_area.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.Start)

        for _ in range(position.get("line", 0)):
            cursor.movePosition(QTextCursor.MoveOperation.NextBlock)

        cursor.movePosition(
            QTextCursor.MoveOperation.Right,
            QTextCursor.MoveMode.MoveAnchor,
            position.get("column", 0)
        )

        self._text_area.setTextCursor(cursor)

    def get_cursor_position(self) -> Dict[str, int]:
        """
        Get current cursor position.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self._text_area.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }
