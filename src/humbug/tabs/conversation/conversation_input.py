"""Input widget that matches history message styling."""

import os
import sys
from typing import Dict, List, Tuple

from PySide6.QtCore import Signal, Qt, QRect, QSize, QPoint
from PySide6.QtGui import QTextCursor, QTextDocument, QIcon
from PySide6.QtWidgets import QWidget, QToolButton, QHBoxLayout, QVBoxLayout, QLabel, QPushButton, QScrollArea, QFrame, QSizePolicy

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
    attach_requested = Signal(QPoint)
    modified = Signal()

    def __init__(self, style: AIMessageSource, parent: QWidget | None = None) -> None:
        """Initialize the conversation input widget."""
        self._is_streaming = False
        self._current_model = ""
        self._submit_button: QToolButton | None = None
        self._stop_button: QToolButton | None = None
        self._settings_button: QToolButton | None = None
        self._attach_button: QToolButton | None = None
        self._attachments: List[Tuple[str, str, str]] = []  # (filename, content, upload_path)
        self._attachments_bar: QWidget | None = None
        self._chips_layout: QHBoxLayout | None = None

        super().__init__(style, parent=parent, is_input=True)

        # Connect text cursor signals
        self._text_area = self._sections[0].text_area()

        self._text_area.cursorPositionChanged.connect(self.cursor_position_changed)
        self._text_area.page_key_scroll_requested.connect(self.page_key_scroll_requested)

        # Cap the input widget's vertical height to its natural content size.
        # Without this the messages_layout stretch gives the input all available space.
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Maximum)

        # Attachments bar (shown between text area and banner when files are attached)
        self._attachments_bar = QWidget(self)
        self._attachments_bar.setObjectName("_attachments_bar")
        chips_outer = QHBoxLayout(self._attachments_bar)
        chips_outer.setContentsMargins(0, 2, 0, 2)
        chips_outer.setSpacing(0)

        chips_scroll = QScrollArea()
        chips_scroll.setObjectName("_chips_scroll")
        chips_scroll.setFrameShape(QScrollArea.Shape.NoFrame)
        chips_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        chips_scroll.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        chips_scroll.setWidgetResizable(True)
        chips_scroll.setFixedHeight(58)

        chips_container = QWidget()
        chips_container.setObjectName("_chips_container")
        self._chips_layout = QHBoxLayout(chips_container)
        self._chips_layout.setContentsMargins(4, 0, 4, 0)
        self._chips_layout.setSpacing(4)
        self._chips_layout.addStretch()
        chips_scroll.setWidget(chips_container)
        chips_outer.addWidget(chips_scroll)

        self._attachments_bar.hide()
        # Insert between sections_container and banner
        self._layout.insertWidget(self._layout.count() - 1, self._attachments_bar)

        # Create attach button — placed at the LEFT of the banner (bottom-left of input)
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

    def set_model(self, model: str) -> None:
        """Set the model name for the input prompt."""
        self._current_model = model
        self._update_banner_text()

    def _on_language_changed(self) -> None:
        """Handle language change event."""
        self._update_banner_text()

        strings = self._language_manager.strings()

        if self._attach_button:
            self._attach_button.setToolTip(strings.tooltip_attach_document)

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
            return "⌘J"

        return "Ctrl+J"

    def apply_style(self) -> None:
        """Apply style changes."""
        super().apply_style()

        # Apply icon and styling
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        # Update attach button — plus icon in a rounded square
        if self._attach_button:
            attach_base_size = 18
            attach_scaled_size = int(attach_base_size * self._style_manager.zoom_factor())
            attach_icon_size = QSize(attach_scaled_size, attach_scaled_size)
            self._attach_button.setIcon(QIcon(self._style_manager.scale_icon("plus", attach_base_size)))
            self._attach_button.setIconSize(attach_icon_size)

        # Update submit/interrupt button
        if self._submit_button:
            self._submit_button.setIcon(QIcon(self._style_manager.scale_icon("submit", icon_base_size)))
            self._submit_button.setIconSize(icon_size)

        # Update settings button
        if self._settings_button:
            self._settings_button.setIcon(QIcon(self._style_manager.scale_icon("cog", icon_base_size)))
            self._settings_button.setIconSize(icon_size)

        # Update stop button if it exists
        if self._stop_button:
            self._stop_button.setIcon(QIcon(self._style_manager.scale_icon("stop", icon_base_size)))
            self._stop_button.setIconSize(icon_size)

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
            # Show both stop and interrupt buttons
            if self._stop_button:
                self._stop_button.show()
                self._stop_button.setEnabled(True)

            if self._submit_button:
                self._submit_button.setEnabled(has_content)

        else:
            # Hide stop button, show only submit
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
        """Handle attach button click — emit the button's top-left global position."""
        btn = self._attach_button
        if btn is not None:
            self.attach_requested.emit(btn.mapToGlobal(QPoint(0, 0)))
        else:
            self.attach_requested.emit(QPoint())

    def add_attachment(self, filename: str, content: str, upload_path: str = "") -> None:
        """Add a document attachment and show its chip in the bar."""
        self._attachments.append((filename, content, upload_path))
        self._rebuild_chips()
        self._update_button_states()

    def get_attachments(self) -> List[Tuple[str, str, str]]:
        """Return the list of (filename, content, upload_path) attachments."""
        return list(self._attachments)

    def clear_attachments(self) -> None:
        """Remove all attachments and hide the bar."""
        self._attachments.clear()
        self._rebuild_chips()
        self._update_button_states()

    @staticmethod
    def _ext_color(filename: str) -> str:
        """Return a background color hex string based on the file extension."""
        colors = {
            ".pdf": "#e05c4b",
            ".docx": "#2b7cd3", ".doc": "#2b7cd3",
            ".png": "#2eaa6e", ".jpg": "#2eaa6e", ".jpeg": "#2eaa6e", ".gif": "#2eaa6e",
            ".csv": "#8e44ad",
            ".json": "#e67e22", ".yaml": "#e67e22", ".yml": "#e67e22",
            ".md": "#16a085", ".rst": "#16a085",
            ".txt": "#7f8c8d",
        }
        return colors.get(os.path.splitext(filename)[1].lower(), "#7f8c8d")

    def _rebuild_chips(self) -> None:
        """Rebuild the attachment chips strip from the current attachments list."""
        if self._chips_layout is None or self._attachments_bar is None:
            return

        # Remove all existing chip widgets (leave the trailing stretch)
        while self._chips_layout.count() > 1:
            item = self._chips_layout.takeAt(0)
            if item and item.widget():
                item.widget().deleteLater()

        for index, (filename, _, _upload_path) in enumerate(self._attachments):
            ext = os.path.splitext(filename)[1].upper().lstrip('.') or "FILE"
            display_name = filename if len(filename) <= 18 else filename[:15] + "..."

            chip = QWidget()
            chip.setObjectName("_attachment_chip")
            chip_layout = QHBoxLayout(chip)
            chip_layout.setContentsMargins(6, 6, 8, 6)
            chip_layout.setSpacing(8)

            # Colored icon badge showing file type
            icon_label = QLabel(ext[:4])
            icon_label.setFixedSize(32, 32)
            icon_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            icon_label.setStyleSheet(
                f"background-color: {self._ext_color(filename)}; border-radius: 6px;"
                " color: white; font-size: 7pt; font-weight: bold; border: none;"
            )
            chip_layout.addWidget(icon_label)

            # Filename and type stacked vertically
            info = QWidget()
            info.setObjectName("_chip_info")
            info_layout = QVBoxLayout(info)
            info_layout.setContentsMargins(0, 0, 0, 0)
            info_layout.setSpacing(1)

            name_label = QLabel(display_name)
            name_label.setObjectName("_chip_label")
            info_layout.addWidget(name_label)

            type_label = QLabel(ext[:4])
            type_label.setObjectName("_chip_type_label")
            info_layout.addWidget(type_label)

            chip_layout.addWidget(info)

            remove_btn = QPushButton("✕")
            remove_btn.setObjectName("_chip_remove")
            remove_btn.setFixedSize(16, 16)
            remove_btn.setFlat(True)
            remove_btn.clicked.connect(lambda _checked, i=index: self._remove_attachment(i))
            chip_layout.addWidget(remove_btn)

            self._chips_layout.insertWidget(self._chips_layout.count() - 1, chip)

        self._attachments_bar.setVisible(bool(self._attachments))

    def _remove_attachment(self, index: int) -> None:
        """Remove the attachment at the given index."""
        if 0 <= index < len(self._attachments):
            self._attachments.pop(index)
            self._rebuild_chips()
            self._update_button_states()

    def focus_end(self) -> None:
        """Set focus to the input area and move the cursor to the end."""
        cursor = self._text_area.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.End)
        self._text_area.setTextCursor(cursor)
        self._text_area.setFocus()

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

        # Move cursor to specified position
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
