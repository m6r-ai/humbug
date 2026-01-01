import logging
import os
import time
from difflib import unified_diff
from typing import List, Tuple, Dict, Any, cast

from PySide6.QtWidgets import QPlainTextEdit, QWidget, QTextEdit, QFileDialog
from PySide6.QtCore import Qt, QRect, Signal, QTimer
from PySide6.QtGui import (
    QPainter, QTextCursor, QKeyEvent, QPalette, QBrush, QTextCharFormat,
    QResizeEvent, QPaintEvent, QTextDocument
)

from diff import DiffParseError, DiffMatchError, DiffValidationError, DiffApplicationError
from syntax import ProgrammingLanguage, ProgrammingLanguageUtils

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType, MessageBoxButton
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.style_manager import StyleManager
from humbug.tabs.code_block_highlighter import CodeBlockHighlighter
from humbug.tabs.editor.editor_diff_applier import EditorDiffApplier
from humbug.tabs.editor.editor_line_number_area import EditorLineNumberArea


class EditorWidget(QPlainTextEdit):
    """Text editor widget with line numbers, syntax highlighting, and find functionality."""

    # Content/state changes
    content_modified = Signal(bool)  # True if modified, False if saved
    text_changed = Signal()          # Emitted on every text change
    status_updated = Signal()        # Request status bar update
    file_saved = Signal(str)         # File path saved

    def __init__(self, path: str = "", untitled_number: int | None = None, parent: QWidget | None = None) -> None:
        """
        Initialize the editor widget.

        Args:
            path: Optional file path to load
            untitled_number: Optional untitled file number for new files
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("EditorWidget")

        # File state
        self._path = path
        self._untitled_number = untitled_number
        self._last_save_content = ""
        self._is_modified = False

        # Editor settings
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)  # No word wrap for code
        self.setTabStopDistance(32)  # 4 spaces worth of tab stops

        # Enable standard scrollbars
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode()

        # Setup line number area
        self._line_number_area = EditorLineNumberArea(
            self, self._line_number_area_width, self._line_number_area_paint_event
        )
        font = self._line_number_area.font()
        font.setFamilies(self._style_manager.monospace_font_families())
        self.setFont(font)
        self._line_number_area.setFont(font)

        self.blockCountChanged.connect(self._update_line_number_area_width)
        self.updateRequest.connect(self._update_line_number_area)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._update_line_number_area_width()

        # Highlighted text should retain any underlying colours (e.g. syntax highlighting)
        palette = self.palette()
        palette.setBrush(QPalette.ColorRole.HighlightedText, QBrush(Qt.BrushStyle.NoBrush))
        self.setPalette(palette)

        # Initialize find functionality
        self._matches: List[Tuple[int, int]] = []  # List of (start, end) positions
        self._current_match = -1
        self._last_search = ""

        # Programming language and syntax highlighting
        self._current_programming_language = ProgrammingLanguage.TEXT
        self._highlighter = CodeBlockHighlighter(self.document())

        # Auto-backup functionality
        self._auto_backup_timer = QTimer(self)
        self._auto_backup_timer.timeout.connect(self._auto_backup)

        # Mindspace integration
        self._mindspace_manager = MindspaceManager()
        self._mindspace_manager.settings_changed.connect(self._on_mindspace_settings_changed)

        # Connect text changes
        self.textChanged.connect(self._on_text_changed)
        self.cursorPositionChanged.connect(self.status_updated)

        # Connect to style changes
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        # Load file if path provided
        if self._path:
            self._load_file()

        # Update programming language based on path
        self._update_programming_language_from_path()

        # Update auto-backup based on current mindspace settings
        self._update_auto_backup_from_settings()

    def _load_file(self) -> None:
        """Load content from file path."""
        if not self._path or not os.path.exists(self._path):
            return

        try:
            with open(self._path, 'r', encoding='utf-8') as f:
                content = f.read()

            self.setPlainText(content)
            self._last_save_content = content
            self._set_modified(False)

        except Exception as e:
            self._logger.error("Failed to load file '%s': %s", self._path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_opening_file_title,
                strings.could_not_open.format(self._path, str(e))
            )

    def refresh_content(self) -> None:
        """
        Refresh the editor content from disk.

        This method reads the current file from disk and replaces the editor content.
        If the file is unreadable, the editor will be left empty.
        The cursor position will be reset to the beginning and any selection will be cleared.
        """
        if not self._path:
            self._logger.debug("No path set, cannot refresh content")
            return

        try:
            if os.path.exists(self._path):
                with open(self._path, 'r', encoding='utf-8') as f:
                    content = f.read()

                self._logger.debug("Refreshing content from file: %s", self._path)
                self.setPlainText(content)
                self._last_save_content = content

            else:
                self._logger.debug("File no longer exists, clearing content: %s", self._path)
                self.setPlainText("")
                self._last_save_content = ""

            self._set_modified(False)

            # Reset cursor to beginning and clear selection
            cursor = QTextCursor(self.document())
            cursor.movePosition(QTextCursor.MoveOperation.Start)
            self.setTextCursor(cursor)

        except Exception as e:
            self._logger.error("Failed to refresh content from file '%s': %s", self._path, str(e))
            # If file is unreadable, leave empty editor view
            self.setPlainText("")
            self._last_save_content = ""

            # Reset cursor to beginning
            cursor = QTextCursor(self.document())
            cursor.movePosition(QTextCursor.MoveOperation.Start)
            self.setTextCursor(cursor)

    def _update_programming_language_from_path(self) -> None:
        """Update programming language based on current path."""
        if self._path:
            new_language = ProgrammingLanguageUtils.from_file_extension(self._path)

        else:
            new_language = ProgrammingLanguage.TEXT

        self._update_programming_language(new_language)

    def _update_programming_language(self, new_language: ProgrammingLanguage) -> None:
        """
        Update the syntax highlighting language.

        Args:
            new_language: The new programming language to use
        """
        if self._current_programming_language != new_language:
            self._current_programming_language = new_language
            self._highlighter.set_language(new_language)
            self.status_updated.emit()

    def _update_auto_backup_from_settings(self) -> None:
        """Update auto-backup settings from mindspace."""
        if not self._mindspace_manager.has_mindspace():
            self._auto_backup_timer.stop()
            return

        settings = self._mindspace_manager.settings()
        if settings is None:
            self._auto_backup_timer.stop()
            return

        self._update_auto_backup_settings(settings.auto_backup, settings.auto_backup_interval)

    def _update_auto_backup_settings(self, enabled: bool, interval: int) -> None:
        """Update auto-backup settings."""
        if enabled:
            self._auto_backup_timer.setInterval(interval * 1000)  # Convert to milliseconds
            if self._is_modified:
                # If we have unsaved changes, start backup immediately
                self._auto_backup_timer.start()
            return

        clear_backups = self._auto_backup_timer.isActive()
        self._auto_backup_timer.stop()

        # Clean up any existing backups since auto-backup is disabled
        if clear_backups:
            self._cleanup_backup_files()

    def _set_modified(self, modified: bool) -> None:
        """
        Set the modified state and emit appropriate signals.

        Args:
            modified: Whether the content is modified
        """
        if self._is_modified != modified:
            self._is_modified = modified
            self.content_modified.emit(modified)

    def _on_mindspace_settings_changed(self) -> None:
        """Handle mindspace settings changes."""
        self._update_auto_backup_from_settings()

    def _on_language_changed(self) -> None:
        """Handle language changes by updating the UI."""
        self._update_line_number_area_width()
        self.viewport().update()
        self.status_updated.emit()

    def _on_text_changed(self) -> None:
        """Handle changes to editor content."""
        current_content = self.toPlainText()
        is_modified = current_content != self._last_save_content
        self._set_modified(is_modified)
        self.text_changed.emit()

        if not self._mindspace_manager.has_mindspace():
            return

        settings = self._mindspace_manager.settings()
        if settings is None:
            return

        if settings.auto_backup:
            if is_modified and not self._auto_backup_timer.isActive():
                self._auto_backup_timer.start()

            elif not is_modified:
                self._auto_backup_timer.stop()

    def _auto_backup(self) -> None:
        """Handle auto-backup functionality."""
        if not self._is_modified:
            return

        # All backups should now go in mindspace .humbug/backups
        if not self._mindspace_manager.has_mindspace():
            return  # No backups without a mindspace

        backup_dir = self._mindspace_manager.get_absolute_path(os.path.join(".humbug", "backups"))
        os.makedirs(backup_dir, exist_ok=True)

        if not self._path:
            # For untitled files, use timestamp-based backup in mindspace
            prefix = f"backup-{self._untitled_number}-"
            current_time = int(time.time())
            try:
                # Clean up old backups for this untitled file
                for file in os.listdir(backup_dir):
                    if file.startswith(prefix):
                        file_path = os.path.join(backup_dir, file)

                        # Keep only backups from last hour
                        if current_time - os.path.getctime(file_path) > 3600:
                            try:
                                os.remove(file_path)

                            except OSError as e:
                                self._logger.warning("Failed to remove old backup %s: %s", file_path, str(e))

            except OSError as e:
                self._logger.warning("Failed to clean up old backups: %s", str(e))

            backup_file = os.path.join(
                backup_dir,
                f"{prefix}{current_time}.txt"
            )

        else:
            backup_file = f"{self._path}.backup"

            # Clean up any very old backups that might have been left behind
            try:
                if os.path.exists(backup_file):
                    if time.time() - os.path.getctime(backup_file) > 86400:  # 24 hours
                        try:
                            os.remove(backup_file)

                        except OSError:
                            pass  # Ignore cleanup errors for old files

            except OSError:
                pass  # Ignore stat errors

        try:
            with open(backup_file, 'w', encoding='utf-8') as f:
                f.write(self.toPlainText())

        except Exception as e:
            self._logger.error("Failed to create backup file '%s': %s", backup_file, str(e))

    def _cleanup_backup_files(self) -> None:
        """Clean up any backup files for this editor."""
        if not self._mindspace_manager.has_mindspace():
            return

        if self._path:
            # Clean up backup for saved file
            backup_file = f"{self._path}.backup"
            try:
                if os.path.exists(backup_file):
                    os.remove(backup_file)

            except OSError as e:
                self._logger.warning("Failed to remove backup file %s: %s", backup_file, str(e))

        elif self._untitled_number:
            # Clean up backups for untitled file
            backup_dir = self._mindspace_manager.get_absolute_path(os.path.join(".humbug", "backups"))
            prefix = f"backup-{self._untitled_number}-"
            try:
                for file in os.listdir(backup_dir):
                    if file.startswith(prefix):
                        try:
                            os.remove(os.path.join(backup_dir, file))

                        except OSError as e:
                            self._logger.warning("Failed to remove backup %s: %s", file, str(e))

            except OSError as e:
                self._logger.warning("Failed to clean up backups: %s", str(e))

    def path(self) -> str:
        """Get the current file path."""
        return self._path

    def set_path(self, path: str) -> None:
        """
        Set the file path and update language detection.

        Args:
            path: Path to file
        """
        self._path = path
        self._untitled_number = None
        self._update_programming_language_from_path()

    def is_modified(self) -> bool:
        """Check if the content has been modified."""
        return self._is_modified

    def can_close(self) -> bool:
        """Check if the editor can be closed, handling unsaved changes."""
        if not self._is_modified:
            return True

        strings = self._language_manager.strings()
        document_name = self._path or f'Untitled-{self._untitled_number}'
        result = MessageBox.show_message(
            self,
            MessageBoxType.QUESTION,
            strings.save_changes_title,
            strings.unsaved_changes.format(document_name),
            [MessageBoxButton.SAVE, MessageBoxButton.DISCARD, MessageBoxButton.CANCEL]
        )

        if result == MessageBoxButton.SAVE:
            return self.save_file()

        if result == MessageBoxButton.DISCARD:
            self._set_modified(False)
            return True

        return False

    def close_widget(self) -> None:
        """Close the editor widget and clean up resources."""
        # Delete any backup files when we close
        if self._auto_backup_timer.isActive():
            self._cleanup_backup_files()

    def save_file(self) -> bool:
        """
        Save the current file.

        Returns:
            bool: True if save was successful
        """
        if not self._path:
            return self.save_file_as()

        try:
            content = self.toPlainText()
            with open(self._path, 'w', encoding='utf-8') as f:
                f.write(content)

            self._last_save_content = content
            self._set_modified(False)

            # Delete any backup files
            backup_file = f"{self._path}.backup"
            try:
                if os.path.exists(backup_file):
                    os.remove(backup_file)

            except OSError as e:
                self._logger.warning("Failed to remove backup file %s: %s", backup_file, str(e))

            self.file_saved.emit(self._path)
            return True

        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_saving_file_title,
                strings.could_not_save.format(self._path, str(e))
            )
            return False

    def save_file_as(self) -> bool:
        """
        Show save as dialog and save file.

        Returns:
            bool: True if save was successful
        """
        strings = self._language_manager.strings()
        export_dialog = QFileDialog()
        export_dialog.setWindowTitle(strings.file_dialog_save_file)

        if self._path:
            export_dialog.setDirectory(self._path)

        else:
            fd_dir = self._mindspace_manager.file_dialog_directory()
            if not fd_dir:
                return False

            export_dialog.setDirectory(fd_dir)

        export_dialog.setAcceptMode(QFileDialog.AcceptMode.AcceptSave)
        if export_dialog.exec_() != QFileDialog.DialogCode.Accepted:
            return False

        filename = export_dialog.selectedFiles()[0]
        self._mindspace_manager.update_file_dialog_directory(filename)
        self.set_path(filename)

        return self.save_file()

    def get_status_info(self) -> Dict[str, Any]:
        """
        Get status information for the status bar.

        Returns:
            Dictionary with status information
        """
        cursor = self.textCursor()
        line = cursor.blockNumber() + 1
        column = cursor.columnNumber() + 1

        # Get file info
        encoding = "UTF-8"
        line_ending = "LF"  # We could detect this from file content

        # Get language name for display
        file_type = ProgrammingLanguageUtils.get_display_name(self._current_programming_language)

        return {
            'line': line,
            'column': column,
            'encoding': encoding,
            'line_ending': line_ending,
            'type': file_type
        }

    def create_state_metadata(self, temp_state: bool) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Args:
            temp_state: Whether this is temporary state for moving tabs

        Returns:
            Dictionary containing editor state metadata
        """
        metadata: Dict[str, Any] = {}

        metadata["language"] = self._current_programming_language.name
        metadata["cursor"] = self._get_cursor_position()
        metadata["horizontal_scroll"] = self.horizontalScrollBar().value()
        metadata["vertical_scroll"] = self.verticalScrollBar().value()

        if temp_state:
            metadata["content"] = self.toPlainText()

        return metadata

    def restore_from_metadata(self, metadata: Dict[str, Any]) -> None:
        """
        Restore widget state from metadata.

        Args:
            metadata: Dictionary containing state metadata
        """
        if not metadata:
            return

        if "content" in metadata:
            self.setPlainText(metadata["content"])

        # Restore language if specified
        if "language" in metadata:
            language = ProgrammingLanguage[metadata["language"]]
            self._update_programming_language(language)

        # Restore cursor position if present
        if "cursor" in metadata:
            self._set_cursor_position(metadata["cursor"])

        # Restore scroll positions if present
        if "horizontal_scroll" in metadata:
            self.horizontalScrollBar().setValue(metadata["horizontal_scroll"])

        if "vertical_scroll" in metadata:
            self.verticalScrollBar().setValue(metadata["vertical_scroll"])

    def _set_cursor_position(self, position: Dict[str, int]) -> None:
        """
        Set cursor position in editor.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        if not position:
            return

        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.Start)

        # Move cursor to specified position
        for _ in range(position.get("line", 0)):
            cursor.movePosition(QTextCursor.MoveOperation.NextBlock)

        cursor.movePosition(
            QTextCursor.MoveOperation.Right,
            QTextCursor.MoveMode.MoveAnchor,
            position.get("column", 0)
        )

        self.setTextCursor(cursor)
        self.centerCursor()

    def _get_cursor_position(self) -> Dict[str, int]:
        """
        Get current cursor position from editor.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }

    def _line_number_area_width(self) -> int:
        """Calculate the width needed for the line number area."""
        digits = 1
        max_num = max(1, self.blockCount())
        while max_num >= 10:
            max_num //= 10
            digits += 1

        digit_width = self.fontMetrics().horizontalAdvance('9')
        return digit_width * (digits + 4)

    def _update_line_number_area_width(self) -> None:
        """Update the margins to accommodate the line numbers."""
        width = self._line_number_area_width()

        # Set margin on appropriate side based on layout direction
        if self.layoutDirection() == Qt.LayoutDirection.RightToLeft:
            self.setViewportMargins(0, 0, 0, 0)  # Right margin

        else:
            self.setViewportMargins(width, 0, 0, 0)  # Left margin

    def _update_line_number_area(self, rect: QRect, dy: int) -> None:
        """Handle updates to the line number area."""
        if dy:
            self._line_number_area.scroll(0, dy)

        else:
            self._line_number_area.update(0, rect.y(),
                self._line_number_area.width(), rect.height())

        if rect.contains(self.viewport().rect()):
            self._update_line_number_area_width()

    def resizeEvent(self, event: QResizeEvent) -> None:  # type: ignore[override]
        """Handle resize events."""
        super().resizeEvent(event)
        cr = self.contentsRect()
        width = self._line_number_area_width()

        if self.layoutDirection() == Qt.LayoutDirection.RightToLeft:
            self._line_number_area.setGeometry(
                cr.right() - width,
                cr.top(),
                width,
                cr.height()
            )

        else:
            self._line_number_area.setGeometry(
                cr.left(),
                cr.top(),
                width,
                cr.height()
            )

    def _line_number_area_paint_event(self, event: QPaintEvent) -> None:
        """Paint the line numbers."""
        painter = QPainter(self._line_number_area)
        bg_color = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE)
        painter.fillRect(event.rect(), bg_color)

        painter.setFont(self.font())

        block = self.firstVisibleBlock()
        block_number = block.blockNumber()
        offset = self.contentOffset()
        top = self.blockBoundingGeometry(block).translated(offset).top()
        bottom = top + self.blockBoundingRect(block).height()

        # Use two space widths for padding
        padding = self.fontMetrics().horizontalAdvance('9') * 2

        # Adjust alignment and padding based on layout direction
        is_rtl = self.layoutDirection() == Qt.LayoutDirection.RightToLeft
        alignment = Qt.AlignmentFlag.AlignLeft if is_rtl else Qt.AlignmentFlag.AlignRight

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                text_color = self._style_manager.get_color(ColorRole.LINE_NUMBER)
                painter.setPen(text_color)
                text_rect = QRect(
                    0,
                    int(top),
                    self._line_number_area.width() - padding,
                    self.fontMetrics().height()
                )
                painter.drawText(text_rect, alignment, number)

            block = block.next()
            top = bottom
            bottom = top + self.blockBoundingRect(block).height()
            block_number += 1

    def _indent_single_line_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Indent a single line using soft tabs.

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        # Calculate spaces needed to reach next tab stop
        current_column = cursor.position() - cursor.block().position()
        spaces_needed = tab_size - (current_column % tab_size)
        cursor.insertText(" " * spaces_needed)

    def _indent_single_line_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Indent a single line using hard tabs.

        Args:
            cursor: The current text cursor
        """
        cursor.insertText("\t")

    def _indent_block_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Indent a block of text using soft tabs.

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't indent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        start += tab_size
        while cursor.position() <= end - end_offs:
            if not cursor.atBlockEnd():
                cursor.insertText(" " * tab_size)
                end += tab_size

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                # We hit the end of the file
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _indent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Indent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't indent that line
        end_offs = 1
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        start += 1
        while cursor.position() <= end - end_offs:
            if not cursor.atBlockEnd():
                cursor.insertText("\t")
                end += 1

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                # We hit the end of the file
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _outdent_single_line_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Outdent a single line using soft tabs.

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        current_column = cursor.position() - cursor.block().position()
        deletes_needed = 1 + ((current_column - 1) % tab_size)
        deletes_needed = min(deletes_needed, current_column)

        while deletes_needed > 0:
            text = cursor.block().text()
            if not text or text[current_column - 1] != " ":
                break

            cursor.deletePreviousChar()
            current_column -= 1
            deletes_needed -= 1

    def _outdent_single_line_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Outdent a single line using hard tabs.

        Args:
            cursor: The current text cursor
        """
        current_column = cursor.position() - cursor.block().position()
        if current_column > 0:
            text = cursor.block().text()
            if text and text[current_column - 1] == "\t":
                cursor.deletePreviousChar()

    def _outdent_block_soft_tabs(self, cursor: QTextCursor, tab_size: int) -> None:
        """
        Outdent a block of text using soft tabs.

        Args:
            cursor: The current text cursor
            tab_size: Number of spaces to use for indentation
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't outdent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        # Work out how far to move the start postion
        current_column = start - cursor.block().position()
        first_line = cursor.block().text()
        first_line_spaces = len(first_line) - len(first_line.lstrip(" "))
        first_line_spaces = min(first_line_spaces, tab_size)
        first_line_spaces = min(first_line_spaces, current_column)
        start -= first_line_spaces

        while cursor.position() <= end - end_offs:
            deletes_needed = tab_size

            while deletes_needed > 0:
                text = cursor.block().text()
                if not text or text[0] != " ":
                    break

                cursor.deleteChar()
                deletes_needed -= 1
                end -= 1

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                # We hit the end of the block
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def _outdent_block_hard_tabs(self, cursor: QTextCursor) -> None:
        """
        Outdent a block of text using hard tabs.

        Args:
            cursor: The current text cursor
        """
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        reverse: bool = start == cursor.position()

        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)

        # If selection ends at start of line, don't outdent that line
        end_offs = 0
        check_cursor = QTextCursor(cursor)
        check_cursor.setPosition(end)
        if check_cursor.atBlockStart():
            end_offs = 1

        # Work out how far to move the start postion
        current_column = start - cursor.block().position()
        first_line = cursor.block().text()
        if first_line and first_line[0] == "\t" and current_column > 0:
            start -= 1

        while cursor.position() <= end - end_offs:
            text = cursor.block().text()
            if text and text[0] == "\t":
                cursor.deleteChar()
                end -= 1

            if not cursor.movePosition(QTextCursor.MoveOperation.NextBlock):
                # We hit the end of the block
                break

        cursor.setPosition(start if not reverse else end)
        cursor.setPosition(end if not reverse else start, QTextCursor.MoveMode.KeepAnchor)

    def keyPressEvent(self, event: QKeyEvent) -> None:  # type: ignore[override]
        """
        Handle special key events.

        Args:
            event: The key event to handle
        """
        if event.key() == Qt.Key.Key_Home:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.StartOfLine)
            self.setTextCursor(cursor)
            event.accept()
            return

        if event.key() == Qt.Key.Key_End:
            cursor = self.textCursor()
            cursor.movePosition(QTextCursor.MoveOperation.EndOfLine)
            self.setTextCursor(cursor)
            event.accept()
            return

        if event.key() == Qt.Key.Key_Tab:
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace():
                super().keyPressEvent(event)
                return

            settings = cast(MindspaceSettings, mindspace_manager.settings())

            scrollbar = self.verticalScrollBar()
            current_scroll = scrollbar.value()
            cursor.beginEditBlock()
            try:
                if not cursor.hasSelection():
                    if settings.use_soft_tabs:
                        self._indent_single_line_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._indent_single_line_hard_tabs(cursor)

                else:
                    if settings.use_soft_tabs:
                        self._indent_block_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._indent_block_hard_tabs(cursor)

            finally:
                cursor.endEditBlock()
                self.setTextCursor(cursor)
                scrollbar.setValue(current_scroll)

            event.accept()
            return

        if event.key() == Qt.Key.Key_Backtab:  # Shift+Tab
            cursor = self.textCursor()
            mindspace_manager = MindspaceManager()
            if not mindspace_manager.has_mindspace():
                super().keyPressEvent(event)
                return

            settings = cast(MindspaceSettings, mindspace_manager.settings())

            scrollbar = self.verticalScrollBar()
            current_scroll = scrollbar.value()
            cursor.beginEditBlock()
            try:
                if not cursor.hasSelection():
                    if settings.use_soft_tabs:
                        self._outdent_single_line_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._outdent_single_line_hard_tabs(cursor)

                else:
                    if settings.use_soft_tabs:
                        self._outdent_block_soft_tabs(cursor, settings.tab_size)

                    else:
                        self._outdent_block_hard_tabs(cursor)

            finally:
                cursor.endEditBlock()
                self.setTextCursor(cursor)
                scrollbar.setValue(current_scroll)

            event.accept()
            return

        super().keyPressEvent(event)

    def _on_style_changed(self) -> None:
        """Handle style changes affecting search highlighting."""
        # Update font size
        zoom_factor = self._style_manager.zoom_factor()
        font = self.font()
        base_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_size * zoom_factor)
        self.setFont(font)

        # Update tab stops - scale with zoom
        space_width = self._style_manager.get_space_width()
        self.setTabStopDistance(space_width * 8)

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode() != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode()
            self._highlighter.rehighlight()

        # Scale line number area
        self._update_line_number_area_width()

        self._highlight_matches()

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            {self._style_manager.get_menu_stylesheet()}

            QPlainTextEdit {{
                border: none;
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                selection-color: none;
            }}

            QScrollBar:vertical, QScrollBar:horizontal {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
                height: 12px;
            }}
            QScrollBar::handle:vertical, QScrollBar::handle:horizontal {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
                min-width: 20px;
            }}
            QScrollBar::add-page, QScrollBar::sub-page {{
                background: none;
            }}
            QScrollBar::add-line, QScrollBar::sub-line {{
                height: 0px;
                width: 0px;
            }}

            QAbstractScrollArea::corner {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
        """)

    def _find_closest_match_to_cursor(self) -> int:
        """
        Find the match closest to the current cursor position.

        Returns:
            Index of the closest match, or 0 if no matches exist
        """
        if not self._matches:
            return -1

        cursor_pos = self.textCursor().position()
        closest_index = 0
        closest_distance = abs(self._matches[0][0] - cursor_pos)

        for i, (start, end) in enumerate(self._matches):
            # Calculate distance to start of match
            distance_to_start = abs(start - cursor_pos)
            # If cursor is within the match, distance is 0
            if start <= cursor_pos <= end:
                return i

            if distance_to_start < closest_distance:
                closest_distance = distance_to_start
                closest_index = i

        return closest_index

    def find_text(self, text: str, forward: bool = True, move_cursor: bool = True) -> None:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position (only used when move_cursor is True)
            move_cursor: Whether to move cursor to a match (True for user navigation, False for automatic updates)
        """
        # Clear existing highlights if search text changed
        if text != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_match = -1
            self._last_search = text

        document = self.document()

        # Find all matches if this is a new search
        if not self._matches and text:
            cursor = QTextCursor(document)
            while True:
                cursor = document.find(text, cursor)
                if cursor.isNull():
                    break

                self._matches.append((cursor.selectionStart(), cursor.selectionEnd()))

        if not self._matches:
            return

        if move_cursor:
            # User navigation - move to next/previous match
            if forward:
                self._current_match = (self._current_match + 1) % len(self._matches)

            else:
                self._current_match = (self._current_match - 1) if self._current_match > 0 else len(self._matches) - 1

            # Scroll to current match
            self._scroll_to_match(self._current_match)

        else:
            # Automatic update - find closest match to current cursor position without moving cursor
            self._current_match = self._find_closest_match_to_cursor()

        # Highlight all matches
        self._highlight_matches()

    def set_current_match_index(self, index: int) -> None:
        """
        Set the current match index without moving the cursor.

        Args:
            index: 0-based index of the match to make current
        """
        if 0 <= index < len(self._matches):
            self._current_match = index
            self._highlight_matches()

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

        if not self._matches:
            return

        found_format = QTextCharFormat()
        found_format.setBackground(self._style_manager.get_color(ColorRole.TEXT_FOUND))
        dim_found_format = QTextCharFormat()
        dim_found_format.setBackground(self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM))

        # Create extra selections list
        selections = []

        # Highlight all matches
        for i, (start, end) in enumerate(self._matches):
            cursor = QTextCursor(self.document())
            cursor.setPosition(start)
            cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)

            # Create extra selection
            extra_selection = QTextEdit.ExtraSelection()

            # We have to tell mypy to ingore attributes it doesn't know about
            extra_selection.cursor = cursor  # type: ignore

            # Use different format for current match
            if i == self._current_match:
                extra_selection.format = found_format  # type: ignore

            else:
                extra_selection.format = dim_found_format  # type: ignore

            selections.append(extra_selection)

        # Apply selections
        self.setExtraSelections(selections)

    def _scroll_to_match(self, match_index: int) -> None:
        """
        Scroll to ensure the given match is visible.

        Args:
            match_index: Index of match to scroll to
        """
        if 0 <= match_index < len(self._matches):
            cursor = QTextCursor(self.document())
            cursor.setPosition(self._matches[match_index][0])
            self.setTextCursor(cursor)
            self.centerCursor()

    def _clear_highlights(self) -> None:
        """Clear all search highlights."""
        self.setExtraSelections([])

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        return self._current_match + 1, len(self._matches)

    def clear_find(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_match = -1
        self._last_search = ""

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return self.document().isUndoAvailable()

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return self.document().isRedoAvailable()

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self.textCursor().hasSelection()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self.textCursor().hasSelection()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return True

    def get_text_range(self, start_line: int | None = None, end_line: int | None = None) -> str:
        """
        Get text from document, optionally limited to line range.

        Args:
            start_line: Starting line number (1-indexed, inclusive), None for start of document
            end_line: Ending line number (1-indexed, inclusive), None for end of document

        Returns:
            Text content for the specified range

        Raises:
            ValueError: If line numbers are invalid
        """
        if start_line is None and end_line is None:
            return self.toPlainText()

        document = self.document()
        total_lines = document.blockCount()

        if start_line is None:
            start_line = 1

        if end_line is None:
            end_line = total_lines

        if start_line < 1:
            raise ValueError(f"start_line must be >= 1, got {start_line}")

        if end_line < start_line:
            raise ValueError(f"end_line ({end_line}) must be >= start_line ({start_line})")

        if start_line > total_lines:
            raise ValueError(f"start_line ({start_line}) exceeds document length ({total_lines} lines)")

        end_line = min(end_line, total_lines)

        start_block = document.findBlockByLineNumber(start_line - 1)
        end_block = document.findBlockByLineNumber(end_line - 1)

        if not start_block.isValid() or not end_block.isValid():
            raise ValueError("Invalid line range")

        cursor = QTextCursor(start_block)
        cursor.setPosition(end_block.position() + end_block.length() - 1, QTextCursor.MoveMode.KeepAnchor)

        text = cursor.selectedText()
        text = text.replace('\u2029', '\n')

        return text

    def get_cursor_info(self) -> Dict[str, Any]:
        """
        Get current cursor position and selection information.

        Returns:
            Dictionary with cursor and selection information:
            - line: Current line number (1-indexed)
            - column: Current column number (1-indexed)
            - has_selection: Whether text is selected
            - selection_start_line: Start line of selection (1-indexed, if has_selection)
            - selection_start_column: Start column of selection (1-indexed, if has_selection)
            - selection_end_line: End line of selection (1-indexed, if has_selection)
            - selection_end_column: End column of selection (1-indexed, if has_selection)
            - selected_text: The selected text (if has_selection)
        """
        cursor = self.textCursor()
        document = self.document()

        current_line = cursor.blockNumber() + 1
        current_column = cursor.columnNumber() + 1

        info: Dict[str, Any] = {
            'line': current_line,
            'column': current_column,
            'has_selection': cursor.hasSelection()
        }

        if cursor.hasSelection():
            selection_start = cursor.selectionStart()
            selection_end = cursor.selectionEnd()

            start_cursor = QTextCursor(document)
            start_cursor.setPosition(selection_start)
            info['selection_start_line'] = start_cursor.blockNumber() + 1
            info['selection_start_column'] = start_cursor.columnNumber() + 1

            end_cursor = QTextCursor(document)
            end_cursor.setPosition(selection_end)
            info['selection_end_line'] = end_cursor.blockNumber() + 1
            info['selection_end_column'] = end_cursor.columnNumber() + 1

            selected_text = cursor.selectedText()
            info['selected_text'] = selected_text.replace('\u2029', '\n')

        return info

    def get_editor_info(self) -> Dict[str, Any]:
        """
        Get editor metadata and document information.

        Returns:
            Dictionary with editor information:
            - line_count: Total number of lines
            - language: Programming language name
            - language_id: Programming language identifier
            - encoding: File encoding
            - is_modified: Whether document has unsaved changes
            - file_path: Path to file (empty string if untitled)
            - untitled_number: Untitled file number (None if saved file)
        """
        return {
            'line_count': self.document().blockCount(),
            'language': ProgrammingLanguageUtils.get_display_name(self._current_programming_language),
            'language_id': self._current_programming_language.name,
            'encoding': 'UTF-8',
            'is_modified': self._is_modified,
            'file_path': self._path,
            'untitled_number': self._untitled_number
        }

    def goto_line(self, line: int, column: int = 1) -> None:
        """
        Move cursor to specific line and column.

        Args:
            line: Target line number (1-indexed)
            column: Target column number (1-indexed, default 1)

        Raises:
            ValueError: If line or column is invalid
        """
        document = self.document()
        total_lines = document.blockCount()

        if line < 1:
            raise ValueError(f"line must be >= 1, got {line}")

        if line > total_lines:
            raise ValueError(f"line ({line}) exceeds document length ({total_lines} lines)")

        if column < 1:
            raise ValueError(f"column must be >= 1, got {column}")

        target_block = document.findBlockByLineNumber(line - 1)
        if not target_block.isValid():
            raise ValueError(f"Invalid line number: {line}")

        line_length = target_block.length() - 1  # -1 for newline character
        if column > line_length + 1:  # +1 because we can position at end of line
            raise ValueError(f"column ({column}) exceeds line length ({line_length})")

        cursor = QTextCursor(target_block)
        cursor.movePosition(
            QTextCursor.MoveOperation.Right,
            QTextCursor.MoveMode.MoveAnchor,
            column - 1
        )

        self.setTextCursor(cursor)
        self.centerCursor()

    def find_all_occurrences(self, search_text: str, case_sensitive: bool = False) -> List[Dict[str, Any]]:
        """
        Find all occurrences of text in the document.

        Args:
            search_text: Text to search for
            case_sensitive: Whether search should be case-sensitive

        Returns:
            List of dictionaries with match information:
            - line: Line number (1-indexed)
            - column: Column number (1-indexed)
            - match_text: The matched text
            - context: Line of text containing the match
        """
        if not search_text:
            return []

        document = self.document()
        matches: List[Dict[str, Any]] = []

        find_flags = QTextDocument.FindFlag(0)
        if case_sensitive:
            find_flags |= QTextDocument.FindFlag.FindCaseSensitively

        cursor = QTextCursor(document)
        while True:
            cursor = document.find(search_text, cursor, find_flags)
            if cursor.isNull():
                break

            line = cursor.blockNumber() + 1
            column = cursor.columnNumber() + 1
            match_text = cursor.selectedText()

            block = cursor.block()
            context = block.text()

            matches.append({
                'line': line,
                'column': column,
                'match_text': match_text,
                'context': context
            })

        return matches

    def get_selected_text(self) -> str:
        """
        Get the currently selected text.

        Returns:
            Selected text, or empty string if no selection
        """
        cursor = self.textCursor()
        if not cursor.hasSelection():
            return ""

        selected_text = cursor.selectedText()
        # Qt uses U+2029 for paragraph separators, convert to newlines
        return selected_text.replace('\u2029', '\n')

    def get_diff(self, context_lines: int = 3) -> str:
        """
        Generate a unified diff between saved file content and current buffer.

        This shows what changes would be saved if save_file() were called.
        Useful for previewing modifications before committing them to disk.

        Args:
            context_lines: Number of context lines to include in diff (default 3)

        Returns:
            Unified diff string showing changes, or empty string if:
            - No modifications exist (buffer matches saved content)
            - File has never been saved (untitled file with no saved content)

        Example:
            >>> diff = editor.get_diff()
            >>> if diff:
            ...     print("Changes to be saved:")
            ...     print(diff)
        """
        # Return empty string for untitled files or no saved content
        if not self._last_save_content and not self._path:
            return ""

        current_content = self.toPlainText()

        # Return empty string if no changes
        if current_content == self._last_save_content:
            return ""

        # Generate unified diff
        saved_lines = self._last_save_content.splitlines(keepends=True)
        current_lines = current_content.splitlines(keepends=True)
        filename = self._path if self._path else "untitled"

        diff_lines = unified_diff(
            saved_lines, current_lines, fromfile=f"a/{filename}", tofile=f"b/{filename}", n=context_lines
        )
        return ''.join(diff_lines)

    def apply_unified_diff(self, diff_text: str) -> Dict[str, Any]:
        """
        Apply a unified diff to the editor content.

        This operation is atomic - either all hunks apply successfully or none do.
        The diff is applied with fuzzy matching to handle minor line movements.

        Args:
            diff_text: Unified diff format text

        Returns:
            Dictionary with operation result:
            - success: bool - Whether the diff was applied successfully
            - message: str - Human-readable result message
            - hunks_applied: int - Number of hunks applied (if successful)
            - error_details: dict - Detailed error information (if failed)
        """
        diff_applier = EditorDiffApplier(confidence_threshold=0.75, search_window=50)
        cursor = self.textCursor()

        try:
            result = diff_applier.apply_diff(diff_text, self.document(), cursor=cursor)

        except (DiffParseError, DiffMatchError, DiffValidationError, DiffApplicationError) as e:
            # Convert diff exceptions to the format expected by callers
            error_details = getattr(e, 'error_details', None) or {
                'phase': 'diff_application',
                'reason': str(e)
            }
            return {
                'success': False,
                'message': str(e),
                'error_details': error_details
            }

        if result.success:
            # Set the cursor back to the editor to reflect the new position
            self.setTextCursor(cursor)
            self.centerCursor()

            self._set_modified(True)

        # Convert DiffApplicationResult to dict format
        return {
            'success': result.success,
            'message': result.message,
            'hunks_applied': result.hunks_applied,
            'error_details': result.error_details
        }
