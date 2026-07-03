import logging
import os
import time
from difflib import unified_diff
from typing import Any, cast

from PySide6.QtWidgets import QPlainTextEdit, QWidget, QTextEdit, QFileDialog
from PySide6.QtCore import Qt, QRect, Signal, QTimer, QRegularExpression
from PySide6.QtGui import (
    QPainter, QTextCursor, QKeyEvent, QPalette, QBrush, QTextCharFormat,
    QResizeEvent, QPaintEvent, QTextDocument, QContextMenuEvent, QWheelEvent, QTextOption
)

from diff import DiffParseError, DiffMatchError, DiffValidationError, DiffApplicationError
from editor_context.editor_diff_applier import EditorDiffApplier
from mindspace.mindspace_settings import MindspaceSettings
from syntax import ProgrammingLanguage, ProgrammingLanguageUtils

from desktop.code_block_highlighter import CodeBlockHighlighter
from desktop.mindspace.mindspace_vcs_poller import MindspaceVCSPoller
from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxType, MessageBoxButton
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from desktop.widgets import LineNumberArea, SMOOTH_SCROLL_DURATION_MS, SMOOTH_SCROLL_INTERVAL_MS


class EditorWidget(QPlainTextEdit):
    """Text editor widget with line numbers, syntax highlighting, and find functionality."""

    # Content/state changes
    content_modified = Signal(bool)  # True if modified, False if saved
    text_changed = Signal()          # Emitted on every text change
    status_updated = Signal()        # Request status bar update
    file_saved = Signal(str)         # File path saved

    # Line-comment token per language, used by the toggle-comment command.
    # Languages absent here have no line-comment support in the editor.
    _LINE_COMMENT_PREFIXES: dict[ProgrammingLanguage, str] = {
        ProgrammingLanguage.C: "//",
        ProgrammingLanguage.CPP: "//",
        ProgrammingLanguage.CSHARP: "//",
        ProgrammingLanguage.GO: "//",
        ProgrammingLanguage.JAVA: "//",
        ProgrammingLanguage.JAVASCRIPT: "//",
        ProgrammingLanguage.TYPESCRIPT: "//",
        ProgrammingLanguage.KOTLIN: "//",
        ProgrammingLanguage.RUST: "//",
        ProgrammingLanguage.SWIFT: "//",
        ProgrammingLanguage.SOLIDITY: "//",
        ProgrammingLanguage.PHP: "//",
        ProgrammingLanguage.PYTHON: "#",
        ProgrammingLanguage.BASH: "#",
        ProgrammingLanguage.RUBY: "#",
        ProgrammingLanguage.TOML: "#",
        ProgrammingLanguage.YAML: "#",
        ProgrammingLanguage.LUA: "--",
        ProgrammingLanguage.SCHEME: ";",
    }

    # Auto-closing bracket pairs and self-closing quote characters.
    _AUTO_PAIRS: dict[str, str] = {"(": ")", "[": "]", "{": "}"}
    _AUTO_QUOTES: tuple[str, ...] = ('"', "'", "`")

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
        self.setObjectName("EditorWidget")

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
        self._last_highlights_version = self._style_manager.highlights_version()

        # Setup line number area
        self._line_number_area = LineNumberArea(
            self, self._line_number_area_width, self._line_number_area_paint_event
        )
        font = self._style_manager.make_monospace_font()
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
        self._matches: list[tuple[int, int]] = []  # List of (start, end) positions
        self._current_match = -1
        self._last_search: tuple = ("", False, False)

        # Composable editor decorations (applied together via setExtraSelections).
        self._find_selections: list[QTextEdit.ExtraSelection] = []

        # Smooth scrolling
        self._smooth_scroll_timer = QTimer(self)
        self._smooth_scroll_timer.setInterval(SMOOTH_SCROLL_INTERVAL_MS)
        self._smooth_scroll_timer.timeout.connect(self._update_smooth_scroll)
        self._smooth_scroll_target: int = 0
        self._smooth_scroll_start: int = 0
        self._smooth_scroll_distance: int = 0
        self._smooth_scroll_duration: int = SMOOTH_SCROLL_DURATION_MS
        self._smooth_scroll_time: int = 0

        # Syntax highlighting
        self._syntax = ProgrammingLanguage.TEXT
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

        # Keep the current-line / bracket-match / occurrence decorations in sync
        # with the caret and selection.
        self.cursorPositionChanged.connect(self._refresh_extra_selections)
        self.selectionChanged.connect(self._refresh_extra_selections)


        # Load file if path provided
        if self._path:
            self._load_file()

        # Update syntax highlighting based on path
        self._update_syntax_from_path()

        # Update auto-backup based on current mindspace settings
        self._update_auto_backup_from_settings()
        self._apply_whitespace_rendering()

    def _load_file(self) -> None:
        """Load content from file path."""
        if not self._path or not os.path.exists(self._path):
            return

        with open(self._path, 'r', encoding='utf-8') as f:
            content = f.read()

        self.setPlainText(content)
        self._last_save_content = self.toPlainText()
        self._set_modified(False)

    def refresh_content(self) -> None:
        """
        Refresh the editor content from disk.

        This method reads the current file from disk and replaces the editor content.
        If the file is unreadable, the editor will be left empty.
        The scroll position is preserved; the cursor is moved to the start only when the
        file no longer exists.
        """
        if not self._path:
            self._logger.debug("No path set, cannot refresh content")
            return

        try:
            if os.path.exists(self._path):
                saved_vscroll = self.verticalScrollBar().value()
                saved_hscroll = self.horizontalScrollBar().value()

                with open(self._path, 'r', encoding='utf-8') as f:
                    content = f.read()

                self._logger.debug("Refreshing content from file: %s", self._path)
                self.setPlainText(content)
                self._last_save_content = self.toPlainText()
                self._set_modified(False)

                # Restore scroll position.  setPlainText resets both scrollbars to
                # 0, so we put them back immediately; the range is valid at this point.
                self.verticalScrollBar().setValue(saved_vscroll)
                self.horizontalScrollBar().setValue(saved_hscroll)

            else:
                self._logger.debug("File no longer exists, clearing content: %s", self._path)
                self.setPlainText("")
                self._last_save_content = ""
                self._set_modified(False)

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

    def _update_syntax_from_path(self) -> None:
        """Update syntax highlighting based on current path."""
        if self._path:
            new_syntax = ProgrammingLanguageUtils.from_file_extension(self._path)

        else:
            new_syntax = ProgrammingLanguage.TEXT

        self._update_syntax(new_syntax)

    def _update_syntax(self, new_syntax: ProgrammingLanguage) -> None:
        """
        Update the syntax highlighting.

        Args:
            new_syntax: The new programming language to use
        """
        if self._syntax != new_syntax:
            self._syntax = new_syntax
            self._highlighter.set_syntax(new_syntax)
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
        self._apply_whitespace_rendering()

    def _apply_whitespace_rendering(self) -> None:
        """Show or hide rendered spaces/tabs per the mindspace setting."""
        show = False
        if self._mindspace_manager.has_mindspace():
            show = cast(MindspaceSettings, self._mindspace_manager.settings()).show_whitespace

        option = self.document().defaultTextOption()
        flags = option.flags()
        if show:
            flags |= QTextOption.Flag.ShowTabsAndSpaces

        else:
            flags &= ~QTextOption.Flag.ShowTabsAndSpaces

        option.setFlags(flags)
        self.document().setDefaultTextOption(option)

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

        if self._matches:
            self._matches = []

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
        self._update_syntax_from_path()

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
            try:
                return self.save_file()

            except Exception as e:
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.error_saving_file_title,
                    strings.could_not_save.format(document_name, str(e))
                )
                return False

        if result == MessageBoxButton.DISCARD:
            self._set_modified(False)
            return True

        return False

    def close_widget(self) -> None:
        """Close the editor widget and clean up resources."""
        # Delete any backup files when we close
        if self._auto_backup_timer.isActive():
            self._cleanup_backup_files()

    def _apply_save_cleanup(self) -> None:
        """Trim trailing whitespace and/or ensure a final newline per settings."""
        if not self._mindspace_manager.has_mindspace():
            return

        settings = cast(MindspaceSettings, self._mindspace_manager.settings())
        if not (settings.trim_trailing_whitespace or settings.ensure_final_newline):
            return

        newline = chr(10)
        original = self.toPlainText()
        lines = original.split(newline)
        if settings.trim_trailing_whitespace:
            lines = [line.rstrip(" \t") for line in lines]

        cleaned = newline.join(lines)
        if settings.ensure_final_newline and cleaned and not cleaned.endswith(newline):
            cleaned += newline

        if cleaned == original:
            return

        # Replace the whole document as one undoable edit, keeping the caret and
        # scroll position stable.
        caret = self.textCursor().position()
        vscroll = self.verticalScrollBar().value()
        editor = self.textCursor()
        editor.beginEditBlock()
        editor.select(QTextCursor.SelectionType.Document)
        editor.insertText(cleaned)
        editor.endEditBlock()

        restored = self.textCursor()
        restored.setPosition(min(caret, len(cleaned)))
        self.setTextCursor(restored)
        self.verticalScrollBar().setValue(vscroll)

    def save_file(self) -> bool:
        """
        Save the current file.

        Returns:
            bool: True if save was successful
        """
        if not self._path:
            return self.save_file_as()

        self._apply_save_cleanup()
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

        try:
            return self.save_file()

        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_saving_file_title,
                strings.could_not_save.format(filename, str(e))
            )
            return False

    def get_status_info(self) -> dict[str, Any]:
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
        file_type = ProgrammingLanguageUtils.get_display_name(self._syntax)

        return {
            'line': line,
            'column': column,
            'encoding': encoding,
            'line_ending': line_ending,
            'type': file_type
        }

    def create_state_metadata(self, temp_state: bool) -> dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Args:
            temp_state: Whether this is temporary state for moving tabs

        Returns:
            Dictionary containing editor state metadata
        """
        metadata: dict[str, Any] = {}

        metadata["syntax"] = self._syntax.name
        metadata["cursor"] = self._get_cursor_position()
        metadata["horizontal_scroll"] = self.horizontalScrollBar().value()
        metadata["vertical_scroll"] = self.verticalScrollBar().value()

        if temp_state:
            metadata["content"] = self.toPlainText()

        return metadata

    def restore_from_metadata(self, metadata: dict[str, Any]) -> None:
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
            self._update_syntax(language)

        # Restore cursor position if present
        if "cursor" in metadata:
            self._set_cursor_position(metadata["cursor"])

        # Restore scroll positions if present
        if "horizontal_scroll" in metadata:
            self.horizontalScrollBar().setValue(metadata["horizontal_scroll"])

        if "vertical_scroll" in metadata:
            self.verticalScrollBar().setValue(metadata["vertical_scroll"])

    def _set_cursor_position(self, position: dict[str, int]) -> None:
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
        self._start_smooth_scroll_to_cursor(cursor)

    def _get_cursor_position(self) -> dict[str, int]:
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

    def paintEvent(self, event: QPaintEvent) -> None:  # type: ignore[override]
        """Paint the editor, then overlay indentation guide lines."""
        super().paintEvent(event)
        self._paint_indent_guides(event)

    def wheelEvent(self, event: QWheelEvent) -> None:  # type: ignore[override]
        """Ctrl+scroll adjusts the application zoom; otherwise scroll normally."""
        if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            delta = event.angleDelta().y()
            if delta != 0:
                self._zoom_by(1 if delta > 0 else -1)

            event.accept()
            return

        super().wheelEvent(event)

    def _zoom_by(self, direction: int) -> None:
        """Step the application zoom by one point, keeping an integer font size."""
        base = self._style_manager.base_font_size()
        if base <= 0:
            return

        current_pt = round(base * self._style_manager.zoom_factor())
        min_pt = max(1, round(base * 0.5))
        max_pt = round(base * 2.0)
        new_pt = min(max_pt, max(min_pt, current_pt + direction))
        self._style_manager.set_zoom(new_pt / base)

    def _paint_indent_guides(self, event: QPaintEvent) -> None:
        """Draw subtle vertical guides at each indentation level of visible lines."""
        space_width = self.fontMetrics().horizontalAdvance(" ")
        _, tab_size = self._current_indent_settings()
        if space_width <= 0 or tab_size <= 0:
            return

        painter = QPainter(self.viewport())
        painter.setPen(self._style_manager.get_color(ColorRole.EDITOR_INDENT_GUIDE))
        offset = self.contentOffset()
        margin = self.document().documentMargin()
        rect_bottom = event.rect().bottom()

        block = self.firstVisibleBlock()
        while block.isValid():
            geometry = self.blockBoundingGeometry(block).translated(offset)
            if geometry.top() > rect_bottom:
                break

            if block.isVisible():
                indent_cols = self._leading_indent_columns(block.text(), tab_size)
                level = tab_size
                while level < indent_cols:
                    x = int(geometry.left() + margin + level * space_width)
                    painter.drawLine(x, int(geometry.top()), x, int(geometry.bottom()) - 1)
                    level += tab_size

            block = block.next()

        painter.end()

    @staticmethod
    def _leading_indent_columns(text: str, tab_size: int) -> int:
        """Return the visual column width of *text*'s leading whitespace."""
        cols = 0
        for ch in text:
            if ch == " ":
                cols += 1

            elif ch == "\t":
                cols += tab_size - (cols % tab_size)

            else:
                break

        return cols

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
                block_height = int(self.blockBoundingRect(block).height())
                text_rect = QRect(
                    0,
                    int(top),
                    self._line_number_area.width() - padding,
                    block_height
                )
                painter.drawText(text_rect, alignment | Qt.AlignmentFlag.AlignVCenter, number)

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
        modifiers = event.modifiers()
        key = event.key()
        ctrl = bool(modifiers & Qt.KeyboardModifier.ControlModifier)
        alt = bool(modifiers & Qt.KeyboardModifier.AltModifier)
        shift = bool(modifiers & Qt.KeyboardModifier.ShiftModifier)

        # Toggle line comment (Ctrl+/).
        if ctrl and not alt and key == Qt.Key.Key_Slash:
            self._toggle_line_comment()
            event.accept()
            return

        # Delete the current line(s) (Ctrl+Shift+K).
        if ctrl and shift and key == Qt.Key.Key_K:
            self._delete_lines()
            event.accept()
            return

        # Duplicate the current line(s) (Shift+Alt+Up/Down).
        if alt and shift and key in (Qt.Key.Key_Up, Qt.Key.Key_Down):
            self._duplicate_lines()
            event.accept()
            return

        # Move the current line(s) up/down (Alt+Up/Down).
        if alt and not ctrl and not shift and key in (Qt.Key.Key_Up, Qt.Key.Key_Down):
            self._move_lines(-1 if key == Qt.Key.Key_Up else 1)
            event.accept()
            return

        # Auto-indent on Enter/Return (plain, or with Shift).
        if not ctrl and not alt and key in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            self._insert_newline_with_indent()
            event.accept()
            return

        # Delete an empty auto-inserted pair with a single Backspace.
        if key == Qt.Key.Key_Backspace and not modifiers and self._backspace_between_pair():
            event.accept()
            return

        # Auto-close brackets/quotes, wrap the selection, or skip over a closer.
        if not ctrl and not alt and event.text() and self._handle_auto_pair(event.text()):
            event.accept()
            return

        # Smart Home: first press → first non-whitespace, second → column 0.
        if key == Qt.Key.Key_Home and not ctrl and not alt:
            cursor = self.textCursor()
            mode = QTextCursor.MoveMode.KeepAnchor if shift else QTextCursor.MoveMode.MoveAnchor
            block = cursor.block()
            text = block.text()
            first_non_ws = len(text) - len(text.lstrip(" \t"))
            col = cursor.position() - block.position()
            target_col = 0 if col == first_non_ws else first_non_ws
            cursor.setPosition(block.position() + target_col, mode)
            self.setTextCursor(cursor)
            event.accept()
            return

        if event.key() == Qt.Key.Key_End:
            cursor = self.textCursor()
            mode = (
                QTextCursor.MoveMode.KeepAnchor
                if event.modifiers() & Qt.KeyboardModifier.ShiftModifier
                else QTextCursor.MoveMode.MoveAnchor
            )
            cursor.movePosition(QTextCursor.MoveOperation.EndOfLine, mode)
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

    def apply_style(self) -> None:
        """Apply current style settings."""
        # Capture the block number at the viewport midpoint before the font
        # changes so we can restore it to the centre after re-layout.
        visible_lines = self.viewport().height() // max(1, self.fontMetrics().lineSpacing())
        centre_block = self.verticalScrollBar().value() + visible_lines // 2

        font = self._style_manager.make_monospace_font()
        self.setFont(font)

        # Update tab stops - scale with zoom
        space_width = self._style_manager.get_space_width()
        self.setTabStopDistance(space_width * 8)

        # Rehighlight when highlight formats change (covers palette and mode switches)
        current_hv = self._style_manager.highlights_version()
        if current_hv != self._last_highlights_version:
            self._last_highlights_version = current_hv
            self._highlighter.rehighlight()

        # Scale line number area
        self._update_line_number_area_width()

        self._highlight_matches()

        # Re-layout from setFont() is async, so defer the scroll restoration.
        QTimer.singleShot(0, lambda: self._restore_centre_block(centre_block))

    def _restore_centre_block(self, centre_block: int) -> None:
        """Scroll so that centre_block sits at the vertical midpoint of the viewport."""
        vbar = self.verticalScrollBar()
        visible_lines = self.viewport().height() // max(1, self.fontMetrics().lineSpacing())
        target = max(vbar.minimum(), min(vbar.maximum(), centre_block - visible_lines // 2))
        vbar.setValue(target)

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

    def find_text(
        self,
        text: str,
        forward: bool = True,
        move_cursor: bool = True,
        case_sensitive: bool = False,
        regexp: bool = False
    ) -> None:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position (only used when move_cursor is True)
            move_cursor: Whether to move cursor to a match (True for user navigation, False for automatic updates)
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.
        """
        # Clear existing highlights if search text changed
        if (text, case_sensitive, regexp) != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_match = -1
            self._last_search = (text, case_sensitive, regexp)

        document = self.document()

        # Find all matches if this is a new search
        rescanned = False
        if not self._matches and text:
            rescanned = True
            max_matches = 500
            cursor = QTextCursor(document)
            if regexp:
                flags = QRegularExpression.PatternOption(0)
                if not case_sensitive:
                    flags |= QRegularExpression.PatternOption.CaseInsensitiveOption

                pattern = QRegularExpression(text, flags)
                if pattern.isValid():
                    find_flags = QTextDocument.FindFlag(0)
                    if case_sensitive:
                        find_flags |= QTextDocument.FindFlag.FindCaseSensitively

                    while True:
                        cursor = document.find(pattern, cursor, find_flags)
                        if cursor.isNull():
                            break

                        self._matches.append((cursor.selectionStart(), cursor.selectionEnd()))
                        if len(self._matches) >= max_matches:
                            break

            else:
                find_flags = QTextDocument.FindFlag(0)
                if case_sensitive:
                    find_flags |= QTextDocument.FindFlag.FindCaseSensitively

                while True:
                    cursor = document.find(text, cursor, find_flags)
                    if cursor.isNull():
                        break

                    self._matches.append((cursor.selectionStart(), cursor.selectionEnd()))
                    if len(self._matches) >= max_matches:
                        break

        if not self._matches:
            return

        if move_cursor:
            # User navigation - move to next/previous match
            # If we just re-scanned because matches were invalidated by an edit,
            # re-anchor _current_match from the cursor position before stepping,
            # so we don't navigate from a stale index.
            if rescanned:
                self._current_match = self._last_match_before_cursor()

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

    def find_text_at_line(
        self,
        text: str,
        line_number: int,
        case_sensitive: bool = False,
        regexp: bool = False,
    ) -> None:
        """
        Highlight all matches and scroll to the first match on the given 1-based line number.

        If already positioned on a match on this line, advances to the next match on the
        same line (wrapping within the line). Falls back to the first match in the document
        if no match is found on that line.

        Args:
            text: Text to search for
            line_number: 1-based line number to scroll to
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.
        """
        self.find_text(text, forward=True, move_cursor=False, case_sensitive=case_sensitive, regexp=regexp)
        if not self._matches:
            return

        document = self.document()
        line_indices = [
            i for i, (start, _end) in enumerate(self._matches)
            if document.findBlock(start).blockNumber() + 1 == line_number
        ]
        if not line_indices:
            target = 0

        elif self._current_match in line_indices:
            pos = line_indices.index(self._current_match)
            target = line_indices[(pos + 1) % len(line_indices)]

        else:
            target = line_indices[0]

        self._current_match = target
        self._highlight_matches()
        self._scroll_to_match(target)

    def set_current_match_index(self, index: int) -> None:
        """
        Set the current match index without moving the cursor.

        Args:
            index: 0-based index of the match to make current
        """
        if 0 <= index < len(self._matches):
            self._current_match = index
            self._highlight_matches()

    def refresh_find(self) -> None:
        """
        Re-run the current search after a document edit, restoring the match index.

        Clears the cached match list and re-scans the document using the same
        search parameters as the last search.  After re-scanning, _current_match
        is set to the last match before the cursor so that the next forward
        navigation lands on the first match at or after the cursor.  The cursor
        and scroll position are not changed.
        """
        search_text, case_sensitive, regexp = self._last_search
        if not search_text:
            return

        self._matches = []
        self._current_match = -1
        # Reset _last_search so find_text treats this as a new search and
        # re-populates _matches, but keep the same search parameters.
        self._last_search = ("", False, False)
        self.find_text(search_text, forward=True, move_cursor=False, case_sensitive=case_sensitive, regexp=regexp)

        if self._matches:
            self._current_match = self._last_match_before_cursor()
            self._highlight_matches()

    def _last_match_before_cursor(self) -> int:
        """
        Return the index of the last match whose start is at or before the cursor.

        This is used after a re-scan so that the next forward navigation lands
        on the first match after the cursor position.  Returns -1 if all
        matches are after the cursor (so the next forward step wraps to
        match 0).
        """
        cursor_pos = self.textCursor().position()
        result = -1
        for i, (start, _end) in enumerate(self._matches):
            if start <= cursor_pos:
                result = i

            else:
                break

        return result

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

        # Hand the find matches to the shared composer so they coexist with the
        # current-line, bracket-match, and occurrence decorations.
        self._find_selections = selections
        self._refresh_extra_selections()

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
            self._start_smooth_scroll_to_cursor(cursor)

    def _clear_highlights(self) -> None:
        """Clear the search-match highlights (other decorations are preserved)."""
        self._find_selections = []
        self._refresh_extra_selections()

    def _refresh_extra_selections(self) -> None:
        """
        Recompose every editor decoration and apply it in one pass.

        Qt only supports a single ``setExtraSelections`` list, so the current-line
        tint, occurrence highlights, search matches, and bracket-match markers are
        all built here and applied together.  Order matters: later entries paint
        over earlier ones where they overlap.
        """
        selections: list[QTextEdit.ExtraSelection] = []
        selections.extend(self._current_line_selections())
        selections.extend(self._occurrence_selections())
        selections.extend(self._find_selections)
        selections.extend(self._bracket_match_selections())
        self.setExtraSelections(selections)

    def _make_selection(self, start: int, end: int, color: ColorRole,
                        full_width: bool = False) -> "QTextEdit.ExtraSelection":
        """Build a single extra-selection over [start, end) with the given colour."""
        cursor = QTextCursor(self.document())
        cursor.setPosition(start)
        cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)

        selection = QTextEdit.ExtraSelection()
        fmt = QTextCharFormat()
        fmt.setBackground(self._style_manager.get_color(color))
        if full_width:
            fmt.setProperty(QTextCharFormat.Property.FullWidthSelection, True)

        selection.format = fmt  # type: ignore[attr-defined]
        selection.cursor = cursor  # type: ignore[attr-defined]
        return selection

    def _current_line_selections(self) -> list["QTextEdit.ExtraSelection"]:
        """Return a full-width tint for the caret's line (only when there's no selection)."""
        cursor = self.textCursor()
        if cursor.hasSelection():
            return []

        block = cursor.block()
        return [self._make_selection(
            block.position(), block.position(), ColorRole.EDITOR_CURRENT_LINE, full_width=True
        )]

    def _bracket_match_selections(self) -> list["QTextEdit.ExtraSelection"]:
        """Highlight the bracket adjacent to the caret and its matching partner."""
        cursor = self.textCursor()
        if cursor.hasSelection():
            return []

        text = self.toPlainText()
        pos = cursor.position()
        pairs = {"(": ")", "[": "]", "{": "}"}
        closers = {v: k for k, v in pairs.items()}

        # Prefer the bracket to the right of the caret, else the one to the left.
        for probe in (pos, pos - 1):
            if 0 <= probe < len(text):
                ch = text[probe]
                if ch in pairs:
                    match = self._find_matching_bracket(text, probe, 1, ch, pairs[ch])

                elif ch in closers:
                    match = self._find_matching_bracket(text, probe, -1, ch, closers[ch])

                else:
                    continue

                if match is not None:
                    return [
                        self._make_selection(probe, probe + 1, ColorRole.EDITOR_BRACKET_MATCH),
                        self._make_selection(match, match + 1, ColorRole.EDITOR_BRACKET_MATCH),
                    ]

        return []

    @staticmethod
    def _find_matching_bracket(text: str, start: int, direction: int,
                               open_ch: str, close_ch: str) -> int | None:
        """Scan from *start* in *direction* for the bracket matching *open_ch*."""
        depth = 0
        i = start
        while 0 <= i < len(text):
            ch = text[i]
            if ch == open_ch:
                depth += 1

            elif ch == close_ch:
                depth -= 1
                if depth == 0:
                    return i

            i += direction

        return None

    def _occurrence_selections(self) -> list["QTextEdit.ExtraSelection"]:
        """Highlight other occurrences of the selected word."""
        cursor = self.textCursor()
        if not cursor.hasSelection():
            return []

        # Only highlight a single-line, word-like selection of a sensible length.
        selected = cursor.selectedText()
        stripped = selected.strip()
        if len(stripped) < 2 or stripped != selected or not self._is_word_like(selected):
            return []

        text = self.toPlainText()
        if len(text) > 500_000:
            return []

        sel_start = cursor.selectionStart()
        selections: list[QTextEdit.ExtraSelection] = []
        needle_len = len(selected)
        idx = text.find(selected)
        while idx != -1:
            if idx != sel_start:  # skip the active selection itself
                selections.append(
                    self._make_selection(idx, idx + needle_len, ColorRole.EDITOR_OCCURRENCE)
                )

            idx = text.find(selected, idx + needle_len)

        return selections

    @staticmethod
    def _is_word_like(s: str) -> bool:
        """True if *s* is a single identifier-style token (letters, digits, underscore)."""
        return all(c.isalnum() or c == "_" for c in s)

    # -- Auto-closing brackets and quotes ------------------------------------

    def _handle_auto_pair(self, ch: str) -> bool:
        """
        Handle a typed bracket or quote, returning True if it was consumed.

        Wraps a selection, skips over an existing closer, or inserts a matching
        pair with the caret placed between the two characters.
        """
        cursor = self.textCursor()
        text = self.toPlainText()

        # Wrap the current selection in the pair.
        if cursor.hasSelection() and (ch in self._AUTO_PAIRS or ch in self._AUTO_QUOTES):
            closer = self._AUTO_PAIRS.get(ch, ch)
            start = cursor.selectionStart()
            # Qt uses U+2029 (paragraph separator) for newlines in selected text.
            inner = cursor.selectedText().replace(chr(0x2029), chr(10))
            cursor.beginEditBlock()
            cursor.insertText(ch + inner + closer)
            cursor.endEditBlock()
            reselect = self.textCursor()
            reselect.setPosition(start + 1)
            reselect.setPosition(start + 1 + len(inner), QTextCursor.MoveMode.KeepAnchor)
            self.setTextCursor(reselect)
            return True

        if cursor.hasSelection():
            return False

        pos = cursor.position()
        next_ch = text[pos] if pos < len(text) else ""

        # Type over an existing closing bracket or quote instead of duplicating it.
        if (ch in self._AUTO_PAIRS.values() or ch in self._AUTO_QUOTES) and next_ch == ch:
            cursor.movePosition(QTextCursor.MoveOperation.NextCharacter)
            self.setTextCursor(cursor)
            return True

        # Insert a matching bracket pair.
        if ch in self._AUTO_PAIRS:
            cursor.beginEditBlock()
            cursor.insertText(ch + self._AUTO_PAIRS[ch])
            cursor.endEditBlock()
            cursor.movePosition(QTextCursor.MoveOperation.PreviousCharacter)
            self.setTextCursor(cursor)
            return True

        # Insert a matching quote pair, unless we're right after a word (e.g. an
        # apostrophe in "don't") where a single quote is almost always intended.
        if ch in self._AUTO_QUOTES:
            prev_ch = text[pos - 1] if pos > 0 else ""
            if prev_ch.isalnum():
                return False

            cursor.beginEditBlock()
            cursor.insertText(ch + ch)
            cursor.endEditBlock()
            cursor.movePosition(QTextCursor.MoveOperation.PreviousCharacter)
            self.setTextCursor(cursor)
            return True

        return False

    def _backspace_between_pair(self) -> bool:
        """Delete both characters of an empty pair when Backspace is pressed between them."""
        cursor = self.textCursor()
        if cursor.hasSelection():
            return False

        pos = cursor.position()
        text = self.toPlainText()
        if pos == 0 or pos >= len(text):
            return False

        prev_ch = text[pos - 1]
        next_ch = text[pos]
        is_pair = self._AUTO_PAIRS.get(prev_ch) == next_ch or (
            prev_ch in self._AUTO_QUOTES and next_ch == prev_ch
        )
        if not is_pair:
            return False

        cursor.beginEditBlock()
        cursor.deleteChar()
        cursor.deletePreviousChar()
        cursor.endEditBlock()
        return True

    # -- Indentation / line editing helpers ----------------------------------

    def _current_indent_settings(self) -> tuple[bool, int]:
        """Return (use_soft_tabs, tab_size), defaulting to 4-space soft tabs."""
        mindspace_manager = MindspaceManager()
        if mindspace_manager.has_mindspace():
            settings = cast(MindspaceSettings, mindspace_manager.settings())
            return settings.use_soft_tabs, settings.tab_size

        return True, 4

    def _selected_line_range(self, cursor: QTextCursor) -> tuple[int, int]:
        """Return the inclusive (first, last) block numbers the selection covers."""
        # A selection ending exactly at a line start does not pull in that
        # trailing line (matching how block indent/outdent already behaves).
        doc = self.document()
        start = cursor.selectionStart()
        end = cursor.selectionEnd()
        first = doc.findBlock(start).blockNumber()
        last = doc.findBlock(end).blockNumber()
        if last > first and doc.findBlock(end).position() == end:
            last -= 1

        return first, last

    def _insert_newline_with_indent(self) -> None:
        """Insert a newline, carrying the current line's indent plus one extra level."""
        cursor = self.textCursor()
        block_text = cursor.block().text()
        col = cursor.position() - cursor.block().position()
        before_cursor = block_text[:col]
        indent = before_cursor[:len(before_cursor) - len(before_cursor.lstrip(" \t"))]

        use_soft, tab_size = self._current_indent_settings()
        one_level = " " * tab_size if use_soft else "\t"
        extra = one_level if before_cursor.rstrip().endswith(("{", ":", "(", "[")) else ""

        cursor.beginEditBlock()
        cursor.insertText("\n" + indent + extra)
        cursor.endEditBlock()
        self.setTextCursor(cursor)

    def _toggle_line_comment(self) -> None:
        """Comment or uncomment the selected lines using the language's line token."""
        prefix = self._LINE_COMMENT_PREFIXES.get(self._syntax)
        if not prefix:
            return

        doc = self.document()
        cursor = self.textCursor()
        first, last = self._selected_line_range(cursor)
        blocks = [doc.findBlockByNumber(n) for n in range(first, last + 1)]

        # Comment unless every non-blank line is already commented (then uncomment).
        non_blank = [b for b in blocks if b.text().strip()]
        all_commented = bool(non_blank) and all(
            b.text().lstrip(" \t").startswith(prefix) for b in non_blank
        )

        edit = QTextCursor(doc)
        edit.beginEditBlock()
        try:
            for block in blocks:
                text = block.text()
                if not text.strip():
                    continue

                indent_len = len(text) - len(text.lstrip(" \t"))
                edit.setPosition(block.position() + indent_len)
                if all_commented:
                    after = text[indent_len:]
                    remove_len = len(prefix) + 1 if after.startswith(prefix + " ") else len(prefix)
                    edit.setPosition(edit.position() + remove_len, QTextCursor.MoveMode.KeepAnchor)
                    edit.removeSelectedText()

                else:
                    edit.insertText(prefix + " ")

        finally:
            edit.endEditBlock()

    def _line_segment(self, first: int, last: int) -> tuple[int, int]:
        """Return the (start, end) character offsets spanning whole blocks first..last."""
        doc = self.document()
        first_block = doc.findBlockByNumber(first)
        last_block = doc.findBlockByNumber(last)
        return first_block.position(), last_block.position() + len(last_block.text())

    def _duplicate_lines(self) -> None:
        """Duplicate the current line(s) below the selection."""
        cursor = self.textCursor()
        first, last = self._selected_line_range(cursor)
        seg_start, seg_end = self._line_segment(first, last)
        segment = self.toPlainText()[seg_start:seg_end]

        edit = self.textCursor()
        edit.beginEditBlock()
        edit.setPosition(seg_end)
        edit.insertText("\n" + segment)
        edit.endEditBlock()

    def _delete_lines(self) -> None:
        """Delete the current line(s) entirely, including a joining newline."""
        doc = self.document()
        cursor = self.textCursor()
        first, last = self._selected_line_range(cursor)
        seg_start, seg_end = self._line_segment(first, last)

        edit = QTextCursor(doc)
        edit.beginEditBlock()
        if last < doc.blockCount() - 1:
            # Consume the newline after the block range.
            edit.setPosition(seg_start)
            edit.setPosition(doc.findBlockByNumber(last + 1).position(), QTextCursor.MoveMode.KeepAnchor)

        else:
            # Last line in the document: consume the newline before it instead.
            start = seg_start
            if first > 0:
                prev = doc.findBlockByNumber(first - 1)
                start = prev.position() + len(prev.text())

            edit.setPosition(start)
            edit.setPosition(seg_end, QTextCursor.MoveMode.KeepAnchor)

        edit.removeSelectedText()
        edit.endEditBlock()

    def _move_lines(self, direction: int) -> None:
        """Move the current line(s) up (direction<0) or down (direction>0)."""
        doc = self.document()
        cursor = self.textCursor()
        first, last = self._selected_line_range(cursor)

        if direction < 0 and first == 0:
            return

        if direction > 0 and last == doc.blockCount() - 1:
            return

        seg_start, seg_end = self._line_segment(first, last)
        text = self.toPlainText()

        edit = QTextCursor(doc)
        edit.beginEditBlock()
        try:
            if direction < 0:
                prev = doc.findBlockByNumber(first - 1)
                # Remove the previous line plus its trailing newline...
                edit.setPosition(prev.position())
                edit.setPosition(seg_start, QTextCursor.MoveMode.KeepAnchor)
                prev_line = text[prev.position():seg_start - 1]  # exclude the newline
                edit.removeSelectedText()
                # ...and reinsert it after the moved block, which shifted up.
                new_end = seg_end - (seg_start - prev.position())
                edit.setPosition(new_end)
                edit.insertText("\n" + prev_line)

            else:
                nxt = doc.findBlockByNumber(last + 1)
                next_end = nxt.position() + len(nxt.text())
                next_line = text[nxt.position():next_end]
                # Remove the next line plus the newline joining it to the block...
                edit.setPosition(seg_end)
                edit.setPosition(next_end, QTextCursor.MoveMode.KeepAnchor)
                edit.removeSelectedText()
                # ...and reinsert it before the moved block.
                edit.setPosition(seg_start)
                edit.insertText(next_line + "\n")

        finally:
            edit.endEditBlock()

    def _start_smooth_scroll_to_cursor(self, cursor: QTextCursor) -> None:
        """
        Start a smooth scroll to centre the given cursor position in the viewport.

        Args:
            cursor: The cursor whose block should be centred in the viewport
        """
        vbar = self.verticalScrollBar()
        block_number = cursor.block().blockNumber()
        visible_lines = self.viewport().height() // max(1, self.fontMetrics().lineSpacing())
        target = max(vbar.minimum(), min(vbar.maximum(), block_number - visible_lines // 2))

        if self._smooth_scroll_timer.isActive():
            self._smooth_scroll_timer.stop()

        self._smooth_scroll_start = vbar.value()
        self._smooth_scroll_target = target
        self._smooth_scroll_distance = target - self._smooth_scroll_start
        self._smooth_scroll_time = 0
        self._smooth_scroll_timer.start()

    def _update_smooth_scroll(self) -> None:
        """Update the smooth scrolling animation."""
        self._smooth_scroll_time += self._smooth_scroll_timer.interval()
        progress = min(1.0, self._smooth_scroll_time / self._smooth_scroll_duration)
        t = 1 - (1 - progress) ** 3

        # Add 0.5 lines of bias so that int() truncation crosses each line boundary
        # slightly early, avoiding a visible "jump" at the very end of the animation
        # where the easing curve decelerates so slowly that the final line is only
        # reached on the last tick, well after the scroll appears to have stopped.
        new_position = min(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t + 0.5),
        ) if self._smooth_scroll_distance > 0 else max(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t - 0.5),
        )
        self.verticalScrollBar().setValue(new_position)
        if progress >= 1.0 or new_position == self._smooth_scroll_target:
            self._smooth_scroll_timer.stop()

    def get_match_status(self) -> tuple[int, int, bool]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches, truncated)
        """
        total = len(self._matches)
        return self._current_match + 1, total, total == 500

    def clear_find(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_match = -1
        self._last_search = ("", False, False)

    def clear_highlights(self) -> None:
        """Remove find highlights without resetting match state."""
        self._clear_highlights()

    def replace_current(self, replace_text: str) -> bool:
        """
        Replace the current highlighted match with replace_text and advance to the next match.

        When regexp mode was used for the last search, back-references in replace_text
        (e.g. \\1, \\2) are expanded using the captured groups of the current match.

        Args:
            replace_text: Text to substitute for the current match.

        Returns:
            True if a replacement was made, False if there was no current match.
        """
        if not self._matches:
            return False

        self._current_match = max(self._current_match, 0)

        start, end = self._matches[self._current_match]
        _search_text, case_sensitive, regexp = self._last_search

        actual_replacement = replace_text
        if regexp and _search_text:
            flags = QRegularExpression.PatternOption(0)
            if not case_sensitive:
                flags |= QRegularExpression.PatternOption.CaseInsensitiveOption

            pattern = QRegularExpression(_search_text, flags)
            if pattern.isValid():
                document_text = self.toPlainText()
                match_text = document_text[start:end]
                re_match = pattern.match(match_text)
                if re_match.hasMatch():
                    actual_replacement = replace_text
                    for i in range(1, re_match.lastCapturedIndex() + 1):
                        actual_replacement = actual_replacement.replace(f"\\{i}", re_match.captured(i))

        cursor = QTextCursor(self.document())
        cursor.setPosition(start)
        cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)
        cursor.insertText(actual_replacement)

        self._set_modified(True)

        # Re-run the search from scratch so match positions are updated, then advance
        search_text, case_sensitive, regexp = self._last_search
        self.clear_find()
        if search_text:
            self.find_text(search_text, forward=True, move_cursor=True, case_sensitive=case_sensitive, regexp=regexp)

        return True

    def replace_all(self, replace_text: str) -> int:
        """
        Replace all current matches with replace_text in a single undoable operation.

        Args:
            replace_text: Text to substitute for each match.

        Returns:
            Number of replacements made.
        """
        if not self._matches:
            return 0

        search_text, case_sensitive, regexp = self._last_search
        count = len(self._matches)

        original_text = self.toPlainText()
        cursor = QTextCursor(self.document())
        cursor.beginEditBlock()
        try:
            # Iterate in reverse so earlier positions stay valid after each replacement
            for start, end in reversed(self._matches):
                actual_replacement = replace_text
                if regexp and search_text:
                    flags = QRegularExpression.PatternOption(0)
                    if not case_sensitive:
                        flags |= QRegularExpression.PatternOption.CaseInsensitiveOption

                    pattern = QRegularExpression(search_text, flags)
                    if pattern.isValid():
                        match_text = original_text[start:end]
                        re_match = pattern.match(match_text)
                        if re_match.hasMatch():
                            actual_replacement = replace_text
                            for i in range(1, re_match.lastCapturedIndex() + 1):
                                actual_replacement = actual_replacement.replace(
                                    f"\\{i}", re_match.captured(i)
                                )

                replace_cursor = QTextCursor(self.document())
                replace_cursor.setPosition(start)
                replace_cursor.setPosition(end, QTextCursor.MoveMode.KeepAnchor)
                replace_cursor.insertText(actual_replacement)

        finally:
            cursor.endEditBlock()

        self._set_modified(True)
        self.clear_find()
        return count

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

    def get_cursor_info(self) -> dict[str, Any]:
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

        info: dict[str, Any] = {
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

    def get_editor_info(self) -> dict[str, Any]:
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
            'language': ProgrammingLanguageUtils.get_display_name(self._syntax),
            'language_id': self._syntax.name,
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
        self._start_smooth_scroll_to_cursor(cursor)

    def find_all_occurrences(self, search_text: str, case_sensitive: bool = False, regexp: bool = False) -> list[dict[str, Any]]:
        """
        Find all occurrences of text in the document.

        Args:
            search_text: Text to search for
            case_sensitive: Whether search should be case-sensitive
            regexp: If True, treat search_text as a regular expression.

        Returns:
            List of dictionaries with match information:
            - line: Line number (1-indexed)
            - column: Column number (1-indexed)
            - match_text: The matched text
            - context: Line of text containing the match

        Raises:
            ValueError: If regexp is True and search_text is not a valid regular expression.
        """
        if not search_text:
            return []

        document = self.document()
        matches: list[dict[str, Any]] = []

        find_flags = QTextDocument.FindFlag(0)
        if case_sensitive:
            find_flags |= QTextDocument.FindFlag.FindCaseSensitively

        if regexp:
            pattern_flags = QRegularExpression.PatternOption(0)
            if not case_sensitive:
                pattern_flags |= QRegularExpression.PatternOption.CaseInsensitiveOption

            pattern = QRegularExpression(search_text, pattern_flags)
            if not pattern.isValid():
                raise ValueError(f"Invalid regular expression: {pattern.errorString()}")

            cursor = QTextCursor(document)
            while True:
                cursor = document.find(pattern, cursor, find_flags)
                if cursor.isNull():
                    break

                line = cursor.blockNumber() + 1
                column = cursor.columnNumber() + 1
                match_text = cursor.selectedText()
                block = cursor.block()
                matches.append({
                    'line': line,
                    'column': column,
                    'match_text': match_text,
                    'context': block.text()
                })

        else:
            cursor = QTextCursor(document)
            while True:
                cursor = document.find(search_text, cursor, find_flags)
                if cursor.isNull():
                    break

                line = cursor.blockNumber() + 1
                column = cursor.columnNumber() + 1
                match_text = cursor.selectedText()
                block = cursor.block()
                matches.append({
                    'line': line,
                    'column': column,
                    'match_text': match_text,
                    'context': block.text()
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

    def apply_unified_diff(self, diff_text: str) -> dict[str, Any]:
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
            self._start_smooth_scroll_to_cursor(cursor)

            self._set_modified(True)

        # Convert DiffApplicationResult to dict format
        return {
            'success': result.success,
            'message': result.message,
            'hunks_applied': result.hunks_applied,
            'error_details': result.error_details
        }

    def contextMenuEvent(self, event: QContextMenuEvent) -> None:
        """Show a styled context menu replacing the built-in Qt editor menu."""
        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        undo_action = menu.addAction(strings.undo)
        undo_action.setEnabled(self.can_undo())
        undo_action.triggered.connect(self.undo)

        redo_action = menu.addAction(strings.redo)
        redo_action.setEnabled(self.can_redo())
        redo_action.triggered.connect(self.redo)

        menu.addSeparator()

        cut_action = menu.addAction(strings.cut)
        cut_action.setEnabled(self.can_cut())
        cut_action.triggered.connect(self.cut)

        copy_action = menu.addAction(strings.copy)
        copy_action.setEnabled(self.can_copy())
        copy_action.triggered.connect(self.copy)

        paste_action = menu.addAction(strings.paste)
        paste_action.setEnabled(self.can_paste())
        paste_action.triggered.connect(self.paste)

        delete_action = menu.addAction(strings.delete)
        delete_action.setEnabled(self.textCursor().hasSelection())
        delete_action.triggered.connect(self._delete_selected_text)

        menu.addSeparator()

        select_all_action = menu.addAction(strings.select_all)
        select_all_action.triggered.connect(self.selectAll)

        if self._path:
            menu.addSeparator()

            preview_action = menu.addAction(strings.open_in_preview)
            preview_action.triggered.connect(self._open_in_preview)

            if MindspaceVCSPoller().has_repo():
                diff_action = menu.addAction(strings.open_in_diff)
                diff_action.setEnabled(MindspaceVCSPoller().has_vcs_changes(self._path))
                diff_action.triggered.connect(self._open_in_diff)

        menu.exec_(event.globalPos())

    def _delete_selected_text(self) -> None:
        """Delete the currently selected text."""
        self.textCursor().removeSelectedText()

    def _open_in_preview(self) -> None:
        """Open the current file in a preview tab."""
        mindspace_manager = MindspaceManager()
        if not mindspace_manager.has_mindspace():
            return

        contexts = mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(self._path, "preview")
        if existing:
            contexts.focus(existing.context_id)

        else:
            contexts.open(
                context_type="preview",
                path=self._path,
                title=os.path.basename(self._path),
            )

    def _open_in_diff(self) -> None:
        """Open the current file in a diff tab."""
        mindspace_manager = MindspaceManager()
        if not mindspace_manager.has_mindspace():
            return

        contexts = mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(self._path, "diff")
        if existing:
            contexts.focus(existing.context_id)

        else:
            contexts.open(
                context_type="diff",
                path=self._path,
                title=os.path.basename(self._path),
            )
