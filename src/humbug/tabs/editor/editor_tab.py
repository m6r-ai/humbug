import logging
import os
import time
from typing import Dict, cast

from PySide6.QtWidgets import (
    QVBoxLayout, QFileDialog, QWidget
)
from PySide6.QtCore import QTimer
from PySide6.QtGui import QTextCursor

from syntax import ProgrammingLanguage, ProgrammingLanguageUtils

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType, MessageBoxButton
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.status_message import StatusMessage
from humbug.style_manager import StyleManager
from humbug.tabs.editor.editor_highlighter import EditorHighlighter
from humbug.tabs.editor.editor_widget import EditorWidget
from humbug.tabs.find_widget import FindWidget
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType


class EditorTab(TabBase):
    """Tab for editing text files."""

    def __init__(self, tab_id: str, path: str, untitled_number: int | None, parent: QWidget | None = None) -> None:
        """Initialize editor tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)

        self._path = path
        self._untitled_number = untitled_number
        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode()
        self._last_save_content = ""
        self._auto_backup_timer = QTimer(self)
        self._auto_backup_timer.timeout.connect(self._auto_backup)
        self._current_programming_language = ProgrammingLanguage.TEXT
        self._logger = logging.getLogger("EditorTab")

        self._mindspace_manager = MindspaceManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Set up layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Add find widget at top (initially hidden)
        self._find_widget = FindWidget()
        self._find_widget.hide()
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        layout.addWidget(self._find_widget)

        # Create editor
        self._editor_widget = EditorWidget()
        self._editor_widget.textChanged.connect(self._on_text_changed)
        self._editor_widget.cursorPositionChanged.connect(self.update_status)
        layout.addWidget(self._editor_widget)

        # Install activation tracking
        self._editor_widget.activated.connect(self.activated)

        if path and os.path.exists(path):
            with open(path, 'r', encoding='utf-8') as f:
                content = f.read()

            self._editor_widget.setPlainText(content)
            self._last_save_content = content

        # Set up syntax highlighter
        self._highlighter = EditorHighlighter(self._editor_widget.document())

        new_language = ProgrammingLanguageUtils.from_file_extension(path)
        self._update_programming_language(new_language)

        # Connect to style changes
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        self.update_status()

        # Update auto-backup based on current mindspace settings
        if self._mindspace_manager.has_mindspace():
            settings = self._mindspace_manager.settings()
            if settings is not None:
                self.update_auto_backup_settings(settings.auto_backup, settings.auto_backup_interval)

        # Connect to mindspace settings changes
        self._mindspace_manager.settings_changed.connect(self._handle_mindspace_settings_changed)

    def activate(self) -> None:
        """Activate the tab."""
        self._editor_widget.setFocus()

    def _on_language_changed(self) -> None:
        """Update language-specific elements."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._editor_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar with translated terms
        self.update_status()

    def _handle_mindspace_settings_changed(self) -> None:
        """Handle mindspace settings changes."""
        if self._mindspace_manager.has_mindspace():
            settings = cast(MindspaceSettings, self._mindspace_manager.settings())
            self.update_auto_backup_settings(settings.auto_backup, settings.auto_backup_interval)

    def update_auto_backup_settings(self, enabled: bool, interval: int) -> None:
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

    def get_state(self, temp_state: bool=False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata_state = {
            "language": self._current_programming_language.name
        }

        if temp_state:
            metadata_state["content"] = self._editor_widget.toPlainText()

        return TabState(
            type=TabType.EDITOR,
            tab_id=self._tab_id,
            path=self._path if self._path else f"untitled-{self._untitled_number}",
            cursor_position=self._get_cursor_position(),
            horizontal_scroll=self._editor_widget.horizontalScrollBar().value(),
            vertical_scroll=self._editor_widget.verticalScrollBar().value(),
            metadata=metadata_state
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'EditorTab':
        """Create and restore an editor tab from serialized state."""
        # Set filename and load content
        number: int | None = None
        if state.path.startswith("untitled-"):
            number = int(state.path.split("-")[1])

        # Create new tab instance
        tab = cls(state.tab_id, "" if number else state.path, number, parent)

        if state.metadata:
            # Restore language if specified
            if "language" in state.metadata:
                language = ProgrammingLanguage[state.metadata["language"]]
                tab._update_programming_language(language)

            # Restore content if specified
            if "content" in state.metadata:
                tab._editor_widget.setPlainText(state.metadata["content"])

        # Restore cursor position if present
        if state.cursor_position:
            tab._set_cursor_position(state.cursor_position)

        # Restore scroll poisitions if present
        if state.horizontal_scroll:
            tab._editor_widget.horizontalScrollBar().setValue(state.horizontal_scroll)

        if state.vertical_scroll:
            tab._editor_widget.verticalScrollBar().setValue(state.vertical_scroll)

        return tab

    def _set_cursor_position(self, position: Dict[str, int]) -> None:
        """
        Set cursor position in editor.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        if not position:
            return

        cursor = self._editor_widget.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.Start)

        # Move cursor to specified position
        for _ in range(position.get("line", 0)):
            cursor.movePosition(QTextCursor.MoveOperation.NextBlock)

        cursor.movePosition(
            QTextCursor.MoveOperation.Right,
            QTextCursor.MoveMode.MoveAnchor,
            position.get("column", 0)
        )

        self._editor_widget.setTextCursor(cursor)
        self._editor_widget.ensureCursorVisible()

    def _get_cursor_position(self) -> Dict[str, int]:
        """
        Get current cursor position from editor.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self._editor_widget.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }

    def _on_style_changed(self) -> None:
        """
        Handle style and zoom changes.
        """
        # Update font size
        zoom_factor = self._style_manager.zoom_factor()
        font = self._editor_widget.font()
        base_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_size * zoom_factor)
        self._editor_widget.setFont(font)

        # Update tab stops - scale with zoom
        space_width = self._style_manager.get_space_width()
        self._editor_widget.setTabStopDistance(space_width * 8)

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
            QPlainTextEdit {{
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

        # Scale line number area
        self._editor_widget.update_line_number_area_width()

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode() != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode()
            self._highlighter.rehighlight()

    def _update_programming_language(self, new_language: ProgrammingLanguage) -> None:
        """
        Update the syntax highlighting language.

        Args:
            new_language: The new programming language to use
        """
        if self._current_programming_language != new_language:
            self._current_programming_language = new_language
            self._highlighter.set_language(new_language)
            self.update_status()

    def set_path(self, path: str) -> None:
        """
        Set the file being edited.

        Args:
            filename: Path to file or None for new file
        """
        self._path = path

        # Update syntax highlighting based on file extension
        new_language = ProgrammingLanguageUtils.from_file_extension(path)
        self._update_programming_language(new_language)

    def _on_text_changed(self) -> None:
        """Handle changes to editor content."""
        current_content = self._editor_widget.toPlainText()
        is_modified = current_content != self._last_save_content
        self._set_modified(is_modified)

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

    def update_status(self) -> None:
        """Update status bar with current cursor position."""
        cursor = self._editor_widget.textCursor()
        line = cursor.blockNumber() + 1
        column = cursor.columnNumber() + 1

        # Get file info
        encoding = "UTF-8"
        line_ending = "LF"  # We could detect this from file content

        # Get language name for display
        file_type = ProgrammingLanguageUtils.get_display_name(self._current_programming_language)

        strings = self._language_manager.strings()
        message = StatusMessage(
            strings.editor_status.format(
                line=line,
                column=column,
                encoding=encoding,
                line_ending=line_ending,
                type=file_type
            )
        )
        self.status_message.emit(message)

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
                f.write(self._editor_widget.toPlainText())

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

    def can_close_tab(self) -> bool:
        """Check if the file can be closed."""
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
            return self.save()

        if result == MessageBoxButton.DISCARD:
            self._set_modified(False)
            return True

        return False

    def close_tab(self) -> None:
        # Delete any backup files when we close
        if not self._auto_backup_timer.isActive():
            return

        self._cleanup_backup_files()

    def can_save(self) -> bool:
        return self._is_modified

    def save(self) -> bool:
        """
        Save the current file.

        Returns:
            bool: True if save was successful
        """
        if not self._path:
            return self.save_as()

        try:
            content = self._editor_widget.toPlainText()
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

    def can_save_as(self) -> bool:
        return True

    def save_as(self) -> bool:
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
        self._path = filename
        self._untitled_number = None

        new_language = ProgrammingLanguageUtils.from_file_extension(filename)
        self._update_programming_language(new_language)

        return self.save()

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return self._editor_widget.document().isUndoAvailable()

    def undo(self) -> None:
        """Undo the last edit operation."""
        self._editor_widget.undo()

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return self._editor_widget.document().isRedoAvailable()

    def redo(self) -> None:
        """Redo the last undone edit operation."""
        self._editor_widget.redo()

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._editor_widget.textCursor().hasSelection()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._editor_widget.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._editor_widget.textCursor().hasSelection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._editor_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return True

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._editor_widget.paste()

    def can_submit(self) -> bool:
        return False

    def submit(self) -> None:
        """Not implemented for editor tabs."""

    def show_find(self) -> None:
        """Show the find widget."""
        cursor = self._editor_widget.textCursor()
        if cursor.hasSelection():
            # Get the selected text
            text = cursor.selectedText()

            # Only use selection if it's on a single line
            if '\u2029' not in text:  # Qt uses this character for line breaks
                self._find_widget.set_search_text(text)
            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._editor_widget.clear_find()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        self._editor_widget.find_text(text, forward)
        current, total = self._editor_widget.get_match_status()
        self._find_widget.set_match_status(current, total)
