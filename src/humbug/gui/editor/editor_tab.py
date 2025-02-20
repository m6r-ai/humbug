import logging
import os
import time
from typing import Dict, Optional

from PySide6.QtWidgets import (
    QVBoxLayout, QFileDialog
)
from PySide6.QtCore import QTimer
from PySide6.QtGui import QTextCursor

from humbug.gui.color_role import ColorRole
from humbug.gui.editor.editor_highlighter import EditorHighlighter
from humbug.gui.editor.editor_find import EditorFind
from humbug.gui.editor.editor_text_edit import EditorTextEdit
from humbug.gui.find_widget import FindWidget
from humbug.gui.message_box import MessageBox, MessageBoxType, MessageBoxButton
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.language.language_manager import LanguageManager
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.mindspace.mindspace_manager import MindspaceManager


# Map file extensions to programming languages
LANGUAGE_MAP: Dict[str, ProgrammingLanguage] = {
    '.c': ProgrammingLanguage.C,
    '.cc': ProgrammingLanguage.CPP,
    '.conv': ProgrammingLanguage.JSON,
    '.cpp': ProgrammingLanguage.CPP,
    '.css': ProgrammingLanguage.CSS,
    '.cxx': ProgrammingLanguage.CPP,
    '.go': ProgrammingLanguage.GO,
    '.h': ProgrammingLanguage.C,
    '.hh': ProgrammingLanguage.CPP,
    '.hpp': ProgrammingLanguage.CPP,
    '.html': ProgrammingLanguage.HTML,
    '.htm': ProgrammingLanguage.HTML,
    '.hxx': ProgrammingLanguage.CPP,
    '.java': ProgrammingLanguage.JAVA,
    '.js': ProgrammingLanguage.JAVASCRIPT,
    '.json': ProgrammingLanguage.JSON,
    '.jsx': ProgrammingLanguage.JAVASCRIPT,
    '.kt': ProgrammingLanguage.KOTLIN,
    '.kts': ProgrammingLanguage.KOTLIN,
    '.m6r': ProgrammingLanguage.METAPHOR,
    '.md': ProgrammingLanguage.TEXT,
    '.move': ProgrammingLanguage.MOVE,
    '.py': ProgrammingLanguage.PYTHON,
    '.pyw': ProgrammingLanguage.PYTHON,
    '.pyi': ProgrammingLanguage.PYTHON,
    ".rs": ProgrammingLanguage.RUST,
    '.scm': ProgrammingLanguage.SCHEME,
    ".swift": ProgrammingLanguage.SWIFT,
    '.ts': ProgrammingLanguage.TYPESCRIPT,
    '.tsx': ProgrammingLanguage.TYPESCRIPT,
    '.txt': ProgrammingLanguage.TEXT,
}


class EditorTab(TabBase):
    """Tab for editing text files."""

    def __init__(self, tab_id: str, parent=None):
        """Initialize editor tab.

        Args:
            tab_id: Unique identifier for this tab
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)

        self._untitled_number: Optional[int] = None
        self._style_manager = StyleManager()
        self._init_colour_mode = self._style_manager.color_mode
        self._last_save_content = ""
        self._auto_backup_timer = QTimer(self)
        self._auto_backup_timer.timeout.connect(self._auto_backup)
        self._current_programming_language = ProgrammingLanguage.TEXT
        self._logger = logging.getLogger("EditorTab")

        self._mindspace_manager = MindspaceManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

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
        self._editor = EditorTextEdit()
        self._editor.textChanged.connect(self._handle_text_changed)
        self._editor.cursorPositionChanged.connect(self.update_status)
        layout.addWidget(self._editor)

        # Create find handler
        self._find_handler = EditorFind(self._editor)

        self._install_activation_tracking(self._editor)

        # Set up syntax highlighter
        self._highlighter = EditorHighlighter(self._editor.document())

        # Connect to style changes
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        self.update_status()

        # Update auto-backup based on current mindspace settings
        if self._mindspace_manager.has_mindspace:
            settings = self._mindspace_manager.settings
            self.update_auto_backup_settings(settings.auto_backup, settings.auto_backup_interval)

        # Connect to mindspace settings changes
        self._mindspace_manager.settings_changed.connect(self._handle_mindspace_settings_changed)

    def _handle_language_changed(self) -> None:
        """Update language-specific elements."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._find_handler.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar with translated terms
        self.update_status()

    def _handle_mindspace_settings_changed(self):
        """Handle mindspace settings changes."""
        if self._mindspace_manager.has_mindspace:
            settings = self._mindspace_manager.settings
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
            metadata_state["content"] = self._editor.toPlainText()

        return TabState(
            type=TabType.EDITOR,
            tab_id=self.tab_id,
            path=self._path if self._path else f"untitled-{self._untitled_number}",
            cursor_position=self.get_cursor_position(),
            horizontal_scroll=self._editor.horizontalScrollBar().value(),
            vertical_scroll=self._editor.verticalScrollBar().value(),
            metadata=metadata_state
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent=None) -> 'EditorTab':
        """Create and restore an editor tab from serialized state."""
        if state.type != TabType.EDITOR:
            raise ValueError(f"Invalid tab type for EditorTab: {state.type}")

        # Create new tab instance
        tab = cls(state.tab_id, parent)

        # Set filename and load content
        if state.path.startswith("untitled-"):
            number = int(state.path.split("-")[1])
            tab.set_filename(None, number)
        else:
            tab.set_filename(state.path)

        if state.metadata:
            # Restore language if specified
            if "language" in state.metadata:
                language = ProgrammingLanguage[state.metadata["language"]]
                tab._update_programming_language(language)

            # Restore content if specified
            if "content" in state.metadata:
                tab._editor.setPlainText(state.metadata["content"])

        # Restore cursor position if present
        if state.cursor_position:
            tab.set_cursor_position(state.cursor_position)

        # Restore scroll poisitions if present
        if state.horizontal_scroll:
            tab._editor.horizontalScrollBar().setValue(state.horizontal_scroll)

        if state.vertical_scroll:
            tab._editor.verticalScrollBar().setValue(state.vertical_scroll)

        return tab

    def set_cursor_position(self, position: Dict[str, int]) -> None:
        """Set cursor position in editor.

        Args:
            position: Dictionary with 'line' and 'column' keys
        """
        if not position:
            return

        cursor = self._editor.textCursor()
        cursor.movePosition(QTextCursor.Start)

        # Move cursor to specified position
        for _ in range(position.get("line", 0)):
            cursor.movePosition(QTextCursor.NextBlock)

        cursor.movePosition(
            QTextCursor.Right,
            QTextCursor.MoveAnchor,
            position.get("column", 0)
        )

        self._editor.setTextCursor(cursor)
        self._editor.ensureCursorVisible()

    def get_cursor_position(self) -> Dict[str, int]:
        """
        Get current cursor position from editor.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self._editor.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }

    def _handle_style_changed(self) -> None:
        """
        Handle style and zoom changes.
        """
        # Update font size
        zoom_factor = self._style_manager.zoom_factor
        font = self._editor.font()
        base_size = self._style_manager.base_font_size
        font.setPointSizeF(base_size * zoom_factor)
        self._editor.setFont(font)

        # Update tab stops - scale with zoom
        space_width = self._style_manager.get_space_width()
        self._editor.setTabStopDistance(space_width * 8)

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
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
        self._editor.update_line_number_area_width()

        # If we changed colour mode then re-highlight
        if self._style_manager.color_mode != self._init_colour_mode:
            self._init_colour_mode = self._style_manager.color_mode
            self._highlighter.rehighlight()

    def _detect_programming_language(self, filename: Optional[str]) -> ProgrammingLanguage:
        """
        Detect the programming language based on file extension.

        Args:
            filename: The filename to analyze

        Returns:
            The detected programming language
        """
        if not filename:
            return ProgrammingLanguage.TEXT

        ext = os.path.splitext(filename)[1].lower()
        return LANGUAGE_MAP.get(ext, ProgrammingLanguage.TEXT)

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

    @property
    def filename(self) -> str:
        """Get the name of the file being edited."""
        return self._path

    def set_filename(self, filename: Optional[str], untitled_number: Optional[int] = None) -> None:
        """
        Set the file being edited.

        Args:
            filename: Path to file or None for new file
            untitled_number: Number to use for untitled file
        """
        self._path = filename
        self._untitled_number = untitled_number

        # Update syntax highlighting based on file extension
        new_language = self._detect_programming_language(filename)
        self._update_programming_language(new_language)

        if filename and os.path.exists(filename):
            try:
                with open(filename, 'r', encoding='utf-8') as f:
                    content = f.read()
                self._editor.setPlainText(content)
                self._last_save_content = content
                self._set_modified(False)
            except Exception as e:
                strings = self._language_manager.strings
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.error_opening_file_title,
                    strings.could_not_open.format(filename, str(e))
                )
        self._update_title()

    def _update_title(self) -> None:
        """Update the tab title based on filename and modified state."""
        if self._path:
            title = os.path.basename(self._path)
        else:
            title = f"Untitled-{self._untitled_number}"

        if self._is_modified:
            title += "*"

        self.title_changed.emit(self._tab_id, title)

    def _handle_text_changed(self) -> None:
        """Handle changes to editor content."""
        current_content = self._editor.toPlainText()
        is_modified = current_content != self._last_save_content
        self._set_modified(is_modified)

        if self._mindspace_manager.has_mindspace and self._mindspace_manager.settings.auto_backup:
            if is_modified and not self._auto_backup_timer.isActive():
                self._auto_backup_timer.start()
            elif not is_modified:
                self._auto_backup_timer.stop()

    def update_status(self) -> None:
        """Update status bar with current cursor position."""
        cursor = self._editor.textCursor()
        line = cursor.blockNumber() + 1
        column = cursor.columnNumber() + 1

        # Get file info
        encoding = "UTF-8"
        line_ending = "LF"  # We could detect this from file content

        # Get language name from enum
        language_names = {
            ProgrammingLanguage.TEXT: "Text",
            ProgrammingLanguage.C: "C",
            ProgrammingLanguage.CPP: "C++",
            ProgrammingLanguage.CSS: "CSS",
            ProgrammingLanguage.HTML: "HTML",
            ProgrammingLanguage.JAVA: "Java",
            ProgrammingLanguage.JAVASCRIPT: "JavaScript",
            ProgrammingLanguage.JSON: "JSON",
            ProgrammingLanguage.KOTLIN: "Kotlin",
            ProgrammingLanguage.METAPHOR: "Metaphor",
            ProgrammingLanguage.MOVE: "Move",
            ProgrammingLanguage.PYTHON: "Python",
            ProgrammingLanguage.SCHEME: "Scheme",
            ProgrammingLanguage.TYPESCRIPT: "TypeScript"
        }
        file_type = language_names.get(self._current_programming_language, "Text")

        strings = self._language_manager.strings
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
        if not self._mindspace_manager.has_mindspace:
            return  # No backups without a mindspace

        backup_dir = self._mindspace_manager.get_mindspace_path(os.path.join(".humbug", "backups"))
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
                f.write(self._editor.toPlainText())
        except Exception as e:
            self._logger.error("Failed to create backup file '%s': %s", backup_file, str(e))

    def _cleanup_backup_files(self) -> None:
        """Clean up any backup files for this editor."""
        if not self._mindspace_manager.has_mindspace:
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
            backup_dir = self._mindspace_manager.get_mindspace_path(os.path.join(".humbug", "backups"))
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

    def can_close(self) -> bool:
        """Check if the file can be closed."""
        if not self._is_modified:
            return True

        strings = self._language_manager.strings
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
            return True

        return False

    def close(self) -> None:
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
            content = self._editor.toPlainText()
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
            strings = self._language_manager.strings
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
        strings = self._language_manager.strings
        export_dialog = QFileDialog()
        export_dialog.setWindowTitle(strings.file_dialog_save_file)
        export_dialog.setDirectory(self._path or self._mindspace_manager.file_dialog_directory)
        export_dialog.setAcceptMode(QFileDialog.AcceptSave)
        if export_dialog.exec_() != QFileDialog.Accepted:
            return False

        filename = export_dialog.selectedFiles()[0]

        # This is an ugly hack.  On one laptop using ".m6r" as a filename results in Qt
        # adding a ".m".  Need to root cause this.
        if filename.endswith(".m6r.m"):
            filename = filename[:-2]

        self._mindspace_manager.update_file_dialog_directory(filename)
        self._path = filename
        self._untitled_number = None
        self._update_title()

        new_language = self._detect_programming_language(filename)
        self._update_programming_language(new_language)

        return self.save()

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return self._editor.document().isUndoAvailable()

    def undo(self) -> None:
        """Undo the last edit operation."""
        self._editor.undo()

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return self._editor.document().isRedoAvailable()

    def redo(self) -> None:
        """Redo the last undone edit operation."""
        self._editor.redo()

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return self._editor.textCursor().hasSelection()

    def cut(self) -> None:
        """Cut selected text to clipboard."""
        self._editor.cut()

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._editor.textCursor().hasSelection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._editor.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return True

    def paste(self) -> None:
        """Paste text from clipboard."""
        self._editor.paste()

    def can_submit(self) -> bool:
        return False

    def show_find(self):
        """Show the find widget."""
        cursor = self._editor.textCursor()
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

    def _close_find(self):
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._find_handler.clear()

    def _find_next(self, forward: bool = True):
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        self._find_handler.find_text(text, forward)
        current, total = self._find_handler.get_match_status()
        self._find_widget.set_match_status(current, total)
