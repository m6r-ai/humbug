import logging
import os
import time
import uuid
from typing import Dict, Optional

from PySide6.QtWidgets import (
    QVBoxLayout, QFileDialog
)
from PySide6.QtCore import QTimer
from PySide6.QtGui import QTextCursor

from humbug.gui.editor_highlighter import EditorHighlighter
from humbug.gui.color_role import ColorRole
from humbug.gui.editor_text_edit import EditorTextEdit
from humbug.gui.message_box import MessageBox, MessageBoxType, MessageBoxButton
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab_base import TabBase
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.workspace.workspace_manager import WorkspaceManager


# Map file extensions to programming languages
LANGUAGE_MAP: Dict[str, ProgrammingLanguage] = {
    '.c': ProgrammingLanguage.C,
    '.cc': ProgrammingLanguage.CPP,
    '.cpp': ProgrammingLanguage.CPP,
    '.css': ProgrammingLanguage.CSS,
    '.cxx': ProgrammingLanguage.CPP,
    '.h': ProgrammingLanguage.C,
    '.hh': ProgrammingLanguage.CPP,
    '.hpp': ProgrammingLanguage.CPP,
    '.html': ProgrammingLanguage.HTML,
    '.htm': ProgrammingLanguage.HTML,
    '.hxx': ProgrammingLanguage.CPP,
    '.js': ProgrammingLanguage.JAVASCRIPT,
    '.jsx': ProgrammingLanguage.JAVASCRIPT,
    '.m6r': ProgrammingLanguage.METAPHOR,
    '.md': ProgrammingLanguage.TEXT,
    '.py': ProgrammingLanguage.PYTHON,
    '.pyw': ProgrammingLanguage.PYTHON,
    '.pyi': ProgrammingLanguage.PYTHON,
    '.ts': ProgrammingLanguage.TYPESCRIPT,
    '.tsx': ProgrammingLanguage.TYPESCRIPT,
    '.txt': ProgrammingLanguage.TEXT,
}


class EditorTab(TabBase):
    """Tab for editing text files."""

    def __init__(self, tab_id: str, parent=None):
        """
        Initialize editor tab.

        Args:
            tab_id: Unique identifier for this tab
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)

        self._untitled_number: Optional[int] = None
        self._style_manager = StyleManager()
        self._last_save_content = ""
        self._auto_backup_timer = QTimer(self)
        self._auto_backup_timer.timeout.connect(self._auto_backup)
        self._current_language = ProgrammingLanguage.TEXT
        self._logger = logging.getLogger("EditorTab")

        # Set up layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create editor
        self._editor = EditorTextEdit()
        self._editor.textChanged.connect(self._handle_text_changed)
        self._editor.cursorPositionChanged.connect(self.update_status)
        layout.addWidget(self._editor)

        # Set up syntax highlighter
        self._highlighter = EditorHighlighter(self._editor.document())

        # Connect to style changes
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed(self._style_manager.zoom_factor)

        self.update_status()

        # Update auto-backup based on current workspace settings
        workspace_manager = WorkspaceManager()
        if workspace_manager.has_workspace:
            settings = workspace_manager.settings
            self.update_auto_backup_settings(settings.auto_backup, settings.auto_backup_interval)

        # Connect to workspace settings changes
        workspace_manager.settings_changed.connect(self._handle_workspace_settings_changed)

    def _handle_workspace_settings_changed(self):
        """Handle workspace settings changes."""
        workspace_manager = WorkspaceManager()
        if workspace_manager.has_workspace:
            settings = workspace_manager.settings
            self.update_auto_backup_settings(settings.auto_backup, settings.auto_backup_interval)

    def update_auto_backup_settings(self, enabled: bool, interval: int) -> None:
        """Update auto-backup settings."""
        if enabled:
            self._auto_backup_timer.setInterval(interval * 1000)  # Convert to milliseconds
            if self._is_modified:
                # If we have unsaved changes, start backup immediately
                self._auto_backup_timer.start()
            return

        self._auto_backup_timer.stop()
        # Clean up any existing backups since auto-backup is disabled
        self._cleanup_backup_files()

    def get_state(self) -> TabState:
        """Get serializable state for workspace persistence."""
        return TabState(
            type=TabType.EDITOR,
            path=self._path if self._path else f"untitled-{self._untitled_number}",
            cursor_position=self.get_cursor_position(),
            metadata={
                "language": self._current_language.name
            }
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent=None) -> 'EditorTab':
        """Create and restore an editor tab from serialized state."""
        if state.type != TabType.EDITOR:
            raise ValueError(f"Invalid tab type for EditorTab: {state.type}")

        if not os.path.exists(state.path):
            raise FileNotFoundError(f"File not found: {state.path}")

        # Create new tab instance
        tab = cls(str(uuid.uuid4()), parent)

        # Set filename and load content
        if state.path.startswith("untitled-"):
            number = int(state.path.split("-")[1])
            tab.set_filename(None, number)
        else:
            tab.set_filename(state.path)

        # Restore cursor position if present
        if state.cursor_position:
            tab.set_cursor_position(state.cursor_position)

        # Restore language if specified
        if state.metadata and "language" in state.metadata:
            language = ProgrammingLanguage[state.metadata["language"]]
            tab._update_language(language)

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
        """Get current cursor position from editor.

        Returns:
            Dictionary with 'line' and 'column' keys
        """
        cursor = self._editor.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }

    def _handle_style_changed(self, zoom_factor: float = 1.0) -> None:
        """
        Handle style and zoom changes.

        Args:
            zoom_factor: New zoom scaling factor
        """
        # Update font size
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

        # Force a redraw of syntax highlighting
        self._highlighter.rehighlight()

    def _detect_language(self, filename: Optional[str]) -> ProgrammingLanguage:
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

    def _update_language(self, new_language: ProgrammingLanguage) -> None:
        """
        Update the syntax highlighting language.

        Args:
            new_language: The new programming language to use
        """
        if self._current_language != new_language:
            self._current_language = new_language
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
        new_language = self._detect_language(filename)
        self._update_language(new_language)

        if filename and os.path.exists(filename):
            try:
                with open(filename, 'r', encoding='utf-8') as f:
                    content = f.read()
                self._editor.setPlainText(content)
                self._last_save_content = content
                self._set_modified(False)
            except Exception as e:
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    "Error Opening File",
                    f"Could not open {filename}: {str(e)}"
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

        workspace_manager = WorkspaceManager()
        if workspace_manager.has_workspace and workspace_manager.settings.auto_backup:
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
            ProgrammingLanguage.JAVASCRIPT: "JavaScript",
            ProgrammingLanguage.TYPESCRIPT: "TypeScript",
            ProgrammingLanguage.PYTHON: "Python",
            ProgrammingLanguage.METAPHOR: "Metaphor",
        }
        file_type = language_names.get(self._current_language, "Text")

        message = StatusMessage(
            f"Line {line}, Column {column} | {encoding} | {line_ending} | {file_type}"
        )
        self.status_message.emit(message)

    def _auto_backup(self) -> None:
        """Handle auto-backup functionality."""
        if not self._is_modified:
            return

        # All backups should now go in workspace .humbug/backups
        workspace_manager = WorkspaceManager()
        if not workspace_manager.has_workspace:
            return  # No backups without a workspace

        backup_dir = workspace_manager.get_workspace_path(os.path.join(".humbug", "backups"))
        os.makedirs(backup_dir, exist_ok=True)

        if not self._path:
            # For untitled files, use timestamp-based backup in workspace
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
        workspace_manager = WorkspaceManager()
        if not workspace_manager.has_workspace:
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
            backup_dir = workspace_manager.get_workspace_path(os.path.join(".humbug", "backups"))
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

        result = MessageBox.show_message(
            self,
            MessageBoxType.QUESTION,
            "Save Changes?",
            f"Do you want to save changes to {self._path or f'Untitled-{self._untitled_number}'}?",
            [MessageBoxButton.SAVE, MessageBoxButton.DISCARD, MessageBoxButton.CANCEL]
        )

        if result == MessageBoxButton.SAVE:
            return self.save()

        if result == MessageBoxButton.DISCARD:
            # Delete any backup files when discarding changes
            self._cleanup_backup_files()
            return True

        return False

    def close(self) -> None:
        pass

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
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Error Saving File",
                f"Could not save {self._path}: {str(e)}"
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
        filename, _ = QFileDialog.getSaveFileName(
            self,
            "Save As",
            self._path or os.path.expanduser("~/")
        )
        if not filename:
            return False

        self._path = filename
        self._untitled_number = None
        self._update_title()

        new_language = self._detect_language(filename)
        self._update_language(new_language)

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
