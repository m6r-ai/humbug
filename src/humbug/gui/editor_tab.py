import os
import time
from typing import Dict, Optional

from PySide6.QtWidgets import (
    QVBoxLayout, QLabel, QFileDialog, QMessageBox,
)
from PySide6.QtCore import QTimer

from humbug.gui.tab_base import TabBase
from humbug.gui.editor_highlighter import EditorHighlighter
from humbug.gui.color_role import ColorRole
from humbug.gui.editor_text_edit import EditorTextEdit
from humbug.gui.style_manager import StyleManager
from humbug.syntax.programming_language import ProgrammingLanguage


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

        self._filename: Optional[str] = None
        self._untitled_number: Optional[int] = None
        self._style_manager = StyleManager()
        self._last_save_content = ""
        self._auto_save_timer = QTimer(self)
        self._auto_save_timer.setInterval(5 * 60 * 1000)  # 5 minutes
        self._auto_save_timer.timeout.connect(self._auto_save)
        self._current_language = ProgrammingLanguage.TEXT

        # Set up layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create editor
        self._editor = EditorTextEdit()
        self._editor.textChanged.connect(self._handle_text_changed)
        self._editor.cursorPositionChanged.connect(self._update_status)
        layout.addWidget(self._editor)

        # Add status bar
        self._status_bar = QLabel()
        layout.addWidget(self._status_bar)

        # Set up syntax highlighter
        self._highlighter = EditorHighlighter(self._editor.document())

        # Connect to style changes
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed(self._style_manager.zoom_factor)

        self._update_status()

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
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_ACTIVE)};
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
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical,
            QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal {{
                height: 0px;
                width: 0px;
            }}
        """)

        # Update status bar styling
        self._status_bar.setStyleSheet(f"""
            QLabel {{
                background-color: {self._style_manager.get_color_str(ColorRole.STATUS_BAR)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: {2 * zoom_factor}px;
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
            self._update_status()

    def set_filename(self, filename: Optional[str], untitled_number: Optional[int] = None) -> None:
        """
        Set the file being edited.

        Args:
            filename: Path to file or None for new file
            untitled_number: Number to use for untitled file
        """
        self._filename = filename
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
                QMessageBox.critical(
                    self,
                    "Error Opening File",
                    f"Could not open {filename}: {str(e)}"
                )
        self._update_title()

    def _update_title(self) -> None:
        """Update the tab title based on filename and modified state."""
        if self._filename:
            title = os.path.basename(self._filename)
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

        if is_modified and not self._auto_save_timer.isActive():
            self._auto_save_timer.start()
        elif not is_modified:
            self._auto_save_timer.stop()

    def _update_status(self) -> None:
        """Update the status bar with current cursor position."""
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

        self._status_bar.setText(
            f"Line {line}, Column {column} | {encoding} | {line_ending} | {file_type}"
        )

    def _auto_save(self) -> None:
        """Handle auto-save functionality."""
        if not self._is_modified:
            return

        if not self._filename:
            backup_dir = os.path.expanduser("~/.humbug/backups")
            os.makedirs(backup_dir, exist_ok=True)
            backup_file = os.path.join(
                backup_dir,
                f"backup-{self._untitled_number}-{int(time.time())}.txt"
            )
        else:
            backup_file = f"{self._filename}.backup"

        try:
            with open(backup_file, 'w', encoding='utf-8') as f:
                f.write(self._editor.toPlainText())
        except Exception as e:
            self._logger.error("Failed to create backup: %s", str(e))

    def can_close(self) -> bool:
        """Check if the file can be closed."""
        if not self._is_modified:
            return True

        reply = QMessageBox.question(
            self,
            "Save Changes?",
            f"Do you want to save changes to {self._filename or f'Untitled-{self._untitled_number}'}?",
            QMessageBox.Save | QMessageBox.Discard | QMessageBox.Cancel
        )

        if reply == QMessageBox.Save:
            return self.save()
        return reply == QMessageBox.Discard

    def can_save(self) -> bool:
        return self._is_modified

    def save(self) -> bool:
        """
        Save the current file.

        Returns:
            bool: True if save was successful
        """
        if not self._filename:
            return self.save_as()

        try:
            content = self._editor.toPlainText()
            with open(self._filename, 'w', encoding='utf-8') as f:
                f.write(content)

            self._last_save_content = content
            self._set_modified(False)
            return True
        except Exception as e:
            QMessageBox.critical(
                self,
                "Error Saving File",
                f"Could not save {self._filename}: {str(e)}"
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
            self._filename or os.path.expanduser("~/")
        )
        if not filename:
            return False

        self._filename = filename
        self._untitled_number = None
        self._update_title()

        new_language = self._detect_language(filename)
        self._update_language(new_language)

        return self.save()

    def can_undo(self) -> bool:
        return self._editor.document().isUndoAvailable()

    def undo(self) -> None:
        self._editor.undo()

    def can_redo(self) -> bool:
        return self._editor.document().isRedoAvailable()

    def redo(self) -> None:
        self._editor.redo()

    def can_cut(self) -> bool:
        return self._editor.textCursor().hasSelection()

    def cut(self) -> None:
        self._editor.cut()

    def can_copy(self) -> bool:
        return self._editor.textCursor().hasSelection()

    def copy(self) -> None:
        self._editor.copy()

    def can_paste(self) -> bool:
        return True

    def paste(self) -> None:
        self._editor.paste()

    def can_submit(self) -> bool:
        return False
