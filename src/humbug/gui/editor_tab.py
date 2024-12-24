import os
import time
from typing import Optional, Set

from PySide6.QtWidgets import (
    QVBoxLayout, QLabel, QFileDialog, QMessageBox
)
from PySide6.QtCore import Qt, QTimer
from PySide6.QtGui import QTextCharFormat, QSyntaxHighlighter

from humbug.gui.tab_base import TabBase
from humbug.gui.dynamic_text_edit import DynamicTextEdit
from humbug.gui.markdown_highlighter import MarkdownHighlighter
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class EditorTab(TabBase):
    """Tab for editing text files."""

    def __init__(self, tab_id: str, parent=None):
        """Initialize editor tab.
        
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

        # Set up layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create editor
        self._editor = DynamicTextEdit()
        self._editor.textChanged.connect(self._handle_text_changed)
        self._editor.cursorPositionChanged.connect(self._update_status)
        layout.addWidget(self._editor)

        # Add status bar
        self._status_bar = QLabel()
        self._status_bar.setStyleSheet(f"""
            QLabel {{
                background-color: {self._style_manager.get_color_str(ColorRole.STATUS_BAR)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 2px;
            }}
        """)
        layout.addWidget(self._status_bar)

        # Set up syntax highlighter
        self._highlighter = MarkdownHighlighter(self._editor.document())

        self._update_status()

    def set_filename(self, filename: Optional[str], untitled_number: Optional[int] = None) -> None:
        """Set the file being edited.
        
        Args:
            filename: Path to file or None for new file
            untitled_number: Number to use for untitled file
        """
        self._filename = filename
        self._untitled_number = untitled_number
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
        file_type = "Text"
        if self._filename:
            ext = os.path.splitext(self._filename)[1].lower()
            if ext in {'.py', '.pyw'}:
                file_type = "Python"
            elif ext in {'.js', '.jsx', '.ts', '.tsx'}:
                file_type = "JavaScript"
            elif ext == '.html':
                file_type = "HTML"
            elif ext == '.css':
                file_type = "CSS"
            elif ext in {'.c', '.cpp', '.h', '.hpp'}:
                file_type = "C/C++"

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

    def save(self) -> bool:
        """Save the current file.
        
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

    def save_as(self) -> bool:
        """Show save as dialog and save file.
        
        Returns:
            bool: True if save was successful
        """
        filename, _ = QFileDialog.getSaveFileName(
            self,
            "Save As",
            self._filename or os.path.expanduser("~/")
        )
        if filename:
            self._filename = filename
            self._untitled_number = None
            self._update_title()
            return self.save()
        return False

    # Implement abstract methods for editing operations
    def can_undo(self) -> bool:
        return self._editor.document().isUndoAvailable()

    def can_redo(self) -> bool:
        return self._editor.document().isRedoAvailable()

    def undo(self) -> None:
        self._editor.undo()

    def redo(self) -> None:
        self._editor.redo()

    def can_cut(self) -> bool:
        return self._editor.textCursor().hasSelection()

    def can_copy(self) -> bool:
        return self._editor.textCursor().hasSelection()

    def can_paste(self) -> bool:
        return True

    def cut(self) -> None:
        self._editor.cut()

    def copy(self) -> None:
        self._editor.copy()

    def paste(self) -> None:
        self._editor.paste()
