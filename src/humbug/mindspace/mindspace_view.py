"""Main mindspace view widget containing files and conversations views."""

import os

from PySide6.QtWidgets import QWidget, QVBoxLayout, QSplitter, QLabel
from PySide6.QtCore import Qt, Signal

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_conversations_view import MindspaceConversationsView
from humbug.mindspace.mindspace_files_view import MindspaceFilesView
from humbug.style_manager import StyleManager


class MindspaceView(QWidget):
    """Main mindspace view widget containing files and conversations sections."""

    # Forward all file-related signals from files view
    file_single_clicked = Signal(str)  # Emits path when any file is single-clicked
    file_double_clicked = Signal(str)  # Emits path when any file is double-clicked
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)
    file_moved = Signal(str, str)  # Emits (old_path, new_path)
    file_edited = Signal(str)  # Emits path when file is edited

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the mindspace view widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Create main layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create header container for mindspace name
        self._header_widget = QWidget()
        header_layout = QVBoxLayout(self._header_widget)
        header_layout.setContentsMargins(0, 0, 0, 0)
        header_layout.setSpacing(0)

        # Create mindspace label
        self._mindspace_label = QLabel()
        self._mindspace_label.setContentsMargins(0, 0, 0, 0)

        header_layout.addWidget(self._mindspace_label)
        layout.addWidget(self._header_widget)

        # Create splitter for mindpspace views
        self._splitter = QSplitter(Qt.Orientation.Vertical)
        layout.addWidget(self._splitter)

        self._conversations_view = MindspaceConversationsView()
        self._splitter.addWidget(self._conversations_view)

        self._files_view = MindspaceFilesView()
        self._splitter.addWidget(self._files_view)

        # Set equal proportions initially (50/50 split)
        self._splitter.setSizes([1, 1])

        # Set stretch factors - both sections can stretch
        self._splitter.setStretchFactor(0, 1)
        self._splitter.setStretchFactor(1, 1)

        # Connect file view signals to forward them
        self._files_view.file_single_clicked.connect(self.file_single_clicked.emit)
        self._files_view.file_double_clicked.connect(self.file_double_clicked.emit)
        self._files_view.file_deleted.connect(self.file_deleted.emit)
        self._files_view.file_renamed.connect(self.file_renamed.emit)
        self._files_view.file_moved.connect(self.file_moved.emit)
        self._files_view.file_edited.connect(self.file_edited.emit)

        # Set initial label text
        self._mindspace_label.setText(self._language_manager.strings().mindspace_label_none)

        self._on_language_changed()

    def reveal_and_select_file(self, file_path: str) -> None:
        """
        Forward file reveal and select to files view.

        Args:
            file_path: Absolute path to the file to reveal and select
        """
        self._files_view.reveal_and_select_file(file_path)

    def set_mindspace(self, path: str) -> None:
        """
        Set the mindspace root directory.

        Args:
            path: Path to the mindspace directory, or empty string to clear
        """
        # Update the mindspace label
        if not path:
            self._mindspace_label.setText(self._language_manager.strings().mindspace_label_none)

        else:
            self._mindspace_label.setText(os.path.basename(path))

        # Forward to files view
        self._files_view.set_mindspace(path)

    def _on_language_changed(self) -> None:
        """Update when the language changes."""
        # Update mindspace label if no mindspace is active
        current_text = self._mindspace_label.text()
        none_text = self._language_manager.strings().mindspace_label_none
        if current_text == none_text or not current_text:
            self._mindspace_label.setText(none_text)

        self.apply_style()

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Update font size for mindspace label
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._mindspace_label.setFont(font)

        # Style the header widget (mindspace label)
        self._header_widget.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                margin: 2px 0px 0px 0px;
            }}

            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 4px 0px 5px 7px;
            }}
        """)

        # Style the splitter
        self.setStyleSheet(f"""
            QSplitter::handle {{
                background-color: {self._style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                height: 1px;
            }}
        """)

        # Forward style updates to child views
        self._files_view.apply_style()
        self._conversations_view.apply_style()
