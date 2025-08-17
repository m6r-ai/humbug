"""Main mindspace view widget containing files and conversations views."""

import os

from PySide6.QtWidgets import QWidget, QVBoxLayout, QSplitter, QLabel
from PySide6.QtCore import Qt, Signal

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_conversations_view import MindspaceConversationsView
from humbug.mindspace.mindspace_files_view import MindspaceFilesView
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.style_manager import StyleManager


class MindspaceView(QWidget):
    """Main mindspace view widget containing files and conversations sections."""

    # Forward all file-related signals from both views
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
        self._mindspace_manager = MindspaceManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Size tracking for dynamic splitter management
        self._saved_sizes: list[int] = [1, 1, 0]  # Default: equal sizes for sections, 0 for spacer

        # Create main layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create header container for mindspace name
        self._header_widget = QWidget()
        self._header_widget.setObjectName("_header_widget")
        header_layout = QVBoxLayout(self._header_widget)
        header_layout.setContentsMargins(0, 0, 0, 0)
        header_layout.setSpacing(0)

        # Create mindspace label
        self._mindspace_label = QLabel()
        self._mindspace_label.setIndent(0)
        self._mindspace_label.setContentsMargins(0, 0, 0, 0)

        header_layout.addWidget(self._mindspace_label)
        layout.addWidget(self._header_widget)

        # Create splitter for mindspace views
        self._splitter = QSplitter(Qt.Orientation.Vertical)
        layout.addWidget(self._splitter)

        self._conversations_view = MindspaceConversationsView()
        self._splitter.addWidget(self._conversations_view)

        self._files_view = MindspaceFilesView()
        self._splitter.addWidget(self._files_view)

        # Create spacer widget - invisible widget that takes up space when both sections are collapsed
        self._spacer_widget = QWidget()
        self._spacer_widget.setObjectName("_spacer_widget")
        self._splitter.addWidget(self._spacer_widget)

        # Set equal proportions initially for the two sections, no space for spacer
        self._splitter.setSizes([1, 1, 0])

        # Set stretch factors - both sections and spacer can stretch
        self._splitter.setStretchFactor(0, 1)
        self._splitter.setStretchFactor(1, 1)
        self._splitter.setStretchFactor(2, 1)

        # Connect header toggle signals to manage splitter sizes
        self._conversations_view._header.toggled.connect(self._on_conversations_toggled)
        self._files_view._header.toggled.connect(self._on_files_toggled)

        # Connect file view signals to forward them
        self._files_view.file_single_clicked.connect(self.file_single_clicked.emit)
        self._files_view.file_double_clicked.connect(self.file_double_clicked.emit)
        self._files_view.file_deleted.connect(self.file_deleted.emit)
        self._files_view.file_renamed.connect(self.file_renamed.emit)
        self._files_view.file_moved.connect(self.file_moved.emit)
        self._files_view.file_edited.connect(self.file_edited.emit)

        # Connect conversations view signals to forward them
        self._conversations_view.file_single_clicked.connect(self.file_single_clicked.emit)
        self._conversations_view.file_double_clicked.connect(self.file_double_clicked.emit)
        self._conversations_view.file_deleted.connect(self.file_deleted.emit)
        self._conversations_view.file_renamed.connect(self.file_renamed.emit)
        self._conversations_view.file_moved.connect(self.file_moved.emit)
        self._conversations_view.file_edited.connect(self.file_edited.emit)

        # Set initial label text
        self._mindspace_label.setText(self._language_manager.strings().mindspace_label_none)

        self._on_language_changed()

    def _on_conversations_toggled(self, _expanded: bool) -> None:
        """
        Handle conversations section expand/collapse.

        Args:
            expanded: Whether the conversations section is now expanded
        """
        self._update_splitter_sizes()

    def _on_files_toggled(self, _expanded: bool) -> None:
        """
        Handle files section expand/collapse.

        Args:
            expanded: Whether the files section is now expanded
        """
        self._update_splitter_sizes()

    def _update_splitter_sizes(self) -> None:
        """Update splitter sizes based on current expansion states."""
        conversations_expanded = self._conversations_view.is_expanded()
        files_expanded = self._files_view.is_expanded()

        # Get current splitter height
        total_height = self._splitter.height()
        if total_height <= 0:
            # Splitter not yet sized, use default proportions
            total_height = 400

        # Calculate header height based on zoom factor
        header_height = self._conversations_view.get_header_height()

        if conversations_expanded and files_expanded:
            # Both expanded - restore saved sizes or use equal split, no space for spacer
            if len(self._saved_sizes) >= 2 and sum(self._saved_sizes[:2]) > 0:
                # Restore the proportions of the two sections
                conversations_size = self._saved_sizes[0]
                files_size = self._saved_sizes[1]
                self._splitter.setSizes([conversations_size, files_size, 0])

            else:
                # Equal split
                half_height = total_height // 2
                self._splitter.setSizes([half_height, half_height, 0])

        elif conversations_expanded and not files_expanded:
            # Only conversations expanded
            # Save current sizes before changing (if both were previously expanded)
            current_sizes = self._splitter.sizes()
            if len(current_sizes) >= 2 and all(size > header_height for size in current_sizes[:2]):
                self._saved_sizes = current_sizes

            # Give most space to conversations, minimal to files, no space for spacer
            conversations_size = total_height - header_height
            self._splitter.setSizes([conversations_size, header_height, 0])

        elif not conversations_expanded and files_expanded:
            # Only files expanded
            # Save current sizes before changing (if both were previously expanded)
            current_sizes = self._splitter.sizes()
            if len(current_sizes) >= 2 and all(size > header_height for size in current_sizes[:2]):
                self._saved_sizes = current_sizes

            # Give most space to files, minimal to conversations, no space for spacer
            files_size = total_height - header_height
            self._splitter.setSizes([header_height, files_size, 0])

        else:
            # Both collapsed - give minimal space to both sections, rest to spacer
            # Save current sizes before changing
            current_sizes = self._splitter.sizes()
            if len(current_sizes) >= 2 and all(size > header_height for size in current_sizes[:2]):
                self._saved_sizes = current_sizes

            # Give minimal space to both sections, all remaining space to spacer
            spacer_size = total_height - (2 * header_height)
            self._splitter.setSizes([header_height, header_height, spacer_size])

    def get_conversations_expanded_state(self) -> bool:
        """
        Get the expanded state of the conversations section.

        Returns:
            True if conversations section is expanded, False if collapsed
        """
        return self._conversations_view.is_expanded()

    def set_conversations_expanded_state(self, expanded: bool) -> None:
        """
        Set the expanded state of the conversations section.

        Args:
            expanded: Whether the conversations section should be expanded
        """
        self._conversations_view.set_expanded(expanded)

    def get_files_expanded_state(self) -> bool:
        """
        Get the expanded state of the files section.

        Returns:
            True if files section is expanded, False if collapsed
        """
        return self._files_view.is_expanded()

    def set_files_expanded_state(self, expanded: bool) -> None:
        """
        Set the expanded state of the files section.

        Args:
            expanded: Whether the files section should be expanded
        """
        self._files_view.set_expanded(expanded)

    def reveal_and_select_file(self, file_path: str) -> None:
        """
        Reveal and select a file in the appropriate view (files or conversations).

        Args:
            file_path: Absolute path to the file to reveal and select
        """
        if not file_path:
            return

        # Use mindspace manager to properly determine if file is in conversations hierarchy
        if self._mindspace_manager.has_mindspace():
            try:
                relative_path = self._mindspace_manager.get_mindspace_relative_path(file_path)
                if relative_path and relative_path.startswith("conversations" + os.sep):
                    # File is within the mindspace's conversations directory
                    self._conversations_view.reveal_and_select_file(file_path)

                else:
                    # File is elsewhere in mindspace
                    self._files_view.reveal_and_select_file(file_path)

            except Exception:
                # Fallback to files view if path conversion fails
                self._files_view.reveal_and_select_file(file_path)

        else:
            # No mindspace active, default to files view
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

        # Forward to both views
        self._files_view.set_mindspace(path)
        self._conversations_view.set_mindspace(path)

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

        branch_icon_size = round(12 * zoom_factor)
        expand_icon = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"

        # Style the splitter
        self.setStyleSheet(f"""
            #_header_widget {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                margin: 0px;
                padding: 0px;
                border: none;
            }}

            #_header_widget QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                margin: 0px;
                padding: 6px 0px 7px 10px;
            }}

            #MindspaceCollapsibleHeader {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                border-radius: 0px;
                margin: 0px;
                padding: 0px 0px 1px 0px;
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.SPLITTER)};
            }}

            #MindspaceCollapsibleHeader QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 0px;
                margin: 0px;
            }}

            #MindspaceCollapsibleHeader QToolButton#_expand_button {{
                background-color: transparent;
                border: none;
                padding: 0px 0px 0px 2px;
                margin: 0px;
            }}

            QWidget#_spacer_widget {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.SPLITTER)};
            }}

            QSplitter::handle {{
                background-color: {self._style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                height: 0px;
            }}

            QTreeView {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border: none;
                padding: 0 0 0 8px;
            }}
            QTreeView::item {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 2px 0 2px 0;
                margin: 0px;
            }}
            QTreeView::item:selected {{
                background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            }}
            QTreeView::item:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)};
            }}
            QTreeView::branch {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
            }}
            QTreeView::branch:has-children:!has-siblings:closed,
            QTreeView::branch:closed:has-children:has-siblings {{
                image: url("{self._style_manager.get_icon_path(expand_icon)}");
                padding: 0px;
                width: {branch_icon_size}px;
                height: {branch_icon_size}px;
            }}
            QTreeView::branch:open:has-children:!has-siblings,
            QTreeView::branch:open:has-children:has-siblings {{
                image: url("{self._style_manager.get_icon_path("arrow-down")}");
                padding: 0px;
                width: {branch_icon_size}px;
                height: {branch_icon_size}px;
            }}

            QScrollBar:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}

            QToolTip {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 1px;
                margin: 0px;
                border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}

            QMenu::right-arrow {{
                image: url({self._style_manager.get_icon_path('arrow-right')});
                width: 16px;
                height: 16px;
            }}
            QMenu::left-arrow {{
                image: url({self._style_manager.get_icon_path('arrow-left')});
                width: 16px;
                height: 16px;
            }}

            QLineEdit[is_valid="true"] {{
                background-color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER)};
                padding: 2px;
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QLineEdit[is_valid="false"] {{
                background-color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_ERROR)};
                padding: 1px;
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
        """)

        # Forward style updates to child views
        self._files_view.apply_style()
        self._conversations_view.apply_style()

        # Update splitter sizes after style changes (zoom factor may have changed)
        self._update_splitter_sizes()
