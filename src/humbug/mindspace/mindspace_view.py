"""Main mindspace view widget containing files, conversations, and wiki views."""

import os

from PySide6.QtWidgets import QWidget, QVBoxLayout, QSplitter, QLabel
from PySide6.QtCore import Qt, Signal

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.conversations.mindspace_conversations_view import MindspaceConversationsView
from humbug.mindspace.files.mindspace_files_view import MindspaceFilesView
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.mindspace.wiki.mindspace_wiki_view import MindspaceWikiView
from humbug.style_manager import StyleManager


class MindspaceView(QWidget):
    """Main mindspace view widget containing files, conversations, and wiki sections."""

    # Forward all file-related signals from all views
    file_clicked = Signal(MindspaceViewType, str, bool)  # Emits view type, path, and ephemeral flag when any file is clicked
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)
    file_moved = Signal(str, str)  # Emits (old_path, new_path)
    file_edited = Signal(str, bool)  # Emits path and ephemeral flag when file is edited
    file_opened_in_wiki = Signal(str, bool)  # Emits path and ephemeral flag when file is opened in wiki

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the mindspace view widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._mindspace_manager = MindspaceManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Size tracking for dynamic splitter management
        self._saved_sizes: list[int] = [1, 1, 0, 0]  # Default: equal sizes for conversations and files, 0 for wiki and spacer

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

        # Add conversations view
        self._conversations_view = MindspaceConversationsView()
        self._splitter.addWidget(self._conversations_view)

        # Add files view
        self._files_view = MindspaceFilesView()
        self._splitter.addWidget(self._files_view)

        # Add wiki view (starts collapsed)
        self._wiki_view = MindspaceWikiView()
        self._splitter.addWidget(self._wiki_view)

        # Create spacer widget - invisible widget that takes up space when all sections are collapsed
        self._spacer_widget = QWidget()
        self._spacer_widget.setObjectName("_spacer_widget")
        self._splitter.addWidget(self._spacer_widget)

        # Set initial proportions: equal for conversations and files, 0 for wiki and spacer
        self._splitter.setSizes([1, 1, 0, 0])

        # Set stretch factors - all sections and spacer can stretch
        self._splitter.setStretchFactor(0, 1)  # Conversations
        self._splitter.setStretchFactor(1, 1)  # Files
        self._splitter.setStretchFactor(2, 1)  # Wiki
        self._splitter.setStretchFactor(3, 1)  # Spacer

        # Prevent complete collapse by making sections non-collapsible
        self._splitter.setCollapsible(0, False)  # Conversations view cannot be collapsed
        self._splitter.setCollapsible(1, False)  # Files view cannot be collapsed
        self._splitter.setCollapsible(2, False)  # Wiki view cannot be collapsed
        self._splitter.setCollapsible(3, True)   # Spacer widget can be collapsed

        # Set minimum sizes to ensure headers remain visible
        self._update_minimum_sizes()

        # Connect header toggle signals to manage splitter sizes
        self._conversations_view.toggled.connect(self._on_conversations_toggled)
        self._files_view.toggled.connect(self._on_files_toggled)
        self._wiki_view.toggled.connect(self._on_wiki_toggled)

        # Connect files view signals - files view clicks go to editor
        self._files_view.file_clicked.connect(self.file_clicked.emit)
        self._files_view.file_deleted.connect(self.file_deleted.emit)
        self._files_view.file_renamed.connect(self.file_renamed.emit)
        self._files_view.file_moved.connect(self.file_moved.emit)
        self._files_view.file_edited.connect(self.file_edited.emit)
        self._files_view.file_opened_in_wiki.connect(self.file_opened_in_wiki.emit)

        # Connect conversations view signals - conversations view clicks go to editor
        self._conversations_view.file_clicked.connect(self.file_clicked.emit)
        self._conversations_view.file_deleted.connect(self.file_deleted.emit)
        self._conversations_view.file_renamed.connect(self.file_renamed.emit)
        self._conversations_view.file_moved.connect(self.file_moved.emit)
        self._conversations_view.file_edited.connect(self.file_edited.emit)
        self._conversations_view.file_opened_in_wiki.connect(self.file_opened_in_wiki.emit)

        # Connect wiki view signals - wiki view clicks go to wiki
        self._wiki_view.file_clicked.connect(self.file_clicked.emit)
        self._wiki_view.file_deleted.connect(self.file_deleted.emit)
        self._wiki_view.file_renamed.connect(self.file_renamed.emit)
        self._wiki_view.file_moved.connect(self.file_moved.emit)
        self._wiki_view.file_edited.connect(self.file_edited.emit)
        self._wiki_view.file_opened_in_wiki.connect(self.file_opened_in_wiki.emit)

        # Set initial label text
        self._mindspace_label.setText(self._language_manager.strings().mindspace_label_none)

        self._on_language_changed()

    def _update_minimum_sizes(self) -> None:
        """Update minimum sizes for splitter widgets to ensure headers remain visible."""
        # Calculate minimum height needed to show just the header
        conversations_header_height = self._conversations_view.get_header_height()
        conversations_expanded = self._conversations_view.is_expanded()
        files_header_height = self._files_view.get_header_height()
        files_expanded = self._files_view.is_expanded()
        wiki_header_height = self._wiki_view.get_header_height()
        wiki_expanded = self._wiki_view.is_expanded()

        # Set minimum size for each view to their header height
        # This prevents them from being collapsed completely
        if conversations_expanded:
            self._conversations_view.setMinimumHeight(
                conversations_header_height + (conversations_header_height * 2 if conversations_expanded else 0)
            )
            self._conversations_view.setMaximumHeight(16777215)

        else:
            self._conversations_view.setFixedHeight(conversations_header_height)

        if files_expanded:
            self._files_view.setMinimumHeight(
                files_header_height + (files_header_height * 2 if files_expanded else 0)
            )
            self._files_view.setMaximumHeight(16777215)

        else:
            self._files_view.setFixedHeight(files_header_height)

        if wiki_expanded:
            self._wiki_view.setMinimumHeight(
                wiki_header_height + (wiki_header_height * 2 if wiki_expanded else 0)
            )
            self._wiki_view.setMaximumHeight(16777215)

        else:
            self._wiki_view.setFixedHeight(wiki_header_height)

        # The spacer widget can be collapsed to 0
        self._spacer_widget.setMinimumHeight(0)

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

    def _on_wiki_toggled(self, _expanded: bool) -> None:
        """
        Handle wiki section expand/collapse.

        Args:
            expanded: Whether the wiki section is now expanded
        """
        self._update_splitter_sizes()

    def _update_splitter_sizes(self) -> None:
        """Update splitter sizes based on current expansion states."""
        self._update_minimum_sizes()

        conversations_expanded = self._conversations_view.is_expanded()
        files_expanded = self._files_view.is_expanded()
        wiki_expanded = self._wiki_view.is_expanded()

        # Get current splitter height
        total_height = self._splitter.height()
        if total_height <= 0:
            # Splitter not yet sized, use default proportions
            total_height = 600

        # Calculate header height based on zoom factor
        header_height = self._conversations_view.get_header_height()

        # Count expanded sections
        expanded_sections = sum([conversations_expanded, files_expanded, wiki_expanded])

        if expanded_sections == 0:
            # All collapsed - give minimal space to all sections, rest to spacer
            current_sizes = self._splitter.sizes()
            if len(current_sizes) >= 3 and any(size > header_height for size in current_sizes[:3]):
                self._saved_sizes = current_sizes

            # Give minimal space to all sections, all remaining space to spacer
            spacer_size = total_height - (3 * header_height)
            self._splitter.setSizes([header_height, header_height, header_height, spacer_size])

        elif expanded_sections == 1:
            # Only one expanded - give most space to expanded section
            current_sizes = self._splitter.sizes()
            if len(current_sizes) >= 3 and sum(size > header_height for size in current_sizes[:3]) > 1:
                self._saved_sizes = current_sizes

            expanded_size = total_height - (2 * header_height)
            if conversations_expanded:
                self._splitter.setSizes([expanded_size, header_height, header_height, 0])

            elif files_expanded:
                self._splitter.setSizes([header_height, expanded_size, header_height, 0])

            else:  # wiki_expanded
                self._splitter.setSizes([header_height, header_height, expanded_size, 0])

        elif expanded_sections == 2:
            # Two expanded - split space between them
            current_sizes = self._splitter.sizes()
            if len(current_sizes) >= 3 and sum(size > header_height for size in current_sizes[:3]) > 2:
                self._saved_sizes = current_sizes

            available_space = total_height - header_height
            half_space = available_space // 2

            if conversations_expanded and files_expanded:
                self._splitter.setSizes([half_space, half_space, header_height, 0])

            elif conversations_expanded and wiki_expanded:
                self._splitter.setSizes([half_space, header_height, half_space, 0])

            else:  # files_expanded and wiki_expanded
                self._splitter.setSizes([header_height, half_space, half_space, 0])

        else:
            # All three expanded - restore saved sizes or use equal split
            if len(self._saved_sizes) >= 3 and sum(self._saved_sizes[:3]) > 0:
                # Restore the proportions of the three sections
                conversations_size = self._saved_sizes[0]
                files_size = self._saved_sizes[1]
                wiki_size = self._saved_sizes[2]
                self._splitter.setSizes([conversations_size, files_size, wiki_size, 0])

            else:
                # Equal split among three sections
                third_height = total_height // 3
                self._splitter.setSizes([third_height, third_height, third_height, 0])

    def reveal_and_select_file(self, view_type: MindspaceViewType, file_path: str) -> None:
        """
        Reveal and select a file in the appropriate view (files, conversations, or wiki).

        Args:
            file_path: Absolute path to the file to reveal and select
        """
        if not file_path:
            return

        # Use mindspace manager to properly determine if file is in conversations hierarchy
        if not self._mindspace_manager.has_mindspace():
            return

        match view_type:
            case MindspaceViewType.CONVERSATIONS:
                self._conversations_view.reveal_and_select_file(file_path)

            case MindspaceViewType.WIKI:
                self._wiki_view.reveal_and_select_file(file_path)

            case MindspaceViewType.FILES:
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

        # Forward to all views
        self._files_view.set_mindspace(path)
        self._conversations_view.set_mindspace(path)
        self._wiki_view.set_mindspace(path)

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
        font = self._mindspace_label.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._mindspace_label.setFont(font)

        branch_icon_size = round(12 * zoom_factor)
        expand_icon = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"

        # Style the mindspace view
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
                padding: 7px 0px 7px 10px;
            }}

            #MindspaceCollapsibleHeader {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                border-radius: 0px;
                margin: 0px;
                padding: 0px 0px 1px 0px;
            }}
            #MindspaceCollapsibleHeader[splitter="true"] {{
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.SPLITTER)};
            }}
            #MindspaceCollapsibleHeader[hovered="true"] {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)};
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

            MindspaceRootDropWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border: none;
            }}
            MindspaceRootDropWidget[is_drop_target="true"] {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
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
        self._wiki_view.apply_style()

        # Update splitter sizes after style changes (zoom factor may have changed)
        self._update_splitter_sizes()
