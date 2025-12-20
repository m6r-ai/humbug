"""Main mindspace view widget containing files, conversations, and preview views."""

import os

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QToolButton, QPushButton
from PySide6.QtCore import Qt, Signal, QSize
from PySide6.QtGui import QIcon

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.conversations.mindspace_conversations_view import MindspaceConversationsView
from humbug.mindspace.files.mindspace_files_view import MindspaceFilesView
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.mindspace.preview.mindspace_preview_view import MindspacePreviewView
from humbug.style_manager import StyleManager


class MindspaceView(QWidget):
    """Main mindspace view widget containing files, conversations, and preview sections."""

    open_mindspace_requested = Signal()  # Emits when user clicks mindspace name
    # Forward all file-related signals from all views
    file_clicked = Signal(MindspaceViewType, str, bool)  # Emits view type, path, and ephemeral flag when any file is clicked
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)
    file_moved = Signal(str, str)  # Emits (old_path, new_path)
    file_edited = Signal(str, bool)  # Emits path and ephemeral flag when file is edited
    file_opened_in_preview = Signal(str, bool)  # Emits path and ephemeral flag when file is opened in preview
    settings_requested = Signal()  # Emits when settings button is clicked

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the mindspace view widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._mindspace_manager = MindspaceManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Create main layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create header container for mindspace name
        self._header_widget = QWidget()
        self._header_widget.setObjectName("_header_widget")
        header_layout = QHBoxLayout(self._header_widget)
        header_layout.setContentsMargins(8, 6, 8, 6)
        header_layout.setSpacing(0)

        # Create mindspace name button
        self._mindspace_button = QPushButton()
        self._mindspace_button.setObjectName("_mindspace_button")
        self._mindspace_button.clicked.connect(self.open_mindspace_requested.emit)

        header_layout.addWidget(self._mindspace_button)
        header_layout.addStretch()

        # Create settings button (initially hidden)
        self._settings_button = QToolButton(self._header_widget)
        self._settings_button.setObjectName("_settings_button")
        self._settings_button.clicked.connect(self._on_settings_button_clicked)
        self._settings_button.hide()
        header_layout.addWidget(self._settings_button)

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

        # Add preview view (starts collapsed)
        self._preview_view = MindspacePreviewView()
        self._splitter.addWidget(self._preview_view)

        # Create spacer widget - invisible widget that takes up space when all sections are collapsed
        self._spacer_widget = QWidget()
        self._spacer_widget.setObjectName("_spacer_widget")
        self._splitter.addWidget(self._spacer_widget)

        # Set initial proportions: equal for conversations and files, 0 for preview and spacer
        self._splitter.setSizes([1, 1, 0, 0])

        # Set stretch factors - all sections and spacer can stretch
        self._splitter.setStretchFactor(0, 1)  # Conversations
        self._splitter.setStretchFactor(1, 1)  # Files
        self._splitter.setStretchFactor(2, 1)  # Preview
        self._splitter.setStretchFactor(3, 1)  # Spacer

        # Prevent complete collapse by making sections non-collapsible
        self._splitter.setCollapsible(0, False)  # Conversations view cannot be collapsed
        self._splitter.setCollapsible(1, False)  # Files view cannot be collapsed
        self._splitter.setCollapsible(2, False)  # Preview view cannot be collapsed
        self._splitter.setCollapsible(3, True)   # Spacer widget can be collapsed

        # Set minimum sizes to ensure headers remain visible
        self._update_minimum_sizes()

        # Connect header toggle signals to manage splitter sizes
        self._conversations_view.toggled.connect(self._on_conversations_toggled)
        self._files_view.toggled.connect(self._on_files_toggled)
        self._preview_view.toggled.connect(self._on_preview_toggled)

        # Connect files view signals - files view clicks go to editor
        self._files_view.file_clicked.connect(self.file_clicked.emit)
        self._files_view.file_deleted.connect(self.file_deleted.emit)
        self._files_view.file_renamed.connect(self.file_renamed.emit)
        self._files_view.file_moved.connect(self.file_moved.emit)
        self._files_view.file_edited.connect(self.file_edited.emit)
        self._files_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)

        # Connect conversations view signals - conversations view clicks go to editor
        self._conversations_view.file_clicked.connect(self.file_clicked.emit)
        self._conversations_view.file_deleted.connect(self.file_deleted.emit)
        self._conversations_view.file_renamed.connect(self.file_renamed.emit)
        self._conversations_view.file_moved.connect(self.file_moved.emit)
        self._conversations_view.file_edited.connect(self.file_edited.emit)
        self._conversations_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)

        # Connect preview view signals - preview view clicks go to preview
        self._preview_view.file_clicked.connect(self.file_clicked.emit)
        self._preview_view.file_deleted.connect(self.file_deleted.emit)
        self._preview_view.file_renamed.connect(self.file_renamed.emit)
        self._preview_view.file_moved.connect(self.file_moved.emit)
        self._preview_view.file_edited.connect(self.file_edited.emit)
        self._preview_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)

        # Set initial label text
        self._mindspace_button.setText(self._language_manager.strings().mindspace_label_none)

        self._on_language_changed()

    def _update_minimum_sizes(self) -> None:
        """Update minimum sizes for splitter widgets to ensure headers remain visible."""
        # Calculate minimum height needed to show just the header
        conversations_header_height = self._conversations_view.get_header_height()
        conversations_expanded = self._conversations_view.is_expanded()
        files_header_height = self._files_view.get_header_height()
        files_expanded = self._files_view.is_expanded()
        preview_header_height = self._preview_view.get_header_height()
        preview_expanded = self._preview_view.is_expanded()

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

        if preview_expanded:
            self._preview_view.setMinimumHeight(
                preview_header_height + (preview_header_height * 2 if preview_expanded else 0)
            )
            self._preview_view.setMaximumHeight(16777215)

        else:
            self._preview_view.setFixedHeight(preview_header_height)

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

    def _on_preview_toggled(self, _expanded: bool) -> None:
        """
        Handle preview section expand/collapse.

        Args:
            expanded: Whether the preview section is now expanded
        """
        self._update_splitter_sizes()

    def _update_splitter_sizes(self) -> None:
        """Update splitter sizes based on current expansion states."""
        self._update_minimum_sizes()

        conversations_expanded = self._conversations_view.is_expanded()
        files_expanded = self._files_view.is_expanded()
        preview_expanded = self._preview_view.is_expanded()

        # Get current splitter height
        total_height = self._splitter.height()
        if total_height <= 0:
            # Splitter not yet sized, use default proportions
            total_height = 600

        # Calculate header height based on zoom factor
        header_height = self._conversations_view.get_header_height()

        # Count expanded sections
        expanded_sections = sum([conversations_expanded, files_expanded, preview_expanded])

        if expanded_sections == 0:
            # Give minimal space to all sections, all remaining space to spacer
            spacer_size = total_height - (3 * header_height)
            self._splitter.setSizes([header_height, header_height, header_height, spacer_size])
            return

        # We have one or more expanded sections - distribute space evenly among them
        available_space = total_height - ((3 - expanded_sections) * header_height)
        section_space = available_space // expanded_sections

        conversations_space = section_space if conversations_expanded else header_height
        files_space = section_space if files_expanded else header_height
        preview_space = section_space if preview_expanded else header_height

        self._splitter.setSizes([conversations_space, files_space, preview_space, 0])

    def reveal_and_select_file(self, view_type: MindspaceViewType, file_path: str) -> None:
        """
        Reveal and select a file in the appropriate view (files, conversations, or preview).

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

            case MindspaceViewType.FILES:
                self._files_view.reveal_and_select_file(file_path)

            case MindspaceViewType.PREVIEW:
                self._preview_view.reveal_and_select_file(file_path)

    def set_mindspace(self, path: str) -> None:
        """
        Set the mindspace root directory.

        Args:
            path: Path to the mindspace directory, or empty string to clear
        """
        # Update the mindspace label
        if not path:
            self._mindspace_button.setText(self._language_manager.strings().mindspace_label_none)
            self._settings_button.hide()

        else:
            self._mindspace_button.setText(os.path.basename(path))
            self._settings_button.show()

        # Forward to all views
        self._files_view.set_mindspace(path)
        self._conversations_view.set_mindspace(path)
        self._preview_view.set_mindspace(path)

    def _on_settings_button_clicked(self) -> None:
        """Handle settings button click."""
        self.settings_requested.emit()

    def _on_language_changed(self) -> None:
        """Update when the language changes."""
        # Update mindspace label if no mindspace is active
        current_text = self._mindspace_button.text()
        none_text = self._language_manager.strings().mindspace_label_none
        if current_text == none_text or not current_text:
            self._mindspace_button.setText(none_text)

        # Update button tooltip
        self._mindspace_button.setToolTip(self._language_manager.strings().mindspace_name_tooltip)

        # Update settings button tooltip
        self._settings_button.setToolTip(self._language_manager.strings().mindspace_settings)

        self.apply_style()

    def _update_button_styling(self) -> None:
        """Update button styling and icons."""
        # Apply icon and styling
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        # Update settings button
        self._settings_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path("cog"), icon_base_size
        )))
        self._settings_button.setIconSize(icon_size)

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Update font size for mindspace label
        font = self._mindspace_button.font()
        font.setPointSize(int(base_font_size * zoom_factor))
        self._mindspace_button.setFont(font)

        # Update button styling
        self._update_button_styling()

        branch_icon_size = round(12 * zoom_factor)
        expand_icon = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"
        background_color = self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND)

        # Style the mindspace view
        self.setStyleSheet(f"""
            {self._style_manager.get_menu_stylesheet()}

            #_header_widget {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND)};
                margin: 0px;
                padding: 0px;
                border: none;
            }}

            #_header_widget #_settings_button {{
                background-color: {background_color};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 0px;
            }}
            #_header_widget #_settings_button:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER)};
            }}
            #_header_widget #_settings_button:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED)};
            }}
            #_header_widget #_settings_button:disabled {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: {background_color};
            }}

            #_header_widget #_mindspace_button {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                margin: 0px;
                padding: 1px 3px 1px 3px;
            }}
            #_header_widget #_mindspace_button:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER)};
            }}
            #_header_widget #_mindspace_button:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED)};
            }}
            #_header_widget #_mindspace_button:disabled {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: transparent;
            }}

            #MindspaceCollapsibleHeader {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_HEADING)};
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
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.SPLITTER)};
            }}

            QSplitter::handle {{
                background-color: {self._style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                height: 0px;
            }}

            QTreeView {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
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
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
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
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
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
        self._preview_view.apply_style()

        # Update splitter sizes after style changes (zoom factor may have changed)
        self._update_splitter_sizes()
