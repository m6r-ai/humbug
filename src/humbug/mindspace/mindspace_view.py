"""Main mindspace view widget containing files, conversations, and preview views."""

import os

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QToolButton, QPushButton, QLabel
from PySide6.QtCore import Qt, Signal, QSize
from PySide6.QtGui import QIcon, QColor, QPalette

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.conversations.mindspace_conversations_view import MindspaceConversationsView
from humbug.mindspace.files.mindspace_files_view import MindspaceFilesView
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.mindspace.preview.mindspace_preview_view import MindspacePreviewView
from humbug.style_manager import StyleManager, ColorMode


class MindspaceView(QWidget):
    """Main mindspace view widget containing files, conversations, and preview sections."""

    open_mindspace_requested = Signal()  # Emits when user clicks mindspace name
    toggle_panel_requested = Signal()   # Emits when user clicks the collapse/expand button
    # Forward all file-related signals from all views
    file_clicked = Signal(MindspaceViewType, str, bool)  # Emits view type, path, and ephemeral flag when any file is clicked
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)
    file_moved = Signal(str, str)  # Emits (old_path, new_path)
    file_edited = Signal(str, bool)  # Emits path and ephemeral flag when file is edited
    file_opened_in_preview = Signal(str, bool)  # Emits path and ephemeral flag when file is opened in preview
    new_conversation_requested = Signal(str)  # Emits target folder path when user requests new conversation in folder
    settings_requested = Signal()  # Emits when settings button is clicked

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the mindspace view widget."""
        super().__init__(parent)
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

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
        self._header_widget.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
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

        # Toggle button — collapses/expands the sidebar panel
        self._toggle_button = QToolButton(self._header_widget)
        self._toggle_button.setObjectName("_toggle_button")
        self._toggle_button.clicked.connect(self.toggle_panel_requested.emit)
        header_layout.addWidget(self._toggle_button)

        layout.addWidget(self._header_widget)

        # Action buttons row: [+ New Conversation (expanding)] [New Folder (icon)]
        self._action_row = QWidget()
        self._action_row.setObjectName("_action_row")
        action_layout = QHBoxLayout(self._action_row)
        action_layout.setContentsMargins(10, 0, 10, 6)
        action_layout.setSpacing(6)

        self._new_conversation_button = QPushButton()
        self._new_conversation_button.setObjectName("_new_conversation_button")
        self._new_conversation_button.clicked.connect(self._on_new_conversation_clicked)
        self._new_conversation_button.setEnabled(False)
        action_layout.addWidget(self._new_conversation_button)

        self._new_folder_button = QToolButton()
        self._new_folder_button.setObjectName("_new_folder_button")
        self._new_folder_button.clicked.connect(self._on_new_folder_clicked)
        self._new_folder_button.setEnabled(False)
        action_layout.addWidget(self._new_folder_button)

        layout.addWidget(self._action_row)

        # "NAVIGATION" section label
        self._nav_label = QLabel()
        self._nav_label.setObjectName("_nav_label")
        layout.addWidget(self._nav_label)

        # Create splitter for mindspace views
        self._splitter = QSplitter(Qt.Orientation.Vertical)
        self._splitter.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
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
        self._conversations_view.new_conversation_requested.connect(self.new_conversation_requested.emit)

        # Connect preview view signals - preview view clicks go to preview
        self._preview_view.file_clicked.connect(self.file_clicked.emit)
        self._preview_view.file_deleted.connect(self.file_deleted.emit)
        self._preview_view.file_renamed.connect(self.file_renamed.emit)
        self._preview_view.file_moved.connect(self.file_moved.emit)
        self._preview_view.file_edited.connect(self.file_edited.emit)
        self._preview_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)

        # --- Icon rail (shown when the panel is collapsed to narrow mode) ---
        self._icon_rail = QWidget()
        self._icon_rail.setObjectName("_icon_rail")
        self._icon_rail.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        icon_rail_layout = QVBoxLayout(self._icon_rail)
        icon_rail_layout.setContentsMargins(6, 8, 6, 8)
        icon_rail_layout.setSpacing(0)
        icon_rail_layout.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignTop)

        # Single expand button — click to slide the panel back open
        self._rail_expand_btn = QToolButton()
        self._rail_expand_btn.setObjectName("_rail_icon_btn")
        self._rail_expand_btn.clicked.connect(self.toggle_panel_requested.emit)
        icon_rail_layout.addWidget(self._rail_expand_btn, 0, Qt.AlignmentFlag.AlignHCenter)

        layout.addWidget(self._icon_rail)
        self._icon_rail.hide()  # hidden by default (full panel is shown)

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
            self._new_conversation_button.setEnabled(False)
            self._new_folder_button.setEnabled(False)
        else:
            self._mindspace_button.setText(os.path.basename(path))
            self._settings_button.show()
            self._new_conversation_button.setEnabled(True)
            self._new_folder_button.setEnabled(True)

        # Forward to all views
        self._files_view.set_mindspace(path)
        self._conversations_view.set_mindspace(path)
        self._preview_view.set_mindspace(path)

    def set_panel_collapsed(self, collapsed: bool) -> None:
        """Switch between full panel and narrow icon-rail mode."""
        self._header_widget.setVisible(not collapsed)
        self._action_row.setVisible(not collapsed)
        self._nav_label.setVisible(not collapsed)
        self._splitter.setVisible(not collapsed)
        self._icon_rail.setVisible(collapsed)
        self._update_button_styling()

    def _on_new_conversation_clicked(self) -> None:
        """Handle New Conversation button click."""
        self.new_conversation_requested.emit("")

    def _on_new_folder_clicked(self) -> None:
        """Handle New Folder button click — create a folder in the conversations tree."""
        self._conversations_view.start_new_folder()

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

        # Update new conversation button text and nav label
        self._new_conversation_button.setText(f"+ {self._language_manager.strings().new_conversation}")
        self._new_folder_button.setToolTip(self._language_manager.strings().new_folder)
        self._nav_label.setText("NAVIGATION")

        self.apply_style()

    def _update_button_styling(self) -> None:
        """Update button styling and icons."""
        # Apply icon and styling
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * self._style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        # Update settings button
        self._settings_button.setIcon(QIcon(self._style_manager.scale_icon("cog", icon_base_size)))
        self._settings_button.setIconSize(icon_size)

        # Update toggle button (panel collapse)
        toggle_icon_name = (
            "arrow-left"
            if self.layoutDirection() == Qt.LayoutDirection.LeftToRight
            else "arrow-right"
        )
        self._toggle_button.setIcon(QIcon(self._style_manager.scale_icon(toggle_icon_name, icon_base_size)))
        self._toggle_button.setIconSize(icon_size)

        # Folder button icon
        folder_icon_size = QSize(icon_scaled_size, icon_scaled_size)
        self._new_folder_button.setIcon(QIcon(self._style_manager.scale_icon("plus", icon_base_size)))
        self._new_folder_button.setIconSize(folder_icon_size)

        # Rail expand button icon
        rail_base = 16
        rail_size = QSize(int(rail_base * self._style_manager.zoom_factor()),
                          int(rail_base * self._style_manager.zoom_factor()))
        expand_icon_name = (
            "arrow-right"
            if self.layoutDirection() == Qt.LayoutDirection.LeftToRight
            else "arrow-left"
        )
        self._rail_expand_btn.setIcon(QIcon(self._style_manager.scale_icon(expand_icon_name, rail_base)))
        self._rail_expand_btn.setIconSize(rail_size)

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Update font size for mindspace label
        font = self._mindspace_button.font()
        font.setPointSize(int(base_font_size * zoom_factor))
        self._mindspace_button.setFont(font)

        # Update font size for new conversation button
        btn_font = self._new_conversation_button.font()
        btn_font.setPointSizeF(base_font_size * zoom_factor * 0.95)
        btn_font.setBold(True)
        self._new_conversation_button.setFont(btn_font)

        # Update font for nav label
        nav_font = self._nav_label.font()
        nav_font.setPointSizeF(base_font_size * zoom_factor * 0.72)
        nav_font.setBold(True)
        self._nav_label.setFont(nav_font)

        # Update button styling
        self._update_button_styling()

        branch_icon_size = round(12 * zoom_factor)

        # Deal with a few layout direction specifics.  We really shouldn't need to adjust the padding here, but
        # the layout engine seems to be having trouble with it in RTL mode.
        if self.layoutDirection() == Qt.LayoutDirection.LeftToRight:
            expand_icon = "arrow-right"
            tree_left_padding = 6
            tree_right_padding = 10

        else:
            expand_icon = "arrow-left"
            tree_left_padding = 10
            tree_right_padding = 6

        panel_surface = self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        panel_raised = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)
        sidebar_canvas = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        header_surface = (
            "rgba(8, 12, 20, 0.94)"
            if self._style_manager.color_mode() == ColorMode.DARK
            else panel_surface
        )

        # Style the mindspace view
        self.setStyleSheet(f"""
            {self._style_manager.get_menu_stylesheet()}

            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
            }}

            #_header_widget {{
                background-color: {header_surface};
                margin: 8px 8px 6px 8px;
                padding: 0px;
                border: 1px solid {self._style_manager.get_color_str(ColorRole.MENU_BORDER)};
                border-radius: 14px;
            }}

            #_header_widget #_settings_button,
            #_header_widget #_toggle_button {{
                background-color: transparent;
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                padding: 4px;
                margin: 4px;
                border-radius: 10px;
            }}
            #_header_widget #_settings_button:hover,
            #_header_widget #_toggle_button:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            #_header_widget #_settings_button:pressed,
            #_header_widget #_toggle_button:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_PRESSED)};
            }}
            #_header_widget #_settings_button:disabled {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: transparent;
            }}

            #_header_widget #_mindspace_button {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                margin: 0px;
                padding: 10px 12px;
                text-align: left;
                font-weight: 700;
            }}
            #_header_widget #_mindspace_button:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
                border-radius: 10px;
            }}
            #_header_widget #_mindspace_button:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_PRESSED)};
                border-radius: 10px;
            }}
            #_header_widget #_mindspace_button:disabled {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: transparent;
            }}

            #_new_conversation_button {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: #ffffff;
                border: none;
                border-radius: 10px;
                padding: 10px 16px;
                margin: 0px 10px 6px 10px;
                text-align: center;
                font-weight: 700;
                letter-spacing: 0.2px;
            }}
            #_new_conversation_button:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            #_new_conversation_button:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
            #_new_conversation_button:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}

            QWidget#_action_row {{
                background-color: transparent;
            }}

            QToolButton#_new_folder_button {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.MENU_BORDER)};
                border-radius: 10px;
                padding: 9px 10px;
            }}
            QToolButton#_new_folder_button:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QToolButton#_new_folder_button:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QToolButton#_new_folder_button:disabled {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                border-color: transparent;
            }}

            QLabel#_nav_label {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: transparent;
                border: none;
                padding: 6px 14px 4px 14px;
                letter-spacing: 0.8px;
            }}

            QWidget#_icon_rail {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
                border-right: 1px solid {self._style_manager.get_color_str(ColorRole.MENU_BORDER)};
            }}

            QToolButton#_rail_icon_btn {{
                background-color: transparent;
                border: none;
                border-radius: 8px;
                padding: 6px;
                margin: 0px;
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QToolButton#_rail_icon_btn:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            QToolButton#_rail_icon_btn:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_PRESSED)};
            }}

            #MindspaceCollapsibleHeader {{
                background-color: {panel_surface};
                border-radius: 12px;
                margin: 4px 8px;
                padding: 0px;
                border: 1px solid {self._style_manager.get_color_str(ColorRole.MENU_BORDER)};
            }}
            #MindspaceCollapsibleHeader[splitter="true"] {{
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.MENU_BORDER)};
            }}
            #MindspaceCollapsibleHeader[hovered="true"] {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}

            #MindspaceCollapsibleHeader QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 0px;
                margin: 0px;
                font-weight: 600;
            }}

            #MindspaceCollapsibleHeader QToolButton#_expand_button {{
                background-color: transparent;
                border: none;
                padding: 2px 2px 2px 4px;
                margin: 0px;
            }}

            QWidget#_spacer_widget {{
                background-color: {self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
                border-top: none;
            }}

            QSplitter::handle {{
                background-color: transparent;
                margin: 0 8px;
                height: 6px;
            }}

            QTreeView {{
                background-color: {panel_surface};
                alternate-background-color: {panel_raised};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.MENU_BORDER)};
                border-radius: 12px;
                padding: 8px {tree_right_padding}px 8px {tree_left_padding}px;
                margin: 0px 8px 8px 8px;
                outline: none;
            }}
            QTreeView::item {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 6px 8px 6px 4px;
                margin: 1px 0px;
                border-radius: 8px;
            }}
            QTreeView::item:selected {{
                background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_BRIGHT)};
            }}
            QTreeView::item:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            QTreeView::branch {{
                background-color: {panel_surface};
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
                background-color: {panel_surface};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.MENU_BORDER)};
                border-radius: 12px;
            }}
            MindspaceRootDropWidget[is_drop_target="true"] {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER)};
            }}

            {self._style_manager.get_scrollbar_stylesheet()}

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

        canvas_color = QColor(sidebar_canvas)
        panel_color = QColor(header_surface)
        for widget, color in (
            (self, canvas_color),
            (self._header_widget, panel_color),
            (self._splitter, canvas_color),
        ):
            palette = widget.palette()
            palette.setColor(QPalette.ColorRole.Window, color)
            palette.setColor(QPalette.ColorRole.Base, color)
            widget.setPalette(palette)
            widget.setAutoFillBackground(True)

        # Forward style updates to child views
        self._files_view.apply_style()
        self._conversations_view.apply_style()
        self._preview_view.apply_style()

        # Update splitter sizes after style changes (zoom factor may have changed)
        self._update_splitter_sizes()
