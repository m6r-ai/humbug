"""File tree view implementation for mindspace files."""

import os
from typing import Optional

from PySide6.QtWidgets import (
    QFileSystemModel, QWidget, QHBoxLayout, QVBoxLayout, QMenu, QDialog,
    QLabel, QSizePolicy
)
from PySide6.QtCore import Signal, QModelIndex, Qt, QSize

from humbug.gui.color_role import ColorRole
from humbug.gui.conversation_rename_dialog import ConversationRenameDialog
from humbug.gui.file_tree_icon_provider import FileTreeIconProvider
from humbug.gui.file_tree_view import FileTreeView
from humbug.gui.message_box import MessageBox, MessageBoxButton, MessageBoxType
from humbug.gui.style_manager import StyleManager
from humbug.gui.mindspace_file_model import MindspaceFileModel
from humbug.gui.mindspace_file_tree_style import MindspaceFileTreeStyle
from humbug.language.language_manager import LanguageManager


class MindspaceFileTree(QWidget):
    """Tree view widget for displaying mindspace files."""

    file_activated = Signal(str)  # Emits path when file is activated
    file_deleted = Signal(str)  # Emits path when file is deleted
    file_renamed = Signal(str, str)  # Emits (old_path, new_path)

    def __init__(self, parent=None):
        """Initialize the file tree widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create header container
        header_widget = QWidget()
        header_widget.setObjectName("header_widget")
        header_layout = QHBoxLayout(header_widget)
        header_layout.setContentsMargins(0, 0, 0, 0)
        header_layout.setSpacing(0)

        # Create mindspace label
        self._mindspace_label = QLabel()
        self._mindspace_label.setContentsMargins(0, 0, 0, 0)
        header_layout.addWidget(self._mindspace_label)

        # Create a spacer widget
        spacer = QWidget()
        spacer.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)
        header_layout.addWidget(spacer)

        layout.addWidget(header_widget)

        # Create tree view
        self._tree_view = FileTreeView()
        self._tree_view.customContextMenuRequested.connect(self._show_context_menu)
        self._tree_style = MindspaceFileTreeStyle(self._style_manager)
        self._tree_view.setStyle(self._tree_style)

        # Create file system model
        self._icon_provider = FileTreeIconProvider()
        self._fs_model = QFileSystemModel()
        self._fs_model.setReadOnly(True)

        # Create filter model
        self._filter_model = MindspaceFileModel()
        self._filter_model.setSourceModel(self._fs_model)

        # Set model on tree view
        self._tree_view.setModel(self._filter_model)

        # Connect signals
        self._tree_view.activated.connect(self._handle_activation)

        # Add to layout
        layout.addWidget(self._tree_view)

        # Hide horizontal scrollbar
        self._tree_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Apply styling
        self._handle_style_changed()

        # Track current mindspace
        self._mindspace_path: Optional[str] = None
        self._style_manager.style_changed.connect(self._handle_style_changed)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        # Set initial label text
        self._mindspace_label.setText(self._language_manager.strings.mindspace_label_none)

    def _show_context_menu(self, position):
        """Show context menu for file tree items."""
        # Get the index at the clicked position
        index = self._tree_view.indexAt(position)
        if not index.isValid():
            return

        # Get the file path
        source_index = self._filter_model.mapToSource(index)
        path = self._fs_model.filePath(source_index)

        # Don't show menu for directories
        if os.path.isdir(path):
            return

        # Create context menu
        menu = QMenu(self)
        ext = os.path.splitext(path)[1].lower()

        # Add rename option for conversation files
        rename_action = None
        if ext == '.conv':
            rename_action = menu.addAction(self._language_manager.strings.rename_conversation)
            menu.addSeparator()

        delete_action = menu.addAction(self._language_manager.strings.delete_file)

        # Show menu and handle selection
        action = menu.exec_(self._tree_view.viewport().mapToGlobal(position))
        if action == delete_action:
            self._handle_delete_file(path)
            return

        if ext == '.conv' and action == rename_action:
            self._handle_rename_conversation(path)

    def _handle_delete_file(self, path: str):
        """Handle request to delete a file.

        Args:
            path: Path to the file to delete
        """
        # Show confirmation dialog using MessageBox
        strings = self._language_manager.strings
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.confirm_delete_title,
            strings.confirm_delete_message.format(os.path.basename(path)) + "\n\n" +
            strings.delete_warning_detail,
            [MessageBoxButton.YES, MessageBoxButton.NO]
        )

        if result == MessageBoxButton.YES:
            try:
                # First emit signal so tabs can be closed
                self.file_deleted.emit(path)

                # Then delete the file
                os.remove(path)
            except OSError as e:
                strings = self._language_manager.strings
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.file_error_title,
                    strings.error_deleting_file.format(str(e)),
                    [MessageBoxButton.OK]
                )

    def _handle_rename_conversation(self, path: str):
        """Handle request to rename a conversation file.

        Args:
            path: Path to the conversation file to rename
        """
        # Show dialog to get new name
        old_name = os.path.splitext(os.path.basename(path))[0]
        dialog = ConversationRenameDialog(old_name, self)
        if dialog.exec() != QDialog.Accepted:
            return

        new_name = dialog.get_name()
        if not new_name:
            return

        # Ensure name ends with .conv
        if not new_name.endswith('.conv'):
            new_name += '.conv'

        # Get paths
        old_dir = os.path.dirname(path)
        new_path = os.path.join(old_dir, new_name)

        # Check if target already exists
        if os.path.exists(new_path):
            strings = self._language_manager.strings
            MessageBox.show_message(
                self,
                MessageBoxType.WARNING,
                strings.error_title_rename,
                strings.error_rename_exists.format(new_name),
                [MessageBoxButton.OK]
            )
            return

        try:
            # First emit signal so tabs can be updated
            self.file_renamed.emit(path, new_path)

            # Then rename the file
            os.rename(path, new_path)
        except OSError as e:
            strings = self._language_manager.strings
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_title_rename,
                strings.error_rename_failed.format(str(e)),
                [MessageBoxButton.OK]
            )

    def set_mindspace(self, path: str):
        """Set the mindspace root directory."""
        self._mindspace_path = path

        if not path:
            # Clear the model when no mindspace is active
            self._filter_model.set_mindspace_root(None)
            self._mindspace_label.setText(self._language_manager.strings.mindspace_label_none)
            return

        self._fs_model.setRootPath(path)
        self._filter_model.set_mindspace_root(path)
        self._mindspace_label.setText(os.path.basename(path))

        # Set the root index through the proxy model
        root_index = self._filter_model.mapFromSource(
            self._fs_model.index(path)
        )
        self._tree_view.setRootIndex(root_index)

        # Hide size, type, and date columns
        self._tree_view.header().hideSection(1)  # Size
        self._tree_view.header().hideSection(2)  # Type
        self._tree_view.header().hideSection(3)  # Date

    def _handle_activation(self, index: QModelIndex):
        """Handle item activation (double-click or Enter)."""
        # Get the file path from the source model
        source_index = self._filter_model.mapToSource(index)
        path = self._fs_model.filePath(source_index)

        # Only emit for files, not directories
        if os.path.isfile(path):
            self.file_activated.emit(path)

    def _handle_language_changed(self):
        """Update when the language changes."""
        if not self._mindspace_path:
            self._mindspace_label.setText(self._language_manager.strings.mindspace_label_none)

        self._handle_style_changed()

    def _handle_style_changed(self):
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor
        base_font_size = self._style_manager.base_font_size

        # Update font size for label
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._mindspace_label.setFont(font)

        self._icon_provider.update_icons()
        self._fs_model.setIconProvider(self._icon_provider)
        self._tree_view.setIconSize(QSize(16 * zoom_factor, 16 * zoom_factor))

        # Update font size for tree
        self.setFont(font)
        self._tree_view.setFont(font)

        branch_icon_size = int(12 * zoom_factor)
        expand_icon = "arrow-right" if self.layoutDirection() == Qt.LeftToRight else "arrow-left"

        self.setStyleSheet(f"""
            QWidget#header_widget, QWidget#header_widget > QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TAB_INACTIVE)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border: none;
                padding: 8px;
            }}
            QTreeView {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border: none;
                padding: 4px;
            }}
            QTreeView::item {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 2px;
            }}
            QTreeView::item:selected {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
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
                padding: 2px;
                width: {branch_icon_size}px;
                height: {branch_icon_size}px;
            }}
            QTreeView::branch:open:has-children:!has-siblings,
            QTreeView::branch:open:has-children:has-siblings {{
                image: url("{self._style_manager.get_icon_path("arrow-down")}");
                padding: 2px;
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
        """)
