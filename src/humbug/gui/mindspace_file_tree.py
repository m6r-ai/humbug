"""File tree view implementation for mindspace files."""

import os
from typing import Optional

from PySide6.QtWidgets import QFileSystemModel, QWidget, QVBoxLayout, QMenu
from PySide6.QtCore import Signal, QModelIndex, Qt, QSize

from humbug.gui.color_role import ColorRole
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

    def __init__(self, parent=None):
        """Initialize the file tree widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

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
        delete_action = menu.addAction("Delete File")

        # Show menu and handle selection
        action = menu.exec_(self._tree_view.viewport().mapToGlobal(position))
        if action == delete_action:
            self._handle_delete_file(path)

    def _handle_delete_file(self, path: str):
        """Handle request to delete a file.
        
        Args:
            path: Path to the file to delete
        """
        # Show confirmation dialog using MessageBox
        result = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            "Confirm Delete",
            f"Are you sure you want to delete {os.path.basename(path)}?\n\n"
            "Any open tab for this file will be closed without saving.",
            [MessageBoxButton.YES, MessageBoxButton.NO]
        )

        if result == MessageBoxButton.YES:
            try:
                # First emit signal so tabs can be closed
                self.file_deleted.emit(path)

                # Then delete the file
                os.remove(path)
            except OSError as e:
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    "Error",
                    f"Could not delete file: {str(e)}",
                    [MessageBoxButton.OK]
                )

    def set_mindspace(self, path: str):
        """Set the mindspace root directory."""
        if not path:
            # Clear the model when no mindspace is active
            self._filter_model.set_mindspace_root(None)
            return

        self._fs_model.setRootPath(path)
        self._filter_model.set_mindspace_root(path)

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
        self._handle_style_changed()

    def _handle_style_changed(self):
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor
        base_font_size = self._style_manager.base_font_size

        self._icon_provider.update_icons()
        self._fs_model.setIconProvider(self._icon_provider)
        self._tree_view.setIconSize(QSize(16 * zoom_factor, 16 * zoom_factor))

        # Update font size
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._tree_view.setFont(font)

        branch_icon_size = int(12 * zoom_factor)
        expand_icon = "arrow-right" if self.layoutDirection() == Qt.LeftToRight else "arrow-left"

        self.setStyleSheet(f"""
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
