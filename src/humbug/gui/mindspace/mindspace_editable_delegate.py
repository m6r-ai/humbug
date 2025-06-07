"""Custom item delegate for mindspace file tree with inline editing and drop target support."""

import os
from typing import cast

from PySide6.QtWidgets import QStyledItemDelegate, QStyleOptionViewItem
from PySide6.QtCore import Qt, QModelIndex, QPersistentModelIndex, Signal, QRect
from PySide6.QtGui import QPainter, QPen

from humbug.gui.color_role import ColorRole
from humbug.gui.mindspace.mindspace_inline_editor import MindspaceInlineEditor
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceEditableDelegate(QStyledItemDelegate):
    """Custom item delegate that provides visual feedback for drop targets and handles inline editing."""

    edit_finished = Signal(QModelIndex, str)  # index, new_name
    edit_cancelled = Signal()

    def __init__(self, tree_view, style_manager: StyleManager):
        """
        Initialize the delegate.

        Args:
            tree_view: The tree view this delegate is attached to
            style_manager: Style manager for accessing theme colors
        """
        super().__init__()
        self._tree_view = tree_view
        self._style_manager = style_manager
        self._language_manager = LanguageManager()
        self._current_editor: MindspaceInlineEditor | None = None
        self._editing_index: QModelIndex | None = None

    def start_custom_edit(self, index: QModelIndex, tree_view) -> None:
        """
        Start custom inline editing that bypasses the model's editing system.

        Args:
            index: Model index to edit
            tree_view: The tree view widget
        """
        if not index.isValid() or self._current_editor is not None:
            return

        # Get current text and determine if it's a conversation
        current_text = index.data(Qt.ItemDataRole.DisplayRole)
        if not current_text:
            current_text = ""

        # Check if this is a conversation file
        is_conversation = current_text.lower().endswith('.conv')

        # For conversations, remove the .conv extension for editing
        if is_conversation:
            current_text = os.path.splitext(current_text)[0]

        # Create validation callback to check for existing files
        def validation_callback(new_name: str) -> tuple[bool, str]:
            return self._validate_new_name(index, new_name, is_conversation)

        # Create the inline editor
        editor = MindspaceInlineEditor(
            initial_text=current_text,
            is_conversation=is_conversation,
            validation_callback=validation_callback
        )

        # Connect signals
        editor.edit_finished.connect(lambda name: self._handle_edit_finished(index, name))
        editor.edit_cancelled.connect(self._handle_edit_cancelled)

        # Track the current editor
        self._current_editor = editor
        self._editing_index = index

        # Set up the editor geometry and parent
        editor.setParent(tree_view.viewport())

        # Set reference to tree view for viewport calculations
        editor.set_tree_view(tree_view)

        # Get the visual rect for the index
        rect = tree_view.visualRect(index)

        # Set initial geometry (will be adjusted when error shows)
        editor.setGeometry(rect)

        # Show and focus the editor
        editor.show()
        editor.focus_editor()

        # Install event filter to handle clicks outside the editor
        editor.installEventFilter(self)
        tree_view.viewport().installEventFilter(self)

    def eventFilter(self, obj, event) -> bool:
        """Handle events to close editor when clicking outside."""
        if self._current_editor and event.type() == event.Type.MouseButtonPress:
            # Check if the click was outside the editor
            if obj != self._current_editor and not self._current_editor.isAncestorOf(obj):
                # Click outside editor - commit if valid, otherwise cancel
                if self._current_editor.is_valid():
                    self._current_editor.edit_finished.emit(self._current_editor.get_text())

                else:
                    self._current_editor.edit_cancelled.emit()

                return True

        return super().eventFilter(obj, event)

    def paint(self, painter: QPainter, option: QStyleOptionViewItem, index: QModelIndex | QPersistentModelIndex) -> None:
        """
        Paint the item with drop target highlighting if applicable.

        Args:
            painter: The painter to use for drawing
            option: Style options for the item
            index: The model index being painted
        """
        # Check if this is the current drop target
        current_drop_target = self._tree_view.get_current_drop_target()
        is_drop_target = current_drop_target is not None and current_drop_target == index

        if is_drop_target:
            # Save the painter state
            painter.save()

            # Get drop target colors from style manager
            drop_target_bg = self._style_manager.get_color(ColorRole.BUTTON_BACKGROUND_HOVER)
            drop_target_border = self._style_manager.get_color(ColorRole.TEXT_SELECTED)

            # Make the background slightly more transparent for subtlety
            drop_target_bg.setAlpha(128)

            rect = cast(QRect, getattr(option, 'rect'))

            # Draw drop target background
            painter.fillRect(rect, drop_target_bg)

            # Draw drop target border - solid line for clear indication
            painter.setPen(QPen(drop_target_border, 1, Qt.PenStyle.SolidLine))
            painter.drawRect(rect.adjusted(1, 1, -1, -1))

            # Restore painter state
            painter.restore()

        # Call the parent paint method to draw the normal item content
        super().paint(painter, option, index)

    def _validate_new_name(self, index: QModelIndex, new_name: str, is_conversation: bool) -> tuple[bool, str]:
        """
        Validate a new name for uniqueness.

        Args:
            index: Model index being edited
            new_name: Proposed new name
            is_conversation: Whether this is a conversation file

        Returns:
            Tuple of (is_valid, error_message)
        """
        try:
            # Get the file path from the tree view
            file_path = self._tree_view._get_path_from_index(index)
            if not file_path:
                return False, self._language_manager.strings().error_validation_failed

            # Get the directory
            directory = os.path.dirname(file_path)

            # Add extension for conversations
            full_name = new_name + '.conv' if is_conversation else new_name

            # Check if a file with this name already exists
            new_path = os.path.join(directory, full_name)
            if os.path.exists(new_path) and new_path != file_path:
                return False, self._language_manager.strings().rename_error_exists

            return True, ""

        except Exception:
            return False, self._language_manager.strings().error_validation_failed

    def _handle_edit_finished(self, index: QModelIndex, new_name: str) -> None:
        """
        Handle when editing is finished.

        Args:
            index: Model index that was edited
            new_name: The new name entered by the user
        """
        self._cleanup_editor()
        self.edit_finished.emit(index, new_name)

    def _handle_edit_cancelled(self) -> None:
        """
        Handle when editing is cancelled.
        """
        self._cleanup_editor()
        self.edit_cancelled.emit()

    def _cleanup_editor(self) -> None:
        """Clean up the current editor state."""
        if self._current_editor:
            # Remove event filters
            self._current_editor.removeEventFilter(self)
            if self._tree_view and self._tree_view.viewport():
                self._tree_view.viewport().removeEventFilter(self)

            # Hide and delete the editor
            self._current_editor.hide()
            self._current_editor.deleteLater()

        self._current_editor = None
        self._editing_index = None

    def is_editing(self, index: QModelIndex) -> bool:
        """
        Check if the given index is currently being edited.

        Args:
            index: Model index to check

        Returns:
            True if the index is being edited
        """
        return self._editing_index is not None and self._editing_index == index
