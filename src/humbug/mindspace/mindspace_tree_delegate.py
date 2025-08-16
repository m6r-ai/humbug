"""Base item delegate for mindspace tree views with inline editing and drop target support."""

import os

from PySide6.QtWidgets import QStyledItemDelegate, QStyleOptionViewItem, QWidget, QAbstractItemDelegate
from PySide6.QtCore import Qt, QModelIndex, QPersistentModelIndex, Signal, QRect, QAbstractItemModel
from PySide6.QtGui import QPainter, QPen

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_tree_inline_editor import MindspaceTreeInlineEditor
from humbug.mindspace.mindspace_tree_view import MindspaceTreeView
from humbug.style_manager import StyleManager


class MindspaceTreeDelegate(QStyledItemDelegate):
    """Tree item delegate that provides visual feedback for drop targets and handles inline editing."""

    edit_finished = Signal(QModelIndex, str)  # index, new_name
    edit_cancelled = Signal()

    def __init__(self, tree_view: MindspaceTreeView, style_manager: StyleManager):
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

        self.closeEditor.connect(self._on_close_editor)

        # Track editor configuration for the next edit operation
        self._next_edit_select_extension = True

    def _on_close_editor(self, editor: QWidget, hint: QAbstractItemDelegate.EndEditHint) -> None:
        """Handle when Qt closes an editor."""
        if isinstance(editor, MindspaceTreeInlineEditor):
            if hint == QAbstractItemDelegate.EndEditHint.RevertModelCache:
                # This means the edit was cancelled (usually by Escape key)
                self.edit_cancelled.emit()

    def _to_model_index(self, index: QModelIndex | QPersistentModelIndex) -> QModelIndex:
        """
        Convert any index type to QModelIndex.

        Args:
            index: Index to convert

        Returns:
            QModelIndex instance
        """
        if isinstance(index, QModelIndex):
            return index

        # For QPersistentModelIndex, create a new QModelIndex using the model and position info
        if isinstance(index, QPersistentModelIndex) and index.isValid():
            model = index.model()
            if model:
                return model.index(index.row(), index.column(), index.parent())

        # Fallback - return invalid index
        return QModelIndex()

    def set_edit_selection_mode(self, select_extension: bool) -> None:
        """
        Configure how text should be selected for the next edit operation.

        Args:
            select_extension: Whether to select the file extension in addition to the name
        """
        self._next_edit_select_extension = select_extension

    def validate_new_name(self, index: QModelIndex, new_name: str) -> tuple[bool, str]:
        """
        Validate a new name for uniqueness.

        Args:
            index: Model index being edited
            new_name: Proposed new name

        Returns:
            Tuple of (is_valid, error_message)
        """
        try:
            # Get the file path from the tree view
            file_path = self._tree_view.get_path_from_index(index)
            if not file_path:
                return False, self._language_manager.strings().error_validation_failed

            # Get the directory
            directory = os.path.dirname(file_path)

            # Check if a file with this name already exists
            new_path = os.path.join(directory, new_name)
            if os.path.exists(new_path) and not os.path.samefile(new_path, file_path):
                return False, self._language_manager.strings().rename_error_exists

            return True, ""

        except Exception:
            return False, self._language_manager.strings().error_validation_failed

    def createEditor(
        self,
        parent: QWidget,
        _option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex
    ) -> QWidget:
        """
        Create the inline editor widget.

        Args:
            parent: Parent widget (tree view's viewport)
            option: Style options for the item
            index: Model index being edited

        Returns:
            The editor widget
        """
        # Get current text (full filename including extension)
        current_text = index.data(Qt.ItemDataRole.DisplayRole)
        if not current_text:
            current_text = ""

        # Convert index to QModelIndex for our validation callback
        model_index = self._to_model_index(index)

        # Create validation callback to check for existing files
        def validation_callback(new_name: str) -> tuple[bool, str]:
            return self.validate_new_name(model_index, new_name)

        # Create the inline editor
        editor = MindspaceTreeInlineEditor(
            initial_text=current_text,
            validation_callback=validation_callback,
            select_extension=self._next_edit_select_extension,
            parent=parent
        )

        # Connect signals
        editor.edit_finished.connect(lambda name: self._on_edit_finished(model_index, name))
        editor.edit_cancelled.connect(self._on_edit_cancelled)

        # Reset selection mode for next time
        self._next_edit_select_extension = True

        return editor

    def setEditorData(self, editor: QWidget, index: QModelIndex | QPersistentModelIndex) -> None:
        """
        Set the initial data in the editor.

        Args:
            editor: The editor widget
            index: Model index being edited
        """
        if isinstance(editor, MindspaceTreeInlineEditor):
            current_text = index.data(Qt.ItemDataRole.DisplayRole)
            if current_text:
                editor.set_text(current_text)

    def updateEditorGeometry(
        self,
        editor: QWidget,
        option: QStyleOptionViewItem,
        _index: QModelIndex | QPersistentModelIndex
    ) -> None:
        """
        Update the editor geometry. Qt calls this automatically during scrolling.

        Args:
            editor: The editor widget
            option: Style options for the item
            index: Model index being edited
        """
        if isinstance(editor, MindspaceTreeInlineEditor):
            # Calculate text-only rectangle (excluding icon)
            text_rect = self._calculate_text_rect_from_option(option)
            editor.setGeometry(text_rect)

    def setModelData(self, editor: QWidget, _model: QAbstractItemModel, index: QModelIndex | QPersistentModelIndex) -> None:
        """
        Transfer data from editor to model. This is where we handle the actual file operations.

        Args:
            editor: The editor widget
            model: The model
            index: Model index being edited
        """
        if isinstance(editor, MindspaceTreeInlineEditor):
            if editor.is_valid():
                new_name = editor.get_text()

                # Convert index to QModelIndex for our signal
                model_index = self._to_model_index(index)

                # Emit our custom signal instead of modifying the model directly
                # The view will handle the actual file operations
                self.edit_finished.emit(model_index, new_name)

    def _calculate_text_rect_from_option(self, option: QStyleOptionViewItem) -> QRect:
        """
        Calculate the rectangle that contains only the text portion of the item from style option.

        Args:
            option: Style options for the item

        Returns:
            QRect covering only the text area (excluding icon)
        """
        # Get the full rect from the option - rect is a valid property but PySide6 does not expose it correctly
        full_rect = option.rect  # type: ignore

        # Get the icon size (already scaled by zoom factor)
        icon_size = self._tree_view.iconSize()
        icon_width = icon_size.width()

        # Standard spacing between icon and text in Qt tree views (scale with zoom)
        zoom_factor = self._style_manager.zoom_factor()
        icon_text_spacing = round(10 * zoom_factor)

        # Calculate the offset needed to skip over the icon
        icon_offset = icon_width + icon_text_spacing

        # Create text-only rectangle by adjusting the left edge
        text_rect = QRect(
            full_rect.left() + icon_offset,
            full_rect.top(),
            full_rect.width() - icon_offset,
            full_rect.height()
        )

        # Ensure minimum width for editing (scaled)
        min_width = round(50 * zoom_factor)
        if text_rect.width() < min_width:
            text_rect.setWidth(min_width)

        return text_rect

    def _on_edit_finished(self, index: QModelIndex, new_name: str) -> None:
        """
        Handle when editing is finished.

        Args:
            index: Model index that was edited
            new_name: The new name entered by the user
        """
        self.edit_finished.emit(index, new_name)

    def _on_edit_cancelled(self) -> None:
        """
        Handle when editing is cancelled.
        """
        self.edit_cancelled.emit()

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

            rect = option.rect  # type: ignore

            # Draw drop target background
            painter.fillRect(rect, drop_target_bg)

            # Draw drop target border - solid line for clear indication
            painter.setPen(QPen(drop_target_border, 1, Qt.PenStyle.SolidLine))
            painter.drawRect(rect.adjusted(1, 1, -1, -1))

            # Restore painter state
            painter.restore()

        # Call the parent paint method to draw the normal item content
        super().paint(painter, option, index)

    def start_editing(self, index: QModelIndex, select_extension: bool = True) -> None:
        """
        Start editing a specific file path.

        Args:
            index: Model index to edit
            select_extension: Whether to select the file extension in addition to the name
        """
        # Configure selection mode for the next edit
        self.set_edit_selection_mode(select_extension)
        self._tree_view.edit(index)

    def is_editing(self, index: QModelIndex) -> bool:
        """
        Check if the given index is currently being edited.

        Args:
            index: Model index to check

        Returns:
            True if the index is being edited
        """
        # Check if the tree view has an active editor for this index
        current_editor = self._tree_view.indexWidget(index)
        return isinstance(current_editor, MindspaceTreeInlineEditor)
