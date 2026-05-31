"""Base item delegate for sidebar tree views with inline editing and drop target support."""

import os

from PySide6.QtWidgets import QStyledItemDelegate, QStyleOptionViewItem, QWidget, QAbstractItemDelegate
from PySide6.QtCore import Qt, QModelIndex, QPersistentModelIndex, Signal, QRect, QSize, QAbstractItemModel
from PySide6.QtGui import QPainter, QPen

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.sidebar.sidebar_tree_inline_editor import SidebarTreeInlineEditor
from desktop.sidebar.sidebar_tree_view import SidebarTreeView
from desktop.style_manager import StyleManager


class SidebarTreeDelegate(QStyledItemDelegate):
    """Tree item delegate that provides visual feedback for drop targets and handles inline editing."""

    edit_finished = Signal(QModelIndex, str)
    edit_cancelled = Signal()

    def __init__(self, tree_view: SidebarTreeView, style_manager: StyleManager):
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
        self._icon_provider = SidebarTreeIconProvider()

        self.closeEditor.connect(self._on_close_editor)

        self._next_edit_select_extension = True

    def update_icons(self) -> None:
        """Update icons when theme or zoom changes."""
        self._icon_provider.update_icons()

    def initStyleOption(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex
    ) -> None:
        """Set the expanded directory icon before Qt paints the item."""
        super().initStyleOption(option, index)

        model_index = self._to_model_index(index)
        if not model_index.isValid():
            return

        path = self._tree_view.get_path_from_index(model_index)
        if path and os.path.isdir(path) and self._tree_view.isExpanded(model_index):
            option.icon = self._icon_provider.open_folder_icon()

    def _on_close_editor(self, editor: QWidget, hint: QAbstractItemDelegate.EndEditHint) -> None:
        """Handle when Qt closes an editor."""
        if isinstance(editor, SidebarTreeInlineEditor):
            if hint == QAbstractItemDelegate.EndEditHint.RevertModelCache:
                self.edit_cancelled.emit()

    def _to_model_index(self, index: QModelIndex | QPersistentModelIndex) -> QModelIndex:
        """Convert any index type to QModelIndex."""
        if isinstance(index, QModelIndex):
            return index

        if isinstance(index, QPersistentModelIndex) and index.isValid():
            model = index.model()
            if model:
                return model.index(index.row(), index.column(), index.parent())

        return QModelIndex()

    def set_edit_selection_mode(self, select_extension: bool) -> None:
        """Configure how text should be selected for the next edit operation."""
        self._next_edit_select_extension = select_extension

    def validate_new_name(self, index: QModelIndex, new_name: str) -> tuple[bool, str]:
        """Validate a new name for uniqueness."""
        try:
            file_path = self._tree_view.get_path_from_index(index)
            if not file_path:
                return False, self._language_manager.strings().error_validation_failed

            directory = os.path.dirname(file_path)
            new_path = os.path.join(directory, new_name)
            if os.path.exists(new_path) and not os.path.samefile(new_path, file_path):
                return False, self._language_manager.strings().rename_error_exists

            return True, ""

        except Exception:
            return False, self._language_manager.strings().error_validation_failed

    def createEditor(
        self,
        parent: QWidget | None,
        _option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex
    ) -> QWidget:
        """Create the inline editor widget."""
        current_text = index.data(Qt.ItemDataRole.DisplayRole)
        if not current_text:
            current_text = ""

        model_index = self._to_model_index(index)

        def validation_callback(new_name: str) -> tuple[bool, str]:
            return self.validate_new_name(model_index, new_name)

        editor = SidebarTreeInlineEditor(
            initial_text=current_text,
            validation_callback=validation_callback,
            select_extension=self._next_edit_select_extension,
            parent=parent
        )

        editor.edit_finished.connect(lambda name: self._on_edit_finished(model_index, name))
        editor.edit_cancelled.connect(self._on_edit_cancelled)

        self._next_edit_select_extension = True

        return editor

    def setEditorData(self, editor: QWidget, index: QModelIndex | QPersistentModelIndex) -> None:
        """Set the initial data in the editor."""
        if isinstance(editor, SidebarTreeInlineEditor):
            current_text = index.data(Qt.ItemDataRole.DisplayRole)
            if current_text:
                editor.set_text(current_text)

    def updateEditorGeometry(
        self,
        editor: QWidget,
        option: QStyleOptionViewItem,
        _index: QModelIndex | QPersistentModelIndex
    ) -> None:
        """Update the editor geometry."""
        if isinstance(editor, SidebarTreeInlineEditor):
            text_rect = self._calculate_text_rect_from_option(option)
            editor.setGeometry(text_rect)

    def setModelData(self, editor: QWidget, _model: QAbstractItemModel, index: QModelIndex | QPersistentModelIndex) -> None:
        """Transfer data from editor to model."""
        if isinstance(editor, SidebarTreeInlineEditor):
            if editor.is_valid():
                new_name = editor.get_text()
                model_index = self._to_model_index(index)
                self.edit_finished.emit(model_index, new_name)

    def _calculate_text_rect_from_option(self, option: QStyleOptionViewItem) -> QRect:
        """Calculate the rectangle that contains only the text portion of the item."""
        full_rect = option.rect  # type: ignore

        icon_size = self._tree_view.iconSize()
        icon_width = icon_size.width()

        zoom_factor = self._style_manager.zoom_factor()
        icon_text_spacing = round(10 * zoom_factor)

        icon_offset = icon_width + icon_text_spacing

        text_rect = QRect(
            full_rect.left() + icon_offset,
            full_rect.top(),
            full_rect.width() - icon_offset,
            full_rect.height()
        )

        min_width = round(50 * zoom_factor)
        if text_rect.width() < min_width:
            text_rect.setWidth(min_width)

        return text_rect

    def _on_edit_finished(self, index: QModelIndex, new_name: str) -> None:
        """Handle when editing is finished."""
        self.edit_finished.emit(index, new_name)

    def _on_edit_cancelled(self) -> None:
        """Handle when editing is cancelled."""
        self.edit_cancelled.emit()

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        """Return a consistent, zoom-scaled row height on all platforms."""
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        line_height = fm.height()
        row_height = max(line_height + round(10 * zoom), round(24 * zoom))
        return QSize(super().sizeHint(option, index).width(), row_height)

    def paint(self, painter: QPainter, option: QStyleOptionViewItem, index: QModelIndex | QPersistentModelIndex) -> None:
        """Paint the item with drop target highlighting if applicable."""
        current_drop_target = self._tree_view.get_current_drop_target()
        is_drop_target = current_drop_target is not None and current_drop_target == index

        if is_drop_target:
            painter.save()

            drop_target_bg = self._style_manager.get_color(ColorRole.BUTTON_BACKGROUND_HOVER)
            drop_target_border = self._style_manager.get_color(ColorRole.TEXT_SELECTED)

            drop_target_bg.setAlpha(128)

            rect = option.rect  # type: ignore

            painter.fillRect(rect, drop_target_bg)

            painter.setPen(QPen(drop_target_border, 1, Qt.PenStyle.SolidLine))
            painter.drawRect(rect.adjusted(1, 1, -1, -1))

            painter.restore()

        super().paint(painter, option, index)

    def start_editing(self, index: QModelIndex, select_extension: bool = True) -> None:
        """Start editing a specific file path."""
        self.set_edit_selection_mode(select_extension)
        self._tree_view.edit(index)

    def is_editing(self, index: QModelIndex) -> bool:
        """Check if the given index is currently being edited."""
        current_editor = self._tree_view.indexWidget(index)
        return isinstance(current_editor, SidebarTreeInlineEditor)
