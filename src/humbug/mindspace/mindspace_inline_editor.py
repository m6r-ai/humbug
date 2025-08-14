"""Inline editor widget for mindspace file tree items."""

import os
from typing import Callable, cast

from PySide6.QtWidgets import QLineEdit, QVBoxLayout, QLabel, QWidget
from PySide6.QtCore import Signal, Qt, QRect, QObject, QEvent, QModelIndex
from PySide6.QtGui import QFontMetrics, QKeyEvent, QFont

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_files_tree_view import MindspaceFilesTreeView
from humbug.style_manager import StyleManager


class MindspaceInlineEditor(QWidget):
    """Custom inline editor with validation for file/folder names."""

    edit_finished = Signal(str)  # Emits the new name when editing is confirmed
    edit_cancelled = Signal()    # Emits when editing is cancelled

    def __init__(
        self,
        initial_text: str,
        validation_callback: Callable[[str], tuple[bool, str]] | None = None,
        select_extension: bool = True
    ):
        """
        Initialize the inline editor.

        Args:
            initial_text: The initial text to show in the editor
            validation_callback: Optional callback for additional validation
            select_extension: Whether to select the file extension in addition to the name
        """
        super().__init__()
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._validation_callback = validation_callback
        self._select_extension = select_extension
        self._tree_view: MindspaceFilesTreeView | None = None  # Will be set by delegate
        self._editing_index: QModelIndex | None = None  # Will be set by delegate

        # Create layout
        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(0)

        # Create line edit
        self._line_edit = QLineEdit()
        self._line_edit.setText(initial_text)
        self._apply_initial_selection(initial_text, select_extension)
        self._line_edit.textChanged.connect(self._validate_input)
        self._layout.addWidget(self._line_edit)

        # Create error label (initially hidden)
        self._error_label = QLabel()
        self._error_label.setWordWrap(True)
        self._error_label.hide()
        self._layout.addWidget(self._error_label)

        # Track validation state and positioning
        self._is_valid = True
        self._error_above = False  # Track if error is positioned above

        # Connect to style changes for zoom updates
        self._style_manager.style_changed.connect(self._on_style_changed)

        # Apply initial styling with zoom
        self._apply_styling()

        # Perform initial validation
        self._validate_input()

        # Install event filter to handle key events
        self._line_edit.installEventFilter(self)

    def _apply_initial_selection(self, text: str, select_extension: bool) -> None:
        """
        Apply appropriate text selection based on the selection mode.

        Args:
            text: The text to apply selection to
            select_extension: Whether to include extension in selection
        """
        if select_extension:
            # Select all text (for new files)
            self._line_edit.selectAll()
            return

        # Select only the filename part (for rename/duplicate)
        name_part, ext = os.path.splitext(text)
        if ext and name_part:  # Has extension
            self._line_edit.setSelection(0, len(name_part))
            return

        # No extension or empty name, select all
        self._line_edit.selectAll()

    def set_tree_view(self, tree_view: MindspaceFilesTreeView) -> None:
        """
        Set reference to tree view for viewport calculations.

        Args:
            tree_view: The tree view widget containing this editor
        """
        self._tree_view = tree_view

    def set_editing_index(self, index: QModelIndex) -> None:
        """
        Set the model index being edited for position calculations.

        Args:
            index: The model index of the item being edited
        """
        self._editing_index = index

    def _on_style_changed(self) -> None:
        """Handle style/zoom changes by updating fonts and sizes."""
        self.adjust_widget_size()
        self._apply_styling()

    def _get_scaled_font(self) -> QFont:
        """Get appropriately scaled font for current zoom level."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        font = QFont()
        font.setPointSizeF(base_font_size * zoom_factor)
        return font

    def _calculate_text_rect(self, index: QModelIndex, tree_view: MindspaceFilesTreeView) -> QRect:
        """
        Calculate the rectangle that contains only the text portion of the item.
        This is duplicated from the delegate to avoid circular imports.

        Args:
            index: Model index of the item
            tree_view: The tree view widget

        Returns:
            QRect covering only the text area (excluding icon)
        """
        # Get the full visual rect (this already has correct positioning)
        full_rect = tree_view.visualRect(index)

        # Get the icon size (already scaled by zoom factor)
        icon_size = tree_view.iconSize()
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

    def _get_current_line_edit_position(self) -> int:
        """
        Get the current position where the line edit should be placed.
        This recalculates based on the current tree view item position.

        Returns:
            Top position for the line edit in viewport coordinates
        """
        if not self._tree_view or not self._editing_index or not self._editing_index.isValid():
            # Fallback to current geometry if we can't calculate from tree view
            return self.geometry().top()

        # Calculate the text rect for the current index (this handles zoom changes)
        text_rect = self._calculate_text_rect(self._editing_index, self._tree_view)
        return text_rect.top()

    def _edit_width(self) -> int:
        """
        Get the width of the edit control.

        Returns:
            Width in pixels, or 0 if no parent is set
        """
        if not self._tree_view or not self._tree_view.viewport():
            return 0

        return self._tree_view.viewport().width() - self.geometry().left()

    def _calculate_required_height(self) -> int:
        """
        Calculate the total height needed for the editor and error message.

        Returns:
            Required height in pixels
        """
        # Use actual size hints which now include zoom scaling
        line_edit_height = self._line_edit.sizeHint().height()

        if self._error_label.isVisible():
            # Use the actual width that will be set
            optimal_width = self._edit_width() - 1
            zoom_factor = self._style_manager.zoom_factor()

            # Account for padding in width calculation
            available_text_width = optimal_width - 4  # Subtract padding (2px left + 2px right)

            font_metrics = QFontMetrics(self._error_label.font())
            text = self._error_label.text()

            # Calculate required height for word-wrapped text
            text_rect = font_metrics.boundingRect(
                0, 0, available_text_width, 0,
                Qt.TextFlag.TextWordWrap, text
            )
            error_height = text_rect.height() + round(8 * zoom_factor)  # Add padding

            return line_edit_height + error_height

        return line_edit_height

    def adjust_widget_size(self) -> None:
        """Adjust the widget size and position to accommodate error message."""
        if not self.parent():
            return

        current_rect = self.geometry()

        # Get the current position where the line edit should be placed
        # This recalculates based on current zoom and tree view layout
        current_line_edit_top = self._get_current_line_edit_position()

        required_height = self._calculate_required_height()

        # Get parent viewport bounds
        parent = cast(QWidget, self.parent())
        viewport_rect = parent.rect()

        # Calculate optimal width
        zoom_factor = self._style_manager.zoom_factor()
        min_width = round(64 * zoom_factor)
        optimal_width = self._edit_width() - 1
        widget_width = max(min_width, optimal_width)

        # Get line edit height and error height
        line_edit_height = self._line_edit.sizeHint().height()
        error_height = required_height - line_edit_height

        # Determine error positioning if error is visible
        if self._error_label.isVisible():
            # Check if error should go above or below based on available space
            error_below_would_fit = (current_line_edit_top + required_height) <= viewport_rect.bottom()
            error_above_would_fit = (current_line_edit_top - error_height) >= viewport_rect.top()

            # Prefer below unless it doesn't fit and above does fit
            should_put_error_above = not error_below_would_fit and error_above_would_fit

            # Update layout if error position needs to change
            if should_put_error_above != self._error_above:
                self._error_above = should_put_error_above
                if self._error_above:
                    self._set_error_above_layout()

                else:
                    self._set_error_below_layout()

        # Calculate final widget position
        if self._error_label.isVisible() and self._error_above:
            # Error above: widget top moves up so line edit stays at current position
            new_widget_top = current_line_edit_top - error_height

        else:
            # Error below or no error: widget top stays at current line edit position
            new_widget_top = current_line_edit_top

        # Set the final geometry
        new_rect = QRect(current_rect.left(), new_widget_top, widget_width, required_height)
        self.setGeometry(new_rect)
        self._ensure_editor_visible()

    def _set_error_above_layout(self) -> None:
        """Rearrange layout to show error above line edit."""
        layout = self._layout
        layout.removeWidget(self._error_label)
        layout.removeWidget(self._line_edit)
        layout.addWidget(self._error_label)
        layout.addWidget(self._line_edit)

    def _set_error_below_layout(self) -> None:
        """Rearrange layout to show error below line edit."""
        layout = self._layout
        layout.removeWidget(self._line_edit)
        layout.removeWidget(self._error_label)
        layout.addWidget(self._line_edit)
        layout.addWidget(self._error_label)

    def _ensure_editor_visible(self) -> None:
        """Ensure the entire editor (including error) is visible in viewport."""
        if not self._tree_view or not self._error_label.isVisible():
            return

        viewport = self._tree_view.viewport()
        viewport_rect = viewport.rect()
        editor_rect = self.geometry()

        # Scale padding value
        zoom_factor = self._style_manager.zoom_factor()
        padding = round(5 * zoom_factor)

        # Check if editor extends beyond viewport vertically
        if editor_rect.bottom() > viewport_rect.bottom():
            # Calculate how much we need to scroll
            scroll_delta = editor_rect.bottom() - viewport_rect.bottom()

            # Get current scroll position and adjust
            scroll_bar = self._tree_view.verticalScrollBar()
            if scroll_bar:
                current_value = scroll_bar.value()
                new_value = min(scroll_bar.maximum(), current_value + scroll_delta + padding)
                scroll_bar.setValue(new_value)

        # For horizontal scrolling, only scroll if the line edit itself is not visible
        # Don't force horizontal scrolling for error message overflow
        line_edit_rect = self._line_edit.geometry()
        line_edit_global = self.mapToParent(line_edit_rect.topLeft())
        line_edit_in_parent = QRect(line_edit_global, line_edit_rect.size())

        if line_edit_in_parent.right() > viewport_rect.right():
            # Get horizontal scroll bar and adjust if available
            h_scroll_bar = self._tree_view.horizontalScrollBar()
            if h_scroll_bar and h_scroll_bar.isVisible():
                scroll_delta = line_edit_in_parent.right() - viewport_rect.right()
                current_value = h_scroll_bar.value()
                new_value = min(h_scroll_bar.maximum(), current_value + scroll_delta + padding)
                h_scroll_bar.setValue(new_value)

    def _validate_input(self) -> None:
        """Validate the current input and update visual feedback."""
        text = self._line_edit.text().strip()

        # Check basic validation
        if not text:
            self._set_validation_state(False, self._language_manager.strings().error_empty_name)
            return

        # Check for invalid filesystem characters
        invalid_chars = r'\/:*?"<>|'
        if any(c in invalid_chars for c in text):
            self._set_validation_state(False, self._language_manager.strings().error_invalid_characters)
            return

        # Call additional validation callback if provided
        if self._validation_callback:
            is_valid, error_message = self._validation_callback(text)
            if not is_valid:
                self._set_validation_state(False, error_message)
                return

        # All validation passed
        self._set_validation_state(True, "")

    def _set_validation_state(self, is_valid: bool, error_message: str) -> None:
        """
        Set the validation state and update visual feedback.

        Args:
            is_valid: Whether the current input is valid
            error_message: Error message to display (empty if valid)
        """
        was_showing_error = not self._is_valid
        self._is_valid = is_valid

        if is_valid:
            # If we were showing an error and now we're not, we need to handle the transition
            if was_showing_error:
                self._error_label.hide()
                self._error_above = False
                self._set_error_below_layout()

            else:
                self._error_label.hide()

        else:
            self._error_label.setText(error_message)
            self._error_label.show()

        self.adjust_widget_size()
        self._apply_styling()

    def _apply_styling(self) -> None:
        """Apply styling based on validation state and current zoom."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Set scaled fonts
        self._line_edit.setFont(self._get_scaled_font())
        self._error_label.setFont(self._get_scaled_font())

        if self._is_valid:
            # Normal styling with subtle background to distinguish from tree item
            line_edit_style = f"""
                QLineEdit {{
                    background-color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER)};
                    padding: 2px;
                    font-size: {base_font_size * zoom_factor}pt;
                    selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                    selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                }}
            """

        else:
            # Error styling - red background
            line_edit_style = f"""
                QLineEdit {{
                    background-color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_ERROR)};
                    padding: 2px;
                    font-size: {base_font_size * zoom_factor}pt;
                    selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                    selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                }}
            """

        error_color = self._style_manager.get_color_str(ColorRole.EDIT_BOX_ERROR)
        error_label_style = f"""
            QLabel {{
                background-color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.EDIT_BOX_ERROR)};
                border: 1px solid {error_color};
                border-top: {"1px" if self._error_above else "0px"} solid {error_color};
                border-bottom: {"0px" if self._error_above else "1px"} solid {error_color};
                padding: {"2px" if self._error_above else "1px"} 2px 2px 2px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
        """

        self._line_edit.setStyleSheet(line_edit_style)
        self._error_label.setStyleSheet(error_label_style)
        self._error_label.setAlignment(Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop)

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """Handle key events for the line edit."""
        if obj == self._line_edit and event.type() == event.Type.KeyPress:
            key_event = cast(QKeyEvent, event)

            if key_event.key() == Qt.Key.Key_Return or key_event.key() == Qt.Key.Key_Enter:
                # Only accept if validation passes
                if self._is_valid:
                    self.edit_finished.emit(self._line_edit.text().strip())

                return True

            if key_event.key() == Qt.Key.Key_Escape:
                self.edit_cancelled.emit()
                return True

        return super().eventFilter(obj, event)

    def get_text(self) -> str:
        """Get the current text in the editor."""
        return self._line_edit.text().strip()

    def is_valid(self) -> bool:
        """Check if the current input is valid."""
        return self._is_valid

    def focus_editor(self) -> None:
        """Set focus to the line edit."""
        self._line_edit.setFocus()
        # Don't call selectAll() here as selection was already applied in constructor

    def cleanup_connections(self) -> None:
        """Clean up signal connections when editor is being destroyed."""
        try:
            self._style_manager.style_changed.disconnect(self._on_style_changed)

        except (AttributeError, TypeError):
            pass  # Connection may not exist or may already be disconnected
