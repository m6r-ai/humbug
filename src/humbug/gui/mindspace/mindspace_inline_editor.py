"""Inline editor widget for mindspace file tree items."""

from typing import Callable, cast

from PySide6.QtWidgets import QLineEdit, QVBoxLayout, QLabel, QWidget
from PySide6.QtCore import Signal, Qt, QTimer, QRect, QObject, QEvent
from PySide6.QtGui import QFontMetrics, QKeyEvent, QFont

from humbug.gui.color_role import ColorRole
from humbug.gui.mindspace.mindspace_file_tree_view import MindspaceFileTreeView
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceInlineEditor(QWidget):
    """Custom inline editor with validation for file/folder names."""

    edit_finished = Signal(str)  # Emits the new name when editing is confirmed
    edit_cancelled = Signal()    # Emits when editing is cancelled

    def __init__(
        self,
        initial_text: str,
        validation_callback: Callable[[str], tuple[bool, str]] | None = None
    ):
        """
        Initialize the inline editor.

        Args:
            initial_text: The initial text to show in the editor
            validation_callback: Optional callback for additional validation
        """
        super().__init__()
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._validation_callback = validation_callback
        self._tree_view: MindspaceFileTreeView | None = None  # Will be set by delegate

        # Create layout
        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(2)

        # Create line edit
        self._line_edit = QLineEdit()
        self._line_edit.setText(initial_text)
        self._line_edit.selectAll()
        self._line_edit.textChanged.connect(self._validate_input)
        self._layout.addWidget(self._line_edit)

        # Create error label (initially hidden)
        self._error_label = QLabel()
        self._error_label.setWordWrap(True)
        self._error_label.hide()
        self._layout.addWidget(self._error_label)

        # Track validation state
        self._is_valid = True
        self._error_above = False  # Track if error is positioned above

        # Connect to style changes for zoom updates
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Apply initial styling with zoom
        self._apply_styling()

        # Perform initial validation
        self._validate_input()

        # Install event filter to handle key events
        self._line_edit.installEventFilter(self)

    def set_tree_view(self, tree_view: MindspaceFileTreeView) -> None:
        """
        Set reference to tree view for viewport calculations.

        Args:
            tree_view: The tree view widget containing this editor
        """
        self._tree_view = tree_view

    def _handle_style_changed(self) -> None:
        """Handle style/zoom changes by updating fonts and sizes."""
        self._apply_styling()
        # Recalculate geometry if editor is visible
        if self.isVisible():
            QTimer.singleShot(0, self._adjust_widget_size)

    def _get_scaled_font(self) -> QFont:
        """Get appropriately scaled font for current zoom level."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        font = QFont()
        font.setPointSizeF(base_font_size * zoom_factor)
        return font

    def _get_scaled_error_font(self) -> QFont:
        """Get appropriately scaled font for error label."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        font = QFont()
        # Error font is slightly smaller than base font
        font.setPointSizeF((base_font_size - 2) * zoom_factor)
        return font

    def _calculate_required_height(self) -> int:
        """
        Calculate the total height needed for the editor and error message.

        Returns:
            Required height in pixels
        """
        # Use actual size hints which now include zoom scaling
        line_edit_height = self._line_edit.sizeHint().height()

        if self._error_label.isVisible():
            # Font metrics now use scaled font
            font_metrics = QFontMetrics(self._error_label.font())
            text = self._error_label.text()

            # Scale available width
            zoom_factor = self._style_manager.zoom_factor()
            available_width = max(int(150 * zoom_factor), self.width() - int(8 * zoom_factor))

            # Calculate required height for word-wrapped text
            text_rect = font_metrics.boundingRect(
                0, 0, available_width, 0,
                Qt.TextFlag.TextWordWrap, text
            )
            error_height = text_rect.height() + int(6 * zoom_factor)  # Add scaled padding

            layout_spacing = self._layout.spacing()
            return line_edit_height + error_height + layout_spacing

        return line_edit_height

    def _adjust_widget_size(self) -> None:
        """Adjust the widget size and position to accommodate error message."""
        if not self.parent():
            return

        required_height = self._calculate_required_height()
        current_rect = self.geometry()

        # Get parent viewport bounds
        parent = cast(QWidget, self.parent())
        viewport_rect = parent.rect()

        # Calculate new geometry
        new_rect = QRect(current_rect)
        new_rect.setHeight(required_height)

        # Determine if error should be above or below
        error_below_would_fit = new_rect.bottom() <= viewport_rect.bottom()

        if not error_below_would_fit and current_rect.top() > viewport_rect.top():
            # Position error message above the line edit
            new_rect.moveBottom(current_rect.bottom())
            if not self._error_above:
                self._set_error_above_layout()
                self._error_above = True

        else:
            # Position error message below the line edit (default)
            # Keep the top position the same
            if self._error_above:
                self._set_error_below_layout()
                self._error_above = False

        # Ensure the editor doesn't extend beyond the viewport horizontally
        # This is especially important for text-only editing where width might be constrained
        if new_rect.right() > viewport_rect.right():
            # Adjust width to fit within viewport
            zoom_factor = self._style_manager.zoom_factor()
            min_width = int(100 * zoom_factor)  # Scale minimum width
            new_width = viewport_rect.right() - new_rect.left()
            new_rect.setWidth(max(min_width, new_width))

        self.setGeometry(new_rect)

        # Ensure the editor stays visible in viewport
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
        padding = int(5 * zoom_factor)

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

        # Check if editor extends beyond viewport horizontally (less common but possible)
        if editor_rect.right() > viewport_rect.right():
            # Get horizontal scroll bar and adjust if available
            h_scroll_bar = self._tree_view.horizontalScrollBar()
            if h_scroll_bar and h_scroll_bar.isVisible():
                scroll_delta = editor_rect.right() - viewport_rect.right()
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

        # Check for invalid file system characters
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
        self._is_valid = is_valid

        if is_valid:
            self._error_label.hide()
        else:
            self._error_label.setText(error_message)
            # Ensure error label has scaled font
            self._error_label.setFont(self._get_scaled_error_font())
            self._error_label.show()

        self._apply_styling()

        # Adjust widget size to accommodate error message
        # Use QTimer to ensure layout has been updated
        QTimer.singleShot(0, self._adjust_widget_size)

    def _apply_styling(self) -> None:
        """Apply styling based on validation state and current zoom."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Set scaled fonts
        self._line_edit.setFont(self._get_scaled_font())
        self._error_label.setFont(self._get_scaled_error_font())

        if self._is_valid:
            # Normal styling with subtle background to distinguish from tree item
            line_edit_style = f"""
                QLineEdit {{
                    background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                    padding: 2px;
                    font-size: {base_font_size * zoom_factor}pt;
                    selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                }}
                QLineEdit:focus {{
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                    background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                }}
            """

        else:
            # Error styling - red background
            error_color = "#ffebee"  # Light red background
            error_border = "#f44336"  # Red border
            line_edit_style = f"""
                QLineEdit {{
                    background-color: {error_color};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {error_border};
                    padding: 2px;
                    font-size: {base_font_size * zoom_factor}pt;
                    selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                }}
                QLineEdit:focus {{
                    border: 1px solid {error_border};
                    background-color: {error_color};
                }}
            """

        # Enhanced error label styling with better visibility and scaled dimensions
        error_padding_v = int(3 * zoom_factor)
        error_padding_h = int(6 * zoom_factor)
        error_font_size = (base_font_size - 2) * zoom_factor

        error_label_style = f"""
            QLabel {{
                color: #f44336;
                font-size: {error_font_size}pt;
                padding: {error_padding_v}px {error_padding_h}px;
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                border: 1px solid #f44336;
                margin: 0px;
            }}
        """

        self._line_edit.setStyleSheet(line_edit_style)
        self._error_label.setStyleSheet(error_label_style)
        self._error_label.setAlignment(Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """Handle key events for the line edit."""
        if watched == self._line_edit and event.type() == event.Type.KeyPress:
            key_event = cast(QKeyEvent, event)

            if key_event.key() == Qt.Key.Key_Return or key_event.key() == Qt.Key.Key_Enter:
                # Only accept if validation passes
                if self._is_valid:
                    self.edit_finished.emit(self._line_edit.text().strip())

                return True

            elif key_event.key() == Qt.Key.Key_Escape:
                self.edit_cancelled.emit()
                return True

        return super().eventFilter(watched, event)

    def get_text(self) -> str:
        """Get the current text in the editor."""
        return self._line_edit.text().strip()

    def is_valid(self) -> bool:
        """Check if the current input is valid."""
        return self._is_valid

    def focus_editor(self) -> None:
        """Set focus to the line edit."""
        self._line_edit.setFocus()
        self._line_edit.selectAll()

    def cleanup_connections(self) -> None:
        """Clean up signal connections when editor is being destroyed."""
        try:
            self._style_manager.style_changed.disconnect(self._handle_style_changed)

        except (AttributeError, TypeError):
            pass  # Connection may not exist or may already be disconnected
