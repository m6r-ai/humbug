"""Inline editor widget for mindspace file tree items."""

from typing import Callable

from PySide6.QtWidgets import QLineEdit, QVBoxLayout, QLabel, QWidget
from PySide6.QtCore import Signal, Qt, QTimer, QRect
from PySide6.QtGui import QFontMetrics

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


class MindspaceInlineEditor(QWidget):
    """Custom inline editor with validation for file/folder names."""

    edit_finished = Signal(str)  # Emits the new name when editing is confirmed
    edit_cancelled = Signal()    # Emits when editing is cancelled

    def __init__(
        self,
        initial_text: str,
        is_conversation: bool = False,
        validation_callback: Callable[[str], tuple[bool, str]] | None = None
    ):
        """
        Initialize the inline editor.

        Args:
            initial_text: The initial text to show in the editor
            is_conversation: Whether this is editing a conversation file
            validation_callback: Optional callback for additional validation
        """
        super().__init__()
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._is_conversation = is_conversation
        self._validation_callback = validation_callback
        self._tree_view = None  # Will be set by delegate

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)

        # Create line edit
        self._line_edit = QLineEdit()
        self._line_edit.setText(initial_text)
        self._line_edit.selectAll()
        self._line_edit.textChanged.connect(self._validate_input)
        layout.addWidget(self._line_edit)

        # Create error label (initially hidden)
        self._error_label = QLabel()
        self._error_label.setWordWrap(True)
        self._error_label.hide()
        layout.addWidget(self._error_label)

        # Track validation state
        self._is_valid = True
        self._error_above = False  # Track if error is positioned above

        # Apply initial styling
        self._apply_styling()

        # Perform initial validation
        self._validate_input()

        # Install event filter to handle key events
        self._line_edit.installEventFilter(self)

    def set_tree_view(self, tree_view) -> None:
        """
        Set reference to tree view for viewport calculations.

        Args:
            tree_view: The tree view widget containing this editor
        """
        self._tree_view = tree_view

    def _calculate_required_height(self) -> int:
        """
        Calculate the total height needed for the editor and error message.

        Returns:
            Required height in pixels
        """
        line_edit_height = self._line_edit.sizeHint().height()

        if self._error_label.isVisible():
            # Calculate error label height based on text content
            font_metrics = QFontMetrics(self._error_label.font())
            text = self._error_label.text()

            # Get the width available for the error label
            available_width = max(200, self.width() - 8)  # Account for padding

            # Calculate required height for word-wrapped text
            text_rect = font_metrics.boundingRect(
                0, 0, available_width, 0,
                Qt.TextFlag.TextWordWrap, text
            )
            error_height = text_rect.height() + 4  # Add padding

            layout_spacing = self.layout().spacing()
            return line_edit_height + error_height + layout_spacing

        return line_edit_height

    def _adjust_widget_size(self) -> None:
        """Adjust the widget size and position to accommodate error message."""
        if not self.parent():
            return

        required_height = self._calculate_required_height()
        current_rect = self.geometry()

        # Get parent viewport bounds
        viewport = self.parent()
        viewport_rect = viewport.rect()

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

        self.setGeometry(new_rect)

        # Ensure the editor stays visible in viewport
        self._ensure_editor_visible()

    def _set_error_above_layout(self) -> None:
        """Rearrange layout to show error above line edit."""
        layout = self.layout()
        layout.removeWidget(self._error_label)
        layout.removeWidget(self._line_edit)
        layout.addWidget(self._error_label)
        layout.addWidget(self._line_edit)

    def _set_error_below_layout(self) -> None:
        """Rearrange layout to show error below line edit."""
        layout = self.layout()
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

        # Check if editor extends beyond viewport
        if editor_rect.bottom() > viewport_rect.bottom():
            # Calculate how much we need to scroll
            scroll_delta = editor_rect.bottom() - viewport_rect.bottom()

            # Get current scroll position and adjust
            scroll_bar = self._tree_view.verticalScrollBar()
            if scroll_bar:
                current_value = scroll_bar.value()
                new_value = min(scroll_bar.maximum(), current_value + scroll_delta + 5)  # +5 for padding
                scroll_bar.setValue(new_value)

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

        # For conversations, ensure .conv extension
        if self._is_conversation and not text.endswith('.conv'):
            text_with_ext = text + '.conv'
            self._line_edit.setText(text_with_ext)
            # Re-validate with the extension
            if any(c in invalid_chars for c in text_with_ext):
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
            self._error_label.show()

        self._apply_styling()

        # Adjust widget size to accommodate error message
        # Use QTimer to ensure layout has been updated
        QTimer.singleShot(0, self._adjust_widget_size)

    def _apply_styling(self) -> None:
        """Apply styling based on validation state."""
        if self._is_valid:
            # Normal styling
            line_edit_style = f"""
                QLineEdit {{
                    background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                    color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                    padding: 2px;
                    border-radius: 2px;
                }}
                QLineEdit:focus {{
                    border: 1px solid {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
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
                    border-radius: 2px;
                }}
                QLineEdit:focus {{
                    border: 1px solid {error_border};
                }}
            """

        # Enhanced error label styling with better visibility
        error_label_style = f"""
            QLabel {{
                color: #f44336;
                font-size: 11px;
                padding: 2px 4px;
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                border: 1px solid #f44336;
                border-radius: 2px;
                margin: 0px;
            }}
        """

        self._line_edit.setStyleSheet(line_edit_style)
        self._error_label.setStyleSheet(error_label_style)
        self._error_label.setAlignment(Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop)

    def eventFilter(self, obj, event) -> bool:
        """Handle key events for the line edit."""
        if obj == self._line_edit and event.type() == event.Type.KeyPress:
            key_event = event

            if key_event.key() == Qt.Key.Key_Return or key_event.key() == Qt.Key.Key_Enter:
                # Only accept if validation passes
                if self._is_valid:
                    self.edit_finished.emit(self._line_edit.text().strip())

                return True

            elif key_event.key() == Qt.Key.Key_Escape:
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
        self._line_edit.selectAll()
