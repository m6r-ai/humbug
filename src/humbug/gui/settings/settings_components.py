"""
Settings components for creating consistent dialog interfaces.

This module provides reusable components for building settings dialogs with
consistent layout, styling, and behavior throughout the application.
"""

from enum import Enum, auto
from typing import List, Optional, Any, Tuple

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLabel, QCheckBox,
    QComboBox, QSpinBox, QDoubleSpinBox, QLineEdit, QListView
)
from PySide6.QtCore import Qt, Signal

from humbug.gui.style_manager import StyleManager, ColorRole


class SettingType(Enum):
    """Enum for different types of settings components."""
    HEADER = auto()         # Header for grouping settings
    SECTION = auto()        # Section header
    CHECKBOX = auto()       # Boolean setting with checkbox
    COMBO = auto()          # Selection from dropdown
    SPIN = auto()           # Integer spinner
    DOUBLE_SPIN = auto()    # Float spinner
    TEXT = auto()           # Text input
    DISPLAY = auto()        # Read-only display field


class SettingsItem(QWidget):
    """
    Base class for all settings components with consistent styling.

    This provides a common foundation for all settings widgets to ensure
    consistent styling, spacing, and behavior.

    Attributes:
        value_changed (Signal): Emitted when the value of this setting changes
        _style_manager (StyleManager): Reference to the style manager singleton
    """

    value_changed = Signal()

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        """Initialize the settings item with consistent styling."""
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._update_styling)

    def _update_styling(self) -> None:
        """Update styling when application style changes."""

    def is_modified(self) -> bool:
        """Check if this setting has been modified from its initial value."""
        return False

    def reset_modified_state(self) -> None:
        """Reset the modified state after applying changes."""

    def get_value(self) -> Any:
        """Get the current value of this setting."""
        return None

    def set_value(self, value: Any) -> None:
        """Set the current value of this setting."""


class SettingsSpacer(SettingsItem):
    """A simple spacer widget that implements the SettingsItem interface."""

    def __init__(self, height: int = 16, parent: QWidget | None = None) -> None:
        """Initialize a spacer with the specified height."""
        super().__init__(parent)
        self.setFixedHeight(height)


class SettingsHeader(SettingsItem):
    """
    Header for grouping related sections.

    Attributes:
        _label (QLabel): The header title label
    """

    def __init__(self, title: str, parent: Optional[QWidget] = None) -> None:
        """
        Initialize a section header with the specified title.

        Args:
            title: Text to display in the header
            parent: Parent widget
        """
        super().__init__(parent)

        layout = QVBoxLayout()
        layout.setContentsMargins(0, 8, 0, 8)

        self._label = QLabel(title)
        layout.addWidget(self._label)

        self.setLayout(layout)
        self._update_styling()

    def _update_styling(self) -> None:
        """Update section header styling."""
        font_size = self._style_manager.base_font_size()
        zoom_factor = self._style_manager.zoom_factor()
        scaled_font_size = int(font_size * zoom_factor * 2.0)  # 100% larger than base

        color = self._style_manager.get_color_str(ColorRole.TEXT_HEADING)
        self._label.setStyleSheet(f"""
            QLabel {{
                font-size: {scaled_font_size}pt;
                font-weight: bold;
                color: {color};
            }}
        """)

    def set_label(self, text: str) -> None:
        """Set the header label text."""
        self._label.setText(text)


class SettingsSection(SettingsItem):
    """
    Section header for grouping related settings.

    Attributes:
        _label (QLabel): The section title label
    """

    def __init__(self, title: str, parent: Optional[QWidget] = None) -> None:
        """
        Initialize a section header with the specified title.

        Args:
            title: Text to display in the section header
            parent: Parent widget
        """
        super().__init__(parent)

        layout = QVBoxLayout()
        layout.setContentsMargins(0, 8, 0, 8)

        self._label = QLabel(title)
        layout.addWidget(self._label)

        self.setLayout(layout)
        self._update_styling()

    def _update_styling(self) -> None:
        """Update section header styling."""
        font_size = self._style_manager.base_font_size()
        zoom_factor = self._style_manager.zoom_factor()
        scaled_font_size = int(font_size * zoom_factor)

        color = self._style_manager.get_color_str(ColorRole.TEXT_BRIGHT)
        self._label.setStyleSheet(f"""
            QLabel {{
                font-size: {scaled_font_size}pt;
                font-weight: bold;
                color: {color};
            }}
        """)


class SettingsCheckbox(SettingsItem):
    """
    Checkbox setting with label for boolean options.

    Attributes:
        _checkbox (QCheckBox): The checkbox control
        _label (QLabel): The description label
        _initial_value (bool): The initial value for detecting changes
    """

    def __init__(self, text: str, parent: Optional[QWidget] = None) -> None:
        """
        Initialize a checkbox setting.

        Args:
            text: Label text for the checkbox
            parent: Parent widget
        """
        super().__init__(parent)

        layout = QHBoxLayout()
        layout.setContentsMargins(0, 8, 0, 8)

        self._checkbox = QCheckBox()
        self._checkbox.stateChanged.connect(self._handle_changed)

        self._label = QLabel(text)

        layout.addWidget(self._checkbox)
        layout.addWidget(self._label)
        layout.addStretch()

        self.setLayout(layout)
        self._initial_value = False
        self._update_styling()

    def _handle_changed(self) -> None:
        """Handle checkbox state changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if checkbox state has changed."""
        return self._checkbox.isChecked() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._checkbox.isChecked()

    def get_value(self) -> bool:
        """Get the current checkbox state."""
        return self._checkbox.isChecked()

    def set_value(self, value: bool) -> None:
        """Set the checkbox state."""
        self._checkbox.setChecked(value)
        self._initial_value = value


class SettingsField(SettingsItem):
    """
    Base class for settings that have a label and control.

    Attributes:
        _label (QLabel): The setting label
        _control (QWidget): The interactive control widget
        _layout (QVBoxLayout): Layout for the setting
    """

    def __init__(self, label_text: str, parent: Optional[QWidget] = None) -> None:
        """
        Initialize a field setting with label and control.

        Args:
            label_text: Text for the setting label
            parent: Parent widget
        """
        super().__init__(parent)

        self._layout = QVBoxLayout()
        self._layout.setContentsMargins(0, 4, 0, 4)

        self._label = QLabel(label_text)
        self._layout.addWidget(self._label)

        self.setLayout(self._layout)


class SettingsCombo(SettingsField):
    """
    Combo box setting for selecting from a list of options.

    Attributes:
        _combo (QComboBox): The combo box control
        _initial_index (int): The initial selected index
        _items (List[tuple]): The items and their data values
    """

    def __init__(self,
                 label_text: str,
                 items: List[Tuple[str, Any]] = None,
                 parent: Optional[QWidget] = None) -> None:
        """
        Initialize a combo box setting.

        Args:
            label_text: Text for the setting label
            items: List of (display_text, data_value) tuples for combo items
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._combo = QComboBox()
        self._combo.setView(QListView())  # For better styling
        self._combo.currentIndexChanged.connect(self._handle_changed)

        # Add items if provided
        self._items = items or []
        if items:
            for display_text, data_value in items:
                self._combo.addItem(display_text, data_value)

        # Add to layout
        self._layout.addWidget(self._combo)
        self._initial_index = self._combo.currentIndex()
        self._update_styling()

    def _handle_changed(self) -> None:
        """Handle combo box selection changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if combo selection has changed."""
        return self._combo.currentIndex() != self._initial_index

    def reset_modified_state(self) -> None:
        """Reset the initial index to current index."""
        self._initial_index = self._combo.currentIndex()

    def get_value(self) -> Any:
        """Get the current selected item's data."""
        return self._combo.currentData()

    def get_text(self) -> str:
        """Get the current selected item's text."""
        return self._combo.currentText()

    def set_value(self, value: Any) -> None:
        """Set the combo box selection by data value."""
        index = self._combo.findData(value)
        if index >= 0:
            self._combo.setCurrentIndex(index)
            self._initial_index = index

    def set_items(self, items: List[Tuple[str, Any]]) -> None:
        """
        Update the items in the combo box.

        Args:
            items: List of (display_text, data_value) tuples
        """
        current_data = self._combo.currentData()

        self._combo.blockSignals(True)
        self._combo.clear()

        for display_text, data_value in items:
            self._combo.addItem(display_text, data_value)

        # Try to restore previous selection
        index = self._combo.findData(current_data)
        if index >= 0:
            self._combo.setCurrentIndex(index)

        self._combo.blockSignals(False)
        self._initial_index = self._combo.currentIndex()

    def _update_styling(self) -> None:
        """Update combo box styling."""
        super()._update_styling()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._combo.setMinimumHeight(min_height)

    def setEnabled(self, enabled: bool) -> None:
        """Enable or disable the combo box."""
        self._combo.setEnabled(enabled)


class SettingsSpinBox(SettingsField):
    """
    Spin box setting for integer values.

    Attributes:
        _spin (QSpinBox): The spin box control
        _initial_value (int): The initial value
    """

    def __init__(self,
                 label_text: str,
                 min_value: int = 0,
                 max_value: int = 100,
                 step: int = 1,
                 parent: Optional[QWidget] = None) -> None:
        """
        Initialize a spin box setting.

        Args:
            label_text: Text for the setting label
            min_value: Minimum allowed value
            max_value: Maximum allowed value
            step: Step size for incrementing/decrementing
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._spin = QSpinBox()
        self._spin.setRange(min_value, max_value)
        self._spin.setSingleStep(step)
        self._spin.valueChanged.connect(self._handle_changed)

        self._layout.addWidget(self._spin)
        self._initial_value = self._spin.value()
        self._update_styling()

    def _handle_changed(self) -> None:
        """Handle spin box value changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if spin box value has changed."""
        return self._spin.value() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._spin.value()

    def get_value(self) -> int:
        """Get the current spin box value."""
        return self._spin.value()

    def set_value(self, value: int) -> None:
        """Set the spin box value."""
        self._spin.setValue(value)
        self._initial_value = value

    def _update_styling(self) -> None:
        """Update spin box styling."""
        super()._update_styling()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._spin.setMinimumHeight(min_height)

    def set_enabled(self, enabled: bool) -> None:
        """Enable or disable the spin box."""
        self._spin.setEnabled(enabled)


class SettingsDoubleSpinBox(SettingsField):
    """
    Double spin box setting for float values.

    Attributes:
        _spin (QDoubleSpinBox): The double spin box control
        _initial_value (float): The initial value
    """

    def __init__(self,
                 label_text: str,
                 min_value: float = 0.0,
                 max_value: float = 100.0,
                 step: float = 1.0,
                 decimals: int = 2,
                 parent: Optional[QWidget] = None) -> None:
        """
        Initialize a double spin box setting.

        Args:
            label_text: Text for the setting label
            min_value: Minimum allowed value
            max_value: Maximum allowed value
            step: Step size for incrementing/decrementing
            decimals: Number of decimal places to display
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._spin = QDoubleSpinBox()
        self._spin.setRange(min_value, max_value)
        self._spin.setSingleStep(step)
        self._spin.setDecimals(decimals)
        self._spin.valueChanged.connect(self._handle_changed)

        self._layout.addWidget(self._spin)
        self._initial_value = self._spin.value()
        self._update_styling()

    def _handle_changed(self) -> None:
        """Handle double spin box value changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if double spin box value has changed."""
        # Use a small epsilon for float comparison
        return abs(self._spin.value() - self._initial_value) > 0.001

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._spin.value()

    def get_value(self) -> float:
        """Get the current double spin box value."""
        return self._spin.value()

    def set_value(self, value: float) -> None:
        """Set the double spin box value."""
        self._spin.setValue(value)
        self._initial_value = value

    def _update_styling(self) -> None:
        """Update double spin box styling."""
        super()._update_styling()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._spin.setMinimumHeight(min_height)

    def set_enabled(self, enabled: bool) -> None:
        """Enable or disable the double spin box."""
        self._spin.setEnabled(enabled)


class SettingsTextField(SettingsField):
    """
    Text field setting for string values.

    Attributes:
        _text_field (QLineEdit): The text field control
        _initial_value (str): The initial value
    """

    def __init__(self,
                 label_text: str,
                 placeholder: str = "",
                 parent: Optional[QWidget] = None) -> None:
        """
        Initialize a text field setting.

        Args:
            label_text: Text for the setting label
            placeholder: Placeholder text for empty field
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._text_field = QLineEdit()
        self._text_field.setPlaceholderText(placeholder)
        self._text_field.textChanged.connect(self._handle_changed)

        self._layout.addWidget(self._text_field)
        self._initial_value = self._text_field.text()
        self._update_styling()

    def _handle_changed(self) -> None:
        """Handle text field changes."""
        self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if text field value has changed."""
        return self._text_field.text() != self._initial_value

    def reset_modified_state(self) -> None:
        """Reset the initial value to current value."""
        self._initial_value = self._text_field.text()

    def get_value(self) -> str:
        """Get the current text field value."""
        return self._text_field.text()

    def set_value(self, value: str) -> None:
        """Set the text field value."""
        self._text_field.setText(value)
        self._initial_value = value

    def _update_styling(self) -> None:
        """Update text field styling."""
        super()._update_styling()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._text_field.setMinimumHeight(min_height)

    def set_enabled(self, enabled: bool) -> None:
        """Enable or disable the text field."""
        self._text_field.setEnabled(enabled)


class SettingsDisplay(SettingsField):
    """
    Read-only display field for showing information.

    Attributes:
        _display (QLabel): The display label
    """

    def __init__(self,
                 label_text: str,
                 value: str = "",
                 parent: Optional[QWidget] = None) -> None:
        """
        Initialize a display field setting.

        Args:
            label_text: Text for the setting label
            value: Initial display value
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._display = QLabel(value)
        self._display.setWordWrap(True)
        self._display.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)

        self._layout.addWidget(self._display)
        self._update_styling()

    def get_value(self) -> str:
        """Get the current display text."""
        return self._display.text()

    def set_value(self, value: str) -> None:
        """Set the display text."""
        self._display.setText(value)

    def _update_styling(self) -> None:
        """Update display field styling."""
        super()._update_styling()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._display.setMinimumHeight(min_height)

        value_color = self._style_manager.get_color_str(ColorRole.TEXT_BRIGHT)
        background = self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)

        self._display.setStyleSheet(f"""
            QLabel {{
                color: {value_color};
                background-color: {background};
                border-radius: 4px;
                padding: 8px;
            }}
        """)


class SettingsContainer(QWidget):
    """
    Container for organized settings with consistent layout and styling.

    This serves as the main widget to hold all settings items with proper
    spacing, scrolling, and organization.

    Attributes:
        value_changed (Signal): Emitted when any contained setting changes
        _style_manager (StyleManager): Reference to the style manager singleton
        _settings (List[SettingsItem]): List of all settings in this container
        _layout (QVBoxLayout): Main layout for the container
    """

    value_changed = Signal()

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        """Initialize the settings container."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._settings = []

        # Create main layout with proper spacing
        self._layout = QVBoxLayout()
        self._layout.setSpacing(0)
        self._layout.setContentsMargins(0, 0, 0, 0)

        self.setLayout(self._layout)

    def add_setting(self, setting: SettingsItem) -> None:
        """
        Add a setting to the container.

        Args:
            setting: The setting item to add
        """
        self._layout.addWidget(setting)
        self._settings.append(setting)
        setting.value_changed.connect(self.value_changed)

    def add_stretch(self) -> None:
        """Add a stretch to push all content upward."""
        self._layout.addStretch()

    def is_modified(self) -> bool:
        """Check if any settings in the container are modified."""
        return any(setting.is_modified() for setting in self._settings)

    def reset_modified_state(self) -> None:
        """Reset the modified state of all settings."""
        for setting in self._settings:
            setting.reset_modified_state()


class SettingsFactory:
    """
    Factory to create settings items with consistent styling and behavior.

    This provides helper methods to create various types of settings items
    with default styling and behavior.
    """

    @staticmethod
    def create_spacer(height: int, parent: Optional[QWidget] = None) -> SettingsSpacer:
        """Create a header."""
        return SettingsSpacer(height, parent)

    @staticmethod
    def create_header(title: str, parent: Optional[QWidget] = None) -> SettingsHeader:
        """Create a header."""
        return SettingsHeader(title, parent)

    @staticmethod
    def create_section(title: str, parent: Optional[QWidget] = None) -> SettingsSection:
        """Create a section header."""
        return SettingsSection(title, parent)

    @staticmethod
    def create_checkbox(text: str, parent: Optional[QWidget] = None) -> SettingsCheckbox:
        """Create a checkbox setting."""
        return SettingsCheckbox(text, parent)

    @staticmethod
    def create_combo(
        label_text: str,
        items: List[Tuple[str, Any]] = None,
        parent: Optional[QWidget] = None
    ) -> SettingsCombo:
        """Create a combo box setting."""
        return SettingsCombo(label_text, items, parent)

    @staticmethod
    def create_spinbox(
        label_text: str,
        min_value: int = 0,
        max_value: int = 100,
        step: int = 1,
        parent: Optional[QWidget] = None
    ) -> SettingsSpinBox:
        """Create a spin box setting."""
        return SettingsSpinBox(label_text, min_value, max_value, step, parent)

    @staticmethod
    def create_double_spinbox(
        label_text: str,
        min_value: float = 0.0,
        max_value: float = 100.0,
        step: float = 1.0,
        decimals: int = 2,
        parent: Optional[QWidget] = None
    ) -> SettingsDoubleSpinBox:
        """Create a double spin box setting."""
        return SettingsDoubleSpinBox(label_text, min_value, max_value, step, decimals, parent)

    @staticmethod
    def create_text_field(
        label_text: str,
        placeholder: str = "",
        parent: Optional[QWidget] = None
    ) -> SettingsTextField:
        """Create a text field setting."""
        return SettingsTextField(label_text, placeholder, parent)

    @staticmethod
    def create_display(
        label_text: str,
        value: str = "",
        parent: Optional[QWidget] = None
    ) -> SettingsDisplay:
        """Create a display field."""
        return SettingsDisplay(label_text, value, parent)
