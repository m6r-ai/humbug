"""
Combo box setting for selecting from a list of options.

This module defines the SettingsCombo class which provides a combo box
setting widget.
"""

from typing import List, Any, Tuple

from PySide6.QtWidgets import QWidget, QComboBox, QListView

from humbug.settings.settings_field import SettingsField


class SettingsCombo(SettingsField):
    """
    Combo box setting for selecting from a list of options.

    Attributes:
        _combo (QComboBox): The combo box control
        _initial_index (int): The initial selected index
        _items (List[tuple]): The items and their data values
    """

    def __init__(
        self,
        label_text: str,
        items: List[Tuple[str, Any]] | None = None,
        parent: QWidget | None = None
    ) -> None:
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
        self._handle_style_changed()

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

    def _handle_style_changed(self) -> None:
        """Update combo box styling."""
        super()._handle_style_changed()

        # Set minimum size based on zoom factor
        zoom_factor = self._style_manager.zoom_factor()
        min_height = int(30 * zoom_factor)
        self._combo.setMinimumHeight(min_height)

    def setEnabled(self, arg__1: bool) -> None:
        """Enable or disable the combo box."""
        self._combo.setEnabled(arg__1)
