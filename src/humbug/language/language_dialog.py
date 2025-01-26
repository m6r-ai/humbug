"""Dialog for selecting application language."""

from PySide6.QtWidgets import QComboBox, QLabel, QHBoxLayout


from humbug.language.language_config import LanguageCode
from humbug.language.language_manager import LanguageManager


def create_language_selector(parent) -> tuple[QHBoxLayout, QComboBox]:
    """Create language selection UI elements.
    
    Args:
        parent: Parent widget for the selector

    Returns:
        Tuple of (layout containing selector, combo box for language selection)
    """
    language_manager = LanguageManager()

    layout = QHBoxLayout()
    label = QLabel(language_manager.strings.select_language)
    combo = QComboBox(parent)

    # Add language options
    language_names = {
        LanguageCode.EN: "English",
        LanguageCode.FR: "Français",
        LanguageCode.AR: "العربية"
    }

    for code in LanguageCode:
        combo.addItem(language_names[code], code)

    # Set current language
    current_index = combo.findData(language_manager.current_language)
    combo.setCurrentIndex(current_index)

    layout.addWidget(label)
    layout.addStretch()
    layout.addWidget(combo)

    return layout, combo
