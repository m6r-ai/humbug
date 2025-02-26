"""Language management singleton."""

from typing import Dict

from PySide6.QtCore import QObject, Signal

from humbug.language.en.en_strings import get_english_strings
from humbug.language.fr.fr_strings import get_french_strings
from humbug.language.ar.ar_strings import get_arabic_strings
from humbug.language.language_code import LanguageCode
from humbug.language.language_strings import LanguageStrings


class LanguageManager(QObject):
    """Singleton manager for application-wide language settings."""

    language_changed = Signal()
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super(LanguageManager, cls).__new__(cls)
        return cls._instance

    def __init__(self):
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._current_language = LanguageCode.EN
            self._initialized = True
            self._language_mappings: Dict[LanguageCode, LanguageStrings] = {
                LanguageCode.EN: get_english_strings(),
                LanguageCode.FR: get_french_strings(),
                LanguageCode.AR: get_arabic_strings()
            }

    @property
    def strings(self) -> LanguageStrings:
        """Get strings for current language."""
        return self._language_mappings[self._current_language]

    @property
    def current_language(self) -> LanguageCode:
        """Get current language code."""
        return self._current_language

    def set_language(self, code: LanguageCode) -> None:
        """Set new language and emit change signal."""
        if code != self._current_language:
            self._current_language = code
            self.language_changed.emit()

    @property
    def left_to_right(self) -> bool:
        """Are we using a left to right language?"""
        return self._current_language is not LanguageCode.AR
