"""User settings module for storing application-wide settings."""

from dataclasses import dataclass, field
import json
import os
from typing import Dict, Optional

from humbug.language.language_code import LanguageCode
from humbug.gui.style_manager import ColorMode


@dataclass
class UserSettings:
    """
    User-specific application settings.
    """
    api_keys: Dict[str, str] = field(default_factory=dict)
    language: LanguageCode = LanguageCode.EN
    font_size: float = None  # None means use the default font size
    theme: ColorMode = ColorMode.DARK  # Default to dark mode

    @classmethod
    def create_default(cls) -> "UserSettings":
        """Create a new UserSettings object with default empty values."""
        return cls(
            api_keys={
                "ANTHROPIC_API_KEY": "",
                "DEEPSEEK_API_KEY": "",
                "GOOGLE_API_KEY": "",
                "M6R_API_KEY": "",
                "MISTRAL_API_KEY": "",
                "OPENAI_API_KEY": ""
            },
            language=LanguageCode.EN,
            font_size=None,
            theme=ColorMode.DARK
        )

    @classmethod
    def load(cls, path: str) -> "UserSettings":
        """
        Load user settings from file.

        Args:
            path: Path to the settings file

        Returns:
            UserSettings object with loaded values

        Raises:
            json.JSONDecodeError: If file contains invalid JSON
        """
        # Start with default settings
        settings = cls.create_default()

        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                if "api_keys" in data:
                    for key, value in data["api_keys"].items():
                        settings.api_keys[key] = value

                language_code = data.get("language", "EN")
                settings.language = LanguageCode[language_code]
                settings.font_size = data.get("fontSize", None)

                # Load theme if available, otherwise use default (dark mode)
                theme_str = data.get("theme", "DARK")
                try:
                    settings.theme = ColorMode[theme_str]
                except (KeyError, ValueError):
                    settings.theme = ColorMode.DARK

        except json.JSONDecodeError:
            raise

        return settings

    @classmethod
    def load_legacy(cls, path: str) -> "UserSettings":
        """
        Load legacy user settings from file.

        Args:
            path: Path to the settings file

        Returns:
            UserSettings object with loaded values

        Raises:
            json.JSONDecodeError: If file contains invalid JSON
        """
        # Start with default settings
        settings = cls.create_default()

        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                for key in settings.api_keys:
                    if key in data:
                        settings.api_keys[key] = data[key]

        except json.JSONDecodeError:
            raise

        return settings

    def save(self, path: str) -> None:
        """
        Save user settings to file.

        Args:
            path: Path to save the settings file

        Raises:
            OSError: If there's an issue creating the directory or writing the file
        """
        # Ensure directory exists
        os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)

        # Save settings in a structured format for future extensibility
        data = {
            "api_keys": self.api_keys,
            "language": self.language.name,
            "fontSize": self.font_size,
            "theme": self.theme.name,
        }

        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)

        # Set secure permissions for settings file (contains API keys)
        os.chmod(path, 0o600)
