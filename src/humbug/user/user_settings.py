"""User settings module for storing application-wide settings."""

from dataclasses import dataclass, field
import json
import os
from typing import Dict

from humbug.ai.ai_backend_settings import AIBackendSettings
from humbug.language.language_code import LanguageCode
from humbug.gui.style_manager import ColorMode


@dataclass
class UserSettings:
    """
    User-specific application settings.
    """
    ai_backends: Dict[str, AIBackendSettings] = field(default_factory=dict)
    language: LanguageCode = LanguageCode.EN
    font_size: float| None = None  # None means use the default font size
    theme: ColorMode = ColorMode.DARK  # Default to dark mode

    @classmethod
    def create_default(cls) -> "UserSettings":
        """Create a new UserSettings object with default empty values."""
        return cls(
            ai_backends={
                "anthropic": AIBackendSettings(),
                "deepseek": AIBackendSettings(),
                "google": AIBackendSettings(),
                "m6r": AIBackendSettings(),
                "mistral": AIBackendSettings(),
                "openai": AIBackendSettings(),
                "ollama": AIBackendSettings(),
                "xai": AIBackendSettings()
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

        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)

            # Load AI backend settings
            if "ai_backends" in data:
                for backend_id, backend_data in data["ai_backends"].items():
                    settings.ai_backends[backend_id] = AIBackendSettings(
                        enabled=backend_data.get("enabled", False),
                        api_key=backend_data.get("api_key", ""),
                        url=backend_data.get("url", "")
                    )

            # Legacy support for older settings format (v0.8 through v0.11)
            elif "api_keys" in data:
                backend_mapping = {
                    "ANTHROPIC_API_KEY": "anthropic",
                    "DEEPSEEK_API_KEY": "deepseek",
                    "GOOGLE_API_KEY": "google",
                    "M6R_API_KEY": "m6r",
                    "MISTRAL_API_KEY": "mistral",
                    "OPENAI_API_KEY": "openai",
                    "XAI_API_KEY": "xai"
                }

                for backend_id, api_key in data["api_keys"].items():
                    settings.ai_backends[backend_mapping[backend_id]] = AIBackendSettings(
                        enabled=api_key is not None and api_key != "",
                        api_key=api_key,
                        url=""
                    )

            # Load other settings
            language_code = data.get("language", "EN")
            settings.language = LanguageCode[language_code]
            settings.font_size = data.get("fontSize", None)

            # Load theme if available, otherwise use default (dark mode)
            theme_str = data.get("theme", "DARK")
            try:
                settings.theme = ColorMode[theme_str]

            except (KeyError, ValueError):
                settings.theme = ColorMode.DARK

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

        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)

            # Convert legacy API keys to backend settings
            backend_mapping = {
                "ANTHROPIC_API_KEY": "anthropic",
                "DEEPSEEK_API_KEY": "deepseek",
                "GOOGLE_API_KEY": "google",
                "M6R_API_KEY": "m6r",
                "MISTRAL_API_KEY": "mistral",
                "OPENAI_API_KEY": "openai",
                "XAI_API_KEY": "xai"
            }

            for key, backend_id in backend_mapping.items():
                if key in data and data[key]:
                    settings.ai_backends[backend_id] = AIBackendSettings(
                        enabled=True,
                        api_key=data[key],
                        url=""
                    )

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

        # Convert backend settings to serializable format
        ai_backends_data = {}
        for backend_id, backend_settings in self.ai_backends.items():
            ai_backends_data[backend_id] = {
                "enabled": backend_settings.enabled,
                "api_key": backend_settings.api_key,
                "url": backend_settings.url
            }

        # Save settings in a structured format for future extensibility
        data = {
            "ai_backends": ai_backends_data,
            "language": self.language.name,
            "fontSize": self.font_size,
            "theme": self.theme.name,
        }

        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)

        # Set secure permissions for settings file (contains API keys)
        os.chmod(path, 0o600)
