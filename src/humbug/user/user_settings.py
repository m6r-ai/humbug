"""User settings module for storing application-wide settings."""

from dataclasses import dataclass, field
import json
import os
from typing import Dict, List

from ai import AIBackendSettings
from ai_tool.filesystem.filesystem_access_settings import FilesystemAccessSettings

from humbug.language.language_code import LanguageCode
from humbug.style_manager import ColorMode
from humbug.user.user_file_sort_order import UserFileSortOrder


@dataclass
class UserSettings:
    """
    User-specific application settings.
    """
    ai_backends: Dict[str, AIBackendSettings] = field(default_factory=dict)
    language: LanguageCode = LanguageCode.EN
    font_size: float| None = None  # None means use the default font size
    theme: ColorMode = ColorMode.DARK  # Default to dark mode
    file_sort_order: UserFileSortOrder = UserFileSortOrder.DIRECTORIES_FIRST
    allow_external_file_access: bool = True
    external_file_allowlist: List[str] = field(default_factory=list)
    external_file_denylist: List[str] = field(default_factory=list)

    @classmethod
    def create_default(cls) -> "UserSettings":
        """Create a new UserSettings object with default empty values."""
        return cls(
            ai_backends={
                "anthropic": AIBackendSettings(),
                "deepseek": AIBackendSettings(),
                "google": AIBackendSettings(),
                "mistral": AIBackendSettings(),
                "openai": AIBackendSettings(),
                "ollama": AIBackendSettings(),
                "vllm": AIBackendSettings(),
                "xai": AIBackendSettings(),
                "zai": AIBackendSettings()
            },
            language=LanguageCode.EN,
            font_size=None,
            theme=ColorMode.DARK,
            file_sort_order=UserFileSortOrder.DIRECTORIES_FIRST,
            allow_external_file_access=True,
            external_file_allowlist=FilesystemAccessSettings.get_default_allowlist(),
            external_file_denylist=FilesystemAccessSettings.get_default_denylist()
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
                    if backend_id in settings.ai_backends:
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
                    "MISTRAL_API_KEY": "mistral",
                    "OLLAMA_API_KEY": "ollama",
                    "OPENAI_API_KEY": "openai",
                    "XAI_API_KEY": "xai",
                    "ZAI_API_KEY": "zai"
                }

                for backend_id, api_key in data["api_keys"].items():
                    if backend_id in backend_mapping:
                        mapped_backend = backend_mapping[backend_id]
                        settings.ai_backends[mapped_backend] = AIBackendSettings(
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

            # Load file sort order if available, otherwise use default
            sort_order_str = data.get("fileSortOrder", "DIRECTORIES_FIRST")
            try:
                settings.file_sort_order = UserFileSortOrder[sort_order_str]

            except (KeyError, ValueError):
                settings.file_sort_order = UserFileSortOrder.DIRECTORIES_FIRST

            # Load external file access settings
            settings.allow_external_file_access = data.get("allowExternalFileAccess", True)
            settings.external_file_allowlist = data.get(
                "externalFileAllowlist",
                FilesystemAccessSettings.get_default_allowlist()
            )
            settings.external_file_denylist = data.get(
                "externalFileDenylist",
                FilesystemAccessSettings.get_default_denylist()
            )

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
                "MISTRAL_API_KEY": "mistral",
                "OLLAMA_API_KEY": "ollama",
                "OPENAI_API_KEY": "openai",
                "XAI_API_KEY": "xai",
                "ZAI_API_KEY": "zai"
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
            "fileSortOrder": self.file_sort_order.name,
            "allowExternalFileAccess": self.allow_external_file_access,
            "externalFileAllowlist": self.external_file_allowlist,
            "externalFileDenylist": self.external_file_denylist
        }

        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)

        # Set secure permissions for settings file (contains API keys)
        os.chmod(path, 0o600)
