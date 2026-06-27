"""User settings module for storing application-wide settings."""

from dataclasses import dataclass, field
import json
import os
import logging
from typing import Any

from ai import AIBackendSettings
from filesystem_ai_tool.filesystem_access_settings import FilesystemAccessSettings

from desktop.language.language_code import LanguageCode
from desktop.color_theme import ColorTheme
from desktop.user.user_file_sort_order import UserFileSortOrder


@dataclass
class UserSettings:
    """
    User-specific application settings.
    """
    _logger = logging.getLogger("UserSettings")

    ai_backends: dict[str, AIBackendSettings] = field(default_factory=dict)
    language: LanguageCode = LanguageCode.EN
    font_size: float| None = None  # None means use the default font size
    theme: ColorTheme = ColorTheme.SYSTEM
    custom_colors: dict[str, dict[str, str]] = field(default_factory=dict)
    # Named custom colour themes the user has saved: name -> {role: {mode: hex}}.
    saved_color_themes: dict[str, dict[str, dict[str, str]]] = field(default_factory=dict)
    # Name of the saved custom theme currently in use, or None for the live ("Manually") custom set.
    active_custom_theme_name: str | None = None
    file_sort_order: UserFileSortOrder = UserFileSortOrder.DIRECTORIES_FIRST
    font_ligatures: bool = True
    allow_external_file_access: bool = True
    check_for_updates: bool = True
    external_file_allowlist: list[str] = field(default_factory=list)
    external_file_denylist: list[str] = field(default_factory=list)

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
                "ollama-cloud": AIBackendSettings(),
                "vllm": AIBackendSettings(),
                "xai": AIBackendSettings(),
                "zai": AIBackendSettings()
            },
            language=LanguageCode.EN,
            font_size=None,
            theme=ColorTheme.SYSTEM,
            file_sort_order=UserFileSortOrder.DIRECTORIES_FIRST,
            font_ligatures=True,
            allow_external_file_access=True,
            check_for_updates=True,
            external_file_allowlist=FilesystemAccessSettings.get_default_allowlist(),
            external_file_denylist=FilesystemAccessSettings.get_default_denylist()
        )

    @staticmethod
    def _validate_ai_backend_data(backend_data: Any) -> bool:
        """
        Validate AI backend data structure.

        Args:
            backend_data: Data to validate

        Returns:
            True if valid, False otherwise
        """
        if not isinstance(backend_data, dict):
            return False

        # Check for required/expected keys with correct types
        if "enabled" in backend_data and not isinstance(backend_data["enabled"], bool):
            return False

        if "api_key" in backend_data and not isinstance(backend_data["api_key"], str):
            return False

        if "url" in backend_data and not isinstance(backend_data["url"], str):
            return False

        return True

    @staticmethod
    def _validate_list_of_strings(data: Any) -> bool:
        """
        Validate that data is a list of strings.

        Args:
            data: Data to validate

        Returns:
            True if valid list of strings, False otherwise
        """
        if not isinstance(data, list):
            return False

        return all(isinstance(item, str) for item in data)

    @classmethod
    def _safe_load_json(cls, path: str) -> dict:
        """
        Safely load and parse JSON from file.

        Args:
            path: Path to the JSON file

        Returns:
            Parsed JSON data as dictionary

        Raises:
            json.JSONDecodeError: If file contains invalid JSON
            ValueError: If JSON root is not a dictionary
        """
        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)

            # Validate that root is a dictionary
            if not isinstance(data, dict):
                cls._logger.error(
                    "Invalid settings file format: root must be a JSON object/dictionary, got %s",
                    type(data).__name__
                )
                raise ValueError(
                    f"Invalid settings file format: expected JSON object, got {type(data).__name__}"
                )

            return data

        except json.JSONDecodeError as e:
            cls._logger.error(
                "Failed to parse JSON from %s: %s at line %d column %d",
                path, e.msg, e.lineno, e.colno
            )
            raise

        except UnicodeDecodeError as e:
            cls._logger.error("Failed to decode file %s: %s", path, str(e))
            raise json.JSONDecodeError(f"File encoding error: {str(e)}", "", 0)

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
            ValueError: If JSON structure is invalid
        """
        # Start with default settings
        settings = cls.create_default()

        # Load and validate JSON
        data = cls._safe_load_json(path)

        # Load AI backend settings
        if "ai_backends" in data:
            if not isinstance(data["ai_backends"], dict):
                cls._logger.warning(
                    "Invalid ai_backends format in %s: expected dict, got %s. Using defaults.",
                    path, type(data["ai_backends"]).__name__
                )

            else:
                for backend_id, backend_data in data["ai_backends"].items():
                    if not isinstance(backend_id, str):
                        cls._logger.warning("Skipping non-string backend ID: %s", backend_id)
                        continue

                    if not backend_id in settings.ai_backends:
                        cls._logger.debug("Ignoring unknown backend: %s", backend_id)
                        continue

                    if not cls._validate_ai_backend_data(backend_data):
                        cls._logger.warning(
                            "Invalid backend data for %s in %s. Using defaults.",
                            backend_id, path
                        )
                        continue

                    settings.ai_backends[backend_id] = AIBackendSettings(
                        enabled=backend_data.get("enabled", False),
                        api_key=backend_data.get("api_key", ""),
                        url=backend_data.get("url", "")
                    )

        # Legacy support for older settings format (v0.8 through v0.11)
        elif "api_keys" in data:
            if not isinstance(data["api_keys"], dict):
                cls._logger.warning(
                    "Invalid api_keys format in %s: expected dict, got %s. Using defaults.",
                    path, type(data["api_keys"]).__name__
                )

            else:
                cls._load_legacy_api_keys(settings, data["api_keys"], path)

        # Load other settings with validation
        language_code = data.get("language", "EN")
        if not isinstance(language_code, str):
            cls._logger.warning(
                "Invalid language type in %s: expected str, got %s. Using default.",
                path, type(language_code).__name__
            )

        else:
            try:
                settings.language = LanguageCode[language_code]

            except (KeyError, ValueError):
                cls._logger.warning(
                    "Invalid language code '%s' in %s. Using default (EN).",
                    language_code, path
                )
                settings.language = LanguageCode.EN

        # Load font size with validation
        font_size = data.get("fontSize", None)
        if font_size is not None and not isinstance(font_size, (int, float)):
            cls._logger.warning(
                "Invalid fontSize type in %s: expected number, got %s. Using default.",
                path, type(font_size).__name__
            )

        else:
            settings.font_size = font_size

        # Load theme if available, otherwise use default (dark mode)
        theme_str = data.get("theme", "SYSTEM")
        if not isinstance(theme_str, str):
            cls._logger.warning(
                "Invalid theme type in %s: expected str, got %s. Using default.",
                path, type(theme_str).__name__
            )
            settings.theme = ColorTheme.SYSTEM

        else:
            try:
                settings.theme = ColorTheme[theme_str]

            except (KeyError, ValueError):
                cls._logger.warning(
                    "Invalid theme '%s' in %s. Using default (DARK).",
                    theme_str, path
                )
                settings.theme = ColorTheme.SYSTEM

        # Load file sort order if available, otherwise use default
        sort_order_str = data.get("fileSortOrder", "DIRECTORIES_FIRST")
        if not isinstance(sort_order_str, str):
            cls._logger.warning(
                "Invalid fileSortOrder type in %s: expected str, got %s. Using default.",
                path, type(sort_order_str).__name__
            )
            settings.file_sort_order = UserFileSortOrder.DIRECTORIES_FIRST

        else:
            try:
                settings.file_sort_order = UserFileSortOrder[sort_order_str]

            except (KeyError, ValueError):
                cls._logger.warning(
                    "Invalid fileSortOrder '%s' in %s. Using default (DIRECTORIES_FIRST).",
                    sort_order_str, path
                )
                settings.file_sort_order = UserFileSortOrder.DIRECTORIES_FIRST


        # Load font ligatures setting with validation
        font_ligatures = data.get("fontLigatures", True)
        if not isinstance(font_ligatures, bool):
            cls._logger.warning(
                "Invalid fontLigatures type in %s: expected bool, got %s. Using default.",
                path, type(font_ligatures).__name__
            )

        else:
            settings.font_ligatures = font_ligatures

        # Load check for updates setting with validation
        check_for_updates = data.get("checkForUpdates", True)
        if not isinstance(check_for_updates, bool):
            cls._logger.warning(
                "Invalid checkForUpdates type in %s: expected bool, got %s. Using default.",
                path, type(check_for_updates).__name__
            )

        else:
            settings.check_for_updates = check_for_updates

        # Load external file access settings with validation
        allow_external = data.get("allowExternalFileAccess", True)
        if not isinstance(allow_external, bool):
            cls._logger.warning(
                "Invalid allowExternalFileAccess type in %s: expected bool, got %s. Using default.",
                path, type(allow_external).__name__
            )

        else:
            settings.allow_external_file_access = allow_external

        # Load allowlist with validation
        allowlist = data.get("externalFileAllowlist", [])
        if not cls._validate_list_of_strings(allowlist):
            cls._logger.warning(
                "Invalid externalFileAllowlist in %s: expected list of strings. Using defaults.",
                path
            )
            settings.external_file_allowlist = FilesystemAccessSettings.get_default_allowlist()

        else:
            settings.external_file_allowlist = allowlist if allowlist else FilesystemAccessSettings.get_default_allowlist()

        # Load denylist with validation
        denylist = data.get("externalFileDenylist", [])
        if not cls._validate_list_of_strings(denylist):
            cls._logger.warning(
                "Invalid externalFileDenylist in %s: expected list of strings. Using defaults.",
                path
            )
            settings.external_file_denylist = FilesystemAccessSettings.get_default_denylist()

        else:
            settings.external_file_denylist = denylist if denylist else FilesystemAccessSettings.get_default_denylist()

        # Load custom color overrides
        custom_colors_raw = data.get("customColors", {})
        if isinstance(custom_colors_raw, dict):
            validated: dict[str, dict[str, str]] = {}
            for role_key, mode_map in custom_colors_raw.items():
                if not isinstance(role_key, str) or not isinstance(mode_map, dict):
                    continue

                validated_modes: dict[str, str] = {}
                for mode_key, color_val in mode_map.items():
                    if isinstance(mode_key, str) and isinstance(color_val, str):
                        validated_modes[mode_key] = color_val

                if validated_modes:
                    validated[role_key] = validated_modes

            settings.custom_colors = validated

        else:
            cls._logger.warning(
                "Invalid customColors in %s: expected dict. Using empty.",
                path
            )

        # Load saved (named) custom colour themes
        saved_themes_raw = data.get("savedColorThemes", {})
        if isinstance(saved_themes_raw, dict):
            validated_themes: dict[str, dict[str, dict[str, str]]] = {}
            for theme_name, theme_colors in saved_themes_raw.items():
                if not isinstance(theme_name, str) or not isinstance(theme_colors, dict):
                    continue

                validated_theme: dict[str, dict[str, str]] = {}
                for role_key, mode_map in theme_colors.items():
                    if not isinstance(role_key, str) or not isinstance(mode_map, dict):
                        continue

                    validated_modes = {
                        mode_key: color_val
                        for mode_key, color_val in mode_map.items()
                        if isinstance(mode_key, str) and isinstance(color_val, str)
                    }
                    if validated_modes:
                        validated_theme[role_key] = validated_modes

                validated_themes[theme_name] = validated_theme

            settings.saved_color_themes = validated_themes

        else:
            cls._logger.warning(
                "Invalid savedColorThemes in %s: expected dict. Using empty.",
                path
            )

        # Load the name of the active saved custom theme (None means the live custom set)
        active_custom_theme = data.get("activeCustomThemeName", None)
        if active_custom_theme is None or (
            isinstance(active_custom_theme, str) and active_custom_theme in settings.saved_color_themes
        ):
            settings.active_custom_theme_name = active_custom_theme

        else:
            cls._logger.warning(
                "Invalid activeCustomThemeName in %s. Using live custom colours.",
                path
            )
            settings.active_custom_theme_name = None

        return settings

    @classmethod
    def _load_legacy_api_keys(cls, settings: "UserSettings", api_keys: dict, path: str) -> None:
        """
        Load legacy API keys format into settings.

        Args:
            settings: UserSettings object to populate
            api_keys: Dictionary of legacy API keys
            path: Path to settings file (for logging)
        """
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

        for backend_id, api_key in api_keys.items():
            if not isinstance(backend_id, str):
                cls._logger.warning("Skipping non-string API key ID in %s: %s", path, backend_id)
                continue

            if backend_id in backend_mapping:
                if not isinstance(api_key, str):
                    cls._logger.warning(
                        "Invalid API key type for %s in %s: expected str, got %s. Skipping.",
                        backend_id, path, type(api_key).__name__
                    )
                    continue

                mapped_backend = backend_mapping[backend_id]
                settings.ai_backends[mapped_backend] = AIBackendSettings(
                    enabled=api_key is not None and api_key != "",
                    api_key=api_key,
                    url=""
                )

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
            ValueError: If JSON structure is invalid
        """
        # Start with default settings
        settings = cls.create_default()

        # Load and validate JSON
        data = cls._safe_load_json(path)

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
                if not isinstance(data[key], str):
                    cls._logger.warning(
                        "Invalid API key type for %s in %s: expected str, got %s. Skipping.",
                        key, path, type(data[key]).__name__
                    )
                    continue

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
            "fontLigatures": self.font_ligatures,
            "theme": self.theme.name,
            "customColors": self.custom_colors,
            "savedColorThemes": self.saved_color_themes,
            "activeCustomThemeName": self.active_custom_theme_name,
            "fileSortOrder": self.file_sort_order.name,
            "checkForUpdates": self.check_for_updates,
            "allowExternalFileAccess": self.allow_external_file_access,
            "externalFileAllowlist": self.external_file_allowlist,
            "externalFileDenylist": self.external_file_denylist
        }

        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)

        # Set secure permissions for settings file (contains API keys)
        os.chmod(path, 0o600)
