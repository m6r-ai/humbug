"""User settings module for storing application-wide settings."""

from dataclasses import dataclass, field
import json
import os
from typing import Dict


@dataclass
class UserSettings:
    """
    Store user-specific application settings.
    
    Currently focused on API keys for AI providers, but can be extended
    for other user-specific settings in the future.
    """
    # API keys for various services
    api_keys: Dict[str, str] = field(default_factory=dict)

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
            }
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
            # Only attempt to read if file exists and is not empty
            if os.path.exists(path):
                with open(path, 'r', encoding='utf-8') as f:
                    file_content = f.read()
                    if file_content.strip():
                        data = json.loads(file_content)

                        # Update API keys from file
                        if "api_keys" in data:
                            for key, value in data["api_keys"].items():
                                settings.api_keys[key] = value
                        else:
                            # Legacy format - direct key/value pairs
                            for key in settings.api_keys:
                                if key in data:
                                    settings.api_keys[key] = data[key]

        except (FileNotFoundError, json.JSONDecodeError) as e:
            # Re-raise JSON decode errors, but ignore file not found
            if isinstance(e, json.JSONDecodeError):
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
            "api_keys": self.api_keys
        }

        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)

        # Set secure permissions for settings file (contains API keys)
        os.chmod(path, 0o600)
