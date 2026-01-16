from dataclasses import dataclass
import json
import logging
from typing import Dict

from ai import AIConversationSettings, AIReasoningCapability
from ai_tool import AIToolManager


@dataclass
class MindspaceSettings:
    """
    Settings for the mindspace.

    This class handles the loading and saving of settings to a JSON file.
    """
    _logger = logging.getLogger("MindspaceSettings")

    enabled_tools: Dict[str, bool]
    model: str = AIConversationSettings.get_default_model({})  # Will be overridden with actual backends
    temperature: float = 0.7  # Default temperature
    reasoning: AIReasoningCapability = AIReasoningCapability.NO_REASONING
    use_soft_tabs: bool = True
    tab_size: int = 4
    auto_backup: bool = False  # Default to off
    auto_backup_interval: int = 300  # Default 5 minutes in seconds
    terminal_scrollback_enabled: bool = True  # Default to limited scrollback
    terminal_scrollback_lines: int = 10000  # Default 10000 lines
    terminal_close_on_exit: bool = True  # Default to native terminal behavior

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

    @staticmethod
    def _safe_get_dict(data: dict, key: str, default: dict | None = None) -> dict:
        """
        Safely get a dictionary value from data, with validation.

        Args:
            data: Source dictionary
            key: Key to retrieve
            default: Default value if key not found or invalid

        Returns:
            Dictionary value or default
        """
        if default is None:
            default = {}

        value = data.get(key, default)
        if not isinstance(value, dict):
            MindspaceSettings._logger.warning(
                "Invalid type for '%s': expected dict, got %s. Using default.",
                key, type(value).__name__
            )
            return default

        return value

    @classmethod
    def load(cls, path: str) -> "MindspaceSettings":
        """
        Load settings from a JSON file.

        Args:
            path: Path to the settings file

        Returns:
            MindspaceSettings object with loaded values

        Raises:
            json.JSONDecodeError: If file contains invalid JSON
            ValueError: If JSON structure is invalid
        """
        # Load and validate JSON
        data = cls._safe_load_json(path)

        # Safely extract nested dictionaries with validation
        editor = cls._safe_get_dict(data, "editor")
        conversation = cls._safe_get_dict(data, "conversation")
        tools = cls._safe_get_dict(data, "tools")
        terminal = cls._safe_get_dict(data, "terminal")

        # Get defaults
        default_model = AIConversationSettings.get_default_model({})
        default_reasoning = AIConversationSettings.get_reasoning_capability(default_model)

        # Load model with validation
        model = conversation.get("model", default_model)
        if not isinstance(model, str):
            cls._logger.warning(
                "Invalid model type in %s: expected str, got %s. Using default.",
                path, type(model).__name__
            )
            model = default_model

        # Load temperature with validation
        temperature = conversation.get("temperature", 0.7)
        if not isinstance(temperature, (int, float)):
            cls._logger.warning(
                "Invalid temperature type in %s: expected number, got %s. Using default.",
                path, type(temperature).__name__
            )
            temperature = 0.7

        elif temperature < 0.0 or temperature > 2.0:
            cls._logger.warning(
                "Invalid temperature value in %s: %s (must be 0.0-2.0). Using default.",
                path, temperature
            )
            temperature = 0.7

        # Handle reasoning bitflag from JSON with validation
        reasoning_value = conversation.get("reasoning", default_reasoning.value)
        if not isinstance(reasoning_value, int):
            cls._logger.warning(
                "Invalid reasoning type in %s: expected int, got %s. Using default.",
                path, type(reasoning_value).__name__
            )
            reasoning = default_reasoning

        else:
            try:
                reasoning = AIReasoningCapability(reasoning_value)

            except ValueError:
                cls._logger.warning(
                    "Invalid reasoning value in %s: %s. Using default.",
                    path, reasoning_value
                )
                reasoning = default_reasoning

        # Handle enabled tools - start with defaults and override with saved values
        tool_manager = AIToolManager()
        enabled_tools = tool_manager.get_default_enabled_tools()
        saved_enabled_tools = tools.get("enabled", {})

        # Validate that enabled tools is a dictionary
        if not isinstance(saved_enabled_tools, dict):
            cls._logger.warning(
                "Invalid enabled tools type in %s: expected dict, got %s. Using defaults.",
                path, type(saved_enabled_tools).__name__
            )
            saved_enabled_tools = {}

        # Update with saved values, but only for tools that exist in the configuration
        for tool_name, enabled in saved_enabled_tools.items():
            if not isinstance(tool_name, str):
                cls._logger.warning("Skipping non-string tool name in %s: %s", path, tool_name)
                continue

            if not isinstance(enabled, bool):
                cls._logger.warning(
                    "Invalid enabled value for tool '%s' in %s: expected bool, got %s. Skipping.",
                    tool_name, path, type(enabled).__name__
                )
                continue

            if tool_name in enabled_tools:
                enabled_tools[tool_name] = enabled

        # Load editor settings with validation
        use_soft_tabs = editor.get("useSoftTabs", True)
        if not isinstance(use_soft_tabs, bool):
            cls._logger.warning(
                "Invalid useSoftTabs type in %s: expected bool, got %s. Using default.",
                path, type(use_soft_tabs).__name__
            )
            use_soft_tabs = True

        tab_size = editor.get("tabSize", 4)
        if not isinstance(tab_size, int):
            cls._logger.warning(
                "Invalid tabSize type in %s: expected int, got %s. Using default.",
                path, type(tab_size).__name__
            )
            tab_size = 4

        elif tab_size < 1 or tab_size > 16:
            cls._logger.warning(
                "Invalid tabSize value in %s: %s (must be 1-16). Using default.",
                path, tab_size
            )
            tab_size = 4

        auto_backup = editor.get("autoBackup", False)
        if not isinstance(auto_backup, bool):
            cls._logger.warning(
                "Invalid autoBackup type in %s: expected bool, got %s. Using default.",
                path, type(auto_backup).__name__
            )
            auto_backup = False

        auto_backup_interval = editor.get("autoBackupInterval", 300)
        if not isinstance(auto_backup_interval, int):
            cls._logger.warning(
                "Invalid autoBackupInterval type in %s: expected int, got %s. Using default.",
                path, type(auto_backup_interval).__name__
            )
            auto_backup_interval = 300

        elif auto_backup_interval < 30:
            cls._logger.warning(
                "Invalid autoBackupInterval value in %s: %s (must be >= 30). Using default.",
                path, auto_backup_interval
            )
            auto_backup_interval = 300

        # Load terminal settings with validation
        terminal_scrollback_enabled = terminal.get("scrollbackEnabled", True)
        if not isinstance(terminal_scrollback_enabled, bool):
            cls._logger.warning(
                "Invalid scrollbackEnabled type in %s: expected bool, got %s. Using default.",
                path, type(terminal_scrollback_enabled).__name__
            )
            terminal_scrollback_enabled = True

        terminal_scrollback_lines = terminal.get("scrollbackLines", 10000)
        if not isinstance(terminal_scrollback_lines, int):
            cls._logger.warning(
                "Invalid scrollbackLines type in %s: expected int, got %s. Using default.",
                path, type(terminal_scrollback_lines).__name__
            )
            terminal_scrollback_lines = 10000

        elif terminal_scrollback_lines < 0:
            cls._logger.warning(
                "Invalid scrollbackLines value in %s: %s (must be >= 0). Using default.",
                path, terminal_scrollback_lines
            )
            terminal_scrollback_lines = 10000

        terminal_close_on_exit = terminal.get("closeOnExit", True)
        if not isinstance(terminal_close_on_exit, bool):
            cls._logger.warning(
                "Invalid closeOnExit type in %s: expected bool, got %s. Using default.",
                path, type(terminal_close_on_exit).__name__
            )
            terminal_close_on_exit = True

        return cls(
            model=model,
            temperature=temperature,
            reasoning=reasoning,
            use_soft_tabs=use_soft_tabs,
            tab_size=tab_size,
            auto_backup=auto_backup,
            auto_backup_interval=auto_backup_interval,
            terminal_scrollback_enabled=terminal_scrollback_enabled,
            terminal_scrollback_lines=terminal_scrollback_lines,
            terminal_close_on_exit=terminal_close_on_exit,
            enabled_tools=enabled_tools
        )

    def save(self, path: str) -> None:
        """Save settings to a JSON file."""
        data = {
            "conversation": {
                "model": self.model,
                "temperature": self.temperature,
                "reasoning": self.reasoning.value,  # Use .value to get the integer value of the enum
            },
            "editor": {
                "useSoftTabs": self.use_soft_tabs,
                "tabSize": self.tab_size,
                "autoBackup": self.auto_backup,
                "autoBackupInterval": self.auto_backup_interval,
            },
            "terminal": {
                "scrollbackEnabled": self.terminal_scrollback_enabled,
                "scrollbackLines": self.terminal_scrollback_lines,
                "closeOnExit": self.terminal_close_on_exit,
            },
            "tools": {
                "enabled": self.enabled_tools,
            },
        }
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)
