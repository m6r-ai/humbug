from dataclasses import dataclass
import json
from typing import Dict

from ai import AIConversationSettings, AIReasoningCapability
from ai_tool import AIToolManager


@dataclass
class MindspaceSettings:
    """
    Settings for the mindspace.

    This class handles the loading and saving of settings to a JSON file.
    """
    enabled_tools: Dict[str, bool]
    model: str = AIConversationSettings.get_default_model({})  # Will be overridden with actual backends
    temperature: float = 0.7  # Default temperature
    reasoning: AIReasoningCapability = AIReasoningCapability.NO_REASONING
    use_soft_tabs: bool = True
    tab_size: int = 4
    auto_backup: bool = False  # Default to off
    auto_backup_interval: int = 300  # Default 5 minutes in seconds

    @classmethod
    def load(cls, path: str) -> "MindspaceSettings":
        """Load settings from a JSON file."""
        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)
            editor = data.get("editor", {})
            conversation = data.get("conversation", {})
            tools = data.get("tools", {})

            default_model = AIConversationSettings.get_default_model({})
            default_reasoning = AIConversationSettings.get_reasoning_capability(default_model)

            # Handle reasoning bitflag from JSON
            reasoning_value = conversation.get("reasoning", default_reasoning.value)
            reasoning = AIReasoningCapability(reasoning_value)

            # Handle enabled tools - start with defaults and override with saved values
            tool_manager = AIToolManager()
            enabled_tools = tool_manager.get_default_enabled_tools()
            saved_enabled_tools = tools.get("enabled", {})

            # Update with saved values, but only for tools that exist in the configuration
            for tool_name, enabled in saved_enabled_tools.items():
                if tool_name in enabled_tools:
                    enabled_tools[tool_name] = enabled

            return cls(
                model=conversation.get("model", default_model),
                temperature=conversation.get("temperature", 0.7),
                reasoning=reasoning,
                use_soft_tabs=editor.get("useSoftTabs", True),
                tab_size=editor.get("tabSize", 4),
                auto_backup=editor.get("autoBackup", False),
                auto_backup_interval=editor.get("autoBackupInterval", 300),
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
            "tools": {
                "enabled": self.enabled_tools,
            },
        }
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)
