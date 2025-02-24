from dataclasses import dataclass
import json

from humbug.ai.conversation_settings import ConversationSettings
from humbug.language.language_code import LanguageCode


@dataclass
class MindspaceSettings:
    use_soft_tabs: bool = True
    tab_size: int = 4
    font_size: float = None  # None means use the default font size
    auto_backup: bool = False  # Default to off
    auto_backup_interval: int = 300  # Default 5 minutes in seconds
    language: LanguageCode = LanguageCode.EN
    model: str = ConversationSettings.get_default_model({})  # Will be overridden with actual backends
    temperature: float = 0.7  # Default temperature

    @classmethod
    def load(cls, path: str) -> "MindspaceSettings":
        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)
            editor = data.get("editor", {})
            conversation = data.get("conversation", {})
            language_code = editor.get("language", "EN")
            return cls(
                use_soft_tabs=editor.get("useSoftTabs", True),
                tab_size=editor.get("tabSize", 4),
                font_size=editor.get("fontSize", None),
                auto_backup=editor.get("autoBackup", False),
                auto_backup_interval=editor.get("autoBackupInterval", 300),
                language=LanguageCode[language_code],
                model=conversation.get("model", ConversationSettings.get_default_model({})),
                temperature=conversation.get("temperature", 0.7)
            )

    def save(self, path: str) -> None:
        data = {
            "language": self.language.name,
            "fontSize": self.font_size,
            "conversation": {
                "model": self.model,
                "temperature": self.temperature
            },
            "editor": {
                "useSoftTabs": self.use_soft_tabs,
                "tabSize": self.tab_size,
                "autoBackup": self.auto_backup,
                "autoBackupInterval": self.auto_backup_interval,
            },
        }
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)
