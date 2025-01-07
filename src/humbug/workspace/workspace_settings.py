from dataclasses import dataclass
import json

@dataclass
class WorkspaceSettings:
    use_soft_tabs: bool = True
    tab_size: int = 4
    font_size: float = None  # None means use the default font size

    @classmethod
    def load(cls, path: str) -> "WorkspaceSettings":
        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)
            editor = data.get("editor", {})
            return cls(
                use_soft_tabs=editor.get("useSoftTabs", True),
                tab_size=editor.get("tabSize", 4),
                font_size=editor.get("fontSize", None)
            )

    def save(self, path: str) -> None:
        data = {
            "editor": {
                "useSoftTabs": self.use_soft_tabs,
                "tabSize": self.tab_size,
                "fontSize": self.font_size
            }
        }
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)
