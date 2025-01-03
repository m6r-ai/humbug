from dataclasses import dataclass
import json
import os
from typing import Optional

@dataclass
class WorkspaceSettings:
    use_soft_tabs: bool = True
    tab_size: int = 4

    @classmethod
    def load(cls, path: str) -> "WorkspaceSettings":
        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)
            editor = data.get("editor", {})
            return cls(
                use_soft_tabs=editor.get("useSoftTabs", True),
                tab_size=editor.get("tabSize", 4)
            )

    def save(self, path: str) -> None:
        data = {
            "editor": {
                "useSoftTabs": self.use_soft_tabs,
                "tabSize": self.tab_size
            }
        }
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)

class WorkspaceManager:
    def __init__(self):
        self._workspace_path: Optional[str] = None
        self._settings: Optional[WorkspaceSettings] = None
        self._home_config = os.path.expanduser("~/.humbug/workspace.json")

    def create_workspace(self, path: str) -> bool:
        workspace_dir = os.path.join(path, ".humbug")
        if os.path.exists(workspace_dir):
            return False

        os.makedirs(workspace_dir)
        settings = WorkspaceSettings()
        settings.save(os.path.join(workspace_dir, "settings.json"))
        with open(os.path.join(workspace_dir, "recents.json"), 'w', encoding='utf-8') as f:
            json.dump({"tabs": []}, f)
        return True

    def open_workspace(self, path: str) -> Optional[WorkspaceSettings]:
        workspace_dir = os.path.join(path, ".humbug")
        if not os.path.exists(workspace_dir):
            return None

        try:
            settings = WorkspaceSettings.load(os.path.join(workspace_dir, "settings.json"))
            self._workspace_path = path
            self._settings = settings
            self._update_home_tracking()
            return settings
        except Exception:
            return None

    def _update_home_tracking(self):
        os.makedirs(os.path.dirname(self._home_config), exist_ok=True)
        with open(self._home_config, 'w', encoding='utf-8' ) as f:
            json.dump({"lastWorkspace": self._workspace_path}, f, indent=2)
