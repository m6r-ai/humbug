"""Tab hosting the git commit-history view."""

import os

from PySide6.QtWidgets import QVBoxLayout, QWidget

from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.tab import TabBase, TabState
from desktop.git_history_tab.git_history_widget import GitHistoryWidget


class GitHistoryTab(TabBase):
    """Read-only tab showing a repository's commit history and diffs."""

    def __init__(self, tab_id: str, path: str = "", parent: QWidget | None = None) -> None:
        super().__init__(tab_id, parent)
        self._path = path
        self._style_manager = StyleManager()

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._widget = GitHistoryWidget(path, self)
        self._widget.status_updated.connect(self.update_status)
        layout.addWidget(self._widget)

        self._style_manager.style_changed.connect(self.apply_style)
        self.apply_style()

        if path:
            self._widget.reload()

    def tool_name(self) -> str:
        return "git_history"

    def tab_title_from_path(self) -> str:
        if self._path:
            return f"History — {os.path.basename(self._path.rstrip(os.sep))}"

        return "History"

    def set_path(self, path: str) -> None:
        self._path = path
        self._widget.set_repo_root(path)

    def on_path_renamed(self, new_path: str) -> None:
        self.set_path(new_path)

    def set_active(self, widget: QWidget, active: bool) -> None:
        if active:
            self.activated.emit()

    def activate(self) -> None:
        pass

    def can_close_tab(self) -> bool:
        return True

    def close_tab(self) -> None:
        pass

    def can_save(self) -> bool:
        return False

    def save(self) -> bool:
        return True

    def can_save_as(self) -> bool:
        return False

    def save_as(self) -> bool:
        return True

    def can_undo(self) -> bool:
        return False

    def undo(self) -> None:
        pass

    def can_redo(self) -> bool:
        return False

    def redo(self) -> None:
        pass

    def can_cut(self) -> bool:
        return False

    def cut(self) -> None:
        pass

    def can_copy(self) -> bool:
        return False

    def copy(self) -> None:
        pass

    def can_paste(self) -> bool:
        return False

    def paste(self) -> None:
        pass

    def can_submit(self) -> bool:
        return False

    def submit(self) -> None:
        pass

    def show_find(self) -> None:
        pass

    def update_status(self) -> None:
        count = self._widget.commit_count()
        self.status_message.emit(StatusMessage(f"History — {count} commits"))

    def apply_style(self) -> None:
        self._widget.apply_style()

    def get_state(self, temp_state: bool = False) -> TabState:
        return TabState(
            type=self.tool_name(),
            tab_id=self._tab_id,
            path=self._path,
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> "GitHistoryTab":
        return cls(state.tab_id, state.path, parent)
