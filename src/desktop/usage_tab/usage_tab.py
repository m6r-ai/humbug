"""Token usage tab."""

from PySide6.QtWidgets import QVBoxLayout, QWidget

from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.tab import TabBase, TabState
from desktop.usage_tab.usage_widget import UsageWidget


class UsageTab(TabBase):
    """Tab showing per-mindspace token usage by provider and model."""

    def __init__(self, tab_id: str, parent: QWidget | None = None) -> None:
        super().__init__(tab_id, parent)
        self._style_manager = StyleManager()

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._usage_widget = UsageWidget(self)
        layout.addWidget(self._usage_widget)

        self._style_manager.style_changed.connect(self.apply_style)

    def tool_name(self) -> str:
        return "usage"

    def tab_title_from_path(self) -> str:
        return "Token Usage"

    def on_path_renamed(self, new_path: str) -> None:
        pass

    def set_active(self, widget: QWidget, active: bool) -> None:
        if active:
            self.activated.emit()

    def activate(self) -> None:
        self._usage_widget.refresh()

    def set_path(self, path: str) -> None:
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
        self.status_message.emit(StatusMessage("Token Usage"))

    def preferred_width(self) -> int | None:
        return int(self._style_manager.nice_tab_width() * self._style_manager.zoom_factor())

    def apply_style(self) -> None:
        self._usage_widget.apply_style()

    def get_state(self, temp_state: bool = False) -> TabState:
        return TabState(type=self.tool_name(), tab_id=self._tab_id, path="", metadata={})

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> "UsageTab":
        return cls(state.tab_id, parent)
