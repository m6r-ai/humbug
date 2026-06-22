"""Token usage tab."""

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QHBoxLayout, QSizePolicy, QVBoxLayout, QWidget

from desktop.status_message import StatusMessage
from desktop.color_role import ColorRole
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

        usage_container = QWidget()
        usage_container_layout = QHBoxLayout(usage_container)
        usage_container_layout.setContentsMargins(0, 0, 0, 0)
        usage_container_layout.setSpacing(0)
        usage_container_layout.setAlignment(Qt.AlignmentFlag.AlignHCenter)
        self._usage_widget.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        usage_container_layout.addWidget(self._usage_widget)
        layout.addWidget(usage_container)

        self._style_manager.style_changed.connect(self.apply_style)
        self._usage_widget.refreshed.connect(self._on_widget_refreshed)
        self.apply_style()

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
        return self._style_manager.scaled_tab_width()

    def apply_style(self) -> None:
        self._usage_widget.setMaximumWidth(self._style_manager.scaled_tab_width())
        self._usage_widget.apply_style()

        new_stylesheet = self._build_stylesheet()
        if new_stylesheet != self.styleSheet():
            self.setStyleSheet(new_stylesheet)

    def _on_widget_refreshed(self) -> None:
        """Apply the stylesheet after the widget has rebuilt its content."""
        new_stylesheet = self._build_stylesheet()
        if new_stylesheet != self.styleSheet():
            self.setStyleSheet(new_stylesheet)

    def _build_stylesheet(self) -> str:
        """Build the stylesheet for this tab."""
        style_manager = StyleManager()
        zoom = style_manager.zoom_factor()
        base = style_manager.base_font_size()
        fs = base * zoom
        border_radius = int(style_manager.message_bubble_spacing())

        soft_bg = style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        msg_bg = style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)
        border = style_manager.get_color_str(ColorRole.MESSAGE_USER_BORDER)
        sep = style_manager.get_color_str(ColorRole.MESSAGE_BORDER)
        hover_qc = style_manager.get_color(ColorRole.TEXT_PRIMARY)
        hover = f"rgba({hover_qc.red()}, {hover_qc.green()}, {hover_qc.blue()}, 0.06)"
        text = style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        dim = style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        heading = style_manager.get_color_str(ColorRole.TEXT_HEADING)
        mono = style_manager.make_monospace_font().family()

        return f"""
            #UsageWidget QWidget {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            #UsageWidget QLabel#UsageSectionLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
                background: transparent;
            }}

            #UsageWidget QFrame#UsageHeroCard {{
                background-color: {soft_bg};
                border: 1px solid {border};
                border-radius: {border_radius}px;
            }}
            #UsageWidget QFrame#UsageHeroCard QWidget,
            #UsageWidget QFrame#UsageHeroCard QLabel {{
                background: transparent;
            }}
            #UsageWidget QWidget#UsageHeroPane {{
                background: transparent;
            }}
            #UsageWidget QLabel#UsageHeroPaneLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: 600;
                letter-spacing: 0;
                background: transparent;
            }}
            #UsageWidget QLabel#UsageHeroTokens {{
                color: {heading};
                font-size: {round(fs * 2.0)}pt;
                font-weight: bold;
                font-family: "{mono}";
                background: transparent;
            }}
            #UsageWidget QLabel#UsageHeroSub {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                background: transparent;
            }}

            #UsageWidget QFrame#UsageCard {{
                background-color: {soft_bg};
                border: 1px solid {border};
                border-radius: {border_radius}px;
            }}
            #UsageWidget QFrame#UsageCard QWidget,
            #UsageWidget QFrame#UsageCard QLabel {{
                background: transparent;
            }}

            #UsageWidget QLabel#UsageStatLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: 600;
                background: transparent;
            }}
            #UsageWidget QLabel#UsageStatValue {{
                color: {text};
                font-size: {round(fs * 1.6)}pt;
                font-weight: bold;
                font-family: "{mono}";
                background: transparent;
            }}
            #UsageWidget QLabel#UsageStatNote {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                background: transparent;
            }}

            #UsageWidget QFrame#UsageProviderCard {{
                background-color: {msg_bg};
                border: 1px solid {sep};
                border-radius: {border_radius}px;
            }}
            #UsageWidget QFrame#UsageProviderCard QWidget,
            #UsageWidget QFrame#UsageProviderCard QLabel {{
                background: transparent;
            }}
            #UsageWidget QLabel#UsageProviderName {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
            }}
            #UsageWidget QLabel#UsageProviderTokens {{
                color: {text};
                font-size: {round(fs * 1.18)}pt;
                font-weight: bold;
                font-family: "{mono}";
            }}
            #UsageWidget QLabel#UsageProviderDetail {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
            }}

            #UsageWidget QFrame#UsageTableCard {{
                background-color: {msg_bg};
                border: 1px solid {sep};
                border-radius: {border_radius}px;
            }}
            #UsageWidget QFrame#UsageTableCard QWidget,
            #UsageWidget QFrame#UsageTableCard QLabel {{
                background: transparent;
            }}

            #UsageWidget QWidget#UsageTableHeader {{
                background: transparent;
            }}
            #UsageWidget QLabel#UsageTableHeaderCell {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
                background: transparent;
            }}

            #UsageWidget QLabel#UsageProviderLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
                background: transparent;
            }}

            #UsageWidget QWidget#UsageModelRow {{
                background: transparent;
                border-radius: {border_radius}px;
            }}
            #UsageWidget QWidget#UsageModelRow:hover {{
                background-color: {hover};
            }}
            #UsageWidget QLabel#UsageModelName {{
                color: {text};
                font-size: {round(fs)}pt;
                font-weight: 500;
                background: transparent;
            }}
            #UsageWidget QLabel#UsageModelTokens {{
                color: {dim};
                font-size: {round(fs)}pt;
                font-family: "{mono}";
                background: transparent;
            }}
            #UsageWidget QLabel#UsageModelDetail {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                font-family: "{mono}";
                background: transparent;
            }}

            #UsageWidget QWidget#UsagePager {{
                background: transparent;
            }}
            #UsageWidget QLabel#UsagePagerLabel,
            #UsageWidget QLabel#UsagePagerPage {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                background: transparent;
            }}
            #UsageWidget QLabel#UsagePagerPage {{
                min-width: {round(86 * zoom)}px;
            }}
            #UsageWidget QPushButton#UsagePagerButton {{
                background: transparent;
                color: {dim};
                border: 1px solid {border};
                border-radius: {border_radius}px;
                padding: {round(5 * zoom)}px {round(12 * zoom)}px;
                font-size: {round(fs * 0.85)}pt;
                font-weight: 500;
            }}
            #UsageWidget QPushButton#UsagePagerButton:hover {{
                background-color: {hover};
                color: {text};
                border-color: {dim};
            }}
            #UsageWidget QPushButton#UsagePagerButton:pressed {{
                background-color: {hover};
            }}
            #UsageWidget QPushButton#UsagePagerButton:disabled {{
                color: {dim};
                border-color: {sep};
            }}

            #UsageWidget QFrame#UsageRowSep {{
                color: {sep};
                border: none;
                border-top: 1px solid {sep};
                max-height: 1px;
                background: transparent;
            }}

            #UsageWidget QFrame#UsageEmptyCard {{
                background-color: {soft_bg};
                border: 1px solid {border};
                border-radius: {border_radius}px;
            }}
            #UsageWidget QFrame#UsageEmptyCard QLabel {{
                background: transparent;
            }}
            #UsageWidget QLabel#UsageEmptyTitle {{
                color: {text};
                font-size: {round(fs * 1.08)}pt;
                font-weight: bold;
            }}
            #UsageWidget QLabel#UsageEmptyMessage {{
                color: {dim};
                font-size: {round(fs)}pt;
            }}

            #UsageWidget QPushButton#UsageResetBtn {{
                background: transparent;
                color: {dim};
                border: 1px solid {border};
                border-radius: {border_radius}px;
                padding: {round(7 * zoom)}px {round(20 * zoom)}px;
                font-size: {round(fs)}pt;
                font-weight: 500;
            }}
            #UsageWidget QPushButton#UsageResetBtn:hover {{
                background-color: {hover};
                color: {text};
                border-color: {dim};
            }}
            #UsageWidget QPushButton#UsageResetBtn:pressed {{
                background-color: {hover};
            }}
            #UsageWidget QPushButton#UsageResetBtn:disabled {{
                color: {dim};
                border-color: {sep};
            }}

            {style_manager.get_scrollbar_stylesheet("#UsageWidget QScrollBar")}
        """

    def get_state(self, temp_state: bool = False) -> TabState:
        return TabState(type=self.tool_name(), tab_id=self._tab_id, path="", metadata={})

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> "UsageTab":
        return cls(state.tab_id, parent)
