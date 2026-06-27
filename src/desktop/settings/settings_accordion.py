"""Collapsible settings section built on the shared Accordion widget."""


from PySide6.QtWidgets import QLabel, QVBoxLayout, QWidget

from desktop.color_role import ColorRole
from desktop.settings.settings_item import SettingsItem
from desktop.widgets import Accordion


class SettingsAccordion(SettingsItem):
    """
    Collapsible accordion that groups settings under a clickable header.

    A thin SettingsItem wrapper around the shared Accordion widget: the title
    sits in the accordion header and settings items are added into its body, so
    the expand/collapse animation and chrome are shared with the rest of the app.
    """

    def __init__(self, title: str, expanded: bool = False, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self._content_items: list[SettingsItem] = []

        outer = QVBoxLayout(self)
        outer.setContentsMargins(0, 5, 0, 5)
        outer.setSpacing(0)

        self._accordion = Accordion(expanded=expanded)
        self._accordion.body_layout().setContentsMargins(14, 8, 14, 14)
        self._accordion.body_layout().setSpacing(0)

        self._title_label = QLabel(title)
        self._title_label.setObjectName("AccordionTitle")
        self._accordion.header_layout().addWidget(self._title_label, 1)

        outer.addWidget(self._accordion)

        self._on_style_changed()

    def add_content(self, item: SettingsItem) -> None:
        """Add a settings item into the collapsible content area."""
        self._accordion.body_layout().addWidget(item)
        self._content_items.append(item)
        item.value_changed.connect(self.value_changed)

    def set_label(self, text: str) -> None:
        """Set the accordion header title text."""
        self._title_label.setText(text)

    def is_modified(self) -> bool:
        return any(item.is_modified() for item in self._content_items)

    def reset_modified_state(self) -> None:
        for item in self._content_items:
            item.reset_modified_state()

    def _on_style_changed(self) -> None:
        super()._on_style_changed()

        font_pt = int(self._style_manager.base_font_size() * self._style_manager.zoom_factor())
        text_color = self._style_manager.get_color_str(ColorRole.TEXT_BRIGHT)

        self._title_label.setStyleSheet(f"""
            QLabel#AccordionTitle {{
                color: {text_color};
                font-size: {font_pt}pt;
                font-weight: bold;
                background: transparent;
                border: none;
                padding: 0px;
                margin: 0px;
            }}
        """)
