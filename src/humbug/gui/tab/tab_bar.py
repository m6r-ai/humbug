from typing import cast

from PySide6.QtWidgets import QTabBar, QWidget
from PySide6.QtCore import QEvent
from PySide6.QtGui import QHoverEvent

from humbug.gui.tab.tab_label import TabLabel


class TabBar(QTabBar):
    """Tab bar that can provide precise updates on hover events."""
    def __init__(self, parent: QWidget | None = None):
        super().__init__(parent)
        self.setExpanding(False)
        self.setDocumentMode(True)
        self.setMouseTracking(True)
        self.current_hovered_tab = -1

    def event(self, event: QEvent) -> bool:
        """Process hover events."""
        if event.type() == QEvent.Type.HoverMove:
            pos = cast(QHoverEvent, event).pos()
            tab_index = self.tabAt(pos)

            # Only emit signal when hovering over a new tab
            if tab_index != self.current_hovered_tab:
                if self.current_hovered_tab != -1:
                    label = self.tabButton(self.current_hovered_tab, QTabBar.ButtonPosition.LeftSide)
                    if label:
                        cast(TabLabel, label).update_hover_state(False)

                if tab_index != -1:
                    label = self.tabButton(tab_index, QTabBar.ButtonPosition.LeftSide)
                    if label:
                        cast(TabLabel, label).update_hover_state(True)

                self.current_hovered_tab = tab_index

        # Handle mouse leaving the widget
        elif event.type() == QEvent.Type.Leave:
            if self.current_hovered_tab != -1:
                label = self.tabButton(self.current_hovered_tab, QTabBar.ButtonPosition.LeftSide)
                if label:
                    cast(TabLabel, label).update_hover_state(False)

                self.current_hovered_tab = -1

        return super().event(event)
