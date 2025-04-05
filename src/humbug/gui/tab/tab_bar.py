from PySide6.QtWidgets import QTabBar
from PySide6.QtCore import QEvent


class TabBar(QTabBar):
    """Tab bar that can provide precise updates on hover events."""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setExpanding(False)
        self.setDocumentMode(True)
        self.setMouseTracking(True)
        self.current_hovered_tab = -1

    def event(self, event):
        """Process hover events."""
        if event.type() == QEvent.Type.HoverMove:
            pos = event.pos()
            tab_index = self.tabAt(pos)

            # Only emit signal when hovering over a new tab
            if tab_index != self.current_hovered_tab:
                if self.current_hovered_tab != -1:
                    label = self.tabButton(self.current_hovered_tab, QTabBar.LeftSide)
                    if label:
                        label.update_hover_state(False)

                if tab_index != -1:
                    label = self.tabButton(tab_index, QTabBar.LeftSide)
                    if label:
                        label.update_hover_state(True)

                self.current_hovered_tab = tab_index

        # Handle mouse leaving the widget
        elif event.type() == QEvent.Type.Leave:
            if self.current_hovered_tab != -1:
                label = self.tabButton(self.current_hovered_tab, QTabBar.LeftSide)
                if label:
                    label.update_hover_state(False)

                self.current_hovered_tab = -1

        return super().event(event)
