from typing import cast

from PySide6.QtWidgets import QTabBar, QWidget
from PySide6.QtCore import QEvent, QObject
from PySide6.QtGui import QHoverEvent, QCursor

from humbug.gui.tab.tab_label import TabLabel


class TabBar(QTabBar):
    """Tab bar that can provide precise updates on hover events."""
    def __init__(self, parent: QWidget | None = None):
        super().__init__(parent)
        self.setExpanding(False)
        self.setDocumentMode(True)
        self.setMouseTracking(True)
        self.current_hovered_tab = -1
        self.installEventFilter(self)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """
        Filter events for the text area viewport.

        Args:
            watched: The object being watched
            event: The event that occurred

        Returns:
            True if the event was handled, False to pass it along
        """
        # If we've moved the hover position then update the hover state
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

        # If the mouse leaves the tab bar, reset the hover state
        elif event.type() == QEvent.Type.Leave:
            if self.current_hovered_tab != -1:
                label = self.tabButton(self.current_hovered_tab, QTabBar.ButtonPosition.LeftSide)
                if label:
                    cast(TabLabel, label).update_hover_state(False)

                self.current_hovered_tab = -1

        # If a child is removed, update the hover state
        elif event.type() == QEvent.Type.ChildRemoved:
            if isinstance(watched, TabBar):
                current_widget = cast(TabBar, watched)
                mouse_pos = current_widget.mapFromGlobal(QCursor.pos())

                # Find which tab is under the mouse
                tab_index = current_widget.tabAt(mouse_pos)

                if tab_index != -1:
                    label = self.tabButton(tab_index, QTabBar.ButtonPosition.LeftSide)
                    if label:
                        cast(TabLabel, label).update_hover_state(True)

                self.current_hovered_tab = tab_index

        # Pass all other events to the parent class
        return super().eventFilter(watched, event)
