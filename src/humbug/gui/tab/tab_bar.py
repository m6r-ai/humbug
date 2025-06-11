from typing import cast

from PySide6.QtWidgets import QTabBar, QWidget
from PySide6.QtCore import QEvent, QObject
from PySide6.QtGui import QHoverEvent, QCursor, QPainter, QPaintEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.tab_label import TabLabel


class TabBar(QTabBar):
    """Tab bar that can provide precise updates on hover events and custom background painting."""

    def __init__(self, parent: QWidget | None = None):
        super().__init__(parent)
        self.setExpanding(False)
        self.setDocumentMode(True)
        self.setMouseTracking(True)
        self.current_hovered_tab = -1
        self.installEventFilter(self)
        self._style_manager = StyleManager()

    def set_tab_state(self, index: int, is_current: bool, is_updated: bool, is_active_column: bool) -> None:
        """
        Set the state for a tab to control its background painting.

        Args:
            index: Tab index
            is_current: Whether this tab is currently selected
            is_updated: Whether this tab has updated content
            is_active_column: Whether this tab is in the active column
        """
        tab_state = {
            'is_current': is_current,
            'is_updated': is_updated,
            'is_active_column': is_active_column
        }
        self.setTabData(index, tab_state)

    def _get_current_tab_index(self) -> int:
        """
        Find the index of the currently active tab.

        Returns:
            Index of the current tab, or -1 if no tab is current
        """
        for index in range(self.count()):
            tab_state = self.tabData(index)
            if isinstance(tab_state, dict) and tab_state.get('is_current', False):
                return index
        return -1

    def _should_draw_left_border(self, index: int, current_tab_index: int) -> bool:
        """
        Determine if a left border should be drawn for the given tab.

        Args:
            index: Tab index to check
            current_tab_index: Index of the currently active tab

        Returns:
            True if a left border should be drawn
        """
        # Don't draw border on the leftmost tab
        if index == 0:
            return False

        # Don't draw border if this tab is immediately to the right of the current tab
        if current_tab_index != -1 and index == current_tab_index + 1:
            return False

        return True

    def _get_tab_background_color(self, is_current: bool, is_updated: bool, is_hovered: bool) -> ColorRole:
        """
        Get the appropriate background color for a tab based on its state.

        Args:
            is_current: Whether this tab is currently selected
            is_updated: Whether this tab has updated content
            is_hovered: Whether this tab is currently hovered

        Returns:
            ColorRole for the appropriate background color
        """
        if is_hovered:
            return ColorRole.TAB_BACKGROUND_HOVER

        if is_current:
            return ColorRole.TAB_BACKGROUND_ACTIVE

        if is_updated:
            return ColorRole.TAB_BACKGROUND_UPDATED

        return ColorRole.TAB_BACKGROUND_INACTIVE

    def paintEvent(self, event: QPaintEvent) -> None:  # type: ignore[override]
        """
        Custom paint event to draw tab backgrounds based on individual tab states.

        Args:
            event: The paint event
        """
        # Create painter
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Find the currently active tab once for all border calculations
        current_tab_index = self._get_current_tab_index()

        # Paint each tab's background
        for index in range(self.count()):
            tab_rect = self.tabRect(index)
            if not tab_rect.intersects(event.rect()):
                continue  # Skip tabs not in the update region

            # Get tab state
            tab_state = self.tabData(index)
            if not isinstance(tab_state, dict):
                tab_state = {}

            is_current = tab_state.get('is_current', False)
            is_updated = tab_state.get('is_updated', False)
            is_active_column = tab_state.get('is_active_column', False)
            is_hovered = index == self.current_hovered_tab

            # Get background color
            background_color = self._get_tab_background_color(is_current, is_updated, is_hovered)
            color = self._style_manager.get_color(background_color)

            # Fill the tab background
            painter.fillRect(tab_rect, color)

            # Draw left border if conditions are met
            if self._should_draw_left_border(index, current_tab_index):
                left_border_color = self._style_manager.get_color(ColorRole.TAB_BAR_BACKGROUND)
                left_border_rect = tab_rect.adjusted(0, 0, -tab_rect.width() + 1, 0)
                painter.fillRect(left_border_rect, left_border_color)

            # Draw top border for current tabs
            if is_current:
                border_color_role = ColorRole.TAB_BORDER_ACTIVE if is_active_column else ColorRole.TAB_BACKGROUND_ACTIVE
                border_color = self._style_manager.get_color(border_color_role)

                # Draw 2px top border
                border_rect = tab_rect.adjusted(0, 0, 0, -tab_rect.height() + 2)
                painter.fillRect(border_rect, border_color)

            # Draw bottom border for all tabs
            bottom_border_color = self._style_manager.get_color(ColorRole.TAB_BAR_BACKGROUND)
            if not is_current:  # Only non-current tabs get bottom border
                bottom_border_rect = tab_rect.adjusted(0, tab_rect.height() - 1, 0, 0)
                painter.fillRect(bottom_border_rect, bottom_border_color)

        # Let Qt paint the text and other tab elements on top
        # We need to temporarily disable our custom painting to avoid infinite recursion
        try:
            # Call the parent paint method with a modified style that makes backgrounds transparent
            original_style = self.styleSheet()
            transparent_style = original_style + """
                QTabBar::tab {
                    background: transparent;
                    border: none;
                }
            """
            self.setStyleSheet(transparent_style)

            # Paint the text and other elements
            super().paintEvent(event)

            # Restore original style
            self.setStyleSheet(original_style)

        except Exception:
            # If anything goes wrong, just call the parent paint event
            super().paintEvent(event)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """
        Filter events to detect widget activation and hover state changes.

        Args:
            watched: The object being watched
            event: The event that occurred

        Returns:
            True if the event was handled, False to pass it along
        """
        # Handle hover state changes
        if event.type() == QEvent.Type.HoverMove:
            pos = cast(QHoverEvent, event).pos()
            tab_index = self.tabAt(pos)

            # Only emit signal when hovering over a new tab
            if tab_index != self.current_hovered_tab:
                old_hovered = self.current_hovered_tab
                self.current_hovered_tab = tab_index

                # Update TabLabel hover states
                if old_hovered != -1:
                    label = self.tabButton(old_hovered, QTabBar.ButtonPosition.LeftSide)
                    if label:
                        assert isinstance(label, TabLabel), "Expected TabLabel instance"
                        label.update_hover_state(False)

                if tab_index != -1:
                    label = self.tabButton(tab_index, QTabBar.ButtonPosition.LeftSide)
                    if label:
                        assert isinstance(label, TabLabel), "Expected TabLabel instance"
                        label.update_hover_state(True)

                # Trigger repaint for the affected tabs
                if old_hovered != -1:
                    self.update(self.tabRect(old_hovered))

                if tab_index != -1:
                    self.update(self.tabRect(tab_index))

        # If the mouse leaves the tab bar, reset the hover state
        elif event.type() == QEvent.Type.Leave:
            if self.current_hovered_tab != -1:
                old_hovered = self.current_hovered_tab

                # Update TabLabel hover state
                label = self.tabButton(old_hovered, QTabBar.ButtonPosition.LeftSide)
                if label:
                    assert isinstance(label, TabLabel), "Expected TabLabel instance"
                    label.update_hover_state(False)

                self.current_hovered_tab = -1

                # Trigger repaint for the previously hovered tab
                self.update(self.tabRect(old_hovered))

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
                        assert isinstance(label, TabLabel), "Expected TabLabel instance"
                        label.update_hover_state(True)

                self.current_hovered_tab = tab_index

        # Pass all other events to the parent class
        return super().eventFilter(watched, event)
