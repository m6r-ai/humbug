from typing import cast

from PySide6.QtWidgets import QTabBar, QWidget, QToolButton
from PySide6.QtCore import QEvent, QObject, Qt
from PySide6.QtGui import QHoverEvent, QCursor, QPainter, QPaintEvent, QPen, QWheelEvent

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.tab_label import TabLabel


class TabBar(QTabBar):
    """Tab bar that can provide precise updates on hover events and custom background painting."""

    def __init__(self, parent: QWidget | None = None):
        super().__init__(parent)

        # Initialize all attributes before any Qt calls that may fire events
        self.current_hovered_tab = -1
        self._style_manager = StyleManager()
        self._scroll_pixel_accumulator = 0
        self._scroll_pixels_per_step = 28

        self.setExpanding(False)
        self.setDocumentMode(True)
        self.setMouseTracking(True)
        self.setUsesScrollButtons(True)
        self.installEventFilter(self)
        self.apply_style()

    def update_tab_size(self) -> None:
        """
        Efficiently update the layout after a tab label is changed.
        """
        self.adjustSize()
        self.updateGeometry()

    def apply_style(self) -> None:
        """Apply styling to the tab bar. Native scroll buttons are hidden; nav buttons live in the corner widget."""
        self.setStyleSheet(f"""
            QTabBar {{
                border: none;
                margin: 0px;
                padding: 6px 8px 4px 8px;
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
            }}
            QTabBar::tab {{
                border: none;
                margin: 0px 4px 0px 0px;
                padding: 4px 0px 6px 0px;
            }}
            QTabBar::scroller {{
                width: 0px;
            }}
        """)
        self._hide_native_scroll_buttons()

    def _hide_native_scroll_buttons(self) -> None:
        """Hide the native scroll buttons — scrolling is driven by the corner nav widget."""
        for button in self.findChildren(QToolButton):
            if button.arrowType() in (Qt.ArrowType.LeftArrow, Qt.ArrowType.RightArrow):
                button.hide()

    def _scroll_tabs(self, direction: int) -> bool:
        """Scroll the tab strip by clicking the hidden native arrow buttons."""
        arrow_type = Qt.ArrowType.RightArrow if direction > 0 else Qt.ArrowType.LeftArrow
        for button in self.findChildren(QToolButton):
            if button.arrowType() != arrow_type:
                continue
            if not button.isEnabled():
                continue
            button.click()
            return True
        return False

    def scroll_tabs(self, direction: int) -> None:
        """Public method for the corner nav widget to trigger scrolling. direction: -1 left, +1 right."""
        self._scroll_tabs(direction)

    def _scroll_tabs_by_steps(self, steps: int) -> bool:
        """Scroll the tab strip immediately by the requested number of native steps."""
        if steps == 0:
            return False

        direction = 1 if steps > 0 else -1
        moved = False
        for _ in range(abs(steps)):
            if not self._scroll_tabs(direction):
                break
            moved = True

        return moved

    def wheelEvent(self, event: QWheelEvent) -> None:  # type: ignore[override]
        """Use wheel or trackpad gestures to scroll the tab strip without showing a scrollbar."""
        pixel_delta = event.pixelDelta()
        angle_delta = event.angleDelta()

        # Trackpad: use pixel-level accumulator for proportional, smooth scrolling
        if pixel_delta.x() != 0 or pixel_delta.y() != 0:
            px = pixel_delta.x() if pixel_delta.x() != 0 else pixel_delta.y()
            self._scroll_pixel_accumulator += px
            step_count = int(self._scroll_pixel_accumulator / self._scroll_pixels_per_step)
            if step_count != 0:
                self._scroll_pixel_accumulator -= step_count * self._scroll_pixels_per_step
                step_count = max(-4, min(4, step_count))
                if self._scroll_tabs_by_steps(-step_count):
                    event.accept()
                    return
            else:
                event.accept()
                return

        # Mouse wheel: use angle delta
        delta = angle_delta.x()
        if delta == 0:
            delta = angle_delta.y()

        if delta != 0:
            step_count = max(1, min(3, abs(delta) // 120))
            if self._scroll_tabs_by_steps(step_count if delta < 0 else -step_count):
                event.accept()
                return

        super().wheelEvent(event)

    def event(self, event: QEvent) -> bool:
        """Re-hide native scroll buttons when Qt recreates them."""
        handled = super().event(event)
        if event.type() in (QEvent.Type.Show, QEvent.Type.LayoutRequest, QEvent.Type.ChildAdded):
            self._hide_native_scroll_buttons()
        return handled

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

    def _get_tab_background_color(
        self,
        is_active_column: bool,
        is_current: bool,
        is_updated: bool,
        is_hovered: bool
    ) -> ColorRole:
        """
        Get the appropriate background color for a tab based on its state.

        Args:
            is_active_column: Whether this tab is in the active column
            is_current: Whether this tab is currently selected
            is_updated: Whether this tab has updated content
            is_hovered: Whether this tab is currently hovered

        Returns:
            ColorRole for the appropriate background color
        """
        if is_hovered and (not is_current or not is_active_column):
            return ColorRole.TAB_BACKGROUND_HOVER

        if is_updated:
            return ColorRole.TAB_BACKGROUND_UPDATED

        if is_current:
            return ColorRole.TAB_BACKGROUND_ACTIVE

        return ColorRole.TAB_BACKGROUND_INACTIVE

    def paintEvent(self, event: QPaintEvent) -> None:  # type: ignore[override]
        """
        Custom paint event to draw tab backgrounds based on individual tab states.

        Args:
            event: The paint event
        """
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.fillRect(self.rect(), self._style_manager.get_color(ColorRole.TAB_BAR_BACKGROUND))

        for index in range(self.count()):
            tab_state = self.tabData(index)
            if not isinstance(tab_state, dict):
                tab_state = {}

            is_current = tab_state.get('is_current', False)
            tab_rect = self.tabRect(index)
            if not tab_rect.intersects(event.rect()):
                continue

            is_updated = tab_state.get('is_updated', False)
            is_active_column = tab_state.get('is_active_column', False)
            is_hovered = index == self.current_hovered_tab

            background_color = self._get_tab_background_color(is_active_column, is_current, is_updated, is_hovered)
            color = self._style_manager.get_color(background_color)
            tab_pill_rect = tab_rect.adjusted(2, 3, -2, -1)

            border_color_role = ColorRole.MENU_BORDER
            if is_current:
                border_color_role = ColorRole.TAB_BORDER_ACTIVE if is_active_column else ColorRole.SPLITTER

            painter.setBrush(color)
            painter.setPen(QPen(self._style_manager.get_color(border_color_role), 1))
            painter.drawRoundedRect(tab_pill_rect, 10, 10)

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
