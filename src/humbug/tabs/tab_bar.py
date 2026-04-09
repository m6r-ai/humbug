from typing import cast

from PySide6.QtCore import QEvent, QObject, QRect, Qt
from PySide6.QtGui import QHoverEvent, QCursor, QPainter, QPaintEvent, QWheelEvent
from PySide6.QtWidgets import QTabBar, QWidget, QToolButton

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.tab_label import TabLabel


class TabBar(QTabBar):
    """Tab bar that can provide precise updates on hover events and custom background painting."""

    def __init__(self, parent: QWidget | None = None):
        super().__init__(parent)
        self.setExpanding(False)
        self.setDocumentMode(True)
        self.setMouseTracking(True)
        self._current_hovered_tab = -1
        self._scroll_pixel_accumulator = 0
        self._scroll_pixels_per_step = 32
        self._drop_index = -1
        self.installEventFilter(self)
        self._style_manager = StyleManager()

    def update_tab_size(self) -> None:
        """
        Efficiently update the layout after a tab label is changed.
        """
        self.adjustSize()
        self.updateGeometry()

    def set_drop_index(self, index: int) -> None:
        """
        Set the insertion index for an in-progress drag and trigger a repaint.

        Args:
            index: Insertion position (0..count inclusive); -1 clears the indicator.
        """
        if index != self._drop_index:
            self._drop_index = index
            self.update()

    def clear_drop_index(self) -> None:
        """Clear the drop insertion indicator and trigger a repaint."""
        if self._drop_index != -1:
            self._drop_index = -1
            self.update()

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

    def wheelEvent(self, event: QWheelEvent) -> None:
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
        # Create painter
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        rightmost_tab_right = 0

        # Calculate the top border thickness we need.  Annoyingly this has to based on whether we're full screen or
        # not because full screen has no border whereas windows have a 1px border.
        border_px = 2
        parent = self.parentWidget()
        while parent:
            if parent.isFullScreen():
                border_px = 1
                break

            parent = parent.parentWidget()

        prev_is_current = False

        # Paint each tab's background
        for index in range(self.count()):
            # Get tab state
            tab_state = self.tabData(index)
            if not isinstance(tab_state, dict):
                tab_state = {}

            is_current = tab_state.get('is_current', False)

            tab_rect = self.tabRect(index)
            if not tab_rect.intersects(event.rect()):
                prev_is_current = is_current
                continue  # Skip tabs not in the update region

            # Track the rightmost position
            rightmost_tab_right = max(rightmost_tab_right, tab_rect.right() + 1)

            is_updated = tab_state.get('is_updated', False)
            is_active_column = tab_state.get('is_active_column', False)
            is_hovered = index == self._current_hovered_tab

            # Get background color
            background_color = self._get_tab_background_color(is_active_column, is_current, is_updated, is_hovered)
            color = self._style_manager.get_color(background_color)

            # Fill the tab background
            painter.fillRect(tab_rect, color)

            # Draw left border if we're not the first tab
            if index > 0:
                left_border_color = self._style_manager.get_color(
                    ColorRole.SPLITTER if is_current or prev_is_current else ColorRole.TAB_SPLITTER
                )
                left_border_rect = tab_rect.adjusted(0, 0, -tab_rect.width() + 1, 0)
                painter.fillRect(left_border_rect, left_border_color)

            # Draw top border
            border_color_role = ColorRole.TAB_SPLITTER
            if is_current:
                if is_active_column:
                    border_color_role = ColorRole.TAB_BORDER_ACTIVE

                else:
                    border_color_role = ColorRole.SPLITTER

            border_color = self._style_manager.get_color(border_color_role)
            border_rect = tab_rect.adjusted(0, 0, 0, -tab_rect.height() + border_px)
            painter.fillRect(border_rect, border_color)

            # Draw bottom border for all tabs
            bottom_border_color = self._style_manager.get_color(ColorRole.SPLITTER)
            if not is_current:  # Only non-current tabs get bottom border
                bottom_border_rect = tab_rect.adjusted(0, tab_rect.height() - 1, 0, 0)
                painter.fillRect(bottom_border_rect, bottom_border_color)

            prev_is_current = is_current

        # Paint right edge of rightmost tab and bottom edge of empty space
        if self.count() > 0 and rightmost_tab_right < self.width():
            splitter_color = self._style_manager.get_color(
                ColorRole.SPLITTER if prev_is_current else ColorRole.TAB_SPLITTER
            )

            # Paint right edge of the rightmost tab
            right_edge_rect = self.rect()
            right_edge_rect.setLeft(rightmost_tab_right)
            right_edge_rect.setWidth(1)
            painter.fillRect(right_edge_rect, splitter_color)

            # Paint bottom edge of empty space to the right
            if rightmost_tab_right + 1 < self.width():
                empty_space_bottom_rect = self.rect()
                empty_space_bottom_rect.setLeft(rightmost_tab_right)
                empty_space_bottom_rect.setTop(self.height() - 1)
                empty_space_bottom_rect.setHeight(1)
                colour = self._style_manager.get_color(ColorRole.SPLITTER)
                painter.fillRect(empty_space_bottom_rect, colour)

        # Let Qt paint the text and other tab elements on top
        painter.end()
        super().paintEvent(event)

        # Draw the drop insertion bar on top of everything else.
        if self._drop_index != -1:
            bar_color = self._style_manager.get_color(ColorRole.TAB_BORDER_ACTIVE)
            bar_width = 2

            if self._drop_index < self.count():
                # Draw at the left edge of the tab at drop_index.
                tab_rect = self.tabRect(self._drop_index)
                bar_x = tab_rect.left()
            elif self.count() > 0:
                # Append: draw at the right edge of the last tab.
                tab_rect = self.tabRect(self.count() - 1)
                bar_x = tab_rect.right() + 1
            else:
                # Empty tab bar: draw at the left edge.
                bar_x = 0

            bar_painter = QPainter(self)
            bar_rect = QRect(bar_x, 0, bar_width, self.height())
            bar_painter.fillRect(bar_rect, bar_color)
            bar_painter.end()

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
            if tab_index != self._current_hovered_tab:
                old_hovered = self._current_hovered_tab
                self._current_hovered_tab = tab_index

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
            if self._current_hovered_tab != -1:
                old_hovered = self._current_hovered_tab

                # Update TabLabel hover state
                label = self.tabButton(old_hovered, QTabBar.ButtonPosition.LeftSide)
                if label:
                    assert isinstance(label, TabLabel), "Expected TabLabel instance"
                    label.update_hover_state(False)

                self._current_hovered_tab = -1

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

                self._current_hovered_tab = tab_index

        # Pass all other events to the parent class
        return super().eventFilter(watched, event)
