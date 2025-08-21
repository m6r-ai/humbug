"""Auto-scrolling scroll area for mindspace tree views."""

from PySide6.QtCore import QTimer, QPoint
from PySide6.QtWidgets import QScrollArea
from PySide6.QtGui import QCursor


class MindspaceTreeScrollArea(QScrollArea):
    """QScrollArea subclass that provides auto-scrolling functionality for tree views during drag operations."""

    def __init__(self, parent=None):
        """Initialize the auto-scrolling scroll area."""
        super().__init__(parent)

        # Auto-scroll timer for drag operations
        self._auto_scroll_timer = QTimer()
        self._auto_scroll_timer.timeout.connect(self._perform_auto_scroll)
        self._auto_scroll_timer.setInterval(30)
        self._scroll_direction: int = 0  # -1 for up, 1 for down, 0 for no scroll
        self._scroll_speed: int = 0  # Pixels to scroll per timer interval

        # Auto-scroll configuration
        self._scroll_zone_size = 30  # Pixels from edge to trigger scrolling
        self._min_scroll_speed = 8  # Minimum scroll speed
        self._max_scroll_speed = 25  # Maximum scroll speed

    def start_auto_scroll(self, start_scrolling: bool) -> None:
        """
        Start auto-scrolling in the specified direction.
        
        This method is called by child widgets via signal, but we override the speed
        calculation to use our own coordinate system.

        Args:
            start_scrolling: Whether to start or stop scrolling
        """
        if not start_scrolling:
            self.stop_auto_scroll()
            return

        if not self._auto_scroll_timer.isActive():
            self._auto_scroll_timer.start()

    def stop_auto_scroll(self) -> None:
        """Stop auto-scrolling."""
        if self._auto_scroll_timer.isActive():
            self._auto_scroll_timer.stop()

        self._scroll_direction = 0
        self._scroll_speed = 0

    def _get_scroll_direction_and_speed(self, global_pos: QPoint | None = None) -> tuple[int, int]:
        """
        Calculate scroll direction and speed based on mouse position relative to this scroll area.

        Args:
            global_pos: Global mouse position, if None uses current cursor position

        Returns:
            Tuple of (direction, speed) where direction is -1/0/1 and speed is pixels per scroll
        """
        if global_pos is None:
            global_pos = QCursor.pos()

        # Convert global position to scroll area coordinates
        scroll_area_pos = self.mapFromGlobal(global_pos)
        scroll_area_rect = self.rect()

        # Calculate distance from scroll area edges
        distance_from_top = scroll_area_pos.y()
        distance_from_bottom = scroll_area_pos.y() - scroll_area_rect.height()

        # Check if we should scroll up (mouse above or near top of scroll area)
        if distance_from_top < self._scroll_zone_size:
            # Calculate how far we are from the scroll zone
            if distance_from_top < 0:
                # Mouse is above scroll area - use distance above for speed calculation
                distance_out = min(-distance_from_top, scroll_area_rect.height())
                speed_factor = min(1.0, distance_out / self._scroll_zone_size)

            else:
                # Mouse is in the top scroll zone
                distance_out = self._scroll_zone_size - distance_from_top
                speed_factor = distance_out / self._scroll_zone_size

            speed = int(self._min_scroll_speed + (self._max_scroll_speed - self._min_scroll_speed) * speed_factor)
            return -1, speed

        # Check if we should scroll down (mouse below or near bottom of scroll area)
        if distance_from_bottom > -self._scroll_zone_size:
            # Calculate how far we are from the scroll zone
            if distance_from_bottom > 0:
                # Mouse is below scroll area - use distance below for speed calculation
                distance_out = min(distance_from_bottom, scroll_area_rect.height())
                speed_factor = min(1.0, distance_out / self._scroll_zone_size)

            else:
                # Mouse is in the bottom scroll zone
                distance_out = distance_from_bottom + self._scroll_zone_size
                speed_factor = distance_out / self._scroll_zone_size

            speed = int(self._min_scroll_speed + (self._max_scroll_speed - self._min_scroll_speed) * speed_factor)
            return 1, speed

        # Not in scroll zone
        return 0, 0

    def _perform_auto_scroll(self) -> None:
        """Perform the actual scrolling operation."""
        # Recalculate direction and speed on each timer tick in case mouse moved
        direction, speed = self._get_scroll_direction_and_speed()
        if direction == 0:
            self.stop_auto_scroll()
            return

        # Update current direction and speed
        self._scroll_direction = direction
        self._scroll_speed = speed

        # Get the vertical scroll bar
        scroll_bar = self.verticalScrollBar()
        if not scroll_bar:
            self.stop_auto_scroll()
            return

        current_value = scroll_bar.value()

        if self._scroll_direction < 0:
            new_value = max(scroll_bar.minimum(), current_value - self._scroll_speed)

        else:
            new_value = min(scroll_bar.maximum(), current_value + self._scroll_speed)

        # Only scroll if we can actually move
        if new_value != current_value:
            scroll_bar.setValue(new_value)

        else:
            # We've hit the boundary, stop scrolling
            self.stop_auto_scroll()
