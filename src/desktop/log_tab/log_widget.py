"""Log widget implementation for displaying mindspace message log."""

import logging
import re
from typing import Any

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy
)
from PySide6.QtCore import QTimer, QPoint, Qt, Signal, QObject
from PySide6.QtGui import QCursor, QGuiApplication, QResizeEvent

from mindspace.mindspace_message import MindspaceMessage
from mindspace.mindspace_interactions import MindspaceInteractions

from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from desktop.log_tab.log_message import LogMessage
from desktop.widgets import SMOOTH_SCROLL_DURATION_MS, SMOOTH_SCROLL_INTERVAL_MS


class LogWidget(QWidget):
    """Widget for displaying mindspace message log with message history."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signal to request scrolling to a specific widget and position
    scroll_requested = Signal(QWidget, int)  # Widget to scroll to, position within widget

    # Signal to notify tab when content is updated while user is scrolled up
    update_label = Signal()

    # Emits when the has-seen-latest-update state changes
    has_seen_latest_update_changed = Signal(bool)

    VIEW_PAGE_SIZE = 500

    MAX_FIND_MATCHES = 500

    FIND_BATCH_SIZE = 500

    MAX_LOADED_PAGES = 4

    def __init__(
        self,
        parent: QWidget | None = None,
    ) -> None:
        """
        Initialize the log widget.

        Args:
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("LogWidget")
        self.setObjectName("LogWidget")

        self._mindspace_manager = MindspaceManager()
        self._mindspace_manager.interactions_updated.connect(self.load_messages)

        self._style_manager = StyleManager()

        # Widget tracking
        self._messages: list[LogMessage] = []
        self._message_with_selection: LogMessage | None = None

        # Initialize tracking variables
        self._auto_scroll = True
        self._pending_anchor: tuple[int, int] | None = None
        self._window_extended = False

        # Create layout
        log_layout = QVBoxLayout(self)
        self.setLayout(log_layout)
        log_layout.setContentsMargins(0, 0, 0, 0)
        log_layout.setSpacing(0)

        # Set up the scroll area
        self._scroll_area = QScrollArea()
        self._scroll_area.setFrameStyle(0)
        self._scroll_area.setWidgetResizable(True)
        self._scroll_area.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Create messages container widget
        self._messages_container = QWidget()
        self._messages_layout = QVBoxLayout(self._messages_container)
        self._messages_container.setLayout(self._messages_layout)

        spacing = int(self._style_manager.message_bubble_spacing())
        self._messages_layout.setSpacing(spacing)
        self._messages_layout.setContentsMargins(spacing, spacing, spacing, spacing)
        self._messages_layout.addStretch()

        self._messages_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setWidget(self._messages_container)

        self._scroll_area.viewport().setAutoFillBackground(True)
        log_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights: dict[LogMessage, list[tuple[int, int]]] = {}

        # Tracking for spotlighted message
        self._spotlighted_message_index = -1

        self._language_manager = LanguageManager()

        # Create timer for scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos: QPoint | None = None

        # Timer for smooth animated scrolling
        self._smooth_scroll_timer = QTimer(self)
        self._smooth_scroll_timer.setInterval(SMOOTH_SCROLL_INTERVAL_MS)
        self._smooth_scroll_timer.timeout.connect(self._update_smooth_scroll)

        self._deferred_scroll_timer = QTimer(self)
        self._deferred_scroll_timer.setSingleShot(True)
        self._deferred_scroll_timer.setInterval(0)
        self._deferred_scroll_timer.timeout.connect(self._on_deferred_scroll)
        self._deferred_scroll_position: int = 0
        self._smooth_scroll_target: int = 0
        self._smooth_scroll_start: int = 0
        self._smooth_scroll_distance: int = 0
        self._smooth_scroll_duration: int = SMOOTH_SCROLL_DURATION_MS
        self._smooth_scroll_time: int = 0

        # Setup context menu
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_log_context_menu)

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)


        # Find functionality.  Matches are found by searching the log store, so they
        # cover entries the view is not showing; each match is identified by the
        # sequence number of its entry and its ordinal within that entry.
        self._find_matches: list[tuple[int, int]] = []
        self._find_truncated = False
        self._current_match_index = -1
        self._last_search: tuple = ("", False, False)
        self._highlighted_widgets: set[LogMessage] = set()

        self._find_scroll_timer = QTimer(self)
        self._find_scroll_timer.setSingleShot(True)
        self._find_scroll_timer.setInterval(0)
        self._find_scroll_timer.timeout.connect(self._apply_find_scroll)
        self._pending_find_scroll: tuple[int, int] | None = None

        # True while the view follows the end of the log; False once a find has
        # pinned it to an older page.
        self._showing_tail = True

        # Load messages when initialized
        if self._mindspace_manager.has_mindspace():
            self.load_messages()

    def load_messages(self) -> None:
        """
        Load the interaction log entries that are not yet in the view.

        The log store is unbounded, so the view holds at most VIEW_PAGE_SIZE entries
        and reads only the entries appended since the last load.  Entries that fall
        off the front of the page are discarded, which is what keeps the number of
        widgets bounded however long the log grows.
        """
        if not self._mindspace_manager.has_mindspace():
            return

        store = self._mindspace_manager.interactions()
        total = store.count()

        if not self._messages or total < self._messages[-1].sequence():
            self._showing_tail = True
            self._load_page(store, total)
            return

        if not self._showing_tail:
            # The view is pinned to an older page by a find; only the status changes.
            self.status_updated.emit()
            return

        last_loaded = self._messages[-1].sequence()
        appended = store.read(last_loaded + 1, total - last_loaded)
        if not appended:
            return

        for message in appended:
            self._add_log_message(message)

        self._trim_to_page()

        if self._auto_scroll:
            self._scroll_to_bottom()

        else:
            # User is scrolled up, notify them there's new content below
            self.update_label.emit()

        self.status_updated.emit()

    def _load_page(self, store: MindspaceInteractions, total: int) -> None:
        """
        Rebuild the view with the newest page of entries.

        Args:
            store: The interaction log store to read from.
            total: Number of entries in the log.
        """
        self._clear_messages()

        self._showing_tail = True
        if total == 0:
            self.status_updated.emit()
            return

        page_start = max(1, total - self.VIEW_PAGE_SIZE + 1)
        for message in store.read(page_start, self.VIEW_PAGE_SIZE):
            self._add_log_message(message)

        self._auto_scroll = True
        self._scroll_to_bottom()
        self.status_updated.emit()

    def page_status(self) -> tuple[int, int, int]:
        """
        Return the range of entries the view is showing and the log's total.

        Returns:
            Tuple of (first_sequence, last_sequence, total_entries), with the
            sequence range reported as (0, 0) when the view is empty.
        """
        if not self._mindspace_manager.has_mindspace():
            return 0, 0, 0

        total = self._mindspace_manager.interactions().count()
        if not self._messages:
            return 0, 0, total

        return self._messages[0].sequence(), self._messages[-1].sequence(), total

    def _trim_to_page(self) -> None:
        """Discard the oldest entries once the view holds more pages than it keeps."""
        limit = self.VIEW_PAGE_SIZE
        if self._window_extended:
            limit = self.MAX_LOADED_PAGES * self.VIEW_PAGE_SIZE

        excess = len(self._messages) - limit
        if excess <= 0:
            return

        if not self._auto_scroll:
            self._anchor_view()

        for _ in range(excess):
            message = self._messages.pop(0)
            self._messages_layout.removeWidget(message)
            message.deleteLater()

        if self._message_with_selection not in self._messages:
            self._message_with_selection = None

        if self._spotlighted_message_index != -1:
            self._spotlighted_message_index = max(-1, self._spotlighted_message_index - excess)

        self._highlighted_widgets.clear()

    def _clear_messages(self) -> None:
        """Remove every message widget from the view."""
        for msg in self._messages:
            self._messages_layout.removeWidget(msg)
            msg.deleteLater()

        self._messages.clear()
        self._message_with_selection = None
        self._spotlighted_message_index = -1
        self._highlighted_widgets.clear()
        self._window_extended = False

    def _add_log_message(self, message: MindspaceMessage) -> None:
        """Add a message widget below the ones already shown."""
        msg_widget = self._create_log_message(message)

        # Add widget before the stretch
        self._messages_layout.insertWidget(self._messages_layout.count() - 1, msg_widget)
        self._messages.append(msg_widget)

    def _prepend_log_message(self, message: MindspaceMessage) -> None:
        """Add a message widget above the ones already shown."""
        msg_widget = self._create_log_message(message)
        self._messages_layout.insertWidget(0, msg_widget)
        self._messages.insert(0, msg_widget)

        if self._spotlighted_message_index != -1:
            self._spotlighted_message_index += 1

    def _create_log_message(self, message: MindspaceMessage) -> LogMessage:
        """
        Create the widget for one log entry.

        Args:
            message: Entry to create a widget for.

        Returns:
            The widget, wired up to this widget's selection and scroll handling.
        """
        msg_widget = LogMessage(
            message.content,
            message.level,
            message.timestamp,
            message.sequence,
            message.context,
            self
        )
        msg_widget.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(msg_widget, has_selection)
        )
        msg_widget.scroll_requested.connect(self._on_scroll_requested)
        msg_widget.mouse_released.connect(self._stop_scroll)

        return msg_widget

    def _on_scroll_requested(self, mouse_pos: QPoint) -> None:
        """Begin scroll handling for selection drag."""
        viewport_pos = self._scroll_area.viewport().mapFromGlobal(mouse_pos)

        if not self._scroll_timer.isActive():
            self._scroll_timer.start()

        self._last_mouse_pos = viewport_pos

    def _stop_scroll(self) -> None:
        """Stop any ongoing selection scrolling."""
        if self._scroll_timer.isActive():
            self._scroll_timer.stop()

        self._last_mouse_pos = None

    def _update_scroll(self) -> None:
        """Update scroll position based on mouse position."""
        if self._last_mouse_pos is None:
            self._scroll_timer.stop()
            return

        viewport = self._scroll_area.viewport()
        scrollbar = self._scroll_area.verticalScrollBar()
        current_val = scrollbar.value()
        viewport_height = viewport.height()

        # Calculate scroll amount based on distance from viewport edges
        if self._last_mouse_pos.y() < 0:
            # Above viewport
            distance_out = -self._last_mouse_pos.y()
            if distance_out > viewport_height * 2:
                scrollbar.setValue(scrollbar.minimum())

            else:
                screen = QGuiApplication.screenAt(QCursor.pos())
                if screen is None or QCursor.pos().y() <= screen.availableGeometry().top() + 4:
                    distance_out = 250

                scroll_amount = min(50, max(10, distance_out // 5))
                new_val = max(scrollbar.minimum(), current_val - scroll_amount)
                scrollbar.setValue(new_val)

        elif self._last_mouse_pos.y() > viewport_height:
            # Below viewport
            distance_out = self._last_mouse_pos.y() - viewport_height
            if distance_out > viewport_height * 2:
                scrollbar.setValue(scrollbar.maximum())

            else:
                screen = QGuiApplication.screenAt(QCursor.pos())
                if screen is None or QCursor.pos().y() >= screen.availableGeometry().bottom() - 4:
                    distance_out = 250

                scroll_amount = min(50, max(10, distance_out // 5))
                new_val = min(scrollbar.maximum(), current_val + scroll_amount)
                scrollbar.setValue(new_val)

        # Update mouse position
        self._last_mouse_pos = self._scroll_area.viewport().mapFromGlobal(QCursor.pos())

    def _start_smooth_scroll(self, target_value: int) -> None:
        """
        Start smooth scrolling animation to target value.

        Args:
            target_value: Target scroll position
        """
        scrollbar = self._scroll_area.verticalScrollBar()

        # If we're already scrolling, stop the current animation
        if self._smooth_scroll_timer.isActive():
            self._smooth_scroll_timer.stop()

        # Set up the animation parameters
        self._smooth_scroll_start = scrollbar.value()
        self._smooth_scroll_target = target_value
        self._smooth_scroll_distance = target_value - self._smooth_scroll_start
        self._smooth_scroll_time = 0

        # Start the animation timer
        self._smooth_scroll_timer.start()

    def _update_smooth_scroll(self) -> None:
        """Update the smooth scrolling animation."""
        self._smooth_scroll_time += self._smooth_scroll_timer.interval()
        progress = min(1.0, self._smooth_scroll_time / self._smooth_scroll_duration)
        t = 1 - (1 - progress) ** 3

        # Add 0.5 lines of bias so that int() truncation crosses each pixel boundary
        # slightly early, avoiding a visible "jump" at the very end of the animation
        # where the easing curve decelerates so slowly that the final line is only
        # reached on the last tick, well after the scroll appears to have stopped.
        new_position = min(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t + 0.5),
        ) if self._smooth_scroll_distance > 0 else max(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t - 0.5),
        )
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(new_position)
        if progress >= 1.0 or new_position == self._smooth_scroll_target:
            self._smooth_scroll_timer.stop()

    def _on_scroll_value_changed(self, value: int) -> None:
        """
        Handle scroll value changes to detect user scrolling.

        Args:
            value (int): The new scroll value
        """
        vbar = self._scroll_area.verticalScrollBar()

        at_bottom = value == vbar.maximum()
        self._auto_scroll = at_bottom

        self.has_seen_latest_update_changed.emit(at_bottom)

        if value == vbar.minimum() and not self._auto_scroll:
            self._load_earlier()

        if at_bottom and not self._showing_tail:
            self._load_later()

    def _on_scroll_range_changed(self, _minimum: int, _maximum: int) -> None:
        """
        Handle the scroll range changing.

        Args:
            _minimum: New minimum scroll value.
            _maximum: New maximum scroll value.
        """
        if self._auto_scroll:
            self._scroll_to_bottom()

    def _scroll_to_bottom(self) -> None:
        """Scroll to the bottom of the content."""
        if self._pending_anchor is not None:
            self._pending_anchor = None
            self._deferred_scroll_timer.stop()

        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())

    def _on_deferred_scroll(self) -> None:
        """Restore scroll position after layout has settled."""
        scrollbar = self._scroll_area.verticalScrollBar()
        anchor = self._pending_anchor
        self._pending_anchor = None
        if anchor is not None:
            sequence, offset = anchor
            for widget in self._messages:
                if widget.sequence() != sequence:
                    continue

                scrollbar.setValue(widget.mapTo(self._messages_container, QPoint(0, 0)).y() + offset)
                return

            return

        scrollbar.setValue(self._deferred_scroll_position)

    def _anchor_view(self) -> None:
        """
        Keep the reader on the same entry while entries are added or removed.

        The entry at the top of the viewport and how far into it the viewport
        starts are remembered, and the scroll position is put back to them once
        the layout that follows the change has settled.
        """
        scroll_value = self._scroll_area.verticalScrollBar().value()

        for widget in self._messages:
            top = widget.mapTo(self._messages_container, QPoint(0, 0)).y()
            if top + widget.height() > scroll_value:
                self._pending_anchor = (widget.sequence(), scroll_value - top)
                self._deferred_scroll_timer.start()
                return

    def _load_earlier(self) -> None:
        """
        Load the page of entries before the ones on show.
        Loading stops once the view holds MAX_LOADED_PAGES pages; the status bar
        reports the range on show, so the reader can see how far back they are.
        """
        if not self._messages or not self._mindspace_manager.has_mindspace():
            return

        first = self._messages[0].sequence()
        if first == 1 or len(self._messages) >= self.MAX_LOADED_PAGES * self.VIEW_PAGE_SIZE:
            return

        store = self._mindspace_manager.interactions()
        page_start = max(1, first - self.VIEW_PAGE_SIZE)
        messages = store.read(page_start, first - page_start)
        if not messages:
            return

        self._anchor_view()
        for message in reversed(messages):
            self._prepend_log_message(message)

        self._window_extended = True
        self.status_updated.emit()

    def _load_later(self) -> None:
        """
        Load the page of entries after the ones on show.

        This is how the reader gets from a page they arrived at by searching back to
        the end of the log.  The view stays on the entries being read and the newly
        loaded page is added below them.
        """
        if not self._messages or not self._mindspace_manager.has_mindspace():
            return

        store = self._mindspace_manager.interactions()
        total = store.count()
        last = self._messages[-1].sequence()
        if last >= total:
            return

        messages = store.read(last + 1, self.VIEW_PAGE_SIZE)
        if not messages:
            return

        self._anchor_view()
        self._auto_scroll = False
        for message in messages:
            self._add_log_message(message)

        self._window_extended = True
        self._showing_tail = self._messages[-1].sequence() == total
        self._trim_to_page()
        self.status_updated.emit()

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Handle tab activation state changes.

        Args:
            widget: The widget that triggered the activation change
            active: True if the tab is now active, False otherwise
        """
        if not active:
            self._deactivate_widget(widget)
            return

        self._activate_widget(widget)

    def _activate_widget(self, widget: QWidget) -> None:
        """
        Handle activation of a widget, spotlighting the associated message.

        Args:
            widget: The widget that was activated
        """
        # Find the LogMessage that contains this widget
        message_widget = self._find_log_message(widget)
        if message_widget is None:
            # We couldn't find it so active the last spotlighted message or input
            if self._spotlighted_message_index != -1:
                self._messages[self._spotlighted_message_index].set_spotlighted(True)
                self._messages[self._spotlighted_message_index].setFocus()
                return

            # Otherwise spotlight the first message if available
            if self._messages:
                self._spotlighted_message_index = 0
                self._messages[0].set_spotlighted(True)
                self._messages[0].setFocus()

            return

        if message_widget.is_spotlighted():
            return

        # Remove spotlight from the currently spotlighted message
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)

        # Set spotlight on the new message
        if message_widget in self._messages:
            self._spotlighted_message_index = self._messages.index(message_widget)
            message_widget.set_spotlighted(True)

    def _deactivate_widget(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget, checking if spotlight is leaving the associated message.

        Args:
            widget: The widget that lost focus
        """
        # Find the LogMessage that contains this widget
        message_widget = self._find_log_message(widget)
        if message_widget is None:
            return

        # Remove spotlight from the currently spotlighted message
        if self._spotlighted_message_index != -1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)

    def _find_log_message(self, widget: QWidget) -> LogMessage | None:
        """
        Find the LogMessage that contains the given widget.

        Args:
            widget: The widget to find the containing LogMessage for

        Returns:
            The LogMessage containing the widget, or None if not found
        """
        current: QObject = widget
        while current:
            if isinstance(current, LogMessage):
                return current

            current = current.parent()

        return None

    def navigate_to_next_message(self) -> bool:
        """Navigate to the next message if possible."""
        if not self._messages:
            return False

        # If no message is spotlighted, spotlight the first one
        if self._spotlighted_message_index == -1:
            self._spotlighted_message_index = 0
            self._spotlight_message()
            return True

        # Otherwise move to next message if possible
        if self._spotlighted_message_index < len(self._messages) - 1:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index += 1
            self._spotlight_message()
            return True

        return False

    def navigate_to_previous_message(self) -> bool:
        """Navigate to the previous message if possible."""
        if not self._messages:
            return False

        # If no message is spotlighted, spotlight the last one
        if self._spotlighted_message_index == -1:
            self._spotlighted_message_index = len(self._messages) - 1
            self._spotlight_message()
            return True

        # Otherwise move to previous message if possible
        if self._spotlighted_message_index > 0:
            self._messages[self._spotlighted_message_index].set_spotlighted(False)
            self._spotlighted_message_index -= 1
            self._spotlight_message()
            return True

        return False

    def _spotlight_message(self) -> None:
        """Spotlight the specified message."""
        index = self._spotlighted_message_index
        if 0 <= index < len(self._messages):
            self._messages[index].set_spotlighted(True)
            self._messages[index].setFocus()
            self._scroll_to_message(self._messages[index])

    def _perform_scroll_to_position(self, message: LogMessage, y_offset: int) -> None:
        """
        Scroll to position a message at a specific Y offset from the top of viewport.

        This is a low-level helper that performs the actual scrolling operation.

        Args:
            message: Message widget to scroll to
            y_offset: Offset from top of viewport (positive values move message down from top)
        """
        message_pos = message.mapTo(self._messages_container, QPoint(0, 0))
        scroll_value = message_pos.y() - y_offset

        # Clamp to valid range
        scrollbar = self._scroll_area.verticalScrollBar()
        scroll_value = max(scrollbar.minimum(), min(scrollbar.maximum(), scroll_value))

        self._start_smooth_scroll(scroll_value)

    def _scroll_to_message(self, message: LogMessage) -> None:
        """Ensure the message is visible in the scroll area."""
        # Get the position of the message in the scroll area
        message_pos = message.mapTo(self._messages_container, QPoint(0, 0))

        # Calculate the visible region
        scroll_value = self._scroll_area.verticalScrollBar().value()
        viewport_height = self._scroll_area.viewport().height()

        delta = message_pos.y() - scroll_value

        message_spacing = int(self._style_manager.message_bubble_spacing())

        # Determine if scrolling is needed
        if delta < 0:
            # Message is above visible area
            self._perform_scroll_to_position(message, message_spacing)

        elif delta + message.height() > viewport_height:
            # Message is below visible area
            if message.height() > viewport_height:
                # Message is taller than viewport, position at top
                self._perform_scroll_to_position(message, message_spacing)

            else:
                # Message fits in viewport, position at bottom
                bottom_offset = viewport_height - message.height() - message_spacing
                self._perform_scroll_to_position(message, bottom_offset)

    def can_navigate_next_message(self) -> bool:
        """Check if navigation to next message is possible."""
        return (
            len(self._messages) > 0 and
            (self._spotlighted_message_index == -1 or self._spotlighted_message_index < len(self._messages) - 1)
        )

    def can_navigate_previous_message(self) -> bool:
        """Check if navigation to previous message is possible."""
        return (
            len(self._messages) > 0 and
            (self._spotlighted_message_index == -1 or self._spotlighted_message_index > 0)
        )

    def _on_selection_changed(self, message_widget: LogMessage, has_selection: bool) -> None:
        """Handle selection changes in message widgets."""
        if not has_selection:
            if self._message_with_selection:
                msg = self._message_with_selection
                self._message_with_selection = None
                msg.clear_selection()

            return

        if self._message_with_selection and self._message_with_selection != message_widget:
            self._message_with_selection.clear_selection()

        self._message_with_selection = message_widget

    def has_selection(self) -> bool:
        """Check if any message has selected text."""
        return self._message_with_selection is not None and self._message_with_selection.has_selection()

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_bottom()

    def apply_style(self) -> None:
        """Apply the current style to this widget."""
        zoom_factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)

        for message in self._messages:
            message.apply_style()

    def _show_log_context_menu(self, pos: QPoint) -> None:
        """
        Create and show the context menu at the given position.

        Args:
            pos: Local coordinates for menu position
        """
        menu = self._style_manager.create_menu(self)
        strings = self._language_manager.strings()

        # Copy action
        copy_action = menu.addAction(strings.copy)
        copy_action.setEnabled(self.has_selection())
        copy_action.triggered.connect(self.copy)

        # Show menu at click position
        menu.exec_(self.mapToGlobal(pos))

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self.has_selection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        if self._message_with_selection:
            self._message_with_selection.copy_selection()

    def create_view_state(self) -> dict[str, Any]:
        """
        Create a dictionary capturing the log view's state.

        Returns:
            Dictionary containing the scroll position and the sequence number of the
            spotlighted message, or -1 if no message is spotlighted.
        """
        sequence = -1
        if 0 <= self._spotlighted_message_index < len(self._messages):
            sequence = self._messages[self._spotlighted_message_index].sequence()

        return {
            "scroll_position": self._scroll_area.verticalScrollBar().value(),
            "spotlighted_message_sequence": sequence,
        }

    def restore_view_state(self, state: dict[str, Any]) -> None:
        """
        Restore the log view's state.

        Args:
            state: Dictionary produced by create_view_state.
        """
        if not state:
            return

        # Refresh messages if we have a mindspace
        if self._mindspace_manager.has_mindspace():
            self.load_messages()

        # Restore scroll position if specified
        if "scroll_position" in state:
            # Use a timer to ensure the scroll happens after layout is complete
            self._deferred_scroll_position = state["scroll_position"]
            self._deferred_scroll_timer.start()

        self._restore_spotlight(state.get("spotlighted_message_sequence", -1))

    def _restore_spotlight(self, sequence: int) -> None:
        """
        Spotlight the message with the given sequence number, if it is loaded.

        Args:
            sequence: Sequence number of the message to spotlight, or -1 for none.
        """
        self._spotlighted_message_index = -1
        for index, message in enumerate(self._messages):
            if message.sequence() != sequence:
                continue

            self._spotlighted_message_index = index
            message.set_spotlighted(True)
            message.setFocus()
            return

    def get_selected_text(self) -> str:
        """
        Get current selected text if any.

        Returns:
            The selected text or empty string
        """
        if self._message_with_selection:
            return self._message_with_selection.get_selected_text()

        return ""

    def find_text(
        self, text: str, forward: bool = True, case_sensitive: bool = False, regexp: bool = False
    ) -> tuple[int, int, bool]:
        """
        Find text across the whole log and move to the next or previous match.

        Every record in the log is searched, not only the entries the view is
        showing, so a search cannot miss an entry that lies outside the current
        page.  Showing a match loads the page of entries containing it.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.

        Returns:
            Tuple of (current_match, total_matches, truncated)
        """
        if (text, case_sensitive, regexp) != self._last_search:
            self._reset_find()
            self._last_search = (text, case_sensitive, regexp)

        if not text:
            self._return_to_tail()
            return 0, 0, False

        if not self._find_matches:
            self._find_matches, self._find_truncated = self._search_log(
                text, case_sensitive, regexp
            )

        if not self._find_matches:
            return 0, 0, False

        self._current_match_index = self._next_match_index(forward)
        self._show_current_match()
        return self.get_match_status()

    def _search_log(
        self, text: str, case_sensitive: bool, regexp: bool
    ) -> tuple[list[tuple[int, int]], bool]:
        """
        Search every entry in the log.

        Args:
            text: Text or pattern to search for
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.

        Returns:
            The (sequence, ordinal) of every match, and whether the match limit was
            reached before the whole log had been searched.
        """
        if not self._mindspace_manager.has_mindspace():
            return [], False

        try:
            pattern = re.compile(
                text if regexp else re.escape(text),
                0 if case_sensitive else re.IGNORECASE
            )

        except re.error:
            return [], False

        store = self._mindspace_manager.interactions()
        total = store.count()
        matches: list[tuple[int, int]] = []
        sequence = 1

        while sequence <= total:
            batch = store.read(sequence, self.FIND_BATCH_SIZE)
            if not batch:
                break

            for message in batch:
                for ordinal in range(sum(1 for _ in pattern.finditer(message.content))):
                    matches.append((message.sequence, ordinal))
                    if len(matches) >= self.MAX_FIND_MATCHES:
                        return matches, True

            sequence = batch[-1].sequence + 1

        return matches, False

    def _next_match_index(self, forward: bool) -> int:
        """
        Return the index of the match to move to, wrapping at either end.

        Args:
            forward: Whether to move forward rather than back.
        """
        last = len(self._find_matches) - 1
        if self._current_match_index == -1:
            return 0 if forward else last

        index = self._current_match_index + (1 if forward else -1)
        if index > last:
            return 0

        if index < 0:
            return last

        return index

    def _show_current_match(self) -> None:
        """Load the page containing the current match and scroll to it."""
        self._auto_scroll = False
        sequence, _ = self._find_matches[self._current_match_index]
        self._ensure_page_shows(sequence)
        self._highlight_page_matches()

        self._pending_find_scroll = self._find_matches[self._current_match_index]
        self._find_scroll_timer.start()

    def _ensure_page_shows(self, sequence: int) -> None:
        """
        Load a page of entries containing the given sequence number.

        Args:
            sequence: Sequence number that the view must contain.
        """
        first = self._messages[0].sequence() if self._messages else 0
        last = self._messages[-1].sequence() if self._messages else 0
        if first <= sequence <= last:
            return

        store = self._mindspace_manager.interactions()
        page_start = max(1, sequence - self.VIEW_PAGE_SIZE // 2)
        total = store.count()

        self._clear_messages()
        for message in store.read(page_start, self.VIEW_PAGE_SIZE):
            self._add_log_message(message)

        self._showing_tail = bool(self._messages) and self._messages[-1].sequence() == total
        self._auto_scroll = False
        self.status_updated.emit()

    def _highlight_page_matches(self) -> None:
        """Highlight the matches held by the entries the view is showing."""
        self._clear_highlights()
        if not self._find_matches or self._current_match_index == -1:
            return

        text, case_sensitive, regexp = self._last_search
        current_sequence, current_ordinal = self._find_matches[self._current_match_index]
        matching_sequences = {sequence for sequence, _ in self._find_matches}

        for widget in self._messages:
            if widget.sequence() not in matching_sequences:
                continue

            matches = widget.find_text(text, case_sensitive, regexp)
            if not matches:
                continue

            current = current_ordinal if widget.sequence() == current_sequence else -1
            widget.highlight_matches(matches, current)
            self._highlighted_widgets.add(widget)

    def _apply_find_scroll(self) -> None:
        """Scroll to the pending find match once the layout has settled."""
        pending = self._pending_find_scroll
        self._pending_find_scroll = None
        if pending is None:
            return

        text, case_sensitive, regexp = self._last_search
        for widget in self._messages:
            if widget.sequence() != pending[0]:
                continue

            matches = widget.find_text(text, case_sensitive, regexp)
            if pending[1] < len(matches):
                self._handle_find_scroll(widget, matches[pending[1]][0])

            return

    def _handle_find_scroll(self, widget: LogMessage, position: int) -> None:
        """
        Handle scroll requests from find operations.

        Args:
            widget: Widget to scroll to
            position: Text position within the section
        """
        # Get position relative to the message widget
        pos_in_message = widget.select_and_scroll_to_position(position)

        # Map the match position to the messages container coordinate system and
        # smooth-scroll so the match sits roughly a quarter of the way down the viewport
        pos_in_container = widget.mapTo(self._messages_container, pos_in_message)
        viewport_height = self._scroll_area.viewport().height()
        target = pos_in_container.y() - viewport_height // 4
        scrollbar = self._scroll_area.verticalScrollBar()
        target = max(scrollbar.minimum(), min(scrollbar.maximum(), target))
        self._start_smooth_scroll(target)

    def _clear_highlights(self) -> None:
        """Clear all highlights from all widgets."""
        for widget in self._highlighted_widgets:
            widget.clear_highlights()

        self._highlighted_widgets.clear()

    def _reset_find(self) -> None:
        """Discard the current search results and their highlights."""
        self._clear_highlights()
        self._find_matches = []
        self._find_truncated = False
        self._current_match_index = -1

    def _return_to_tail(self) -> None:
        """Return the view to the newest page if a find had pinned it elsewhere."""
        if self._showing_tail or not self._mindspace_manager.has_mindspace():
            return

        store = self._mindspace_manager.interactions()
        self._load_page(store, store.count())

    def clear_highlights(self) -> None:
        """Public method to clear highlights."""
        self._reset_find()
        self._last_search = ("", False, False)
        self._return_to_tail()

    def get_match_status(self) -> tuple[int, int, bool]:
        """
        Get current match status.

        Returns:
            Tuple of (current_match, total_matches, truncated)
        """
        total_matches = len(self._find_matches)
        if self._current_match_index == -1:
            return 0, total_matches, self._find_truncated

        return self._current_match_index + 1, total_matches, self._find_truncated
