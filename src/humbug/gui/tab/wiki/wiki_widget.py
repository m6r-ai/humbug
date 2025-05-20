"""Wiki content widget implementation."""

import logging
import re
from datetime import datetime
from typing import Dict, List, Any, Set, Tuple, cast

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy
)
from PySide6.QtCore import Signal, Qt, QPoint, QEvent, QObject, QTimer
from PySide6.QtGui import QCursor, QResizeEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.wiki.wiki_content import WikiContent
from humbug.gui.tab.wiki.wiki_file_content import WikiFileContent
from humbug.gui.tab.wiki.wiki_markdown_content import WikiMarkdownContent
from humbug.gui.tab.wiki.wiki_markdown_preview_content import WikiMarkdownPreviewContent
from humbug.gui.tab.wiki.wiki_error import WikiIOError
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_wiki import MindspaceWiki, MindspaceWikiContentType


class WikiWidgetEventFilter(QObject):
    """Event filter to track activation events from child widgets."""

    widget_activated = Signal(object)
    widget_deactivated = Signal(object)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the event filter."""
        super().__init__(parent)

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """
        Filter events to detect widget activation.

        Args:
            obj: The object that received the event
            event: The event that was received

        Returns:
            True if event was handled, False to pass to the target object
        """
        if event.type() in (QEvent.Type.MouseButtonPress, QEvent.Type.FocusIn):
            # Simply emit the signal with the object that received the event
            self.widget_activated.emit(obj)
            return False  # Don't consume the event

        if event.type() == QEvent.Type.FocusOut:
            # Emit a widget deactivated signal
            self.widget_deactivated.emit(obj)
            return False  # Don't consume the event

        return super().eventFilter(obj, event)


class WikiWidget(QWidget):
    """Widget for displaying wiki content."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Signal to request scrolling to a specific widget and position
    scrollRequested = Signal(QWidget, int)

    # Emits when parent should be activated by user interaction
    activated = Signal()

    # Emits when a link is clicked
    open_link = Signal(str)

    # Emits when a file edit button is clicked
    edit_file = Signal(str)

    def __init__(
        self,
        path: str,
        timestamp: datetime,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize the wiki content widget.

        Args:
            path: Full path to wiki file
            timestamp: ISO format timestamp for the wiki
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("WikiWidget")
        self._path = path
        self._timestamp = timestamp

        self._mindspace_wiki = MindspaceWiki()

        # Widget tracking
        self._content_blocks: List[WikiContent] = []
        self._content_with_selection: WikiContent | None = None

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Create layout
        content_layout = QVBoxLayout(self)
        self.setLayout(content_layout)
        content_layout.setContentsMargins(0, 0, 0, 0)
        content_layout.setSpacing(0)

        # Set up the scroll area
        self._scroll_area = QScrollArea()
        self._scroll_area.setFrameStyle(0)
        self._scroll_area.setWidgetResizable(True)
        self._scroll_area.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # Create content container widget
        self._content_container = QWidget()
        self._content_layout = QVBoxLayout(self._content_container)
        self._content_container.setLayout(self._content_layout)

        spacing = int(self._style_manager.message_bubble_spacing())
        self._content_layout.setSpacing(spacing)
        self._content_layout.setContentsMargins(spacing, spacing, spacing, spacing)
        self._content_layout.addStretch()

        self._content_container.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll_area.setWidget(self._content_container)

        # Add the scroll area to the main layout
        content_layout.addWidget(self._scroll_area)

        # Setup signals for search highlights
        self._search_highlights: Dict[WikiContent, List[Tuple[int, int, int]]] = {}

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        # Create timer for smooth scrolling
        self._scroll_timer = QTimer(self)
        self._scroll_timer.setInterval(16)  # ~60fps
        self._scroll_timer.timeout.connect(self._update_scroll)
        self._last_mouse_pos: QPoint | None = None

        # Timer for smooth animated scrolling
        self._smooth_scroll_timer = QTimer(self)
        self._smooth_scroll_timer.setInterval(16)  # ~60fps
        self._smooth_scroll_timer.timeout.connect(self._update_smooth_scroll)
        self._smooth_scroll_target: int = 0
        self._smooth_scroll_start: int = 0
        self._smooth_scroll_distance: int = 0
        self._smooth_scroll_duration: int = 500  # ms
        self._smooth_scroll_time: int = 0

        # Connect to the vertical scrollbar's change signals
        self._scroll_area.verticalScrollBar().valueChanged.connect(self._on_scroll_value_changed)
        self._scroll_area.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        self._handle_style_changed()

        # Find functionality
        self._matches: List[Tuple[WikiContent, List[Tuple[int, int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[WikiContent] = set()

        # Set up activation tracking
        self._event_filter = WikiWidgetEventFilter(self)
        self._event_filter.widget_activated.connect(self._handle_widget_activation)
        self._event_filter.widget_deactivated.connect(self._handle_widget_deactivation)

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update status if needed
        self.status_updated.emit()

    def _add_content_block(self, content_type: MindspaceWikiContentType, content: str) -> WikiContent:
        """
        Add a new content block to the wiki view.

        Args:
            content: The content text

        Returns:
            The created WikiContent widget
        """
        if content_type == MindspaceWikiContentType.MARKDOWN:
            content_widget: WikiContent = WikiMarkdownContent(self)

        elif content_type == MindspaceWikiContentType.MARKDOWN_PREVIEW:
            content_widget = WikiMarkdownPreviewContent(self)

        elif content_type == MindspaceWikiContentType.FILE:
            content_widget = WikiFileContent(self)

        else:
            # Default to markdown for unknown types
            self._logger.warning("Unknown content type: %s, defaulting to markdown", content_type)
            content_widget = WikiMarkdownContent(self)

        content_widget.selectionChanged.connect(
            lambda has_selection: self._handle_selection_changed(content_widget, has_selection)
        )
        content_widget.scrollRequested.connect(self._handle_selection_scroll)
        content_widget.mouseReleased.connect(self._stop_scroll)
        content_widget.edit_clicked.connect(self._handle_edit_clicked)

        # Connect to the new linkClicked signal
        content_widget.linkClicked.connect(self._handle_link_clicked)

        content_widget.set_content(content, self._path)

        # Add widget before the stretch
        self._content_layout.insertWidget(self._content_layout.count() - 1, content_widget)
        self._content_blocks.append(content_widget)

        self._install_activation_tracking(content_widget)

        return content_widget

    # Add a handler for the edit_clicked signal:
    def _handle_edit_clicked(self) -> None:
        """Handle edit button clicks from content blocks."""
        # Activate this widget
        self.activated.emit()

        # Emit the edit_file signal
        self.edit_file.emit(self._path)

    def _handle_link_clicked(self, url: str) -> None:
        """
        Handle link clicks from content blocks.

        Args:
            url: The URL that was clicked
        """
        # Activate this widget
        self.activated.emit()

        # Handle local links (anchors starting with #)
        if url.startswith("#"):
            # Extract the target ID without the # prefix
            target_id = url[1:]
            self.scroll_to_target(target_id)
            return

        self.open_link.emit(url)

    def _install_activation_tracking(self, widget: QWidget) -> None:
        """
        Install event filter on widget and all its children recursively.

        Call this for any new widgets added to the wiki widget.

        Args:
            widget: Widget to track for activation events
        """
        widget.installEventFilter(self._event_filter)
        child: QWidget
        for child in widget.findChildren(QWidget):
            cast(QWidget, child).installEventFilter(self._event_filter)

    def _handle_widget_activation(self, _widget: QWidget) -> None:
        """
        Handle activation of a widget.

        Args:
            widget: The widget that was activated
        """
        # Emit activated signal to let the tab know this wiki was clicked
        self.activated.emit()

    def _handle_widget_deactivation(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget.

        Args:
            widget: The widget lost focus
        """

    def load_content(self) -> None:
        """Load content from the mindspace wiki."""
        try:
            # Use MindspaceWiki to get content
            contents = self._mindspace_wiki.get_wiki_content(self._path)

            # Clear existing content blocks
            self.clear_content()

            # Add content blocks
            for content_type, content in contents:
                self._add_content_block(content_type, content)

            # Ensure we're scrolled to the top
            self._auto_scroll = True
            self._scroll_to_top()

        except Exception as e:
            raise WikiIOError(f"Failed to read wiki file: {str(e)}") from e

    def clear_content(self) -> None:
        """Clear all content blocks."""
        for content_widget in self._content_blocks:
            self._content_layout.removeWidget(content_widget)
            content_widget.deleteLater()

        self._content_blocks = []
        self._content_with_selection = None

    def update_path(self, new_path: str) -> None:
        """Update the path for the wiki file.

        Args:
            new_path: New path for the wiki file
        """
        self._path = new_path

    def _on_scroll_value_changed(self, value: int) -> None:
        """
        Handle scroll value changes to detect user scrolling.

        Args:
            value (int): The new scroll value
        """
        # Get the vertical scrollbar
        vbar = self._scroll_area.verticalScrollBar()

        # Check if we're at the top
        at_top = value == vbar.minimum()

        # If user scrolls down, disable auto-scroll
        if not at_top:
            self._auto_scroll = False

        # If user scrolls to top, re-enable auto-scroll
        if at_top:
            self._auto_scroll = True

    def _on_scroll_range_changed(self, _minimum: int, maximum: int) -> None:
        """Handle the scroll range changing."""
        if self._auto_scroll:
            self._scroll_to_top()

        self._last_scroll_maximum = maximum

    def _scroll_to_top(self) -> None:
        """Scroll to the top of the content."""
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.minimum())

    def _start_smooth_scroll(self, target_value: int) -> None:
        """
        Start smooth scrolling animation to target value.

        Args:
            target_value: Target scroll position
        """
        scrollbar = self._scroll_area.verticalScrollBar()

        # If we're already scrolling or the value is out of range, do nothing
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

        # Calculate progress (0 to 1)
        progress = min(1.0, self._smooth_scroll_time / self._smooth_scroll_duration)

        # Apply easing function (ease out cubic)
        t = 1 - (1 - progress) ** 3

        # Calculate new position
        new_position = self._smooth_scroll_start + int(self._smooth_scroll_distance * t)

        # Update scrollbar position
        scrollbar = self._scroll_area.verticalScrollBar()
        scrollbar.setValue(new_position)

        # Stop the timer when animation is complete
        if progress >= 1.0:
            self._smooth_scroll_timer.stop()

    def _handle_selection_scroll(self, mouse_pos: QPoint) -> None:
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
                scroll_amount = min(50, max(10, distance_out // 5))
                new_val = max(scrollbar.minimum(), current_val - scroll_amount)
                scrollbar.setValue(new_val)

        elif self._last_mouse_pos.y() > viewport_height:
            # Below viewport
            distance_out = self._last_mouse_pos.y() - viewport_height
            if distance_out > viewport_height * 2:
                scrollbar.setValue(scrollbar.maximum())
            else:
                scroll_amount = min(50, max(10, distance_out // 5))
                new_val = min(scrollbar.maximum(), current_val + scroll_amount)
                scrollbar.setValue(new_val)

        # Update mouse position
        self._last_mouse_pos = self._scroll_area.viewport().mapFromGlobal(QCursor.pos())

    def _handle_selection_changed(self, content_widget: WikiContent, has_selection: bool) -> None:
        """Handle selection changes in content widgets."""
        if not has_selection:
            if self._content_with_selection:
                content = self._content_with_selection
                self._content_with_selection = None
                content.clear_selection()
            return

        if self._content_with_selection and self._content_with_selection != content_widget:
            self._content_with_selection.clear_selection()

        self._content_with_selection = content_widget

    def scroll_to_target(self, target_id: str) -> None:
        """
        Scroll to a target element by ID.

        Args:
            target_id: The ID of the target element to scroll to
        """
        # Normalize the target ID to handle different formats
        target_id = self._normalize_id(target_id)

        # Try to find the target in content blocks
        for content_block in self._content_blocks:
            target_position = content_block.find_element_by_id(target_id)
            if target_position:
                section_idx, _block_num, position = target_position

                # Get the position to scroll to
                target_point = content_block.select_and_scroll_to_position(section_idx, position)

                # Map position to scroll area coordinates and get the target point
                pos_in_scroll_area = content_block.mapTo(self._content_container, target_point)
                target_scroll = pos_in_scroll_area.y() - 50  # 50px margin above target

                # Smoothly scroll to the target position
                self._start_smooth_scroll(target_scroll)
                return

    def _normalize_id(self, id_str: str) -> str:
        """
        Normalize an ID string to be consistent with how IDs are generated.

        Args:
            id_str: The ID string to normalize

        Returns:
            Normalized ID string
        """
        # Convert to lowercase
        id_str = id_str.lower()

        # Replace spaces with hyphens
        id_str = id_str.replace(' ', '-')

        # Remove special characters
        id_str = re.sub(r'[^a-z0-9-]', '', id_str)

        # Ensure it doesn't start with a number (same as in renderer)
        if id_str and id_str[0].isdigit():
            id_str = 'h-' + id_str

        return id_str

    def has_selection(self) -> bool:
        """Check if any content has selected text."""
        return self._content_with_selection is not None and self._content_with_selection.has_selection()

    def get_selected_text(self) -> str:
        """
        Get current selected text if any.

        Returns:
            The selected text or empty string
        """
        if self._content_with_selection:
            return self._content_with_selection.get_selected_text()

        return ""

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

        if self._auto_scroll:
            self._scroll_to_top()

    def _handle_style_changed(self) -> None:
        """Handle style changes."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        self._content_container.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
        """)
        self._scroll_area.setStyleSheet(f"""
            QScrollArea {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
            QScrollBar:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self.has_selection()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        if self._content_with_selection:
            self._content_with_selection.copy_selection()

    def find_text(self, text: str, forward: bool = True) -> Tuple[int, int]:
        """
        Find all instances of text and highlight them.

        Args:
            text: Text to search for
            forward: Whether to search forward from current position

        Returns:
            Tuple of (current_match, total_matches)
        """
        # Get searchable widgets
        widgets = self._content_blocks

        # Clear existing highlights if search text changed
        if text != self._last_search:
            self._clear_highlights()
            self._matches = []
            self._current_widget_index = -1
            self._current_match_index = -1
            self._last_search = text

        # Find all matches if this is a new search
        if not self._matches and text:
            for widget in widgets:
                widget_matches = widget.find_text(text)
                if widget_matches:
                    self._matches.append((widget, widget_matches))

        if not self._matches:
            return 0, 0

        # Move to next/previous match
        if self._current_widget_index == -1:
            # First search - start at beginning or end depending on direction
            if forward:
                self._current_widget_index = 0
                self._current_match_index = 0
            else:
                self._current_widget_index = len(self._matches) - 1
                self._current_match_index = len(self._matches[self._current_widget_index][1]) - 1

        else:
            # Move to next/previous match
            if forward:
                self._current_match_index += 1
                # If we've reached the end of matches in current widget
                if self._current_match_index >= len(self._matches[self._current_widget_index][1]):
                    self._current_widget_index += 1
                    # If we've reached the end of widgets, wrap around
                    if self._current_widget_index >= len(self._matches):
                        self._current_widget_index = 0
                    self._current_match_index = 0

            else:
                self._current_match_index -= 1
                # If we've reached the start of matches in current widget
                if self._current_match_index < 0:
                    self._current_widget_index -= 1
                    # If we've reached the start of widgets, wrap around
                    if self._current_widget_index < 0:
                        self._current_widget_index = len(self._matches) - 1
                    self._current_match_index = len(self._matches[self._current_widget_index][1]) - 1

        # Highlight all matches
        self._highlight_matches()

        # Scroll to current match
        self._scroll_to_current_match()

        # Return current match status
        return self.get_match_status()

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

        if not self._matches:
            return

        # Get colors from style manager
        highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND)
        dim_highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM)

        # Highlight matches in each widget
        for widget_idx, (widget, matches) in enumerate(self._matches):
            # Set current_match_index to highlight the current match
            current_match_idx = self._current_match_index if widget_idx == self._current_widget_index else -1

            # Highlight matches in this widget
            widget.highlight_matches(
                matches,
                current_match_idx,
                highlight_color,
                dim_highlight_color
            )

            # Track highlighted widgets
            self._highlighted_widgets.add(widget)

    def _scroll_to_current_match(self) -> None:
        """Request scroll to ensure the current match is visible."""
        if not self._matches:
            return

        widget, matches = self._matches[self._current_widget_index]
        section_num, start, _ = matches[self._current_match_index]

        # Trigger scrolling to this position
        self.handle_find_scroll(widget, section_num, start)

    def handle_find_scroll(self, widget: WikiContent, section_num: int, position: int) -> None:
        """
        Handle scroll requests from find operations.

        Args:
            widget: Widget to scroll to
            section_num: Section number within the widget
            position: Text position within the section
        """
        # Get position relative to the content widget
        pos_in_content = widget.select_and_scroll_to_position(section_num, position)
        if pos_in_content == QPoint(0, 0) and section_num > 0:
            # Handle case where position wasn't found
            return

        # Map position from content widget to the scroll area's coordinate system
        # This is safe because we know the relationship between these widgets
        pos_in_scroll_area = widget.mapTo(self._scroll_area.widget(), pos_in_content)

        # Ensure the point is visible in the scroll area
        self._scroll_area.ensureVisible(
            pos_in_scroll_area.x(),  # x
            pos_in_scroll_area.y(),  # y
            10,  # xmargin
            50   # ymargin - provide some context around the match
        )

    def _clear_highlights(self) -> None:
        """Clear all search highlights."""
        # Clear highlights from all tracked widgets
        for widget in self._highlighted_widgets:
            widget.clear_highlights()

        self._highlighted_widgets.clear()

    def get_match_status(self) -> Tuple[int, int]:
        """
        Get the current match status.

        Returns:
            Tuple of (current_match, total_matches)
        """
        if not self._matches:
            return 0, 0

        total_matches = sum(len(matches) for _, matches in self._matches)
        if self._current_widget_index == -1:
            return 0, total_matches

        current_match = sum(len(matches) for _, matches in self._matches[:self._current_widget_index])
        current_match += self._current_match_index + 1

        return current_match, total_matches

    def clear_find(self) -> None:
        """Clear all find state."""
        self._clear_highlights()
        self._matches = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""

    def create_state_metadata(self) -> Dict[str, Any]:
        """
        Create metadata dictionary capturing current widget state.

        Returns:
            Dictionary containing wiki state metadata
        """
        metadata: Dict[str, Any] = {}

        # Store scroll position
        scrollbar = self._scroll_area.verticalScrollBar()
        metadata["scroll_position"] = scrollbar.value()

        return metadata

    def restore_from_metadata(self, metadata: Dict[str, Any]) -> None:
        """
        Restore widget state from metadata.

        Args:
            metadata: Dictionary containing state metadata
        """
        if not metadata:
            return

        # Restore scroll position if specified
        if "scroll_position" in metadata:
            # Use a timer to ensure the scroll happens after layout is complete
            QTimer.singleShot(0, lambda: self._scroll_area.verticalScrollBar().setValue(metadata["scroll_position"]))
