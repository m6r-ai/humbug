"""Wiki content widget implementation with file change detection."""

import logging
import os
import re
from typing import Dict, List, Any, Set, Tuple, cast

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QScrollArea, QSizePolicy
)
from PySide6.QtCore import Signal, Qt, QPoint, QEvent, QObject, QTimer
from PySide6.QtGui import QCursor, QResizeEvent

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_file_watcher import MindspaceFileWatcher
from humbug.style_manager import StyleManager
from humbug.tabs.wiki.wiki_content import WikiContent, WikiContentType
from humbug.tabs.wiki.wiki_content_widget import WikiContentWidget
from humbug.tabs.wiki.wiki_error import WikiIOError
from humbug.tabs.wiki.wiki_file_content import WikiFileContent
from humbug.tabs.wiki.wiki_markdown_content import WikiMarkdownContent
from humbug.tabs.wiki.wiki_markdown_preview_content import WikiMarkdownPreviewContent


class WikiWidgetEventFilter(QObject):
    """Event filter to track activation events from child widgets."""

    widget_activated = Signal(object)
    widget_deactivated = Signal(object)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the event filter."""
        super().__init__(parent)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
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
            self.widget_activated.emit(watched)
            return False  # Don't consume the event

        if event.type() == QEvent.Type.FocusOut:
            # Emit a widget deactivated signal
            self.widget_deactivated.emit(watched)
            return False  # Don't consume the event

        return super().eventFilter(watched, event)


class WikiWidget(QWidget):
    """Widget for displaying wiki content with automatic refresh capability."""

    # Signal to notify tab of status changes
    status_updated = Signal()

    # Emits when parent should be activated by user interaction
    activated = Signal()

    # Emits when a link is clicked
    open_link = Signal(str)

    # Emits when a file edit button is clicked
    edit_file = Signal(str)

    # Emits when content has been refreshed due to file changes
    content_refreshed = Signal()

    def __init__(
        self,
        path: str,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize the wiki content widget.

        Args:
            path: Full path to wiki file
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._logger = logging.getLogger("WikiWidget")
        self._path = path

        self._wiki = WikiContent()

        # File watching integration
        self._file_watcher = MindspaceFileWatcher()
        self._watched_paths: Set[str] = set()

        # Widget tracking
        self._content_blocks: List[WikiContentWidget] = []
        self._content_with_selection: WikiContentWidget | None = None

        # Initialize tracking variables
        self._auto_scroll = True
        self._last_scroll_maximum = 0

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._on_style_changed)

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
        self._search_highlights: Dict[WikiContentWidget, List[Tuple[int, int, int]]] = {}

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

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

        self._on_style_changed()

        # Find functionality
        self._matches: List[Tuple[WikiContentWidget, List[Tuple[int, int, int]]]] = []
        self._current_widget_index = -1
        self._current_match_index = -1
        self._last_search = ""
        self._highlighted_widgets: Set[WikiContentWidget] = set()

        # Set up activation tracking
        self._event_filter = WikiWidgetEventFilter(self)
        self._event_filter.widget_activated.connect(self._on_widget_activated)
        self._event_filter.widget_deactivated.connect(self._on_widget_deactivated)

    def __del__(self) -> None:
        """Clean up file watching when widget is destroyed."""
        self._unregister_file_watching()

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update status if needed
        self.status_updated.emit()

    def _register_file_watching(self, dependencies: Set[str]) -> None:
        """Register current path and any dependencies for file watching."""
        # Register each dependency with the file watcher
        for dep_path in dependencies:
            if dep_path not in self._watched_paths:
                self._file_watcher.watch_file(dep_path, self._handle_file_changed)
                self._watched_paths.add(dep_path)
                self._logger.debug("Watching file: %s", dep_path)

    def _unregister_file_watching(self) -> None:
        """Unregister all file watching for this widget."""
        for watched_path in self._watched_paths.copy():
            self._file_watcher.unwatch_file(watched_path, self._handle_file_changed)
            self._watched_paths.remove(watched_path)
            self._logger.debug("Stopped watching file: %s", watched_path)

    def _handle_file_changed(self, changed_path: str) -> None:
        """
        Handle notification that a watched file has changed.

        Args:
            changed_path: Path of the file that changed
        """
        self._logger.debug("File changed: %s", changed_path)

        # Check if the main path still exists
        if not os.path.exists(self._path):
            self._logger.info("Main path no longer exists: %s", self._path)
            return

        # Refresh content while preserving UI state
        self.refresh_content()

    def refresh_content(self) -> None:
        """Refresh content from disk, preserving scroll position and other UI state."""
        try:
            # Save current state
            saved_scroll_pos = self._scroll_area.verticalScrollBar().value()
            saved_selection = None
            saved_find_state = {
                'matches': self._matches.copy(),
                'current_widget_index': self._current_widget_index,
                'current_match_index': self._current_match_index,
                'last_search': self._last_search
            }

            if self._content_with_selection:
                saved_selection = self._content_with_selection.get_selected_text()

            # Unregister old file watching
            self._unregister_file_watching()

            # Reload content
            self.load_content()

            # Restore state
            QTimer.singleShot(0, lambda: self._restore_ui_state(
                saved_scroll_pos, saved_selection, saved_find_state
            ))

            # Emit refresh signal
            self.content_refreshed.emit()

        except Exception as e:
            self._logger.error("Failed to refresh content: %s", str(e))

    def _restore_ui_state(self, scroll_pos: int, selection: str | None, find_state: Dict) -> None:
        """
        Restore UI state after content refresh.

        Args:
            scroll_pos: Saved scroll position
            selection: Saved text selection
            find_state: Saved find/search state
        """
        # Restore scroll position
        self._scroll_area.verticalScrollBar().setValue(scroll_pos)

        # Restore selection if possible
        if selection:
            # Try to find and restore the same text selection
            for content_block in self._content_blocks:
                if selection in content_block.get_selected_text():
                    break

        # Restore find state if there was an active search
        if find_state['last_search']:
            # Re-run the search to restore highlights
            self._last_search = ""  # Reset to force re-search
            _current, total = self.find_text(find_state['last_search'], True)

            # Try to restore the current match position
            if (find_state['current_widget_index'] >= 0 and
                find_state['current_match_index'] >= 0):
                # Navigate to approximately the same match position
                target_match = (find_state['current_widget_index'] *
                              (find_state['current_match_index'] + 1))
                for _ in range(min(target_match, total)):
                    self.find_text(find_state['last_search'], True)

    def _add_content_block(self, content_type: WikiContentType, content: str) -> WikiContentWidget:
        """
        Add a new content block to the wiki view.

        Args:
            content_type: Type of content to create
            content: The content text

        Returns:
            The created WikiContentWidget widget
        """
        if content_type == WikiContentType.MARKDOWN:
            content_widget: WikiContentWidget = WikiMarkdownContent(self)

        elif content_type == WikiContentType.MARKDOWN_PREVIEW:
            content_widget = WikiMarkdownPreviewContent(self)

        elif content_type == WikiContentType.FILE:
            content_widget = WikiFileContent(self)

        else:
            # Default to markdown for unknown types
            self._logger.warning("Unknown content type: %s, defaulting to markdown", content_type)
            content_widget = WikiMarkdownContent(self)

        content_widget.selection_changed.connect(
            lambda has_selection: self._on_selection_changed(content_widget, has_selection)
        )
        content_widget.scroll_requested.connect(self._on_scroll_requested)
        content_widget.mouse_released.connect(self._stop_scroll)
        content_widget.edit_clicked.connect(self._on_edit_clicked)
        content_widget.link_clicked.connect(self._on_link_clicked)

        content_widget.set_content(content, self._path)

        # Add widget before the stretch
        self._content_layout.insertWidget(self._content_layout.count() - 1, content_widget)
        self._content_blocks.append(content_widget)

        self._install_activation_tracking(content_widget)

        return content_widget

    def _on_edit_clicked(self) -> None:
        """Handle edit button clicks from content blocks."""
        # Activate this widget
        self.activated.emit()

        # Emit the edit_file signal
        self.edit_file.emit(self._path)

    def _on_link_clicked(self, url: str) -> None:
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

    def _on_widget_activated(self, _widget: QWidget) -> None:
        """
        Handle activation of a widget.

        Args:
            widget: The widget that was activated
        """
        # Emit activated signal to let the tab know this wiki was clicked
        self.activated.emit()

    def _on_widget_deactivated(self, widget: QWidget) -> None:
        """
        Handle deactivation of a widget.

        Args:
            widget: The widget lost focus
        """

    def load_content(self) -> None:
        """Load content from the mindspace wiki."""
        try:
            # Get content and dependencies
            content_list, dependencies = self._wiki.get_wiki_content(self._path)

            # Clear existing content blocks
            self.clear_content()

            # Add content blocks
            for content_type, content in content_list:
                self._add_content_block(content_type, content)

            # Register file watching for all dependencies
            self._register_file_watching(dependencies)

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

    def resolve_link(self, current_path: str, target_path: str) -> str | None:
        """
        Resolve a link to an absolute path.

        Args:
            current_path: Path of the current wiki page
            target_path: Target path from the link

        Returns:
            Absolute path to the target or None if it's an external link
        """
        return self._wiki.resolve_link(current_path, target_path)

    def set_path(self, new_path: str) -> None:
        """
        Set the path for the wiki file.

        Args:
            new_path: New path for the wiki file
        """
        # Unregister old file watching
        self._unregister_file_watching()

        self._path = new_path
        self.load_content()

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

    def _on_selection_changed(self, content_widget: WikiContentWidget, has_selection: bool) -> None:
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

    def _build_widget_style(self) -> str:
        """Build styles for the conversation widget."""
        style_manager = self._style_manager

        return f"""
            QWidget {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}

            QScrollArea {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
            QScrollBar:vertical {{
                background-color: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-page:vertical,
            QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical,
            QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """

    def _build_wiki_file_content_style(self) -> str:
        """Build styles for the WikiFileContent widget."""
        style_manager = self._style_manager

        return f"""
            #WikiFileContent {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: {int(style_manager.message_bubble_spacing())}px;
                border: none;
            }}

            #WikiFileContent #_content_container {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            #WikiFileContent #_header_container {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            #WikiFileContent #_language_header {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_LANGUAGE)};
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            #WikiFileContent #_text_area {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
                background-color: transparent;
            }}

            #WikiFileContent #_text_area QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #WikiFileContent #_text_area QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            #WikiFileContent #_text_area QScrollBar::add-page:horizontal,
            #WikiFileContent #_text_area QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            #WikiFileContent #_text_area QScrollBar::add-line:horizontal,
            #WikiFileContent #_text_area QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
            #WikiFileContent #_text_area QScrollBar:vertical {{
                width: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #WikiFileContent #_text_area QScrollBar::handle:vertical {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            #WikiFileContent #_text_area QScrollBar::add-page:vertical,
            #WikiFileContent #_text_area QScrollBar::sub-page:vertical {{
                background: none;
            }}
            #WikiFileContent #_text_area QScrollBar::add-line:vertical,
            #WikiFileContent #_text_area QScrollBar::sub-line:vertical {{
                height: 0px;
            }}

            #WikiFileContent #_edit_button {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 0;
                padding: 0px;
            }}
            #WikiFileContent #_edit_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            #WikiFileContent #_edit_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
        """

    def _build_wiki_markdown_content_styles(self) -> str:
        """Build styles for the main container."""
        style_manager = self._style_manager
        return f"""
            QWidget#WikiMarkdownContent {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            QWidget#WikiMarkdownContent[contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            #WikiMarkdownContent QWidget#_sections_container {{
                background-color: transparent;
                border: none;
                margin: 0;
                padding: 0;
            }}
        """

    def _build_wiki_markdown_content_section_styles(self) -> str:
        """Build styles for language headers within sections."""
        style_manager = self._style_manager
        border_radius = int(style_manager.message_bubble_spacing())

        return f"""
            /* Default section styling */
            QFrame#WikiMarkdownContentSection {{
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}

            /* Text sections - normal (not contained) */
            QFrame#WikiMarkdownContentSection[section_type="text"][contained="false"] {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            /* Text sections - contained */
            QFrame#WikiMarkdownContentSection[section_type="text"][contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            /* Code sections - normal (not contained) */
            QFrame#WikiMarkdownContentSection[section_type="code"][contained="false"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            /* Code sections - contained */
            QFrame#WikiMarkdownContentSection[section_type="code"][contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
            }}

            QFrame#WikiMarkdownContentSection QLabel {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_LANGUAGE)};
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            /* Text areas within sections */
            #WikiMarkdownContentSection QTextEdit {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 0;
                margin: 0;
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            }}

            /* Header containers within sections */
            #WikiMarkdownContentSection QWidget {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            /* Scrollbars within sections */
            #WikiMarkdownContentSection QScrollBar:horizontal {{
                height: 12px;
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            #WikiMarkdownContentSection QScrollBar::handle:horizontal {{
                background: {style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            #WikiMarkdownContentSection QScrollBar::add-page:horizontal,
            #WikiMarkdownContentSection QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            #WikiMarkdownContentSection QScrollBar::add-line:horizontal,
            #WikiMarkdownContentSection QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """

    def _build_wiki_markdown_preview_content_style(self) -> str:
        """Build styles for the WikiMarkdownPreviewContent widget."""
        style_manager = self._style_manager

        return f"""
            QFrame#WikiMarkdownPreviewContent {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: {int(style_manager.message_bubble_spacing())}px;
                border: 0;
            }}
        """

    def _on_style_changed(self) -> None:
        """Handle style changes."""
        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        stylesheet_parts = [
            self._build_widget_style(),
            self._build_wiki_file_content_style(),
            self._build_wiki_markdown_content_styles(),
            self._build_wiki_markdown_content_section_styles(),
            self._build_wiki_markdown_preview_content_style(),
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

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

        self._highlight_matches()
        self._scroll_to_current_match()
        return self.get_match_status()

    def _highlight_matches(self) -> None:
        """Update the highlighting of all matches."""
        self._clear_highlights()

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

    def _handle_find_scroll(self, widget: WikiContentWidget, section_num: int, position: int) -> None:
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

    def _scroll_to_current_match(self) -> None:
        """Request scroll to ensure the current match is visible."""
        widget, matches = self._matches[self._current_widget_index]
        section_num, start, _ = matches[self._current_match_index]

        # Trigger scrolling to this position
        self._handle_find_scroll(widget, section_num, start)

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
