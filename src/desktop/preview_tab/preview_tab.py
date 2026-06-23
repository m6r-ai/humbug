"""Preview tab implementation with file change detection."""

import os
import logging
from typing import Any, Dict, List, Tuple, cast

from PySide6.QtCore import Qt, QUrl, QRegularExpression
from PySide6.QtGui import QDesktopServices
from PySide6.QtWidgets import QHBoxLayout, QSizePolicy, QVBoxLayout, QWidget

from context.context_registry import ContextRegistry
from preview_context.preview_context import PreviewContext

from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxType
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.preview_tab.preview_error import PreviewError
from desktop.preview_tab.preview_widget import PreviewWidget
from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.color_role import ColorRole
from desktop.tab import TabBase, TabState
from desktop.widgets import FindWidget


class PreviewTab(TabBase):
    """Preview tab for previewing content."""

    def __init__(
        self,
        tab_id: str,
        path: str,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize the preview tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided.
            path: Full path to preview file
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("PreviewTab")
        self._path = path

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Add find widget at top (initially hidden)
        self._find_widget = FindWidget(self)
        self._find_widget.hide()
        self._find_widget.set_preferred_width(self.preferred_width)
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.search_changed.connect(self._on_search_changed)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        layout.addWidget(self._find_widget)

        # Create preview content widget
        mindspace_manager = MindspaceManager()
        self._preview_content_widget = PreviewWidget(path, mindspace_manager, self)
        self._preview_content_widget.status_updated.connect(self.update_status)
        self._preview_content_widget.open_link.connect(self._on_open_link)
        self._preview_content_widget.edit_file.connect(self._on_edit_file)

        # Connect new signals for file watching
        self._preview_content_widget.content_refreshed.connect(self._on_content_refreshed)

        preview_container = QWidget()
        preview_container_layout = QHBoxLayout(preview_container)
        preview_container_layout.setContentsMargins(0, 0, 0, 0)
        preview_container_layout.setSpacing(0)
        preview_container_layout.setAlignment(Qt.AlignmentFlag.AlignHCenter)
        self._preview_content_widget.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        preview_container_layout.addWidget(self._preview_content_widget)
        layout.addWidget(preview_container)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Load content
        self._preview_content_widget.load_content()

        self._start_file_watching(self._path)
        self.apply_style()

    def tool_name(self) -> str:
        """Return the tool name for this tab type."""
        return "preview"

    def register_context_models(self, registry: ContextRegistry) -> None:
        """Register the PreviewContext with the registry."""
        preview_context = PreviewContext(
            context_id=self._tab_id,
            path=self._path,
            content_blocks=self.get_content_blocks(),
            on_scroll_to_position=self.scroll_to_content_position,
        )
        registry.register_model(self._tab_id, preview_context)

    def on_path_renamed(self, new_path: str) -> None:
        """Update path and emit a new tab bar label after a file rename."""
        self.set_path(new_path)
        self.tab_label_changed.emit(self._tab_id, os.path.basename(new_path))

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Set the active state of the tab.

        Args:
            widget: Widget that is being activated/deactivated
            active: True if the tab is now active, False otherwise
        """
        if active:
            self.activated.emit()

    def activate(self) -> None:
        """Activate the tab."""
        self._preview_content_widget.activate()

    def _on_content_refreshed(self) -> None:
        """Handle when preview content has been refreshed due to file changes."""
        self._logger.debug("Preview content refreshed for path: %s", self._path)
        self.set_updated(True)

        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total, truncated = self._preview_content_widget.get_match_status()
            self._find_widget.set_match_status(current, total, truncated)

        self.update_status()

    def scroll_to_anchor(self, anchor: str) -> None:
        """
        Scroll to the specified anchor in the preview content.

        Args:
            anchor: Anchor ID to scroll to
        """
        # Delegate to the preview content widget
        self._preview_content_widget.scroll_to_target(anchor)

    def _on_open_link(self, url: str) -> None:
        """
        Handle opening links.

        We don't need to handle local anchor links as the PreviewWidget does that.

        Args:
            url: The URL or file path to open
        """
        try:
            # Try to resolve the link path
            resolved_path = self._preview_content_widget.resolve_link(cast(str, self._path), url)
            if resolved_path is not None:
                mindspace_manager = MindspaceManager()
                if mindspace_manager.has_mindspace():
                    contexts = mindspace_manager.mindspace().contexts()
                    existing = contexts.get_by_path_and_type(resolved_path, "preview")
                    if existing:
                        contexts.focus(existing.context_id)

                    else:
                        norm_path = os.path.normpath(resolved_path)
                        contexts.open(
                            context_type="preview",
                            path=resolved_path,
                            title=os.path.basename(norm_path),
                            is_ephemeral=True,
                            requester_id=self._tab_id,
                        )
                return

            # Otherwise, it's an external link - open in browser
            self._open_external_url(url)

        except PreviewError as e:
            # Show error message if link couldn't be handled
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_opening_file_title,
                strings.could_not_open.format(url, str(e)),
            )

    def _on_edit_file(self, path: str) -> None:
        """Open a file from the preview in an editor tab."""
        mindspace_manager = MindspaceManager()
        if not mindspace_manager.has_mindspace():
            return

        contexts = mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(path, "editor")
        if existing:
            contexts.focus(existing.context_id)

        else:
            contexts.open(
                context_type="editor",
                path=path,
                title=os.path.basename(path),
            )

    def _open_external_url(self, url: str) -> None:
        """
        Open an external URL in the system's default web browser.

        Args:
            url: The URL to open
        """
        # Make sure the URL has a scheme
        if not url.startswith(('http://', 'https://')):
            url = 'http://' + url

        # Use Qt's QDesktopServices to open the URL in the default browser
        QDesktopServices.openUrl(QUrl(url))

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total, truncated = self._preview_content_widget.get_match_status()
            self._find_widget.set_match_status(current, total, truncated)

        # Update status bar
        self.update_status()

    def set_path(self, path: str) -> None:
        """
        Set the file being edited.

        Args:
            path: Path to file
        """
        # Stop watching old path
        if self._path != path:
            self.stop_file_watching()

        self._path = path
        self._preview_content_widget.set_path(path)

        # Start watching new path
        if path:
            self._start_file_watching(path)

    def update_status(self) -> None:
        """Update status bar."""
        strings = self._language_manager.strings()
        message = StatusMessage(
            strings.preview_status.format(
                path=self._path
            )
        )
        self.status_message.emit(message)

    def preferred_width(self) -> int | None:
        """Return the preferred column width matching the preview content max width."""
        style_manager = StyleManager()
        return style_manager.scaled_tab_width()

    def can_close_tab(self) -> bool:
        """Check if preview can be closed."""
        return True

    def close_tab(self) -> None:
        """Close the preview tab."""
        self.stop_file_watching()
        self._preview_content_widget.close_widget()

    def get_state(self, temp_state: bool = False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = {}

        # Get widget-specific metadata
        metadata.update(self._preview_content_widget.create_state_metadata())

        if temp_state:
            metadata['find_widget'] = self._find_widget.create_state_metadata()

        return TabState(
            type=self.tool_name(),
            tab_id=self._tab_id,
            path=self._path,
            metadata=metadata,
            is_ephemeral=self._is_ephemeral
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'PreviewTab':
        """Create and restore a preview tab from serialized state."""
        tab = cls(state.tab_id, state.path, parent)
        if state.is_ephemeral:
            tab._is_ephemeral = True

        # Load preview content
        try:
            # Restore widget-specific state if metadata present
            if state.metadata:
                tab._preview_content_widget.restore_from_metadata(state.metadata)

                if 'find_widget' in state.metadata:
                    tab._find_widget.restore_from_metadata(state.metadata['find_widget'])

            return tab

        except Exception as e:
            raise PreviewError(f"Failed to restore preview tab: {str(e)}") from e

    def can_save(self) -> bool:
        """Check if preview can be saved."""
        return False  # Read-only for now

    def save(self) -> bool:
        """Save preview (not applicable)."""
        return True

    def can_save_as(self) -> bool:
        """Check if preview can be saved as."""
        return False  # Read-only for now

    def save_as(self) -> bool:
        """Save preview as (not applicable)."""
        return True

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return False

    def undo(self) -> None:
        """Undo not supported for preview."""

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for preview."""

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return False  # Read-only content

    def cut(self) -> None:
        """Cut not supported for preview."""

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._preview_content_widget.can_copy()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._preview_content_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return False  # Read-only content

    def paste(self) -> None:
        """Paste not supported for preview."""

    def can_submit(self) -> bool:
        """Check if terminal can submit (not supported)."""
        return False

    def submit(self) -> None:
        """Submit terminal input (not supported)."""

    def show_find(self) -> None:
        """Show the find widget."""
        # Get selected text if any
        if self._preview_content_widget.has_selection():
            selected_text = self._preview_content_widget.get_selected_text()
            if selected_text:
                self._find_widget.set_search_text(selected_text)

            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def apply_find_search(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a programmatic find/highlight request to the preview tab."""
        self._find_widget.set_case_sensitive(case_sensitive)
        self._find_widget.set_regexp(regexp)
        self._find_widget.set_search_text(text)
        self._find_widget.show()
        current, total, truncated = self._preview_content_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)
        self._find_widget.set_match_status(current, total, truncated)
        self._find_widget.setFocus()

    def apply_search_highlight(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:
        """Apply a transient search highlight without altering the local find widget."""
        self._preview_content_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)

    def clear_search_highlight(self) -> None:
        """Clear transient search highlights."""
        self._preview_content_widget.clear_highlights()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._preview_content_widget.clear_highlights()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        current, total, truncated = self._preview_content_widget.find_text(
            text, forward, case_sensitive=case_sensitive, regexp=regexp
        )
        self._find_widget.set_match_status(current, total, truncated)

    def _on_search_changed(self) -> None:
        """Handle search text or mode changes - update matches without navigating."""
        text, case_sensitive, regexp = self._find_widget.current_search_request()
        if regexp:
            if text and not QRegularExpression(text).isValid():
                self._find_widget.set_invalid_regexp()
                return

        current, total, truncated = self._preview_content_widget.find_text(text, True, case_sensitive=case_sensitive, regexp=regexp)
        self._find_widget.set_match_status(current, total, truncated)

    def get_preview_info(self) -> Dict[str, Any]:
        """
        Get high-level metadata about the preview content.

        Returns:
            Dictionary containing preview metadata
        """
        return self._preview_content_widget.get_preview_info()

    def get_content_blocks(self) -> List[Tuple[Any, str]]:
        """
        Return the cached raw content blocks for the PreviewContext.

        Returns:
            List of (PreviewContentType, str) tuples from the last load.
        """
        return self._preview_content_widget.get_content_blocks()

    def search_content(
        self,
        search_text: str,
        case_sensitive: bool = False,
        max_results: int = 50,
        regexp: bool = False
    ) -> Dict[str, Any]:
        """
        Search for text across all content blocks.

        Args:
            search_text: Text to search for
            case_sensitive: Whether search should be case-sensitive
            max_results: Maximum number of results to return
            regexp: If True, treat search_text as a regular expression.

        Returns:
            Dictionary containing search results with matches and context
        """
        return self._preview_content_widget.search_content(search_text, case_sensitive, max_results, regexp)

    def scroll_to_content_position(
        self,
        block_index: int,
        section_index: int = 0,
        position: int = 0,
        viewport_position: str = "center"
    ) -> bool:
        """
        Scroll to a specific position in the content.

        Args:
            block_index: Index of the content block
            section_index: Index of the section within the block
            position: Text position within the section
            viewport_position: Where to position in viewport ("top", "center", "bottom")

        Returns:
            True if scroll was successful, False otherwise
        """
        return self._preview_content_widget.scroll_to_content_position(
            block_index, section_index, position, viewport_position
        )

    def apply_style(self) -> None:
        """Apply current style settings to the tab's content widgets."""
        self._find_widget.apply_style()
        style_manager = StyleManager()
        self._preview_content_widget.setMaximumWidth(style_manager.scaled_tab_width())
        self._preview_content_widget.apply_style()

        new_stylesheet = self._build_stylesheet()
        if new_stylesheet != self.styleSheet():
            self.setStyleSheet(new_stylesheet)

    def _build_stylesheet(self) -> str:
        """Build the stylesheet for this tab."""
        style_manager = StyleManager()
        border_radius = int(style_manager.message_bubble_spacing())
        return f"""
            #PreviewWidget QWidget {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            #PreviewWidget #PreviewFileContent {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 1px solid {style_manager.get_color_str(ColorRole.CODE_BORDER)};
            }}

            #PreviewWidget #PreviewFileContent #_content_container {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            #PreviewWidget #PreviewFileContent #_header_container {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            #PreviewWidget #PreviewFileContent #_syntax_header {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYNTAX)};
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            #PreviewWidget #PreviewFileContent #_text_area {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
                background-color: transparent;
            }}

            {style_manager.get_scrollbar_stylesheet("#PreviewWidget #PreviewFileContent #_text_area QScrollBar")}

            #PreviewWidget #PreviewFileContent #_edit_button {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 0;
                padding: 0px;
            }}
            #PreviewWidget #PreviewFileContent #_edit_button:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            #PreviewWidget #PreviewFileContent #_edit_button:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}

            #PreviewWidget QWidget#PreviewMarkdownContent {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}

            #PreviewWidget QWidget#PreviewMarkdownContent[contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            #PreviewWidget #PreviewMarkdownContent QWidget#_sections_container {{
                background-color: transparent;
                border: none;
                margin: 0;
                padding: 0;
            }}

            #PreviewWidget QFrame#PreviewMarkdownContentSection {{
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}

            #PreviewWidget QFrame#PreviewMarkdownContentSection[section_type="text"][contained="false"] {{
                background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            #PreviewWidget QFrame#PreviewMarkdownContentSection[section_type="text"][contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}

            #PreviewWidget QFrame#PreviewMarkdownContentSection[section_type="code"][contained="false"] {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                border: 1px solid {style_manager.get_color_str(ColorRole.CODE_BORDER)};
            }}

            #PreviewWidget QFrame#PreviewMarkdownContentSection[section_type="code"][contained="true"] {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                border: 1px solid {style_manager.get_color_str(ColorRole.CODE_BORDER)};
            }}

            #PreviewWidget QFrame#PreviewMarkdownContentSection QLabel {{
                color: {style_manager.get_color_str(ColorRole.MESSAGE_SYNTAX)};
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            #PreviewWidget #PreviewMarkdownContentSection QTextEdit {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 0;
                margin: 0;
                selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            }}

            #PreviewWidget #PreviewMarkdownContentSection QWidget {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            {style_manager.get_scrollbar_stylesheet("#PreviewWidget #PreviewMarkdownContentSection QScrollBar")}

            #PreviewWidget QFrame#PreviewMarkdownPreviewContent {{
                background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: {border_radius}px;
                border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_BORDER)};
            }}

            {style_manager.get_scrollbar_stylesheet("#PreviewWidget QScrollBar")}
        """
