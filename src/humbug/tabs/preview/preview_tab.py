"""Preview tab implementation with file change detection."""

import logging
from typing import Any, Dict, cast

from PySide6.QtCore import QUrl, Signal
from PySide6.QtGui import QDesktopServices
from PySide6.QtWidgets import QVBoxLayout, QWidget

from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType
from humbug.status_message import StatusMessage
from humbug.tabs.find_widget import FindWidget
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType
from humbug.tabs.preview.preview_error import PreviewError
from humbug.tabs.preview.preview_widget import PreviewWidget


class PreviewTab(TabBase):
    """Preview tab for previewing content."""

    # Signal to request opening a new preview tab
    open_link_requested = Signal(str)

    # Signal to request editing a file
    edit_file_requested = Signal(str)

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
        self._find_widget = FindWidget()
        self._find_widget.hide()
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        layout.addWidget(self._find_widget)

        # Create preview content widget
        self._preview_content_widget = PreviewWidget(path, self)
        self._preview_content_widget.status_updated.connect(self.update_status)
        self._preview_content_widget.open_link.connect(self._on_open_link)
        self._preview_content_widget.edit_file.connect(self.edit_file_requested)

        # Connect new signals for file watching
        self._preview_content_widget.content_refreshed.connect(self._on_content_refreshed)

        layout.addWidget(self._preview_content_widget)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Load content
        self._preview_content_widget.load_content()

        self._start_file_watching(self._path)

    def activate(self) -> None:
        """Activate the tab."""
        self._preview_content_widget.activate()

    def _on_content_refreshed(self) -> None:
        """Handle when preview content has been refreshed due to file changes."""
        self._logger.debug("Preview content refreshed for path: %s", self._path)
        self.set_updated(True)

        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._preview_content_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

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
                # It's a local mindspace link - open in preview tab
                self.open_link_requested.emit(resolved_path)
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
            current, total = self._preview_content_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

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
            self._stop_file_watching()

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

    def can_close_tab(self) -> bool:
        """Check if preview can be closed."""
        return True

    def close_tab(self) -> None:
        """Close the preview tab."""
        self._stop_file_watching()
        self._preview_content_widget.close_widget()

    def get_state(self, temp_state: bool = False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = {}

        # Get widget-specific metadata
        metadata.update(self._preview_content_widget.create_state_metadata())

        if temp_state:
            metadata['find_widget'] = self._find_widget.create_state_metadata()

        return TabState(
            type=TabType.PREVIEW,
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

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._preview_content_widget.clear_find()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        current, total = self._preview_content_widget.find_text(text, forward)
        self._find_widget.set_match_status(current, total)

    def get_preview_info(self) -> Dict[str, Any]:
        """
        Get high-level metadata about the preview content.

        Returns:
            Dictionary containing preview metadata
        """
        return self._preview_content_widget.get_preview_info()

    def search_content(
        self,
        search_text: str,
        case_sensitive: bool = False,
        max_results: int = 50
    ) -> Dict[str, Any]:
        """
        Search for text across all content blocks.

        Args:
            search_text: Text to search for
            case_sensitive: Whether search should be case-sensitive
            max_results: Maximum number of results to return

        Returns:
            Dictionary containing search results with matches and context
        """
        return self._preview_content_widget.search_content(search_text, case_sensitive, max_results)

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
