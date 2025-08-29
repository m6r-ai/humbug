"""Wiki tab implementation with file change detection."""

import logging
from typing import cast

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
from humbug.tabs.wiki.wiki_error import WikiError
from humbug.tabs.wiki.wiki_widget import WikiWidget


class WikiTab(TabBase):
    """Wiki tab for displaying wiki-like content."""

    # Signal to request opening a new wiki tab
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
        Initialize the wiki tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided.
            path: Full path to wiki file
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("WikiTab")
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

        # Create wiki content widget
        self._wiki_content_widget = WikiWidget(path, self)
        self._wiki_content_widget.status_updated.connect(self.update_status)
        self._wiki_content_widget.open_link.connect(self._on_open_link)
        self._wiki_content_widget.edit_file.connect(self.edit_file_requested)

        # Connect new signals for file watching
        self._wiki_content_widget.content_refreshed.connect(self._on_content_refreshed)

        layout.addWidget(self._wiki_content_widget)

        # Install activation tracking
        self._wiki_content_widget.activated.connect(self.activated)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Load content
        self._wiki_content_widget.load_content()

        self._start_file_watching(self._path)

    def _on_content_refreshed(self) -> None:
        """Handle when wiki content has been refreshed due to file changes."""
        self._logger.debug("Wiki content refreshed for path: %s", self._path)
        self.set_updated(True)
        self.update_status()

    def scroll_to_anchor(self, anchor: str) -> None:
        """
        Scroll to the specified anchor in the wiki content.

        Args:
            anchor: Anchor ID to scroll to
        """
        # Delegate to the wiki content widget
        self._wiki_content_widget.scroll_to_target(anchor)

    def _on_open_link(self, url: str) -> None:
        """
        Handle opening links.

        We don't need to handle local anchor links as the WikiWidget does that.

        Args:
            url: The URL or file path to open
        """
        try:
            # Try to resolve the link path
            resolved_path = self._wiki_content_widget.resolve_link(cast(str, self._path), url)
            if resolved_path is not None:
                # It's a local mindspace link - open in wiki tab
                self.open_link_requested.emit(resolved_path)
                return

            # Otherwise, it's an external link - open in browser
            self._open_external_url(url)

        except WikiError as e:
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
            current, total = self._wiki_content_widget.get_match_status()
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
        self._wiki_content_widget.set_path(path)

        # Start watching new path
        if path:
            self._start_file_watching(path)

    def update_status(self) -> None:
        """Update status bar."""
        strings = self._language_manager.strings()
        message = StatusMessage(
            strings.wiki_status.format(
                path=self._path
            )
        )
        self.status_message.emit(message)

    def can_close_tab(self) -> bool:
        """Check if wiki can be closed."""
        return True

    def close_tab(self) -> None:
        """Close the wiki tab."""

    def get_state(self, temp_state: bool = False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = {}

        # Get widget-specific metadata
        metadata.update(self._wiki_content_widget.create_state_metadata())

        return TabState(
            type=TabType.WIKI,
            tab_id=self._tab_id,
            path=self._path,
            metadata=metadata,
            is_ephemeral=self._is_ephemeral
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'WikiTab':
        """Create and restore a wiki tab from serialized state."""
        tab = cls(state.tab_id, state.path, parent)
        if state.is_ephemeral:
            tab._is_ephemeral = True

        # Load wiki content
        try:
            # Restore widget-specific state if metadata present
            if state.metadata:
                tab._wiki_content_widget.restore_from_metadata(state.metadata)

            return tab

        except Exception as e:
            raise WikiError(f"Failed to restore wiki tab: {str(e)}") from e

    def can_save(self) -> bool:
        """Check if wiki can be saved."""
        return False  # Read-only for now

    def save(self) -> bool:
        """Save wiki (not applicable)."""
        return True

    def can_save_as(self) -> bool:
        """Check if wiki can be saved as."""
        return False  # Read-only for now

    def save_as(self) -> bool:
        """Save wiki as (not applicable)."""
        return True

    def can_undo(self) -> bool:
        """Check if undo is available."""
        return False

    def undo(self) -> None:
        """Undo not supported for wiki."""

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for wiki."""

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return False  # Read-only content

    def cut(self) -> None:
        """Cut not supported for wiki."""

    def can_copy(self) -> bool:
        """Check if copy is available."""
        return self._wiki_content_widget.can_copy()

    def copy(self) -> None:
        """Copy selected text to clipboard."""
        self._wiki_content_widget.copy()

    def can_paste(self) -> bool:
        """Check if paste is available."""
        return False  # Read-only content

    def paste(self) -> None:
        """Paste not supported for wiki."""

    def can_submit(self) -> bool:
        """Check if terminal can submit (not supported)."""
        return False

    def submit(self) -> None:
        """Submit terminal input (not supported)."""

    def show_find(self) -> None:
        """Show the find widget."""
        # Get selected text if any
        if self._wiki_content_widget.has_selection():
            selected_text = self._wiki_content_widget.get_selected_text()
            if selected_text:
                self._find_widget.set_search_text(selected_text)
            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._wiki_content_widget.clear_find()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        current, total = self._wiki_content_widget.find_text(text, forward)
        self._find_widget.set_match_status(current, total)
