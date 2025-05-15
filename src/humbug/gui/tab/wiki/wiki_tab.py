"""Wiki tab implementation."""

import logging
import os
from datetime import datetime
from typing import cast

from PySide6.QtWidgets import QVBoxLayout, QWidget
from PySide6.QtCore import Signal

from humbug.gui.color_role import ColorRole
from humbug.gui.find_widget import FindWidget
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.tab_base import TabBase
from humbug.gui.tab.tab_state import TabState
from humbug.gui.tab.tab_type import TabType
from humbug.language.language_manager import LanguageManager
from humbug.wiki.wiki_widget import WikiWidget
from humbug.wiki.wiki_error import WikiError


class WikiTab(TabBase):
    """Wiki tab for displaying wiki-like content."""

    def __init__(
        self,
        tab_id: str,
        path: str,
        timestamp: datetime,
        parent: QWidget | None = None
    ) -> None:
        """
        Initialize the wiki tab.

        Args:
            tab_id: Unique identifier for this tab
            path: Full path to wiki file
            timestamp: ISO format timestamp for the wiki
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("WikiTab")
        self._path: str = path
        self._timestamp = timestamp

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
        self._wiki_content_widget = WikiWidget(path, timestamp, self)
        self._wiki_content_widget.status_updated.connect(self.update_status)
        layout.addWidget(self._wiki_content_widget)

        # Install activation tracking
        self._install_activation_tracking(self._wiki_content_widget)
        self._wiki_content_widget.activated.connect(self.activated)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._wiki_content_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar
        self.update_status()

    def update_path(self, new_id: str, new_path: str) -> None:
        """Update the wiki file path.

        Args:
            new_id: New ID for the wiki
            new_path: New path for the wiki file
        """
        self._path = new_path
        self._tab_id = new_id
        self._wiki_content_widget.update_path(new_path)

    def update_status(self) -> None:
        """Update status bar."""
        pass  # No specific status needed for wiki tabs

    def can_close_tab(self) -> bool:
        """Check if wiki can be closed."""
        return True

    def close_tab(self) -> None:
        """Close the wiki tab."""
        pass  # No specific cleanup needed

    def get_state(self, temp_state: bool = False) -> TabState:
        """Get serializable state for mindspace persistence."""
        metadata = {}

        # Get widget-specific metadata
        metadata.update(self._wiki_content_widget.create_state_metadata())

        return TabState(
            type=TabType.WIKI,  # New tab type for wiki
            tab_id=self._tab_id,
            path=self._path,
            timestamp=self._timestamp,
            metadata=metadata
        )

    @classmethod
    def load_from_file(cls, path: str, parent: QWidget | None = None) -> 'WikiTab':
        """
        Load a wiki tab from a file.

        Args:
            path: Path to wiki file
            parent: Optional parent widget

        Returns:
            Created WikiTab instance

        Raises:
            WikiError: If the wiki tab cannot be loaded
        """
        try:
            # Create wiki tab
            wiki_id = os.path.splitext(os.path.basename(path))[0]
            timestamp = datetime.now()  # Use current time as default

            # Create the tab
            wiki_tab = cls(wiki_id, path, timestamp, parent)

            # Load content
            wiki_tab._wiki_content_widget.load_content()

            return wiki_tab

        except Exception as e:
            raise WikiError(f"Failed to create wiki tab: {str(e)}") from e

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget | None = None) -> 'WikiTab':
        """Create and restore a wiki tab from serialized state."""
        if not state.timestamp:
            raise WikiError("Wiki tab requires timestamp")

        tab = cls(state.tab_id, state.path, state.timestamp, parent)

        # Load wiki content
        try:
            # Load content
            tab._wiki_content_widget.load_content()

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
        pass

    def can_redo(self) -> bool:
        """Check if redo is available."""
        return False

    def redo(self) -> None:
        """Redo not supported for wiki."""
        pass

    def can_cut(self) -> bool:
        """Check if cut is available."""
        return False  # Read-only content

    def cut(self) -> None:
        """Cut not supported for wiki."""
        pass

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
        pass

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

    def _handle_style_changed(self) -> None:
        """Handle style changes."""
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border-top: 2px solid {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}
        """)
