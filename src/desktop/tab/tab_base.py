import uuid
import os
import logging

from PySide6.QtWidgets import QFrame, QWidget
from PySide6.QtCore import Signal

from context.context_registry import ContextRegistry

from desktop.file_watcher import FileWatcher
from desktop.status_message import StatusMessage
from desktop.tab.tab_state import TabState


class TabBase(QFrame):
    """Base class for all tab content."""

    # Common signals that both conversation and editor tabs will need
    modified_state_changed = Signal(str, bool)  # Emits (tab_id, is_modified)
    updated_state_changed = Signal(str, bool)  # Emits (tab_id, is_updated)
    file_state_changed = Signal(str, bool)  # Emits (tab_id, file_exists)
    status_message = Signal(StatusMessage)
    activated = Signal()  # Emits when tab is activated by user interaction
    tab_label_changed = Signal(str, str)  # Emits (tab_id, new_title) when the tab bar label should change
    close_requested = Signal()  # Emits when tab requests to be closed

    def __init__(self, tab_id: str, parent: QWidget | None = None) -> None:
        """
        Initialize the base tab.

        Args:
            tab_id: Unique identifier for this tab.  If not provided, a new UUID will be generated.
            parent: Optional parent widget
        """
        super().__init__(parent)
        if not tab_id:
            tab_id = str(uuid.uuid4())

        self._tab_id = tab_id
        self._is_modified = False
        self._is_updated = False
        self._has_seen_latest_update = True
        self._is_ephemeral = False
        self._path: str = ""
        self._logger = logging.getLogger(self.__class__.__name__)

        # File watching
        self._file_watcher = FileWatcher()
        self._file_exists = True

    def set_active(self, widget: QWidget, active: bool) -> None:
        """
        Set the tab as active or inactive.

        Args:
            widget: The widget that triggered the activation change
            active: True if the tab is now active, False otherwise
        """

    def activate(self) -> None:
        """Activate the tab."""

    def tab_id(self) -> str:
        """Get the tab's unique identifier."""
        return self._tab_id

    def is_modified(self) -> bool:
        """Check if the tab's content has unsaved modifications."""
        return self._is_modified

    def _set_modified(self, modified: bool) -> None:
        """Update the modified state and emit signal if changed."""
        if modified != self._is_modified:
            self._is_modified = modified
            self.modified_state_changed.emit(self._tab_id, modified)

    def is_updated(self) -> bool:
        """Check if the tab's content has been updated since last viewed."""
        return self._is_updated

    def set_updated(self, updated: bool) -> None:
        """
        Update the updated state and emit signal if changed.

        Args:
            updated: Whether the tab content has been updated
        """
        if updated != self._is_updated:
            self._is_updated = updated
            self.updated_state_changed.emit(self._tab_id, updated)

    def has_seen_latest_update(self) -> bool:
        """Check if the user has seen the latest update."""
        return self._has_seen_latest_update

    def set_has_seen_latest_update(self, seen: bool) -> None:
        """
        Set whether the user has seen the latest update.

        Args:
            seen: True if the user has seen the latest update, False otherwise
        """
        if seen != self._has_seen_latest_update:
            self._has_seen_latest_update = seen
            self.updated_state_changed.emit(self._tab_id, self._is_updated)

    def is_ephemeral(self) -> bool:
        """Check if this tab is ephemeral (will be auto-closed)."""
        return self._is_ephemeral

    def set_ephemeral(self, ephemeral: bool) -> None:
        """Set the ephemeral state of this tab."""
        self._is_ephemeral = ephemeral

    def path(self) -> str:
        """
        Get the path associated with this tab, if any.

        Returns:
            Path as a string, or None if not set
        """
        return self._path

    def tool_name(self) -> str:
        """Return the registered tool name for this tab type (e.g. 'editor', 'conversation').

        Used by TabManager for context type registration, tab info reporting, and
        icon selection.  Every concrete tab subclass must override this.
        """
        raise NotImplementedError("Subclasses must implement tool_name")

    def tab_icon(self) -> str:
        """Return the icon name used in the tab bar (e.g. 'editor', 'conversation').

        Defaults to tool_name().  Override only when the icon name differs.
        """
        return self.tool_name()

    def tab_title_from_path(self) -> str:
        """Derive a display title from the tab's current path.

        Used when restoring tabs and when updating labels after a rename.
        The default returns the basename of the path, which is correct for most
        tab types.  Override for tabs that need a different derivation (e.g.
        conversation strips the extension, terminal uses a fixed string).
        """
        return os.path.basename(self._path) if self._path else ""

    def on_modified_changed(self, modified: bool) -> None:
        """Called by TabManager when this tab's modified state changes.

        Override to implement tab-type-specific behaviour such as updating the
        tab label or making an ephemeral tab permanent.

        Args:
            modified: True if the tab now has unsaved changes.
        """

    def on_path_renamed(self, new_path: str) -> None:
        """Called by TabManager when the file at this tab's path has been renamed.

        The default implementation calls set_path() so file watching is updated.
        Override to also update the tab bar label or perform other tab-specific
        housekeeping.  Implementations must call super().on_path_renamed(new_path)
        or call set_path() themselves.

        Args:
            new_path: The new absolute path after the rename.
        """
        self.set_path(new_path)

    def register_context_models(self, registry: ContextRegistry) -> None:
        """Register context models for this tab with the given registry.

        Called after the tab is added to a column and after a tab is moved
        between columns.  The default implementation is a no-op.  Tabs that
        own a context model (editor, terminal, preview, conversation) override
        this to register it.

        Args:
            registry: The ContextRegistry for the active mindspace.
        """
    def set_path(self, path: str) -> None:
        """
        Set the path associated with this tab.

        Args:
            path: Path to associate with this tab
        """
        raise NotImplementedError("Subclasses must implement set_path")

    def is_path_missing(self) -> bool:
        """
        Check if the path associated with this tab exists.

        Returns:
            True if the path does not exist, False otherwise
        """
        # If we don't have a path, it can't be missing
        if not self._path:
            return False

        return not os.path.exists(self._path)

    def _start_file_watching(self, path: str) -> None:
        """
        Start watching a file for changes.

        Args:
            path: Absolute path to watch
        """
        if not path:
            return

        assert os.path.isabs(path), f"Path must be absolute: {path}"

        # Stop watching the old path if any
        self.stop_file_watching()

        try:
            self._file_watcher.watch_file(path, self._handle_file_changed)
            self._logger.debug("Started watching file: %s", path)

            # Check initial file state
            self._check_file_state(path)

        except Exception as e:
            self._logger.error("Failed to start file watching for %s: %s", path, str(e))

    def stop_file_watching(self) -> None:
        """Stop watching the current file."""
        if self._path:
            try:
                self._file_watcher.unwatch_file(self._path, self._handle_file_changed)
                self._logger.debug("Stopped watching file: %s", self._path)

            except Exception as e:
                self._logger.error("Failed to stop file watching for %s: %s", self._path, str(e))

    def _handle_file_changed(self, changed_path: str) -> None:
        """
        Handle notification that the watched file has changed.

        Args:
            changed_path: Path of the file that changed
        """
        self._logger.debug("File changed: %s", changed_path)
        self._check_file_state(changed_path)

    def _check_file_state(self, path: str) -> None:
        """
        Check if the file exists and emit signal if state changed.

        Args:
            path: Path to check
        """
        try:
            file_exists = os.path.exists(path)
            if file_exists != self._file_exists:
                self._file_exists = file_exists
                self.file_state_changed.emit(self._tab_id, file_exists)
                self._logger.debug("File state changed for %s: exists=%s", path, file_exists)

        except Exception as e:
            self._logger.error("Failed to check file state for %s: %s", path, str(e))
            # Assume file doesn't exist if we can't check
            if self._file_exists:
                self._file_exists = False
                self.file_state_changed.emit(self._tab_id, False)

    def get_state(self, temp_state: bool) -> TabState:
        """
        Get serializable state for mindspace persistence.

        Must be implemented by subclasses to provide their specific state.

        Args:
            temp_state: True if we're saving temporary state to restore locally,
                False if we're persisting state

        Returns:
            TabState object containing serializable state
        """
        raise NotImplementedError("Subclasses must implement get_state")

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'TabBase':
        """
        Create and restore a tab from serialized state.

        Must be implemented by subclasses to handle their specific state.

        Args:
            state: TabState object containing serialized state
            parent: Parent widget

        Returns:
            Newly created and restored tab instance

        Raises:
            ValueError: If state is invalid for this tab type
        """
        raise NotImplementedError("Subclasses must implement restore_from_state")

    def can_close_tab(self) -> bool:
        """
        Check if the tab can be closed.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_close")

    def close_tab(self) -> None:
        """
        Close the tab's content if applicable.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement save")

    def can_save(self) -> bool:
        """
        Check if the tab can be saved.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_save")

    def save(self) -> bool:
        """
        Save the tab's content if applicable.

        Must be implemented by subclasses.

        Returns:
            bool: True if save was successful or not needed
        """
        raise NotImplementedError("Subclasses must implement save")

    def can_save_as(self) -> bool:
        """
        Check if the tab can be saved "as".

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_save_as")

    def save_as(self) -> bool:
        """
        Save the tab's content "as" if applicable.

        Must be implemented by subclasses.

        Returns:
            bool: True if save was successful or not needed
        """
        raise NotImplementedError("Subclasses must implement save_as")

    def can_undo(self) -> bool:
        """
        Check if undo operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_undo")

    def undo(self) -> None:
        """
        Perform undo operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement undo")

    def can_redo(self) -> bool:
        """
        Check if redo operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_redo")

    def redo(self) -> None:
        """
        Perform redo operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement redo")

    def can_cut(self) -> bool:
        """
        Check if cut operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_cut")

    def cut(self) -> None:
        """
        Perform cut operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement cut")

    def can_copy(self) -> bool:
        """
        Check if copy operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_copy")

    def copy(self) -> None:
        """
        Perform copy operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement copy")

    def can_paste(self) -> bool:
        """
        Check if paste operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_paste")

    def paste(self) -> None:
        """
        Perform paste operation.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement paste")

    def show_find(self) -> None:
        """Show the find widget."""
        raise NotImplementedError("Subclasses must implement show_find")

    def can_show_find_replace(self) -> bool:
        """Return True if this tab supports find-and-replace."""
        return False

    def show_find_replace(self) -> None:
        """Show the find widget with the replace row expanded."""

    def can_show_goto_line(self) -> bool:
        """Return True if this tab supports go-to-line."""
        return False

    def show_goto_line(self) -> None:
        """Show the go-to-line dialog."""

    def apply_find_search(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:  # pylint: disable=unused-argument
        """Apply a programmatic find/highlight request when supported."""
        return None

    def _close_find(self) -> None:
        """Close the find widget and clear any active find state."""
        return None

    def apply_search_highlight(self, text: str, case_sensitive: bool = False, regexp: bool = False) -> None:  # pylint: disable=unused-argument
        """Apply a transient search highlight without mutating local find UI state."""
        return None

    def navigate_to_search_match(
        self, text: str, line_number: int | None, message_id: str | None, case_sensitive: bool = False, regexp: bool = False  # pylint: disable=unused-argument
    ) -> None:
        """Close any active find, then highlight all matches and scroll to the one at line_number."""
        self._close_find()
        self.apply_search_highlight(text, case_sensitive=case_sensitive, regexp=regexp)

    def clear_search_highlight(self) -> None:
        """Clear any transient search highlight without affecting local find UI state."""
        return None

    def can_submit(self) -> bool:
        """
        Check if submit operation is available.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement can_submit")

    def submit(self) -> None:
        """
        Submit the current message.

        Must be implemented by subclasses.
        """
        raise NotImplementedError("Subclasses must implement submit")

    def update_status(self) -> None:
        """
        Update status bar with tab-specific status information.

        Must be implemented by subclasses to provide their specific status.
        """
        raise NotImplementedError("Subclasses must implement update_status")

    def clear_history(self) -> None:
        """Clear the tab's history if it has one.

        No-op by default.  Override in tab types that maintain a command or
        message history (e.g. ShellTab).
        """

    def can_show_conversation_settings_dialog(self) -> bool:
        """Return True if this tab can show a conversation settings dialog.

        False by default.  Override in ConversationTab.
        """
        return False

    def show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog if supported.

        No-op by default.  Override in ConversationTab.
        """

    def handle_esc_key(self) -> bool:
        """Handle the Escape key if this tab has a use for it.

        Returns True if the key was consumed, False otherwise.
        No-op by default.  Override in ConversationTab.
        """
        return False

    def can_navigate_next_message(self) -> bool:
        """Return True if this tab supports forward message/hunk navigation.

        False by default.  Override in tabs that support navigation
        (ConversationTab, DiffTab, LogTab, ShellTab).
        """
        return False

    def navigate_next_message(self) -> None:
        """Navigate to the next message or hunk if supported.

        No-op by default.  Override in tabs that support navigation.
        """

    def can_navigate_previous_message(self) -> bool:
        """Return True if this tab supports backward message/hunk navigation.

        False by default.  Override in tabs that support navigation.
        """
        return False

    def navigate_previous_message(self) -> None:
        """Navigate to the previous message or hunk if supported.

        No-op by default.  Override in tabs that support navigation.
        """

    def is_navigating_as_hunks(self) -> bool:
        """Return True if this tab navigates by diff hunks rather than messages.

        False by default.  Override in DiffTab.
        """
        return False

    def preferred_width(self) -> int | None:
        """
        Return the preferred maximum pixel width for this tab's column, or None for no preference.

        When None, the column containing this tab will take whatever space is available after
        constrained columns have been allocated their preferred widths.
        """
        return None

    def apply_style(self) -> None:
        """Apply current style settings to the tab and its content.

        Called by TabManager when the application style changes.  The default
        implementation is a no-op.  Subclasses that own styled content widgets
        should override this to forward the call to those widgets.
        """
