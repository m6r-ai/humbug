"""Journal panel: lists the mutations recorded in the change journal."""

from collections.abc import Callable
from datetime import date, datetime, timezone
import os

from PySide6.QtCore import QDateTime, QFileInfo, QPoint, QSize, Qt, Signal
from PySide6.QtGui import QDrag, QIcon, QMouseEvent
from PySide6.QtWidgets import (
    QApplication, QFrame, QLabel, QSpacerItem, QSizePolicy, QTreeWidget, QTreeWidgetItem,
    QVBoxLayout, QWidget, QDateTimeEdit, QHBoxLayout, QToolButton,
)

from mindspace.file_journal import FileJournal, JournalPurgeResult
from mindspace.journal_entry import JournalEntry, JournalActor
from mindspace.journal_reversal import JournalRevertOutcome

from diff_context.diff_context import JournalDiffTarget
from desktop.drop_payload import journal_mime_data
from desktop.language.language_manager import LanguageManager
from desktop.file_watcher.file_watcher import FileWatcher
from desktop.journal_sidebar.recorded_version import open_recorded_version
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar.sidebar_pane_style import build_tree_pane_stylesheet
from desktop.sidebar.sidebar_row_spacing_delegate import SidebarRowSpacingDelegate
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.sidebar.sidebar_tree_style import SidebarTreeStyle
from desktop.style_manager import StyleManager
from desktop.color_role import ColorRole

_ENTRY_ROLE = Qt.ItemDataRole.UserRole + 1
_MAX_ROWS = 500

class JournalSidebar(SidebarBase):
    """
    Lists recent file mutations and reverses them.

    The panel reports what actually happened rather than what was intended: every
    row is a recorded mutation, and reversing them delegates to the journal, which
    refuses to touch a path that has changed since it was recorded.  A refusal is
    shown as a result, not hidden.

    The list is bounded to the newest few hundred mutations so that a long-lived
    mindspace cannot make the panel expensive to build or to read.  The header says
    how many are not shown, and the journal's own horizon is reported as a fact.

    The entries are held in memory between refreshes, so a mutation costs a read of
    the records made since the last look rather than a re-read of the whole index.
    """

    file_opened_in_preview = Signal(str)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Build the panel's widgets."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._mindspace_manager = MindspaceManager()
        self._icon_provider = SidebarTreeIconProvider()
        self._file_watcher = FileWatcher()
        self._watched_paths: set[str] = set()
        self._mindspace_path = ""
        self._empty_state_spacer: QSpacerItem | None = None
        self._entries: list[JournalEntry] = []
        self._by_sequence: dict[int, JournalEntry] = {}
        self._older_count = 0
        self._last_sequence = 0
        self._journal_bytes = 0
        self._until: datetime | None = None

        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(0)

        self._header = SidebarSectionHeader(self._language_manager.strings().journal, self)
        self._layout.addWidget(self._header)

        window_bar = QWidget(self)
        window_bar.setObjectName("_window_bar")
        self._window_layout = QHBoxLayout(window_bar)
        self._window_layout.setSpacing(0)

        self._window_button = QToolButton(window_bar)
        self._window_button.setObjectName("windowButton")
        self._window_button.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
        self._window_button.clicked.connect(self._on_window_button_clicked)
        self._window_layout.addWidget(self._window_button)

        self._clear_window_button = QToolButton(window_bar)
        self._clear_window_button.setObjectName("windowButton")
        self._clear_window_button.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
        self._clear_window_button.clicked.connect(self._show_all_changes)
        self._window_layout.addWidget(self._clear_window_button)
        self._window_layout.addStretch()

        self._layout.addWidget(window_bar)

        self._window_editor = QWidget(self)
        self._window_editor.setObjectName("_window_editor")
        editor_layout = QVBoxLayout(self._window_editor)
        editor_layout.setContentsMargins(0, 0, 0, 0)
        editor_layout.setSpacing(0)

        self._until_edit = QDateTimeEdit(self._window_editor)
        self._until_edit.setObjectName("sinceEdit")
        self._until_edit.setCalendarPopup(True)
        self._until_edit.setDisplayFormat("yyyy-MM-dd HH:mm")
        self._until_edit.dateTimeChanged.connect(self._on_until_changed)
        editor_layout.addWidget(self._until_edit)

        self._window_editor.hide()
        self._layout.addWidget(self._window_editor)

        self._status_label = QLabel(self)
        self._status_label.setIndent(0)
        self._status_label.setObjectName("_status_label")
        self._status_label.setWordWrap(True)
        self._layout.addWidget(self._status_label)

        self._tree = JournalTreeWidget(self._drag_sequence_for, self)
        self._tree.setObjectName("JournalSidebarTree")
        self._tree.setStyle(SidebarTreeStyle())
        self._tree.setFrameShape(QFrame.Shape.NoFrame)
        self._tree.setHeaderHidden(True)
        self._tree.setRootIsDecorated(False)
        self._tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._show_context_menu)
        self._tree.itemClicked.connect(self._on_item_clicked)
        self._tree.itemDoubleClicked.connect(self._on_item_double_clicked)
        self._tree.setItemDelegate(SidebarRowSpacingDelegate(self._tree, self._style_manager))
        self._layout.addWidget(self._tree)

        self._on_language_changed()

    def set_mindspace(self, path: str) -> None:
        """Set the active mindspace root and refresh the listing."""
        self._mindspace_path = path
        self._forget_entries()
        if path:
            self._watch_journal()

        else:
            self._unwatch_journal()

        self.refresh()

    def _unwatch_journal(self) -> None:
        """Unregister every journal path currently being watched."""
        for path in self._watched_paths:
            self._file_watcher.unwatch_file(path, self._on_journal_changed)

        self._watched_paths.clear()

    def _watch_journal(self) -> None:
        """
        Watch the journal for new entries.

        Appends to the segment in use change only its modification time, while
        rotation changes the directory listing, so both the directory and the
        newest segment are watched.
        """
        self._unwatch_journal()
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return

        journal_dir = mindspace.journal_dir()
        for path in (journal_dir, self._newest_segment(journal_dir)):
            if path:
                self._file_watcher.watch_file(path, self._on_journal_changed)
                self._watched_paths.add(path)

    @staticmethod
    def _newest_segment(journal_dir: str) -> str:
        """
        Return the segment new entries are appended to, or an empty string.

        Args:
            journal_dir: The journal directory.
        """
        try:
            names = [name for name in os.listdir(journal_dir) if name.endswith(".jsonl")]

        except FileNotFoundError:
            return ""

        return os.path.join(journal_dir, max(names)) if names else ""

    def _on_journal_changed(self, _path: str) -> None:
        """
        Re-establish the watches and pick up what has been recorded since.

        Rotation moves the newest segment, so the registrations are rebuilt first.
        Only the new records are read: the entries already shown are kept, so a
        mutation costs a read of the newest segment rather than of the whole index.

        Args:
            _path: The path the watcher reported, which is one of the journal paths.
        """
        self._watch_journal()
        self._refresh_from_journal()

    def _refresh_from_journal(self) -> None:
        """
        Add the records made since the listing was last built.

        History can also be removed under the panel, by a purge or by a
        cancellation, and neither can be applied incrementally: both reload.
        """
        strings = self._language_manager.strings()
        mindspace = self._mindspace_manager.mindspace()
        if not self._mindspace_path or not mindspace.has_mindspace():
            self._forget_entries()
            self._show_empty_state(strings.changes_no_mindspace_message)
            return

        journal = mindspace.file_journal()
        if self._until is not None:
            # A windowed listing cannot be extended from the tail: the new records may
            # fall outside it, and the count of what is not shown depends on the window.
            self.refresh()
            return

        newest = journal.newest_sequence()
        if newest < self._last_sequence:
            self.refresh()
            return

        added = newest - self._last_sequence
        if added == 0:
            return

        fresh = journal.entries_from(self._last_sequence + 1, added)
        if len(fresh) < added:
            self.refresh()
            return

        self._last_sequence = newest
        self._remember_entries(self._entries + fresh)
        self._tree.clear()
        self._render()

    def refresh(self) -> None:
        """
        Reload the listing from the change journal.

        This is the expensive path, used when the panel is activated, when the
        mindspace changes, and whenever history was removed under it.  It is also
        where the journal's size is measured, because a directory walk on every
        appended record would cost more than the guard is worth.
        """
        strings = self._language_manager.strings()
        mindspace = self._mindspace_manager.mindspace()
        self._tree.clear()

        if not self._mindspace_path or not mindspace.has_mindspace():
            self._forget_entries()
            self._show_empty_state(strings.changes_no_mindspace_message)
            return

        journal = mindspace.file_journal()
        self._journal_bytes = journal.size_bytes()

        # The window selects from the store, rather than filtering what was already
        # selected.  Filtering the newest rows and then applying a window would report
        # "nothing recorded" whenever the window lies beyond the rows held, which is the
        # one answer that is definitely wrong.
        entries = journal.all_entries()
        until = self._until
        if until is not None:
            entries = [entry for entry in entries if self._recorded_by(entry, until)]

        self._last_sequence = journal.newest_sequence()
        self._remember_entries(entries, replace=True)
        self._render()

    def _recorded_by(self, entry: JournalEntry, until: datetime) -> bool:
        """
        Return True when an entry was recorded at or before the chosen time.

        The bound is inclusive so that choosing the time shown on a row includes that
        row, which is what anyone picking a time off the list will expect.

        Args:
            entry: The entry to test.
            until: The bound, which the caller has already established is set.
        """
        try:
            return datetime.fromisoformat(entry.timestamp) <= until

        except ValueError:
            return False

    def _render(self) -> None:
        """Show the entries held, or the empty state when there are none."""
        strings = self._language_manager.strings()
        self._update_window_button()
        if not self._entries:
            if self._until is None:
                self._show_empty_state(strings.changes_empty_message)

            else:
                self._show_empty_state(strings.changes_until.format(self._format_time(self._until.isoformat())))

            return

        self._show_tree()
        self._populate()

    def _remember_entries(self, entries: list[JournalEntry], replace: bool = False) -> None:
        """
        Keep the newest _MAX_ROWS entries, counting the rest as older.

        The two callers mean different things by the list they pass.  A reload passes
        every entry the journal holds, so the count is rebuilt from scratch.  An
        incremental refresh passes only what was already held plus the new records, so
        the entries counted as older on a previous pass are not repeated and the count
        accumulates.  Treating a reload as incremental makes the count grow without
        bound, because each reload adds its overflow to every earlier reload's.

        Args:
            entries: The entries to keep, oldest first.
            replace: True when entries is the whole history, so the older count is
                rebuilt rather than added to.
        """
        self._entries = entries[-_MAX_ROWS:]
        older = len(entries) - len(self._entries)
        self._older_count = older if replace else self._older_count + older
        self._by_sequence = {entry.sequence: entry for entry in self._entries}

    def _forget_entries(self) -> None:
        """Discard everything the listing holds."""
        self._entries = []
        self._by_sequence = {}
        self._older_count = 0
        self._last_sequence = 0
        self._journal_bytes = 0

    def _show_tree(self) -> None:
        """Show the tree and report how far back the journal reaches."""
        strings = self._language_manager.strings()
        lines: list[str] = []
        if self._until is not None:
            lines.append(strings.changes_until.format(self._format_time(self._until.isoformat())))

        if self._journal_bytes > FileJournal.WARN_BYTES:
            lines.append(strings.changes_size_warning.format(self._journal_bytes / (1024 * 1024)))

        self._status_label.setText("\n".join(lines))

        # The label carries vertical padding, so showing it with nothing to say leaves a
        # blank band under the controls.  There is usually nothing to say: the bound is
        # only reported when one is set, and the size warning only when it applies.
        self._status_label.setVisible(bool(lines))
        self._tree.show()
        if self._empty_state_spacer is not None:
            self._layout.removeItem(self._empty_state_spacer)
            self._empty_state_spacer = None

    def _show_empty_state(self, message: str) -> None:
        """Show a message in place of the tree, without leaving an empty box behind."""
        self._status_label.setText(message)
        self._status_label.show()
        self._tree.hide()
        if self._empty_state_spacer is None:
            self._empty_state_spacer = QSpacerItem(0, 0, QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Expanding)
            self._layout.addItem(self._empty_state_spacer)

    def _populate(self) -> None:
        """
        Build the tree from the entries held, newest first, split into buckets.

        Only the newest _MAX_ROWS entries are listed, with a final row saying how
        many older ones exist, so a long history cannot make the panel unbounded.
        """
        strings = self._language_manager.strings()
        headers: dict[str, QTreeWidgetItem] = {}
        today = datetime.now(timezone.utc).date()

        for entry in reversed(self._entries):
            bucket = strings.changes_bucket_today if self._entry_date(entry) == today else strings.changes_bucket_earlier
            if bucket not in headers:
                headers[bucket] = self._add_header(bucket)

            item = QTreeWidgetItem(self._tree, [self._row_text(entry)])
            item.setIcon(0, self._icon_for(entry))
            item.setToolTip(0, self._tooltip_text(entry))
            item.setData(0, _ENTRY_ROLE, entry.sequence)

        if self._older_count:
            self._add_header(strings.changes_older_not_shown.format(self._older_count))

    def _add_header(self, text: str) -> QTreeWidgetItem:
        """
        Add a non-selectable row that introduces a group of entries.

        Groups are rendered flat rather than nested: nesting indents every entry
        by the tree's indentation, which costs more width than the grouping is
        worth in a narrow panel.

        Args:
            text: The group label.
        """
        header = QTreeWidgetItem(self._tree, [text])
        header.setFlags(Qt.ItemFlag.NoItemFlags)
        return header

    @staticmethod
    def _entry_date(entry: JournalEntry) -> date | None:
        """Return the date an entry was recorded, or None when unreadable."""
        try:
            return datetime.fromisoformat(entry.timestamp).date()

        except ValueError:
            return None

    @staticmethod
    def _format_time(timestamp: str) -> str:
        """
        Render a recorded timestamp as stored, or as it is written when unreadable.

        Timestamps are recorded and reported in UTC throughout, so they are not
        converted for display: a reader comparing the list against the time they
        chose in the picker is comparing two UTC values.

        Args:
            timestamp: The ISO-8601 timestamp recorded with an entry.
        """
        try:
            return datetime.fromisoformat(timestamp).strftime("%Y-%m-%d %H:%M")

        except ValueError:
            return timestamp

    def _row_text(self, entry: JournalEntry) -> str:
        """
        Describe one mutation in a single line.

        Args:
            entry: The entry to describe.
        """
        return os.path.basename(entry.path) or entry.path

    def _tooltip_text(self, entry: JournalEntry) -> str:
        """
        Describe a mutation in full, for the row's tooltip.

        Row labels show only the file name because paths are routinely too long
        for the panel, so everything else the row encodes is reported here.
        The path is the mindspace-relative one the entry records, and a
        conversation recorded as a transcript path is reported the same way;
        the mindspace root is the same for every row and would only lengthen
        each of them.

        Args:
            entry: The entry to describe.
        """
        if entry.actor is JournalActor.AI:
            actor = "AI"

        elif entry.actor is JournalActor.HUMAN:
            actor = "You"

        else:
            actor = "Detected"

        when = self._format_time(entry.timestamp)

        lines = [entry.path, f"{entry.operation.value} · {actor} · {when}"]
        if entry.conversation:
            lines.append(f"conversation {self._relative_conversation(entry.conversation)}")

        if not entry.is_revertable():
            lines.append("this change cannot be reverted")

        return "\n".join(lines)

    def _relative_conversation(self, conversation: str) -> str:
        """
        Name the conversation a change belongs to, relative to the mindspace.

        A conversation is identified by the identifier its transcript carries,
        which is not a path at all.  Entries written before transcripts carried
        identifiers hold the transcript's path instead, and that is reported
        relative to the mindspace root like every other path in the panel.

        Args:
            conversation: The conversation value recorded with the entry.

        Returns:
            The identifier unchanged, or the transcript path made relative.
        """
        if not os.path.isabs(conversation):
            return conversation

        return self._mindspace_manager.get_relative_path(conversation)

    def _icon_for(self, entry: JournalEntry) -> QIcon:
        """
        Return the icon representing a mutated path.

        Args:
            entry: The entry naming the path.
        """
        mindspace = self._mindspace_manager.mindspace()
        absolute_path = os.path.join(mindspace.mindspace_path(), entry.path)
        return self._icon_provider.icon(QFileInfo(absolute_path))

    def _refresh_row_icons(self) -> None:
        """
        Re-request every row's icon at the current zoom factor.

        Qt does not enlarge an icon beyond the pixmap it already holds, so a row
        that keeps the icon handed to it when the listing was built stays at the
        size it was drawn then.  The icon provider's cache is cleared before this
        runs, so each icon is rebuilt at the zoom factor in force.
        """
        for index in range(self._tree.topLevelItemCount()):
            item = self._tree.topLevelItem(index)
            if item is None:
                continue

            entry = self._entry_for_item(item)
            if entry is not None:
                item.setIcon(0, self._icon_for(entry))

    def _entry_for_item(self, item: QTreeWidgetItem) -> JournalEntry | None:
        """
        Return the entry a tree item stands for, if it stands for one.

        Args:
            item: The tree item under the cursor.
        """
        sequence = item.data(0, _ENTRY_ROLE)
        if sequence is None:
            return None

        return self._by_sequence.get(sequence)

    def _on_item_clicked(self, item: QTreeWidgetItem) -> None:
        """
        Show the version the clicked change produced, beside whatever is open.

        Clicking a row asks about that change, so it shows the change and not the
        file: the two are the same only while nothing has rewritten the file since.

        Args:
            item: The row that was clicked.
        """
        entry = self._entry_for_item(item)
        if entry is not None:
            open_recorded_version(entry.sequence, ephemeral=True)

    def _on_item_double_clicked(self, item: QTreeWidgetItem) -> None:
        """
        Show the version the double-clicked change produced, and keep the tab open.

        The first click of the pair has already opened the version ephemerally, so
        this promotes that tab rather than opening a second one.

        Args:
            item: The row that was double-clicked.
        """
        entry = self._entry_for_item(item)
        if entry is not None:
            open_recorded_version(entry.sequence, ephemeral=False)

    def _drag_sequence_for(self, item: QTreeWidgetItem) -> int | None:
        """
        Return the change a row can be dragged as, or None when it names none.

        A change that produced no version at all - a removal, or a directory - cannot
        be shown wherever it is dropped, so the drag does not start.  Whether the
        journal still holds the content is a question about a blob rather than about
        the row, and is left to the drop, which reads it once instead of on every
        mouse move.

        Args:
            item: The row being dragged.
        """
        entry = self._entry_for_item(item)
        if entry is None or not self._this_version_available(entry):
            return None

        return entry.sequence

    def _show_context_menu(self, position: QPoint) -> None:
        """
        Offer the actions that apply to the row under the cursor.

        Right-clicking outside a row offers forgetting all history instead,
        since that is not about any one change.

        Args:
            position: Cursor position within the tree.
        """
        item = self._tree.itemAt(position)
        if item is None:
            self._show_retention_menu(position)
            return

        entry = self._entry_for_item(item)
        if entry is None:
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)

        this_action = menu.addAction(strings.changes_view_this_version_action)
        this_action.setEnabled(self._this_version_available(entry))
        this_action.triggered.connect(lambda: self._view_this_version(entry))

        replaced_action = menu.addAction(strings.changes_view_replaced_action)
        replaced_action.setEnabled(self._replaced_version_available(entry))
        replaced_action.triggered.connect(lambda: self._view_version(entry))

        compare_action = menu.addAction(strings.changes_compare_action)
        compare_action.setEnabled(self._replaced_version_available(entry))
        compare_action.triggered.connect(lambda: self._compare_with_current(entry))

        menu.addSeparator()
        menu.addAction(strings.changes_revert_file_action).triggered.connect(
            lambda: self._revert(self._entries_for(entry))
        )
        menu.addAction(strings.changes_restore_to_point_action).triggered.connect(
            lambda: self._restore_to_point(entry)
        )
        menu.addSeparator()
        menu.addAction(strings.changes_forget_through_action).triggered.connect(
            lambda: self._forget_through(entry)
        )
        menu.exec(self._tree.viewport().mapToGlobal(position))

    def _show_retention_menu(self, position: QPoint) -> None:
        """
        Offer forgetting the whole journal.

        Args:
            position: Cursor position within the tree.
        """
        mindspace = self._mindspace_manager.mindspace()
        if not self._mindspace_path or not mindspace.has_mindspace():
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)
        menu.addAction(strings.changes_forget_all_action).triggered.connect(self._forget_all)
        menu.exec(self._tree.viewport().mapToGlobal(position))

    def _on_window_button_clicked(self) -> None:
        """
        Reveal or hide the time picker in place.

        A modal dialog for this interrupts the panel to ask one question and hides the
        list while it is answered, which is the opposite of what a filter is for.  The
        picker belongs beside the list it filters.
        """
        self._window_editor.setVisible(not self._window_editor.isVisible())
        if self._window_editor.isVisible():
            self._until_edit.setDateTime(QDateTime(self._window_end()))
            self._until_edit.setFocus()

    def _update_window_button(self) -> None:
        """
        Keep the button's wording fixed, and offer to drop the bound when there is one.

        The bound appears in the status line rather than on the button: a control that
        changes its own label to report its state reads as two different buttons, and
        the button's job is to open the picker, not to be the readout.
        """
        strings = self._language_manager.strings()
        self._window_button.setText(strings.changes_since_action)

        if self._until is None:
            self._clear_window_button.hide()
            return

        self._clear_window_button.setText("\u00d7")
        self._clear_window_button.setToolTip(strings.changes_show_all_action)
        self._clear_window_button.show()

    def _window_end(self) -> datetime:
        """
        Return the time the picker should show.

        With no bound in force the picker opens at now, because "everything up to now"
        is what the list is already showing: winding it back narrows the list, which is
        the only direction that does anything.
        """
        if self._until is not None:
            return self._until

        return datetime.now(timezone.utc)

    def _on_until_changed(self, value: QDateTime) -> None:
        """
        Apply the picker's time as the window, as soon as it changes.

        There is no OK button: the picker is the filter, and a change that has to be
        confirmed is a change the reader cannot see the effect of while choosing it.

        Args:
            value: The time the picker now shows.
        """
        self._until = value.toPython().replace(tzinfo=timezone.utc)
        self._forget_entries()
        self.refresh()

    def _show_all_changes(self) -> None:
        """Drop the time window and list everything the journal holds."""
        self._until = None
        self._window_editor.hide()
        self._forget_entries()
        self.refresh()

    def _forget_through(self, entry: JournalEntry) -> None:
        """
        Discard this change and every earlier one, after confirming it.

        Args:
            entry: The last change to forget.
        """
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return

        strings = self._language_manager.strings()
        scope = strings.changes_forget_scope_through.format(self._row_text(entry))
        self._confirm_and_purge(scope, lambda: mindspace.purge_journal_through(entry.sequence))

    def _forget_all(self) -> None:
        """Discard the whole journal, after confirming it."""
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return

        strings = self._language_manager.strings()
        scope = strings.changes_forget_scope_all
        self._confirm_and_purge(scope, mindspace.purge_whole_journal)

    def _confirm_and_purge(self, scope: str, purge: Callable[[], JournalPurgeResult]) -> None:
        """
        Confirm a destructive purge, run it, and report what it reclaimed.

        Args:
            scope: Description of what will be forgotten, for the confirmation.
            purge: The purge to run once the user agrees.
        """
        strings = self._language_manager.strings()
        confirm = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.changes_forget_title,
            strings.changes_forget_confirm.format(scope),
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True,
        )
        if confirm != MessageBoxButton.YES:
            return

        result = purge()
        MessageBox.show_message(
            self,
            MessageBoxType.INFORMATION,
            strings.changes_forget_title,
            strings.changes_forget_result.format(
                result.entries_dropped, result.bytes_reclaimed / (1024 * 1024)
            ),
        )
        self.refresh()

    def _compare_with_current(self, entry: JournalEntry) -> None:
        """
        Open a diff of the version this change replaced against the file as it is now.

        Args:
            entry: The entry whose replaced version should be compared.
        """
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return

        mindspace.contexts().open(
            context_type="diff",
            path=os.path.join(mindspace.mindspace_path(), entry.path),
            title=os.path.basename(entry.path) or entry.path,
            is_ephemeral=True,
            initial_model=JournalDiffTarget(sequence=entry.sequence),
        )

    @staticmethod
    def _this_version_available(entry: JournalEntry) -> bool:
        """
        Return True when this change produced a version the journal can show.

        A removal produced nothing, so it has no version of its own.  Anything else
        produced content, and content is shown from the change rather than from the
        file: what is on disk now is some later state, and this panel is a history.

        Args:
            entry: The entry to test.
        """
        return not entry.pre_image_is_directory and entry.post_image_hash is not None

    def _view_this_version(self, entry: JournalEntry) -> None:
        """
        Show the version this change produced, as the change recorded it.

        For a change whose file still holds exactly that content, this is also what is
        on disk.  For one that has since been edited — by anything — it is the state
        the panel is describing, and not the file.  Showing the file would quietly
        answer a different question from the one the row asks.

        The tab is ephemeral and read-only: the panel is a history, and editing from it
        would be an edit dressed up as inspection.  A change with nothing to show is
        reported as such, because this asks for it by name.

        Args:
            entry: The entry whose produced version should be shown.
        """
        if open_recorded_version(entry.sequence, ephemeral=True) is None:
            self._report_this_version_unavailable()

    @staticmethod
    def _replaced_version_available(entry: JournalEntry) -> bool:
        """
        Return True when the change replaced a version the journal still holds.

        A creation replaced nothing.  A change whose earlier content was too large
        to capture has no content of its own, but the trash may still hold it, so
        a recorded trash entry counts too.  A directory has no content to show at
        all, so it never does, even though it can be restored.

        Args:
            entry: The entry to test.
        """
        if entry.pre_image_is_directory:
            return False

        if entry.pre_image_trash is not None:
            return True

        return entry.existed_before and entry.pre_image_hash is not None

    def _view_version(self, entry: JournalEntry) -> None:
        """
        Show the version this entry recorded, in whichever view suits it.

        Text opens in a read-only editor tab, because the content exists only in
        the journal and has nowhere to be saved back to.  Content that is not
        text cannot be rendered from bytes, so it is shown in a preview tab when
        the file on disk still holds exactly that version, and reported as
        unavailable otherwise.

        Args:
            entry: The entry whose recorded version should be shown.
        """
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return

        content = mindspace.journal_pre_image(entry)
        if content is None:
            self._report_version_unavailable()
            return

        try:
            text = content.decode("utf-8")

        except UnicodeDecodeError:
            self._preview_version(entry)
            return

        mindspace.contexts().open(
            context_type="editor",
            path=os.path.join(mindspace.mindspace_path(), entry.path),
            title=os.path.basename(entry.path) or entry.path,
            is_ephemeral=True,
            read_only=True,
            initial_model=text,
        )

    def _preview_version(self, entry: JournalEntry) -> None:
        """
        Show a non-text replaced version in a preview tab, when it has a path.

        Content the trash holds can be previewed from the trash entry itself.  A
        replaced binary file whose content exists only as a journal blob has no
        path to preview, so it is reported as unavailable instead.

        Args:
            entry: The entry whose replaced version should be previewed.
        """
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace() or entry.pre_image_trash is None:
            self._report_version_unavailable()
            return

        self.file_opened_in_preview.emit(os.path.join(mindspace.trash_dir(), entry.pre_image_trash))

    def _report_version_unavailable(self) -> None:
        """Tell the user that the recorded version cannot be shown."""
        strings = self._language_manager.strings()
        MessageBox.show_message(
            self,
            MessageBoxType.INFORMATION,
            strings.changes_revert_title,
            strings.changes_version_unavailable,
        )

    def _report_this_version_unavailable(self) -> None:
        """Tell the user that this change has no version of its own to show."""
        strings = self._language_manager.strings()
        MessageBox.show_message(
            self,
            MessageBoxType.INFORMATION,
            strings.changes_revert_title,
            strings.changes_this_version_unavailable,
        )

    def _entries_for(self, entry: JournalEntry) -> list[JournalEntry]:
        """
        Return the entries that must be reverted with a given one.

        An operation that recorded more than one entry, such as a move, can only
        be reversed as a whole, so its other half is included.

        Args:
            entry: The selected entry.
        """
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return [entry]

        if not entry.group:
            return [entry]

        return [candidate for candidate in mindspace.file_journal().all_entries() if candidate.group == entry.group]

    def _restore_to_point(self, entry: JournalEntry) -> None:
        """
        Offer to undo every change recorded after a given one.

        Reversal lands each affected file on the state held before the earliest
        entry being reversed, so undoing what came after a change leaves the
        mindspace as it stood at that change.

        Args:
            entry: The change whose state should be kept.
        """
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return

        entries = mindspace.file_journal().entries_after(entry.sequence)
        strings = self._language_manager.strings()
        self._revert(entries, strings.changes_restore_to_point_confirm.format(len(entries)))

    def _revert(self, entries: list[JournalEntry], confirmation: str | None = None) -> None:
        """
        Confirm and then reverse a fixed set of entries.

        Args:
            entries: The entries to reverse, resolved before the user is asked.
            confirmation: What to ask before acting, or None to ask in the default
                terms for reversing a set of changes.
        """
        if not entries:
            return

        strings = self._language_manager.strings()
        question = confirmation or strings.changes_revert_confirm.format(len(entries))
        confirm = MessageBox.show_message(
            self,
            MessageBoxType.WARNING,
            strings.changes_revert_title,
            question,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            True,
        )
        if confirm != MessageBoxButton.YES:
            return

        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            return

        result = mindspace.revert_journal_entries(entries)
        self._report(result.reverted_paths(), result.failed_outcomes())
        self.refresh()

    def _report(self, reverted: list[str], failed: list[JournalRevertOutcome]) -> None:
        """
        Tell the user what a reversal achieved, including what it refused.

        Args:
            reverted: Paths that were restored.
            failed: Outcomes that were not restored.
        """
        strings = self._language_manager.strings()
        lines = [strings.changes_revert_result.format(len(reverted), len(failed))]

        for outcome in failed:
            lines.append(f"{outcome.path} — {outcome.status.value}: {outcome.detail}")

        MessageBox.show_message(
            self,
            MessageBoxType.WARNING if failed else MessageBoxType.INFORMATION,
            strings.changes_revert_title,
            "\n".join(lines),
        )

    def _on_language_changed(self) -> None:
        """Retranslate the static labels and rebuild the listing."""
        strings = self._language_manager.strings()
        self._header.set_title(strings.journal)
        self.refresh()
        self.apply_style()

    def apply_style(self) -> None:
        """Apply the current application style."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        self._header.apply_style()

        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._status_label.setFont(font)
        self._window_button.setFont(font)
        self._clear_window_button.setFont(font)
        self._until_edit.setFont(font)
        self._tree.setFont(font)
        icon_size = round(16 * zoom_factor)
        self._tree.setIconSize(QSize(icon_size, icon_size))
        self._icon_provider.update_icons()
        self._refresh_row_icons()

        subtle_text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        inset = self._style_manager.scale(SidebarSectionHeader.TITLE_INSET)
        tree_inset = self._style_manager.scale(SidebarSectionHeader.TITLE_INSET - 4)
        field_padding = round(3 * zoom_factor)
        self._window_layout.setContentsMargins(
            inset, round(5 * zoom_factor), inset, round(3 * zoom_factor)
        )
        self._window_layout.setSpacing(round(6 * zoom_factor))
        self._until_edit.setMinimumHeight(round(22 * zoom_factor))
        editor_layout = self._window_editor.layout()
        if editor_layout is not None:
            editor_layout.setContentsMargins(
                inset, round(3 * zoom_factor), inset, round(5 * zoom_factor)
            )

        self.setStyleSheet(
            build_tree_pane_stylesheet(
                self._style_manager,
                "JournalSidebar",
                "QTreeWidget#JournalSidebarTree",
                self.layoutDirection(),
                zoom_factor,
            )
            + f"""
            QWidget#_window_bar {{
                background-color: transparent;
                border: none;
            }}
            QToolButton#windowButton {{
                color: {subtle_text};
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND)};
                border: none;
                border-radius: 4px;
                padding: {field_padding}px;
            }}
            QToolButton#windowButton:hover {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER)};
            }}
            QDateTimeEdit#sinceEdit {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                border: 1px solid {self._style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER)};
                border-radius: 4px;
                padding: {field_padding}px;
            }}
            QLabel#_status_label {{
                color: {subtle_text};
                padding: {round(12 * zoom_factor)}px {inset}px;
            }}
            QTreeWidget#JournalSidebarTree::item {{
                padding-left: {tree_inset}px;
            }}
            QTreeWidget#JournalSidebarTree {{
                border: none;
            }}
            """
        )


class JournalTreeWidget(QTreeWidget):
    """QTreeWidget subclass that can be dragged from, carrying the change a row names."""

    def __init__(
        self,
        sequence_for: Callable[[QTreeWidgetItem], int | None],
        parent: QWidget | None = None,
    ) -> None:
        """
        Build the tree.

        Args:
            sequence_for: Returns the change a row names, or None when the row stands
                for no change or for one with nothing to show.
            parent: The panel the tree belongs to.
        """
        super().__init__(parent)
        self._sequence_for = sequence_for
        self._drag_start_pos: QPoint | None = None

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """
        Record the position a drag would start from.

        Args:
            event: The mouse press event.
        """
        if event.button() & Qt.MouseButton.LeftButton:
            self._drag_start_pos = event.pos()

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """
        Begin a drag once the cursor has moved far enough from the press point.

        The drag carries the change and nothing else, so a drop target that would
        open or move the file itself does not accept it.

        Args:
            event: The mouse move event.
        """
        if not event.buttons() & Qt.MouseButton.LeftButton:
            return

        if self._drag_start_pos is None:
            return

        if (event.pos() - self._drag_start_pos).manhattanLength() < QApplication.startDragDistance():
            return

        item = self.itemAt(self._drag_start_pos)
        if item is None:
            return

        sequence = self._sequence_for(item)
        if sequence is None:
            return

        drag = QDrag(self)
        drag.setMimeData(journal_mime_data(sequence))
        drag.setPixmap(self.viewport().grab(self.visualItemRect(item)))
        drag.setHotSpot(event.pos() - self._drag_start_pos)
        drag.exec_(Qt.DropAction.MoveAction | Qt.DropAction.CopyAction)

        self._drag_start_pos = None
