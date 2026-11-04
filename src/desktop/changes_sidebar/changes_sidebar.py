"""Recent changes panel: lists the mutations recorded in the change journal."""

from collections.abc import Callable
from datetime import date, datetime, timezone
import os

from PySide6.QtCore import QFileInfo, QPoint, QSize, Qt, Signal
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import (
    QFrame, QLabel, QSpacerItem, QSizePolicy, QTreeWidget, QTreeWidgetItem, QVBoxLayout, QWidget,
)

from mindspace.file_journal import FileJournal, JournalPurgeResult
from mindspace.journal_entry import JournalEntry, JournalActor
from mindspace.journal_reversal import JournalRevertOutcome

from diff_context.diff_context import JournalDiffTarget
from desktop.language.language_manager import LanguageManager
from desktop.file_watcher.file_watcher import FileWatcher
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


class ChangesSidebar(SidebarBase):
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

        self._layout = QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(0)

        self._header = SidebarSectionHeader(self._language_manager.strings().changes, self)
        self._layout.addWidget(self._header)

        self._status_label = QLabel(self)
        self._status_label.setIndent(0)
        self._status_label.setObjectName("_status_label")
        self._status_label.setWordWrap(True)
        self._layout.addWidget(self._status_label)

        self._tree = QTreeWidget(self)
        self._tree.setObjectName("ChangesSidebarTree")
        self._tree.setStyle(SidebarTreeStyle())
        self._tree.setFrameShape(QFrame.Shape.NoFrame)
        self._tree.setHeaderHidden(True)
        self._tree.setRootIsDecorated(False)
        self._tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._show_context_menu)
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
        self._last_sequence = journal.newest_sequence()
        self._journal_bytes = journal.size_bytes()
        self._remember_entries(journal.all_entries())
        self._render()

    def _render(self) -> None:
        """Show the entries held, or the empty state when there are none."""
        strings = self._language_manager.strings()
        if not self._entries:
            self._show_empty_state(strings.changes_empty_message)
            return

        self._show_tree()
        self._populate()

    def _remember_entries(self, entries: list[JournalEntry]) -> None:
        """
        Keep the newest _MAX_ROWS entries and count the rest as older.

        Args:
            entries: The entries to keep, oldest first.  Any already counted as
                older are not repeated here, so the count accumulates.
        """
        self._entries = entries[-_MAX_ROWS:]
        self._older_count += len(entries) - len(self._entries)
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
        if self._entries:
            lines.append(strings.changes_horizon.format(self._format_time(self._entries[0].timestamp)))

        if self._journal_bytes > FileJournal.WARN_BYTES:
            lines.append(strings.changes_size_warning.format(self._journal_bytes / (1024 * 1024)))

        self._status_label.setText("\n".join(lines))
        self._status_label.show()
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
        """Return the local date an entry was recorded, or None when unreadable."""
        try:
            return datetime.fromisoformat(entry.timestamp).astimezone().date()

        except ValueError:
            return None

    @staticmethod
    def _format_time(timestamp: str) -> str:
        """
        Render a recorded timestamp in the user's local time, or as stored.

        Args:
            timestamp: The ISO-8601 timestamp recorded with an entry.
        """
        try:
            return datetime.fromisoformat(timestamp).astimezone().strftime("%Y-%m-%d %H:%M")

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

        Args:
            entry: The entry to describe.
        """
        actor = "AI" if entry.actor is JournalActor.AI else "You"
        when = self._format_time(entry.timestamp)

        lines = [entry.path, f"{entry.operation.value} · {actor} · {when}"]
        if entry.conversation:
            lines.append(f"conversation {entry.conversation}")

        if not entry.is_revertable():
            lines.append("this change cannot be reverted")

        return "\n".join(lines)

    def _icon_for(self, entry: JournalEntry) -> QIcon:
        """
        Return the icon representing a mutated path.

        Args:
            entry: The entry naming the path.
        """
        mindspace = self._mindspace_manager.mindspace()
        absolute_path = os.path.join(mindspace.mindspace_path(), entry.path)
        return self._icon_provider.icon(QFileInfo(absolute_path))

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
        view_action = menu.addAction(strings.changes_view_replaced_action)
        view_action.setEnabled(self._replaced_version_available(entry))
        view_action.triggered.connect(
            lambda: self._view_version(entry)
        )
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
        self._header.set_title(strings.changes)
        self.refresh()
        self.apply_style()

    def apply_style(self) -> None:
        """Apply the current application style."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        self._header.apply_style()
        self._icon_provider.update_icons()

        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._status_label.setFont(font)
        self._tree.setFont(font)
        icon_size = round(16 * zoom_factor)
        self._tree.setIconSize(QSize(icon_size, icon_size))

        subtle_text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        inset = self._style_manager.scale(SidebarSectionHeader.TITLE_INSET)

        self.setStyleSheet(
            build_tree_pane_stylesheet(
                self._style_manager,
                "ChangesSidebar",
                "QTreeWidget#ChangesSidebarTree",
                self.layoutDirection(),
                zoom_factor,
            )
            + f"""
            QLabel#_status_label {{
                color: {subtle_text};
                padding: {round(12 * zoom_factor)}px {inset}px;
            }}
            QTreeWidget#ChangesSidebarTree::item {{
                padding-left: {inset}px;
            }}
            QTreeWidget#ChangesSidebarTree {{
                border: none;
            }}
            """
        )
