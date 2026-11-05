from collections.abc import Callable
from datetime import datetime, timezone
import json
import logging
import os
import shutil
from typing import Any
import uuid

from ai import AIConversationSettings
from ai_tool import AIToolManager
from context.context_registry import ContextRegistry
from conversation_context.conversation_context import ConversationContext

from mindspace.file_journal import FileJournal, JournalPurgeResult
from mindspace.journal_entry import JournalActor, JournalEntry
from mindspace.journal_reversal import JournalRevertResult, JournalReverser
from mindspace.mindspace_change_watcher import ChangeCheckpoint, MindspaceChangeWatcher
from mindspace.mindspace_error import MindspaceError, MindspaceExistsError, MindspaceHumbugPathError, MindspaceNotFoundError
from mindspace.mindspace_interactions import MindspaceInteractions
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace_message import MindspaceMessage
from mindspace.mindspace_scan_pruned_paths import scan_pruned_paths
from mindspace.mindspace_settings import MindspaceSettings
from mindspace.mindspace_tree_scanner import TreeSnapshot
from mindspace.mindspace_usage import MindspaceUsage
from mindspace.trash_manifest import TrashEntry, TrashManifest, strip_trash_prefix


class Mindspace:
    """
    Core mindspace model.

    Owns settings, the interaction log, path resolution, and session persistence.
    """

    MINDSPACE_DIR = ".humbug"
    SETTINGS_FILE = "settings.json"
    SESSION_FILE = "session.json"
    INTERACTIONS_DIR = "log"
    USAGE_FILE = "usage.json"
    GITIGNORE_FILE = ".gitignore"
    CONVERSATIONS_DIR = "conversations"
    TRASH_DIR = "trash"
    JOURNAL_DIR = "journal"

    def __init__(
        self,
        on_settings_changed: Callable[[], None],
        on_interactions_updated: Callable[[], None],
        on_usage_updated: Callable[[], None] | None = None,
    ) -> None:
        """
        Initialise the mindspace model.

        Args:
            on_settings_changed: Called whenever settings change or a mindspace
                is opened or closed.
            on_interactions_updated: Called whenever a new interaction is added.
            on_usage_updated: Called whenever usage stats are updated.
        """
        self._on_settings_changed = on_settings_changed
        self._on_interactions_updated = on_interactions_updated
        self._on_usage_updated = on_usage_updated
        self._path: str = ""
        self._settings: MindspaceSettings | None = None
        self._interactions = MindspaceInteractions()
        self._file_journal = FileJournal()
        self._change_watcher = MindspaceChangeWatcher("", self._file_journal, frozenset(), 5.0)
        self._usage = MindspaceUsage()
        self._context_registry = ContextRegistry()
        self._tool_manager = AIToolManager()
        self._logger = logging.getLogger("Mindspace")

    def mindspace_path(self) -> str:
        """Return the absolute path to the open mindspace, or empty string."""
        return self._path

    def has_mindspace(self) -> bool:
        """Return True if a mindspace is currently open."""
        return bool(self._path)

    def settings(self) -> MindspaceSettings | None:
        """Return current mindspace settings, or None if no mindspace is open."""
        return self._settings

    def contexts(self) -> ContextRegistry:
        """
        Return the context registry for this mindspace.

        The registry tracks all open contexts and emits events when they change.
        """
        return self._context_registry

    def check_mindspace(self, path: str) -> bool:
        """Return True if a mindspace exists at path."""
        return os.path.exists(os.path.join(path, self.MINDSPACE_DIR))

    def create_mindspace(self, path: str, folders: list[str]) -> None:
        """
        Create a new mindspace at path.

        Args:
            path: Directory in which to create the mindspace.
            folders: Subdirectories to create inside the mindspace root.

        Raises:
            MindspaceExistsError: A mindspace already exists at path.
            MindspaceError: Filesystem error during creation.
        """
        mindspace_dir = os.path.join(path, self.MINDSPACE_DIR)
        if os.path.exists(mindspace_dir):
            raise MindspaceExistsError(f"Mindspace already exists at {path}")

        try:
            os.makedirs(mindspace_dir)

            for folder in folders:
                os.makedirs(os.path.join(path, folder), exist_ok=True)

            settings = MindspaceSettings(self._tool_manager.get_default_enabled_tools())
            settings.save(os.path.join(mindspace_dir, self.SETTINGS_FILE))

            with open(os.path.join(mindspace_dir, self.SESSION_FILE), 'w', encoding='utf-8') as f:
                json.dump({"tabs": []}, f, indent=4)

            self._ensure_gitignore(mindspace_dir)

        except OSError as e:
            self._logger.error("Failed to create mindspace at %s: %s", path, str(e))
            if os.path.exists(mindspace_dir):
                shutil.rmtree(mindspace_dir, ignore_errors=True)

            for folder in folders:
                folder_path = os.path.join(path, folder)
                if os.path.exists(folder_path):
                    shutil.rmtree(folder_path, ignore_errors=True)

            raise MindspaceError(f"Failed to create mindspace: {str(e)}") from e

    def _ensure_gitignore(self, mindspace_dir: str) -> None:
        """
        Make a .humbug/ directory ignore itself in any git repository it sits in.

        A mindspace is often a folder that is already a git repository.  .humbug/ is
        Humbug's state, not the user's, so it must never be committed with their work.
        Ignoring it from inside leaves the user's own .gitignore alone.

        If Humbug comes to version .humbug/ with a repository of its own, this file
        would ignore everything in that repository too, and must be replaced by an
        exclusion that does not live inside .humbug/.

        Args:
            mindspace_dir: The .humbug/ directory.

        Raises:
            OSError: The file could not be written.
        """
        gitignore_path = os.path.join(mindspace_dir, self.GITIGNORE_FILE)
        if os.path.exists(gitignore_path):
            return

        with open(gitignore_path, 'w', encoding='utf-8') as f:
            f.write("*\n")

    def open_mindspace(self, path: str) -> None:
        """
        Open an existing mindspace.

        Args:
            path: Path to the mindspace directory.

        Raises:
            MindspaceNotFoundError: No mindspace exists at path.
            MindspaceError: Error loading settings.
        """
        mindspace_dir = os.path.join(path, self.MINDSPACE_DIR)
        if not os.path.exists(mindspace_dir):
            raise MindspaceNotFoundError(f"No mindspace found at {path}")

        # Mindspaces created before .humbug/ ignored itself get the file now.  Not
        # having it is no reason to refuse to open, so a failure is only logged.
        try:
            self._ensure_gitignore(mindspace_dir)

        except OSError as e:
            self._logger.warning("Failed to write %s in %s: %s", self.GITIGNORE_FILE, mindspace_dir, str(e))

        try:
            settings = MindspaceSettings.load(os.path.join(mindspace_dir, self.SETTINGS_FILE))
            self._path = path
            self._migrate_conversations()
            self._settings = settings
            self._open_interactions()
            self._load_usage()
            self._file_journal.open(self.journal_dir())
            self._change_watcher = MindspaceChangeWatcher(
                path,
                self._file_journal,
                scan_pruned_paths(settings.changes_pruned_paths),
                settings.changes_checkpoint_interval,
            )
            self._change_watcher.open()
            self._apply_tool_settings(settings)
            self._on_settings_changed()

        except Exception as e:
            self._logger.error("Failed to open mindspace at %s: %s", path, str(e))
            raise MindspaceError(f"Failed to open mindspace: {str(e)}") from e

    def close_mindspace(self) -> None:
        """Close the current mindspace and reset all state."""
        if self.has_mindspace():
            self._path = ""
            self._settings = None
            self._interactions.close()
            self._file_journal.close()
            self._change_watcher.close()
            self._usage = MindspaceUsage()
            self._context_registry.clear()
            self._reset_tool_manager()
            self._on_settings_changed()

    def update_settings(self, new_settings: MindspaceSettings) -> None:
        """
        Persist and apply updated settings.

        Args:
            new_settings: New settings to apply.

        Raises:
            MindspaceError: Settings could not be saved.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        settings_path = os.path.join(self._path, self.MINDSPACE_DIR, self.SETTINGS_FILE)
        try:
            new_settings.save(settings_path)
            self._settings = new_settings
            self._apply_tool_settings(new_settings)
            self._change_watcher = MindspaceChangeWatcher(
                self._path,
                self._file_journal,
                scan_pruned_paths(new_settings.changes_pruned_paths),
                new_settings.changes_checkpoint_interval,
            )
            self._change_watcher.open()
            self._on_settings_changed()

        except OSError as e:
            raise MindspaceError(f"Failed to save mindspace settings: {str(e)}") from e

    def apply_ai_settings_to_all(self, settings: AIConversationSettings) -> None:
        """
        Persist AI settings as the mindspace default and apply them to every open conversation.

        Updates the mindspace default's AI fields, then broadcasts the settings to
        each open conversation context.  The UI reacts through each context's
        on_settings_applied callback, so the mindspace remains the source of truth.

        Args:
            settings: The AIConversationSettings to apply to all conversations.
        """
        current = self._settings
        if current is not None:
            current.model = settings.model
            current.provider = settings.provider
            current.temperature = settings.temperature
            current.reasoning = settings.reasoning
            current.reasoning_effort = settings.reasoning_effort
            self.update_settings(current)

        for info in self._context_registry.list_all():
            model = self._context_registry.get_model(info.context_id, ConversationContext)
            if model is not None:
                model.update_conversation_settings(settings)

    def conversations_dir(self) -> str:
        """
        Return the absolute path to the conversations directory.

        The conversations directory lives inside the .humbug directory so that
        conversations do not clutter the user's mindspace content.  The directory
        is created if it does not already exist.

        Returns:
            Absolute path to the conversations directory.

        Raises:
            MindspaceError: No mindspace is open or directory could not be created.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        path = os.path.join(self._path, self.MINDSPACE_DIR, self.CONVERSATIONS_DIR)
        try:
            os.makedirs(path, exist_ok=True)

        except OSError as e:
            raise MindspaceError(f"Failed to create conversations directory: {str(e)}") from e

        return path

    def conversations_rel_path(self) -> str:
        """
        Return the mindspace-relative path to the conversations directory.

        Returns:
            Path relative to the mindspace root, e.g. '.humbug/conversations'.
        """
        return os.path.join(self.MINDSPACE_DIR, self.CONVERSATIONS_DIR)

    def journal_dir(self) -> str:
        """
        Return the absolute path to the change journal directory, creating it if needed.

        The journal lives inside .humbug so that the AI can neither read it nor
        invoke a reversal: every tool path resolver refuses .humbug/ paths.

        Returns:
            Absolute path to the journal directory.

        Raises:
            MindspaceError: No mindspace is open or directory could not be created.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        path = os.path.join(self._path, self.MINDSPACE_DIR, self.JOURNAL_DIR)
        try:
            os.makedirs(path, exist_ok=True)

        except OSError as e:
            raise MindspaceError(f"Failed to create journal directory: {str(e)}") from e

        return path

    def file_journal(self) -> FileJournal:
        """
        Return the change journal for the open mindspace.

        Returns:
            The file journal.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._file_journal

    def change_watcher(self) -> MindspaceChangeWatcher:
        """
        Return the change watcher for the open mindspace.

        The watcher reports file changes that no journal entry accounts for.  It
        does not make them revertable: a checkpoint holds metadata and no content.

        Returns:
            The change watcher.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._change_watcher

    def checkpoint_changes(self) -> ChangeCheckpoint:
        """
        Walk the mindspace and record what changed since the previous checkpoint.

        Recording never raises into the caller's work beyond a write failure: a
        scan that cannot complete reports nothing rather than failing the action
        that triggered it, on the same terms as the journal's own recording.

        Returns:
            The checkpoint that was recorded.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._change_watcher.checkpoint()

    def scan_tree(self) -> TreeSnapshot:
        """
        Walk the mindspace tree without recording anything.

        This is the half of a checkpoint that must not run on the thread that owns
        the interface, so a caller with one takes the walk there and hands it back.

        Returns:
            The walk.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._change_watcher.scan()

    def checkpoint_change_snapshot(self, snapshot: TreeSnapshot) -> ChangeCheckpoint:
        """
        Record the changes a walk found.

        Args:
            snapshot: A walk taken by ``scan_tree``.

        Returns:
            The checkpoint that was recorded.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._change_watcher.checkpoint(snapshot)

    def journal_target(self, absolute_path: str) -> tuple[FileJournal, str] | None:
        """
        Return the journal and the relative path for a path the journal can record.

        A path outside the mindspace, or inside a .humbug/ directory, is outside the
        journal's scope and yields None.  Every front end that records a mutation
        asks this rather than repeating the two checks, so the scope cannot drift
        between the tool layer and the panels.

        Args:
            absolute_path: Path about to be mutated.

        Returns:
            The journal and the path relative to the mindspace root, or None when the
            path is outside the journal's scope.
        """
        if not self.has_mindspace():
            return None

        relative_path = self.get_mindspace_relative_path(absolute_path)
        if relative_path is None or self.is_humbug_path(absolute_path):
            return None

        return self._file_journal, relative_path

    def purge_journal(self, before: datetime) -> JournalPurgeResult:
        """
        Discard recorded file mutations older than a point in time.

        This is destructive and irreversible: the caller must have told the user
        so before asking.  Content the trash still holds is untouched, because
        the journal does not own it.

        Args:
            before: The earliest recorded mutation to keep.

        Returns:
            What was removed.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        result = self._file_journal.purge_before(before)
        self._change_watcher.redeclare_retained()
        return result

    def purge_journal_through(self, sequence: int) -> JournalPurgeResult:
        """
        Discard recorded file mutations up to and including a sequence number.

        Destructive and irreversible, like ``purge_journal``, but addressed to a
        record rather than a clock: the caller has chosen the last change worth
        keeping.

        Args:
            sequence: The last sequence number to discard.

        Returns:
            What was removed.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        result = self._file_journal.purge_through(sequence)
        self._change_watcher.redeclare_retained()
        return result

    def purge_whole_journal(self) -> JournalPurgeResult:
        """
        Discard every recorded file mutation.

        Returns:
            What was removed.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        result = self._file_journal.purge_all()
        self._change_watcher.redeclare_retained()
        return result

    def revert_journal_entries(
        self,
        entries: list[JournalEntry],
        actor: JournalActor = JournalActor.HUMAN,
        conversation: str = "",
    ) -> JournalRevertResult:
        """
        Reverse recorded file mutations, restoring each path to its earlier state.

        Args:
            entries: The journal entries to reverse.
            actor: Who is asking for the reversal, recorded against the changes
                the reversal itself makes.
            conversation: Identifier of the conversation that asked for the
                reversal, empty when no conversation is involved.

        Returns:
            The outcome for each affected path.

        Raises:
            MindspaceError: No mindspace is open.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return JournalReverser(
            self._file_journal,
            self._path,
            self.trash_dir(),
            self.restore_trashed,
            actor,
            conversation,
        ).revert(entries)

    def journal_pre_image(self, entry: JournalEntry) -> bytes | None:
        """
        Return the content a path held before a recorded mutation.

        Content the trash kept is read from the trash entry, and content the
        journal kept is read from its own store, so a caller does not have to
        know which kind of removal it is looking at.

        Args:
            entry: The entry whose earlier content is wanted.

        Returns:
            The earlier content, or None when it was not captured or no longer
            exists.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        if entry.pre_image_trash is not None:
            trashed_path = os.path.join(self.trash_dir(), entry.pre_image_trash)
            try:
                with open(trashed_path, 'rb') as f:
                    return f.read()

            except OSError:
                return None

        return self._file_journal.pre_image(entry)

    def trash_dir(self) -> str:
        """
        Return the absolute path to the trash directory, creating it if needed.

        The trash directory lives inside .humbug, alongside but outside of
        conversations/, so trashed items never appear in the conversations
        tree.  Used to support undoing a delete.

        Returns:
            Absolute path to the trash directory.

        Raises:
            MindspaceError: No mindspace is open or directory could not be created.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        path = os.path.join(self._path, self.MINDSPACE_DIR, self.TRASH_DIR)
        try:
            os.makedirs(path, exist_ok=True)

        except OSError as e:
            raise MindspaceError(f"Failed to create trash directory: {str(e)}") from e

        return path

    def _trash_manifest_path(self) -> str:
        """Return the absolute path to the trash manifest file, creating trash_dir() if needed."""
        return os.path.join(self.trash_dir(), "manifest.json")

    def new_trash_path(self, original_path: str) -> str:
        """
        Return a unique location within the trash directory for original_path.

        Args:
            original_path: Absolute path of the item about to be trashed.

        Returns:
            Absolute path within trash_dir() to move it to.
        """
        unique_name = f"{uuid.uuid4().hex[:8]}_{os.path.basename(original_path)}"
        return os.path.join(self.trash_dir(), unique_name)

    def record_trashed(self, original_path: str, trash_path: str) -> None:
        """
        Record where a trashed item originally lived, so it can later be restored.

        Args:
            original_path: Absolute path the item was moved from.
            trash_path: Absolute path within trash_dir() the item now lives at.
        """
        manifest = TrashManifest.load(self._trash_manifest_path())
        manifest.record(
            os.path.basename(trash_path),
            os.path.relpath(original_path, self._path),
            datetime.now(timezone.utc).isoformat(),
            os.path.isdir(trash_path),
        )
        manifest.save(self._trash_manifest_path())

    def trash_file(self, absolute_path: str, actor: JournalActor, conversation: str = "") -> str:
        """
        Move a file or an empty directory into the trash and record the removal.

        The trash is the cheapest possible capture for a removal, and the right
        home for content someone deliberately discarded, so the journal records
        where the content went instead of copying it.  Reversal consults that
        record and delegates restoration to ``restore_trashed``, which means
        emptying the trash makes the removal unrevertable rather than restoring
        something else.

        A directory is recorded as having existed with no content captured, because
        there is no content to capture; the trash entry is what reversal restores.

        Args:
            absolute_path: File or empty directory to move into the trash.
            actor: Who is removing the file.
            conversation: Identifier of the conversation responsible, empty when
                no conversation is involved.

        Returns:
            The absolute path within trash_dir() the file now lives at.

        Raises:
            MindspaceError: The path is outside the mindspace, inside .humbug/, or
                could not be moved.
        """
        relative_path = self.get_mindspace_relative_path(absolute_path)
        if relative_path is None or self.is_humbug_path(absolute_path):
            raise MindspaceError(f"Cannot trash '{absolute_path}': it is outside the journalled mindspace")

        is_directory = os.path.isdir(absolute_path)
        trash_path = self.new_trash_path(absolute_path)
        self._file_journal.record_removal(
            absolute_path,
            relative_path,
            actor,
            conversation,
            trash_name=os.path.basename(trash_path),
            pre_image_is_directory=is_directory,
        )

        try:
            os.makedirs(self.trash_dir(), exist_ok=True)
            shutil.move(absolute_path, trash_path)

        except OSError as e:
            raise MindspaceError(f"Failed to trash '{absolute_path}': {str(e)}") from e

        self.record_trashed(absolute_path, trash_path)
        return trash_path

    def forget_trashed(self, trash_path: str) -> None:
        """Remove trash_path's manifest entry, e.g. once it has been restored."""
        manifest = TrashManifest.load(self._trash_manifest_path())
        manifest.forget(os.path.basename(trash_path))
        manifest.save(self._trash_manifest_path())

    def purge_trashed(self, trash_path: str) -> None:
        """Permanently delete a trashed item and forget its manifest entry."""
        if os.path.isdir(trash_path):
            shutil.rmtree(trash_path, ignore_errors=True)

        elif os.path.exists(trash_path):
            os.remove(trash_path)

        self.forget_trashed(trash_path)

    def restore_trashed(self, trash_name: str) -> str:
        """
        Restore a trashed item to its original location.

        Items trashed before this feature existed (or whose manifest entry
        was otherwise lost) have no known original location, and are
        restored into the conversations directory instead.

        Args:
            trash_name: Basename of the item within trash_dir().

        Returns:
            The absolute path the item was restored to.

        Raises:
            MindspaceError: The trashed item no longer exists, or something
                already occupies the restore destination.
        """
        trash_path = os.path.join(self.trash_dir(), trash_name)
        if not os.path.exists(trash_path):
            raise MindspaceError(f"'{trash_name}' is no longer in the trash")

        manifest = TrashManifest.load(self._trash_manifest_path())
        entry = manifest.get(trash_name)
        target = (
            os.path.join(self._path, entry.original_path) if entry is not None
            else os.path.join(self.conversations_dir(), strip_trash_prefix(trash_name))
        )

        if os.path.exists(target):
            raise MindspaceError(f"Cannot restore '{trash_name}': '{target}' already exists")

        try:
            os.makedirs(os.path.dirname(target), exist_ok=True)
            shutil.move(trash_path, target)

        except OSError as e:
            raise MindspaceError(f"Failed to restore '{trash_name}': {str(e)}") from e

        manifest.forget(trash_name)
        manifest.save(self._trash_manifest_path())
        return target

    def empty_trash(self) -> None:
        """Permanently delete everything currently in the trash."""
        trash_dir = self.trash_dir()
        manifest_name = os.path.basename(self._trash_manifest_path())
        for scan_entry in os.scandir(trash_dir):
            if scan_entry.name == manifest_name:
                continue

            if scan_entry.is_dir():
                shutil.rmtree(scan_entry.path, ignore_errors=True)

            else:
                os.remove(scan_entry.path)

        TrashManifest().save(self._trash_manifest_path())

    def list_trashed(self) -> list[TrashEntry]:
        """
        Return metadata for everything currently in the trash, newest first.

        Items with no manifest entry (e.g. left over from before this
        feature existed) are still listed, with their original location
        left blank and their deletion time approximated from the
        filesystem.

        Returns:
            TrashEntry objects sorted by deleted_at, newest first.
        """
        trash_dir = self.trash_dir()
        manifest = TrashManifest.load(self._trash_manifest_path())
        manifest_name = os.path.basename(self._trash_manifest_path())

        entries = []
        for scan_entry in os.scandir(trash_dir):
            if scan_entry.name == manifest_name:
                continue

            known = manifest.get(scan_entry.name)
            if known is not None:
                entries.append(known)
                continue

            entries.append(TrashEntry(
                trash_name=scan_entry.name,
                original_path="",
                deleted_at=datetime.fromtimestamp(
                    scan_entry.stat().st_mtime, tz=timezone.utc
                ).isoformat(),
                is_dir=scan_entry.is_dir(),
            ))

        entries.sort(key=lambda e: e.deleted_at, reverse=True)
        return entries

    def can_pin_path(self, abs_path: str) -> bool:
        """
        Return True if abs_path is allowed to be pinned.

        Folders and root conversations can be pinned at any depth — pinning
        a folder shows it in the sidebar's Pinned section as well as its
        normal location.  Delegate/fork children are excluded: pulling a
        single fork branch out of its parent conversation while leaving the
        rest behind would be a structural oddity.

        Args:
            abs_path: Absolute filesystem path to check.

        Returns:
            True if the path may be pinned.
        """
        if os.path.isdir(abs_path):
            return True

        return not self._has_delegate_parent(abs_path)

    def _has_delegate_parent(self, abs_path: str) -> bool:
        """
        Return True if the conversation file at abs_path is a fork/delegate
        child (its metadata.parent field is set).

        Args:
            abs_path: Absolute path to a .conv file.

        Returns:
            True if the file has a delegate parent.
        """
        try:
            with open(abs_path, 'r', encoding='utf-8') as f:
                data = json.load(f)

        except (OSError, json.JSONDecodeError):
            return False

        if not isinstance(data, dict):
            return False

        metadata = data.get('metadata')
        return isinstance(metadata, dict) and metadata.get('parent') is not None

    def is_path_pinned(self, abs_path: str) -> bool:
        """
        Return True if the given absolute path is directly pinned.

        This is an exact match against pinned_paths — it does not consider
        whether an ancestor folder is pinned.  Used internally by tree
        building and root de-duplication, which need exact semantics.  For
        UI decisions (e.g. a context-menu label), use is_effectively_pinned().

        Args:
            abs_path: Absolute filesystem path to check.

        Returns:
            True if the path is pinned, False otherwise.
        """
        if not self.has_mindspace() or self._settings is None:
            return False

        rel_path = self.get_mindspace_relative_path(abs_path)
        return rel_path is not None and rel_path in self._settings.pinned_paths

    def is_effectively_pinned(self, abs_path: str) -> bool:
        """
        Return True if abs_path is pinned, either directly or via an ancestor folder.

        A conversation inside a pinned folder is effectively pinned even
        though its own path was never added to pinned_paths — the folder's
        pin covers it.  Intended for UI decisions, such as whether a
        context menu should offer "Pin" or "Unpin".

        Args:
            abs_path: Absolute filesystem path to check.

        Returns:
            True if the path is pinned directly or through an ancestor.
        """
        if self.is_path_pinned(abs_path):
            return True

        if not self.has_mindspace() or self._settings is None:
            return False

        return self._find_pinned_ancestor_dir(abs_path, self._settings.pinned_paths) is not None

    def folder_has_pinned_content(self, folder_abs_path: str) -> bool:
        """
        Return True if folder_abs_path is pinned, or has any pinned conversation inside it.

        Covers both the fully-pinned case (the folder's own path is in
        pinned_paths) and the partially-pinned case (some conversation
        nested inside it is individually pinned).  Used to decide whether
        the folder's natural-location copy should show a pin indicator.

        Args:
            folder_abs_path: Absolute path to the folder to check.

        Returns:
            True if the folder or anything inside it is pinned.
        """
        if not self.has_mindspace() or self._settings is None:
            return False

        folder_rel = self.get_mindspace_relative_path(folder_abs_path)
        if folder_rel is None:
            return False

        prefix = folder_rel + os.sep
        return any(p == folder_rel or p.startswith(prefix) for p in self._settings.pinned_paths)

    def set_path_pinned(self, abs_path: str, pinned: bool) -> None:
        """
        Pin or unpin an absolute path, persisting the change.

        Pinning a conversation that completes the set of pinned conversations
        in its folder — either because it's the only conversation there, or
        because every other conversation in that folder is already pinned —
        promotes the pin to the folder itself, and any now-redundant
        individual pins for its conversations are dropped.

        Unpinning is symmetric in both directions: unpinning a folder also
        unpins any conversations inside it that are still individually
        pinned, so a folder never leaves stale or contradictory pin state
        behind.  Unpinning a conversation that is only *effectively* pinned
        (its own path isn't in pinned_paths, but an ancestor folder's is) is
        the inverse of promotion: the ancestor's pin is replaced with
        individual pins for every other conversation it covered, so the rest
        of the folder stays pinned.  If there's nothing left to cover, the
        ancestor simply ends up unpinned — so unpinning every conversation
        in a folder one at a time ends with the folder itself unpinned.

        Args:
            abs_path: Absolute filesystem path to pin or unpin.
            pinned: True to pin, False to unpin.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        assert self._settings is not None

        target = abs_path
        if pinned and not os.path.isdir(abs_path):
            promoted = self._folder_pin_promotion(abs_path)
            if promoted is not None:
                target = promoted

        if pinned and not self.can_pin_path(target):
            return

        rel_path = self.get_mindspace_relative_path(target)
        if rel_path is None:
            return

        pinned_paths = self._settings.pinned_paths
        changed = False

        if pinned and rel_path not in pinned_paths:
            pinned_paths.append(rel_path)
            changed = True

        elif not pinned and rel_path in pinned_paths:
            pinned_paths.remove(rel_path)
            changed = True

        elif not pinned and not os.path.isdir(target) and self._depromote_ancestor(target, pinned_paths):
            changed = True

        if os.path.isdir(target) and self._prune_pinned_descendants(target, pinned_paths):
            changed = True

        if not changed:
            return

        self.update_settings(self._settings)

    def _find_pinned_ancestor_dir(self, abs_path: str, pinned_paths: list[str]) -> str | None:
        """
        Find the nearest ancestor folder of abs_path that is directly pinned.

        Args:
            abs_path: Absolute path to start searching upward from.
            pinned_paths: The pinned_paths list to search against.

        Returns:
            The pinned ancestor folder's absolute path, or None if there
            isn't one (including if abs_path itself is outside the mindspace).
        """
        pinned_set = set(pinned_paths)
        conversations_root = os.path.normpath(self.conversations_dir())
        parent_dir = os.path.normpath(os.path.dirname(abs_path))

        while True:
            parent_rel = self.get_mindspace_relative_path(parent_dir)
            if parent_rel is not None and parent_rel in pinned_set:
                return parent_dir

            if parent_dir == conversations_root:
                return None

            next_parent = os.path.normpath(os.path.dirname(parent_dir))
            if next_parent == parent_dir:
                return None

            parent_dir = next_parent

    def _collect_conv_files(self, dir_path: str) -> list[str]:
        """
        Recursively collect every .conv file under dir_path.

        Args:
            dir_path: Absolute path to the directory to walk.

        Returns:
            Absolute paths of all .conv files found, in no particular order.
        """
        found = []
        for root, _dirs, files in os.walk(dir_path):
            for filename in files:
                if filename.lower().endswith('.conv'):
                    found.append(os.path.normpath(os.path.join(root, filename)))

        return found

    def _depromote_ancestor(self, abs_path: str, pinned_paths: list[str]) -> bool:
        """
        Undo a pinned ancestor folder's coverage of abs_path, in place.

        Removes the pinned ancestor's own entry and replaces it with
        individual entries for every other conversation it covered (skipping
        delegate children, which can never be individually pinned).  If
        abs_path was the only conversation the ancestor covered, nothing is
        added back and the ancestor ends up simply unpinned.

        Args:
            abs_path: Absolute path to the conversation being unpinned.
            pinned_paths: The pinned_paths list to mutate in place.

        Returns:
            True if a pinned ancestor was found and de-promoted.
        """
        ancestor_dir = self._find_pinned_ancestor_dir(abs_path, pinned_paths)
        if ancestor_dir is None:
            return False

        ancestor_rel = self.get_mindspace_relative_path(ancestor_dir)
        if ancestor_rel is None:
            return False

        pinned_paths.remove(ancestor_rel)

        abs_path_norm = os.path.normpath(abs_path)
        for conv_path in self._collect_conv_files(ancestor_dir):
            if conv_path == abs_path_norm:
                continue

            if self._has_delegate_parent(conv_path):
                continue

            conv_rel = self.get_mindspace_relative_path(conv_path)
            if conv_rel is not None and conv_rel not in pinned_paths:
                pinned_paths.append(conv_rel)

        return True

    def _folder_pin_promotion(self, abs_path: str) -> str | None:
        """
        Return abs_path's parent folder if pinning abs_path completes that folder.

        "Completes" means every pinnable conversation in the folder —
        including abs_path itself — is now individually pinned, whether
        because it's the only one or because pinning this one was the last
        of several.  Delegate/fork children are ignored for this check since
        they can never be individually pinned in the first place.

        Args:
            abs_path: Absolute path to a conversation file about to be pinned.

        Returns:
            The parent folder's absolute path, or None if the parent is the
            conversations root, or some other conversation there isn't pinned.
        """
        assert self._settings is not None

        parent_dir = os.path.dirname(abs_path)
        if os.path.normpath(parent_dir) == os.path.normpath(self.conversations_dir()):
            return None

        try:
            entries = os.listdir(parent_dir)

        except OSError:
            return None

        pinned_set = set(self._settings.pinned_paths)
        abs_path_norm = os.path.normpath(abs_path)
        for entry in entries:
            if not entry.lower().endswith('.conv'):
                continue

            sibling = os.path.normpath(os.path.join(parent_dir, entry))
            if sibling == abs_path_norm:
                continue

            if self._has_delegate_parent(sibling):
                continue

            sibling_rel = self.get_mindspace_relative_path(sibling)
            if sibling_rel is None or sibling_rel not in pinned_set:
                return None

        return parent_dir

    def _prune_pinned_descendants(self, folder_abs_path: str, pinned_paths: list[str]) -> bool:
        """
        Remove pinned entries nested under folder_abs_path, in place.

        Leaves the folder's own entry (if present) untouched — only entries
        strictly inside it are considered redundant or stale.

        Args:
            folder_abs_path: Absolute path to the folder whose pinned or
                unpinned state is being applied.
            pinned_paths: The pinned_paths list to prune, mutated in place.

        Returns:
            True if any entries were removed.
        """
        folder_rel = self.get_mindspace_relative_path(folder_abs_path)
        if folder_rel is None:
            return False

        prefix = folder_rel + os.sep
        remaining = [p for p in pinned_paths if p == folder_rel or not p.startswith(prefix)]
        if len(remaining) == len(pinned_paths):
            return False

        pinned_paths[:] = remaining
        return True

    def migrate_pinned_path(self, old_abs_path: str, new_abs_path: str) -> None:
        """
        Update pinned entries after a path is renamed or moved.

        Rewrites any pinned entry equal to, or nested under, old_abs_path so
        it points at new_abs_path instead.  No-op if nothing pinned is affected.

        Args:
            old_abs_path: Absolute path before the rename/move.
            new_abs_path: Absolute path after the rename/move.
        """
        if not self.has_mindspace() or self._settings is None:
            return

        old_rel = self.get_mindspace_relative_path(old_abs_path)
        new_rel = self.get_mindspace_relative_path(new_abs_path)
        if old_rel is None or new_rel is None:
            return

        old_prefix = old_rel + os.sep
        changed = False
        updated_paths = []
        for pinned_path in self._settings.pinned_paths:
            if pinned_path == old_rel:
                updated_paths.append(new_rel)
                changed = True

            elif pinned_path.startswith(old_prefix):
                updated_paths.append(new_rel + os.sep + pinned_path[len(old_prefix):])
                changed = True

            else:
                updated_paths.append(pinned_path)

        if not changed:
            return

        self._settings.pinned_paths = updated_paths
        self.update_settings(self._settings)

    def unpin_path_tree(self, abs_path: str) -> None:
        """
        Remove pinned entries for a path and anything nested under it.

        Intended to be called after a file or folder is deleted, to avoid
        leaving stale entries in settings.  No-op if nothing pinned is affected.

        Args:
            abs_path: Absolute path that was deleted.
        """
        if not self.has_mindspace() or self._settings is None:
            return

        rel_path = self.get_mindspace_relative_path(abs_path)
        if rel_path is None:
            return

        prefix = rel_path + os.sep
        remaining = [
            p for p in self._settings.pinned_paths
            if p != rel_path and not p.startswith(prefix)
        ]
        if len(remaining) == len(self._settings.pinned_paths):
            return

        self._settings.pinned_paths = remaining
        self.update_settings(self._settings)

    def pinned_root_paths(self) -> list[str]:
        """
        Return absolute paths for pinned entries that should be lifted into
        the sidebar's Pinned section.

        A pinned entry nested under another pinned entry is not a root of
        its own — it comes along for free as part of its ancestor's subtree.
        Entries that no longer exist on disk are silently skipped.

        Returns:
            Absolute paths of pinned roots, shallowest first.
        """
        if not self.has_mindspace() or self._settings is None:
            return []

        candidates = sorted(self._settings.pinned_paths, key=lambda p: p.count(os.sep))
        roots: list[str] = []
        for rel_path in candidates:
            abs_path = self.get_absolute_path(rel_path)
            if not os.path.exists(abs_path):
                continue

            if any(
                abs_path == root or abs_path.startswith(root + os.sep)
                for root in roots
            ):
                continue

            roots.append(abs_path)

        return roots

    def get_absolute_path(self, path: str) -> str:
        """
        Convert a mindspace-relative path to an absolute path.

        Args:
            path: Absolute path or path relative to the mindspace root.

        Returns:
            Absolute path.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        path = os.path.expanduser(path)
        if os.path.isabs(path):
            return os.path.abspath(path)

        return os.path.abspath(os.path.join(self._path, path))

    def get_relative_path(self, path: str) -> str:
        """
        Convert an absolute path to a mindspace-relative path if possible.

        Args:
            path: Absolute path to convert.

        Returns:
            Path relative to mindspace root, or the absolute path if outside.
        """
        abs_path = os.path.abspath(os.path.expanduser(path))
        if not self.has_mindspace():
            return abs_path

        try:
            mindspace_abs = os.path.abspath(self._path)
            if os.path.commonpath([abs_path, mindspace_abs]) != mindspace_abs:
                return abs_path

            return os.path.relpath(abs_path, self._path)

        except ValueError:
            self._logger.warning(
                "Failed to make path '%s' relative to mindspace '%s'",
                path, self._path
            )
            raise

    def get_mindspace_relative_path(self, path: str) -> str | None:
        """
        Convert an absolute path to a mindspace-relative path, or None if outside.

        Args:
            path: Absolute path to convert.

        Returns:
            Relative path if within mindspace, None otherwise.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        abs_path = os.path.abspath(os.path.expanduser(path))

        try:
            mindspace_abs = os.path.abspath(self._path)
            if os.path.commonpath([abs_path, mindspace_abs]) != mindspace_abs:
                return None

            return os.path.relpath(abs_path, self._path)

        except ValueError:
            return None

    def is_humbug_path(self, path: str) -> bool:
        """
        Return True if the given path is inside any .humbug/ directory in the mindspace.

        The .humbug/ directory holds Humbug-internal state (conversations, settings,
        the interaction log, usage data, and shell history).  It is excluded from
        all AI tool operations so that the AI cannot read or modify its own audit
        trail.  This is the single predicate that every tool-facing path check must
        use.

        Every .humbug/ below the root is excluded too, not just the root's own: a
        folder inside this mindspace may be a mindspace in its own right, and its
        state is no more this mindspace's AI's to read than the root's is.

        Args:
            path: Absolute or relative path to test.

        Returns:
            True if the path resolves inside a .humbug/ directory, False otherwise.
        """
        if not self.has_mindspace():
            return False

        # Resolve symlinks so a link inside the mindspace cannot point into .humbug/.
        abs_path = os.path.realpath(os.path.abspath(os.path.expanduser(path)))
        mindspace_abs = os.path.realpath(os.path.abspath(self._path))

        try:
            if os.path.commonpath([abs_path, mindspace_abs]) != mindspace_abs:
                return False

        except ValueError:
            return False

        # Only the part below the root is checked, so a mindspace that itself lives
        # under a folder named .humbug does not have every path excluded.
        relative_path = os.path.relpath(abs_path, mindspace_abs)
        return self.MINDSPACE_DIR in relative_path.split(os.sep)

    def resolve_tool_path(self, path: str) -> tuple[str, str]:
        """
        Resolve a path for AI-tool use, enforcing the mindspace boundary and .humbug exclusion.

        This is the single mechanism every AI tool must use to resolve a path
        argument.  It rejects paths outside the mindspace and paths inside the
        .humbug/ directory.  Unlike get_mindspace_relative_path, which is used for
        internal operations that legitimately touch .humbug/ (such as conversation
        pinning), this method applies the tool-facing rule.

        Args:
            path: Absolute or relative path to resolve.

        Returns:
            Tuple of (absolute path, mindspace-relative path).

        Raises:
            MindspaceError: If no mindspace is open, or the path is outside the
                mindspace boundary.
            MindspaceHumbugPathError: If the path is inside the .humbug/ directory.
                This is a subclass of MindspaceError so callers that reject both
                cases need only catch MindspaceError.
        """
        if not self.has_mindspace():
            raise MindspaceError("No mindspace is open")

        # Resolve symlinks on both the target and the mindspace root so that a link
        # inside the mindspace cannot escape the boundary or point into .humbug/,
        # and so the comparison is consistent on platforms where the temp or home
        # directory is itself a symlink (e.g. macOS /var -> /private/var).
        abs_path = os.path.realpath(os.path.abspath(os.path.expanduser(path)))
        mindspace_abs = os.path.realpath(os.path.abspath(self._path))

        try:
            within_mindspace = os.path.commonpath([abs_path, mindspace_abs]) == mindspace_abs

        except ValueError:
            within_mindspace = False

        if not within_mindspace:
            raise MindspaceError(f"Path is outside mindspace boundaries: {path}")

        if self.is_humbug_path(abs_path):
            raise MindspaceHumbugPathError(
                f"Path is inside the .humbug/ directory, which is managed by Humbug internally: {path}"
            )

        return abs_path, os.path.relpath(abs_path, mindspace_abs)

    def ensure_mindspace_dir(self, dir_path: str) -> str:
        """
        Ensure a directory exists within the mindspace, creating it if needed.

        Args:
            dir_path: Directory path relative to mindspace root.

        Returns:
            Absolute path to the directory.

        Raises:
            MindspaceError: Directory could not be created.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        abs_path = self.get_absolute_path(dir_path)
        try:
            os.makedirs(abs_path, exist_ok=True)
            return abs_path

        except OSError as e:
            raise MindspaceError(f"Failed to create directory '{dir_path}': {e}") from e

    def add_interaction(
        self,
        level: MindspaceLogLevel,
        content: str,
        context: dict[str, str] | None = None
    ) -> MindspaceMessage:
        """
        Append a message to the interaction log.

        Args:
            level: Severity level of the message.
            content: Message text.
            context: Optional structured context that attributes the interaction,
                typically the result of conversation_log_context().

        Returns:
            The created MindspaceMessage.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        message = self._interactions.append(level, content, context=context)
        self._on_interactions_updated()
        return message

    def conversation_log_context(self, requester: Any) -> dict[str, str] | None:
        """
        Return log context that attributes an interaction to a conversation.

        The conversation is identified by the identifier its transcript carries,
        which survives renaming, so an entry stays attributable after the file is
        renamed or moved.  Entries written before transcripts carried identifiers
        hold a mindspace-relative path instead, and readers must tolerate both.

        Args:
            requester: The requester of the interaction.  Anything exposing a
                conversation_id() method, such as an AIConversation, identifies a
                conversation; anything else does not, and logging continues unattributed.

        Returns:
            Context mapping the conversation to its identifier, or None when the
            requester does not identify a conversation.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        identifier = self.conversation_identifier(requester)
        if not identifier:
            return None

        if os.path.isabs(identifier):
            identifier = self.get_relative_path(identifier)

        return {MindspaceMessage.CONVERSATION_CONTEXT: identifier}

    def conversation_identifier(self, requester: Any) -> str:
        """
        Return the identifier of the conversation making a request.

        Prefers the identifier the transcript carries, which survives renaming,
        and falls back to the transcript path for conversations whose transcript
        has none yet and for requesters that name no conversation at all.  Both
        forms are opaque to callers, which must not assume either.

        Args:
            requester: The requester of the interaction.

        Returns:
            The conversation identifier, or an empty string when the requester
            does not identify a conversation.
        """
        for name in ("conversation_uuid", "conversation_id"):
            accessor = getattr(requester, name, None)
            if callable(accessor):
                value = accessor()
                if value:
                    return str(value)

        return ""

    def interactions(self) -> MindspaceInteractions:
        """
        Return the interaction log store.

        Returns:
            The store, which answers sequence-addressed reads and reports whether
            appends are succeeding.
        """
        assert self.has_mindspace(), "No mindspace is currently open"
        return self._interactions

    def save_mindspace_state(self, state: dict) -> None:
        """
        Persist session state (open tabs, layout) to disk.

        Paths stored in the state dict are converted to mindspace-relative form
        before writing so the session is portable.

        Args:
            state: Dictionary containing tabs and layout state.

        Raises:
            MindspaceError: State could not be saved.
        """
        if not self.has_mindspace():
            raise MindspaceError("No mindspace is active")

        try:
            self.ensure_mindspace_dir(self.MINDSPACE_DIR)

            for entry in state.get('contexts', []):
                layout = entry.get('layout', {})
                path = layout.get('path', '')
                if os.path.isabs(path):
                    relative = os.path.relpath(path, self._path)
                    if relative.startswith(os.pardir + os.sep) or relative == os.pardir:
                        continue  # Path outside mindspace — keep absolute

                    layout['path'] = relative

            session_file = os.path.join(self._path, self.MINDSPACE_DIR, self.SESSION_FILE)
            with open(session_file, 'w', encoding='utf-8') as f:
                json.dump(state, f, indent=4)

        except OSError as e:
            raise MindspaceError(f"Failed to save mindspace state: {str(e)}") from e

    def load_mindspace_state(self) -> dict:
        """
        Load session state from disk.

        Relative paths in the returned state are resolved to absolute paths.

        Returns:
            Dictionary containing tabs and layout state.

        Raises:
            MindspaceError: State could not be loaded.
        """
        if not self.has_mindspace():
            raise MindspaceError("No mindspace is active")

        session_file = os.path.join(self._path, self.MINDSPACE_DIR, self.SESSION_FILE)
        if not os.path.exists(session_file):
            return {}

        try:
            with open(session_file, encoding='utf-8') as f:
                state = json.load(f)

            for entry in state.get('contexts', []):
                layout = entry.get('layout', {})
                path = layout.get('path', '')
                if path and not os.path.isabs(path):
                    layout['path'] = os.path.normpath(os.path.join(self._path, path))

            return state

        except json.JSONDecodeError as e:
            raise MindspaceError(f"Failed to parse mindspace state: {str(e)}") from e

        except OSError as e:
            raise MindspaceError(f"Failed to load mindspace state: {str(e)}") from e

    def usage(self) -> MindspaceUsage:
        """Return the current mindspace usage stats."""
        return self._usage

    def update_usage(
        self,
        provider: str,
        model: str,
        input_tokens: int,
        output_tokens: int,
        cache_write_tokens: int = 0,
        cache_read_tokens: int = 0,
    ) -> None:
        """
        Record usage from a completed AI response and persist to disk.

        Args:
            provider: Provider identifier (e.g. 'anthropic').
            model: Model name (e.g. 'claude-sonnet-4-6').
            input_tokens: Number of prompt tokens consumed.
            output_tokens: Number of completion tokens generated.
            cache_write_tokens: Tokens written to provider cache.
            cache_read_tokens: Tokens read from provider cache.
        """
        self._usage.record(
            provider, model, input_tokens, output_tokens,
            cache_write_tokens=cache_write_tokens,
            cache_read_tokens=cache_read_tokens,
        )
        self._save_usage()
        if self._on_usage_updated:
            self._on_usage_updated()

    def reset_usage(self) -> None:
        """Clear all accumulated usage stats and persist the empty state."""
        self._usage.reset()
        self._save_usage()
        if self._on_usage_updated:
            self._on_usage_updated()

    def _migrate_conversations(self) -> None:
        """
        Migrate conversations from the legacy mindspace-root location to .humbug/.

        If a 'conversations' directory exists at the mindspace root (the legacy
        location) and no conversations directory exists inside .humbug, move it.
        This is a one-time migration that runs when a mindspace is opened.
        """
        old_path = os.path.join(self._path, self.CONVERSATIONS_DIR)
        new_path = os.path.join(self._path, self.MINDSPACE_DIR, self.CONVERSATIONS_DIR)

        if not os.path.isdir(old_path):
            return

        if os.path.isdir(new_path):
            # Both exist — log and leave the old one in place rather than risk data loss.
            self._logger.warning(
                "Conversations directory exists at both '%s' and '%s'; "
                "leaving legacy directory in place",
                old_path, new_path
            )
            return

        try:
            shutil.move(old_path, new_path)
            self._migrate_session_paths(
                self.CONVERSATIONS_DIR,
                os.path.join(self.MINDSPACE_DIR, self.CONVERSATIONS_DIR))
            self._logger.info("Migrated conversations from '%s' to '%s'", old_path, new_path)

        except OSError as e:
            self._logger.error("Failed to migrate conversations from '%s' to '%s': %s",
                               old_path, new_path, str(e))

    def _migrate_session_paths(self, old_dir: str, new_dir: str) -> None:
        """
        Rewrite conversation paths in the session file after migration.

        Session paths are stored relative to the mindspace root.  This updates
        any path that starts with the old conversations prefix to use the new
        .humbug/conversations prefix.

        Args:
            old_dir: The old conversations directory name relative to mindspace root.
            new_dir: The new conversations directory path relative to mindspace root.
        """
        session_file = os.path.join(self._path, self.MINDSPACE_DIR, self.SESSION_FILE)
        if not os.path.exists(session_file):
            return

        try:
            with open(session_file, encoding='utf-8') as f:
                state = json.load(f)

            old_prefix = old_dir + os.sep
            new_prefix = new_dir + os.sep
            changed = False
            for tab_state in state.get('tabs', []):
                path = tab_state.get('path')
                if path and path.startswith(old_prefix):
                    tab_state['path'] = new_prefix + path[len(old_prefix):]
                    changed = True

            if changed:
                with open(session_file, 'w', encoding='utf-8') as f:
                    json.dump(state, f, indent=4)

        except (json.JSONDecodeError, OSError) as e:
            self._logger.warning("Failed to migrate session paths: %s", str(e))

    def _apply_tool_settings(self, settings: MindspaceSettings) -> None:
        """Apply tool enabled states from settings to the tool manager."""
        try:
            self._tool_manager.set_tool_enabled_states(settings.enabled_tools)

        except Exception as e:
            self._logger.error("Failed to apply tool settings: %s", e)

    def _reset_tool_manager(self) -> None:
        """Reset tool manager to defaults when no mindspace is open."""
        try:
            self._tool_manager.set_tool_enabled_states(
                self._tool_manager.get_default_enabled_tools()
            )

        except Exception as e:
            self._logger.error("Failed to reset tool manager: %s", e)

    def _save_usage(self) -> None:
        """Persist usage stats to disk."""
        try:
            mindspace_dir = os.path.join(self._path, self.MINDSPACE_DIR)
            os.makedirs(mindspace_dir, exist_ok=True)
            usage_path = os.path.join(mindspace_dir, self.USAGE_FILE)
            with open(usage_path, 'w', encoding='utf-8') as f:
                json.dump(self._usage.to_dict(), f, indent=4)

        except OSError as e:
            self._logger.error("Failed to save usage stats: %s", e)

    def _load_usage(self) -> None:
        """Load usage stats from disk, silently ignoring missing files."""
        try:
            usage_path = os.path.join(self._path, self.MINDSPACE_DIR, self.USAGE_FILE)
            if not os.path.exists(usage_path):
                self._usage = MindspaceUsage()
                return

            with open(usage_path, encoding='utf-8') as f:
                data = json.load(f)

            self._usage = MindspaceUsage.from_dict(data)

        except Exception as e:
            self._logger.info("Failed to load usage stats: %s", e)
            self._usage = MindspaceUsage()

    def _open_interactions(self) -> None:
        """Attach the interaction log store to this mindspace's log directory."""
        try:
            self._interactions.open(
                os.path.join(self._path, self.MINDSPACE_DIR, self.INTERACTIONS_DIR)
            )

        except Exception as e:
            self._logger.info("Failed to open interactions: %s", e)
