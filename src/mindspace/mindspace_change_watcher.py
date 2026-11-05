"""Checkpoints that detect file changes no journal entry explains."""

from dataclasses import dataclass, field
from datetime import datetime, timezone
import json
import logging
import os

from mindspace.file_journal import FileJournal
from mindspace.journal_entry import JournalEntry
from mindspace.mindspace_tree_scanner import (
    MindspaceTreeScanner, TreeDifference, TreeEntry, TreeSnapshot
)


@dataclass
class ChangeCheckpoint:
    """
    One checkpoint: what changed since the checkpoint before it.

    A checkpoint records what changed and when, and never why.  The paths it
    reports are those that differ from the previous checkpoint and that no
    journal entry accounts for, which is what makes them visible rather than
    silent.

    Attributes:
        sequence: The checkpoint's position in the series, starting at 1.
        timestamp: ISO-8601 UTC time the checkpoint was taken.
        difference: Every path that changed since the previous checkpoint.
        attributed: The subset of those paths a journal entry accounts for.
        unwatched: True when the previous checkpoint was written by an earlier
            session, so the change happened while Humbug was not watching and
            the window it fell in is unknown.
    """

    sequence: int
    timestamp: str
    difference: TreeDifference
    attributed: set[str] = field(default_factory=set)
    unwatched: bool = False

    def unrecorded(self) -> set[str]:
        """Return the changed paths no journal entry accounts for."""
        return self.difference.paths() - self.attributed

    def to_dict(self) -> dict:
        """Return the checkpoint as a JSON-compatible dict."""
        return {
            "sequence": self.sequence,
            "timestamp": self.timestamp,
            "difference": self.difference.to_dict(),
            "attributed": sorted(self.attributed),
            "unwatched": self.unwatched,
        }

    @staticmethod
    def from_dict(data: dict) -> "ChangeCheckpoint":
        """
        Deserialise a checkpoint from a dict produced by ``to_dict``.

        Args:
            data: The dict to read.

        Returns:
            The checkpoint.

        Raises:
            ValueError: A field is missing or of the wrong type.
        """
        try:
            sequence = data["sequence"]
            timestamp = data["timestamp"]
            difference = data["difference"]
            attributed = data.get("attributed", [])
            unwatched = data.get("unwatched", False)

        except KeyError as e:
            raise ValueError(f"checkpoint is missing field {str(e)}") from e

        if not isinstance(sequence, int) or not isinstance(timestamp, str):
            raise ValueError("a checkpoint needs an integer sequence and a string timestamp")

        if not isinstance(difference, dict):
            raise ValueError("a checkpoint needs a difference object")

        if not isinstance(attributed, list) or not all(isinstance(p, str) for p in attributed):
            raise ValueError("attributed must be a list of strings")

        if not isinstance(unwatched, bool):
            raise ValueError("unwatched must be a boolean")

        return ChangeCheckpoint(
            sequence=sequence,
            timestamp=timestamp,
            difference=TreeDifference.from_dict(difference),
            attributed=set(attributed),
            unwatched=unwatched,
        )


class MindspaceChangeWatcher:
    """
    Compares successive metadata walks of a mindspace and reports the differences.

    The baseline is a file rather than memory, so the first checkpoint after a
    mindspace opens can report what changed while Humbug was closed: that case is
    indistinguishable from silence otherwise, and it is the case users create most
    often.

    Writing that file relies on a mindspace being open in only one instance at a
    time, which ``MindspaceManager`` guarantees with an exclusive lock.  Without
    that guarantee two instances would overwrite each other's baselines.

    A path the journal recorded between two checkpoints is attributed to the
    journal.  When something else also changed that path in the same window the
    attribution is wrong in the safe direction: Humbug is credited with a change
    it only partly made.  Distinguishing the two would mean hashing content on
    every checkpoint, which is the cost this design exists to avoid.
    """

    WATCH_DIR = "watch"
    BASELINE_FILE = "baseline.json"
    CHANGES_FILE = "changes.jsonl"
    PREVIOUS_CHANGES_FILE = "changes.1.jsonl"
    MAX_CHANGES_BYTES = 4 * 1024 * 1024
    WALK_BUDGET_FRACTION = 0.1

    def __init__(
        self,
        mindspace_path: str,
        journal: FileJournal,
        pruned_dirs: frozenset[str],
        interval_seconds: float,
    ) -> None:
        """
        Initialise a watcher for a mindspace.

        Args:
            mindspace_path: Absolute path to the mindspace root.
            journal: The journal, consulted to attribute a change it recorded.
            pruned_dirs: Directory names not to descend.
            interval_seconds: The configured interval between checkpoints.
        """
        self._logger = logging.getLogger("MindspaceChangeWatcher")
        self._directory = os.path.join(mindspace_path, ".humbug", self.WATCH_DIR)
        self._journal = journal
        self._scanner = MindspaceTreeScanner(mindspace_path, pruned_dirs)
        self._interval_seconds = interval_seconds
        self._baseline: dict[str, TreeEntry] = {}
        self._baseline_digests: dict[str, str] = {}
        self._baseline_sequence = 0
        self._journal_sequence = 0
        self._sequence = 0
        self._checkpoints_this_session = 0
        self._has_baseline = False

    def open(self) -> None:
        """
        Load the persisted baseline, so the next checkpoint spans the last session.

        A baseline that cannot be read is treated as absent, which makes the next
        checkpoint a first one that reports nothing: a damaged baseline must not
        turn every file in the mindspace into a reported change.
        """
        self._baseline = {}
        self._baseline_digests = {}
        self._baseline_sequence = 0
        self._journal_sequence = self._journal.newest_sequence()
        self._sequence = self._count_recorded_checkpoints()
        self._checkpoints_this_session = 0
        self._has_baseline = False

        path = self._baseline_path()
        try:
            with open(path, encoding='utf-8') as f:
                data = json.load(f)

        except FileNotFoundError:
            return

        except (OSError, json.JSONDecodeError) as e:
            self._logger.warning("Failed to read the change baseline at %s: %s", path, e)
            return

        try:
            entries = data["entries"]
            sequence = data["sequence"]
            if not isinstance(entries, dict) or not isinstance(sequence, int):
                raise ValueError("baseline is malformed")

            self._baseline = {
                path: TreeEntry.from_list(values) for path, values in entries.items()
            }
            digests = data.get("digests", {})
            if not isinstance(digests, dict) or not all(
                isinstance(value, str) for value in digests.values()
            ):
                raise ValueError("baseline digests must be a mapping of strings")

            self._baseline_digests = {
                path: digest for path, digest in digests.items() if isinstance(path, str)
            }
            self._baseline_sequence = sequence
            journal_sequence = data.get("journal_sequence", 0)
            if not isinstance(journal_sequence, int):
                raise ValueError("baseline journal_sequence must be an integer")

            self._journal_sequence = journal_sequence
            self._has_baseline = True

            # Content a purge reclaimed is dropped from what this session claims, so a
            # restart cannot reinstate a digest the store no longer holds.  The baseline
            # file itself is rewritten by the next checkpoint.
            surviving = self._journal.stored_digests()
            self._baseline_digests = {
                path: digest for path, digest in self._baseline_digests.items() if digest in surviving
            }

        except (KeyError, TypeError, ValueError) as e:
            self._logger.warning("Ignoring a malformed change baseline at %s: %s", path, e)
            self._baseline = {}
            self._baseline_digests = {}
            self._baseline_sequence = 0
            self._journal_sequence = 0
            self._has_baseline = False

    def close(self) -> None:
        """Discard everything held in memory."""
        self._baseline = {}
        self._baseline_digests = {}
        self._baseline_sequence = 0
        self._journal_sequence = 0
        self._sequence = 0
        self._checkpoints_this_session = 0
        self._has_baseline = False

    def checkpoint(self, snapshot: TreeSnapshot | None = None) -> ChangeCheckpoint:
        """
        Walk the tree, record every change it finds, and store the new baseline.

        The baseline is written before the checkpoint is recorded, so a crash
        between the two loses the report of a change rather than reporting the
        same change twice.  Reporting a change that has already been reported is
        the more confusing failure of the two.

        A checkpoint with no baseline to compare against establishes one and reports
        nothing.  Reporting every file in the mindspace as new, which is what the
        difference against nothing is, would bury the first real change in noise.

        A walk may be passed in, which is how a caller keeps it off the thread that
        owns the interface.  Everything after the walk runs on the calling thread,
        so the journal is only ever written from one place.

        Args:
            snapshot: A walk already taken, or None to take one here.

        Returns:
            The checkpoint, whose difference is everything that changed since the
            previous one.

        Raises:
            OSError: The baseline or the record could not be written.
        """
        if snapshot is None:
            snapshot = self._scan()

        if self._has_baseline:
            difference = TreeDifference.between(self._baseline, snapshot.entries)

        else:
            difference = TreeDifference()

        unwatched = self._has_baseline and self._checkpoints_this_session == 0

        digests = self._capture(snapshot, difference)
        attributed = self._journal_paths_in(difference, digests)

        checkpoint = ChangeCheckpoint(
            sequence=self._sequence + 1,
            timestamp=datetime.now(timezone.utc).isoformat(),
            difference=difference,
            attributed=attributed,
            unwatched=unwatched,
        )

        self._record_detections(difference, digests, attributed)
        self.note_walk_cost(snapshot)
        self._advance_journal_sequence(digests)
        self._write_baseline(snapshot.entries, checkpoint.sequence, digests)
        self._baseline = snapshot.entries
        self._baseline_digests = digests
        self._baseline_sequence = checkpoint.sequence
        self._sequence = checkpoint.sequence
        self._checkpoints_this_session += 1
        self._has_baseline = True

        if not difference.is_empty():
            self._append_checkpoint(checkpoint)

        return checkpoint

    def interval_for(self, snapshot: TreeSnapshot) -> float:
        """
        Return the interval to use after a walk, given how long that walk took.

        A walk is allowed to occupy at most a tenth of the interval, so a slow
        machine or a tree being watched by antivirus lengthens the interval instead
        of making the scanner a load source.  The configured interval is the floor.

        Args:
            snapshot: The walk that just completed.

        Returns:
            The interval in seconds.
        """
        spent = snapshot.duration_ms / 1000.0
        if spent <= 0:
            return self._interval_seconds

        return max(self._interval_seconds, spent / self.WALK_BUDGET_FRACTION)

    def note_walk_cost(self, snapshot: TreeSnapshot) -> None:
        """
        Adopt a longer interval when the last walk was expensive for its interval.

        This is what makes the cadence self-correcting: a machine where a walk costs
        hundreds of milliseconds gets a slower cadence rather than a permanent load,
        and the guarantee shortens to match instead of the interface stalling.  It
        only ever lengthens, because the configured value is the floor and a cheap
        walk is no reason to change what the user asked for.

        Args:
            snapshot: The walk that just completed.
        """
        adjusted = self.interval_for(snapshot)
        if adjusted > self._interval_seconds:
            self._logger.info(
                "Lengthening the checkpoint interval from %.1f s to %.1f s: the walk took %.1f ms",
                self._interval_seconds, adjusted, snapshot.duration_ms
            )
            self._interval_seconds = adjusted

    def scan(self) -> TreeSnapshot:
        """Walk the tree without recording anything, for a caller with its own thread."""
        return self._scan()

    def redeclare_retained(self) -> None:
        """
        Declare only the retained content that survived, after history was discarded.

        A purge reclaims blobs no live entry refers to, which deliberately includes the
        state a baseline was keeping.  What must not survive is the *claim*: a baseline
        still naming a digest whose blob has been reclaimed would offer a restore that
        fails on the way to the file rather than reporting that it cannot go back that
        far.  Dropping the digest records the loss where it happened, so the entry is
        reported as not revertable and the next capture starts from what exists.

        The baseline is rewritten so that a restart does not reintroduce the claim.

        Raises:
            OSError: The baseline could not be rewritten.
        """
        surviving = self._journal.stored_digests()
        self._baseline_digests = {
            path: digest
            for path, digest in self._baseline_digests.items()
            if digest in surviving
        }

        if self._has_baseline:
            self._write_baseline(self._baseline, self._baseline_sequence, self._baseline_digests)

    def interval_seconds(self) -> float:
        """Return the configured interval, which is the floor a measurement can raise."""
        return self._interval_seconds

    def recorded_changes(self, limit: int) -> list[ChangeCheckpoint]:
        """
        Return the recorded checkpoints that reported a change, oldest first.

        An idle tree records nothing, so this is the history of activity rather
        than a list of every checkpoint taken.

        Args:
            limit: Maximum number of checkpoints to return, taking the newest.

        Returns:
            The checkpoints that reported something, oldest first.
        """
        checkpoints = self._read_checkpoints(self._changes_path())
        checkpoints.extend(self._read_checkpoints(self._previous_changes_path()))

        return checkpoints[-limit:]

    def horizon(self) -> str:
        """
        Return when the recorded history begins, or an empty string when there is none.

        This is the fact the interface reports rather than a configured promise: the
        reach of what the scanner can account for is however far back its records go.
        """
        checkpoints = self._read_checkpoints(self._previous_changes_path())
        if checkpoints:
            return checkpoints[0].timestamp

        checkpoints = self._read_checkpoints(self._changes_path())
        return checkpoints[0].timestamp if checkpoints else ""

    def _scan(self) -> TreeSnapshot:
        """Walk the tree, logging rather than raising a failure."""
        snapshot = self._scanner.scan()
        if snapshot.duration_ms > self._interval_seconds * 1000.0:
            self._logger.debug(
                "Metadata walk took %.1f ms, longer than the %0.1f s interval",
                snapshot.duration_ms, self._interval_seconds
            )

        return snapshot

    def _journal_paths_in(self, difference: TreeDifference, digests: dict[str, str]) -> set[str]:
        """
        Return the changed paths a journal entry accounts for.

        An entry accounts for a change only when the path now holds what the entry says
        the mutation produced.  Matching the path alone would attribute a change to an
        entry describing something else — most often the entry this very walk is about
        to record, whose effect has not happened yet.

        Args:
            difference: The paths that changed.
            digests: The retained content's digest for each path, after this walk.
        """
        if difference.is_empty():
            return set()

        changed = difference.paths()
        return {
            entry.path
            for entry in self._unattributed_entries()
            if entry.path in changed and self._effect_observed(entry, digests)
        }

    def _advance_journal_sequence(self, digests: dict[str, str]) -> None:
        """
        Move the attribution watermark forward, stopping short of entries not yet seen.

        Moving the watermark past an entry that has not been observed forgets it: the
        mutation happens after the walk, the next walk reports it, and there is no
        longer an entry to attribute it to.  So the watermark may only pass entries
        whose effect this walk has seen, and an entry still ahead of the tree stops it
        until a later walk catches up.

        The common case is that the entry is still ahead of us, because a writer records
        before it mutates and a walk usually runs between the two.  That is exactly the
        case a naive watermark loses.

        Args:
            digests: The retained content's digest for each path, after this walk.
        """
        for entry in self._unattributed_entries():
            if entry.is_void():
                continue

            if not self._effect_observed(entry, digests):
                return

            self._journal_sequence = entry.sequence

    def _unattributed_entries(self) -> list[JournalEntry]:
        """Return the entries recorded since the attribution watermark."""
        try:
            return self._journal.entries_after(self._journal_sequence)

        except OSError as e:
            self._logger.warning("Failed to read the journal to attribute changes: %s", e)
            return []

    @staticmethod
    def _effect_observed(entry: JournalEntry, digests: dict[str, str]) -> bool:
        """
        Return True if what the entry produced is what the tree now holds.

        A removal is observed when the path is no longer there, which the caller
        establishes by having no digest for it.  A write is observed when the digest
        matches the post-image the entry recorded.

        Args:
            entry: The entry to test.
            digests: The retained content's digest for each path, after this walk.
        """
        if entry.post_image_hash is None:
            return entry.path not in digests

        return digests.get(entry.path) == entry.post_image_hash

    def _capture(
        self,
        snapshot: TreeSnapshot,
        difference: TreeDifference,
    ) -> dict[str, str]:
        """
        Store the content a later restore will need, without recording anything.

        Content is retained for every file when there is no baseline yet, because
        the first change after that has to have a pre-image to name.  Afterwards it
        is retained only for files whose metadata moved, so an idle tree costs no
        reads at all.

        Retention happens before anything is decided, because the digests it produces
        are how a change is matched to the entry that explains it.

        Args:
            snapshot: The walk just taken.
            difference: What changed since the previous checkpoint.

        Returns:
            The digest of the retained content for each path that has one.
        """
        digests: dict[str, str] = {}
        if not self._has_baseline:
            for path in snapshot.entries:
                digest = self._retain(path)
                if digest is not None:
                    digests[path] = digest

            return digests

        for path in snapshot.entries:
            if path not in difference.paths():
                previous = self._baseline_digests.get(path)
                if previous is not None:
                    digests[path] = previous

        for path in difference.added | difference.changed:
            digest = self._retain(path)
            if digest is not None:
                digests[path] = digest

        return digests

    def _record_detections(
        self,
        difference: TreeDifference,
        digests: dict[str, str],
        attributed: set[str],
    ) -> None:
        """
        Record every change no journal entry accounts for.

        A path Humbug already recorded is not recorded again: one change is one entry,
        and the entry Humbug wrote knows who made it.

        Args:
            difference: What changed since the previous checkpoint.
            digests: The retained content's digest for each path that has one.
            attributed: The changed paths the journal already accounts for.
        """
        for path in difference.added | difference.changed:
            if path in attributed:
                continue

            self._journal.record_detected(
                path,
                self._baseline_digests.get(path),
                digests.get(path),
                existed_before=path in self._baseline,
            )

        for path in difference.removed:
            if path in attributed:
                continue

            self._journal.record_detected(
                path,
                self._baseline_digests.get(path),
                None,
                existed_before=True,
            )

    def _retain(self, relative_path: str) -> str | None:
        """
        Store a file's content and return the digest naming it.

        There is no size limit.  The content is copied in chunks, so retaining a large
        file costs the time to read and write it once and no memory at all; excluding
        it would mean a change to a large file could be seen and never undone, which is
        the distinction the user cannot be expected to hold in mind.

        Args:
            relative_path: The path, relative to the mindspace root.

        Returns:
            The digest of the stored content, or None.
        """
        absolute_path = os.path.join(self._scanner.root(), relative_path)
        return self._journal.retain_file(absolute_path)

    def _write_baseline(self, entries: dict[str, TreeEntry], sequence: int, digests: dict[str, str]) -> None:
        """
        Persist the baseline atomically, so a torn write cannot be read back.

        Args:
            entries: The metadata of every file the baseline covers.
            sequence: The checkpoint number the baseline belongs to.
            digests: The retained content's digest for each path that has one.

        Raises:
            OSError: The baseline could not be written.
        """
        os.makedirs(self._directory, exist_ok=True)
        path = self._baseline_path()
        temporary = path + ".tmp"

        data = {
            "sequence": sequence,
            "journal_sequence": self._journal_sequence,
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "entries": {
                entry_path: entry.to_list() for entry_path, entry in entries.items()
            },
            "digests": digests,
        }

        try:
            with open(temporary, 'w', encoding='utf-8') as f:
                json.dump(data, f)
                f.flush()
                os.fsync(f.fileno())

            os.replace(temporary, path)

        except OSError:
            try:
                os.remove(temporary)

            except OSError:
                pass

            raise

    def _append_checkpoint(self, checkpoint: ChangeCheckpoint) -> None:
        """
        Append a checkpoint to the change log, rotating the log when it is full.

        Args:
            checkpoint: The checkpoint to record.

        Raises:
            OSError: The record could not be written.
        """
        os.makedirs(self._directory, exist_ok=True)
        path = self._changes_path()

        try:
            if os.path.getsize(path) >= self.MAX_CHANGES_BYTES:
                os.replace(path, self._previous_changes_path())

        except OSError:
            pass

        with open(path, 'a', encoding='utf-8') as f:
            f.write(json.dumps(checkpoint.to_dict()) + "\n")

    def _count_recorded_checkpoints(self) -> int:
        """Return how many checkpoints the recorded history holds."""
        return len(self._read_checkpoints(self._changes_path())) + len(
            self._read_checkpoints(self._previous_changes_path())
        )

    @staticmethod
    def _read_checkpoints(path: str) -> list[ChangeCheckpoint]:
        """
        Read a change log, skipping records that cannot be parsed.

        A torn final line is normal after a crash, and is not a reason to lose the
        records before it.

        Args:
            path: The log to read.
        """
        checkpoints: list[ChangeCheckpoint] = []

        try:
            with open(path, encoding='utf-8') as f:
                lines = f.readlines()

        except FileNotFoundError:
            return checkpoints

        except OSError:
            return checkpoints

        for line in lines:
            if not line.strip():
                continue

            try:
                checkpoints.append(ChangeCheckpoint.from_dict(json.loads(line)))

            except (json.JSONDecodeError, ValueError):
                continue

        return checkpoints

    def _baseline_path(self) -> str:
        """Return the path of the persisted baseline."""
        return os.path.join(self._directory, self.BASELINE_FILE)

    def _changes_path(self) -> str:
        """Return the path of the current change log."""
        return os.path.join(self._directory, self.CHANGES_FILE)

    def _previous_changes_path(self) -> str:
        """Return the path of the rotated change log."""
        return os.path.join(self._directory, self.PREVIOUS_CHANGES_FILE)
