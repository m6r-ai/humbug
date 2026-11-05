"""Background checkpointing of the mindspace tree, so terminal effects are recorded."""

import asyncio
import logging

from PySide6.QtCore import QObject, QTimer, Signal

from desktop.mindspace.mindspace_manager import MindspaceManager
from mindspace.mindspace_tree_scanner import TreeSnapshot


class MindspaceChangePoller(QObject):
    """
    Takes a checkpoint on a timer, keeping the walk off the Qt main thread.

    A checkpoint has two halves that must not share a thread.  Walking the tree is
    blocking I/O and runs on the executor, on the same terms as the VCS poller.  The
    half that follows — hashing what moved, storing it, and appending entries —
    writes the journal, and the journal is not safe for concurrent writers, so it
    stays on the main thread where the rest of the interface already writes to it.

    The cadence follows activity.  An idle mindspace still walks on the interval,
    because the whole point is to notice changes nothing in Humbug made, and a walk
    of a quiet tree costs a few milliseconds and no reads.  The interval lengthens
    itself when a walk turns out to be expensive: a machine with antivirus in the
    way gets a slower cadence instead of a permanent load, and the guarantee shortens
    to match rather than the interface stalling.

    One instance, because there is one open mindspace and one journal.
    """

    checkpoint_taken = Signal()

    _instance = None
    _logger = logging.getLogger("MindspaceChangePoller")

    def __new__(cls) -> 'MindspaceChangePoller':
        """Create or return the singleton instance."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialise the poller if not already done."""
        if hasattr(self, '_initialized'):
            return

        super().__init__()
        self._initialized = True

        self._mindspace_manager = MindspaceManager()
        self._running = False
        self._in_flight = False
        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.timeout.connect(self._on_trigger)

    def start(self) -> None:
        """Begin checkpointing the open mindspace, taking one immediately."""
        self._running = True
        self._schedule()
        self._on_trigger()

    def stop(self) -> None:
        """Stop checkpointing, leaving any walk already dispatched to finish."""
        self._running = False
        self._timer.stop()

    def is_running(self) -> bool:
        """Return True if checkpoints are being taken."""
        return self._running

    def trigger(self) -> None:
        """
        Take a checkpoint now, unless one is already running.

        This is called from paths that are hit far more often than a checkpoint is
        worth: every write to a terminal, and every burst of its output.  The only
        thing that must not happen is two walks running at once, so that is exactly
        what is prevented — not a rate, which would be a proxy for it and would drop
        the checkpoint that a second command needs a moment after the first.

        Coalescing is a side effect rather than the aim: a burst of activity while a
        walk is in flight produces one checkpoint when it finishes, because the walk
        that is running will observe the state the later activity produced.
        """
        if not self._running:
            return

        if self._in_flight:
            return

        self._on_trigger()

    def _schedule(self) -> None:
        """Arm the timer for the next checkpoint, at the configured interval."""
        if not self._running:
            return

        mindspace = self._mindspace_manager.mindspace()
        interval = 5.0
        if mindspace.has_mindspace():
            watcher = mindspace.change_watcher()
            interval = watcher.interval_seconds()

        self._timer.start(max(1, int(interval * 1000)))

    def _on_trigger(self) -> None:
        """Dispatch a checkpoint to the event loop if one is not already running."""
        mindspace = self._mindspace_manager.mindspace()
        if not self._running or not mindspace.has_mindspace() or self._in_flight:
            return

        self._in_flight = True
        loop = asyncio.get_event_loop()
        loop.create_task(self._run_checkpoint())

    async def _run_checkpoint(self) -> None:
        """Walk on the executor, then record on the main thread."""
        mindspace = self._mindspace_manager.mindspace()
        if not mindspace.has_mindspace():
            self._in_flight = False
            return

        try:
            loop = asyncio.get_event_loop()
            snapshot: TreeSnapshot = await loop.run_in_executor(None, mindspace.scan_tree)

        except Exception as e:
            self._logger.error("Failed to walk the mindspace for a checkpoint: %s", e)
            self._in_flight = False
            self._schedule()
            return

        if not self._running:
            self._in_flight = False
            return

        try:
            mindspace.checkpoint_change_snapshot(snapshot)

        except Exception as e:
            self._logger.error("Failed to record a checkpoint: %s", e)

        else:
            self.checkpoint_taken.emit()

        finally:
            self._in_flight = False
            self._schedule()
