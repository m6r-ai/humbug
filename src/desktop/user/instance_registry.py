"""
Registry of running Humbug instances.

Each instance writes a small JSON file describing itself, so that any instance can
answer "is another Humbug already working on this mindspace?".  This is what prevents
two instances sharing one mindspace, which would mean two writers to the same
``.humbug/`` state — including the audit log, whose value depends on being a single
witness to what occurred.

One file per instance rather than a single shared file: each instance only ever writes
its own file, so there is no write race between instances.
"""

from dataclasses import dataclass
import ctypes
import logging
import os
import sys
import time

from desktop import json_store


@dataclass
class InstanceRecord:
    """A running Humbug instance, as recorded in the registry."""

    pid: int
    start_time: float
    mindspace_path: str
    launched_at: float


class InstanceRegistry:
    """
    Reads and writes the registry of running Humbug instances.

    The registry lives in ``~/.humbug/instances/``, one file per instance named after
    its process id.
    """

    INSTANCES_DIR = "instances"

    _logger = logging.getLogger("InstanceRegistry")

    def __init__(self, humbug_dir: str | None = None) -> None:
        """
        Initialise the registry.

        Args:
            humbug_dir: Path to the user's ``~/.humbug`` directory.  Defaults to the
                standard location, which is where every instance registers.
        """
        if humbug_dir is None:
            humbug_dir = os.path.expanduser("~/.humbug")

        self._instances_dir = os.path.join(humbug_dir, self.INSTANCES_DIR)

    def register(self, mindspace_path: str) -> None:
        """
        Record this process in the registry, replacing any previous entry.

        Args:
            mindspace_path: The mindspace this instance currently has open, or an
                empty string if none
        """
        record = InstanceRecord(
            pid=os.getpid(),
            start_time=self._process_start_time(os.getpid()),
            mindspace_path=mindspace_path,
            launched_at=time.time()
        )

        try:
            json_store.write_json(self._record_path(os.getpid()), {
                "pid": record.pid,
                "start_time": record.start_time,
                "mindspace_path": record.mindspace_path,
                "launched_at": record.launched_at,
            })

        except OSError as e:
            # Registration is a convenience for other instances, not a correctness
            # requirement for this one, so a failure must not stop the app starting.
            self._logger.warning("Failed to register instance: %s", str(e))

    def unregister(self) -> None:
        """Remove this process's entry from the registry."""
        try:
            os.remove(self._record_path(os.getpid()))

        except FileNotFoundError:
            pass

        except OSError as e:
            self._logger.warning("Failed to unregister instance: %s", str(e))

    def find_live_instance(self, mindspace_path: str) -> InstanceRecord | None:
        """
        Find a live instance other than this one that has the given mindspace open.

        Stale entries — those whose process is gone, or whose pid has been reused by an
        unrelated process — are removed as they are encountered.

        Args:
            mindspace_path: Absolute path to the mindspace to look for

        Returns:
            The record of a live instance with that mindspace open, or None
        """
        if not mindspace_path:
            return None

        target = os.path.realpath(mindspace_path)
        self_pid = os.getpid()

        try:
            names = os.listdir(self._instances_dir)

        except FileNotFoundError:
            return None

        except OSError as e:
            self._logger.warning("Failed to read instance registry: %s", str(e))
            return None

        for name in names:
            if not name.endswith(".json"):
                continue

            record = self._read_record(os.path.join(self._instances_dir, name))
            if record is None:
                continue

            if record.pid == self_pid:
                continue

            if not self._is_alive(record):
                self._remove_stale(record.pid)
                continue

            if os.path.realpath(record.mindspace_path) == target:
                return record

        return None

    def is_open_elsewhere(self, mindspace_path: str) -> bool:
        """
        Return True if another live instance has the given mindspace open.

        Args:
            mindspace_path: Absolute path to the mindspace to check
        """
        return self.find_live_instance(mindspace_path) is not None

    def _record_path(self, pid: int) -> str:
        """Return the path of the registry file for a process id."""
        return os.path.join(self._instances_dir, f"{pid}.json")

    def _read_record(self, path: str) -> InstanceRecord | None:
        """
        Read one registry file, returning None if it is unreadable or malformed.

        A malformed file is treated as absent rather than as an error: it can only be
        the residue of a crashed or interrupted write, and it carries no usable
        information either way.
        """
        data = json_store.read_json(path)
        if data is None:
            return None

        pid = data.get("pid")
        start_time = data.get("start_time")
        mindspace_path = data.get("mindspace_path")
        launched_at = data.get("launched_at")

        if not isinstance(pid, int) or not isinstance(start_time, (int, float)):
            return None

        if not isinstance(mindspace_path, str) or not isinstance(launched_at, (int, float)):
            return None

        return InstanceRecord(
            pid=pid,
            start_time=float(start_time),
            mindspace_path=mindspace_path,
            launched_at=float(launched_at)
        )

    def _is_alive(self, record: InstanceRecord) -> bool:
        """
        Return True if the recorded process is still running.

        The process start time is compared as well as the pid, so that a pid reused by
        an unrelated process after a crash is not mistaken for the instance that
        recorded it.

        Where the start time cannot be determined (no ``/proc``, as on macOS and
        Windows) the comparison is skipped and the check degrades to pid existence.
        That is weaker: a reused pid can then be mistaken for a live instance, which
        would wrongly report a mindspace as already open.  The failure is safe — the
        user is told to look for an existing window rather than being given a second
        instance — but it is a real limitation of the non-Linux paths.
        """
        if not self._process_exists(record.pid):
            return False

        current_start = self._process_start_time(record.pid)
        if current_start == 0.0 or record.start_time == 0.0:
            return True

        return abs(current_start - record.start_time) < 1.0

    def _process_exists(self, pid: int) -> bool:
        """Return True if a process with the given id exists."""
        if pid <= 0:
            return False

        if sys.platform == "win32":
            return self._process_exists_windows(pid)

        try:
            os.kill(pid, 0)

        except ProcessLookupError:
            return False

        except PermissionError:
            # The process exists but belongs to another user.
            return True

        except OSError:
            return False

        return True

    def _process_exists_windows(self, pid: int) -> bool:
        """
        Return True if a process with the given id exists, on Windows.

        ``os.kill`` cannot be used to probe liveness here: on Windows only
        ``CTRL_C_EVENT`` and ``CTRL_BREAK_EVENT`` are valid signals, and any other
        value terminates the target process outright.  Using it to ask whether another
        instance is alive would therefore kill that instance.  The process is opened
        for query instead and its exit code inspected.
        """
        process_query_limited_information = 0x1000
        still_active = 259
        error_access_denied = 5

        kernel32 = ctypes.windll.kernel32  # type: ignore[attr-defined]
        handle = kernel32.OpenProcess(process_query_limited_information, False, pid)
        if not handle:
            # A process owned by another user exists but cannot be opened.
            return bool(kernel32.GetLastError() == error_access_denied)

        try:
            exit_code = ctypes.c_ulong()
            if not kernel32.GetExitCodeProcess(handle, ctypes.byref(exit_code)):
                return False

            return bool(exit_code.value == still_active)

        finally:
            kernel32.CloseHandle(handle)

    def _process_start_time(self, pid: int) -> float:
        """
        Return the start time of a process, or 0.0 if it cannot be determined.

        A 0.0 result means the pid-reuse check degrades to a pid-existence check.  That
        is a weaker check, but it is still correct in the common case and never
        produces a false negative.
        """
        try:
            return os.stat(f"/proc/{pid}").st_mtime

        except OSError:
            pass

        return 0.0

    def _remove_stale(self, pid: int) -> None:
        """Remove a stale registry entry, ignoring failure."""
        try:
            os.remove(self._record_path(pid))

        except OSError:
            pass
