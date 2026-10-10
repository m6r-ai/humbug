"""Concurrency-safe reading and writing of the JSON files Humbug keeps on disk."""

from collections.abc import Callable, Iterator
from contextlib import contextmanager
import json
import logging
import os
import sys
import tempfile
from typing import Any, IO

_logger = logging.getLogger("JsonStore")

if sys.platform == "win32":
    import msvcrt  # pylint: disable=import-error

else:
    import fcntl


def _lock_file(handle: IO[str], blocking: bool) -> bool:
    """
    Take an exclusive advisory lock on an open file.

    The lock is held by the operating system, so it is released automatically if
    the process exits or crashes.  There are no stale locks to clean up.

    Args:
        handle: Open file handle to lock.
        blocking: Wait for the lock when True, give up immediately when False.

    Returns:
        True if the lock was taken, False if it was held elsewhere.
    """
    try:
        if sys.platform == "win32":
            mode = msvcrt.LK_LOCK if blocking else msvcrt.LK_NBLCK
            msvcrt.locking(handle.fileno(), mode, 1)
            return True

        flags = fcntl.LOCK_EX
        if not blocking:
            flags |= fcntl.LOCK_NB

        fcntl.flock(handle.fileno(), flags)
        return True

    except OSError:
        return False


@contextmanager
def locked(path: str) -> Iterator[None]:
    """
    Hold an exclusive lock for the duration of the block, waiting if necessary.

    The lock is taken on a sibling ".lock" file so that the file being guarded
    can still be replaced atomically while the lock is held.

    Args:
        path: Path of the file being guarded.
    """
    lock_path = f"{path}.lock"
    os.makedirs(os.path.dirname(lock_path), mode=0o700, exist_ok=True)

    with open(lock_path, 'w', encoding='utf-8') as handle:
        _lock_file(handle, blocking=True)
        yield


def acquire(path: str) -> IO[str] | None:
    """
    Take an exclusive lock without waiting, to be held until the handle closes.

    Used to claim ownership of a resource for as long as a process runs.  The
    caller keeps the returned handle open; closing it releases the claim.

    Args:
        path: Path of the file being guarded.

    Returns:
        An open handle holding the lock, or None if another process holds it.
    """
    lock_path = f"{path}.lock"
    try:
        os.makedirs(os.path.dirname(lock_path), mode=0o700, exist_ok=True)
        handle = open(lock_path, 'w', encoding='utf-8')  # pylint: disable=consider-using-with

    except OSError as e:
        _logger.error("Failed to open lock file %s: %s", lock_path, str(e))
        return None

    if not _lock_file(handle, blocking=False):
        handle.close()
        return None

    return handle


def read_json(path: str) -> dict[str, Any] | None:
    """
    Read a JSON object from a file.

    Args:
        path: Path to read.

    Returns:
        The parsed object, or None if the file is missing, unreadable, or does
        not contain a JSON object.
    """
    try:
        with open(path, encoding='utf-8') as f:
            data = json.load(f)

    except (OSError, json.JSONDecodeError):
        return None

    if not isinstance(data, dict):
        return None

    return data


def write_json(path: str, data: dict[str, Any], mode: int = 0o600) -> None:
    """
    Write a JSON object by replacing the target file atomically.

    A reader either sees the previous contents or the new contents, never a
    partially written file.  The temporary file is uniquely named, so writers
    in different threads or processes cannot disturb each other mid-write.

    Args:
        path: Path to write.
        data: JSON-serialisable object to store.
        mode: Permission bits for the file.  These files hold user
            configuration, so they default to being readable only by the user.

    Raises:
        OSError: If the file cannot be written.
        TypeError: If the data is not JSON-serialisable.
    """
    directory = os.path.dirname(path)
    os.makedirs(directory, mode=0o700, exist_ok=True)
    handle, temp_path = tempfile.mkstemp(dir=directory, prefix=".json-store-")

    try:
        with os.fdopen(handle, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)

        os.chmod(temp_path, mode)
        os.replace(temp_path, path)

    except Exception:
        try:
            os.remove(temp_path)

        except OSError:
            pass

        raise


def update_json(path: str, mutate: Callable[[dict[str, Any]], dict[str, Any]], mode: int = 0o600) -> None:
    """
    Re-read, transform, and rewrite a JSON file while holding its lock.

    Reading inside the lock means a concurrent writer's changes are merged
    rather than discarded, which a read-then-write sequence would do.

    Args:
        path: Path to update.
        mutate: Called with the current contents (empty if absent); returns the
            contents to store.
        mode: Permission bits for the file.
    """
    with locked(path):
        current = read_json(path) or {}
        try:
            write_json(path, mutate(current), mode)

        except OSError as e:
            _logger.error("Failed to update %s: %s", path, str(e))
