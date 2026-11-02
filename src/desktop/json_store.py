"""Concurrency-safe reading and writing of the JSON files Humbug keeps on disk."""

from collections.abc import Callable, Iterator
from contextlib import contextmanager
import errno
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


_UNSUPPORTED_ERRNOS = frozenset({errno.ENOLCK, errno.ENOSYS, errno.ENOTSUP, errno.EOPNOTSUPP})


def _ensure_parent(path: str) -> None:
    """
    Create the directory a file will live in.

    Args:
        path: Path of the file whose directory is needed.  A bare filename has no
            directory part, in which case the current directory already exists and
            there is nothing to create.
    """
    directory = os.path.dirname(path)
    if directory:
        os.makedirs(directory, mode=0o700, exist_ok=True)


def _locking_unsupported(error: OSError, path: str) -> bool:
    """
    Return True if the error means this filesystem cannot lock at all.

    Some network and virtual filesystems do not implement locking.  A home
    directory on such a filesystem must not render Humbug unusable, so the caller
    proceeds without the lock.  That gives up mutual exclusion, which is why it is
    reported rather than passed over quietly.

    Args:
        error: The error raised when taking the lock.
        path: Path being locked, for the report.
    """
    if error.errno not in _UNSUPPORTED_ERRNOS:
        return False

    _logger.warning(
        "Filesystem holding %s does not support locking (%s); continuing without it. "
        "Concurrent Humbug instances cannot be coordinated on this filesystem.",
        path, error.strerror
    )
    return True


def _lock_file(handle: IO[str], path: str, blocking: bool) -> bool:
    """
    Take an exclusive advisory lock on an open file.

    The lock is held by the operating system, so it is released automatically if
    the process exits or crashes.  There are no stale locks to clean up.

    Args:
        handle: Open file handle to lock.
        path: Path being locked, used for reporting.
        blocking: Wait for the lock when True, give up immediately when False.

    Returns:
        True if the caller may proceed, False if the lock is held elsewhere.

    Raises:
        OSError: If the lock could not be taken for any reason other than the
            filesystem not supporting locks, or another holder when blocking.
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

    except OSError as e:
        if _locking_unsupported(e, path):
            return True

        if not blocking:
            return False

        raise


@contextmanager
def locked(path: str) -> Iterator[None]:
    """
    Hold an exclusive lock for the duration of the block, waiting if necessary.

    The lock is taken on a sibling ".lock" file so that the file being guarded
    can still be replaced atomically while the lock is held.

    Args:
        path: Path of the file being guarded.

    Raises:
        OSError: If the lock could not be taken.  The guarded block does not run,
            rather than running without the protection it asked for.
    """
    lock_path = f"{path}.lock"
    _ensure_parent(lock_path)

    with open(lock_path, 'w', encoding='utf-8') as handle:
        _lock_file(handle, lock_path, blocking=True)
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
        _ensure_parent(lock_path)
        handle = open(lock_path, 'w', encoding='utf-8')  # pylint: disable=consider-using-with

    except OSError as e:
        _logger.error("Failed to open lock file %s: %s", lock_path, str(e))
        return None

    if not _lock_file(handle, lock_path, blocking=False):
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
    _ensure_parent(path)
    directory = os.path.dirname(path) or "."
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

    An update that cannot be made is logged and skipped rather than raised.  The
    stored file keeps its previous contents, which is the right outcome for the
    incidental records this is used for; callers that must know whether a write
    succeeded should use locked() and write_json() directly.

    The failures skipped are an I/O error taking the lock or writing the file, and
    a mutate function that returns something that cannot be serialised.

    Args:
        path: Path to update.
        mutate: Called with the current contents (empty if absent); returns the
            contents to store.
        mode: Permission bits for the file.
    """
    try:
        with locked(path):
            current = read_json(path) or {}
            write_json(path, mutate(current), mode)

    except (OSError, TypeError) as e:
        _logger.error("Failed to update %s: %s", path, str(e))
