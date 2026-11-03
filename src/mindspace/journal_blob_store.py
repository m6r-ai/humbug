"""Content-addressed storage for the pre-images held by the change journal."""

import hashlib
import os


class JournalBlobStore:
    """
    Stores pre-image content keyed by its SHA-256 digest.

    Blobs are fanned out one level on the first two hexadecimal characters of
    the digest.  Identical content is stored once, so repeated writes of the
    same bytes cost nothing beyond the digest.

    The store is not responsible for deciding what to keep: it holds content
    until something removes it.
    """

    FANOUT_LENGTH = 2

    def __init__(self, directory: str) -> None:
        """
        Initialise a store rooted at a directory.

        Args:
            directory: Directory holding the blob tree.  It is created when the
                first blob is stored.
        """
        self._directory = directory

    def directory(self) -> str:
        """Return the root directory of the blob store."""
        return self._directory

    @staticmethod
    def digest_of(content: bytes) -> str:
        """Return the SHA-256 digest of content as lowercase hexadecimal."""
        return hashlib.sha256(content).hexdigest()

    @staticmethod
    def digest_of_file(path: str) -> str:
        """
        Return the SHA-256 digest of a file's content, read in chunks.

        Chunked reading keeps the cost independent of the file's size in memory,
        which matters for the copy and move operations where the content is
        never held whole.

        Args:
            path: File to read.

        Returns:
            The digest as lowercase hexadecimal.

        Raises:
            OSError: The file could not be read.
        """
        digest = hashlib.sha256()
        with open(path, 'rb') as f:
            while chunk := f.read(1024 * 1024):
                digest.update(chunk)

        return digest.hexdigest()

    def store(self, content: bytes) -> str:
        """
        Store content and return its digest.

        The blob is written to a temporary file and renamed into place, so a
        crash can leave an unreferenced temporary file but never a blob whose
        content is only partly written.

        Args:
            content: Raw bytes to store.

        Returns:
            The SHA-256 digest identifying the stored content.

        Raises:
            OSError: The blob could not be written.
        """
        digest = self.digest_of(content)
        path = self.path_for(digest)
        if os.path.exists(path):
            return digest

        os.makedirs(os.path.dirname(path), exist_ok=True)
        temp_path = f"{path}.tmp"

        try:
            with open(temp_path, 'wb') as f:
                f.write(content)
                f.flush()
                os.fsync(f.fileno())

            os.replace(temp_path, path)

        except OSError:
            self._remove_quietly(temp_path)
            raise

        return digest

    def retrieve(self, digest: str) -> bytes | None:
        """
        Return the content of a stored blob, or None if it is not present.

        Args:
            digest: Digest returned by store().

        Returns:
            The stored bytes, or None when no blob with that digest exists.

        Raises:
            OSError: The blob exists but could not be read.
        """
        path = self.path_for(digest)
        try:
            with open(path, 'rb') as f:
                return f.read()

        except FileNotFoundError:
            return None

    def path_for(self, digest: str) -> str:
        """
        Return the path a blob with the given digest is stored at.

        Args:
            digest: SHA-256 digest of the content.

        Returns:
            Absolute path within the blob tree.
        """
        prefix = digest[:self.FANOUT_LENGTH]
        return os.path.join(self._directory, prefix, digest)

    @staticmethod
    def _remove_quietly(path: str) -> None:
        """Remove path if it exists, ignoring the failure if it does not."""
        try:
            os.remove(path)

        except OSError:
            pass
