"""Content-addressed storage for the pre-images held by the change journal."""

import hashlib
import os
import tempfile


class JournalBlobStore:
    """
    Stores pre-image content keyed by its SHA-256 digest.

    Blobs are fanned out one level on the first two hexadecimal characters of
    the digest.  Identical content is stored once, so repeated writes of the
    same bytes cost nothing beyond the digest.

    The store is not responsible for deciding what to keep: it holds content
    until something removes it.  ``sweep`` is how a caller reclaims the blobs
    nothing refers to any more, and it is deliberately driven by a set the
    caller computes rather than by any knowledge of the index.
    """

    FANOUT_LENGTH = 2
    TEMP_SUFFIX = ".tmp"

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
        content is only partly written.  The temporary file is created with a
        unique name so that two stores of the same content cannot collide.

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

        directory = os.path.dirname(path)
        os.makedirs(directory, exist_ok=True)
        descriptor, temp_path = tempfile.mkstemp(dir=directory, prefix=digest, suffix=self.TEMP_SUFFIX)

        try:
            with os.fdopen(descriptor, 'wb') as f:
                f.write(content)
                f.flush()
                os.fsync(f.fileno())

            os.replace(temp_path, path)

        except OSError:
            self._remove_quietly(temp_path)
            raise

        return digest

    def store_file(self, source_path: str) -> str:
        """
        Store a file's content and return its digest, without holding it in memory.

        Content is copied in chunks through a temporary file and renamed into place,
        so storing a file of any size costs a constant amount of memory.  This is what
        lets a large file be captured at all: reading it whole would make the capture
        cost scale with the file rather than with the change.

        Args:
            source_path: The file to store.

        Returns:
            The SHA-256 digest identifying the stored content.

        Raises:
            OSError: The source could not be read or the blob could not be written.
        """
        digest = hashlib.sha256()
        directory = os.path.dirname(self.path_for("0" * 64))
        os.makedirs(directory, exist_ok=True)
        descriptor, temp_path = tempfile.mkstemp(dir=directory, prefix="storing", suffix=self.TEMP_SUFFIX)

        try:
            with os.fdopen(descriptor, 'wb') as destination, open(source_path, 'rb') as source:
                while chunk := source.read(1024 * 1024):
                    digest.update(chunk)
                    destination.write(chunk)

                destination.flush()
                os.fsync(destination.fileno())

            hexdigest = digest.hexdigest()
            path = self.path_for(hexdigest)
            if os.path.exists(path):
                self._remove_quietly(temp_path)
                return hexdigest

            os.makedirs(os.path.dirname(path), exist_ok=True)
            os.replace(temp_path, path)

        except OSError:
            self._remove_quietly(temp_path)
            raise

        return hexdigest

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

    def digests(self) -> set[str]:
        """
        Return the digest of every blob the store holds.

        Returns:
            The digests, which are the blob names, or an empty set when the
            store has nothing in it.
        """
        found: set[str] = set()

        try:
            prefixes = os.listdir(self._directory)

        except FileNotFoundError:
            return found

        except OSError:
            return found

        for prefix in prefixes:
            prefix_dir = os.path.join(self._directory, prefix)
            if not os.path.isdir(prefix_dir):
                continue

            try:
                names = os.listdir(prefix_dir)

            except OSError:
                continue

            for name in names:
                if not name.endswith(self.TEMP_SUFFIX):
                    found.add(name)

        return found

    def sweep(self, keep: set[str]) -> int:
        """
        Delete every blob whose digest is not in keep, and return the bytes freed.

        A temporary file left behind by an interrupted store is removed too: it
        is not a blob, nothing can reference it, and it will never be renamed
        into place by a later store because temporary names are unique.

        Args:
            keep: Digests that must survive, being those a live index entry
                still refers to.

        Returns:
            The number of bytes removed from the store.
        """
        reclaimed = 0

        try:
            prefixes = os.listdir(self._directory)

        except OSError:
            return 0

        for prefix in prefixes:
            prefix_dir = os.path.join(self._directory, prefix)
            if not os.path.isdir(prefix_dir):
                continue

            try:
                names = os.listdir(prefix_dir)

            except OSError:
                continue

            for name in names:
                if name.endswith(self.TEMP_SUFFIX):
                    self._remove_quietly(os.path.join(prefix_dir, name))
                    continue

                if name in keep:
                    continue

                path = os.path.join(prefix_dir, name)
                try:
                    reclaimed += os.path.getsize(path)

                except OSError:
                    pass

                self._remove_quietly(path)

        return reclaimed

    def total_bytes(self) -> int:
        """
        Return the number of bytes the blob tree occupies.

        Temporary files are counted: they occupy space until a sweep removes
        them, and a size guard should see the space that is actually in use.

        Returns:
            The total size of the blob tree in bytes.
        """
        total = 0

        for root, _dirs, names in os.walk(self._directory):
            for name in names:
                try:
                    total += os.path.getsize(os.path.join(root, name))

                except OSError:
                    pass

        return total

    @staticmethod
    def _remove_quietly(path: str) -> None:
        """Remove path if it exists, ignoring the failure if it does not."""
        try:
            os.remove(path)

        except OSError:
            pass
