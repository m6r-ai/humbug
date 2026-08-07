"""Tests for ContextRegistry path normalisation and deduplication."""
import os

from context.context_registry import ContextRegistry


class TestPathNormalization:
    """Tests for path normalisation in the context registry."""

    def test_open_normalizes_redundant_separators(self) -> None:
        """Paths with redundant separators are normalised on open."""
        registry = ContextRegistry()
        registry.open(
            context_type="editor",
            path=os.path.join("/tmp", "a", "", "b", "file.py"),
            title="file.py",
        )

        info = registry.get_by_path_and_type("/tmp/a/b/file.py", "editor")
        assert info is not None
        assert info.path == os.path.normpath("/tmp/a/b/file.py")

    def test_open_normalizes_dot_components(self) -> None:
        """Paths with '.' components are normalised on open."""
        registry = ContextRegistry()
        registry.open(
            context_type="editor",
            path="/tmp/a/./b/file.py",
            title="file.py",
        )

        info = registry.get_by_path_and_type("/tmp/a/b/file.py", "editor")
        assert info is not None

    def test_open_normalizes_dotdot_components(self) -> None:
        """Paths with '..' components are normalised on open."""
        registry = ContextRegistry()
        registry.open(
            context_type="editor",
            path="/tmp/a/sub/../b/file.py",
            title="file.py",
        )

        info = registry.get_by_path_and_type("/tmp/a/b/file.py", "editor")
        assert info is not None

    def test_open_preserves_empty_path(self) -> None:
        """Empty paths are stored as empty strings, not normalised."""
        registry = ContextRegistry()
        cid = registry.open(
            context_type="terminal",
            path="",
            title="Terminal",
        )

        info = registry.get(cid)
        assert info is not None
        assert info.path == ""

    def test_get_by_path_and_type_normalizes_lookup(self) -> None:
        """Lookup with an un-normalised path finds a context stored normalised."""
        registry = ContextRegistry()
        registry.open(
            context_type="editor",
            path="/tmp/a/b/file.py",
            title="file.py",
        )

        info = registry.get_by_path_and_type("/tmp/a/./b/file.py", "editor")
        assert info is not None

    def test_duplicate_path_prevented_after_normalization(self) -> None:
        """
        Opening the same file via two different path strings does not create
        a duplicate — the second open returns the first context.
        """
        registry = ContextRegistry()
        registry.open(
            context_type="editor",
            path="/tmp/a/b/file.py",
            title="file.py",
        )

        # A different string representation of the same path
        existing = registry.get_by_path_and_type("/tmp/a/sub/../b/file.py", "editor")
        assert existing is not None

        # The lookup should find the already-open context, not None
        assert existing.path == os.path.normpath("/tmp/a/b/file.py")

    def test_different_context_types_same_path_coexist(self) -> None:
        """Different context types for the same path are both found."""
        registry = ContextRegistry()
        registry.open(
            context_type="editor",
            path="/tmp/file.py",
            title="file.py",
        )
        registry.open(
            context_type="diff",
            path="/tmp/file.py",
            title="file.py",
        )

        assert registry.get_by_path_and_type("/tmp/file.py", "editor") is not None
        assert registry.get_by_path_and_type("/tmp/file.py", "diff") is not None

    def test_update_normalizes_path(self) -> None:
        """Updating a path normalises the new value."""
        registry = ContextRegistry()
        cid = registry.open(
            context_type="editor",
            path="/tmp/old.py",
            title="old.py",
        )

        registry.update(cid, path="/tmp/new/./file.py", title="file.py")

        info = registry.get(cid)
        assert info is not None
        assert info.path == os.path.normpath("/tmp/new/file.py")

        # Lookup with a normalised path finds it
        assert registry.get_by_path_and_type("/tmp/new/file.py", "editor") is not None
