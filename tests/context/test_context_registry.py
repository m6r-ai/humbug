"""Tests for ContextRegistry path normalisation and deduplication."""
import os

from context.context_registry import ContextRegistry
from conversation_context.conversation_context import ConversationContext


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


class TestColumnOperations:
    """Tests for the column-level operations on the context registry."""

    def test_split_column_right_moves_context_and_shifts(self) -> None:
        """Splitting right moves the context to a new column and shifts later ones."""
        registry = ContextRegistry()
        c0, c1, c2 = self._split_many(registry)

        registry.split_column(c0, split_left=False)

        assert registry.get(c0) is not None and registry.get(c0).column == 1
        assert registry.get(c1) is not None and registry.get(c1).column == 0
        assert registry.get(c2) is not None and registry.get(c2).column == 2

    def _split_many(self, registry: ContextRegistry) -> list[str]:
        """Open three contexts in two columns and return their IDs."""
        c0 = registry.open(context_type="editor", column=0)
        c1 = registry.open(context_type="editor", column=0)
        c2 = registry.open(context_type="terminal", column=1)
        return [c0, c1, c2]

    def test_split_column_left_keeps_context_index_and_shifts_others(self) -> None:
        """Splitting left keeps the context at its index and shifts others right."""
        registry = ContextRegistry()
        c0, c1, c2 = self._split_many(registry)

        registry.split_column(c0, split_left=True)

        assert registry.get(c0) is not None and registry.get(c0).column == 0
        assert registry.get(c1) is not None and registry.get(c1).column == 1
        assert registry.get(c2) is not None and registry.get(c2).column == 2

    def test_split_column_emits_event(self) -> None:
        """Splitting a column emits a COLUMN_SPLIT event."""
        from context.context_registry import ContextEvent
        registry = ContextRegistry()
        c0, _, _ = self._split_many(registry)

        events: list[tuple[str, bool]] = []
        registry.register_callback(
            ContextEvent.COLUMN_SPLIT,
            lambda cid, sl: events.append((cid, sl)),
        )

        registry.split_column(c0, split_left=False)

        assert len(events) == 1
        assert events[0] == (c0, False)

    def test_merge_column_moves_contexts_and_shifts(self) -> None:
        """Merging a column moves its contexts left and shifts later columns."""
        registry = ContextRegistry()
        c0 = registry.open(context_type="editor", column=0)
        c1 = registry.open(context_type="terminal", column=1)
        c2 = registry.open(context_type="preview", column=2)

        registry.merge_column(1, merge_left=True)

        assert registry.get(c0) is not None and registry.get(c0).column == 0
        assert registry.get(c1) is not None and registry.get(c1).column == 0
        assert registry.get(c2) is not None and registry.get(c2).column == 1

    def test_merge_column_emits_event(self) -> None:
        """Merging a column emits a COLUMN_MERGE event."""
        from context.context_registry import ContextEvent
        registry = ContextRegistry()
        registry.open(context_type="editor", column=0)
        registry.open(context_type="terminal", column=1)

        events: list[tuple[int, bool]] = []
        registry.register_callback(
            ContextEvent.COLUMN_MERGE,
            lambda col, ml: events.append((col, ml)),
        )

        registry.merge_column(1, merge_left=True)

        assert len(events) == 1
        assert events[0] == (1, True)

    def test_swap_column_exchanges_columns(self) -> None:
        """Swapping two columns exchanges their contexts' column indices."""
        registry = ContextRegistry()
        c0 = registry.open(context_type="editor", column=0)
        c1 = registry.open(context_type="editor", column=0)
        c2 = registry.open(context_type="terminal", column=1)

        registry.swap_column(1, swap_left=True)

        assert registry.get(c0) is not None and registry.get(c0).column == 1
        assert registry.get(c1) is not None and registry.get(c1).column == 1
        assert registry.get(c2) is not None and registry.get(c2).column == 0

    def test_swap_column_emits_event(self) -> None:
        """Swapping a column emits a COLUMN_SWAP event."""
        from context.context_registry import ContextEvent
        registry = ContextRegistry()
        registry.open(context_type="editor", column=0)
        registry.open(context_type="terminal", column=1)

        events: list[tuple[int, bool]] = []
        registry.register_callback(
            ContextEvent.COLUMN_SWAP,
            lambda col, sl: events.append((col, sl)),
        )

        registry.swap_column(1, swap_left=True)

        assert len(events) == 1
        assert events[0] == (1, True)

    def test_split_column_raises_for_missing_context(self) -> None:
        """Splitting a column for a missing context raises ValueError."""
        registry = ContextRegistry()

        try:
            registry.split_column("missing", split_left=False)
        except ValueError:
            return

        raise AssertionError("Expected ValueError")


class _FakeTranscript:
    """Minimal stand-in for AITranscriptConversation that records settings updates."""

    def __init__(self) -> None:
        self.applied: list[object] = []

    def update_conversation_settings(self, settings: object) -> None:
        self.applied.append(settings)


class TestBroadcastConversationSettings:
    """Tests for broadcasting conversation settings via the registry."""

    def _register_conversation(self, registry: ContextRegistry, transcript: _FakeTranscript) -> str:
        """Open a conversation context registered with the given transcript."""
        cid = registry.open(context_type="conversation")
        context = ConversationContext(
            context_id=cid,
            ai_transcript_conversation=transcript,  # type: ignore[arg-type]
        )
        registry.register_model(cid, context)
        return cid

    def test_broadcast_updates_all_conversation_contexts(self) -> None:
        """Broadcasting settings updates every registered conversation context."""
        registry = ContextRegistry()
        transcripts = [_FakeTranscript(), _FakeTranscript()]
        settings = object()

        for transcript in transcripts:
            self._register_conversation(registry, transcript)

        registry.broadcast_conversation_settings(settings)

        for transcript in transcripts:
            assert transcript.applied == [settings]

    def test_broadcast_skips_non_conversation_contexts(self) -> None:
        """Broadcasting settings leaves non-conversation contexts untouched."""
        registry = ContextRegistry()
        registry.open(context_type="editor")
        transcript = _FakeTranscript()
        settings = object()
        self._register_conversation(registry, transcript)

        registry.broadcast_conversation_settings(settings)

        assert transcript.applied == [settings]
