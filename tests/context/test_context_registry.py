"""Tests for the ContextRegistry: path normalisation, layout, and state persistence."""
import os

from context.context_registry import ContextEvent, ContextRegistry


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


class _StubModel:
    """Minimal context model used to exercise save/restore."""

    context_type = "stub"

    def __init__(self, value: str = "") -> None:
        self._value = value

    def save_content_state(self) -> dict:
        """Return the stub's content state."""
        return {"value": self._value}

    def value(self) -> str:
        """Return the stub's value."""
        return self._value


class TestSaveState:
    """Tests for ContextRegistry.save_state."""

    def test_save_state_captures_layout(self) -> None:
        """Layout fields are captured for each open context."""
        registry = ContextRegistry()
        registry.open(
            context_type="editor",
            path="/tmp/a/file.py",
            title="file.py",
            is_ephemeral=True,
            column=2,
        )

        state = registry.save_state()

        assert state["num_columns"] == 3
        assert len(state["contexts"]) == 1
        layout = state["contexts"][0]["layout"]
        assert layout["context_type"] == "editor"
        assert layout["path"] == "/tmp/a/file.py"
        assert layout["title"] == "file.py"
        assert layout["is_ephemeral"] is True
        assert layout["column"] == 2

    def test_save_state_captures_content_from_model(self) -> None:
        """Content state is taken from the model's save_content_state."""
        registry = ContextRegistry()
        cid = registry.open(context_type="stub", initial_model=_StubModel("hello"))

        state = registry.save_state()

        entry = state["contexts"][0]
        assert entry["layout"]["context_id"] == cid
        assert entry["content"] == {"value": "hello"}

    def test_save_state_content_empty_without_model(self) -> None:
        """A context with no model contributes empty content state."""
        registry = ContextRegistry()
        registry.open(context_type="diff", path="/tmp/a/file.py")

        state = registry.save_state()

        assert state["contexts"][0]["content"] == {}

    def test_save_state_captures_frontend_state(self) -> None:
        """Opaque frontend state is included in the saved state."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/file.py")
        registry.set_frontend_state(cid, {"cursor": {"line": 3, "column": 1}})

        state = registry.save_state()

        assert state["contexts"][0]["frontend"] == {"cursor": {"line": 3, "column": 1}}

    def test_save_state_captures_focused_id(self) -> None:
        """The currently focused context id is captured."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/file.py")
        registry.focus(cid)

        state = registry.save_state()

        assert state["focused_id"] == cid


class TestRestoreState:
    """Tests for ContextRegistry.restore_state."""

    def test_restore_state_round_trips_layout(self) -> None:
        """Layout survives a save/restore round trip."""
        registry = ContextRegistry()
        cid = registry.open(
            context_type="editor",
            path="/tmp/a/file.py",
            title="file.py",
            is_ephemeral=True,
            column=1,
        )
        state = registry.save_state()

        restored = ContextRegistry()
        restored.restore_state(state)

        info = restored.get(cid)
        assert info is not None
        assert info.context_type == "editor"
        assert info.path == "/tmp/a/file.py"
        assert info.title == "file.py"
        assert info.is_ephemeral is True
        assert info.column == 1

    def test_restore_state_retains_content_state(self) -> None:
        """Content state from the saved model is retained after restore."""
        registry = ContextRegistry()
        cid = registry.open(context_type="stub", initial_model=_StubModel("hello"))
        state = registry.save_state()

        restored = ContextRegistry()
        restored.restore_state(state)

        assert restored.get_content_state(cid) == {"value": "hello"}

    def test_restore_state_content_state_empty_without_model(self) -> None:
        """A context with no model restores with empty content state."""
        registry = ContextRegistry()
        cid = registry.open(context_type="diff", path="/tmp/a/file.py")
        state = registry.save_state()

        restored = ContextRegistry()
        restored.restore_state(state)

        assert restored.get(cid) is not None
        assert restored.get_content_state(cid) == {}

    def test_restore_state_restores_frontend_state(self) -> None:
        """Opaque frontend state survives a save/restore round trip."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/file.py")
        registry.set_frontend_state(cid, {"scroll": 42})
        state = registry.save_state()

        restored = ContextRegistry()
        restored.restore_state(state)

        assert restored.get_frontend_state(cid) == {"scroll": 42}

    def test_restore_state_restores_focus(self) -> None:
        """The focused context is restored."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/file.py")
        registry.focus(cid)
        state = registry.save_state()

        restored = ContextRegistry()
        restored.restore_state(state)

        assert restored.current_context_id() == cid

    def test_restore_state_clears_existing_contexts(self) -> None:
        """Restoring replaces any contexts already in the registry."""
        registry = ContextRegistry()
        registry.open(context_type="editor", path="/tmp/a/old.py")
        state = ContextRegistry().save_state()

        registry.restore_state(state)

        assert len(registry) == 0

    def test_restore_state_emits_opened_for_each_context(self) -> None:
        """OPENED is emitted for each restored context."""
        registry = ContextRegistry()
        registry.open(context_type="editor", path="/tmp/a/file.py")
        state = registry.save_state()

        restored = ContextRegistry()
        opened: list[str] = []
        restored.register_callback(
            ContextEvent.OPENED, lambda info, _ephemeral, _requester="": opened.append(info.context_id)
        )
        restored.restore_state(state)

        assert len(opened) == 1


class TestFrontendState:
    """Tests for opaque frontend state storage."""

    def test_get_frontend_state_defaults_to_empty(self) -> None:
        """An unset context returns an empty dictionary."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/file.py")

        assert registry.get_frontend_state(cid) == {}

    def test_frontend_state_removed_on_close(self) -> None:
        """Closing a context discards its frontend state."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/file.py")
        registry.set_frontend_state(cid, {"scroll": 1})

        registry.close(cid)

        assert registry.get_frontend_state(cid) == {}


class TestContentState:
    """Tests for content state retained during restore."""

    def test_get_content_state_defaults_to_empty(self) -> None:
        """A context opened normally has no retained content state."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/file.py")

        assert registry.get_content_state(cid) == {}

    def test_content_state_removed_on_close(self) -> None:
        """Closing a restored context discards its retained content state."""
        registry = ContextRegistry()
        cid = registry.open(context_type="stub", initial_model=_StubModel("hello"))
        state = registry.save_state()

        restored = ContextRegistry()
        restored.restore_state(state)
        restored.close(cid)

        assert restored.get_content_state(cid) == {}


class TestOrdering:
    """Tests for position within a column."""

    def test_open_appends_to_end_of_column(self) -> None:
        """Contexts opened without a position are appended in order."""
        registry = ContextRegistry()
        first = registry.open(context_type="editor", path="/tmp/a/1.py")
        second = registry.open(context_type="editor", path="/tmp/a/2.py")

        assert registry.get(first).position == 0
        assert registry.get(second).position == 1

    def test_open_with_explicit_position(self) -> None:
        """An explicit position is stored as given."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/1.py", position=5)

        assert registry.get(cid).position == 5

    def test_positions_are_per_column(self) -> None:
        """Each column numbers its contexts independently from zero."""
        registry = ContextRegistry()
        left = registry.open(context_type="editor", path="/tmp/a/1.py", column=0)
        right = registry.open(context_type="editor", path="/tmp/a/2.py", column=1)

        assert registry.get(left).position == 0
        assert registry.get(right).position == 0

    def test_close_renumbers_remaining_contexts(self) -> None:
        """Closing a context leaves the remaining positions contiguous."""
        registry = ContextRegistry()
        first = registry.open(context_type="editor", path="/tmp/a/1.py")
        second = registry.open(context_type="editor", path="/tmp/a/2.py")
        third = registry.open(context_type="editor", path="/tmp/a/3.py")

        registry.close(second)

        assert registry.get(first).position == 0
        assert registry.get(third).position == 1

    def test_move_appends_to_target_column(self) -> None:
        """A moved context is appended to the end of the target column."""
        registry = ContextRegistry()
        existing = registry.open(context_type="editor", path="/tmp/a/1.py", column=1)
        moving = registry.open(context_type="editor", path="/tmp/a/2.py", column=0)

        registry.move(moving, 1)

        assert registry.get(existing).position == 0
        assert registry.get(moving).position == 1

    def test_move_renumbers_source_column(self) -> None:
        """Moving a context out of a column leaves no gap behind."""
        registry = ContextRegistry()
        first = registry.open(context_type="editor", path="/tmp/a/1.py", column=0)
        moving = registry.open(context_type="editor", path="/tmp/a/2.py", column=0)
        third = registry.open(context_type="editor", path="/tmp/a/3.py", column=0)

        registry.move(moving, 1)

        assert registry.get(first).position == 0
        assert registry.get(third).position == 1

    def test_save_state_orders_contexts_by_column_and_position(self) -> None:
        """Saved contexts are ordered by column then position."""
        registry = ContextRegistry()
        registry.open(context_type="editor", path="/tmp/a/2.py", column=0, position=1)
        registry.open(context_type="editor", path="/tmp/a/1.py", column=0, position=0)
        registry.open(context_type="editor", path="/tmp/a/3.py", column=1, position=0)

        state = registry.save_state()

        paths = [entry["layout"]["path"] for entry in state["contexts"]]
        assert paths == ["/tmp/a/1.py", "/tmp/a/2.py", "/tmp/a/3.py"]

    def test_restore_state_preserves_order(self) -> None:
        """Position within each column survives a save/restore round trip."""
        registry = ContextRegistry()
        first = registry.open(context_type="editor", path="/tmp/a/1.py", column=0)
        second = registry.open(context_type="editor", path="/tmp/a/2.py", column=0)
        third = registry.open(context_type="editor", path="/tmp/a/3.py", column=1)
        state = registry.save_state()

        restored = ContextRegistry()
        restored.restore_state(state)

        assert restored.get(first).position == 0
        assert restored.get(second).position == 1
        assert restored.get(third).position == 0
        assert restored.get(third).column == 1

    def test_split_column_renumbers_positions(self) -> None:
        """Splitting a column leaves both columns numbered from zero."""
        registry = ContextRegistry()
        moved = registry.open(context_type="editor", path="/tmp/a/1.py", column=0)
        registry.open(context_type="editor", path="/tmp/a/2.py", column=0)

        registry.split_column(moved, split_left=False)

        positions = sorted(
            info.position for info in registry.list_all() if info.column == 1
        )
        assert positions == [0]


class TestCurrentByColumn:
    """Tests for the per-column current tab carried through save/restore."""

    def test_save_state_records_supplied_current_tabs(self) -> None:
        """The current tab supplied by the frontend is written to the state."""
        registry = ContextRegistry()
        first = registry.open(context_type="editor", path="/tmp/a/1.py", column=0)
        second = registry.open(context_type="editor", path="/tmp/a/2.py", column=1)

        state = registry.save_state(current_by_column={0: first, 1: second})

        assert state["current_by_column"] == {"0": first, "1": second}

    def test_save_state_defaults_to_empty_current_tabs(self) -> None:
        """Omitting the current tabs yields an empty mapping."""
        registry = ContextRegistry()
        registry.open(context_type="editor", path="/tmp/a/1.py")

        state = registry.save_state()

        assert state["current_by_column"] == {}

    def test_restore_state_exposes_current_tabs(self) -> None:
        """The current tabs survive a save/restore round trip."""
        registry = ContextRegistry()
        first = registry.open(context_type="editor", path="/tmp/a/1.py", column=0)
        second = registry.open(context_type="editor", path="/tmp/a/2.py", column=1)
        state = registry.save_state(current_by_column={0: first, 1: second})

        restored = ContextRegistry()
        restored.restore_state(state)

        assert restored.get_current_by_column() == {0: first, 1: second}

    def test_restore_state_current_tabs_are_ints(self) -> None:
        """Column keys are restored as integers, not strings."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/1.py")
        state = registry.save_state(current_by_column={2: cid})

        restored = ContextRegistry()
        restored.restore_state(state)

        assert list(restored.get_current_by_column().keys()) == [2]

    def test_get_current_by_column_defaults_to_empty(self) -> None:
        """A registry that has not restored state has no current tabs."""
        registry = ContextRegistry()

        assert registry.get_current_by_column() == {}

    def test_clear_discards_current_tabs(self) -> None:
        """Clearing the registry discards any restored current tabs."""
        registry = ContextRegistry()
        cid = registry.open(context_type="editor", path="/tmp/a/1.py")
        state = registry.save_state(current_by_column={0: cid})

        restored = ContextRegistry()
        restored.restore_state(state)
        restored.clear()

        assert restored.get_current_by_column() == {}
