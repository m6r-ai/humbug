"""Tests for SidebarOperationStack."""

from desktop.sidebar.sidebar_operation_stack import ReversibleOperation, SidebarOperationStack


def _counting_operation(calls: list[str], label: str) -> ReversibleOperation:
    """Build an operation that records which of its callbacks ran."""
    return ReversibleOperation(
        undo=lambda: calls.append(f"undo-{label}"),
        redo=lambda: calls.append(f"redo-{label}"),
        on_discard=lambda: calls.append(f"discard-{label}"),
    )


class TestPushUndoRedo:
    """Basic push/undo/redo round-tripping."""

    def test_undo_then_redo_round_trips(self):
        calls: list[str] = []
        stack = SidebarOperationStack()
        stack.push(_counting_operation(calls, "a"))

        assert stack.can_undo()
        assert not stack.can_redo()

        stack.undo()
        assert calls == ["undo-a"]
        assert not stack.can_undo()
        assert stack.can_redo()

        stack.redo()
        assert calls == ["undo-a", "redo-a"]
        assert stack.can_undo()
        assert not stack.can_redo()

    def test_undo_redo_are_no_ops_when_empty(self):
        stack = SidebarOperationStack()
        stack.undo()
        stack.redo()

        assert not stack.can_undo()
        assert not stack.can_redo()

    def test_push_clears_redo_stack(self):
        calls: list[str] = []
        stack = SidebarOperationStack()
        stack.push(_counting_operation(calls, "a"))
        stack.undo()
        assert stack.can_redo()

        stack.push(_counting_operation(calls, "b"))

        assert not stack.can_redo()

    def test_undo_order_is_last_in_first_out(self):
        calls: list[str] = []
        stack = SidebarOperationStack()
        stack.push(_counting_operation(calls, "a"))
        stack.push(_counting_operation(calls, "b"))

        stack.undo()
        stack.undo()

        assert calls == ["undo-b", "undo-a"]


class TestCapacityEviction:
    """Bounded capacity and cleanup of discarded operations."""

    def test_eviction_discards_only_the_oldest(self):
        calls: list[str] = []
        stack = SidebarOperationStack(capacity=2)
        stack.push(_counting_operation(calls, "a"))
        stack.push(_counting_operation(calls, "b"))
        stack.push(_counting_operation(calls, "c"))

        assert "discard-a" in calls
        assert "discard-b" not in calls
        assert "discard-c" not in calls

    def test_clear_purges_remaining_undo_entries_only(self):
        calls: list[str] = []
        stack = SidebarOperationStack()
        stack.push(_counting_operation(calls, "a"))
        stack.push(_counting_operation(calls, "b"))
        stack.undo()  # "b" moves to the redo stack

        stack.clear()

        assert "discard-a" in calls
        assert "discard-b" not in calls  # sitting in redo, nothing to purge
        assert not stack.can_undo()
        assert not stack.can_redo()
