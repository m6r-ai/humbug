"""Reusable undo/redo stack for reversible sidebar file operations."""

from collections.abc import Callable
from dataclasses import dataclass


@dataclass
class ReversibleOperation:
    """
    A single undoable/redoable sidebar operation.

    The stack itself has no knowledge of filesystem details — the owning
    sidebar supplies closures that perform the actual reversal.
    """

    undo: Callable[[], None]
    redo: Callable[[], None]
    on_discard: Callable[[], None] | None = None
    """Called if this operation is dropped from the undo stack without ever
    being undone again (capacity eviction, or the stack being cleared) — used
    by delete operations to purge their trashed file once it can no longer be
    restored."""


class SidebarOperationStack:
    """
    Bounded undo/redo stack of reversible sidebar file operations.

    Pushing a new operation always clears the redo stack, matching standard
    undo/redo semantics. When the undo stack exceeds capacity, the oldest
    entry is discarded and its on_discard callback (if any) is invoked.
    """

    def __init__(self, capacity: int = 20) -> None:
        """
        Initialize the stack.

        Args:
            capacity: Maximum number of undoable operations retained.
        """
        self._capacity = capacity
        self._undo_stack: list[ReversibleOperation] = []
        self._redo_stack: list[ReversibleOperation] = []

    def push(self, operation: ReversibleOperation) -> None:
        """Record a newly performed operation, clearing any redo history."""
        self._undo_stack.append(operation)
        self._redo_stack.clear()

        while len(self._undo_stack) > self._capacity:
            discarded = self._undo_stack.pop(0)
            if discarded.on_discard:
                discarded.on_discard()

    def can_undo(self) -> bool:
        """Return True if there is an operation to undo."""
        return bool(self._undo_stack)

    def undo(self) -> None:
        """Reverse the most recent operation and move it to the redo stack."""
        if not self._undo_stack:
            return

        operation = self._undo_stack.pop()
        operation.undo()
        self._redo_stack.append(operation)

    def can_redo(self) -> bool:
        """Return True if there is an undone operation to redo."""
        return bool(self._redo_stack)

    def redo(self) -> None:
        """Re-apply the most recently undone operation and move it back to the undo stack."""
        if not self._redo_stack:
            return

        operation = self._redo_stack.pop()
        operation.redo()
        self._undo_stack.append(operation)

    def clear(self) -> None:
        """
        Discard all undo/redo history.

        Any still-trashed delete operations are purged via on_discard, since
        their undo history is being abandoned. Used when switching mindspaces.
        """
        for operation in self._undo_stack:
            if operation.on_discard:
                operation.on_discard()

        self._undo_stack.clear()
        self._redo_stack.clear()
