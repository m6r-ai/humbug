from collections.abc import Callable
from enum import Enum, auto
import logging
import os.path
from typing import Any, TypeVar
import uuid

from context.context_info import ContextInfo


class ContextEvent(Enum):
    """Events emitted by the ContextRegistry."""
    OPENED = auto()        # args: (context_info: ContextInfo, is_ephemeral: bool, requester_id: str)
    CLOSED = auto()        # args: (context_id: str)
    UPDATED = auto()       # args: (context_info: ContextInfo)
    FOCUSED = auto()       # args: (context_id: str)
    MOVED = auto()         # args: (context_id: str, column: int)
    COLUMN_SPLIT = auto()  # args: (context_id: str, split_left: bool)
    COLUMN_MERGE = auto()  # args: (column: int, merge_left: bool)
    COLUMN_SWAP = auto()   # args: (column: int, swap_left: bool)


class ContextRegistry:
    """
    Tracks all open contexts.

    Frontend-agnostic: notifies observers via registered callbacks.  A UI can
    subscribe to these events and create or destroys visualizations accordingly.

    ContextInfo objects are immutable snapshots.  Mutable state lives here;
    callers should call get() again when they need a fresh view.
    """

    MAX_COLUMNS = 6

    def __init__(self) -> None:
        """Initialise an empty registry."""
        self._contexts: dict[str, ContextInfo] = {}
        self._models: dict[str, Any] = {}
        self._content_state: dict[str, dict[str, Any]] = {}
        self._frontend_state: dict[str, dict[str, Any]] = {}
        self._current_by_column: dict[int, str] = {}
        self._callbacks: dict[ContextEvent, set[Callable]] = {
            event: set() for event in ContextEvent
        }
        self._current_context_id: str | None = None
        self._logger = logging.getLogger("ContextRegistry")

    def register_callback(self, event: ContextEvent, callback: Callable) -> None:
        """
        Register a callback for a context event.

        Args:
            event: The event to subscribe to.
            callback: Callable invoked with event-specific arguments when the
                event fires.
        """
        self._callbacks[event].add(callback)

    def unregister_callback(self, event: ContextEvent, callback: Callable) -> None:
        """
        Unregister a previously registered callback.

        Args:
            event: The event to unsubscribe from.
            callback: The callback to remove.
        """
        self._callbacks[event].discard(callback)

    @staticmethod
    def _normalize_path(path: str) -> str:
        """
        Normalise a path so that string comparisons are reliable.

        Uses os.path.normpath which collapses redundant separators and
        resolves '.' and '..' components without resolving symlinks.
        Empty paths are returned unchanged.

        Args:
            path: File path to normalise (may be empty).

        Returns:
            Normalised path, or the empty string if *path* was empty.
        """
        if not path:
            return path

        return os.path.normpath(path)

    def open(
        self,
        context_type: str,
        path: str  = "",
        title: str  = "",
        is_ephemeral: bool = False,
        context_id: str  = "",
        initial_model: Any = None,
        requester_id: str = "",
        column: int = 0,
        position: int | None = None,
    ) -> str:
        """
        Register a new open context and emit OPENED.

        Args:
            context_type:  The kind of context being opened.
            path:          Associated file path, or empty string.
            title:         Display title.
            is_ephemeral:  True if this context should auto-close when another
                           context is opened in the same column.
            context_id:    Stable ID to use (e.g. when restoring from session).
                           A new UUID is generated if not provided.
            initial_model: Optional model object to register atomically with
                           the context.  Stored before OPENED is emitted so
                           subscribers can retrieve it immediately.
            requester_id:  Optional ID of the context that is requesting this
                           open.  An empty string means the open is
                           user-initiated; a non-empty value means it was
                           triggered by another context (e.g. an AI
                           conversation or shell command).  Forwarded
                           opaquely to OPENED callbacks so the frontend can
                           use it for tab placement and focus decisions.
            column:        Column index (0-based) for layout.  Defaults to 0.
            position:      Index within the column.  When None the context is
                           appended to the end of the column.

        Returns:
            The context_id for the newly registered context.
        """
        if not context_id:
            context_id = str(uuid.uuid4())

        if position is None:
            position = self._next_position(column)

        info = ContextInfo(
            context_id=context_id,
            context_type=context_type,
            path=self._normalize_path(path),
            title=title,
            is_modified=False,
            is_ephemeral=is_ephemeral,
            column=column,
            position=position,
        )
        self._contexts[context_id] = info
        if initial_model is not None:
            self._models[context_id] = initial_model

        self._emit(ContextEvent.OPENED, info, is_ephemeral, requester_id)
        return context_id

    def close(self, context_id: str) -> None:
        """
        Deregister a context and emit CLOSED.

        Args:
            context_id: ID of the context to close.
        """
        info = self._contexts.get(context_id)
        if context_id in self._contexts:
            del self._contexts[context_id]
            self._emit(ContextEvent.CLOSED, context_id)

        if info is not None:
            self._normalize_positions(info.column)

        self._models.pop(context_id, None)
        self._content_state.pop(context_id, None)
        self._frontend_state.pop(context_id, None)

        if self._current_context_id == context_id:
            self._current_context_id = None

    def update(self, context_id: str, **kwargs: Any) -> None:
        """
        Update mutable fields on a context and emit UPDATED.

        Only title, path, is_modified, is_ephemeral, column, and position may
        be updated.
        Unknown keys are silently ignored.

        Args:
            context_id: ID of the context to update.
            **kwargs:   Fields to update (title, path, is_modified,
                        is_ephemeral, column, position).
        """
        info = self._contexts.get(context_id)
        if info is None:
            return

        allowed = {"title", "path", "is_modified", "is_ephemeral", "column", "position"}
        updates = {k: v for k, v in kwargs.items() if k in allowed}
        if not updates:
            return

        if "path" in updates:
            updates["path"] = self._normalize_path(updates["path"])

        self._contexts[context_id] = ContextInfo(
            context_id=info.context_id,
            context_type=info.context_type,
            path=updates.get("path", info.path),
            title=updates.get("title", info.title),
            is_modified=updates.get("is_modified", info.is_modified),
            is_ephemeral=updates.get("is_ephemeral", info.is_ephemeral),
            column=updates.get("column", info.column),
            position=updates.get("position", info.position),
        )
        self._emit(ContextEvent.UPDATED, self._contexts[context_id])

    def focus(self, context_id: str) -> None:
        """
        Signal that a context has been brought to the front.

        Stores the current context id and emits FOCUSED.  The Qt layer scrolls
        to the tab; a CLI might print the context title.

        Args:
            context_id: ID of the context being focused.
        """
        if context_id in self._contexts:
            self._current_context_id = context_id
            self._emit(ContextEvent.FOCUSED, context_id)

    def move(self, context_id: str, column: int) -> None:
        """
        Move a context to a different column and emit MOVED.

        Args:
            context_id: ID of the context to move.
            column:     Target column index (0-based, max 5).

        Raises:
            ValueError: If the context does not exist or column is out of range.
        """
        info = self._contexts.get(context_id)
        if info is None:
            raise ValueError(f"Context not found: {context_id}")

        if not 0 <= column < self.MAX_COLUMNS:
            raise ValueError(
                f"Column must be 0-{self.MAX_COLUMNS - 1}, got {column}"
            )

        if info.column == column:
            return

        source_column = info.column
        self._contexts[context_id] = ContextInfo(
            context_id=info.context_id,
            context_type=info.context_type,
            path=info.path,
            title=info.title,
            is_modified=info.is_modified,
            is_ephemeral=info.is_ephemeral,
            column=column,
            position=self._next_position(column),
        )
        self._normalize_positions(source_column)
        self._normalize_positions(column)
        self._emit(ContextEvent.MOVED, context_id, column)

    def split_column(self, context_id: str, split_left: bool) -> None:
        """
        Split the column containing a context, moving that context to a new column.

        The context is moved to a newly inserted column.  When ``split_left`` is
        True the new column is inserted at the current column's index and the
        context stays at that index; otherwise the new column is inserted to the
        right and the context moves to the new index.  All other contexts at or
        beyond the insertion point shift right by one.  Emits COLUMN_SPLIT.

        Args:
            context_id: ID of the context to move into the new column.
            split_left: Whether the new column is inserted to the left.

        Raises:
            ValueError: If the context does not exist or the split would exceed
                the maximum number of columns.
        """
        info = self._contexts.get(context_id)
        if info is None:
            raise ValueError(f"Context not found: {context_id}")

        current = info.column
        if self.num_columns() >= self.MAX_COLUMNS:
            raise ValueError(
                f"Cannot split: maximum {self.MAX_COLUMNS} columns reached"
            )

        if split_left:
            for cid, other in list(self._contexts.items()):
                if cid != context_id and other.column >= current:
                    self._set_column(cid, other.column + 1)

            self._set_column(context_id, current)

        else:
            for cid, other in list(self._contexts.items()):
                if cid != context_id and other.column > current:
                    self._set_column(cid, other.column + 1)

            self._set_column(context_id, current + 1)

        self._normalize_all_positions()
        self._emit(ContextEvent.COLUMN_SPLIT, context_id, split_left)

    def merge_column(self, column: int, merge_left: bool) -> None:
        """
        Merge the column at ``column`` into the adjacent column.

        All contexts in the merged column move to the adjacent column and the
        column is removed, shifting any columns to the right left by one.
        Emits COLUMN_MERGE.

        Args:
            column:     Index of the column to merge away.
            merge_left: True to merge into the left neighbour, False for the right.

        Raises:
            ValueError: If the column is out of range or there is no adjacent
                column to merge into.
        """
        if not 0 <= column < self.num_columns():
            raise ValueError(f"Column out of range: {column}")

        target = column + (-1 if merge_left else 1)
        if not 0 <= target < self.MAX_COLUMNS:
            raise ValueError(f"No adjacent column to merge into for column {column}")

        for cid, other in list(self._contexts.items()):
            if other.column == column:
                self._set_column(cid, target)

            elif other.column > column:
                self._set_column(cid, other.column - 1)

        self._normalize_all_positions()
        self._emit(ContextEvent.COLUMN_MERGE, column, merge_left)

    def swap_column(self, column: int, swap_left: bool) -> None:
        """
        Swap the column at ``column`` with the adjacent column.

        All contexts in the two columns exchange column indices.  Emits
        COLUMN_SWAP.

        Args:
            column:     Index of the column to swap.
            swap_left:  True if swapping with the left column, False for the right.

        Raises:
            ValueError: If the column is out of range or there is no adjacent
                column to swap with.
        """
        if not 0 <= column < self.num_columns():
            raise ValueError(f"Column out of range: {column}")

        target = column + (-1 if swap_left else 1)
        if not 0 <= target < self.MAX_COLUMNS:
            raise ValueError(f"No adjacent column to swap with for column {column}")

        for cid, other in list(self._contexts.items()):
            if other.column == column:
                self._set_column(cid, target)

            elif other.column == target:
                self._set_column(cid, column)

        self._normalize_all_positions()
        self._emit(ContextEvent.COLUMN_SWAP, column, swap_left)

    def _set_column(self, context_id: str, column: int) -> None:
        """
        Update a context's column index without emitting an event.

        Used internally by the column operations, which emit a single
        high-level event after updating all affected contexts.

        Args:
            context_id: ID of the context to update.
            column:     New column index.
        """
        info = self._contexts[context_id]
        self._contexts[context_id] = ContextInfo(
            context_id=info.context_id,
            context_type=info.context_type,
            path=info.path,
            title=info.title,
            is_modified=info.is_modified,
            is_ephemeral=info.is_ephemeral,
            column=column,
            position=info.position,
        )

    def _next_position(self, column: int) -> int:
        """
        Return the position one past the last context in a column.

        Args:
            column: Column index to inspect.

        Returns:
            The next free position in the column, or 0 if it is empty.
        """
        positions = [
            info.position for info in self._contexts.values() if info.column == column
        ]
        return max(positions) + 1 if positions else 0

    def _normalize_positions(self, column: int) -> None:
        """
        Renumber a column's contexts to contiguous positions from zero.

        Contexts are ordered by their current position, so relative order is
        preserved while gaps left by moves and closes are removed.

        Args:
            column: Column index to renumber.
        """
        members = sorted(
            (info for info in self._contexts.values() if info.column == column),
            key=lambda info: info.position,
        )
        for position, info in enumerate(members):
            if info.position == position:
                continue

            self._contexts[info.context_id] = ContextInfo(
                context_id=info.context_id,
                context_type=info.context_type,
                path=info.path,
                title=info.title,
                is_modified=info.is_modified,
                is_ephemeral=info.is_ephemeral,
                column=info.column,
                position=position,
            )

    def _normalize_all_positions(self) -> None:
        """Renumber positions in every column that currently has contexts."""
        for column in range(self.num_columns()):
            self._normalize_positions(column)

    def make_permanent(self, context_id: str) -> None:
        """
        Convert an ephemeral context to permanent.

        If the context is not ephemeral or does not exist, this is a no-op.

        Args:
            context_id: ID of the context to make permanent.
        """
        info = self._contexts.get(context_id)
        if info is None or not info.is_ephemeral:
            return

        self._contexts[context_id] = ContextInfo(
            context_id=info.context_id,
            context_type=info.context_type,
            path=info.path,
            title=info.title,
            is_modified=info.is_modified,
            is_ephemeral=False,
            column=info.column,
            position=info.position,
        )
        self._emit(ContextEvent.UPDATED, self._contexts[context_id])

    def current_context_id(self) -> str | None:
        """
        Return the ID of the currently focused context, or None.

        Returns:
            The current context ID, or None if no context is focused.
        """
        return self._current_context_id

    def clear(self) -> None:
        """
        Remove all contexts without emitting events.
        """
        self._contexts.clear()
        self._models.clear()
        self._content_state.clear()
        self._frontend_state.clear()
        self._current_by_column.clear()
        self._current_context_id = None

    def save_state(self, current_by_column: dict[int, str] | None = None) -> dict[str, Any]:
        """
        Serialise the full workspace state to a JSON-safe dictionary.

        Each context contributes three parts: its layout (from ContextInfo),
        its content state (from the model's save_content_state), and its opaque
        frontend state.  Contexts without a registered model contribute empty
        content state.

        Args:
            current_by_column: Maps a column index to the context id that is
                currently visible in that column.  The registry does not track
                this itself, so the frontend supplies it.

        Returns:
            Dictionary with contexts, focused_id, num_columns, and
            current_by_column keys.
        """
        contexts = []
        ordered = sorted(
            self._contexts.values(), key=lambda info: (info.column, info.position)
        )
        for info in ordered:
            contexts.append({
                "layout": {
                    "context_id": info.context_id,
                    "context_type": info.context_type,
                    "path": info.path,
                    "title": info.title,
                    "is_ephemeral": info.is_ephemeral,
                    "column": info.column,
                    "position": info.position,
                },
                "content": self._content_state_for(info.context_id),
                "frontend": self._frontend_state.get(info.context_id, {}),
            })

        return {
            "contexts": contexts,
            "focused_id": self._current_context_id,
            "num_columns": self.num_columns(),
            "current_by_column": {
                str(column): context_id
                for column, context_id in (current_by_column or {}).items()
            },
        }

    def _content_state_for(self, context_id: str) -> dict[str, Any]:
        """
        Return the content state for a context, or empty if it has no model.

        Args:
            context_id: ID of the context to serialise.

        Returns:
            JSON-safe dictionary of content state.
        """
        model = self._models.get(context_id)
        if model is None:
            return {}

        save = getattr(model, "save_content_state", None)
        if save is None:
            return {}

        try:
            return save()

        except Exception:
            self._logger.exception(
                "Failed to save content state for context %s", context_id
            )
            return {}

    def restore_state(self, state: dict[str, Any]) -> None:
        """
        Rebuild the workspace from a dictionary produced by save_state.

        Existing state is cleared first.  Each context's content state and
        opaque frontend state are retained and made available via
        get_content_state and get_frontend_state before OPENED is emitted, so
        that subscribers handling OPENED can reconstruct the context.

        Args:
            state: Dictionary with contexts, focused_id, num_columns, and
                current_by_column keys.
        """
        self.clear()

        self._current_by_column = {
            int(column): context_id
            for column, context_id in state.get("current_by_column", {}).items()
        }

        entries = sorted(
            state.get("contexts", []),
            key=lambda entry: (
                entry.get("layout", {}).get("column", 0),
                entry.get("layout", {}).get("position", 0),
            ),
        )
        for entry in entries:
            layout = entry.get("layout", {})
            context_type = layout.get("context_type", "")
            context_id = layout.get("context_id", "")
            if not context_type or not context_id:
                continue

            self._content_state[context_id] = entry.get("content", {})
            self._frontend_state[context_id] = entry.get("frontend", {})
            self.open(
                context_type=context_type,
                path=layout.get("path", ""),
                title=layout.get("title", ""),
                is_ephemeral=layout.get("is_ephemeral", False),
                context_id=context_id,
                column=layout.get("column", 0),
                position=layout.get("position", 0),
            )

        focused_id = state.get("focused_id")
        if focused_id is not None:
            self.focus(focused_id)

    def get_content_state(self, context_id: str) -> dict[str, Any]:
        """
        Return the content state retained for a context during restore.

        Content state is populated by restore_state and holds the information
        a frontend needs to reconstruct a context that is not already present
        in its ContextInfo (for example a terminal's command).  It is empty for
        contexts opened normally rather than restored.

        Args:
            context_id: ID of the context to retrieve content state for.

        Returns:
            The retained dictionary, or an empty dictionary if none was set.
        """
        return self._content_state.get(context_id, {})

    def get(self, context_id: str) -> ContextInfo | None:
        """
        Return a snapshot of a context by ID, or None if not found.

        Args:
            context_id: ID of the context to retrieve.

        Returns:
            Immutable ContextInfo snapshot, or None.
        """
        return self._contexts.get(context_id)

    def get_by_path_and_type(self, path: str, context_type: str) -> ContextInfo | None:
        """
        Find an open context by its associated path and type.

        Multiple contexts of different types may share the same path
        (e.g. an editor tab and a diff tab open for the same file).

        Args:
            path: Absolute file path to search for.
            context_type: The context type string to match (e.g. 'editor', 'diff').

        Returns:
            Immutable ContextInfo snapshot, or None if not found.
        """
        normalized = self._normalize_path(path)
        for info in self._contexts.values():
            if info.path == normalized and info.context_type == context_type:
                return info

        return None

    def list_all(self) -> list[ContextInfo]:
        """
        Return a snapshot list of all open contexts in registration order.

        Returns:
            List of ContextInfo snapshots.
        """
        return list(self._contexts.values())

    def num_columns(self) -> int:
        """
        Return the number of columns currently in use.

        A column is "in use" if at least one context is assigned to it.

        Returns:
            Number of columns with at least one context.
        """
        if not self._contexts:
            return 0

        return max(info.column for info in self._contexts.values()) + 1

    T = TypeVar('T')

    def register_model(self, context_id: str, model: Any) -> None:
        """
        Associate a context model object with a context_id.

        The model is stored alongside the ContextInfo entry and can be
        retrieved by any code that has access to the registry.  It is
        automatically removed when the context is closed.

        Args:
            context_id: ID of the context to associate the model with.
            model: The context model object (e.g. TerminalContext).
        """
        self._models[context_id] = model

    def get_model(self, context_id: str, model_type: 'type[T]') -> 'T | None':
        """
        Retrieve the context model for a context_id, type-checked.

        Args:
            context_id: ID of the context whose model to retrieve.
            model_type: Expected type of the model.

        Returns:
            The model if found and of the correct type, otherwise None.
        """
        model = self._models.get(context_id)
        if model is None:
            return None

        if not isinstance(model, model_type):
            return None

        return model

    def set_frontend_state(self, context_id: str, state: dict[str, Any]) -> None:
        """
        Store opaque frontend state for a context.

        The registry stores and returns this state but never interprets it.
        A frontend uses it to preserve view state (cursor positions, scroll
        offsets) that has no meaning without a view.  It is included in
        save_state and removed when the context closes.

        Args:
            context_id: ID of the context the state belongs to.
            state: JSON-safe dictionary of frontend state.
        """
        self._frontend_state[context_id] = state

    def get_frontend_state(self, context_id: str) -> dict[str, Any]:
        """
        Return the opaque frontend state stored for a context.

        Args:
            context_id: ID of the context to retrieve state for.

        Returns:
            The stored dictionary, or an empty dictionary if none was set.
        """
        return self._frontend_state.get(context_id, {})

    def get_current_by_column(self) -> dict[int, str]:
        """
        Return the context id that was visible in each column when saved.

        This is populated by restore_state from the saved state.  The registry
        does not track which context is visible in a column itself; a frontend
        supplies it at save time and applies it after restore.

        Returns:
            Mapping of column index to context id.
        """
        return dict(self._current_by_column)

    def __len__(self) -> int:
        """Return the number of open contexts."""
        return len(self._contexts)

    def _emit(self, event: ContextEvent, *args: Any) -> None:
        """Invoke all callbacks registered for an event."""
        for callback in self._callbacks[event]:
            try:
                callback(*args)

            except Exception:
                self._logger.exception(
                    "Error in ContextRegistry callback for %s", event
                )
