import uuid
import logging
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Set, TypeVar, Type

from context.context_info import ContextInfo


class ContextEvent(Enum):
    """Events emitted by the ContextRegistry."""
    OPENED  = auto()   # args: (context_info: ContextInfo, is_ephemeral: bool)
    CLOSED  = auto()   # args: (context_id: str)
    UPDATED = auto()   # args: (context_info: ContextInfo)
    FOCUSED = auto()   # args: (context_id: str)


class ContextRegistry:
    """
    Tracks all open contexts within a mindspace.

    Frontend-agnostic: notifies observers via registered callbacks.  A UI can
    subscribe to these events and create or destroys visualizations accordingly.

    ContextInfo objects are immutable snapshots.  Mutable state lives here;
    callers should call get() again when they need a fresh view.
    """

    def __init__(self) -> None:
        """Initialise an empty registry."""
        self._contexts: Dict[str, ContextInfo] = {}
        self._models: Dict[str, Any] = {}
        self._callbacks: Dict[ContextEvent, Set[Callable]] = {
            event: set() for event in ContextEvent
        }
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

    def open(
        self,
        context_type: str,
        path: str  = "",
        title: str  = "",
        is_ephemeral: bool = False,
        context_id: str  = "",
        initial_model: Any = None,
        requester_id: str = "",
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
                           open.  Forwarded opaquely to OPENED callbacks so the
                           frontend can use it for tab placement decisions.

        Returns:
            The context_id for the newly registered context.
        """
        if not context_id:
            context_id = str(uuid.uuid4())

        info = ContextInfo(
            context_id=context_id,
            context_type=context_type,
            path=path,
            title=title,
            is_modified=False,
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
        if context_id in self._contexts:
            del self._contexts[context_id]
            self._emit(ContextEvent.CLOSED, context_id)
        self._models.pop(context_id, None)

    def update(self, context_id: str, **kwargs: Any) -> None:
        """
        Update mutable fields on a context and emit UPDATED.

        Only title, path, and is_modified may be updated.
        Unknown keys are silently ignored.

        Args:
            context_id: ID of the context to update.
            **kwargs:   Fields to update (title, is_modified).
        """
        info = self._contexts.get(context_id)
        if info is None:
            return

        allowed = {"title", "path", "is_modified"}
        updates = {k: v for k, v in kwargs.items() if k in allowed}
        if not updates:
            return

        self._contexts[context_id] = ContextInfo(
            context_id=info.context_id,
            context_type=info.context_type,
            path=updates.get("path", info.path),
            title=updates.get("title", info.title),
            is_modified=updates.get("is_modified", info.is_modified),
        )
        self._emit(ContextEvent.UPDATED, self._contexts[context_id])

    def focus(self, context_id: str) -> None:
        """
        Signal that a context has been brought to the front.

        This is a transient intent — it does not change stored state, only
        notifies observers.  The Qt layer scrolls to the tab; a CLI might print
        the context title; the registry itself records nothing.

        Args:
            context_id: ID of the context being focused.
        """
        if context_id in self._contexts:
            self._emit(ContextEvent.FOCUSED, context_id)

    def clear(self) -> None:
        """
        Remove all contexts without emitting events.

        Used when a mindspace is closed — the Qt layer tears down tabs through
        its own mechanisms, so events are not needed.
        """
        self._contexts.clear()
        self._models.clear()

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
        for info in self._contexts.values():
            if info.path == path and info.context_type == context_type:
                return info

        return None

    def list_all(self) -> List[ContextInfo]:
        """
        Return a snapshot list of all open contexts in registration order.

        Returns:
            List of ContextInfo snapshots.
        """
        return list(self._contexts.values())

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

    def get_model(self, context_id: str, model_type: 'Type[T]') -> 'T | None':
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
