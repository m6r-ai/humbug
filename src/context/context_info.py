from dataclasses import dataclass


@dataclass(frozen=True)
class ContextInfo:
    """
    Immutable snapshot of an open context's state.

    Callers receive this as a point-in-time view.  The registry owns the live
    state; call ContextRegistry.get() again to get a fresh snapshot.
    """
    context_id: str
    context_type: str
    path: str   # empty string if no path associated
    title: str
    is_modified: bool
