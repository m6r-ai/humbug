from dataclasses import dataclass

from mindspace.context.context_type import ContextType


@dataclass(frozen=True)
class ContextInfo:
    """
    Immutable snapshot of an open context's state.

    Callers receive this as a point-in-time view.  The registry owns the live
    state; call ContextRegistry.get() again to get a fresh snapshot.
    """
    context_id:   str
    context_type: ContextType
    path:         str   # empty string if not path-associated
    title:        str
    is_ephemeral: bool
    is_modified:  bool
