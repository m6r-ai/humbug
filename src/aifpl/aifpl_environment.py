"""Environment management for AIFPL variable and function scoping."""

from typing import Dict, Any
from dataclasses import dataclass, field

from aifpl.aifpl_value import AIFPLValue


@dataclass(frozen=True)
class AIFPLEnvironment:
    """
    Immutable environment for variable and function bindings with lexical scoping.

    Supports nested scopes where inner environments can access outer bindings
    but not vice versa.
    """
    bindings: Dict[str, AIFPLValue] = field(default_factory=dict)
    parent: 'AIFPLEnvironment | None' = None
    function: Any = None  # Optional AIFPLFunction reference for debugging

    def __repr__(self) -> str:
        """String representation for debugging."""
        local_bindings = list(self.bindings.keys())

        # Get name from function reference if available
        name = self.function.name if self.function and hasattr(self.function, 'name') else "anonymous"

        # Get parent name from parent's function reference
        parent_name = (
            self.parent.function.name if self.parent and self.parent.function and
            hasattr(self.parent.function, 'name') else "anonymous"
        )
        parent_info = f" (parent: {parent_name})" if self.parent else ""
        return f"AIFPLEnvironment({name}: {local_bindings}{parent_info})"
