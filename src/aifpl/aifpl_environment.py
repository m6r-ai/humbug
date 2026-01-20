"""Environment management for AIFPL variable and function scoping."""

from typing import Dict, List, Any
from dataclasses import dataclass, field

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import AIFPLValue, AIFPLRecursivePlaceholder


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

    def define(self, name: str, value: AIFPLValue) -> 'AIFPLEnvironment':
        """
        Return new environment with a variable defined.

        Args:
            name: Variable name
            value: Variable value (AIFPLValue)

        Returns:
            New environment with the binding added
        """
        new_bindings = {**self.bindings, name: value}
        return AIFPLEnvironment(new_bindings, self.parent, self.function)

    def define_many(self, new_bindings: Dict[str, AIFPLValue]) -> 'AIFPLEnvironment':
        """
        Return new environment with multiple variables defined at once.

        This is more efficient than calling define() multiple times because
        it only does one dictionary copy instead of N copies for N bindings.

        Args:
            new_bindings: Dictionary of name -> value bindings to add

        Returns:
            New environment with all bindings added
        """
        if not new_bindings:
            return self

        merged_bindings = {**self.bindings, **new_bindings}
        return AIFPLEnvironment(merged_bindings, self.parent, self.function)

    def lookup(self, name: str) -> AIFPLValue:
        """
        Look up a variable in this environment or parent environments.

        Args:
            name: Variable name to look up

        Returns:
            Variable value

        Raises:
            AIFPLEvalError: If variable is not found
        """
        if name in self.bindings:
            value = self.bindings[name]

            # Handle recursive placeholders
            if isinstance(value, AIFPLRecursivePlaceholder):
                return value.get_resolved_value()

            return value

        if self.parent is not None:
            return self.parent.lookup(name)

        # Create helpful error message with available bindings
        available = self.get_available_bindings()
        available_str = ", ".join(f"'{name}'" for name in sorted(available))
        raise AIFPLEvalError(f"Undefined variable: '{name}'. Available bindings: {available_str}")

    def get_available_bindings(self) -> List[str]:
        """Get all available binding names in this environment chain."""
        available = list(self.bindings.keys())

        if self.parent is not None:
            available.extend(self.parent.get_available_bindings())

        return available

    def __repr__(self) -> str:
        """String representation for debugging."""
        local_bindings = list(self.bindings.keys())

        # Get name from function reference if available
        name = self.function.name if self.function and hasattr(self.function, 'name') else "anonymous"

        # Get parent name from parent's function reference
        parent_name = self.parent.function.name if self.parent and self.parent.function and hasattr(self.parent.function, 'name') else "anonymous"
        parent_info = f" (parent: {parent_name})" if self.parent else ""
        return f"AIFPLEnvironment({name}: {local_bindings}{parent_info})"
