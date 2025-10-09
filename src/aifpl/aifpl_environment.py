"""Environment management for AIFPL variable and function scoping."""

from typing import Dict, List
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
    name: str = "anonymous"

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
        return AIFPLEnvironment(new_bindings, self.parent, self.name)

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
        parent_info = f" (parent: {self.parent.name})" if self.parent else ""
        return f"AIFPLEnvironment({self.name}: {local_bindings}{parent_info})"
