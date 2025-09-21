"""Environment management for AIFPL variable and function scoping."""

from typing import Any, Dict, Optional, List
from dataclasses import dataclass, field

from aifpl.aifpl_error import AIFPLEvalError


@dataclass(frozen=True)
class AIFPLEnvironment:
    """
    Immutable environment for variable and function bindings with lexical scoping.

    Supports nested scopes where inner environments can access outer bindings
    but not vice versa.
    """
    bindings: Dict[str, Any] = field(default_factory=dict)  # Any = AIFPLValue, avoiding circular import
    parent: Optional['AIFPLEnvironment'] = None
    name: str = "anonymous"

    def define(self, name: str, value: Any) -> 'AIFPLEnvironment':
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

    def lookup(self, name: str) -> Any:
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
            if hasattr(value, 'get_resolved_value'):
                return value.get_resolved_value()

            return value

        if self.parent is not None:
            return self.parent.lookup(name)

        # Create helpful error message with available bindings
        available = self.get_available_bindings()
        if available:
            available_str = ", ".join(f"'{name}'" for name in sorted(available))
            raise AIFPLEvalError(f"Undefined variable: '{name}'. Available bindings: {available_str}")

        raise AIFPLEvalError(f"Undefined variable: '{name}'. No bindings available in current scope")

    def has_binding(self, name: str) -> bool:
        """
        Check if a variable has a binding in this environment or parent environments.

        Args:
            name: Variable name to check

        Returns:
            True if variable has a binding, False otherwise
        """
        if name in self.bindings:
            return True

        if self.parent is not None:
            return self.parent.has_binding(name)

        return False

    def is_defined(self, name: str) -> bool:
        """
        Check if a variable is defined in this environment or parent environments.

        Args:
            name: Variable name to check

        Returns:
            True if variable is defined, False otherwise
        """
        return self.has_binding(name)

    def get_local_bindings(self) -> Dict[str, Any]:
        """
        Get bindings defined in this environment only (not parents).

        Returns:
            Dictionary of local bindings
        """
        return self.bindings.copy()

    def get_available_bindings(self) -> List[str]:
        """Get all available binding names in this environment chain."""
        available = list(self.bindings.keys())

        if self.parent is not None:
            available.extend(self.parent.get_available_bindings())

        return available

    def create_child(self, name: str = "child") -> 'AIFPLEnvironment':
        """
        Create a child environment with this environment as parent.

        Args:
            name: Name for the child environment (for debugging)

        Returns:
            New child environment
        """
        return AIFPLEnvironment({}, self, name)

    def __repr__(self) -> str:
        """String representation for debugging."""
        local_bindings = list(self.bindings.keys())
        parent_info = f" (parent: {self.parent.name})" if self.parent else ""
        return f"AIFPLEnvironment({self.name}: {local_bindings}{parent_info})"


@dataclass(frozen=True)
class AIFPLTailCall:
    """Represents a tail call to be optimized."""
    function: Any  # AIFPLSExpression
    arguments: List[Any]  # List[AIFPLSExpression]
    environment: AIFPLEnvironment


class AIFPLCallStack:
    """
    Call stack for tracking function calls and providing detailed error messages.
    """

    @dataclass
    class CallFrame:
        """Represents a single function call frame."""
        function_name: str
        arguments: Dict[str, Any]
        expression: str
        position: int

    def __init__(self) -> None:
        """Initialize empty call stack."""
        self.frames: List[AIFPLCallStack.CallFrame] = []

    def push(self, function_name: str, arguments: Dict[str, Any], expression: str = "", position: int = 0) -> None:
        """
        Push a new call frame onto the stack.

        Args:
            function_name: Name of the function being called
            arguments: Dictionary of parameter names to values
            expression: String representation of the expression
            position: Position in source code
        """
        frame = AIFPLCallStack.CallFrame(
            function_name=function_name,
            arguments=arguments,
            expression=expression,
            position=position
        )
        self.frames.append(frame)

    def pop(self) -> Optional['AIFPLCallStack.CallFrame']:
        """
        Pop the top call frame from the stack.

        Returns:
            The popped frame, or None if stack is empty
        """
        if self.frames:
            return self.frames.pop()

        return None

    def peek(self) -> Optional['AIFPLCallStack.CallFrame']:
        """
        Peek at the top call frame without removing it.

        Returns:
            The top frame, or None if stack is empty
        """
        if self.frames:
            return self.frames[-1]

        return None

    def is_empty(self) -> bool:
        """Check if the call stack is empty."""
        return len(self.frames) == 0

    def depth(self) -> int:
        """Get the current call stack depth."""
        return len(self.frames)

    def format_stack_trace(self, max_frames: int = 10) -> str:
        """
        Format the call stack as a string for error messages.

        Args:
            max_frames: Maximum number of frames to include

        Returns:
            Formatted stack trace string
        """
        if not self.frames:
            return "  (no function calls)"

        lines = []
        frames_to_show = self.frames[-max_frames:] if len(self.frames) > max_frames else self.frames

        if len(self.frames) > max_frames:
            lines.append(f"  ... ({len(self.frames) - max_frames} more frames)")

        for i, frame in enumerate(frames_to_show):
            indent = "  " + "  " * i
            args_str = ", ".join(f"{k}={repr(v)}" for k, v in frame.arguments.items())
            if args_str:
                lines.append(f"{indent}{frame.function_name}({args_str})")

            else:
                lines.append(f"{indent}{frame.function_name}()")

            if frame.expression:
                lines.append(f"{indent}  -> {frame.expression}")

        return "\n".join(lines)

    def __repr__(self) -> str:
        """String representation for debugging."""
        return f"AIFPLCallStack(depth={len(self.frames)})"
