"""Environment management for AIFPL variable and function scoping."""

from typing import Dict, List
from dataclasses import dataclass

from aifpl.aifpl_value import AIFPLValue


class AIFPLCallStack:
    """
    Call stack for tracking function calls and providing detailed error messages.
    """

    @dataclass
    class CallFrame:
        """Represents a single function call frame."""
        function_name: str
        arguments: Dict[str, AIFPLValue]
        expression: AIFPLValue

    def __init__(self) -> None:
        """Initialize empty call stack."""
        self.frames: List[AIFPLCallStack.CallFrame] = []

    def push(self, function_name: str, arguments: Dict[str, AIFPLValue], expression: AIFPLValue) -> None:
        """
        Push a new call frame onto the stack.

        Args:
            function_name: Name of the function being called
            arguments: Dictionary of parameter names to values
            expression: AST node of the expression
        """
        frame = AIFPLCallStack.CallFrame(
            function_name=function_name,
            arguments=arguments,
            expression=expression
        )
        self.frames.append(frame)

    def pop(self) -> 'AIFPLCallStack.CallFrame | None':
        """
        Pop the top call frame from the stack.

        Returns:
            The popped frame, or None if stack is empty
        """
        assert self.frames, "Attempted to pop from an empty call stack"
        return self.frames.pop()

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
            if frame.arguments:
                args_str = ", ".join(f"{k}={repr(v)}" for k, v in frame.arguments.items())
                lines.append(f"{indent}{frame.function_name}({args_str})")

            else:
                lines.append(f"{indent}{frame.function_name}()")

            # Convert expression to string only when formatting (during error)
            expr_str = str(frame.expression) if frame.expression and hasattr(frame.expression, '__str__') else "<body>"
            lines.append(f"{indent}  -> {expr_str}")

        return "\n".join(lines)

    def __repr__(self) -> str:
        """String representation for debugging."""
        return f"AIFPLCallStack(depth={len(self.frames)})"
