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
        expression: str
        position: int

    def __init__(self) -> None:
        """Initialize empty call stack."""
        self.frames: List[AIFPLCallStack.CallFrame] = []

    def push(self, function_name: str, arguments: Dict[str, AIFPLValue], expression: str = "", position: int = 0) -> None:
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

    def pop(self) -> 'AIFPLCallStack.CallFrame | None':
        """
        Pop the top call frame from the stack.

        Returns:
            The popped frame, or None if stack is empty
        """
        if self.frames:
            return self.frames.pop()

        return None

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
