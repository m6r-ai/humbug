"""Signs that a mission's work has stalled: errors that may pass, and actions repeated to no effect."""

import hashlib
import json
from typing import Any

from ai_tool import AIToolCall, AIToolResult


# Errors the AI service gives when it cannot be reached or is too busy: they are likely to pass.
_TRANSIENT_ERROR_CODES = {"network_error", "rate_limit", "server_error"}


def is_transient_error(error: dict[str, Any] | None) -> bool:
    """
    Return True if a failed turn's error is one that may pass if the turn is tried again later.

    A lost connection, a rate limit and a server error may; a rejected key, a bad request or a
    cancellation will not, however long Mission Control waits.

    Args:
        error: The error recorded on the conversation's error message
    """
    if not error:
        return False

    code = str(error.get("code", ""))
    if code in _TRANSIENT_ERROR_CODES:
        return True

    # A status code the service still gave after its own retries ran out.
    return code == "429" or (code.isdigit() and code.startswith("5"))


class RepeatedActionCounter:
    """
    Counts how many times in a row a conversation has made the same tool call with the same result.

    An action that keeps giving the same answer is going nowhere: polling a command whose
    output changes, or retrying something that then works, is not counted as a repeat.
    """

    def __init__(self) -> None:
        """Start with nothing seen."""
        self._calls: dict[str, str] = {}
        self._last = ""
        self._count = 0

    def note_calls(self, tool_calls: list[AIToolCall]) -> None:
        """Remember the tool calls a conversation made, so their results can be matched to them."""
        for call in tool_calls:
            self._calls[call.id] = f"{call.name}:{json.dumps(call.arguments, sort_keys=True, default=str)}"

    def note_results(self, tool_results: list[AIToolResult]) -> int:
        """
        Note the results of tool calls, returning how many times in a row this outcome has been seen.

        Args:
            tool_results: The results the conversation received together
        """
        parts = []
        for result in tool_results:
            call = self._calls.pop(result.id, result.name)
            parts.append(f"{call}\n{result.content}\n{result.error or ''}")

        outcome = hashlib.sha256("\n\n".join(sorted(parts)).encode("utf-8")).hexdigest()
        if outcome == self._last:
            self._count += 1

        else:
            self._last = outcome
            self._count = 1

        return self._count

    def reset(self) -> None:
        """Forget the repeats seen so far, as when the user steps in."""
        self._last = ""
        self._count = 0
