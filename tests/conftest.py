"""
Shared fixtures for all tests.
"""
from typing import Any

import pytest

from ai_tool import AIToolCall


class MockRequester:
    """
    Minimal mock of AIConversation for tool tests.

    Simulates a conversation where Menai help has been read.
    """

    def __init__(self, menai_help_read: bool = True) -> None:
        self._menai_help_read = menai_help_read

    def menai_help_read(self) -> bool:
        return self._menai_help_read


@pytest.fixture
def make_tool_call():
    """Factory for creating AIToolCall objects for testing."""
    counter = [0]

    def _make_call(tool_name: str, arguments: dict[str, Any]) -> AIToolCall:
        counter[0] += 1
        return AIToolCall(
            id=f"test_call_{counter[0]}",
            name=tool_name,
            arguments=arguments
        )

    return _make_call
