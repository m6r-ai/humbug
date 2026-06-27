"""
Shared fixtures for all tests.
"""
from typing import Any

import pytest

from ai_tool import AIToolCall


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
