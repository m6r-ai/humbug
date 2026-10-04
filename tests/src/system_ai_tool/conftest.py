"""
Shared fixtures and utilities for system AI tool tests.
"""
import asyncio
import os
import tempfile
from typing import Any
from unittest.mock import MagicMock

import pytest

from ai_tool import AIToolCall
from mindspace.mindspace import Mindspace
from system_ai_tool.system_ai_tool import SystemAITool


def _make_tool_call(operation: str, **kwargs: Any) -> AIToolCall:
    """Create an AIToolCall for the given operation and arguments."""
    arguments: dict[str, Any] = {"operation": operation}
    arguments.update(kwargs)
    return AIToolCall(id="test-id", name="system", arguments=arguments)


def _make_auth_callback(authorized: bool = True) -> MagicMock:
    """Create a mocked authorization callback."""
    mock = MagicMock()

    async def mock_auth_callback(
        _tool_name: str,
        _arguments: dict[str, Any],
        _context: str,
        _auth_context: str | None,
        _destructive: bool,
    ) -> bool:
        return authorized

    mock.side_effect = mock_auth_callback
    return mock


def _execute_tool(
    tool: SystemAITool,
    tool_call: AIToolCall,
    auth_callback: MagicMock | None = None,
) -> Any:
    """Execute a tool call synchronously and return the result."""
    if auth_callback is None:
        auth_callback = _make_auth_callback(authorized=True)

    return asyncio.run(tool.execute(tool_call, None, auth_callback))


@pytest.fixture
def temp_mindspace() -> Any:
    """
    Create a temp directory with a real Mindspace instance.

    Yields (mindspace, mindspace_path).
    """
    with tempfile.TemporaryDirectory() as mindspace_path:
        mindspace = Mindspace(
            on_settings_changed=lambda: None,
            on_interactions_updated=lambda: None,
            on_usage_updated=lambda: None,
        )
        mindspace.create_mindspace(mindspace_path, [])
        mindspace.open_mindspace(mindspace_path)

        yield mindspace, mindspace_path


@pytest.fixture
def system_tool(temp_mindspace: Any) -> SystemAITool:
    """Fixture providing a SystemAITool with a real mindspace."""
    mindspace, _ = temp_mindspace
    return SystemAITool(mindspace)


@pytest.fixture
def mock_authorization() -> MagicMock:
    """Fixture providing a mocked authorization callback that approves."""
    return _make_auth_callback(authorized=True)


@pytest.fixture
def mock_authorization_denied() -> MagicMock:
    """Fixture providing a mocked authorization callback that denies."""
    return _make_auth_callback(authorized=False)


@pytest.fixture
def make_tool_call() -> Any:
    """Fixture providing the tool call factory."""
    return _make_tool_call
