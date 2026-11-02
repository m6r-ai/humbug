"""Tests for AIConversation interruptible tool waits."""

import asyncio
from typing import Any
from unittest.mock import patch

import pytest

from ai import AIConversation
from ai import ai_conversation
from ai.ai_conversation import AIConversationEvent, INTERRUPTIBLE_WAIT_THRESHOLD_SECONDS
from ai.ai_message import AIMessage, AIMessageSource


def _record_events(conversation: AIConversation) -> list[AIConversationEvent]:
    """Register callbacks that record every interruptible-wait event."""
    events: list[AIConversationEvent] = []

    async def _on_started(*_args: Any) -> None:
        events.append(AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_STARTED)

    async def _on_ended(*_args: Any) -> None:
        events.append(AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_ENDED)

    conversation.register_callback(
        AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_STARTED, _on_started
    )
    conversation.register_callback(
        AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_ENDED, _on_ended
    )

    return events


def test_short_wait_does_not_emit_events():
    """A wait at or below the threshold completes without any UI events."""
    conversation = AIConversation()
    events = _record_events(conversation)

    interrupted = asyncio.run(
        conversation.interruptible_sleep(INTERRUPTIBLE_WAIT_THRESHOLD_SECONDS)
    )

    assert interrupted is False
    assert events == []


def test_long_wait_completes_and_emits_start_and_end():
    """A long wait that is not interrupted returns False and emits both events."""
    conversation = AIConversation()
    events = _record_events(conversation)

    with patch.object(ai_conversation, "INTERRUPTIBLE_WAIT_THRESHOLD_SECONDS", 0.0):
        interrupted = asyncio.run(conversation.interruptible_sleep(0.05))

    assert interrupted is False
    assert events == [
        AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_STARTED,
        AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_ENDED,
    ]


def test_long_wait_can_be_interrupted():
    """Interrupting a long wait returns True and emits both events."""
    conversation = AIConversation()
    events = _record_events(conversation)

    async def _run() -> bool:
        sleep_task = asyncio.ensure_future(conversation.interruptible_sleep(30.0))
        await asyncio.sleep(0.01)
        await conversation.interrupt_pending_wait()
        return await sleep_task

    with patch.object(ai_conversation, "INTERRUPTIBLE_WAIT_THRESHOLD_SECONDS", 0.0):
        interrupted = asyncio.run(_run())

    assert interrupted is True
    assert events == [
        AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_STARTED,
        AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_ENDED,
    ]


def test_interrupt_pending_wait_is_noop_without_wait():
    """Interrupting when no wait is pending does nothing."""
    conversation = AIConversation()

    asyncio.run(conversation.interrupt_pending_wait())


def test_started_event_receives_pending_tool_call_message():
    """The started event carries the current pending tool call message and duration."""
    conversation = AIConversation()
    message = AIMessage.create(AIMessageSource.TOOL_CALL, "{}")
    conversation._pending_tool_call_message = message  # pylint: disable=protected-access

    received: list[tuple[Any, Any]] = []

    async def _on_started(pending_message: Any, duration: Any) -> None:
        received.append((pending_message, duration))

    conversation.register_callback(
        AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_STARTED, _on_started
    )

    with patch.object(ai_conversation, "INTERRUPTIBLE_WAIT_THRESHOLD_SECONDS", 0.0):
        asyncio.run(conversation.interruptible_sleep(0.05))

    assert received == [(message, 0.05)]


def test_pending_interrupt_future_cleared_after_wait():
    """The pending interrupt future is cleared once the wait finishes."""
    conversation = AIConversation()

    with patch.object(ai_conversation, "INTERRUPTIBLE_WAIT_THRESHOLD_SECONDS", 0.0):
        asyncio.run(conversation.interruptible_sleep(0.05))

    assert conversation._pending_interrupt_future is None  # pylint: disable=protected-access


def test_cancel_current_tasks_interrupts_pending_wait():
    """Cancelling current tasks resolves a pending interruptible wait."""
    conversation = AIConversation()

    async def _run() -> bool:
        sleep_task = asyncio.ensure_future(conversation.interruptible_sleep(30.0))
        await asyncio.sleep(0.01)
        conversation.cancel_current_tasks()
        return await sleep_task

    interrupted = asyncio.run(_run())

    assert interrupted is True
