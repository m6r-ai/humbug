"""Tests for which message an authorization request says it belongs to."""

import asyncio
from typing import Any

from ai import AIConversation, AIConversationEvent, AIMessage
from ai_tool import AIToolApprovalKind, AIToolCall, AIToolResult

_TOOL_CALL = AIToolCall(id="call-1", name="http", arguments={"url": "https://example.com"})


async def _wait_until_asking(conversation: AIConversation) -> None:
    """Wait until the conversation is waiting for a human to answer an approval request."""
    for _ in range(100):
        if conversation._pending_authorization_future is not None:  # pylint: disable=protected-access
            return

        await asyncio.sleep(0)

    raise AssertionError("The request never reached the approval wait")


async def _ask(conversation: AIConversation, answer: bool) -> bool:
    """Ask for authorization, answer it as a human, and return whether it was granted."""
    request = asyncio.create_task(
        conversation._request_tool_authorization(  # pylint: disable=protected-access
            "http", {}, "GET the URL", None, False, AIToolApprovalKind.STANDARD
        )
    )
    await _wait_until_asking(conversation)

    if answer:
        await conversation.approve_pending_tool_calls()

    else:
        await conversation.reject_pending_tool_calls("The user rejected the request")

    return await request


def _execute_one_tool_call(conversation: AIConversation) -> None:
    """Run one tool call whose tool asks twice, approving and then rejecting."""
    async def tool(tool_call: AIToolCall) -> AIToolResult:
        assert await _ask(conversation, True) is True
        assert await _ask(conversation, False) is False

        return AIToolResult(id=tool_call.id, name=tool_call.name, content="done")

    async def no_ai() -> None:
        """Stand in for the AI request that follows the tool results."""

    setattr(conversation, "_execute_tool", tool)
    setattr(conversation, "_start_ai", no_ai)
    asyncio.run(conversation._execute_tool_calls([_TOOL_CALL]))  # pylint: disable=protected-access


class TestTheMessageOfAnAuthorizationRequest:
    """Tests for the message an authorization request is placed on."""

    def test_a_second_request_in_one_tool_call_names_the_same_message(self) -> None:
        """A tool that asks twice has both requests placed on its own tool-call message."""
        conversation = AIConversation()
        seen: list[AIMessage | None] = []

        async def on_required(message: AIMessage | None, *_args: Any) -> None:
            seen.append(message)

        conversation.register_callback(AIConversationEvent.TOOL_APPROVAL_REQUIRED, on_required)

        _execute_one_tool_call(conversation)

        assert len(seen) == 2
        assert seen[0] is not None
        assert seen[1] is seen[0]

    def test_the_message_is_released_when_the_tool_call_finishes(self) -> None:
        """The conversation holds no tool-call message once the tool results are in."""
        conversation = AIConversation()

        _execute_one_tool_call(conversation)

        assert conversation._executing_tool_call_message is None  # pylint: disable=protected-access

