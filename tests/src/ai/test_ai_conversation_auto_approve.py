"""Tests for the turn-scoped standing approval of journalled file changes."""

import asyncio
from typing import Any

from ai import AIConversation, AIConversationEvent
from ai_tool import AIToolApprovalKind


def _request(conversation: AIConversation, kind: AIToolApprovalKind, tool_name: str = "filesystem") -> Any:
    """Start an authorization request and return the task that resolves with its answer."""
    return asyncio.create_task(
        conversation._request_tool_authorization(  # pylint: disable=protected-access
            tool_name, {"operation": "write_file"}, "This will create a file", None, False, kind
        )
    )


async def _wait_until_asking(conversation: AIConversation) -> None:
    """Wait until the conversation is waiting for a human to answer an approval request."""
    for _ in range(100):
        if conversation._pending_authorization_future is not None:  # pylint: disable=protected-access
            return

        await asyncio.sleep(0)

    raise AssertionError("The request never reached the approval wait")


def _answerable(conversation: AIConversation, kind: AIToolApprovalKind, authorised: bool) -> bool:
    """Ask for authorization, answer it as a human, and return whether it was granted."""
    async def run() -> bool:
        task = _request(conversation, kind)
        await _wait_until_asking(conversation)

        if authorised:
            await conversation.approve_pending_tool_calls()

        else:
            await conversation.reject_pending_tool_calls("User rejected the request")

        return await task

    return asyncio.run(run())


class TestStandingApproval:
    """Tests for whether a standing approval resolves an authorization request."""

    def test_journalled_file_change_is_approved_without_asking(self) -> None:
        """A standing approval resolves a change the journal records, without asking."""
        conversation = AIConversation()
        conversation.set_auto_approve_file_changes(True)

        def run() -> bool:
            return asyncio.run(
                conversation._request_tool_authorization(  # pylint: disable=protected-access
                    "filesystem", {"operation": "write_file"}, "This will create a file",
                    None, False, AIToolApprovalKind.MINDSPACE_FILE_CHANGE
                )
            )

        assert run() is True

    def test_journalled_file_change_asks_without_a_standing_approval(self) -> None:
        """Without a standing approval, a journalled change waits for the human to decide."""
        conversation = AIConversation()

        assert _answerable(conversation, AIToolApprovalKind.MINDSPACE_FILE_CHANGE, True) is True

    def test_standard_request_is_never_resolved_by_a_standing_approval(self) -> None:
        """A standing approval does not resolve a request the journal cannot reverse."""
        conversation = AIConversation()
        conversation.set_auto_approve_file_changes(True)

        assert _answerable(conversation, AIToolApprovalKind.STANDARD, False) is False

    def test_standard_request_is_approved_only_by_the_human(self) -> None:
        """A standard request is granted when the human grants it."""
        conversation = AIConversation()
        conversation.set_auto_approve_file_changes(True)

        assert _answerable(conversation, AIToolApprovalKind.STANDARD, True) is True

    def test_auto_approval_is_announced(self) -> None:
        """Resolving a request without asking announces it, so the decision is visible."""
        conversation = AIConversation()
        conversation.set_auto_approve_file_changes(True)
        announced: list[str] = []

        async def on_auto_approved(_message: Any, reason: str) -> None:
            announced.append(reason)

        conversation.register_callback(AIConversationEvent.TOOL_APPROVAL_AUTO_APPROVED, on_auto_approved)

        async def run() -> bool:
            return await conversation._request_tool_authorization(  # pylint: disable=protected-access
                "editor", {"operation": "save"}, "Save the file", None, True,
                AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            )

        assert asyncio.run(run()) is True
        assert announced == ["Save the file"]


class TestStandingApprovalLifetime:
    """Tests for how long a standing approval lasts."""

    def test_starts_off(self) -> None:
        """A conversation never starts with a standing approval in force."""
        assert AIConversation().auto_approve_file_changes() is False

    def test_cleared_by_cancelling_the_turn(self) -> None:
        """Cancelling the turn withdraws the standing approval."""
        conversation = AIConversation()
        conversation.set_auto_approve_file_changes(True)

        conversation.cancel_current_tasks(notify=False)

        assert conversation.auto_approve_file_changes() is False

    def test_cleared_when_the_user_sends_a_message(self) -> None:
        """Sending a message starts a new turn, which withdraws the standing approval."""
        conversation = AIConversation()
        conversation.set_auto_approve_file_changes(True)

        async def submit() -> None:
            await conversation.submit_message("User", "hello")
            conversation.cancel_current_tasks(notify=False)

        asyncio.run(submit())

        assert conversation.auto_approve_file_changes() is False
