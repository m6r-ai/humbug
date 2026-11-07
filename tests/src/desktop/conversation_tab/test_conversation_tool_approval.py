"""Tests for the tool approval UI, from the request to the buttons the user sees."""

import asyncio
import os
from typing import Any

import pytest

from ai import AIConversation, AIMessage, AIMessageSource
from ai_tool import AIToolApprovalKind, AIToolCall
from ai_transcript_conversation import AITranscriptConversation
from desktop.conversation_tab.conversation_message import ConversationMessage
from desktop.conversation_tab.conversation_widget import ConversationWidget
from mindspace.mindspace_message import MindspaceMessage

_TOOL_CALL = AIToolCall(id="call-1", name="filesystem", arguments={"operation": "write_file"})


def _make_widget(mindspace_sandbox) -> tuple[ConversationWidget, AIConversation]:
    """Return a conversation widget backed by a fresh transcript, and its conversation."""
    transcript_path = os.path.join(mindspace_sandbox.conversations_dir, "approval.conv")
    transcript = AITranscriptConversation(transcript_path)
    widget = ConversationWidget(transcript_path, ai_transcript_conversation=transcript)

    return widget, widget._ai_conversation.inner_conversation()  # pylint: disable=protected-access


def _show_tool_call_message(widget: ConversationWidget, conversation: AIConversation) -> AIMessage:
    """Render a tool-call message in the widget, as a pending tool call would be."""
    message = AIMessage.create(
        AIMessageSource.TOOL_CALL,
        "{}",
        tool_calls=[_TOOL_CALL],
        completed=True,
    )
    conversation.get_conversation_history().add_message(message)
    widget._load_message_history(  # pylint: disable=protected-access
        conversation.get_conversation_history().get_messages(),
        True
    )
    conversation._pending_tool_call_message = message  # pylint: disable=protected-access

    return message


def _message_widget(widget: ConversationWidget, message: AIMessage) -> ConversationMessage:
    """Return the widget displaying the given message."""
    for candidate in widget._ordered_widgets():  # pylint: disable=protected-access
        if candidate.message_id() == message.id:
            return candidate

    raise AssertionError(f"No widget for message {message.id}")


async def _wait_for(predicate: Any) -> None:
    """Wait until the predicate holds, giving up rather than hanging the suite."""
    for _ in range(200):
        if predicate():
            return

        await asyncio.sleep(0)

    raise AssertionError("The condition never became true")


def _request(
    conversation: AIConversation,
    kind: AIToolApprovalKind,
    reason: str = "This will create a file 'notes.txt'"
) -> Any:
    """Start an authorization request and return the task that resolves with the answer."""
    return asyncio.create_task(
        conversation._request_tool_authorization(  # pylint: disable=protected-access
            "filesystem", {"operation": "write_file"}, reason, None, False, kind
        )
    )


def _latest(mindspace_sandbox) -> MindspaceMessage:
    """Return the newest entry in the mindspace's interaction log."""
    return mindspace_sandbox.manager.interactions().newest(1)[0]


class TestApprovalUiIsShown:
    """Tests that a request reaching the widget produces an approval the user can answer."""

    def test_request_shows_the_approval_ui_on_the_tool_call_message(self, mindspace_sandbox) -> None:
        """A request for approval displays the buttons on the tool-call message."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)

        async def scenario() -> bool:
            task = _request(conversation, AIToolApprovalKind.MINDSPACE_FILE_CHANGE)
            await _wait_for(lambda: widget._pending_tool_call_approval is not None)  # pylint: disable=protected-access

            approved_widget = _message_widget(widget, message)
            assert approved_widget._approval_widget is not None  # pylint: disable=protected-access
            assert approved_widget._approval_approve_button is not None  # pylint: disable=protected-access
            assert approved_widget._approval_reject_button is not None  # pylint: disable=protected-access
            assert approved_widget._approval_i_am_unsure_button is not None  # pylint: disable=protected-access

            approved_widget._approval_approve_button.click()  # pylint: disable=protected-access

            return await task

        assert asyncio.run(scenario()) is True

    def test_approving_through_the_button_resolves_the_request(self, mindspace_sandbox) -> None:
        """Clicking Approve grants the request and removes the prompt."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)

        async def scenario() -> bool:
            task = _request(conversation, AIToolApprovalKind.MINDSPACE_FILE_CHANGE)
            await _wait_for(lambda: widget._pending_tool_call_approval is not None)  # pylint: disable=protected-access
            approved_widget = _message_widget(widget, message)
            assert approved_widget._approval_approve_button is not None  # pylint: disable=protected-access

            approved_widget._approval_approve_button.click()  # pylint: disable=protected-access
            result = await task

            assert approved_widget._approval_widget is None  # pylint: disable=protected-access
            assert widget._pending_tool_call_approval is None  # pylint: disable=protected-access

            return result

        assert asyncio.run(scenario()) is True

    def test_rejecting_through_the_button_denies_the_request(self, mindspace_sandbox) -> None:
        """Clicking Reject denies the request."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)

        async def scenario() -> bool:
            task = _request(conversation, AIToolApprovalKind.MINDSPACE_FILE_CHANGE)
            await _wait_for(lambda: widget._pending_tool_call_approval is not None)  # pylint: disable=protected-access
            approved_widget = _message_widget(widget, message)
            assert approved_widget._approval_reject_button is not None  # pylint: disable=protected-access

            approved_widget._approval_reject_button.click()  # pylint: disable=protected-access

            return await task

        assert asyncio.run(scenario()) is False

    def test_a_journalled_change_offers_the_standing_approval_switch(self, mindspace_sandbox) -> None:
        """The switch to approve file changes for the rest of the turn is offered here."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)

        async def scenario() -> None:
            task = _request(conversation, AIToolApprovalKind.MINDSPACE_FILE_CHANGE)
            await _wait_for(lambda: widget._pending_tool_call_approval is not None)  # pylint: disable=protected-access
            approved_widget = _message_widget(widget, message)

            assert approved_widget._approval_auto_approve_switch is not None  # pylint: disable=protected-access
            assert approved_widget._approval_auto_approve_label is not None  # pylint: disable=protected-access

            await conversation.reject_pending_tool_calls()
            await task

        asyncio.run(scenario())

    def test_a_standard_request_offers_no_standing_approval_switch(self, mindspace_sandbox) -> None:
        """A request the journal cannot reverse offers no switch, which would imply otherwise."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)

        async def scenario() -> None:
            task = _request(conversation, AIToolApprovalKind.STANDARD)
            await _wait_for(lambda: widget._pending_tool_call_approval is not None)  # pylint: disable=protected-access
            approved_widget = _message_widget(widget, message)

            assert approved_widget._approval_widget is not None  # pylint: disable=protected-access
            assert approved_widget._approval_auto_approve_switch is None  # pylint: disable=protected-access

            await conversation.reject_pending_tool_calls()
            await task

        asyncio.run(scenario())

    def test_switching_the_standing_approval_on_arms_the_conversation(self, mindspace_sandbox) -> None:
        """Turning the switch on arms the conversation and records the human's decision."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)

        async def scenario() -> None:
            task = _request(conversation, AIToolApprovalKind.MINDSPACE_FILE_CHANGE)
            await _wait_for(lambda: widget._pending_tool_call_approval is not None)  # pylint: disable=protected-access
            approved_widget = _message_widget(widget, message)
            assert approved_widget._approval_auto_approve_switch is not None  # pylint: disable=protected-access

            approved_widget._approval_auto_approve_switch.setChecked(True)  # pylint: disable=protected-access

            assert conversation.auto_approve_file_changes() is True
            assert _latest(mindspace_sandbox).content == (
                "Human enabled auto-approval of file changes for the rest of this turn"
            )

            await conversation.reject_pending_tool_calls()
            await task

        asyncio.run(scenario())


class TestStandingApprovalThroughTheWidget:
    """Tests for the widget's handling of a request a standing approval resolved."""

    def test_armed_conversation_shows_no_prompt_and_logs_the_decision(self, mindspace_sandbox) -> None:
        """A covered request is resolved silently, and the log says a standing approval did it."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)
        conversation.set_auto_approve_file_changes(True)

        async def scenario() -> bool:
            result = await _request(conversation, AIToolApprovalKind.MINDSPACE_FILE_CHANGE)
            await asyncio.sleep(0)

            assert widget._pending_tool_call_approval is None  # pylint: disable=protected-access
            assert _message_widget(widget, message)._approval_widget is None  # pylint: disable=protected-access

            return result

        assert asyncio.run(scenario()) is True
        assert _latest(mindspace_sandbox).content == (
            "Human's standing approval authorised tool call: This will create a file 'notes.txt'"
        )

    def test_armed_conversation_still_asks_for_a_standard_request(self, mindspace_sandbox) -> None:
        """A request outside the journal's scope still reaches the user while armed."""
        widget, conversation = _make_widget(mindspace_sandbox)
        message = _show_tool_call_message(widget, conversation)
        conversation.set_auto_approve_file_changes(True)

        async def scenario() -> bool:
            task = _request(conversation, AIToolApprovalKind.STANDARD)
            await _wait_for(lambda: widget._pending_tool_call_approval is not None)  # pylint: disable=protected-access

            approved_widget = _message_widget(widget, message)
            assert approved_widget._approval_widget is not None  # pylint: disable=protected-access
            assert approved_widget._approval_auto_approve_switch is None  # pylint: disable=protected-access

            await conversation.reject_pending_tool_calls()

            return await task

        assert asyncio.run(scenario()) is False


class TestWithdrawingTheStandingApproval:
    """Tests for the widget's handling of the switch being turned back off."""

    def test_turning_the_switch_off_withdraws_the_approval_and_logs_it(self, mindspace_sandbox) -> None:
        """Withdrawing the standing approval disarms the conversation and is recorded."""
        widget, conversation = _make_widget(mindspace_sandbox)
        conversation.set_auto_approve_file_changes(True)

        widget._on_auto_approve_toggled(False)  # pylint: disable=protected-access

        assert conversation.auto_approve_file_changes() is False
        assert _latest(mindspace_sandbox).content == "Human withdrew auto-approval of file changes"


class TestWithdrawingTheStandingApproval:
    """Tests for the widget's handling of the switch being turned back off."""

    def test_turning_the_switch_off_withdraws_the_approval_and_logs_it(self, mindspace_sandbox) -> None:
        """Withdrawing the standing approval disarms the conversation and is recorded."""
        widget, conversation = _make_widget(mindspace_sandbox)
        conversation.set_auto_approve_file_changes(True)

        widget._on_auto_approve_toggled(False)  # pylint: disable=protected-access

        assert conversation.auto_approve_file_changes() is False
        assert _latest(mindspace_sandbox).content == "Human withdrew auto-approval of file changes"


class TestApprovalMessageWidget:
    """Tests for the approval UI on a message widget, without a conversation behind it."""

    def test_showing_the_ui_stores_everything_a_restored_tab_needs(self, qapp) -> None:
        """The approval state survives the widget being rebuilt in another column."""
        message = ConversationMessage(AIMessageSource.TOOL_CALL, content="{}")

        message.show_tool_approval_ui(_TOOL_CALL, "A reason", "a diff", True, True)
        info = message.get_tool_approval_info()

        assert info is not None
        assert info["tool_call"] is _TOOL_CALL
        assert info["reason"] == "A reason"
        assert info["context"] == "a diff"
        assert info["destructive"] is True
        assert info["approvable"] is True

    def test_no_approval_ui_reports_no_state(self, qapp) -> None:
        """A message with no prompt reports nothing to restore."""
        message = ConversationMessage(AIMessageSource.TOOL_CALL, content="{}")

        assert message.get_tool_approval_info() is None

    def test_unsure_emits_its_signal_and_removes_the_ui(self, qapp) -> None:
        """Asking to discuss the request emits the signal and clears the prompt."""
        message = ConversationMessage(AIMessageSource.TOOL_CALL, content="{}")
        unsure: list[bool] = []
        message.tool_call_i_am_unsure.connect(lambda: unsure.append(True))
        message.show_tool_approval_ui(_TOOL_CALL, "A reason", None, False, True)
        assert message._approval_i_am_unsure_button is not None  # pylint: disable=protected-access

        message._approval_i_am_unsure_button.click()  # pylint: disable=protected-access

        assert unsure == [True]
        assert message._approval_widget is None  # pylint: disable=protected-access

    @pytest.mark.parametrize("approvable", [True, False])
    def test_removing_the_ui_clears_the_switch_and_the_stored_state(self, qapp, approvable: bool) -> None:
        """Removing the prompt leaves nothing behind, whichever kind of request it was."""
        message = ConversationMessage(AIMessageSource.TOOL_CALL, content="{}")
        message.show_tool_approval_ui(_TOOL_CALL, "A reason", None, False, approvable)

        message.remove_tool_approval_ui()

        assert message._approval_widget is None  # pylint: disable=protected-access
        assert message._approval_auto_approve_switch is None  # pylint: disable=protected-access
        assert message._approval_auto_approve_label is None  # pylint: disable=protected-access
        assert message.get_tool_approval_info() is None
