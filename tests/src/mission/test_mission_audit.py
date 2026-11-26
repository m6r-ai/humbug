"""Tests for the audit trail of mission conversations that have no tab."""

import asyncio
import os

import pytest

from ai import AIMessage, AIMessageSource, AIConversationEvent
from ai_tool import AIToolCall
from ai_transcript_conversation import AITranscriptConversation
from mindspace.mindspace import Mindspace
from mission.mission_audit import MissionConversationAuditor


@pytest.fixture
def mindspace(tmp_path):
    """An open mindspace in a temporary directory."""
    ms = Mindspace(on_settings_changed=lambda: None, on_interactions_updated=lambda: None)
    ms.create_mindspace(str(tmp_path), [])
    ms.open_mindspace(str(tmp_path))
    yield ms
    ms.close_mindspace()


class TestMissionConversationAuditor:
    """Turns of a conversation without a tab are logged as a conversation tab would log them."""

    def test_tool_calls_are_logged_with_their_operations(self, mindspace) -> None:
        """A response that only calls tools names each call, with its operation when it has one."""
        async def run() -> None:
            transcript = AITranscriptConversation(os.path.join(mindspace.conversations_dir(), "lead.conv"))
            auditor = MissionConversationAuditor(mindspace, transcript)
            message = AIMessage.create(source=AIMessageSource.AI, content="", completed=True)
            message.tool_calls = [
                AIToolCall(id="t1", name="filesystem", arguments={"operation": "read_file", "path": "a.md"}),
                AIToolCall(id="t2", name="clock", arguments={}),
            ]
            await transcript.inner_conversation()._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, message
            )

            logged = [entry.content for entry in mindspace.interactions().newest(5)]
            assert "AI requested tool calls: filesystem.read_file, clock" in logged
            auditor.detach()

        asyncio.run(run())

    def test_a_prompt_with_attachments_is_logged_as_a_tab_logs_it(self, mindspace) -> None:
        """A human prompt records how many files were attached, exactly as the conversation tab records it."""
        async def run() -> None:
            transcript = AITranscriptConversation(os.path.join(mindspace.conversations_dir(), "lead.conv"))
            auditor = MissionConversationAuditor(mindspace, transcript)
            message = AIMessage.create(
                AIMessageSource.USER, "Summarise this", attachments=["a-guid", "b-guid"], completed=True
            )
            await transcript.inner_conversation()._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, message
            )

            logged = [entry.content for entry in mindspace.interactions().newest(5)]
            assert "Human submitted prompt: 'Summarise this'\nattachments: 2" in logged
            auditor.detach()

        asyncio.run(run())

    def test_detaching_leaves_a_tab_usage_callback_in_place(self, mindspace) -> None:
        """An auditor detached after a tab took the conversation's usage callback leaves the tab's callback set."""
        transcript = AITranscriptConversation(os.path.join(mindspace.conversations_dir(), "lead.conv"))
        auditor = MissionConversationAuditor(mindspace, transcript)
        history = transcript.get_conversation_history()

        def tab_callback() -> None:
            pass

        history.set_on_usage_changed(tab_callback)
        auditor.detach()
        assert history.on_usage_changed() is tab_callback

        auditor = MissionConversationAuditor(mindspace, transcript)
        auditor.detach()
        assert history.on_usage_changed() is None
