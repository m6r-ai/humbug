"""Tests for the Mission Control service: missions, membership and the decision inbox."""

import asyncio
import os

import pytest

from ai import AIConversation, AIConversationEvent, AIConversationParent, AIMessage, AIMessageSource
from ai_transcript_conversation import AITranscriptConversation
from ai_tool import AIToolApprovalKind
from mindspace.mindspace import Mindspace
from mission import mission_service as mission_service_module
from mission.mission_service import MAX_CONTINUES_WITHOUT_PROGRESS, MISSION_REQUESTER, MissionService
from mission.mission_snapshot import MissionActivityKind, MissionAgentState, MissionStatus
from mission.mission import AutoApproveLevel, MissionMode, MissionTaskStatus
from mission.mission_reviewer import ReviewRequest, ReviewVerdict
from mission.mission_store import MissionStore
from context.context_registry import ContextEvent


@pytest.fixture
def mindspace(tmp_path):
    """An open mindspace in a temporary directory."""
    ms = Mindspace(on_settings_changed=lambda: None, on_interactions_updated=lambda: None)
    ms.create_mindspace(str(tmp_path), [])
    ms.open_mindspace(str(tmp_path))
    yield ms
    ms.close_mindspace()


@pytest.fixture
def submitted(monkeypatch):
    """Record messages submitted to conversations instead of sending them to an AI."""
    calls: list[tuple[str | None, str]] = []

    async def fake_submit(_self, requester, user_message, attachment_guids=None):  # pylint: disable=unused-argument
        calls.append((requester, user_message))

    monkeypatch.setattr(AIConversation, "submit_message", fake_submit)
    return calls


def _lead(service: MissionService, mission) -> AITranscriptConversation:
    """Return the lead conversation Mission Control is driving for an autonomous mission."""
    tracked = service._lead_for(mission)  # pylint: disable=protected-access
    assert tracked is not None
    return tracked.transcript


def _open_conversation(mindspace: Mindspace, name: str, requester_id: str = "") -> tuple[str, AITranscriptConversation]:
    """Open a conversation context backed by a new transcript."""
    path = os.path.join(mindspace.conversations_dir(), f"{name}.conv")
    transcript = AITranscriptConversation(path)
    context_id = mindspace.contexts().open(
        context_type="conversation", path=path, title=name,
        initial_model=transcript, requester_id=requester_id,
    )
    return context_id, transcript


def _request_approval(transcript: AITranscriptConversation) -> asyncio.Task:
    """Start a tool authorization request on a conversation, as a tool would."""
    conversation = transcript.inner_conversation()
    return asyncio.create_task(conversation._request_tool_authorization(  # pylint: disable=protected-access
        "terminal", {"operation": "send_command", "command": "make docs"}, "Run make docs", "$ make docs", True,
        AIToolApprovalKind.STANDARD
    ))


def _plan_message(statuses: list[str]) -> AIMessage:
    tasks = ", ".join(
        f'{{"id": "{i}", "title": "Task {i}", "status": "{status}"}}' for i, status in enumerate(statuses, start=1)
    )
    content = f'Plan:\n```mission-plan\n{{"title": "Better title", "summary": "S", "tasks": [{tasks}]}}\n```'
    return AIMessage.create(source=AIMessageSource.AI, content=content, completed=True)


class TestDecisionInbox:
    """Approvals from any open conversation can be answered in one place, exactly once."""

    def test_pending_approval_appears_and_is_approved_once(self, mindspace):
        """An approval request is listed, approving it resolves the request, and it cannot be answered twice."""
        async def run() -> None:
            _context_id, transcript = _open_conversation(mindspace, "Chat")
            service = MissionService(mindspace)
            service.start()

            request = _request_approval(transcript)
            await asyncio.sleep(0)

            decisions = service.decisions()
            assert len(decisions) == 1
            assert decisions[0].tool_name == "terminal.send_command"
            assert decisions[0].reason == "Run make docs"
            assert decisions[0].detail == "$ make docs"
            assert decisions[0].destructive
            assert decisions[0].conversation_title == "Chat"
            assert decisions[0].mission_id is None

            assert await service.approve(decisions[0].decision_id)
            assert await request is True
            assert not service.decisions()
            assert not await service.approve(decisions[0].decision_id)

            service.stop()
            transcript.close()

        asyncio.run(run())

    def test_rejecting_denies_the_request(self, mindspace):
        """Rejecting a decision denies the tool call."""
        async def run() -> None:
            _context_id, transcript = _open_conversation(mindspace, "Chat")
            service = MissionService(mindspace)
            service.start()

            request = _request_approval(transcript)
            await asyncio.sleep(0)

            assert await service.reject(service.decisions()[0].decision_id)
            assert await request is False

            service.stop()
            transcript.close()

        asyncio.run(run())

    def test_decision_answered_in_its_tab_cannot_be_answered_again(self, mindspace):
        """Once the conversation's own tab answers a request, the inbox no longer lists or answers it."""
        async def run() -> None:
            _context_id, transcript = _open_conversation(mindspace, "Chat")
            service = MissionService(mindspace)
            service.start()

            request = _request_approval(transcript)
            await asyncio.sleep(0)
            stale_id = service.decisions()[0].decision_id

            await transcript.reject_pending_tool_calls("Rejected in the tab")
            assert await request is False

            assert not service.decisions()
            assert not await service.approve(stale_id)

            service.stop()
            transcript.close()

        asyncio.run(run())

    def test_request_waiting_before_the_service_starts_is_listed(self, mindspace):
        """A conversation already waiting for approval is picked up when the service starts."""
        async def run() -> None:
            _context_id, transcript = _open_conversation(mindspace, "Chat")
            request = _request_approval(transcript)
            await asyncio.sleep(0)

            service = MissionService(mindspace)
            service.start()

            decisions = service.decisions()
            assert len(decisions) == 1
            assert await service.approve(decisions[0].decision_id)
            assert await request is True

            service.stop()
            transcript.close()

        asyncio.run(run())

    def test_unsure_declines_and_asks_to_discuss(self, mindspace, submitted):
        """Being unsure declines the request and asks the conversation to talk it through."""
        async def run() -> None:
            _context_id, transcript = _open_conversation(mindspace, "Chat")
            service = MissionService(mindspace)
            service.start()

            request = _request_approval(transcript)
            await asyncio.sleep(0)

            assert await service.unsure(service.decisions()[0].decision_id)
            assert await request is False
            assert submitted == [(None, "I'm not sure about this tool request. Let's discuss.")]

            service.stop()
            transcript.close()

        asyncio.run(run())


class TestMissions:
    """Missions are recorded, led by a conversation, and follow the plan the lead reports."""

    def test_creating_a_mission_records_it_and_briefs_a_lead(self, mindspace, submitted):
        """A new mission is saved, its lead conversation is opened and briefed, and the start is logged."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()

            mission = await service.create_mission("  Build a mobile connector  ")

            assert mission.objective == "Build a mobile connector"
            assert mission.title == "Build a mobile connector"
            stored = MissionStore(os.path.join(mindspace.mindspace_path(), ".humbug", "missions")).load_all()
            assert [m.mission_id for m in stored] == [mission.mission_id]

            assert mission.mode == MissionMode.AUTONOMOUS
            assert not mindspace.contexts().list_all()

            assert len(submitted) == 1
            assert submitted[0][0] == MISSION_REQUESTER
            assert "Build a mobile connector" in submitted[0][1]
            assert "mission-plan" in submitted[0][1]

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert [a.is_lead for a in snapshot.agents] == [True]

            logged = [m.content for m in mindspace.interactions().newest(10)]
            assert any("Human started mission" in content for content in logged)

            service.stop()

        asyncio.run(run())

    def test_empty_objective_is_refused(self, mindspace):
        """A mission needs an objective."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()

            with pytest.raises(Exception, match="Describe what you want to achieve"):
                await service.create_mission("   ")

            assert not service.missions()
            service.stop()

        asyncio.run(run())

    def test_conversation_delegated_from_the_lead_joins_the_mission(self, mindspace, submitted):
        """A conversation the lead delegated to is shown as one of the mission's agents."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            delegation = AIMessage.create(source=AIMessageSource.AI, content="", completed=True)
            _lead(service, mission).get_conversation_history().add_message(delegation)

            child_path = os.path.join(mindspace.conversations_dir(), "dAI-child.conv")
            child = AITranscriptConversation(child_path)
            child.get_conversation_history().set_parent(
                AIConversationParent(message_id=delegation.id, tool_call_id="call-1")
            )
            mindspace.contexts().open(
                context_type="conversation", path=child_path, title="dAI-child", initial_model=child
            )
            _other_id, other = _open_conversation(mindspace, "Unrelated")
            await asyncio.sleep(0)

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert [(a.title, a.is_lead) for a in snapshot.agents][1:] == [("dAI-child", False)]
            assert len(submitted) == 1

            service.stop()
            child.close()
            other.close()

        asyncio.run(run())

    def test_plan_reported_by_the_lead_updates_the_mission(self, mindspace, submitted):
        """The lead's plan block becomes the mission's plan, is saved, and drives its status."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            lead = _lead(service, mission)
            conversation = lead.inner_conversation()

            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["completed", "pending"])
            )

            assert [t.title for t in mission.tasks] == ["Task 1", "Task 2"]
            assert mission.title == "Better title"
            assert service.status(mission.mission_id) == MissionStatus.IDLE
            stored = MissionStore(os.path.join(mindspace.mindspace_path(), ".humbug", "missions")).load_all()
            assert len(stored[0].tasks) == 2

            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["completed", "completed"])
            )
            assert service.status(mission.mission_id) == MissionStatus.COMPLETED
            assert len(submitted) == 1

            service.stop()

        asyncio.run(run())

    def test_snapshot_shows_the_leads_latest_words_without_the_plan(self, mindspace, submitted):
        """The lead's latest reply is offered without its plan block, skipping replies that are only a plan."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            lead = _lead(service, mission)
            history = lead.get_conversation_history()

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert snapshot.lead_message == ""
            assert snapshot.lead_message_at is None

            question = _plan_message(["blocked"])
            question.content = "Which folder name should I use?\n\n" + question.content.replace("Plan:\n", "")
            history.add_message(question)
            plan_only = _plan_message(["in_progress"])
            plan_only.content = plan_only.content.replace("Plan:\n", "")
            history.add_message(plan_only)

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert snapshot.lead_message == "Which folder name should I use?"
            assert snapshot.lead_message_at == question.timestamp
            assert len(submitted) == 1

            service.stop()

        asyncio.run(run())

    def test_stopped_mission_stays_stopped_until_guided(self, mindspace, submitted):
        """Stopping a mission is recorded, and sending guidance resumes it."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")

            service.stop_mission(mission.mission_id)
            assert service.status(mission.mission_id) == MissionStatus.STOPPED
            stored = MissionStore(os.path.join(mindspace.mindspace_path(), ".humbug", "missions")).load_all()
            assert stored[0].stopped

            await service.send_guidance(mission.mission_id, "Carry on")
            assert service.status(mission.mission_id) != MissionStatus.STOPPED
            assert submitted[-1] == (None, "Carry on")

            service.stop()

        asyncio.run(run())

    def test_removed_mission_keeps_its_conversation(self, mindspace, submitted):
        """Removing a mission deletes its record but not its lead conversation."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")

            service.remove_mission(mission.mission_id)

            assert not service.missions()
            assert os.path.exists(mindspace.get_absolute_path(mission.lead_path))
            assert len(submitted) == 1

            service.stop()

        asyncio.run(run())


@pytest.fixture
def no_continue_delay(monkeypatch):
    """Ask the lead to continue immediately rather than after a pause."""
    monkeypatch.setattr(mission_service_module, "AUTO_CONTINUE_DELAY_SECONDS", 0)


async def _start_mission_with_plan(mindspace, statuses: list[str]):
    """Start a mission, report a plan from its lead, and return the service, mission and lead conversation."""
    service = MissionService(mindspace)
    service.start()
    mission = await service.create_mission("Objective")
    conversation = _lead(service, mission).inner_conversation()
    await conversation._trigger_event(  # pylint: disable=protected-access
        AIConversationEvent.MESSAGE_COMPLETED, _plan_message(statuses)
    )
    return service, mission, conversation


async def _finish_turn(conversation: AIConversation) -> None:
    """End the lead's turn and let any automatic continue run."""
    await conversation._trigger_event(AIConversationEvent.COMPLETED)  # pylint: disable=protected-access
    for _ in range(5):
        await asyncio.sleep(0)


def _continues(submitted: list[tuple[str | None, str]]) -> int:
    return sum(1 for requester, text in submitted if requester == MISSION_REQUESTER and text.startswith("Continue"))


class TestAutomaticContinue:
    """The lead is asked to carry on by itself while there is unfinished, unblocked work."""

    def test_lead_is_asked_to_continue_while_steps_remain(self, mindspace, submitted, no_continue_delay):  # pylint: disable=unused-argument
        """After the lead's turn ends with unfinished steps, it is asked to continue."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["completed", "pending"])

            await _finish_turn(conversation)

            assert _continues(submitted) == 1
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert snapshot.activity[0].kind == MissionActivityKind.AUTO_CONTINUED
            service.stop()

        asyncio.run(run())

    def test_blocked_step_waits_for_the_user(self, mindspace, submitted, no_continue_delay):  # pylint: disable=unused-argument
        """A step the lead marked blocked leaves the mission waiting for the user."""
        async def run() -> None:
            service, _mission, conversation = await _start_mission_with_plan(mindspace, ["blocked", "pending"])

            await _finish_turn(conversation)

            assert _continues(submitted) == 0
            service.stop()

        asyncio.run(run())

    def test_finished_or_stopped_missions_are_not_continued(self, mindspace, submitted, no_continue_delay):  # pylint: disable=unused-argument
        """Nothing is sent once every step is done, or after the user stops the mission."""
        async def run() -> None:
            service, _mission, conversation = await _start_mission_with_plan(mindspace, ["completed", "completed"])
            await _finish_turn(conversation)
            assert _continues(submitted) == 0
            service.stop()

            service, mission, conversation = await _start_mission_with_plan(mindspace, ["pending"])
            service.stop_mission(mission.mission_id)
            await _finish_turn(conversation)
            assert _continues(submitted) == 0
            service.stop()

        asyncio.run(run())

    def test_continuing_pauses_when_no_step_is_finished(self, mindspace, submitted, no_continue_delay):  # pylint: disable=unused-argument
        """After repeated continues without progress the lead is left for the user, until a step completes."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["pending", "pending"])

            for _ in range(MAX_CONTINUES_WITHOUT_PROGRESS + 2):
                await _finish_turn(conversation)

            assert _continues(submitted) == MAX_CONTINUES_WITHOUT_PROGRESS
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert [a.kind for a in snapshot.activity].count(MissionActivityKind.AUTO_PAUSED) == 1

            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["completed", "pending"])
            )
            await _finish_turn(conversation)
            assert _continues(submitted) == MAX_CONTINUES_WITHOUT_PROGRESS + 1
            service.stop()

        asyncio.run(run())


def _store(mindspace) -> MissionStore:
    return MissionStore(os.path.join(mindspace.mindspace_path(), ".humbug", "missions"))


class TestInterruptedMissions:
    """A mission that was working when Humbug closed can be picked up where it stopped."""

    def test_mission_working_when_humbug_closed_is_interrupted_and_can_be_resumed(self, mindspace, submitted):
        """A mission still marked active when the service starts is interrupted, and resuming briefs the lead."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["completed", "pending"])
            assert _store(mindspace).load_all()[0].active
            service.stop()

            restarted = MissionService(mindspace)
            restarted.start()
            assert restarted.status(mission.mission_id) == MissionStatus.INTERRUPTED
            assert restarted.is_interrupted(mission.mission_id)

            await restarted.resume_mission(mission.mission_id)

            assert not restarted.is_interrupted(mission.mission_id)
            assert submitted[-1][0] == MISSION_REQUESTER
            assert "Humbug was closed while this mission was in progress" in submitted[-1][1]
            snapshot = restarted.snapshot(mission.mission_id)
            assert snapshot is not None
            assert snapshot.activity[0].kind == MissionActivityKind.RESUMED
            restarted.stop()

        asyncio.run(run())

    def test_mission_waiting_for_the_user_is_not_interrupted(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A mission whose lead finished its turn and is waiting for the user is not treated as interrupted."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["blocked", "pending"])
            await _finish_turn(conversation)
            assert not _store(mindspace).load_all()[0].active
            service.stop()

            restarted = MissionService(mindspace)
            restarted.start()
            assert restarted.status(mission.mission_id) != MissionStatus.INTERRUPTED
            with pytest.raises(Exception, match="not waiting to be resumed"):
                await restarted.resume_mission(mission.mission_id)

            restarted.stop()

        asyncio.run(run())

    def test_stopped_mission_is_not_interrupted(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A mission the user stopped stays stopped after a restart."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["pending"])
            service.stop_mission(mission.mission_id)
            service.stop()

            restarted = MissionService(mindspace)
            restarted.start()
            assert restarted.status(mission.mission_id) == MissionStatus.STOPPED
            restarted.stop()

        asyncio.run(run())

    def test_interrupted_missions_resume_automatically_when_chosen(self, mindspace, submitted):
        """With automatic resume chosen, an interrupted mission is resumed as soon as the service starts."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["pending"])
            assert not service.resume_automatically()
            service.set_resume_automatically(True)
            service.stop()
            briefed = len(submitted)

            restarted = MissionService(mindspace)
            restarted.start()
            for _ in range(5):
                await asyncio.sleep(0)

            assert restarted.resume_automatically()
            assert not restarted.is_interrupted(mission.mission_id)
            assert len(submitted) == briefed + 1
            assert "Humbug was closed" in submitted[-1][1]
            restarted.stop()

        asyncio.run(run())

    def test_interrupted_mission_waits_without_automatic_resume(self, mindspace, submitted):
        """Without automatic resume, nothing is sent until the user chooses to resume."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["pending"])
            service.stop()
            briefed = len(submitted)

            restarted = MissionService(mindspace)
            restarted.start()
            for _ in range(5):
                await asyncio.sleep(0)

            assert restarted.is_interrupted(mission.mission_id)
            assert len(submitted) == briefed
            restarted.stop()

        asyncio.run(run())


class TestMissionModes:
    """A mission is followed either in Mission Control or through its conversation tab, never both."""

    def test_switching_to_classic_hands_the_running_conversation_to_a_tab(self, mindspace, submitted, no_continue_delay):  # pylint: disable=unused-argument
        """The tab adopts the conversation Mission Control was driving, and Mission Control stops carrying it on."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["pending"])

            context_id = service.switch_to_classic(mission.mission_id)

            assert context_id is not None
            assert mindspace.contexts().get_model(context_id, AITranscriptConversation).inner_conversation() is conversation
            assert mission.mode == MissionMode.CLASSIC
            assert _store(mindspace).load_all()[0].mode == MissionMode.CLASSIC

            await _finish_turn(conversation)
            assert _continues(submitted) == 0
            service.stop()

        asyncio.run(run())

    def test_switching_to_autonomous_closes_the_tab_and_carries_on(self, mindspace, submitted, no_continue_delay):  # pylint: disable=unused-argument
        """The tab is closed, Mission Control loads the conversation and asks it to continue unfinished work."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["completed", "pending"])
            context_id = service.switch_to_classic(mission.mission_id)

            service.switch_to_autonomous(mission.mission_id)
            for _ in range(5):
                await asyncio.sleep(0)

            assert mindspace.contexts().get(context_id) is None
            assert mission.mode == MissionMode.AUTONOMOUS
            assert _lead(service, mission).path() == mindspace.get_absolute_path(mission.lead_path)
            assert _continues(submitted) == 1
            service.stop()

        asyncio.run(run())

    def test_busy_conversation_is_not_switched_to_autonomous(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A conversation in the middle of a reply keeps its tab."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["pending"])
            service.switch_to_classic(mission.mission_id)
            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.AI_CONNECTED, AIMessage.create(AIMessageSource.AI_CONNECTED, "", completed=False)
            )

            with pytest.raises(Exception, match="busy"):
                service.switch_to_autonomous(mission.mission_id)

            assert mission.mode == MissionMode.CLASSIC
            service.stop()

        asyncio.run(run())

    def test_opening_the_conversation_as_a_tab_moves_the_mission_to_classic(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Opening an autonomous mission's conversation from the sidebar makes the tab the one place it is followed."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["pending"])
            lead_path = mindspace.get_absolute_path(mission.lead_path)
            tab_transcript = AITranscriptConversation(lead_path)
            mindspace.contexts().open(
                context_type="conversation", path=lead_path, title="lead", initial_model=tab_transcript
            )
            for _ in range(5):
                await asyncio.sleep(0)

            assert mission.mode == MissionMode.CLASSIC
            assert service._lead_for(mission).transcript is tab_transcript  # pylint: disable=protected-access
            service.stop()
            tab_transcript.close()

        asyncio.run(run())


class TestStoppingTasks:
    """Any task can be stopped, and stays stopped."""

    def test_stopped_task_stays_stopped_and_the_lead_is_told(self, mindspace, submitted):
        """Stopping a task marks it stopped, tells the lead, and later plans cannot restart it."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["completed", "in_progress"])

            service.stop_task(mission.mission_id, "2")
            for _ in range(5):
                await asyncio.sleep(0)

            assert mission.tasks[1].status == MissionTaskStatus.STOPPED
            assert _store(mindspace).load_all()[0].stopped_tasks == ["2"]
            assert submitted[-1][0] == MISSION_REQUESTER
            assert "stopped task 2" in submitted[-1][1]

            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["completed", "in_progress"])
            )
            assert mission.tasks[1].status == MissionTaskStatus.STOPPED
            assert service.status(mission.mission_id) == MissionStatus.COMPLETED
            service.stop()

        asyncio.run(run())

    def test_stopping_an_unknown_task_is_refused(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Only tasks in the plan can be stopped."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["pending"])

            with pytest.raises(Exception, match="no such task"):
                service.stop_task(mission.mission_id, "99")

            service.stop()


    def test_stopping_a_task_whose_delegate_is_not_open_says_so(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Stopping a task whose delegate is not an open conversation still stops the task and says the delegate may still be running."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["in_progress"])
            content = (
                'Plan:\n```mission-plan\n{"title": "Better title", "summary": "S", '
                '"tasks": [{"id": "1", "title": "Task 1", "status": "in_progress", "agent": "ghost"}]}\n```'
            )
            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED,
                AIMessage.create(source=AIMessageSource.AI, content=content, completed=True),
            )
            assert mission.tasks[0].agent == "ghost"

            service.stop_task(mission.mission_id, "1")
            for _ in range(5):
                await asyncio.sleep(0)

            assert mission.tasks[0].status == MissionTaskStatus.STOPPED
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            stopped = [a for a in snapshot.activity if a.kind == MissionActivityKind.TASK_STOPPED]
            assert stopped
            assert "could not be found" in stopped[-1].detail
            service.stop()

        asyncio.run(run())


class TestTablessAudit:
    """A mission conversation without a tab is still recorded as a tab would record it."""

    def test_lead_without_a_tab_is_recorded_in_the_interaction_log(self, mindspace, submitted):  # pylint: disable=unused-argument
        """The lead's responses reach the interaction log, attributed to its conversation."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["pending"])
            reply = AIMessage.create(source=AIMessageSource.AI, content="Working on it now", completed=True)

            await conversation._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, reply)  # pylint: disable=protected-access

            entries = mindspace.interactions().newest(20)
            assert any(e.content == "AI response: 'Working on it now'" for e in entries)
            assert mission.mode == MissionMode.AUTONOMOUS
            service.stop()

        asyncio.run(run())


class _FakeReviewer:
    """Stands in for the AI reviewer, recording what it was asked and returning a fixed verdict."""

    def __init__(self, approve: bool, reason: str = "Fits the plan") -> None:
        self.requests: list[ReviewRequest] = []
        self._verdict = ReviewVerdict(approve, reason)

    async def review(self, request: ReviewRequest) -> ReviewVerdict:
        self.requests.append(request)
        return self._verdict


async def _mission_waiting_on_approval(
    mindspace, reviewer: _FakeReviewer, level: AutoApproveLevel, destructive: bool,
    kind: AIToolApprovalKind = AIToolApprovalKind.STANDARD
):
    """Start a mission at an auto-approve level whose lead then asks for approval."""
    service = MissionService(mindspace, reviewer=reviewer)  # type: ignore[arg-type]
    service.start()
    mission = await service.create_mission("Write the docs")
    service.set_auto_approve(mission.mission_id, level)
    conversation = _lead(service, mission).inner_conversation()
    # A journalled request is a file change; anything else is modelled as a terminal command.
    if kind == AIToolApprovalKind.MINDSPACE_FILE_CHANGE:
        tool, arguments, reason = "filesystem", {"operation": "write_file", "path": "docs/a.md"}, "Write docs/a.md"

    else:
        tool, arguments, reason = "terminal", {"operation": "send_command", "command": "make docs"}, "Run make docs"

    request = asyncio.create_task(conversation._request_tool_authorization(  # pylint: disable=protected-access
        tool, arguments, reason, "+ hello", destructive, kind
    ))
    for _ in range(5):
        await asyncio.sleep(0)

    return service, mission, request


class TestAutoApprove:
    """An AI reviewer approves a mission's requests within the chosen level, and leaves the rest for the user."""

    def test_safe_request_is_approved_by_the_reviewer(self, mindspace, submitted):  # pylint: disable=unused-argument
        """At Safe, a non-destructive request the reviewer approves goes ahead and is logged."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.SAFE, destructive=False
            )

            assert await request is True
            assert reviewer.requests[0].objective == "Write the docs"
            assert reviewer.requests[0].tool_name == "terminal.send_command"
            assert not service.decisions()
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert MissionActivityKind.AUTO_APPROVED in [a.kind for a in snapshot.activity]
            logged = [m.content for m in mindspace.interactions().newest(20)]
            assert any(c.startswith("AI reviewer approved tool call") for c in logged)
            service.stop()

        asyncio.run(run())

    def test_destructive_request_at_safe_comes_to_the_user(self, mindspace, submitted):  # pylint: disable=unused-argument
        """At Safe, a destructive request is not reviewed and waits for the user."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.SAFE, destructive=True
            )

            assert not reviewer.requests
            assert not request.done()
            assert service.status(mission.mission_id) == MissionStatus.NEEDS_YOU
            request.cancel()
            service.stop()

        asyncio.run(run())

    def test_everything_level_lets_the_reviewer_approve_destructive_requests(self, mindspace, submitted):  # pylint: disable=unused-argument
        """At Everything, even a destructive request is decided by the reviewer."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, _mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.ALL, destructive=True
            )

            assert await request is True
            assert reviewer.requests[0].destructive
            service.stop()

        asyncio.run(run())

    def test_request_the_reviewer_doubts_stays_with_the_user(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A request the reviewer will not approve stays in the inbox with the reviewer's reason."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=False, reason="Touches files outside the docs")
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.ALL, destructive=False
            )

            assert not request.done()
            decisions = service.decisions()
            assert decisions[0].review_note == "Touches files outside the docs"
            assert not decisions[0].reviewing
            assert service.status(mission.mission_id) == MissionStatus.NEEDS_YOU
            request.cancel()
            service.stop()

        asyncio.run(run())

    def test_auto_approve_is_off_by_default_and_in_classic_mode(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Nothing is reviewed when auto-approve is off, or when the mission is driven from its tab."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, _mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.OFF, destructive=False
            )
            assert not reviewer.requests
            request.cancel()
            service.stop()

            service = MissionService(mindspace, reviewer=reviewer)  # type: ignore[arg-type]
            service.start()
            mission = await service.create_mission("Other")
            service.switch_to_classic(mission.mission_id)
            service.set_auto_approve(mission.mission_id, AutoApproveLevel.ALL)
            conversation = service._lead_for(mission).conversation  # pylint: disable=protected-access
            request = asyncio.create_task(conversation._request_tool_authorization(  # pylint: disable=protected-access
                "filesystem", {"operation": "write_file"}, "Write", None, False,
                AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            ))
            for _ in range(5):
                await asyncio.sleep(0)

            assert not reviewer.requests
            request.cancel()
            service.stop()

        asyncio.run(run())

    def test_turning_auto_approve_on_reviews_waiting_requests(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Choosing a level reviews requests that were already waiting."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.OFF, destructive=False
            )
            assert not request.done()

            service.set_auto_approve(mission.mission_id, AutoApproveLevel.SAFE)
            for _ in range(5):
                await asyncio.sleep(0)

            assert await request is True
            assert _store(mindspace).load_all()[0].auto_approve == AutoApproveLevel.SAFE
            service.stop()

        asyncio.run(run())

    def test_reviewer_sees_what_the_user_asked_for_after_the_objective(self, mindspace, submitted):  # pylint: disable=unused-argument
        """The reviewer judges against the user's later messages and the lead's update, not only the first objective."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service = MissionService(mindspace, reviewer=reviewer)  # type: ignore[arg-type]
            service.start()
            mission = await service.create_mission("HI")
            lead = _lead(service, mission)
            history = lead.get_conversation_history()
            history.add_message(AIMessage.create(AIMessageSource.USER, "Create a login page in New/"))
            history.add_message(AIMessage.create(AIMessageSource.AI, "Writing New/login.html now.", completed=True))
            service.set_auto_approve(mission.mission_id, AutoApproveLevel.SAFE)

            request = asyncio.create_task(lead.inner_conversation()._request_tool_authorization(  # pylint: disable=protected-access
                "filesystem", {"operation": "write_file"}, "Write New/login.html", None, False,
                AIToolApprovalKind.STANDARD
            ))
            for _ in range(5):
                await asyncio.sleep(0)

            assert await request is True
            seen = reviewer.requests[0]
            assert seen.objective == "HI"
            assert seen.instructions == ["Create a login page in New/"]
            assert seen.lead_update == "Writing New/login.html now."
            service.stop()

        asyncio.run(run())


    def test_journalled_change_at_everything_uses_the_conversations_standing_approval(self, mindspace, submitted):  # pylint: disable=unused-argument
        """At Everything, a change the journal records is approved without a review, and arms the standing approval."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=False)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.ALL, destructive=True,
                kind=AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            )

            assert await asyncio.wait_for(request, timeout=1) is True
            assert not reviewer.requests
            conversation = _lead(service, mission).inner_conversation()
            assert conversation.auto_approve_file_changes()

            # The rest of the turn's journalled changes are approved by the conversation itself.
            again = await asyncio.wait_for(conversation._request_tool_authorization(  # pylint: disable=protected-access
                "filesystem", {"operation": "write_file", "path": "docs/b.md"}, "Write docs/b.md", None, True,
                AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            ), timeout=1)
            assert again is True
            assert not service.decisions()
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            kinds = [a.kind for a in snapshot.activity]
            assert kinds.count(MissionActivityKind.REVERSIBLE_APPROVED) == 2
            service.stop()

        asyncio.run(run())

    def test_safe_approves_a_journalled_change_without_a_standing_approval(self, mindspace, submitted):  # pylint: disable=unused-argument
        """At Safe, a non-destructive journalled change is approved on its own, and nothing is armed for later ones."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=False)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.SAFE, destructive=False,
                kind=AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            )

            assert await asyncio.wait_for(request, timeout=1) is True
            assert not reviewer.requests
            assert not _lead(service, mission).inner_conversation().auto_approve_file_changes()
            service.stop()

        asyncio.run(run())

    def test_destructive_journalled_change_at_safe_comes_to_the_user(self, mindspace, submitted):  # pylint: disable=unused-argument
        """At Safe, a destructive change waits for the user even when the journal records it."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.SAFE, destructive=True,
                kind=AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            )

            assert not request.done()
            assert not reviewer.requests
            assert service.decisions()
            assert not _lead(service, mission).inner_conversation().auto_approve_file_changes()
            request.cancel()
            service.stop()

        asyncio.run(run())

    def test_journalled_change_waits_for_the_user_when_auto_approve_is_off(self, mindspace, submitted):  # pylint: disable=unused-argument
        """With auto-approve off, a journalled change still comes to the user, labelled as journalled."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.OFF, destructive=False,
                kind=AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            )

            assert not request.done()
            assert service.decisions()[0].reversible
            assert not _lead(service, mission).inner_conversation().auto_approve_file_changes()
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            requested = [a for a in snapshot.activity if a.kind == MissionActivityKind.APPROVAL_REQUESTED]
            assert requested[0].detail == "filesystem.write_file (journalled)"
            assert requested[0].more == "Write docs/a.md\n\n+ hello"
            request.cancel()
            service.stop()

        asyncio.run(run())

    def test_other_requests_are_not_labelled_journalled(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A request the journal does not record is named by its tool alone."""
        async def run() -> None:
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, _FakeReviewer(approve=True), AutoApproveLevel.OFF, destructive=False
            )

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            requested = [a for a in snapshot.activity if a.kind == MissionActivityKind.APPROVAL_REQUESTED]
            assert requested[0].detail == "terminal.send_command"
            request.cancel()
            service.stop()

        asyncio.run(run())

    def test_lowering_the_level_from_everything_withdraws_the_standing_approval(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Leaving Everything withdraws a standing approval the mission granted."""
        async def run() -> None:
            for level in (AutoApproveLevel.SAFE, AutoApproveLevel.OFF):
                service, mission, request = await _mission_waiting_on_approval(
                    mindspace, _FakeReviewer(approve=True), AutoApproveLevel.ALL, destructive=False,
                    kind=AIToolApprovalKind.MINDSPACE_FILE_CHANGE
                )
                assert await asyncio.wait_for(request, timeout=1) is True
                conversation = _lead(service, mission).inner_conversation()
                assert conversation.auto_approve_file_changes()

                service.set_auto_approve(mission.mission_id, level)
                assert not conversation.auto_approve_file_changes()
                service.stop()

        asyncio.run(run())

    def test_raising_the_level_reviews_a_request_left_for_the_user(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A destructive request waiting at Safe is reviewed and approved as soon as the level becomes Everything."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=True)
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.SAFE, destructive=True
            )
            assert not reviewer.requests
            assert not request.done()

            service.set_auto_approve(mission.mission_id, AutoApproveLevel.ALL)
            for _ in range(5):
                await asyncio.sleep(0)

            assert await request is True
            assert reviewer.requests[0].destructive
            service.stop()

        asyncio.run(run())

    def test_changing_the_level_reviews_a_request_the_reviewer_left_again(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A request the reviewer left for the user is reviewed afresh when the level changes."""
        async def run() -> None:
            reviewer = _FakeReviewer(approve=False, reason="Not sure")
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, reviewer, AutoApproveLevel.SAFE, destructive=False
            )
            assert service.decisions()[0].review_note == "Not sure"

            reviewer._verdict = ReviewVerdict(True, "Fits the plan")  # pylint: disable=protected-access
            service.set_auto_approve(mission.mission_id, AutoApproveLevel.ALL)
            for _ in range(5):
                await asyncio.sleep(0)

            assert await request is True
            assert len(reviewer.requests) == 2
            service.stop()

        asyncio.run(run())

    def test_standing_approval_used_names_the_tool_it_approved(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A change the standing approval resolves is reported in the feed under the tool that asked for it."""
        async def run() -> None:
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, _FakeReviewer(approve=False), AutoApproveLevel.ALL, destructive=False,
                kind=AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            )
            assert await asyncio.wait_for(request, timeout=1) is True
            conversation = _lead(service, mission).inner_conversation()

            again = await asyncio.wait_for(conversation._request_tool_authorization(  # pylint: disable=protected-access
                "filesystem", {"operation": "write_file", "path": "docs/b.md"}, "Write docs/b.md", None, False,
                AIToolApprovalKind.MINDSPACE_FILE_CHANGE
            ), timeout=1)
            assert again is True
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            approved = [a for a in snapshot.activity if a.kind == MissionActivityKind.REVERSIBLE_APPROVED]
            assert approved[-1].detail == "filesystem.write_file"
            service.stop()

        asyncio.run(run())

    def test_standing_approval_used_ignores_a_conversation_outside_any_mission(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A standing approval resolving a change in a conversation no mission drives records nothing."""
        async def run() -> None:
            service = MissionService(mindspace, reviewer=_FakeReviewer(approve=True))  # type: ignore[arg-type]
            service.start()
            mission = await service.create_mission("Write the docs")
            context_id, _transcript = _open_conversation(mindspace, "outsider")
            for _ in range(5):
                await asyncio.sleep(0)

            service.handle_standing_approval_used(service._tracked[context_id], "Write docs/a.md")  # pylint: disable=protected-access

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert not [a for a in snapshot.activity if a.kind == MissionActivityKind.REVERSIBLE_APPROVED]
            service.stop()

        asyncio.run(run())


class TestPlanHistory:
    """A finished plan moves to the mission's history when new work starts."""

    def test_new_request_after_a_finished_plan_starts_an_empty_plan(self, mindspace, submitted):
        """Guidance sent to a finished mission archives its plan, so progress starts again from zero."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["completed", "completed"])
            assert service.status(mission.mission_id) == MissionStatus.COMPLETED

            await service.send_guidance(mission.mission_id, "Yes, go ahead with the suggestions")
            for _ in range(5):
                await asyncio.sleep(0)

            assert not mission.tasks
            assert mission.progress() == 0.0
            assert service.status(mission.mission_id) != MissionStatus.COMPLETED
            assert len(mission.history) == 1
            assert [t.status for t in mission.history[0].tasks] == [MissionTaskStatus.COMPLETED] * 2
            assert mission.history[0].finished_at
            assert "moved to this mission's history" in submitted[-1][1]
            assert "Yes, go ahead with the suggestions" in submitted[-1][1]
            stored = _store(mindspace).load_all()[0]
            assert len(stored.history) == 1 and not stored.tasks
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert MissionActivityKind.PLAN_ARCHIVED in [a.kind for a in snapshot.activity]
            service.stop()

        asyncio.run(run())

    def test_new_plan_after_a_finished_plan_replaces_it(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A lead that reports unfinished work after the plan was done starts a new plan; the old one is history."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["completed"])

            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["pending", "pending"])
            )

            assert [t.status for t in mission.tasks] == [MissionTaskStatus.PENDING] * 2
            assert len(mission.history) == 1
            assert len(mission.history[0].tasks) == 1
            service.stop()

        asyncio.run(run())

    def test_finished_plan_repeated_by_the_lead_stays_in_history(self, mindspace, submitted):  # pylint: disable=unused-argument
        """The lead repeating the archived plan does not bring it back as the current plan."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["completed"])
            await service.send_guidance(mission.mission_id, "Now do the next thing")
            for _ in range(5):
                await asyncio.sleep(0)

            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["completed"])
            )

            assert not mission.tasks
            assert len(mission.history) == 1
            service.stop()

        asyncio.run(run())

    def test_finished_plan_reported_again_is_not_archived(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Without new work, a finished plan stays the current plan."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["completed"])

            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["completed"])
            )

            assert len(mission.tasks) == 1
            assert not mission.history
            service.stop()

        asyncio.run(run())


class TestRenamingMissions:
    """A mission keeps the name the user gives it."""

    def test_renamed_mission_keeps_its_name_through_plan_updates(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A user-chosen name is saved and is not replaced by the title in the lead's next plan."""
        async def run() -> None:
            service, mission, conversation = await _start_mission_with_plan(mindspace, ["pending"])
            assert mission.title == "Better title"

            service.rename_mission(mission.mission_id, "  Release   notes  ")

            assert mission.title == "Release notes"
            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.MESSAGE_COMPLETED, _plan_message(["completed"])
            )
            assert mission.title == "Release notes"
            stored = _store(mindspace).load_all()[0]
            assert (stored.title, stored.title_locked) == ("Release notes", True)
            service.stop()

        asyncio.run(run())

    def test_empty_name_is_refused(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A mission cannot be given an empty name."""
        async def run() -> None:
            service, mission, _conversation = await _start_mission_with_plan(mindspace, ["pending"])

            with pytest.raises(Exception, match="needs a name"):
                service.rename_mission(mission.mission_id, "   ")

            assert mission.title == "Better title"
            service.stop()

        asyncio.run(run())


class TestAgentHierarchy:
    """Each agent knows which conversation delegated to it."""

    def test_delegated_conversation_names_its_delegator(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A helper's parent is the conversation whose message delegated to it; the lead has none."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            delegation = AIMessage.create(source=AIMessageSource.AI, content="", completed=True)
            _lead(service, mission).get_conversation_history().add_message(delegation)

            child_path = os.path.join(mindspace.conversations_dir(), "dAI-helper.conv")
            child = AITranscriptConversation(child_path)
            child.get_conversation_history().set_parent(
                AIConversationParent(message_id=delegation.id, tool_call_id="call-1")
            )
            child_id = mindspace.contexts().open(
                context_type="conversation", path=child_path, title="dAI-helper", initial_model=child
            )
            await asyncio.sleep(0)

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            lead_agent, helper = snapshot.agents
            assert lead_agent.is_lead and lead_agent.parent_id == ""
            assert helper.context_id == child_id
            assert helper.parent_id == lead_agent.context_id
            service.stop()
            child.close()

        asyncio.run(run())


class TestBackgroundWork:
    """An autonomous mission's work happens out of the user's way, and waits can be cut short."""

    def test_conversations_an_autonomous_lead_delegates_open_in_the_background(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A helper delegated by an autonomous lead is background work; an unrelated conversation is not."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            lead = _lead(service, mission)
            delegation = AIMessage.create(source=AIMessageSource.AI, content="", completed=True)
            lead.get_conversation_history().add_message(delegation)

            path = os.path.join(mindspace.conversations_dir(), "dAI-helper.conv")
            helper = AITranscriptConversation(path)
            helper.get_conversation_history().set_parent(
                AIConversationParent(message_id=delegation.id, tool_call_id="call")
            )
            registry = mindspace.contexts()
            decided: list[bool] = []
            registry.register_callback(
                ContextEvent.OPENED,
                lambda info, _ephemeral, requester: decided.append(service.runs_in_background(info, requester))
            )
            registry.open(context_type="conversation", path=path, title="dAI-helper", initial_model=helper)
            _open_conversation(mindspace, "unrelated")

            assert decided == [True, False]

            service.switch_to_classic(mission.mission_id)
            info = registry.get_by_path_and_type(path, "conversation")
            assert info is not None
            assert not service.runs_in_background(info, "")
            service.stop()

        asyncio.run(run())

    def test_a_wait_is_shown_and_can_be_finished_early(self, mindspace, submitted):  # pylint: disable=unused-argument
        """A clock wait appears on the agent and finishing it interrupts the wait."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            conversation = _lead(service, mission).inner_conversation()
            await conversation._trigger_event(  # pylint: disable=protected-access
                AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_STARTED, None, 120.0
            )

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            lead = snapshot.agents[0]
            assert lead.wait_seconds == 120.0
            assert lead.wait_started_at is not None

            interrupted: list[bool] = []

            async def fake_interrupt() -> None:
                interrupted.append(True)

            conversation.interrupt_pending_wait = fake_interrupt  # type: ignore[method-assign]
            assert await service.finish_wait(lead.context_id) is True
            assert interrupted == [True]

            await conversation._trigger_event(AIConversationEvent.TOOL_INTERRUPTIBLE_WAIT_ENDED)  # pylint: disable=protected-access
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert snapshot.agents[0].wait_started_at is None
            assert await service.finish_wait(lead.context_id) is False
            service.stop()

        asyncio.run(run())

    def test_conversation_is_shown_beside_mission_control(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Showing a conversation from Mission Control moves it out of Mission Control's column."""
        service = MissionService(mindspace)
        service.start()
        registry = mindspace.contexts()
        control = registry.open(context_type="mission", title="Mission Control")
        context_id, _transcript = _open_conversation(mindspace, "helper")
        focused: list[str] = []
        registry.register_callback(ContextEvent.FOCUSED, focused.append)

        service.show_beside(context_id, control)

        assert registry.get(context_id).column == registry.get(control).column + 1
        assert focused == [context_id]
        service.stop()

    def test_activity_shows_what_the_user_asked(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Starting a mission and guiding it record the user's own words."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Write the release notes")
            await service.send_guidance(mission.mission_id, "Keep them short")

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            details = {entry.kind: entry.detail for entry in snapshot.activity}
            assert details[MissionActivityKind.MISSION_STARTED] == "Write the release notes"
            assert details[MissionActivityKind.GUIDANCE_SENT] == "Keep them short"
            service.stop()

        asyncio.run(run())


class TestAgentProgress:
    """What the feed and the agents show follows what the user and the agents actually do."""

    def test_agent_is_working_once_its_approval_is_answered(self, mindspace, submitted):  # pylint: disable=unused-argument
        """An approved tool runs, so the agent is working, not still waiting for the user, while it does."""
        async def run() -> None:
            service, mission, request = await _mission_waiting_on_approval(
                mindspace, _FakeReviewer(approve=False), AutoApproveLevel.OFF, destructive=False
            )
            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert snapshot.agents[0].state == MissionAgentState.WAITING_FOR_APPROVAL

            assert await service.approve(service.decisions()[0].decision_id)
            assert await asyncio.wait_for(request, timeout=1) is True

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            assert snapshot.agents[0].state == MissionAgentState.WORKING
            service.stop()

        asyncio.run(run())

    def test_message_typed_into_a_mission_conversation_is_in_the_feed(self, mindspace, submitted):  # pylint: disable=unused-argument
        """What the user types into a mission's conversation is recorded once, with the full text to open."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            conversation = _lead(service, mission).inner_conversation()
            text = "Please also add a changelog entry. " * 20
            for source in (AIMessageSource.USER_QUEUED, AIMessageSource.USER):
                message = AIMessage.create(source, text)
                await conversation._trigger_event(  # pylint: disable=protected-access
                    AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, message
                )

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            typed = [a for a in snapshot.activity if a.kind == MissionActivityKind.USER_MESSAGE]
            assert len(typed) == 1
            assert typed[0].more == text.strip()
            service.stop()

        asyncio.run(run())

    def test_messages_sent_for_the_user_and_by_agents_are_not_repeated(self, mindspace, submitted):  # pylint: disable=unused-argument
        """Guidance sent from Mission Control and prompts from other agents are not recorded as typed input."""
        async def run() -> None:
            service = MissionService(mindspace)
            service.start()
            mission = await service.create_mission("Objective")
            await service.send_guidance(mission.mission_id, "Use British spelling")
            conversation = _lead(service, mission).inner_conversation()
            for message in (
                AIMessage.create(AIMessageSource.USER, "Use British spelling"),
                AIMessage.create(AIMessageSource.USER, "Do part two", user_name="Parent AI"),
            ):
                await conversation._trigger_event(  # pylint: disable=protected-access
                    AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, message
                )

            snapshot = service.snapshot(mission.mission_id)
            assert snapshot is not None
            kinds = [a.kind for a in snapshot.activity]
            assert MissionActivityKind.USER_MESSAGE not in kinds
            guidance = next(a for a in snapshot.activity if a.kind == MissionActivityKind.GUIDANCE_SENT)
            assert guidance.more == "Use British spelling"
            service.stop()

        asyncio.run(run())
