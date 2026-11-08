"""Tests for the Mission Control tab and its widget."""

import asyncio
import dataclasses
from datetime import datetime, timezone
import types
from unittest import mock

import pytest

from PySide6.QtCore import QPoint, QPointF, QPropertyAnimation, QRectF, Qt
from PySide6.QtGui import QImage, QPainterPath, QRegion, QWheelEvent
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication, QFrame, QPushButton, QWidget

from ai import AIConversation, AIConversationEvent, AIMessage, AIMessageSource
from ai_tool import AIToolApprovalKind, AIToolCall
from context.context_registry import ContextEvent
from mission import AutoApproveLevel, MissionAgent, MissionAgentState, MissionDecision, MissionMode, MissionStatus
from PySide6.QtWidgets import QLabel

from desktop.mission_tab import mission_tab as mission_tab_module
from desktop.mission_tab import mission_widget as mission_widget_module
from desktop.mission_tab import mission_scene as mission_scene_module
from desktop.mission_tab.mission_display import (
    MissionActivityView, MissionAgentView, MissionChipView, MissionDecisionView, MissionHeaderView, MissionPhaseView, MissionStatTarget,
    MissionStatView, MissionStepState, MissionStepView, MissionTone, MissionView
)
from desktop.message_box import MessageBoxButton
from desktop.mission_tab.mission_mode_menus import MissionModeMenus
from desktop.mission_tab.mission_service_holder import MissionServiceHolder
from desktop.mission_tab.mission_network import AgentNetworkView
from desktop.mission_tab.mission_scene import ConfettiOverlay, MissionJourney, MissionOrb
from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager
from desktop.mission_tab.mission_tab import MissionTab, agent_names
from desktop.mission_tab.mission_widget import MissionWidget, submit_key_text


def _decision(destructive: bool = False) -> MissionDecisionView:
    return MissionDecisionView(
        decision_id="d1",
        context_id="c1",
        tool_name="filesystem.write_file",
        source_text="From Chat",
        time_text="10:00",
        reason="Write a file",
        detail="+ hello",
        destructive=destructive,
        reviewing=False,
        review_note="",
        auto_note="",
    )


@pytest.fixture
def submitted(monkeypatch):
    """Record messages submitted to conversations instead of sending them to an AI."""
    calls: list[str] = []

    async def fake_submit(_self, _requester, user_message, attachment_guids=None):  # pylint: disable=unused-argument
        calls.append(user_message)

    monkeypatch.setattr(AIConversation, "submit_message", fake_submit)
    return calls


class TestMissionWidget:
    """The widget reports what the user does and holds no state of its own."""

    def test_decision_buttons_report_the_decision(self, qapp) -> None:  # pylint: disable=unused-argument
        """Approve, Unsure and Reject each report the decision they belong to."""
        widget = MissionWidget()
        events: list[tuple[str, str]] = []
        widget.decision_approved.connect(lambda d: events.append(("approve", d)))
        widget.decision_unsure.connect(lambda d: events.append(("unsure", d)))
        widget.decision_rejected.connect(lambda d: events.append(("reject", d)))

        widget.set_decisions([_decision()])
        widget.findChildren(QPushButton, "MissionApprove")[0].click()
        unsure, reject = widget.findChildren(QPushButton, "MissionSecondaryButton")
        unsure.click()
        reject.click()

        assert events == [("approve", "d1"), ("unsure", "d1"), ("reject", "d1")]
        widget.deleteLater()

    def test_destructive_decision_uses_the_destructive_approve_button(self, qapp) -> None:  # pylint: disable=unused-argument
        """A destructive request is approved with the destructive-styled button."""
        widget = MissionWidget()

        widget.set_decisions([_decision(destructive=True)])

        assert widget.findChildren(QPushButton, "MissionApproveDestructive")
        assert not widget.findChildren(QPushButton, "MissionApprove")
        widget.deleteLater()

    def test_enter_adds_a_line_and_ctrl_enter_sends(self, qapp) -> None:  # pylint: disable=unused-argument
        """As in a conversation, Enter starts a new line and Ctrl+Enter (Cmd+Enter on macOS) sends."""
        widget = MissionWidget()
        sent: list[str] = []
        widget.submit_requested.connect(sent.append)
        composer = widget._input  # pylint: disable=protected-access

        QTest.keyClicks(composer, "first line")
        QTest.keyClick(composer, Qt.Key.Key_Return)
        QTest.keyClicks(composer, "second line")
        assert not sent
        assert composer.toPlainText() == "first line\nsecond line"

        QTest.keyClick(composer, Qt.Key.Key_Return, Qt.KeyboardModifier.ControlModifier)
        assert sent == ["first line\nsecond line"]
        assert composer.toPlainText() == ""
        widget.deleteLater()

    def test_composer_hint_names_the_send_key(self, qapp) -> None:  # pylint: disable=unused-argument
        """The composer says which key sends, as a conversation's input does."""
        widget = MissionWidget()

        assert submit_key_text() in widget._input.placeholderText()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_composer_submits_trimmed_text_and_clears(self, qapp) -> None:  # pylint: disable=unused-argument
        """Sending reports the text without surrounding space and empties the composer."""
        widget = MissionWidget()
        sent: list[str] = []
        widget.submit_requested.connect(sent.append)

        widget._input.setPlainText("  Ship the feature  ")  # pylint: disable=protected-access
        widget._send_button.click()  # pylint: disable=protected-access

        assert sent == ["Ship the feature"]
        assert widget._input.toPlainText() == ""  # pylint: disable=protected-access
        assert not widget._send_button.isEnabled()  # pylint: disable=protected-access
        widget.deleteLater()


def _step(
    task_id: str, state: MissionStepState, progress: str = "", agent_context_id: str = "", can_stop: bool = False
) -> MissionStepView:
    return MissionStepView(
        task_id=task_id, number=task_id, title=f"Step {task_id}", detail="", state=state,
        status_text=state.name, tone=MissionTone.NEUTRAL, progress=progress, evidence="",
        agent_title="dAI-helper" if agent_context_id else "", agent_context_id=agent_context_id,
        can_stop=can_stop,
    )


def _stats() -> list[MissionStatView]:
    return [
        MissionStatView("≡", "3", "Tasks", MissionTone.ACCENT, MissionStatTarget.ALL_STEPS),
        MissionStatView("✓", "1", "Completed", MissionTone.SUCCESS, MissionStatTarget.DONE_STEPS),
        MissionStatView("▶", "1", "In progress", MissionTone.INFO, MissionStatTarget.ACTIVE_STEPS),
    ]


def _shown_titles(widget: MissionWidget) -> list[str]:
    return [label.text() for label in widget.findChildren(QLabel, "MissionStepTitle") if label.isVisibleTo(widget._plan_card)]


class TestPlanInteraction:
    """Statistics filter the plan, and opening a step shows its progress."""

    def test_clicking_a_statistic_filters_the_plan_and_clicking_again_clears_it(self, qapp) -> None:  # pylint: disable=unused-argument
        """Choosing Completed shows only completed steps; choosing it again shows them all."""
        widget = MissionWidget()
        widget._set_stats(_stats())  # pylint: disable=protected-access
        widget._set_steps([  # pylint: disable=protected-access
            _step("1", MissionStepState.DONE), _step("2", MissionStepState.ACTIVE), _step("3", MissionStepState.PENDING)
        ])
        assert _shown_titles(widget) == ["Step 1", "Step 2", "Step 3"]

        widget._stat_cards[1].clicked.emit()  # pylint: disable=protected-access
        assert _shown_titles(widget) == ["Step 1"]
        assert not widget._filter_chip.isHidden()  # pylint: disable=protected-access

        widget._stat_cards[1].clicked.emit()  # pylint: disable=protected-access
        assert _shown_titles(widget) == ["Step 1", "Step 2", "Step 3"]
        assert widget._filter_chip.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_opening_a_step_shows_its_progress_and_its_agent(self, qapp) -> None:  # pylint: disable=unused-argument
        """An opened step shows the lead's progress note and can open the conversation working on it."""
        widget = MissionWidget()
        requested: list[str] = []
        widget.conversation_requested.connect(requested.append)
        widget._set_steps([_step("1", MissionStepState.ACTIVE, "Half the tests pass", "ctx-1")])  # pylint: disable=protected-access

        texts = [
            label.text() for label in widget.findChildren(QLabel, "MissionStepProgressText") if label.isVisibleTo(widget._plan_card)
        ]
        assert "Half the tests pass" not in texts

        widget._toggle_step("1")  # pylint: disable=protected-access
        texts = [
            label.text() for label in widget.findChildren(QLabel, "MissionStepProgressText") if label.isVisibleTo(widget._plan_card)
        ]
        assert "Half the tests pass" in texts
        [b for b in widget.findChildren(QPushButton, "MissionGhostButton") if b.isVisibleTo(widget._plan_card)][-1].click()
        assert requested == ["ctx-1"]
        widget.deleteLater()


class TestPlanHistory:
    """Plans finished earlier are kept in a history the user can open."""

    def test_history_button_appears_only_with_earlier_plans(self, qapp) -> None:  # pylint: disable=unused-argument
        """The History button counts earlier plans and is hidden when there are none."""
        widget = MissionWidget()
        widget._set_history([])  # pylint: disable=protected-access
        assert widget._history_button.isHidden()  # pylint: disable=protected-access

        phase = MissionPhaseView("Login page", "", "2026-11-06 10:00", "2 of 2 steps done", [])
        widget._set_history([phase])  # pylint: disable=protected-access
        assert not widget._history_button.isHidden()  # pylint: disable=protected-access
        assert "History (1)" in widget._history_button.text()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_opening_an_earlier_plan_shows_its_steps(self, qapp) -> None:  # pylint: disable=unused-argument
        """Opening the history lists earlier plans; opening one lists its steps."""
        widget = MissionWidget()
        phase = MissionPhaseView(
            "Login page", "", "2026-11-06 10:00", "1 of 1 steps done", [_step("1", MissionStepState.DONE)]
        )
        widget._set_history([phase])  # pylint: disable=protected-access
        assert widget._history_panel.isHidden()  # pylint: disable=protected-access

        widget._history_button.click()  # pylint: disable=protected-access
        assert not widget._history_panel.isHidden()  # pylint: disable=protected-access
        titles = [label.text() for label in widget._history_panel.findChildren(QLabel, "MissionStepTitle")]  # pylint: disable=protected-access
        assert titles == ["Login page"]
        texts = [label.text() for label in widget._history_panel.findChildren(QLabel, "MissionStepProgressText")]  # pylint: disable=protected-access
        assert "Step 1" not in texts

        widget._toggle_phase(widget._phase_key(phase))  # pylint: disable=protected-access
        texts = [label.text() for label in widget._history_panel.findChildren(QLabel, "MissionStepProgressText")]  # pylint: disable=protected-access
        assert "Step 1" in texts
        widget.deleteLater()

    def test_empty_plan_after_history_says_new_work_is_being_planned(self, qapp) -> None:  # pylint: disable=unused-argument
        """With earlier plans and no current one, the plan explains that a new plan is coming."""
        widget = MissionWidget()
        widget._set_history([MissionPhaseView("Old", "", "", "1 of 1 steps done", [])])  # pylint: disable=protected-access
        widget._set_steps([])  # pylint: disable=protected-access
        assert "Earlier plans are in History" in widget._plan_empty.text()  # pylint: disable=protected-access
        widget.deleteLater()


class TestStopTaskButton:
    """A step that can be stopped offers a button that reports which task to stop."""

    def test_stop_button_reports_the_task(self, qapp) -> None:  # pylint: disable=unused-argument
        """Opening a stoppable step shows Stop task, which reports the step's task."""
        widget = MissionWidget()
        stopped: list[str] = []
        widget.task_stop_requested.connect(stopped.append)
        widget._set_steps([_step("4", MissionStepState.ACTIVE, can_stop=True)])  # pylint: disable=protected-access
        widget._toggle_step("4")  # pylint: disable=protected-access

        buttons = [
            b for b in widget.findChildren(QPushButton, "MissionStopTaskButton") if b.isVisibleTo(widget._plan_card)  # pylint: disable=protected-access
        ]
        buttons[0].click()

        assert stopped == ["4"]
        widget.deleteLater()

    def test_finished_step_cannot_be_stopped(self, qapp) -> None:  # pylint: disable=unused-argument
        """A step that cannot be stopped has no Stop task button."""
        widget = MissionWidget()
        widget._set_steps([_step("1", MissionStepState.DONE)])  # pylint: disable=protected-access
        widget._toggle_step("1")  # pylint: disable=protected-access

        buttons = [
            b for b in widget.findChildren(QPushButton, "MissionStopTaskButton") if b.isVisibleTo(widget._plan_card)  # pylint: disable=protected-access
        ]
        assert not buttons
        widget.deleteLater()


class TestLeadMessage:
    """The lead's latest message is shown with a clear place to reply."""

    def test_waiting_message_invites_a_reply(self, qapp) -> None:  # pylint: disable=unused-argument
        """A message the lead is waiting on is shown, and the composer asks for a reply."""
        widget = MissionWidget()

        widget._set_lead_message("Which folder?", "10:30", True)  # pylint: disable=protected-access
        widget._has_mission = True  # pylint: disable=protected-access
        widget._update_composer_target("Mission")  # pylint: disable=protected-access

        assert not widget._lead_card.isHidden()  # pylint: disable=protected-access
        assert not widget._lead_pill.isHidden()  # pylint: disable=protected-access
        assert widget._input.placeholderText().startswith("Reply to the mission lead…")  # pylint: disable=protected-access
        widget.deleteLater()

    def test_reply_is_offered_only_when_the_lead_is_waiting(self, qapp) -> None:  # pylint: disable=unused-argument
        """A progress update shows no Reply button; a question the lead is waiting on does."""
        widget = MissionWidget()

        widget._set_lead_message("Halfway through the tests.", "10:30", False)  # pylint: disable=protected-access
        assert widget._reply_button.isHidden()  # pylint: disable=protected-access

        widget._set_lead_message("Which folder should I use?", "10:31", True)  # pylint: disable=protected-access
        assert not widget._reply_button.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_auto_approve_choice_is_reported(self, qapp) -> None:  # pylint: disable=unused-argument
        """Choosing an auto-approve level reports it for the owner to apply."""
        widget = MissionWidget()
        chosen: list[str] = []
        widget.auto_approve_selected.connect(chosen.append)

        widget._auto_approve_buttons["safe"].click()  # pylint: disable=protected-access

        assert chosen == ["safe"]
        widget.deleteLater()

    def test_no_message_hides_the_card(self, qapp) -> None:  # pylint: disable=unused-argument
        """Without a message from the lead there is no card."""
        widget = MissionWidget()

        widget._set_lead_message("", "", False)  # pylint: disable=protected-access

        assert widget._lead_card.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()


class TestMissionTab:
    """The tab shows the welcome until a mission exists, then shows the mission."""

    def test_new_mission_is_started_from_the_composer_and_shown(self, mindspace_sandbox, submitted) -> None:
        """Submitting an objective starts a mission, briefs its lead and shows the mission."""
        async def run() -> None:
            tab = MissionTab("mission-tab")
            tab.attach_service()
            widget = tab._widget  # pylint: disable=protected-access
            assert not widget._hero.isHidden()  # pylint: disable=protected-access
            assert widget._dashboard.isHidden()  # pylint: disable=protected-access

            widget.submit_requested.emit("Write release notes")
            for _ in range(5):
                await asyncio.sleep(0)

            tab._refresh()  # pylint: disable=protected-access
            assert not tab.can_submit()
            assert widget._hero.isHidden()  # pylint: disable=protected-access
            assert not widget._dashboard.isHidden()  # pylint: disable=protected-access
            assert widget._title.text() == "Write release notes"  # pylint: disable=protected-access
            assert len(submitted) == 1
            assert "Write release notes" in submitted[0]

            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())
        assert not mindspace_sandbox.manager.mindspace().contexts().list_all()

    def test_switching_to_classic_opens_the_conversation_beside_the_tab(self, mindspace_sandbox, submitted) -> None:
        """A new mission has no conversation tab; switching it to classic opens one beside Mission Control."""
        async def run() -> None:
            opened: list[str] = []
            contexts = mindspace_sandbox.manager.mindspace().contexts()
            contexts.register_callback(
                ContextEvent.OPENED, lambda _info, _ephemeral, requester_id: opened.append(requester_id)
            )
            tab = MissionTab("mission-tab")
            tab.attach_service()
            tab._widget.submit_requested.emit("Tidy the docs")  # pylint: disable=protected-access
            for _ in range(5):
                await asyncio.sleep(0)

            assert not opened
            tab._widget.switch_to_classic_requested.emit()  # pylint: disable=protected-access
            assert opened == ["mission-tab"]
            tab._refresh()  # pylint: disable=protected-access
            assert not tab._widget._classic_card.isHidden()  # pylint: disable=protected-access
            assert tab._widget._lower_container.isHidden()  # pylint: disable=protected-access
            assert len(submitted) == 1
            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())

    def test_closing_the_tab_keeps_following_missions(self, mindspace_sandbox) -> None:  # pylint: disable=unused-argument
        """The service outlives the tab, so a reopened tab sees the same missions and activity."""
        tab = MissionTab("mission-tab")
        tab.attach_service()
        first = tab._service  # pylint: disable=protected-access
        tab.close_tab()
        tab.deleteLater()

        again = MissionTab("mission-tab-2")
        again.attach_service()
        assert again._service is first  # pylint: disable=protected-access
        again.close_tab()
        again.deleteLater()
        MissionServiceHolder().release()


class TestModeMenus:
    """Mission conversations offer to switch to the mode they are not in."""

    def test_entries_follow_the_mission_mode(self, mindspace_sandbox, submitted) -> None:  # pylint: disable=unused-argument
        """The sidebar offers Classic for an autonomous mission, and the tab offers Autonomous once it is classic."""
        async def run() -> None:
            tab = MissionTab("mission-tab")
            tab.attach_service()
            tab._widget.submit_requested.emit("Tidy the docs")  # pylint: disable=protected-access
            for _ in range(5):
                await asyncio.sleep(0)

            service = MissionServiceHolder().current_service()
            assert service is not None
            mission = service.missions()[0]
            lead_path = mindspace_sandbox.manager.mindspace().get_absolute_path(mission.lead_path)
            shown: list[str] = []
            menus = MissionModeMenus(tab, shown.append)

            entries = menus.file_entries(lead_path)
            assert [label for label, _ in entries] == ["Switch to Classic mode"]
            assert not menus.file_entries(lead_path + ".other")

            entries[0][1]()
            assert mission.mode == MissionMode.CLASSIC
            assert [label for label, _ in menus.file_entries(lead_path)] == ["Switch to Autonomous mode"]

            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())


class TestMissionChipMenu:
    """Each mission chip has an options button that renames or deletes the mission."""

    def test_every_mission_chip_has_an_options_button(self, qapp) -> None:  # pylint: disable=unused-argument
        """A chip is followed by its own options button, which is named for screen readers."""
        widget = MissionWidget()
        widget.set_missions([
            MissionChipView("m1", "First", MissionTone.ACCENT, True),
            MissionChipView("m2", "Second", MissionTone.NEUTRAL, False),
        ], False)

        buttons = widget.findChildren(QPushButton, "MissionChipMenu")
        assert len(buttons) == 2
        assert all(b.accessibleName() == "Mission options" for b in buttons)
        widget.deleteLater()

    def test_rename_and_delete_from_the_tab(self, mindspace_sandbox, submitted, monkeypatch) -> None:  # pylint: disable=unused-argument
        """Renaming asks for a name and keeps it; deleting asks first and removes the mission."""
        async def run() -> None:
            tab = MissionTab("mission-tab")
            tab.attach_service()
            tab._widget.submit_requested.emit("Tidy the docs")  # pylint: disable=protected-access
            for _ in range(5):
                await asyncio.sleep(0)

            service = MissionServiceHolder().current_service()
            assert service is not None
            mission = service.missions()[0]

            monkeypatch.setattr(mission_tab_module.QInputDialog, "getText", lambda *_a, **_k: ("Docs cleanup", True))
            tab._widget.mission_rename_requested.emit(mission.mission_id)  # pylint: disable=protected-access
            assert mission.title == "Docs cleanup"

            monkeypatch.setattr(mission_tab_module.MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.NO)
            tab._widget.mission_delete_requested.emit(mission.mission_id)  # pylint: disable=protected-access
            assert service.missions()

            monkeypatch.setattr(mission_tab_module.MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.YES)
            tab._widget.mission_delete_requested.emit(mission.mission_id)  # pylint: disable=protected-access
            assert not service.missions()

            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())


def _agent_view(context_id: str, is_lead: bool = False, parent_id: str = "", activity: str = "idle") -> MissionAgentView:
    return MissionAgentView(
        context_id=context_id, title=context_id, role_text="role", is_lead=is_lead, state_text=activity,
        tone=MissionTone.NEUTRAL, busy=activity in ("working", "thinking"), clickable=not is_lead,
        name=context_id.upper(), parent_id=parent_id, activity=activity,
    )


class TestAgentNames:
    """Agents get short, stable, distinct names."""

    def test_names_are_stable_and_distinct(self) -> None:
        """The same agents always get the same names, and no two share one."""
        agents = [
            MissionAgent("lead", "Mission-1", True, MissionAgentState.IDLE, ""),
            MissionAgent("a", "dAI-a", False, MissionAgentState.WORKING, "", "lead"),
            MissionAgent("b", "dAI-b", False, MissionAgentState.THINKING, "", "lead"),
        ]

        first = agent_names("mission-1", agents)
        second = agent_names("mission-1", agents)

        assert first == second
        assert len(set(first.values())) == 3
        assert all(not name.startswith(("Mission-", "dAI-")) for name in first.values())


class TestAgentNetwork:
    """The network places each agent below the one that delegated to it, and opens agents on click."""

    def test_agents_are_laid_out_by_who_delegated_to_whom(self, qapp) -> None:  # pylint: disable=unused-argument
        """You are on top, the lead below, its delegates below it, and theirs below them."""
        network = AgentNetworkView()
        network.resize(800, 600)
        network.set_agents([
            _agent_view("lead", is_lead=True, activity="thinking"),
            _agent_view("a", parent_id="lead", activity="working"),
            _agent_view("b", parent_id="lead", activity="waiting"),
            _agent_view("c", parent_id="a"),
        ], "You")

        levels = {node.node_id: node.level for node in network._nodes}  # pylint: disable=protected-access
        assert levels == {"__user__": 0, "lead": 1, "a": 2, "b": 2, "c": 3}
        assert network.sizeHint().height() > 3 * 100
        network.deleteLater()

    def test_clicking_an_agent_opens_it(self, qapp) -> None:  # pylint: disable=unused-argument
        """Clicking a delegate's robot asks for its conversation; the user's own node does nothing."""
        network = AgentNetworkView()
        network.resize(800, 600)
        clicked: list[str] = []
        network.agent_clicked.connect(clicked.append)
        network.set_agents([_agent_view("lead", is_lead=True), _agent_view("a", parent_id="lead")], "You")
        nodes = {node.node_id: node for node in network._nodes}  # pylint: disable=protected-access

        QTest.mouseClick(network, Qt.MouseButton.LeftButton, pos=nodes["__user__"].center.toPoint())
        QTest.mouseClick(network, Qt.MouseButton.LeftButton, pos=nodes["a"].center.toPoint())

        assert clicked == ["a"]
        network.deleteLater()


class TestTalkingAgents:
    """Agents say what they are doing, and a waiting agent can be answered from its speech bubble."""

    def test_waiting_agent_offers_approve_and_reject_once_it_has_spoken(self, qapp) -> None:  # pylint: disable=unused-argument
        """The buttons appear when the remark is typed out, and answering reports the decision once."""
        network = AgentNetworkView()
        network.resize(800, 600)
        approved: list[str] = []
        network.decision_approved.connect(approved.append)
        waiting = dataclasses.replace(
            _agent_view("a", parent_id="lead", activity="waiting"),
            speech="I need your approval: Run the tests", decision_id="decision-1",
        )
        network.set_agents([_agent_view("lead", is_lead=True), waiting], "You", "Approve", "Reject")
        _decision_id, (approve, _unsure, reject, _more) = network._buttons["a"]  # pylint: disable=protected-access
        assert approve.isHidden() and reject.isHidden()

        network._phase += 10  # pylint: disable=protected-access
        network._place_buttons()  # pylint: disable=protected-access
        assert not approve.isHidden() and not reject.isHidden()
        assert approve.text() == "Approve"

        approve.click()
        assert approved == ["decision-1"]
        assert not approve.isEnabled() and not reject.isEnabled()

        network.set_agents([_agent_view("lead", is_lead=True), _agent_view("a", parent_id="lead")], "You")
        assert not network._buttons  # pylint: disable=protected-access
        network.deleteLater()

    def test_a_repeated_remark_is_not_typed_out_again(self, qapp) -> None:  # pylint: disable=unused-argument
        """Only a new remark starts typing afresh."""
        network = AgentNetworkView()
        network.resize(800, 600)
        lead = dataclasses.replace(_agent_view("lead", is_lead=True, activity="thinking"), speech="Thinking…")
        network.set_agents([lead], "You")
        network._phase += 5  # pylint: disable=protected-access
        network.set_agents([lead], "You")
        assert not network._typing("lead")  # pylint: disable=protected-access

        network.set_agents([dataclasses.replace(lead, speech="Writing a.md for you…")], "You")
        assert network._typing("lead")  # pylint: disable=protected-access
        network.deleteLater()

    def test_agents_say_what_they_are_doing(self, mindspace_sandbox, submitted) -> None:  # pylint: disable=unused-argument
        """The lead says which file it is writing, and asks for approval with buttons when it needs it."""
        async def run() -> None:
            tab = MissionTab("mission-tab")
            tab.attach_service()
            tab._widget.submit_requested.emit("Write the docs")  # pylint: disable=protected-access
            for _ in range(5):
                await asyncio.sleep(0)

            service = MissionServiceHolder().current_service()
            assert service is not None
            mission = service.missions()[0]
            conversation = service._lead_for(mission).conversation  # pylint: disable=protected-access
            call = AIMessage.create(source=AIMessageSource.TOOL_CALL, content="", completed=False)
            call.tool_calls = [AIToolCall(id="t", name="filesystem", arguments={
                "operation": "write_file", "path": "docs/guide/intro.md"
            })]
            await conversation._trigger_event(AIConversationEvent.MESSAGE_ADDED, call)  # pylint: disable=protected-access
            tab._refresh()  # pylint: disable=protected-access
            lead = tab._widget._agents[0]  # pylint: disable=protected-access
            assert lead.speech == "Writing intro.md for you…"
            assert not lead.decision_id

            request = asyncio.create_task(conversation._request_tool_authorization(  # pylint: disable=protected-access
                "terminal", {"operation": "send_command"}, "Run the tests", None, True,
                AIToolApprovalKind.STANDARD
            ))
            for _ in range(5):
                await asyncio.sleep(0)

            tab._refresh()  # pylint: disable=protected-access
            lead = tab._widget._agents[0]  # pylint: disable=protected-access
            assert lead.speech == "I need your approval: Run the tests"
            assert lead.decision_id == service.decisions()[0].decision_id
            assert lead.decision_destructive

            request.cancel()
            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())

    def test_waiting_agent_can_be_told_to_finish_waiting(self, qapp) -> None:  # pylint: disable=unused-argument
        """An agent in a clock wait offers Finish waiting, which reports the agent once."""
        network = AgentNetworkView()
        network.resize(800, 600)
        finished: list[str] = []
        network.wait_finish_requested.connect(finished.append)
        sleeping = dataclasses.replace(
            _agent_view("lead", is_lead=True, activity="working"),
            speech="Taking a planned pause of 2m.", wait_seconds=120.0, wait_started=1000.0,
        )
        network.set_agents([sleeping], "You", "Approve", "Reject", "Finish waiting", "{0} left")
        network._phase += 10  # pylint: disable=protected-access
        network._place_buttons()  # pylint: disable=protected-access
        _key, (finish,) = network._buttons["lead"]  # pylint: disable=protected-access
        assert finish.text() == "Finish waiting"
        assert not finish.isHidden()

        finish.click()
        assert finished == ["lead"]
        assert not finish.isEnabled()
        network.deleteLater()

    def test_network_height_does_not_change_as_agents_talk(self, qapp) -> None:  # pylint: disable=unused-argument
        """Every agent has the same space for its card, so speech never makes the view jump."""
        network = AgentNetworkView()
        network.resize(800, 600)
        quiet = _agent_view("lead", is_lead=True)
        network.set_agents([quiet], "You")
        height = network.sizeHint().height()

        network.set_agents([dataclasses.replace(quiet, speech="A much longer remark " * 20)], "You")
        assert network.sizeHint().height() == height
        network.deleteLater()


class TestDecisionReasons:
    """Long approval requests are summarised on the card, with the rest kept in the detail."""

    def test_long_reason_is_split_into_a_summary_and_the_rest(self) -> None:
        """The first paragraph is the summary; later paragraphs go with the detail."""
        summary, rest = mission_widget_module._split_reason(  # pylint: disable=protected-access
            "The AI wants to delegate a task.\n\nTask prompt:\nDo the thing"
        )
        assert summary == "The AI wants to delegate a task."
        assert rest == "Task prompt:\nDo the thing"

        summary, rest = mission_widget_module._split_reason("x" * 500)  # pylint: disable=protected-access
        assert len(summary) <= 220 and summary.endswith("…")
        assert rest == "x" * 500


def _view(completed: bool, stage: int, lead_message: str = "All done: the docs are in docs/.") -> MissionView:
    """Return a minimal mission view at a stage of its journey."""
    header = MissionHeaderView(
        title="Write the docs", summary="", status_text="Done" if completed else "Running",
        tone=MissionTone.SUCCESS if completed else MissionTone.ACCENT, progress=1.0 if completed else 0.5,
        progress_text="", can_stop=not completed, can_resume=False, is_classic=False, live=not completed,
        auto_approve="off", stage=stage, stage_tone=MissionTone.SUCCESS if completed else MissionTone.ACCENT
    )
    return MissionView(
        header=header, stats=_stats(), steps=[], agents=[], agents_badge="", activity=[],
        lead_message=lead_message, lead_message_time="10:30", awaiting_reply=False,
        completed=completed, report_text="Steps done: 2 of 2  ·  Agents on the team: 1"
    )


class TestMissionJourney:
    """The mission shows its way from the user's request to the AI's final report."""

    def test_stage_follows_the_mission_status(self) -> None:
        """Planning, working and done map to the journey's stages."""
        stage = mission_tab_module._stage  # pylint: disable=protected-access
        assert stage(MissionStatus.PLANNING, has_plan=False) == 1
        assert stage(MissionStatus.RUNNING, has_plan=False) == 1
        assert stage(MissionStatus.RUNNING, has_plan=True) == 2
        assert stage(MissionStatus.NEEDS_YOU, has_plan=True) == 2
        assert stage(MissionStatus.COMPLETED, has_plan=True) == 3

    def test_journey_has_room_for_the_current_stage_glow(self, qapp) -> None:  # pylint: disable=unused-argument
        """The glow and orbit round the current stage are never cut off at the top."""
        journey = MissionJourney()
        journey.set_labels(["You asked", "AI plans it", "AI team works", "Job done"])
        bead = journey._bead()  # pylint: disable=protected-access
        reach = bead * mission_scene_module._CURRENT_SCALE * mission_scene_module._GLOW_REACH  # pylint: disable=protected-access
        assert journey._track_y() >= reach  # pylint: disable=protected-access
        assert journey.sizeHint().height() > journey._track_y() + bead  # pylint: disable=protected-access
        journey.deleteLater()

    def test_journey_shows_the_current_stage(self, qapp) -> None:  # pylint: disable=unused-argument
        """The journey bar is given the stage from the view."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))

        assert widget._journey.stage() == 2  # pylint: disable=protected-access
        assert widget._report_card.isHidden()  # pylint: disable=protected-access
        assert not widget._lead_card.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_finished_mission_shows_the_final_report(self, qapp) -> None:  # pylint: disable=unused-argument
        """Once done, the lead's last message is shown as the final report, and the moment is celebrated."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        widget.set_mission(_view(completed=True, stage=3))

        assert not widget._report_card.isHidden()  # pylint: disable=protected-access
        assert widget._lead_card.isHidden()  # pylint: disable=protected-access
        assert "docs/" in widget._report_body.text()  # pylint: disable=protected-access
        assert widget._report_stats.text().startswith("Steps done: 2 of 2")  # pylint: disable=protected-access
        assert widget._confetti.is_celebrating()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_opening_a_finished_mission_does_not_celebrate_again(self, qapp) -> None:  # pylint: disable=unused-argument
        """Selecting a mission that finished earlier shows the report without the confetti."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=True, stage=3))

        assert not widget._report_card.isHidden()  # pylint: disable=protected-access
        assert not widget._confetti.is_celebrating()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_follow_up_and_new_mission_are_offered(self, qapp) -> None:  # pylint: disable=unused-argument
        """The report offers a new mission, which the widget reports for its owner to start."""
        widget = MissionWidget()
        requested: list[bool] = []
        widget.new_mission_requested.connect(lambda: requested.append(True))
        widget.set_mission(_view(completed=True, stage=3))

        widget._report_new_button.click()  # pylint: disable=protected-access

        assert requested == [True]
        widget.deleteLater()

    def test_activity_feed_uses_its_set_height(self, qapp) -> None:  # pylint: disable=unused-argument
        """The activity feed is given its own height, scaled with the zoom, whatever it holds."""
        widget = MissionWidget()
        widget.apply_style()

        expected = round(mission_widget_module._ACTIVITY_HEIGHT * StyleManager().zoom_factor())  # pylint: disable=protected-access
        assert widget._activity_scroll.height() == expected  # pylint: disable=protected-access
        widget.deleteLater()


class TestMissionScene:
    """The painted scenes keep their state and stop animating when they are done."""

    def test_confetti_falls_and_then_stops(self, qapp, monkeypatch) -> None:  # pylint: disable=unused-argument
        """A burst is in the air for a while, then it is over and the layer is put away."""
        clock = [100.0]
        monkeypatch.setattr(mission_scene_module.time, "monotonic", lambda: clock[0])
        page = QWidget()
        page.resize(400, 300)
        overlay = ConfettiOverlay(page)

        overlay.burst(QPointF(200, 150))
        assert overlay.is_celebrating()
        assert overlay.geometry() == page.rect()

        clock[0] += mission_scene_module._CONFETTI_SECONDS + 0.1  # pylint: disable=protected-access
        assert not overlay.is_celebrating()
        overlay._tick()  # pylint: disable=protected-access
        assert overlay.isHidden()
        page.deleteLater()

    def test_orb_sizes_itself_to_its_diameter(self, qapp) -> None:  # pylint: disable=unused-argument
        """The orb asks for a square as wide as the diameter it was given, scaled with the zoom."""
        orb = MissionOrb("✦", MissionTone.ACCENT, 120)

        expected = round(120 * StyleManager().zoom_factor())
        assert orb.sizeHint().width() == expected
        assert orb.sizeHint().height() == expected
        assert orb.minimumSizeHint() == orb.sizeHint()
        orb.deleteLater()

    def test_orb_animates_only_while_shown(self, qapp) -> None:  # pylint: disable=unused-argument
        """The orb's timer runs while it is visible and stops when it is hidden."""
        orb = MissionOrb("✦", MissionTone.ACCENT, 120)
        orb.show()

        assert orb._timer.isActive()  # pylint: disable=protected-access
        orb.hide()
        assert not orb._timer.isActive()  # pylint: disable=protected-access
        orb.deleteLater()

    def test_journey_reports_its_stage(self, qapp) -> None:  # pylint: disable=unused-argument
        """The journey keeps the stage it was given."""
        journey = MissionJourney()
        journey.set_labels(["You asked", "AI plans it", "AI team works", "Job done"])

        journey.set_stage(2, MissionTone.ACCENT, live=True)

        assert journey.stage() == 2
        journey.deleteLater()


class TestAgentFollowUp:
    """Waiting agents lead to the full request, and the feed opens to show what happened."""

    def test_read_more_reports_the_decision(self, qapp) -> None:  # pylint: disable=unused-argument
        """Read more, in a waiting agent's bubble, asks to show that agent's request."""
        network = AgentNetworkView()
        network.resize(800, 600)
        shown: list[str] = []
        network.decision_details_requested.connect(shown.append)
        waiting = dataclasses.replace(
            _agent_view("a", parent_id="lead", activity="waiting"),
            speech="I need your approval: Run the tests", decision_id="decision-1",
        )
        network.set_agents([_agent_view("lead", is_lead=True), waiting], "You", "Approve", "Reject", more_label="Read more")
        _decision_id, (_approve, _unsure, _reject, more) = network._buttons["a"]  # pylint: disable=protected-access

        assert more.text() == "Read more"
        more.click()
        assert shown == ["decision-1"]
        network.deleteLater()

    def test_unsure_is_offered_beside_approve_and_reject(self, qapp) -> None:  # pylint: disable=unused-argument
        """A waiting agent's bubble offers Unsure too, and it reports the decision once."""
        network = AgentNetworkView()
        network.resize(800, 600)
        unsure_answers: list[str] = []
        network.decision_unsure.connect(unsure_answers.append)
        waiting = dataclasses.replace(
            _agent_view("a", parent_id="lead", activity="waiting"),
            speech="I need your approval: Run the tests", decision_id="decision-1",
        )
        network.set_agents(
            [_agent_view("lead", is_lead=True), waiting], "You", "Approve", "Reject",
            more_label="Read more", unsure_label="Unsure"
        )
        _decision_id, buttons = network._buttons["a"]  # pylint: disable=protected-access
        assert [button.text() for button in buttons] == ["Approve", "Unsure", "Reject", "Read more"]

        buttons[1].click()
        assert unsure_answers == ["decision-1"]
        assert not any(button.isEnabled() for button in buttons[:3])
        network.deleteLater()

    def test_show_decision_highlights_its_card(self, qapp) -> None:  # pylint: disable=unused-argument
        """Being taken to a decision highlights its card in the inbox until the highlight times out."""
        widget = MissionWidget()
        widget.set_decisions([_decision()])

        widget.show_decision("d1")
        card = widget._decision_cards["d1"]  # pylint: disable=protected-access
        assert card.objectName() == "MissionDecisionCardHighlight"

        widget._clear_highlight()  # pylint: disable=protected-access
        assert card.objectName() == "MissionDecisionCard"
        widget.deleteLater()

    def test_activity_entry_opens_to_show_the_whole_story(self, qapp) -> None:  # pylint: disable=unused-argument
        """An entry with more to tell opens on click and closes on a second click."""
        widget = MissionWidget()
        full = "Please also add a changelog entry, and keep it short."
        widget._set_activity([  # pylint: disable=protected-access
            MissionActivityView("✎", "You: “Please also add…”", "10:00:00", MissionTone.ACCENT, more=full),
            MissionActivityView("▶", "Juno started working", "10:00:01", MissionTone.INFO),
        ])

        def rows() -> list[QWidget]:
            layout = widget._activity_layout  # pylint: disable=protected-access
            return [layout.itemAt(index).widget() for index in range(layout.count())]

        def details() -> list[str]:
            return [label.text() for row in rows() for label in row.findChildren(QLabel, "MissionActivityMore")]

        assert [row.objectName() for row in rows()] == ["MissionActivityRow", ""]
        assert not details()

        QTest.mouseClick(rows()[0], Qt.MouseButton.LeftButton)
        assert rows()[0].objectName() == "MissionActivityRowOpen"
        assert len(details()) == 1 and "changelog" in details()[0]

        QTest.mouseClick(rows()[0], Qt.MouseButton.LeftButton)
        assert not details()
        widget.deleteLater()

    def test_send_button_is_an_icon(self, qapp) -> None:  # pylint: disable=unused-argument
        """The send button shows a send icon, and keeps its name for tooltips and screen readers."""
        widget = MissionWidget()
        widget.apply_style()

        assert not widget._send_button.text()  # pylint: disable=protected-access
        assert not widget._send_button.icon().isNull()  # pylint: disable=protected-access
        assert widget._send_button.accessibleName() == "Send"  # pylint: disable=protected-access
        widget.deleteLater()

    def test_agent_rows_that_cannot_open_are_not_tab_stops(self, qapp) -> None:  # pylint: disable=unused-argument
        """Only agents whose conversation can be shown take keyboard focus."""
        widget = MissionWidget()
        widget._set_agents([_agent_view("lead", is_lead=True), _agent_view("a", parent_id="lead")], "")  # pylint: disable=protected-access

        policies = [row.focusPolicy() for row in widget.findChildren(QFrame, "MissionAgentRow")]

        assert sorted(policies, key=lambda p: p.value) == [Qt.FocusPolicy.NoFocus, Qt.FocusPolicy.StrongFocus]
        widget.deleteLater()

    def test_lead_waiting_on_a_helper_says_so(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """A lead whose delegation is running names the helper it is waiting for."""
        tab = MissionTab("mission")
        lead = MissionAgent(
            context_id="lead", title="lead", is_lead=True, state=MissionAgentState.WORKING,
            tool_name="delegate_ai", tool_target=""
        )
        snapshot = types.SimpleNamespace(mission=None)
        speech = tab._agent_speech(lead, "Theo", "", snapshot, None, ["Ada"])  # type: ignore[arg-type]  # pylint: disable=protected-access
        assert speech == "Waiting for Ada to finish and report back…"
        tab.deleteLater()


class TestFriendlyDashboard:
    """The dashboard says plainly what is happening and what the user needs to do."""

    def test_waiting_requests_raise_a_banner_that_leads_to_them(self, qapp) -> None:  # pylint: disable=unused-argument
        """While requests wait, a banner counts them, and Review now highlights the oldest."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        assert widget._attention.isHidden()  # pylint: disable=protected-access

        widget.set_decisions([_decision()])
        assert not widget._attention.isHidden()  # pylint: disable=protected-access
        assert widget._attention_text.text().startswith("1 ")  # pylint: disable=protected-access

        widget._attention_button.click()  # pylint: disable=protected-access
        assert widget._decision_cards["d1"].objectName() == "MissionDecisionCardHighlight"  # pylint: disable=protected-access

        widget.set_decisions([])
        assert widget._attention.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_auto_approve_level_is_explained(self, qapp) -> None:  # pylint: disable=unused-argument
        """Each auto-approve level comes with a sentence saying what it means."""
        widget = MissionWidget()
        notes = set()
        for level in ("off", "safe", "all"):
            widget._show_auto_approve(level)  # pylint: disable=protected-access
            notes.add(widget._auto_approve_note.text())  # pylint: disable=protected-access

        assert len(notes) == 3 and "" not in notes
        widget.deleteLater()

    def test_agents_list_is_folded_until_asked_for(self, qapp) -> None:  # pylint: disable=unused-argument
        """The list of agents is hidden by default and toggled by its button."""
        widget = MissionWidget()
        assert widget._agents_list.isHidden()  # pylint: disable=protected-access

        widget._agents_list_button.click()  # pylint: disable=protected-access
        assert not widget._agents_list.isHidden()  # pylint: disable=protected-access
        assert "Hide" in widget._agents_list_button.text()  # pylint: disable=protected-access

        widget._agents_list_button.click()  # pylint: disable=protected-access
        assert widget._agents_list.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_now_line_shows_what_the_lead_says(self, qapp) -> None:  # pylint: disable=unused-argument
        """The header repeats the lead's latest words, and hides the line when there are none."""
        widget = MissionWidget()
        view = _view(completed=False, stage=2)
        widget.set_mission(dataclasses.replace(
            view, header=dataclasses.replace(view.header, now_text="Theo: Waiting for Ada to finish…")
        ))
        assert not widget._now_row.isHidden()  # pylint: disable=protected-access
        assert widget._now_text.text() == "Theo: Waiting for Ada to finish…"  # pylint: disable=protected-access

        widget.set_mission(view)
        assert widget._now_row.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_journey_fill_follows_the_plan_while_the_team_works(self, qapp) -> None:  # pylint: disable=unused-argument
        """While the team works, the fill heads part of the way to the last stage."""
        journey = MissionJourney()
        journey.set_stage(2, MissionTone.ACCENT, live=True, fraction=0.5)

        assert journey._fill_target() == 2.5  # pylint: disable=protected-access
        journey.deleteLater()


def _many_chips(count: int, selected: int) -> list[MissionChipView]:
    return [
        MissionChipView(f"m{index}", f"Mission number {index} with a long title", MissionTone.ACCENT, index == selected)
        for index in range(count)
    ]


def _shown_strip(width: int, chips: list[MissionChipView]) -> MissionWidget:
    """Return a widget of the given width, shown, with its mission chips laid out."""
    widget = MissionWidget()
    widget.resize(width, 800)
    widget.apply_style()
    widget.show()
    widget.set_missions(chips, new_mission_selected=False)
    for _ in range(3):
        QApplication.processEvents()

    return widget


class TestMissionChipStrip:
    """Missions that do not fit scroll sideways, with arrows and swipes, while New mission stays put."""

    def test_arrows_appear_only_when_missions_overflow(self, qapp) -> None:  # pylint: disable=unused-argument
        """A few missions need no arrows; many missions get them."""
        widget = _shown_strip(1400, _many_chips(2, 0))
        assert widget._chip_left.isHidden() and widget._chip_right.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

        widget = _shown_strip(700, _many_chips(8, 0))
        assert not widget._chip_left.isHidden() and not widget._chip_right.isHidden()  # pylint: disable=protected-access
        assert not widget._chip_left.isEnabled() and widget._chip_right.isEnabled()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_new_mission_stays_outside_the_scrolling_strip(self, qapp) -> None:  # pylint: disable=unused-argument
        """New mission is never scrolled away."""
        widget = _shown_strip(700, _many_chips(8, 0))
        new_chip = widget.findChildren(QPushButton, "MissionNewChip")[0]

        assert not widget._chip_strip.isAncestorOf(new_chip)  # pylint: disable=protected-access
        widget.deleteLater()

    def test_right_arrow_scrolls_towards_later_missions(self, qapp) -> None:  # pylint: disable=unused-argument
        """The right arrow moves the strip, after which the left arrow can move it back."""
        widget = _shown_strip(700, _many_chips(8, 0))
        strip = widget._chip_strip  # pylint: disable=protected-access
        strip._animation.setDuration(0)  # pylint: disable=protected-access

        widget._chip_right.click()  # pylint: disable=protected-access
        QApplication.processEvents()

        assert strip.horizontalScrollBar().value() > 0
        assert widget._chip_left.isEnabled()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_sideways_swipe_moves_the_strip_and_vertical_scroll_does_not(self, qapp) -> None:  # pylint: disable=unused-argument
        """A trackpad swipe sideways scrolls the missions; scrolling up or down is left to the page."""
        widget = _shown_strip(700, _many_chips(8, 0))
        strip = widget._chip_strip  # pylint: disable=protected-access
        centre = QPointF(strip.width() / 2, strip.height() / 2)

        def wheel(dx: int, dy: int) -> QWheelEvent:
            return QWheelEvent(
                centre, strip.mapToGlobal(centre), QPoint(dx, dy), QPoint(dx, dy),
                Qt.MouseButton.NoButton, Qt.KeyboardModifier.NoModifier, Qt.ScrollPhase.NoScrollPhase, False
            )

        vertical = wheel(0, -120)
        strip.wheelEvent(vertical)
        assert strip.horizontalScrollBar().value() == 0
        assert not vertical.isAccepted()

        sideways = wheel(-120, 0)
        strip.wheelEvent(sideways)
        assert strip.horizontalScrollBar().value() == 120
        assert sideways.isAccepted()
        widget.deleteLater()

    def test_selected_mission_is_scrolled_into_view(self, qapp) -> None:  # pylint: disable=unused-argument
        """Selecting a mission at the far end glides its chip into view."""
        widget = _shown_strip(700, _many_chips(8, 7))
        QTest.qWait(400)

        assert widget._chip_strip.horizontalScrollBar().value() > 0  # pylint: disable=protected-access
        widget.deleteLater()

    def test_strip_keeps_its_height_when_missions_arrive_later(self, qapp) -> None:  # pylint: disable=unused-argument
        """Missions added after the strip is shown are drawn at full height, not squashed out of sight."""
        widget = _shown_strip(1000, _many_chips(1, 0))
        widget.set_missions(_many_chips(3, 0), new_mission_selected=False)
        QTest.qWait(50)

        strip = widget._chip_strip  # pylint: disable=protected-access
        assert strip.height() > 0
        assert strip.height() == strip.sizeHint().height()
        widget.deleteLater()


class TestSmoothScrolling:
    """Moving the page to show something glides there rather than jumping."""

    def test_review_now_glides_to_the_request(self, qapp) -> None:  # pylint: disable=unused-argument
        """The page starts where it was and glides until the request is in view."""
        widget = MissionWidget()
        widget.resize(420, 500)
        widget.apply_style()
        widget.show()
        widget.set_mission(_view(completed=False, stage=2))
        widget.set_decisions([_decision()])
        for _ in range(3):
            QApplication.processEvents()

        scrollbar = widget._scroll.verticalScrollBar()  # pylint: disable=protected-access
        assert scrollbar.value() == 0

        widget._attention_button.click()  # pylint: disable=protected-access
        QApplication.processEvents()
        animation = widget._scroll_animations[widget._scroll]  # pylint: disable=protected-access
        assert animation.state() == QPropertyAnimation.State.Running
        assert scrollbar.value() < animation.endValue()

        QTest.qWait(animation.duration() + 100)
        assert scrollbar.value() == animation.endValue() > 0
        widget.deleteLater()


def _waiting_view() -> MissionView:
    """Return a mission whose helper waits for an answer, with a plan tall enough to scroll."""
    waiting = dataclasses.replace(
        _agent_view("a", parent_id="lead", activity="waiting"),
        speech="I need your approval: Write a file", decision_id="d1",
    )
    return dataclasses.replace(
        _view(completed=False, stage=2),
        agents=[_agent_view("lead", is_lead=True), waiting],
        steps=[_step(str(n), MissionStepState.PENDING) for n in range(1, 9)],
    )


class TestAnsweringInPlace:
    """Answering in an agent's bubble leaves the user where they are; only Read more moves the page."""

    def test_answering_in_a_bubble_keeps_the_agents_in_place(self, qapp) -> None:  # pylint: disable=unused-argument
        """The call to action above disappears once answered, and the page makes up for it."""
        widget = MissionWidget()
        widget.resize(900, 600)
        widget.apply_style()
        widget.show()
        widget.set_mission(_waiting_view())
        widget.set_decisions([_decision()])
        approved: list[str] = []
        widget.decision_approved.connect(approved.append)
        QTest.qWait(50)

        network = widget._agent_network  # pylint: disable=protected-access
        content = widget._scroll.widget()  # pylint: disable=protected-access
        scrollbar = widget._scroll.verticalScrollBar()  # pylint: disable=protected-access
        scrollbar.setValue(network.mapTo(content, QPoint(0, 0)).y() - 40)
        on_screen = network.mapTo(content, QPoint(0, 0)).y() - scrollbar.value()
        assert scrollbar.value() > 0

        network.decision_approved.emit("d1")
        widget.set_decisions([])
        QTest.qWait(50)

        assert approved == ["d1"]
        assert widget._attention.isHidden()  # pylint: disable=protected-access
        assert network.mapTo(content, QPoint(0, 0)).y() - scrollbar.value() == on_screen
        widget.deleteLater()

    def test_read_more_glides_to_the_request(self, qapp) -> None:  # pylint: disable=unused-argument
        """Read more is the one button in a bubble that takes the user to the inbox, gliding there."""
        widget = MissionWidget()
        widget.resize(420, 500)
        widget.apply_style()
        widget.show()
        widget.set_mission(_waiting_view())
        widget.set_decisions([_decision()])
        QTest.qWait(50)

        widget._agent_network.decision_details_requested.emit("d1")  # pylint: disable=protected-access
        QApplication.processEvents()
        animation = widget._scroll_animations[widget._scroll]  # pylint: disable=protected-access
        assert animation.state() == QPropertyAnimation.State.Running
        widget.deleteLater()


class TestWaitingAlerts:
    """A mission waiting for an answer is pointed out from wherever the user is."""

    def test_a_waiting_mission_shows_a_count_on_its_chip(self, qapp) -> None:  # pylint: disable=unused-argument
        """Only the chip of a waiting mission carries the count."""
        widget = MissionWidget()
        widget.set_missions([
            MissionChipView("m1", "First", MissionTone.ACCENT, True),
            MissionChipView("m2", "Second", MissionTone.WARNING, False, "Needs you", waiting_text="2"),
        ], False)
        badges = widget.findChildren(QWidget, "MissionChipWaiting")
        assert len(badges) == 1
        assert badges[0].toolTip() == "Needs you"
        widget.deleteLater()

    def test_another_waiting_mission_is_one_click_away(self, qapp) -> None:  # pylint: disable=unused-argument
        """The notice names the waiting mission and its button opens it."""
        widget = MissionWidget()
        selected: list[str] = []
        widget.mission_selected.connect(selected.append)
        widget.set_waiting_elsewhere("m2", "“Second” is waiting for your decision")
        assert not widget._elsewhere.isHidden()  # pylint: disable=protected-access
        assert widget._elsewhere_text.text() == "“Second” is waiting for your decision"  # pylint: disable=protected-access

        widget._elsewhere_button.click()  # pylint: disable=protected-access
        assert selected == ["m2"]

        widget.set_waiting_elsewhere("", "")
        assert widget._elsewhere.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_each_mission_shows_only_its_own_requests(self, qapp) -> None:  # pylint: disable=unused-argument
        """Requests from other missions stay out of the inbox and are pointed out; those outside any mission stay."""
        missions = [
            types.SimpleNamespace(mission_id="m1", title="First", mode=MissionMode.AUTONOMOUS),
            types.SimpleNamespace(mission_id="m2", title="Second", mode=MissionMode.AUTONOMOUS),
        ]
        service = types.SimpleNamespace(
            missions=lambda: missions,
            snapshot=lambda _mission_id: None,
            status=lambda mission_id: MissionStatus.NEEDS_YOU if mission_id == "m2" else MissionStatus.RUNNING,
            resume_automatically=lambda: False,
            decisions=lambda: [
                MissionDecision(
                    decision_id="d1", context_id="c1", conversation_title="Helper", mission_id="m2",
                    tool_name="system", reason="Open a tab", detail=None, destructive=False,
                    requested_at=datetime.now(timezone.utc),
                ),
                MissionDecision(
                    decision_id="d2", context_id="c2", conversation_title="Chat", mission_id=None,
                    tool_name="system", reason="Open a tab", detail=None, destructive=False,
                    requested_at=datetime.now(timezone.utc),
                ),
            ],
        )
        tab = MissionTab("mission-tab")
        tab._service = service  # pylint: disable=protected-access
        tab._starting_new = True  # pylint: disable=protected-access
        tab._refresh()  # pylint: disable=protected-access

        widget = tab._widget  # pylint: disable=protected-access
        assert [d.decision_id for d in widget._decisions] == ["d2"]  # pylint: disable=protected-access
        assert widget._elsewhere_mission_id == "m2"  # pylint: disable=protected-access
        assert widget._elsewhere_text.text() == "“Second” is waiting for your decision"  # pylint: disable=protected-access
        assert [chip.waiting_text for chip in widget._chips] == ["", "1"]  # pylint: disable=protected-access
        tab._service = None  # pylint: disable=protected-access
        tab.deleteLater()


def _waiting_network(helpers: int, width: int) -> AgentNetworkView:
    """Return the agents of a shown Mission Control page, where the last of the lead's helpers waits for an answer."""
    host = MissionWidget()
    host.resize(width, 1200)
    host.apply_style()
    host.show()
    agents = [_agent_view("lead", is_lead=True)]
    for index in range(helpers):
        agents.append(_agent_view(f"h{index}", parent_id="lead"))

    agents[-1] = dataclasses.replace(
        _agent_view(f"h{helpers - 1}", parent_id="lead", activity="waiting"),
        speech="I need your approval: The AI is requesting to GET https://api.example.com/v1/forecast", decision_id="d1",
    )
    host.set_mission(dataclasses.replace(_view(completed=False, stage=2), agents=agents))
    QTest.qWait(50)
    network = host._agent_network  # pylint: disable=protected-access
    network._phase += 10  # pylint: disable=protected-access
    network._place_buttons()  # pylint: disable=protected-access
    return network


class TestBubbleButtons:
    """A waiting agent's bubble answers its request without leaving the agents."""

    def test_reject_reports_the_decision_once(self, qapp) -> None:  # pylint: disable=unused-argument
        """Reject reports a rejection only, and no answer can be given twice."""
        network = _waiting_network(1, 2000)
        approved: list[str] = []
        rejected: list[str] = []
        network.decision_approved.connect(approved.append)
        network.decision_rejected.connect(rejected.append)
        _key, (approve, unsure, reject, _more) = network._buttons["h0"]  # pylint: disable=protected-access

        reject.click()
        assert rejected == ["d1"] and not approved
        assert not approve.isEnabled() and not unsure.isEnabled() and not reject.isEnabled()
        network.window().deleteLater()

    def test_buttons_take_new_labels(self, qapp) -> None:  # pylint: disable=unused-argument
        """Changing language relabels a bubble's buttons in place."""
        network = _waiting_network(1, 2000)
        _key, buttons = network._buttons["h0"]  # pylint: disable=protected-access
        agents = network._nodes  # pylint: disable=protected-access
        views = [node.agent for node in agents if node.agent is not None]
        network.set_agents(views, "Vous", "Approuver", "Rejeter", "Terminer", "{0}", "Lire la suite", "Pas sûr")

        assert network._buttons["h0"][1] == buttons  # pylint: disable=protected-access
        assert [button.text() for button in buttons] == ["Approuver", "Pas sûr", "Rejeter", "Lire la suite"]
        network.window().deleteLater()

    def test_wide_bubble_keeps_every_button_on_one_row(self, qapp) -> None:  # pylint: disable=unused-argument
        """With room to spare, Read more sits beside the answer buttons."""
        network = _waiting_network(1, 2000)
        _key, buttons = network._buttons["h0"]  # pylint: disable=protected-access
        assert len({button.y() for button in buttons}) == 1
        assert all(button.width() >= button.sizeHint().width() for button in buttons)
        network.window().deleteLater()

    def test_narrow_bubble_moves_read_more_to_its_own_row(self, qapp) -> None:  # pylint: disable=unused-argument
        """Sharing a level, the answer buttons keep their full width and Read more goes above them, inside the card."""
        network = _waiting_network(3, 1300)
        _key, (approve, unsure, reject, more) = network._buttons["h2"]  # pylint: disable=protected-access
        node = next(n for n in network._nodes if n.node_id == "h2")  # pylint: disable=protected-access
        card = network._bubble_rect(node)  # pylint: disable=protected-access

        assert approve.y() == unsure.y() == reject.y()
        assert more.geometry().bottom() < approve.y()
        assert all(button.width() >= button.sizeHint().width() for button in (approve, unsure, reject))
        for button in (approve, unsure, reject, more):
            assert card.contains(QRectF(button.geometry()))

        network.window().deleteLater()

    def test_answering_keeps_focus_on_the_agents(self, qapp) -> None:  # pylint: disable=unused-argument
        """A focused button that is answered hands focus to the picture, not to the next button on the page."""
        network = _waiting_network(1, 2000)
        network.show()
        network.activateWindow()
        _key, (approve, _unsure, _reject, _more) = network._buttons["h0"]  # pylint: disable=protected-access
        approve.setFocus()
        QApplication.processEvents()
        assert approve.hasFocus()

        approve.click()
        assert network.hasFocus()
        network.window().deleteLater()

    def test_finish_waiting_keeps_focus_on_the_agents(self, qapp) -> None:  # pylint: disable=unused-argument
        """Cutting a wait short also keeps focus on the picture."""
        network = AgentNetworkView()
        network.resize(800, 600)
        sleeping = dataclasses.replace(
            _agent_view("a", parent_id="lead", activity="working"), speech="Taking a pause", wait_started=1.0,
            wait_seconds=60.0,
        )
        network.set_agents([_agent_view("lead", is_lead=True), sleeping], "You", finish_label="Finish waiting")
        network._phase += 10  # pylint: disable=protected-access
        network._place_buttons()  # pylint: disable=protected-access
        network.show()
        network.activateWindow()
        finished: list[str] = []
        network.wait_finish_requested.connect(finished.append)
        finish = network._buttons["a"][1][0]  # pylint: disable=protected-access
        finish.setFocus()
        QApplication.processEvents()

        finish.click()
        assert finished == ["a"]
        assert network.hasFocus() and not finish.isEnabled()
        network.deleteLater()

    def test_link_to_a_helper_starts_at_the_bottom_of_the_speaking_agents_bubble(self, qapp) -> None:  # pylint: disable=unused-argument
        """A delegator that is talking is joined to its helper from under its bubble."""
        network = _waiting_network(1, 2000)
        lead = dataclasses.replace(_agent_view("lead", is_lead=True), speech="Bringing in a helper")
        helper = _agent_view("h0", parent_id="lead", activity="working")
        network.set_agents([lead, helper], "You")
        nodes = {node.node_id: node for node in network._nodes}  # pylint: disable=protected-access
        painter = mock.MagicMock()

        network._paint_edge(painter, nodes["lead"], nodes["h0"])  # pylint: disable=protected-access
        path = next(call.args[0] for call in painter.drawPath.call_args_list if isinstance(call.args[0], QPainterPath))
        start = path.elementAt(0)
        assert start.x == nodes["lead"].center.x()
        assert start.y == network._bubble_rect(nodes["lead"]).bottom()  # pylint: disable=protected-access
        network.window().deleteLater()


def _held_widget() -> MissionWidget:
    """Return a shown widget with a waiting helper, scrolled so the agents are part way down the screen."""
    widget = MissionWidget()
    widget.resize(900, 600)
    widget.apply_style()
    widget.show()
    widget.set_mission(_waiting_view())
    widget.set_decisions([_decision()])
    QTest.qWait(50)
    network = widget._agent_network  # pylint: disable=protected-access
    scrollbar = widget._scroll.verticalScrollBar()  # pylint: disable=protected-access
    scrollbar.setValue(network.mapTo(widget._scroll.widget(), QPoint(0, 0)).y() - 40)  # pylint: disable=protected-access
    return widget


class TestHoldingThePage:
    """Every answer given in a bubble holds the agents still; the hold ends on its own or when the user is taken elsewhere."""

    @pytest.mark.parametrize("network_signal, widget_signal, key", [
        ("decision_approved", "decision_approved", "d1"),
        ("decision_unsure", "decision_unsure", "d1"),
        ("decision_rejected", "decision_rejected", "d1"),
        ("wait_finish_requested", "wait_finish_requested", "a"),
    ])
    def test_each_answer_is_passed_on_and_holds_the_agents(self, qapp, network_signal, widget_signal, key) -> None:  # pylint: disable=unused-argument
        """Approve, Unsure, Reject and Finish waiting all reach the owner and hold the page."""
        widget = _held_widget()
        received: list[str] = []
        getattr(widget, widget_signal).connect(received.append)

        getattr(widget._agent_network, network_signal).emit(key)  # pylint: disable=protected-access
        assert received == [key]
        assert widget._hold_offset == 40  # pylint: disable=protected-access
        assert widget._hold_timer.isActive()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_hold_ends_by_itself(self, qapp) -> None:  # pylint: disable=unused-argument
        """Once the hold has run out, a change above the agents moves the page as usual."""
        widget = _held_widget()
        scrollbar = widget._scroll.verticalScrollBar()  # pylint: disable=protected-access
        widget._agent_network.decision_approved.emit("d1")  # pylint: disable=protected-access
        widget._hold_timer.timeout.emit()  # pylint: disable=protected-access
        assert widget._hold_offset is None  # pylint: disable=protected-access

        before = scrollbar.value()
        widget.set_decisions([])
        QTest.qWait(50)
        assert scrollbar.value() == before
        widget.deleteLater()

    def test_read_more_after_answering_is_not_held_back(self, qapp) -> None:  # pylint: disable=unused-argument
        """Being taken to a request ends any hold, so the glide is not undone."""
        widget = _held_widget()
        widget.set_decisions([_decision(), dataclasses.replace(_decision(), decision_id="d2")])
        QTest.qWait(20)
        widget._agent_network.decision_approved.emit("d1")  # pylint: disable=protected-access
        widget.show_decision("d2")
        QApplication.processEvents()
        assert widget._hold_offset is None  # pylint: disable=protected-access
        widget.deleteLater()

    def test_nothing_is_held_while_the_agents_are_hidden(self, qapp) -> None:  # pylint: disable=unused-argument
        """With no agents on screen there is nothing to hold, but the answer is still passed on."""
        widget = MissionWidget()
        approved: list[str] = []
        widget.decision_approved.connect(approved.append)
        widget._agent_network.decision_approved.emit("d1")  # pylint: disable=protected-access
        assert approved == ["d1"]
        assert widget._hold_offset is None  # pylint: disable=protected-access
        widget.deleteLater()


class TestGliding:
    """The page glides only as far as it must, in either direction."""

    def test_glides_up_to_something_above(self, qapp) -> None:  # pylint: disable=unused-argument
        """Something above the screen is glided down to."""
        widget = MissionWidget()
        widget.resize(420, 500)
        widget.apply_style()
        widget.show()
        widget.set_mission(_waiting_view())
        QTest.qWait(50)
        scrollbar = widget._scroll.verticalScrollBar()  # pylint: disable=protected-access
        scrollbar.setValue(scrollbar.maximum())

        widget.glide_to(widget._journey)  # pylint: disable=protected-access
        QApplication.processEvents()
        animation = widget._scroll_animations[widget._scroll]  # pylint: disable=protected-access
        assert animation.state() == QPropertyAnimation.State.Running
        assert animation.endValue() < animation.startValue()
        widget.deleteLater()

    def test_no_move_when_already_in_view(self, qapp) -> None:  # pylint: disable=unused-argument
        """Something already on screen does not move the page."""
        widget = MissionWidget()
        widget.resize(900, 900)
        widget.apply_style()
        widget.show()
        widget.set_mission(_waiting_view())
        QTest.qWait(50)

        widget.glide_to(widget._journey)  # pylint: disable=protected-access
        QApplication.processEvents()
        assert widget._scroll not in widget._scroll_animations  # pylint: disable=protected-access
        widget.deleteLater()

    @pytest.mark.parametrize("target, card", [
        (MissionStatTarget.INBOX, "_inbox_card"), (MissionStatTarget.AGENTS, "_agents_card"),
    ])
    def test_statistics_glide_to_their_cards(self, qapp, target, card) -> None:  # pylint: disable=unused-argument
        """The approvals and agents figures glide to the inbox and the agents."""
        widget = MissionWidget()
        widget._on_stat_clicked(target)  # pylint: disable=protected-access
        assert widget._glide_target is getattr(widget, card)  # pylint: disable=protected-access
        assert widget._glide_timer.isActive()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_unknown_request_goes_nowhere(self, qapp) -> None:  # pylint: disable=unused-argument
        """Asking to see a request that has gone moves and highlights nothing."""
        widget = MissionWidget()
        widget.set_decisions([_decision()])
        widget.show_decision("gone")
        assert widget._glide_target is None  # pylint: disable=protected-access
        assert widget._highlighted_card is None  # pylint: disable=protected-access
        widget.deleteLater()

    def test_choosing_an_earlier_mission_glides_the_chips_back(self, qapp) -> None:  # pylint: disable=unused-argument
        """The strip glides back towards the start when an earlier mission is selected."""
        widget = _shown_strip(600, _many_chips(10, 9))
        QTest.qWait(500)
        scrollbar = widget._chip_strip.horizontalScrollBar()  # pylint: disable=protected-access
        assert scrollbar.value() > 0

        widget.set_missions(_many_chips(10, 0), new_mission_selected=False)
        QTest.qWait(500)
        assert scrollbar.value() == scrollbar.minimum()
        widget.deleteLater()


class TestDesignSystemButtons:
    """Mission Control's buttons are painted in the application's button colours."""

    @pytest.mark.parametrize("name, role", [
        ("MissionApprove", ColorRole.BUTTON_BACKGROUND_RECOMMENDED),
        ("MissionReplyButton", ColorRole.BUTTON_BACKGROUND_RECOMMENDED),
        ("MissionSecondaryButton", ColorRole.BUTTON_SECONDARY_BACKGROUND),
        ("MissionGhostButton", ColorRole.BUTTON_SECONDARY_BACKGROUND),
        ("MissionApproveDestructive", ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE),
        ("MissionStopButton", ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE),
        ("MissionStopTaskButton", ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE),
    ])
    def test_button_uses_its_colour_role(self, qapp, name, role) -> None:  # pylint: disable=unused-argument
        """Each kind of button fills with the colour the rest of the application uses for it."""
        widget = MissionWidget()
        widget.resize(900, 700)
        widget.apply_style()
        widget.show()
        button = QPushButton("Approve", widget)
        button.setObjectName(name)
        button.show()
        button.adjustSize()
        QApplication.processEvents()

        image = button.grab().toImage()
        assert image.pixelColor(5, image.height() // 2).name() == StyleManager().get_color(role).name()

        button.setEnabled(False)
        QApplication.processEvents()
        image = button.grab().toImage()
        disabled = StyleManager().get_color(ColorRole.BUTTON_BACKGROUND_DISABLED).name()
        assert image.pixelColor(5, image.height() // 2).name() == disabled
        widget.deleteLater()


class TestMoreActions:
    """Switching to classic and deleting are kept in the mission's ⋯ menu."""

    @staticmethod
    def _choose(widget: MissionWidget, monkeypatch, label: str) -> list[str]:
        """Open the ⋯ menu, pick the entry with a label, and return the labels offered."""
        offered: list[str] = []
        menu = mock.MagicMock()
        menu.addAction.side_effect = lambda text: offered.append(text) or text
        menu.exec.side_effect = lambda _position: label
        monkeypatch.setattr(widget._style_manager, "create_menu", lambda _parent: menu)  # pylint: disable=protected-access
        widget._show_more_actions()  # pylint: disable=protected-access
        return offered

    def test_delete_is_in_the_menu(self, qapp, monkeypatch) -> None:  # pylint: disable=unused-argument
        """Choosing Delete asks for the mission to be removed."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        removed: list[bool] = []
        widget.remove_requested.connect(lambda: removed.append(True))
        strings = widget._language_manager.strings()  # pylint: disable=protected-access

        offered = self._choose(widget, monkeypatch, strings.mission_delete)
        assert offered == [strings.mission_switch_to_classic, strings.mission_delete]
        assert removed == [True]
        widget.deleteLater()

    def test_switch_to_classic_is_offered_only_in_autonomous_mode(self, qapp, monkeypatch) -> None:  # pylint: disable=unused-argument
        """An autonomous mission can be switched to classic from the menu; a classic one is not offered it."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        switched: list[bool] = []
        widget.switch_to_classic_requested.connect(lambda: switched.append(True))
        strings = widget._language_manager.strings()  # pylint: disable=protected-access

        self._choose(widget, monkeypatch, strings.mission_switch_to_classic)
        assert switched == [True]

        classic = _view(completed=False, stage=2)
        classic = dataclasses.replace(classic, header=dataclasses.replace(classic.header, is_classic=True))
        widget.set_mission(classic)
        assert self._choose(widget, monkeypatch, "") == [strings.mission_delete]
        widget.deleteLater()


class TestClickableRows:
    """Rows that open something answer the keyboard as well as the mouse."""

    @pytest.mark.parametrize("key", [Qt.Key.Key_Return, Qt.Key.Key_Enter, Qt.Key.Key_Space])
    def test_return_enter_and_space_click_a_row(self, qapp, key) -> None:  # pylint: disable=unused-argument
        """Each of the usual keys opens the row."""
        row = mission_widget_module._ClickableFrame()  # pylint: disable=protected-access
        clicked: list[bool] = []
        row.clicked.connect(lambda: clicked.append(True))
        QTest.keyClick(row, key)
        assert clicked == [True]
        row.deleteLater()

    def test_other_keys_do_not_click(self, qapp) -> None:  # pylint: disable=unused-argument
        """Typing a letter on a row does nothing."""
        row = mission_widget_module._ClickableFrame()  # pylint: disable=protected-access
        clicked: list[bool] = []
        row.clicked.connect(lambda: clicked.append(True))
        QTest.keyClick(row, Qt.Key.Key_A)
        assert not clicked
        row.deleteLater()


class TestScenePainting:
    """The journey and the confetti really draw what they promise."""

    @staticmethod
    def _journey(live: bool) -> MissionJourney:
        journey = MissionJourney()
        journey.set_labels(["You asked", "AI plans it", "AI team works", "Job done"])
        journey.set_stage(1, MissionTone.ACCENT, live)
        journey._shown_stage = 1.0  # pylint: disable=protected-access
        journey.resize(600, journey.sizeHint().height())
        return journey

    @staticmethod
    def _painted(widget: QWidget) -> QImage:
        image = QImage(widget.size(), QImage.Format.Format_ARGB32)
        image.fill(Qt.GlobalColor.transparent)
        widget.render(image, QPoint(), QRegion(), QWidget.RenderFlag.DrawChildren)
        return image

    def test_working_stage_draws_its_orbit_and_still_stage_does_not(self, qapp) -> None:  # pylint: disable=unused-argument
        """Only while the AI works is the current stage circled by its orbit and moon."""
        live = self._journey(True)
        still = self._journey(False)
        live._start = still._start  # pylint: disable=protected-access
        assert self._painted(live) != self._painted(still)
        live.deleteLater()
        still.deleteLater()

    def test_nothing_is_cut_off_at_the_top(self, qapp) -> None:  # pylint: disable=unused-argument
        """The glow above the current stage fades out before it reaches the top edge."""
        image = self._painted(self._journey(True))
        assert all(image.pixelColor(x, 0).alpha() < 40 for x in range(image.width()))

    def test_fill_glides_towards_the_current_stage(self, qapp) -> None:  # pylint: disable=unused-argument
        """Each frame moves the fill part of the way, until it arrives."""
        journey = self._journey(False)
        journey._shown_stage = 0.0  # pylint: disable=protected-access
        journey._tick()  # pylint: disable=protected-access
        assert 0.0 < journey._shown_stage < 1.0  # pylint: disable=protected-access

        journey._shown_stage = 0.999  # pylint: disable=protected-access
        journey._tick()  # pylint: disable=protected-access
        assert journey._shown_stage == 1.0  # pylint: disable=protected-access
        journey.deleteLater()

    def test_confetti_draws_pieces(self, qapp) -> None:  # pylint: disable=unused-argument
        """A burst puts coloured pieces on screen."""
        host = QWidget()
        host.resize(600, 400)
        overlay = ConfettiOverlay(host)
        overlay.burst(QPointF(300, 150))
        image = self._painted(overlay)
        assert any(image.pixelColor(x, y).alpha() > 0 for x in range(0, 600, 3) for y in range(0, 400, 3))
        host.deleteLater()


class TestWaitingElsewhereText:
    """The notice about other missions names one, counts several, and never points at the mission on screen."""

    @staticmethod
    def _tab(selected: str | None, waiting: list[str]) -> MissionTab:
        missions = [
            types.SimpleNamespace(mission_id=m, title=m.upper(), mode=MissionMode.AUTONOMOUS, auto_approve=AutoApproveLevel.OFF)
            for m in ("m1", "m2", "m3")
        ]
        decisions = [
            MissionDecision(
                decision_id=f"d-{m}", context_id=f"c-{m}", conversation_title="Helper", mission_id=m, tool_name="system",
                reason="Open a tab", detail=None, destructive=False, requested_at=datetime.now(timezone.utc),
            )
            for m in waiting
        ]
        service = types.SimpleNamespace(
            missions=lambda: missions, snapshot=lambda _mission_id: None,
            status=lambda mission_id: MissionStatus.NEEDS_YOU if mission_id in waiting else MissionStatus.RUNNING,
            resume_automatically=lambda: False, decisions=lambda: decisions,
        )
        tab = MissionTab("mission-tab")
        tab._service = service  # pylint: disable=protected-access
        tab._selected_mission_id = selected  # pylint: disable=protected-access
        tab._starting_new = selected is None  # pylint: disable=protected-access
        tab._refresh()  # pylint: disable=protected-access
        return tab

    def test_several_waiting_missions_are_counted(self, qapp) -> None:  # pylint: disable=unused-argument
        """With more than one other mission waiting, the notice counts them and goes to the first."""
        tab = self._tab("m1", ["m2", "m3"])
        widget = tab._widget  # pylint: disable=protected-access
        assert widget._elsewhere_text.text() == "2 other missions are waiting for your decisions"  # pylint: disable=protected-access
        assert widget._elsewhere_mission_id == "m2"  # pylint: disable=protected-access
        assert widget._decisions == []  # pylint: disable=protected-access
        tab._service = None  # pylint: disable=protected-access
        tab.deleteLater()

    def test_the_mission_on_screen_is_not_pointed_at(self, qapp) -> None:  # pylint: disable=unused-argument
        """A request from the mission on screen goes in its inbox, with no notice about other missions."""
        tab = self._tab("m2", ["m2"])
        widget = tab._widget  # pylint: disable=protected-access
        assert widget._elsewhere.isHidden()  # pylint: disable=protected-access
        assert [d.decision_id for d in widget._decisions] == ["d-m2"]  # pylint: disable=protected-access
        tab._service = None  # pylint: disable=protected-access
        tab.deleteLater()

    def test_no_notice_without_waiting_missions(self, qapp) -> None:  # pylint: disable=unused-argument
        """Nothing waiting, no notice and no counts on the chips."""
        tab = self._tab("m1", [])
        widget = tab._widget  # pylint: disable=protected-access
        assert widget._elsewhere.isHidden()  # pylint: disable=protected-access
        assert all(not chip.waiting_text for chip in widget._chips)  # pylint: disable=protected-access
        tab._service = None  # pylint: disable=protected-access
        tab.deleteLater()
