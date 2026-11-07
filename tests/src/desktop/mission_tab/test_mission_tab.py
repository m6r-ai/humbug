"""Tests for the Mission Control tab and its widget."""

import asyncio
import dataclasses

import pytest

from PySide6.QtCore import Qt
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QPushButton

from ai import AIConversation, AIConversationEvent, AIMessage, AIMessageSource
from ai_tool import AIToolCall
from context.context_registry import ContextEvent
from mission import MissionAgent, MissionAgentState, MissionMode
from PySide6.QtWidgets import QLabel

from desktop.mission_tab import mission_tab as mission_tab_module
from desktop.mission_tab import mission_widget as mission_widget_module
from desktop.mission_tab.mission_display import (
    MissionAgentView, MissionChipView, MissionDecisionView, MissionPhaseView, MissionStatTarget, MissionStatView, MissionStepState, MissionStepView, MissionTone
)
from desktop.message_box import MessageBoxButton
from desktop.mission_tab.mission_mode_menus import MissionModeMenus
from desktop.mission_tab.mission_service_holder import MissionServiceHolder
from desktop.mission_tab.mission_network import AgentNetworkView
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
        _decision_id, (approve, reject) = network._buttons["a"]  # pylint: disable=protected-access
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
                "terminal", {"operation": "send_command"}, "Run the tests", None, True
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
