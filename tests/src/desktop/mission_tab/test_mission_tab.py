"""Tests for the Mission Control tab and its widget."""

import asyncio
import dataclasses
from datetime import datetime, timezone
import types
from unittest import mock

import pytest

from PySide6.QtCore import QEvent, QPoint, QPointF, QPropertyAnimation, QRectF, Qt
from PySide6.QtGui import QImage, QMouseEvent, QPainterPath, QRegion, QWheelEvent
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication, QFrame, QPushButton, QToolButton, QVBoxLayout, QWidget

from ai import AIConversation, AIConversationEvent, AIMessage, AIMessageSource
from ai_tool import AIToolApprovalKind, AIToolCall
from context.context_registry import ContextEvent
from mission import (
    AutoApproveLevel, Mission, MissionAgent, MissionAgentState, MissionDecision, MissionExchange, MissionMode,
    MissionPhase, MissionSnapshot, MissionSpeaker, MissionStatus, MissionTask, MissionTaskStatus
)
from PySide6.QtWidgets import QLabel

from desktop.mission_tab import mission_tab as mission_tab_module
from desktop.mission_tab import mission_widget as mission_widget_module
from desktop.mission_tab import mission_scene as mission_scene_module
from desktop.mission_tab import mission_network as mission_network_module
from desktop.mission_tab.mission_display import (
    MissionActivityView, MissionAgentView, MissionChatView, MissionChipView, MissionDecisionView, MissionGlanceView,
    MissionHeaderView, MissionPhaseView,
    MissionResultView, MissionStatTarget,
    MissionStatView, MissionStepState, MissionStepView, MissionTone, MissionView
)
from desktop.message_box import MessageBoxButton
from desktop.mission_tab.mission_mode_menus import MissionModeMenus
from desktop.mission_tab.mission_service_holder import MissionServiceHolder
from desktop.mission_tab.mission_motion import MissionMotion
from desktop.mission_tab.mission_network import AgentNetworkView
from desktop.mission_tab.mission_painted import BusySpinner, StatusPill, fade_in
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


class TestAutoApproveBeforeStarting:
    """The auto-approve level can be chosen on the new-mission screen, before the mission starts."""

    def test_new_mission_screen_offers_the_levels(self, qapp) -> None:  # pylint: disable=unused-argument
        """The welcome shows the three levels with Off chosen and explained, and reports a click."""
        widget = MissionWidget()
        widget.set_mission(None)
        buttons = widget._hero_auto_approve_buttons  # pylint: disable=protected-access
        assert [buttons[level].text() for level in ("off", "safe", "all")] == ["Off", "Safe actions", "Everything"]
        assert buttons["off"].isChecked()
        assert widget._hero_auto_approve_note.text()  # pylint: disable=protected-access

        chosen: list[str] = []
        widget.new_mission_auto_approve_selected.connect(chosen.append)
        buttons["safe"].click()
        assert chosen == ["safe"]
        widget.deleteLater()

    def test_mission_starts_with_the_chosen_level(self, mindspace_sandbox, submitted, monkeypatch) -> None:  # pylint: disable=unused-argument
        """A level chosen before starting is the mission's from the start; Everything is confirmed first."""
        async def run() -> None:
            tab = MissionTab("mission-tab")
            tab.attach_service()
            tab._on_new_mission_requested()  # pylint: disable=protected-access
            hero = tab._widget._hero_auto_approve_buttons  # pylint: disable=protected-access

            monkeypatch.setattr(mission_tab_module.MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.NO)
            tab._widget.new_mission_auto_approve_selected.emit("all")  # pylint: disable=protected-access
            assert hero["off"].isChecked()

            monkeypatch.setattr(mission_tab_module.MessageBox, "show_message", lambda *_a, **_k: MessageBoxButton.YES)
            tab._widget.new_mission_auto_approve_selected.emit("all")  # pylint: disable=protected-access
            assert hero["all"].isChecked()

            tab._widget.submit_requested.emit("Tidy the docs")  # pylint: disable=protected-access
            for _ in range(5):
                await asyncio.sleep(0)

            service = MissionServiceHolder().current_service()
            assert service is not None
            assert service.missions()[0].auto_approve == AutoApproveLevel.ALL

            # The next mission starts from Off again unless the user chooses otherwise.
            assert hero["off"].isChecked()
            tab._on_new_mission_requested()  # pylint: disable=protected-access
            tab._widget.submit_requested.emit("Fix the tests")  # pylint: disable=protected-access
            for _ in range(5):
                await asyncio.sleep(0)

            newest = service.missions()[0]
            assert newest.objective == "Fix the tests"
            assert newest.auto_approve == AutoApproveLevel.OFF

            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())


class TestSidebarFollowsTheMission:
    """Mission Control says which conversation leads the selected mission, so the sidebar can highlight it."""

    def test_selected_missions_lead_conversation_is_announced(self, mindspace_sandbox, submitted) -> None:  # pylint: disable=unused-argument
        """Selecting a mission announces its lead's file; starting a new mission clears it."""
        async def run() -> None:
            tab = MissionTab("mission-tab")
            announced: list[str] = []
            tab.lead_conversation_changed.connect(announced.append)
            tab.attach_service()
            for objective in ("Tidy the docs", "Fix the tests"):
                tab._on_new_mission_requested()  # pylint: disable=protected-access
                tab._widget.submit_requested.emit(objective)  # pylint: disable=protected-access
                for _ in range(5):
                    await asyncio.sleep(0)

            service = MissionServiceHolder().current_service()
            assert service is not None
            newest, oldest = service.missions()
            mindspace = mindspace_sandbox.manager.mindspace()

            tab._on_mission_selected(oldest.mission_id)  # pylint: disable=protected-access
            assert tab.lead_conversation_path() == mindspace.get_absolute_path(oldest.lead_path)
            assert announced[-1] == tab.lead_conversation_path()

            tab._on_mission_selected(newest.mission_id)  # pylint: disable=protected-access
            assert announced[-1] == mindspace.get_absolute_path(newest.lead_path)

            count = len(announced)
            tab._on_mission_selected(newest.mission_id)  # pylint: disable=protected-access
            assert len(announced) == count

            tab._on_new_mission_requested()  # pylint: disable=protected-access
            assert tab.lead_conversation_path() == ""
            assert announced[-1] == ""

            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())

    def test_service_gives_no_path_for_an_unknown_mission(self, mindspace_sandbox) -> None:  # pylint: disable=unused-argument
        """Asking for a mission that does not exist gives no path rather than an error."""
        service = MissionServiceHolder().service_for(mindspace_sandbox.manager.mindspace())
        assert service.lead_conversation_path("missing") == ""
        MissionServiceHolder().release()


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
        lead_message="" if completed else lead_message, lead_message_time="10:30", awaiting_reply=False,
        completed=completed, report_text="Steps done: 2 of 2  ·  Agents on the team: 1",
        report_message=lead_message if completed else "",
    )


class TestEverythingBanner:
    """While a mission approves everything, a warning that cannot be scrolled away says so and offers to turn it off."""

    def test_banner_shows_only_while_everything_is_on(self, qapp) -> None:  # pylint: disable=unused-argument
        """The banner names the mission at Everything, and is hidden at other levels, in Classic and with no mission."""
        widget = MissionWidget()
        view = _view(completed=False, stage=2)
        everything = dataclasses.replace(view, header=dataclasses.replace(view.header, auto_approve="all"))

        widget.set_mission(everything)
        assert not widget._everything_banner.isHidden()  # pylint: disable=protected-access
        assert "Write the docs" in widget._everything_text.text()  # pylint: disable=protected-access

        widget.set_mission(view)
        assert widget._everything_banner.isHidden()  # pylint: disable=protected-access

        widget.set_mission(dataclasses.replace(
            everything, header=dataclasses.replace(everything.header, is_classic=True)
        ))
        assert widget._everything_banner.isHidden()  # pylint: disable=protected-access

        widget.set_mission(everything)
        widget.set_mission(None)
        assert widget._everything_banner.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_banner_stays_in_view_outside_the_scrolling_page(self, qapp) -> None:  # pylint: disable=unused-argument
        """The banner is not part of the page that scrolls."""
        widget = MissionWidget()
        assert not widget._scroll.isAncestorOf(widget._everything_banner)  # pylint: disable=protected-access
        widget.deleteLater()

    def test_turn_off_asks_for_off(self, qapp) -> None:  # pylint: disable=unused-argument
        """Turn off asks the owner for the Off level, as choosing Off in the level row does."""
        widget = MissionWidget()
        chosen: list[str] = []
        widget.auto_approve_selected.connect(chosen.append)
        widget._everything_off_button.click()  # pylint: disable=protected-access
        assert chosen == ["off"]
        widget.deleteLater()


@pytest.fixture
def still():
    """Reduce motion for one test, and restore it afterwards, as the choice is shared by every view."""
    MissionMotion().set_reduced(True)
    yield
    MissionMotion().set_reduced(False)


class TestUpdatesKeepWhatIsShown:
    """An update rebuilds only what changed, so a busy mission does not stall the interface."""

    def test_new_message_keeps_the_bubbles_already_shown(self, qapp) -> None:  # pylint: disable=unused-argument
        """Adding to the conversation keeps every earlier row; changing the last entry replaces only it."""
        widget = MissionWidget()
        chat = [MissionChatView("user", "You", "Write the docs", "10:00"), MissionChatView("lead", "Theo", "On it.", "10:01")]
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=chat))
        first = [row for _key, row in widget._chat_rows]  # pylint: disable=protected-access

        longer = chat + [MissionChatView("tool", "", "Read docs/a.md", "10:02")]
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=longer))
        rows = [row for _key, row in widget._chat_rows]  # pylint: disable=protected-access
        assert rows[:2] == first and len(rows) == 3
        assert [widget._chat_layout.itemAt(i).widget() for i in range(3)] == rows  # pylint: disable=protected-access

        changed = chat[:1] + [MissionChatView("lead", "Theo", "Done.", "10:03")]
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=changed))
        rows = [row for _key, row in widget._chat_rows]  # pylint: disable=protected-access
        assert rows[0] is first[0] and rows[1] is not first[1] and len(rows) == 2
        assert widget._chat_layout.count() == 2  # pylint: disable=protected-access
        widget.deleteLater()

    def test_new_activity_keeps_the_rows_already_shown(self, qapp) -> None:  # pylint: disable=unused-argument
        """A new entry at the top is the only row made; the rest are kept, in order."""
        widget = MissionWidget()
        older = [MissionActivityView("▶", f"Step {i}", f"10:00:0{i}", MissionTone.INFO) for i in range(3)]
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), activity=older))
        before = [row for _key, row in widget._activity_rows]  # pylint: disable=protected-access

        newer = [MissionActivityView("✓", "Done", "10:00:09", MissionTone.SUCCESS)] + older
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), activity=newer))
        after = [row for _key, row in widget._activity_rows]  # pylint: disable=protected-access
        assert after[1:] == before
        assert [widget._activity_layout.itemAt(i).widget() for i in range(4)] == after  # pylint: disable=protected-access
        widget.deleteLater()

    def test_hidden_pill_stops_pulsing_until_shown(self, qapp) -> None:  # pylint: disable=unused-argument
        """A live pill that is not on screen keeps no timer running."""
        host = QWidget()
        pill = StatusPill(host)
        pill.set_pill("Running", MissionTone.ACCENT, pulsing=True)
        pill._advance_pulse()  # pylint: disable=protected-access
        assert not pill._pulse_timer.isActive()  # pylint: disable=protected-access

        host.show()
        assert pill._pulse_timer.isActive()  # pylint: disable=protected-access
        host.deleteLater()


class TestReducedMotion:
    """With motion reduced, Mission Control draws everything in its final state at once."""

    def test_remarks_are_shown_whole_and_the_agents_stand_still(self, qapp, still) -> None:  # pylint: disable=unused-argument
        """A new remark is not typed out, and nothing animates while nobody is waiting."""
        network = AgentNetworkView()
        network.resize(800, 600)
        working = dataclasses.replace(
            _agent_view("a", parent_id="lead", activity="working"), speech="Writing intro.md for you…"
        )
        network.set_agents([_agent_view("lead", is_lead=True), working], "You")
        network.show()

        assert not network._typing("a")  # pylint: disable=protected-access
        assert not network._timer.isActive()  # pylint: disable=protected-access
        network.deleteLater()

    def test_remark_being_typed_is_finished_when_motion_is_reduced(self, qapp) -> None:  # pylint: disable=unused-argument
        """Turning motion down shows a half-typed remark whole straight away."""
        network = AgentNetworkView()
        network.resize(800, 600)
        working = dataclasses.replace(
            _agent_view("a", parent_id="lead", activity="working"), speech="Writing intro.md for you…"
        )
        network.set_agents([_agent_view("lead", is_lead=True), working], "You")
        assert network._typing("a")  # pylint: disable=protected-access

        MissionMotion().set_reduced(True)
        try:
            network.apply_motion()
            assert not network._typing("a")  # pylint: disable=protected-access

        finally:
            MissionMotion().set_reduced(False)

        network.deleteLater()

    def test_nothing_fades_spins_pulses_glides_or_flies(self, qapp, still) -> None:  # pylint: disable=unused-argument
        """Fades, spinners, pulses, the journey's glide and confetti all stay still."""
        card = QWidget()
        fade_in(card)
        assert card.graphicsEffect() is None

        spinner = BusySpinner(MissionTone.ACCENT)
        spinner.advance()
        assert spinner._angle == 0  # pylint: disable=protected-access

        pill = StatusPill()
        pill.set_pill("Running", MissionTone.ACCENT, pulsing=True)
        assert not pill._pulse_timer.isActive()  # pylint: disable=protected-access

        journey = MissionJourney()
        journey.set_labels(["You asked", "AI plans it", "AI team works", "Job done"])
        journey.show()
        journey.set_stage(2, MissionTone.ACCENT, live=True)
        assert journey._shown_stage == 2  # pylint: disable=protected-access
        assert not journey._timer.isActive()  # pylint: disable=protected-access

        page = QWidget()
        page.resize(400, 300)
        overlay = ConfettiOverlay(page)
        overlay.burst(QPointF(200, 150))
        assert not overlay.isVisible()
        for widget in (card, spinner, pill, journey, page):
            widget.deleteLater()

    def test_checkbox_shows_the_choice_and_reports_changes(self, qapp) -> None:  # pylint: disable=unused-argument
        """Showing the saved choice reports nothing; ticking the box reports the new choice."""
        widget = MissionWidget()
        changes: list[bool] = []
        widget.reduce_motion_toggled.connect(changes.append)
        try:
            widget.set_reduce_motion(True)
            assert widget._reduce_motion.isChecked() and MissionMotion().reduced()  # pylint: disable=protected-access
            assert not changes

            widget._reduce_motion.setChecked(False)  # pylint: disable=protected-access
            assert changes == [False]

        finally:
            widget.set_reduce_motion(False)

        widget.deleteLater()


class TestAgentPictureForEveryone:
    """The agent picture can be left with Tab and is described in words for assistive technology."""

    def test_tab_moves_on_past_the_picture(self, qapp) -> None:  # pylint: disable=unused-argument
        """Tab and Shift+Tab leave the picture for the controls around it; it does not keep the keyboard."""
        host = QWidget()
        layout = QVBoxLayout(host)
        before, network, after = QPushButton("before"), AgentNetworkView(), QPushButton("after")
        for widget in (before, network, after):
            layout.addWidget(widget)

        network.set_agents([_agent_view("lead", is_lead=True), _agent_view("a", parent_id="lead")], "You")
        host.show()
        QTest.qWaitForWindowExposed(host)

        network.setFocus()
        QTest.keyClick(network, Qt.Key.Key_Tab)
        assert QApplication.focusWidget() is after

        network.setFocus()
        QTest.keyClick(network, Qt.Key.Key_Backtab, Qt.KeyboardModifier.ShiftModifier)
        assert QApplication.focusWidget() is before
        host.deleteLater()

    def test_picture_describes_every_agent_and_the_highlighted_one_first(self, qapp) -> None:  # pylint: disable=unused-argument
        """Each agent is described with what it is doing; moving the highlight brings that agent to the front."""
        network = AgentNetworkView()
        lead = dataclasses.replace(_agent_view("lead", is_lead=True), speech="Planning the docs")
        helper = dataclasses.replace(_agent_view("a", parent_id="lead", activity="working"), speech="Writing intro.md")
        network.set_agents([lead, helper], "You")

        description = network.accessibleDescription()
        assert "LEAD" in description and "Planning the docs" in description
        assert "A" in description and "Writing intro.md" in description

        index = next(i for i, node in enumerate(network._nodes) if node.node_id == "a")  # pylint: disable=protected-access
        network._focus_node(index)  # pylint: disable=protected-access
        assert network.accessibleDescription().splitlines()[0].startswith("A ")
        network.deleteLater()


class TestMissionAtAGlance:
    """The mission is summed up at the top, and the details below can be folded away."""

    @staticmethod
    def _snapshot(
        mission: Mission, status: MissionStatus, lead_state: MissionAgentState = MissionAgentState.IDLE,
        conversation: list[MissionExchange] | None = None, decisions: int = 0
    ) -> MissionSnapshot:
        decision = MissionDecision(
            decision_id="d", context_id="lead", conversation_title="lead", mission_id=mission.mission_id,
            tool_name="terminal", reason="Run make", detail=None, destructive=True,
            requested_at=datetime.now(timezone.utc),
        )
        return MissionSnapshot(
            mission=mission, status=status,
            agents=[MissionAgent("lead", "lead", True, lead_state, "")],
            decisions=[decision] * decisions, activity=[], lead_message="", lead_message_at=None,
            conversation=conversation or [],
        )

    @staticmethod
    def _mission() -> Mission:
        return Mission(
            mission_id="m", objective="Write the docs", title="Write the docs", lead_path="lead.conv",
            created_at="2026-11-10T09:00:00+00:00",
            tasks=[
                MissionTask("1", "Outline", "", MissionTaskStatus.COMPLETED),
                MissionTask("2", "Draft", "", MissionTaskStatus.COMPLETED),
                MissionTask("3", "Review", "", MissionTaskStatus.PENDING),
            ],
        )

    def test_glance_says_what_holds_the_mission_up_and_what_it_last_finished(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """Each status that needs the user says so; otherwise nothing holds it up."""
        tab = MissionTab("mission")
        mission = self._mission()

        running = tab._glance_view(self._snapshot(mission, MissionStatus.RUNNING), "")  # pylint: disable=protected-access
        assert running.blocking_text == "Nothing" and running.blocking_tone == MissionTone.SUCCESS
        assert running.last_done_text == "Draft"
        assert running.approvals_text == "None"
        assert running.now_text == "Nothing running"

        waiting = tab._glance_view(self._snapshot(mission, MissionStatus.NEEDS_YOU, decisions=2), "LEAD: Running make")  # pylint: disable=protected-access
        assert waiting.blocking_tone == MissionTone.WARNING
        assert waiting.approvals_text == "2 request(s)"
        assert waiting.now_text == "LEAD: Running make"

        paused = tab._glance_view(self._snapshot(mission, MissionStatus.PAUSED), "")  # pylint: disable=protected-access
        assert paused.blocking_text == "Paused by you"

        mission.tasks[2].status = MissionTaskStatus.BLOCKED
        blocked = tab._glance_view(self._snapshot(mission, MissionStatus.IDLE), "")  # pylint: disable=protected-access
        assert blocked.blocking_text == "Blocked step: Review"

        mission.tasks = []
        mission.history = [MissionPhase("Earlier docs", "", [], "2026-11-09T09:00:00+00:00")]
        fresh = tab._glance_view(self._snapshot(mission, MissionStatus.PLANNING), "")  # pylint: disable=protected-access
        assert fresh.last_done_text == "Earlier docs"
        tab.deleteLater()

    def test_follow_up_to_a_finished_mission_shows_it_is_being_worked_on(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """While the lead works on the user's latest message, the message is named at the top; not before or after."""
        tab = MissionTab("mission")
        mission = self._mission()
        follow_up = [
            MissionExchange(MissionSpeaker.USER, "Write the docs"),
            MissionExchange(MissionSpeaker.LEAD, "All done."),
            MissionExchange(MissionSpeaker.USER, "Now add a changelog\nwith every release"),
        ]

        working = self._snapshot(mission, MissionStatus.RUNNING, MissionAgentState.THINKING, follow_up)
        assert tab._working_on_text(working) == "Working on your message: \u201cNow add a changelog\u201d"  # pylint: disable=protected-access

        # Just sent: the lead has not connected yet, but the message is already being dealt with.
        sent = self._snapshot(mission, MissionStatus.RUNNING, MissionAgentState.IDLE, follow_up)
        assert tab._working_on_text(sent).startswith("Working on your message")  # pylint: disable=protected-access

        for status, state in (
            (MissionStatus.FAILED, MissionAgentState.ERROR), (MissionStatus.STOPPED, MissionAgentState.IDLE),
            (MissionStatus.INTERRUPTED, MissionAgentState.IDLE),
        ):
            assert tab._working_on_text(self._snapshot(mission, status, state, follow_up)) == ""  # pylint: disable=protected-access

        answered = self._snapshot(
            mission, MissionStatus.RUNNING, MissionAgentState.WORKING,
            follow_up + [MissionExchange(MissionSpeaker.LEAD, "Adding it now.")]
        )
        assert tab._working_on_text(answered) == ""  # pylint: disable=protected-access

        first = self._snapshot(mission, MissionStatus.PLANNING, MissionAgentState.THINKING, follow_up[:1])
        assert tab._working_on_text(first) == ""  # pylint: disable=protected-access

        mission.mode = MissionMode.CLASSIC
        classic = tab._mission_view(working)  # pylint: disable=protected-access
        assert classic.glance is None and classic.working_on_text == ""
        tab.deleteLater()

    def test_details_fold_away_and_come_back(self, qapp) -> None:  # pylint: disable=unused-argument
        """Hiding the details leaves the glance, the header and the inbox; showing them brings everything back."""
        widget = MissionWidget()
        glance = MissionGlanceView("LEAD: Drafting", "Nothing", MissionTone.SUCCESS, "None", MissionTone.NEUTRAL, "Outline")
        view = dataclasses.replace(
            _view(completed=False, stage=2), glance=glance,
            results=[MissionResultView("r1", "Round 1", "You asked: docs", "Done.")],
        )
        widget.set_mission(view)
        details = (widget._lower_container, widget._activity_card, widget._results_card)  # pylint: disable=protected-access
        assert not widget._glance_card.isHidden()  # pylint: disable=protected-access
        assert all(not card.isHidden() for card in details)

        widget._details_button.click()  # pylint: disable=protected-access
        assert all(card.isHidden() for card in details)
        assert not widget._glance_card.isHidden() and not widget._inbox_card.isHidden()  # pylint: disable=protected-access

        widget._details_button.click()  # pylint: disable=protected-access
        assert all(not card.isHidden() for card in details)
        widget.deleteLater()

    def test_working_on_strip_names_the_message(self, qapp) -> None:  # pylint: disable=unused-argument
        """The strip shows only while there is a message being worked on."""
        widget = MissionWidget()
        widget.set_mission(dataclasses.replace(
            _view(completed=False, stage=2), working_on_text="Working on your message: \u201cAdd a changelog\u201d"
        ))
        assert not widget._working_on.isHidden()  # pylint: disable=protected-access
        assert "Add a changelog" in widget._working_on_text.text()  # pylint: disable=protected-access

        widget.set_mission(_view(completed=False, stage=2))
        assert widget._working_on.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()


class TestMilestonesAndCelebration:
    """A finished step is reported as progress; only the whole mission is celebrated, once, when watched."""

    @staticmethod
    def _mission(statuses: list[MissionTaskStatus]) -> Mission:
        return Mission(
            mission_id="m", objective="Write the docs", title="Write the docs", lead_path="lead.conv",
            created_at="2026-11-10T09:00:00+00:00",
            tasks=[MissionTask(str(i + 1), f"Step {i + 1}", "", status) for i, status in enumerate(statuses)],
        )

    @staticmethod
    def _snapshot(mission: Mission, status: MissionStatus) -> MissionSnapshot:
        return MissionSnapshot(
            mission=mission, status=status, agents=[], decisions=[], activity=[], lead_message="All done.",
            lead_message_at=None,
        )

    def test_finished_step_says_what_comes_next(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """A step done while others remain is reported with the next one, never as the mission done."""
        tab = MissionTab("mission")
        done, active, pending = MissionTaskStatus.COMPLETED, MissionTaskStatus.IN_PROGRESS, MissionTaskStatus.PENDING
        going = self._snapshot(self._mission([done, active, pending]), MissionStatus.RUNNING)
        assert tab._milestone_text(going) == "Step 1 of 3 done: Step 1  ·  Next: Step 2"  # pylint: disable=protected-access

        finished = self._snapshot(self._mission([done, done]), MissionStatus.COMPLETED)
        assert tab._milestone_text(finished) == ""  # pylint: disable=protected-access

        # Every step done, but the goal not yet met: the lead is planning what is left, not finished.
        short_of_goal = self._snapshot(self._mission([done, done]), MissionStatus.RUNNING)
        assert tab._milestone_text(short_of_goal) == (  # pylint: disable=protected-access
            "Step 2 of 2 done: Step 2  ·  Next: planning what is left of the goal"
        )

        asked_more = self._mission([done, done])
        asked_more.follow_up = "Now add a changelog"
        assert tab._milestone_text(self._snapshot(asked_more, MissionStatus.PLANNING)) == (  # pylint: disable=protected-access
            "Step 2 of 2 done: Step 2  ·  Next: planning your new request"
        )

        new_round = self._mission([pending])
        new_round.history = [MissionPhase("Write the guide", "", [], "2026-11-09T09:00:00+00:00")]
        assert tab._milestone_text(self._snapshot(new_round, MissionStatus.RUNNING)) == (  # pylint: disable=protected-access
            "Finished: Write the guide  ·  Next: Step 1"
        )
        assert tab._milestone_text(self._snapshot(self._mission([pending]), MissionStatus.RUNNING)) == ""  # pylint: disable=protected-access
        tab.deleteLater()

    def test_completion_is_celebrated_once_and_only_when_watched(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """Opening a finished mission does not celebrate; watching it finish does, once; finishing again does not."""
        tab = MissionTab("mission")
        tab._widget.isVisible = lambda: True  # type: ignore[method-assign]  # pylint: disable=protected-access
        marked: list[str] = []
        service = types.SimpleNamespace(mark_celebrated=marked.append)
        mission = self._mission([MissionTaskStatus.COMPLETED])

        def shown(status: MissionStatus) -> bool:
            snapshot = self._snapshot(mission, status)
            view = tab._with_celebration(service, snapshot, tab._mission_view(snapshot))  # type: ignore[arg-type]  # pylint: disable=protected-access
            return view.celebrate

        assert not shown(MissionStatus.COMPLETED)
        assert not shown(MissionStatus.RUNNING)
        assert shown(MissionStatus.COMPLETED)
        assert marked == ["m"]

        mission.celebrated = True
        assert not shown(MissionStatus.RUNNING)
        assert not shown(MissionStatus.COMPLETED)
        assert marked == ["m"]
        tab.deleteLater()

    def test_milestone_is_shown_on_the_mission_card(self, qapp) -> None:  # pylint: disable=unused-argument
        """The line appears with its text, and goes when there is none."""
        widget = MissionWidget()
        view = _view(completed=False, stage=2)
        widget.set_mission(dataclasses.replace(
            view, header=dataclasses.replace(view.header, milestone_text="Step 1 of 3 done: Outline")
        ))
        assert not widget._milestone_row.isHidden()  # pylint: disable=protected-access
        assert widget._milestone_text.text() == "Step 1 of 3 done: Outline"  # pylint: disable=protected-access

        widget.set_mission(view)
        assert widget._milestone_row.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()


class TestApprovalComprehension:
    """A destructive request shows its target and whether it can be undone before it can be approved."""

    @staticmethod
    def _decision(destructive: bool, reversible: bool, target: str = "docs/a.md") -> MissionDecision:
        return MissionDecision(
            decision_id="d", context_id="c", conversation_title="Lead", mission_id=None,
            tool_name="filesystem.delete_file", reason="Delete docs/a.md", detail=None, destructive=destructive,
            requested_at=datetime.now(timezone.utc), reversible=reversible, target=target,
        )

    def test_view_names_the_target_and_says_whether_it_can_be_undone(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """Every request names its target; only a destructive one says whether it can be undone."""
        tab = MissionTab("mission")
        undoable = tab._decision_view(self._decision(destructive=True, reversible=True))  # pylint: disable=protected-access
        permanent = tab._decision_view(self._decision(destructive=True, reversible=False))  # pylint: disable=protected-access
        harmless = tab._decision_view(self._decision(destructive=False, reversible=False))  # pylint: disable=protected-access
        untargeted = tab._decision_view(self._decision(destructive=False, reversible=False, target=""))  # pylint: disable=protected-access

        assert undoable.target_text == "Acts on: docs/a.md"
        assert undoable.undo_text.startswith("Can be undone")
        assert permanent.undo_text.startswith("Cannot be undone")
        assert harmless.target_text and not harmless.undo_text
        assert not untargeted.target_text
        tab.deleteLater()

    def test_inbox_card_shows_the_target_and_undo_before_approve(self, qapp) -> None:  # pylint: disable=unused-argument
        """The card carries both facts as text the user can read and select."""
        widget = MissionWidget()
        widget.set_decisions([dataclasses.replace(
            _decision(destructive=True), target_text="Acts on: docs/a.md",
            undo_text="Cannot be undone by Humbug once approved."
        )])
        target = widget.findChildren(QLabel, "MissionDecisionTarget")
        undo = widget.findChildren(QLabel, "MissionDecisionUndo")
        assert [label.text() for label in undo] == ["Cannot be undone by Humbug once approved."]
        assert len(target) == 1 and "docs/a.md" in target[0].text().replace("\u200b", "")
        widget.deleteLater()

    def test_agent_card_sends_a_destructive_request_to_the_inbox(self, qapp) -> None:  # pylint: disable=unused-argument
        """A destructive request offers Review, which opens it in the inbox and approves nothing."""
        network = AgentNetworkView()
        network.resize(800, 600)
        approved: list[str] = []
        shown: list[str] = []
        network.decision_approved.connect(approved.append)
        network.decision_details_requested.connect(shown.append)
        waiting = dataclasses.replace(
            _agent_view("a", parent_id="lead", activity="waiting"),
            speech="I need your approval: Delete docs/a.md", decision_id="decision-1", decision_destructive=True,
        )
        network.set_agents(
            [_agent_view("lead", is_lead=True), waiting], "You", "Approve", "Reject", review_label="Review"
        )
        _key, buttons = network._buttons["a"]  # pylint: disable=protected-access
        assert [button.text() for button in buttons][0] == "Review"
        assert "Approve" not in [button.text() for button in buttons]

        buttons[0].click()
        assert shown == ["decision-1"] and not approved
        network.deleteLater()


class TestPauseControls:
    """A working mission offers Pause; a paused one says so and offers Continue."""

    def test_pause_is_offered_while_working_and_continue_while_paused(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """The header offers Pause to a running mission, Continue to a paused one, and neither in Classic."""
        tab = MissionTab("mission")
        mission = Mission(
            mission_id="m", objective="Write the docs", title="Write the docs", lead_path="lead.conv",
            created_at="2026-11-10T09:00:00+00:00",
        )

        def header(status: MissionStatus) -> MissionHeaderView:
            snapshot = MissionSnapshot(
                mission=mission, status=status, agents=[], decisions=[], activity=[],
                lead_message="", lead_message_at=None,
            )
            return tab._mission_view(snapshot).header  # pylint: disable=protected-access

        running = header(MissionStatus.RUNNING)
        assert running.can_pause and not running.paused

        mission.paused = True
        paused = header(MissionStatus.PAUSED)
        assert paused.paused and not paused.can_pause and paused.can_stop
        assert paused.status_text == "Paused"

        mission.mode = MissionMode.CLASSIC
        classic = header(MissionStatus.PAUSED)
        assert not classic.paused and not classic.can_pause
        tab.deleteLater()

    def test_paused_mission_shows_continue_and_says_why(self, qapp) -> None:  # pylint: disable=unused-argument
        """Continue and the paused notice replace Pause, and each button asks for its action."""
        widget = MissionWidget()
        view = _view(completed=False, stage=2)
        widget.set_mission(dataclasses.replace(view, header=dataclasses.replace(view.header, can_pause=True)))
        assert not widget._pause_button.isHidden()  # pylint: disable=protected-access
        assert widget._unpause_button.isHidden() and widget._paused_notice.isHidden()  # pylint: disable=protected-access

        asked: list[str] = []
        widget.pause_requested.connect(lambda: asked.append("pause"))
        widget.unpause_requested.connect(lambda: asked.append("continue"))
        widget._pause_button.click()  # pylint: disable=protected-access

        widget.set_mission(dataclasses.replace(view, header=dataclasses.replace(view.header, paused=True)))
        assert widget._pause_button.isHidden()  # pylint: disable=protected-access
        assert not widget._unpause_button.isHidden() and not widget._paused_notice.isHidden()  # pylint: disable=protected-access
        widget._unpause_button.click()  # pylint: disable=protected-access
        assert asked == ["pause", "continue"]
        widget.deleteLater()


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
        """Once done, the lead's last message is shown as the final report, and the moment is celebrated when asked."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        widget.set_mission(dataclasses.replace(_view(completed=True, stage=3), celebrate=True))

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
            resume_automatically=lambda: False, reduce_motion=lambda: False,
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
            resume_automatically=lambda: False, reduce_motion=lambda: False, decisions=lambda: decisions,
            lead_conversation_path=lambda mission_id: f"/{mission_id}.conv",
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



def _helper(actions: int = 0, last_tool: str = "", target: str = "", is_lead: bool = False) -> MissionAgent:
    """Return an agent thinking between steps, having taken a number of them."""
    return MissionAgent(
        context_id="h", title="dAI-h", is_lead=is_lead, state=MissionAgentState.THINKING, tool_name="",
        actions=actions, last_tool_name=last_tool, last_tool_target=target, model="Claude Sonnet 4.5",
    )


class TestDashboardOrder:
    """The dashboard shows the team before the plan."""

    def test_agents_come_before_the_plan(self, qapp) -> None:  # pylint: disable=unused-argument
        """The Agents card sits above the Mission Plan."""
        widget = MissionWidget()
        layout = widget._agents_card.parentWidget().layout()  # pylint: disable=protected-access
        agents = layout.indexOf(widget._agents_card)  # pylint: disable=protected-access
        plan = layout.indexOf(widget._lower_container)  # pylint: disable=protected-access
        assert 0 <= agents < plan
        widget.deleteLater()


class TestMissionConversationCard:
    """The conversation with the lead is shown like a conversation tab, without its input."""

    _CHAT = [
        MissionChatView("user", "You", "What does *it* do?", "10:00"),
        MissionChatView("lead", "Theo", "It tracks **missions**.", "10:01", "DeepSeek V4.1 Flash"),
        MissionChatView("tool", "", "filesystem.read_file · README.md", "10:01"),
    ]

    def test_messages_replies_and_tools_are_shown(self, qapp) -> None:  # pylint: disable=unused-argument
        """The user's message keeps what they typed, the lead's reply is formatted, and a tool gets a line."""
        widget = MissionWidget()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=self._CHAT))

        assert not widget._chat_card.isHidden()  # pylint: disable=protected-access
        card = widget._chat_card  # pylint: disable=protected-access
        user = card.findChild(QFrame, "MissionChatUser")
        lead = card.findChild(QFrame, "MissionChatLead")
        user_text = [label for label in user.findChildren(QLabel) if label.objectName() == "MissionLeadText"][0]
        lead_text = [label for label in lead.findChildren(QLabel) if label.objectName() == "MissionLeadText"][0]
        assert user_text.text() == "What does *it* do?"
        assert user_text.textFormat() == Qt.TextFormat.PlainText
        assert lead_text.textFormat() == Qt.TextFormat.MarkdownText
        assert any("filesystem.read_file · README.md" in label.text() for label in card.findChildren(QLabel))
        assert widget._chat_pill._text == "1"  # pylint: disable=protected-access
        widget.deleteLater()

    def test_reply_shows_the_model_beside_the_agent_name(self, qapp) -> None:  # pylint: disable=unused-argument
        """The lead's reply is tagged with the model that wrote it; the user's message has no tag."""
        widget = MissionWidget()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=self._CHAT))
        card = widget._chat_card  # pylint: disable=protected-access
        lead = card.findChild(QFrame, "MissionChatLead")
        user = card.findChild(QFrame, "MissionChatUser")
        tag = lead.findChild(QLabel, "MissionChatModel")
        assert tag is not None and tag.text() == "DeepSeek V4.1 Flash"
        assert user.findChild(QLabel, "MissionChatModel") is None
        widget.deleteLater()

    def test_model_name_is_shown_whole_when_there_is_room(self, qapp) -> None:  # pylint: disable=unused-argument
        """The tag's padding is allowed for, so a model name that fits is not cut short with an ellipsis."""
        widget = MissionWidget()
        widget.apply_style()
        widget.resize(1500, 1100)
        widget.show()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=self._CHAT))
        for _ in range(10):
            QApplication.processEvents()

        tag = widget._chat_card.findChild(QLabel, "MissionChatModel")  # pylint: disable=protected-access
        assert QLabel.text(tag) == "DeepSeek V4.1 Flash"
        widget.deleteLater()

    def test_mediator_passes_on_the_model(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """The model recorded with a reply reaches its display record."""
        tab = MissionTab("mission")
        view = tab._chat_view(MissionExchange(MissionSpeaker.LEAD, "Hi", None, "DeepSeek"), "Theo")  # pylint: disable=protected-access
        assert view.model_text == "DeepSeek"
        tab.deleteLater()

    def test_conversation_is_always_open(self, qapp) -> None:  # pylint: disable=unused-argument
        """The conversation is shown in full as soon as there is one, with no heading to click to open it."""
        widget = MissionWidget()
        widget.resize(1300, 1200)
        widget.show()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=self._CHAT))
        QApplication.processEvents()
        assert widget._chat_scroll.isVisible()  # pylint: disable=protected-access
        assert not hasattr(widget, "_toggle_chat")
        widget.deleteLater()

    def test_crowded_reply_keeps_the_height_its_text_needs(self, qapp) -> None:  # pylint: disable=unused-argument
        """Each reply holds its own height, so no layout can squeeze it into a single clipped line."""
        widget = MissionWidget()
        widget.apply_style()
        widget.resize(1300, 1200)
        widget.show()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=self._CHAT))
        for _ in range(10):
            QApplication.processEvents()

        labels = widget._chat_card.findChildren(QLabel, "MissionLeadText")  # pylint: disable=protected-access
        assert len(labels) == 2
        for label in labels:
            assert label.minimumHeight() > 0
            assert label.minimumHeight() == label.heightForWidth(label.width())

        widget.deleteLater()

    def test_conversation_scrolls_beyond_its_maximum_height(self, qapp) -> None:  # pylint: disable=unused-argument
        """A short conversation takes only the room it needs; a long one stops at a maximum and scrolls."""
        widget = MissionWidget()
        widget.apply_style()
        widget.resize(1300, 1200)
        widget.show()
        scroll = widget._chat_scroll  # pylint: disable=protected-access

        def show(conversation: list[MissionChatView]) -> None:
            widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=conversation))
            for _ in range(10):
                QApplication.processEvents()

        show(self._CHAT)
        assert 0 < scroll.height() < scroll.maximumHeight()
        assert scroll.verticalScrollBar().maximum() == 0

        essay = "# Essay\n\n" + "A long paragraph. " * 600
        show(self._CHAT + [MissionChatView("lead", "Theo", essay, "10:02")])
        bar = scroll.verticalScrollBar()
        assert scroll.height() == scroll.maximumHeight()
        assert bar.maximum() > 0
        assert bar.value() == bar.maximum()
        widget.deleteLater()

    def test_conversation_fits_its_content_after_it_changes(self, qapp) -> None:  # pylint: disable=unused-argument
        """No reply is cut off, and nothing is left blank below the newest entry when the content shrinks."""
        StyleManager().set_zoom(1.4)
        widget = MissionWidget()
        widget.apply_style()
        widget.resize(1500, 1100)
        widget.show()
        reply = (
            "Everything is wired correctly. Now let me write the CHANGELOG entry and add entries for the three changes."
        )
        chat = [MissionChatView("user", "You", "Please review this PR", "20:40")]
        for _ in range(8):
            chat.append(MissionChatView("lead", "Orion", reply, "20:52", "DeepSeek V4.1 Flash"))
            chat.append(MissionChatView("tool", "", "filesystem.read_file_lines · CHANGELOG.md", "20:52"))

        def show(conversation: list[MissionChatView]) -> None:
            widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=conversation))
            for _ in range(10):
                QApplication.processEvents()

        try:
            show(chat + [MissionChatView("lead", "Orion", "A long paragraph. " * 400, "20:53")])
            show(chat[:5])
            feed = widget._chat_scroll.widget()  # pylint: disable=protected-access
            layout = feed.layout()
            last = layout.itemAt(layout.count() - 1).widget()
            assert feed.height() - last.geometry().bottom() <= 2
            for label in widget._chat_card.findChildren(QLabel, "MissionLeadText"):  # pylint: disable=protected-access
                if label.isVisible():
                    assert label.height() >= label.heightForWidth(label.width())

        finally:
            StyleManager().set_zoom(1.0)
            widget.deleteLater()

    def test_wrapped_text_tells_its_layout_the_height_it_needs(self, qapp) -> None:  # pylint: disable=unused-argument
        """Every reply and tool line reports its height for its width, so layouts never guess it and clip it."""
        widget = MissionWidget()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), conversation=self._CHAT))
        labels = [
            label for label in widget._chat_card.findChildren(QLabel)  # pylint: disable=protected-access
            if label.wordWrap() and label is not widget._chat_hint  # pylint: disable=protected-access
        ]
        assert len(labels) == 3
        for label in labels:
            assert label.sizePolicy().hasHeightForWidth()

        widget.deleteLater()

    def test_conversation_sits_below_the_activity(self, qapp) -> None:  # pylint: disable=unused-argument
        """The conversation is in the side column, directly under Recent Activity."""
        widget = MissionWidget()
        side = widget._side_column.layout()  # pylint: disable=protected-access
        activity = side.indexOf(widget._activity_card)  # pylint: disable=protected-access
        assert side.indexOf(widget._chat_card) == activity + 1  # pylint: disable=protected-access
        widget.set_mission(None)
        assert widget._chat_card.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_card_is_hidden_without_a_conversation_and_in_classic_mode(self, qapp) -> None:  # pylint: disable=unused-argument
        """There is nothing to show before anything is said, and a classic mission has its own tab."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        assert widget._chat_card.isHidden()  # pylint: disable=protected-access

        view = _view(completed=False, stage=2)
        classic = dataclasses.replace(
            view, header=dataclasses.replace(view.header, is_classic=True), conversation=self._CHAT
        )
        widget.set_mission(classic)
        assert widget._chat_card.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_lead_message_card_fits_each_new_message(self, qapp) -> None:  # pylint: disable=unused-argument
        """The card grows for a long message and shrinks back for a short one that follows it."""
        widget = MissionWidget()
        widget.apply_style()
        widget.resize(1300, 1200)
        widget.show()

        def shown_height(message: str) -> int:
            widget.set_mission(_view(completed=False, stage=2, lead_message=message))
            for _ in range(5):
                QApplication.processEvents()

            return widget._lead_body.height()  # pylint: disable=protected-access

        short = shown_height("Short reply.")
        long = shown_height("# Essay\n\n" + "A long paragraph. " * 600)
        after = shown_height("Short again.")
        body = widget._lead_body  # pylint: disable=protected-access
        assert long > short * 10
        assert after == short
        assert body.height() == body.heightForWidth(body.width())
        widget.deleteLater()

    def test_mediator_names_who_said_what(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """The user is "You", the lead goes by its name, and a tool has no speaker."""
        tab = MissionTab("mission")
        at = datetime(2026, 11, 8, 9, 30, tzinfo=timezone.utc)
        views = [
            tab._chat_view(MissionExchange(speaker, "text", at), "Theo")  # pylint: disable=protected-access
            for speaker in (MissionSpeaker.USER, MissionSpeaker.LEAD, MissionSpeaker.TOOL)
        ]
        assert [(v.kind, v.speaker) for v in views] == [("user", "You"), ("lead", "Theo"), ("tool", "")]
        assert views[0].time_text == at.astimezone().strftime("%H:%M")
        assert tab._chat_view(MissionExchange(MissionSpeaker.LEAD, "x"), "Theo").time_text == ""  # pylint: disable=protected-access
        tab.deleteLater()

    def test_long_lead_message_is_shown_whole(self, qapp) -> None:  # pylint: disable=unused-argument
        """The lead's latest message and the final report are never cut short."""
        essay = "# Essay\n\n" + "A long paragraph. " * 800 + "\n\nThe end."
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2, lead_message=essay))
        assert widget._lead_body.text() == essay  # pylint: disable=protected-access

        widget.set_mission(_view(completed=True, stage=3, lead_message=essay))
        assert widget._report_body.text() == essay  # pylint: disable=protected-access
        widget.deleteLater()


class TestAnsweredQuestions:
    """A question the lead answered is shown as answered, not as work waiting for the user."""

    @staticmethod
    def _view(status: MissionStatus, tasks: list[MissionTask], lead_message: str, report: str = "") -> MissionView:
        tab = MissionTab("mission")
        mission = Mission(
            mission_id="m", objective="What does it do?", title="What does it do?", lead_path="lead.conv",
            created_at="2026-11-08T09:00:00+00:00", tasks=tasks, report=report,
        )
        snapshot = MissionSnapshot(
            mission=mission, status=status, agents=[], decisions=[], activity=[],
            lead_message=lead_message, lead_message_at=None,
        )
        view = tab._mission_view(snapshot)  # pylint: disable=protected-access
        tab.deleteLater()
        return view

    def test_answer_is_shown_as_answered_and_done(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """The answer is the lead's message, without asking the user to reply, and the journey is done."""
        view = self._view(MissionStatus.REPLIED, [], "It tracks missions.")

        assert view.header.status_text == "Answered"
        assert view.header.tone == MissionTone.SUCCESS
        assert view.header.stage == 3
        assert not view.header.can_stop
        assert view.lead_message == "It tracks missions."
        assert not view.awaiting_reply
        assert not view.completed and view.report_message == ""
        assert not view.steps

    def test_failed_mission_is_shown_as_failed_not_done(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """A failed lead turn shows as failed, in red, where the mission had got to, without a final report."""
        view = self._view(MissionStatus.FAILED, [], "Looking at the PR.")

        assert view.header.status_text == "Failed"
        assert view.header.tone == MissionTone.DANGER
        assert view.header.stage_tone == MissionTone.DANGER
        assert view.header.stage == 1
        assert not view.completed and view.report_message == ""
        assert not view.awaiting_reply
        assert mission_tab_module._stage(MissionStatus.FAILED, has_plan=True) == 2  # pylint: disable=protected-access

    def test_answered_stage_is_the_last(self) -> None:
        """An answered question needs no plan, so it is done as soon as it is answered."""
        assert mission_tab_module._stage(MissionStatus.REPLIED, has_plan=False) == 3  # pylint: disable=protected-access

    def test_finished_mission_shows_its_last_message_as_the_report(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """Without a follow-up, the lead's last message is the final report and is not repeated above it."""
        done = [MissionTask("1", "Write", "", MissionTaskStatus.COMPLETED)]
        view = self._view(MissionStatus.COMPLETED, done, "All done.")

        assert view.completed
        assert view.report_message == "All done."
        assert view.lead_message == ""

    def test_answer_to_a_question_after_the_mission_keeps_the_report(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """A question asked once the mission is done is answered on its own; the final report stays as it was."""
        done = [MissionTask("1", "Write", "", MissionTaskStatus.COMPLETED)]
        view = self._view(MissionStatus.COMPLETED, done, "docs/README.md", report="All done.")

        assert view.completed
        assert view.report_message == "All done."
        assert view.lead_message == "docs/README.md"

    def test_widget_shows_the_answer_beside_the_report(self, qapp) -> None:  # pylint: disable=unused-argument
        """The widget shows the report and the later answer each in its own card."""
        widget = MissionWidget()
        view = dataclasses.replace(
            _view(completed=True, stage=3), lead_message="docs/README.md", report_message="All done."
        )
        widget.set_mission(view)

        assert not widget._report_card.isHidden()  # pylint: disable=protected-access
        assert widget._report_body.text() == "All done."  # pylint: disable=protected-access
        assert not widget._lead_card.isHidden()  # pylint: disable=protected-access
        assert widget._lead_body.text() == "docs/README.md"  # pylint: disable=protected-access
        widget.deleteLater()


class TestAgentProgressSpeech:
    """An agent introduces itself once, then tells the user what it has done and how far it has got."""

    @staticmethod
    def _speech(agent: MissionAgent, task_title: str = "") -> str:
        tab = MissionTab("mission")
        mission = types.SimpleNamespace(tasks=[])
        snapshot = types.SimpleNamespace(mission=mission, status=MissionStatus.RUNNING, lead_message="")
        speech = tab._agent_speech(agent, "Mira", task_title, snapshot, None, [])  # type: ignore[arg-type]  # pylint: disable=protected-access
        tab.deleteLater()
        return speech

    def test_new_helper_introduces_itself(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """Before its first step, a helper says hello."""
        assert self._speech(_helper()) == "Hi, I'm Mira! Getting started on my part…"

    def test_helper_does_not_introduce_itself_again(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """Between steps, a helper says what it just did instead of saying hello again."""
        speech = self._speech(_helper(3, "filesystem.read_file", "src/pairing/handshake.py"))
        assert speech == "Read handshake.py, now working out what's next… (3 actions so far)"

    def test_progress_is_shown_even_with_a_plan_step(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """A helper on a plan step also reports its latest step, once it has one."""
        speech = self._speech(_helper(1, "terminal.send_command", "pytest -q"), task_title="Write the tests")
        assert speech == "Ran pytest -q, now reading the result… (1 action so far)"

    def test_lead_reports_its_progress_too(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """The lead, once it has acted, says what it did rather than repeating that it is planning."""
        speech = self._speech(_helper(2, "delegate_ai", "", is_lead=True))
        assert speech == "My helper reported back, now reading what they found… (2 actions so far)"

    @pytest.mark.parametrize("tool, target, expected", [
        ("filesystem.write_file", "docs/a.md", "Wrote a.md, now checking it and moving on…"),
        ("filesystem.delete_file", "old.txt", "Removed old.txt, now moving on…"),
        ("filesystem.move_file", "", "Done with the files, now working out what's next…"),
        ("editor.apply_diff", "src/app.py", "Edited app.py, now checking it and moving on…"),
        ("http.get", "https://example.com", "Got an answer from the web, now reading it…"),
        ("clock", "", "Finished with clock, now working out what's next…"),
    ])
    def test_each_kind_of_step_is_told(self, mindspace_sandbox, qapp, tool, target, expected) -> None:  # pylint: disable=unused-argument
        """Every kind of tool has its own words for having been used."""
        tab = MissionTab("mission")
        assert tab._tool_speech(tool, target, done=True) == expected  # pylint: disable=protected-access
        tab.deleteLater()

    def test_agent_view_carries_the_model(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """The model each agent uses reaches its display record."""
        tab = MissionTab("mission")
        view = tab._agent_view(_helper(), True, "Mira", "")  # pylint: disable=protected-access
        assert view.model_text == "Claude Sonnet 4.5"
        tab.deleteLater()


class TestAgentModel:
    """The model each agent uses is shown beside it, in the picture and in the list."""

    def test_list_names_each_agents_model(self, qapp) -> None:  # pylint: disable=unused-argument
        """The agents list says which model each agent uses after its role."""
        widget = MissionWidget()
        agent = dataclasses.replace(_agent_view("a", parent_id="lead"), model_text="Claude Sonnet 4.5")
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), agents=[_agent_view("lead", is_lead=True), agent]))
        roles = [label.text() for label in widget.findChildren(QLabel, "MissionAgentRole")]
        assert "role  ·  Claude Sonnet 4.5" in roles
        assert "role" in roles
        widget.deleteLater()

    def test_picture_draws_the_model_tag(self, qapp) -> None:  # pylint: disable=unused-argument
        """An agent with a known model has a tag under its name; the space for it is kept either way."""
        def painted(model: str) -> tuple[QImage, int]:
            network = AgentNetworkView()
            agent = dataclasses.replace(_agent_view("a", parent_id="lead"), model_text=model)
            network.set_agents([_agent_view("lead", is_lead=True), agent], "You")
            network.resize(600, network.sizeHint().height())
            image = QImage(network.size(), QImage.Format.Format_ARGB32)
            image.fill(Qt.GlobalColor.transparent)
            network.render(image, QPoint(), QRegion(), QWidget.RenderFlag.DrawChildren)
            height = network.sizeHint().height()
            network.deleteLater()
            return image, height

        with_model, height_with = painted("Claude Sonnet 4.5")
        without_model, height_without = painted("")
        assert with_model != without_model
        assert height_with == height_without

    def test_hover_names_the_model(self, qapp) -> None:  # pylint: disable=unused-argument
        """The tooltip over an agent includes its model."""
        network = AgentNetworkView()
        agent = dataclasses.replace(_agent_view("a", parent_id="lead"), model_text="Claude Sonnet 4.5")
        network.set_agents([_agent_view("lead", is_lead=True), agent], "You")
        network.resize(600, network.sizeHint().height())
        node = next(n for n in network._nodes if n.node_id == "a")  # pylint: disable=protected-access
        shown: list[str] = []
        move = QMouseEvent(
            QEvent.Type.MouseMove, node.center, network.mapToGlobal(node.center), Qt.MouseButton.NoButton,
            Qt.MouseButton.NoButton, Qt.KeyboardModifier.NoModifier
        )
        with mock.patch.object(mission_network_module.QToolTip, "showText", lambda _pos, text, *_args: shown.append(text)):
            network.mouseMoveEvent(move)

        assert shown and "Claude Sonnet 4.5" in shown[-1]
        network.deleteLater()



def _results() -> list[MissionResultView]:
    return [
        MissionResultView("2:b", "Round 2  ·  2026-11-08 10:00  ·  2 of 2 steps done", "You asked: Add a changelog",
                          "**Changelog added.** See CHANGELOG.md."),
        MissionResultView("1:a", "Round 1  ·  2026-11-08 09:00  ·  3 of 3 steps done", "You asked: Write the docs", ""),
    ]


class TestEarlierResultsCard:
    """Earlier rounds stay on screen after a follow-up, each opening to its full report."""

    def test_card_is_hidden_until_there_is_an_earlier_round(self, qapp) -> None:  # pylint: disable=unused-argument
        """A mission on its first round has no earlier results to show."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        assert widget._results_card.isHidden()  # pylint: disable=protected-access

        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), results=_results()))
        assert not widget._results_card.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_rounds_are_listed_newest_first_with_what_was_asked(self, qapp) -> None:  # pylint: disable=unused-argument
        """Each round shows its heading, the request and the first line of its report until opened."""
        widget = MissionWidget()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), results=_results()))
        texts = [label.text() for label in widget._results_card.findChildren(QLabel)]  # pylint: disable=protected-access
        assert texts.index(_results()[0].heading) < texts.index(_results()[1].heading)
        assert "You asked: Add a changelog" in texts
        assert "Changelog added. See CHANGELOG.md." in texts
        assert "The lead gave no final report for this round." in texts
        widget.deleteLater()

    def test_a_round_opens_to_its_full_report_and_closes_again(self, qapp) -> None:  # pylint: disable=unused-argument
        """Clicking a round shows the whole report as formatted text; clicking again folds it."""
        widget = MissionWidget()
        widget.set_mission(dataclasses.replace(_view(completed=False, stage=2), results=_results()))

        widget._toggle_result("2:b")  # pylint: disable=protected-access
        bodies = [l for l in widget._results_card.findChildren(QLabel, "MissionLeadText") if not l.isHidden()]  # pylint: disable=protected-access
        assert [b.text() for b in bodies] == ["**Changelog added.** See CHANGELOG.md."]
        assert bodies[0].textFormat() == Qt.TextFormat.MarkdownText

        widget._toggle_result("2:b")  # pylint: disable=protected-access
        QApplication.processEvents()
        layout = widget._results_layout  # pylint: disable=protected-access
        shown = [layout.itemAt(i).widget() for i in range(layout.count())]
        assert all(not row.findChildren(QLabel, "MissionLeadText") for row in shown)
        widget.deleteLater()

    def test_mediator_turns_history_into_rounds(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """Rounds are numbered in order, listed newest first, and plans kept before results were recorded are left out."""
        tab = MissionTab("mission")
        done = [MissionTask("1", "Write", "", MissionTaskStatus.COMPLETED)]
        phases = [
            MissionPhase("Old", "", done, "2026-11-08T08:00:00+00:00"),
            MissionPhase("Docs", "", done, "2026-11-08T09:00:00+00:00", request="Write the docs\nPlease", report="Done."),
            MissionPhase("Log", "", done, "2026-11-08T10:00:00+00:00", request="Add a changelog", report=""),
        ]
        views = [tab._result_view(number, phase) for number, phase in enumerate(phases, start=1)]  # pylint: disable=protected-access
        assert views[1].heading.startswith("Round 2  ·  ")
        assert views[1].heading.endswith("1 of 1 steps done")
        assert views[1].request_text == "You asked: Write the docs"
        assert views[1].report == "Done."
        assert views[0].request_text == ""
        tab.deleteLater()


class TestFailedMissionRecovery:
    """A mission whose lead failed offers a way to find out why and to try again."""

    @staticmethod
    def _failed_view(lead_context_id: str) -> MissionView:
        header = MissionHeaderView(
            title="Review the PR", summary="", status_text="Failed", tone=MissionTone.DANGER,
            progress=0.0, progress_text="", can_stop=False, can_resume=False, is_classic=False,
            live=False, auto_approve="off", stage=2, stage_tone=MissionTone.DANGER,
            lead_context_id=lead_context_id, can_retry=True,
        )
        return MissionView(
            header=header, stats=[], steps=[], agents=[], agents_badge="", activity=[],
            lead_message="", lead_message_time="", awaiting_reply=False,
        )

    def test_failed_mission_is_retried_and_interrupted_one_resumed(self, mindspace_sandbox, qapp) -> None:  # pylint: disable=unused-argument
        """A failed mission offers Retry and an interrupted one Resume, never the other's action."""
        tab = MissionTab("mission")
        mission = Mission(
            mission_id="m", objective="Review the PR", title="Review the PR", lead_path="lead.conv",
            created_at="2026-11-08T09:00:00+00:00",
        )
        for status, can_retry, can_resume in (
            (MissionStatus.FAILED, True, False), (MissionStatus.INTERRUPTED, False, True)
        ):
            snapshot = MissionSnapshot(
                mission=mission, status=status, agents=[], decisions=[], activity=[],
                lead_message="", lead_message_at=None,
            )
            header = tab._mission_view(snapshot).header  # pylint: disable=protected-access
            assert (header.can_retry, header.can_resume) == (can_retry, can_resume)

        tab.deleteLater()

    def test_failed_mission_shows_retry_without_the_interrupted_notice(self, qapp) -> None:  # pylint: disable=unused-argument
        """Retry asks for the request to be sent again; Resume and the interruption notice stay hidden."""
        widget = MissionWidget()
        widget.set_mission(self._failed_view("lead-ctx"))

        assert not widget._retry_button.isHidden()  # pylint: disable=protected-access
        assert widget._resume_button.isHidden()  # pylint: disable=protected-access
        assert widget._interrupted_notice.isHidden()  # pylint: disable=protected-access

        retried: list[bool] = []
        widget.retry_requested.connect(lambda: retried.append(True))
        widget._retry_button.click()  # pylint: disable=protected-access
        assert retried == [True]
        widget.deleteLater()

    def test_failed_mission_offers_its_lead_conversation(self, qapp) -> None:  # pylint: disable=unused-argument
        """The notice and the button appear only for a failed mission, and the button opens the lead."""
        widget = MissionWidget()
        widget.set_mission(self._failed_view("lead-ctx"))

        assert not widget._failed_notice.isHidden()  # pylint: disable=protected-access
        assert not widget._lead_conversation_button.isHidden()  # pylint: disable=protected-access

        opened: list[str] = []
        widget.conversation_requested.connect(opened.append)
        widget._lead_conversation_button.click()  # pylint: disable=protected-access
        assert opened == ["lead-ctx"]
        widget.deleteLater()

    def test_running_mission_shows_no_failed_notice(self, qapp) -> None:  # pylint: disable=unused-argument
        """A mission that is not failed says nothing about an error."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))

        assert widget._failed_notice.isHidden()  # pylint: disable=protected-access
        assert widget._lead_conversation_button.isHidden()  # pylint: disable=protected-access
        assert widget._retry_button.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_failed_notice_is_styled_as_a_failure(self, qapp) -> None:  # pylint: disable=unused-argument
        """The notice has a rule of its own, so a failure does not read like the warning an interruption gives."""
        widget = MissionWidget()
        widget.apply_style()
        sheet = widget.styleSheet()
        assert "QLabel#MissionFailedNotice {" in sheet
        assert widget._failed_notice.objectName() == "MissionFailedNotice"  # pylint: disable=protected-access
        widget.deleteLater()


class TestConfettiResize:
    """The celebration covers the page it is thrown over, even if the page changes size."""

    def test_overlay_follows_a_resized_page(self, qapp) -> None:  # pylint: disable=unused-argument
        """Growing the page while the confetti is in the air grows the overlay with it."""
        page = QWidget()
        page.resize(800, 600)
        page.show()
        QApplication.processEvents()

        overlay = ConfettiOverlay(page)
        overlay.burst(QPointF(400, 300))
        QApplication.processEvents()
        assert overlay.geometry() == page.rect()

        page.resize(1200, 900)
        QApplication.processEvents()
        assert overlay.geometry() == page.rect()
        page.deleteLater()


class TestModelTagElision:
    """A long model name is shortened with an ellipsis rather than being cut mid-word."""

    def test_long_model_name_is_elided(self, qapp) -> None:  # pylint: disable=unused-argument
        """The tag keeps the full name for its tooltip and shows an ellipsis when it does not fit."""
        widget = MissionWidget()
        widget.apply_style()
        widget.resize(1300, 1200)
        widget.show()
        QApplication.processEvents()

        name = "DeepSeek V4.1 Flash (deepseek-v4-flash:0731, ollama-cloud)"
        widget.set_mission(dataclasses.replace(
            _view(completed=False, stage=2),
            conversation=[
                MissionChatView("user", "You", "Hi", "10:00"),
                MissionChatView("lead", "Theo", "Hello there.", "10:01", name),
            ],
        ))
        for _ in range(5):
            QApplication.processEvents()

        tag = widget._chat_card.findChild(QLabel, "MissionChatModel")  # pylint: disable=protected-access
        assert tag is not None
        assert tag.text() == name
        assert tag.toolTip() == name
        assert tag.width() <= widget._chat_card.width()  # pylint: disable=protected-access
        widget.deleteLater()


class TestAgentNetworkKeyboard:
    """The picture of who is working can be read and used without a mouse."""

    @staticmethod
    def _agents() -> list[MissionAgentView]:
        return [
            MissionAgentView(
                context_id="lead", title="lead.conv", role_text="Mission lead", is_lead=True,
                state_text="Working", tone=MissionTone.ACCENT, busy=True, clickable=True,
                name="Atlas", speech="Working on it…", model_text="DeepSeek V4.1 Flash",
            ),
            MissionAgentView(
                context_id="helper", title="helper.conv", role_text="Write the docs", is_lead=False,
                state_text="Idle", tone=MissionTone.NEUTRAL, busy=False, clickable=True,
                name="Nova", parent_id="lead",
            ),
        ]

    def test_network_can_take_focus(self, qapp) -> None:  # pylint: disable=unused-argument
        """The view is a focus stop, so a keyboard user can reach it at all."""
        view = AgentNetworkView()
        assert view.focusPolicy() == Qt.FocusPolicy.StrongFocus
        view.deleteLater()

    def test_arrow_keys_move_the_highlight(self, qapp) -> None:  # pylint: disable=unused-argument
        """Down steps to the next agent and wraps round, and Home and End jump to the ends."""
        view = AgentNetworkView()
        view.set_agents(self._agents(), "You", "Approve", "Reject", "Finish waiting", "{0} left", "Read more", "Unsure")
        view.resize(600, 400)
        view.show()
        QApplication.processEvents()

        view.setFocus()
        QApplication.processEvents()
        assert view._hover_id == "__user__"  # pylint: disable=protected-access

        QTest.keyClick(view, Qt.Key.Key_Down)
        assert view._hover_id == "lead"  # pylint: disable=protected-access

        QTest.keyClick(view, Qt.Key.Key_Down)
        assert view._hover_id == "helper"  # pylint: disable=protected-access

        QTest.keyClick(view, Qt.Key.Key_Down)
        assert view._hover_id == "__user__"  # pylint: disable=protected-access

        QTest.keyClick(view, Qt.Key.Key_End)
        assert view._hover_id == "helper"  # pylint: disable=protected-access

        QTest.keyClick(view, Qt.Key.Key_Home)
        assert view._hover_id == "__user__"  # pylint: disable=protected-access
        view.deleteLater()

    def test_return_opens_the_highlighted_agent(self, qapp) -> None:  # pylint: disable=unused-argument
        """Return opens the conversation of the agent the highlight is on."""
        view = AgentNetworkView()
        view.set_agents(self._agents(), "You", "Approve", "Reject", "Finish waiting", "{0} left", "Read more", "Unsure")
        view.resize(600, 400)
        view.show()
        QApplication.processEvents()

        opened: list[str] = []
        view.agent_clicked.connect(opened.append)
        view.setFocus()
        QApplication.processEvents()
        QTest.keyClick(view, Qt.Key.Key_Down)
        QTest.keyClick(view, Qt.Key.Key_Return)
        assert opened == ["lead"]
        view.deleteLater()


class TestStylesheetAndStageTones:
    """The built stylesheet stays readable, and every status has its own journey tone."""

    def test_scoped_stylesheet_keeps_its_line_breaks(self, qapp) -> None:  # pylint: disable=unused-argument
        """Each rule starts on its own line rather than being glued to the closing brace before it."""
        widget = MissionWidget()
        widget.apply_style()
        sheet = widget.styleSheet()
        assert "}#MissionWidget" not in sheet
        assert "\n#MissionWidget" in sheet
        widget.deleteLater()

    def test_every_status_has_a_stage_tone(self) -> None:
        """Interrupted and stopped are named in the stage tones rather than falling back silently."""
        tones = mission_tab_module._STAGE_TONES  # pylint: disable=protected-access
        assert tones[MissionStatus.INTERRUPTED] == MissionTone.WARNING
        assert tones[MissionStatus.STOPPED] == MissionTone.DANGER


class TestChatHintVisibility:
    """The conversation hint only appears alongside messages."""

    def test_hint_is_hidden_when_there_is_no_conversation(self, qapp) -> None:  # pylint: disable=unused-argument
        """With no messages the hint is hidden; with messages it is shown."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        assert widget._chat_hint.isHidden()  # pylint: disable=protected-access

        widget.set_mission(dataclasses.replace(
            _view(completed=False, stage=2),
            conversation=[MissionChatView("user", "You", "Hi", "10:00")],
        ))
        assert not widget._chat_hint.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()


class TestComposerTools:
    """The composer attaches files and opens AI settings as a conversation's input does, without naming the mission."""

    def test_composer_does_not_repeat_the_missions_name(self, qapp) -> None:  # pylint: disable=unused-argument
        """Guiding the mission on show, the composer shows no pill naming it; a new mission is still marked."""
        widget = MissionWidget()
        widget.set_mission(_view(completed=False, stage=2))
        assert widget._target_pill.isHidden()  # pylint: disable=protected-access

        widget.set_mission(None)
        assert not widget._target_pill.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_attach_and_settings_buttons_ask_the_owner(self, qapp) -> None:  # pylint: disable=unused-argument
        """The paperclip asks for a file and the cog for settings, carrying the conversation input's icons and tips."""
        widget = MissionWidget()
        widget.apply_style()
        asked: list[str] = []
        widget.attach_requested.connect(lambda: asked.append("attach"))
        widget.settings_requested.connect(lambda: asked.append("settings"))
        widget._attach_button.click()  # pylint: disable=protected-access
        widget._settings_button.click()  # pylint: disable=protected-access
        assert asked == ["attach", "settings"]
        assert not widget._attach_button.icon().isNull()  # pylint: disable=protected-access
        assert not widget._settings_button.icon().isNull()  # pylint: disable=protected-access
        assert widget._attach_button.toolTip()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_attached_files_are_shown_removable_and_taken_once(self, qapp) -> None:  # pylint: disable=unused-argument
        """Each attached file is shown as a chip that can be removed, and sending takes the files."""
        widget = MissionWidget()
        widget.add_attachment("a.txt", "A")
        widget.add_attachment("b.txt", "B")
        assert not widget._attachments_bar.isHidden()  # pylint: disable=protected-access

        chip = widget._attachments_layout.itemAt(0).widget()  # pylint: disable=protected-access
        chip.findChild(QToolButton).click()
        assert widget.take_attachments() == [("b.txt", "B")]
        assert widget.take_attachments() == []
        assert widget._attachments_bar.isHidden()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_composer_scrollbar_uses_the_shared_style(self, qapp) -> None:  # pylint: disable=unused-argument
        """The composer's scrollbar is styled as every other scrollbar is."""
        widget = MissionWidget()
        widget.apply_style()
        assert "#MissionComposerInput QScrollBar" in widget.styleSheet()
        widget.deleteLater()

    def test_attached_file_reaches_the_new_missions_lead(self, mindspace_sandbox, monkeypatch, tmp_path) -> None:  # pylint: disable=unused-argument
        """A file attached in the composer is read as a conversation tab reads it and sent with the mission's brief."""
        sent: list[list[str] | None] = []

        async def fake_submit(_self, _requester, _user_message, attachment_guids=None):
            sent.append(attachment_guids)

        monkeypatch.setattr(AIConversation, "submit_message", fake_submit)
        notes = tmp_path / "notes.md"
        notes.write_text("Use tabs", encoding="utf-8")
        monkeypatch.setattr(mission_tab_module, "choose_attachment", lambda _parent: str(notes))

        async def run() -> None:
            tab = MissionTab("mission-tab")
            tab.attach_service()
            widget = tab._widget  # pylint: disable=protected-access
            widget._attach_button.click()  # pylint: disable=protected-access
            widget.submit_requested.emit("Format the code")
            for _ in range(5):
                await asyncio.sleep(0)

            service = tab._service  # pylint: disable=protected-access
            assert service is not None
            mission = service.missions()[0]
            history = service._lead_for(mission).transcript.get_conversation_history()  # pylint: disable=protected-access
            assert history.get_attachment(sent[-1][0])["content"] == "Use tabs"
            assert widget.take_attachments() == []

            tab.close_tab()
            tab.deleteLater()
            MissionServiceHolder().release()

        asyncio.run(run())


class TestWrappedTextIsMeasuredOnce:
    """Wrapped text is measured once for each width, and again whenever what it needs could change."""

    def test_height_follows_text_format_and_font(self, qapp) -> None:  # pylint: disable=unused-argument
        """After the text, its format or the font changes, the height is the one a label measuring every time gives."""
        label = mission_widget_module._WrapLabel()  # pylint: disable=protected-access
        plain = QLabel()
        plain.setWordWrap(True)
        heights: list[int] = []

        def check() -> None:
            heights.append(label.heightForWidth(200))
            assert heights[-1] == label.heightForWidth(200) == plain.heightForWidth(200)

        for widget in (label, plain):
            widget.setText("One line")
        check()

        for widget in (label, plain):
            widget.setText("A much longer piece of text that has to wrap onto several lines at this width " * 3)
        check()

        for widget in (label, plain):
            widget.setText("Intro\n\n- one\n- two\n- three")
        check()

        for widget in (label, plain):
            widget.setTextFormat(Qt.TextFormat.MarkdownText)
        check()

        font = label.font()
        font.setPointSizeF(font.pointSizeF() * 3)
        for widget in (label, plain):
            widget.setFont(font)
        check()

        assert heights[1] > heights[0] and heights[4] > heights[3]
        label.deleteLater()
        plain.deleteLater()
