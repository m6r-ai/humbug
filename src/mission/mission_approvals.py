"""The decision inbox and auto-approval for missions: every approval decision Mission Control makes or relays."""

from collections.abc import Callable, Coroutine
from dataclasses import dataclass, replace
from datetime import datetime, timezone
import logging
from typing import TYPE_CHECKING, Any
import uuid

from ai import AIMessage, AIMessageSource
from ai_tool import AIToolApprovalKind, AIToolCall
from mindspace.mindspace import Mindspace
from mindspace.mindspace_conversation_log import tool_call_label
from mindspace.mindspace_log_level import MindspaceLogLevel
from mission.mission import AutoApproveLevel, Mission, MissionMode
from mission.mission_reviewer import MissionApprovalReviewer, ReviewRequest, ReviewVerdict
from mission.mission_snapshot import MissionActivityKind, MissionDecision
from mission.mission_tool_label import tool_call_target

if TYPE_CHECKING:
    from mission.mission_service import _TrackedConversation


_REJECT_REASON = "Tool call was rejected by the user"
_UNSURE_REASON = "User is unsure about this tool request and wants to discuss"
_UNSURE_MESSAGE = "I'm not sure about this tool request. Let's discuss."

# How much of the user's later messages, and of the lead's latest update, the approval reviewer sees.
_REVIEW_INSTRUCTION_COUNT = 6
_REVIEW_TEXT_LIMIT = 800

# What the user grants by confirming the Everything auto-approve level (ADR-0026), recorded with the confirmation.
_EVERYTHING_AUTHORITY = (
    "every tool request made while the mission is autonomous, without review, including destructive file "
    "operations, terminal commands (which cannot be limited to particular files), delegation and network writes; "
    "revoked by choosing another level, switching the mission to classic mode, or stopping it"
)


def _allows(level: AutoApproveLevel, destructive: bool) -> bool:
    """Return whether an auto-approve level lets a request be approved without the user."""
    return level == AutoApproveLevel.ALL or (level == AutoApproveLevel.SAFE and not destructive)


@dataclass
class _PendingDecision:
    """A decision together with what identifies the request it answers."""
    decision: MissionDecision
    tool_call_id: str
    message_id: str | None
    reviewing: bool = False
    review_note: str = ""


@dataclass(frozen=True)
class MissionApprovalsHost:
    """
    What the approvals use of the mission service that owns them.

    The dictionaries are the service's own, shared rather than copied, so the
    approvals always see the missions and conversations the service is following.
    """
    missions: dict[str, Mission]
    tracked: dict[str, "_TrackedConversation"]
    members: dict[str, str]
    record: Callable[[MissionActivityKind, str, str, str | None, str], None]
    notify: Callable[[], None]
    save: Callable[[Mission], None]
    title_for: Callable[[str], str]
    run_task: Callable[[Coroutine[Any, Any, None], str | None], None]
    lead_for: Callable[[Mission], "_TrackedConversation | None"]
    latest_lead_message: Callable[[Mission], tuple[str, datetime | None]]
    expect_message: Callable[[str, str], None]


class MissionApprovals:
    """
    Holds the requests waiting for the user, and approves those a mission's auto-approve level covers.

    Every decision is bound to the tool call it answers, and is discarded the moment
    its conversation is no longer waiting on that call, so an answer can never reach
    a different request.  Requests are approved without the user only for an
    autonomous mission that is not paused, and only as its level allows: a change
    the journal records under Safe actions or Everything, anything under
    Everything, and otherwise what the reviewer approves.  The reviewer fails
    closed, leaving the request for the user.  See ADR-0026.
    """

    def __init__(self, mindspace: Mindspace, reviewer: MissionApprovalReviewer, host: MissionApprovalsHost) -> None:
        """
        Initialise the approvals.

        Args:
            mindspace: The open mindspace, whose interaction log records every decision
            reviewer: Reviews requests for missions that allow auto-approval
            host: The state and helpers of the mission service that owns the approvals
        """
        self._mindspace = mindspace
        self._reviewer = reviewer
        self._host = host
        self._logger = logging.getLogger("MissionApprovals")
        self._decisions: dict[str, _PendingDecision] = {}

    def decisions(self) -> list[MissionDecision]:
        """
        Return every decision waiting for the user in this mindspace, oldest first.

        A decision is only listed while its conversation is still waiting on the
        same request, so one answered elsewhere disappears even if no event said so.
        """
        live: list[MissionDecision] = []
        for context_id, pending in self._decisions.items():
            tracked = self._host.tracked.get(context_id)
            if tracked is None:
                continue

            request = tracked.conversation.pending_tool_authorization()
            if request is not None and request.tool_call.id == pending.tool_call_id:
                live.append(replace(pending.decision, reviewing=pending.reviewing, review_note=pending.review_note))

        return sorted(live, key=lambda d: d.requested_at)

    def forget(self, context_id: str) -> None:
        """Drop any decision for a conversation that is no longer followed."""
        self._decisions.pop(context_id, None)

    def clear(self) -> None:
        """Drop every decision."""
        self._decisions.clear()

    def clear_answered(self, context_id: str, message_id: str) -> bool:
        """
        Drop a conversation's decision once the message it answers has completed.

        Args:
            context_id: Context of the conversation
            message_id: The message that completed

        Returns:
            True if a decision was dropped
        """
        pending = self._decisions.get(context_id)
        if pending is None or pending.message_id != message_id:
            return False

        del self._decisions[context_id]
        return True

    async def approve(self, decision_id: str) -> bool:
        """
        Approve a pending decision.

        Args:
            decision_id: Decision to approve

        Returns:
            True if the request was approved, False if it was no longer pending
        """
        found = self._take_decision(decision_id)
        if found is None:
            return False

        tracked, pending = found
        self._log_decision(tracked, f"Human approved tool call from Mission Control: {pending.decision.tool_name}")
        self._host.record(
            MissionActivityKind.APPROVED, self._host.title_for(tracked.context_id),
            pending.decision.tool_name, pending.decision.mission_id, pending.decision.reason
        )
        await tracked.transcript.approve_pending_tool_calls()
        self._host.notify()
        return True

    async def reject(self, decision_id: str) -> bool:
        """
        Reject a pending decision.

        Args:
            decision_id: Decision to reject

        Returns:
            True if the request was rejected, False if it was no longer pending
        """
        found = self._take_decision(decision_id)
        if found is None:
            return False

        tracked, pending = found
        self._log_decision(tracked, f"Human denied tool call from Mission Control: {pending.decision.tool_name}")
        self._host.record(
            MissionActivityKind.REJECTED, self._host.title_for(tracked.context_id),
            pending.decision.tool_name, pending.decision.mission_id, pending.decision.reason
        )
        await tracked.transcript.reject_pending_tool_calls(_REJECT_REASON)
        self._host.notify()
        return True

    async def unsure(self, decision_id: str) -> bool:
        """
        Decline a pending decision and ask the conversation to discuss it.

        Args:
            decision_id: Decision the user is unsure about

        Returns:
            True if the request was declined, False if it was no longer pending
        """
        found = self._take_decision(decision_id)
        if found is None:
            return False

        tracked, pending = found
        self._log_decision(
            tracked,
            f"Human indicated uncertainty about tool call from Mission Control: {pending.decision.tool_name}"
        )
        self._host.record(
            MissionActivityKind.REJECTED, self._host.title_for(tracked.context_id),
            pending.decision.tool_name, pending.decision.mission_id, pending.decision.reason
        )
        await tracked.transcript.reject_pending_tool_calls(_UNSURE_REASON)
        self._host.expect_message(tracked.context_id, _UNSURE_MESSAGE)
        await tracked.transcript.submit_message(None, _UNSURE_MESSAGE)
        self._host.notify()
        return True

    def _take_decision(self, decision_id: str) -> "tuple[_TrackedConversation, _PendingDecision] | None":
        """
        Remove a decision if it still matches the request its conversation is waiting on.

        A decision that no longer matches, because the request was answered or
        replaced, is discarded so it can never answer a different request.

        Args:
            decision_id: Decision to take

        Returns:
            The conversation and decision, or None if the decision is stale
        """
        context_id = next(
            (ctx for ctx, p in self._decisions.items() if p.decision.decision_id == decision_id), None
        )
        if context_id is None:
            return None

        pending = self._decisions.pop(context_id)
        tracked = self._host.tracked.get(context_id)
        if tracked is None:
            self._host.notify()
            return None

        request = tracked.conversation.pending_tool_authorization()
        if request is None or request.tool_call.id != pending.tool_call_id:
            self._host.notify()
            return None

        return tracked, pending

    def _log_decision(self, tracked: "_TrackedConversation", content: str) -> None:
        """Record a decision in the interaction log, attributed to its conversation."""
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            content,
            context=self._mindspace.conversation_log_context(tracked.conversation)
        )

    def handle_approval_required(
        self,
        tracked: "_TrackedConversation",
        message: AIMessage | None,
        tool_call: AIToolCall,
        reason: str,
        context: str | None,
        destructive: bool,
        kind: AIToolApprovalKind
    ) -> None:
        """Add a conversation's approval request to the decision inbox, and have it reviewed if allowed."""
        self.add_decision(tracked, message, tool_call, reason, context, destructive, kind)
        mission_id = self._host.members.get(tracked.context_id)
        if mission_id is not None:
            # A change the journal records can usually be reversed, which matters to the user
            # deciding on it, so the feed says which kind of request this is.
            label = tool_call_label(tool_call)
            if kind == AIToolApprovalKind.MINDSPACE_FILE_CHANGE:
                label = f"{label} (journalled)"

            self._host.record(
                MissionActivityKind.APPROVAL_REQUESTED, self._host.title_for(tracked.context_id),
                label, mission_id, f"{reason}\n\n{context}" if context else reason
            )
            self._consider_review(tracked.context_id)

        self._host.notify()

    def set_auto_approve(self, mission: Mission, level: AutoApproveLevel) -> None:
        """
        Choose which of a mission's approval requests may be approved without the user.

        Requests already waiting are reviewed straight away under the new level.

        Args:
            mission: Mission to change
            level: The new level
        """
        if mission.auto_approve == level:
            return

        mission.auto_approve = level
        if level == AutoApproveLevel.ALL:
            mission.everything_confirmed_at = datetime.now(timezone.utc).isoformat()

        else:
            mission.everything_confirmed_at = ""
            # The standing approvals were granted under Everything, which the user has withdrawn.
            self.withdraw_standing_approvals(mission.mission_id)

        self._host.save(mission)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO, f"Human set auto-approve for mission '{mission.title}' to {level.value}"
        )
        if level == AutoApproveLevel.ALL:
            self.log_everything_confirmed(mission)

        for pending in list(self._decisions.values()):
            if pending.decision.mission_id == mission.mission_id and not pending.reviewing:
                # A new level is a new instruction, so requests already left for the user are reconsidered.
                pending.review_note = ""

        self.review_waiting(mission.mission_id)
        self._host.notify()

    def review_waiting(self, mission_id: str) -> None:
        """Consider again every request a mission has waiting, under its current level."""
        for context_id, pending in list(self._decisions.items()):
            if pending.decision.mission_id == mission_id:
                self._consider_review(context_id)

    def withdraw_standing_approvals(self, mission_id: str) -> None:
        """Turn off the standing approval for file changes in each of a mission's conversations."""
        for context_id, member_mission in self._host.members.items():
            tracked = self._host.tracked.get(context_id)
            if member_mission == mission_id and tracked is not None:
                tracked.transcript.set_auto_approve_file_changes(False)

    def log_everything_confirmed(self, mission: Mission) -> None:
        """Record the user's confirmation of the Everything level: the approval in advance it rests on."""
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Human confirmed Everything auto-approve for mission '{mission.title}'\n"
            f"mission: {mission.mission_id}\n"
            f"confirmed at: {mission.everything_confirmed_at}\n"
            f"authority: {_EVERYTHING_AUTHORITY}"
        )

    def revoke_everything(self, mission: Mission, reason: str) -> None:
        """
        Take a mission off the Everything level, back to Off, and say why.

        Everything is the user's approval in advance for an autonomous mission that is
        going, so it ends when the mission leaves autonomous mode or is stopped (ADR-0026).
        Every way out of autonomous mode calls this.
        """
        if mission.auto_approve != AutoApproveLevel.ALL:
            return

        mission.auto_approve = AutoApproveLevel.OFF
        mission.everything_confirmed_at = ""
        self.withdraw_standing_approvals(mission.mission_id)
        self._host.save(mission)
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Everything auto-approve for mission '{mission.title}' was revoked, as {reason}; auto-approve is now off"
        )

    def _user_instructions(self, mission: Mission) -> list[str]:
        """Return the most recent messages the user typed to a mission's lead, oldest first."""
        tracked = self._host.lead_for(mission)
        if tracked is None:
            return []

        messages = [
            message.content.strip()[:_REVIEW_TEXT_LIMIT]
            for message in tracked.transcript.get_conversation_history().get_messages()
            if message.source == AIMessageSource.USER and not message.tool_results and not message.user_name
            and message.content.strip()
        ]
        return messages[-_REVIEW_INSTRUCTION_COUNT:]

    def _consider_review(self, context_id: str) -> None:
        """Send a pending decision to the AI reviewer if its mission's auto-approve level covers it."""
        pending = self._decisions.get(context_id)
        if pending is None or pending.reviewing or pending.review_note:
            return

        mission = self._host.missions.get(pending.decision.mission_id or "")
        if mission is None or mission.mode != MissionMode.AUTONOMOUS or mission.paused:
            return

        if mission.auto_approve == AutoApproveLevel.OFF:
            return

        if pending.decision.reversible and _allows(mission.auto_approve, pending.decision.destructive):
            # The journal records this change, which is the condition the conversation's own
            # standing approval rests on, so it is approved rather than reviewed.
            pending.reviewing = True
            self._host.run_task(self._approve_reversible(mission, pending), mission.mission_id)
            return

        if mission.auto_approve == AutoApproveLevel.ALL:
            # The user chose to let this mission run end to end, so nothing waits for them.
            pending.reviewing = True
            self._host.run_task(self._approve_everything(mission, pending), mission.mission_id)
            return

        if mission.auto_approve == AutoApproveLevel.SAFE and pending.decision.destructive:
            return

        pending.reviewing = True
        self._host.run_task(self._review_decision(mission, context_id, pending), mission.mission_id)

    async def _approve_reversible(self, mission: Mission, pending: _PendingDecision) -> None:
        """
        Approve a change the journal can undo, and grant the conversation's standing approval.

        The standing approval is the conversation's own, from ADR-0022: it covers only changes
        the journal records and lapses when the turn ends, so each turn's first such change
        comes back here, where the mission's auto-approve level is checked again.
        """
        decision = pending.decision
        if (
            mission.mode != MissionMode.AUTONOMOUS or mission.paused
            or not _allows(mission.auto_approve, decision.destructive)
        ):
            # The user changed the level or the mode, or paused the mission, since this was scheduled.
            pending.reviewing = False
            self._host.notify()
            return

        found = self._take_decision(decision.decision_id)
        if found is None:
            return

        tracked, _ = found

        # The standing approval covers every change the journal records, destructive ones
        # included, so it is only granted where the level allows those too.
        standing = mission.auto_approve == AutoApproveLevel.ALL
        if standing:
            tracked.transcript.set_auto_approve_file_changes(True)

        self._log_decision(
            tracked,
            f"Mission '{mission.title}' auto-approve approved a change the journal records"
            + (", and granted a standing approval for such changes for the rest of the turn" if standing else "")
            + f": {decision.tool_name}"
        )
        self._host.record(
            MissionActivityKind.REVERSIBLE_APPROVED, self._host.title_for(tracked.context_id), decision.tool_name,
            mission.mission_id, decision.reason
        )
        await tracked.transcript.approve_pending_tool_calls()
        self._host.notify()

    async def _approve_everything(self, mission: Mission, pending: _PendingDecision) -> None:
        """Approve a request without a review, because the mission's auto-approve level is Everything."""
        decision = pending.decision
        if mission.mode != MissionMode.AUTONOMOUS or mission.paused or mission.auto_approve != AutoApproveLevel.ALL:
            # The user changed the level or the mode, or paused the mission, since this was scheduled.
            pending.reviewing = False
            self._host.notify()
            self._consider_review(decision.context_id)
            return

        found = self._take_decision(decision.decision_id)
        if found is None:
            return

        tracked, _ = found
        self._log_decision(
            tracked,
            f"Mission '{mission.title}' auto-approve is set to Everything, so approved: {decision.tool_name}\n"
            f"standing approval: Everything confirmed at {mission.everything_confirmed_at}"
        )
        self._host.record(
            MissionActivityKind.EVERYTHING_APPROVED, self._host.title_for(tracked.context_id), decision.tool_name,
            mission.mission_id, decision.reason
        )
        await tracked.transcript.approve_pending_tool_calls()
        self._host.notify()

    async def _review_decision(self, mission: Mission, context_id: str, pending: _PendingDecision) -> None:
        """Ask the reviewer about a decision and act on the verdict if the request is still waiting."""
        tracked = self._host.tracked.get(context_id)
        if tracked is None:
            return

        decision = pending.decision
        lead_update, _when = self._host.latest_lead_message(mission)
        request = ReviewRequest(
            objective=mission.objective,
            title=mission.title,
            summary=mission.summary,
            instructions=self._user_instructions(mission),
            lead_update=lead_update[:_REVIEW_TEXT_LIMIT],
            plan=[f"{task.task_id}. {task.title} [{task.status.value}]" for task in mission.tasks],
            tool_name=decision.tool_name,
            reason=decision.reason,
            detail=decision.detail or "",
            destructive=decision.destructive,
            settings=tracked.conversation.conversation_settings(),
        )
        try:
            verdict = await self._reviewer.review(request)

        except Exception as e:
            self._logger.warning("Approval review failed: %s", e)
            verdict = ReviewVerdict(False, "The review could not be completed")

        if self._decisions.get(context_id) is not pending:
            return

        if mission.paused:
            # Paused while the reviewer was thinking: the request waits, and is reviewed again on continuing.
            pending.reviewing = False
            self._host.notify()
            return

        if verdict.approved and _allows(mission.auto_approve, decision.destructive) and (
            mission.mode == MissionMode.AUTONOMOUS
        ):
            found = self._take_decision(decision.decision_id)
            if found is not None:
                found_tracked, _ = found
                self._log_decision(
                    found_tracked,
                    f"AI reviewer approved tool call for mission '{mission.title}': {decision.tool_name}\n"
                    f"reason: {verdict.reason}"
                )
                self._host.record(
                    MissionActivityKind.AUTO_APPROVED, self._host.title_for(context_id), decision.tool_name,
                    mission.mission_id, f"{decision.reason}\n\n{verdict.reason}"
                )
                await found_tracked.transcript.approve_pending_tool_calls()
                self._host.notify()
                return

        pending.reviewing = False
        pending.review_note = verdict.reason
        self._log_decision(
            tracked,
            f"AI reviewer left tool call for the human: {decision.tool_name}\nreason: {verdict.reason}"
        )
        self._host.record(
            MissionActivityKind.REVIEW_DEFERRED, self._host.title_for(context_id), decision.tool_name,
            mission.mission_id, f"{decision.reason}\n\n{verdict.reason}"
        )
        self._host.notify()

    def add_decision(
        self,
        tracked: "_TrackedConversation",
        message: AIMessage | None,
        tool_call: AIToolCall,
        reason: str,
        context: str | None,
        destructive: bool,
        kind: AIToolApprovalKind
    ) -> None:
        """Record a pending approval for a conversation, replacing any earlier one."""
        decision = MissionDecision(
            decision_id=uuid.uuid4().hex,
            context_id=tracked.context_id,
            conversation_title=self._host.title_for(tracked.context_id),
            mission_id=self._host.members.get(tracked.context_id),
            tool_name=tool_call_label(tool_call),
            reason=reason,
            detail=context,
            destructive=destructive,
            requested_at=datetime.now(timezone.utc),
            reversible=kind == AIToolApprovalKind.MINDSPACE_FILE_CHANGE,
            target=tool_call_target(tool_call),
        )
        self._decisions[tracked.context_id] = _PendingDecision(
            decision=decision,
            tool_call_id=tool_call.id,
            message_id=message.id if message is not None else None,
        )
