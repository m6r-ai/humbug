"""Tests for reading mission plans out of the mission lead's messages."""

from mission.mission import MissionTaskStatus
from mission.mission_plan_parser import parse_mission_plan


def _block(body: str) -> str:
    return f"Here is the plan.\n\n```mission-plan\n{body}\n```\n"


class TestParseMissionPlan:
    """The parser finds the current plan and tolerates loosely written plans."""

    def test_message_without_plan_has_no_plan(self):
        """Ordinary text yields no plan."""
        assert parse_mission_plan("Nothing to see here.") is None

    def test_plan_is_read_with_title_summary_and_tasks(self):
        """A well-formed block yields its title, summary and tasks in order."""
        plan = parse_mission_plan(_block(
            '{"title": "Ship it", "summary": "Do the thing", "tasks": ['
            '{"id": "1", "title": "Survey", "status": "completed"},'
            '{"id": "2", "title": "Build", "detail": "Write code", "status": "in_progress"}]}'
        ))

        assert plan is not None
        assert plan.title == "Ship it"
        assert plan.summary == "Do the thing"
        assert [t.title for t in plan.tasks] == ["Survey", "Build"]
        assert plan.tasks[0].status == MissionTaskStatus.COMPLETED
        assert plan.tasks[1].status == MissionTaskStatus.IN_PROGRESS
        assert plan.tasks[1].detail == "Write code"

    def test_last_valid_block_is_the_current_plan(self):
        """When a message repeats the plan, the last valid block wins."""
        text = (
            _block('{"tasks": [{"title": "Old"}]}')
            + _block('{"tasks": [{"title": "New"}]}')
            + _block("not json")
        )

        plan = parse_mission_plan(text)

        assert plan is not None
        assert [t.title for t in plan.tasks] == ["New"]

    def test_block_without_tasks_is_ignored(self):
        """A block with no usable tasks is not treated as an empty plan."""
        assert parse_mission_plan(_block('{"title": "Empty", "tasks": []}')) is None
        assert parse_mission_plan(_block('{"tasks": [{"detail": "no title"}]}')) is None

    def test_tasks_without_ids_are_numbered(self):
        """Tasks missing an id are numbered by position."""
        plan = parse_mission_plan(_block('{"tasks": [{"title": "A"}, {"title": "B"}]}'))

        assert plan is not None
        assert [t.task_id for t in plan.tasks] == ["1", "2"]

    def test_status_aliases_and_unknown_values(self):
        """Common status spellings are understood and unknown ones count as pending."""
        plan = parse_mission_plan(_block(
            '{"tasks": [{"title": "A", "status": "done"}, {"title": "B", "status": "In Progress"},'
            '{"title": "C", "status": "failed"}, {"title": "D", "status": "whatever"}]}'
        ))

        assert plan is not None
        assert [t.status for t in plan.tasks] == [
            MissionTaskStatus.COMPLETED,
            MissionTaskStatus.IN_PROGRESS,
            MissionTaskStatus.BLOCKED,
            MissionTaskStatus.PENDING,
        ]

    def test_progress_evidence_and_agent_are_read(self):
        """A task's progress note, evidence and delegate are kept."""
        plan = parse_mission_plan(_block(
            '{"tasks": [{"title": "A", "status": "completed", "progress": "Done it", '
            '"evidence": "Tests pass", "agent": ".humbug/conversations/dAI-1.conv"}, {"title": "B"}]}'
        ))

        assert plan is not None
        assert (plan.tasks[0].progress, plan.tasks[0].evidence, plan.tasks[0].agent) == (
            "Done it", "Tests pass", ".humbug/conversations/dAI-1.conv"
        )
        assert (plan.tasks[1].progress, plan.tasks[1].evidence, plan.tasks[1].agent) == ("", "", "")


class TestGoalMet:
    """The lead says when the user's whole goal is met, separately from finishing the plan's tasks."""

    @staticmethod
    def _plan(goal: str) -> str:
        return (
            '```mission-plan\n{"title": "T", "summary": "S", ' + goal +
            '"tasks": [{"id": "1", "title": "Do it", "status": "completed"}]}\n```'
        )

    def test_goal_met_is_read_only_when_reported_true(self) -> None:
        """Only an explicit true reports the goal met; false, a missing field or anything else does not."""
        for goal, met in (
            ('"goal_met": true, ', True), ('"goal_met": false, ', False), ("", False), ('"goal_met": "yes", ', False)
        ):
            plan = parse_mission_plan(self._plan(goal))
            assert plan is not None and plan.goal_met is met
