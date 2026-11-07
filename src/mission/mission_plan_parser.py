"""Extraction of mission plans written by the mission lead."""

from dataclasses import dataclass
import json
import re

from mission.mission import MissionTask


PLAN_FENCE = "mission-plan"

_PLAN_BLOCK = re.compile(r"```" + PLAN_FENCE + r"[ \t]*\n(.*?)```", re.DOTALL)


@dataclass(frozen=True)
class MissionPlan:
    """A plan reported by the mission lead."""
    title: str
    summary: str
    tasks: list[MissionTask]


def strip_mission_plan(text: str) -> str:
    """
    Return a message with its plan blocks removed.

    The plan is shown separately, so what remains is what the lead said to the user.

    Args:
        text: Message text written by the mission lead

    Returns:
        The text without plan blocks, trimmed
    """
    return _PLAN_BLOCK.sub("", text).strip()


def parse_mission_plan(text: str) -> MissionPlan | None:
    """
    Return the last valid plan block in a message, if there is one.

    The mission lead reports its plan as JSON in a fenced block tagged
    `mission-plan`.  A message may repeat the block as the plan evolves, so the
    last block that parses is the current one.  Blocks that are not valid JSON
    or have no tasks are ignored rather than treated as an empty plan.

    Args:
        text: Message text written by the mission lead

    Returns:
        The plan, or None if the message contains no usable plan
    """
    for block in reversed(_PLAN_BLOCK.findall(text)):
        try:
            data = json.loads(block)

        except json.JSONDecodeError:
            continue

        if not isinstance(data, dict):
            continue

        raw_tasks = data.get("tasks")
        if not isinstance(raw_tasks, list):
            continue

        tasks: list[MissionTask] = []
        for index, raw in enumerate(raw_tasks, start=1):
            if not isinstance(raw, dict):
                continue

            task = MissionTask.from_dict(raw)
            if not task.title:
                continue

            if not task.task_id:
                task.task_id = str(index)

            tasks.append(task)

        if not tasks:
            continue

        title = data.get("title")
        summary = data.get("summary")
        return MissionPlan(
            title=title.strip() if isinstance(title, str) else "",
            summary=summary.strip() if isinstance(summary, str) else "",
            tasks=tasks,
        )

    return None
