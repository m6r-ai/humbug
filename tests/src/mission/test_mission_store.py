"""Tests for mission records and their storage."""

import os

from mission.mission import Mission, MissionCheckResult, MissionPhase, MissionTask, MissionTaskStatus
from mission.mission_store import MissionStore


def _mission(mission_id: str = "m1", created_at: str = "2026-11-06T10:00:00+00:00") -> Mission:
    return Mission(
        mission_id=mission_id,
        objective="Build the thing",
        title="Thing",
        lead_path=os.path.join(".humbug", "conversations", "Mission-1.conv"),
        created_at=created_at,
        summary="Summary",
        tasks=[
            MissionTask("1", "First", "Detail", MissionTaskStatus.COMPLETED),
            MissionTask("2", "Second", "", MissionTaskStatus.IN_PROGRESS),
        ],
    )


class TestMissionStore:
    """Mission records survive a round trip and damaged records do not hide others."""

    def test_saved_mission_loads_back_identically(self, tmp_path):
        """A saved mission is read back with every field intact."""
        store = MissionStore(str(tmp_path / "missions"))
        mission = _mission()

        store.save(mission)

        assert store.load_all() == [mission]

    def test_plan_history_survives_a_round_trip(self, tmp_path):
        """Finished plans moved to a mission's history are saved and read back."""
        store = MissionStore(str(tmp_path / "missions"))
        mission = _mission()
        mission.history.append(MissionPhase(
            title="Earlier",
            summary="Earlier work",
            tasks=[MissionTask("1", "Done before", "", MissionTaskStatus.COMPLETED)],
            finished_at="2026-11-06T09:00:00+00:00",
        ))

        store.save(mission)

        assert store.load_all()[0].history == mission.history

    def test_earlier_results_survive_a_round_trip(self, tmp_path):
        """What the user asked and the lead's report are saved with each finished plan, with the current request."""
        store = MissionStore(str(tmp_path / "missions"))
        mission = _mission()
        mission.request = "Now add a changelog"
        mission.history.append(MissionPhase(
            title="Earlier", summary="", tasks=[], finished_at="2026-11-06T09:00:00+00:00",
            request="Build the thing", report="**All done.** The thing is built.",
        ))

        store.save(mission)

        loaded = store.load_all()[0]
        assert loaded.request == "Now add a changelog"
        assert loaded.history[0].request == "Build the thing"
        assert loaded.history[0].report == "**All done.** The thing is built."

    def test_records_from_before_results_were_kept_still_load(self):
        """A record without requests or reports loads with them empty."""
        data = _mission().to_dict()
        del data["request"]
        data["history"] = [{"title": "Earlier", "summary": "", "tasks": [], "finishedAt": "2026-11-06T09:00:00+00:00"}]

        loaded = Mission.from_dict(data)
        assert loaded.request == ""
        assert loaded.history[0].request == "" and loaded.history[0].report == ""
        assert loaded.follow_up == "" and loaded.report == ""

    def test_follow_up_on_a_finished_plan_survives_a_round_trip(self, tmp_path):
        """A follow-up the lead has not yet turned into new work, and the report it follows, are saved."""
        store = MissionStore(str(tmp_path / "missions"))
        mission = _mission()
        mission.follow_up = "Which file did you change?"
        mission.report = "**All done.** The docs are in docs/."

        store.save(mission)

        loaded = store.load_all()[0]
        assert loaded.follow_up == "Which file did you change?"
        assert loaded.report == "**All done.** The docs are in docs/."

    def test_everything_confirmation_survives_a_round_trip(self, tmp_path):
        """When the user confirmed Everything is saved, and an older record without it loads with none."""
        store = MissionStore(str(tmp_path / "missions"))
        mission = _mission()
        mission.everything_confirmed_at = "2026-11-10T09:00:00+00:00"
        store.save(mission)
        assert store.load_all()[0].everything_confirmed_at == "2026-11-10T09:00:00+00:00"

        data = _mission().to_dict()
        del data["everythingConfirmedAt"]
        assert Mission.from_dict(data).everything_confirmed_at == ""

    def test_pause_survives_a_round_trip(self, tmp_path):
        """A paused mission is saved paused, and an older record without the flag loads unpaused."""
        store = MissionStore(str(tmp_path / "missions"))
        mission = _mission()
        mission.paused = True
        store.save(mission)
        assert store.load_all()[0].paused

        data = _mission().to_dict()
        del data["paused"]
        assert not Mission.from_dict(data).paused

    def test_older_finished_record_counts_as_celebrated(self):
        """A record from before celebrations were remembered is celebrated if it is finished, so it is not again."""
        finished = _mission()
        for task in finished.tasks:
            task.status = MissionTaskStatus.COMPLETED

        data = finished.to_dict()
        del data["celebrated"]
        assert Mission.from_dict(data).celebrated

        unfinished = _mission().to_dict()
        del unfinished["celebrated"]
        assert not Mission.from_dict(unfinished).celebrated

    def test_goal_survives_a_round_trip_and_older_records_complete_as_before(self, tmp_path):
        """Whether the goal is reported and met is saved; a record from before is complete when its plan is."""
        store = MissionStore(str(tmp_path / "missions"))
        mission = _mission()
        for task in mission.tasks:
            task.status = MissionTaskStatus.COMPLETED

        mission.reports_goal = True
        store.save(mission)
        loaded = store.load_all()[0]
        assert loaded.reports_goal and not loaded.goal_met
        assert loaded.plan_finished() and not loaded.is_complete()

        data = mission.to_dict()
        del data["reportsGoal"], data["goalMet"]
        assert Mission.from_dict(data).is_complete()

    def test_missions_load_oldest_first(self, tmp_path):
        """Missions are returned in creation order."""
        store = MissionStore(str(tmp_path / "missions"))
        store.save(_mission("b", "2026-11-06T12:00:00+00:00"))
        store.save(_mission("a", "2026-11-06T11:00:00+00:00"))

        assert [m.mission_id for m in store.load_all()] == ["a", "b"]

    def test_damaged_record_is_skipped(self, tmp_path):
        """An unreadable record is skipped and the rest still load."""
        missions_dir = tmp_path / "missions"
        store = MissionStore(str(missions_dir))
        store.save(_mission())
        (missions_dir / "broken.json").write_text("{not json", encoding="utf-8")
        (missions_dir / "incomplete.json").write_text('{"objective": "no id"}', encoding="utf-8")

        assert [m.mission_id for m in store.load_all()] == ["m1"]

    def test_save_leaves_no_temporary_files(self, tmp_path):
        """Only the record itself remains after a save."""
        missions_dir = tmp_path / "missions"
        store = MissionStore(str(missions_dir))
        store.save(_mission())
        store.save(_mission())

        assert os.listdir(missions_dir) == ["m1.json"]

    def test_delete_removes_record_and_ignores_missing(self, tmp_path):
        """Deleting removes the record, and deleting again is harmless."""
        store = MissionStore(str(tmp_path / "missions"))
        store.save(_mission())

        store.delete("m1")
        store.delete("m1")

        assert not store.load_all()

    def test_missing_directory_has_no_missions(self, tmp_path):
        """A mindspace that has never had a mission has none."""
        assert not MissionStore(str(tmp_path / "missions")).load_all()


class TestMissionProgress:
    """Progress reflects completed and in-progress tasks."""

    def test_progress_counts_in_progress_as_half(self):
        """One completed and one in-progress task of two is three quarters done."""
        assert _mission().progress() == 0.75

    def test_mission_without_plan_has_no_progress_and_is_not_complete(self):
        """A mission with no plan is neither progressing nor complete."""
        mission = _mission()
        mission.tasks = []

        assert mission.progress() == 0.0
        assert not mission.is_complete()

    def test_mission_is_complete_when_every_task_is(self):
        """A mission whose tasks are all completed is complete."""
        mission = _mission()
        for task in mission.tasks:
            task.status = MissionTaskStatus.COMPLETED

        assert mission.is_complete()
        assert mission.completed_count() == 2


class TestDoneChecks:
    """A mission with done checks is complete only once every check is reported passed."""

    @staticmethod
    def _finished(checks: list[str], results: list[bool]) -> Mission:
        mission = _mission()
        for task in mission.tasks:
            task.status = MissionTaskStatus.COMPLETED

        mission.reports_goal = True
        mission.goal_met = True
        mission.done_checks = checks
        mission.check_results = [MissionCheckResult(f"c{i}", passed) for i, passed in enumerate(results)]
        return mission

    def test_every_check_must_pass(self):
        """All checks passed completes the mission; one failed or one not reported does not."""
        assert self._finished(["a", "b"], [True, True]).is_complete()
        assert not self._finished(["a", "b"], [True, False]).is_complete()
        assert not self._finished(["a", "b"], [True]).is_complete()

    def test_no_checks_is_complete_as_before(self):
        """Without done checks a finished mission whose goal is met is complete."""
        assert self._finished([], []).is_complete()

    def test_stopping_a_task_ends_the_mission_whatever_the_checks(self):
        """A mission the user cut short by stopping a task is complete even with a check failing."""
        mission = self._finished(["a"], [False])
        mission.stopped_tasks = ["1"]
        assert mission.is_complete()

    def test_checks_survive_a_round_trip(self):
        """The checks, their results and the rounds are saved and loaded."""
        mission = self._finished(["npm test"], [False])
        mission.check_results = [MissionCheckResult("npm test", False, "2 failures")]
        mission.check_rounds = 2
        loaded = Mission.from_dict(mission.to_dict())
        assert loaded.done_checks == ["npm test"]
        assert loaded.check_results == [MissionCheckResult("npm test", False, "2 failures")]
        assert loaded.check_rounds == 2

    def test_record_without_checks_loads_with_none(self):
        """A record from before done checks loads with none, and so completes as it did."""
        data = _mission().to_dict()
        for key in ("doneChecks", "checkResults", "checkRounds"):
            data.pop(key)

        loaded = Mission.from_dict(data)
        assert loaded.done_checks == [] and loaded.check_results == [] and loaded.check_rounds == 0


class TestSkippedSteps:
    """Steps the user skips before approving a plan are left out of its progress and kept with the mission."""

    def test_skipped_steps_count_neither_way(self):
        """Progress and the step count are of the steps still planned."""
        mission = _mission()
        mission.tasks[0].status = MissionTaskStatus.STOPPED
        mission.skipped_tasks = [mission.tasks[0].task_id]
        assert mission.planned_count() == 1
        assert mission.progress() == 0.5

    def test_changes_survive_a_round_trip(self):
        """The skipped steps and the lead's own order are saved and loaded."""
        mission = _mission()
        mission.skipped_tasks = ["2"]
        mission.reviewed_order = ["1", "2"]
        loaded = Mission.from_dict(mission.to_dict())
        assert loaded.skipped_tasks == ["2"] and loaded.reviewed_order == ["1", "2"]
        assert loaded.plan_edited()


def test_token_counts_survive_a_round_trip_and_bad_values_are_none():
    """Token counts are saved and loaded; a missing or malformed count loads as zero."""
    from mission.mission import Mission  # pylint: disable=import-outside-toplevel
    mission = Mission(mission_id="m", objective="o", title="t", lead_path="l.conv", created_at="2026-01-01T00:00:00+00:00")
    mission.input_tokens, mission.output_tokens = 1234, 56
    loaded = Mission.from_dict(mission.to_dict())
    assert (loaded.input_tokens, loaded.output_tokens) == (1234, 56)

    data = mission.to_dict()
    data.update(inputTokens="lots", outputTokens=-3)
    loaded = Mission.from_dict(data)
    assert (loaded.input_tokens, loaded.output_tokens) == (0, 0)
    data.pop("inputTokens")
    assert Mission.from_dict(data).input_tokens == 0
