#!/usr/bin/env python3
"""
MS Project XML exporter for the project planner.

Converts a scheduled planner JSON file (output of merge_cpm_results)
back into a Microsoft Project XML file (.xml).

Scheduled positions are stored as working-day offsets from the project
start.  This module converts those offsets back to MS Project datetimes
using the project's calendar.
"""

import argparse
import sys
import xml.etree.ElementTree as ET
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

from msproject_import import _offset_to_datetime

NS = "http://schemas.microsoft.com/project"

# Planner day names -> MS Project DayType codes (1=Sun, 2=Mon, ..., 7=Sat)
DAY_NAME_TO_TYPE = {
    "sun": "1", "mon": "2", "tue": "3", "wed": "4",
    "thu": "5", "fri": "6", "sat": "7",
}

# Planner dependency type strings -> MS Project type codes
DEPENDENCY_TYPE_CODES = {
    "finish-to-finish": "0",
    "finish-to-start":  "1",
    "start-to-start":   "2",
    "start-to-finish":  "3",
}


def _sub(parent: ET.Element, tag: str, text: str) -> ET.Element:
    """Add a child element with text content."""
    el = ET.SubElement(parent, tag)
    el.text = text
    return el


def _offset_to_dt(offset, schedule: dict, is_start: bool = False) -> Optional[str]:
    """Convert a working-day offset to an MS Project datetime string."""
    if offset is None or offset is False:
        return None
    try:
        offset = float(offset)
    except (TypeError, ValueError):
        return None
    return _offset_to_datetime(
        offset,
        schedule["project-start-datetime"],
        set(schedule.get("working-days", ["mon", "tue", "wed", "thu", "fri"])),
        schedule.get("hours-per-day", 8.0),
        schedule.get("working-periods"),
        is_start=is_start,
    )


def _days_to_iso_duration(days: Optional[float], hours_per_day: float) -> str:
    """Convert decimal working days to ISO 8601 duration string PTxHyMzS."""
    if days is None:
        return "PT0H0M0S"
    total_minutes = days * hours_per_day * 60.0
    hours = int(total_minutes // 60)
    minutes = int(total_minutes % 60)
    return f"PT{hours}H{minutes}M0S"


def _duration_format(duration_days: float, is_summary: bool) -> str:
    """
    Return the MS Project DurationFormat code for a task.

      21 = estimated days  (summary tasks — MS Project rolls these up)
       7 = days            (whole-day or zero-duration tasks)
       5 = hours           (sub-day tasks where hours is the natural unit)
    """
    if is_summary:
        return "21"
    whole = abs(duration_days - round(duration_days)) < 1e-9
    if not whole and duration_days > 0.0:
        return "5"
    return "7"


def _days_to_slack_tenths(days: Optional[float], hours_per_day: float) -> int:
    """Convert decimal working days to MS Project slack (tenths of a minute)."""
    if days is None:
        return 0
    return int(days * hours_per_day * 60.0 * 10)


def _task_uid(task: dict) -> str:
    """Return the original MS Project UID for a task."""
    return task.get("source-ids", {}).get("msproject", "")


def _resource_uid(resource: dict) -> str:
    """Return the original MS Project UID for a resource."""
    return resource.get("source-ids", {}).get("msproject", "")


def _percent_complete(task: dict) -> int:
    """Return percent complete as an integer 0-100."""
    progress = task.get("progress", 0.0) or 0.0
    return int(round(progress * 100))


def _build_project_summary_task(
    project: dict,
    schedule: dict,
    tasks_el: ET.Element,
) -> None:
    """Write the UID=0 project summary task."""
    calc = project.get("calculated", {})
    hours_per_day = schedule.get("hours-per-day", 8.0)

    proj_start_dt = _offset_to_dt(0.0, schedule)
    proj_end_offset = calc.get("latest-finish")
    proj_end_dt = _offset_to_dt(proj_end_offset, schedule)

    total_days = calc.get("total-duration-days") or 0.0
    duration_str = _days_to_iso_duration(total_days, hours_per_day)
    total_hours = total_days * hours_per_day
    work_str = f"PT{int(total_hours)}H0M0S"

    task_el = ET.SubElement(tasks_el, "Task")
    _sub(task_el, "UID", "0")
    _sub(task_el, "ID", "0")
    _sub(task_el, "Name", project.get("name", "Project"))
    _sub(task_el, "Active", "1")
    _sub(task_el, "Manual", "0")
    _sub(task_el, "Type", "1")
    _sub(task_el, "IsNull", "0")
    _sub(task_el, "WBS", "0")
    _sub(task_el, "OutlineNumber", "0")
    _sub(task_el, "OutlineLevel", "0")
    _sub(task_el, "Priority", "500")
    if proj_start_dt:
        _sub(task_el, "Start", proj_start_dt)
    if proj_end_dt:
        _sub(task_el, "Finish", proj_end_dt)
    _sub(task_el, "Duration", duration_str)
    if proj_start_dt:
        _sub(task_el, "ManualStart", proj_start_dt)
    if proj_end_dt:
        _sub(task_el, "ManualFinish", proj_end_dt)
    _sub(task_el, "ManualDuration", duration_str)
    _sub(task_el, "DurationFormat", "21")
    _sub(task_el, "FreeformDurationFormat", "7")
    _sub(task_el, "Work", work_str)
    _sub(task_el, "ResumeValid", "0")
    _sub(task_el, "EffortDriven", "0")
    _sub(task_el, "Recurring", "0")
    _sub(task_el, "OverAllocated", "0")
    _sub(task_el, "Estimated", "0")
    _sub(task_el, "Milestone", "0")
    _sub(task_el, "Summary", "1")
    _sub(task_el, "DisplayAsSummary", "0")
    _sub(task_el, "Critical", "0")
    _sub(task_el, "IsSubproject", "0")
    _sub(task_el, "IsSubprojectReadOnly", "0")
    _sub(task_el, "ExternalTask", "0")
    if proj_start_dt:
        _sub(task_el, "EarlyStart", proj_start_dt)
    if proj_end_dt:
        _sub(task_el, "EarlyFinish", proj_end_dt)
    if proj_start_dt:
        _sub(task_el, "LateStart", proj_start_dt)
    if proj_end_dt:
        _sub(task_el, "LateFinish", proj_end_dt)
    _sub(task_el, "StartVariance", "0")
    _sub(task_el, "FinishVariance", "0")
    _sub(task_el, "WorkVariance", "0.00")
    _sub(task_el, "FreeSlack", "0")
    _sub(task_el, "TotalSlack", "0")
    _sub(task_el, "StartSlack", "0")
    _sub(task_el, "FinishSlack", "0")
    _sub(task_el, "FixedCost", "0")
    _sub(task_el, "FixedCostAccrual", "3")
    _sub(task_el, "PercentComplete", "0")
    _sub(task_el, "PercentWorkComplete", "0")
    _sub(task_el, "Cost", "0")
    _sub(task_el, "OvertimeCost", "0")
    _sub(task_el, "OvertimeWork", "PT0H0M0S")
    _sub(task_el, "ActualDuration", "PT0H0M0S")
    _sub(task_el, "ActualCost", "0")
    _sub(task_el, "ActualOvertimeCost", "0")
    _sub(task_el, "ActualWork", "PT0H0M0S")
    _sub(task_el, "ActualOvertimeWork", "PT0H0M0S")
    _sub(task_el, "RegularWork", work_str)
    _sub(task_el, "RemainingDuration", duration_str)
    _sub(task_el, "RemainingCost", "0")
    _sub(task_el, "RemainingWork", work_str)
    _sub(task_el, "RemainingOvertimeCost", "0")
    _sub(task_el, "RemainingOvertimeWork", "PT0H0M0S")
    _sub(task_el, "ACWP", "0.00")
    _sub(task_el, "CV", "0.00")
    _sub(task_el, "ConstraintType", "0")
    _sub(task_el, "CalendarUID", "-1")
    _sub(task_el, "LevelAssignments", "1")
    _sub(task_el, "LevelingCanSplit", "1")
    _sub(task_el, "LevelingDelay", "0")
    _sub(task_el, "LevelingDelayFormat", "8")
    _sub(task_el, "IgnoreResourceCalendar", "0")
    _sub(task_el, "HideBar", "0")
    _sub(task_el, "Rollup", "0")
    _sub(task_el, "BCWS", "0.00")
    _sub(task_el, "BCWP", "0.00")
    _sub(task_el, "PhysicalPercentComplete", "0")
    _sub(task_el, "EarnedValueMethod", "0")
    _sub(task_el, "IsPublished", "0")
    _sub(task_el, "CommitmentType", "0")


def _build_task_element(
    task: dict,
    dep_map: dict,
    schedule: dict,
    tasks_el: ET.Element,
) -> None:
    """Write a single task element in the canonical MS Project XML field order."""
    uid = _task_uid(task)
    if not uid:
        return

    hours_per_day = schedule.get("hours-per-day", 8.0)
    is_summary = task.get("is-summary", False)
    is_milestone = task.get("is-milestone", False)
    duration_days = task.get("duration-days") or 0.0
    duration_str = _days_to_iso_duration(duration_days, hours_per_day)
    assigned = task.get("assigned-resources", [])
    work_str = duration_str if assigned else "PT0H0M0S"

    earliest_start = task.get("earliest-start")
    earliest_finish = task.get("earliest-finish")
    latest_start = task.get("latest-start")
    latest_finish = task.get("latest-finish")
    slack_days = task.get("slack-days")
    on_critical = task.get("on-critical-path", False)

    start_offset = earliest_start if earliest_start is not None and earliest_start is not False \
        else task.get("start-offset")
    finish_offset = earliest_finish if earliest_finish is not None and earliest_finish is not False \
        else task.get("end-offset")

    use_is_start = duration_days > 0.0
    start_dt = _offset_to_dt(start_offset, schedule, is_start=use_is_start)
    finish_dt = _offset_to_dt(finish_offset, schedule)
    early_start_dt = _offset_to_dt(earliest_start, schedule, is_start=use_is_start)
    early_finish_dt = _offset_to_dt(earliest_finish, schedule)
    late_start_dt = _offset_to_dt(latest_start, schedule, is_start=use_is_start)
    late_finish_dt = _offset_to_dt(latest_finish, schedule)

    percent = _percent_complete(task)
    slack_tenths = _days_to_slack_tenths(slack_days, hours_per_day)

    cal_id = task.get("calendar-id", "")
    cal_source_uid = "-1"
    if cal_id and cal_id != "cal-1":
        cal_source_uid = cal_id.replace("cal-", "")

    stored_fmt = task.get("duration-format")
    dur_fmt = stored_fmt if stored_fmt else _duration_format(duration_days, is_summary)

    task_el = ET.SubElement(tasks_el, "Task")
    _sub(task_el, "UID", uid)
    _sub(task_el, "ID", uid)
    _sub(task_el, "Name", task.get("name", ""))
    _sub(task_el, "Active", "1")
    _sub(task_el, "Manual", "0")
    _sub(task_el, "Type", "1" if is_summary else "0")
    _sub(task_el, "IsNull", "0")
    _sub(task_el, "WBS", task.get("wbs", ""))
    _sub(task_el, "OutlineNumber", task.get("wbs", ""))
    _sub(task_el, "OutlineLevel", str(task.get("outline-level", 1)))
    _sub(task_el, "Priority", "500")
    if start_dt:
        _sub(task_el, "Start", start_dt)
    if finish_dt:
        _sub(task_el, "Finish", finish_dt)
    _sub(task_el, "Duration", duration_str)
    if start_dt:
        _sub(task_el, "ManualStart", start_dt)
    if finish_dt:
        _sub(task_el, "ManualFinish", finish_dt)
    _sub(task_el, "ManualDuration", duration_str)
    _sub(task_el, "DurationFormat", dur_fmt)
    _sub(task_el, "FreeformDurationFormat", "7")
    _sub(task_el, "Work", work_str)
    _sub(task_el, "ResumeValid", "0")
    _sub(task_el, "EffortDriven", "1" if assigned else "0")
    _sub(task_el, "Recurring", "0")
    _sub(task_el, "OverAllocated", "0")
    _sub(task_el, "Estimated", "0")
    _sub(task_el, "Milestone", "1" if is_milestone else "0")
    _sub(task_el, "Summary", "1" if is_summary else "0")
    _sub(task_el, "DisplayAsSummary", "0")
    _sub(task_el, "Critical", "1" if on_critical else "0")
    _sub(task_el, "IsSubproject", "0")
    _sub(task_el, "IsSubprojectReadOnly", "0")
    _sub(task_el, "ExternalTask", "0")
    if early_start_dt:
        _sub(task_el, "EarlyStart", early_start_dt)
    if early_finish_dt:
        _sub(task_el, "EarlyFinish", early_finish_dt)
    if late_start_dt:
        _sub(task_el, "LateStart", late_start_dt)
    if late_finish_dt:
        _sub(task_el, "LateFinish", late_finish_dt)
    _sub(task_el, "StartVariance", "0")
    _sub(task_el, "FinishVariance", "0")
    _sub(task_el, "WorkVariance", "0.00")
    _sub(task_el, "FreeSlack", str(slack_tenths))
    _sub(task_el, "TotalSlack", str(slack_tenths))
    _sub(task_el, "StartSlack", str(slack_tenths))
    _sub(task_el, "FinishSlack", str(slack_tenths))
    _sub(task_el, "FixedCost", "0")
    _sub(task_el, "FixedCostAccrual", "3")
    _sub(task_el, "PercentComplete", str(percent))
    _sub(task_el, "PercentWorkComplete", str(percent))
    _sub(task_el, "Cost", "0")
    _sub(task_el, "OvertimeCost", "0")
    _sub(task_el, "OvertimeWork", "PT0H0M0S")
    _sub(task_el, "ActualDuration", "PT0H0M0S")
    _sub(task_el, "ActualCost", "0")
    _sub(task_el, "ActualOvertimeCost", "0")
    _sub(task_el, "ActualWork", "PT0H0M0S")
    _sub(task_el, "ActualOvertimeWork", "PT0H0M0S")
    _sub(task_el, "RegularWork", work_str)
    _sub(task_el, "RemainingDuration", duration_str)
    _sub(task_el, "RemainingCost", "0")
    _sub(task_el, "RemainingWork", work_str)
    _sub(task_el, "RemainingOvertimeCost", "0")
    _sub(task_el, "RemainingOvertimeWork", "PT0H0M0S")
    _sub(task_el, "ACWP", "0.00")
    _sub(task_el, "CV", "0.00")
    _sub(task_el, "ConstraintType", "0")
    _sub(task_el, "CalendarUID", cal_source_uid)
    _sub(task_el, "LevelAssignments", "1")
    _sub(task_el, "LevelingCanSplit", "1")
    _sub(task_el, "LevelingDelay", "0")
    _sub(task_el, "LevelingDelayFormat", "8")
    _sub(task_el, "IgnoreResourceCalendar", "0")
    _sub(task_el, "HideBar", "0")
    _sub(task_el, "Rollup", "1" if is_summary else "0")
    _sub(task_el, "BCWS", "0.00")
    _sub(task_el, "BCWP", "0.00")
    _sub(task_el, "PhysicalPercentComplete", "0")
    _sub(task_el, "EarnedValueMethod", "0")
    _sub(task_el, "IsPublished", "0" if is_summary else "1")
    _sub(task_el, "CommitmentType", "0")
    for dep in dep_map.get(uid, []):
        pred_el = ET.SubElement(task_el, "PredecessorLink")
        _sub(pred_el, "PredecessorUID", dep["pred_uid"])
        _sub(pred_el, "Type", DEPENDENCY_TYPE_CODES.get(dep["type"], "1"))
        _sub(pred_el, "CrossProject", "0")
        lag_tenths = _days_to_slack_tenths(dep["lag_days"], hours_per_day)
        _sub(pred_el, "LinkLag", str(lag_tenths))
        _sub(pred_el, "LagFormat", "7")


def _build_calendars(project: dict, calendars_el: ET.Element) -> None:
    """Write calendar elements."""
    all_day_names = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"]

    for cal in project.get("calendars", []):
        uid = cal.get("source-uid", "")
        if not uid:
            continue

        cal_el = ET.SubElement(calendars_el, "Calendar")
        _sub(cal_el, "UID", uid)
        _sub(cal_el, "Name", cal.get("name", ""))
        is_base = "1" if cal.get("is-base-calendar", False) else "0"
        _sub(cal_el, "IsBaseCalendar", is_base)
        _sub(cal_el, "IsBaselineCalendar", "0")
        _sub(cal_el, "BaseCalendarUID", "-1" if cal.get("is-base-calendar") else "1")

        working_days = set(cal.get("working-days") or [])
        if not working_days:
            continue

        weekdays_el = ET.SubElement(cal_el, "WeekDays")
        for day_name in all_day_names:
            day_type = DAY_NAME_TO_TYPE[day_name]
            is_working = day_name in working_days
            wd_el = ET.SubElement(weekdays_el, "WeekDay")
            _sub(wd_el, "DayType", day_type)
            _sub(wd_el, "DayWorking", "1" if is_working else "0")
            if is_working:
                wt_el = ET.SubElement(wd_el, "WorkingTimes")
                wt1 = ET.SubElement(wt_el, "WorkingTime")
                _sub(wt1, "FromTime", "08:00:00")
                _sub(wt1, "ToTime", "12:00:00")
                wt2 = ET.SubElement(wt_el, "WorkingTime")
                _sub(wt2, "FromTime", "13:00:00")
                _sub(wt2, "ToTime", "17:00:00")


def _build_resources(project: dict, resources_el: ET.Element) -> None:
    """Write resource elements."""
    res0 = ET.SubElement(resources_el, "Resource")
    _sub(res0, "UID", "0")
    _sub(res0, "ID", "0")
    _sub(res0, "Type", "1")
    _sub(res0, "IsNull", "0")
    _sub(res0, "MaxUnits", "1.00")

    for resource in project.get("resources", []):
        uid = _resource_uid(resource)
        if not uid:
            continue
        res_el = ET.SubElement(resources_el, "Resource")
        _sub(res_el, "UID", uid)
        _sub(res_el, "ID", uid)
        _sub(res_el, "Name", resource.get("name", ""))
        _sub(res_el, "Type", "1")
        _sub(res_el, "IsNull", "0")
        initials = resource.get("initials") or resource.get("name", "?")[0].upper()
        _sub(res_el, "Initials", initials)
        _sub(res_el, "MaxUnits", "1.00")
        _sub(res_el, "CanLevel", "1")
        _sub(res_el, "AccrueAt", "3")

        cal_uid = _find_resource_calendar_uid(project, resource)
        if cal_uid:
            _sub(res_el, "CalendarUID", cal_uid)

        _sub(res_el, "IsGeneric", "0")
        _sub(res_el, "IsInactive", "0")
        _sub(res_el, "IsEnterprise", "0")
        _sub(res_el, "BookingType", "0")
        _sub(res_el, "IsCostResource", "0")
        _sub(res_el, "IsBudget", "0")


def _find_resource_calendar_uid(project: dict, resource: dict) -> Optional[str]:
    """Find the calendar UID for a resource by matching calendar name to resource name."""
    resource_name = resource.get("name", "").lower()
    for cal in project.get("calendars", []):
        if cal.get("name", "").lower() == resource_name:
            return cal.get("source-uid")
    return None


def _build_assignments(
    project: dict,
    schedule: dict,
    assignments_el: ET.Element,
) -> None:
    """
    Write assignment elements in the canonical MS Project XML field order.

    Every task gets exactly one assignment.  Tasks with assigned resources
    get ResourceUID set to the resource's MS Project UID; tasks without
    assigned resources get ResourceUID=-65535 (the MS Project unassigned
    placeholder).  All fields are written in schema order.
    """
    task_map = {_task_uid(t): t for t in project.get("tasks", []) if _task_uid(t)}
    resource_uid_map = {
        r.get("id"): _resource_uid(r)
        for r in project.get("resources", [])
        if r.get("id") and _resource_uid(r)
    }
    hours_per_day = schedule.get("hours-per-day", 8.0)
    assign_uid = 1

    for task in project.get("tasks", []):
        task_uid = _task_uid(task)
        if not task_uid:
            continue

        task_obj = task_map.get(task_uid)
        if not task_obj:
            continue

        resources = task.get("assigned-resources", [])
        is_milestone = task.get("is-milestone", False)
        duration_days = task_obj.get("duration-days") or 0.0
        use_is_start = duration_days > 0.0

        # Resolve start/finish from computed offsets
        start_offset = task_obj.get("earliest-start")
        if start_offset is None or start_offset is False:
            start_offset = task_obj.get("start-offset")
        finish_offset = task_obj.get("earliest-finish")
        if finish_offset is None or finish_offset is False:
            finish_offset = task_obj.get("end-offset")

        start_dt = _offset_to_dt(start_offset, schedule, is_start=use_is_start)
        finish_dt = _offset_to_dt(finish_offset, schedule)
        work_str = _days_to_iso_duration(duration_days, hours_per_day)
        percent = str(_percent_complete(task))

        # Build list of resource UIDs — fall back to unassigned placeholder
        resource_uids = [resource_uid_map[r] for r in resources if r in resource_uid_map]
        if not resource_uids:
            resource_uids = ["-65535"]

        for res_uid in resource_uids:
            is_unassigned = res_uid == "-65535"
            task_work = work_str

            assign_el = ET.SubElement(assignments_el, "Assignment")
            _sub(assign_el, "UID", str(assign_uid))
            _sub(assign_el, "TaskUID", task_uid)
            _sub(assign_el, "ResourceUID", res_uid)
            _sub(assign_el, "PercentWorkComplete", percent)
            _sub(assign_el, "ActualCost", "0")
            _sub(assign_el, "ActualOvertimeCost", "0")
            _sub(assign_el, "ActualOvertimeWork", "PT0H0M0S")
            _sub(assign_el, "ActualWork", "PT0H0M0S")
            _sub(assign_el, "ACWP", "0.00")
            _sub(assign_el, "Confirmed", "0")
            _sub(assign_el, "Cost", "0")
            _sub(assign_el, "CostRateTable", "0")
            _sub(assign_el, "RateScale", "0")
            _sub(assign_el, "CostVariance", "0")
            _sub(assign_el, "CV", "0.00")
            _sub(assign_el, "Delay", "0")
            if finish_dt:
                _sub(assign_el, "Finish", finish_dt)
            _sub(assign_el, "FinishVariance", "0")
            _sub(assign_el, "WorkVariance", "0.00")
            _sub(assign_el, "HasFixedRateUnits", "1")
            _sub(assign_el, "FixedMaterial", "0")
            _sub(assign_el, "LevelingDelay", "0")
            _sub(assign_el, "LevelingDelayFormat", "7")
            _sub(assign_el, "LinkedFields", "0")
            _sub(assign_el, "Milestone", "1" if is_milestone else "0")
            _sub(assign_el, "Overallocated", "0")
            _sub(assign_el, "OvertimeCost", "0")
            _sub(assign_el, "OvertimeWork", "PT0H0M0S")
            _sub(assign_el, "RegularWork", task_work)
            _sub(assign_el, "RemainingCost", "0")
            _sub(assign_el, "RemainingOvertimeCost", "0")
            _sub(assign_el, "RemainingOvertimeWork", "PT0H0M0S")
            _sub(assign_el, "RemainingWork", task_work)
            _sub(assign_el, "ResponsePending", "0")
            if start_dt:
                _sub(assign_el, "Start", start_dt)
            _sub(assign_el, "StartVariance", "0")
            _sub(assign_el, "Units", "1")
            _sub(assign_el, "UpdateNeeded", "0")
            _sub(assign_el, "VAC", "0.00")
            _sub(assign_el, "Work", task_work)
            _sub(assign_el, "WorkContour", "0")
            _sub(assign_el, "BCWS", "0.00")
            _sub(assign_el, "BCWP", "0.00")
            _sub(assign_el, "BookingType", "0")
            _sub(assign_el, "BudgetCost", "0")
            _sub(assign_el, "BudgetWork", "PT0H0M0S")
            assign_uid += 1


def _build_predecessor_map(project: dict) -> dict:
    """Build a map of task_uid -> list of predecessor dicts."""
    id_to_uid = {t["id"]: _task_uid(t) for t in project.get("tasks", []) if _task_uid(t)}

    dep_map: dict[str, list] = {}
    for dep in project.get("dependencies", []):
        from_id = dep.get("from-task", "")
        to_id = dep.get("to-task", "")
        pred_uid = id_to_uid.get(from_id)
        succ_uid = id_to_uid.get(to_id)
        if not pred_uid or not succ_uid:
            continue
        dep_map.setdefault(succ_uid, []).append({
            "pred_uid": pred_uid,
            "type": dep.get("type", "finish-to-start"),
            "lag_days": dep.get("lag-days", 0.0),
        })
    return dep_map


def export_msproject_xml(project: dict, xml_path: str) -> None:
    """
    Export a scheduled planner project dict to a Microsoft Project XML file.

    Args:
        project:  Project dict (output of merge_cpm_results).
        xml_path: Destination .xml file path.
    """
    schedule = project.get("schedule", {})
    hours_per_day = schedule.get("hours-per-day", 8.0)
    minutes_per_day = int(hours_per_day * 60)
    minutes_per_week = minutes_per_day * 5

    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S")
    proj_start_dt = _offset_to_dt(0.0, schedule)
    calc = project.get("calculated", {})
    proj_end_dt = _offset_to_dt(calc.get("latest-finish"), schedule)

    root = ET.Element("Project")
    root.set("xmlns", NS)

    _sub(root, "SaveVersion", "14")
    _sub(root, "Name", project.get("name", "Exported Project"))
    _sub(root, "Title", project.get("name", "Exported Project"))
    _sub(root, "Company", project.get("company", ""))
    _sub(root, "CreationDate", project.get("created", now))
    _sub(root, "LastSaved", now)
    _sub(root, "ScheduleFromStart", "1")
    if proj_start_dt:
        _sub(root, "StartDate", proj_start_dt)
    if proj_end_dt:
        _sub(root, "FinishDate", proj_end_dt)
    _sub(root, "CalendarUID", "1")
    _sub(root, "DefaultStartTime", "08:00:00")
    _sub(root, "DefaultFinishTime", "17:00:00")
    _sub(root, "MinutesPerDay", str(minutes_per_day))
    _sub(root, "MinutesPerWeek", str(minutes_per_week))
    _sub(root, "DaysPerMonth", "20")
    _sub(root, "DefaultTaskType", "0")
    _sub(root, "DurationFormat", "7")
    _sub(root, "WorkFormat", "2")
    _sub(root, "HonorConstraints", "0")
    _sub(root, "CriticalSlackLimit", "0")

    calendars_el = ET.SubElement(root, "Calendars")
    _build_calendars(project, calendars_el)

    tasks_el = ET.SubElement(root, "Tasks")
    dep_map = _build_predecessor_map(project)
    _build_project_summary_task(project, schedule, tasks_el)

    for task in project.get("tasks", []):
        _build_task_element(task, dep_map, schedule, tasks_el)

    resources_el = ET.SubElement(root, "Resources")
    _build_resources(project, resources_el)

    assignments_el = ET.SubElement(root, "Assignments")
    _build_assignments(project, schedule, assignments_el)

    ET.indent(root, space="\t")
    tree = ET.ElementTree(root)
    ET.register_namespace("", NS)
    tree.write(xml_path, encoding="UTF-8", xml_declaration=True)


def main() -> None:
    """CLI entry point: export a planner JSON file to MS Project XML."""
    parser = argparse.ArgumentParser(
        description="Export a scheduled planner JSON file to MS Project XML format."
    )
    parser.add_argument("json_file", help="Path to the scheduled planner JSON file")
    parser.add_argument("xml_file", help="Path to write the MS Project XML file")
    args = parser.parse_args()

    import json
    with open(args.json_file, "r", encoding="utf-8") as f:
        project = json.load(f)

    export_msproject_xml(project, args.xml_file)

    task_count = sum(1 for t in project.get("tasks", []) if not t.get("is-summary"))
    summary_count = sum(1 for t in project.get("tasks", []) if t.get("is-summary"))
    print(
        f"Exported: {task_count} tasks, {summary_count} summaries, "
        f"{len(project.get('dependencies', []))} dependencies, "
        f"{len(project.get('resources', []))} resources",
        file=sys.stderr,
    )
    print(f"Written to {args.xml_file}", file=sys.stderr)


if __name__ == "__main__":
    main()
