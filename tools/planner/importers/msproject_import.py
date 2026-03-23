#!/usr/bin/env python3
"""
MS Project XML importer for the project planner.

Converts a Microsoft Project XML file (.xml) into the canonical planner
JSON format defined in tools/planner/README.md.
"""

import json
import re
import xml.etree.ElementTree as ET
from datetime import datetime, timezone
from typing import Any, Optional


NS = "http://schemas.microsoft.com/project"

# MS Project dependency type codes -> planner strings
DEPENDENCY_TYPES = {
    "0": "finish-to-finish",
    "1": "finish-to-start",
    "2": "start-to-start",
    "3": "start-to-finish",
}

# MS Project WeekDay DayType codes (1=Sun, 2=Mon, ..., 7=Sat)
DAY_TYPE_NAMES = {
    "1": "sun",
    "2": "mon",
    "3": "tue",
    "4": "wed",
    "5": "thu",
    "6": "fri",
    "7": "sat",
}


def _tag(name: str) -> str:
    """Return a fully-qualified XML tag name."""
    return f"{{{NS}}}{name}"


def _text(element: ET.Element, tag: str, default: Any = None) -> Any:
    """Return the text of a child element, or default if absent."""
    child = element.find(_tag(tag))
    if child is None or child.text is None:
        return default
    return child.text.strip()


def _int(element: ET.Element, tag: str, default: int = 0) -> int:
    value = _text(element, tag)
    if value is None:
        return default
    try:
        return int(value)
    except ValueError:
        return default


def _float(element: ET.Element, tag: str, default: float = 0.0) -> float:
    value = _text(element, tag)
    if value is None:
        return default
    try:
        return float(value)
    except ValueError:
        return default


def _parse_iso_duration_hours(duration_str: str) -> Optional[float]:
    """
    Parse an ISO 8601 duration string of the form PTxHyMzS and return
    the total value in hours.  Returns None if the string is absent or
    cannot be parsed.
    """
    if not duration_str:
        return None
    match = re.match(r"PT(?:(\d+(?:\.\d+)?)H)?(?:(\d+(?:\.\d+)?)M)?(?:(\d+(?:\.\d+)?)S)?", duration_str)
    if not match:
        return None
    hours = float(match.group(1) or 0)
    minutes = float(match.group(2) or 0)
    seconds = float(match.group(3) or 0)
    return hours + minutes / 60.0 + seconds / 3600.0


def _hours_to_work_days(hours: Optional[float], hours_per_day: float) -> Optional[float]:
    """Convert a duration in hours to working days."""
    if hours is None or hours_per_day <= 0:
        return None
    days = hours / hours_per_day
    # Round to 2 decimal places; always return float
    return round(days, 2)


def _parse_datetime_date(dt_str: Optional[str]) -> Optional[str]:
    """
    Extract just the date part (YYYY-MM-DD) from an ISO datetime string.
    Returns None if the string is absent or not a real date.
    """
    if not dt_str:
        return None
    # Sentinel values used by MS Project for 'not set'
    if dt_str.startswith("1984-01-01") or dt_str.startswith("2049-12-31"):
        return None
    return dt_str[:10]


def _slugify(name: str) -> str:
    """Convert a resource name to a simple lowercase slug for use as an id."""
    slug = name.lower()
    slug = re.sub(r"[^a-z0-9]+", "-", slug)
    return slug.strip("-")


def _parse_calendars(root: ET.Element) -> list[dict]:
    """Parse <Calendars> into planner calendar objects."""
    calendars = []
    calendars_el = root.find(_tag("Calendars"))
    if calendars_el is None:
        return calendars

    for cal_el in calendars_el.findall(_tag("Calendar")):
        uid = _text(cal_el, "UID", "")
        name = _text(cal_el, "Name", f"Calendar-{uid}")
        is_base = _int(cal_el, "IsBaseCalendar", 0)

        working_days = set()
        non_working_days = set()

        weekdays_el = cal_el.find(_tag("WeekDays"))
        if weekdays_el is not None:
            for wd_el in weekdays_el.findall(_tag("WeekDay")):
                day_type = _text(wd_el, "DayType", "")
                day_working = _int(wd_el, "DayWorking", 0)
                day_name = DAY_TYPE_NAMES.get(day_type)
                if day_name:
                    if day_working:
                        working_days.add(day_name)
                    else:
                        non_working_days.add(day_name)

        # Determine calendar type from working days
        all_seven = {"mon", "tue", "wed", "thu", "fri", "sat", "sun"}
        standard_five = {"mon", "tue", "wed", "thu", "fri"}
        if working_days == all_seven:
            cal_type = "7-day"
        elif working_days == standard_five or (not working_days and not non_working_days):
            cal_type = "5-day"
        else:
            cal_type = "custom"

        calendars.append({
            "id": f"cal-{uid}",
            "name": name,
            "type": cal_type,
            "is-base-calendar": bool(is_base),
            "working-days": sorted(working_days) if working_days else sorted(standard_five),
            "holidays": [],
            "source-uid": uid,
        })

    return calendars


def _parse_resources(root: ET.Element) -> tuple[list[dict], dict[str, str]]:
    """
    Parse <Resources> into planner resource objects.

    Returns:
        resources: list of resource dicts
        uid_to_id: mapping from MS Project resource UID to planner resource id
    """
    resources = []
    uid_to_id: dict[str, str] = {}

    resources_el = root.find(_tag("Resources"))
    if resources_el is None:
        return resources, uid_to_id

    for res_el in resources_el.findall(_tag("Resource")):
        uid = _text(res_el, "UID", "")
        name = _text(res_el, "Name", "")
        res_type = _int(res_el, "Type", 1)  # 1=Work, 2=Material, 3=Cost
        is_null = _int(res_el, "IsNull", 0)

        # Skip the placeholder UID=0 resource and null resources
        if uid == "0" or is_null:
            continue
        # Skip unnamed resources
        if not name:
            continue

        initials = _text(res_el, "Initials", name[0].upper() if name else "")
        resource_id = _slugify(name)

        # Ensure uniqueness by appending uid if needed
        existing_ids = {r["id"] for r in resources}
        if resource_id in existing_ids:
            resource_id = f"{resource_id}-{uid}"

        uid_to_id[uid] = resource_id

        type_map = {1: "work", 2: "material", 3: "cost"}
        resources.append({
            "id": resource_id,
            "name": name,
            "initials": initials,
            "type": type_map.get(res_type, "work"),
            "external": False,
            "source-ids": {"msproject": uid},
        })

    return resources, uid_to_id


def _build_task_resource_map(root: ET.Element, uid_to_resource_id: dict[str, str]) -> dict[str, list[str]]:
    """
    Parse <Assignments> and return a mapping of task UID -> list of resource ids.
    Assignments with ResourceUID=-65535 (unassigned) are ignored.
    """
    task_resources: dict[str, list[str]] = {}

    assignments_el = root.find(_tag("Assignments"))
    if assignments_el is None:
        return task_resources

    for assign_el in assignments_el.findall(_tag("Assignment")):
        task_uid = _text(assign_el, "TaskUID", "")
        resource_uid = _text(assign_el, "ResourceUID", "")

        if not task_uid or not resource_uid:
            continue
        # -65535 means no resource assigned
        if resource_uid in ("-65535", "0"):
            continue

        resource_id = uid_to_resource_id.get(resource_uid)
        if resource_id is None:
            continue

        task_resources.setdefault(task_uid, [])
        if resource_id not in task_resources[task_uid]:
            task_resources[task_uid].append(resource_id)

    return task_resources


def _infer_status(percent_complete: int, milestone: bool) -> str:
    """Infer planner status from MS Project percent complete."""
    if milestone:
        return "complete" if percent_complete == 100 else "not-started"
    if percent_complete == 100:
        return "complete"
    if percent_complete > 0:
        return "in-progress"
    return "not-started"


def _parse_tasks(
    root: ET.Element,
    hours_per_day: float,
    task_resource_map: dict[str, list[str]],
    uid_to_cal_id: dict[str, str],
) -> tuple[list[dict], list[dict], list[dict], dict[str, str]]:
    """
    Parse <Tasks> into planner tasks, dependencies, and milestones.

    Returns:
        tasks: list of task dicts (all non-milestone tasks including summaries,
               with parent-id and children fields populated)
        dependencies: list of dependency dicts
        milestones: list of milestone dicts
        uid_to_task_id: mapping from MS Project task UID to planner task id
    """
    tasks = []
    dependencies = []
    milestones = []
    uid_to_task_id: dict[str, str] = {}

    tasks_el = root.find(_tag("Tasks"))
    if tasks_el is None:
        return tasks, dependencies, milestones, uid_to_task_id

    # First pass: collect all tasks and build uid->id map
    raw_tasks = []
    task_counter = 1

    for task_el in tasks_el.findall(_tag("Task")):
        uid = _text(task_el, "UID", "")
        # Skip the project summary task (UID=0)
        if uid == "0":
            continue

        is_null = _int(task_el, "IsNull", 0)
        if is_null:
            continue

        is_summary = _int(task_el, "Summary", 0)
        is_milestone = _int(task_el, "Milestone", 0)
        name = _text(task_el, "Name", f"Task-{uid}")

        task_id = f"T{task_counter:03d}"
        uid_to_task_id[uid] = task_id
        task_counter += 1

        raw_tasks.append((uid, task_id, name, is_summary, is_milestone, task_el))

    # Second pass: build full task/milestone objects and dependencies
    for uid, task_id, name, is_summary, is_milestone, task_el in raw_tasks:
        percent_complete = _int(task_el, "PercentComplete", 0)
        status = _infer_status(percent_complete, bool(is_milestone))
        progress = round(percent_complete / 100.0, 2)

        start_str = _text(task_el, "Start")
        finish_str = _text(task_el, "Finish")
        start_date = _parse_datetime_date(start_str)
        end_date = _parse_datetime_date(finish_str)

        duration_str = _text(task_el, "Duration", "")
        duration_hours = _parse_iso_duration_hours(duration_str)
        duration_days = _hours_to_work_days(duration_hours, hours_per_day)

        wbs = _text(task_el, "WBS", "")
        outline_level = _int(task_el, "OutlineLevel", 1)
        critical = _int(task_el, "Critical", 0)

        cal_uid = _text(task_el, "CalendarUID", "-1")
        if cal_uid == "-1":
            calendar_id = "cal-1"  # project default
        else:
            calendar_id = uid_to_cal_id.get(cal_uid, "cal-1")

        assigned_resources = task_resource_map.get(uid, [])
        owner = assigned_resources[0] if assigned_resources else None

        # Determine schedule mode
        if start_date and end_date:
            schedule_mode = "fixed-dates"
        elif duration_days is not None:
            schedule_mode = "duration-based"
        else:
            schedule_mode = "unscheduled"

        if is_milestone:
            milestones.append({
                "id": task_id,
                "name": name,
                "target-date": start_date or end_date,
                "actual-date": None,
                "status": status,
                "wbs": wbs,
                "source-ids": {"msproject": uid},
            })

        if not is_summary:
            task_dict: dict[str, Any] = {
                "id": task_id,
                "name": name,
                "owner": owner if not is_milestone else None,
                "team": None,
                "start-date": start_date or end_date,
                "end-date": start_date or end_date,
                "duration-days": 0.0 if is_milestone else duration_days,
                "schedule-mode": "fixed-dates" if is_milestone else schedule_mode,
                "calendar-type": "5-day",
                "calendar-id": calendar_id,
                "status": status,
                "progress": progress,
                "type": "milestone" if is_milestone else "task",
                "priority": "critical" if critical else "normal",
                "wbs": wbs,
                "outline-level": outline_level,
                "is-summary": False,
                "is-milestone": bool(is_milestone),
                "assigned-resources": assigned_resources,
                "authoritative-source": "msproject",
                "source-ids": {"msproject": uid},
                "last-updated": None,
                "external-parties": [],
                "parent-id": None,
                "children": [],
            }
            tasks.append(task_dict)

        # Parse predecessor links -> dependencies
        for pred_el in task_el.findall(_tag("PredecessorLink")):
            pred_uid = _text(pred_el, "PredecessorUID", "")
            if not pred_uid:
                continue

            dep_type_code = _text(pred_el, "Type", "1")
            dep_type = DEPENDENCY_TYPES.get(dep_type_code, "finish-to-start")

            # LinkLag is in tenths of a minute; LagFormat=7 means minutes
            link_lag_raw = _int(pred_el, "LinkLag", 0)
            lag_format = _text(pred_el, "LagFormat", "7")
            lag_days = _convert_lag_to_days(link_lag_raw, lag_format, hours_per_day)

            dependencies.append({
                "from-task": pred_uid,   # still UID at this stage; resolved below
                "to-task": uid,
                "type": dep_type,
                "lag-days": lag_days,
                "source": "msproject",
            })

    # Resolve dependency UIDs to planner task IDs, dropping any that reference
    # the project summary (UID=0) or unknown tasks
    resolved_deps = []
    for dep in dependencies:
        from_id = uid_to_task_id.get(dep["from-task"])
        to_id = uid_to_task_id.get(dep["to-task"])
        if from_id and to_id:
            resolved_deps.append({
                **dep,
                "from-task": from_id,
                "to-task": to_id,
            })

    # Build parent-child hierarchy.
    # MS Project XML emits tasks in outline order, so we can track the most
    # recent task at each outline level as we walk the list.
    all_task_map: dict[str, dict] = {t["id"]: t for t in tasks}
    level_stack: dict[int, str] = {}

    for task in tasks:
        level = task["outline-level"]
        parent_level = level - 1

        if parent_level > 0 and parent_level in level_stack:
            parent_id = level_stack[parent_level]
            task["parent-id"] = parent_id
            parent = all_task_map.get(parent_id)
            if parent is not None:
                parent["children"].append(task["id"])

        level_stack[level] = task["id"]
        # Invalidate any deeper levels now that we've moved to this level
        for deeper in [l for l in level_stack if l > level]:
            del level_stack[deeper]

    return tasks, resolved_deps, milestones, uid_to_task_id


def _convert_lag_to_days(link_lag: int, lag_format: str, hours_per_day: float) -> float:
    """
    Convert a MS Project LinkLag value to decimal working days.

    MS Project stores lag in tenths of a minute for most formats.
    LagFormat codes relevant here:
      3  = minutes (tenths of a minute stored)
      4  = elapsed minutes
      5  = hours (tenths of a minute stored)
      7  = days (tenths of a minute stored, where 1 day = minutes_per_day * 10)
      8  = weeks
      19 = percent
    In practice the raw value is always in tenths of a minute.
    """
    if link_lag == 0:
        return 0.0

    tenths_of_minute = link_lag
    total_minutes = tenths_of_minute / 10.0
    minutes_per_day = hours_per_day * 60.0

    if lag_format in ("7", "35"):   # days / elapsed days
        return round(total_minutes / minutes_per_day, 2)
    if lag_format in ("5", "33"):   # hours / elapsed hours
        return round(total_minutes / 60.0 / hours_per_day, 2)
    if lag_format in ("3", "31"):   # minutes / elapsed minutes
        return round(total_minutes / minutes_per_day, 2)
    if lag_format in ("8", "36"):   # weeks
        return round(total_minutes / (minutes_per_day * 5), 2)

    # Default: treat as minutes
    return round(total_minutes / minutes_per_day, 2)


def import_msproject_xml(xml_path: str) -> dict:
    """
    Parse a Microsoft Project XML file and return a planner-format project dict.

    Args:
        xml_path: Path to the .xml file exported from MS Project.

    Returns:
        A project dict in the canonical planner JSON format.
    """
    tree = ET.parse(xml_path)
    root = tree.getroot()

    # Project-level metadata
    name = _text(root, "Title") or _text(root, "Name") or "Imported Project"
    company = _text(root, "Company", "")
    created_str = _text(root, "CreationDate", "")
    last_saved_str = _text(root, "LastSaved", "")
    start_date = _parse_datetime_date(_text(root, "StartDate"))
    finish_date = _parse_datetime_date(_text(root, "FinishDate"))

    minutes_per_day = _int(root, "MinutesPerDay", 480)
    hours_per_day = minutes_per_day / 60.0

    # Parse calendars
    calendars = _parse_calendars(root)
    uid_to_cal_id = {c["source-uid"]: c["id"] for c in calendars}

    # Parse resources
    resources, uid_to_resource_id = _parse_resources(root)

    # Build task->resource assignment map
    task_resource_map = _build_task_resource_map(root, uid_to_resource_id)

    # Parse tasks, dependencies, milestones
    tasks, dependencies, milestones, uid_to_task_id = _parse_tasks(
        root, hours_per_day, task_resource_map, uid_to_cal_id
    )

    # Build id-mappings section
    id_mappings = {
        "msproject": {uid: tid for uid, tid in uid_to_task_id.items()}
    }

    now = datetime.now(timezone.utc).isoformat()

    project = {
        "project-id": re.sub(r"[^A-Z0-9\-]", "-", name.upper())[:32],
        "name": name,
        "company": company,
        "created": created_str or now,
        "last-modified": last_saved_str or now,
        "version": 1,
        "source-file": xml_path,
        "import-timestamp": now,
        "schedule": {
            "start-date": start_date,
            "finish-date": finish_date,
            "hours-per-day": hours_per_day,
        },
        "tasks": tasks,
        "dependencies": dependencies,
        "resources": resources,
        "external-parties": [],
        "milestones": milestones,
        "calendars": calendars,
        "id-mappings": id_mappings,
        "validation": {
            "last-validated": None,
            "errors": [],
            "warnings": [],
        },
        "calculated": {
            "critical-path": None,
            "earliest-start": None,
            "latest-finish": None,
            "total-duration-days": None,
            "calculation-timestamp": None,
        },
    }

    return project


def main() -> None:
    """CLI entry point: import an MS Project XML file and print JSON."""
    import argparse
    import sys

    parser = argparse.ArgumentParser(
        description="Import a Microsoft Project XML file into planner JSON format."
    )
    parser.add_argument("xml_file", help="Path to the MS Project XML file")
    parser.add_argument(
        "-o", "--output",
        help="Output JSON file path (default: print to stdout)",
        default=None,
    )
    parser.add_argument(
        "--indent",
        type=int,
        default=2,
        help="JSON indentation level (default: 2)",
    )
    args = parser.parse_args()

    project = import_msproject_xml(args.xml_file)

    all_tasks = project["tasks"]
    task_count = sum(1 for t in all_tasks if not t.get("is-summary") and not t.get("is-milestone"))
    summary_count = sum(1 for t in all_tasks if t.get("is-summary"))
    milestone_count = len(project["milestones"])
    dep_count = len(project["dependencies"])
    resource_count = len(project["resources"])
    calendar_count = len(project["calendars"])

    print(
        f"Imported: {task_count} tasks, {summary_count} summaries, "
        f"{milestone_count} milestones, {dep_count} dependencies, "
        f"{resource_count} resources, {calendar_count} calendars",
        file=sys.stderr,
    )

    output_json = json.dumps(project, indent=args.indent, default=str)

    if args.output:
        with open(args.output, "w", encoding="utf-8") as f:
            f.write(output_json)
        print(f"Written to {args.output}", file=sys.stderr)
    else:
        print(output_json)


if __name__ == "__main__":
    main()
