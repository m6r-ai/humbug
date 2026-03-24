#!/usr/bin/env python3
"""
End-to-end project planner: load a planner JSON file and run analysis.

Usage:
    python3 run_planner.py <project.json> [-o output.json] [-x output.xml]

The input JSON file is produced by:
    python3 tools/planner/importers/msproject_import.py <file.xml> -o <project.json>
"""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))
sys.path.insert(0, str(Path(__file__).parent))
sys.path.insert(0, str(Path(__file__).parent / "importers"))

from menai_bridge import (
    MenaiBridge,
    expand_summary_dependencies,
    merge_cpm_results,
    save_project,
)
from msproject_export import export_msproject_xml
from msproject_import import _offset_to_datetime


def print_section(title: str) -> None:
    print(f"\n{'=' * 70}")
    print(f" {title}")
    print('=' * 70)


def _fmt_offset(offset, schedule: dict, is_start: bool = False) -> str:
    """Convert a working-day offset to a display datetime string."""
    if offset is None or offset is False:
        return "?"
    try:
        return _offset_to_datetime(
            float(offset),
            schedule["project-start-datetime"],
            set(schedule.get("working-days", ["mon", "tue", "wed", "thu", "fri"])),
            schedule.get("hours-per-day", 8.0),
            schedule.get("working-periods"),
            is_start=is_start,
        )
    except Exception:
        return str(offset)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Run project planning analysis on a planner JSON file."
    )
    parser.add_argument("json_file", help="Path to the input planner JSON file")
    parser.add_argument(
        "-o", "--output",
        help="Path to write the scheduled project JSON (optional)",
        default=None,
    )
    parser.add_argument(
        "-x", "--xml",
        help="Path to write the scheduled project as MS Project XML (optional)",
        default=None,
    )
    args = parser.parse_args()

    bridge = MenaiBridge()

    # =========================================================================
    print_section("LOADING PROJECT")

    full_project = bridge.load_project(args.json_file)
    schedule = full_project.get("schedule", {})

    all_tasks = full_project["tasks"]
    summary_tasks = [t for t in all_tasks if t.get("is-summary", False)]
    leaf_tasks = [t for t in all_tasks if not t.get("is-summary", False) and not t.get("is-milestone", False)]
    milestone_tasks = [t for t in all_tasks if t.get("is-milestone", False)]

    print(f"  Project   : {full_project['name']}")
    print(f"  Tasks     : {len(leaf_tasks)} leaf  +  {len(summary_tasks)} summary  +  {len(milestone_tasks)} milestone  =  {len(all_tasks)} total")
    print(f"  Deps      : {len(full_project['dependencies'])} (original)")
    print(f"  Resources : {len(full_project['resources'])}")
    print(f"  Calendars : {len(full_project['calendars'])}")
    print(f"  Project start : {schedule.get('project-start-datetime', '?')}")

    # =========================================================================
    print_section("PREPARING CPM INPUT")

    cpm_project = expand_summary_dependencies(full_project)

    print(f"  Leaf + milestone tasks : {len(cpm_project['tasks'])}")
    print(f"  Expanded deps          : {len(cpm_project['dependencies'])}")

    preamble = MenaiBridge.struct_preamble()
    cpm_project_expr = bridge.python_to_menai_project(cpm_project)

    # =========================================================================
    print_section("VALIDATION")

    validation = bridge.evaluate(f'''
        {preamble}
          (let* ((validation (import "tools/planner/validation"))
                 (validate   (dict-get validation "validate-project"))
                 (project    {cpm_project_expr}))
            (validate project)))
    ''')

    valid = validation.get("valid", False)
    errors = validation.get("errors", [])
    cycles = validation.get("circular-dependencies-found", 0)
    tasks_with_errors = validation.get("tasks-with-errors", 0)
    deps_with_errors = validation.get("dependencies-with-errors", 0)

    print(f"  Valid              : {'✓' if valid else '✗'}")
    print(f"  Task errors        : {tasks_with_errors}")
    print(f"  Dependency errors  : {deps_with_errors}")
    print(f"  Circular deps      : {cycles}")

    if errors:
        print(f"\n  Errors ({len(errors)}):")
        for e in errors[:10]:
            print(f"    • {e}")
        if len(errors) > 10:
            print(f"    ... and {len(errors) - 10} more")

    if cycles > 0:
        print("\n  ⚠ Circular dependencies detected — scheduling results may be incomplete.")

    if not valid:
        print("\n  ⚠ Proceeding with scheduling despite validation errors.")

    # =========================================================================
    print_section("SCHEDULING (CPM)")

    scheduled = bridge.evaluate(f'''
        {preamble}
          (let* ((scheduling (import "tools/planner/scheduling"))
                 (schedule   (dict-get scheduling "schedule-project"))
                 (project    {cpm_project_expr}))
            (schedule project)))
    ''')

    scheduled_tasks = scheduled.get("tasks", [])
    critical_ids = set(scheduled.get("critical-path", []) or [])

    scheduled_count = sum(
        1 for t in scheduled_tasks
        if t.get("earliest-start") is not None and t["earliest-start"] is not False
    )
    print(f"  Tasks scheduled    : {scheduled_count} / {len(scheduled_tasks)}")
    print(f"  Critical path      : {len(critical_ids)} tasks")

    if critical_ids:
        critical_tasks = [t for t in scheduled_tasks if t.get("id") in critical_ids]
        critical_tasks.sort(key=lambda t: t.get("earliest-start") or 0.0)
        print(f"\n  Critical path tasks (in schedule order):")
        for t in critical_tasks:
            es = _fmt_offset(t.get("earliest-start"), schedule, is_start=True)
            ef = _fmt_offset(t.get("earliest-finish"), schedule)
            slack = t.get("slack-days", "?")
            print(f"    {t['id']:6s}  {es} → {ef}  slack={slack}  {t.get('name', '')}")

    # =========================================================================
    print_section("SCHEDULE SUMMARY")

    unscheduled = [
        t for t in scheduled_tasks
        if t.get("earliest-start") is None or t["earliest-start"] is False
    ]
    if unscheduled:
        print(f"  ⚠ {len(unscheduled)} task(s) could not be scheduled:")
        for t in unscheduled[:10]:
            print(f"    {t['id']:6s}  {t.get('name', '')}")
        if len(unscheduled) > 10:
            print(f"    ... and {len(unscheduled) - 10} more")
    else:
        starts = [
            t["earliest-start"] for t in scheduled_tasks
            if t.get("earliest-start") is not None and t.get("earliest-start") is not False
        ]
        finishes = [
            t["earliest-finish"] for t in scheduled_tasks
            if t.get("earliest-finish") is not None and t.get("earliest-finish") is not False
        ]
        if starts and finishes:
            print(f"  Project start  : {_fmt_offset(min(starts), schedule, is_start=True)}")
            print(f"  Project finish : {_fmt_offset(max(finishes), schedule)}")

    # =========================================================================
    print_section("RESOURCE LOADING")

    scheduled_map = {t["id"]: t for t in scheduled_tasks}
    resources = full_project.get("resources", [])
    if resources:
        for resource in resources:
            rid = resource["id"]
            assigned = [
                scheduled_map[t["id"]]
                for t in leaf_tasks
                if rid in t.get("assigned-resources", [])
                and t["id"] in scheduled_map
                and scheduled_map[t["id"]].get("earliest-start") is not None
                and scheduled_map[t["id"]]["earliest-start"] is not False
            ]
            if assigned:
                total_days = sum(t.get("duration-days") or 0.0 for t in assigned)
                print(f"  {resource['name']:30s}  {len(assigned):3d} tasks  {total_days:6.1f} days")

    # =========================================================================
    output_project = None

    if args.output or args.xml:
        output_project = merge_cpm_results(full_project, scheduled, critical_ids)

    if args.output:
        print_section("EXPORTING JSON")
        save_project(output_project, args.output)
        print(f"  Written to: {args.output}")
        calc = output_project.get("calculated", {})
        print(f"  Project start  : {_fmt_offset(calc.get('earliest-start'), schedule, is_start=True)}")
        print(f"  Project finish : {_fmt_offset(calc.get('latest-finish'), schedule)}")
        print(f"  Total work     : {calc.get('total-duration-days')} days")
        print(f"  Critical path  : {len(calc.get('critical-path', []))} tasks")

    if args.xml:
        print_section("EXPORTING XML")
        export_msproject_xml(output_project, args.xml)
        print(f"  Written to: {args.xml}")


if __name__ == "__main__":
    main()
