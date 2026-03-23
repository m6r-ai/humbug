#!/usr/bin/env python3
"""
End-to-end project planner: load a planner JSON file and run analysis.

Usage:
    python3 run_planner.py <project.json>

The JSON file is produced by:
    python3 tools/planner/importers/msproject_import.py <file.xml> -o <project.json>
"""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))
sys.path.insert(0, str(Path(__file__).parent))

from menai_bridge import MenaiBridge, expand_summary_dependencies, format_menai_result


def print_section(title: str) -> None:
    print(f"\n{'=' * 70}")
    print(f" {title}")
    print('=' * 70)


def main() -> None:
    parser = argparse.ArgumentParser(description="Run project planning analysis on a planner JSON file.")
    parser.add_argument("json_file", help="Path to the planner JSON file")
    args = parser.parse_args()

    bridge = MenaiBridge()

    # =========================================================================
    print_section("LOADING PROJECT")

    full_project = bridge.load_project(args.json_file)

    all_tasks = full_project["tasks"]
    summary_tasks = [t for t in all_tasks if t.get("is-summary", False)]
    leaf_tasks = [t for t in all_tasks if not t.get("is-summary", False)]

    print(f"  Project  : {full_project['name']}")
    print(f"  Tasks    : {len(leaf_tasks)} leaf  +  {len(summary_tasks)} summary  =  {len(all_tasks)} total")
    print(f"  Deps     : {len(full_project['dependencies'])} (original)")
    print(f"  Resources: {len(full_project['resources'])}")
    print(f"  Calendars: {len(full_project['calendars'])}")
    if full_project.get("milestones"):
        print(f"  Milestones: {len(full_project['milestones'])}")

    # =========================================================================
    print_section("PREPARING CPM INPUT")

    cpm_project = expand_summary_dependencies(full_project)

    print(f"  Leaf tasks       : {len(cpm_project['tasks'])}")
    print(f"  Expanded deps    : {len(cpm_project['dependencies'])}")

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
        flat_errors = []
        for e in errors:
            if isinstance(e, list):
                flat_errors.extend(e)
            elif e:
                flat_errors.append(str(e))
        if flat_errors:
            print(f"\n  Errors ({len(flat_errors)}):")
            for e in flat_errors[:10]:
                print(f"    • {e}")
            if len(flat_errors) > 10:
                print(f"    ... and {len(flat_errors) - 10} more")

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
        if t.get("earliest-start") and t.get("earliest-start") is not False
    )
    print(f"  Tasks scheduled    : {scheduled_count} / {len(scheduled_tasks)}")
    print(f"  Critical path      : {len(critical_ids)} tasks")

    if critical_ids:
        critical_tasks = [t for t in scheduled_tasks if t.get("id") in critical_ids]
        critical_tasks.sort(key=lambda t: t.get("earliest-start") or "")
        print(f"\n  Critical path tasks (in schedule order):")
        for t in critical_tasks:
            es = t.get("earliest-start", "?")
            ef = t.get("earliest-finish", "?")
            slack = t.get("slack-days", "?")
            print(f"    {t['id']:6s}  {es} → {ef}  slack={slack}  {t.get('name', '')}")

    # =========================================================================
    print_section("SCHEDULE SUMMARY")

    unscheduled = [
        t for t in scheduled_tasks
        if not t.get("earliest-start") or t.get("earliest-start") is False
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
            if t.get("earliest-start") and t["earliest-start"] is not False
        ]
        finishes = [
            t["earliest-finish"] for t in scheduled_tasks
            if t.get("earliest-finish") and t["earliest-finish"] is not False
        ]
        if starts and finishes:
            print(f"  Project start  : {min(starts)}")
            print(f"  Project finish : {max(finishes)}")

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
                and scheduled_map[t["id"]].get("earliest-start")
                and scheduled_map[t["id"]]["earliest-start"] is not False
            ]
            if assigned:
                total_days = sum(t.get("duration-days") or 0.0 for t in assigned)
                print(f"  {resource['name']:30s}  {len(assigned):3d} tasks  {total_days:6.1f} days")


if __name__ == "__main__":
    main()
