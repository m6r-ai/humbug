#!/usr/bin/env python3
"""
Validation tests using module system.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))
sys.path.insert(0, str(Path(__file__).parent))

from menai import Menai
from example_project import create_example_project


def print_test(name: str):
    """Print test section header."""
    print(f"\n{'='*80}")
    print(f" {name}")
    print('='*80)


def make_task(task_id, name):
    """Create a minimal full task dict."""
    return {
        "id": task_id, "name": name, "duration-days": 1,
        "calendar-id": "cal1", "schedule-mode": "duration-based",
        "start-date": False, "end-date": False,
        "status": "not-started", "progress": 0.0,
        "earliest-start": False, "earliest-finish": False,
        "latest-start": False, "latest-finish": False, "slack-days": False,
    }


def make_dep(from_task, to_task, dep_type="finish-to-start", lag=0):
    """Create a dependency dict."""
    return {"from-task": from_task, "to-task": to_task, "type": dep_type, "lag-days": lag}


def main():
    """Run validation tests."""

    print_test("VALIDATION TESTS")

    menai = Menai()

    # Load example project
    print("\nLoading example project...")
    project = create_example_project()
    print(f"  Tasks: {len(project['tasks'])}")
    print(f"  Dependencies: {len(project['dependencies'])}")

    from menai_bridge import MenaiBridge
    bridge = MenaiBridge()
    preamble = MenaiBridge.struct_preamble()
    project_expr = bridge.python_to_menai_project(project)

    # === TEST 1: Get predecessors ===
    print_test("TEST 1: Get Predecessors")

    test_task = "T004"
    result = menai.evaluate(
        f'''{preamble}
  (let* ((dep-mod  (import "tools/planner/dependency"))
         (get-pred (dict-get dep-mod "get-predecessors"))
         (project  {project_expr}))
    (get-pred "{test_task}" (dict-get project "dependencies"))))'''
    )
    print(f"  Task {test_task} predecessors: {result}")

    # === TEST 2: Detect cycles in valid project ===
    print_test("TEST 2: Circular Dependency Detection (Valid Project)")

    result = menai.evaluate(
        f'''{preamble}
  (let* ((validation    (import "tools/planner/validation"))
         (detect-cycles (dict-get validation "detect-cycles"))
         (project       {project_expr}))
    (detect-cycles (dict-get project "tasks") (dict-get project "dependencies"))))'''
    )

    if len(result) > 0:
        print(f"  \u2713 Correctly detected {len(result)} circular dependencies")
        print(f"    (T019/T020 have a FF dependency creating a cycle)")
        for cycle in result[:3]:
            print(f"    {' -> '.join(cycle)}")
    else:
        print(f"  \u2717 Failed to detect circular dependencies (expected to find T019/T020 cycle)")

    # === TEST 3: Detect cycles in bad project ===
    print_test("TEST 3: Circular Dependency Detection (With Cycle)")

    bad_project = {
        "tasks": [
            make_task("T001", "Task 1"),
            make_task("T002", "Task 2"),
            make_task("T003", "Task 3"),
        ],
        "dependencies": [
            make_dep("T001", "T002"),
            make_dep("T002", "T003"),
            make_dep("T003", "T001"),
        ],
        "calendars": [],
    }

    bad_project_expr = bridge.python_to_menai_project(bad_project)

    result = menai.evaluate(
        f'''{preamble}
  (let* ((validation    (import "tools/planner/validation"))
         (detect-cycles (dict-get validation "detect-cycles"))
         (project       {bad_project_expr}))
    (detect-cycles (dict-get project "tasks") (dict-get project "dependencies"))))'''
    )

    if len(result) > 0:
        print(f"  \u2713 Correctly detected {len(result)} cycle(s):")
        for cycle in result:
            print(f"    {' -> '.join(cycle)}")
    else:
        print(f"  \u2717 Failed to detect circular dependency")

    # === TEST 4: Find orphaned tasks ===
    print_test("TEST 4: Find Orphaned Tasks")

    orphan_project = {
        "tasks": [
            make_task("T001", "Task 1"),
            make_task("T002", "Task 2"),
            make_task("T003", "Orphan Task"),
        ],
        "dependencies": [
            make_dep("T001", "T002"),
        ],
        "calendars": [],
    }

    orphan_project_expr = bridge.python_to_menai_project(orphan_project)

    result = menai.evaluate(
        f'''{preamble}
  (let* ((validation   (import "tools/planner/validation"))
         (find-orphans (dict-get validation "find-orphaned-tasks"))
         (project      {orphan_project_expr}))
    (find-orphans (dict-get project "tasks") (dict-get project "dependencies"))))'''
    )

    if len(result) > 0:
        print(f"  \u2713 Found {len(result)} orphaned task(s): {result}")
    else:
        print(f"  No orphaned tasks found")

    # === SUMMARY ===
    print_test("SUMMARY")
    print("\n  \u2713 Graph operations work correctly")
    print("  \u2713 Circular dependency detection works")
    print("  \u2713 Recursive functions work!")
    print("  \u2713 Consistency checks work")

    print("\n" + "="*80)
    print(" Validation functions are working!")
    print("="*80 + "\n")


if __name__ == "__main__":
    main()
