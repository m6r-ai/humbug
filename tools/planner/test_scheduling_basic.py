#!/usr/bin/env python3
"""
Basic scheduling test with a simple 3-task project.

Tests the forward-pass, backward-pass, and complete CPM scheduling.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))
sys.path.insert(0, str(Path(__file__).parent))

from menai import Menai
from menai_bridge import MenaiBridge


def print_section(title: str):
    """Print a formatted section header."""
    print(f"\n{'='*80}")
    print(f" {title}")
    print('='*80)


def create_simple_project():
    """
    Create a simple 3-task project:
    T1 (5 days) -> T2 (3 days) -> T3 (2 days)
    
    All using 5-day calendar, finish-to-start dependencies.
    """
    return {
        "project-id": "SIMPLE-TEST",
        "name": "Simple Test Project",
        "project-start": "2025-03-03",  # Monday
        
        "tasks": [
            {
                "id": "T1",
                "name": "Task 1",
                "duration-days": 5,
                "calendar-id": "cal1",
                "schedule-mode": "duration-based",
                "start-date": False,
                "end-date": False,
                "status": "not-started",
                "progress": 0.0,
            },
            {
                "id": "T2",
                "name": "Task 2",
                "duration-days": 3,
                "calendar-id": "cal1",
                "schedule-mode": "duration-based",
                "start-date": False,
                "end-date": False,
                "status": "not-started",
                "progress": 0.0,
            },
            {
                "id": "T3",
                "name": "Task 3",
                "duration-days": 2,
                "calendar-id": "cal1",
                "schedule-mode": "duration-based",
                "start-date": False,
                "end-date": False,
                "status": "not-started",
                "progress": 0.0,
            },
        ],
        
        "dependencies": [
            {
                "from-task": "T1",
                "to-task": "T2",
                "type": "finish-to-start",
                "lag-days": 0,
            },
            {
                "from-task": "T2",
                "to-task": "T3",
                "type": "finish-to-start",
                "lag-days": 0,
            },
        ],
        
        "calendars": [
            {
                "id": "cal1",
                "name": "5-day week",
                "type": "5-day",
                "working-days": {"mon", "tue", "wed", "thu", "fri"},
                "holidays": set(),
            }
        ],
    }


def main():
    """Run basic scheduling tests."""
    
    print_section("BASIC SCHEDULING TEST")
    
    menai = Menai()
    bridge = MenaiBridge()
    
    # Create simple project
    project = create_simple_project()
    project_expr = bridge.python_to_menai(project)
    
    print("\nProject structure:")
    print(f"  Start date: {project['project-start']}")
    print(f"  Tasks: {len(project['tasks'])}")
    print(f"  Dependencies: {len(project['dependencies'])}")
    print("\n  Task chain: T1 (5 days) -> T2 (3 days) -> T3 (2 days)")
    print(f"  Expected total: 10 working days")
    
    # === TEST 1: Forward pass ===
    print_section("TEST 1: Forward Pass")
    
    result = menai.evaluate(f'''
        (let* ((scheduling (import "tools/planner/scheduling"))
              (forward-pass (dict-get scheduling "forward-pass"))
              (project {project_expr}))
          (forward-pass project))
    ''')
    
    tasks = result.get('tasks', [])
    print(f"\n  Scheduled {len(tasks)} tasks:")
    
    for task in tasks:
        tid = task.get('id')
        start = task.get('earliest-start', 'N/A')
        end = task.get('earliest-finish', 'N/A')
        print(f"    {tid}: {start} -> {end}")
    
    if len(tasks) == 3:
        print(f"\n  ✓ All tasks scheduled")
        
        # Verify dates
        t1_start = tasks[0].get('earliest-start')
        t1_end = tasks[0].get('earliest-finish')
        t2_start = tasks[1].get('earliest-start')
        t2_end = tasks[1].get('earliest-finish')
        t3_start = tasks[2].get('earliest-start')
        t3_end = tasks[2].get('earliest-finish')
        
        print(f"\n  Verification:")
        print(f"    T1 starts: {t1_start} (should be 2025-01-01 or project-start)")
        print(f"    T1 ends: {t1_end}")
        print(f"    T2 starts: {t2_start} (should equal T1 end)")
        print(f"    T2 ends: {t2_end}")
        print(f"    T3 starts: {t3_start} (should equal T2 end)")
        print(f"    T3 ends: {t3_end}")
        
        # Check T2 starts when T1 ends
        if t2_start == t1_end:
            print(f"  ✓ T2 correctly starts when T1 ends")
        else:
            print(f"  ✗ T2 should start on {t1_end}, but starts on {t2_start}")
        
        # Check T3 starts when T2 ends
        if t3_start == t2_end:
            print(f"  ✓ T3 correctly starts when T2 ends")
        else:
            print(f"  ✗ T3 should start on {t2_end}, but starts on {t3_start}")
    else:
        print(f"  ✗ Expected 3 tasks, got {len(tasks)}")
    
    # === TEST 2: Full CPM (forward + backward + slack + critical path) ===
    print_section("TEST 2: Full CPM Analysis")
    
    result = menai.evaluate(f'''
        (let* ((scheduling (import "tools/planner/scheduling"))
              (schedule-project (dict-get scheduling "schedule-project"))
              (project {project_expr}))
          (schedule-project project))
    ''')
    
    tasks = result.get('tasks', [])
    critical_path = result.get('critical-path', [])
    
    print(f"\n  Scheduled {len(tasks)} tasks")
    print(f"  Critical path: {len(critical_path)} tasks")
    
    if len(tasks) == 3:
        print(f"\n  Task details:")
        for task in tasks:
            tid = task.get('id')
            e_start = task.get('earliest-start', 'N/A')
            e_end = task.get('earliest-finish', 'N/A')
            l_start = task.get('latest-start', 'N/A')
            l_end = task.get('latest-finish', 'N/A')
            slack = task.get('slack', 'N/A')
            
            print(f"    {tid}:")
            print(f"      Earliest: {e_start} -> {e_end}")
            print(f"      Latest:   {l_start} -> {l_end}")
            print(f"      Slack:    {slack} days")
        
        print(f"\n  Critical path tasks: {critical_path}")
        
        # In a simple chain, all tasks should be critical (zero slack)
        if len(critical_path) == 3:
            print(f"  ✓ All tasks on critical path (as expected for simple chain)")
        else:
            print(f"  ⚠ Expected all 3 tasks on critical path, got {len(critical_path)}")
    else:
        print(f"  ✗ Expected 3 tasks, got {len(tasks)}")
    
    # === TEST 3: Test with start-to-start dependency ===
    print_section("TEST 3: Start-to-Start Dependency")
    
    project_ss = create_simple_project()
    # Change T1->T2 to start-to-start with 2 day lag
    project_ss['dependencies'][0]['type'] = 'start-to-start'
    project_ss['dependencies'][0]['lag-days'] = 2
    
    project_ss_expr = bridge.python_to_menai(project_ss)
    
    print("\n  Modified: T1 --[SS, lag=2]--> T2")
    print("  T2 should start 2 days after T1 starts")
    
    result = menai.evaluate(f'''
        (let* ((scheduling (import "tools/planner/scheduling"))
              (forward-pass (dict-get scheduling "forward-pass"))
              (project {project_ss_expr}))
          (forward-pass project))
    ''')
    
    tasks = result.get('tasks', [])
    if len(tasks) >= 2:
        t1 = next((t for t in tasks if t['id'] == 'T1'), None)
        t2 = next((t for t in tasks if t['id'] == 'T2'), None)
        
        if t1 and t2:
            t1_start = t1.get('earliest-start')
            t2_start = t2.get('earliest-start')
            print(f"\n    T1 starts: {t1_start}")
            print(f"    T2 starts: {t2_start}")
            print(f"  (T2 should start 2 working days after T1)")
    
    # === SUMMARY ===
    print_section("SUMMARY")
    
    print("\n  Tests completed:")
    print(f"    • Forward pass: {len(tasks)} tasks scheduled")
    print(f"    • Full CPM: {len(critical_path)} critical path tasks")
    print(f"    • SS dependency: tested")
    
    print("\n" + "="*80)


if __name__ == "__main__":
    main()
