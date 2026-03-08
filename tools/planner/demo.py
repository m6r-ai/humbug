#!/usr/bin/env python3
"""
Demonstration of project planning with Menai modules.

This script loads the example project and demonstrates:
1. Loading project data into Menai
2. Querying project information using modules
3. Basic analysis operations
"""

import sys
from pathlib import Path

# Add paths
sys.path.insert(0, str(Path(__file__).parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from example_project import create_example_project
from menai_bridge import MenaiBridge
from menai import Menai


def print_section(title: str):
    """Print a formatted section header."""
    print("\n" + "=" * 80)
    print(f" {title}")
    print("=" * 80)


def main():
    """Run project planning demonstration."""
    
    print_section("PROJECT PLANNING WITH Menai - DEMONSTRATION")
    
    # Create example project
    print("\nLoading example project...")
    project = create_example_project()
    
    print(f"  Project: {project['name']}")
    print(f"  Tasks: {len(project['tasks'])}")
    print(f"  Dependencies: {len(project['dependencies'])}")
    print(f"  Resources: {len(project['resources'])}")
    print(f"  External Parties: {len(project['external-parties'])}")
    
    # Initialize Menai and bridge for data conversion
    menai = Menai()
    bridge = MenaiBridge()
    project_expr = bridge.python_to_menai(project)
    
    # === DEMO 1: Basic project queries ===
    print_section("DEMO 1: Basic Project Queries")
    
    print("\n1.1 Get all task IDs:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (map (lambda (task) (dict-get task "id")) 
               (dict-get project "tasks")))
    ''')
    print(f"  Result: {result[:10]}... ({len(result)} total)")
    
    print("\n1.2 Get all task names:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (map (lambda (task) (dict-get task "name")) 
               (dict-get project "tasks")))
    ''')
    for i, name in enumerate(result[:5], 1):
        print(f"  {i}. {name}")
    print(f"  ... ({len(result)} total tasks)")
    
    print("\n1.3 Count tasks by status:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((tasks (dict-get project "tasks"))
                (statuses (map (lambda (task) (dict-get task "status")) tasks)))
            (list
              (list "complete" (list-length (filter (lambda (s) (string=? s "complete")) statuses)))
              (list "in-progress" (list-length (filter (lambda (s) (string=? s "in-progress")) statuses)))
              (list "not-started" (list-length (filter (lambda (s) (string=? s "not-started")) statuses))))))
    ''')
    print("  Status counts:")
    for status, count in result:
        print(f"    {status}: {count}")
    
    # === DEMO 2: Filtering and searching ===
    print_section("DEMO 2: Filtering and Searching")
    
    print("\n2.1 Find all critical priority tasks:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((tasks (dict-get project "tasks"))
                (critical-tasks (filter 
                  (lambda (task) (string=? (dict-get task "priority") "critical"))
                  tasks)))
            (map (lambda (task) (list (dict-get task "id") (dict-get task "name"))) 
                 critical-tasks)))
    ''')
    print(f"  Found {len(result)} critical tasks:")
    for task_id, name in result[:5]:
        print(f"    {task_id}: {name}")
    if len(result) > 5:
        print(f"    ... and {len(result) - 5} more")
    
    print("\n2.2 Find all tasks involving external parties:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((tasks (dict-get project "tasks"))
                (external-tasks (filter 
                  (lambda (task) (> (list-length (dict-get task "external-parties")) 0))
                  tasks)))
            (map (lambda (task) 
                   (list (dict-get task "id") 
                         (dict-get task "name")
                         (dict-get task "external-parties"))) 
                 external-tasks)))
    ''')
    print(f"  Found {len(result)} tasks with external dependencies:")
    for task_id, name, parties in result:
        print(f"    {task_id}: {name}")
        print(f"      External parties: {parties}")
    
    print("\n2.3 Find all tasks owned by 'alice':")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((tasks (dict-get project "tasks"))
                (alice-tasks (filter 
                  (lambda (task) (string=? (dict-get task "owner") "alice"))
                  tasks)))
            (map (lambda (task) 
                   (list (dict-get task "id") 
                         (dict-get task "name")
                         (dict-get task "status"))) 
                 alice-tasks)))
    ''')
    print(f"  Alice owns {len(result)} tasks:")
    for task_id, name, status in result:
        print(f"    {task_id}: {name} ({status})")
    
    # === DEMO 3: Dependency analysis ===
    print_section("DEMO 3: Dependency Analysis")
    
    print("\n3.1 Find all dependencies for task T004:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((deps (dict-get project "dependencies"))
                (task-deps (filter 
                  (lambda (dep) (string=? (dict-get dep "to-task") "T004"))
                  deps)))
            (map (lambda (dep) 
                   (list (dict-get dep "from-task")
                         (dict-get dep "type")
                         (dict-get dep "lag-days"))) 
                 task-deps)))
    ''')
    print(f"  Task T004 has {len(result)} predecessor(s):")
    for from_task, dep_type, lag in result:
        print(f"    {from_task} --[{dep_type}, lag={lag}]--> T004")
    
    print("\n3.2 Find all tasks that depend on T004:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((deps (dict-get project "dependencies"))
                (task-deps (filter 
                  (lambda (dep) (string=? (dict-get dep "from-task") "T004"))
                  deps)))
            (map (lambda (dep) 
                   (list (dict-get dep "to-task")
                         (dict-get dep "type")
                         (dict-get dep "lag-days"))) 
                 task-deps)))
    ''')
    print(f"  {len(result)} task(s) depend on T004:")
    for to_task, dep_type, lag in result:
        print(f"    T004 --[{dep_type}, lag={lag}]--> {to_task}")
    
    print("\n3.3 Count dependency types:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((deps (dict-get project "dependencies"))
                (types (map (lambda (dep) (dict-get dep "type")) deps)))
            (list
              (list "finish-to-start" (list-length (filter (lambda (t) (string=? t "finish-to-start")) types)))
              (list "start-to-start" (list-length (filter (lambda (t) (string=? t "start-to-start")) types)))
              (list "finish-to-finish" (list-length (filter (lambda (t) (string=? t "finish-to-finish")) types)))
              (list "start-to-finish" (list-length (filter (lambda (t) (string=? t "start-to-finish")) types))))))
    ''')
    print("  Dependency type distribution:")
    for dep_type, count in result:
        if count > 0:
            print(f"    {dep_type}: {count}")
    
    # === DEMO 4: Resource analysis ===
    print_section("DEMO 4: Resource Analysis")
    
    print("\n4.1 Count tasks per team:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((tasks (dict-get project "tasks"))
                (teams (map (lambda (task) (dict-get task "team")) tasks)))
            (list
              (list "backend" (list-length (filter (lambda (t) (string=? t "backend")) teams)))
              (list "frontend" (list-length (filter (lambda (t) (string=? t "frontend")) teams)))
              (list "infra" (list-length (filter (lambda (t) (string=? t "infra")) teams)))
              (list "qa" (list-length (filter (lambda (t) (string=? t "qa")) teams)))
              (list "design" (list-length (filter (lambda (t) (string=? t "design")) teams)))
              (list "external" (list-length (filter (lambda (t) (string=? t "external")) teams))))))
    ''')
    print("  Tasks per team:")
    for team, count in result:
        if count > 0:
            print(f"    {team}: {count} tasks")
    
    print("\n4.2 Calculate total duration by team:")
    result = menai.evaluate(f'''
        (let ((project {project_expr}))
          (let ((tasks (dict-get project "tasks")))
            (list
              (list "backend" (fold + 0 (map (lambda (t) (dict-get t "duration-days"))
                (filter (lambda (t) (string=? (dict-get t "team") "backend")) tasks))))
              (list "frontend" (fold + 0 (map (lambda (t) (dict-get t "duration-days"))
                (filter (lambda (t) (string=? (dict-get t "team") "frontend")) tasks))))
              (list "infra" (fold + 0 (map (lambda (t) (dict-get t "duration-days"))
                (filter (lambda (t) (string=? (dict-get t "team") "infra")) tasks))))
              (list "qa" (fold + 0 (map (lambda (t) (dict-get t "duration-days"))
                (filter (lambda (t) (string=? (dict-get t "team") "qa")) tasks)))))))
    ''')
    print("  Total work-days per team:")
    for team, days in result:
        print(f"    {team}: {days} days")
    
    # === DEMO 5: Using validation module ===
    print_section("DEMO 5: Using Validation Module")
    
    print("\n5.1 Check for circular dependencies:")
    result = menai.evaluate(f'''
        (let ((validation (import "tools/planner/validation"))
              (detect-cycles (dict-get validation "detect-cycles"))
              (project {project_expr}))
          (detect-cycles (dict-get project "tasks") 
                        (dict-get project "dependencies")))
    ''')
    if len(result) > 0:
        print(f"  Found {len(result)} circular dependencies")
        print(f"  First cycle: {' -> '.join(result[0])}")
    else:
        print("  ✓ No circular dependencies found")
    
    print("\n5.2 Find orphaned tasks (no dependencies):")
    result = menai.evaluate(f'''
        (let ((validation (import "tools/planner/validation"))
              (find-orphans (dict-get validation "find-orphaned-tasks"))
              (project {project_expr}))
          (find-orphans (dict-get project "tasks") 
                       (dict-get project "dependencies")))
    ''')
    if len(result) > 0:
        print(f"  Found {len(result)} orphaned task(s): {result}")
    else:
        print("  ✓ No orphaned tasks found")
    
    # === DEMO 6: Using calendar module ===
    print_section("DEMO 6: Using Calendar Module")
    
    print("\n6.1 Calculate working days between dates:")
    result = menai.evaluate('''
        (let ((calendar (import "tools/planner/calendar"))
              (working-days-between (dict-get calendar "working-days-between"))
              (test-cal (dict
                (list "working-days" (list "mon" "tue" "wed" "thu" "fri"))
                (list "holidays" (list)))))
          (working-days-between "2025-03-03" "2025-03-14" test-cal))
    ''')
    print(f"  Working days from 2025-03-03 to 2025-03-14: {result} days")
    
    print("\n6.2 Add working days to a date:")
    result = menai.evaluate('''
        (let ((calendar (import "tools/planner/calendar"))
              (add-working-days (dict-get calendar "add-working-days"))
              (test-cal (dict
                (list "working-days" (list "mon" "tue" "wed" "thu" "fri"))
                (list "holidays" (list)))))
          (add-working-days "2025-03-03" 10 test-cal))
    ''')
    print(f"  2025-03-03 + 10 working days = {result}")
    
    # === SUMMARY ===
    print_section("SUMMARY")
    
    print("\nThis demonstration showed:")
    print("  ✓ Loading complex project data into Menai")
    print("  ✓ Querying tasks by various criteria")
    print("  ✓ Filtering and searching with lambda functions")
    print("  ✓ Analyzing dependencies and relationships")
    print("  ✓ Aggregating data (counts, sums)")
    print("  ✓ Using validation module for cycle detection")
    print("  ✓ Using calendar module for date arithmetic")
    print("\nNext steps:")
    print("  - Implement forward-pass scheduling")
    print("  - Build critical path calculation")
    print("  - Add backward-pass and slack calculation")
    print("  - Create what-if scenario support")
    
    print("\n" + "=" * 80)


if __name__ == "__main__":
    main()
