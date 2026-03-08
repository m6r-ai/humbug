# Project Planner - Query Examples

This document shows practical examples of querying project data with Menai.

## Setup

```python
from example_project import create_example_project
from menai_bridge import MenaiBridge

# Load project
project = create_example_project()

# Create bridge
bridge = MenaiBridge()

# Run query
result = bridge.evaluate_with_data(
    '<Menai expression>',
    {"project": project}
)
```

## Basic Queries

### Get all task names

```menai
(map (lambda (task) (alist-get task "name"))
     (alist-get project "tasks"))
```

**Result:**
```
['System Architecture Design',
 'API Design and Specification',
 'Database Schema Design',
 ...]
```

### Count total tasks

```menai
(list-length (alist-get project "tasks"))
```

**Result:** `20`

### Get task by ID

```menai
(find (lambda (task) (string=? (alist-get task "id") "T004"))
      (alist-get project "tasks"))
```

**Result:**
```python
{
  'id': 'T004',
  'name': 'Backend API Implementation',
  'owner': 'alice',
  'status': 'in-progress',
  ...
}
```

## Filtering

### Find all critical priority tasks

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) (string=? (alist-get task "priority") "critical"))
    tasks))
```

**Result:** 13 tasks with priority="critical"

### Find all incomplete tasks

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) 
      (boolean-not (string=? (alist-get task "status") "complete")))
    tasks))
```

**Result:** 17 tasks (3 complete, 17 not complete)

### Find all tasks owned by a specific person

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) (string=? (alist-get task "owner") "alice"))
    tasks))
```

**Result:** 3 tasks owned by alice

### Find all external vendor tasks

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) (string=? (alist-get task "team") "external"))
    tasks))
```

**Result:** 3 tasks owned by external vendors

## Aggregation

### Count tasks by status

```menai
(let ((tasks (alist-get project "tasks"))
      (statuses (map (lambda (task) (alist-get task "status")) tasks)))
  (list
    (list "complete" 
          (list-length (filter (lambda (s) (string=? s "complete")) statuses)))
    (list "in-progress" 
          (list-length (filter (lambda (s) (string=? s "in-progress")) statuses)))
    (list "not-started" 
          (list-length (filter (lambda (s) (string=? s "not-started")) statuses)))))
```

**Result:**
```python
[['complete', 3],
 ['in-progress', 2],
 ['not-started', 15]]
```

### Count tasks by team

```menai
(let ((tasks (alist-get project "tasks"))
      (teams (map (lambda (task) (alist-get task "team")) tasks)))
  (list
    (list "backend" 
          (list-length (filter (lambda (t) (string=? t "backend")) teams)))
    (list "frontend" 
          (list-length (filter (lambda (t) (string=? t "frontend")) teams)))
    (list "infra" 
          (list-length (filter (lambda (t) (string=? t "infra")) teams)))
    (list "qa" 
          (list-length (filter (lambda (t) (string=? t "qa")) teams)))
    (list "external" 
          (list-length (filter (lambda (t) (string=? t "external")) teams)))))
```

**Result:**
```python
[['backend', 6],
 ['frontend', 1],
 ['infra', 6],
 ['qa', 3],
 ['external', 3]]
```

### Calculate total duration by team

```menai
(let ((tasks (alist-get project "tasks")))
  (list
    (list "backend" 
          (fold + 0 
            (map (lambda (t) (alist-get t "duration-days"))
              (filter (lambda (t) (string=? (alist-get t "team") "backend")) 
                      tasks))))
    (list "infra" 
          (fold + 0 
            (map (lambda (t) (alist-get t "duration-days"))
              (filter (lambda (t) (string=? (alist-get t "team") "infra")) 
                      tasks))))))
```

**Result:**
```python
[['backend', 48.0],
 ['infra', 23.0]]
```

## Dependency Analysis

### Find all predecessors of a task

```menai
(let ((deps (alist-get project "dependencies")))
  (filter 
    (lambda (dep) (string=? (alist-get dep "to-task") "T004"))
    deps))
```

**Result:**
```python
[{'from-task': 'T002',
  'to-task': 'T004',
  'type': 'finish-to-start',
  'lag-days': 0,
  'source': 'jira'}]
```

### Find all successors of a task

```menai
(let ((deps (alist-get project "dependencies")))
  (filter 
    (lambda (dep) (string=? (alist-get dep "from-task") "T004"))
    deps))
```

**Result:**
```python
[{'from-task': 'T004', 'to-task': 'T006', 'type': 'start-to-start', ...},
 {'from-task': 'T004', 'to-task': 'T010', 'type': 'finish-to-start', ...},
 {'from-task': 'T004', 'to-task': 'T015', 'type': 'finish-to-start', ...}]
```

### Count dependency types

```menai
(let ((deps (alist-get project "dependencies"))
      (types (map (lambda (dep) (alist-get dep "type")) deps)))
  (list
    (list "finish-to-start" 
          (list-length (filter (lambda (t) (string=? t "finish-to-start")) types)))
    (list "start-to-start" 
          (list-length (filter (lambda (t) (string=? t "start-to-start")) types)))
    (list "finish-to-finish" 
          (list-length (filter (lambda (t) (string=? t "finish-to-finish")) types)))))
```

**Result:**
```python
[['finish-to-start', 22],
 ['start-to-start', 4],
 ['finish-to-finish', 1]]
```

### Find tasks with no predecessors (start tasks)

```menai
(let ((tasks (alist-get project "tasks"))
      (deps (alist-get project "dependencies"))
      (task-ids (map (lambda (t) (alist-get t "id")) tasks))
      (dependent-ids (map (lambda (d) (alist-get d "to-task")) deps)))
  (filter 
    (lambda (id) (boolean-not (list-member? id dependent-ids)))
    task-ids))
```

**Result:**
```python
['T001']  # Only T001 has no predecessors
```

## Complex Queries

### Find critical tasks with external dependencies

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) 
      (and (string=? (alist-get task "priority") "critical")
           (> (list-length (alist-get task "external-parties")) 0)))
    tasks))
```

**Result:** 3 tasks (T008, T010, T015)

### Find in-progress tasks owned by backend team

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) 
      (and (string=? (alist-get task "status") "in-progress")
           (string=? (alist-get task "team") "backend")))
    tasks))
```

**Result:** 2 tasks (T004, T005)

### Find all 7-day calendar tasks (deployment tasks)

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) (string=? (alist-get task "calendar-type") "7-day"))
    tasks))
```

**Result:** 4 tasks (T014, T018, T019, T020)

### Find tasks that are >50% complete

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task) (> (alist-get task "progress") 0.5))
    tasks))
```

**Result:** 3 tasks (all with progress=1.0)

## Transformation

### Extract just ID and name from all tasks

```menai
(let ((tasks (alist-get project "tasks")))
  (map 
    (lambda (task) 
      (list (alist-get task "id") 
            (alist-get task "name")))
    tasks))
```

**Result:**
```python
[['T001', 'System Architecture Design'],
 ['T002', 'API Design and Specification'],
 ['T003', 'Database Schema Design'],
 ...]
```

### Create summary for each task

```menai
(let ((tasks (alist-get project "tasks")))
  (map 
    (lambda (task) 
      (alist
        (list "id" (alist-get task "id"))
        (list "name" (alist-get task "name"))
        (list "owner" (alist-get task "owner"))
        (list "status" (alist-get task "status"))
        (list "duration" (alist-get task "duration-days"))))
    tasks))
```

**Result:** List of alists with just the summary fields

### Build owner → tasks mapping

```menai
(let ((tasks (alist-get project "tasks")))
  (map 
    (lambda (owner)
      (list owner
            (filter 
              (lambda (task) (string=? (alist-get task "owner") owner))
              tasks)))
    (list "alice" "bob" "eve")))
```

**Result:** List of [owner, [tasks]] pairs

## Pipeline Queries

### Find critical backend tasks and extract names

```menai
(let ((tasks (alist-get project "tasks")))
  (map 
    (lambda (task) (alist-get task "name"))
    (filter 
      (lambda (task) 
        (and (string=? (alist-get task "priority") "critical")
             (string=? (alist-get task "team") "backend")))
      tasks)))
```

**Result:**
```python
['API Design and Specification',
 'Backend API Implementation',
 'Database Implementation',
 'Authentication & Authorization']
```

### Calculate average task duration

```menai
(let ((tasks (alist-get project "tasks"))
      (durations (map (lambda (t) (alist-get t "duration-days")) tasks))
      (total (fold + 0 durations))
      (count (list-length durations)))
  (/ total count))
```

**Result:** `7.05` (average days per task)

### Find longest task

```menai
(let ((tasks (alist-get project "tasks"))
      (durations (map (lambda (t) (alist-get t "duration-days")) tasks))
      (max-duration (fold max 0 durations)))
  (find 
    (lambda (task) (= (alist-get task "duration-days") max-duration))
    tasks))
```

**Result:** Task T004 or T008 (both 15 days)

## Advanced Patterns

### Group tasks by status

```menai
(let ((tasks (alist-get project "tasks")))
  (alist
    (list "complete" 
          (filter (lambda (t) (string=? (alist-get t "status") "complete")) tasks))
    (list "in-progress" 
          (filter (lambda (t) (string=? (alist-get t "status") "in-progress")) tasks))
    (list "not-started" 
          (filter (lambda (t) (string=? (alist-get t "status") "not-started")) tasks))))
```

**Result:** Alist with status → [tasks] mapping

### Find tasks with most dependencies

```menai
(let ((tasks (alist-get project "tasks"))
      (deps (alist-get project "dependencies")))
  (map 
    (lambda (task)
      (let ((task-id (alist-get task "id"))
            (pred-count (list-length 
                          (filter 
                            (lambda (d) (string=? (alist-get d "to-task") task-id))
                            deps))))
        (list task-id 
              (alist-get task "name") 
              pred-count)))
    tasks))
```

**Result:** List of [task-id, name, predecessor-count] for each task

### Calculate task "weight" (duration × priority)

```menai
(let ((tasks (alist-get project "tasks"))
      (priority-score (lambda (p)
        (match p
          ("critical" 3)
          ("high" 2)
          ("medium" 1)
          (_ 0)))))
  (map 
    (lambda (task)
      (let ((duration (alist-get task "duration-days"))
            (priority (alist-get task "priority")))
        (list (alist-get task "id")
              (alist-get task "name")
              (* duration (priority-score priority)))))
    tasks))
```

**Result:** List of [task-id, name, weight] showing importance × effort

## Tips

### Use `let` for readability

Instead of:
```menai
(map (lambda (task) (alist-get task "name"))
     (filter (lambda (task) (string=? (alist-get task "status") "complete"))
             (alist-get project "tasks")))
```

Write:
```menai
(let ((tasks (alist-get project "tasks"))
      (complete-tasks (filter 
                        (lambda (task) 
                          (string=? (alist-get task "status") "complete"))
                        tasks)))
  (map (lambda (task) (alist-get task "name")) complete-tasks))
```

### Extract helper functions

```menai
(let ((tasks (alist-get project "tasks"))
      (is-critical? (lambda (task) 
                      (string=? (alist-get task "priority") "critical")))
      (get-name (lambda (task) (alist-get task "name"))))
  (map get-name (filter is-critical? tasks)))
```

### Use `match` for complex conditions

```menai
(let ((tasks (alist-get project "tasks")))
  (filter 
    (lambda (task)
      (match (alist-get task "status")
        ("complete" #t)
        ("in-progress" (> (alist-get task "progress") 0.5))
        (_ #f)))
    tasks))
```

## Next: Analysis Functions

These examples show **querying** existing data. The next phase will add **analysis** functions:

- `calculate-critical-path` - Find critical path through project
- `calculate-earliest-dates` - Forward pass scheduling
- `calculate-latest-dates` - Backward pass scheduling
- `find-slack` - Calculate float for each task
- `detect-cycles` - Find circular dependencies
- `resource-loading` - Calculate work per resource over time

Stay tuned!
