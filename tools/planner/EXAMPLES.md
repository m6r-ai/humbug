# Project Planner - Query Examples

Practical examples of querying project data with Menai.  All examples assume the
project has been loaded and converted to a Menai expression via the bridge.

## Setup

```python
import json
from menai_bridge import MenaiBridge, expand_summary_dependencies

bridge = MenaiBridge()

# Load from JSON file
project = bridge.load_project("plan.json")

# For queries that don't need scheduling, pass the full project directly
preamble = MenaiBridge.struct_preamble()
project_expr = bridge.python_to_menai_project(project)

# Run a query
result = bridge.evaluate(f'''
    {preamble}
    (let* ((project {project_expr}))
      <expression>)
''')
```

## Task Queries

Tasks in the project dict are Menai structs.  Use `struct-get` with a symbol
to access fields, or `dict-get` for the outer project dict.

### Get all task names

```menai
(map-list (lambda (t) (struct-get t 'name))
          (dict-get project "tasks"))
```

### Count tasks by status

```menai
(let ((tasks (dict-get project "tasks")))
  (list
    (list "not-started"
          (list-length (filter-list (lambda (t) (string=? (struct-get t 'status) "not-started")) tasks)))
    (list "in-progress"
          (list-length (filter-list (lambda (t) (string=? (struct-get t 'status) "in-progress")) tasks)))
    (list "complete"
          (list-length (filter-list (lambda (t) (string=? (struct-get t 'status) "complete")) tasks)))))
```

### Find tasks by owner

```menai
(filter-list (lambda (t) (string=? (struct-get t 'owner) "alice"))
             (dict-get project "tasks"))
```

### Find leaf tasks only (exclude summaries and milestones)

```menai
(filter-list (lambda (t)
               (and (string=? (struct-get t 'schedule-mode) "duration-based")
                    (float>? (struct-get t 'duration-days) 0.0)))
             (dict-get project "tasks"))
```

### Find tasks on the critical path

After scheduling, tasks have a `slack-days` field.  Tasks with `slack-days <= 0`
are on the critical path.

```menai
(filter-list (lambda (t) (float<=? (struct-get t 'slack-days) 0.0))
             (dict-get project "tasks"))
```

### Find task by ID

```menai
(find-list (lambda (t) (string=? (struct-get t 'id) "T029"))
           (dict-get project "tasks"))
```

### Calculate total duration of leaf tasks

```menai
(let ((tasks (dict-get project "tasks")))
  (fold-list float+
             0.0
             (map-list (lambda (t) (struct-get t 'duration-days))
                       (filter-list (lambda (t) (float>? (struct-get t 'duration-days) 0.0))
                                    tasks))))
```

## Dependency Queries

Dependencies are also structs with fields `from-task`, `to-task`, `type`, `lag-days`.

### Find all predecessors of a task

```menai
(let ((dep-mod (import "tools/planner/dependency"))
      (get-pred (dict-get dep-mod "get-predecessors")))
  (get-pred "T030" (dict-get project "dependencies")))
```

### Find all successors of a task

```menai
(let ((dep-mod (import "tools/planner/dependency"))
      (get-succ (dict-get dep-mod "get-successors")))
  (get-succ "T029" (dict-get project "dependencies")))
```

### Count dependency types

```menai
(let ((deps (dict-get project "dependencies")))
  (list
    (list "finish-to-start"
          (list-length (filter-list (lambda (d) (string=? (struct-get d 'type) "finish-to-start")) deps)))
    (list "start-to-start"
          (list-length (filter-list (lambda (d) (string=? (struct-get d 'type) "start-to-start")) deps)))
    (list "finish-to-finish"
          (list-length (filter-list (lambda (d) (string=? (struct-get d 'type) "finish-to-finish")) deps)))))
```

### Find dependencies with negative lag (lead time)

```menai
(filter-list (lambda (d) (float<? (struct-get d 'lag-days) 0.0))
             (dict-get project "dependencies"))
```

## Validation Queries

### Check for circular dependencies

```menai
(let ((validation    (import "tools/planner/validation"))
      (detect-cycles (dict-get validation "detect-cycles")))
  (detect-cycles (dict-get project "tasks")
                 (dict-get project "dependencies")))
```

Returns a list of cycles (each cycle is a list of task IDs).  Empty list = no cycles.

### Find orphaned tasks (no dependencies at all)

```menai
(let ((validation   (import "tools/planner/validation"))
      (find-orphans (dict-get validation "find-orphaned-tasks")))
  (find-orphans (dict-get project "tasks")
                (dict-get project "dependencies")))
```

### Full project validation

```menai
(let ((validation (import "tools/planner/validation"))
      (validate   (dict-get validation "validate-project")))
  (validate project))
```

Returns a dict with `"valid"`, `"errors"`, `"task-count"`, `"dependency-count"`,
`"circular-dependencies-found"` etc.

## Scheduling Queries

### Run full CPM schedule

```menai
(let ((scheduling (import "tools/planner/scheduling"))
      (schedule   (dict-get scheduling "schedule-project")))
  (schedule project))
```

Returns the project with `earliest-start`, `earliest-finish`, `latest-start`,
`latest-finish`, and `slack-days` populated on each task, plus a `"critical-path"`
list of task IDs.

### Forward pass only

```menai
(let ((scheduling    (import "tools/planner/scheduling"))
      (forward-pass  (dict-get scheduling "forward-pass")))
  (forward-pass project))
```

### Find the longest chain (project duration)

After scheduling:

```menai
(let ((tasks (dict-get project "tasks")))
  (fold-list float-max
             0.0
             (map-list (lambda (t) (struct-get t 'earliest-finish))
                       tasks)))
```

## Resource Queries

Resources are plain dicts (not structs) in the project.

### List all resource names

```menai
(map-list (lambda (r) (dict-get r "name"))
          (dict-get project "resources"))
```

### Find tasks assigned to a resource

Given a resource id (e.g. `"developer"`), find all tasks where it appears in
`assigned-resources`.  Note: `assigned-resources` is a list field on the task
dict, not a struct field.

```python
# This is easier in Python after getting the scheduled project back:
resource_id = "developer"
assigned = [t for t in scheduled_tasks
            if resource_id in t.get("assigned-resources", [])]
```

## Python-Side Queries

Some queries are more natural in Python after retrieving the scheduled project.

### Summary of scheduled project

```python
from menai_bridge import MenaiBridge, expand_summary_dependencies, merge_cpm_results

bridge = MenaiBridge()
project = bridge.load_project("plan.json")
cpm_project = expand_summary_dependencies(project)
preamble = MenaiBridge.struct_preamble()
project_expr = bridge.python_to_menai_project(cpm_project)

scheduled = bridge.evaluate(f'''
    {preamble}
    (let* ((scheduling (import "tools/planner/scheduling"))
           (schedule   (dict-get scheduling "schedule-project"))
           (project    {project_expr}))
      (schedule project))
''')

critical_ids = set(scheduled.get("critical-path", []))
output = merge_cpm_results(project, scheduled, critical_ids)

# Now query the output in Python
tasks = output["tasks"]
critical = [t for t in tasks if t.get("on-critical-path")]
print(f"Critical path: {len(critical)} tasks")
print(f"Project finish offset: {output['calculated']['latest-finish']}")
```
