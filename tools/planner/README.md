# Project Planner with Menai

A project planning and analysis tool using Menai (AI Functional Programming Language) for critical path calculation, dependency analysis, and schedule validation.

## Overview

This tool imports project data from MS Project XML, computes a CPM schedule using Menai's pure functional engine, and exports the results back to MS Project XML (or planner JSON). It is designed to handle real-world complexity:

- **Sub-day task granularity** — tasks can start and finish at any time within a working day
- **All four dependency types** — FS, SS, FF, SF with positive and negative lag (including percent lag)
- **Split working days** — correctly handles calendars with morning/afternoon periods and lunch breaks
- **Hierarchy preservation** — summary tasks and milestones are preserved through the full import/schedule/export cycle
- **Exact arithmetic** — scheduling uses working-day offsets (floats) so fractional durations accumulate without rounding error

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                      Python Layer                             │
│  importers/msproject_import.py  — XML → planner JSON         │
│  importers/msproject_export.py  — planner JSON → XML         │
│  run_planner.py                 — end-to-end pipeline        │
│  menai_bridge.py                — Python ↔ Menai conversion  │
└────────────────────────┬─────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│                     Menai Core                                │
│  task.menai        — task struct, field updates              │
│  dependency.menai  — dependency struct, constraint logic     │
│  scheduling.menai  — CPM forward/backward pass, slack        │
│  validation.menai  — cycle detection, consistency checks     │
│  calendar.menai    — date arithmetic, offset conversion      │
└──────────────────────────────────────────────────────────────┘
```

## Scheduling Model

### Working-Day Offsets

All scheduling positions are represented as **floating-point working-day offsets from the project start**. An offset of `0.0` is the project start instant; `1.0` is exactly one working day later; `0.5` is half a working day later.

This model correctly handles sub-day precision:

| Clock time | Offset (8h/day, 08:00-12:00, 13:00-17:00) |
|---|---|
| `2026-03-23T08:00:00` (project start) | `0.0` |
| `2026-03-23T12:00:00` (end of morning) | `0.5` |
| `2026-03-23T13:00:00` (start of afternoon) | `0.5` |
| `2026-03-23T17:00:00` (end of day) | `1.0` |
| `2026-03-24T08:00:00` (next day start) | `1.0` |

The CPM engine operates entirely on these floats — no calendar lookups during scheduling. The calendar is only used at the import boundary (datetime → offset) and export boundary (offset → datetime).

### CPM Algorithm

The Menai scheduling engine implements the standard Critical Path Method:

1. **Forward pass** — computes `earliest-start` and `earliest-finish` for each task by topological traversal, applying dependency constraints as float arithmetic
2. **Backward pass** — computes `latest-start` and `latest-finish` working backwards from the project end
3. **Slack** — `latest-start - earliest-start` for each task
4. **Critical path** — tasks with slack ≤ 0

Dependency constraints are pure float addition:

| Type | Forward constraint |
|---|---|
| Finish-to-Start | `succ-start = pred-finish + lag` |
| Start-to-Start | `succ-start = pred-start + lag` |
| Finish-to-Finish | `succ-finish = pred-finish + lag` |
| Start-to-Finish | `succ-finish = pred-start + lag` |

### Summary Tasks and Hierarchy

Summary tasks are excluded from the CPM graph — only leaf tasks and milestones are scheduled. After CPM completes, a rollup pass computes summary task positions from their children:

- Summary `earliest-start` = min of children's `earliest-start`
- Summary `earliest-finish` = max of children's `earliest-finish`
- Summary `on-critical-path` = true if any child is on the critical path

## Data Model

### Task

```python
{
  "id": "T029",
  "name": "Develop code",
  "owner": "developer",
  "team": None,

  # Scheduling offsets (working days from project start)
  "start-offset": 35.0,        # original imported position
  "end-offset": 50.0,          # original imported position

  # Original datetime strings (preserved for round-trip fidelity)
  "start-date": "2026-05-11",
  "end-date": "2026-05-29",
  "start-time": "08:00:00",
  "end-time": "17:00:00",

  "duration-days": 15.0,       # always float
  "schedule-mode": "fixed-dates" | "duration-based" | "unscheduled",
  "calendar-id": "cal-1",

  "status": "not-started" | "in-progress" | "complete",
  "progress": 0.0,             # 0.0 to 1.0

  "type": "task" | "summary" | "milestone",
  "is-summary": False,
  "is-milestone": False,

  # Hierarchy
  "parent-id": "T025",         # planner ID of parent summary, or None
  "children": [],              # planner IDs of direct children (summaries only)
  "wbs": "5.1.1",
  "outline-level": 3,

  # CPM results (populated after scheduling)
  "earliest-start": 35.0,
  "earliest-finish": 50.0,
  "latest-start": 35.0,
  "latest-finish": 50.0,
  "slack-days": 0.0,
  "on-critical-path": True,

  # Provenance
  "authoritative-source": "msproject",
  "source-ids": {"msproject": "29"},
  "assigned-resources": ["developer"],
  "external-parties": []
}
```

### Dependency

```python
{
  "from-task": "T029",
  "to-task": "T030",
  "type": "finish-to-start",   # "finish-to-start" | "start-to-start" |
                               # "finish-to-finish" | "start-to-finish"
  "lag-days": -11.25,          # float; negative = lead time
  "source": "msproject"
}
```

Lag is always stored in working days. Percent lag (MS Project `LagFormat=19`) is converted at import time using the predecessor's duration: `lag_days = (percent / 100) * predecessor_duration_days`.

### Calendar

```python
{
  "id": "cal-1",
  "name": "Standard",
  "type": "5-day",
  "is-base-calendar": True,
  "working-days": ["mon", "tue", "wed", "thu", "fri"],
  "working-periods": [[8.0, 12.0], [13.0, 17.0]],  # (start_hour, end_hour) pairs
  "holidays": [],
  "source-uid": "1"
}
```

### Project Structure

```python
{
  "project-id": "SOFTWARE-DEVELOPMENT",
  "name": "Software Development",
  "company": "...",
  "created": "2026-03-23T15:46:00",
  "last-modified": "...",
  "version": 1,
  "source-file": "path/to/original.xml",

  "schedule": {
    "start-date": "2026-03-23",
    "finish-date": "2026-08-03",
    "hours-per-day": 8.0,
    "project-start-datetime": "2026-03-23T08:00:00",
    "working-days": ["mon", "tue", "wed", "thu", "fri"],
    "working-periods": [[8.0, 12.0], [13.0, 17.0]]
  },

  "tasks": [...],           # all tasks including summaries and milestones
  "dependencies": [...],    # original dependencies (may reference summary tasks)
  "resources": [...],
  "milestones": [...],      # milestone metadata (also present in tasks list)
  "calendars": [...],
  "external-parties": [],
  "id-mappings": {"msproject": {"1": "T001", ...}},

  "validation": {"last-validated": None, "errors": [], "warnings": []},

  "calculated": {
    "critical-path": ["T002", "T003", ...],
    "earliest-start": 0.0,          # offset of project start
    "latest-finish": 97.75,         # offset of project end
    "total-duration-days": 191.0,
    "calculation-timestamp": "..."
  }
}
```

## Files

### Menai Modules

| File | Purpose |
|---|---|
| `task.menai` | Task struct with offset-based scheduling fields; field update helpers; single-task validation |
| `dependency.menai` | Dependency struct; predecessor/successor queries; forward/backward constraint application (pure float arithmetic) |
| `scheduling.menai` | Complete CPM implementation: forward pass, backward pass, slack calculation, critical path identification |
| `validation.menai` | Project validation: circular dependency detection (DFS), task/dependency consistency checks |
| `calendar.menai` | Date arithmetic: working-day stepping, offset↔datetime conversion with working-period awareness |

### Python Layer

| File | Purpose |
|---|---|
| `importers/msproject_import.py` | Parse MS Project XML → planner JSON; compute working-day offsets from datetimes; handle all lag formats including percent lag |
| `importers/msproject_export.py` | Convert planner JSON → MS Project XML; convert offsets back to datetimes using working periods |
| `menai_bridge.py` | Convert Python dicts/lists to Menai expressions; `expand_summary_dependencies()` for CPM input; `merge_cpm_results()` with summary rollup; `save_project()` |
| `run_planner.py` | End-to-end pipeline: load → validate → schedule → export |
| `example_project.py` | Hardcoded 20-task example project (used by validation tests) |

### Tests

| File | Purpose |
|---|---|
| `test_calendar.py` | Calendar arithmetic tests (all passing) |
| `test_scheduling_basic.py` | CPM tests on a simple 3-task chain (all passing) |
| `test_validation_simple.py` | Validation tests: cycle detection, dependency queries (all passing) |

## Usage

### Full Pipeline: XML → Schedule → XML

```bash
# Step 1: Import MS Project XML to planner JSON
python3 tools/planner/importers/msproject_import.py plan.xml -o plan.json

# Step 2: Schedule and export
python3 tools/planner/run_planner.py plan.json -o plan-scheduled.json -x plan-scheduled.xml
```

### Run Tests

```bash
python3 tools/planner/test_calendar.py
python3 tools/planner/test_scheduling_basic.py
python3 tools/planner/test_validation_simple.py
```

### Python API

```python
from tools.planner.menai_bridge import MenaiBridge, expand_summary_dependencies, merge_cpm_results

bridge = MenaiBridge()

# Load project
project = bridge.load_project("plan.json")

# Prepare CPM input (expand summary deps, filter to leaf tasks)
cpm_project = expand_summary_dependencies(project)

# Schedule
preamble = MenaiBridge.struct_preamble()
project_expr = bridge.python_to_menai_project(cpm_project)

scheduled = bridge.evaluate(f'''
    {preamble}
    (let* ((scheduling (import "tools/planner/scheduling"))
           (schedule   (dict-get scheduling "schedule-project"))
           (project    {project_expr}))
      (schedule project))
''')

# Merge computed offsets back into full project (includes summary rollup)
critical_ids = set(scheduled.get("critical-path", []))
output = merge_cpm_results(project, scheduled, critical_ids)
```

## Design Decisions

### Offset-Based Scheduling

The CPM engine works in **working-day offsets** rather than calendar dates. This means:

- Sub-day precision is preserved exactly — a 0.5-day task starting at `13:00:00` finishes at `17:00:00` the same day, and its successor correctly starts at `08:00:00` the next day
- Percent lag (`LagFormat=19` in MS Project) is correctly converted to working days using the predecessor's duration
- Floating-point arithmetic is exact for quarter-day granularity (the finest granularity in typical MS Project files)
- The CPM engine is calendar-agnostic — no calendar lookups during scheduling

### Summary Task Handling

Summary tasks are not nodes in the CPM graph. Instead:

1. Dependencies referencing summary tasks are **expanded** to equivalent leaf-task dependencies by `expand_summary_dependencies()`
2. The CPM engine schedules leaf tasks and milestones only
3. A **rollup pass** in `merge_cpm_results()` computes summary positions from their children

This preserves the original hierarchy for re-export while ensuring the schedule is computed from first principles.

### Round-Trip Fidelity

The importer preserves original datetime strings (`start-time`, `end-time`) alongside computed offsets. The exporter uses these original times when the computed date matches the original — ensuring that tasks whose schedule hasn't changed are exported with their exact original times, not approximations.

## Requirements

- Python 3.8+
- Menai (included in Humbug)
- No external dependencies
