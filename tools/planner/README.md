# Project Planner with Menai

A sophisticated project planning and analysis tool using Menai (AI Functional Programming Language) for complex dependency analysis, critical path calculation, and what-if scenario planning.

## Overview

This tool addresses the challenges of managing large software build and deployment programs with:
- **1000+ tasks** across multiple teams
- **100+ people** including internal and external resources
- **Multiple source systems** (Jira, MS Project, Excel) with no single source of truth
- **Complex dependencies** (FS, SS, FF, SF constraints)
- **Mixed calendars** (5-day and 7-day work weeks)
- **External vendor dependencies** with varying reliability
- **Highly unpredictable design tasks** alongside predictable deployment work

### Why Menai?

Menai's functional programming model is ideal for project planning because:
- **Immutable data** makes scenario planning safe (no accidental mutations)
- **Pattern matching** simplifies complex dependency logic
- **Higher-order functions** (map, filter, fold) enable elegant queries
- **Alists** provide efficient key-value data structures
- **Pure functions** ensure reproducible calculations
- **No side effects** means parallel computation is straightforward

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Python Layer                              │
│  - Data import (Jira/MSProject/Excel)                       │
│  - ID reconciliation and conflict resolution                │
│  - Report generation and visualization                      │
│  - CLI and AI interface                                     │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                  Menai Bridge                                │
│  - Convert Python ↔ Menai data structures                   │
│  - Manage Menai evaluation                                  │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                    Menai Core                                │
│  - Dependency graph analysis                                │
│  - Critical path calculation (CPM algorithm)                │
│  - Calendar arithmetic (working days)                       │
│  - Scenario management and comparison                       │
│  - Validation (circular deps, overallocation, etc.)        │
└─────────────────────────────────────────────────────────────┘
```

## Data Model

### Core Entities

#### Task
```python
{
  "id": "TASK-123",
  "name": "Deploy API Gateway",
  "owner": "alice",
  "team": "infra-team",
  
  # Scheduling (dates OR duration)
  "start-date": "2025-02-01",        # ISO date or None
  "end-date": "2025-02-10",          # ISO date or None
  "duration-days": 10,               # Work days or None
  "schedule-mode": "fixed-dates",    # "fixed-dates" | "duration-based" | "unscheduled"
  
  # Calendar
  "calendar-type": "5-day",          # "5-day" | "7-day"
  "calendar-id": "standard-5day",
  
  # Status
  "status": "in-progress",           # "not-started" | "in-progress" | "complete" | "blocked"
  "progress": 0.3,                   # 0.0 to 1.0
  
  # Source tracking (multi-system reconciliation)
  "authoritative-source": "jira",
  "source-ids": {
    "jira": "PROJ-456",
    "msproject": "23",
    "excel": "row-15"
  },
  "last-updated": "2025-01-15T10:30:00",
  
  # External parties
  "external-parties": ["vendor-acme"],
  
  # Optional metadata
  "type": "deployment",              # Informational only
  "priority": "high"                 # Informational only
}
```

#### Dependency
```python
{
  "from-task": "TASK-120",
  "to-task": "TASK-123",
  "type": "finish-to-start",         # "finish-to-start" | "start-to-start" | 
                                     # "finish-to-finish" | "start-to-finish"
  "lag-days": 2,                     # Positive=delay, negative=lead time
  "source": "msproject"
}
```

#### Calendar
```python
{
  "id": "standard-5day",
  "name": "Standard Work Week",
  "type": "5-day",
  "working-days": ["mon", "tue", "wed", "thu", "fri"],
  "holidays": ["2025-12-25", "2025-12-26", "2026-01-01"]
}
```

#### Resource
```python
{
  "id": "alice",
  "name": "Alice Smith",
  "team": "infra-team",
  "role": "devops-engineer",
  "external": False
}
```

#### External Party
```python
{
  "id": "vendor-acme",
  "name": "ACME Corp",
  "contact": "pm@acme.com",
  "calendar-id": "vendor-calendar",
  "tasks-owned": ["TASK-200", "TASK-201"],
  "tasks-involved": ["TASK-150"]
}
```

### Complete Project Structure

```python
{
  "project-id": "BUILD-2025",
  "name": "Software Build and Deployment Programme",
  "created": "2025-01-01T00:00:00",
  "last-modified": "2025-01-15T14:30:00",
  "version": 47,
  
  "tasks": [...],
  "dependencies": [...],
  "resources": [...],
  "external-parties": [...],
  "milestones": [...],
  "calendars": [...],
  
  "id-mappings": {
    "jira": {"PROJ-456": "TASK-123", ...},
    "msproject": {"23": "TASK-123", ...},
    "excel": {"row-15": "TASK-123", ...}
  },
  
  "validation": {
    "last-validated": "2025-01-15T14:30:00",
    "errors": [],
    "warnings": []
  },
  
  "calculated": {
    "critical-path": ["TASK-100", "TASK-120", ...],
    "earliest-start": "2025-02-01",
    "latest-finish": "2025-06-30",
    "total-duration-days": 150,
    "calculation-timestamp": "2025-01-15T14:30:00"
  }
}
```

## Files

### Current Implementation

- **`example_project.py`** - Creates a realistic 20-task software project with:
  - Design, development, testing, and deployment phases
  - Multiple dependency types (FS, SS, FF, SF)
  - Mixed 5-day and 7-day calendars
  - External vendor dependencies
  - Multi-source data (Jira, MS Project, Excel)

- **`menai_bridge.py`** - Bridge between Python and Menai:
  - Converts Python dict/list ↔ Menai alist/list
  - Evaluates Menai expressions with data bindings
  - Loads and executes Menai files

- **`demo.py`** - Demonstration of Menai project queries:
  - Basic queries (task lists, counts, filtering)
  - Dependency analysis
  - Resource allocation
  - Calendar type distribution
  - Complex multi-condition queries

### Planned Implementation

- **`validation.menai`** - Project validation functions:
  - Circular dependency detection
  - Data consistency checks
  - Resource overallocation detection
  - Calendar constraint validation

- **`calendar.menai`** - Calendar arithmetic:
  - Calculate working days between dates
  - Add/subtract working days
  - Handle holidays and weekends
  - Support multiple calendar types

- **`scheduling.menai`** - Critical path method (CPM):
  - Forward pass (earliest start/finish)
  - Backward pass (latest start/finish)
  - Slack calculation
  - Critical path identification
  - Dependency constraint application

- **`scenarios.menai`** - What-if scenario support:
  - Create scenarios from baseline
  - Apply changes (delays, new dependencies, resource changes)
  - Compare scenarios
  - Impact analysis

- **`importers/`** - Data import modules:
  - `jira_import.py` - Import from Jira CSV/JSON
  - `msproject_import.py` - Import from MS Project XML
  - `excel_import.py` - Import from Excel spreadsheets
  - `reconcile.py` - ID mapping and conflict resolution

- **`cli.py`** - Command-line interface:
  - Import from multiple sources
  - Run analysis queries
  - Generate reports
  - Interactive AI mode

## Usage

### Running the Demo

```bash
cd tools/project_planner
python demo.py
```

This demonstrates:
- Loading a 20-task project into Menai
- Querying tasks by status, priority, team, owner
- Analyzing dependencies
- Resource allocation analysis
- Calendar type distribution
- Complex multi-condition queries

### Testing the Bridge

```bash
python menai_bridge.py
```

This tests:
- Python → Menai data conversion
- Simple value, list, and dict conversion
- Nested structure handling
- Menai evaluation with Python data
- Alist and list operations

### Viewing Example Data

```bash
python example_project.py | less
```

This outputs the complete example project as JSON.

## Example Queries

### Find all critical priority tasks
```python
bridge.evaluate_with_data(
    '''(let ((tasks (alist-get project "tasks"))
             (critical (filter 
               (lambda (task) (string=? (alist-get task "priority") "critical"))
               tasks)))
          (map (lambda (task) (alist-get task "name")) critical))''',
    {"project": project}
)
```

### Find tasks owned by a specific person
```python
bridge.evaluate_with_data(
    '''(let ((tasks (alist-get project "tasks")))
          (filter 
            (lambda (task) (string=? (alist-get task "owner") "alice"))
            tasks))''',
    {"project": project}
)
```

### Count tasks by team
```python
bridge.evaluate_with_data(
    '''(let ((tasks (alist-get project "tasks"))
             (teams (map (lambda (task) (alist-get task "team")) tasks)))
          (list
            (list "backend" (list-length (filter (lambda (t) (string=? t "backend")) teams)))
            (list "frontend" (list-length (filter (lambda (t) (string=? t "frontend")) teams)))
            (list "infra" (list-length (filter (lambda (t) (string=? t "infra")) teams)))))''',
    {"project": project}
)
```

### Find all predecessors of a task
```python
bridge.evaluate_with_data(
    '''(let ((deps (alist-get project "dependencies")))
          (filter 
            (lambda (dep) (string=? (alist-get dep "to-task") "TASK-123"))
            deps))''',
    {"project": project}
)
```

## Design Decisions

### 1. This is the System of Record
The tool is designed to be the **definitive source of truth**. Other systems (Jira, MS Project, Excel) import into this system and must synchronize with it.

### 2. Multi-Source Reconciliation
Each task tracks its source system IDs and has an "authoritative-source" field. During import, conflicts are resolved based on source priority.

### 3. Flexible Scheduling
Tasks can have:
- Fixed dates (start + end)
- Duration-based (calculated from dependencies + duration)
- Unscheduled (boolean-not yet placed on timeline)

### 4. Mixed Calendars
Different tasks can use different calendars (5-day vs 7-day). This is critical for deployment tasks that run 24/7 vs. development tasks on business days.

### 5. Scenario Objects
What-if scenarios are first-class objects that contain:
- Full project state
- List of changes from parent scenario
- Impact summary
- Comparison data

### 6. Validation as Data
Validation results are stored in the project structure, not just logged. This allows tracking validation status over time.

## Next Steps

### Phase 1: Foundation (In Progress)
- [x] Data model definition
- [x] Example project with realistic complexity
- [x] Python ↔ Menai bridge
- [x] Basic query demonstrations
- [ ] Calendar arithmetic functions
- [ ] Circular dependency detection
- [ ] Data validation functions

### Phase 2: Core Analysis
- [ ] Forward pass (earliest dates)
- [ ] Backward pass (latest dates)
- [ ] Critical path calculation
- [ ] Slack calculation
- [ ] Resource loading analysis

### Phase 3: What-If Scenarios
- [ ] Scenario creation and management
- [ ] Scenario comparison
- [ ] Impact analysis
- [ ] Change tracking

### Phase 4: Import/Export
- [ ] Jira CSV/JSON importer
- [ ] MS Project XML importer
- [ ] Excel importer
- [ ] ID reconciliation
- [ ] Conflict resolution
- [ ] Export to various formats

### Phase 5: User Interface
- [ ] Command-line interface
- [ ] Interactive AI mode (natural language queries)
- [ ] Report generation
- [ ] Visualization (Gantt charts, network diagrams)

## Requirements

- Python 3.8+
- Menai (included in Humbug)
- No external dependencies for core functionality

## License

Same as Humbug project.
