#!/usr/bin/env python3
"""
Bridge between Python data structures and Menai.

Converts Python dictionaries/lists to Menai dict/list expressions,
evaluates Menai code with data, and converts results back to Python.
"""

import sys
import json
from pathlib import Path
from typing import Any, Dict

# Add src to path so we can import menai
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from menai import Menai


# Field lists for the planner struct types, in declaration order.
# A Python dict is converted to a struct constructor call when its keys
# exactly match one of these sets.
_TASK_FIELDS = (
    "id", "name", "duration-days", "calendar-id", "schedule-mode",
    "start-offset", "end-offset", "status", "progress",
    "earliest-start", "earliest-finish",
    "latest-start", "latest-finish", "slack-days",
)
_TASK_KEYS = frozenset(_TASK_FIELDS)

_DEPENDENCY_FIELDS = ("from-task", "to-task", "type", "lag-days")
_DEPENDENCY_KEYS = frozenset(_DEPENDENCY_FIELDS)

_CALENDAR_FIELDS = ("id", "name", "type", "working-days", "holidays")
_CALENDAR_KEYS = frozenset(_CALENDAR_FIELDS)

# Struct module imports needed to construct the types at runtime
_STRUCT_IMPORTS = '''(let* (
  (task-mod (import "tools/planner/task"))
  (dep-mod  (import "tools/planner/dependency"))
  (make-task       (dict-get task-mod "task"))
  (make-dependency (dict-get dep-mod  "dependency")))'''


def _leaf_ids_under(task_id: str, task_map: dict) -> list:
    """
    Return all leaf task IDs that are descendants of task_id.

    A leaf is any task with an empty children list.
    """
    task = task_map.get(task_id)
    if task is None:
        return []
    children = task.get("children", [])
    if not children:
        return [task_id]
    leaves = []
    for child_id in children:
        leaves.extend(_leaf_ids_under(child_id, task_map))
    return leaves


def expand_summary_dependencies(project: dict) -> dict:
    """
    Return a CPM-ready copy of the project with summary tasks removed and
    any dependencies that reference summary tasks expanded to their
    equivalent leaf-task dependencies.

    The original project dict is not modified.  Summary task data is
    preserved there for rollup and re-export.

    Expansion rules:
      - dep from summary S -> task T  becomes  dep from each leaf of S -> T
      - dep from task T -> summary S  becomes  dep from T -> each leaf of S
      - dep from summary S1 -> summary S2  becomes  all leaves(S1) -> all leaves(S2)

    Duplicate dependencies (same from/to/type) are removed.
    """
    task_map = {t["id"]: t for t in project["tasks"]}
    summary_ids = {t["id"] for t in project["tasks"] if t.get("is-summary", False)}

    expanded_deps = []
    seen: set = set()

    for dep in project.get("dependencies", []):
        from_id = dep["from-task"]
        to_id = dep["to-task"]
        dep_type = dep["type"]
        lag_days = dep["lag-days"]
        source = dep.get("source", "msproject")

        from_ids = _leaf_ids_under(from_id, task_map) if from_id in summary_ids else [from_id]
        to_ids = _leaf_ids_under(to_id, task_map) if to_id in summary_ids else [to_id]

        for f in from_ids:
            for t in to_ids:
                key = (f, t, dep_type)
                if key not in seen:
                    seen.add(key)
                    expanded_deps.append({
                        "from-task": f,
                        "to-task": t,
                        "type": dep_type,
                        "lag-days": lag_days,
                        "source": source,
                    })

    leaf_tasks = [t for t in project["tasks"] if not t.get("is-summary", False)]

    return {
        **project,
        "tasks": leaf_tasks,
        "dependencies": expanded_deps,
    }


# Fields written back from the CPM result into each task
_CPM_FIELDS = ("earliest-start", "earliest-finish", "latest-start", "latest-finish", "slack-days")


def _valid_offset(v) -> bool:
    """Return True if v is a valid (non-None, non-False) scheduling offset."""
    return v is not None and v is not False


def merge_cpm_results(full_project: dict, scheduled_project: dict, critical_ids: set) -> dict:
    """
    Merge CPM-computed offsets back into the full project (which includes
    summary tasks) and roll up summary offsets from their children.

    Args:
        full_project:      The complete project dict from load_project(),
                           containing all tasks including summaries.
        scheduled_project: The CPM output dict from schedule-project,
                           containing only leaf/milestone tasks with
                           computed offset fields (floats).
        critical_ids:      Set of task IDs on the critical path.

    Returns:
        A new project dict with all tasks updated and the calculated
        section populated.  Offsets are working-day floats from project
        start.  The original dicts are not modified.
    """
    # Build a lookup of CPM results by task id
    cpm_map: dict[str, dict] = {
        t["id"]: t for t in scheduled_project.get("tasks", [])
    }

    # Copy all tasks, merging CPM fields into leaf/milestone tasks
    updated_tasks = []
    for task in full_project["tasks"]:
        task = dict(task)  # shallow copy so we don't mutate the original
        cpm = cpm_map.get(task["id"])
        if cpm:
            for field in _CPM_FIELDS:
                val = cpm.get(field)
                # Menai returns #f for unset fields; normalise to None
                task[field] = None if val is False else val
        else:
            # Summary tasks — initialise fields; rollup fills them below
            for field in _CPM_FIELDS:
                task.setdefault(field, None)
        task["on-critical-path"] = task["id"] in critical_ids
        updated_tasks.append(task)

    # Roll up summary dates bottom-up.
    # Tasks are in outline order so reversing gives us leaves before parents.
    task_map = {t["id"]: t for t in updated_tasks}

    for task in reversed(updated_tasks):
        if not task.get("is-summary"):
            continue
        children = [task_map[cid] for cid in task.get("children", []) if cid in task_map]
        if not children:
            continue

        es_dates = [c["earliest-start"]  for c in children if _valid_offset(c.get("earliest-start"))]
        ef_dates = [c["earliest-finish"] for c in children if _valid_offset(c.get("earliest-finish"))]
        ls_dates = [c["latest-start"]    for c in children if _valid_offset(c.get("latest-start"))]
        lf_dates = [c["latest-finish"]   for c in children if _valid_offset(c.get("latest-finish"))]
        slack_vals = [c["slack-days"] for c in children if c.get("slack-days") is not None]
        task["earliest-start"] = min(es_dates) if es_dates else None
        task["earliest-finish"] = max(ef_dates) if ef_dates else None
        task["latest-start"] = min(ls_dates) if ls_dates else None
        task["latest-finish"] = max(lf_dates) if lf_dates else None
        task["slack-days"] = min(slack_vals) if slack_vals else None
        task["on-critical-path"] = any(c.get("on-critical-path") for c in children)

    # Populate the calculated section
    all_starts = [t["earliest-start"] for t in updated_tasks if _valid_offset(t.get("earliest-start"))]
    all_finishes = [t["earliest-finish"] for t in updated_tasks if _valid_offset(t.get("earliest-finish"))]
    project_start = min(all_starts) if all_starts else None
    project_finish = max(all_finishes) if all_finishes else None

    leaf_tasks = [t for t in updated_tasks if not t.get("is-summary") and not t.get("is-milestone")]
    total_days = sum(
        t.get("duration-days") or 0.0
        for t in leaf_tasks
        if _valid_offset(t.get("earliest-start"))
    )

    from datetime import datetime, timezone
    now = datetime.now(timezone.utc).isoformat()

    calculated = {
        "critical-path": sorted(critical_ids),
        "earliest-start": project_start,
        "latest-finish": project_finish,
        "total-duration-days": round(total_days, 2),
        "calculation-timestamp": now,
    }

    return {
        **full_project,
        "tasks": updated_tasks,
        "calculated": calculated,
    }


def save_project(project: dict, json_path: str, indent: int = 2) -> None:
    """
    Write a project dict to a JSON file.

    Args:
        project:   Project dict (typically the output of merge_cpm_results).
        json_path: Destination file path.
        indent:    JSON indentation level.
    """
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(project, f, indent=indent, default=str)


class MenaiBridge:
    """Bridge for converting between Python and Menai data structures."""
    
    def __init__(self):
        """
        Initialize Menai bridge.
        """
        self.menai = Menai()

    @staticmethod
    def struct_preamble() -> str:
        """
        Return a Menai let* preamble that imports and binds the planner struct
        constructors.  Embed this at the start of any expression that needs to
        construct task, dependency, or calendar structs.

        Usage:
            preamble = MenaiBridge.struct_preamble()
            expr = f\"\"\"{preamble}
              ... your bindings ...
              body)\"\"\")
        """
        return _STRUCT_IMPORTS

    def python_to_menai_task(self, value: dict) -> str:
        """Convert a Python task dict to a Menai task struct constructor call."""
        def field_value(f):
            v = value.get(f)
            # Numeric scheduling fields must always be floats
            if f in ("duration-days", "start-offset", "end-offset",
                     "earliest-start", "earliest-finish",
                     "latest-start", "latest-finish", "slack-days") and isinstance(v, int):
                v = float(v)
            return self.python_to_menai(v)
        fields = [field_value(f) for f in _TASK_FIELDS]
        return f'(make-task {" ".join(fields)})'

    def python_to_menai_dependency(self, value: dict) -> str:
        """Convert a Python dependency dict to a Menai dependency struct constructor call."""
        def field_value(f):
            v = value[f]
            # lag-days must always be a float for the Menai calendar arithmetic
            if f == "lag-days" and isinstance(v, int):
                v = float(v)
            return self.python_to_menai(v)
        fields = [field_value(f) for f in _DEPENDENCY_FIELDS]
        return f'(make-dependency {" ".join(fields)})'

    def python_to_menai_calendar(self, value: dict) -> str:
        """Convert a Python calendar dict to a Menai calendar struct constructor call."""
        def field_value(f):
            v = value[f]
            # working-days and holidays must be sets for Menai set-member? to work.
            # JSON round-trips these as lists, so normalise here.
            if f in ("working-days", "holidays") and isinstance(v, list):
                v = set(v)
            return self.python_to_menai(v)
        fields = [field_value(f) for f in _CALENDAR_FIELDS]
        return f'(make-calendar {" ".join(fields)})'

    def load_project(self, json_path: str) -> dict:
        """
        Load a project from a JSON file produced by the MS Project importer.

        Applies any normalisation required before passing to Menai:
          - Ensures duration-days, start-offset, end-offset and lag-days are floats

        Args:
            json_path: Path to the planner JSON file.

        Returns:
            Full project dict ready for python_to_menai_project().
            Summary tasks are retained; callers are responsible for
            producing a CPM-ready view via expand_summary_dependencies().
        """
        with open(json_path, "r", encoding="utf-8") as f:
            project = json.load(f)

        for task in project["tasks"]:
            for field in ("duration-days", "start-offset", "end-offset"):
                v = task.get(field)
                if isinstance(v, int):
                    task[field] = float(v)

        for dep in project.get("dependencies", []):
            lag = dep.get("lag-days")
            if isinstance(lag, int):
                dep["lag-days"] = float(lag)

        return project

    def python_to_menai_project(self, project: dict) -> str:
        """
        Convert a Python project dict to a Menai expression with tasks
        and dependencies as structs.  Calendars are passed as generic dicts
        since the CPM engine no longer uses them.  Must be used inside an
        expression that has the struct_preamble bindings in scope.
        """
        tasks = f'(list {" ".join(self.python_to_menai_task(t) for t in project["tasks"])})'
        deps  = f'(list {" ".join(self.python_to_menai_dependency(d) for d in project["dependencies"])})'
        other = {k: v for k, v in project.items() if k not in ("tasks", "dependencies")}
        other_pairs = " ".join(
            f'{self.python_to_menai(k)} {self.python_to_menai(v)}'
            for k, v in other.items()
        )
        return f'(dict "tasks" {tasks} "dependencies" {deps} {other_pairs})'

    def python_to_menai(self, value: Any) -> str:
        """
        Convert Python value to Menai expression string.
        
        Args:
            value: Python value (dict, list, str, int, float, bool, None)
            
        Returns:
            Menai expression as string
        """
        if value is None:
            return "#f"
        
        elif isinstance(value, bool):
            return "#t" if value else "#f"
        
        elif isinstance(value, (int, float)):
            return str(value)
        
        elif isinstance(value, str):
            # Escape special characters
            escaped = value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")
            return f'"{escaped}"'
        
        elif isinstance(value, list):
            # Convert to Menai list
            elements = [self.python_to_menai(item) for item in value]
            return f"(list {' '.join(elements)})"
        
        elif isinstance(value, set):
            # Convert to Menai set
            elements = [self.python_to_menai(item) for item in value]
            return f"(set {' '.join(elements)})"

        elif isinstance(value, dict):
            # Convert to a generic Menai dict.
            # Use python_to_menai_task/dependency/calendar for struct types.
            pairs = []
            for key, val in value.items():
                key_expr = self.python_to_menai(key)
                val_expr = self.python_to_menai(val)
                pairs.append(f"{key_expr} {val_expr}")
            return f"(dict {' '.join(pairs)})"
        
        else:
            raise TypeError(f"Cannot convert Python type {type(value)} to Menai")
    
    def evaluate(self, expression: str) -> Any:
        """
        Evaluate an Menai expression and return Python result.
        
        Args:
            expression: Menai expression string
            
        Returns:
            Python value (dict, list, str, int, float, bool, None)
        """
        return self.menai.evaluate(expression)
    
    def evaluate_with_data(self, expression: str, data: Dict[str, Any]) -> Any:
        """
        Evaluate an Menai expression with Python data bound to variables.
        
        Args:
            expression: Menai expression string that references variables
            data: Dictionary mapping variable names to Python values
            
        Returns:
            Python value result
            
        Example:
            bridge.evaluate_with_data(
                "(+ x y)",
                {"x": 5, "y": 10}
            )
            # Returns: 15
        """
        # Build let expression that binds all variables
        bindings = []
        for var_name, var_value in data.items():
            menai_value = self.python_to_menai(var_value)
            bindings.append(f"({var_name} {menai_value})")
        
        let_expr = f"(let ({' '.join(bindings)}) {expression})"
        return self.evaluate(let_expr)
    
    def load_menai_file(self, filepath: str) -> str:
        """
        Load Menai code from a file.
        
        Args:
            filepath: Path to .menai file
            
        Returns:
            Menai code as string
        """
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.read()
    
    def evaluate_file(self, filepath: str, data: Dict[str, Any] = None) -> Any:
        """
        Evaluate Menai code from a file with optional data bindings.
        
        Args:
            filepath: Path to .menai file
            data: Optional dictionary of variable bindings
            
        Returns:
            Python value result
        """
        code = self.load_menai_file(filepath)
        
        if data:
            return self.evaluate_with_data(code, data)
        else:
            return self.evaluate(code)


def format_menai_result(result: Any, indent: int = 0) -> str:
    """
    Format Menai result (Python value) as human-readable string.
    
    Args:
        result: Python value from Menai evaluation
        indent: Current indentation level
        
    Returns:
        Formatted string
    """
    indent_str = "  " * indent
    
    if isinstance(result, dict):
        lines = ["{"]
        for key, value in result.items():
            formatted_value = format_menai_result(value, indent + 1)
            lines.append(f"  {indent_str}{key}: {formatted_value}")
        lines.append(f"{indent_str}}}")
        return "\n".join(lines)
    
    elif isinstance(result, list):
        if not result:
            return "[]"
        
        # Check if all elements are simple (not dict/list)
        all_simple = all(not isinstance(item, (dict, list)) for item in result)
        
        if all_simple and len(result) <= 5:
            # Inline short simple lists
            return "[" + ", ".join(str(item) for item in result) + "]"
        
        elif all_simple:
            # Multi-line simple lists
            lines = ["["]
            for item in result:
                lines.append(f"  {indent_str}{item}")
            lines.append(f"{indent_str}]")
            return "\n".join(lines)
        
        else:
            # Complex lists with nested structures
            lines = ["["]
            for item in result:
                formatted_item = format_menai_result(item, indent + 1)
                lines.append(f"  {indent_str}{formatted_item}")
            lines.append(f"{indent_str}]")
            return "\n".join(lines)
    
    else:
        return str(result)

