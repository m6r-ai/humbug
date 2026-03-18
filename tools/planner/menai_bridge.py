#!/usr/bin/env python3
"""
Bridge between Python data structures and Menai.

Converts Python dictionaries/lists to Menai dict/list expressions,
evaluates Menai code with data, and converts results back to Python.
"""

import sys
from pathlib import Path
from typing import Any, Dict, List, Union

# Add src to path so we can import menai
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from menai import Menai


# Field lists for the planner struct types, in declaration order.
# A Python dict is converted to a struct constructor call when its keys
# exactly match one of these sets.
_TASK_FIELDS = (
    "id", "name", "duration-days", "calendar-id", "schedule-mode",
    "start-date", "end-date", "status", "progress",
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
  (cal-mod  (import "tools/planner/calendar"))
  (task-mod (import "tools/planner/task"))
  (dep-mod  (import "tools/planner/dependency"))
  (make-calendar   (dict-get cal-mod  "calendar"))
  (make-task       (dict-get task-mod "task"))
  (make-dependency (dict-get dep-mod  "dependency")))'''


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
        fields = [self.python_to_menai(value.get(f)) for f in _TASK_FIELDS]
        return f'(make-task {" ".join(fields)})'

    def python_to_menai_dependency(self, value: dict) -> str:
        """Convert a Python dependency dict to a Menai dependency struct constructor call."""
        fields = [self.python_to_menai(value[f]) for f in _DEPENDENCY_FIELDS]
        return f'(make-dependency {" ".join(fields)})'

    def python_to_menai_calendar(self, value: dict) -> str:
        """Convert a Python calendar dict to a Menai calendar struct constructor call."""
        fields = [self.python_to_menai(value[f]) for f in _CALENDAR_FIELDS]
        return f'(make-calendar {" ".join(fields)})'

    def python_to_menai_project(self, project: dict) -> str:
        """
        Convert a Python project dict to a Menai expression with tasks,
        dependencies, and calendars as structs.  Must be used inside an
        expression that has the struct_preamble bindings in scope.
        """
        tasks = f'(list {" ".join(self.python_to_menai_task(t) for t in project["tasks"])})'
        deps  = f'(list {" ".join(self.python_to_menai_dependency(d) for d in project["dependencies"])})'
        cals  = f'(list {" ".join(self.python_to_menai_calendar(c) for c in project.get("calendars", []))})'
        other = {k: v for k, v in project.items() if k not in ("tasks", "dependencies", "calendars")}
        other_pairs = " ".join(
            f'(list {self.python_to_menai(k)} {self.python_to_menai(v)})'
            for k, v in other.items()
        )
        return f'(dict (list "tasks" {tasks}) (list "dependencies" {deps}) (list "calendars" {cals}) {other_pairs})'

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
                pairs.append(f"(list {key_expr} {val_expr})")
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


if __name__ == "__main__":
    """Test the bridge with example conversions."""
    
    bridge = MenaiBridge()
    
    print("=" * 80)
    print("Menai BRIDGE TEST")
    print("=" * 80)
    
    # Test 1: Simple values
    print("\n1. Simple value conversion:")
    print(f"   Python: 42")
    print(f"   Menai:  {bridge.python_to_menai(42)}")
    print(f"   Python: 'hello'")
    print(f"   Menai:  {bridge.python_to_menai('hello')}")
    print(f"   Python: True")
    print(f"   Menai:  {bridge.python_to_menai(True)}")
    
    # Test 2: List conversion
    print("\n2. List conversion:")
    py_list = [1, 2, 3, 4, 5]
    menai_list = bridge.python_to_menai(py_list)
    print(f"   Python: {py_list}")
    print(f"   Menai:  {menai_list}")
    
    # Test 3: Dict (dict) conversion
    print("\n3. Dictionary (dict) conversion:")
    py_dict = {"name": "Alice", "age": 30, "active": True}
    menai_dict = bridge.python_to_menai(py_dict)
    print(f"   Python: {py_dict}")
    print(f"   Menai:  {menai_dict}")
    
    # Test 4: Nested structure (task-like)
    print("\n4. Nested structure (task):")
    task = {
        "id": "T001",
        "name": "Example Task",
        "duration-days": 10,
        "status": "in-progress",
        "dependencies": ["T000"],
        "metadata": {
            "priority": "high",
            "owner": "alice"
        }
    }
    menai_task = bridge.python_to_menai(task)
    print(f"   Python task: {task}")
    print(f"   Menai task:  {menai_task[:100]}...")
    
    # Test 5: Evaluate with data
    print("\n5. Evaluate Menai with Python data:")
    result = bridge.evaluate_with_data(
        "(+ x y z)",
        {"x": 10, "y": 20, "z": 30}
    )
    print(f"   Expression: (+ x y z)")
    print(f"   Data: x=10, y=20, z=30")
    print(f"   Result: {result}")
    
    # Test 6: Evaluate dict operations
    print("\n6. Evaluate dict operations:")
    result = bridge.evaluate_with_data(
        '(dict-get task "name")',
        {"task": {"id": "T001", "name": "Test Task", "duration": 5}}
    )
    print(f'   Expression: (dict-get task "name")')
    print(f"   Result: {result}")
    
    # Test 7: List operations
    print("\n7. Evaluate list operations:")
    result = bridge.evaluate_with_data(
        "(map (lambda (x) (* x x)) numbers)",
        {"numbers": [1, 2, 3, 4, 5]}
    )
    print(f"   Expression: (map (lambda (x) (* x x)) numbers)")
    print(f"   Data: numbers=[1, 2, 3, 4, 5]")
    print(f"   Result: {result}")
    
    print("\n" + "=" * 80)
    print("All tests completed!")
    print("=" * 80)
