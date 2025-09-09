"""
Utility functions for the dependency checker.
"""

from pathlib import Path
from typing import Dict


def create_dependency_graph(config_data: Dict) -> str:
    """Create a DOT format dependency graph."""
    lines = []
    lines.append("digraph dependencies {")
    lines.append("    rankdir=TB;")
    lines.append("    node [shape=box, style=rounded];")
    lines.append("")

    modules = config_data.get('modules', {})

    # Add nodes
    for module in modules.keys():
        lines.append(f'    "{module}";')

    lines.append("")

    # Add edges
    for module, dependencies in modules.items():
        for dep in dependencies:
            lines.append(f'    "{module}" -> "{dep}";')

    lines.append("}")
    return "\n".join(lines)


def get_project_stats(src_root: str) -> Dict:
    """Get basic statistics about the project."""
    stats = {
        'total_files': 0,
        'python_files': 0,
        'total_lines': 0,
        'modules': [],
        'largest_module': None,
        'largest_module_files': 0
    }

    src_path = Path(src_root)
    if not src_path.exists():
        return stats

    module_file_counts = {}

    for file_path in src_path.rglob("*"):
        if file_path.is_file():
            stats['total_files'] += 1

            if file_path.suffix == '.py':
                stats['python_files'] += 1

                # Count lines
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        stats['total_lines'] += sum(1 for line in f)

                except Exception:
                    pass

                # Track module file counts
                try:
                    rel_path = file_path.relative_to(src_path)
                    if rel_path.parts:
                        module = rel_path.parts[0]
                        module_file_counts[module] = module_file_counts.get(module, 0) + 1
                except ValueError:
                    pass

    stats['modules'] = list(module_file_counts.keys())

    if module_file_counts:
        largest_module = max(module_file_counts, key=module_file_counts.get)
        stats['largest_module'] = largest_module
        stats['largest_module_files'] = module_file_counts[largest_module]

    return stats
