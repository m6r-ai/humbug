"""Dependency analysis for let bindings to support automatic recursion detection."""

from typing import Dict, List, Set, Tuple
from dataclasses import dataclass

from aifpl.aifpl_parser import AIFPLSExpression, AIFPLFunctionCall, AIFPLLambdaExpr, AIFPLLetExpr
from aifpl.aifpl_value import AIFPLSymbol


@dataclass
class BindingGroup:
    """Represents a group of bindings that should be evaluated together."""
    names: Set[str]
    bindings: List[Tuple[str, AIFPLSExpression]]
    is_recursive: bool
    depends_on: Set[str]  # Other groups this depends on


class DependencyAnalyzer:
    """Analyzes dependencies in let bindings to determine evaluation strategy."""

    def analyze_let_bindings(self, bindings: List[Tuple[str, AIFPLSExpression]]) -> List[BindingGroup]:
        """
        Analyze let bindings and group them by dependencies.

        Returns:
            List of BindingGroup objects in topological order
        """
        # Step 1: Find what variables each binding references
        dependencies = {}
        binding_names = {name for name, _ in bindings}

        for name, expr in bindings:
            free_vars = self._find_free_variables(expr)
            # Only consider dependencies on other bindings in this let
            local_deps = free_vars & binding_names
            dependencies[name] = local_deps

        # Step 2: Find strongly connected components (recursive groups)
        scc_groups = self._find_strongly_connected_components(dependencies)

        # Step 3: Create BindingGroup objects
        groups = []
        binding_dict = dict(bindings)

        for group_names in scc_groups:
            group_bindings = [(name, binding_dict[name]) for name in group_names]
            is_recursive = len(group_names) > 1 or any(
                name in dependencies[name] for name in group_names
            )

            # Find external dependencies (dependencies on other groups)
            external_deps = set()
            for name in group_names:
                external_deps.update(dependencies[name] - group_names)

            groups.append(BindingGroup(
                names=group_names,
                bindings=group_bindings,
                is_recursive=is_recursive,
                depends_on=external_deps
            ))

        # Step 4: Sort groups in topological order
        return self._topological_sort_groups(groups)

    def _find_free_variables(self, expr: AIFPLSExpression) -> Set[str]:
        """Find all free variables (symbols) in an expression."""
        free_vars = set()

        if isinstance(expr, AIFPLSymbol):
            free_vars.add(expr.name)

        elif isinstance(expr, AIFPLFunctionCall):
            free_vars.update(self._find_free_variables(expr.function))
            for arg in expr.arguments:
                free_vars.update(self._find_free_variables(arg))

        elif isinstance(expr, AIFPLLambdaExpr):
            # Find free variables in body, excluding parameters
            body_vars = self._find_free_variables(expr.body)
            param_names = set(expr.parameters)
            free_vars.update(body_vars - param_names)

        elif isinstance(expr, AIFPLLetExpr):
            # Handle nested lets
            binding_names = {name for name, _ in expr.bindings}

            # Free variables in binding expressions
            for _, binding_expr in expr.bindings:
                free_vars.update(self._find_free_variables(binding_expr))

            # Free variables in body, excluding bound names
            body_vars = self._find_free_variables(expr.body)
            free_vars.update(body_vars - binding_names)

        # For other expression types (numbers, strings, lists, etc.), no free variables
        return free_vars

    def _find_strongly_connected_components(self, graph: Dict[str, Set[str]]) -> List[Set[str]]:
        """
        Find strongly connected components using Tarjan's algorithm.

        Args:
            graph: Dict mapping node names to their dependencies

        Returns:
            List of sets, each representing a strongly connected component
        """
        index_counter = [0]
        stack: List[str] = []
        lowlinks: Dict[str, int] = {}
        index: Dict[str, int] = {}
        on_stack: Dict[str, bool] = {}
        result: List[Set[str]] = []

        def strongconnect(node: str) -> None:
            index[node] = index_counter[0]
            lowlinks[node] = index_counter[0]
            index_counter[0] += 1
            stack.append(node)
            on_stack[node] = True

            # Consider successors
            for successor in graph.get(node, set()):
                if successor not in index:
                    strongconnect(successor)
                    lowlinks[node] = min(lowlinks[node], lowlinks[successor])

                elif on_stack.get(successor, False):
                    lowlinks[node] = min(lowlinks[node], index[successor])

            # If node is a root, pop the stack and create SCC
            if lowlinks[node] == index[node]:
                component: Set[str] = set()
                while True:
                    w = stack.pop()
                    on_stack[w] = False
                    component.add(w)
                    if w == node:
                        break

                result.append(component)

        for node in graph:
            if node not in index:
                strongconnect(node)

        return result

    def _topological_sort_groups(self, groups: List[BindingGroup]) -> List[BindingGroup]:
        """Sort binding groups in topological order."""
        # Create mapping from group names to groups
        name_to_group: Dict[str, BindingGroup] = {}
        for group in groups:
            for name in group.names:
                name_to_group[name] = group

        # Build dependency graph between groups
        group_deps: Dict[int, Set[int]] = {}
        for group in groups:
            group_id = id(group)  # Use object id as unique identifier
            deps: Set[int] = set()
            for dep_name in group.depends_on:
                if dep_name in name_to_group:
                    deps.add(id(name_to_group[dep_name]))

            group_deps[group_id] = deps

        # Topological sort
        visited: Set[int] = set()
        temp_visited: Set[int] = set()
        result: List[BindingGroup] = []
        group_by_id: Dict[int, BindingGroup] = {id(group): group for group in groups}

        def visit(group_id: int) -> None:
            if group_id in temp_visited:
                raise ValueError("Circular dependency detected")

            if group_id in visited:
                return

            temp_visited.add(group_id)
            for dep_id in group_deps.get(group_id, set()):
                visit(dep_id)

            temp_visited.remove(group_id)
            visited.add(group_id)
            result.append(group_by_id[group_id])

        for group in groups:
            if id(group) not in visited:
                visit(id(group))

        return result
