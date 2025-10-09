"""Dependency analysis for let bindings to support automatic recursion detection."""

from typing import Dict, List, Set, Tuple
from dataclasses import dataclass

from aifpl.aifpl_value import AIFPLValue, AIFPLSymbol, AIFPLList


@dataclass
class AIFPLBindingGroup:
    """Represents a group of bindings that should be evaluated together."""
    names: Set[str]
    bindings: List[Tuple[str, AIFPLValue]]
    is_recursive: bool
    depends_on: Set[str]  # Other groups this depends on


class AIFPLDependencyAnalyzer:
    """Analyzes dependencies in let bindings to determine evaluation strategy."""

    def analyze_let_bindings(self, bindings: List[Tuple[str, AIFPLValue]]) -> List[AIFPLBindingGroup]:
        """
        Analyze let bindings and group them by dependencies.

        Returns:
            List of AIFPLBindingGroup objects in topological order
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

        # Step 3: Create AIFPLBindingGroup objects
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

            groups.append(AIFPLBindingGroup(
                names=group_names,
                bindings=group_bindings,
                is_recursive=is_recursive,
                depends_on=external_deps
            ))

        # Step 4: Sort groups in topological order
        return self._topological_sort_groups(groups)

    def _find_free_variables(self, expr: AIFPLValue) -> Set[str]:
        """Find all free variables (symbols) in an expression."""
        free_vars = set()

        if isinstance(expr, AIFPLSymbol):
            free_vars.add(expr.name)

        elif isinstance(expr, AIFPLList):
            if not expr.is_empty():
                first_elem = expr.first()

                # Handle special forms
                if isinstance(first_elem, AIFPLSymbol):
                    if first_elem.name == "lambda":
                        # (lambda (params...) body)
                        if expr.length() == 3:
                            param_list = expr.get(1)
                            body = expr.get(2)

                            # Extract parameter names
                            param_names = set()
                            if isinstance(param_list, AIFPLList):
                                for param in param_list.elements:
                                    if isinstance(param, AIFPLSymbol):
                                        param_names.add(param.name)

                            # Find free variables in body, excluding parameters
                            body_vars = self._find_free_variables(body)
                            free_vars.update(body_vars - param_names)

                        return free_vars

                    if first_elem.name == "let":
                        # (let ((var1 val1) (var2 val2) ...) body)
                        if expr.length() == 3:
                            binding_list = expr.get(1)
                            body = expr.get(2)

                            binding_names = set()

                            # Process bindings
                            if isinstance(binding_list, AIFPLList):
                                for binding in binding_list.elements:
                                    if isinstance(binding, AIFPLList) and binding.length() == 2:
                                        var_name = binding.get(0)
                                        var_value = binding.get(1)

                                        if isinstance(var_name, AIFPLSymbol):
                                            binding_names.add(var_name.name)

                                        # Free variables in binding expressions
                                        free_vars.update(self._find_free_variables(var_value))

                            # Free variables in body, excluding bound names
                            body_vars = self._find_free_variables(body)
                            free_vars.update(body_vars - binding_names)

                        return free_vars

                # Regular list - process all elements
                for elem in expr.elements:
                    free_vars.update(self._find_free_variables(elem))

        # For other expression types (numbers, strings, booleans, etc.), no free variables
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

    def _topological_sort_groups(self, groups: List[AIFPLBindingGroup]) -> List[AIFPLBindingGroup]:
        """Sort binding groups in topological order."""
        # Create mapping from group names to groups
        name_to_group: Dict[str, AIFPLBindingGroup] = {}
        for group in groups:
            for name in group.names:
                name_to_group[name] = group

        # Build dependency graph between groups
        group_deps: Dict[int, Set[int]] = {}
        for group in groups:
            group_id = id(group)  # Use object id as unique identifier
            deps: Set[int] = set()
            for dep_name in group.depends_on:
                deps.add(id(name_to_group[dep_name]))

            group_deps[group_id] = deps

        # Topological sort
        visited: Set[int] = set()
        temp_visited: Set[int] = set()
        result: List[AIFPLBindingGroup] = []
        group_by_id: Dict[int, AIFPLBindingGroup] = {id(group): group for group in groups}

        def visit(group_id: int) -> None:
            assert group_id not in temp_visited, "Circular dependency detected"
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
