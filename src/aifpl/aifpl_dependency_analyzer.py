"""
Dependency analysis for letrec bindings to support mutual recursion.
Note: This is only used for letrec, not for regular let (which uses simple sequential binding).
"""

from typing import Dict, List, Set, Tuple
from dataclasses import dataclass

from aifpl.aifpl_ast import AIFPLASTNode, AIFPLASTSymbol, AIFPLASTList


@dataclass
class AIFPLBindingGroup:
    """Represents a group of bindings that should be evaluated together."""
    names: Set[str]
    bindings: List[Tuple[str, AIFPLASTNode]]
    is_recursive: bool
    depends_on: Set[str]  # Other groups this depends on


class AIFPLDependencyAnalyzer:
    """Analyzes dependencies in let bindings to determine evaluation strategy."""

    def analyze_letrec_bindings(self, bindings: List[Tuple[str, AIFPLASTNode]]) -> List[AIFPLBindingGroup]:
        """
        Analyze letrec bindings and group them by dependencies.

        For letrec, all bindings can reference each other (including themselves).
        Returns:
            List of AIFPLBindingGroup objects in topological order
        """
        # Step 1: Find what variables each binding references
        dependencies = {}
        binding_names = {name for name, _ in bindings}

        for name, expr in bindings:
            free_vars = self._find_free_variables(expr)
            # For letrec, include ALL references to binding names (including self-references)
            # This allows mutual recursion and self-recursion
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

        return groups

    def _find_free_variables(self, expr: AIFPLASTNode) -> Set[str]:
        """Find all free variables (symbols) in an expression."""
        free_vars = set()

        if isinstance(expr, AIFPLASTSymbol):
            free_vars.add(expr.name)

        elif isinstance(expr, AIFPLASTList):
            if not expr.is_empty():
                first_elem = expr.first()

                # Handle special forms
                if isinstance(first_elem, AIFPLASTSymbol):
                    if first_elem.name == "lambda":
                        # (lambda (params...) body)
                        assert expr.length() == 3, "Lambda expressions must have exactly 3 elements (validated by evaluator)"

                        param_list = expr.get(1)
                        body = expr.get(2)

                        assert isinstance(param_list, AIFPLASTList), "Lambda parameter list must be a list (validated by evaluator)"

                        # Extract parameter names (all validated by evaluator to be symbols)
                        param_names = set()
                        for param in param_list.elements:
                            assert isinstance(param, AIFPLASTSymbol), "Lambda parameters must be symbols (validated by evaluator)"
                            param_names.add(param.name)

                        # Find free variables in body, excluding parameters
                        body_vars = self._find_free_variables(body)
                        free_vars.update(body_vars - param_names)

                        return free_vars

                    if first_elem.name == "let":
                        # (let ((var1 val1) (var2 val2) ...) body)
                        assert expr.length() == 3, "Let expressions must have exactly 3 elements (validated by evaluator)"

                        binding_list = expr.get(1)
                        body = expr.get(2)

                        assert isinstance(binding_list, AIFPLASTList), "Let binding list must be a list (validated by evaluator)"

                        binding_names = set()

                        # Process bindings (all validated by evaluator)
                        for binding in binding_list.elements:
                            assert isinstance(binding, AIFPLASTList), "Let bindings must be lists (validated by evaluator)"
                            assert binding.length() == 2, "Let bindings must have exactly 2 elements (validated by evaluator)"

                            var_name = binding.get(0)
                            var_value = binding.get(1)

                            assert isinstance(var_name, AIFPLASTSymbol), \
                                "Let binding variables must be symbols (validated by evaluator)"
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
        Note: SCCs are returned in topological order (dependencies before dependents).

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
