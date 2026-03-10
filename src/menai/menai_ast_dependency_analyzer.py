"""
Dependency analysis for letrec bindings to support mutual recursion.
Note: This is only used for letrec, not for regular let (which uses simple sequential binding).
"""

from typing import Dict, FrozenSet, List, Set, Tuple
from dataclasses import dataclass

from menai.menai_ast import MenaiASTNode, MenaiASTSymbol, MenaiASTList


@dataclass
class MenaiBindingGroup:
    """Represents a group of bindings that should be evaluated together."""
    names: Set[str]
    bindings: List[Tuple[str, MenaiASTNode]]
    is_recursive: bool
    depends_on: Set[str]  # Other groups this depends on


class MenaiASTDependencyAnalyzer:
    """Analyzes dependencies in let bindings to determine evaluation strategy."""

    def analyze_letrec_bindings(self, bindings: List[Tuple[str, MenaiASTNode]]) -> List[MenaiBindingGroup]:
        """
        Analyze letrec bindings and group them by dependencies.

        For letrec, all bindings can reference each other (including themselves).
        Returns:
            List of MenaiBindingGroup objects in topological order
        """
        # Step 1: Find what variables each binding references, in a single pass.
        binding_names = frozenset(name for name, _ in bindings)
        dependencies = self._collect_dependencies(bindings, binding_names)

        # Step 2: Find strongly connected components (recursive groups)
        scc_groups = self._find_strongly_connected_components(dependencies)

        # Step 3: Create MenaiBindingGroup objects
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

            groups.append(MenaiBindingGroup(
                names=group_names,
                bindings=group_bindings,
                is_recursive=is_recursive,
                depends_on=external_deps
            ))

        return groups

    def _collect_dependencies(
        self,
        bindings: List[Tuple[str, MenaiASTNode]],
        binding_names: FrozenSet[str],
    ) -> Dict[str, Set[str]]:
        """
        Build the dependency map for all bindings in a single combined pass.

        For each binding name, returns the subset of binding_names that appear
        as free references in its expression (respecting inner shadowing).
        """
        dependencies: Dict[str, Set[str]] = {name: set() for name, _ in bindings}
        for name, expr in bindings:
            self._scan_refs(expr, binding_names, frozenset(), dependencies[name])
        return dependencies

    def _scan_refs(
        self,
        expr: MenaiASTNode,
        targets: FrozenSet[str],
        shadowed: FrozenSet[str],
        refs: Set[str],
    ) -> None:
        """
        Recursively scan expr for references to names in targets that are not
        shadowed by an inner binding, accumulating hits into refs.

        targets:  the letrec binding names we care about (never changes).
        shadowed: names bound by enclosing inner forms (grows as we descend).
        refs:     the output set for the current top-level binding.
        """
        if isinstance(expr, MenaiASTSymbol):
            if expr.name in targets and expr.name not in shadowed:
                refs.add(expr.name)
            return

        if not isinstance(expr, MenaiASTList) or expr.is_empty():
            return

        first = expr.first()
        if not isinstance(first, MenaiASTSymbol):
            for elem in expr.elements:
                self._scan_refs(elem, targets, shadowed, refs)
            return

        name = first.name

        if name == 'lambda':
            if len(expr.elements) == 3:
                param_list, body = expr.elements[1], expr.elements[2]
                new_shadowed = shadowed
                if isinstance(param_list, MenaiASTList):
                    extra = frozenset(
                        p.name for p in param_list.elements
                        if isinstance(p, MenaiASTSymbol) and p.name != '.'
                    )
                    if extra:
                        new_shadowed = shadowed | extra
                self._scan_refs(body, targets, new_shadowed, refs)
            return

        if name == 'let':
            if len(expr.elements) == 3:
                binding_list, body = expr.elements[1], expr.elements[2]
                new_shadowed = shadowed
                if isinstance(binding_list, MenaiASTList):
                    for binding in binding_list.elements:
                        if isinstance(binding, MenaiASTList) and len(binding.elements) == 2:
                            self._scan_refs(binding.elements[1], targets, shadowed, refs)
                            var = binding.elements[0]
                            if isinstance(var, MenaiASTSymbol):
                                new_shadowed = new_shadowed | frozenset({var.name})
                self._scan_refs(body, targets, new_shadowed, refs)
            return

        if name == 'letrec':
            if len(expr.elements) == 3:
                binding_list, body = expr.elements[1], expr.elements[2]
                new_shadowed = shadowed
                if isinstance(binding_list, MenaiASTList):
                    extra = frozenset(
                        binding.elements[0].name
                        for binding in binding_list.elements
                        if isinstance(binding, MenaiASTList)
                        and len(binding.elements) == 2
                        and isinstance(binding.elements[0], MenaiASTSymbol)
                    )
                    if extra:
                        new_shadowed = shadowed | extra
                    for binding in binding_list.elements:
                        if isinstance(binding, MenaiASTList) and len(binding.elements) == 2:
                            self._scan_refs(binding.elements[1], targets, new_shadowed, refs)
                self._scan_refs(body, targets, new_shadowed, refs)
            return

        if name == 'let*':
            if len(expr.elements) == 3:
                binding_list, body = expr.elements[1], expr.elements[2]
                new_shadowed = shadowed
                if isinstance(binding_list, MenaiASTList):
                    for binding in binding_list.elements:
                        if isinstance(binding, MenaiASTList) and len(binding.elements) == 2:
                            self._scan_refs(binding.elements[1], targets, new_shadowed, refs)
                            var = binding.elements[0]
                            if isinstance(var, MenaiASTSymbol):
                                new_shadowed = new_shadowed | frozenset({var.name})
                self._scan_refs(body, targets, new_shadowed, refs)
            return

        for elem in expr.elements:
            self._scan_refs(elem, targets, shadowed, refs)

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
