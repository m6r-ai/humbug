"""
Free variable analyzer for the Menai compiler.

This is a standalone analysis pass that runs on the desugared AST and
annotates every lambda node in the tree with its set of free variable names.

A name is free in a lambda if:
  - It appears as a symbol reference somewhere in the lambda body
  - It is not a parameter of this lambda
  - It is not bound by a let or letrec that encloses the reference within
    the lambda body
  - It resolves to a local variable in some enclosing scope (i.e. it is not
    a global builtin or prelude name)

The pass produces a FreeVarInfo object that maps id(lambda_node) ->
frozenset[str] for every lambda in the tree, including lambdas nested inside
other lambdas.  Callers use FreeVarInfo.get(node) to query the annotation.

Design notes
------------
- The pass operates on the desugared AST, so it can assume:
    * No match, and, or, let*, letrec with mixed bindings
    * Every letrec is a genuine mutually-recursive group of lambdas
    * All other constructs are: if, let, letrec, lambda, quote, trace,
      function calls, and literals
- Scope is tracked as a stack of frozensets of bound names.  A name is
  "local" if it appears in any frame of the stack; otherwise it is global.
- The pass does NOT distinguish captured free variables from parent
  references (recursive back-edges).  That classification is Step 3 and
  depends on letrec structure.  Here we simply report all names that are
  free in the lambda body and resolve to a local in some enclosing scope.
- The pass is purely analytical: it does not modify the AST.
"""

from dataclasses import dataclass, field
from typing import FrozenSet, List

from menai.menai_ast import (
    MenaiASTList,
    MenaiASTNode,
    MenaiASTSymbol,
)


@dataclass
class FreeVarInfo:
    """
    Annotation produced by MenaiFreeVarAnalyzer.

    Maps id(lambda_node) -> frozenset of free variable names for every
    lambda node encountered during the walk.
    """
    _data: dict[int, FrozenSet[str]] = field(default_factory=dict)

    def get(self, lambda_node: MenaiASTList) -> FrozenSet[str]:
        """Return the free variable set for a lambda node."""
        return self._data.get(id(lambda_node), frozenset())

    def record(self, lambda_node: MenaiASTList, free_vars: FrozenSet[str]) -> None:
        """Record the free variable set for a lambda node (internal use)."""
        self._data[id(lambda_node)] = free_vars

    def all_lambdas(self) -> list[tuple[int, FrozenSet[str]]]:
        """Return all (node_id, free_vars) pairs, for testing/debugging."""
        return list(self._data.items())


class MenaiFreeVarAnalyzer:
    """
    Annotates every lambda in a desugared AST with its free variable names.

    Usage::

        info = MenaiFreeVarAnalyzer().analyze(desugared_ast)
        free = info.get(some_lambda_node)   # frozenset[str]
    """

    def analyze(self, expr: MenaiASTNode) -> FreeVarInfo:
        """
        Walk the entire desugared AST and return a FreeVarInfo annotation.

        Args:
            expr: Root of the desugared AST.

        Returns:
            FreeVarInfo mapping every lambda node to its free variable names.
        """
        info = FreeVarInfo()
        # The outermost scope has no local bindings.
        self._walk(expr, scope_stack=[], info=info)
        return info

    def _walk(
        self,
        expr: MenaiASTNode,
        scope_stack: List[FrozenSet[str]],
        info: FreeVarInfo,
    ) -> None:
        """
        Recursively walk expr, updating info with free-var annotations for
        every lambda encountered.

        Args:
            expr:        Current AST node.
            scope_stack: Stack of frozensets, one per enclosing local scope.
                         Innermost scope is last.  A name is "local" iff it
                         appears in any frame of this stack.
            info:        Accumulator for the annotation results.
        """
        if not isinstance(expr, MenaiASTList):
            # Literals and bare symbols carry no lambdas to annotate.
            return

        if expr.is_empty():
            return

        first = expr.first()
        if not isinstance(first, MenaiASTSymbol):
            # Anonymous call head — walk all elements as expressions.
            for elem in expr.elements:
                self._walk(elem, scope_stack, info)

            return

        name = first.name

        if name == 'lambda':
            self._walk_lambda(expr, scope_stack, info)

        elif name == 'let':
            self._walk_let(expr, scope_stack, info)

        elif name == 'letrec':
            self._walk_letrec(expr, scope_stack, info)

        elif name == 'if':
            # (if condition then else) — walk all three sub-expressions.
            assert len(expr.elements) == 4
            self._walk(expr.elements[1], scope_stack, info)
            self._walk(expr.elements[2], scope_stack, info)
            self._walk(expr.elements[3], scope_stack, info)

        elif name == 'quote':
            # Quoted data is never evaluated; no lambdas inside.
            pass

        elif name == 'trace':
            # (trace msg1 ... msgN expr) — walk all arguments.
            for elem in expr.elements[1:]:
                self._walk(elem, scope_stack, info)

        else:
            # Regular function call or other special form whose sub-expressions
            # are all evaluated in the current scope (error, apply, etc.).
            for elem in expr.elements:
                self._walk(elem, scope_stack, info)

    def _walk_lambda(
        self,
        expr: MenaiASTList,
        scope_stack: List[FrozenSet[str]],
        info: FreeVarInfo,
    ) -> None:
        """
        Process a lambda node.

        Records the free variables of this lambda in info, then recurses
        into the body with the lambda's parameters added to the scope.

        Free variable definition: a name that is referenced in the body,
        not bound by a parameter or inner let/letrec, and resolves to a
        local in some enclosing scope (i.e. it is in scope_stack).
        """
        assert len(expr.elements) == 3, "Lambda must have exactly 3 elements after desugaring"

        params_node = expr.elements[1]
        body = expr.elements[2]

        assert isinstance(params_node, MenaiASTList)

        # Collect parameter names, skipping the variadic dot sentinel.
        param_names: FrozenSet[str] = frozenset(
            p.name
            for p in params_node.elements
            if isinstance(p, MenaiASTSymbol) and p.name != '.'
        )

        # Compute the set of names referenced in the body that are not bound
        # by param_names or any inner binding, but ARE in scope_stack.
        free_vars = self._collect_free(body, bound=param_names, scope_stack=scope_stack)
        info.record(expr, free_vars)

        # Recurse into the body with params added as a new scope frame so
        # that nested lambdas see the correct local bindings.
        self._walk(body, scope_stack + [param_names], info)

    def _walk_let(
        self,
        expr: MenaiASTList,
        scope_stack: List[FrozenSet[str]],
        info: FreeVarInfo,
    ) -> None:
        """
        Process a let node.

        Binding values are walked in the current scope (parallel semantics).
        The body is walked with the binding names added as a new scope frame.
        """
        assert len(expr.elements) == 3
        bindings_node = expr.elements[1]
        body = expr.elements[2]
        assert isinstance(bindings_node, MenaiASTList)

        bound_names: FrozenSet[str] = frozenset(
            b.elements[0].name
            for b in bindings_node.elements
            if isinstance(b, MenaiASTList) and isinstance(b.elements[0], MenaiASTSymbol)
        )

        # Binding values see the outer scope (parallel let semantics).
        for binding in bindings_node.elements:
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
            self._walk(binding.elements[1], scope_stack, info)

        # Body sees the new bindings.
        self._walk(body, scope_stack + [bound_names], info)

    def _walk_letrec(
        self,
        expr: MenaiASTList,
        scope_stack: List[FrozenSet[str]],
        info: FreeVarInfo,
    ) -> None:
        """
        Process a letrec node.

        After desugaring, every letrec is a genuine mutually-recursive group.
        All binding names are in scope for both the binding values and the body.
        """
        assert len(expr.elements) == 3
        bindings_node = expr.elements[1]
        body = expr.elements[2]
        assert isinstance(bindings_node, MenaiASTList)

        bound_names: FrozenSet[str] = frozenset(
            b.elements[0].name
            for b in bindings_node.elements
            if isinstance(b, MenaiASTList) and isinstance(b.elements[0], MenaiASTSymbol)
        )

        # All names are in scope for binding values AND body (mutual recursion).
        inner_stack = scope_stack + [bound_names]

        for binding in bindings_node.elements:
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
            self._walk(binding.elements[1], inner_stack, info)

        self._walk(body, inner_stack, info)

    def _collect_free(
        self,
        expr: MenaiASTNode,
        bound: FrozenSet[str],
        scope_stack: List[FrozenSet[str]],
    ) -> FrozenSet[str]:
        """
        Return the set of names that are:
          - referenced in expr
          - not in bound (the lambda's own parameters, plus any inner bindings
            accumulated during the recursive walk of expr)
          - present in scope_stack (i.e. they are locals in an enclosing scope,
            not global builtins or prelude names)

        This is used exclusively to compute the free variable annotation for a
        single lambda node.  It does NOT recurse into nested lambdas to collect
        their free vars — those are handled by the outer _walk_lambda call.
        However, it DOES cross nested lambda boundaries to find names that the
        outer lambda needs to capture so the inner lambda can use them.

        Args:
            expr:        AST subtree to scan.
            bound:       Names bound at the current point within the lambda body.
            scope_stack: Enclosing local scopes (outside the lambda being analysed).

        Returns:
            frozenset of free variable names.
        """
        result: set[str] = set()
        self._collect_free_rec(expr, bound, scope_stack, result)
        return frozenset(result)

    def _collect_free_rec(
        self,
        expr: MenaiASTNode,
        bound: FrozenSet[str],
        scope_stack: List[FrozenSet[str]],
        result: set[str],
    ) -> None:
        """Recursive helper for _collect_free."""

        if isinstance(expr, MenaiASTSymbol):
            name = expr.name
            if name not in bound and self._is_local(name, scope_stack):
                result.add(name)

            return

        if not isinstance(expr, MenaiASTList) or expr.is_empty():
            return

        first = expr.first()
        if not isinstance(first, MenaiASTSymbol):
            # Anonymous call head — recurse into all elements.
            for elem in expr.elements:
                self._collect_free_rec(elem, bound, scope_stack, result)
            return

        fname = first.name

        if fname == 'lambda':
            # Nested lambda: its parameters shadow names from the enclosing
            # lambda.  We need to find what the nested lambda references from
            # *our* scope, so the enclosing lambda can capture it.
            assert len(expr.elements) == 3
            nested_params_node = expr.elements[1]
            nested_body = expr.elements[2]
            assert isinstance(nested_params_node, MenaiASTList)

            nested_params: FrozenSet[str] = frozenset(
                p.name
                for p in nested_params_node.elements
                if isinstance(p, MenaiASTSymbol) and p.name != '.'
            )

            # Recurse into nested body with nested params added to bound.
            self._collect_free_rec(nested_body, bound | nested_params, scope_stack, result)

        elif fname == 'let':
            assert len(expr.elements) == 3
            bindings_node = expr.elements[1]
            body = expr.elements[2]
            assert isinstance(bindings_node, MenaiASTList)

            let_names: FrozenSet[str] = frozenset(
                b.elements[0].name
                for b in bindings_node.elements
                if isinstance(b, MenaiASTList) and isinstance(b.elements[0], MenaiASTSymbol)
            )

            # Binding values see the outer bound set (parallel let semantics).
            for binding in bindings_node.elements:
                assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
                self._collect_free_rec(binding.elements[1], bound, scope_stack, result)

            # Body sees the let-bound names.
            self._collect_free_rec(body, bound | let_names, scope_stack, result)

        elif fname == 'letrec':
            assert len(expr.elements) == 3
            bindings_node = expr.elements[1]
            body = expr.elements[2]
            assert isinstance(bindings_node, MenaiASTList)

            letrec_names: FrozenSet[str] = frozenset(
                b.elements[0].name
                for b in bindings_node.elements
                if isinstance(b, MenaiASTList) and isinstance(b.elements[0], MenaiASTSymbol)
            )

            # All names in scope for both binding values and body.
            inner_bound = bound | letrec_names
            for binding in bindings_node.elements:
                assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
                self._collect_free_rec(binding.elements[1], inner_bound, scope_stack, result)

            self._collect_free_rec(body, inner_bound, scope_stack, result)

        elif fname == 'quote':
            # Quoted data is never evaluated.
            pass

        else:
            # if, trace, error, apply, regular calls — recurse into all elements.
            for elem in expr.elements:
                self._collect_free_rec(elem, bound, scope_stack, result)

    # ------------------------------------------------------------------
    # Scope helpers
    # ------------------------------------------------------------------

    def _is_local(self, name: str, scope_stack: List[FrozenSet[str]]) -> bool:
        """
        Return True if name appears in any frame of scope_stack.

        A name that is NOT in scope_stack is either a global builtin, a prelude
        function, or an unbound name (which the semantic analyser would have
        already rejected).  In all those cases it is not a free variable of
        the lambda being analysed.
        """
        return any(name in frame for frame in scope_stack)
