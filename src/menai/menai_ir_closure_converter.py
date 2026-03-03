"""
Menai IR Closure Converter.

Currently a pure tree-rewriting identity pass: recurses into every IR node
type — including sibling_free_var_plans and outer_free_var_plans, which are
evaluated in the enclosing frame and may themselves contain nested lambdas —
but returns a structurally identical tree without making any changes.

Serves as the insertion point for any pre-lifting transformation that needs
to run after free-variable classification and before lambda lifting.
"""

from __future__ import annotations

from typing import List

from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIREmptyList,
    MenaiIRError,
    MenaiIRExpr,
    MenaiIRIf,
    MenaiIRLambda,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRQuote,
    MenaiIRReturn,
    MenaiIRTrace,
    MenaiIRVariable,
)


class MenaiIRClosureConverter:
    """
    Makes closure capture explicit in the IR tree (Step 4).

    Currently a tree-rewriting identity pass that recurses into every node
    (including free_var_plans and parent_ref_plans) without changing the
    lambda structure.  Serves as the pipeline insertion point for the
    lambda-lifting transformation (Step 5).

    The pass is stateless between calls.

    Usage::

        new_ir = MenaiIRClosureConverter().convert(ir)
    """

    def convert(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """
        Walk *ir* and return a new tree with all closure captures made
        explicit.

        Args:
            ir: IR tree produced by MenaiIRParentRefClassifier and addressed
                by MenaiIRAddresser.

        Returns:
            New IR tree (structurally equivalent; ready for addresser re-run).
        """
        return self._walk(ir)

    def _walk(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """Recursively walk and rewrite *ir*."""

        if isinstance(ir, MenaiIRLambda):
            return self._convert_lambda(ir)

        if isinstance(ir, MenaiIRLet):
            return self._walk_let(ir)

        if isinstance(ir, MenaiIRLetrec):
            return self._walk_letrec(ir)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._walk(ir.condition_plan),
                then_plan=self._walk(ir.then_plan),
                else_plan=self._walk(ir.else_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRCall):
            return self._walk_call(ir)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._walk(ir.value_plan))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._walk(m) for m in ir.message_plans],
                value_plan=self._walk(ir.value_plan),
            )

        if isinstance(ir, (MenaiIRVariable, MenaiIRConstant, MenaiIRQuote,
                           MenaiIREmptyList, MenaiIRError)):
            # Leaf nodes — return unchanged.
            return ir

        raise TypeError(
            f"MenaiIRClosureConverter: unhandled IR node type {type(ir).__name__}"
        )

    # ------------------------------------------------------------------
    # Node-specific handlers
    # ------------------------------------------------------------------

    def _convert_lambda(self, ir: MenaiIRLambda) -> MenaiIRLambda:
        """
        Convert a lambda node.

        Recurses into the body and into free_var_plans (evaluated in the
        enclosing frame and may contain nested lambdas that need converting).
        (parent_ref_plans removed — letrec siblings are now regular free_vars)

        The lambda's params, sibling_free_vars, outer_free_vars, param_count, and
        max_locals are preserved: the MAKE_CLOSURE mechanism already makes
        capture explicit at the bytecode level, and param_count must not
        change (it controls how many arguments ENTER pops from the call
        stack).
        """
        new_body = self._walk(ir.body_plan)

        # Recurse into sibling/outer free_var_plans: evaluated in the enclosing frame and
        # may themselves contain lambdas.
        new_sibling_free_var_plans: List[MenaiIRExpr] = [
            self._walk(p) for p in ir.sibling_free_var_plans
        ]
        new_outer_free_var_plans: List[MenaiIRExpr] = [
            self._walk(p) for p in ir.outer_free_var_plans
        ]

        return MenaiIRLambda(
            params=ir.params,
            body_plan=new_body,
            sibling_free_vars=ir.sibling_free_vars,
            sibling_free_var_plans=new_sibling_free_var_plans,
            outer_free_vars=ir.outer_free_vars,
            outer_free_var_plans=new_outer_free_var_plans,
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _walk_let(self, ir: MenaiIRLet) -> MenaiIRLet:
        """Walk a let node, recursing into binding values and body."""
        new_bindings = [(name, self._walk(value_plan)) for name, value_plan in ir.bindings]
        return MenaiIRLet(
            bindings=new_bindings,
            body_plan=self._walk(ir.body_plan),
            in_tail_position=ir.in_tail_position,
        )

    def _walk_letrec(self, ir: MenaiIRLetrec) -> MenaiIRLetrec:
        """Walk a letrec node, recursing into binding values and body."""
        new_bindings = [(name, self._walk(value_plan)) for name, value_plan in ir.bindings]
        return MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=self._walk(ir.body_plan),
            in_tail_position=ir.in_tail_position,
        )

    def _walk_call(self, ir: MenaiIRCall) -> MenaiIRCall:
        """Walk a call node."""
        new_args = [self._walk(a) for a in ir.arg_plans]

        return MenaiIRCall(
            func_plan=self._walk(ir.func_plan),
            arg_plans=new_args,
            is_tail_call=ir.is_tail_call,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )
