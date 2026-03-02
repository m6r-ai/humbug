"""
Menai IR Lambda Lifter — worker/wrapper transformation.

This pass walks the IR tree and replaces every MenaiIRLambda that has free
variables or parent references with:

    (let ((helper (lambda (p0..pN fv0..fvM) <original body>)))
      (lambda (p0..pN)
        <tail-calls helper with p0..pN + captured fv0..fvM>))

Terminology
-----------
- *helper*  — the lifted, fully-closed function.  Takes original params plus
              all captured values (former free_vars) as explicit parameters.
              sibling_free_vars=[], outer_free_vars=[].
- *wrapper* — thin lambda with the original arity.  Captures the helper plus
              all formerly-captured values via MAKE_CLOSURE, then tail-calls
              the helper passing everything through.  Correct for first-class
              uses (map-list, fold-list, return values, etc.).

After this pass every MenaiIRLambda in the tree has sibling_free_vars==[] and
outer_free_vars==[].

Lambdas that are already closed are left structurally unchanged.

Variables remain symbolic
--------------------------
All MenaiIRVariable nodes emitted by this pass carry depth=-1, index=-1
(unresolved sentinels), exactly as the IR builder emits them.  MenaiIRAddresser
runs once after all transformation and optimisation passes and resolves
everything in a single final pass.  The _reset_vars step from the old
index-based implementation is no longer needed.

Pipeline position
-----------------
    MenaiIRBuilder
        ↓
    MenaiIRClosureConverter
        ↓
    MenaiIRLambdaLifter          ← THIS PASS
        ↓
    IR optimization passes
        ↓
    MenaiIRAddresser             (single final run)
        ↓
    MenaiCodeGen

Usage
-----
    lifter = MenaiIRLambdaLifter()
    new_ir = lifter.lift(ir)
"""

from __future__ import annotations

from typing import List, Tuple

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


class MenaiIRLambdaLifter:
    """
    Implements the worker/wrapper lambda lifting transformation.

    Every MenaiIRLambda with non-empty sibling_free_vars or outer_free_vars is
    replaced by a let-bound helper (fully closed) and a thin wrapper (original
    arity, captures everything via MAKE_CLOSURE, tail-calls the helper).

    Usage::

        new_ir = MenaiIRLambdaLifter().lift(ir)
    """

    def __init__(self) -> None:
        self._lift_counter: int = 0

    def lift(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """Walk *ir* and return a new tree with all capturing lambdas lifted."""
        return self._walk(ir)

    def _walk(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """Recursively walk the IR tree and lift capturing lambdas."""
        if isinstance(ir, MenaiIRLambda):
            return self._lift_lambda(ir)

        if isinstance(ir, MenaiIRLet):
            return MenaiIRLet(
                bindings=[(name, self._walk(value_plan)) for name, value_plan, *_ in ir.bindings],
                body_plan=self._walk(ir.body_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRLetrec):
            return MenaiIRLetrec(
                bindings=[(name, self._walk(value_plan)) for name, value_plan, *_ in ir.bindings],
                body_plan=self._walk(ir.body_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._walk(ir.condition_plan),
                then_plan=self._walk(ir.then_plan),
                else_plan=self._walk(ir.else_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRCall):
            return MenaiIRCall(
                func_plan=self._walk(ir.func_plan),
                arg_plans=[self._walk(a) for a in ir.arg_plans],
                is_tail_call=ir.is_tail_call,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._walk(ir.value_plan))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._walk(m) for m in ir.message_plans],
                value_plan=self._walk(ir.value_plan),
            )

        if isinstance(ir, (MenaiIRVariable, MenaiIRConstant, MenaiIRQuote,
                           MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRLambdaLifter: unhandled IR node type {type(ir).__name__}"
        )

    # ------------------------------------------------------------------
    # Lambda lifting
    # ------------------------------------------------------------------

    def _lift_lambda(self, ir: MenaiIRLambda) -> MenaiIRExpr:
        """
        Lift a capturing lambda into a (let (helper) wrapper) pair.

        Recurses into free_var_plans and the body first so that nested
        capturing lambdas are lifted before we process the outer one.

        If the lambda is already closed (no free vars), only recurse into
        the body and return a structurally identical node.
        """
        # Always recurse into free_var_plans — they may contain nested lambdas.
        new_sibling_free_var_plans: List[MenaiIRExpr] = [
            self._walk(p) for p in ir.sibling_free_var_plans
        ]
        new_outer_free_var_plans: List[MenaiIRExpr] = [
            self._walk(p) for p in ir.outer_free_var_plans
        ]

        # Recurse into the body.
        new_body = self._walk(ir.body_plan)

        if not ir.sibling_free_vars and not ir.outer_free_vars:
            # Already closed — return structurally unchanged.
            return MenaiIRLambda(
                params=ir.params,
                body_plan=new_body,
                sibling_free_vars=[],
                sibling_free_var_plans=[],
                outer_free_vars=[],
                outer_free_var_plans=[],
                param_count=ir.param_count,
                is_variadic=ir.is_variadic,
                binding_name=ir.binding_name,
                source_line=ir.source_line,
                source_file=ir.source_file,
            )

        # Build all_captured: (name, free_var_plan) for every value the
        # original lambda needed from outside its own frame.
        all_captured: List[Tuple[str, MenaiIRExpr]] = list(zip(
            ir.sibling_free_vars + ir.outer_free_vars,
            new_sibling_free_var_plans + new_outer_free_var_plans,
        ))

        captured_names: List[str] = [name for name, _ in all_captured]
        orig_param_count = ir.param_count

        # Helper lambda
        #
        # params = original params + all captured names
        # body = original body (variables are already symbolic — no reset needed)
        # is_variadic = False (wrapper packs rest args; helper receives plain list)
        helper_params = list(ir.params) + captured_names
        helper_param_count = len(helper_params)
        helper_name = self._next_lifted_name(ir.binding_name)

        helper_lambda = MenaiIRLambda(
            params=helper_params,
            body_plan=new_body,
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=[],
            outer_free_var_plans=[],
            param_count=helper_param_count,
            is_variadic=False,
            binding_name=helper_name,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

        # Wrapper lambda
        #
        # params = original params (arity unchanged)
        # sibling_free_vars = original sibling captures (still need PATCH_CLOSURE)
        # outer_free_vars = [helper] + original outer captures
        # body = tail-call helper(p0..pN-1, cap0..capM-1)
        # All variable references are symbolic (depth=-1, index=-1).

        wrapper_sibling_free_vars: List[str] = list(ir.sibling_free_vars)
        wrapper_sibling_free_var_plans: List[MenaiIRExpr] = list(new_sibling_free_var_plans)

        wrapper_outer_free_vars: List[str] = [helper_name] + list(ir.outer_free_vars)
        wrapper_outer_free_var_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(name=helper_name, var_type='local', is_parent_ref=False),
        ] + list(new_outer_free_var_plans)

        # Arguments to the helper: original params then all captured names.
        wrapper_arg_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(name=pname, var_type='local', is_parent_ref=False)
            for pname in ir.params
        ] + [
            MenaiIRVariable(name=cname, var_type='local', is_parent_ref=False)
            for cname in captured_names
        ]

        wrapper_lambda = MenaiIRLambda(
            params=ir.params,
            body_plan=MenaiIRReturn(
                value_plan=MenaiIRCall(
                    func_plan=MenaiIRVariable(
                        name=helper_name, var_type='local', is_parent_ref=False,
                    ),
                    arg_plans=wrapper_arg_plans,
                    is_tail_call=True,
                    is_builtin=False,
                    builtin_name=None,
                )
            ),
            sibling_free_vars=wrapper_sibling_free_vars,
            sibling_free_var_plans=wrapper_sibling_free_var_plans,
            outer_free_vars=wrapper_outer_free_vars,
            outer_free_var_plans=wrapper_outer_free_var_plans,
            param_count=orig_param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

        # Wrap in a let that binds the helper.
        return MenaiIRLet(
            bindings=[(helper_name, helper_lambda)],
            body_plan=wrapper_lambda,
            in_tail_position=False,
        )

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _next_lifted_name(self, binding_name: str | None) -> str:
        """Generate a unique name for a lifted helper lambda."""
        n = self._lift_counter
        self._lift_counter += 1
        if binding_name:
            return f"<lifted-{n}-{binding_name}>"
        return f"<lifted-{n}>"
