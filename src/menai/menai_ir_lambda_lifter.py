"""
Menai IR Lambda Lifter — worker/wrapper transformation.

This pass walks the IR tree and replaces every MenaiIRLambda that has free
variables with a fully-closed helper and a thin wrapper.

For a standalone capturing lambda (inside a let or as a plain expression):

    (let ((<lifted-N-f> (lambda (p0..pN fv0..fvM) <original body>)))
      (lambda (p0..pN)
        (tail-call <lifted-N-f> p0..pN fv0..fvM)))

For a letrec group, all helpers from all capturing bindings are hoisted into
a single enclosing let, and the letrec retains only the wrappers:

    (let ((<lifted-N-f> (lambda (p0..pN fvs-of-f...) <body-of-f>))
          (<lifted-M-g> (lambda (q0..qM fvs-of-g...) <body-of-g>)))
      (letrec ((f (lambda (p0..pN) (tail-call <lifted-N-f> p0..pN fvs-of-f...)))
               (g (lambda (q0..qM) (tail-call <lifted-M-g> q0..qM fvs-of-g...))))
        <body>))

Terminology
-----------
- *helper*  — the lifted, fully-closed function.  Takes original params plus
              all captured values (former free_vars) as explicit parameters.
              sibling_free_vars=[], outer_free_vars=[].
- *wrapper* — thin lambda with the original arity.  Captures the helper plus
              all formerly-captured values via MAKE_CLOSURE, then tail-calls
              the helper passing everything through.  Correct for first-class
              uses (map-list, fold-list, return values, etc.).  For the letrec
              case the wrapper still captures its sibling wrappers via
              PATCH_CLOSURE, but the helper is an outer (let-bound) capture
              that is available at MAKE_CLOSURE time.

Hoisting helpers out of the letrec makes them co-visible with all sibling
wrappers in the enclosing let scope, which allows the inliner to see through
wrapper call sites inside sibling helpers.

After this pass every MenaiIRLambda in the tree has sibling_free_vars==[] and
outer_free_vars==[].

Lambdas that are already closed are left structurally unchanged.

Variables remain symbolic
--------------------------
All MenaiIRVariable nodes emitted by this pass carry depth=-1, index=-1
(unresolved sentinels), exactly as the IR builder emits them.  MenaiIRAddresser
resolves everything in a single pass after all transformation and optimisation
passes are complete.
"""

from __future__ import annotations

from typing import List, Optional, Tuple

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

    For letrec groups all helpers are hoisted into a single enclosing let so
    that they are co-visible with their sibling wrappers, enabling the inliner
    to see through wrapper call sites inside sibling helpers.

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
            return self._walk_letrec(ir)

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

    def _walk_letrec(self, ir: MenaiIRLetrec) -> MenaiIRExpr:
        """
        Walk a letrec group, hoisting all helpers into an enclosing let.

        For each binding whose value is a capturing lambda, _lift_lambda_parts
        returns the (helper, wrapper) pair.  All helpers are collected and
        emitted as a single let that wraps the reconstructed letrec, making
        them co-visible with all sibling wrappers.

        Bindings whose values are already-closed lambdas or any other
        expression are walked normally and kept in the letrec unchanged.

        If no binding produces a helper the letrec is returned as-is (with
        walked binding values and body).
        """
        hoisted_helpers: List[Tuple[str, MenaiIRExpr]] = []
        new_bindings: List[Tuple[str, MenaiIRExpr]] = []

        for name, value_plan, *_ in ir.bindings:
            if isinstance(value_plan, MenaiIRLambda):
                helper, wrapper = self._lift_lambda_parts(value_plan)
                if helper is not None:
                    # Capturing lambda: hoist helper, keep wrapper in letrec.
                    assert helper.binding_name is not None
                    hoisted_helpers.append((helper.binding_name, helper))
                    new_bindings.append((name, wrapper))

                else:
                    # Already-closed lambda: keep as-is (wrapper is the closed lambda).
                    new_bindings.append((name, wrapper))

            else:
                new_bindings.append((name, self._walk(value_plan)))

        new_body = self._walk(ir.body_plan)

        new_letrec = MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

        if not hoisted_helpers:
            return new_letrec

        # Wrap the letrec in a let that binds all helpers first.
        return MenaiIRLet(
            bindings=hoisted_helpers,
            body_plan=new_letrec,
            in_tail_position=False,
        )

    # ------------------------------------------------------------------
    # Lambda lifting
    # ------------------------------------------------------------------

    def _lift_lambda(self, ir: MenaiIRLambda) -> MenaiIRExpr:
        """
        Lift a capturing lambda into a (let (helper) wrapper) pair.

        Delegates to _lift_lambda_parts for the core split, then wraps the
        result in a let for the standalone (non-letrec) case.

        If the lambda is already closed, returns the closed lambda unchanged.
        """
        helper, wrapper = self._lift_lambda_parts(ir)
        if helper is None:
            # Already closed — wrapper is the unchanged closed lambda.
            return wrapper

        # Standalone case: wrap helper and wrapper in a local let.
        assert helper.binding_name is not None
        return MenaiIRLet(
            bindings=[(helper.binding_name, helper)],
            body_plan=wrapper,
            in_tail_position=False,
        )

    def _lift_lambda_parts(
        self, ir: MenaiIRLambda
    ) -> Tuple[Optional[MenaiIRLambda], MenaiIRExpr]:
        """
        Core of the worker/wrapper split.

        Returns (helper, wrapper) when the lambda captures anything, or
        (None, closed_lambda) when it is already fully closed.

        The helper is always a MenaiIRLambda with no free vars.
        The wrapper is a MenaiIRLambda with the original arity that tail-calls
        the helper.

        Called by both _lift_lambda (standalone case) and _walk_letrec (letrec
        group case).  Recurses into free_var_plans and the body before
        performing the split so that nested capturing lambdas are lifted first.
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
            # Already closed — return (None, closed_lambda).
            return None, MenaiIRLambda(
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
            MenaiIRVariable(name=n, var_type='local', is_parent_ref=False)
            for n in list(ir.params) + captured_names
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

        return helper_lambda, wrapper_lambda

    def _next_lifted_name(self, binding_name: str | None) -> str:
        """Generate a unique name for a lifted helper lambda."""
        n = self._lift_counter
        self._lift_counter += 1
        if binding_name:
            return f"<lifted-{n}-{binding_name}>"
        return f"<lifted-{n}>"
