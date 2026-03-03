"""
Menai IR Devirtualizer — static wrapper call-site inlining.

After lambda lifting every capturing lambda has been split into a fully-closed
helper and a thin wrapper.  The wrapper exists so that the original function
can still be used as a first-class value (passed to map-list, returned, etc.).
At call sites where the wrapper is called *directly by name*, however, the
wrapper indirection is pure overhead: we pay for MAKE_CLOSURE, a closure
object, and a CALL/TAIL_CALL dispatch just to immediately turn around and
TAIL_CALL the helper.

This pass eliminates that overhead by rewriting every static wrapper call site
to call the helper directly, passing the original arguments followed by the
wrapper's captured values (the free-var plans).

Transformation
--------------
A static wrapper call site looks like:

    (call <wrapper-var> arg0 .. argN)

where <wrapper-var> resolves to a let-binding whose value is a
MenaiIRLambda with is_wrapper=True, params [p0..pN], and:

    outer_free_var_plans  = [<helper-var>, fv0-plan, .., fvM-plan]
    sibling_free_var_plans = [s0-plan, .., sK-plan]

It is rewritten to:

    (call <helper-var> arg0 .. argN s0-plan .. sK-plan fv0-plan .. fvM-plan)

The tail-call-ness of the original call site is preserved.  Note that
<helper-var> is always outer_free_var_plans[0] — the helper is always the
first entry in the wrapper's outer_free_vars (as produced by
MenaiIRLambdaLifter._lift_lambda_parts).

Variadic wrappers
-----------------
Wrappers for variadic lambdas (is_variadic=True) are skipped.  Inlining a
variadic call site requires reasoning about rest-argument packing and is left
for a future pass.

Scope of the pass
-----------------
This is a one-shot pass, not a fixed-point pass.  It runs once immediately
after lambda lifting and before the IR optimisation loop.  It never creates
new wrapper call sites, so a second run would find nothing to do.

Dead wrapper bindings left behind (use count now zero) are cleaned up by
MenaiIROptimizer in the first iteration of the subsequent fixed-point loop.

Variables remain symbolic
-------------------------
All MenaiIRVariable nodes emitted or forwarded by this pass carry depth=-1,
index=-1, exactly as every other pre-addresser pass.
"""

from __future__ import annotations

from typing import Dict, List, Tuple

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


class MenaiIRDevirtualizer:
    """
    One-shot pass: inline all static wrapper call sites.

    Walks the IR tree, maintaining a map from wrapper variable names to their
    MenaiIRLambda nodes.  At every MenaiIRCall whose func_plan is a local
    variable that resolves to a non-variadic wrapper, the call is rewritten to
    call the helper directly.

    Usage::

        new_ir = MenaiIRDevirtualizer().devirtualize(ir)
    """

    def __init__(self) -> None:
        self._devirtualizations: int = 0

    def devirtualize(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """Walk *ir* and return a new tree with all static wrapper calls inlined."""
        self._devirtualizations = 0
        return self._walk(ir, wrapper_map={})

    def devirtualizations(self) -> int:
        """Return the number of call sites rewritten by the most recent devirtualize() call."""
        return self._devirtualizations

    # ------------------------------------------------------------------
    # Tree walk
    # ------------------------------------------------------------------

    def _walk(
        self,
        ir: MenaiIRExpr,
        wrapper_map: Dict[str, MenaiIRLambda],
    ) -> MenaiIRExpr:
        """Recursively walk *ir*, rewriting static wrapper call sites."""

        if isinstance(ir, MenaiIRLet):
            return self._walk_let(ir, wrapper_map)

        if isinstance(ir, MenaiIRLetrec):
            return self._walk_letrec(ir, wrapper_map)

        if isinstance(ir, MenaiIRCall):
            return self._walk_call(ir, wrapper_map)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._walk(ir.condition_plan, wrapper_map),
                then_plan=self._walk(ir.then_plan, wrapper_map),
                else_plan=self._walk(ir.else_plan, wrapper_map),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRLambda):
            return self._walk_lambda(ir, wrapper_map)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._walk(ir.value_plan, wrapper_map))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._walk(m, wrapper_map) for m in ir.message_plans],
                value_plan=self._walk(ir.value_plan, wrapper_map),
            )

        if isinstance(ir, (MenaiIRVariable, MenaiIRConstant, MenaiIRQuote,
                           MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRDevirtualizer: unhandled IR node type {type(ir).__name__}"
        )

    def _walk_let(
        self,
        ir: MenaiIRLet,
        wrapper_map: Dict[str, MenaiIRLambda],
    ) -> MenaiIRExpr:
        """
        Walk a let node.

        Binding values are walked first (they may contain wrapper call sites).
        Any binding whose value is a non-variadic wrapper lambda is added to
        the wrapper_map for the body scope.
        """
        new_bindings: List[Tuple[str, MenaiIRExpr]] = []
        body_wrapper_map = dict(wrapper_map)

        for name, value_plan, *_ in ir.bindings:
            new_value = self._walk(value_plan, wrapper_map)
            new_bindings.append((name, new_value))

            # Register non-variadic wrappers so call sites in the body can be
            # devirtualized.
            if (isinstance(new_value, MenaiIRLambda)
                    and new_value.is_wrapper
                    and not new_value.is_variadic):
                body_wrapper_map[name] = new_value

        new_body = self._walk(ir.body_plan, body_wrapper_map)

        return MenaiIRLet(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_letrec(
        self,
        ir: MenaiIRLetrec,
        wrapper_map: Dict[str, MenaiIRLambda],
    ) -> MenaiIRExpr:
        """
        Walk a letrec node.

        After lambda lifting, wrappers in a letrec are still wrappers — they
        tail-call their (let-bound) helper.  They are registered in the
        wrapper_map so that call sites in sibling binding values and the body
        can be devirtualized.

        Note: the helpers themselves are in the enclosing let (hoisted by the
        lifter), so they are already in wrapper_map when we get here.
        """
        # First pass: register all wrapper bindings in the map so they are
        # visible to sibling binding values and the body (mutual recursion).
        inner_wrapper_map = dict(wrapper_map)
        for name, value_plan, *_ in ir.bindings:
            if (isinstance(value_plan, MenaiIRLambda)
                    and value_plan.is_wrapper
                    and not value_plan.is_variadic):
                inner_wrapper_map[name] = value_plan

        new_bindings: List[Tuple[str, MenaiIRExpr]] = []
        for name, value_plan, *_ in ir.bindings:
            new_bindings.append((name, self._walk(value_plan, inner_wrapper_map)))

        new_body = self._walk(ir.body_plan, inner_wrapper_map)

        return MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_lambda(
        self,
        ir: MenaiIRLambda,
        wrapper_map: Dict[str, MenaiIRLambda],
    ) -> MenaiIRLambda:
        """
        Walk a lambda node.

        Wrappers visible in the enclosing scope are still devirtualizable inside
        the lambda body — UNLESS a param or free var shadows the name.  Inside
        the lambda, params and free vars are plain local values, not wrapper
        bindings, so any shadowed name must be removed from the map before
        descending into the body.

        free_var_plans are evaluated in the enclosing frame and are walked with
        the current wrapper_map.
        """
        # Remove any wrapper_map entry whose name is shadowed by a param or
        # free var of this lambda.  Inside the body those names are plain
        # locals, not wrapper closures.
        shadow = set(ir.params) | set(ir.sibling_free_vars) | set(ir.outer_free_vars)
        body_wrapper_map = {k: v for k, v in wrapper_map.items() if k not in shadow}

        new_sibling_fvp = [self._walk(p, wrapper_map) for p in ir.sibling_free_var_plans]
        new_outer_fvp = [self._walk(p, wrapper_map) for p in ir.outer_free_var_plans]
        new_body = self._walk(ir.body_plan, body_wrapper_map)

        return MenaiIRLambda(
            params=ir.params,
            body_plan=new_body,
            sibling_free_vars=ir.sibling_free_vars,
            sibling_free_var_plans=new_sibling_fvp,
            outer_free_vars=ir.outer_free_vars,
            outer_free_var_plans=new_outer_fvp,
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            source_line=ir.source_line,
            source_file=ir.source_file,
            is_wrapper=ir.is_wrapper,
            lifted_helper_name=ir.lifted_helper_name,
        )

    def _walk_call(
        self,
        ir: MenaiIRCall,
        wrapper_map: Dict[str, MenaiIRLambda],
    ) -> MenaiIRExpr:
        """
        Walk a call node, devirtualizing if the callee is a known wrapper.

        A call is devirtualizable when:
          - func_plan is a MenaiIRVariable with var_type='local'
          - that name is in wrapper_map (resolves to a non-variadic wrapper)
          - argument count matches the wrapper's param_count
          - the call is not a builtin call
        """
        # Always walk arguments first.
        new_args = [self._walk(a, wrapper_map) for a in ir.arg_plans]
        new_func = self._walk(ir.func_plan, wrapper_map)

        if (not ir.is_builtin
                and isinstance(ir.func_plan, MenaiIRVariable)
                and ir.func_plan.var_type == 'local'
                and ir.func_plan.name in wrapper_map):

            wrapper = wrapper_map[ir.func_plan.name]

            if len(new_args) == wrapper.param_count:
                return self._devirtualize_call(ir, wrapper, new_args)

        return MenaiIRCall(
            func_plan=new_func,
            arg_plans=new_args,
            is_tail_call=ir.is_tail_call,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )

    # ------------------------------------------------------------------
    # Devirtualization
    # ------------------------------------------------------------------

    def _devirtualize_call(
        self,
        ir: MenaiIRCall,
        wrapper: MenaiIRLambda,
        new_args: List[MenaiIRExpr],
    ) -> MenaiIRCall:
        """
        Rewrite a static wrapper call to call the helper directly.

        The wrapper's outer_free_var_plans[0] is always the helper variable
        (as produced by MenaiIRLambdaLifter).  The remaining outer_free_var_plans
        and all sibling_free_var_plans are the captured values that the helper
        expects as its extra parameters.

        New call args: original_args + sibling_fv_plans + outer_fv_plans[1:]
        New callee:    outer_free_var_plans[0]  (the helper variable)
        """
        assert wrapper.outer_free_var_plans, (
            "Devirtualizer: wrapper has no outer_free_var_plans — "
            "expected at least the helper variable"
        )

        helper_var = wrapper.outer_free_var_plans[0]
        extra_args = list(wrapper.sibling_free_var_plans) + list(wrapper.outer_free_var_plans[1:])

        self._devirtualizations += 1

        return MenaiIRCall(
            func_plan=helper_var,
            arg_plans=new_args + extra_args,
            is_tail_call=ir.is_tail_call,
            is_builtin=False,
            builtin_name=None,
        )
