"""
Menai IR Lambda Lifter — Step 5.

This pass implements the A′ (worker/wrapper) lambda lifting transformation.
It walks the IR tree and replaces every MenaiIRLambda that has free variables
or parent references with:

    (let ((helper (lambda (p0..pN fv0..fvM pr0..prK) <original body>)))
      (lambda (p0..pN)
        <tail-calls helper with p0..pN + captured fv0..fvM + pr0..prK>))

Terminology
-----------
- *helper*  — the lifted, fully-closed function.  Takes original params plus
              all captured values (former free_vars and former parent_refs) as
              explicit parameters.  free_vars=[], parent_refs=[].
- *wrapper* — thin lambda with the original arity.  Captures the helper plus
              all formerly-captured values via MAKE_CLOSURE, then tail-calls
              the helper passing everything through.  Correct for first-class
              uses (map-list, fold-list, return values, etc.).

After this pass every MenaiIRLambda in the tree has free_vars==[] and
parent_refs==[].  LOAD_PARENT_VAR is never emitted.

Lambdas that are already closed (free_vars==[], parent_refs==[]) are left
structurally unchanged.

Why no tail-call demotion is needed
------------------------------------
The previous attempt at this pass needed to demote is_tail_recursive calls
inside helper bodies to prevent JUMP 0 from being emitted with the wrong
arity.  That problem no longer exists: is_tail_recursive was removed from
MenaiIRCall in the Step A refactor.  JUMP 0 detection now happens in the
codegen by matching the callee name against ctx.current_lambda_name.  The
helper has a distinct binding_name ("<lifted-N-foo>") that will never match
the original function name, so the codegen naturally emits TAIL_CALL (not
JUMP 0) for any self-call inside the helper.  No special handling required.

parent_refs → free_vars on the wrapper
----------------------------------------
Former parent_refs become regular free_vars on the wrapper.  Their
free_var_plans are the original parent_ref_plans with is_parent_ref=False.
Because free_var_plans are evaluated in the *enclosing* frame (before
MAKE_CLOSURE executes), and the letrec-bound names are live ordinary local
slots in that frame at that point, the addresser resolves them as plain
LOAD_VAR — no LOAD_PARENT_VAR needed anywhere.

Body variable reset
--------------------
After the first addresser run the body's MenaiIRVariable nodes carry
resolved depth/index values for the *original* lambda's slot layout.
When those variables become parameters of the helper (which has a different
slot layout), those addresses are stale.  Before placing the body in the
helper, _reset_vars() resets every local MenaiIRVariable to depth=-1,
index=-1, is_parent_ref=False so the second addresser run re-resolves them
against the helper's new scope dict.

_reset_vars() does NOT descend into nested MenaiIRLambda nodes — their
bodies have their own scope and their own addresser pass.  It only resets
variables at the current lambda's scope level (depth=0 references) and
cross-frame references (depth>0) that are no longer valid after lifting.
In practice it resets ALL local variables at every depth, because after
lifting the only correct addresses are those the second addresser assigns.

var_index sentinel
------------------
The let binding for the helper is emitted with var_index=-1.  The second
MenaiIRAddresser run recognises this sentinel and allocates the next free
slot in the current lambda frame, avoiding the need for the lifter to know
the enclosing frame's slot layout.

Slot layout inside the wrapper frame after ENTER + MAKE_CLOSURE:
    0 .. N-1     original params          (ENTER)
    N            helper function          (MAKE_CLOSURE slot 0)
    N+1 .. N+M   captured values          (MAKE_CLOSURE slots 1..M)

where N = original param_count, M = len(free_vars) + len(parent_refs).

Pipeline position
-----------------
    MenaiIRAddresser             (first run)
        ↓
    MenaiIRClosureConverter      (Step 4 — identity pass)
        ↓
    MenaiIRLambdaLifter          ← THIS PASS
        ↓
    MenaiIRAddresser             (second run — fixes all depth/index after lifting)
        ↓
    IR optimization passes
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
    Implements the A′ (worker/wrapper) lambda lifting transformation.

    Every MenaiIRLambda with non-empty free_vars or parent_refs is replaced
    by a let-bound helper (fully closed) and a thin wrapper (original arity,
    captures everything via MAKE_CLOSURE, tail-calls the helper).

    The pass is stateless between calls except for the counter used to
    generate unique helper names.

    Usage::

        new_ir = MenaiIRLambdaLifter().lift(ir)
    """

    def __init__(self) -> None:
        self._lift_counter: int = 0

    def lift(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """
        Walk *ir* and return a new tree with all capturing lambdas lifted.

        Args:
            ir: IR tree after the first MenaiIRAddresser run (variables
                resolved, free_vars / parent_refs correctly classified).

        Returns:
            New IR tree where every MenaiIRLambda has free_vars==[] and
            parent_refs==[].  New variable references emitted by the lifter
            have depth=-1, index=-1; the second addresser run resolves them.
        """
        return self._walk(ir)

    # ------------------------------------------------------------------
    # Main recursive walk
    # ------------------------------------------------------------------

    def _walk(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """Recursively walk and rewrite *ir*."""

        if isinstance(ir, MenaiIRLambda):
            return self._lift_lambda(ir)

        if isinstance(ir, MenaiIRLet):
            return MenaiIRLet(
                bindings=[
                    (name, self._walk(value_plan), var_index)
                    for name, value_plan, var_index in ir.bindings
                ],
                body_plan=self._walk(ir.body_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRLetrec):
            return MenaiIRLetrec(
                bindings=[
                    (name, self._walk(value_plan), var_index)
                    for name, value_plan, var_index in ir.bindings
                ],
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

        If the lambda is already closed (free_vars==[]), only recurse into
        the body and return a structurally identical node.
        """
        # Always recurse into free_var_plans — evaluated in the enclosing
        # frame and may contain nested lambdas.
        new_free_var_plans: List[MenaiIRExpr] = [
            self._walk(p) for p in ir.free_var_plans
        ]

        # Recurse into the body — lifts any nested capturing lambdas.
        new_body = self._walk(ir.body_plan)

        if not ir.free_vars:
            # Already closed — return structurally unchanged.
            return MenaiIRLambda(
                params=ir.params,
                body_plan=new_body,
                free_vars=[],
                free_var_plans=[],
                param_count=ir.param_count,
                is_variadic=ir.is_variadic,
                binding_name=ir.binding_name,
                sibling_bindings=ir.sibling_bindings,
                max_locals=ir.max_locals,
                source_line=ir.source_line,
                source_file=ir.source_file,
            )

        # ------------------------------------------------------------------
        # Build all_captured: (name, free_var_plan) for every value the
        # original lambda needed from outside its own frame.
        # Letrec siblings are now regular free_vars (no parent_refs).
        # ------------------------------------------------------------------
        all_captured: List[Tuple[str, MenaiIRExpr]] = []

        for name, plan in zip(ir.free_vars, new_free_var_plans):
            all_captured.append((name, plan))

        captured_names: List[str] = [name for name, _ in all_captured]
        captured_plans: List[MenaiIRExpr] = [plan for _, plan in all_captured]
        n_captured = len(all_captured)
        orig_param_count = ir.param_count

        # ------------------------------------------------------------------
        # Helper lambda
        # ------------------------------------------------------------------
        # params       = original params + all captured names
        # is_variadic  = False — the helper always receives a plain (already-packed)
        #                list for the rest parameter.  For a non-variadic original
        #                lambda this makes no difference.  For a variadic original
        #                lambda the wrapper packs the rest args via ENTER (is_variadic
        #                stays True on the wrapper) and passes the resulting list as
        #                an ordinary argument to the helper.  Making the helper
        #                non-variadic prevents _check_and_pack_args from re-packing
        #                the captured slots into a new rest list.
        # body         = original body with ALL local variable references reset to
        #                unresolved sentinels (depth=-1, index=-1, is_parent_ref=False).
        #                The second addresser run re-resolves them against the helper's
        #                new scope dict (params 0..N-1, captured N..N+M-1).
        #
        # JUMP 0 suppression is automatic: the helper's binding_name is
        # "<lifted-N-foo>", which never matches the original function name
        # stored in ctx.current_lambda_name, so the codegen emits TAIL_CALL
        # (not JUMP 0) for any tail call in the helper body.
        helper_params = list(ir.params) + captured_names
        helper_param_count = len(helper_params)
        helper_name = self._next_lifted_name(ir.binding_name)

        helper_lambda = MenaiIRLambda(
            params=helper_params,
            body_plan=self._reset_vars(new_body),
            free_vars=[],
            free_var_plans=[],
            param_count=helper_param_count,
            is_variadic=False,   # helper always takes a plain list; see comment above
            binding_name=helper_name,
            sibling_bindings=[],
            max_locals=max(ir.max_locals, helper_param_count),
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

        # ------------------------------------------------------------------
        # Wrapper lambda
        # ------------------------------------------------------------------
        # params    = original params (arity unchanged — correct for
        #             first-class uses: map-list, fold-list, apply, etc.)
        # free_vars = [helper] + captured_names
        # Body: tail-call helper(p0..pN-1, cap0..capM-1).
        # All variable references use depth=-1, index=-1 (unresolved);
        # the second addresser run fills them in from the wrapper's scope dict.
        #
        # Slot layout inside the wrapper frame:
        #   0 .. N-1    original params  (ENTER)
        #   N           helper           (MAKE_CLOSURE slot 0)
        #   N+1 .. N+M  captured values  (MAKE_CLOSURE slots 1..M)

        wrapper_free_vars = [helper_name] + captured_names
        wrapper_free_var_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(
                name=helper_name,
                var_type='local',
                depth=-1,
                index=-1,
                is_parent_ref=False,
            )
        ] + captured_plans

        # Arguments passed to the helper: original params then captured names.
        wrapper_arg_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(
                name=pname,
                var_type='local',
                depth=-1,
                index=-1,
                is_parent_ref=False,
            )
            for pname in ir.params
        ] + [
            MenaiIRVariable(
                name=cname,
                var_type='local',
                depth=-1,
                index=-1,
                is_parent_ref=False,
            )
            for cname in captured_names
        ]

        wrapper_lambda = MenaiIRLambda(
            params=ir.params,
            body_plan=MenaiIRReturn(
                value_plan=MenaiIRCall(
                    func_plan=MenaiIRVariable(
                        name=helper_name,
                        var_type='local',
                        depth=-1,
                        index=-1,
                        is_parent_ref=False,
                    ),
                    arg_plans=wrapper_arg_plans,
                    is_tail_call=True,
                    is_builtin=False,
                    builtin_name=None,
                )
            ),
            free_vars=wrapper_free_vars,
            free_var_plans=wrapper_free_var_plans,
            param_count=orig_param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            sibling_bindings=[],
            # N params + 1 helper slot + M captured slots
            max_locals=orig_param_count + 1 + n_captured,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

        # ------------------------------------------------------------------
        # Wrap in a let that binds the helper.
        # var_index=-1 sentinel: the second addresser run allocates the next
        # free slot in the current lambda frame for this binding.
        # ------------------------------------------------------------------
        return MenaiIRLet(
            bindings=[(helper_name, helper_lambda, -1)],
            body_plan=wrapper_lambda,
            in_tail_position=False,
        )

    # ------------------------------------------------------------------
    # Variable reset
    # ------------------------------------------------------------------

    def _reset_vars(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """
        Reset all local MenaiIRVariable nodes to unresolved sentinels.

        Every MenaiIRVariable with var_type='local' gets:
            depth=-1, index=-1, is_parent_ref=False

        This is applied to the helper body before the second addresser run,
        because the body's variables were resolved against the *original*
        lambda's slot layout (which is now stale — the helper has extra
        params).  The second addresser run then resolves them correctly
        against the helper's new scope dict.

        Does NOT descend into nested MenaiIRLambda bodies — those have their
        own scope and will be re-addressed correctly by the second addresser
        run when it recurses into them.  Does recurse into free_var_plans of
        nested lambdas (evaluated in the enclosing frame).

        Global variables are left unchanged (they carry no frame-relative
        address).
        """
        if isinstance(ir, MenaiIRVariable):
            if ir.var_type == 'local':
                return MenaiIRVariable(
                    name=ir.name,
                    var_type='local',
                    depth=-1,
                    index=-1,
                    is_parent_ref=False,
                )
            return ir

        if isinstance(ir, MenaiIRLambda):
            # Do NOT reset variables inside the nested lambda's body — that
            # body has its own scope.  Only reset the free_var_plans and
            # free_var_plans, which are evaluated in the enclosing frame
            # (the helper's frame) and must be re-addressed there.
            return MenaiIRLambda(
                params=ir.params,
                body_plan=ir.body_plan,  # body left as-is; addresser handles it
                free_vars=ir.free_vars,
                free_var_plans=[self._reset_vars(p) for p in ir.free_var_plans],
                param_count=ir.param_count,
                is_variadic=ir.is_variadic,
                binding_name=ir.binding_name,
                sibling_bindings=ir.sibling_bindings,
                max_locals=ir.max_locals,
                source_line=ir.source_line,
                source_file=ir.source_file,
            )

        if isinstance(ir, MenaiIRLet):
            return MenaiIRLet(
                bindings=[
                    (name, self._reset_vars(value_plan), var_index)
                    for name, value_plan, var_index in ir.bindings
                ],
                body_plan=self._reset_vars(ir.body_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRLetrec):
            return MenaiIRLetrec(
                bindings=[
                    (name, self._reset_vars(value_plan), var_index)
                    for name, value_plan, var_index in ir.bindings
                ],
                body_plan=self._reset_vars(ir.body_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._reset_vars(ir.condition_plan),
                then_plan=self._reset_vars(ir.then_plan),
                else_plan=self._reset_vars(ir.else_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRCall):
            return MenaiIRCall(
                func_plan=self._reset_vars(ir.func_plan),
                arg_plans=[self._reset_vars(a) for a in ir.arg_plans],
                is_tail_call=ir.is_tail_call,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._reset_vars(ir.value_plan))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._reset_vars(m) for m in ir.message_plans],
                value_plan=self._reset_vars(ir.value_plan),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRQuote,
                           MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRLambdaLifter._reset_vars: unhandled IR node type "
            f"{type(ir).__name__}"
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
