"""
Menai IR Inline-Once Pass - single-use let binding inliner.

Consumes an IRUseCounts annotation (produced by MenaiIRUseCounter) and inlines
any MenaiIRLet binding that has exactly one total use, subject to the lambda
boundary rule described below.

Why this is different from MenaiIRCopyPropagator
-------------------------------------------------
Copy propagation inlines *trivially copyable* values (constants, variable
loads, etc.) at any use count, because duplicating a cheap value costs nothing.
This pass instead targets bindings with *any* value — including calls,
if-expressions, and other compound nodes — but only when the use count is
exactly 1.  With a single use and no work duplication, inlining is always
profitable in a pure language.

The two passes compose naturally:
  - Copy propagation may reduce multi-use bindings to single-use (by
    eliminating uses inside value expressions of other propagated bindings).
  - Inline-once then eliminates the remaining single-use bindings.
  - The dead-binding eliminator cleans up zero-use slots left by both.

Lambda boundary rule
--------------------
Substituting a value plan at a use site inside a child lambda is only
problematic when the value plan itself contains frame-relative variable
references that would need their depth adjusted.  The rule therefore depends
on the *type* of the value plan, not just whether the binding is captured:

  MenaiIRVariable(var_type='local', depth=0):
      Requires external_count(frame_id, var_index) == 0.
      A depth=0 local reference is frame-relative.  Substituting it inside
      a child lambda would produce a reference with the wrong depth.

  All other value plan types (MenaiIRCall, MenaiIRIf, MenaiIRConstant,
  MenaiIRQuote, MenaiIREmptyList, MenaiIRVariable(global), etc.):
      external_count is irrelevant.  These node types contain no
      frame-relative addresses, so they can be safely substituted anywhere
      in the tree — including inside lambda bodies and free_var_plans.

Motivating example for the permissive case:

    (let ((result (integer+ x 1)))
      (lambda () result))

Here result has total_count=1 and external_count=1 (captured).  The brief's
original uniform rule would block inlining.  The corrected rule allows it:
(integer+ x 1) is a MenaiIRCall with no frame-relative addresses, so it is
safe to substitute directly into the lambda's free_var_plans.  This eliminates
the STORE_VAR/LOAD_VAR pair and the closure captures the computed value
directly rather than a slot reference.

Scope of the pass: let only, not letrec
----------------------------------------
Inline-once is applied only to MenaiIRLet bindings.  MenaiIRLetrec bindings
are skipped for the same reasons as in copy propagation:
  - They may be mutually recursive.
  - Their values are almost always lambdas, which may be called many times.
  - The dead-binding eliminator handles the main letrec dead-code case.

The pass still recurses into letrec bodies to optimize inner let nodes.

Interaction with MenaiIROptimizer
----------------------------------
After inlining, the eliminated bindings have zero uses.  The existing
dead-binding eliminator will clean them up on the next pass.  The pass manager
runs all passes to fixed point, so multi-step chains are fully resolved across
iterations.

Implements MenaiIROptimizationPass so it can be managed by the IR pass manager
in MenaiCompiler.
"""

from typing import Dict, List, Optional, Tuple, cast

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
from menai.menai_ir_optimization_pass import MenaiIROptimizationPass
from menai.menai_ir_use_counter import IRUseCounts, MenaiIRUseCounter


class MenaiIRInlineOnce(MenaiIROptimizationPass):
    """
    IR-level single-use let binding inliner.

    Implements MenaiIROptimizationPass: call optimize(ir) to get back a
    transformed IR tree and a boolean indicating whether any changes were made.
    Use counts are computed internally so callers do not need to manage them.

    The pass is stateless between calls — all mutable state (the frame stack)
    is passed explicitly through the recursive walk, and the use-count
    annotation is recomputed fresh on every call to optimize().

    Usage::

        new_ir, changed = MenaiIRInlineOnce().optimize(ir)
    """

    def __init__(self) -> None:
        self._inlinings: int = 0
        self._counts: Optional[IRUseCounts] = None

    @property
    def inlinings(self) -> int:
        """Number of bindings inlined during the last optimize() call."""
        return self._inlinings

    def optimize(self, ir: MenaiIRExpr) -> Tuple[MenaiIRExpr, bool]:
        """
        Return an inline-once-optimized copy of *ir* and a flag indicating
        whether any changes were made.

        Use counts are computed internally before the transformation pass.

        Args:
            ir: Root IR node to optimize (output of MenaiIRBuilder.build() or
                a previous optimization pass).

        Returns:
            Tuple of (new_ir, changed).  changed is True if at least one
            binding was inlined.
        """
        self._inlinings = 0
        self._counts = MenaiIRUseCounter().count(ir)
        new_ir = self._inline(ir, frame_stack=[0])
        return new_ir, self._inlinings > 0

    def _inline(self, ir: MenaiIRExpr, frame_stack: List[int]) -> MenaiIRExpr:
        """Recursively inline single-use bindings in *ir*."""
        if isinstance(ir, MenaiIRLet):
            return self._inline_let(ir, frame_stack)

        if isinstance(ir, MenaiIRLetrec):
            return self._inline_letrec(ir, frame_stack)

        if isinstance(ir, MenaiIRIf):
            return self._inline_if(ir, frame_stack)

        if isinstance(ir, MenaiIRLambda):
            return self._inline_lambda(ir, frame_stack)

        if isinstance(ir, MenaiIRCall):
            return self._inline_call(ir, frame_stack)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._inline(ir.value_plan, frame_stack))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._inline(m, frame_stack) for m in ir.message_plans],
                value_plan=self._inline(ir.value_plan, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRVariable,
                            MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            # Leaf nodes — nothing to inline into.
            return ir

        raise TypeError(
            f"MenaiIRInlineOnce: unhandled IR node type {type(ir).__name__}"
        )

    def _inline_let(self, ir: MenaiIRLet, frame_stack: List[int]) -> MenaiIRExpr:
        """
        Optimize a let node by inlining single-use bindings.

        For each binding (name, value_plan, var_index):
          - If total_count == 1 and _can_inline() passes, substitute value_plan
            at the single use site in body_plan and drop the binding.
          - Otherwise, keep the binding and recursively optimize its value.

        If all bindings are inlined away, collapse the let to its body.

        Note: binding values are evaluated in the outer scope (parallel let
        semantics), so we never substitute one binding's value into another
        binding's value expression within the same let — only into body_plan.
        """
        current_frame = frame_stack[-1]
        counts = cast(IRUseCounts, self._counts)

        # Determine which bindings to inline: {var_index: value_plan}.
        to_inline: Dict[int, MenaiIRExpr] = {}
        for name, value_plan, var_index in ir.bindings:
            if counts.total_count(current_frame, var_index) == 1:
                if self._can_inline(value_plan, counts, current_frame, var_index):
                    to_inline[var_index] = value_plan

        # Build the new binding list.  For bindings we are NOT inlining,
        # recursively optimize their value plans (they may contain inner lets).
        live: List[Tuple[str, MenaiIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            if var_index in to_inline:
                # This binding will be substituted away — drop it.
                self._inlinings += 1
                continue
            live.append((name, self._inline(value_plan, frame_stack), var_index))

        # Recursively optimize the body, then substitute inlined bindings.
        opt_body = self._inline(ir.body_plan, frame_stack)
        if to_inline:
            opt_body = self._substitute(opt_body, to_inline, frame_stack)

        if not live:
            # All bindings were inlined — the let form itself is gone.
            return opt_body

        return MenaiIRLet(
            bindings=live,
            body_plan=opt_body,
            in_tail_position=ir.in_tail_position,
        )

    def _inline_letrec(self, ir: MenaiIRLetrec, frame_stack: List[int]) -> MenaiIRExpr:
        """
        Optimize a letrec node.

        Inline-once is NOT applied to letrec bindings (see module docstring).
        We still recurse into binding value plans and the body so that inner
        let nodes are optimized.
        """
        live: List[Tuple[str, MenaiIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            live.append((name, self._inline(value_plan, frame_stack), var_index))

        opt_body = self._inline(ir.body_plan, frame_stack)

        return MenaiIRLetrec(
            bindings=live,
            body_plan=opt_body,
            binding_groups=ir.binding_groups,
            recursive_bindings=ir.recursive_bindings,
            in_tail_position=ir.in_tail_position,
        )

    def _inline_if(self, ir: MenaiIRIf, frame_stack: List[int]) -> MenaiIRIf:
        return MenaiIRIf(
            condition_plan=self._inline(ir.condition_plan, frame_stack),
            then_plan=self._inline(ir.then_plan, frame_stack),
            else_plan=self._inline(ir.else_plan, frame_stack),
            in_tail_position=ir.in_tail_position,
        )

    def _inline_lambda(
        self, ir: MenaiIRLambda, frame_stack: List[int]
    ) -> MenaiIRLambda:
        """
        Optimize a lambda node.

        The lambda body is optimized in the lambda's own frame.
        free_var_plans and parent_ref_plans are evaluated in the enclosing
        frame and are walked with the current frame_stack.
        """
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        if lambda_frame_id is None:
            # Defensive: counter didn't visit this node.
            child_stack = frame_stack

        else:
            child_stack = frame_stack + [lambda_frame_id]

        return MenaiIRLambda(
            params=ir.params,
            body_plan=self._inline(ir.body_plan, child_stack),
            free_vars=ir.free_vars,
            free_var_plans=ir.free_var_plans,   # leaf nodes; no substitution needed here
            parent_refs=ir.parent_refs,
            parent_ref_plans=ir.parent_ref_plans,  # leaf nodes; no substitution needed here
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            sibling_bindings=ir.sibling_bindings,
            max_locals=ir.max_locals,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _inline_call(self, ir: MenaiIRCall, frame_stack: List[int]) -> MenaiIRCall:
        opt_args = [self._inline(a, frame_stack) for a in ir.arg_plans]

        if ir.is_tail_recursive:
            # func_plan is a sentinel ('<tail-recursive>') — do not optimize it.
            return MenaiIRCall(
                func_plan=ir.func_plan,
                arg_plans=opt_args,
                is_tail_call=ir.is_tail_call,
                is_tail_recursive=ir.is_tail_recursive,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        return MenaiIRCall(
            func_plan=self._inline(ir.func_plan, frame_stack),
            arg_plans=opt_args,
            is_tail_call=ir.is_tail_call,
            is_tail_recursive=ir.is_tail_recursive,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )

    def _substitute(
        self,
        ir: MenaiIRExpr,
        replacements: Dict[int, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        """
        Walk *ir* and replace every MenaiIRVariable(var_type='local',
        depth=0, index=k) with replacements[k], for each k in *replacements*.

        This walk operates strictly within the current frame.  When it
        encounters a MenaiIRLambda it does NOT descend into the lambda's
        body_plan (that is a child frame where depth=0 refers to the lambda's
        own locals, not the enclosing let's slots).  It DOES substitute in
        the lambda's free_var_plans and parent_ref_plans, because those are
        evaluated in the enclosing frame.

        The tail-recursive sentinel func_plan is never substituted.
        """
        if isinstance(ir, MenaiIRVariable):
            if (ir.var_type == 'local'
                    and ir.depth == 0
                    and not ir.is_parent_ref
                    and ir.index in replacements):
                return replacements[ir.index]

            return ir

        if isinstance(ir, MenaiIRLet):
            return self._substitute_let(ir, replacements, frame_stack)

        if isinstance(ir, MenaiIRLetrec):
            return self._substitute_letrec(ir, replacements, frame_stack)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._substitute(ir.condition_plan, replacements, frame_stack),
                then_plan=self._substitute(ir.then_plan, replacements, frame_stack),
                else_plan=self._substitute(ir.else_plan, replacements, frame_stack),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRLambda):
            return self._substitute_lambda(ir, replacements, frame_stack)

        if isinstance(ir, MenaiIRCall):
            return self._substitute_call(ir, replacements, frame_stack)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(
                value_plan=self._substitute(ir.value_plan, replacements, frame_stack)
            )

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[
                    self._substitute(m, replacements, frame_stack)
                    for m in ir.message_plans
                ],
                value_plan=self._substitute(ir.value_plan, replacements, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            # Leaf nodes with no variable references — nothing to substitute.
            return ir

        raise TypeError(
            f"MenaiIRInlineOnce._substitute: unhandled IR node type "
            f"{type(ir).__name__}"
        )

    def _substitute_let(
        self,
        ir: MenaiIRLet,
        replacements: Dict[int, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        """
        Substitute into a let node encountered during the substitution walk.

        Binding values are in the outer scope (parallel let semantics), so we
        substitute into them.  The let's own bindings shadow any outer slots
        with the same index inside the body — we remove those from the
        replacements map before descending into body_plan.

        After substitution we also run _inline on the result so that any
        newly eligible single-use bindings are inlined in the same pass.
        """
        inner_indices = {var_index for _, _, var_index in ir.bindings}

        # Substitute into binding value expressions (outer scope — no shadowing).
        new_bindings: List[Tuple[str, MenaiIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            new_value = self._substitute(value_plan, replacements, frame_stack)
            new_bindings.append((name, new_value, var_index))

        # Remove shadowed slots for the body.
        body_replacements = {
            k: v for k, v in replacements.items() if k not in inner_indices
        }
        new_body = self._substitute(ir.body_plan, body_replacements, frame_stack)

        new_let = MenaiIRLet(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

        # Run _inline on the reconstructed let so that opportunities exposed
        # by substitution are taken in this same pass.
        return self._inline(new_let, frame_stack)

    def _substitute_letrec(
        self,
        ir: MenaiIRLetrec,
        replacements: Dict[int, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        """
        Substitute into a letrec node encountered during the substitution walk.

        letrec bindings are in scope for their own value expressions (unlike
        let), so the inner indices shadow the outer replacements for both
        binding values and the body.
        """
        inner_indices = {var_index for _, _, var_index in ir.bindings}
        inner_replacements = {
            k: v for k, v in replacements.items() if k not in inner_indices
        }

        new_bindings: List[Tuple[str, MenaiIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            new_value = self._substitute(value_plan, inner_replacements, frame_stack)
            new_bindings.append((name, new_value, var_index))

        new_body = self._substitute(ir.body_plan, inner_replacements, frame_stack)

        new_letrec = MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=new_body,
            binding_groups=ir.binding_groups,
            recursive_bindings=ir.recursive_bindings,
            in_tail_position=ir.in_tail_position,
        )

        # Run _inline on the reconstructed letrec so that inner lets are
        # optimized in this same pass.
        return self._inline(new_letrec, frame_stack)

    def _substitute_lambda(
        self,
        ir: MenaiIRLambda,
        replacements: Dict[int, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRLambda:
        """
        Substitute into a lambda node encountered during the substitution walk.

        The lambda's body_plan is in a child frame — depth=0 there refers to
        the lambda's own locals — so we do NOT descend into it for substitution.

        The lambda's free_var_plans and parent_ref_plans ARE evaluated in the
        enclosing frame, so we substitute into them.  This is the key path
        that allows inlining captured single-use call bindings (the motivating
        example in the module docstring).

        After substituting into free_var_plans / parent_ref_plans we run
        _inline_lambda on the result so that the lambda body is still optimized.
        """
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        if lambda_frame_id is None:
            child_stack = frame_stack

        else:
            child_stack = frame_stack + [lambda_frame_id]

        new_free_var_plans = [
            self._substitute(fvp, replacements, frame_stack)
            for fvp in ir.free_var_plans
        ]
        new_parent_ref_plans = [
            self._substitute(prp, replacements, frame_stack)
            for prp in ir.parent_ref_plans
        ]

        # Body is in the child frame — optimize but do not substitute the
        # enclosing frame's replacements.
        new_body = self._inline(ir.body_plan, child_stack)

        return MenaiIRLambda(
            params=ir.params,
            body_plan=new_body,
            free_vars=ir.free_vars,
            free_var_plans=new_free_var_plans,
            parent_refs=ir.parent_refs,
            parent_ref_plans=new_parent_ref_plans,
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            sibling_bindings=ir.sibling_bindings,
            max_locals=ir.max_locals,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _substitute_call(
        self,
        ir: MenaiIRCall,
        replacements: Dict[int, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRCall:
        """Substitute into a call node, skipping the tail-recursive sentinel."""
        opt_args = [
            self._substitute(a, replacements, frame_stack) for a in ir.arg_plans
        ]

        if ir.is_tail_recursive:
            # func_plan is a sentinel — never substitute into it.
            return MenaiIRCall(
                func_plan=ir.func_plan,
                arg_plans=opt_args,
                is_tail_call=ir.is_tail_call,
                is_tail_recursive=ir.is_tail_recursive,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        return MenaiIRCall(
            func_plan=self._substitute(ir.func_plan, replacements, frame_stack),
            arg_plans=opt_args,
            is_tail_call=ir.is_tail_call,
            is_tail_recursive=ir.is_tail_recursive,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )

    # ------------------------------------------------------------------
    # Eligibility check
    # ------------------------------------------------------------------

    @staticmethod
    def _can_inline(
        value_plan: MenaiIRExpr,
        counts: IRUseCounts,
        frame_id: int,
        var_index: int,
    ) -> bool:
        """
        Return True if *value_plan* can be safely inlined at its single use
        site of *var_index* in *frame_id*.

        The check depends on whether value_plan is a depth=0 local variable
        reference:

          MenaiIRVariable(var_type='local', depth=0):
              Requires external_count == 0.  A depth=0 local reference is
              frame-relative; substituting it inside a child lambda would
              produce a reference with the wrong depth.

          All other value plan types:
              external_count is irrelevant.  These types contain no
              frame-relative addresses and can be substituted anywhere in the
              tree, including inside lambda bodies and free_var_plans.

        Note: total_count == 1 is checked by the caller (_inline_let) before
        calling this method.  Dead bindings (total_count == 0) are left for
        MenaiIROptimizer.
        """
        if isinstance(value_plan, MenaiIRVariable):
            if (value_plan.var_type == 'local'
                    and value_plan.depth == 0
                    and not value_plan.is_parent_ref):
                # Frame-relative reference — safe only when not captured.
                return counts.external_count(frame_id, var_index) == 0

            # Global variable or parent-ref: no frame-relative address concern.
            return True

        # All other node types (constants, calls, if-exprs, quotes, lambdas,
        # empty lists, errors, etc.) contain no frame-relative addresses.
        return True
