"""
Menai IR Copy Propagator - copy propagation optimization pass over the IR tree.

Consumes an IRUseCounts annotation (produced by MenaiIRUseCounter) and applies
copy propagation to MenaiIRLet bindings whose right-hand side is *trivially
copyable* — meaning it is safe and profitable to substitute the value directly
at every use site rather than storing it in a local slot.

What is trivially copyable?
---------------------------
A value plan is trivially copyable when duplicating it at every use site is
both *semantically correct* and *not more expensive* than the original
store-then-load sequence.  In a pure language like Menai, correctness is
never a concern (no side effects, no aliasing), so the question reduces to
cost.  The following node types qualify:

  MenaiIRConstant   — a compile-time constant; zero-cost to duplicate.
  MenaiIREmptyList  — the empty list singleton; zero-cost to duplicate.
  MenaiIRQuote      — a quoted literal; zero-cost to duplicate.
  MenaiIRVariable(var_type='global')
                    — a global/builtin name lookup; always immutable and
                      O(1) to load, so inlining is never worse than a
                      STORE_VAR + LOAD_VAR pair.
  MenaiIRVariable(var_type='local', depth=0, is_parent_ref=False)
                    — a local variable load in the current frame; O(1) and
                      cheaper than the store+load pair.  Safe to inline
                      provided no use site crosses a lambda boundary (see
                      the lambda boundary rule below).

Lambda boundary rule
--------------------
When a value plan contains a MenaiIRVariable with var_type='local' and
depth=0, the index is frame-relative.  If we substitute that plan at a use
site *inside a child lambda*, the depth would need to be incremented — but
we do not rewrite depths here.  Therefore:

  - MenaiIRConstant, MenaiIREmptyList, MenaiIRQuote: always safe to inline
    regardless of lambda boundaries (they contain no frame-relative variable
    references).
  - MenaiIRVariable(var_type='global'): always safe (globals use name-table
    lookup, not frame-relative addressing).
  - MenaiIRVariable(var_type='local', depth=0): safe ONLY when
    external_count(frame_id, var_index) == 0, i.e. the binding is never
    captured by any child lambda.  If it is captured, we leave it alone.

Scope of the pass: let only, not letrec
----------------------------------------
Copy propagation is applied only to MenaiIRLet bindings.  MenaiIRLetrec
bindings are skipped because:
  - They may be mutually recursive (inlining could create cycles).
  - Their values are almost always lambdas, which are not trivially copyable.
  - The dead-binding eliminator already handles the main letrec case.

Substitution walk
-----------------
For a qualifying binding (name, value_plan, var_index) in a MenaiIRLet:

  1. Walk the let's body_plan and replace every occurrence of
     MenaiIRVariable(var_type='local', depth=0, index=var_index)
     with a fresh copy of value_plan.
  2. Drop the binding from the let's binding list (it now has zero uses).
  3. If all bindings are dropped, collapse the let to its body.

The walk descends into all IR nodes in the current frame.  When it
encounters a MenaiIRLambda it does NOT descend into the lambda's body_plan
(that is a child frame where depth=0 refers to the lambda's own locals).
However, it DOES substitute in the lambda's free_var_plans and
parent_ref_plans, because those are evaluated in the enclosing frame.

The walk also skips the sentinel func_plan of tail-recursive calls
(is_tail_recursive=True), exactly as MenaiIROptimizer does.

Interaction with MenaiIROptimizer
----------------------------------
After copy propagation, the eliminated bindings have zero uses.  The
existing dead-binding eliminator will clean them up on the next pass.
Because the pass manager runs all passes to fixed point, the two passes
compose naturally without any special coordination.

Implements MenaiIROptimizationPass so it can be managed by the IR pass
manager in MenaiCompiler.
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


class MenaiIRCopyPropagator(MenaiIROptimizationPass):
    """
    IR-level copy propagation pass.

    Implements MenaiIROptimizationPass: call optimize(ir) to get back a
    transformed IR tree and a boolean indicating whether any changes were made.
    Use counts are computed internally so callers do not need to manage them.

    The propagator is stateless between calls — all mutable state (the frame
    stack) is passed explicitly through the recursive walk, and the use-count
    annotation is recomputed fresh on every call to optimize().

    Usage::

        new_ir, changed = MenaiIRCopyPropagator().optimize(ir)
    """

    def __init__(self) -> None:
        self._substitutions: int = 0
        self._counts: Optional[IRUseCounts] = None

    @property
    def substitutions(self) -> int:
        """Number of bindings copy-propagated during the last optimize() call."""
        return self._substitutions

    def optimize(self, ir: MenaiIRExpr) -> Tuple[MenaiIRExpr, bool]:
        """
        Return a copy-propagated copy of *ir* and a flag indicating whether
        any changes were made.

        Use counts are computed internally before the transformation pass.

        Args:
            ir: Root IR node to optimize (output of MenaiIRBuilder.build() or
                a previous optimization pass).

        Returns:
            Tuple of (new_ir, changed).  changed is True if at least one
            binding was copy-propagated.
        """
        self._substitutions = 0
        self._counts = MenaiIRUseCounter().count(ir)
        new_ir = self._prop(ir, frame_stack=[0])
        return new_ir, self._substitutions > 0

    # ------------------------------------------------------------------
    # Main recursive walk
    # ------------------------------------------------------------------

    def _prop(self, ir: MenaiIRExpr, frame_stack: List[int]) -> MenaiIRExpr:
        """Recursively copy-propagate *ir* in the context of *frame_stack*."""
        if isinstance(ir, MenaiIRLet):
            return self._prop_let(ir, frame_stack)

        if isinstance(ir, MenaiIRLetrec):
            return self._prop_letrec(ir, frame_stack)

        if isinstance(ir, MenaiIRIf):
            return self._prop_if(ir, frame_stack)

        if isinstance(ir, MenaiIRLambda):
            return self._prop_lambda(ir, frame_stack)

        if isinstance(ir, MenaiIRCall):
            return self._prop_call(ir, frame_stack)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._prop(ir.value_plan, frame_stack))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._prop(m, frame_stack) for m in ir.message_plans],
                value_plan=self._prop(ir.value_plan, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRVariable,
                            MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            # Leaf nodes — nothing to propagate into.
            return ir

        raise TypeError(
            f"MenaiIRCopyPropagator: unhandled IR node type {type(ir).__name__}"
        )

    # ------------------------------------------------------------------
    # Let: the core of copy propagation
    # ------------------------------------------------------------------

    def _prop_let(self, ir: MenaiIRLet, frame_stack: List[int]) -> MenaiIRExpr:
        """
        Optimize a let node by copy-propagating trivially copyable bindings.

        For each binding (name, value_plan, var_index):
          - If value_plan is trivially copyable AND can be safely inlined
            (see _can_inline), substitute value_plan for every occurrence of
            var_index in the body_plan, then drop the binding.
          - Otherwise, keep the binding and recursively optimize its value.

        If all bindings are propagated away, collapse the let to its body.
        """
        current_frame = frame_stack[-1]
        counts = cast(IRUseCounts, self._counts)

        # Collect which bindings to propagate: {var_index: value_plan}.
        # We process bindings in order so that later bindings in the same let
        # can benefit from earlier propagations (though in a parallel let the
        # binding values cannot reference each other, so this only matters for
        # the body).
        to_propagate: Dict[int, MenaiIRExpr] = {}

        for name, value_plan, var_index in ir.bindings:
            if self._is_trivially_copyable(value_plan):
                if self._can_inline(value_plan, counts, current_frame, var_index):
                    to_propagate[var_index] = value_plan

        # Build the new binding list.  For bindings we are NOT propagating,
        # we still recursively optimize their value plans (they may contain
        # inner lets that benefit from propagation).
        live: List[Tuple[str, MenaiIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            if var_index in to_propagate:
                # This binding will be substituted away — drop it.
                self._substitutions += 1
                continue
            live.append((name, self._prop(value_plan, frame_stack), var_index))

        # Substitute all propagated bindings into the body.  We apply them
        # all in a single pass of the body tree.
        opt_body = self._prop(ir.body_plan, frame_stack)
        if to_propagate:
            opt_body = self._substitute(opt_body, to_propagate, frame_stack)

        if not live:
            # All bindings were propagated — the let form itself is gone.
            return opt_body

        return MenaiIRLet(
            bindings=live,
            body_plan=opt_body,
            in_tail_position=ir.in_tail_position,
        )

    # ------------------------------------------------------------------
    # Letrec: descend but do not propagate
    # ------------------------------------------------------------------

    def _prop_letrec(self, ir: MenaiIRLetrec, frame_stack: List[int]) -> MenaiIRExpr:
        """
        Optimize a letrec node.

        Copy propagation is NOT applied to letrec bindings (see module
        docstring for rationale).  We still recurse into binding value plans
        and the body so that inner let nodes are optimized.
        """
        live: List[Tuple[str, MenaiIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            live.append((name, self._prop(value_plan, frame_stack), var_index))

        opt_body = self._prop(ir.body_plan, frame_stack)

        return MenaiIRLetrec(
            bindings=live,
            body_plan=opt_body,
            binding_groups=ir.binding_groups,
            recursive_bindings=ir.recursive_bindings,
            in_tail_position=ir.in_tail_position,
        )

    # ------------------------------------------------------------------
    # Structural recursion for non-let nodes
    # ------------------------------------------------------------------

    def _prop_if(self, ir: MenaiIRIf, frame_stack: List[int]) -> MenaiIRIf:
        return MenaiIRIf(
            condition_plan=self._prop(ir.condition_plan, frame_stack),
            then_plan=self._prop(ir.then_plan, frame_stack),
            else_plan=self._prop(ir.else_plan, frame_stack),
            in_tail_position=ir.in_tail_position,
        )

    def _prop_lambda(self, ir: MenaiIRLambda, frame_stack: List[int]) -> MenaiIRLambda:
        """
        Optimize a lambda node.

        The lambda body is optimized in the lambda's own frame (looked up from
        the lambda_frame_ids map populated by the use counter).

        free_var_plans and parent_ref_plans are evaluated in the *enclosing*
        frame, so they are walked with the current frame_stack.  However,
        these are always MenaiIRVariable leaf nodes — the use counter already
        counted them as uses in the enclosing frame — so there is nothing to
        substitute into them here (substitution happens in _substitute, which
        is called from _prop_let on the body of the let, not on the lambda
        node itself).  We still pass them through _prop for completeness and
        future-proofing, but since they are leaf MenaiIRVariable nodes _prop
        will return them unchanged.
        """
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        if lambda_frame_id is None:
            # Defensive: counter didn't visit this node.  Optimize body in
            # the current frame (safe but conservative).
            child_stack = frame_stack
        else:
            child_stack = frame_stack + [lambda_frame_id]

        return MenaiIRLambda(
            params=ir.params,
            body_plan=self._prop(ir.body_plan, child_stack),
            free_vars=ir.free_vars,
            free_var_plans=ir.free_var_plans,   # leaf nodes; no substitution needed
            parent_refs=ir.parent_refs,
            parent_ref_plans=ir.parent_ref_plans,  # leaf nodes; no substitution needed
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            sibling_bindings=ir.sibling_bindings,
            max_locals=ir.max_locals,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _prop_call(self, ir: MenaiIRCall, frame_stack: List[int]) -> MenaiIRCall:
        opt_args = [self._prop(a, frame_stack) for a in ir.arg_plans]

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
            func_plan=self._prop(ir.func_plan, frame_stack),
            arg_plans=opt_args,
            is_tail_call=ir.is_tail_call,
            is_tail_recursive=ir.is_tail_recursive,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )

    # ------------------------------------------------------------------
    # Substitution walk
    # ------------------------------------------------------------------

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

        Args:
            ir:           IR node to walk.
            replacements: Map from local slot index to the value plan to
                          substitute at every use site.
            frame_stack:  Current frame stack (used only to recurse into
                          inner let/letrec nodes via _prop, which handles
                          their own substitution context).

        Returns:
            A new IR node with all matching variable references replaced.
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
            f"MenaiIRCopyPropagator._substitute: unhandled IR node type "
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

        In a parallel let, binding value expressions are evaluated in the
        *outer* scope (they cannot reference the let's own bindings), so we
        substitute into them.  The body is also in the same frame, so we
        substitute there too.

        However, the let's own bindings introduce new local slots.  If any of
        those slots shadow a slot in *replacements* (same index), we must NOT
        substitute the outer replacement at uses of the inner binding inside
        the body.  We remove such shadowed indices from the replacements map
        before descending into the body.

        After substitution we also run _prop on the result so that any newly
        trivially-copyable bindings exposed by the substitution are propagated
        in the same pass.
        """
        # Collect the var_indices introduced by this inner let.
        inner_indices = {var_index for _, _, var_index in ir.bindings}

        # Substitute into binding value expressions (outer scope — no shadowing
        # concern because binding values cannot reference the let's own slots).
        new_bindings: List[Tuple[str, MenaiIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            new_value = self._substitute(value_plan, replacements, frame_stack)
            new_bindings.append((name, new_value, var_index))

        # Build the replacements map for the body, removing any slots that are
        # shadowed by this inner let's own bindings.
        body_replacements = {
            k: v for k, v in replacements.items() if k not in inner_indices
        }

        new_body = self._substitute(ir.body_plan, body_replacements, frame_stack)

        new_let = MenaiIRLet(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

        # Run _prop on the reconstructed let so that any copy-propagation
        # opportunities within it are taken in this same pass.
        return self._prop(new_let, frame_stack)

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

        # Remove shadowed slots from replacements for both values and body.
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

        # Run _prop on the reconstructed letrec so that inner lets are
        # optimized in this same pass.
        return self._prop(new_letrec, frame_stack)

    def _substitute_lambda(
        self,
        ir: MenaiIRLambda,
        replacements: Dict[int, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRLambda:
        """
        Substitute into a lambda node encountered during the substitution walk.

        The lambda's body_plan is in a child frame — depth=0 there refers to
        the lambda's own locals, not the enclosing let's slots — so we do NOT
        descend into it for substitution.

        The lambda's free_var_plans and parent_ref_plans ARE evaluated in the
        enclosing frame, so we substitute into them.  In practice these are
        always MenaiIRVariable leaf nodes, so _substitute will either replace
        them (if their index is in replacements) or return them unchanged.

        After substituting into free_var_plans / parent_ref_plans we run
        _prop_lambda on the result so that the lambda body is still optimized
        by the main propagation walk.
        """
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        if lambda_frame_id is None:
            child_stack = frame_stack
        else:
            child_stack = frame_stack + [lambda_frame_id]

        # Substitute into free_var_plans (evaluated in enclosing frame).
        new_free_var_plans = [
            self._substitute(fvp, replacements, frame_stack)
            for fvp in ir.free_var_plans
        ]

        # Substitute into parent_ref_plans (evaluated in enclosing frame).
        new_parent_ref_plans = [
            self._substitute(prp, replacements, frame_stack)
            for prp in ir.parent_ref_plans
        ]

        # The body is in the child frame — propagate there but do NOT
        # substitute the enclosing frame's replacements.
        new_body = self._prop(ir.body_plan, child_stack)

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
    # Inlineability predicates
    # ------------------------------------------------------------------

    @staticmethod
    def _is_trivially_copyable(value_plan: MenaiIRExpr) -> bool:
        """
        Return True if *value_plan* is a candidate for copy propagation.

        A plan is trivially copyable when duplicating it at every use site
        costs no more than the original STORE_VAR + LOAD_VAR sequence.

        Trivially copyable node types:
          - MenaiIRConstant   (compile-time constant)
          - MenaiIREmptyList  (empty list singleton)
          - MenaiIRQuote      (quoted literal)
          - MenaiIRVariable   (variable load — either global or local depth=0)

        Lambdas, calls, if-expressions, and all other compound nodes are NOT
        trivially copyable because duplicating them would duplicate work.
        """
        return isinstance(
            value_plan,
            (MenaiIRConstant, MenaiIREmptyList, MenaiIRQuote, MenaiIRVariable),
        )

    def _can_inline(
        self,
        value_plan: MenaiIRExpr,
        counts: IRUseCounts,
        frame_id: int,
        var_index: int,
    ) -> bool:
        """
        Return True if *value_plan* can be safely inlined at every use site
        of *var_index* in *frame_id*.

        For node types that contain no frame-relative variable references
        (MenaiIRConstant, MenaiIREmptyList, MenaiIRQuote, and
        MenaiIRVariable with var_type='global'), inlining is always safe
        regardless of whether the binding is captured by a child lambda.

        For MenaiIRVariable with var_type='local' and depth=0, inlining is
        safe only when external_count(frame_id, var_index) == 0 — i.e. the
        binding is never captured by any child lambda.  If it is captured,
        the depth of the substituted variable reference would need to be
        incremented inside the lambda body, which we do not do here.

        Args:
            value_plan: The trivially copyable value plan to potentially inline.
            counts:     Use counts for the current IR tree.
            frame_id:   Frame ID of the let that owns the binding.
            var_index:  Local slot index of the binding being considered.

        Returns:
            True if the binding can be safely copy-propagated.
        """
        if isinstance(value_plan, (MenaiIRConstant, MenaiIREmptyList, MenaiIRQuote)):
            # No frame-relative variable references — always safe.
            return True

        if isinstance(value_plan, MenaiIRVariable):
            if value_plan.var_type == 'global':
                # Global name-table lookup — always safe.
                return True

            if (value_plan.var_type == 'local'
                    and value_plan.depth == 0
                    and not value_plan.is_parent_ref):
                # Local variable load — safe only if not captured by any child
                # lambda (external_count == 0).
                return counts.external_count(frame_id, var_index) == 0

        # Any other case (shouldn't be reached if _is_trivially_copyable is
        # called first, but be defensive).
        return False
