"""
Menai IR Parent-Ref Classifier.

This pass walks a fully-constructed (but not yet addressed) IR tree and
performs two related tasks:

1. Re-classifies each MenaiIRLambda's free variables into two lists:

   free_vars / free_var_plans
       Variables captured at closure-creation time via MAKE_CLOSURE.
       These are values from the enclosing scope that the lambda needs but
       that are NOT recursive back-edges to a letrec binding.

   parent_refs / parent_ref_plans
       Recursive back-edges.  The lambda references a name that is bound by
       an enclosing MenaiIRLetrec anywhere in its lexical ancestry.  At
       runtime these use LOAD_PARENT_VAR to walk up the parent-frame chain
       rather than being captured into a closure slot.

2. Sets is_parent_ref correctly on every MenaiIRVariable node inside
   lambda bodies.  The IR builder sets is_parent_ref=False on all body
   variables; this pass corrects that for variables whose name is a
   parent_ref of the enclosing lambda.

Classification rule
-------------------
A free variable name is a parent_ref if and only if it appears in the set
of names bound by any MenaiIRLetrec that is an ancestor of the lambda in
the IR tree.  Everything else is a captured free_var.

This replaces the IR builder's inline classification, which used three
context fields (current_binding_name, sibling_bindings, parent_ref_names)
to track the same information.  The IR tree already encodes the letrec
structure, so we can derive the classification by walking it directly.

Why parent_refs cross lambda boundaries
----------------------------------------
Consider:

    (letrec ((f (lambda (n)
                  (map-list (lambda (x) (f x))
                            (list n)))))
      (f 5))

The inner lambda (lambda (x) (f x)) references f.  f is bound by the
letrec that is an ancestor of both lambdas.  The inner lambda should use
LOAD_PARENT_VAR depth=2 to reach f directly in the letrec frame, rather
than capturing it from the outer lambda's closure.  This avoids an extra
level of indirection and is the behaviour the VM expects.

Therefore letrec_bound — the set of names bound by enclosing letrecs —
accumulates across all lambda boundaries and is never reset.

Pipeline position
-----------------
    MenaiIRBuilder               (emits unresolved IR; all free vars in free_vars,
                                  is_parent_ref=False everywhere)
        ↓
    MenaiIRParentRefClassifier   ← THIS PASS
        ↓
    MenaiIRAddresser             (resolves depth/index)
        ↓
    IR optimization passes
        ↓
    MenaiCodeGen

Usage
-----
    classifier = MenaiIRParentRefClassifier()
    new_ir = classifier.classify(ir)
"""

from __future__ import annotations

from typing import FrozenSet, List, Tuple

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


class MenaiIRParentRefClassifier:
    """
    Re-classifies free_vars / parent_refs on every MenaiIRLambda, and fixes
    is_parent_ref on all body MenaiIRVariable nodes.

    The pass is stateless between calls.  All state is passed explicitly
    through the recursive walk.

    Usage::

        new_ir = MenaiIRParentRefClassifier().classify(ir)
    """

    def classify(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """
        Walk *ir* and return a new tree with corrected free_vars / parent_refs
        on every MenaiIRLambda, and correct is_parent_ref on all body variables.

        Args:
            ir: IR tree produced by MenaiIRBuilder.

        Returns:
            New IR tree with all classifications corrected.
        """
        # letrec_bound: names bound by any enclosing MenaiIRLetrec.
        # parent_ref_names: names that are parent_refs in the current lambda
        #                   (empty at top level — no enclosing lambda yet).
        return self._walk(ir, letrec_bound=frozenset(), parent_ref_names=frozenset())

    # ------------------------------------------------------------------
    # Main recursive walk
    # ------------------------------------------------------------------

    def _walk(
        self,
        ir: MenaiIRExpr,
        letrec_bound: FrozenSet[str],
        parent_ref_names: FrozenSet[str],
    ) -> MenaiIRExpr:
        """
        Recursively walk *ir*.

        letrec_bound  — names bound by any enclosing MenaiIRLetrec (never reset).
        parent_ref_names — parent_refs of the immediately enclosing lambda
                           (used to fix is_parent_ref on body variables).
        """
        if isinstance(ir, MenaiIRVariable):
            return self._fix_body_variable(ir, parent_ref_names)

        if isinstance(ir, MenaiIRLambda):
            return self._walk_lambda(ir, letrec_bound)

        if isinstance(ir, MenaiIRLetrec):
            return self._walk_letrec(ir, letrec_bound, parent_ref_names)

        if isinstance(ir, MenaiIRLet):
            return self._walk_let(ir, letrec_bound, parent_ref_names)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._walk(ir.condition_plan, letrec_bound, parent_ref_names),
                then_plan=self._walk(ir.then_plan, letrec_bound, parent_ref_names),
                else_plan=self._walk(ir.else_plan, letrec_bound, parent_ref_names),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRCall):
            return self._walk_call(ir, letrec_bound, parent_ref_names)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(
                value_plan=self._walk(ir.value_plan, letrec_bound, parent_ref_names)
            )

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[
                    self._walk(m, letrec_bound, parent_ref_names)
                    for m in ir.message_plans
                ],
                value_plan=self._walk(ir.value_plan, letrec_bound, parent_ref_names),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRParentRefClassifier: unhandled IR node type "
            f"{type(ir).__name__}"
        )

    def _fix_body_variable(
        self, ir: MenaiIRVariable, parent_ref_names: FrozenSet[str]
    ) -> MenaiIRVariable:
        """
        Return a copy of *ir* with is_parent_ref set correctly.

        A body variable is a parent_ref if its name is in parent_ref_names
        (the parent_refs of the immediately enclosing lambda).
        """
        should_be = ir.name in parent_ref_names
        if ir.is_parent_ref == should_be:
            return ir
        return MenaiIRVariable(
            name=ir.name,
            var_type=ir.var_type,
            depth=ir.depth,
            index=ir.index,
            is_parent_ref=should_be,
        )

    def _walk_letrec(
        self,
        ir: MenaiIRLetrec,
        letrec_bound: FrozenSet[str],
        parent_ref_names: FrozenSet[str],
    ) -> MenaiIRLetrec:
        """
        Walk a letrec node.

        All binding names are added to letrec_bound before descending into
        the binding values and body.  This means any lambda inside the
        letrec (at any nesting depth) will see these names as parent_refs.
        """
        new_bound = letrec_bound | frozenset(name for name, _, _ in ir.bindings)

        new_bindings: List[Tuple[str, MenaiIRExpr, int]] = [
            (name, self._walk(value_plan, new_bound, parent_ref_names), var_index)
            for name, value_plan, var_index in ir.bindings
        ]
        new_body = self._walk(ir.body_plan, new_bound, parent_ref_names)

        return MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_let(
        self,
        ir: MenaiIRLet,
        letrec_bound: FrozenSet[str],
        parent_ref_names: FrozenSet[str],
    ) -> MenaiIRLet:
        """
        Walk a let node.

        Let bindings are not recursive, so they do not contribute to
        letrec_bound.  We simply recurse with the unchanged state.
        """
        new_bindings: List[Tuple[str, MenaiIRExpr, int]] = [
            (name, self._walk(value_plan, letrec_bound, parent_ref_names), var_index)
            for name, value_plan, var_index in ir.bindings
        ]
        new_body = self._walk(ir.body_plan, letrec_bound, parent_ref_names)

        return MenaiIRLet(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_lambda(
        self,
        ir: MenaiIRLambda,
        letrec_bound: FrozenSet[str],
    ) -> MenaiIRLambda:
        """
        Walk a lambda node.

        1. Reclassify free_vars / parent_refs based on letrec_bound.
        2. Recurse into the body with the new parent_ref_names so that
           body MenaiIRVariable nodes get is_parent_ref set correctly.
        3. letrec_bound is NOT reset — it accumulates across lambda boundaries.
        """
        # Combine all free names (IR builder puts everything in free_vars).
        all_free: List[str] = list(ir.free_vars) + list(ir.parent_refs)

        # Build name -> plan map from whatever the IR builder produced.
        original_plans: dict[str, MenaiIRVariable] = {}
        for name, plan in zip(ir.free_vars, ir.free_var_plans):
            assert isinstance(plan, MenaiIRVariable)
            original_plans[name] = plan
        for name, plan in zip(ir.parent_refs, ir.parent_ref_plans):
            assert isinstance(plan, MenaiIRVariable)
            original_plans[name] = plan

        # Reclassify.
        new_free_vars: List[str] = []
        new_free_var_plans: List[MenaiIRExpr] = []
        new_parent_refs: List[str] = []
        new_parent_ref_plans: List[MenaiIRExpr] = []

        for name in all_free:
            plan = original_plans[name]
            if name in letrec_bound:
                new_parent_refs.append(name)
                new_parent_ref_plans.append(MenaiIRVariable(
                    name=plan.name,
                    var_type=plan.var_type,
                    is_parent_ref=True,
                ))
            else:
                new_free_vars.append(name)
                new_free_var_plans.append(MenaiIRVariable(
                    name=plan.name,
                    var_type=plan.var_type,
                    is_parent_ref=False,
                ))

        # The body variables that are parent_refs are exactly new_parent_refs.
        body_parent_ref_names: FrozenSet[str] = frozenset(new_parent_refs)

        # Recompute max_locals.
        # The IR builder set max_locals based on the original slot layout:
        #   params (N slots) + captured free_vars (M slots) + any nested lets
        # Names that moved from free_vars to parent_refs no longer occupy local
        # slots.  Each such move reduces max_locals by 1.
        # Names already in ir.parent_refs were already not in the slot count,
        # so we only count newly-promoted names (those not in ir.parent_refs).
        original_parent_ref_set = frozenset(ir.parent_refs)
        newly_promoted = sum(
            1 for name in new_parent_refs if name not in original_parent_ref_set
        )
        new_max_locals = ir.max_locals - newly_promoted

        # Recurse into the body with the updated parent_ref_names.
        # letrec_bound is passed through unchanged.
        new_body = self._walk(ir.body_plan, letrec_bound, body_parent_ref_names)

        return MenaiIRLambda(
            params=ir.params,
            body_plan=new_body,
            free_vars=new_free_vars,
            free_var_plans=new_free_var_plans,
            parent_refs=new_parent_refs,
            parent_ref_plans=new_parent_ref_plans,
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            sibling_bindings=ir.sibling_bindings,
            max_locals=new_max_locals,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _walk_call(
        self,
        ir: MenaiIRCall,
        letrec_bound: FrozenSet[str],
        parent_ref_names: FrozenSet[str],
    ) -> MenaiIRCall:
        """Walk a call node."""
        new_args = [
            self._walk(a, letrec_bound, parent_ref_names) for a in ir.arg_plans
        ]

        return MenaiIRCall(
            func_plan=self._walk(ir.func_plan, letrec_bound, parent_ref_names),
            arg_plans=new_args,
            is_tail_call=ir.is_tail_call,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )
