"""
Menai IR Copy Propagator - copy propagation optimization pass over the IR tree.

Consumes an IRUseCounts annotation (produced by MenaiIRUseCounter) and applies
copy propagation to MenaiIRLet bindings whose right-hand side is *trivially
copyable* — meaning it is safe and profitable to substitute the value directly
at every use site rather than storing it in a local slot.

What is trivially copyable?
---------------------------
A value plan is trivially copyable when duplicating it at every use site is
both semantically correct and not more expensive than the original
store-then-load sequence.  In a pure language like Menai, correctness is never
a concern (no side effects, no aliasing), so the question reduces to cost:

  MenaiIRConstant   — a compile-time constant; zero-cost to duplicate.
  MenaiIREmptyList  — the empty list singleton; zero-cost to duplicate.
  MenaiIRQuote      — a quoted literal; zero-cost to duplicate.
  MenaiIRVariable(var_type='global')
                    — a global/builtin name lookup; always immutable and O(1).
  MenaiIRVariable(var_type='local')
                    — a local variable reference; O(1) to load.

Lambda boundary rule
--------------------
There is none.  Variables are symbolic throughout the optimisation pipeline
(depth=-1, index=-1 until MenaiIRAddresser runs).  Substituting a name
reference at any position in the tree — including inside a child lambda body
or free_var_plans — is always safe: the addresser will resolve the name
correctly in its new position.

Scope of the pass: let only, not letrec
----------------------------------------
Copy propagation is applied only to MenaiIRLet bindings.  MenaiIRLetrec
bindings are skipped because they may be mutually recursive and their values
are almost always lambdas (not trivially copyable).

Substitution walk
-----------------
For a qualifying binding (name, value_plan) in a MenaiIRLet:
  1. Walk the let's body_plan and replace every MenaiIRVariable(name=n,
     var_type='local') with a fresh copy of value_plan.
  2. Drop the binding from the let's binding list.
  3. If all bindings are dropped, collapse the let to its body.

Shadowing: when the walk descends into an inner let or letrec that binds the
same name, that name is removed from the substitution map for the inner body
so that the inner binding is not incorrectly replaced.

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


class MenaiIRCopyPropagator(MenaiIROptimizationPass):
    """
    IR-level copy propagation pass.

    Usage::

        new_ir, changed = MenaiIRCopyPropagator().optimize(ir)
    """

    def __init__(self) -> None:
        self._substitutions: int = 0
        self._counts: Optional[IRUseCounts] = None

    @property
    def substitutions(self) -> int:
        return self._substitutions

    def optimize(self, ir: MenaiIRExpr) -> Tuple[MenaiIRExpr, bool]:
        self._substitutions = 0
        self._counts = MenaiIRUseCounter().count(ir)
        new_ir = self._prop(ir, frame_stack=[0])
        return new_ir, self._substitutions > 0

    def _prop(self, ir: MenaiIRExpr, frame_stack: List[int]) -> MenaiIRExpr:
        """Recursively copy-propagate ir in the context of frame_stack."""
        if isinstance(ir, MenaiIRLet):
            return self._prop_let(ir, frame_stack)

        if isinstance(ir, MenaiIRLetrec):
            return self._prop_letrec(ir, frame_stack)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._prop(ir.condition_plan, frame_stack),
                then_plan=self._prop(ir.then_plan, frame_stack),
                else_plan=self._prop(ir.else_plan, frame_stack),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRLambda):
            return self._prop_lambda(ir, frame_stack)

        if isinstance(ir, MenaiIRCall):
            return MenaiIRCall(
                func_plan=self._prop(ir.func_plan, frame_stack),
                arg_plans=[self._prop(a, frame_stack) for a in ir.arg_plans],
                is_tail_call=ir.is_tail_call,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._prop(ir.value_plan, frame_stack))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._prop(m, frame_stack) for m in ir.message_plans],
                value_plan=self._prop(ir.value_plan, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRVariable, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRCopyPropagator: unhandled IR node type {type(ir).__name__}"
        )

    def _prop_let(self, ir: MenaiIRLet, frame_stack: List[int]) -> MenaiIRExpr:
        """
        Propagate trivially copyable bindings.

        For each binding (name, value_plan):
          - If value_plan is trivially copyable, substitute it at every use of
            name in body_plan and drop the binding.
          - Otherwise keep the binding and recursively optimise its value.
        """
        to_propagate: Dict[str, MenaiIRExpr] = {}  # name → value (for substitution)
        to_propagate_ids: set = set()               # id(binding) for bindings being propagated
        for binding in ir.bindings:
            name, value_plan, *_ = binding
            if self._is_trivially_copyable(value_plan):
                to_propagate[name] = value_plan
                to_propagate_ids.add(id(binding))


        live: List[Tuple[str, MenaiIRExpr]] = []
        for binding in ir.bindings:
            name, value_plan, *_ = binding
            if id(binding) in to_propagate_ids:
                self._substitutions += 1
                continue

            live.append((name, self._prop(value_plan, frame_stack)))

        opt_body = self._prop(ir.body_plan, frame_stack)
        if to_propagate:
            opt_body = self._substitute(opt_body, to_propagate, frame_stack)

        if not live:
            return opt_body

        return MenaiIRLet(
            bindings=live,
            body_plan=opt_body,
            in_tail_position=ir.in_tail_position,
        )

    def _prop_letrec(self, ir: MenaiIRLetrec, frame_stack: List[int]) -> MenaiIRExpr:
        """Recurse into letrec without propagating its bindings."""
        live: List[Tuple[str, MenaiIRExpr]] = []
        for name, value_plan, *_ in ir.bindings:
            live.append((name, self._prop(value_plan, frame_stack)))

        return MenaiIRLetrec(
            bindings=live,
            body_plan=self._prop(ir.body_plan, frame_stack),
            in_tail_position=ir.in_tail_position,
        )

    def _prop_lambda(self, ir: MenaiIRLambda, frame_stack: List[int]) -> MenaiIRLambda:
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        child_stack = frame_stack if lambda_frame_id is None else frame_stack + [lambda_frame_id]

        return MenaiIRLambda(
            params=ir.params,
            body_plan=self._prop(ir.body_plan, child_stack),
            sibling_free_vars=ir.sibling_free_vars,
            sibling_free_var_plans=[self._prop(p, frame_stack) for p in ir.sibling_free_var_plans],
            outer_free_vars=ir.outer_free_vars,
            outer_free_var_plans=[self._prop(p, frame_stack) for p in ir.outer_free_var_plans],
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _substitute(
        self,
        ir: MenaiIRExpr,
        replacements: Dict[str, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        """
        Walk ir and replace every MenaiIRVariable(var_type='local', name=n)
        with replacements[n] for each n in replacements.

        Shadowing: when an inner let/letrec binds a name that is in
        replacements, that name is removed from the map before descending
        into the inner body so the inner binding is not replaced.
        """
        if isinstance(ir, MenaiIRVariable):
            if ir.var_type == 'local' and ir.name in replacements:
                return replacements[ir.name]

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
            return MenaiIRCall(
                func_plan=self._substitute(ir.func_plan, replacements, frame_stack),
                arg_plans=[self._substitute(a, replacements, frame_stack) for a in ir.arg_plans],
                is_tail_call=ir.is_tail_call,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(
                value_plan=self._substitute(ir.value_plan, replacements, frame_stack)
            )

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._substitute(m, replacements, frame_stack) for m in ir.message_plans],
                value_plan=self._substitute(ir.value_plan, replacements, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRCopyPropagator._substitute: unhandled IR node type {type(ir).__name__}"
        )

    def _substitute_let(
        self,
        ir: MenaiIRLet,
        replacements: Dict[str, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        # Binding values are in the outer scope — substitute into them.
        new_bindings: List[Tuple[str, MenaiIRExpr]] = []
        for name, value_plan, *_ in ir.bindings:
            new_bindings.append((name, self._substitute(value_plan, replacements, frame_stack)))

        # Remove shadowed names for the body.
        inner_names = {name for name, *_ in ir.bindings}
        body_replacements = {k: v for k, v in replacements.items() if k not in inner_names}
        new_body = self._substitute(ir.body_plan, body_replacements, frame_stack)

        new_let = MenaiIRLet(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )
        return self._prop(new_let, frame_stack)

    def _substitute_letrec(
        self,
        ir: MenaiIRLetrec,
        replacements: Dict[str, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        # letrec bindings are in scope for their own values — remove shadowed names.
        inner_names = {name for name, *_ in ir.bindings}
        inner_replacements = {k: v for k, v in replacements.items() if k not in inner_names}

        new_bindings: List[Tuple[str, MenaiIRExpr]] = []
        for name, value_plan, *_ in ir.bindings:
            new_bindings.append((name, self._substitute(value_plan, inner_replacements, frame_stack)))

        new_body = self._substitute(ir.body_plan, inner_replacements, frame_stack)

        new_letrec = MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )
        return self._prop(new_letrec, frame_stack)

    def _substitute_lambda(
        self,
        ir: MenaiIRLambda,
        replacements: Dict[str, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRLambda:
        """
        Substitute into a lambda node.

        The lambda's params and free_vars shadow the outer replacements inside
        the body.  free_var_plans are evaluated in the enclosing frame and are
        substituted freely.  The body is descended into with shadowed names
        removed from replacements.
        """
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        child_stack = frame_stack if lambda_frame_id is None else frame_stack + [lambda_frame_id]

        # Substitute into free_var_plans (evaluated in enclosing frame).
        new_sibling_fvp = [self._substitute(p, replacements, frame_stack) for p in ir.sibling_free_var_plans]
        new_outer_fvp = [self._substitute(p, replacements, frame_stack) for p in ir.outer_free_var_plans]

        # Remove names shadowed by the lambda's own params and free vars.
        shadow = set(ir.params) | set(ir.sibling_free_vars) | set(ir.outer_free_vars)
        body_replacements = {k: v for k, v in replacements.items() if k not in shadow}
        new_body = self._substitute(ir.body_plan, body_replacements, child_stack)
        # Also run _prop on the body so inner opportunities are taken.
        new_body = self._prop(new_body, child_stack)

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
        )

    @staticmethod
    def _is_trivially_copyable(value_plan: MenaiIRExpr) -> bool:
        """
        Return True if *value_plan* is a candidate for copy propagation.

        Trivially copyable: MenaiIRConstant, MenaiIREmptyList, MenaiIRQuote,
        MenaiIRVariable (both local and global).

        Lambdas, calls, if-expressions, and other compound nodes are NOT
        trivially copyable — duplicating them would duplicate work.
        """
        return isinstance(
            value_plan,
            (MenaiIRConstant, MenaiIREmptyList, MenaiIRQuote, MenaiIRVariable),
        )
