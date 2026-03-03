"""
Menai IR Inline-Once Pass - single-use let binding inliner.

Consumes an IRUseCounts annotation (produced by MenaiIRUseCounter) and inlines
any MenaiIRLet binding that has exactly one total use, subject to the lambda
exclusion rule described below.

Why this is different from MenaiIRCopyPropagator
-------------------------------------------------
Copy propagation inlines *trivially copyable* values (constants, variable
loads, etc.) at any use count, because duplicating a cheap value costs nothing.
This pass instead targets bindings with *any* value — including calls,
if-expressions, and other compound nodes — but only when the use count is
exactly 1.  With a single use and no work duplication, inlining is always
profitable in a pure language.

Lambda exclusion rule
---------------------
MenaiIRLambda values are never inlined, even when use count is 1.  A lambda
is a closure definition that may be called many times even if its binding slot
is referenced only once.  Inlining it would duplicate the closure creation and
all of its free-variable loads, causing code explosion.  Dead lambdas
(total_count == 0) are handled by MenaiIROptimizer instead.

No lambda boundary rule
-----------------------
Variables are symbolic throughout the optimisation pipeline — MenaiIRVariable
carries only name and var_type.  Substituting any value plan at any position
in the tree is safe: MenaiCFGBuilder resolves all names correctly.

Shadowing
---------
When the substitution walk descends into an inner let/letrec that binds the
same name as a pending substitution, that name is removed from the map for
the inner body so the inner binding is not incorrectly replaced.  For lambdas,
params and free_vars shadow the enclosing substitutions.

Scope of the pass: let only, not letrec
----------------------------------------
Inline-once is applied only to MenaiIRLet bindings.  MenaiIRLetrec bindings
are skipped (same rationale as copy propagation).  The pass still recurses
into letrec bodies to optimise inner let nodes.

Implements MenaiIROptimizationPass so it can be managed by the IR pass manager
in MenaiCompiler.
"""

from typing import Dict, List, Tuple, cast

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

    Usage::

        new_ir, changed = MenaiIRInlineOnce().optimize(ir)
    """

    def __init__(self) -> None:
        self._inlinings: int = 0
        self._counts: IRUseCounts | None = None

    def inlinings(self) -> int:
        """Return the number of inlinings performed by the most recent optimize() call."""
        return self._inlinings

    def optimize(self, ir: MenaiIRExpr) -> Tuple[MenaiIRExpr, bool]:
        self._inlinings = 0
        self._counts = MenaiIRUseCounter().count(ir)
        new_ir = self._inline(ir, frame_stack=[0])
        return new_ir, self._inlinings > 0

    def _inline(self, ir: MenaiIRExpr, frame_stack: List[int]) -> MenaiIRExpr:
        """Recursively inline single-use let bindings in ir."""
        if isinstance(ir, MenaiIRLet):
            return self._inline_let(ir, frame_stack)

        if isinstance(ir, MenaiIRLetrec):
            return self._inline_letrec(ir, frame_stack)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._inline(ir.condition_plan, frame_stack),
                then_plan=self._inline(ir.then_plan, frame_stack),
                else_plan=self._inline(ir.else_plan, frame_stack),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRLambda):
            return self._inline_lambda(ir, frame_stack)

        if isinstance(ir, MenaiIRCall):
            return MenaiIRCall(
                func_plan=self._inline(ir.func_plan, frame_stack),
                arg_plans=[self._inline(a, frame_stack) for a in ir.arg_plans],
                is_tail_call=ir.is_tail_call,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._inline(ir.value_plan, frame_stack))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._inline(m, frame_stack) for m in ir.message_plans],
                value_plan=self._inline(ir.value_plan, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRVariable, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRInlineOnce: unhandled IR node type {type(ir).__name__}"
        )

    def _inline_let(self, ir: MenaiIRLet, frame_stack: List[int]) -> MenaiIRExpr:
        """
        Inline single-use let bindings.

        For each binding (name, value_plan):
          - If total_count == 1 and value_plan is not a lambda, substitute
            value_plan at the single use site in body_plan and drop the binding.
          - Otherwise keep the binding and recursively optimise its value.
        """
        current_frame = frame_stack[-1]
        counts = cast(IRUseCounts, self._counts)

        to_inline: Dict[str, MenaiIRExpr] = {}
        for binding in ir.bindings:
            name, value_plan, *_ = binding
            if counts.total_count(current_frame, id(binding)) == 1:
                if not isinstance(value_plan, MenaiIRLambda):
                    to_inline[name] = value_plan

        live: List[Tuple[str, MenaiIRExpr]] = []
        for name, value_plan, *_ in ir.bindings:
            if name in to_inline:
                self._inlinings += 1
                continue

            live.append((name, self._inline(value_plan, frame_stack)))

        opt_body = self._inline(ir.body_plan, frame_stack)
        if to_inline:
            opt_body = self._substitute(opt_body, to_inline, frame_stack)

        if not live:
            return opt_body

        return MenaiIRLet(
            bindings=live,
            body_plan=opt_body,
            in_tail_position=ir.in_tail_position,
        )

    def _inline_letrec(self, ir: MenaiIRLetrec, frame_stack: List[int]) -> MenaiIRExpr:
        """Recurse into letrec without inlining its bindings."""
        live: List[Tuple[str, MenaiIRExpr]] = []
        for name, value_plan, *_ in ir.bindings:
            live.append((name, self._inline(value_plan, frame_stack)))

        return MenaiIRLetrec(
            bindings=live,
            body_plan=self._inline(ir.body_plan, frame_stack),
            in_tail_position=ir.in_tail_position,
        )

    def _inline_lambda(self, ir: MenaiIRLambda, frame_stack: List[int]) -> MenaiIRLambda:
        """Optimize a lambda node."""
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        child_stack = frame_stack if lambda_frame_id is None else frame_stack + [lambda_frame_id]

        return MenaiIRLambda(
            params=ir.params,
            body_plan=self._inline(ir.body_plan, child_stack),
            sibling_free_vars=ir.sibling_free_vars,
            sibling_free_var_plans=ir.sibling_free_var_plans,
            outer_free_vars=ir.outer_free_vars,
            outer_free_var_plans=ir.outer_free_var_plans,
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

        Shadowing: inner let/letrec bindings and lambda params/free_vars that
        share a name with a pending substitution are removed from the map
        before descending into the inner scope.
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
            f"MenaiIRInlineOnce._substitute: unhandled IR node type {type(ir).__name__}"
        )

    def _substitute_let(
        self,
        ir: MenaiIRLet,
        replacements: Dict[str, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        """Substitute into a let node."""
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
        return self._inline(new_let, frame_stack)

    def _substitute_letrec(
        self,
        ir: MenaiIRLetrec,
        replacements: Dict[str, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRExpr:
        """Substitute into a letrec node."""
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
        return self._inline(new_letrec, frame_stack)

    def _substitute_lambda(
        self,
        ir: MenaiIRLambda,
        replacements: Dict[str, MenaiIRExpr],
        frame_stack: List[int],
    ) -> MenaiIRLambda:
        """
        Substitute into a lambda node.

        free_var_plans are evaluated in the enclosing frame — substitute freely.
        The body is descended into with params and free_vars removed from
        replacements (they shadow the outer bindings).
        """
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        child_stack = frame_stack if lambda_frame_id is None else frame_stack + [lambda_frame_id]

        new_sibling_fvp = [self._substitute(p, replacements, frame_stack) for p in ir.sibling_free_var_plans]
        new_outer_fvp = [self._substitute(p, replacements, frame_stack) for p in ir.outer_free_var_plans]

        shadow = set(ir.params) | set(ir.sibling_free_vars) | set(ir.outer_free_vars)
        body_replacements = {k: v for k, v in replacements.items() if k not in shadow}
        new_body = self._substitute(ir.body_plan, body_replacements, child_stack)
        new_body = self._inline(new_body, child_stack)

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
