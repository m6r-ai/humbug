"""
Menai IR Optimizer - transformation pass over the IR tree.

Consumes an IRUseCounts annotation (produced by MenaiIRUseCounter) and applies
IR-level optimizations that are safe because Menai is a pure functional language
— every binding is immutable and every expression is side-effect-free.
"""

from typing import List, Tuple, cast

from menai.menai_ir import (
    MenaiIRExpr,
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIRBuildList,
    MenaiIRBuildDict,
    MenaiIREmptyList,
    MenaiIRError,
    MenaiIRIf,
    MenaiIRLambda,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRQuote,
    MenaiIRReturn,
    MenaiIRTrace,
    MenaiIRVariable,
)
from menai.menai_value import MenaiBoolean
from menai.menai_ir_use_counter import MenaiIRUseCounter, IRUseCounts
from menai.menai_ir_optimization_pass import MenaiIROptimizationPass


class MenaiIROptimizer(MenaiIROptimizationPass):
    """
    IR-level optimization pass.

    Implements MenaiIROptimizationPass: call optimize(ir) to get back a
    transformed IR tree and a boolean indicating whether any changes were made.
    Use counts are computed internally so callers do not need to manage them.

    Usage::

        new_ir, changed = MenaiIROptimizer().optimize(ir)
    """

    def __init__(self) -> None:
        self._eliminations = 0
        self._counts: IRUseCounts | None = None

    def optimize(self, ir: MenaiIRExpr) -> tuple[MenaiIRExpr, bool]:
        """Return an optimized IR tree and a boolean indicating whether any changes were made."""
        self._eliminations = 0
        self._counts = MenaiIRUseCounter().count(ir)
        new_ir = self._opt(ir, frame_stack=[0])
        return new_ir, self._eliminations > 0

    def _opt(self, ir: MenaiIRExpr, frame_stack: List[int]) -> MenaiIRExpr:
        """Recursively walk the IR tree and apply optimizations."""
        if isinstance(ir, MenaiIRLet):
            return self._opt_let(ir, frame_stack)

        if isinstance(ir, MenaiIRLetrec):
            return self._opt_letrec(ir, frame_stack)

        if isinstance(ir, MenaiIRIf):
            return self._opt_if(ir, frame_stack)

        if isinstance(ir, MenaiIRLambda):
            return self._opt_lambda(ir, frame_stack)

        if isinstance(ir, MenaiIRCall):
            return self._opt_call(ir, frame_stack)

        if isinstance(ir, MenaiIRBuildList):
            return MenaiIRBuildList(
                element_plans=[self._opt(e, frame_stack) for e in ir.element_plans],
            )

        if isinstance(ir, MenaiIRBuildDict):
            return MenaiIRBuildDict(
                pair_plans=[(self._opt(k, frame_stack), self._opt(v, frame_stack))
                            for k, v in ir.pair_plans],
            )

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._opt(ir.value_plan, frame_stack))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._opt(m, frame_stack) for m in ir.message_plans],
                value_plan=self._opt(ir.value_plan, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRVariable, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(f"MenaiIROptimizer: unhandled IR node type {type(ir).__name__}")

    def _opt_let(self, ir: MenaiIRLet, frame_stack: List[int]) -> MenaiIRExpr:
        """Drop dead let bindings (total use count == 0)."""
        current_frame = frame_stack[-1]
        counts = cast(IRUseCounts, self._counts)

        live: List[Tuple[str, MenaiIRExpr]] = []
        for binding in ir.bindings:
            name, value_plan, *_ = binding
            if counts.total_count(current_frame, id(binding)) == 0:
                self._eliminations += 1
                continue
            live.append((name, self._opt(value_plan, frame_stack)))

        opt_body = self._opt(ir.body_plan, frame_stack)

        if not live:
            return opt_body

        return MenaiIRLet(
            bindings=live,
            body_plan=opt_body,
            in_tail_position=ir.in_tail_position,
        )

    def _opt_letrec(self, ir: MenaiIRLetrec, frame_stack: List[int]) -> MenaiIRExpr:
        """Drop dead letrec bindings (total use count == 0)."""
        current_frame = frame_stack[-1]
        counts = cast(IRUseCounts, self._counts)

        live: List[Tuple[str, MenaiIRExpr]] = []
        for binding in ir.bindings:
            name, value_plan, *_ = binding
            if counts.total_count(current_frame, id(binding)) == 0:
                self._eliminations += 1
                continue

            live.append((name, self._opt(value_plan, frame_stack)))

        opt_body = self._opt(ir.body_plan, frame_stack)

        if not live:
            return opt_body

        return MenaiIRLetrec(
            bindings=live,
            body_plan=opt_body,
            in_tail_position=ir.in_tail_position,
        )

    def _opt_if(self, ir: MenaiIRIf, frame_stack: List[int]) -> MenaiIRExpr:
        opt_condition = self._opt(ir.condition_plan, frame_stack)
        opt_then = self._opt(ir.then_plan, frame_stack)
        opt_else = self._opt(ir.else_plan, frame_stack)

        # Boolean identity elimination:
        #   (if <cond> #t #f)  →  <cond>
        #   (if <cond> #f #t)  →  (boolean-not <cond>)
        if (isinstance(opt_then, MenaiIRConstant)
                and isinstance(opt_then.value, MenaiBoolean)
                and isinstance(opt_else, MenaiIRConstant)
                and isinstance(opt_else.value, MenaiBoolean)):
            if opt_then.value.value and not opt_else.value.value:
                # (if cond #t #f) →  cond
                self._eliminations += 1
                return opt_condition

            if not opt_then.value.value and opt_else.value.value:
                # (if cond #f #t) → (boolean-not cond)
                self._eliminations += 1
                return MenaiIRCall(
                    func_plan=MenaiIRVariable(
                            name='boolean-not', var_type='global'
                    ),
                    arg_plans=[opt_condition],
                    is_tail_call=ir.in_tail_position,
                    is_builtin=True,
                    builtin_name='boolean-not',
                )

        return MenaiIRIf(
            condition_plan=opt_condition,
            then_plan=opt_then,
            else_plan=opt_else,
            in_tail_position=ir.in_tail_position,
        )

    def _opt_lambda(self, ir: MenaiIRLambda, frame_stack: List[int]) -> MenaiIRLambda:
        """Optimize the body of a lambda."""
        counts = cast(IRUseCounts, self._counts)
        lambda_frame_id = counts.lambda_frame_ids.get(id(ir))
        child_stack = frame_stack if lambda_frame_id is None else frame_stack + [lambda_frame_id]

        return MenaiIRLambda(
            params=ir.params,
            body_plan=self._opt(ir.body_plan, child_stack),
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

    def _opt_call(self, ir: MenaiIRCall, frame_stack: List[int]) -> MenaiIRCall:
        """Optimize the function and argument plans of a call."""
        return MenaiIRCall(
            func_plan=self._opt(ir.func_plan, frame_stack),
            arg_plans=[self._opt(a, frame_stack) for a in ir.arg_plans],
            is_tail_call=ir.is_tail_call,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )
