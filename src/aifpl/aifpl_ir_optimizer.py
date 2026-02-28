"""
AIFPL IR Optimizer - transformation pass over the IR tree.

Consumes an IRUseCounts annotation (produced by AIFPLIRUseCounter) and applies
IR-level optimizations that are safe because AIFPL is a pure functional language
— every binding is immutable and every expression is side-effect-free.

Current optimizations
---------------------
1. Dead binding elimination
   Any let/letrec binding whose total use count is zero is dropped entirely.
   Because AIFPL is pure, the value expression can never have side effects, so
   removing it is always safe.  When *all* bindings in a let/letrec are dead the
   entire form collapses to its body.

   letrec note: a binding whose only uses are is_parent_ref self-calls is also
   considered dead from the outside — the recursive group can never be reached
   and is dropped.

The optimizer produces a new IR tree; the original is never mutated.  The
IRUseCounts annotation is consumed read-only and is not updated — callers that
want fixed-point iteration should re-run AIFPLIRUseCounter on the new tree.
"""

from __future__ import annotations

from typing import List, Tuple

from aifpl.aifpl_ir import (
    AIFPLIRExpr,
    AIFPLIRCall,
    AIFPLIRConstant,
    AIFPLIREmptyList,
    AIFPLIRError,
    AIFPLIRIf,
    AIFPLIRLambda,
    AIFPLIRLet,
    AIFPLIRLetrec,
    AIFPLIRQuote,
    AIFPLIRReturn,
    AIFPLIRTrace,
    AIFPLIRVariable,
)
from aifpl.aifpl_ir_use_counter import IRUseCounts


class AIFPLIROptimizer:
    """
    IR-level optimization pass.

    Instantiate with a pre-computed IRUseCounts, then call optimize() on the
    root IR node.  The optimizer is stateless with respect to the IR tree —
    all mutable state (the frame stack) is passed explicitly through the
    recursive walk.

    Usage::

        counts = AIFPLIRUseCounter().count(ir)
        ir = AIFPLIROptimizer(counts).optimize(ir)
    """

    def __init__(self, counts: IRUseCounts) -> None:
        self._counts = counts
        self._eliminations = 0

    @property
    def eliminations(self) -> int:
        """Number of dead bindings eliminated during the last optimize() call."""
        return self._eliminations

    def optimize(self, ir: AIFPLIRExpr) -> AIFPLIRExpr:
        """
        Return an optimized copy of *ir*.

        Args:
            ir: Root IR node to optimize (output of AIFPLIRBuilder.build()).

        Returns:
            New IR tree with optimizations applied.  May be structurally
            identical to the input if no optimizations fired.
        """
        # Frame 0 is always the top-level module frame, matching the counter.
        self._eliminations = 0
        return self._opt(ir, frame_stack=[0])

    def _opt(self, ir: AIFPLIRExpr, frame_stack: List[int]) -> AIFPLIRExpr:
        """Recursively optimize *ir* in the context of *frame_stack*."""
        if isinstance(ir, AIFPLIRLet):
            return self._opt_let(ir, frame_stack)

        if isinstance(ir, AIFPLIRLetrec):
            return self._opt_letrec(ir, frame_stack)

        if isinstance(ir, AIFPLIRIf):
            return self._opt_if(ir, frame_stack)

        if isinstance(ir, AIFPLIRLambda):
            return self._opt_lambda(ir, frame_stack)

        if isinstance(ir, AIFPLIRCall):
            return self._opt_call(ir, frame_stack)

        if isinstance(ir, AIFPLIRReturn):
            return AIFPLIRReturn(value_plan=self._opt(ir.value_plan, frame_stack))

        if isinstance(ir, AIFPLIRTrace):
            return AIFPLIRTrace(
                message_plans=[self._opt(m, frame_stack) for m in ir.message_plans],
                value_plan=self._opt(ir.value_plan, frame_stack),
            )

        if isinstance(ir, (AIFPLIRConstant, AIFPLIRVariable,
                            AIFPLIRQuote, AIFPLIREmptyList, AIFPLIRError)):
            # Leaf nodes — nothing to optimize.
            return ir

        raise TypeError(f"AIFPLIROptimizer: unhandled IR node type {type(ir).__name__}")

    def _opt_let(self, ir: AIFPLIRLet, frame_stack: List[int]) -> AIFPLIRExpr:
        """
        Optimize a let node.

        Dead bindings (total use count == 0) are dropped.  If all bindings are
        dead the let collapses to its (optimized) body.
        """
        current_frame = frame_stack[-1]

        live: List[Tuple[str, AIFPLIRExpr, int]] = []
        for name, value_plan, var_index in ir.bindings:
            if self._counts.total_count(current_frame, var_index) == 0:
                # Dead binding — drop it.
                self._eliminations += 1
                continue
            live.append((name, self._opt(value_plan, frame_stack), var_index))

        opt_body = self._opt(ir.body_plan, frame_stack)

        if not live:
            # All bindings were dead — the let form itself is gone.
            return opt_body

        return AIFPLIRLet(
            bindings=live,
            body_plan=opt_body,
            in_tail_position=ir.in_tail_position,
        )

    def _opt_letrec(self, ir: AIFPLIRLetrec, frame_stack: List[int]) -> AIFPLIRExpr:
        """
        Optimize a letrec node.

        A binding is dead when its total use count is zero, OR when every use
        is an is_parent_ref self-call (the binding is unreachable from outside
        its own recursive group).

        We compute the self-reference count for each binding by inspecting the
        binding_groups metadata that the IR builder already attached.
        """
        current_frame = frame_stack[-1]

        # Count the number of is_parent_ref uses per slot by walking the
        # binding value plans.  We do a lightweight local scan here rather
        # than threading extra data through the counter, because this is a
        # letrec-specific concern.
        self_ref_counts: dict[int, int] = {}
        for name, value_plan, var_index in ir.bindings:
            if name in ir.recursive_bindings:
                self_ref_counts[var_index] = self._count_parent_refs(value_plan, var_index)

        live: List[Tuple[str, AIFPLIRExpr, int]] = []
        dead_names = set()
        for name, value_plan, var_index in ir.bindings:
            total = self._counts.total_count(current_frame, var_index)
            self_refs = self_ref_counts.get(var_index, 0)

            if total == 0 or (name in ir.recursive_bindings and total == self_refs):
                # Dead — entirely unreachable from outside.
                dead_names.add(name)
                self._eliminations += 1
                continue

            live.append((name, self._opt(value_plan, frame_stack), var_index))

        opt_body = self._opt(ir.body_plan, frame_stack)

        if not live:
            return opt_body

        # Strip dead names from recursive_bindings.
        new_recursive = ir.recursive_bindings - dead_names

        # Keep binding_groups as-is.  The groups carry AST-level metadata
        # (bindings: List[Tuple[str, AIFPLASTNode]]) that we cannot reconstruct
        # at the IR level, and the codegen only uses them for topological
        # ordering — which remains valid even if some names have been dropped
        # from ir.bindings, because the codegen iterates ir.bindings directly
        # and the groups are only consulted for the is_recursive flag.
        # Passing the original groups through is therefore safe.
        return AIFPLIRLetrec(
            bindings=live,
            body_plan=opt_body,
            binding_groups=ir.binding_groups,
            recursive_bindings=new_recursive,
            in_tail_position=ir.in_tail_position,
        )

    def _count_parent_refs(self, ir: AIFPLIRExpr, var_index: int) -> int:
        """
        Count the number of AIFPLIRVariable nodes in *ir* that are
        is_parent_ref references to *var_index* in *frame_id*.

        This is a lightweight local scan used only by _opt_letrec.
        """
        count = 0
        stack: List[AIFPLIRExpr] = [ir]
        while stack:
            node = stack.pop()

            if isinstance(node, AIFPLIRVariable):
                if (node.is_parent_ref and node.var_type == 'local'
                        and node.index == var_index):
                    count += 1

            elif isinstance(node, AIFPLIRLet):
                for _, vp, _ in node.bindings:
                    stack.append(vp)

                stack.append(node.body_plan)

            elif isinstance(node, AIFPLIRLetrec):
                for _, vp, _ in node.bindings:
                    stack.append(vp)

                stack.append(node.body_plan)

            elif isinstance(node, AIFPLIRIf):
                stack.extend([node.condition_plan, node.then_plan, node.else_plan])

            elif isinstance(node, AIFPLIRCall):
                if not node.is_tail_recursive:
                    stack.append(node.func_plan)

                stack.extend(node.arg_plans)

            elif isinstance(node, AIFPLIRReturn):
                stack.append(node.value_plan)

            elif isinstance(node, AIFPLIRTrace):
                stack.extend(node.message_plans)
                stack.append(node.value_plan)

            elif isinstance(node, AIFPLIRLambda):
                # Don't descend into nested lambdas — their is_parent_ref
                # nodes refer to their own enclosing frame, not ours.
                pass

            # Leaf nodes (Constant, Quote, EmptyList, Error) — nothing to do.

        return count

    def _opt_if(self, ir: AIFPLIRIf, frame_stack: List[int]) -> AIFPLIRIf:
        return AIFPLIRIf(
            condition_plan=self._opt(ir.condition_plan, frame_stack),
            then_plan=self._opt(ir.then_plan, frame_stack),
            else_plan=self._opt(ir.else_plan, frame_stack),
            in_tail_position=ir.in_tail_position,
        )

    def _opt_lambda(self, ir: AIFPLIRLambda, frame_stack: List[int]) -> AIFPLIRLambda:
        """
        Optimize a lambda node.

        The lambda body is optimized in the lambda's own frame (looked up from
        the lambda_frame_ids map populated by the counter).
        """
        lambda_frame_id = self._counts.lambda_frame_ids.get(id(ir))
        if lambda_frame_id is None:
            # The counter didn't visit this node (shouldn't happen in normal
            # use, but be defensive).  Optimize body in the current frame.
            child_stack = frame_stack

        else:
            child_stack = frame_stack + [lambda_frame_id]

        return AIFPLIRLambda(
            params=ir.params,
            body_plan=self._opt(ir.body_plan, child_stack),
            free_vars=ir.free_vars,
            free_var_plans=ir.free_var_plans,
            parent_refs=ir.parent_refs,
            parent_ref_plans=ir.parent_ref_plans,
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            sibling_bindings=ir.sibling_bindings,
            max_locals=ir.max_locals,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _opt_call(self, ir: AIFPLIRCall, frame_stack: List[int]) -> AIFPLIRCall:
        opt_args = [self._opt(a, frame_stack) for a in ir.arg_plans]

        if ir.is_tail_recursive:
            # func_plan is a sentinel — don't optimize it.
            return AIFPLIRCall(
                func_plan=ir.func_plan,
                arg_plans=opt_args,
                is_tail_call=ir.is_tail_call,
                is_tail_recursive=ir.is_tail_recursive,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        return AIFPLIRCall(
            func_plan=self._opt(ir.func_plan, frame_stack),
            arg_plans=opt_args,
            is_tail_call=ir.is_tail_call,
            is_tail_recursive=ir.is_tail_recursive,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )
