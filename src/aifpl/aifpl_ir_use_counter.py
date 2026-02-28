"""
AIFPL IR Use Counter - pure analysis pass over the IR tree.

Walks an AIFPLIRExpr tree and counts every reference to every locally-bound
variable, producing an IRUseCounts annotation that downstream passes (e.g.
AIFPLIROptimizer) can consume without re-deriving the same information.

Key concepts
------------
- A *frame* corresponds to a single lambda (or the top-level module scope).
  Each frame owns a contiguous set of local variable slots (indexed by int).
- An *AIFPLIRVariable* with var_type='local' and depth=0 refers to a slot in
  the *current* frame; depth=1 refers to the immediately enclosing lambda's
  frame, and so on.
- Uses are split into two buckets per frame:
    local    — references that occur *within* the frame itself
    external — references that cross at least one lambda boundary (i.e. the
               variable is captured by a child lambda).  These are recorded on
               the *defining* frame so the optimizer can tell whether a binding
               is reachable from outside its own lambda.

letrec / recursive bindings
---------------------------
AIFPLIRVariable.is_parent_ref marks back-references that implement recursion
(LOAD_PARENT_VAR in the VM).  These are counted in the 'local' bucket of the
*defining* frame (depth tells us which frame owns the slot), but callers can
inspect is_parent_ref on individual variable nodes when they need to distinguish
self-calls from external uses.  The IRUseCounts.is_only_self_referencing()
helper encapsulates the common "is this binding unreachable from outside its
own recursive group?" query.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List

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


@dataclass
class FrameUseCounts:
    """
    Use counts for all local variable slots within a single lambda frame.

    local    — uses that occur within this frame (including is_parent_ref
               back-references from recursive lambdas *inside* this frame).
    external — uses that cross at least one lambda boundary outward from this
               frame (i.e. the slot is captured as a free variable by a nested
               lambda).  A slot can appear in both buckets simultaneously.
    """
    local: Dict[int, int] = field(default_factory=dict)     # slot → count
    external: Dict[int, int] = field(default_factory=dict)  # slot → count


@dataclass
class IRUseCounts:
    """
    Complete use-count annotation for an IR tree.

    frames
        One FrameUseCounts per lambda frame encountered during the walk.
        Frame 0 is always the top-level module frame.

    lambda_frame_ids
        Maps id(AIFPLIRLambda) → frame_id so callers can look up the
        FrameUseCounts for a specific lambda node directly.
        Using id() is safe because IRUseCounts must not outlive the IR tree
        it was built from (the IR is immutable; no node is ever replaced in
        place).
    """
    frames: List[FrameUseCounts] = field(default_factory=list)
    lambda_frame_ids: Dict[int, int] = field(default_factory=dict)

    def local_count(self, frame_id: int, var_index: int) -> int:
        """Uses of var_index that occur *within* frame frame_id."""
        return self.frames[frame_id].local.get(var_index, 0)

    def external_count(self, frame_id: int, var_index: int) -> int:
        """Uses of var_index from frame frame_id captured by child lambdas."""
        return self.frames[frame_id].external.get(var_index, 0)

    def total_count(self, frame_id: int, var_index: int) -> int:
        """Total uses: local + external captures."""
        return self.local_count(frame_id, var_index) + self.external_count(frame_id, var_index)

    def is_only_self_referencing(self, frame_id: int, var_index: int,
                                 self_ref_count: int) -> bool:
        """
        Return True when the *only* uses of var_index in frame_id are the
        self_ref_count recursive back-references (is_parent_ref), i.e. the
        binding is unreachable from outside its own recursive group.

        self_ref_count is the number of is_parent_ref uses that the caller
        already knows about (typically 1 for a singly-recursive function).
        A total_count equal to self_ref_count means every reference is a
        self-call — nothing from outside the group reaches the binding.
        """
        return self.total_count(frame_id, var_index) == self_ref_count


class AIFPLIRUseCounter:
    """
    Pure analysis pass: walk an IR tree and produce an IRUseCounts annotation.

    No transformation is performed; the IR is not modified.

    Usage::

        counts = AIFPLIRUseCounter().count(ir)
        n = counts.total_count(frame_id=0, var_index=3)
    """

    def count(self, ir: AIFPLIRExpr) -> IRUseCounts:
        """
        Walk *ir* and return a fully populated IRUseCounts.

        Args:
            ir: Root of the IR tree (output of AIFPLIRBuilder.build()).

        Returns:
            IRUseCounts annotation for the entire tree.
        """
        result = IRUseCounts()
        module_frame_id = self._push_frame(result)
        self._walk(ir, result, frame_stack=[module_frame_id])
        return result

    def _push_frame(self, result: IRUseCounts) -> int:
        """Append a fresh FrameUseCounts and return its index."""
        frame_id = len(result.frames)
        result.frames.append(FrameUseCounts())
        return frame_id

    def _inc_local(self, result: IRUseCounts, frame_id: int, var_index: int) -> None:
        counts = result.frames[frame_id].local
        counts[var_index] = counts.get(var_index, 0) + 1

    def _inc_external(self, result: IRUseCounts, frame_id: int, var_index: int) -> None:
        counts = result.frames[frame_id].external
        counts[var_index] = counts.get(var_index, 0) + 1

    def _walk(self, ir: AIFPLIRExpr, result: IRUseCounts,
              frame_stack: List[int]) -> None:
        """
        Recursively walk *ir*, updating *result* in place.

        frame_stack is a list of frame IDs from outermost (index 0) to
        current (index -1).  It is never mutated; child calls receive a
        new list extended by one element when crossing a lambda boundary.
        """
        if isinstance(ir, AIFPLIRVariable):
            self._walk_variable(ir, result, frame_stack)

        elif isinstance(ir, AIFPLIRLambda):
            self._walk_lambda(ir, result, frame_stack)

        elif isinstance(ir, AIFPLIRLet):
            self._walk_let(ir, result, frame_stack)

        elif isinstance(ir, AIFPLIRLetrec):
            self._walk_letrec(ir, result, frame_stack)

        elif isinstance(ir, AIFPLIRIf):
            self._walk(ir.condition_plan, result, frame_stack)
            self._walk(ir.then_plan, result, frame_stack)
            self._walk(ir.else_plan, result, frame_stack)

        elif isinstance(ir, AIFPLIRCall):
            self._walk_call(ir, result, frame_stack)

        elif isinstance(ir, AIFPLIRReturn):
            self._walk(ir.value_plan, result, frame_stack)

        elif isinstance(ir, AIFPLIRTrace):
            for msg in ir.message_plans:
                self._walk(msg, result, frame_stack)
            self._walk(ir.value_plan, result, frame_stack)

        elif isinstance(ir, (AIFPLIRConstant, AIFPLIRQuote, AIFPLIREmptyList, AIFPLIRError)):
            # Leaf nodes with no variable references — nothing to count.
            pass

        else:
            raise TypeError(f"AIFPLIRUseCounter: unhandled IR node type {type(ir).__name__}")

    def _walk_variable(self, ir: AIFPLIRVariable, result: IRUseCounts,
                       frame_stack: List[int]) -> None:
        """Count a variable reference."""
        if ir.var_type != 'local':
            # Global / builtin — no binding to count.
            return

        depth = ir.depth
        if depth >= len(frame_stack):
            # Defensive: depth exceeds frame stack depth — skip rather than crash.
            return

        # Resolve to the defining frame.
        # frame_stack[-1] is current frame, frame_stack[-(1+depth)] is the
        # frame 'depth' lambda boundaries above the current one.
        defining_frame_id = frame_stack[-(1 + depth)]

        if depth == 0:
            # Same frame — local use.
            self._inc_local(result, defining_frame_id, ir.index)

        else:
            # Cross-frame reference — this is a capture (free variable use).
            # Count it as 'external' on the defining frame.
            self._inc_external(result, defining_frame_id, ir.index)

    def _walk_lambda(self, ir: AIFPLIRLambda, result: IRUseCounts,
                     frame_stack: List[int]) -> None:
        """
        Walk a lambda node.

        free_var_plans and parent_ref_plans load variables from the *current*
        frame (they are evaluated in the enclosing scope to build the closure),
        so they are walked with the current frame_stack.

        The lambda body is walked in a new frame pushed onto the stack.
        """
        # Assign a new frame to this lambda.
        lambda_frame_id = self._push_frame(result)
        result.lambda_frame_ids[id(ir)] = lambda_frame_id

        # Free variable loads happen in the *enclosing* frame.
        for fv_plan in ir.free_var_plans:
            self._walk(fv_plan, result, frame_stack)

        # Parent-ref loads also happen in the enclosing frame (LOAD_PARENT_VAR
        # is emitted by the parent, not inside the lambda body).
        for pr_plan in ir.parent_ref_plans:
            self._walk(pr_plan, result, frame_stack)

        # Walk the body in the new frame.
        self._walk(ir.body_plan, result, frame_stack + [lambda_frame_id])

    def _walk_let(self, ir: AIFPLIRLet, result: IRUseCounts,
                  frame_stack: List[int]) -> None:
        """
        Walk a let node.

        Binding value expressions and the body all live in the same frame —
        let does not introduce a new lambda boundary.
        """
        for _name, value_plan, _idx in ir.bindings:
            self._walk(value_plan, result, frame_stack)

        self._walk(ir.body_plan, result, frame_stack)

    def _walk_letrec(self, ir: AIFPLIRLetrec, result: IRUseCounts,
                     frame_stack: List[int]) -> None:
        """
        Walk a letrec node.

        Same frame rules as let.  Recursive back-references (is_parent_ref)
        will be counted as local uses of the defining frame's slot, which is
        correct — they are references within the same frame even though they
        cross a lambda boundary textually.  The is_parent_ref flag on the
        variable node allows callers to distinguish them if needed.
        """
        for _name, value_plan, _idx in ir.bindings:
            self._walk(value_plan, result, frame_stack)

        self._walk(ir.body_plan, result, frame_stack)

    def _walk_call(self, ir: AIFPLIRCall, result: IRUseCounts,
                   frame_stack: List[int]) -> None:
        """Walk a call node."""
        # For tail-recursive self-calls the func_plan is a sentinel variable
        # with name '<tail-recursive>' — it has var_type='local' and index=0
        # but does NOT represent a real binding.  We skip it here; the actual
        # argument expressions are what matter for use counting.
        if not ir.is_tail_recursive:
            self._walk(ir.func_plan, result, frame_stack)

        for arg_plan in ir.arg_plans:
            self._walk(arg_plan, result, frame_stack)
