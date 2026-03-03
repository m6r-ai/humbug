"""
Menai IR Use Counter - pure analysis pass over the IR tree.

Walks a MenaiIRExpr tree and counts every reference to every locally-bound
variable, producing an IRUseCounts annotation that downstream passes
(MenaiIRCopyPropagator, MenaiIRInlineOnce, MenaiIROptimizer) can consume.

Design
------
Variables in the IR are symbolic — MenaiIRVariable nodes carry only a name
and var_type.  The use counter therefore works entirely on names,
maintaining its own scope chain to resolve each variable reference to the
frame that owns it.

A *frame* corresponds to a single lambda (or the top-level module scope).
Each frame owns a set of variable names (its parameters and any let/letrec
bindings within it).  Use counts are keyed by (frame_id, name).

There is no local/external split.  A use is simply a use of a name — the
frame_id tells us which frame's binding is being referenced, regardless of
whether the reference crosses lambda boundaries.

Scope chain
-----------
The counter maintains a scope_stack: List[Dict[str, int]] where each entry
maps name → frame_id.  When a MenaiIRLambda is entered, a new scope level
is pushed with the lambda's params and free_vars mapped to the lambda's own
frame_id.  When a MenaiIRLet or MenaiIRLetrec is encountered, its binding
names are pushed onto the *current* lambda's scope level (they share the
same frame_id as the enclosing lambda).

Resolution: to find the defining frame for a name, search the scope_stack
from innermost to outermost.  The first match gives the frame_id.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Tuple

from menai.menai_ir import (
    MenaiIRExpr,
    MenaiIRCall,
    MenaiIRConstant,
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


@dataclass
class FrameUseCounts:
    """
    Use counts for bindings within a single lambda frame.

    Keyed by binding_id — the id() of the binding tuple (name, value_plan)
    from the MenaiIRLet or MenaiIRLetrec node, or a synthetic id for lambda
    params/free-vars.  Using id() of the tuple object gives each binding a
    unique identity even when two bindings share the same name (shadowing).
    """
    counts: Dict[int, int] = field(default_factory=dict)  # binding_id → use count


@dataclass
class IRUseCounts:
    """
    Complete use-count annotation for an IR tree.

    frames
        One FrameUseCounts per lambda frame encountered during the walk.
        Frame 0 is always the top-level module frame.

    lambda_frame_ids
        Maps id(MenaiIRLambda) → frame_id so callers can look up the
        FrameUseCounts for a specific lambda node directly.
    """
    frames: List[FrameUseCounts] = field(default_factory=list)
    lambda_frame_ids: Dict[int, int] = field(default_factory=dict)

    def total_count(self, frame_id: int, binding_id: int) -> int:
        """Total uses of the binding identified by *binding_id* within *frame_id*."""
        if frame_id >= len(self.frames):

            return 0
        return self.frames[frame_id].counts.get(binding_id, 0)


class MenaiIRUseCounter:
    """
    Pure analysis pass: walk an IR tree and produce an IRUseCounts annotation.

    No transformation is performed; the IR is not modified.

    Usage::

        counts = MenaiIRUseCounter().count(ir)
        n = counts.total_count(frame_id=0, name='x')
    """

    def count(self, ir: MenaiIRExpr) -> IRUseCounts:
        """
        Walk *ir* and return a fully populated IRUseCounts.

        Args:
            ir: Root of the IR tree.

        Returns:
            IRUseCounts annotation for the entire tree.
        """
        result = IRUseCounts()
        module_frame_id = self._push_frame(result)

        # scope_stack: list of dicts mapping name → frame_id.
        # Each dict corresponds to one scope level; multiple levels can share
        # the same frame_id (let/letrec within a lambda do not create a new frame).
        scope_stack: List[Dict[str, Tuple[int, int]]] = [{}]
        self._walk(ir, result, scope_stack, module_frame_id)
        return result

    def _push_frame(self, result: IRUseCounts) -> int:
        """Append a fresh FrameUseCounts and return its frame_id."""
        frame_id = len(result.frames)
        result.frames.append(FrameUseCounts())
        return frame_id

    def _inc(self, result: IRUseCounts, frame_id: int, binding_id: int) -> None:
        """Increment the use count for the given binding."""
        counts = result.frames[frame_id].counts
        counts[binding_id] = counts.get(binding_id, 0) + 1

    def _resolve_name(self, name: str, scope_stack: List[Dict[str, Tuple[int, int]]]) -> Tuple[int, int] | None:
        """
        Search scope_stack (innermost first) for *name*.
        Returns (frame_id, binding_id) of the defining binding, or None if not found.
        """
        for scope in reversed(scope_stack):
            if name in scope:
                return scope[name]
        return None

    def _walk(
        self,
        ir: MenaiIRExpr,
        result: IRUseCounts,
        scope_stack: List[Dict[str, Tuple[int, int]]],
        current_frame_id: int,
    ) -> None:
        """Recursively walk *ir*, updating *result* in place."""

        if isinstance(ir, MenaiIRVariable):
            self._walk_variable(ir, result, scope_stack)

        elif isinstance(ir, MenaiIRLambda):
            self._walk_lambda(ir, result, scope_stack, current_frame_id)

        elif isinstance(ir, MenaiIRLet):
            self._walk_let(ir, result, scope_stack, current_frame_id)

        elif isinstance(ir, MenaiIRLetrec):
            self._walk_letrec(ir, result, scope_stack, current_frame_id)

        elif isinstance(ir, MenaiIRIf):
            self._walk(ir.condition_plan, result, scope_stack, current_frame_id)
            self._walk(ir.then_plan, result, scope_stack, current_frame_id)
            self._walk(ir.else_plan, result, scope_stack, current_frame_id)

        elif isinstance(ir, MenaiIRCall):
            self._walk(ir.func_plan, result, scope_stack, current_frame_id)
            for arg in ir.arg_plans:
                self._walk(arg, result, scope_stack, current_frame_id)

        elif isinstance(ir, MenaiIRReturn):
            self._walk(ir.value_plan, result, scope_stack, current_frame_id)

        elif isinstance(ir, MenaiIRTrace):
            for msg in ir.message_plans:
                self._walk(msg, result, scope_stack, current_frame_id)
            self._walk(ir.value_plan, result, scope_stack, current_frame_id)

        elif isinstance(ir, (MenaiIRConstant, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            pass  # Leaf nodes — nothing to count.

        else:
            raise TypeError(f"MenaiIRUseCounter: unhandled IR node type {type(ir).__name__}")

    def _walk_variable(
        self,
        ir: MenaiIRVariable,
        result: IRUseCounts,
        scope_stack: List[Dict[str, Tuple[int, int]]],
    ) -> None:
        """Count a variable reference."""
        if ir.var_type != 'local':
            return  # Globals have no binding to count.

        resolved = self._resolve_name(ir.name, scope_stack)
        if resolved is not None:
            frame_id, binding_id = resolved
            self._inc(result, frame_id, binding_id)

    def _walk_lambda(
        self,
        ir: MenaiIRLambda,
        result: IRUseCounts,
        scope_stack: List[Dict[str, Tuple[int, int]]],
        current_frame_id: int,
    ) -> None:
        """
        Walk a lambda node.

        free_var_plans are evaluated in the enclosing frame — walk them with
        the current scope_stack.

        The lambda body is walked in a new frame with a new scope level
        containing the lambda's params and captured free vars.
        """
        # Assign a new frame to this lambda.
        lambda_frame_id = self._push_frame(result)
        result.lambda_frame_ids[id(ir)] = lambda_frame_id

        # Walk free_var_plans in the enclosing scope (they load from there).
        for fv_plan in ir.sibling_free_var_plans + ir.outer_free_var_plans:
            self._walk(fv_plan, result, scope_stack, current_frame_id)

        # Build the lambda's own scope: params + captured free vars → lambda_frame_id.
        # Each param/free-var gets a synthetic binding_id (a fresh negative integer
        # to avoid colliding with id() values from binding tuples).
        lambda_scope: Dict[str, Tuple[int, int]] = {}
        for name in ir.params + ir.sibling_free_vars + ir.outer_free_vars:
            synthetic_id = -id(ir) - hash(name)  # unique per (lambda, name)
            lambda_scope[name] = (lambda_frame_id, synthetic_id)

        child_stack = scope_stack + [lambda_scope]
        self._walk(ir.body_plan, result, child_stack, lambda_frame_id)

    def _walk_let(
        self,
        ir: MenaiIRLet,
        result: IRUseCounts,
        scope_stack: List[Dict[str, Tuple[int, int]]],
        current_frame_id: int,
    ) -> None:
        """
        Walk a let node.

        Binding values are walked in the current scope (parallel let semantics).
        The body is walked with a new scope level containing the let's bindings,
        all mapped to the current frame_id (let does not create a new frame).
        """
        for _name, value_plan, *_ in ir.bindings:
            self._walk(value_plan, result, scope_stack, current_frame_id)

        # Each binding tuple gets a unique binding_id via id().
        # The scope maps name → (frame_id, binding_id) so shadowed names are distinct.
        let_scope: Dict[str, Tuple[int, int]] = {
            name: (current_frame_id, id(binding))
            for binding in ir.bindings
            for name, *_ in [binding]
        }
        body_stack = scope_stack + [let_scope]
        self._walk(ir.body_plan, result, body_stack, current_frame_id)

    def _walk_letrec(
        self,
        ir: MenaiIRLetrec,
        result: IRUseCounts,
        scope_stack: List[Dict[str, Tuple[int, int]]],
        current_frame_id: int,
    ) -> None:
        """
        Walk a letrec node.

        All binding names are in scope for both binding values and the body
        (mutual recursion).  Like let, letrec does not create a new frame.
        """
        letrec_scope: Dict[str, Tuple[int, int]] = {
            name: (current_frame_id, id(binding))
            for binding in ir.bindings
            for name, *_ in [binding]
        }
        inner_stack = scope_stack + [letrec_scope]

        for _name, value_plan, *_ in ir.bindings:
            self._walk(value_plan, result, inner_stack, current_frame_id)

        self._walk(ir.body_plan, result, inner_stack, current_frame_id)
