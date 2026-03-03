"""
Menai IR Addresser - resolves symbolic variable names to frame-relative slot addresses.

Walks every MenaiIRVariable node in the tree, fills in the depth and index
fields, and sets max_locals on every MenaiIRLambda.

Background
----------
All passes upstream of MenaiIRAddresser work with symbolic variables:
MenaiIRVariable nodes carry only name and var_type; depth and index are -1
(unresolved sentinels).  MenaiIRLet and MenaiIRLetrec binding tuples carry
only (name, value_plan) — no slot indices.  This design means every IR
transformation pass can freely restructure the tree without worrying about
stale addresses.

The addresser is the single place where:
  - slot indices are allocated for every let/letrec binding
  - variable references are resolved to (depth, index)
  - max_locals is computed for every lambda frame

Address model
-------------
- var_type='global': depth=0, index=0 (placeholder; codegen assigns the
  real name-table index when it emits LOAD_NAME).
- var_type='local', depth=0: slot in the current lambda frame.  index is
  the slot number within that frame.
- var_type='local', depth>0: slot in an ancestor frame.  depth is the
  number of lambda-frame boundaries to cross; index is the slot in that
  ancestor frame.  (depth>0 references do not currently arise after lambda
  lifting, since all captures are flattened into frame locals by that pass.)

Scope representation
--------------------
The addresser maintains:

  frame_stack: List[LambdaFrame]

Each LambdaFrame is a list of ScopeDicts.  A ScopeDict maps name → slot index
within the current lambda frame.  Multiple ScopeDicts in one LambdaFrame
represent nested let/letrec forms within the same lambda (no new VM frame).

A separate slot_counter_stack: List[List[int]] tracks the next free slot for
each lambda frame (one mutable [int] per frame so it can be updated as
bindings are allocated).

Slot allocation
---------------
Within each lambda frame, slots are allocated in order of first encounter:
  0 .. N-1          lambda params
  N .. N+S-1        sibling free vars (MAKE_CLOSURE / PATCH_CLOSURE)
  N+S .. N+S+O-1    outer free vars   (MAKE_CLOSURE)
  N+S+O ..          let/letrec bindings in tree order (depth-first)

max_locals
----------
After processing a lambda's body, the addresser sets max_locals on the
returned MenaiIRLambda node to the highest slot index used + 1.
"""

from typing import Dict, List, Tuple

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


# Type aliases for clarity
ScopeDict = Dict[str, int]      # name -> slot index within a lambda frame
LambdaFrame = List[ScopeDict]   # stack of scope dicts within one lambda
FrameStack = List[LambdaFrame]  # stack of lambda frames (outermost first)
SlotCounters = List[List[int]]  # one [next_slot] per lambda frame


class MenaiIRAddresser:
    """
    Resolves all symbolic MenaiIRVariable nodes to frame-relative addresses,
    allocates slot indices for all let/letrec bindings, and sets max_locals
    on every MenaiIRLambda.

    Runs once, after all IR transformation and optimisation passes.

    Usage::

        addressed_ir = MenaiIRAddresser().address(ir)
    """

    def address(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """
        Walk *ir* and return a new tree with all variables resolved and all
        slots allocated.

        Args:
            ir: IR tree after all transformation and optimisation passes.

        Returns:
            New IR tree with fully resolved MenaiIRVariable nodes and
            correct max_locals on every MenaiIRLambda.
        """
        # Top-level module frame: one lambda frame with one empty scope dict.
        # Slot counter starts at 0.
        initial_frame_stack: FrameStack = [[{}]]
        initial_counters: SlotCounters = [[0]]
        return self._walk(ir, initial_frame_stack, initial_counters)

    def _walk(
        self,
        ir: MenaiIRExpr,
        frame_stack: FrameStack,
        counters: SlotCounters,
    ) -> MenaiIRExpr:
        """Recursively address *ir*."""

        if isinstance(ir, MenaiIRVariable):
            return self._address_variable(ir, frame_stack)

        if isinstance(ir, MenaiIRLet):
            return self._walk_let(ir, frame_stack, counters)

        if isinstance(ir, MenaiIRLetrec):
            return self._walk_letrec(ir, frame_stack, counters)

        if isinstance(ir, MenaiIRLambda):
            return self._walk_lambda(ir, frame_stack, counters)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._walk(ir.condition_plan, frame_stack, counters),
                then_plan=self._walk(ir.then_plan, frame_stack, counters),
                else_plan=self._walk(ir.else_plan, frame_stack, counters),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRCall):
            return MenaiIRCall(
                func_plan=self._walk(ir.func_plan, frame_stack, counters),
                arg_plans=[self._walk(a, frame_stack, counters) for a in ir.arg_plans],
                is_tail_call=ir.is_tail_call,
                is_builtin=ir.is_builtin,
                builtin_name=ir.builtin_name,
            )

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._walk(ir.value_plan, frame_stack, counters))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._walk(m, frame_stack, counters) for m in ir.message_plans],
                value_plan=self._walk(ir.value_plan, frame_stack, counters),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            return ir

        raise TypeError(
            f"MenaiIRAddresser: unhandled IR node type {type(ir).__name__}"
        )

    def _address_variable(self, ir: MenaiIRVariable, frame_stack: FrameStack) -> MenaiIRVariable:
        """Resolve a variable reference to its frame-relative address."""
        if ir.var_type == 'global':
            return ir

        depth, index = self._resolve_local(ir.name, frame_stack)

        if depth == -1:
            # Not found — should not happen after semantic analysis.
            return ir

        return MenaiIRVariable(
            name=ir.name,
            var_type='local',
            depth=depth,
            index=index,
            is_parent_ref=ir.is_parent_ref,
        )

    def _resolve_local(self, name: str, frame_stack: FrameStack) -> Tuple[int, int]:
        """
        Search frame_stack for *name*, returning (depth, index).

        depth counts lambda-frame boundaries crossed (0 = current lambda).
        Returns (-1, -1) if not found.
        """
        for depth, lambda_frame in enumerate(reversed(frame_stack)):
            for scope_dict in reversed(lambda_frame):
                if name in scope_dict:
                    return depth, scope_dict[name]

        return -1, -1

    def _walk_let(
        self,
        ir: MenaiIRLet,
        frame_stack: FrameStack,
        counters: SlotCounters,
    ) -> MenaiIRLet:
        """
        Walk a let node, allocating a slot for each binding.

        Binding values are walked in the current scope (parallel let semantics).
        The body is walked with a new scope dict pushed onto the current lambda
        frame containing the newly-allocated slots.
        """
        # Walk binding values in the current scope (names not yet visible).
        new_bindings: List[Tuple[str, MenaiIRExpr]] = []
        allocated: List[Tuple[str, int]] = []
        for name, value_plan in ir.bindings:
            new_value = self._walk(value_plan, frame_stack, counters)
            slot = self._alloc_slot(counters)
            new_bindings.append((name, new_value))
            allocated.append((name, slot))

        # Build scope dict for the body.
        let_scope: ScopeDict = dict(allocated)

        current_lambda_frame = frame_stack[-1]
        body_lambda_frame = current_lambda_frame + [let_scope]
        body_frame_stack = frame_stack[:-1] + [body_lambda_frame]

        new_body = self._walk(ir.body_plan, body_frame_stack, counters)

        # Reconstruct with allocated slots embedded in the binding tuples.
        # The codegen reads slot indices from the scope dict via the frame_stack,
        # but we also need to pass them to the codegen via the IR node.
        # We store them as a parallel list by rebuilding the bindings as
        # (name, value_plan, slot) — but wait: MenaiIRLet.bindings is now
        # List[tuple[str, MenaiIRExpr]] with no slot.  The codegen must get
        # slot indices from the addresser's output somehow.
        #
        # Solution: we extend the binding tuple back to (name, value_plan, slot)
        # only in the addressed output that the codegen sees.  We do this by
        # returning a _MenaiIRLetAddressed node — but that would require a new
        # IR node type.
        #
        # Simpler: keep MenaiIRLet.bindings as List[tuple[str, MenaiIRExpr, int]]
        # in the *addressed* output only.  The addresser fills in the int.
        # Upstream passes (which never read the int) use the two-tuple form.
        #
        # We implement this by having MenaiIRLet accept both forms: the codegen
        # always unpacks three elements; upstream passes always produce two.
        # Python tuples are heterogeneous so this works at runtime, but it
        # is not type-safe.  A cleaner approach: use a separate addressed IR
        # type.  For now we store the slot in the binding tuple as a third
        # element so the codegen can read it, matching what the codegen expects.
        addressed_bindings = [
            (name, value_plan, slot)
            for (name, value_plan), (_, slot) in zip(new_bindings, allocated)
        ]

        return MenaiIRLet(
            bindings=addressed_bindings,  # type: ignore[arg-type]
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_letrec(
        self,
        ir: MenaiIRLetrec,
        frame_stack: FrameStack,
        counters: SlotCounters,
    ) -> MenaiIRLetrec:
        """
        Walk a letrec node, allocating a slot for each binding.

        All binding names are in scope for both binding values and the body
        (mutual recursion).
        """
        # Allocate slots for all bindings up front.
        allocated: List[Tuple[str, int]] = []
        for name, _ in ir.bindings:
            slot = self._alloc_slot(counters)
            allocated.append((name, slot))

        letrec_scope: ScopeDict = dict(allocated)

        current_lambda_frame = frame_stack[-1]
        inner_lambda_frame = current_lambda_frame + [letrec_scope]
        inner_frame_stack = frame_stack[:-1] + [inner_lambda_frame]

        new_bindings: List[Tuple[str, MenaiIRExpr]] = []
        for (name, value_plan), (_, slot) in zip(ir.bindings, allocated):
            new_value = self._walk(value_plan, inner_frame_stack, counters)
            new_bindings.append((name, new_value))

        new_body = self._walk(ir.body_plan, inner_frame_stack, counters)

        addressed_bindings = [
            (name, value_plan, slot)
            for (name, value_plan), (_, slot) in zip(new_bindings, allocated)
        ]

        return MenaiIRLetrec(
            bindings=addressed_bindings,  # type: ignore[arg-type]
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_lambda(
        self,
        ir: MenaiIRLambda,
        frame_stack: FrameStack,
        counters: SlotCounters,
    ) -> MenaiIRLambda:
        """
        Walk a lambda node.

        free_var_plans are evaluated in the enclosing frame — walk them with
        the current frame_stack and counters.

        The lambda body is walked with a new lambda frame pushed onto
        frame_stack.  Slot layout within the new frame:
            0 .. N-1          params
            N .. N+S-1        sibling_free_vars
            N+S .. N+S+O-1    outer_free_vars
            N+S+O ..          let/letrec bindings (allocated as encountered)

        max_locals is set to the highest slot allocated in this frame + 1.
        """
        # Walk free_var_plans in the enclosing frame.
        new_sibling_fvp = [self._walk(p, frame_stack, counters) for p in ir.sibling_free_var_plans]
        new_outer_fvp = [self._walk(p, frame_stack, counters) for p in ir.outer_free_var_plans]

        # Build the lambda's own scope dict: params first, then free vars.
        lambda_scope: ScopeDict = {}
        slot = 0
        for name in ir.params:
            lambda_scope[name] = slot
            slot += 1

        for name in ir.sibling_free_vars + ir.outer_free_vars:
            lambda_scope[name] = slot
            slot += 1

        # Push a new lambda frame with its own slot counter.
        child_frame_stack = frame_stack + [[lambda_scope]]
        child_counters = counters + [[slot]]  # next free slot after params+free_vars

        new_body = self._walk(ir.body_plan, child_frame_stack, child_counters)

        # max_locals = highest slot used in this frame + 1.
        max_locals = child_counters[-1][0]  # counter was incremented for each allocated slot

        return MenaiIRLambda(
            params=ir.params,
            body_plan=new_body,
            sibling_free_vars=ir.sibling_free_vars,
            sibling_free_var_plans=new_sibling_fvp,
            outer_free_vars=ir.outer_free_vars,
            outer_free_var_plans=new_outer_fvp,
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            max_locals=max_locals,
            binding_name=ir.binding_name,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _alloc_slot(self, counters: SlotCounters) -> int:
        """
        Allocate the next free slot in the current (innermost) lambda frame.

        counters[-1] is a one-element list [next_slot] for the current frame.
        We mutate it in place so that all code sharing this counters list
        sees the updated value.
        """
        slot = counters[-1][0]
        counters[-1][0] = slot + 1
        return slot
