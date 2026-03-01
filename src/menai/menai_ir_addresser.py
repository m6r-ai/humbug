"""
Menai IR Addresser - resolves variable names to frame-relative addresses.

This pass runs on the IR tree after MenaiIRBuilder has constructed it and
before any IR optimization passes or code generation.  It walks every
MenaiIRVariable node in the tree and fills in the depth and index fields,
which the IR builder leaves as sentinels (-1) to indicate "not yet resolved".

Background
----------
The IR builder constructs MenaiIRVariable nodes with only the name and
var_type fields populated.  depth and index are left as -1 (unresolved).
This separation means the IR tree structure — including the free_vars /
parent_refs split on MenaiIRLambda — is determined purely from the AST
without baking in frame-relative addresses that would be invalidated by
any subsequent structural transformation (e.g. closure conversion, lambda
lifting).

The addresser is the single place where frame-relative addresses are
computed.  It uses the same scope-chain logic that resolve_variable() used
to use inline in the IR builder.

Address model
-------------
- var_type='global': depth=0, index=0 (placeholder; codegen assigns the
  real name-table index when it emits LOAD_NAME).
- var_type='local', depth=0: slot in the current lambda frame.  index is
  the slot number within that frame.
- var_type='local', depth>0: slot in an ancestor frame.  depth is the
  number of lambda-frame boundaries to cross; index is the slot in that
  ancestor frame.  Emitted as LOAD_PARENT_VAR index depth.

Scope representation
--------------------
The addresser maintains a two-level scope structure:

  frame_stack: List[List[Dict[str, int]]]

Each element of frame_stack is a *lambda frame* — a list of scope dicts,
one per let/letrec nesting level within that lambda.  Lambda frames are
separated by lambda boundaries (depth increments).  Let/letrec forms push
a new scope dict onto the innermost lambda frame's list without creating a
new depth level.

Resolution: to look up a name, we search from the innermost scope dict
outward within the current lambda frame first (depth=0), then repeat for
each enclosing lambda frame (depth=1, 2, ...).

Slot indices on MenaiIRLet and MenaiIRLetrec bindings are already correct
(set by the IR builder) and are used directly to populate scope dicts.
The addresser does not re-allocate slots; it only resolves names to the
already-allocated slots.

Usage
-----
    addresser = MenaiIRAddresser()
    addressed_ir = addresser.address(ir)

The input ir must be a complete IR tree as produced by MenaiIRBuilder.build()
with all MenaiIRVariable.depth and .index fields set to -1 (unresolved).
The output is a new IR tree (the input is not mutated) with all
MenaiIRVariable nodes fully resolved.
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
ScopeDict = Dict[str, int]          # name -> slot index within a lambda frame
LambdaFrame = List[ScopeDict]       # stack of scope dicts within one lambda
FrameStack = List[LambdaFrame]      # stack of lambda frames (outermost first)


class MenaiIRAddresser:
    """
    Resolves all MenaiIRVariable(depth=-1, index=-1) nodes in an IR tree to
    their correct frame-relative addresses.

    The addresser is stateless between calls — all scope state is passed
    as immutable lists through the recursive walk (new lists are constructed
    rather than mutating existing ones).

    Usage::

        addressed_ir = MenaiIRAddresser().address(ir)
    """

    def address(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """
        Walk *ir* and return a new tree with all variable references resolved.

        Args:
            ir: IR tree produced by MenaiIRBuilder (variables have depth=-1,
                index=-1 for locals, or depth=0, index=0 for globals).

        Returns:
            New IR tree with all MenaiIRVariable nodes fully addressed.
        """
        # The top-level module scope: one lambda frame containing one empty scope dict.
        initial_frame_stack: FrameStack = [[{}]]
        return self._walk(ir, initial_frame_stack)

    def _walk(self, ir: MenaiIRExpr, frame_stack: FrameStack) -> MenaiIRExpr:
        """Recursively address *ir* in the context of *frame_stack*."""

        if isinstance(ir, MenaiIRVariable):
            return self._address_variable(ir, frame_stack)

        if isinstance(ir, MenaiIRLet):
            return self._walk_let(ir, frame_stack)

        if isinstance(ir, MenaiIRLetrec):
            return self._walk_letrec(ir, frame_stack)

        if isinstance(ir, MenaiIRLambda):
            return self._walk_lambda(ir, frame_stack)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._walk(ir.condition_plan, frame_stack),
                then_plan=self._walk(ir.then_plan, frame_stack),
                else_plan=self._walk(ir.else_plan, frame_stack),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRCall):
            return self._walk_call(ir, frame_stack)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._walk(ir.value_plan, frame_stack))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._walk(m, frame_stack) for m in ir.message_plans],
                value_plan=self._walk(ir.value_plan, frame_stack),
            )

        if isinstance(ir, (MenaiIRConstant, MenaiIRQuote, MenaiIREmptyList, MenaiIRError)):
            # Leaf nodes with no variable references — return unchanged.
            return ir

        raise TypeError(
            f"MenaiIRAddresser: unhandled IR node type {type(ir).__name__}"
        )

    def _address_variable(
        self, ir: MenaiIRVariable, frame_stack: FrameStack
    ) -> MenaiIRVariable:
        """
        Resolve a variable reference to its frame-relative address.

        For globals the node is returned unchanged (depth=0, index=0;
        the codegen assigns the real name-table index).

        For locals we search from the innermost scope outward across all
        lambda frames, counting lambda-frame boundaries crossed (depth).
        """
        if ir.var_type == 'global':
            # Global — no frame-relative address needed.
            return ir

        depth, index = self._resolve_local(ir.name, frame_stack)

        if depth == -1:
            # Name not found in any frame.  This should not happen after the
            # semantic analyser has validated the program.  Return the node
            # unchanged so downstream passes can report a clear error.
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
        Search *frame_stack* for *name*, returning (depth, index).

        depth counts lambda-frame boundaries crossed (0 = current lambda).
        Returns (-1, -1) if not found.

        Within each lambda frame we search the scope dicts from innermost
        (last) to outermost (first) — this handles let/letrec shadowing
        within the same lambda without incrementing depth.
        """
        for depth, lambda_frame in enumerate(reversed(frame_stack)):
            # Search scope dicts within this lambda frame, innermost first.
            for scope_dict in reversed(lambda_frame):
                if name in scope_dict:
                    return depth, scope_dict[name]

        return -1, -1

    def _walk_let(self, ir: MenaiIRLet, frame_stack: FrameStack) -> MenaiIRLet:
        """
        Walk a let node.

        Let bindings live in the same lambda frame as their enclosing context
        (let does not create a new VM frame / lambda boundary).

        Binding values are walked in the current scope (parallel let semantics
        — values cannot reference the let's own bindings).

        For the body we push a new scope dict onto the current lambda frame
        containing the let's bindings.  This correctly handles shadowing:
        the new scope dict sits on top of any existing scope dicts in the
        same lambda frame, and the old scope dicts are unaffected.
        """
        # Walk binding values in the current scope (names not yet visible).
        new_bindings: List[tuple] = []
        for name, value_plan, var_index in ir.bindings:
            new_value = self._walk(value_plan, frame_stack)
            new_bindings.append((name, new_value, var_index))

        # Build a new scope dict for the let's bindings.
        let_scope: ScopeDict = {name: var_index for name, _, var_index in new_bindings}

        # Push the new scope onto the current lambda frame (last in frame_stack).
        # We construct a new lambda frame list rather than mutating the existing
        # one, so the original frame_stack is not affected.
        current_lambda_frame = frame_stack[-1]
        body_lambda_frame = current_lambda_frame + [let_scope]
        body_frame_stack = frame_stack[:-1] + [body_lambda_frame]

        new_body = self._walk(ir.body_plan, body_frame_stack)

        return MenaiIRLet(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_letrec(self, ir: MenaiIRLetrec, frame_stack: FrameStack) -> MenaiIRLetrec:
        """
        Walk a letrec node.

        All binding names are in scope for both binding values and the body
        (mutual recursion).  Like let, letrec does not create a new VM frame.

        We push a new scope dict containing all the letrec's bindings onto
        the current lambda frame before walking both the binding values and
        the body.
        """
        letrec_scope: ScopeDict = {name: var_index for name, _, var_index in ir.bindings}

        current_lambda_frame = frame_stack[-1]
        inner_lambda_frame = current_lambda_frame + [letrec_scope]
        inner_frame_stack = frame_stack[:-1] + [inner_lambda_frame]

        new_bindings: List[tuple] = []
        for name, value_plan, var_index in ir.bindings:
            new_value = self._walk(value_plan, inner_frame_stack)
            new_bindings.append((name, new_value, var_index))

        new_body = self._walk(ir.body_plan, inner_frame_stack)

        return MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=new_body,
            in_tail_position=ir.in_tail_position,
        )

    def _walk_lambda(self, ir: MenaiIRLambda, frame_stack: FrameStack) -> MenaiIRLambda:
        """
        Walk a lambda node.

        free_var_plans and parent_ref_plans are evaluated in the *enclosing*
        frame (they load values from the enclosing scope to build the closure),
        so they are walked with the current frame_stack.

        The lambda body is walked with a new lambda frame pushed onto the
        frame_stack.  This new frame contains a single scope dict with the
        lambda's parameters and captured free variables.  Any reference that
        crosses this boundary increments depth by 1.

        Parent refs are NOT added to the lambda frame — they are accessed
        via LOAD_PARENT_VAR from the enclosing frame (depth > 0).
        """
        # Walk free_var_plans and parent_ref_plans in the enclosing frame.
        new_free_var_plans = [self._walk(p, frame_stack) for p in ir.free_var_plans]
        new_parent_ref_plans = [self._walk(p, frame_stack) for p in ir.parent_ref_plans]

        # Build the lambda's own scope dict.
        # Parameters occupy slots 0..N-1; captured free vars occupy N..N+M-1.
        lambda_scope: ScopeDict = {}
        for i, param in enumerate(ir.params):
            lambda_scope[param] = i

        param_count = len(ir.params)
        for i, free_var in enumerate(ir.free_vars):
            lambda_scope[free_var] = param_count + i

        # Parent refs are NOT in the lambda scope — they resolve to the
        # enclosing frame (depth > 0) via LOAD_PARENT_VAR.

        # Push a new lambda frame containing just the lambda's scope dict.
        child_frame_stack = frame_stack + [[lambda_scope]]

        new_body = self._walk(ir.body_plan, child_frame_stack)

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

    def _walk_call(self, ir: MenaiIRCall, frame_stack: FrameStack) -> MenaiIRCall:
        """Walk a call node."""
        new_args = [self._walk(a, frame_stack) for a in ir.arg_plans]

        return MenaiIRCall(
            func_plan=self._walk(ir.func_plan, frame_stack),
            arg_plans=new_args,
            is_tail_call=ir.is_tail_call,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )
