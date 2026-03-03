"""
VM code generator for the Menai compiler.

Translates a MenaiCFGFunction (SSA CFG) into a CodeObject ready for
execution by the Menai VM.

Position in the pipeline
------------------------
    MenaiCFGFunction  →  MenaiVMCodeGen  →  CodeObject

This pass replaces MenaiCodeGen and MenaiCodeGenContext entirely.

SSA-to-stack mapping
--------------------
The VM is a stack machine, but the CFG uses explicit SSA values.  We bridge
this by assigning each MenaiCFGValue a local variable slot, then emitting
STORE_VAR after each instruction that produces a value and LOAD_VAR wherever
that value is consumed.

This is deliberately naive — it produces correct code and gives the existing
test suite something to run against.  A future peephole pass or smarter
slot allocator can eliminate redundant STORE/LOAD pairs.

Slot layout (mirrors MenaiIRAddresser)
--------------------------------------
Within each lambda frame:
    0 .. P-1          parameters          (MenaiCFGParamInstr)
    P .. P+F-1        captured free vars  (MenaiCFGFreeVarInstr)
    P+F ..            all other SSA values, allocated in definition order

Phi nodes
---------
A MenaiCFGPhiInstr emits no instructions.  Both predecessor blocks store
their result into the phi's slot before jumping to the join block.  The join
block then simply loads from that slot (via the normal LOAD_VAR path) when
the phi value is used downstream.

The mechanism: when we assign a slot to a phi value, we also record that
each predecessor block's "outgoing value" (the value it contributes to the
phi) must be stored into that same slot before its terminator jump.  We
handle this via a _phi_stores dict: block_id → list of (value, slot) pairs
to emit as STORE_VAR before the block's terminator.

Block ordering
--------------
We emit blocks in the order they appear in MenaiCFGFunction.blocks (which
is construction order from the builder).  The builder always appends:
  entry, then_N, else_N, join_N for each if-expression, in nesting order.
This means the fall-through from a branch's true-block to its join is never
adjacent in the linear order (we always emit an explicit JUMP), which is
fine — the VM handles it correctly.

letrec patching
---------------
MenaiCFGPatchClosureInstr in block.patch_instrs is emitted after the block's
regular instructions and before its terminator.  The VM PATCH_CLOSURE opcode
takes (closure_slot, capture_index) and pops the value from the stack, so
we load the value, then emit PATCH_CLOSURE with the closure's slot and the
capture index.

self-loop (direct tail recursion)
----------------------------------
MenaiCFGSelfLoopTerm stores the new argument values into parameter slots
0..N-1 then emits JUMP 0 (back to the entry block).  The entry block's
ENTER instruction re-reads the slots, so we must NOT re-emit ENTER on
the loop-back path — JUMP 0 lands at instruction index 0 which is ENTER,
and ENTER pops from the *locals* array, not the stack.  Wait — actually the
VM ENTER opcode pops N values off the stack into locals.  On the loop-back
path (JUMP 0) the stack is empty, so we must store new arg values into the
parameter slots directly (STORE_VAR 0..N-1) and then JUMP past ENTER, or
store into the slots and jump to instruction 1.

Re-reading the VM design: JUMP 0 is the canonical self-tail-call target and
the VM's ENTER opcode is only executed on function entry (it pops from the
call stack frame set up by CALL/TAIL_CALL).  The correct pattern is:
  - Evaluate new args onto the stack
  - JUMP 0  (which re-executes ENTER, which pops the args back into slots)
But that only works if the args are on the stack in the right order when
ENTER fires.  Looking at the existing codegen: it pushes new arg values
onto the stack and then emits JUMP 0, so ENTER re-runs and re-pops them.
We follow the same convention.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

from menai.menai_bytecode import BUILTIN_OPCODE_MAP, CodeObject, Instruction, Opcode
from menai.menai_cfg import (
    MenaiCFGApplyInstr,
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGBuiltinInstr,
    MenaiCFGCallInstr,
    MenaiCFGConstInstr,
    MenaiCFGFreeVarInstr,
    MenaiCFGFunction,
    MenaiCFGGlobalInstr,
    MenaiCFGInstr,
    MenaiCFGJumpTerm,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPatchClosureInstr,
    MenaiCFGParamInstr,
    MenaiCFGPhiInstr,
    MenaiCFGRaiseTerm,
    MenaiCFGReturnTerm,
    MenaiCFGSelfLoopTerm,
    MenaiCFGTailApplyTerm,
    MenaiCFGTailCallTerm,
    MenaiCFGTerminator,
    MenaiCFGTraceInstr,
    MenaiCFGValue,
)
from menai.menai_value import (
    MenaiBoolean,
    MenaiComplex,
    MenaiFloat,
    MenaiFunction,
    MenaiInteger,
    MenaiList,
    MenaiNone,
    MenaiString,
    MenaiValue,
)


# Derived opcode maps — same as in the old MenaiCodeGen.
UNARY_OPS  = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 1}
BINARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 2}
TERNARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 3}

BUILD_OPS = {
    'list': Opcode.LIST,
    'dict': Opcode.DICT,
}


@dataclass
class _EmitContext:
    """
    Mutable state for emitting one MenaiCFGFunction into a CodeObject.

    Tracks the instruction stream, constant/name pools, nested code objects,
    and the SSA-value-to-slot mapping.
    """
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[MenaiValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    code_objects: List[CodeObject] = field(default_factory=list)
    constant_map: Dict[tuple, int] = field(default_factory=dict)
    name_map: Dict[str, int] = field(default_factory=dict)

    # SSA value id → local slot index
    slot_map: Dict[int, int] = field(default_factory=dict)
    next_slot: int = 0

    # Phi result id → slot, populated lazily on first store by a predecessor.
    # Kept separate so we can assert that a phi slot is allocated before the
    # join block tries to read it (via slot_of on the phi result).
    phi_slot_map: Dict[int, int] = field(default_factory=dict)

    # Name of the current function (for self-loop detection label)
    current_lambda_name: Optional[str] = None

    def alloc_slot(self, value: MenaiCFGValue) -> int:
        """Allocate the next free slot for `value` and record the mapping."""
        slot = self.next_slot
        self.next_slot += 1
        self.slot_map[value.id] = slot
        return slot

    def assign_slot(self, value: MenaiCFGValue, slot: int) -> None:
        """Assign a specific slot to `value` (used for params/free-vars)."""
        self.slot_map[value.id] = slot
        if slot >= self.next_slot:
            self.next_slot = slot + 1

    def slot_of(self, value: MenaiCFGValue) -> int:
        """Return the slot assigned to `value`.  Asserts it exists."""
        assert value.id in self.slot_map, (
            f"MenaiVMCodeGen: SSA value {value} has no assigned slot"
        )
        return self.slot_map[value.id]

    def emit(self, opcode: Opcode, arg1: int = 0, arg2: int = 0) -> int:
        """Emit an instruction, returning its index."""
        idx = len(self.instructions)
        self.instructions.append(Instruction(opcode, arg1, arg2))
        return idx

    def patch_jump(self, instr_index: int, target: int) -> None:
        self.instructions[instr_index].arg1 = target

    def current_index(self) -> int:
        return len(self.instructions)

    def add_constant(self, value: MenaiValue) -> int:
        if isinstance(value, (MenaiInteger, MenaiFloat, MenaiComplex,
                               MenaiBoolean, MenaiString)):
            key: tuple = (type(value).__name__, value.value)
        else:
            key = (id(value),)
        if key in self.constant_map:
            return self.constant_map[key]
        idx = len(self.constants)
        self.constants.append(value)
        self.constant_map[key] = idx
        return idx

    def add_name(self, name: str) -> int:
        if name in self.name_map:
            return self.name_map[name]
        idx = len(self.names)
        self.names.append(name)
        self.name_map[name] = idx
        return idx

    def add_code_object(self, code_obj: CodeObject) -> int:
        idx = len(self.code_objects)
        self.code_objects.append(code_obj)
        return idx


class MenaiVMCodeGen:
    """
    Generates a CodeObject from a MenaiCFGFunction.

    Usage::

        code_obj = MenaiVMCodeGen().generate(cfg_function, name="<module>")
    """

    def __init__(self) -> None:
        self._lambda_counter = 0

    def generate(self, func: MenaiCFGFunction, name: str = "<module>") -> CodeObject:
        """
        Generate a top-level CodeObject from a MenaiCFGFunction.

        Args:
            func: The CFG function to compile (top-level module body).
            name: Name for the resulting CodeObject.

        Returns:
            A CodeObject ready for execution by the Menai VM.
        """
        ctx = _EmitContext(current_lambda_name=func.binding_name)
        self._emit_function_body(func, ctx)
        return CodeObject(
            instructions=ctx.instructions,
            constants=ctx.constants,
            names=ctx.names,
            code_objects=ctx.code_objects,
            param_count=0,
            local_count=ctx.next_slot,
            name=name,
        )

    # ------------------------------------------------------------------
    # Function body emission
    # ------------------------------------------------------------------

    def _emit_function_body(self, func: MenaiCFGFunction, ctx: _EmitContext) -> None:
        """
        Emit all blocks of `func` into `ctx` in reverse post-order (RPO).

        Phase 1: assign fixed slots to all params and free-var values.
        Phase 2: emit each block's instructions, patch_instrs, and terminator.
        Phase 3: back-patch all forward jump targets.

        RPO guarantees that all predecessors of a block are emitted before
        the block itself (for non-back-edges / self-loops).  This is required
        for the lazy phi-slot allocation scheme: when a predecessor emits its
        JumpTerm, it calls _emit_phi_stores_for_successor which allocates the
        phi slot; by the time the join block is emitted, the phi slot exists.
        """
        # Phase 1: assign fixed slots to params and free-vars only.
        # Phi slots are allocated lazily in _emit_phi_stores_for_successor.
        param_count = len(func.params)

        for block in func.blocks:
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGParamInstr):
                    ctx.assign_slot(instr.result, instr.index)
                elif isinstance(instr, MenaiCFGFreeVarInstr):
                    ctx.assign_slot(instr.result, param_count + instr.index)

        # Phase 2: emit blocks.
        # We need to back-patch jump targets, so we collect
        # (instr_index, target_block) pairs as we go.
        block_start: Dict[int, int] = {}   # block id → instruction index of first instr
        forward_jumps: List[Tuple[int, MenaiCFGBlock]] = []  # (instr_idx, target_block)

        for block in self._rpo(func):
            block_start[block.id] = ctx.current_index()
            self._emit_block(block, ctx, forward_jumps, func)

        # Phase 3: back-patch forward jumps.
        for instr_idx, target_block in forward_jumps:
            ctx.patch_jump(instr_idx, block_start[target_block.id])

    def _rpo(self, func: MenaiCFGFunction) -> List['MenaiCFGBlock']:
        """
        Return the reachable blocks of `func` in reverse post-order.

        RPO is computed by DFS from the entry block, collecting blocks in
        post-order (appended after all successors are visited), then reversing.
        Self-loop back-edges (SelfLoopTerm → entry) are not followed.
        Unreachable blocks (no path from entry) are excluded.
        """
        visited: set = set()
        post_order: List[MenaiCFGBlock] = []

        def dfs(block: MenaiCFGBlock) -> None:
            if block.id in visited:
                return
            visited.add(block.id)
            term = block.terminator
            if isinstance(term, MenaiCFGJumpTerm):
                dfs(term.target)
            elif isinstance(term, MenaiCFGBranchTerm):
                dfs(term.true_block)
                dfs(term.false_block)
            # ReturnTerm, TailCallTerm, TailApplyTerm, SelfLoopTerm, RaiseTerm
            # have no successors (SelfLoopTerm loops to entry but we don't
            # follow it — it's a back-edge handled by JUMP 0).
            post_order.append(block)

        dfs(func.entry)
        post_order.reverse()
        return post_order

    def _emit_block(
        self,
        block: MenaiCFGBlock,
        ctx: _EmitContext,
        forward_jumps: List[Tuple[int, MenaiCFGBlock]],
        func: MenaiCFGFunction,
    ) -> None:
        """Emit all instructions and the terminator for one block."""
        # Regular instructions.
        for instr in block.instrs:
            self._emit_instr(instr, ctx)

        # Patch instructions (letrec closure fixup).
        for patch in block.patch_instrs:
            self._emit_patch(patch, ctx)

        # Terminator.
        assert block.terminator is not None, (
            f"MenaiVMCodeGen: block {block.id} ({block.label}) has no terminator"
        )
        self._emit_terminator(block, block.terminator, ctx, forward_jumps, func)

    # ------------------------------------------------------------------
    # Instruction emission
    # ------------------------------------------------------------------

    def _emit_instr(self, instr: MenaiCFGInstr, ctx: _EmitContext) -> None:
        """Emit a single non-terminator instruction."""
        if isinstance(instr, MenaiCFGParamInstr):
            # Params are already in their slots via ENTER; nothing to emit.
            return

        if isinstance(instr, MenaiCFGFreeVarInstr):
            # Free vars are in their slots after ENTER loads them; nothing to emit.
            return

        if isinstance(instr, MenaiCFGPhiInstr):
            # Phi nodes emit no instructions.  The phi result slot is
            # allocated lazily by _emit_phi_stores_for_successor when a
            # predecessor stores into it.  By the time the join block's
            # instructions run, the slot is guaranteed to exist in slot_map.
            if instr.result.id in ctx.phi_slot_map:
                ctx.slot_map[instr.result.id] = ctx.phi_slot_map[instr.result.id]
            return

        if isinstance(instr, MenaiCFGConstInstr):
            self._emit_load_value(instr.value, ctx)
            slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.STORE_VAR, slot)
            return

        if isinstance(instr, MenaiCFGGlobalInstr):
            name_idx = ctx.add_name(instr.name)
            ctx.emit(Opcode.LOAD_NAME, name_idx)
            slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.STORE_VAR, slot)
            return

        if isinstance(instr, MenaiCFGBuiltinInstr):
            self._emit_builtin(instr, ctx)
            return

        if isinstance(instr, MenaiCFGCallInstr):
            for arg in instr.args:
                ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(arg))
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(instr.func))
            ctx.emit(Opcode.CALL, len(instr.args))
            slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.STORE_VAR, slot)
            return

        if isinstance(instr, MenaiCFGApplyInstr):
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(instr.func))
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(instr.arg_list))
            ctx.emit(Opcode.APPLY)
            slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.STORE_VAR, slot)
            return

        if isinstance(instr, MenaiCFGMakeClosureInstr):
            self._emit_make_closure(instr, ctx)
            return

        if isinstance(instr, MenaiCFGTraceInstr):
            for msg in instr.messages:
                ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(msg))
                ctx.emit(Opcode.EMIT_TRACE)
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(instr.value))
            slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.STORE_VAR, slot)
            return

        if isinstance(instr, MenaiCFGPatchClosureInstr):
            # Patch instructions may appear in block.instrs (letrec sibling
            # captures emitted before the body) as well as block.patch_instrs.
            # Delegate to the shared helper.
            self._emit_patch(instr, ctx)
            return

        raise TypeError(f"MenaiVMCodeGen: unhandled instruction {type(instr).__name__}")

    def _emit_phi_stores_for_successor(
        self,
        current_block: MenaiCFGBlock,
        successor: MenaiCFGBlock,
        ctx: _EmitContext,
    ) -> None:
        """
        Before jumping to `successor`, store each incoming phi value that
        this block contributes into the phi's pre-assigned slot.

        Called by jump/branch terminator emission immediately before the
        JUMP instruction so that the incoming value's slot is guaranteed
        to exist (it was defined earlier in this block's instruction stream).
        """
        for instr in successor.instrs:
            if not isinstance(instr, MenaiCFGPhiInstr):
                break
            for incoming_val, pred_block in instr.incoming:
                if pred_block.id == current_block.id:
                    # Allocate the phi slot lazily on first store.
                    if instr.result.id not in ctx.phi_slot_map:
                        phi_slot = ctx.alloc_slot(instr.result)
                        ctx.phi_slot_map[instr.result.id] = phi_slot
                    else:
                        phi_slot = ctx.phi_slot_map[instr.result.id]
                    src_slot = ctx.slot_of(incoming_val)
                    ctx.emit(Opcode.LOAD_VAR, src_slot)
                    ctx.emit(Opcode.STORE_VAR, phi_slot)

    def _emit_patch(self, patch: MenaiCFGPatchClosureInstr, ctx: _EmitContext) -> None:
        """Emit a PATCH_CLOSURE instruction for a letrec fixup."""
        closure_slot = ctx.slot_of(patch.closure)
        ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(patch.value))
        ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, patch.capture_index)

    # ------------------------------------------------------------------
    # Terminator emission
    # ------------------------------------------------------------------

    def _emit_terminator(
        self,
        block: MenaiCFGBlock,
        term: MenaiCFGTerminator,
        ctx: _EmitContext,
        forward_jumps: List[Tuple[int, MenaiCFGBlock]],
        func: MenaiCFGFunction,
    ) -> None:
        if isinstance(term, MenaiCFGReturnTerm):
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(term.value))
            ctx.emit(Opcode.RETURN)
            return

        if isinstance(term, MenaiCFGJumpTerm):
            self._emit_phi_stores_for_successor(block, term.target, ctx)
            jump_idx = ctx.emit(Opcode.JUMP, 0)
            forward_jumps.append((jump_idx, term.target))
            return

        if isinstance(term, MenaiCFGBranchTerm):
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(term.cond))
            false_jump_idx = ctx.emit(Opcode.JUMP_IF_FALSE, 0)
            # True branch: explicit jump (phi stores happen in then/else blocks
            # via their own MenaiCFGJumpTerm to join_block).
            true_jump_idx = ctx.emit(Opcode.JUMP, 0)
            forward_jumps.append((true_jump_idx, term.true_block))
            forward_jumps.append((false_jump_idx, term.false_block))
            return

        if isinstance(term, MenaiCFGTailCallTerm):
            for arg in term.args:
                ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(arg))
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(term.func))
            ctx.emit(Opcode.TAIL_CALL, len(term.args))
            return

        if isinstance(term, MenaiCFGTailApplyTerm):
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(term.func))
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(term.arg_list))
            ctx.emit(Opcode.TAIL_APPLY)
            return

        if isinstance(term, MenaiCFGSelfLoopTerm):
            # Push new arg values onto the stack in order, then JUMP 0.
            # ENTER at instruction 0 will pop them into param slots.
            for arg in term.args:
                ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(arg))
            ctx.emit(Opcode.JUMP, 0)
            return

        if isinstance(term, MenaiCFGRaiseTerm):
            const_idx = ctx.add_constant(term.message)
            ctx.emit(Opcode.RAISE_ERROR, const_idx)
            return

        raise TypeError(f"MenaiVMCodeGen: unhandled terminator {type(term).__name__}")

    # ------------------------------------------------------------------
    # Builtin emission
    # ------------------------------------------------------------------

    def _emit_builtin(self, instr: MenaiCFGBuiltinInstr, ctx: _EmitContext) -> None:
        """
        Emit a builtin operation.  Mirrors the special-case logic from the
        old MenaiCodeGen._generate_call for builtin names.
        """
        op = instr.op
        args = instr.args

        def load_arg(i: int) -> None:
            ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(args[i]))

        def load_all() -> None:
            for a in args:
                ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(a))

        # Special cases with optional / synthesised arguments.
        if op == 'range':
            load_all()
            if len(args) == 2:
                ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(1)))
            ctx.emit(Opcode.RANGE)

        elif op == 'integer->complex':
            load_arg(0)
            if len(args) == 1:
                ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(0)))
            else:
                load_arg(1)
            ctx.emit(Opcode.INTEGER_TO_COMPLEX)

        elif op == 'integer->string':
            load_arg(0)
            if len(args) == 1:
                ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(10)))
            else:
                load_arg(1)
            ctx.emit(Opcode.INTEGER_TO_STRING)

        elif op == 'float->complex':
            load_arg(0)
            if len(args) == 1:
                ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiFloat(0.0)))
            else:
                load_arg(1)
            ctx.emit(Opcode.FLOAT_TO_COMPLEX)

        elif op == 'string->integer':
            load_arg(0)
            if len(args) == 1:
                ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiInteger(10)))
            else:
                load_arg(1)
            ctx.emit(Opcode.STRING_TO_INTEGER)

        elif op == 'string-slice':
            load_arg(0)
            load_arg(1)
            if len(args) == 2:
                # Default end = string-length(str)
                load_arg(0)
                ctx.emit(Opcode.STRING_LENGTH)
            else:
                load_arg(2)
            ctx.emit(Opcode.STRING_SLICE)

        elif op == 'string->list':
            load_arg(0)
            if len(args) == 1:
                ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiString("")))
            else:
                load_arg(1)
            ctx.emit(Opcode.STRING_TO_LIST)

        elif op == 'list-slice':
            load_arg(0)
            load_arg(1)
            if len(args) == 2:
                # Default end = list-length(lst)
                load_arg(0)
                ctx.emit(Opcode.LIST_LENGTH)
            else:
                load_arg(2)
            ctx.emit(Opcode.LIST_SLICE)

        elif op == 'list->string':
            load_arg(0)
            if len(args) == 1:
                ctx.emit(Opcode.LOAD_CONST, ctx.add_constant(MenaiString("")))
            else:
                load_arg(1)
            ctx.emit(Opcode.LIST_TO_STRING)

        elif op == 'dict-get':
            load_all()
            if len(args) == 2:
                ctx.emit(Opcode.LOAD_NONE)
            ctx.emit(Opcode.DICT_GET)

        elif op in BINARY_OPS:
            load_all()
            ctx.emit(BINARY_OPS[op])

        elif op in UNARY_OPS:
            load_all()
            ctx.emit(UNARY_OPS[op])

        elif op in TERNARY_OPS:
            load_all()
            ctx.emit(TERNARY_OPS[op])

        elif op in BUILD_OPS:
            load_all()
            ctx.emit(BUILD_OPS[op], len(args))

        else:
            raise ValueError(f"MenaiVMCodeGen: unknown builtin op {op!r}")

        # Store the result.
        slot = ctx.alloc_slot(instr.result)
        ctx.emit(Opcode.STORE_VAR, slot)

    # ------------------------------------------------------------------
    # Closure emission
    # ------------------------------------------------------------------

    def _emit_make_closure(
        self, instr: MenaiCFGMakeClosureInstr, ctx: _EmitContext
    ) -> None:
        """
        Emit the code to construct a closure or a plain function constant.

        Recursively generates the child CodeObject for instr.function, then
        emits:
          - MAKE_CLOSURE code_idx 0 followed by PATCH_CLOSURE for each outer
            capture, when needs_patching is True.  All free-var slots are
            pre-allocated as None; sibling captures are filled later by the
            letrec phase-3 patch_instrs, and outer captures are filled here
            immediately after MAKE_CLOSURE.  This mirrors the old codegen's
            letrec two-phase approach and ensures outer captures land at the
            correct slot indices (after the sibling slots), not at slot 0.
          - MAKE_CLOSURE with all eagerly-loaded captures packed from slot 0,
            when needs_patching is False but there are captures.
          - LOAD_CONST with a pre-built MenaiFunction, only when there are
            no captures AND needs_patching is False.
        """
        child_func = instr.function
        child_code = self._generate_lambda_code_object(child_func)
        code_idx = ctx.add_code_object(child_code)

        capture_count = len(instr.captures)
        total_free_vars = len(child_func.free_vars)

        if instr.needs_patching:
            # Letrec case: emit MAKE_CLOSURE with 0 captures so the VM
            # pre-allocates None for all free-var slots.  Then immediately
            # PATCH_CLOSURE the outer (non-sibling) captures into their
            # correct slot indices.  Sibling captures are patched later by
            # the block's patch_instrs (phase 3 of letrec).
            ctx.emit(Opcode.MAKE_CLOSURE, code_idx, 0)
            closure_slot = ctx.alloc_slot(instr.result)
            ctx.emit(Opcode.STORE_VAR, closure_slot)
            # Outer captures occupy the tail of free_vars: indices
            # [total_free_vars - capture_count .. total_free_vars - 1].
            outer_start = total_free_vars - capture_count
            for i, cap in enumerate(instr.captures):
                ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(cap))
                ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, outer_start + i)
            return
        elif capture_count > 0:
            # Non-letrec closure with captures: pack all from slot 0.
            for cap in instr.captures:
                ctx.emit(Opcode.LOAD_VAR, ctx.slot_of(cap))
            ctx.emit(Opcode.MAKE_CLOSURE, code_idx, capture_count)
        else:
            # No captures — pre-build a MenaiFunction and store as a constant.
            func_val = MenaiFunction(
                parameters=tuple(child_code.param_names),
                name=child_code.name,
                bytecode=child_code,
                is_variadic=child_code.is_variadic,
            )
            key = ('function', id(child_code))
            if key not in ctx.constant_map:
                ctx.constant_map[key] = len(ctx.constants)
                ctx.constants.append(func_val)
            ctx.emit(Opcode.LOAD_CONST, ctx.constant_map[key])

        slot = ctx.alloc_slot(instr.result)
        ctx.emit(Opcode.STORE_VAR, slot)

    def _generate_lambda_code_object(self, func: MenaiCFGFunction) -> CodeObject:
        """
        Recursively generate a CodeObject for a nested lambda MenaiCFGFunction.
        """
        child_ctx = _EmitContext(current_lambda_name=func.binding_name)

        param_count = len(func.params)
        free_var_count = len(func.free_vars)

        # Emit ENTER to pop parameters into slots 0..P-1.
        if param_count > 0:
            child_ctx.emit(Opcode.ENTER, param_count)

        # Reserve slots for params and free vars so alloc_slot starts above them.
        child_ctx.next_slot = param_count + free_var_count

        self._emit_function_body(func, child_ctx)

        # Build the human-readable name.
        if func.binding_name:
            lambda_name = func.binding_name
        else:
            lambda_name = f"<lambda-{self._lambda_counter}>"
            self._lambda_counter += 1

        param_word = "param" if param_count == 1 else "params"
        lambda_name = f"{lambda_name}({param_count} {param_word})"

        return CodeObject(
            instructions=child_ctx.instructions,
            constants=child_ctx.constants,
            names=child_ctx.names,
            code_objects=child_ctx.code_objects,
            free_vars=func.free_vars,
            param_names=func.params,
            param_count=param_count,
            local_count=child_ctx.next_slot,
            is_variadic=func.is_variadic,
            name=lambda_name,
            source_line=func.source_line,
            source_file=func.source_file,
        )

    # ------------------------------------------------------------------
    # Value loading helpers
    # ------------------------------------------------------------------

    def _emit_load_value(self, value: MenaiValue, ctx: _EmitContext) -> None:
        """Emit the appropriate LOAD instruction for a constant value."""
        if isinstance(value, MenaiNone):
            ctx.emit(Opcode.LOAD_NONE)
            return

        if isinstance(value, MenaiBoolean):
            ctx.emit(Opcode.LOAD_TRUE if value.value else Opcode.LOAD_FALSE)
            return

        if isinstance(value, MenaiList) and len(value.elements) == 0:
            ctx.emit(Opcode.LOAD_EMPTY_LIST)
            return

        const_idx = ctx.add_constant(value)
        ctx.emit(Opcode.LOAD_CONST, const_idx)
