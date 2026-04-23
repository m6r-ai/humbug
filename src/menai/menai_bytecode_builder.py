"""
Bytecode emitter for the Menai VM backend.

Translates a MenaiVCodeFunction into a CodeObject ready for execution by
the Menai VM.

This is the final pass of the VM backend pipeline:

    MenaiVCodeFunction
        → allocate_slots      (assign virtual registers to slots)
        → peephole            (eliminate redundant moves and jumps)
        → MenaiBytecodeBuilder (emit CodeObject)  ← this file

The emitter has no knowledge of SSA, phi nodes, or liveness.  It simply walks
the flat VCode instruction list and emits the corresponding bytecode, resolving
label references to instruction indices in a back-patch phase.

Move instructions are treated as a parallel assignment group whenever they
appear consecutively.  This is necessary because phi-elimination and self-loop
TCO both emit groups of moves that must be interpreted as simultaneous
assignments.  A sequencing algorithm detects cycles and breaks them with a
scratch slot so that the sequential bytecode MOVE instructions are correct.
"""

import array
from dataclasses import dataclass, field
from typing import Dict, List, Set, Tuple

from menai.menai_bytecode import BUILTIN_OPCODE_MAP, CodeObject, Opcode, pack_instruction, make_instructions_array
from menai.menai_error import MenaiCodegenError
from menai.menai_value import (
    MenaiBoolean,
    MenaiComplex,
    MenaiDict,
    MenaiFloat,
    MenaiFunction,
    MenaiInteger,
    MenaiList,
    MenaiNone,
    MenaiSet,
    MenaiString,
    MenaiValue,
)
from menai.menai_vcode import (
    MenaiVCodeApply,
    MenaiVCodeBuiltin,
    MenaiVCodeCall,
    MenaiVCodeFunction,
    MenaiVCodeJump,
    MenaiVCodeJumpIfFalse,
    MenaiVCodeJumpIfTrue,
    MenaiVCodeLabel,
    MenaiVCodeLoadConst,
    MenaiVCodeLoadName,
    MenaiVCodeMakeClosure,
    MenaiVCodeMove,
    MenaiVCodePatchClosure,
    MenaiVCodeMakeStruct,
    MenaiVCodeRaise,
    MenaiVCodeReg,
    MenaiVCodeReturn,
    MenaiVCodeTailApply,
    MenaiVCodeTailCall,
    MenaiVCodeTrace,
)
from menai.menai_vcode_allocator import SlotMap, allocate_slots
from menai.menai_vcode_peephole import peephole

from menai.menai_bytecode import (
    _OPCODE_SHIFT, _DEST_SHIFT, _SRC0_SHIFT, _SRC1_SHIFT,
    _FIELD_MASK, _OPCODE_MASK,
)


UNARY_OPS  = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 1}
BINARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 2}
TERNARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 3}


_FIELD_NAMES = ('opcode', 'dest', 'src0', 'src1', 'src2')
_SHIFTS = {
    'opcode': _OPCODE_SHIFT,
    'dest':   _DEST_SHIFT,
    'src0':   _SRC0_SHIFT,
    'src1':   _SRC1_SHIFT,
    'src2':   0,
}
_MASKS = {
    'opcode': _OPCODE_MASK,
    'dest':   _FIELD_MASK,
    'src0':   _FIELD_MASK,
    'src1':   _FIELD_MASK,
    'src2':   _FIELD_MASK,
}


def _patch_instruction(instructions: 'array.array[int]', idx: int, field_name: str, value: int) -> None:
    """Patch a single field in a packed instruction word in-place."""
    shift = _SHIFTS[field_name]
    mask  = _MASKS[field_name]
    word  = instructions[idx]
    word  = (word & ~(mask << shift)) | ((value & mask) << shift)
    instructions[idx] = word


@dataclass
class _EmitContext:
    """Mutable state for emitting one MenaiVCodeFunction into a CodeObject."""
    instructions: 'array.array[int]' = field(default_factory=make_instructions_array)
    constants: List[MenaiValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    code_objects: List[CodeObject] = field(default_factory=list)
    constant_map: Dict[tuple, int] = field(default_factory=dict)
    name_map: Dict[str, int] = field(default_factory=dict)
    slot_map: SlotMap = field(default_factory=lambda: SlotMap(slots={}, slot_count=0))
    max_outgoing_args: int = 0

    def slot_of(self, reg: MenaiVCodeReg) -> int:
        """Get the slot index assigned to reg."""
        return self.slot_map.slot_of(reg)

    def emit(self, opcode: Opcode, src0: int = 0, src1: int = 0, dest: int = 0, src2: int = 0) -> int:
        """Emit an instruction and return its index."""
        idx = len(self.instructions)
        self.instructions.append(pack_instruction(int(opcode), dest, src0, src1, src2))
        return idx

    def current_index(self) -> int:
        """Get the index of the next instruction to be emitted."""
        return len(self.instructions)

    def patch(self, instr_index: int, field_name: str, value: int) -> None:
        """Patch the specified field of an instruction."""
        _patch_instruction(self.instructions, instr_index, field_name, value)

    def emit_constant(self, value: MenaiValue, dest: int) -> None:
        """Emit instructions to load a constant value into dest."""
        if isinstance(value, MenaiNone):
            self.emit(Opcode.LOAD_NONE, dest=dest)
            return

        if isinstance(value, MenaiBoolean):
            self.emit(Opcode.LOAD_TRUE if value.value else Opcode.LOAD_FALSE, dest=dest)
            return

        if isinstance(value, MenaiList) and len(value.elements) == 0:
            self.emit(Opcode.LOAD_EMPTY_LIST, dest=dest)
            return

        if isinstance(value, MenaiDict) and len(value.pairs) == 0:
            self.emit(Opcode.LOAD_EMPTY_DICT, dest=dest)
            return

        if isinstance(value, MenaiSet) and len(value.elements) == 0:
            self.emit(Opcode.LOAD_EMPTY_SET, dest=dest)
            return

        const_idx = self.add_constant(value)
        self.emit(Opcode.LOAD_CONST, const_idx, dest=dest)

    # Maximum index that fits in a 12-bit instruction field.
    _MAX_INDEX = 0xFFF

    def add_constant(self, value: MenaiValue) -> int:
        """Add value to the constant pool if not already present, and return its index."""
        if isinstance(value, (MenaiInteger, MenaiFloat, MenaiComplex, MenaiBoolean, MenaiString)):
            key: tuple = (type(value).__name__, value.value)

        else:
            key = (id(value),)

        if key in self.constant_map:
            return self.constant_map[key]

        idx = len(self.constants)
        if idx > self._MAX_INDEX:
            raise MenaiCodegenError(
                f"Constant pool overflow: cannot add constant at index {idx} "
                f"(maximum is {self._MAX_INDEX}). "
                f"Expression contains too many distinct constant values."
            )

        self.constants.append(value)
        self.constant_map[key] = idx
        return idx

    def add_name(self, name: str) -> int:
        """Add a name to the name pool if not already present, and return its index."""
        if name in self.name_map:
            return self.name_map[name]

        idx = len(self.names)
        if idx > self._MAX_INDEX:
            raise MenaiCodegenError(
                f"Name pool overflow: cannot add name '{name}' at index {idx} "
                f"(maximum is {self._MAX_INDEX}). "
                f"Expression references too many distinct global names."
            )

        self.names.append(name)
        self.name_map[name] = idx
        return idx

    def add_code_object(self, code_obj: CodeObject) -> int:
        """Add a code object to the code object pool and return its index."""
        idx = len(self.code_objects)
        if idx > self._MAX_INDEX:
            raise MenaiCodegenError(
                f"Code object pool overflow: cannot add code object at index {idx} "
                f"(maximum is {self._MAX_INDEX}). "
                f"Expression contains too many nested closures."
            )

        self.code_objects.append(code_obj)
        return idx


class MenaiBytecodeBuilder:
    """
    Generates a CodeObject from a MenaiVCodeFunction.

    Runs the VM-specific backend passes:
      1. Allocate slots (allocate_slots)
      2. Peephole optimise (peephole)
      3. Emit bytecode (this class)

    Usage::

        code_obj = MenaiBytecodeBuilder().build(vcode_function, name="<module>")
    """

    def __init__(self) -> None:
        self._lambda_counter = 0

    def build(self, func: MenaiVCodeFunction, name: str = "<module>") -> CodeObject:
        """
        Generate a top-level CodeObject from a MenaiVCodeFunction.

        Args:
            func: The VCode function to compile (top-level module body).
            name: Name for the resulting CodeObject.

        Returns:
            A CodeObject ready for execution by the Menai VM.
        """
        slot_map = allocate_slots(func)
        func = peephole(func, slot_map)

        ctx = _EmitContext(slot_map=slot_map)
        self._emit_vcode(func, ctx)
        return CodeObject(
            instructions=ctx.instructions,
            constants=ctx.constants,
            names=ctx.names,
            code_objects=ctx.code_objects,
            param_count=0,
            local_count=slot_map.local_count,
            outgoing_arg_slots=max(slot_map.slot_count - slot_map.local_count, ctx.max_outgoing_args),
            name=name,
        )

    def _emit_vcode(self, func: MenaiVCodeFunction, ctx: _EmitContext) -> None:
        """
        Emit all instructions of func into ctx.

        Phase 1: emit instructions, recording label positions and collecting
                 forward jump patch sites.
        Phase 2: back-patch all forward jump targets.
        """
        label_index: Dict[str, int] = {}
        forward_jumps: List[Tuple[int, str, str]] = []  # (instr_idx, label, field)

        # The self-loop sentinel label resolves to instruction index 0 — the
        # start of the function body (after ENTER, which is emitted before
        # _emit_vcode is called for lambdas).
        entry_index = ctx.current_index()

        i = 0
        instrs = func.instrs
        while i < len(instrs):
            instr = instrs[i]

            if isinstance(instr, MenaiVCodeLabel):
                label_index[instr.name] = ctx.current_index()
                i += 1
                continue

            if isinstance(instr, MenaiVCodeMove):
                # Collect all consecutive moves into a batch and emit them as
                # a parallel assignment group to avoid sequential-move hazards.
                j = i
                move_pairs: List[Tuple[int, int]] = []
                while j < len(instrs) and isinstance(instrs[j], MenaiVCodeMove):
                    m = instrs[j]
                    assert isinstance(m, MenaiVCodeMove)
                    move_pairs.append((ctx.slot_of(m.dst), ctx.slot_of(m.src)))
                    j += 1

                _emit_parallel_moves(move_pairs, ctx)
                i = j
                continue

            if isinstance(instr, MenaiVCodeLoadConst):
                ctx.emit_constant(instr.value, dest=ctx.slot_of(instr.dst))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeLoadName):
                name_idx = ctx.add_name(instr.name)
                ctx.emit(Opcode.LOAD_NAME, name_idx, dest=ctx.slot_of(instr.dst))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeBuiltin):
                self._emit_builtin(instr, ctx)
                i += 1
                continue

            if isinstance(instr, MenaiVCodeMakeStruct):
                local_count = ctx.slot_map.local_count
                n_fields = len(instr.args)
                # Stage the struct type descriptor into the outgoing zone slot 0,
                # then stage each field value into slots 1..n_fields.
                type_const_idx = ctx.add_constant(instr.struct_type)
                ctx.emit(Opcode.LOAD_CONST, type_const_idx, dest=local_count)
                for j, arg in enumerate(instr.args):
                    src = ctx.slot_of(arg)
                    dst_slot = local_count + 1 + j
                    if src != dst_slot:
                        ctx.emit(Opcode.MOVE, src, dest=dst_slot)
                ctx.max_outgoing_args = max(ctx.max_outgoing_args, 1 + n_fields)
                ctx.emit(Opcode.MAKE_STRUCT, local_count, n_fields, dest=ctx.slot_of(instr.dst))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeCall):
                local_count = ctx.slot_map.local_count
                for j, arg in enumerate(instr.args):
                    src = ctx.slot_of(arg)
                    dst = local_count + j
                    if src != dst:
                        ctx.emit(Opcode.MOVE, src, dest=dst)

                n_args = len(instr.args)
                ctx.max_outgoing_args = max(ctx.max_outgoing_args, n_args)
                ctx.emit(Opcode.CALL, ctx.slot_of(instr.func), n_args, dest=ctx.slot_of(instr.dst))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeTailCall):
                local_count = ctx.slot_map.local_count
                for j, arg in enumerate(instr.args):
                    src = ctx.slot_of(arg)
                    dst = local_count + j
                    if src != dst:
                        ctx.emit(Opcode.MOVE, src, dest=dst)

                n_args = len(instr.args)
                ctx.max_outgoing_args = max(ctx.max_outgoing_args, n_args)
                ctx.emit(Opcode.TAIL_CALL, ctx.slot_of(instr.func), n_args)
                i += 1
                continue

            if isinstance(instr, MenaiVCodeApply):
                ctx.emit(Opcode.APPLY, ctx.slot_of(instr.func), ctx.slot_of(instr.arg_list), dest=ctx.slot_of(instr.dst))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeTailApply):
                ctx.emit(Opcode.TAIL_APPLY, ctx.slot_of(instr.func), ctx.slot_of(instr.arg_list))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeMakeClosure):
                self._emit_make_closure(instr, ctx)
                i += 1
                continue

            if isinstance(instr, MenaiVCodePatchClosure):
                ctx.emit(
                    Opcode.PATCH_CLOSURE,
                    ctx.slot_of(instr.closure),
                    instr.capture_index,
                    src2=ctx.slot_of(instr.value),
                )
                i += 1
                continue

            if isinstance(instr, MenaiVCodeTrace):
                for msg in instr.messages:
                    ctx.emit(Opcode.EMIT_TRACE, ctx.slot_of(msg))

                dst_slot = ctx.slot_of(instr.dst)
                val_slot = ctx.slot_of(instr.value)
                if dst_slot != val_slot:
                    ctx.emit(Opcode.MOVE, val_slot, dest=dst_slot)

                i += 1
                continue

            if isinstance(instr, MenaiVCodeReturn):
                ctx.emit(Opcode.RETURN, ctx.slot_of(instr.value))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeRaise):
                ctx.emit(Opcode.RAISE_ERROR, ctx.slot_of(instr.message))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeJump):
                if instr.label == "__entry__":
                    ctx.emit(Opcode.JUMP, entry_index)

                else:
                    jump_idx = ctx.emit(Opcode.JUMP, 0)
                    forward_jumps.append((jump_idx, instr.label, 'src0'))

                i += 1
                continue

            if isinstance(instr, MenaiVCodeJumpIfTrue):
                cond_slot = ctx.slot_of(instr.cond)
                jump_idx = ctx.emit(Opcode.JUMP_IF_TRUE, cond_slot, 0)
                forward_jumps.append((jump_idx, instr.label, 'src1'))
                i += 1
                continue

            if isinstance(instr, MenaiVCodeJumpIfFalse):
                cond_slot = ctx.slot_of(instr.cond)
                jump_idx = ctx.emit(Opcode.JUMP_IF_FALSE, cond_slot, 0)
                forward_jumps.append((jump_idx, instr.label, 'src1'))
                i += 1
                continue

            raise TypeError(
                f"MenaiBytecodeBuilder: unhandled VCode instruction {type(instr).__name__}"
            )

        # Phase 2: back-patch forward jumps.
        for instr_idx, label, field_name in forward_jumps:
            assert label in label_index, (
                f"MenaiBytecodeBuilder: undefined label {label!r}"
            )
            ctx.patch(instr_idx, field_name, label_index[label])

    def _emit_builtin(self, instr: MenaiVCodeBuiltin, ctx: _EmitContext) -> None:
        op = instr.op
        args = instr.args
        dest = ctx.slot_of(instr.dst)

        opcode, _ = BUILTIN_OPCODE_MAP.get(op, (None, None))
        if opcode is None:
            raise ValueError(f"MenaiBytecodeBuilder: unknown builtin op {op!r}")

        def slot(i: int) -> int:
            return ctx.slot_of(args[i])

        if op in TERNARY_OPS:
            assert len(args) == 3
            ctx.emit(opcode, slot(0), slot(1), dest=dest, src2=slot(2))

        elif op in BINARY_OPS:
            assert len(args) == 2
            ctx.emit(opcode, slot(0), slot(1), dest=dest)

        elif op in UNARY_OPS:
            assert len(args) == 1
            ctx.emit(opcode, slot(0), dest=dest)

        else:
            raise ValueError(f"MenaiBytecodeBuilder: unhandled builtin op {op!r}")

    def _emit_make_closure(self, instr: MenaiVCodeMakeClosure, ctx: _EmitContext) -> None:
        child_code = self._emit_lambda(instr.function)
        code_idx = ctx.add_code_object(child_code)

        capture_count = len(instr.captures)
        total_free_vars = len(instr.function.free_vars)

        if instr.needs_patching or capture_count > 0:
            closure_slot = ctx.slot_of(instr.dst)
            ctx.emit(Opcode.MAKE_CLOSURE, code_idx, 0, dest=closure_slot)
            outer_start = total_free_vars - capture_count
            for i, cap in enumerate(instr.captures):
                value_slot = ctx.slot_of(cap)
                ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, outer_start + i, src2=value_slot)

            return

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
        ctx.emit(Opcode.LOAD_CONST, ctx.constant_map[key], dest=ctx.slot_of(instr.dst))

    def _emit_lambda(self, func: MenaiVCodeFunction) -> CodeObject:
        """Recursively emit a nested lambda MenaiVCodeFunction to a CodeObject."""
        slot_map = allocate_slots(func)
        func = peephole(func, slot_map)

        child_ctx = _EmitContext(slot_map=slot_map)
        param_count = len(func.params)

        self._emit_vcode(func, child_ctx)

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
            local_count=slot_map.local_count,
            outgoing_arg_slots=max(slot_map.slot_count - slot_map.local_count, child_ctx.max_outgoing_args),
            is_variadic=func.is_variadic,
            name=lambda_name,
            source_line=func.source_line,
            source_file=func.source_file,
        )


def _emit_parallel_moves(
    moves: List[Tuple[int, int]],
    ctx: "_EmitContext",
) -> None:
    """
    Emit a set of parallel moves (dst_slot, src_slot) as safe sequential MOVEs.

    A naive sequential emission is incorrect when moves form a cycle, e.g.:
      slot0 ← slot1
      slot1 ← slot0
    The second move would read the value already overwritten by the first.

    This function detects cycles and breaks them using a scratch slot
    (slot_map.slot_count), extending slot_count by 1 if any cycle is found.
    Moves where src == dst are skipped as no-ops.

    Args:
        moves:  List of (dst_slot, src_slot) pairs to emit as parallel moves.
        ctx:    Emit context; ctx.slot_map.slot_count is extended if a scratch
                slot is needed.
    """
    # Filter no-ops.
    pending: Dict[int, int] = {}
    for dst, src in moves:
        if dst != src:
            pending[dst] = src

    if not pending:
        return

    scratch_used = False

    while pending:
        # A move is safe to emit when its destination is not needed as a source
        # by any other pending move — emitting it cannot corrupt another move's
        # input.
        srcs: Set[int] = set(pending.values())
        ready = [dst for dst in pending if dst not in srcs]

        if ready:
            for dst in ready:
                src = pending.pop(dst)
                ctx.emit(Opcode.MOVE, src, dest=dst)

        else:
            # All remaining moves form cycles.  Break one cycle by saving one
            # source value into the scratch slot, then unrolling the cycle.
            scratch = ctx.slot_map.slot_count
            if not scratch_used:
                ctx.slot_map.slot_count += 1
                scratch_used = True

            # Pick any cycle member and save its source to scratch.
            cycle_dst = next(iter(pending))
            cycle_src = pending[cycle_dst]
            ctx.emit(Opcode.MOVE, cycle_src, dest=scratch)

            # Follow the cycle, emitting moves, until we reach cycle_dst again.
            cur = cycle_src
            while cur != cycle_dst:
                next_src = pending.pop(cur)
                ctx.emit(Opcode.MOVE, next_src, dest=cur)
                cur = next_src

            # Close the cycle: write scratch into cycle_dst.
            pending.pop(cycle_dst)
            ctx.emit(Opcode.MOVE, scratch, dest=cycle_dst)
