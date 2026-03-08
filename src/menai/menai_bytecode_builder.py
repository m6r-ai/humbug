"""
Bytecode emitter for the Menai VM backend.

Translates a MenaiVCodeFunction (with allocated slots) into a CodeObject
ready for execution by the Menai VM.

This is the final, purely mechanical pass of the VM backend pipeline:

    MenaiCFGFunction
        → MenaiVCodeBuilder   (linearise, phi-eliminate)
        → allocate_slots      (assign virtual registers to slots)
        → peephole            (eliminate redundant moves and jumps)
        → MenaiBytecodeBuilder (emit CodeObject)  ← this file

The emitter has no knowledge of SSA, phi nodes, or liveness.  It simply
walks the flat VCode instruction list and emits the corresponding bytecode,
resolving label references to instruction indices in a back-patch phase.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Tuple

from menai.menai_bytecode import BUILTIN_OPCODE_MAP, CodeObject, Instruction, Opcode
from menai.menai_cfg import MenaiCFGFunction
from menai.menai_value import (
    MenaiBoolean,
    MenaiComplex,
    MenaiDict,
    MenaiFloat,
    MenaiFunction,
    MenaiInteger,
    MenaiList,
    MenaiNone,
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
    MenaiVCodeRaise,
    MenaiVCodeReg,
    MenaiVCodeReturn,
    MenaiVCodeTailApply,
    MenaiVCodeTailCall,
    MenaiVCodeTrace,
)
from menai.menai_vcode_allocator import SlotMap, allocate_slots
from menai.menai_vcode_builder import MenaiVCodeBuilder
from menai.menai_vcode_peephole import peephole


UNARY_OPS  = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 1}
BINARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 2}
TERNARY_OPS = {name: op for name, (op, arity) in BUILTIN_OPCODE_MAP.items() if arity == 3}


@dataclass
class _EmitContext:
    """Mutable state for emitting one MenaiVCodeFunction into a CodeObject."""
    instructions: List[Instruction] = field(default_factory=list)
    constants: List[MenaiValue] = field(default_factory=list)
    names: List[str] = field(default_factory=list)
    code_objects: List[CodeObject] = field(default_factory=list)
    constant_map: Dict[tuple, int] = field(default_factory=dict)
    name_map: Dict[str, int] = field(default_factory=dict)
    slot_map: SlotMap = field(default_factory=lambda: SlotMap(slots={}, slot_count=0))

    def slot_of(self, reg: MenaiVCodeReg) -> int:
        """Get the slot index assigned to reg."""
        return self.slot_map.slot_of(reg)

    def emit(self, opcode: Opcode, src0: int = 0, src1: int = 0, dest: int = 0, src2: int = 0) -> int:
        """Emit an instruction and return its index."""
        idx = len(self.instructions)
        self.instructions.append(Instruction(int(opcode), dest=dest, src0=src0, src1=src1, src2=src2))
        return idx

    def current_index(self) -> int:
        """Get the index of the next instruction to be emitted."""
        return len(self.instructions)

    def patch(self, instr_index: int, field_name: str, value: int) -> None:
        """Patch the specified field of an instruction."""
        setattr(self.instructions[instr_index], field_name, value)

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

        const_idx = self.add_constant(value)
        self.emit(Opcode.LOAD_CONST, const_idx, dest=dest)

    def add_constant(self, value: MenaiValue) -> int:
        """Add value to the constant pool if not already present, and return its index."""
        if isinstance(value, (MenaiInteger, MenaiFloat, MenaiComplex, MenaiBoolean, MenaiString)):
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
        """Add a name to the name pool if not already present, and return its index."""
        if name in self.name_map:
            return self.name_map[name]

        idx = len(self.names)
        self.names.append(name)
        self.name_map[name] = idx
        return idx

    def add_code_object(self, code_obj: CodeObject) -> int:
        """Add a code object to the code object pool and return its index."""
        idx = len(self.code_objects)
        self.code_objects.append(code_obj)
        return idx


class MenaiBytecodeBuilder:
    """
    Generates a CodeObject from a MenaiCFGFunction.

    Orchestrates the full VM backend pipeline:
      1. Lower CFG to VCode (MenaiVCodeBuilder)
      2. Allocate slots (allocate_slots)
      3. Peephole optimise (peephole)
      4. Emit bytecode (this class)

    Usage::

        code_obj = MenaiBytecodeBuilder().build(cfg_function, name="<module>")
    """

    def __init__(self) -> None:
        self._lambda_counter = 0

    def build(self, func: MenaiCFGFunction, name: str = "<module>") -> CodeObject:
        """
        Generate a top-level CodeObject from a MenaiCFGFunction.

        Args:
            func: The CFG function to compile (top-level module body).
            name: Name for the resulting CodeObject.

        Returns:
            A CodeObject ready for execution by the Menai VM.
        """
        vcode = MenaiVCodeBuilder().build(func)
        slot_map = allocate_slots(vcode)
        vcode = peephole(vcode, slot_map)

        ctx = _EmitContext(slot_map=slot_map)
        self._emit_vcode(vcode, ctx)
        return CodeObject(
            instructions=ctx.instructions,
            constants=ctx.constants,
            names=ctx.names,
            code_objects=ctx.code_objects,
            param_count=0,
            local_count=slot_map.slot_count,
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

        for instr in func.instrs:
            if isinstance(instr, MenaiVCodeLabel):
                label_index[instr.name] = ctx.current_index()
                continue

            if isinstance(instr, MenaiVCodeMove):
                src_slot = ctx.slot_of(instr.src)
                dst_slot = ctx.slot_of(instr.dst)
                # Redundant moves should have been eliminated by the peephole
                # pass, but guard here for safety.
                if src_slot != dst_slot:
                    ctx.emit(Opcode.MOVE, src_slot, dest=dst_slot)

                continue

            if isinstance(instr, MenaiVCodeLoadConst):
                ctx.emit_constant(instr.value, dest=ctx.slot_of(instr.dst))
                continue

            if isinstance(instr, MenaiVCodeLoadName):
                name_idx = ctx.add_name(instr.name)
                ctx.emit(Opcode.LOAD_NAME, name_idx, dest=ctx.slot_of(instr.dst))
                continue

            if isinstance(instr, MenaiVCodeBuiltin):
                self._emit_builtin(instr, ctx)
                continue

            if isinstance(instr, MenaiVCodeCall):
                for arg in instr.args:
                    ctx.emit(Opcode.PUSH, ctx.slot_of(arg))

                ctx.emit(Opcode.PUSH, ctx.slot_of(instr.func))
                ctx.emit(Opcode.CALL, len(instr.args), dest=ctx.slot_of(instr.dst))
                continue

            if isinstance(instr, MenaiVCodeTailCall):
                for arg in instr.args:
                    ctx.emit(Opcode.PUSH, ctx.slot_of(arg))
                ctx.emit(Opcode.PUSH, ctx.slot_of(instr.func))
                ctx.emit(Opcode.TAIL_CALL, len(instr.args))
                continue

            if isinstance(instr, MenaiVCodeApply):
                ctx.emit(Opcode.PUSH, ctx.slot_of(instr.func))
                ctx.emit(Opcode.PUSH, ctx.slot_of(instr.arg_list))
                ctx.emit(Opcode.APPLY, dest=ctx.slot_of(instr.dst))
                continue

            if isinstance(instr, MenaiVCodeTailApply):
                ctx.emit(Opcode.PUSH, ctx.slot_of(instr.func))
                ctx.emit(Opcode.PUSH, ctx.slot_of(instr.arg_list))
                ctx.emit(Opcode.TAIL_APPLY)
                continue

            if isinstance(instr, MenaiVCodeMakeClosure):
                self._emit_make_closure(instr, ctx)
                continue

            if isinstance(instr, MenaiVCodePatchClosure):
                ctx.emit(
                    Opcode.PATCH_CLOSURE,
                    ctx.slot_of(instr.closure),
                    ctx.slot_of(instr.value),
                    src2=instr.capture_index,
                )
                continue

            if isinstance(instr, MenaiVCodeTrace):
                for msg in instr.messages:
                    ctx.emit(Opcode.EMIT_TRACE, ctx.slot_of(msg))

                dst_slot = ctx.slot_of(instr.dst)
                val_slot = ctx.slot_of(instr.value)
                if dst_slot != val_slot:
                    ctx.emit(Opcode.MOVE, val_slot, dest=dst_slot)

                continue

            if isinstance(instr, MenaiVCodeReturn):
                ctx.emit(Opcode.RETURN, ctx.slot_of(instr.value))
                continue

            if isinstance(instr, MenaiVCodeRaise):
                const_idx = ctx.add_constant(instr.message)
                ctx.emit(Opcode.RAISE_ERROR, const_idx)
                continue

            if isinstance(instr, MenaiVCodeJump):
                if instr.label == "__entry__":
                    ctx.emit(Opcode.JUMP, entry_index)

                else:
                    jump_idx = ctx.emit(Opcode.JUMP, 0)
                    forward_jumps.append((jump_idx, instr.label, 'src0'))

                continue

            if isinstance(instr, MenaiVCodeJumpIfTrue):
                cond_slot = ctx.slot_of(instr.cond)
                jump_idx = ctx.emit(Opcode.JUMP_IF_TRUE, cond_slot, 0)
                forward_jumps.append((jump_idx, instr.label, 'src1'))
                continue

            if isinstance(instr, MenaiVCodeJumpIfFalse):
                cond_slot = ctx.slot_of(instr.cond)
                jump_idx = ctx.emit(Opcode.JUMP_IF_FALSE, cond_slot, 0)
                forward_jumps.append((jump_idx, instr.label, 'src1'))
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
                ctx.emit(Opcode.PATCH_CLOSURE, closure_slot, value_slot, src2=outer_start + i)

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

        if param_count > 0:
            child_ctx.emit(Opcode.ENTER, param_count)

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
            local_count=slot_map.slot_count,
            is_variadic=func.is_variadic,
            name=lambda_name,
            source_line=func.source_line,
            source_file=func.source_file,
        )
