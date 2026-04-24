"""
VCode (Virtual Code) — the linear IR for the Menai VM backend.

VCode sits between the SSA CFG and the bytecode emitter.  It is produced by
lowering a MenaiCFGFunction and is consumed by the slot allocator, peephole
optimiser, and bytecode emitter.

Key properties
--------------
- Flat list of instructions per function — no block structure.
- No phi nodes — replaced by explicit MenaiVCodeMove instructions inserted
  during CFG lowering.
- Labels and jumps replace CFG edges.
- Virtual registers (MenaiVCodeReg) are plain integer IDs.  Mapping them to
  concrete slots is the slot allocator's job, not VCode's.
- Nested functions are represented as MenaiVCodeFunction objects referenced
  from MenaiVCodeMakeClosure instructions, mirroring the CFG structure.
"""

from dataclasses import dataclass, field
from typing import List, Optional, Tuple

from menai.menai_value import MenaiValue, MenaiStructType


@dataclass(frozen=True)
class MenaiVCodeReg:
    """
    A virtual register — the unit of value in VCode.

    `id` is unique within the enclosing MenaiVCodeFunction.
    `hint` is a human-readable label for debugging; it carries no semantic
    weight and two registers with the same hint are not the same register.
    """
    id: int
    hint: str = ""

    def __str__(self) -> str:
        if self.hint:
            return f"r{self.id}({self.hint})"
        return f"r{self.id}"

    def __repr__(self) -> str:
        return str(self)


@dataclass
class MenaiVCodeLabel:
    """
    A label marking a jump target in the flat instruction list.

    Labels have no runtime effect; they exist solely so that jump instructions
    can refer to positions by name rather than by index.  The bytecode emitter
    resolves label references to instruction indices.
    """
    name: str


@dataclass
class MenaiVCodeMove:
    """
    dst = src

    Explicit register-to-register copy.  Produced by phi elimination to
    materialise phi node semantics.  The slot allocator may eliminate moves
    where src and dst are assigned the same slot.
    """
    dst: MenaiVCodeReg
    src: MenaiVCodeReg


@dataclass
class MenaiVCodeLoadConst:
    """dst = <constant value>"""
    dst: MenaiVCodeReg
    value: MenaiValue


@dataclass
class MenaiVCodeLoadName:
    """dst = globals[name]"""
    dst: MenaiVCodeReg
    name: str


@dataclass
class MenaiVCodeBuiltin:
    """
    dst = <builtin_op>(args...)

    `op` is the builtin name as it appears in BUILTIN_OPCODE_MAP.
    """
    dst: MenaiVCodeReg
    op: str
    args: List[MenaiVCodeReg]


@dataclass
class MenaiVCodeCall:
    """dst = call func(args...)"""
    dst: MenaiVCodeReg
    func: MenaiVCodeReg
    args: List[MenaiVCodeReg]


@dataclass
class MenaiVCodeTailCall:
    """tail_call func(args...)  — no result, terminates the function."""
    func: MenaiVCodeReg
    args: List[MenaiVCodeReg]


@dataclass
class MenaiVCodeApply:
    """dst = apply(func, arg_list)"""
    dst: MenaiVCodeReg
    func: MenaiVCodeReg
    arg_list: MenaiVCodeReg


@dataclass
class MenaiVCodeTailApply:
    """tail_apply(func, arg_list)  — no result, terminates the function."""
    func: MenaiVCodeReg
    arg_list: MenaiVCodeReg


@dataclass
class MenaiVCodeMakeClosure:
    """
    dst = make_closure(function, captures...)

    `function` is the nested MenaiVCodeFunction.
    `captures` are the outer registers to capture, in order.
    `needs_patching` mirrors the CFG flag — the closure must be allocated
    even if captures is empty because PATCH_CLOSURE will fill sibling slots.
    """
    dst: MenaiVCodeReg
    function: 'MenaiVCodeFunction'
    captures: List[MenaiVCodeReg]
    needs_patching: bool = False


@dataclass
class MenaiVCodeMakeStruct:
    """
    dst = make_struct(struct_type, args...)

    Constructs a new MenaiStruct of type `struct_type` from a list of field
    values.  `struct_type` is the compile-time MenaiStructType descriptor,
    stored directly on the instruction rather than in a register.  The
    bytecode emitter stages the type descriptor and field values into the
    outgoing zone and emits MAKE_STRUCT.
    """
    dst: MenaiVCodeReg
    struct_type: MenaiStructType
    args: List[MenaiVCodeReg]


@dataclass
class MenaiVCodeMakeList:
    """
    dst = make_list(args...)

    Constructs a new MenaiList from N element values.  The bytecode emitter
    stages element values into the outgoing zone and emits MAKE_LIST, which
    allocates the list in a single call.
    """
    dst: MenaiVCodeReg
    args: List[MenaiVCodeReg]


@dataclass
class MenaiVCodeMakeSet:
    """
    dst = make_set(args...)

    Constructs a new MenaiSet from N element values.  The bytecode emitter
    stages element values into the outgoing zone and emits MAKE_SET, which
    allocates the set in a single call.
    """
    dst: MenaiVCodeReg
    args: List[MenaiVCodeReg]


@dataclass
class MenaiVCodeMakeDict:
    """
    dst = make_dict(pairs...)

    Constructs a new MenaiDict from N key-value pairs.  The bytecode emitter
    stages pairs into the outgoing zone as (k0, v0, k1, v1, ...) and emits
    MAKE_DICT, which allocates the dict in a single call.
    """
    dst: MenaiVCodeReg
    pairs: List[Tuple[MenaiVCodeReg, MenaiVCodeReg]]


@dataclass
class MenaiVCodePatchClosure:
    """
    patch_closure(closure, capture_index, value)

    Installs value into capture slot capture_index of closure.
    Used exclusively during letrec initialisation.
    """
    closure: MenaiVCodeReg
    capture_index: int
    value: MenaiVCodeReg


@dataclass
class MenaiVCodeTrace:
    """
    dst = trace(messages..., value)

    Emits each message register via EMIT_TRACE then passes value through
    as the result.
    """
    dst: MenaiVCodeReg
    messages: List[MenaiVCodeReg]
    value: MenaiVCodeReg


@dataclass
class MenaiVCodeJump:
    """Unconditional jump to label."""
    label: str


@dataclass
class MenaiVCodeJumpIfTrue:
    """Jump to label if cond is true."""
    cond: MenaiVCodeReg
    label: str


@dataclass
class MenaiVCodeJumpIfFalse:
    """Jump to label if cond is false."""
    cond: MenaiVCodeReg
    label: str


@dataclass
class MenaiVCodeReturn:
    """Return value from the current function."""
    value: MenaiVCodeReg


@dataclass
class MenaiVCodeRaise:
    """Raise a runtime error with a message string from a register."""
    message: MenaiVCodeReg


# Union of all VCode instruction types.
MenaiVCodeInstr = (  # pylint: disable=invalid-name
    MenaiVCodeLabel
    | MenaiVCodeMove
    | MenaiVCodeLoadConst
    | MenaiVCodeLoadName
    | MenaiVCodeBuiltin
    | MenaiVCodeCall
    | MenaiVCodeTailCall
    | MenaiVCodeApply
    | MenaiVCodeTailApply
    | MenaiVCodeMakeClosure
    | MenaiVCodePatchClosure
    | MenaiVCodeMakeStruct
    | MenaiVCodeMakeList
    | MenaiVCodeMakeSet
    | MenaiVCodeMakeDict
    | MenaiVCodeTrace
    | MenaiVCodeJump
    | MenaiVCodeJumpIfTrue
    | MenaiVCodeJumpIfFalse
    | MenaiVCodeReturn
    | MenaiVCodeRaise
)


@dataclass
class MenaiVCodeFunction:
    """
    The VCode for a single lambda or the top-level module body.

    `instrs` is a flat list of instructions including labels and jumps.
    There is no block structure — control flow is expressed entirely through
    MenaiVCodeLabel, MenaiVCodeJump, MenaiVCodeJumpIfTrue, and
    MenaiVCodeJumpIfFalse.

    `params` and `free_vars` mirror the corresponding CFG fields and are used
    by the slot allocator to assign fixed slots to parameters and captures.

    `reg_count` is the number of virtual registers allocated during lowering,
    used by the slot allocator as the upper bound on register IDs.
    """
    instrs: List[MenaiVCodeInstr] = field(default_factory=list)
    params: List[str] = field(default_factory=list)
    free_vars: List[str] = field(default_factory=list)
    is_variadic: bool = False
    binding_name: Optional[str] = None
    reg_count: int = 0
    source_line: int = 0
    source_file: str = ""

    @property
    def param_count(self) -> int:
        """Return the number of parameters for this VCode function."""
        return len(self.params)

    def __repr__(self) -> str:
        """Return a human-readable representation of the VCode function."""
        name = self.binding_name or "<lambda>"
        lines = [f"MenaiVCodeFunction {name}({', '.join(self.params)}):"]
        if self.free_vars:
            lines.append(f"  free_vars: {self.free_vars}")

        for instr in self.instrs:
            lines.append(f"  {_fmt_instr(instr)}")

        return "\n".join(lines)


def _fmt_regs(regs: List[MenaiVCodeReg]) -> str:
    return "[" + ", ".join(str(r) for r in regs) + "]"


def _fmt_instr(instr: MenaiVCodeInstr) -> str:
    """One-line human-readable representation of a VCode instruction."""
    if isinstance(instr, MenaiVCodeLabel):
        return f"{instr.name}:"

    if isinstance(instr, MenaiVCodeMove):
        return f"{instr.dst} = MOVE {instr.src}"

    if isinstance(instr, MenaiVCodeLoadConst):
        return f"{instr.dst} = LOAD_CONST {instr.value!r}"

    if isinstance(instr, MenaiVCodeLoadName):
        return f"{instr.dst} = LOAD_NAME {instr.name!r}"

    if isinstance(instr, MenaiVCodeBuiltin):
        return f"{instr.dst} = {instr.op} {_fmt_regs(instr.args)}"

    if isinstance(instr, MenaiVCodeCall):
        return f"{instr.dst} = CALL {instr.func} {_fmt_regs(instr.args)}"

    if isinstance(instr, MenaiVCodeTailCall):
        return f"TAIL_CALL {instr.func} {_fmt_regs(instr.args)}"

    if isinstance(instr, MenaiVCodeApply):
        return f"{instr.dst} = APPLY {instr.func} {instr.arg_list}"

    if isinstance(instr, MenaiVCodeTailApply):
        return f"TAIL_APPLY {instr.func} {instr.arg_list}"

    if isinstance(instr, MenaiVCodeMakeClosure):
        name = instr.function.binding_name or "<lambda>"
        return f"{instr.dst} = MAKE_CLOSURE {name!r} {_fmt_regs(instr.captures)}"

    if isinstance(instr, MenaiVCodePatchClosure):
        return f"PATCH_CLOSURE {instr.closure} [{instr.capture_index}] = {instr.value}"

    if isinstance(instr, MenaiVCodeMakeStruct):
        return f"{instr.dst} = MAKE_STRUCT {instr.struct_type.name!r} {_fmt_regs(instr.args)}"

    if isinstance(instr, MenaiVCodeMakeList):
        return f"{instr.dst} = MAKE_LIST {_fmt_regs(instr.args)}"

    if isinstance(instr, MenaiVCodeMakeSet):
        return f"{instr.dst} = MAKE_SET {_fmt_regs(instr.args)}"

    if isinstance(instr, MenaiVCodeMakeDict):
        return f"{instr.dst} = MAKE_DICT {[(str(k), str(v)) for k, v in instr.pairs]}"

    if isinstance(instr, MenaiVCodeTrace):
        return f"{instr.dst} = TRACE {_fmt_regs(instr.messages)} {instr.value}"

    if isinstance(instr, MenaiVCodeJump):
        return f"JUMP {instr.label}"

    if isinstance(instr, MenaiVCodeJumpIfTrue):
        return f"JUMP_IF_TRUE {instr.cond} {instr.label}"

    if isinstance(instr, MenaiVCodeJumpIfFalse):
        return f"JUMP_IF_FALSE {instr.cond} {instr.label}"

    if isinstance(instr, MenaiVCodeReturn):
        return f"RETURN {instr.value}"

    if isinstance(instr, MenaiVCodeRaise):
        return f"RAISE {instr.message}"

    return f"<unknown instr {type(instr).__name__}>"
