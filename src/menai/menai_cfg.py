"""
Control Flow Graph (CFG) data structures for the Menai compiler.

This module defines the SSA-form CFG IR that sits between the IR tree
(menai_ir.py) and the backend code generators.  The code generators
consume this representation.
"""

from dataclasses import dataclass, field
from typing import List, Tuple, Callable

from menai.menai_value import MenaiValue


@dataclass
class MenaiCFGValue:
    """
    An SSA value — the result of exactly one instruction.

    `id` is unique within the enclosing MenaiCFGFunction.
    `hint` is a human-readable label for debugging (source name, "if_result",
    "call_result", etc.).  It carries no semantic weight.
    """
    id: int
    hint: str = ""

    def __str__(self) -> str:
        if self.hint:
            return f"%{self.id}({self.hint})"

        return f"%{self.id}"

    def __repr__(self) -> str:
        return str(self)

    # MenaiCFGValue objects are used as dict keys (e.g. in phi nodes).
    def __hash__(self) -> int:
        return hash(self.id)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, MenaiCFGValue):
            return self.id == other.id

        return NotImplemented


@dataclass
class MenaiCFGConstInstr:
    """
    %result = <literal value>

    Covers all constant types: integer, float, complex, string, boolean,
    none, empty list, and quoted values.
    """
    result: MenaiCFGValue
    value: MenaiValue


@dataclass
class MenaiCFGGlobalInstr:
    """
    %result = load_global <name>

    Loads a global name (builtin, prelude function, or module-level binding
    looked up at runtime via LOAD_NAME).
    """
    result: MenaiCFGValue
    name: str


@dataclass
class MenaiCFGParamInstr:
    """
    %result = param <index>

    Represents a lambda parameter.  `index` is the 0-based position in the
    parameter list.  The VM codegen lowers this to ENTER + LOAD_VAR.
    """
    result: MenaiCFGValue
    index: int
    param_name: str


@dataclass
class MenaiCFGFreeVarInstr:
    """
    %result = free_var <index>

    Loads a captured free variable from the closure's capture list.
    `index` is the position in the combined (sibling + outer) free_vars list
    on the enclosing MenaiCFGFunction.  The VM codegen lowers this to
    LOAD_VAR with the appropriate slot offset.
    """
    result: MenaiCFGValue
    index: int
    var_name: str


@dataclass
class MenaiCFGBuiltinInstr:
    """
    %result = <builtin_op> [%arg, ...]

    A direct builtin operation (opcode-backed).  `op` is the builtin name as
    it appears in BUILTIN_OPCODE_MAP (e.g. 'integer+', 'list-first').
    The VM codegen maps `op` to the corresponding Opcode.

    Also covers the variadic BUILD_OPS ('list', 'dict') and the special-cased
    builtins with optional arguments ('range', 'integer->complex', etc.).
    """
    result: MenaiCFGValue
    op: str
    args: List[MenaiCFGValue]


@dataclass
class MenaiCFGCallInstr:
    """
    %result = call %func [%arg, ...]

    A non-tail function call.  `func` is the SSA value holding the callable.
    """
    result: MenaiCFGValue
    func: MenaiCFGValue
    args: List[MenaiCFGValue]


@dataclass
class MenaiCFGApplyInstr:
    """
    %result = apply %func %arg_list

    A non-tail apply (calls func with arg_list as a Menai list of arguments).
    Lowered to the APPLY opcode by the VM codegen.
    """
    result: MenaiCFGValue
    func: MenaiCFGValue
    arg_list: MenaiCFGValue


@dataclass
class MenaiCFGMakeClosureInstr:
    """
    %result = make_closure <function> [%capture, ...]

    Creates a closure from a nested MenaiCFGFunction and a list of captured
    SSA values.  `captures` is ordered: sibling free vars first, then outer
    free vars, matching the free_vars list on `function`.

    If `captures` is empty the VM codegen may emit LOAD_CONST with a
    pre-built MenaiFunction instead of MAKE_CLOSURE.
    """
    result: MenaiCFGValue
    function: 'MenaiCFGFunction'
    captures: List[MenaiCFGValue]
    needs_patching: bool = False
    # When True, the VM must create a mutable closure object (MAKE_CLOSURE)
    # even if `captures` is empty, because PATCH_CLOSURE instructions will
    # fill in sibling captures after all closures in the letrec are created.
    # The total capture slot count is len(function.free_vars).


@dataclass
class MenaiCFGPatchClosureInstr:
    """
    patch_closure %closure, capture_index, %value

    Installs `value` into capture slot `capture_index` of `closure`.
    Used exclusively during letrec initialisation to break mutual-recursion
    cycles.  The VM codegen lowers this to PATCH_CLOSURE.

    This instruction has no result (it is a side-effecting mutation of the
    closure object).  It is the only instruction in the CFG that does not
    produce an SSA value, but because Menai closures are the only mutable
    objects (and only during initialisation), this does not violate the
    SSA invariant for values.
    """
    closure: MenaiCFGValue
    capture_index: int
    value: MenaiCFGValue


@dataclass
class MenaiCFGPhiInstr:
    """
    %result = phi [(%value_from_block, block), ...]

    Standard SSA phi node.  Each entry pairs an incoming SSA value with the
    predecessor block it comes from.  For Menai, phi nodes appear only at
    `if` join points (one phi per if expression).

    VM codegen: both predecessor blocks leave their value on the stack, so
    the phi emits no instructions — the join block simply continues.

    Native codegen: maps directly to an LLVM phi instruction.
    """
    result: MenaiCFGValue
    incoming: List[Tuple[MenaiCFGValue, 'MenaiCFGBlock']]


@dataclass
class MenaiCFGTraceInstr:
    """
    %result = trace [%msg, ...] %value

    Emits each message via EMIT_TRACE then passes `value` through as the
    result.  The VM codegen emits EMIT_TRACE for each message then leaves
    `value` on the stack.
    """
    result: MenaiCFGValue
    messages: List[MenaiCFGValue]
    value: MenaiCFGValue


# Union of all non-terminator instruction types.
# MenaiCFGPatchClosureInstr is intentionally excluded — it has no result and
# is stored in MenaiCFGBlock.patch_instrs rather than MenaiCFGBlock.instrs.
MenaiCFGInstr = (
    MenaiCFGConstInstr
    | MenaiCFGGlobalInstr
    | MenaiCFGParamInstr
    | MenaiCFGFreeVarInstr
    | MenaiCFGBuiltinInstr
    | MenaiCFGCallInstr
    | MenaiCFGApplyInstr
    | MenaiCFGMakeClosureInstr
    | MenaiCFGPatchClosureInstr
    | MenaiCFGPhiInstr
    | MenaiCFGTraceInstr
)


@dataclass
class MenaiCFGJumpTerm:
    """Unconditional jump to `target`."""
    target: 'MenaiCFGBlock'


@dataclass
class MenaiCFGBranchTerm:
    """
    Conditional branch on `cond`.

    Jumps to `true_block` if cond is truthy, `false_block` otherwise.
    Lowered to JUMP_IF_FALSE by the VM codegen.
    """
    cond: MenaiCFGValue
    true_block: 'MenaiCFGBlock'
    false_block: 'MenaiCFGBlock'


@dataclass
class MenaiCFGReturnTerm:
    """Return `value` from the current function."""
    value: MenaiCFGValue


@dataclass
class MenaiCFGTailCallTerm:
    """
    Tail call to `func` with `args`.

    Lowered to TAIL_CALL by the VM codegen.
    """
    func: MenaiCFGValue
    args: List[MenaiCFGValue]


@dataclass
class MenaiCFGTailApplyTerm:
    """
    Tail apply: call `func` with `arg_list` as a Menai list.

    Lowered to TAIL_APPLY by the VM codegen.
    """
    func: MenaiCFGValue
    arg_list: MenaiCFGValue


@dataclass
class MenaiCFGSelfLoopTerm:
    """
    Direct self-recursive tail call.

    The callee is the enclosing function itself.  `args` are the new
    argument values, in parameter order.  The VM codegen lowers this to
    JUMP 0 (after storing args into the parameter slots).
    """
    args: List[MenaiCFGValue]


@dataclass
class MenaiCFGRaiseTerm:
    """
    Raise a runtime error with a constant message string.

    Lowered to RAISE_ERROR by the VM codegen.
    """
    message: MenaiValue


# Union of all terminator types.
MenaiCFGTerminator = (
    MenaiCFGJumpTerm
    | MenaiCFGBranchTerm
    | MenaiCFGReturnTerm
    | MenaiCFGTailCallTerm
    | MenaiCFGTailApplyTerm
    | MenaiCFGSelfLoopTerm
    | MenaiCFGRaiseTerm
)


@dataclass
class MenaiCFGBlock:
    """
    A basic block: a maximal straight-line sequence of instructions with a
    single entry point and a single exit (the terminator).

    Fields
    ------
    id          : unique integer within the enclosing MenaiCFGFunction (0 = entry)
    label       : human-readable name for debugging ("entry", "then_0", etc.)
    instrs      : non-terminator instructions, in emission order
    patch_instrs: MenaiCFGPatchClosureInstr instructions for letrec fixup,
                  emitted after `instrs` but before the terminator
    terminator  : the block's single exit instruction (set by the builder)
    predecessors: blocks that have an edge to this block (filled in by the
                  builder after all blocks are created)
    """
    id: int
    label: str
    instrs: List[MenaiCFGInstr] = field(default_factory=list)
    patch_instrs: List[MenaiCFGPatchClosureInstr] = field(default_factory=list)
    terminator: MenaiCFGTerminator | None = None
    predecessors: List['MenaiCFGBlock'] = field(default_factory=list)

    def __repr__(self) -> str:
        lines = [f"block {self.id} ({self.label}):"]
        for instr in self.instrs:
            lines.append(f"  {_fmt_instr(instr)}")

        for patch in self.patch_instrs:
            lines.append(f"  patch_closure {patch.closure} [{patch.capture_index}] = {patch.value}")

        if self.terminator is not None:
            lines.append(f"  {_fmt_term(self.terminator)}")

        return "\n".join(lines)


@dataclass
class MenaiCFGFunction:
    """
    The CFG for a single lambda (or the top-level module body).

    `blocks` is ordered with the entry block first.  The builder appends
    blocks in construction order; the VM codegen performs its own traversal.

    Fields
    ------
    blocks       : all basic blocks, entry block at index 0
    params       : parameter names, in order (parallel to param_count)
    free_vars    : captured variable names, sibling free vars first then outer
                   free vars, matching the capture order in MakeClosureInstr
    is_variadic  : True if the last parameter is a rest parameter
    binding_name : the name this lambda is bound to, if any (for self-loop
                   detection and debug names)
    source_line  : source line where the lambda is defined
    source_file  : source file where the lambda is defined
    """
    blocks: List[MenaiCFGBlock] = field(default_factory=list)
    params: List[str] = field(default_factory=list)
    free_vars: List[str] = field(default_factory=list)
    is_variadic: bool = False
    binding_name: str | None = None
    source_line: int = 0
    source_file: str = ""

    @property
    def entry(self) -> MenaiCFGBlock:
        """The entry block (always the first block)."""
        return self.blocks[0]

    @property
    def param_count(self) -> int:
        """Get the number of parameters (length of the params list)."""
        return len(self.params)

    def __repr__(self) -> str:
        name = self.binding_name or "<lambda>"
        lines = [f"MenaiCFGFunction {name}({', '.join(self.params)}):"]
        if self.free_vars:
            lines.append(f"  free_vars: {self.free_vars}")

        for block in self.blocks:
            lines.append(repr(block))

        return "\n".join(lines)


def _fmt_values(vs: List[MenaiCFGValue]) -> str:
    return "[" + ", ".join(str(v) for v in vs) + "]"


def _fmt_instr(instr: MenaiCFGInstr) -> str:
    """One-line human-readable representation of a non-terminator instruction."""
    if isinstance(instr, MenaiCFGConstInstr):
        return f"{instr.result} = const {instr.value!r}"

    if isinstance(instr, MenaiCFGGlobalInstr):
        return f"{instr.result} = global {instr.name!r}"

    if isinstance(instr, MenaiCFGParamInstr):
        return f"{instr.result} = param {instr.index} ({instr.param_name!r})"

    if isinstance(instr, MenaiCFGFreeVarInstr):
        return f"{instr.result} = free_var {instr.index} ({instr.var_name!r})"

    if isinstance(instr, MenaiCFGBuiltinInstr):
        return f"{instr.result} = builtin {instr.op!r} {_fmt_values(instr.args)}"

    if isinstance(instr, MenaiCFGCallInstr):
        return f"{instr.result} = call {instr.func} {_fmt_values(instr.args)}"

    if isinstance(instr, MenaiCFGApplyInstr):
        return f"{instr.result} = apply {instr.func} {instr.arg_list}"

    if isinstance(instr, MenaiCFGMakeClosureInstr):
        name = instr.function.binding_name or "<lambda>"
        return f"{instr.result} = make_closure {name!r} {_fmt_values(instr.captures)}"

    if isinstance(instr, MenaiCFGPhiInstr):
        parts = ", ".join(f"{v} <- block{b.id}" for v, b in instr.incoming)
        return f"{instr.result} = phi [{parts}]"

    if isinstance(instr, MenaiCFGTraceInstr):
        return f"{instr.result} = trace {_fmt_values(instr.messages)} {instr.value}"

    return f"<unknown instr {type(instr).__name__}>"


def _fmt_term(term: MenaiCFGTerminator) -> str:
    """One-line human-readable representation of a terminator."""
    if isinstance(term, MenaiCFGJumpTerm):
        return f"jump block{term.target.id}"

    if isinstance(term, MenaiCFGBranchTerm):
        return (f"branch {term.cond} → block{term.true_block.id} / "
                f"block{term.false_block.id}")

    if isinstance(term, MenaiCFGReturnTerm):
        return f"return {term.value}"

    if isinstance(term, MenaiCFGTailCallTerm):
        return f"tail_call {term.func} {_fmt_values(term.args)}"

    if isinstance(term, MenaiCFGTailApplyTerm):
        return f"tail_apply {term.func} {term.arg_list}"

    if isinstance(term, MenaiCFGSelfLoopTerm):
        return f"self_loop {_fmt_values(term.args)}"

    if isinstance(term, MenaiCFGRaiseTerm):
        return f"raise {term.message!r}"

    return f"<unknown term {type(term).__name__}>"


def relink_predecessors(func: MenaiCFGFunction) -> None:
    """
    Recompute the `predecessors` list for every block in `func` from scratch.

    Called after any structural change to the CFG.  Mutates the predecessor
    lists in place.
    """
    for block in func.blocks:
        block.predecessors = []

    for block in func.blocks:
        term = block.terminator
        if isinstance(term, MenaiCFGJumpTerm):
            _safe_add_pred(term.target, block, func)

        elif isinstance(term, MenaiCFGBranchTerm):
            _safe_add_pred(term.true_block, block, func)
            _safe_add_pred(term.false_block, block, func)


def _safe_add_pred(
    target: MenaiCFGBlock,
    pred: MenaiCFGBlock,
    func: MenaiCFGFunction,
) -> None:
    if any(b.id == target.id for b in func.blocks):
        target.predecessors.append(pred)


def remap_term(
    term: MenaiCFGTerminator | None,
    remap_block: Callable[[MenaiCFGBlock], MenaiCFGBlock],
) -> MenaiCFGTerminator | None:
    """Return a new terminator with all block references remapped."""
    if term is None:
        return None

    if isinstance(term, MenaiCFGJumpTerm):
        new_target = remap_block(term.target)
        if new_target is term.target:
            return term

        return MenaiCFGJumpTerm(target=new_target)

    if isinstance(term, MenaiCFGBranchTerm):
        new_true = remap_block(term.true_block)
        new_false = remap_block(term.false_block)
        if new_true is term.true_block and new_false is term.false_block:
            return term

        return MenaiCFGBranchTerm(
            cond=term.cond,
            true_block=new_true,
            false_block=new_false,
        )

    return term


def value_ids_in_instr(instr: 'MenaiCFGInstr') -> List[int]:
    """Return all input value ids referenced by a non-phi instruction."""
    if isinstance(instr, MenaiCFGBuiltinInstr):
        return [a.id for a in instr.args]

    if isinstance(instr, MenaiCFGCallInstr):
        return [instr.func.id] + [a.id for a in instr.args]

    if isinstance(instr, MenaiCFGApplyInstr):
        return [instr.func.id, instr.arg_list.id]

    if isinstance(instr, MenaiCFGMakeClosureInstr):
        return [c.id for c in instr.captures]

    if isinstance(instr, MenaiCFGPatchClosureInstr):
        return [instr.closure.id, instr.value.id]

    if isinstance(instr, MenaiCFGTraceInstr):
        return [m.id for m in instr.messages] + [instr.value.id]

    # MenaiCFGConstInstr, MenaiCFGGlobalInstr, MenaiCFGParamInstr,
    # MenaiCFGFreeVarInstr: no input value references.
    return []


def value_ids_in_term(term: 'MenaiCFGTerminator') -> List[int]:
    """Return all input value ids referenced by a terminator."""
    if isinstance(term, MenaiCFGReturnTerm):
        return [term.value.id]

    if isinstance(term, MenaiCFGBranchTerm):
        return [term.cond.id]

    if isinstance(term, MenaiCFGTailCallTerm):
        return [term.func.id] + [a.id for a in term.args]

    if isinstance(term, MenaiCFGTailApplyTerm):
        return [term.func.id, term.arg_list.id]

    if isinstance(term, MenaiCFGSelfLoopTerm):
        return [a.id for a in term.args]

    # MenaiCFGJumpTerm, MenaiCFGRaiseTerm: no value references.
    return []
