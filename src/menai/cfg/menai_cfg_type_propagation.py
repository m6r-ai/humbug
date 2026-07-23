"""
CFG pass: type propagation and guard insertion.

Determines the static type of each MenaiCFGValue within a function and inserts
MenaiCFGGuardInstr instructions where a type-specific operation receives an
operand whose type is not statically known.  The operational opcodes in the C VM
rely on guards having already verified types and omit their own runtime type
checks for performance.

Type knowledge sources
----------------------
1. Constants — a MenaiCFGConstInstr's value has a directly readable type.
2. Builtin results — each builtin has a known result type (from
   BUILTIN_TYPE_SIGNATURES in menai_type_signatures.py).
3. Guards — a MenaiCFGGuardInstr asserts a type, so the value is known to
   have that type after the guard executes.
4. Phi joins — if all incoming values share the same known type, the phi
   result has that type; otherwise it is unknown.

Values whose type cannot be determined (parameters, free variables, call
results, apply results) are "unknown".  When an unknown-typed value is used
as an argument to a builtin that expects a specific type, a guard is inserted
before the builtin to check that type at runtime.
Similarly, when a branch terminator's condition has an unknown type, a
boolean guard is inserted before the branch.

Algorithm
---------
The pass works in two phases:

Phase 1 — Forward type propagation.  Walk each block in reverse-post-order.
For each instruction, compute the result type from the instruction's operands
and type signature.  At phi nodes, the type is the meet (join) of all incoming
value types.  Because phi nodes can reference values from later blocks
(back-edges in loops), we iterate to a fixed point.

Phase 2 — Guard insertion.  Walk each block in order.  For each builtin call,
check each argument: if its type is known and matches the expected type, no
guard is needed.  If its type is unknown but the builtin expects a specific
type, insert a guard before the builtin call.  For branch terminators, guard
the condition as a boolean.  After a guard, the value's type becomes known for
subsequent uses, so multiple builtins (or branches) using the same
unknown-typed value only need one guard.

The pass mutates the CFG in place — it inserts MenaiCFGGuardInstr instructions
into block.instrs lists and returns the same MenaiCFGFunction.
"""

from menai.bytecode.menai_type_signatures import BUILTIN_TYPE_SIGNATURES
from menai.cfg.menai_cfg import (
    MenaiCFGApplyInstr,
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGBuiltinInstr,
    MenaiCFGCallInstr,
    MenaiCFGConstInstr,
    MenaiCFGInstr,
    MenaiCFGFreeVarInstr,
    MenaiCFGFunction,
    MenaiCFGGlobalInstr,
    MenaiCFGGuardInstr,
    MenaiCFGMakeClosureInstr,
    MenaiCFGMakeDictInstr,
    MenaiCFGMakeListInstr,
    MenaiCFGMakeSetInstr,
    MenaiCFGMakeStructInstr,
    MenaiCFGParamInstr,
    MenaiCFGPhiInstr,
)
from menai.cfg.menai_cfg_optimization_pass import MenaiCFGOptimizationPass
from menai.menai_value import (
    MenaiBoolean,
    MenaiBytes,
    MenaiComplex,
    MenaiDict,
    MenaiFloat,
    MenaiFunction,
    MenaiInteger,
    MenaiList,
    MenaiNone,
    MenaiSet,
    MenaiString,
    MenaiStruct,
    MenaiStructType,
    MenaiSymbol,
)

# Map from Python value class to Menai type name string.
_VALUE_TYPE_MAP = {
    MenaiNone: 'none',
    MenaiBoolean: 'boolean',
    MenaiInteger: 'integer',
    MenaiFloat: 'float',
    MenaiComplex: 'complex',
    MenaiString: 'string',
    MenaiSymbol: 'symbol',
    MenaiList: 'list',
    MenaiDict: 'dict',
    MenaiSet: 'set',
    MenaiFunction: 'function',
    MenaiBytes: 'bytes',
    MenaiStruct: 'struct',
    MenaiStructType: 'struct-type',
}


def _value_type(value: object) -> str | None:
    """Return the Menai type name for a MenaiValue instance, or None."""
    for cls, name in _VALUE_TYPE_MAP.items():
        if isinstance(value, cls):
            return name

    return None


class MenaiCFGTypePropagation(MenaiCFGOptimizationPass):
    """
    CFG optimization pass that propagates types and inserts guards.

    See module docstring for the algorithm description.
    """

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        """Run type propagation and guard insertion on a single function."""
        # Phase 1: forward type propagation to fixed point.
        types = self._propagate_types(func)

        # Phase 2: insert guards where needed.
        changed = self._insert_guards(func, types)

        return func, changed

    def _propagate_types(self, func: MenaiCFGFunction) -> dict[int, str | None]:
        """
        Compute the known type of every MenaiCFGValue in the function.

        Returns a dict mapping value id → type name (or None for unknown).
        Iterates to a fixed point to handle phi nodes with back-edge inputs.
        """
        types: dict[int, str | None] = {}

        # Iterate to fixed point.
        while True:
            changed = False
            for block in func.blocks:
                for instr in block.instrs:
                    if isinstance(instr, (MenaiCFGConstInstr, MenaiCFGGlobalInstr,
                                          MenaiCFGParamInstr, MenaiCFGFreeVarInstr,
                                          MenaiCFGBuiltinInstr, MenaiCFGCallInstr,
                                          MenaiCFGApplyInstr, MenaiCFGMakeClosureInstr,
                                          MenaiCFGMakeStructInstr, MenaiCFGMakeListInstr,
                                          MenaiCFGMakeSetInstr, MenaiCFGMakeDictInstr,
                                          MenaiCFGPhiInstr)):
                        new_type = self._instr_type(instr, types)
                        old_type = types.get(instr.result.id)
                        if new_type != old_type:
                            types[instr.result.id] = new_type
                            changed = True

            if not changed:
                break

        return types

    def _instr_type(self, instr: object, types: dict[int, str | None]) -> str | None:
        """Determine the result type of an instruction given current type knowledge."""
        if isinstance(instr, MenaiCFGConstInstr):
            return _value_type(instr.value)

        if isinstance(instr, MenaiCFGGuardInstr):
            return instr.expected_type

        if isinstance(instr, MenaiCFGBuiltinInstr):
            sig = BUILTIN_TYPE_SIGNATURES.get(instr.op)
            if sig is not None:
                return sig[1]

            return None

        if isinstance(instr, MenaiCFGMakeListInstr):
            return 'list'

        if isinstance(instr, MenaiCFGMakeSetInstr):
            return 'set'

        if isinstance(instr, MenaiCFGMakeDictInstr):
            return 'dict'

        if isinstance(instr, MenaiCFGMakeStructInstr):
            return 'struct'

        if isinstance(instr, MenaiCFGPhiInstr):
            return self._phi_type(instr, types)

        # Parameters, free vars, globals, calls, apply, closures: unknown.
        return None

    def _phi_type(self, instr: MenaiCFGPhiInstr, types: dict[int, str | None]) -> str | None:
        """Compute the type of a phi node from its incoming values."""
        result_type: str | None = None
        for incoming_val, _ in instr.incoming:
            incoming_type = types.get(incoming_val.id)
            if incoming_type is None:
                return None

            if result_type is None:
                result_type = incoming_type

            elif result_type != incoming_type:
                return None

        return result_type

    def _insert_guards(
        self,
        func: MenaiCFGFunction,
        types: dict[int, str | None],
    ) -> bool:
        """
        Insert guard instructions where type-specific builtins receive
        operands of unknown type.

        As guards are inserted, the types dict is updated so that subsequent
        uses of the same value don't trigger redundant guards.

        Returns True if any guards were inserted.
        """
        changed = False

        for block in func.blocks:
            new_instrs: list[MenaiCFGInstr] = []
            for instr in block.instrs:
                if isinstance(instr, MenaiCFGBuiltinInstr):
                    self._guard_builtin_args(instr, types, new_instrs)

                new_instrs.append(instr)

            self._guard_branch(block, types, new_instrs)

            if len(new_instrs) != len(block.instrs):
                block.instrs = new_instrs
                changed = True

        return changed

    def _guard_builtin_args(
        self,
        instr: MenaiCFGBuiltinInstr,
        types: dict[int, str | None],
        new_instrs: list[MenaiCFGInstr],
    ) -> None:
        """
        Check the arguments of a builtin call and insert guards for any
        argument whose type is unknown but whose expected type is specific.

        Appends guard instructions to new_instrs as needed and updates the
        types dict so that later uses see the guarded type.
        """
        sig = BUILTIN_TYPE_SIGNATURES.get(instr.op)
        if sig is None:
            return

        arg_types, _ = sig
        for i, arg in enumerate(instr.args):
            if i >= len(arg_types):
                break

            expected = arg_types[i]
            if expected is None or expected == 'any':
                continue

            known = types.get(arg.id)
            if known == expected:
                continue

            new_instrs.append(MenaiCFGGuardInstr(
                value=arg,
                expected_type=expected,
            ))
            types[arg.id] = expected

    def _guard_branch(
        self,
        block: MenaiCFGBlock,
        types: dict[int, str | None],
        new_instrs: list[MenaiCFGInstr],
    ) -> None:
        """
        Insert a boolean guard on a branch terminator's condition if its
        type is not statically known to be boolean.

        Appends a guard to new_instrs if needed and updates the types dict.
        """
        term = block.terminator
        if not isinstance(term, MenaiCFGBranchTerm):
            return

        cond_type = types.get(term.cond.id)
        if cond_type == 'boolean':
            return

        new_instrs.append(MenaiCFGGuardInstr(
            value=term.cond,
            expected_type='boolean',
        ))
        types[term.cond.id] = 'boolean'
