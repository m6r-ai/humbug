"""
CFG stack scheduler for the Menai VM code generator.

Analyses a MenaiCFGFunction and classifies each SSA value as either
stack-transient or slotted, before any bytecode is emitted.

Position in the pipeline
------------------------
    MenaiCFGFunction  →  MenaiCFGStackScheduler  →  StackSchedule
                                                         ↓
                                              MenaiVMCodeGen (consults schedule)

What "stack-transient" means
-----------------------------
A stack-transient value is one that the VM code generator can leave on the
evaluation stack rather than storing to a local variable slot.  No STORE_VAR
is emitted after the instruction that produces it, and no LOAD_VAR is emitted
before the instruction that consumes it — the value simply flows from one
instruction to the next via the top of the stack.

Classification rules
--------------------
A value V produced by instruction I at position i in block B is
stack-transient if and only if ALL of the following hold:

  1. V has exactly one use across the entire function.

  2. That single use is either:
       (a) the instruction at position i+1 in block B (i.e. the immediately
           following regular instruction), or
       (b) the block terminator, when I is the last regular instruction in B
           (position i == len(B.instrs) - 1).

  3. V is the LAST SSA operand consumed by the consuming instruction.
     Because the stack is LIFO, only the final value pushed before an opcode
     fires can be the value left on top of the stack by the previous
     instruction.  Any earlier operand will have something pushed on top of
     it before the opcode fires.

  4. None of the hard exclusions apply (see below).

Hard exclusions — values that are always slotted
-------------------------------------------------
  - MenaiCFGParamInstr / MenaiCFGFreeVarInstr results: pre-assigned to fixed
    slots by the VM calling convention; never on the stack at block entry.
  - MenaiCFGPhiInstr results: written by predecessor blocks via STORE_VAR
    before a jump; the join block reads from the slot.
  - MenaiCFGMakeClosureInstr results where needs_patching=True: the
    PATCH_CLOSURE opcode references the closure by slot number, so the
    closure must be in a slot immediately after creation.

Special-case builtins — last SSA operand is NOT the last thing pushed
----------------------------------------------------------------------
Several builtins synthesise extra arguments after the last SSA operand:

  range(a, b)           → pushes synthesised MenaiInteger(1) last
  integer->complex(x)   → pushes synthesised MenaiInteger(0) last
  integer->string(x)    → pushes synthesised MenaiInteger(10) last
  float->complex(x)     → pushes synthesised MenaiFloat(0.0) last
  string->integer(x)    → pushes synthesised MenaiInteger(10) last
  string->list(x)       → pushes synthesised MenaiString("") last
  list->string(x)       → pushes synthesised MenaiString("") last
  dict-get(d, k)        → pushes synthesised LOAD_NONE last
  string-slice(s, i)    → reloads s and computes STRING_LENGTH last
  list-slice(l, i)      → reloads l and computes LIST_LENGTH last

For these cases (when invoked with the default-argument arity), no SSA
operand of the consuming builtin can be stack-transient.

Last-operand rules per consuming instruction type
-------------------------------------------------
For instructions where the last SSA operand IS the last thing pushed:

  MenaiCFGConstInstr       — no SSA operands (value is inline); never a consumer
  MenaiCFGGlobalInstr      — no SSA operands; never a consumer
  MenaiCFGParamInstr       — no SSA operands; never a consumer
  MenaiCFGFreeVarInstr     — no SSA operands; never a consumer
  MenaiCFGPhiInstr         — operands come from predecessor blocks; never a
                             same-block consumer
  MenaiCFGCallInstr        — last SSA operand is `func` (pushed after all args)
  MenaiCFGApplyInstr       — last SSA operand is `arg_list`
  MenaiCFGTraceInstr       — last SSA operand is `value` (messages are each
                             consumed by EMIT_TRACE before `value` is pushed)
  MenaiCFGBuiltinInstr     — last SSA operand is args[-1], EXCEPT for the
                             default-arity special cases listed above
  MenaiCFGMakeClosureInstr — last SSA operand is captures[-1] if any captures
                             exist AND needs_patching is False; but see note
                             below on MakeClosureInstr as a consumer

  Terminators:
  MenaiCFGReturnTerm       — last SSA operand is `value`
  MenaiCFGTailCallTerm     — last SSA operand is `func`
  MenaiCFGTailApplyTerm    — last SSA operand is `arg_list`
  MenaiCFGBranchTerm       — last SSA operand is `cond`
  MenaiCFGSelfLoopTerm     — last SSA operand is args[-1] (if any args)

  MenaiCFGJumpTerm / MenaiCFGRaiseTerm — no SSA operands; never a consumer

Note on MakeClosureInstr as a consumer
---------------------------------------
When needs_patching is False and captures is non-empty, captures are pushed
in order and MAKE_CLOSURE fires — so captures[-1] is the last thing pushed.
When needs_patching is True, the closure is stored to a slot immediately and
then PATCH_CLOSURE instructions follow — the last capture is not the last
thing pushed before any single opcode, so no capture can be transient.
When captures is empty, there are no SSA operands to be transient.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Set

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
    MenaiCFGParamInstr,
    MenaiCFGPatchClosureInstr,
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
from menai.menai_value import MenaiValue

# ---------------------------------------------------------------------------
# Special-case builtins where the default-arity form synthesises extra
# arguments AFTER the last SSA operand.  When invoked at these arities, no
# SSA operand can be stack-transient as a consumer of that builtin.
# ---------------------------------------------------------------------------
_SYNTH_LAST_AT_ARITY: dict[str, int] = {
    'range': 2,
    'integer->complex': 1,
    'integer->string': 1,
    'float->complex': 1,
    'string->integer': 1,
    'string->list': 1,
    'list->string': 1,
    'dict-get': 2,
    'string-slice': 2,
    'list-slice': 2,
}


@dataclass
class StackSchedule:
    """
    The result of stack scheduling analysis for one MenaiCFGFunction.

    transient_ids
        The set of SSA value IDs that are stack-transient.  All other
        SSA values that produce a result are either rematerialisable or slotted.

    remat_values
        Maps SSA value ID → MenaiValue for constants that are rematerialisable.
        A rematerialisable value is a MenaiCFGConstInstr result that is NOT
        stack-transient.  Instead of spilling to a slot, the codegen re-emits
        the constant load instruction at each use site.  This eliminates the
        slot entirely regardless of use count.

    Usage by the code generator::

        schedule = MenaiCFGStackScheduler().schedule(func)
        if schedule.is_transient(value):
            # do not emit STORE_VAR / LOAD_VAR for this value
        elif schedule.is_remat(value):
            # emit a fresh LOAD_CONST / LOAD_NONE / etc. at each use site
    """
    transient_ids: Set[int] = field(default_factory=set)
    remat_values: Dict[int, MenaiValue] = field(default_factory=dict)

    def is_transient(self, value: MenaiCFGValue) -> bool:
        """Return True if `value` should be left on the stack (no slot)."""
        return value.id in self.transient_ids

    def is_remat(self, value: MenaiCFGValue) -> bool:
        """Return True if `value` should be rematerialised at each use site."""
        return value.id in self.remat_values

    def remat_value_of(self, value: MenaiCFGValue) -> MenaiValue:
        """Return the MenaiValue to rematerialise for `value`.  Asserts it exists."""
        assert value.id in self.remat_values, (
            f"StackSchedule: SSA value {value} is not rematerialisable"
        )
        return self.remat_values[value.id]


class MenaiCFGStackScheduler:
    """
    Analyses a MenaiCFGFunction and produces a StackSchedule.

    The scheduler performs two linear passes over the function's blocks:

      Pass 1 — use counting: walk every instruction and terminator in every
               block and count how many times each SSA value is referenced
               as an operand.

      Pass 2 — classification: for each block, walk its instruction list
               and check whether each produced value satisfies all
               stack-transient conditions.  Constants that do not qualify
               as stack-transient are classified as rematerialisable instead
               of slotted.

    The scheduler does not modify the CFG.
    """

    def schedule(self, func: MenaiCFGFunction) -> StackSchedule:
        """
        Analyse `func` and return a StackSchedule.

        Args:
            func: The CFG function to analyse.

        Returns:
            A StackSchedule classifying each SSA value.
        """
        use_counts = self._count_uses(func)
        transient_ids, remat_values = self._classify(func, use_counts)
        return StackSchedule(transient_ids=transient_ids, remat_values=remat_values)

    # ------------------------------------------------------------------
    # Pass 1: use counting
    # ------------------------------------------------------------------

    def _count_uses(self, func: MenaiCFGFunction) -> dict[int, int]:
        """
        Return a dict mapping SSA value ID → total use count across `func`.

        Every reference to a MenaiCFGValue as an operand (in any instruction,
        patch instruction, or terminator, in any block) increments its count.
        """
        counts: dict[int, int] = {}

        def use(v: MenaiCFGValue) -> None:
            counts[v.id] = counts.get(v.id, 0) + 1

        for block in func.blocks:
            for instr in block.instrs:
                self._count_instr_uses(instr, use)
            for patch in block.patch_instrs:
                use(patch.closure)
                use(patch.value)
            if block.terminator is not None:
                self._count_term_uses(block.terminator, use)

        return counts

    def _count_instr_uses(self, instr: MenaiCFGInstr, use) -> None:  # type: ignore[type-arg]
        """Increment use counts for all SSA operands of `instr`."""
        if isinstance(instr, (MenaiCFGParamInstr,
                               MenaiCFGFreeVarInstr,
                               MenaiCFGConstInstr,
                               MenaiCFGGlobalInstr)):
            pass  # No SSA operands.

        elif isinstance(instr, MenaiCFGPhiInstr):
            for val, _block in instr.incoming:
                use(val)

        elif isinstance(instr, MenaiCFGBuiltinInstr):
            for a in instr.args:
                use(a)

        elif isinstance(instr, MenaiCFGCallInstr):
            for a in instr.args:
                use(a)
            use(instr.func)

        elif isinstance(instr, MenaiCFGApplyInstr):
            use(instr.func)
            use(instr.arg_list)

        elif isinstance(instr, MenaiCFGMakeClosureInstr):
            for c in instr.captures:
                use(c)

        elif isinstance(instr, MenaiCFGTraceInstr):
            for m in instr.messages:
                use(m)
            use(instr.value)

        elif isinstance(instr, MenaiCFGPatchClosureInstr):
            use(instr.closure)
            use(instr.value)

    def _count_term_uses(self, term: MenaiCFGTerminator, use) -> None:  # type: ignore[type-arg]
        """Increment use counts for all SSA operands of `term`."""
        if isinstance(term, MenaiCFGReturnTerm):
            use(term.value)
        elif isinstance(term, MenaiCFGJumpTerm):
            pass  # No SSA operands.
        elif isinstance(term, MenaiCFGBranchTerm):
            use(term.cond)
        elif isinstance(term, MenaiCFGTailCallTerm):
            for a in term.args:
                use(a)
            use(term.func)
        elif isinstance(term, MenaiCFGTailApplyTerm):
            use(term.func)
            use(term.arg_list)
        elif isinstance(term, MenaiCFGSelfLoopTerm):
            for a in term.args:
                use(a)
        elif isinstance(term, (MenaiCFGRaiseTerm,)):
            pass  # No SSA operands.

    # ------------------------------------------------------------------
    # Pass 2: classification
    # ------------------------------------------------------------------

    def _classify(
        self,
        func: MenaiCFGFunction,
        use_counts: dict[int, int],
    ) -> tuple[Set[int], Dict[int, MenaiValue]]:
        """
        Return (transient_ids, remat_values) for the function.

        For each block, walk its instruction list.  For each instruction that
        produces a result, check all three conditions (use count, position,
        last-operand, preceding-operands-all-transient) and add to the
        transient set if all pass.

        Constants that do not qualify as transient are added to remat_values
        instead of being left for slot allocation.  remat_values maps SSA
        value ID → MenaiValue so the codegen can re-emit the load at each use.

        The transient set is built incrementally so that condition 4 can
        consult it: a preceding operand displaces the stack top only if it
        will cause a LOAD_VAR to be emitted, which happens when it is slotted
        (neither transient nor rematerialisable).
        """
        transient: Set[int] = set()
        remat: Dict[int, MenaiValue] = {}

        for block in func.blocks:
            instrs = block.instrs
            n = len(instrs)

            for i, instr in enumerate(instrs):
                result = self._result_of(instr)
                if result is None:
                    continue  # No result (PatchClosureInstr).

                # Hard exclusions.
                if self._is_hard_excluded(instr):
                    continue

                # Rematerialisable constants: MenaiCFGConstInstr results that
                # are not stack-transient.  We attempt transient classification
                # first; if that fails, a ConstInstr falls through to remat.
                is_const = isinstance(instr, MenaiCFGConstInstr)

                # --- Attempt transient classification ---
                # Condition 1: exactly one use.
                qualifies_transient = use_counts.get(result.id, 0) == 1

                if qualifies_transient:
                    # Condition 2: the single use is the next instruction or
                    # the terminator.
                    if i + 1 < n:
                        consumer: MenaiCFGInstr | MenaiCFGTerminator = instrs[i + 1]
                        is_term_consumer = False
                    elif block.terminator is not None:
                        consumer = block.terminator
                        is_term_consumer = True
                    else:
                        qualifies_transient = False

                if qualifies_transient:
                    # Condition 3: result is the last SSA operand of consumer.
                    if not self._is_last_operand(result, consumer, is_term_consumer):
                        qualifies_transient = False

                if qualifies_transient:
                    # Condition 4: all preceding operands of consumer must
                    # themselves be transient — meaning no load instruction
                    # (LOAD_VAR or LOAD_CONST) will be emitted for them at
                    # the use site.  Remat operands still emit a LOAD_CONST
                    # at the use site, which would displace this value from
                    # the stack top just as LOAD_VAR would.
                    preceding = self._preceding_operands(result, consumer, is_term_consumer)
                    if any(p.id not in transient for p in preceding):
                        qualifies_transient = False

                if qualifies_transient:
                    transient.add(result.id)
                elif is_const:
                    # Not transient but is a constant: rematerialise instead.
                    remat[result.id] = instr.value  # type: ignore[union-attr]

        return transient, remat

    def _result_of(self, instr: MenaiCFGInstr) -> MenaiCFGValue | None:
        """Return the SSA result of `instr`, or None if it has no result."""
        if isinstance(instr, MenaiCFGPatchClosureInstr):
            return None
        return instr.result  # type: ignore[union-attr]

    def _is_hard_excluded(self, instr: MenaiCFGInstr) -> bool:
        """
        Return True if the result of `instr` must always be slotted,
        regardless of use count or position.
        """
        if isinstance(instr, (MenaiCFGParamInstr, MenaiCFGFreeVarInstr)):
            return True
        if isinstance(instr, MenaiCFGPhiInstr):
            return True
        if isinstance(instr, MenaiCFGMakeClosureInstr) and instr.needs_patching:
            return True
        return False

    def _is_last_operand(
        self,
        value: MenaiCFGValue,
        consumer: MenaiCFGInstr | MenaiCFGTerminator,
        is_terminator: bool,
    ) -> bool:
        """
        Return True if `value` is the last SSA operand consumed by `consumer`.

        "Last SSA operand" means the last SSA value pushed onto the stack
        before the consumer's opcode fires.  For instructions with synthesised
        arguments that are pushed after the last SSA operand, this returns
        False for all SSA operands.
        """
        if is_terminator:
            return self._is_last_operand_of_term(value, consumer)  # type: ignore[arg-type]
        return self._is_last_operand_of_instr(value, consumer)  # type: ignore[arg-type]

    def _is_last_operand_of_instr(
        self, value: MenaiCFGValue, instr: MenaiCFGInstr
    ) -> bool:
        if isinstance(instr, (MenaiCFGConstInstr,
                               MenaiCFGGlobalInstr,
                               MenaiCFGParamInstr,
                               MenaiCFGFreeVarInstr,
                               MenaiCFGPhiInstr,
                               MenaiCFGPatchClosureInstr)):
            # No SSA operands that arrive via stack, or phi operands are
            # cross-block (never same-block immediate predecessor).
            return False

        if isinstance(instr, MenaiCFGCallInstr):
            # Push order: args[0], args[1], ..., args[-1], func.
            # Last SSA operand is func.
            return value.id == instr.func.id

        if isinstance(instr, MenaiCFGApplyInstr):
            # Push order: func, arg_list.
            return value.id == instr.arg_list.id

        if isinstance(instr, MenaiCFGTraceInstr):
            # Each message is pushed then immediately consumed by EMIT_TRACE.
            # Then `value` is pushed last.
            return value.id == instr.value.id

        if isinstance(instr, MenaiCFGBuiltinInstr):
            return self._is_last_operand_of_builtin(value, instr)

        if isinstance(instr, MenaiCFGMakeClosureInstr):
            # needs_patching=True: closure goes to slot immediately, then
            # PATCH_CLOSURE instructions follow — no single "last push".
            if instr.needs_patching:
                return False
            if not instr.captures:
                return False
            # Push order: captures[0], ..., captures[-1], then MAKE_CLOSURE.
            return value.id == instr.captures[-1].id

        return False

    def _preceding_operands(
        self,
        last_value: MenaiCFGValue,
        consumer: MenaiCFGInstr | MenaiCFGTerminator,
        is_terminator: bool,
    ) -> list[MenaiCFGValue]:
        """
        Return the SSA operands of `consumer` that are pushed onto the stack
        BEFORE `last_value` (which is the last operand).

        These are the operands that will have LOAD_VAR emitted for them unless
        they are themselves transient.  If any is slotted, a LOAD_VAR will be
        emitted between `last_value`'s definition and the opcode, displacing
        `last_value` from the stack top.

        Returns an empty list when there are no preceding operands (unary
        consumers, or consumers where last_value is the only operand).
        """
        if is_terminator:
            return self._preceding_operands_of_term(last_value, consumer)  # type: ignore[arg-type]
        return self._preceding_operands_of_instr(last_value, consumer)  # type: ignore[arg-type]

    def _preceding_operands_of_instr(
        self, last_value: MenaiCFGValue, instr: MenaiCFGInstr
    ) -> list[MenaiCFGValue]:
        if isinstance(instr, MenaiCFGCallInstr):
            # Push order: args[0..N-1], func.  last_value == func.
            return list(instr.args)

        if isinstance(instr, MenaiCFGApplyInstr):
            # Push order: func, arg_list.  last_value == arg_list.
            return [instr.func]

        if isinstance(instr, MenaiCFGTraceInstr):
            # Messages are each pushed and immediately consumed by EMIT_TRACE
            # before `value` is pushed — they don't sit on the stack alongside
            # `value`, so they are not "preceding" in the displacement sense.
            return []

        if isinstance(instr, MenaiCFGBuiltinInstr):
            # Push order: args[0..N-1].  last_value == args[-1].
            # Preceding = args[0..N-2].
            return list(instr.args[:-1])

        if isinstance(instr, MenaiCFGMakeClosureInstr):
            # Push order: captures[0..N-1].  last_value == captures[-1].
            return list(instr.captures[:-1])

        return []

    def _preceding_operands_of_term(
        self, last_value: MenaiCFGValue, term: MenaiCFGTerminator
    ) -> list[MenaiCFGValue]:
        if isinstance(term, MenaiCFGTailCallTerm):
            # Push order: args[0..N-1], func.  last_value == func.
            return list(term.args)

        if isinstance(term, MenaiCFGTailApplyTerm):
            # Push order: func, arg_list.  last_value == arg_list.
            return [term.func]

        if isinstance(term, MenaiCFGSelfLoopTerm):
            # Push order: args[0..N-1].  last_value == args[-1].
            return list(term.args[:-1])

        # ReturnTerm, BranchTerm — single operand, no preceding.
        return []

    def _is_last_operand_of_builtin(
        self, value: MenaiCFGValue, instr: MenaiCFGBuiltinInstr
    ) -> bool:
        """
        Return True if `value` is the last SSA operand pushed for this builtin.

        For special-case builtins invoked at the default arity (where a
        synthesised argument is pushed after all SSA operands), returns False
        for all SSA operands.
        """
        op = instr.op
        n_args = len(instr.args)

        # Check whether this op/arity combination synthesises a trailing arg.
        synth_arity = _SYNTH_LAST_AT_ARITY.get(op)
        if synth_arity is not None and n_args == synth_arity:
            return False

        # Normal case: args are pushed in order, last arg is last on stack.
        if not instr.args:
            return False
        return value.id == instr.args[-1].id

    def _is_last_operand_of_term(
        self, value: MenaiCFGValue, term: MenaiCFGTerminator
    ) -> bool:
        if isinstance(term, MenaiCFGReturnTerm):
            return value.id == term.value.id

        if isinstance(term, MenaiCFGJumpTerm):
            return False  # No SSA operands.

        if isinstance(term, MenaiCFGBranchTerm):
            return value.id == term.cond.id

        if isinstance(term, MenaiCFGTailCallTerm):
            # Push order: args[0], ..., args[-1], func.
            return value.id == term.func.id

        if isinstance(term, MenaiCFGTailApplyTerm):
            # Push order: func, arg_list.
            return value.id == term.arg_list.id

        if isinstance(term, MenaiCFGSelfLoopTerm):
            if not term.args:
                return False
            return value.id == term.args[-1].id

        if isinstance(term, MenaiCFGRaiseTerm):
            return False  # No SSA operands.

        return False
