"""
VCode builder — lowers a MenaiCFGFunction (SSA CFG) to MenaiVCodeFunction.

This is the first pass of the VM backend.  It takes the fully optimised SSA
CFG and produces a flat, phi-free linear IR ready for slot allocation,
peephole optimisation, and bytecode emission.

Lowering steps performed for each function
------------------------------------------
1. Compute reverse post-order (RPO) over the reachable CFG blocks.
2. For each block in RPO:
   a. Emit a label for the block.
   b. Emit VCode instructions for each CFG instruction.
   c. Emit phi-elimination moves: for each phi in each *successor* block,
      emit a MenaiVCodeMove copying this block's incoming value into the
      phi result register, immediately before the block's jump/branch.
   d. Emit the block terminator as VCode jumps/returns.
3. Omit the label for the entry block (no predecessor jumps to it by label).
4. Omit unconditional jumps to the immediately following block (fall-through).

Phi elimination
---------------
For each phi instruction  %result = phi [(%v_a, block_A), (%v_b, block_B)]
in a join block J, we insert:
  - At the end of block_A (before its terminator):  MOVE %result ← %v_a
  - At the end of block_B (before its terminator):  MOVE %result ← %v_b

These moves are emitted *after* all other instructions in the predecessor
block but *before* the jump/branch terminator, so that the source value is
still live and the destination is written exactly once on each path.

SSA value → virtual register mapping
--------------------------------------
MenaiCFGValue ids are reused directly as MenaiVCodeReg ids.  Since CFG
values are unique within a function and VCode registers are unique within a
function, this is a safe 1:1 mapping with no renaming required.

Nested functions
----------------
Each MenaiCFGMakeClosureInstr references a nested MenaiCFGFunction.  The
builder recurses to produce a nested MenaiVCodeFunction, which is embedded
in the MenaiVCodeMakeClosure instruction.
"""

from typing import Dict, List, Tuple

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
    MenaiCFGTraceInstr,
    MenaiCFGValue,
)
from menai.menai_vcode import (
    MenaiVCodeApply,
    MenaiVCodeBuiltin,
    MenaiVCodeCall,
    MenaiVCodeFunction,
    MenaiVCodeInstr,
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


class MenaiVCodeBuilder:
    """
    Lowers a MenaiCFGFunction to a MenaiVCodeFunction.

    Usage::

        vcode = MenaiVCodeBuilder().build(cfg_function)
    """

    def build(self, func: MenaiCFGFunction) -> MenaiVCodeFunction:
        """
        Lower the top-level MenaiCFGFunction to a MenaiVCodeFunction.

        Args:
            func: The fully optimised CFG function (top-level module body).

        Returns:
            A MenaiVCodeFunction ready for slot allocation and emission.
        """
        return self._lower_function(func)

    def _lower_function(self, func: MenaiCFGFunction) -> MenaiVCodeFunction:
        """Lower one CFG function (top-level or nested lambda) to VCode."""
        rpo = self._rpo(func)

        # Pre-compute phi moves: for each block, the list of (dst, src) moves
        # to emit before the block's terminator, one per phi in each successor.
        phi_moves: Dict[int, List[Tuple[MenaiVCodeReg, MenaiVCodeReg]]] = {
            block.id: [] for block in rpo
        }
        for block in rpo:
            term = block.terminator
            successors: List[MenaiCFGBlock] = []
            if isinstance(term, MenaiCFGJumpTerm):
                successors = [term.target]

            elif isinstance(term, MenaiCFGBranchTerm):
                successors = [term.true_block, term.false_block]

            for succ in successors:
                for instr in succ.instrs:
                    if not isinstance(instr, MenaiCFGPhiInstr):
                        break
                    for inc_val, inc_pred in instr.incoming:
                        if inc_pred.id == block.id:
                            dst = self._reg(instr.result)
                            src = self._reg(inc_val)
                            phi_moves[block.id].append((dst, src))

        # Emit instructions for each block in RPO order.
        instrs: List[MenaiVCodeInstr] = []
        max_reg_id = -1

        for i, block in enumerate(rpo):
            next_block = rpo[i + 1] if i + 1 < len(rpo) else None

            # Emit a label for every block except the entry block.
            # The entry block has no predecessor that jumps to it by label.
            if i > 0:
                instrs.append(MenaiVCodeLabel(name=self._label(block)))

            # Emit non-terminator instructions.
            for cfg_instr in block.instrs:
                if isinstance(cfg_instr, MenaiCFGPhiInstr):
                    # Phis are eliminated — their results are written by moves
                    # in predecessor blocks.  Track the max reg id for the
                    # phi result so the allocator knows about it.
                    max_reg_id = max(max_reg_id, cfg_instr.result.id)
                    continue

                vcode_instrs = self._lower_instr(cfg_instr)
                instrs.extend(vcode_instrs)
                for vi in vcode_instrs:
                    max_reg_id = max(max_reg_id, self._max_reg_in_instr(vi))

            # Emit patch instructions.
            for patch in block.patch_instrs:
                vi = MenaiVCodePatchClosure(
                    closure=self._reg(patch.closure),
                    capture_index=patch.capture_index,
                    value=self._reg(patch.value),
                )
                instrs.append(vi)
                max_reg_id = max(max_reg_id, patch.closure.id, patch.value.id)

            # Emit phi-elimination moves before the terminator.
            for dst, src in phi_moves[block.id]:
                if dst.id != src.id:
                    instrs.append(MenaiVCodeMove(dst=dst, src=src))
                max_reg_id = max(max_reg_id, dst.id, src.id)

            # Emit the terminator.
            term = block.terminator
            assert term is not None, f"VCodeBuilder: block {block.id} has no terminator"

            if isinstance(term, MenaiCFGReturnTerm):
                instrs.append(MenaiVCodeReturn(value=self._reg(term.value)))
                max_reg_id = max(max_reg_id, term.value.id)

            elif isinstance(term, MenaiCFGJumpTerm):
                target = term.target
                # Omit the jump if the target is the immediately next block.
                if next_block is None or next_block.id != target.id:
                    instrs.append(MenaiVCodeJump(label=self._label(target)))

            elif isinstance(term, MenaiCFGBranchTerm):
                cond = self._reg(term.cond)
                max_reg_id = max(max_reg_id, term.cond.id)
                next_id = next_block.id if next_block is not None else -1

                if next_id == term.false_block.id:
                    # False block falls through — emit JUMP_IF_TRUE to true block.
                    instrs.append(MenaiVCodeJumpIfTrue(cond=cond, label=self._label(term.true_block)))

                elif next_id == term.true_block.id:
                    # True block falls through — emit JUMP_IF_FALSE to false block.
                    instrs.append(MenaiVCodeJumpIfFalse(cond=cond, label=self._label(term.false_block)))

                else:
                    # Neither falls through — emit conditional + unconditional jump.
                    instrs.append(MenaiVCodeJumpIfFalse(cond=cond, label=self._label(term.false_block)))
                    instrs.append(MenaiVCodeJump(label=self._label(term.true_block)))

            elif isinstance(term, MenaiCFGTailCallTerm):
                instrs.append(MenaiVCodeTailCall(
                    func=self._reg(term.func),
                    args=[self._reg(a) for a in term.args],
                ))
                max_reg_id = max(max_reg_id, term.func.id,
                                 *(a.id for a in term.args) if term.args else [-1])

            elif isinstance(term, MenaiCFGTailApplyTerm):
                instrs.append(MenaiVCodeTailApply(
                    func=self._reg(term.func),
                    arg_list=self._reg(term.arg_list),
                ))
                max_reg_id = max(max_reg_id, term.func.id, term.arg_list.id)

            elif isinstance(term, MenaiCFGSelfLoopTerm):
                # Self-loop: jump back to the entry label.  The entry block
                # has no label emitted (it is always first), so we use the
                # special sentinel label "entry" which the bytecode emitter
                # resolves to instruction index 0.
                for arg in term.args:
                    max_reg_id = max(max_reg_id, arg.id)

                # Emit moves for self-loop args into param slots before jumping.
                # The param registers occupy slots 0..P-1; we need to copy the
                # new arg values into those registers.  We emit moves here and
                # the allocator will assign params to their fixed slots.
                param_regs = [
                    self._param_reg(func, idx)
                    for idx in range(len(term.args))
                ]
                for param_reg, arg_val in zip(param_regs, term.args):
                    arg_reg = self._reg(arg_val)
                    if param_reg.id != arg_reg.id:
                        instrs.append(MenaiVCodeMove(dst=param_reg, src=arg_reg))

                instrs.append(MenaiVCodeJump(label="__entry__"))

            elif isinstance(term, MenaiCFGRaiseTerm):
                instrs.append(MenaiVCodeRaise(message=term.message))

            else:
                raise TypeError(
                    f"MenaiVCodeBuilder: unhandled terminator {type(term).__name__}"
                )

        return MenaiVCodeFunction(
            instrs=instrs,
            params=list(func.params),
            free_vars=list(func.free_vars),
            is_variadic=func.is_variadic,
            binding_name=func.binding_name,
            reg_count=max_reg_id + 1,
            source_line=func.source_line,
            source_file=func.source_file,
        )

    def _lower_instr(self, instr: object) -> List[MenaiVCodeInstr]:
        """Lower a single CFG instruction to a list of VCode instructions."""
        if isinstance(instr, (MenaiCFGParamInstr, MenaiCFGFreeVarInstr)):
            # Params and free vars occupy fixed slots assigned by the allocator.
            # No instruction needed — their registers are pre-assigned.
            return []

        if isinstance(instr, MenaiCFGConstInstr):
            return [MenaiVCodeLoadConst(
                dst=self._reg(instr.result),
                value=instr.value,
            )]

        if isinstance(instr, MenaiCFGGlobalInstr):
            return [MenaiVCodeLoadName(
                dst=self._reg(instr.result),
                name=instr.name,
            )]

        if isinstance(instr, MenaiCFGBuiltinInstr):
            return [MenaiVCodeBuiltin(
                dst=self._reg(instr.result),
                op=instr.op,
                args=[self._reg(a) for a in instr.args],
            )]

        if isinstance(instr, MenaiCFGCallInstr):
            return [MenaiVCodeCall(
                dst=self._reg(instr.result),
                func=self._reg(instr.func),
                args=[self._reg(a) for a in instr.args],
            )]

        if isinstance(instr, MenaiCFGApplyInstr):
            return [MenaiVCodeApply(
                dst=self._reg(instr.result),
                func=self._reg(instr.func),
                arg_list=self._reg(instr.arg_list),
            )]

        if isinstance(instr, MenaiCFGMakeClosureInstr):
            child_vcode = self._lower_function(instr.function)
            return [MenaiVCodeMakeClosure(
                dst=self._reg(instr.result),
                function=child_vcode,
                captures=[self._reg(c) for c in instr.captures],
                needs_patching=instr.needs_patching,
            )]

        if isinstance(instr, MenaiCFGPatchClosureInstr):
            return [MenaiVCodePatchClosure(
                closure=self._reg(instr.closure),
                capture_index=instr.capture_index,
                value=self._reg(instr.value),
            )]

        if isinstance(instr, MenaiCFGTraceInstr):
            return [MenaiVCodeTrace(
                dst=self._reg(instr.result),
                messages=[self._reg(m) for m in instr.messages],
                value=self._reg(instr.value),
            )]

        raise TypeError(
            f"MenaiVCodeBuilder: unhandled instruction {type(instr).__name__}"
        )

    def _reg(self, value: MenaiCFGValue) -> MenaiVCodeReg:
        """Convert a CFG SSA value to a VCode virtual register."""
        return MenaiVCodeReg(id=value.id, hint=value.hint)

    def _param_reg(self, func: MenaiCFGFunction, index: int) -> MenaiVCodeReg:
        """Return the virtual register for parameter `index` of `func`."""
        # Params are defined by MenaiCFGParamInstr in the entry block.
        # We find the SSA value id for this param by scanning the entry block.
        for instr in func.blocks[0].instrs:
            if isinstance(instr, MenaiCFGParamInstr) and instr.index == index:
                return self._reg(instr.result)

        raise AssertionError(
            f"MenaiVCodeBuilder: param {index} not found in entry block of {func.binding_name!r}"
        )

    def _label(self, block: MenaiCFGBlock) -> str:
        """Return the label string for a CFG block."""
        return f"__{block.id}_{block.label}__"

    def _rpo(self, func: MenaiCFGFunction) -> List[MenaiCFGBlock]:
        """
        Return reachable blocks in reverse post-order.

        Self-loop back-edges (SelfLoopTerm) are not followed — they are
        back-edges to the entry that are handled by emitting JUMP __entry__.
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

            post_order.append(block)

        dfs(func.entry)
        post_order.reverse()
        return post_order

    def _max_reg_in_instr(self, instr: MenaiVCodeInstr) -> int:
        """Return the highest register id referenced in a VCode instruction."""
        ids: List[int] = []

        if isinstance(instr, MenaiVCodeMove):
            ids = [instr.dst.id, instr.src.id]

        elif isinstance(instr, MenaiVCodeLoadConst):
            ids = [instr.dst.id]

        elif isinstance(instr, MenaiVCodeLoadName):
            ids = [instr.dst.id]

        elif isinstance(instr, MenaiVCodeBuiltin):
            ids = [instr.dst.id] + [r.id for r in instr.args]

        elif isinstance(instr, MenaiVCodeCall):
            ids = [instr.dst.id, instr.func.id] + [r.id for r in instr.args]

        elif isinstance(instr, MenaiVCodeTailCall):
            ids = [instr.func.id] + [r.id for r in instr.args]

        elif isinstance(instr, MenaiVCodeApply):
            ids = [instr.dst.id, instr.func.id, instr.arg_list.id]

        elif isinstance(instr, MenaiVCodeTailApply):
            ids = [instr.func.id, instr.arg_list.id]

        elif isinstance(instr, MenaiVCodeMakeClosure):
            ids = [instr.dst.id] + [r.id for r in instr.captures]

        elif isinstance(instr, MenaiVCodePatchClosure):
            ids = [instr.closure.id, instr.value.id]

        elif isinstance(instr, MenaiVCodeTrace):
            ids = [instr.dst.id, instr.value.id] + [r.id for r in instr.messages]

        elif isinstance(instr, MenaiVCodeJumpIfTrue):
            ids = [instr.cond.id]

        elif isinstance(instr, MenaiVCodeJumpIfFalse):
            ids = [instr.cond.id]

        elif isinstance(instr, MenaiVCodeReturn):
            ids = [instr.value.id]

        return max(ids) if ids else -1
