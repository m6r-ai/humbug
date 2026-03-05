"""
CFG optimizer for the Menai compiler.

Eliminates degenerate block patterns that arise naturally from the CFG
builder's mechanical translation of `if` expressions into branch/then/else/join
quadruples.  The most common case is an `if` whose one branch is a
tail-terminator (tail-call, return, raise): the join block then has a phi with
only one real incoming value, which is pure overhead.

Position in the pipeline
------------------------
    MenaiCFGFunction (from MenaiCFGBuilder)
        → MenaiCFGOptimizer
            → MenaiCFGFunction (optimised)
                → MenaiVMCodeGen

The optimizer runs to a fixed point: it applies all passes in sequence and
repeats until no pass reports a change.

Passes (in order)
-----------------
1. **Stale-phi pruning** — removes incoming entries from phi nodes where the
   listed predecessor block has no actual control-flow edge to the phi's block.
   This is the precondition for trivial-phi detection.

2. **Trivial-phi elimination** — a phi with exactly one remaining incoming
   entry is just an alias.  Substitute the incoming value for the phi result
   throughout the function and remove the phi instruction.

3. **Empty-block bypass** — a block with no instructions, no patch_instrs, and
   an unconditional jump terminator is a pure indirection.  Re-point all its
   predecessors directly to its target.  Update phi incoming entries in the
   target to name the correct predecessor.

4. **Dead-block elimination** — remove blocks unreachable from the entry block.
   Also prunes any phi incoming entries that reference a now-dead block.

Each pass returns a new MenaiCFGFunction and a boolean indicating whether
anything changed.  The outer loop iterates to fixpoint.

Block identity and mutability
------------------------------
MenaiCFGBlock objects are referenced by identity from MenaiCFGTerminator
objects (BranchTerm.true_block, BranchTerm.false_block, JumpTerm.target) and
from MenaiCFGPhiInstr.incoming entries.  To avoid stale references when
blocks are rebuilt, all passes that modify block content do so by **mutating
the block objects in place** (updating instrs, patch_instrs, and terminator
fields).  Block identity is preserved so all existing references remain valid.

This is consistent with the existing treatment of the `predecessors` field,
which is already mutated in place by _relink_predecessors.

The MenaiCFGFunction itself is replaced (new object) when its block list
changes (blocks added or removed), or when nested lambda functions are updated.

SSA value IDs
-------------
The optimizer never allocates new SSA values.  It only substitutes existing
values for one another (phi elimination maps phi-result → incoming-value).
All IDs therefore remain globally unique within the function.
"""

from typing import Dict, List, Optional, Set, Tuple, Callable

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


# ---------------------------------------------------------------------------
# Value substitution map type alias
# ---------------------------------------------------------------------------

# Maps SSA value id → replacement MenaiCFGValue.
# Applied everywhere a value is referenced (operands, phi incoming, etc.).
_SubstMap = Dict[int, MenaiCFGValue]


class MenaiCFGOptimizer:
    """
    Runs all CFG optimization passes to a fixed point on a MenaiCFGFunction
    and all nested lambda functions it contains.

    Usage::

        optimized_func = MenaiCFGOptimizer().optimize(func)
    """

    def optimize(self, func: MenaiCFGFunction) -> MenaiCFGFunction:
        """
        Optimize `func` and all nested lambdas it contains.

        Applies passes to `func` repeatedly until no pass makes a change,
        then recursively optimizes any MenaiCFGFunction objects referenced by
        MenaiCFGMakeClosureInstr instructions.

        Args:
            func: The CFG function to optimize.

        Returns:
            An optimized MenaiCFGFunction.  The block objects within it may
            have been mutated in place; the function object itself is new if
            the block list changed.
        """
        func = self._optimize_function(func)
        func = self._optimize_nested(func)
        return func

    def _optimize_function(self, func: MenaiCFGFunction) -> MenaiCFGFunction:
        """
        Run all passes on `func` to a fixed point.

        Passes mutate block content in place and return a (possibly new)
        MenaiCFGFunction together with a changed flag.
        """
        changed = True
        while changed:
            changed = False

            func, c = _prune_stale_phi_entries(func)
            changed = changed or c

            func, c = _eliminate_trivial_phis(func)
            changed = changed or c

            func, c = _bypass_empty_blocks(func)
            changed = changed or c

            func, c = _eliminate_dead_blocks(func)
            changed = changed or c

        return func

    def _optimize_nested(self, func: MenaiCFGFunction) -> MenaiCFGFunction:
        """
        Recursively optimize all MenaiCFGFunction objects referenced by
        MenaiCFGMakeClosureInstr instructions anywhere in `func`.

        When a nested lambda is optimized and returns a new function object,
        the MakeClosure instruction is updated in place (its `function` field
        is replaced) so that block identity is preserved and no terminator
        references become stale.

        Returns `func` (possibly with mutated MakeClosure instructions).
        """
        for block in func.blocks:
            for i, instr in enumerate(block.instrs):
                if isinstance(instr, MenaiCFGMakeClosureInstr):
                    optimized_child = self.optimize(instr.function)
                    if optimized_child is not instr.function:
                        # Replace the instruction in place (update the list).
                        # We create a new MakeClosure with the same fields
                        # except function, and replace the slot in the list.
                        block.instrs[i] = MenaiCFGMakeClosureInstr(
                            result=instr.result,
                            function=optimized_child,
                            captures=instr.captures,
                            needs_patching=instr.needs_patching,
                        )
        return func


def _prune_stale_phi_entries(
    func: MenaiCFGFunction,
) -> Tuple[MenaiCFGFunction, bool]:
    """
    Remove incoming entries from phi nodes where the listed predecessor block
    has no actual control-flow edge to the phi's containing block.

    A control-flow edge exists from block P to block B iff P's terminator is:
    - MenaiCFGJumpTerm(B)
    - MenaiCFGBranchTerm(..., true_block=B, ...) or (..., false_block=B)

    Mutates block.instrs in place for blocks that contain stale phi entries.

    Args:
        func: The function to transform.

    Returns:
        (func, changed) — the same function object (with mutated blocks) and
        a flag indicating whether any entries were actually removed.
    """
    # Build the set of actual control-flow edges: (pred_id, succ_id).
    edges: Set[Tuple[int, int]] = set()
    for block in func.blocks:
        term = block.terminator
        if isinstance(term, MenaiCFGJumpTerm):
            edges.add((block.id, term.target.id))

        elif isinstance(term, MenaiCFGBranchTerm):
            edges.add((block.id, term.true_block.id))
            edges.add((block.id, term.false_block.id))

    changed = False

    for block in func.blocks:
        for i, instr in enumerate(block.instrs):
            if not isinstance(instr, MenaiCFGPhiInstr):
                continue

            pruned = [
                (val, pred)
                for val, pred in instr.incoming
                if (pred.id, block.id) in edges
            ]
            if len(pruned) != len(instr.incoming):
                block.instrs[i] = MenaiCFGPhiInstr(
                    result=instr.result,
                    incoming=pruned,
                )
                changed = True

    if changed:
        _relink_predecessors(func)

    return func, changed


def _eliminate_trivial_phis(
    func: MenaiCFGFunction,
) -> Tuple[MenaiCFGFunction, bool]:
    """
    Replace each phi node that has exactly one incoming entry with a direct
    reference to that incoming value, then remove the phi instruction.

    A single-incoming phi is just an alias: `v_phi = phi [(v_x, pred)]`
    means v_phi is always v_x.  We substitute v_x for v_phi everywhere in
    the function and drop the phi instruction entirely.

    Zero-incoming phis (which can arise after stale-phi pruning when a join
    block is unreachable) are left alone — the block will be removed by
    dead-block elimination in the next pass.

    Mutates block.instrs, block.patch_instrs, and block.terminator in place.

    Args:
        func: The function to transform.

    Returns:
        (func, changed) — the same function object (with mutated blocks) and
        a flag indicating whether any phis were eliminated.
    """
    # Collect all trivial phis: phi-result-id → replacement value.
    subst: _SubstMap = {}

    for block in func.blocks:
        for instr in block.instrs:
            if isinstance(instr, MenaiCFGPhiInstr) and len(instr.incoming) == 1:
                incoming_val, _ = instr.incoming[0]
                subst[instr.result.id] = incoming_val

    if not subst:
        return func, False

    # Resolve chains: if subst[a] = b and subst[b] = c, then subst[a] = c.
    def resolve(v: MenaiCFGValue) -> MenaiCFGValue:
        seen: Set[int] = set()
        while v.id in subst and v.id not in seen:
            seen.add(v.id)
            v = subst[v.id]

        return v

    # Mutate all blocks in place, applying substitution.
    for block in func.blocks:
        # Remove trivial phi instructions and substitute all value references.
        new_instrs: List[MenaiCFGInstr] = []
        for instr in block.instrs:
            # Drop trivial phi instructions entirely.
            if isinstance(instr, MenaiCFGPhiInstr) and instr.result.id in subst:
                continue

            new_instrs.append(_subst_instr(instr, resolve))

        block.instrs = new_instrs

        block.patch_instrs = [_subst_patch(p, resolve) for p in block.patch_instrs]
        if block.terminator is not None:
            block.terminator = _subst_term(block.terminator, resolve)

    _relink_predecessors(func)
    return func, True


def _bypass_empty_blocks(
    func: MenaiCFGFunction,
) -> Tuple[MenaiCFGFunction, bool]:
    """
    Bypass blocks that are pure indirections: no instructions, no patch_instrs,
    and an unconditional jump terminator.

    For each such empty block E with `MenaiCFGJumpTerm(T)`, E can be bypassed
    only if the bypass is safe with respect to the VM codegen's phi store
    mechanism.

    Phi stores are emitted only when a block emits its own `JumpTerm`.  If a
    BranchTerm block is re-pointed to jump directly to a phi-bearing block, no
    phi store is emitted for the branch's contribution.  Therefore:

    A bypass chain E1 -> E2 -> ... -> T is safe if and only if:
      - T has no phi instructions, OR
      - No block in the chain {E1, E2, ...} has a BranchTerm predecessor.

    The check must be applied to the *ultimate* target T (not each intermediate
    block's immediate target), because a chain of empty blocks can route a
    BranchTerm predecessor through to a phi-bearing ultimate target even when
    each individual intermediate block's immediate target has no phis.

    For each bypassable E with `MenaiCFGJumpTerm(T)`:
    - Re-point every predecessor of E to jump directly to T.
    - In T's phi nodes, replace incoming entries that name E as the predecessor
      with entries naming E's actual predecessor(s) instead.
    - Remove E from the block list (returns a new MenaiCFGFunction).

    The entry block (blocks[0]) is never bypassed even if empty.

    Terminators are mutated in place on predecessor blocks.  Phi incoming
    entries are mutated in place on successor blocks.  The block list is
    replaced (new MenaiCFGFunction) if any blocks are removed.

    Args:
        func: The function to transform.

    Returns:
        (new_func_or_func, changed) — function with empty blocks removed,
        and a flag indicating whether any blocks were bypassed.
    """
    entry_id = func.blocks[0].id

    # Build a map: block_id → block.
    block_map: Dict[int, MenaiCFGBlock] = {b.id: b for b in func.blocks}

    # Build predecessor map first (needed for the branch-predecessor check).
    pred_map_pre: Dict[int, List[MenaiCFGBlock]] = {b.id: [] for b in func.blocks}
    for block in func.blocks:
        term = block.terminator
        if isinstance(term, MenaiCFGJumpTerm):
            if term.target.id in pred_map_pre:
                pred_map_pre[term.target.id].append(block)

        elif isinstance(term, MenaiCFGBranchTerm):
            if term.true_block.id in pred_map_pre:
                pred_map_pre[term.true_block.id].append(block)

            if term.false_block.id in pred_map_pre:
                pred_map_pre[term.false_block.id].append(block)

    def is_empty(block: MenaiCFGBlock) -> bool:
        """A block is a candidate for bypass if it has no instructions,
        no patch_instrs, an unconditional jump terminator, and is not entry.
        Safety against the phi-store constraint is checked separately when
        building the bypass map."""
        return (
            block.id != entry_id
            and not block.instrs
            and not block.patch_instrs
            and isinstance(block.terminator, MenaiCFGJumpTerm)
        )

    def has_phi(block: MenaiCFGBlock) -> bool:
        return any(isinstance(i, MenaiCFGPhiInstr) for i in block.instrs)

    def chain_has_branch_predecessor(start: MenaiCFGBlock) -> bool:
        """Return True if any block in the empty-block chain starting at
        `start` has a BranchTerm predecessor.  Used to determine whether
        bypassing the chain to a phi-bearing ultimate target is safe."""
        seen: Set[int] = set()
        block = start
        while is_empty(block) and block.id not in seen:
            if any(
                isinstance(pred.terminator, MenaiCFGBranchTerm)
                for pred in pred_map_pre.get(block.id, [])
            ):
                return True

            seen.add(block.id)
            assert isinstance(block.terminator, MenaiCFGJumpTerm)
            next_b = block_map.get(block.terminator.target.id)
            if next_b is None:
                break

            block = next_b

        return False

    # Find ultimate non-empty target, following chains.
    def ultimate_target(block: MenaiCFGBlock) -> MenaiCFGBlock:
        seen: Set[int] = set()
        while is_empty(block) and block.id not in seen:
            seen.add(block.id)
            assert isinstance(block.terminator, MenaiCFGJumpTerm)
            next_b = block_map.get(block.terminator.target.id)
            if next_b is None:
                break

            block = next_b

        return block

    # Build bypass map: empty_block_id → ultimate non-empty target.
    # Safety check: if the ultimate target has phis, reject any chain that
    # contains a block with a BranchTerm predecessor (phi stores would be lost).
    bypass: Dict[int, MenaiCFGBlock] = {}
    for block in func.blocks:
        if is_empty(block):
            target = ultimate_target(block)
            if target.id != block.id and not (
                has_phi(target) and chain_has_branch_predecessor(block)
            ):
                bypass[block.id] = target

    if not bypass:
        return func, False

    pred_map = pred_map_pre  # Reuse the map we already built.
    def remap_block(b: MenaiCFGBlock) -> MenaiCFGBlock:
        return bypass.get(b.id, b)

    # Mutate terminators and phi incoming entries in place.
    for block in func.blocks:
        if block.id in bypass:
            continue  # This block is being removed; no need to update it.

        # Update phi incoming entries.
        for i, instr in enumerate(block.instrs):
            if not isinstance(instr, MenaiCFGPhiInstr):
                continue
            new_incoming: List[Tuple[MenaiCFGValue, MenaiCFGBlock]] = []
            for val, pred in instr.incoming:
                if pred.id in bypass:
                    # Replace with entries for each real predecessor of pred.
                    actual_preds = pred_map.get(pred.id, [])
                    for actual_pred in actual_preds:
                        remapped = _find_non_empty_pred(actual_pred, bypass, pred_map)
                        new_incoming.append((val, remapped))

                else:
                    new_incoming.append((val, pred))

            block.instrs[i] = MenaiCFGPhiInstr(
                result=instr.result,
                incoming=new_incoming,
            )

        # Update terminator block references.
        if block.terminator is not None:
            block.terminator = _remap_term(block.terminator, remap_block)

    # Build new block list without bypassed blocks.
    new_blocks = [b for b in func.blocks if b.id not in bypass]
    new_func = MenaiCFGFunction(
        blocks=new_blocks,
        params=func.params,
        free_vars=func.free_vars,
        is_variadic=func.is_variadic,
        binding_name=func.binding_name,
        source_line=func.source_line,
        source_file=func.source_file,
    )
    _relink_predecessors(new_func)
    return new_func, True


def _find_non_empty_pred(
    block: MenaiCFGBlock,
    bypass: Dict[int, MenaiCFGBlock],
    pred_map: Dict[int, List[MenaiCFGBlock]],
) -> MenaiCFGBlock:
    """Walk up the predecessor chain until we find a non-bypassed block."""
    seen: Set[int] = set()
    while block.id in bypass and block.id not in seen:
        seen.add(block.id)
        preds = pred_map.get(block.id, [])
        if not preds:
            break

        block = preds[0]

    return block


def _eliminate_dead_blocks(
    func: MenaiCFGFunction,
) -> Tuple[MenaiCFGFunction, bool]:
    """
    Remove blocks that are unreachable from the entry block.

    Phi incoming entries that reference removed blocks are pruned in place.

    Returns a new MenaiCFGFunction if any blocks were removed, otherwise
    the original function.

    Args:
        func: The function to transform.

    Returns:
        (new_func_or_func, changed) — function with dead blocks removed.
    """
    reachable: Set[int] = set()

    def dfs(block: MenaiCFGBlock) -> None:
        if block.id in reachable:
            return

        reachable.add(block.id)
        term = block.terminator
        if isinstance(term, MenaiCFGJumpTerm):
            dfs(term.target)

        elif isinstance(term, MenaiCFGBranchTerm):
            dfs(term.true_block)
            dfs(term.false_block)

    if func.blocks:
        dfs(func.blocks[0])

    dead = {b.id for b in func.blocks if b.id not in reachable}
    if not dead:
        return func, False

    # Prune stale phi incoming entries that reference dead blocks.
    for block in func.blocks:
        if block.id in dead:
            continue

        for i, instr in enumerate(block.instrs):
            if not isinstance(instr, MenaiCFGPhiInstr):
                continue

            pruned = [
                (val, pred)
                for val, pred in instr.incoming
                if pred.id not in dead
            ]
            if len(pruned) != len(instr.incoming):
                block.instrs[i] = MenaiCFGPhiInstr(
                    result=instr.result,
                    incoming=pruned,
                )

    new_blocks = [b for b in func.blocks if b.id not in dead]
    new_func = MenaiCFGFunction(
        blocks=new_blocks,
        params=func.params,
        free_vars=func.free_vars,
        is_variadic=func.is_variadic,
        binding_name=func.binding_name,
        source_line=func.source_line,
        source_file=func.source_file,
    )
    _relink_predecessors(new_func)
    return new_func, True


def _relink_predecessors(func: MenaiCFGFunction) -> None:
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
    """Add `pred` to `target.predecessors` if target is in func.blocks."""
    if any(b.id == target.id for b in func.blocks):
        target.predecessors.append(pred)


def _subst_instr(
    instr: MenaiCFGInstr,
    resolve: Callable[[MenaiCFGValue], MenaiCFGValue],
) -> MenaiCFGInstr:
    """Return a new instruction with all value references substituted."""
    if isinstance(instr, (MenaiCFGConstInstr,
                           MenaiCFGGlobalInstr,
                           MenaiCFGParamInstr,
                           MenaiCFGFreeVarInstr)):
        return instr

    if isinstance(instr, MenaiCFGPhiInstr):
        new_incoming = [(resolve(val), pred) for val, pred in instr.incoming]
        if new_incoming == instr.incoming:
            return instr

        return MenaiCFGPhiInstr(result=instr.result, incoming=new_incoming)

    if isinstance(instr, MenaiCFGBuiltinInstr):
        new_args = [resolve(a) for a in instr.args]
        if new_args == instr.args:
            return instr

        return MenaiCFGBuiltinInstr(result=instr.result, op=instr.op, args=new_args)

    if isinstance(instr, MenaiCFGCallInstr):
        new_args = [resolve(a) for a in instr.args]
        new_func = resolve(instr.func)
        if new_args == instr.args and new_func is instr.func:
            return instr

        return MenaiCFGCallInstr(result=instr.result, func=new_func, args=new_args)

    if isinstance(instr, MenaiCFGApplyInstr):
        new_func = resolve(instr.func)
        new_arg_list = resolve(instr.arg_list)
        if new_func is instr.func and new_arg_list is instr.arg_list:
            return instr

        return MenaiCFGApplyInstr(
            result=instr.result, func=new_func, arg_list=new_arg_list
        )

    if isinstance(instr, MenaiCFGMakeClosureInstr):
        new_captures = [resolve(c) for c in instr.captures]
        if new_captures == instr.captures:
            return instr

        return MenaiCFGMakeClosureInstr(
            result=instr.result,
            function=instr.function,
            captures=new_captures,
            needs_patching=instr.needs_patching,
        )

    if isinstance(instr, MenaiCFGPatchClosureInstr):
        new_closure = resolve(instr.closure)
        new_value = resolve(instr.value)
        if new_closure is instr.closure and new_value is instr.value:
            return instr

        return MenaiCFGPatchClosureInstr(
            closure=new_closure,
            capture_index=instr.capture_index,
            value=new_value,
        )

    if isinstance(instr, MenaiCFGTraceInstr):
        new_messages = [resolve(m) for m in instr.messages]
        new_value = resolve(instr.value)
        if new_messages == instr.messages and new_value is instr.value:
            return instr

        return MenaiCFGTraceInstr(
            result=instr.result,
            messages=new_messages,
            value=new_value,
        )

    return instr


def _subst_patch(
    patch: MenaiCFGPatchClosureInstr,
    resolve: Callable[[MenaiCFGValue], MenaiCFGValue],
) -> MenaiCFGPatchClosureInstr:
    new_closure = resolve(patch.closure)
    new_value = resolve(patch.value)
    if new_closure is patch.closure and new_value is patch.value:
        return patch

    return MenaiCFGPatchClosureInstr(
        closure=new_closure,
        capture_index=patch.capture_index,
        value=new_value,
    )


def _subst_term(
    term: MenaiCFGTerminator | None,
    resolve: Callable[[MenaiCFGValue], MenaiCFGValue],
) -> MenaiCFGTerminator | None:
    """Return a new terminator with all value references substituted."""
    if term is None:
        return None

    if isinstance(term, MenaiCFGReturnTerm):
        new_val = resolve(term.value)
        if new_val is term.value:
            return term

        return MenaiCFGReturnTerm(value=new_val)

    if isinstance(term, MenaiCFGJumpTerm):
        return term  # No value references.

    if isinstance(term, MenaiCFGBranchTerm):
        new_cond = resolve(term.cond)
        if new_cond is term.cond:
            return term

        return MenaiCFGBranchTerm(
            cond=new_cond,
            true_block=term.true_block,
            false_block=term.false_block,
        )

    if isinstance(term, MenaiCFGTailCallTerm):
        new_args = [resolve(a) for a in term.args]
        new_func = resolve(term.func)
        if new_args == term.args and new_func is term.func:
            return term

        return MenaiCFGTailCallTerm(func=new_func, args=new_args)

    if isinstance(term, MenaiCFGTailApplyTerm):
        new_func = resolve(term.func)
        new_arg_list = resolve(term.arg_list)
        if new_func is term.func and new_arg_list is term.arg_list:
            return term

        return MenaiCFGTailApplyTerm(func=new_func, arg_list=new_arg_list)

    if isinstance(term, MenaiCFGSelfLoopTerm):
        new_args = [resolve(a) for a in term.args]
        if new_args == term.args:
            return term

        return MenaiCFGSelfLoopTerm(args=new_args)

    if isinstance(term, MenaiCFGRaiseTerm):
        return term

    return term


def _remap_term(
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
