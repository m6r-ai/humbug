"""
Liveness analysis and slot allocation for the Menai CFG.

Computes live-in and live-out sets for each basic block via backward dataflow,
then uses those sets to assign register slots to SSA values with slot reuse
across non-interfering values.

The allocator produces a complete slot_map (SSA value id → slot index) and a
slot_count before bytecode emission begins, replacing the sequential
alloc_slot scheme in _EmitContext.

Phi nodes follow the standard SSA liveness convention: an incoming value
(v, pred) is treated as a use of v in pred, not in the join block.  This
correctly models the fact that v must be live at the end of pred.

Params and free vars occupy fixed slots (0..P-1 for params, P..P+F-1 for
free vars) and are pre-seeded into the slot map before greedy allocation
runs over the remaining values.

TraceInstr aliases its result to its value input — the result receives the
same slot as the input rather than a new allocation.
"""

from typing import Dict, FrozenSet, List, Set, Tuple

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGFreeVarInstr,
    MenaiCFGFunction,
    MenaiCFGMakeClosureInstr,
    MenaiCFGInstr,
    MenaiCFGParamInstr,
    MenaiCFGPhiInstr,
    MenaiCFGPatchClosureInstr,
    MenaiCFGTraceInstr,
    MenaiCFGValue,
)
from menai.menai_cfg_collapse_phi_chains import _value_ids_in_instr, _value_ids_in_term


# ---------------------------------------------------------------------------
# Public result type
# ---------------------------------------------------------------------------

class SlotAllocation:
    """Result of slot allocation for one MenaiCFGFunction."""

    def __init__(self, slot_map: Dict[int, int], slot_count: int) -> None:
        self.slot_map = slot_map      # SSA value id → slot index
        self.slot_count = slot_count  # total slots needed (= local_count)


# ---------------------------------------------------------------------------
# Liveness analysis
# ---------------------------------------------------------------------------

def _uses_of_block(block: MenaiCFGBlock) -> Set[int]:
    """
    Compute the upward-exposed uses for a block: value ids that are read
    before being defined within the block.

    Phi incomings are attributed to the predecessor block, not this block,
    so phi incomings are excluded here.  The terminator and patch_instrs
    are included.
    """
    defined: Set[int] = set()
    uses: Set[int] = set()

    def _use(vid: int) -> None:
        if vid not in defined:
            uses.add(vid)

    def _def(vid: int) -> None:
        defined.add(vid)

    for instr in block.instrs:
        if isinstance(instr, MenaiCFGPhiInstr):
            # Phi result is defined here; phi incomings are uses in predecessors.
            _def(instr.result.id)
        else:
            for vid in _value_ids_in_instr(instr):
                _use(vid)
            result = _result_of_instr(instr)
            if result is not None:
                _def(result.id)

    for patch in block.patch_instrs:
        _use(patch.closure.id)
        _use(patch.value.id)

    if block.terminator is not None:
        for vid in _value_ids_in_term(block.terminator):
            _use(vid)

    return uses


def _defs_of_block(block: MenaiCFGBlock) -> Set[int]:
    """
    Compute the set of value ids defined in this block (including phi results).
    """
    defs: Set[int] = set()
    for instr in block.instrs:
        result = _result_of_instr(instr)
        if result is not None:
            defs.add(result.id)
    return defs


def _phi_uses_in_block(block: MenaiCFGBlock) -> Dict[int, Set[int]]:
    """
    For each phi in block, return a mapping from predecessor block id to the
    set of value ids that are used as phi incomings from that predecessor.

    These uses are attributed to the predecessor block for liveness purposes.
    """
    result: Dict[int, Set[int]] = {}
    for instr in block.instrs:
        if not isinstance(instr, MenaiCFGPhiInstr):
            break
        for val, pred in instr.incoming:
            result.setdefault(pred.id, set()).add(val.id)
    return result


def compute_liveness(func: MenaiCFGFunction) -> Tuple[
    Dict[int, FrozenSet[int]],  # live_in:  block id → frozenset of live value ids
    Dict[int, FrozenSet[int]],  # live_out: block id → frozenset of live value ids
]:
    """
    Compute live_in and live_out sets for every block in func via backward
    dataflow, iterated to fixed point in reverse post-order.

    live_out[B] = union of live_in[S] for all successors S of B,
                  plus phi-incoming uses from S that are attributed to B.
    live_in[B]  = use[B] ∪ (live_out[B] − def[B])
    """
    blocks = func.blocks

    # Pre-compute per-block use/def sets and phi-use attribution.
    use: Dict[int, Set[int]] = {}
    defs: Dict[int, Set[int]] = {}
    # phi_uses_from[succ_id][pred_id] = set of value ids used as phi incomings
    # in succ that come from pred.
    phi_uses_from: Dict[int, Dict[int, Set[int]]] = {}

    for block in blocks:
        use[block.id] = _uses_of_block(block)
        defs[block.id] = _defs_of_block(block)
        phi_uses_from[block.id] = _phi_uses_in_block(block)

    # Build successor lists.
    from menai.menai_cfg import MenaiCFGJumpTerm, MenaiCFGBranchTerm, MenaiCFGSelfLoopTerm
    successors: Dict[int, List[MenaiCFGBlock]] = {b.id: [] for b in blocks}
    for block in blocks:
        term = block.terminator
        if term is None:
            continue
        if isinstance(term, MenaiCFGJumpTerm):
            successors[block.id].append(term.target)
        elif isinstance(term, MenaiCFGBranchTerm):
            successors[block.id].append(term.true_block)
            successors[block.id].append(term.false_block)
        elif isinstance(term, MenaiCFGSelfLoopTerm):
            # Back-edge to entry; including it is correct and the fixed-point
            # iteration handles it naturally.
            successors[block.id].append(func.entry)

    # Initialise live sets to empty.
    live_in: Dict[int, Set[int]] = {b.id: set() for b in blocks}
    live_out: Dict[int, Set[int]] = {b.id: set() for b in blocks}

    # Iterate to fixed point.  We iterate in reverse RPO (post-order) for
    # backward dataflow.
    rpo = _rpo(func)
    rpo_reversed = list(reversed(rpo))

    changed = True
    while changed:
        changed = False
        for block in rpo_reversed:
            # live_out[B] = union of live_in[S] for successors S,
            #               plus phi-incoming uses attributed to B from each S.
            new_out: Set[int] = set()
            for succ in successors[block.id]:
                new_out |= live_in[succ.id]
                # Add phi-incoming uses in succ that come from this block.
                phi_from_me = phi_uses_from[succ.id].get(block.id, set())
                new_out |= phi_from_me

            # live_in[B] = use[B] ∪ (live_out[B] − def[B])
            new_in = use[block.id] | (new_out - defs[block.id])

            if new_out != live_out[block.id] or new_in != live_in[block.id]:
                live_out[block.id] = new_out
                live_in[block.id] = new_in
                changed = True

    return (
        {bid: frozenset(s) for bid, s in live_in.items()},
        {bid: frozenset(s) for bid, s in live_out.items()},
    )


# ---------------------------------------------------------------------------
# Slot allocation
# ---------------------------------------------------------------------------

def allocate_slots(func: MenaiCFGFunction) -> SlotAllocation:
    """
    Assign a slot index to every SSA value in func, reusing slots across
    values whose live ranges do not overlap.

    Algorithm
    ---------
    1. Pre-assign fixed slots to params (0..P-1) and free vars (P..P+F-1).
    2. Run backward dataflow to get live_in / live_out per block.
    3. For each block in RPO, compute the precise last-use index for every
       value used in the block (scanning all instrs, patch_instrs, and the
       terminator).
    4. Walk the block instruction-by-instruction, maintaining a live set.
       At each definition point:
         a. Assign the lowest free slot to the result (free = not occupied
            by any currently-live value).
         b. After assignment, kill any inputs whose last use was this
            instruction (they no longer need their slot).
    5. TraceInstr aliases its result to its value input (no new slot).
    6. PhiInstr results are allocated at the phi's position in the join
       block using the live set at that point.

    Returns a SlotAllocation with the complete slot_map and slot_count.
    """
    param_count = len(func.params)
    free_var_count = len(func.free_vars)

    slot_map: Dict[int, int] = {}
    next_new_slot = param_count + free_var_count

    # Phase 1: pre-assign params and free-var slots.
    for block in func.blocks:
        for instr in block.instrs:
            if isinstance(instr, MenaiCFGParamInstr):
                slot_map[instr.result.id] = instr.index
            elif isinstance(instr, MenaiCFGFreeVarInstr):
                slot_map[instr.result.id] = param_count + instr.index

    # Phase 2: liveness.
    live_in, live_out = compute_liveness(func)

    # Phase 3: greedy slot assignment in RPO.
    rpo = _rpo(func)

    def _free_slot(live: Set[int]) -> int:
        """Return the lowest slot index (above params+freevars) not occupied by any live value."""
        nonlocal next_new_slot
        occupied = {slot_map[vid] for vid in live if vid in slot_map}
        slot = param_count + free_var_count
        while slot in occupied:
            slot += 1
        if slot >= next_new_slot:
            next_new_slot = slot + 1
        return slot

    def _assign(value: MenaiCFGValue, live: Set[int]) -> int:
        """Assign the lowest free slot to value if not already assigned."""
        if value.id in slot_map:
            return slot_map[value.id]
        s = _free_slot(live)
        slot_map[value.id] = s
        return s

    for block in rpo:
        # Compute last-use index for every value referenced in this block.
        # Index scheme: instruction i in block.instrs has index i.
        # patch_instrs start at len(block.instrs).
        # terminator is at len(block.instrs) + len(block.patch_instrs).
        last_use: Dict[int, int] = {}

        n_instrs = len(block.instrs)
        n_patches = len(block.patch_instrs)
        term_idx = n_instrs + n_patches

        for i, instr in enumerate(block.instrs):
            if isinstance(instr, MenaiCFGPhiInstr):
                # Phi incomings are uses in predecessor blocks, not here.
                continue
            for vid in _value_ids_in_instr(instr):
                last_use[vid] = i

        for j, patch in enumerate(block.patch_instrs):
            idx = n_instrs + j
            last_use[patch.closure.id] = idx
            last_use[patch.value.id] = idx

        if block.terminator is not None:
            for vid in _value_ids_in_term(block.terminator):
                last_use[vid] = term_idx

        # Start with the live-in set for this block.
        live: Set[int] = set(live_in[block.id])
        block_live_out = live_out[block.id]

        for i, instr in enumerate(block.instrs):
            if isinstance(instr, (MenaiCFGParamInstr, MenaiCFGFreeVarInstr)):
                # Already assigned; ensure they are in the live set.
                live.add(instr.result.id)
                continue

            if isinstance(instr, MenaiCFGPhiInstr):
                # Allocate the phi result using the current live set.
                if instr.result.id not in slot_map:
                    _assign(instr.result, live)
                live.add(instr.result.id)
                # Kill phi result if its last use is at or before this point
                # and it is not needed by successor blocks.
                if last_use.get(instr.result.id, term_idx + 1) <= i and instr.result.id not in block_live_out:
                    live.discard(instr.result.id)
                continue

            if isinstance(instr, MenaiCFGTraceInstr):
                # Messages are used here; kill them if this is their last use.
                for msg in instr.messages:
                    if last_use.get(msg.id, term_idx + 1) <= i and msg.id not in block_live_out:
                        live.discard(msg.id)
                # Result aliases the value input — same slot.
                if instr.value.id in slot_map:
                    slot_map[instr.result.id] = slot_map[instr.value.id]
                else:
                    _assign(instr.result, live)
                    slot_map[instr.value.id] = slot_map[instr.result.id]
                live.add(instr.result.id)
                if last_use.get(instr.result.id, term_idx + 1) <= i and instr.result.id not in block_live_out:
                    live.discard(instr.result.id)
                continue

            if isinstance(instr, MenaiCFGPatchClosureInstr):
                # No result. Kill inputs if this is their last use.
                if last_use.get(instr.closure.id, term_idx + 1) <= i and instr.closure.id not in block_live_out:
                    live.discard(instr.closure.id)
                if last_use.get(instr.value.id, term_idx + 1) <= i and instr.value.id not in block_live_out:
                    live.discard(instr.value.id)
                continue

            # General case: instruction with a result.
            input_ids = _value_ids_in_instr(instr)

            # For MakeClosureInstr, captures are read again after the result
            # slot is written (the bytecode builder emits PATCH_CLOSURE after
            # MAKE_CLOSURE).  Allocate the result first so captures cannot be
            # assigned the same slot as the closure.  For all other instructions
            # the VM reads all sources before writing the destination, so we
            # can kill dead inputs first and let the result reuse their slots.
            from menai.menai_cfg import MenaiCFGMakeClosureInstr
            if isinstance(instr, MenaiCFGMakeClosureInstr):
                result = _result_of_instr(instr)
                if result is not None:
                    _assign(result, live)
                    live.add(result.id)
                for vid in input_ids:
                    if last_use.get(vid, term_idx + 1) <= i and vid not in block_live_out:
                        live.discard(vid)
                continue

            # All other instructions: kill dead inputs first so the result
            # can reuse their slots.
            for vid in input_ids:
                if last_use.get(vid, term_idx + 1) <= i and vid not in block_live_out:
                    live.discard(vid)

            result = _result_of_instr(instr)
            if result is not None:
                _assign(result, live)
                live.add(result.id)

            # Kill result if it has no further uses in this block and is not live-out.
            if result is not None and result.id not in block_live_out:
                if last_use.get(result.id, term_idx + 1) <= i:
                    live.discard(result.id)

    # Ensure slot_count covers everything assigned.
    slot_count = next_new_slot
    for s in slot_map.values():
        if s >= slot_count:
            slot_count = s + 1

    return SlotAllocation(slot_map=slot_map, slot_count=slot_count)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _result_of_instr(instr: MenaiCFGInstr) -> 'MenaiCFGValue | None':
    """Return the SSA result of instr, or None if it produces no result."""
    if isinstance(instr, MenaiCFGPatchClosureInstr):
        return None
    result = getattr(instr, 'result', None)
    return result


def _rpo(func: MenaiCFGFunction) -> List[MenaiCFGBlock]:
    """Return reachable blocks in reverse post-order."""
    from menai.menai_cfg import MenaiCFGJumpTerm, MenaiCFGBranchTerm
    visited: Set[int] = set()
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
