"""
Tests for MenaiCFGStackScheduler.

Each test constructs a minimal MenaiCFGFunction by hand and asserts which
SSA value IDs the scheduler classifies as stack-transient.

The tests are organised around the specific conditions the scheduler checks:

  1. Single-use + immediately-next-instruction + last-operand  → transient
  2. Multi-use                                                 → slotted
  3. Use is not the immediately-next instruction               → slotted
  4. Use is the last operand but earlier operands are slotted  → slotted
     (the core ordering-safety condition)
  5. Hard exclusions (param, free-var, phi, needs_patching)    → slotted
  6. Terminator consumers
  7. Special-case builtins with synthesised trailing arguments → slotted
"""

import pytest

from menai.menai_cfg import (
    MenaiCFGBlock,
    MenaiCFGBuiltinInstr,
    MenaiCFGCallInstr,
    MenaiCFGApplyInstr,
    MenaiCFGConstInstr,
    MenaiCFGFreeVarInstr,
    MenaiCFGFunction,
    MenaiCFGGlobalInstr,
    MenaiCFGMakeClosureInstr,
    MenaiCFGParamInstr,
    MenaiCFGPhiInstr,
    MenaiCFGReturnTerm,
    MenaiCFGBranchTerm,
    MenaiCFGJumpTerm,
    MenaiCFGTailCallTerm,
    MenaiCFGTailApplyTerm,
    MenaiCFGSelfLoopTerm,
    MenaiCFGTraceInstr,
    MenaiCFGValue,
)
from menai.menai_cfg_stack_scheduler import MenaiCFGStackScheduler
from menai.menai_value import MenaiInteger, MenaiBoolean


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_vid = 0


def fresh_value(hint: str = "") -> MenaiCFGValue:
    """Return a MenaiCFGValue with a unique ID."""
    global _vid
    _vid += 1
    return MenaiCFGValue(id=_vid, hint=hint)


def make_func(*blocks: MenaiCFGBlock) -> MenaiCFGFunction:
    """Wrap blocks into a minimal MenaiCFGFunction."""
    return MenaiCFGFunction(blocks=list(blocks))


def make_block(*instrs, terminator=None, label="entry") -> MenaiCFGBlock:
    """Build a MenaiCFGBlock from a list of instructions and a terminator."""
    block = MenaiCFGBlock(id=0, label=label)
    block.instrs = list(instrs)
    block.terminator = terminator
    return block


def schedule(func: MenaiCFGFunction) -> set:
    """Run the scheduler and return the transient ID set."""
    return MenaiCFGStackScheduler().schedule(func).transient_ids


def const_instr(hint: str = "c") -> tuple:
    """Return (value, MenaiCFGConstInstr) for a fresh SSA value."""
    v = fresh_value(hint)
    return v, MenaiCFGConstInstr(result=v, value=MenaiInteger(0))


# ---------------------------------------------------------------------------
# 1. Basic transient: single-use, immediately-next, last operand
# ---------------------------------------------------------------------------

class TestBasicTransient:

    def test_const_into_return(self):
        """
        %0 = const 0
        return %0
        The single use of %0 is the return terminator, which is the last
        (and only) operand.  %0 should be transient.
        """
        v0, c0 = const_instr("v0")
        ret = MenaiCFGReturnTerm(value=v0)
        block = make_block(c0, terminator=ret)
        func = make_func(block)
        assert v0.id in schedule(func)

    def test_const_into_unary_builtin(self):
        """
        %0 = const 0
        %1 = builtin 'not' [%0]
        return %1
        %0 is the only (and therefore last) operand of the unary builtin.
        %0 should be transient.  %1 is used by return → also transient.
        """
        v0, c0 = const_instr("v0")
        v1 = fresh_value("v1")
        b1 = MenaiCFGBuiltinInstr(result=v1, op='not', args=[v0])
        ret = MenaiCFGReturnTerm(value=v1)
        block = make_block(c0, b1, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id in ids
        assert v1.id in ids

    def test_global_into_return(self):
        """
        %0 = global 'foo'
        return %0
        Single use, last operand of return → transient.
        """
        v0 = fresh_value("v0")
        g0 = MenaiCFGGlobalInstr(result=v0, name='foo')
        ret = MenaiCFGReturnTerm(value=v0)
        block = make_block(g0, terminator=ret)
        func = make_func(block)
        assert v0.id in schedule(func)

    def test_const_into_binary_last_arg(self):
        """
        %0 = const 1   (slotted: used as first arg of binary op)
        %1 = const 2   (slotted: last arg, but %0 is slotted and pushed first,
                        so LOAD_VAR(%0) would go on top of %1 → %1 must be slotted)
        %2 = builtin 'integer+' [%0, %1]
        return %2
        Only %2 should be transient (used only by return, single operand).
        """
        v0, c0 = const_instr("v0")
        v1, c1 = const_instr("v1")
        v2 = fresh_value("v2")
        b = MenaiCFGBuiltinInstr(result=v2, op='integer+', args=[v0, v1])
        ret = MenaiCFGReturnTerm(value=v2)
        block = make_block(c0, c1, b, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids, "first arg of binary op must be slotted"
        assert v1.id not in ids, "last arg slotted: preceding arg is slotted, LOAD_VAR would displace it"
        assert v2.id in ids,     "result used only by return should be transient"


# ---------------------------------------------------------------------------
# 2. Multi-use → always slotted
# ---------------------------------------------------------------------------

class TestMultiUse:

    def test_value_used_twice_is_slotted(self):
        """
        %0 = const 1
        %1 = builtin 'integer+' [%0, %0]   ← %0 used twice
        return %1
        %0 has use count 2 → slotted.
        """
        v0, c0 = const_instr("v0")
        v1 = fresh_value("v1")
        b = MenaiCFGBuiltinInstr(result=v1, op='integer+', args=[v0, v0])
        ret = MenaiCFGReturnTerm(value=v1)
        block = make_block(c0, b, terminator=ret)
        func = make_func(block)
        assert v0.id not in schedule(func)

    def test_value_used_in_two_instructions(self):
        """
        %0 = const 1
        %1 = builtin 'not' [%0]
        %2 = builtin 'not' [%0]   ← second use of %0
        return %1
        %0 has use count 2 → slotted.
        """
        v0, c0 = const_instr("v0")
        v1 = fresh_value("v1")
        v2 = fresh_value("v2")
        b1 = MenaiCFGBuiltinInstr(result=v1, op='not', args=[v0])
        b2 = MenaiCFGBuiltinInstr(result=v2, op='not', args=[v0])
        ret = MenaiCFGReturnTerm(value=v1)
        block = make_block(c0, b1, b2, terminator=ret)
        func = make_func(block)
        assert v0.id not in schedule(func)


# ---------------------------------------------------------------------------
# 3. Consumer is not the immediately-next instruction → slotted
# ---------------------------------------------------------------------------

class TestNotImmediatelyNext:

    def test_value_used_two_instructions_later(self):
        """
        %0 = const 1
        %1 = const 2        ← intervening instruction
        %2 = builtin 'integer+' [%0, %1]
        return %2
        %0's consumer is the builtin at i+2, not i+1 → slotted.
        %1's consumer is the builtin at i+1 and it's the last arg, but %0
        is slotted and will be LOAD_VAR'd before the opcode → %1 also slotted.
        %2 is used only by return → transient.
        """
        v0, c0 = const_instr("v0")
        v1, c1 = const_instr("v1")
        v2 = fresh_value("v2")
        b = MenaiCFGBuiltinInstr(result=v2, op='integer+', args=[v0, v1])
        ret = MenaiCFGReturnTerm(value=v2)
        block = make_block(c0, c1, b, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids, "v0 used two instructions later → slotted"
        assert v1.id not in ids, "v1 is last arg but preceding arg %0 is slotted → slotted"
        assert v2.id in ids,     "v2 used only by return → transient"


# ---------------------------------------------------------------------------
# 4. Ordering safety: earlier slotted operands disqualify last-arg transient
# ---------------------------------------------------------------------------

class TestOrderingSafety:

    def test_multi_arg_builtin_only_last_arg_transient(self):
        """
        The key ordering-safety test.

        %0 = const 1   (slotted: used as args[0] of list builtin)
        %1 = const 2   (slotted: used as args[1])
        %2 = const 3   (transient candidate: last arg of list builtin)
        %3 = builtin 'list' [%0, %1, %2]
        return %3

        If %2 were transient, the emitter would leave it on the stack, then
        load %0 and %1 on top of it — producing [3,1,2] instead of [1,2,3].

        %2 MUST be slotted because %0 and %1 are slotted and will be loaded
        (via LOAD_VAR) between %2's definition and the LIST opcode, displacing
        %2 from the stack top.
        """
        v0, c0 = const_instr("v0")
        v1, c1 = const_instr("v1")
        v2, c2 = const_instr("v2")
        v3 = fresh_value("v3")
        b = MenaiCFGBuiltinInstr(result=v3, op='list', args=[v0, v1, v2])
        ret = MenaiCFGReturnTerm(value=v3)
        block = make_block(c0, c1, c2, b, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids, "v0 is not last arg → slotted"
        assert v1.id not in ids, "v1 is not last arg → slotted"
        assert v2.id not in ids, \
            "v2 is last arg but earlier args are slotted → must be slotted too"
        assert v3.id in ids,     "v3 used only by return → transient"

    def test_call_func_not_transient_when_args_are_slotted(self):
        """
        %0 = global 'f'     (slotted: used as func of call, not last push)
        %1 = const 1        (slotted: used as args[0])
        %2 = const 2        (transient candidate: last thing pushed = func...
                             wait, for CALL the push order is args then func)

        Actually for MenaiCFGCallInstr the push order is:
            args[0], args[1], ..., args[-1], func
        So `func` is the last SSA operand pushed.

        %0 = global 'f'
        %1 = const 1
        %2 = call %0(%1)
        return %2

        %0 is used as `func` (last push) but it's defined at i=0 and the
        call is at i=2 (not immediately next) → slotted.
        %1 is used as args[0] (not last push) → slotted.
        %2 is used only by return → transient.
        """
        v0 = fresh_value("f")
        g0 = MenaiCFGGlobalInstr(result=v0, name='f')
        v1, c1 = const_instr("v1")
        v2 = fresh_value("v2")
        call = MenaiCFGCallInstr(result=v2, func=v0, args=[v1])
        ret = MenaiCFGReturnTerm(value=v2)
        block = make_block(g0, c1, call, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids, "func defined at i=0, call at i=2 → slotted"
        assert v1.id not in ids, "arg[0] is not last push for call → slotted"
        assert v2.id in ids,     "call result used only by return → transient"

    def test_unary_call_func_transient_when_immediately_before(self):
        """
        For a zero-argument call, `func` IS the last (and only) SSA operand.

        %0 = global 'f'
        %1 = call %0()
        return %1

        %0 is the last SSA operand of the call (func, no args before it),
        and the call is immediately next → transient.
        """
        v0 = fresh_value("f")
        g0 = MenaiCFGGlobalInstr(result=v0, name='f')
        v1 = fresh_value("v1")
        call = MenaiCFGCallInstr(result=v1, func=v0, args=[])
        ret = MenaiCFGReturnTerm(value=v1)
        block = make_block(g0, call, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id in ids, "func with no args, immediately before call → transient"
        assert v1.id in ids


# ---------------------------------------------------------------------------
# 5. Hard exclusions
# ---------------------------------------------------------------------------

class TestHardExclusions:

    def test_param_always_slotted(self):
        """Param results are always slotted (fixed slot 0..P-1)."""
        v0 = fresh_value("p0")
        p0 = MenaiCFGParamInstr(result=v0, index=0, param_name='x')
        ret = MenaiCFGReturnTerm(value=v0)
        block = make_block(p0, terminator=ret)
        func = make_func(block)
        func.params = ['x']
        assert v0.id not in schedule(func)

    def test_free_var_always_slotted(self):
        """Free-var results are always slotted (fixed slot P..P+F-1)."""
        v0 = fresh_value("fv0")
        fv = MenaiCFGFreeVarInstr(result=v0, index=0, var_name='y')
        ret = MenaiCFGReturnTerm(value=v0)
        block = make_block(fv, terminator=ret)
        func = make_func(block)
        func.free_vars = ['y']
        assert v0.id not in schedule(func)

    def test_phi_always_slotted(self):
        """Phi results are always slotted (written by predecessor blocks)."""
        v0, c0 = const_instr("v0")
        v1, c1 = const_instr("v1")
        vphi = fresh_value("phi")
        phi = MenaiCFGPhiInstr(result=vphi, incoming=[])
        ret = MenaiCFGReturnTerm(value=vphi)
        block = make_block(phi, terminator=ret)
        func = make_func(block)
        assert vphi.id not in schedule(func)

    def test_needs_patching_closure_always_slotted(self):
        """A closure with needs_patching=True must be slotted for PATCH_CLOSURE."""
        vclosure = fresh_value("closure")
        child_func = MenaiCFGFunction(
            blocks=[make_block(terminator=MenaiCFGReturnTerm(value=fresh_value()))],
        )
        mk = MenaiCFGMakeClosureInstr(
            result=vclosure,
            function=child_func,
            captures=[],
            needs_patching=True,
        )
        ret = MenaiCFGReturnTerm(value=vclosure)
        block = make_block(mk, terminator=ret)
        func = make_func(block)
        assert vclosure.id not in schedule(func)


# ---------------------------------------------------------------------------
# 6. Terminator consumers
# ---------------------------------------------------------------------------

class TestTerminatorConsumers:

    def test_tail_call_func_transient(self):
        """
        %0 = global 'f'
        tail_call %0()
        func is the last SSA operand of tail_call with no args → transient.
        """
        v0 = fresh_value("f")
        g0 = MenaiCFGGlobalInstr(result=v0, name='f')
        term = MenaiCFGTailCallTerm(func=v0, args=[])
        block = make_block(g0, terminator=term)
        func = make_func(block)
        assert v0.id in schedule(func)

    def test_tail_call_func_not_transient_when_args_present(self):
        """
        %0 = const 1
        %1 = global 'f'
        tail_call %1(%0)
        Push order: %0, %1.  %1 is last push.
        %1 is last push, but %0 (args[0]) is slotted and pushed first —
        LOAD_VAR(%0) would go on top of %1 → %1 must also be slotted.
        %0 is args[0], not last push → slotted.
        """
        v0, c0 = const_instr("v0")
        v1 = fresh_value("f")
        g1 = MenaiCFGGlobalInstr(result=v1, name='f')
        term = MenaiCFGTailCallTerm(func=v1, args=[v0])
        block = make_block(c0, g1, terminator=term)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids, "arg[0] is not last push → slotted"
        assert v1.id not in ids, "func is last push but preceding arg is slotted → slotted"

    def test_tail_apply_arg_list_transient(self):
        """
        %0 = global 'f'
        %1 = global 'args'
        tail_apply %0 %1
        Push order: func, arg_list.  %1 is last → transient.
        %0 is not last → slotted.  But %0 is slotted and pushed before %1,
        so LOAD_VAR(%0) would displace %1 → %1 also slotted.
        """
        v0 = fresh_value("f")
        g0 = MenaiCFGGlobalInstr(result=v0, name='f')
        v1 = fresh_value("args")
        g1 = MenaiCFGGlobalInstr(result=v1, name='args')
        term = MenaiCFGTailApplyTerm(func=v0, arg_list=v1)
        block = make_block(g0, g1, terminator=term)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids, "func is not last push for tail_apply → slotted"
        assert v1.id not in ids, "arg_list is last push but func is slotted → slotted"

    def test_branch_cond_transient(self):
        """
        %0 = const #t
        branch %0 → then / else
        %0 is the only operand of branch → transient.
        """
        v0, c0 = const_instr("v0")
        then_block = make_block(label="then",
                                terminator=MenaiCFGReturnTerm(value=fresh_value()))
        else_block = make_block(label="else",
                                terminator=MenaiCFGReturnTerm(value=fresh_value()))
        entry = make_block(c0, terminator=MenaiCFGBranchTerm(
            cond=v0, true_block=then_block, false_block=else_block
        ))
        func = make_func(entry, then_block, else_block)
        assert v0.id in schedule(func)

    def test_self_loop_last_arg_transient(self):
        """
        %0 = const 1
        %1 = const 2
        self_loop [%0, %1]
        %0 is args[0], not last → slotted.
        %1 is args[-1], last, but %0 is slotted and pushed before %1 → slotted.
        """
        v0, c0 = const_instr("v0")
        v1, c1 = const_instr("v1")
        term = MenaiCFGSelfLoopTerm(args=[v0, v1])
        block = make_block(c0, c1, terminator=term)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids
        assert v1.id not in ids


# ---------------------------------------------------------------------------
# 7. Special-case builtins with synthesised trailing arguments
# ---------------------------------------------------------------------------

class TestSynthesisedArgBuiltins:

    @pytest.mark.parametrize("op,arity", [
        ('range', 2),
        ('integer->complex', 1),
        ('integer->string', 1),
        ('float->complex', 1),
        ('string->integer', 1),
        ('string->list', 1),
        ('list->string', 1),
        ('dict-get', 2),
        ('string-slice', 2),
        ('list-slice', 2),
    ])
    def test_synth_arg_builtin_no_operand_transient(self, op, arity):
        """
        For each special-case builtin at its default arity, no SSA operand
        should be transient because the codegen pushes a synthesised value
        after the last SSA operand.
        """
        args = [fresh_value(f"a{i}") for i in range(arity)]
        arg_instrs = [MenaiCFGConstInstr(result=a, value=MenaiInteger(0)) for a in args]
        vresult = fresh_value("result")
        builtin = MenaiCFGBuiltinInstr(result=vresult, op=op, args=args)
        ret = MenaiCFGReturnTerm(value=vresult)
        block = make_block(*arg_instrs, builtin, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        for a in args:
            assert a.id not in ids, \
                f"arg of {op!r} at arity {arity} should be slotted (synth trailing arg)"

    def test_range_3_args_last_is_transient(self):
        """
        range with 3 args uses no synthesised trailing arg — last arg is transient.
        %0 = const 0  (slotted: args[0])
        %1 = const 10 (slotted: args[1])
        %2 = const 1  (transient: args[2], last, no synth)
        %3 = builtin 'range' [%0, %1, %2]
        return %3
        """
        v0, c0 = const_instr("start")
        v1, c1 = const_instr("stop")
        v2, c2 = const_instr("step")
        v3 = fresh_value("result")
        b = MenaiCFGBuiltinInstr(result=v3, op='range', args=[v0, v1, v2])
        ret = MenaiCFGReturnTerm(value=v3)
        block = make_block(c0, c1, c2, b, terminator=ret)
        func = make_func(block)
        ids = schedule(func)
        assert v0.id not in ids
        assert v1.id not in ids
        assert v2.id not in ids, \
            "v2 is last arg but v0/v1 are slotted → must be slotted too"
        assert v3.id in ids
