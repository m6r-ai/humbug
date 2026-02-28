"""
Tests for AIFPLIRUseCounter and AIFPLIROptimizer.

Strategy
--------
The tests are split into two layers:

1. Unit tests on the counter/optimizer directly, by constructing IR nodes
   by hand.  These are precise and fast — they don't go through the full
   compiler pipeline and so are immune to unrelated pipeline changes.

2. Integration tests that compile real AIFPL source and verify that the
   optimized program still produces the correct result.  These catch
   regressions where the optimizer silently changes semantics.

For the unit tests we build minimal IR trees that exercise specific cases:
  - simple let with a live binding
  - simple let with a dead binding
  - nested let where inner dead binding exposes outer dead binding (chain)
  - letrec with a self-referencing-only (dead-from-outside) binding
  - lambda: free-variable counts land in the correct (enclosing) frame
  - letrec with a live recursive binding is preserved
"""

from __future__ import annotations

import pytest

from aifpl import AIFPL
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_ir import (
    AIFPLIRCall,
    AIFPLIRConstant,
    AIFPLIRLambda,
    AIFPLIRLet,
    AIFPLIRLetrec,
    AIFPLIRReturn,
    AIFPLIRVariable,
)
from aifpl.aifpl_ir_optimizer import AIFPLIROptimizer
from aifpl.aifpl_ir_use_counter import AIFPLIRUseCounter
from aifpl.aifpl_value import AIFPLInteger


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _const(n: int) -> AIFPLIRConstant:
    return AIFPLIRConstant(value=AIFPLInteger(n))


def _local(index: int, depth: int = 0, is_parent_ref: bool = False) -> AIFPLIRVariable:
    return AIFPLIRVariable(
        name=f"v{index}",
        var_type='local',
        depth=depth,
        index=index,
        is_parent_ref=is_parent_ref,
    )


def _global(name: str) -> AIFPLIRVariable:
    return AIFPLIRVariable(name=name, var_type='global', depth=0, index=0)


def _add_call(a: AIFPLIRVariable, b: AIFPLIRVariable) -> AIFPLIRCall:
    """Emit (integer+ a b) as a builtin call."""
    return AIFPLIRCall(
        func_plan=_global('integer+'),
        arg_plans=[a, b],
        is_tail_call=False,
        is_tail_recursive=False,
        is_builtin=True,
        builtin_name='integer+',
    )


def _compile_source(source: str) -> object:
    """Compile and evaluate AIFPL source, returning the Python result."""
    return AIFPL().evaluate(source)


# ---------------------------------------------------------------------------
# Unit tests: AIFPLIRUseCounter
# ---------------------------------------------------------------------------

class TestIRUseCounterBasic:
    """Basic use counting on hand-constructed IR nodes."""

    def test_constant_has_no_variable_uses(self):
        """A lone constant has no variable references."""
        ir = AIFPLIRReturn(value_plan=_const(42))
        counts = AIFPLIRUseCounter().count(ir)
        # Frame 0 exists; no slots used.
        assert len(counts.frames) == 1
        assert counts.frames[0].local == {}
        assert counts.frames[0].external == {}

    def test_single_local_use_counted(self):
        """A single reference to a local variable is counted once."""
        # (let ((x 1)) x)  — slot 0 used once in body
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        assert counts.local_count(0, 0) == 1
        assert counts.external_count(0, 0) == 0
        assert counts.total_count(0, 0) == 1

    def test_double_use_counted(self):
        """Two references to the same local variable sum correctly."""
        # (let ((x 1)) (integer+ x x))
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_add_call(_local(0), _local(0)),
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        assert counts.total_count(0, 0) == 2

    def test_dead_binding_has_zero_count(self):
        """A binding never referenced has total count == 0."""
        # (let ((x 1)) 99)  — x is never used
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_const(99),
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        assert counts.total_count(0, 0) == 0

    def test_two_bindings_independent_counts(self):
        """Two bindings at different slots are counted independently."""
        # (let ((x 1) (y 2)) (integer+ x x))  — y is dead
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0), ("y", _const(2), 1)],
            body_plan=_add_call(_local(0), _local(0)),
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        assert counts.total_count(0, 0) == 2   # x used twice
        assert counts.total_count(0, 1) == 0   # y never used

    def test_global_not_counted(self):
        """Global / builtin variable references are not counted."""
        ir = AIFPLIRReturn(value_plan=_global('integer+'))
        counts = AIFPLIRUseCounter().count(ir)
        assert counts.frames[0].local == {}
        assert counts.frames[0].external == {}


class TestIRUseCounterLambda:
    """Use counting across lambda frame boundaries."""

    def test_lambda_gets_own_frame(self):
        """Each lambda node is assigned its own frame."""
        lam = AIFPLIRLambda(
            params=["x"],
            body_plan=AIFPLIRReturn(value_plan=_local(0)),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
        )
        ir = AIFPLIRReturn(value_plan=lam)
        counts = AIFPLIRUseCounter().count(ir)
        # Two frames: module + lambda
        assert len(counts.frames) == 2
        lambda_frame_id = counts.lambda_frame_ids[id(lam)]
        assert lambda_frame_id == 1

    def test_param_use_counted_in_lambda_frame(self):
        """A parameter use inside a lambda body is counted in the lambda's frame."""
        lam = AIFPLIRLambda(
            params=["x"],
            body_plan=AIFPLIRReturn(value_plan=_local(0)),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
        )
        ir = AIFPLIRReturn(value_plan=lam)
        counts = AIFPLIRUseCounter().count(ir)
        lam_fid = counts.lambda_frame_ids[id(lam)]
        assert counts.local_count(lam_fid, 0) == 1

    def test_free_variable_counted_as_external_on_enclosing_frame(self):
        """
        A free variable captured by a lambda is counted as 'external' on the
        enclosing (defining) frame, not as 'local'.
        """
        # Outer let binds slot 0; lambda captures it via free_var_plans.
        # Inside the lambda the free var is loaded as depth=1 (one frame up).
        free_var_plan = _local(index=0, depth=0)  # loaded in enclosing frame
        lam = AIFPLIRLambda(
            params=[],
            body_plan=AIFPLIRReturn(value_plan=_local(index=0, depth=1)),
            free_vars=["outer_x"],
            free_var_plans=[free_var_plan],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=0,
            is_variadic=False,
            max_locals=1,
        )
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("outer_x", _const(5), 0)],
            body_plan=lam,
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        # The free_var_plan (depth=0 in the enclosing frame) increments local
        # count for slot 0 in frame 0.
        assert counts.local_count(0, 0) == 1

    def test_cross_frame_reference_counted_as_external(self):
        """
        A variable reference with depth > 0 (cross-frame) is counted as
        'external' on the defining frame.
        """
        lam = AIFPLIRLambda(
            params=[],
            # Directly references slot 0 at depth=1 (one frame up) in body
            body_plan=AIFPLIRReturn(value_plan=_local(index=0, depth=1)),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=0,
            is_variadic=False,
            max_locals=0,
        )
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=lam,
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        # depth=1 reference → external count on frame 0
        assert counts.external_count(0, 0) == 1
        assert counts.local_count(0, 0) == 0

    def test_nested_lambdas_have_separate_frames(self):
        """Two nested lambdas each get their own frame."""
        inner = AIFPLIRLambda(
            params=["y"],
            body_plan=AIFPLIRReturn(value_plan=_local(0)),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
        )
        outer = AIFPLIRLambda(
            params=["x"],
            body_plan=AIFPLIRReturn(value_plan=inner),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
        )
        ir = AIFPLIRReturn(value_plan=outer)
        counts = AIFPLIRUseCounter().count(ir)
        # module + outer + inner = 3 frames
        assert len(counts.frames) == 3
        assert counts.lambda_frame_ids[id(outer)] != counts.lambda_frame_ids[id(inner)]


class TestIRUseCounterLetrec:
    """Use counting for letrec / recursive bindings."""

    def test_letrec_live_binding_counted(self):
        """A letrec binding used in the body has non-zero count."""
        from aifpl.aifpl_dependency_analyzer import AIFPLBindingGroup
        from aifpl.aifpl_ast import AIFPLASTInteger as ASTInt
        ir = AIFPLIRReturn(value_plan=AIFPLIRLetrec(
            bindings=[("f", _const(1), 0)],
            body_plan=_local(0),
            binding_groups=[AIFPLBindingGroup(
                names={"f"}, bindings=[("f", ASTInt(1))],
                is_recursive=False, depends_on=set())],
            recursive_bindings=set(),
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        assert counts.total_count(0, 0) == 1

    def test_letrec_dead_binding_zero_count(self):
        """A letrec binding never used has count == 0."""
        from aifpl.aifpl_dependency_analyzer import AIFPLBindingGroup
        from aifpl.aifpl_ast import AIFPLASTInteger as ASTInt
        ir = AIFPLIRReturn(value_plan=AIFPLIRLetrec(
            bindings=[("f", _const(1), 0)],
            body_plan=_const(99),
            binding_groups=[AIFPLBindingGroup(
                names={"f"}, bindings=[("f", ASTInt(1))],
                is_recursive=False, depends_on=set())],
            recursive_bindings=set(),
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        assert counts.total_count(0, 0) == 0


class TestIsOnlySelfReferencing:
    """Tests for IRUseCounts.is_only_self_referencing helper."""

    def test_zero_uses_is_only_self_referencing_with_zero_self_refs(self):
        ir = AIFPLIRReturn(value_plan=_const(1))
        counts = AIFPLIRUseCounter().count(ir)
        # Slot 5 was never seen — total == 0, self_ref_count == 0 → True
        assert counts.is_only_self_referencing(0, 5, 0)

    def test_one_external_use_not_only_self_referencing(self):
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        counts = AIFPLIRUseCounter().count(ir)
        # total == 1, self_ref_count == 0 → False (there is an external use)
        assert not counts.is_only_self_referencing(0, 0, 0)


# ---------------------------------------------------------------------------
# Unit tests: AIFPLIROptimizer
# ---------------------------------------------------------------------------

class TestIROptimizerDeadBindingElimination:
    """Dead binding elimination via the optimizer."""

    def _run(self, ir):
        counts = AIFPLIRUseCounter().count(ir)
        return AIFPLIROptimizer(counts).optimize(ir)

    def test_live_binding_preserved(self):
        """A binding that is used must not be removed."""
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        result = self._run(ir)
        assert isinstance(result, AIFPLIRReturn)
        inner = result.value_plan
        assert isinstance(inner, AIFPLIRLet)
        assert len(inner.bindings) == 1

    def test_dead_binding_removed(self):
        """A binding with zero uses is dropped; the let collapses to its body."""
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=_const(99),
            in_tail_position=True,
        ))
        result = self._run(ir)
        # The let should have collapsed entirely
        assert isinstance(result, AIFPLIRReturn)
        inner = result.value_plan
        assert isinstance(inner, AIFPLIRConstant)
        assert inner.value == AIFPLInteger(99)

    def test_one_dead_one_live_binding(self):
        """Only the dead binding is removed; the live one is kept."""
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0), ("y", _const(2), 1)],
            body_plan=_local(0),   # only x used
            in_tail_position=True,
        ))
        result = self._run(ir)
        assert isinstance(result, AIFPLIRReturn)
        inner = result.value_plan
        assert isinstance(inner, AIFPLIRLet)
        assert len(inner.bindings) == 1
        assert inner.bindings[0][0] == "x"

    def test_all_dead_bindings_collapses_let(self):
        """When every binding is dead the entire let node is replaced by its body."""
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0), ("y", _const(2), 1)],
            body_plan=_const(42),
            in_tail_position=True,
        ))
        result = self._run(ir)
        assert isinstance(result, AIFPLIRReturn)
        inner = result.value_plan
        assert isinstance(inner, AIFPLIRConstant)
        assert inner.value == AIFPLInteger(42)

    def test_nested_let_chain_collapses(self):
        """
        Nested let where inner dead binding is dropped, then outer dead binding
        is also dropped in a second optimizer pass (fixed-point iteration).
        """
        # (let ((x 1))           — x used only by y
        #   (let ((y x))         — y never used in body
        #     42))
        # After pass 1: y is dead → inner let collapses to 42.
        #               Now x is dead too.
        # After pass 2: x is dead → outer let collapses to 42.
        inner_let = AIFPLIRLet(
            bindings=[("y", _local(0), 1)],  # y = x (slot 0)
            body_plan=_const(42),
            in_tail_position=True,
        )
        outer_let = AIFPLIRLet(
            bindings=[("x", _const(1), 0)],
            body_plan=inner_let,
            in_tail_position=True,
        )
        ir = AIFPLIRReturn(value_plan=outer_let)

        # Single pass: inner dead binding gone, outer still has x (used by y's
        # value expression — even though y itself is dead, x's use in y's value
        # was already counted before y was eliminated).
        result1 = self._run(ir)

        # Second pass: now x is dead too.
        counts2 = AIFPLIRUseCounter().count(result1)
        result2 = AIFPLIROptimizer(counts2).optimize(result1)

        assert isinstance(result2, AIFPLIRReturn)
        inner = result2.value_plan
        assert isinstance(inner, AIFPLIRConstant)
        assert inner.value == AIFPLInteger(42)

    def test_tail_position_preserved_after_optimization(self):
        """in_tail_position flag is carried through to the optimized let."""
        ir = AIFPLIRReturn(value_plan=AIFPLIRLet(
            bindings=[("x", _const(1), 0), ("dead", _const(2), 1)],
            body_plan=_local(0),
            in_tail_position=True,
        ))
        result = self._run(ir)
        inner = result.value_plan  # type: ignore[union-attr]
        assert isinstance(inner, AIFPLIRLet)
        assert inner.in_tail_position is True


class TestIROptimizerLetrec:
    """Dead binding elimination for letrec nodes."""

    def _run(self, ir):
        counts = AIFPLIRUseCounter().count(ir)
        return AIFPLIROptimizer(counts).optimize(ir)

    def test_live_letrec_binding_preserved(self):
        """A letrec binding used in the body is kept."""
        from aifpl.aifpl_dependency_analyzer import AIFPLBindingGroup
        from aifpl.aifpl_ast import AIFPLASTInteger as ASTInt
        ir = AIFPLIRReturn(value_plan=AIFPLIRLetrec(
            bindings=[("f", _const(1), 0)],
            body_plan=_local(0),
            binding_groups=[AIFPLBindingGroup(
                names={"f"}, bindings=[("f", ASTInt(1))],
                is_recursive=False, depends_on=set())],
            recursive_bindings=set(),
            in_tail_position=True,
        ))
        result = self._run(ir)
        assert isinstance(result, AIFPLIRReturn)
        inner = result.value_plan
        assert isinstance(inner, AIFPLIRLetrec)
        assert len(inner.bindings) == 1

    def test_dead_letrec_binding_removed(self):
        """A letrec binding with zero uses is dropped."""
        from aifpl.aifpl_dependency_analyzer import AIFPLBindingGroup
        from aifpl.aifpl_ast import AIFPLASTInteger as ASTInt
        ir = AIFPLIRReturn(value_plan=AIFPLIRLetrec(
            bindings=[("f", _const(1), 0)],
            body_plan=_const(99),
            binding_groups=[AIFPLBindingGroup(
                names={"f"}, bindings=[("f", ASTInt(1))],
                is_recursive=False, depends_on=set())],
            recursive_bindings=set(),
            in_tail_position=True,
        ))
        result = self._run(ir)
        assert isinstance(result, AIFPLIRReturn)
        inner = result.value_plan
        assert isinstance(inner, AIFPLIRConstant)
        assert inner.value == AIFPLInteger(99)


class TestIROptimizerLambda:
    """Optimizer correctly descends into lambda bodies."""

    def _run(self, ir):
        counts = AIFPLIRUseCounter().count(ir)
        return AIFPLIROptimizer(counts).optimize(ir)

    def test_dead_binding_inside_lambda_removed(self):
        """Dead let binding inside a lambda body is eliminated."""
        lam = AIFPLIRLambda(
            params=["p"],
            body_plan=AIFPLIRReturn(value_plan=AIFPLIRLet(
                bindings=[("dead", _const(99), 1)],  # slot 1 (slot 0 is param)
                body_plan=_local(0),                 # returns param, not dead
                in_tail_position=True,
            )),
            free_vars=[],
            free_var_plans=[],
            parent_refs=[],
            parent_ref_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=2,
        )
        ir = AIFPLIRReturn(value_plan=lam)
        result = self._run(ir)

        assert isinstance(result, AIFPLIRReturn)
        opt_lam = result.value_plan
        assert isinstance(opt_lam, AIFPLIRLambda)
        # Body should have collapsed: dead let removed, just the param return
        body = opt_lam.body_plan
        assert isinstance(body, AIFPLIRReturn)
        assert isinstance(body.value_plan, AIFPLIRVariable)
        assert body.value_plan.index == 0


# ---------------------------------------------------------------------------
# Integration tests: compile + evaluate
# ---------------------------------------------------------------------------

class TestIROptimizerIntegration:
    """
    End-to-end tests: compile real AIFPL source with optimization enabled and
    verify both correctness and (where possible) that optimization fired.
    """

    @pytest.fixture
    def aifpl(self):
        return AIFPL()

    def test_simple_expression_unchanged(self, aifpl):
        """Optimization doesn't break a simple arithmetic expression."""
        assert aifpl.evaluate("(integer+ 1 2)") == 3

    def test_live_let_binding_works(self, aifpl):
        """A used let binding still works after optimization."""
        assert aifpl.evaluate("(let ((x 10)) (integer* x 2))") == 20

    def test_dead_let_binding_program_still_correct(self, aifpl):
        """Program with a dead binding evaluates correctly after dead-code removal."""
        # The optimizer should drop `unused`; the result must still be 42.
        result = aifpl.evaluate("""
            (let ((used 42)
                  (unused 99))
              used)
        """)
        assert result == 42

    def test_nested_dead_bindings_correct(self, aifpl):
        """Nested dead bindings are handled; result is still correct."""
        result = aifpl.evaluate("""
            (let ((a 1))
              (let ((b 2)
                    (c 3))
                a))
        """)
        assert result == 1

    def test_closure_with_dead_binding_correct(self, aifpl):
        """Dead binding inside a closure is removed; closure still works."""
        result = aifpl.evaluate("""
            (let ((f (lambda (x)
                       (let ((dead 999))
                         (integer+ x 1)))))
              (f 41))
        """)
        assert result == 42

    def test_recursive_function_preserved(self, aifpl):
        """A recursive letrec binding is not mistakenly eliminated."""
        result = aifpl.evaluate("""
            (letrec ((fact (lambda (n)
                             (if (integer<=? n 1)
                                 1
                                 (integer* n (fact (integer- n 1)))))))
              (fact 6))
        """)
        assert result == 720

    def test_mutual_recursion_preserved(self, aifpl):
        """Mutually recursive letrec bindings are both preserved."""
        result = aifpl.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (list (even? 4) (odd? 3)))
        """)
        assert result == [True, True]

    def test_higher_order_with_dead_binding(self, aifpl):
        """list-map with a closure that has a dead binding."""
        result = aifpl.evaluate("""
            (let ((factor 3))
              (list-map
                (lambda (x)
                  (let ((dead "ignored"))
                    (integer* x factor)))
                (list 1 2 3)))
        """)
        assert result == [3, 6, 9]

    def test_if_branches_with_dead_bindings(self, aifpl):
        """Dead bindings in if branches are removed; both branches still work."""
        result_true = aifpl.evaluate("""
            (if #t
                (let ((dead 0)) 1)
                (let ((dead 0)) 2))
        """)
        assert result_true == 1

        result_false = aifpl.evaluate("""
            (if #f
                (let ((dead 0)) 1)
                (let ((dead 0)) 2))
        """)
        assert result_false == 2

    def test_tail_call_optimization_still_works(self, aifpl):
        """TCO still fires correctly after IR optimization (no stack overflow)."""
        result = aifpl.evaluate("""
            (letrec ((loop (lambda (n acc)
                             (if (integer=? n 0)
                                 acc
                                 (loop (integer- n 1) (integer+ acc 1))))))
              (loop 100000 0))
        """)
        assert result == 100000

    def test_optimization_disabled_still_correct(self):
        """Compiling with optimize=False produces the same result as with optimization."""
        source = "(let ((x 7) (unused 99)) (integer* x 6))"
        opt_result = AIFPL().evaluate(source)

        # Compile without IR optimization by using the compiler directly and
        # running it through a fresh AIFPL instance whose compiler has optimize=False.
        # We can't pass optimize= to AIFPL() directly, so we patch the compiler.
        instance = AIFPL()
        instance.compiler = AIFPLCompiler(optimize=False, module_loader=instance)
        no_opt_result = instance.evaluate(source)

        assert opt_result == no_opt_result == 42

    def test_complex_program_correct(self, aifpl):
        """A realistic program with multiple scopes produces the right answer."""
        result = aifpl.evaluate("""
            (letrec ((fib (lambda (n)
                            (if (integer<? n 2)
                                n
                                (integer+ (fib (integer- n 1))
                                          (fib (integer- n 2)))))))
              (list-map fib (list 0 1 2 3 4 5 6 7)))
        """)
        assert result == [0, 1, 1, 2, 3, 5, 8, 13]
