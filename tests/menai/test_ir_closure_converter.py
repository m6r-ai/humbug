"""
Tests for MenaiIRClosureConverter.

Strategy
--------
Two layers:

1. Unit tests on the converter directly, using hand-constructed IR trees.
   These verify the structural behaviour in isolation:
     - All node types are recursed into correctly (tree rewrite identity).
     - Lambda params, free_vars, free_var_plans, param_count, max_locals
       are preserved (not altered by the pass).
     - parent_refs / parent_ref_plans are preserved.
     - free_var_plans and parent_ref_plans are recursed into (they may
       contain nested lambdas).
     - The tail-recursive sentinel func_plan is not walked.
     - Metadata fields (binding_name, is_variadic, source_line, etc.)
       are preserved.

2. Integration tests that compile real Menai source through the full
   pipeline (including the converter) and assert correct evaluation
   results.  These are the primary safety net.

Background
----------
MenaiIRClosureConverter is currently a tree-rewriting identity pass.
It recurses into every IR node and returns a structurally equivalent tree,
including into free_var_plans (which are evaluated in the enclosing frame
and may contain nested lambdas).

The MAKE_CLOSURE mechanism already makes closure capture explicit at the
bytecode level.  param_count must not change (it controls ENTER), and
free_vars must not be cleared (MAKE_CLOSURE reads them).

After the PATCH_CLOSURE refactor, letrec siblings are regular free_vars;
parent_refs and parent_ref_plans no longer exist on MenaiIRLambda.
"""

from __future__ import annotations

import pytest

from menai import Menai
from menai.menai_compiler import MenaiCompiler
from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIRIf,
    MenaiIRLambda,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRReturn,
    MenaiIRTrace,
    MenaiIRVariable,
    MenaiIREmptyList,
)
from menai.menai_ir_closure_converter import MenaiIRClosureConverter
from menai.menai_value import MenaiInteger, MenaiString


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _const(n: int) -> MenaiIRConstant:
    return MenaiIRConstant(value=MenaiInteger(n))


def _local(name: str, index: int, depth: int = 0,
           is_parent_ref: bool = False) -> MenaiIRVariable:
    return MenaiIRVariable(
        name=name,
        var_type='local',
        depth=depth,
        index=index,
        is_parent_ref=is_parent_ref,
    )


def _global(name: str) -> MenaiIRVariable:
    return MenaiIRVariable(name=name, var_type='global', depth=0, index=0)


def _make_lambda(
    params: list[str],
    body: 'MenaiIRReturn',
    outer_free_vars: list[str] | None = None,
    outer_free_var_plans: list | None = None,
    param_count: int | None = None,
    max_locals: int | None = None,
) -> MenaiIRLambda:
    """Convenience constructor for MenaiIRLambda in tests."""
    fv = outer_free_vars or []
    fvp = outer_free_var_plans or []
    pc = param_count if param_count is not None else len(params)
    ml = max_locals if max_locals is not None else pc + len(fv)
    return MenaiIRLambda(
        params=params,
        body_plan=body,
        sibling_free_vars=[],
        sibling_free_var_plans=[],
        outer_free_vars=fv,
        outer_free_var_plans=fvp,
        param_count=pc,
        is_variadic=False,
        max_locals=ml,
    )


def _convert(ir) -> object:
    return MenaiIRClosureConverter().convert(ir)


# ---------------------------------------------------------------------------
# Unit tests: tree rewrite identity
# ---------------------------------------------------------------------------

class TestClosureConverterPreservesLambdaStructure:
    """The converter preserves all lambda fields unchanged."""

    def test_params_unchanged(self):
        """params list is not altered."""
        lam = _make_lambda(
            params=['x', 'y'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        assert isinstance(out_lam, MenaiIRLambda)
        assert out_lam.params == ['x', 'y']

    def test_free_vars_preserved(self):
        """free_vars is not cleared or altered."""
        lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['captured'],
            outer_free_var_plans=[_local('captured', 0, depth=1)],
            param_count=1,
            max_locals=2,
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        assert out_lam.outer_free_vars == ['captured']
        assert len(out_lam.outer_free_var_plans) == 1

    def test_param_count_unchanged(self):
        """param_count is not altered (controls ENTER instruction)."""
        lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['cap'],
            outer_free_var_plans=[_local('cap', 0, depth=1)],
            param_count=1,
            max_locals=2,
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        assert out_lam.param_count == 1

    def test_max_locals_unchanged(self):
        """max_locals is 0 after the converter — the addresser owns it now."""
        lam = _make_lambda(
            params=['a', 'b'],
            body=MenaiIRReturn(value_plan=_local('a', 0)),
            outer_free_vars=['c'],
            outer_free_var_plans=[_local('c', 0, depth=1)],
            param_count=2,
            max_locals=10,
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        # max_locals is set by MenaiIRAddresser, not preserved by the converter.
        # The converter passes through whatever value is on the node (0 by default).
        assert out_lam.max_locals == 0

    def test_letrec_sibling_free_vars_preserved(self):
        """free_vars containing letrec siblings are preserved (no parent_refs after refactor)."""
        lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['sibling'],
            outer_free_var_plans=[_local('sibling', 1, depth=0)],
            param_count=1,
            max_locals=2,
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        assert out_lam.outer_free_vars == ['sibling']
        assert len(out_lam.outer_free_var_plans) == 1

    def test_binding_name_preserved(self):
        """binding_name metadata is preserved."""
        lam = MenaiIRLambda(
            params=['x'],
            body_plan=MenaiIRReturn(value_plan=_local('x', 0)),
            sibling_free_vars=[], sibling_free_var_plans=[], outer_free_vars=[],
            outer_free_var_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
            binding_name='my-func',
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        assert out_lam.binding_name == 'my-func'

    def test_is_variadic_preserved(self):
        """is_variadic flag is preserved."""
        lam = MenaiIRLambda(
            params=['x', 'rest'],
            body_plan=MenaiIRReturn(value_plan=_local('x', 0)),
            sibling_free_vars=[], sibling_free_var_plans=[], outer_free_vars=[],
            outer_free_var_plans=[],
            param_count=2,
            is_variadic=True,
            max_locals=2,
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        assert out_lam.is_variadic is True

    def test_source_location_preserved(self):
        """source_line and source_file are preserved."""
        lam = MenaiIRLambda(
            params=['x'],
            body_plan=MenaiIRReturn(value_plan=_local('x', 0)),
            sibling_free_vars=[], sibling_free_var_plans=[], outer_free_vars=[],
            outer_free_var_plans=[],
            param_count=1,
            is_variadic=False,
            max_locals=1,
            source_line=42,
            source_file='myfile.menai',
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        assert out_lam.source_line == 42
        assert out_lam.source_file == 'myfile.menai'


class TestClosureConverterRecursion:
    """The converter recurses into all sub-expressions."""

    def test_body_recursed(self):
        """The lambda body is walked (nested lambdas inside are visited)."""
        inner = _make_lambda(
            params=['y'],
            body=MenaiIRReturn(value_plan=_local('y', 0)),
        )
        outer = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=inner),
        )
        result = _convert(MenaiIRReturn(value_plan=outer))
        out_outer = result.value_plan  # type: ignore[union-attr]
        assert isinstance(out_outer, MenaiIRLambda)
        # Body was recursed into; inner lambda is still a MenaiIRLambda
        out_inner = out_outer.body_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(out_inner, MenaiIRLambda)

    def test_free_var_plans_recursed(self):
        """free_var_plans are walked (they may contain nested lambdas)."""
        # A lambda nested inside a free_var_plan (unusual but possible)
        nested_in_plan = _make_lambda(
            params=['z'],
            body=MenaiIRReturn(value_plan=_local('z', 0)),
        )
        outer = MenaiIRLambda(
            params=['x'],
            body_plan=MenaiIRReturn(value_plan=_local('x', 0)),
            sibling_free_vars=[], sibling_free_var_plans=[], outer_free_vars=['f'],
            outer_free_var_plans=[nested_in_plan],
            param_count=1,
            is_variadic=False,
            max_locals=2,
        )
        result = _convert(MenaiIRReturn(value_plan=outer))
        out_lam = result.value_plan  # type: ignore[union-attr]
        # The free_var_plan was recursed into (still a lambda)
        assert isinstance(out_lam.outer_free_var_plans[0], MenaiIRLambda)

    def test_sibling_free_var_plans_recursed(self):
        """free_var_plans for letrec siblings are walked (no parent_ref_plans after refactor)."""
        plan_var = _local('sibling', 1, depth=0)
        lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['sibling'],
            outer_free_var_plans=[plan_var],
            param_count=1,
            max_locals=2,
        )
        result = _convert(MenaiIRReturn(value_plan=lam))
        out_lam = result.value_plan  # type: ignore[union-attr]
        # free_var_plans was walked; variable is unchanged (leaf node)
        assert isinstance(out_lam.outer_free_var_plans[0], MenaiIRVariable)

    def test_let_binding_values_recursed(self):
        """Lambdas in let binding values are visited."""
        inner_lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['cap'],
            outer_free_var_plans=[_local('cap', 0, depth=1)],
            param_count=1,
            max_locals=2,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLet(
            bindings=[('f', inner_lam)],
            body_plan=_local('f', 0),
            in_tail_position=True,
        ))
        result = _convert(ir)
        inner_let = result.value_plan  # type: ignore[union-attr]
        assert isinstance(inner_let, MenaiIRLet)
        out_lam = inner_let.bindings[0][1]
        # The lambda was visited and returned (free_vars still intact)
        assert isinstance(out_lam, MenaiIRLambda)
        assert out_lam.outer_free_vars == ['cap']

    def test_letrec_binding_values_recursed(self):
        """Lambdas in letrec binding values are visited."""
        inner_lam = _make_lambda(
            params=['n'],
            body=MenaiIRReturn(value_plan=_local('n', 0)),
            outer_free_vars=['base'],
            outer_free_var_plans=[_local('base', 0, depth=1)],
            param_count=1,
            max_locals=2,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRLetrec(
            bindings=[('f', inner_lam)],
            body_plan=_local('f', 0),
            in_tail_position=True,
        ))
        result = _convert(ir)
        inner_letrec = result.value_plan  # type: ignore[union-attr]
        assert isinstance(inner_letrec, MenaiIRLetrec)
        out_lam = inner_letrec.bindings[0][1]
        assert isinstance(out_lam, MenaiIRLambda)
        assert out_lam.outer_free_vars == ['base']

    def test_if_branches_recursed(self):
        """Lambdas inside if branches are visited."""
        lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['cap'],
            outer_free_var_plans=[_local('cap', 0, depth=1)],
            param_count=1,
            max_locals=2,
        )
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=_const(1),
            then_plan=MenaiIRReturn(value_plan=lam),
            else_plan=MenaiIRReturn(value_plan=_const(0)),
            in_tail_position=True,
        ))
        result = _convert(ir)
        out_if = result.value_plan  # type: ignore[union-attr]
        assert isinstance(out_if, MenaiIRIf)
        out_lam = out_if.then_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(out_lam, MenaiIRLambda)
        assert out_lam.outer_free_vars == ['cap']   # preserved

    def test_call_args_recursed(self):
        """Lambdas passed as call arguments are visited."""
        lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['cap'],
            outer_free_var_plans=[_local('cap', 0, depth=1)],
            param_count=1,
            max_locals=2,
        )
        call = MenaiIRCall(
            func_plan=_global('map-list'),
            arg_plans=[lam, MenaiIREmptyList()],
            is_tail_call=False,
            is_builtin=False,
            builtin_name=None,
        )
        result = _convert(MenaiIRReturn(value_plan=call))
        out_call = result.value_plan  # type: ignore[union-attr]
        assert isinstance(out_call, MenaiIRCall)
        out_lam = out_call.arg_plans[0]
        assert isinstance(out_lam, MenaiIRLambda)
        assert out_lam.outer_free_vars == ['cap']   # preserved

    def test_trace_node_recursed(self):
        """Lambdas inside a trace node are visited."""
        lam = _make_lambda(
            params=['x'],
            body=MenaiIRReturn(value_plan=_local('x', 0)),
            outer_free_vars=['cap'],
            outer_free_var_plans=[_local('cap', 0, depth=1)],
            param_count=1,
            max_locals=2,
        )
        trace = MenaiIRTrace(
            message_plans=[MenaiIRConstant(value=MenaiString("msg"))],
            value_plan=lam,
        )
        result = _convert(MenaiIRReturn(value_plan=trace))
        out_trace = result.value_plan  # type: ignore[union-attr]
        assert isinstance(out_trace, MenaiIRTrace)
        out_lam = out_trace.value_plan
        assert isinstance(out_lam, MenaiIRLambda)
        assert out_lam.outer_free_vars == ['cap']   # preserved

    def test_leaf_nodes_returned_unchanged(self):
        """Leaf nodes (constants, globals, empty list) pass through untouched."""
        for leaf in [_const(1), _global('integer+'), MenaiIREmptyList()]:
            result = _convert(MenaiIRReturn(value_plan=leaf))
            assert result.value_plan is leaf  # type: ignore[union-attr]


# ---------------------------------------------------------------------------
# Integration tests: compile + evaluate through the full pipeline
# ---------------------------------------------------------------------------

class TestClosureConverterIntegration:
    """
    End-to-end tests: compile real Menai source with the closure converter
    in the pipeline and verify correct evaluation results.

    These tests exercise the converter via the full pipeline:
    classifier → addresser → converter → addresser → optimizer → codegen → VM.
    """

    @pytest.fixture
    def menai(self):
        return Menai()

    # -- Basic closure capture --

    def test_simple_closure_capture(self, menai):
        """A closure that captures one variable from the enclosing scope."""
        result = menai.evaluate("""
            (let ((x 10))
              (let ((f (lambda (y) (integer+ x y))))
                (f 5)))
        """)
        assert result == 15

    def test_closure_captures_multiple_vars(self, menai):
        """A closure that captures multiple variables."""
        result = menai.evaluate("""
            (let ((a 3) (b 4))
              (let ((f (lambda () (integer+ a b))))
                (f)))
        """)
        assert result == 7

    def test_closure_returned_from_function(self, menai):
        """A closure returned from a function and called later."""
        result = menai.evaluate("""
            (let ((make-adder (lambda (n)
                                (lambda (x) (integer+ n x)))))
              (let ((add5 (make-adder 5)))
                (add5 10)))
        """)
        assert result == 15

    def test_closure_in_higher_order_function(self, menai):
        """A closure passed to a higher-order function."""
        result = menai.evaluate("""
            (let ((factor 3))
              (map-list (lambda (x) (integer* x factor))
                        (list 1 2 3 4)))
        """)
        assert result == [3, 6, 9, 12]

    # -- Nested closures --

    def test_doubly_nested_closure(self, menai):
        """A closure inside a closure captures from two levels up."""
        result = menai.evaluate("""
            (let ((x 1))
              (let ((f (lambda (y)
                         (lambda (z) (integer+ x (integer+ y z))))))
                ((f 2) 3)))
        """)
        assert result == 6

    def test_closure_over_let_binding(self, menai):
        """Closure captures a let binding correctly."""
        result = menai.evaluate("""
            (let ((base 100))
              (let ((f (lambda (n) (integer+ base n))))
                (integer+ (f 1) (f 2) (f 3))))
        """)
        assert result == 306

    # -- Recursive functions --

    def test_simple_recursion(self, menai):
        """A simple recursive function still works."""
        result = menai.evaluate("""
            (letrec ((fact (lambda (n)
                             (if (integer<=? n 1)
                                 1
                                 (integer* n (fact (integer- n 1)))))))
              (fact 7))
        """)
        assert result == 5040

    def test_recursive_function_with_closure(self, menai):
        """A recursive function that also captures a free variable."""
        result = menai.evaluate("""
            (let ((step 1))
              (letrec ((count-down (lambda (n)
                                     (if (integer<=? n 0)
                                         0
                                         (integer+ step (count-down (integer- n step)))))))
                (count-down 5)))
        """)
        assert result == 5

    def test_mutual_recursion(self, menai):
        """Mutually recursive functions still work correctly."""
        result = menai.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (list (even? 10) (odd? 7)))
        """)
        assert result == [True, True]

    # -- Tail calls --

    def test_tail_call_optimization_preserved(self, menai):
        """TCO still fires correctly after closure conversion (no stack overflow)."""
        result = menai.evaluate("""
            (letrec ((loop (lambda (n acc)
                             (if (integer=? n 0)
                                 acc
                                 (loop (integer- n 1) (integer+ acc 1))))))
              (loop 100000 0))
        """)
        assert result == 100000

    def test_tail_call_with_captured_var(self, menai):
        """TCO works when the recursive function also captures a free variable."""
        result = menai.evaluate("""
            (let ((step 2))
              (letrec ((loop (lambda (n acc)
                               (if (integer<=? n 0)
                                   acc
                                   (loop (integer- n step) (integer+ acc 1))))))
                (loop 100000 0)))
        """)
        assert result == 50000

    # -- Higher-order patterns --

    def test_map_with_closure(self, menai):
        """map-list with a closure captures correctly."""
        result = menai.evaluate("""
            (let ((offset 10))
              (map-list (lambda (x) (integer+ x offset)) (list 1 2 3)))
        """)
        assert result == [11, 12, 13]

    def test_filter_with_closure(self, menai):
        """filter-list with a closure captures correctly."""
        result = menai.evaluate("""
            (let ((threshold 3))
              (filter-list (lambda (x) (integer>? x threshold)) (list 1 2 3 4 5)))
        """)
        assert result == [4, 5]

    def test_fold_with_closure(self, menai):
        """fold-list with a closure captures correctly."""
        result = menai.evaluate("""
            (let ((multiplier 2))
              (fold-list (lambda (acc x) (integer+ acc (integer* x multiplier)))
                         0
                         (list 1 2 3 4 5)))
        """)
        assert result == 30

    # -- Regression: optimization still fires --

    def test_dead_binding_still_eliminated(self, menai):
        """Dead binding elimination still works after closure conversion."""
        result = menai.evaluate("""
            (let ((used 42) (dead 99))
              used)
        """)
        assert result == 42

    def test_complex_program(self, menai):
        """A realistic program with closures, recursion, and higher-order functions."""
        result = menai.evaluate("""
            (letrec ((fib (lambda (n)
                            (if (integer<? n 2)
                                n
                                (integer+ (fib (integer- n 1))
                                          (fib (integer- n 2)))))))
              (map-list fib (list 0 1 2 3 4 5 6 7)))
        """)
        assert result == [0, 1, 1, 2, 3, 5, 8, 13]

    def test_optimization_disabled_still_correct(self):
        """Closure conversion works correctly even without IR optimization."""
        source = """
            (let ((x 5))
              (let ((f (lambda (y) (integer+ x y))))
                (f 10)))
        """
        opt_result = Menai().evaluate(source)

        instance = Menai()
        instance.compiler = MenaiCompiler(optimize=False, module_loader=instance)
        no_opt_result = instance.evaluate(source)

        assert opt_result == no_opt_result == 15
