"""
Tests for MenaiIRDevirtualizer.

Strategy
--------
Two layers:

1. Unit tests on the devirtualizer directly, constructing IR nodes by hand.
   These verify the structural transformation precisely without going through
   the full compiler pipeline.

2. Integration tests that compile real Menai source and verify that the
   devirtualized program still produces the correct result.

Key structural properties under test
--------------------------------------
- A static call to a let-bound non-variadic wrapper is rewritten to call the
  helper directly, with original args followed by sibling_free_var_plans then
  outer_free_var_plans[1:] (skipping the helper var at index 0).
- The tail-call-ness of the original call site is preserved.
- Non-wrapper calls (closed lambdas, globals, builtins) are left untouched.
- Variadic wrappers are skipped.
- Wrappers in a letrec are registered and their call sites devirtualized.
- Wrappers visible in an outer scope are devirtualizable inside inner lambdas.
- The devirtualizations() counter reflects the number of rewrites.
"""

from __future__ import annotations

import pytest

from menai import Menai
from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIRLambda,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRReturn,
    MenaiIRVariable,
)
from menai.menai_ir_devirtualizer import MenaiIRDevirtualizer
from menai.menai_value import MenaiInteger


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _const(n: int) -> MenaiIRConstant:
    return MenaiIRConstant(value=MenaiInteger(n))


def _local(name: str) -> MenaiIRVariable:
    return MenaiIRVariable(name=name, var_type='local', depth=-1, index=-1)


def _global(name: str) -> MenaiIRVariable:
    return MenaiIRVariable(name=name, var_type='global', depth=0, index=0)


def _call(func, args, *, tail=False) -> MenaiIRCall:
    return MenaiIRCall(
        func_plan=func,
        arg_plans=args,
        is_tail_call=tail,
        is_builtin=False,
        builtin_name=None,
    )


def _wrapper(
    name: str,
    params: list[str],
    helper_name: str,
    outer_captures: list[str],
    sibling_captures: list[str] | None = None,
    *,
    variadic: bool = False,
) -> MenaiIRLambda:
    """
    Build a minimal wrapper MenaiIRLambda as the lambda lifter would produce.

    outer_free_var_plans[0] is always the helper variable.
    outer_captures are the non-helper outer free vars.
    """
    sfv = sibling_captures or []
    return MenaiIRLambda(
        params=params,
        body_plan=MenaiIRReturn(value_plan=_call(
            _local(helper_name),
            [_local(p) for p in params] + [_local(v) for v in sfv] + [_local(v) for v in outer_captures],
            tail=True,
        )),
        sibling_free_vars=sfv,
        sibling_free_var_plans=[_local(v) for v in sfv],
        outer_free_vars=[helper_name] + outer_captures,
        outer_free_var_plans=[_local(helper_name)] + [_local(v) for v in outer_captures],
        param_count=len(params),
        is_variadic=variadic,
        binding_name=name,
        is_wrapper=True,
        lifted_helper_name=helper_name,
    )


def _closed_lambda(name: str, params: list[str]) -> MenaiIRLambda:
    return MenaiIRLambda(
        params=params,
        body_plan=MenaiIRReturn(value_plan=_const(0)),
        sibling_free_vars=[],
        sibling_free_var_plans=[],
        outer_free_vars=[],
        outer_free_var_plans=[],
        param_count=len(params),
        is_variadic=False,
        binding_name=name,
        is_wrapper=False,
        lifted_helper_name=None,
    )


def _devirt(ir):
    return MenaiIRDevirtualizer().devirtualize(ir)


def _devirt_with_count(ir):
    d = MenaiIRDevirtualizer()
    new_ir = d.devirtualize(ir)
    return new_ir, d.devirtualizations()


# ---------------------------------------------------------------------------
# Unit tests: basic devirtualization
# ---------------------------------------------------------------------------

class TestBasicDevirtualization:

    def test_static_wrapper_call_is_rewritten(self):
        """
        A call to a let-bound wrapper is rewritten to call the helper directly.
        """
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['cap'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(1)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        assert isinstance(result, MenaiIRLet)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        # Callee is now the helper variable, not the wrapper variable.
        assert isinstance(body.func_plan, MenaiIRVariable)
        assert body.func_plan.name == 'helper-f'

    def test_original_args_come_first(self):
        """Original call arguments appear first in the rewritten call."""
        wrapper = _wrapper('f', ['x', 'y'], 'helper-f', outer_captures=['cap'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(10), _const(20)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        assert isinstance(body.arg_plans[0], MenaiIRConstant)
        assert body.arg_plans[0].value.value == 10
        assert isinstance(body.arg_plans[1], MenaiIRConstant)
        assert body.arg_plans[1].value.value == 20

    def test_outer_captures_appended_after_args(self):
        """
        The wrapper's outer_free_var_plans[1:] (non-helper captures) are
        appended after the original args.
        """
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['a', 'b'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(1)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        # args: [const(1), local('a'), local('b')]
        assert len(body.arg_plans) == 3
        assert isinstance(body.arg_plans[1], MenaiIRVariable)
        assert body.arg_plans[1].name == 'a'
        assert isinstance(body.arg_plans[2], MenaiIRVariable)
        assert body.arg_plans[2].name == 'b'

    def test_sibling_captures_come_before_outer_captures(self):
        """
        sibling_free_var_plans precede outer_free_var_plans[1:] in the
        rewritten call's argument list.
        """
        wrapper = _wrapper('f', ['x'], 'helper-f',
                           outer_captures=['outer-cap'],
                           sibling_captures=['sib'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(1)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        # args: [const(1), local('sib'), local('outer-cap')]
        assert len(body.arg_plans) == 3
        assert isinstance(body.arg_plans[1], MenaiIRVariable)
        assert body.arg_plans[1].name == 'sib'
        assert isinstance(body.arg_plans[2], MenaiIRVariable)
        assert body.arg_plans[2].name == 'outer-cap'

    def test_tail_call_preserved(self):
        """A tail call to a wrapper is rewritten as a tail call to the helper."""
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['cap'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(1)], tail=True),
            in_tail_position=True,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        assert body.is_tail_call is True

    def test_non_tail_call_preserved(self):
        """A non-tail call to a wrapper is rewritten as a non-tail call to the helper."""
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['cap'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(1)], tail=False),
            in_tail_position=False,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        assert body.is_tail_call is False

    def test_devirtualizations_counter(self):
        """The devirtualizations() counter reflects the number of rewrites."""
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['cap'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=MenaiIRLet(
                bindings=[],
                body_plan=MenaiIRReturn(value_plan=MenaiIRCall(
                    func_plan=_local('f'),
                    arg_plans=[_const(1)],
                    is_tail_call=False,
                    is_builtin=False,
                    builtin_name=None,
                )),
                in_tail_position=False,
            ),
            in_tail_position=False,
        )
        # Two call sites to the same wrapper.
        ir2 = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=MenaiIRReturn(value_plan=MenaiIRCall(
                func_plan=_local('f'),
                arg_plans=[_call(_local('f'), [_const(1)])],
                is_tail_call=False,
                is_builtin=False,
                builtin_name=None,
            )),
            in_tail_position=False,
        )
        _, count = _devirt_with_count(ir2)
        assert count == 2


# ---------------------------------------------------------------------------
# Unit tests: calls that must NOT be devirtualized
# ---------------------------------------------------------------------------

class TestNonDevirtualizedCalls:

    def test_global_variable_call_untouched(self):
        """A call through a global variable is not devirtualized."""
        ir = _call(_global('some-fn'), [_const(1)])
        result = _devirt(ir)
        assert isinstance(result, MenaiIRCall)
        assert isinstance(result.func_plan, MenaiIRVariable)
        assert result.func_plan.var_type == 'global'

    def test_closed_lambda_call_untouched(self):
        """A call to a let-bound closed lambda (is_wrapper=False) is not rewritten."""
        closed = _closed_lambda('f', ['x'])
        ir = MenaiIRLet(
            bindings=[('f', closed)],
            body_plan=_call(_local('f'), [_const(1)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        # Callee is still the wrapper variable, not something else.
        assert isinstance(body.func_plan, MenaiIRVariable)
        assert body.func_plan.name == 'f'

    def test_variadic_wrapper_call_untouched(self):
        """A call to a variadic wrapper is not devirtualized."""
        wrapper = _wrapper('f', ['x', 'rest'], 'helper-f',
                           outer_captures=['cap'], variadic=True)
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(1), _const(2)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        assert isinstance(body.func_plan, MenaiIRVariable)
        assert body.func_plan.name == 'f'

    def test_arg_count_mismatch_untouched(self):
        """A call with wrong arg count is not devirtualized."""
        wrapper = _wrapper('f', ['x', 'y'], 'helper-f', outer_captures=['cap'])
        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            # Only 1 arg, but wrapper expects 2.
            body_plan=_call(_local('f'), [_const(1)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        assert isinstance(body.func_plan, MenaiIRVariable)
        assert body.func_plan.name == 'f'

    def test_builtin_call_untouched(self):
        """A builtin call is never devirtualized."""
        ir = MenaiIRCall(
            func_plan=_global('integer+'),
            arg_plans=[_const(1), _const(2)],
            is_tail_call=False,
            is_builtin=True,
            builtin_name='integer+',
        )
        result = _devirt(ir)
        assert isinstance(result, MenaiIRCall)
        assert result.is_builtin is True

    def test_unknown_local_call_untouched(self):
        """A call to a local variable not in the wrapper_map is not rewritten."""
        # 'f' is not bound in any enclosing let — not in wrapper_map.
        ir = _call(_local('f'), [_const(1)])
        result = _devirt(ir)
        assert isinstance(result, MenaiIRCall)
        assert isinstance(result.func_plan, MenaiIRVariable)
        assert result.func_plan.name == 'f'

    def test_zero_devirtualizations_when_no_wrappers(self):
        """Counter is zero when no wrappers are present."""
        ir = _call(_local('f'), [_const(1)])
        _, count = _devirt_with_count(ir)
        assert count == 0


# ---------------------------------------------------------------------------
# Unit tests: letrec wrappers
# ---------------------------------------------------------------------------

class TestLetrecDevirtualization:

    def test_letrec_wrapper_call_in_body_devirtualized(self):
        """
        A call to a letrec-bound wrapper in the letrec body is devirtualized.
        """
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=[])
        ir = MenaiIRLetrec(
            bindings=[('f', wrapper)],
            body_plan=_call(_local('f'), [_const(1)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        assert isinstance(result, MenaiIRLetrec)
        body = result.body_plan
        assert isinstance(body, MenaiIRCall)
        assert isinstance(body.func_plan, MenaiIRVariable)
        assert body.func_plan.name == 'helper-f'

    def test_letrec_wrapper_call_in_sibling_body_devirtualized(self):
        """
        Inside a letrec, a call to a sibling wrapper inside another binding's
        lambda body is NOT devirtualized — 'f' is in g's sibling_free_vars,
        so inside g's body it is a plain captured slot, not a wrapper closure.
        The call in the letrec body (outside any lambda) IS devirtualized.
        """
        # g's wrapper body calls f's wrapper.
        f_wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=[])
        g_body_call = _call(_local('f'), [_const(1)])
        g_wrapper = MenaiIRLambda(
            params=['y'],
            body_plan=MenaiIRReturn(value_plan=g_body_call),
            sibling_free_vars=['f'],
            sibling_free_var_plans=[_local('f')],
            outer_free_vars=['helper-g'],
            outer_free_var_plans=[_local('helper-g')],
            param_count=1,
            is_variadic=False,
            binding_name='g',
            is_wrapper=True,
            lifted_helper_name='helper-g',
        )
        ir = MenaiIRLetrec(
            bindings=[('f', f_wrapper), ('g', g_wrapper)],
            body_plan=_call(_local('g'), [_const(2)]),
            in_tail_position=False,
        )
        result = _devirt(ir)
        assert isinstance(result, MenaiIRLetrec)

        # g's body call to f is NOT devirtualized — 'f' is shadowed by g's
        # sibling_free_vars, so inside g's body it is a plain captured value.
        _, g_result = result.bindings[1]
        assert isinstance(g_result, MenaiIRLambda)
        inner_call = g_result.body_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(inner_call, MenaiIRCall)
        assert isinstance(inner_call.func_plan, MenaiIRVariable)
        assert inner_call.func_plan.name == 'f'

        # The letrec body call to g IS devirtualized (g is a wrapper, called
        # directly by name in the letrec body scope where g is a wrapper binding).
        body_call = result.body_plan
        assert isinstance(body_call, MenaiIRCall)
        assert isinstance(body_call.func_plan, MenaiIRVariable)
        assert body_call.func_plan.name == 'helper-g'


# ---------------------------------------------------------------------------
# Unit tests: wrapper visible across lambda boundary
# ---------------------------------------------------------------------------

class TestCrossLambdaBoundaryDevirtualization:

    def test_wrapper_captured_as_free_var_not_devirtualized_inside_lambda(self):
        """
        A wrapper bound in an outer let is NOT devirtualizable inside an inner
        lambda that captures it as a free var — inside the lambda body, 'f' is
        a plain captured slot value, not a wrapper closure.
        The call in the outer let body (before the lambda boundary) IS devirtualized.
        """
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['cap'])

        # Inner lambda captures 'f' as a free var and calls it in its body.
        inner_lambda = MenaiIRLambda(
            params=['y'],
            body_plan=MenaiIRReturn(value_plan=_call(_local('f'), [_const(1)])),
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=['f'],
            outer_free_var_plans=[_local('f')],
            param_count=1,
            is_variadic=False,
            binding_name='inner',
            is_wrapper=False,
            lifted_helper_name=None,
        )

        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=MenaiIRLet(
                bindings=[('inner', inner_lambda)],
                body_plan=_call(_local('inner'), [_const(2)]),
                in_tail_position=False,
            ),
            in_tail_position=False,
        )

        result = _devirt(ir)
        assert isinstance(result, MenaiIRLet)
        inner_let = result.body_plan
        assert isinstance(inner_let, MenaiIRLet)

        # The inner lambda's body call to f is NOT devirtualized — 'f' is in
        # outer_free_vars, so inside the lambda body it is a plain captured slot.
        _, inner_result = inner_let.bindings[0]
        assert isinstance(inner_result, MenaiIRLambda)
        call = inner_result.body_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(call, MenaiIRCall)
        assert isinstance(call.func_plan, MenaiIRVariable)
        assert call.func_plan.name == 'f'

        # The outer let body call to 'inner' is not a wrapper — not devirtualized.
        outer_call = inner_let.body_plan
        assert isinstance(outer_call, MenaiIRCall)
        assert isinstance(outer_call.func_plan, MenaiIRVariable)
        assert outer_call.func_plan.name == 'inner'


# ---------------------------------------------------------------------------
# Unit tests: shadowing — wrapper name reused as param or free var
# ---------------------------------------------------------------------------

class TestShadowingPreventsDevirtualization:

    def test_wrapper_name_shadowed_by_param_not_devirtualized(self):
        """
        If a lambda has a param with the same name as an outer wrapper, calls
        to that name inside the lambda body must NOT be devirtualized — the
        param is a plain local value, not a wrapper closure.
        """
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['cap'])

        # Inner lambda has a param also named 'f' — shadows the outer wrapper.
        inner_lambda = MenaiIRLambda(
            params=['f', 'y'],   # 'f' shadows the outer wrapper
            body_plan=MenaiIRReturn(value_plan=_call(_local('f'), [_const(1)])),
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=[],
            outer_free_var_plans=[],
            param_count=2,
            is_variadic=False,
            binding_name='inner',
            is_wrapper=False,
            lifted_helper_name=None,
        )

        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=MenaiIRLet(
                bindings=[('inner', inner_lambda)],
                body_plan=_call(_local('inner'), [_const(2), _const(3)]),
                in_tail_position=False,
            ),
            in_tail_position=False,
        )

        result = _devirt(ir)
        inner_let = result.body_plan
        assert isinstance(inner_let, MenaiIRLet)
        _, inner_result = inner_let.bindings[0]
        assert isinstance(inner_result, MenaiIRLambda)
        # The call inside the inner lambda body should still target 'f' (the param),
        # not 'helper-f'.
        call = inner_result.body_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(call, MenaiIRCall)
        assert isinstance(call.func_plan, MenaiIRVariable)
        assert call.func_plan.name == 'f'

    def test_wrapper_name_shadowed_by_free_var_not_devirtualized(self):
        """
        If a lambda captures a free var with the same name as an outer wrapper,
        calls to that name inside the lambda body must NOT be devirtualized —
        the free var slot holds a plain captured value, not a wrapper closure.
        """
        wrapper = _wrapper('f', ['x'], 'helper-f', outer_captures=['cap'])

        # Inner lambda captures 'f' as a free var (it's a plain captured value
        # inside the lambda, even though it happens to be a wrapper outside).
        inner_lambda = MenaiIRLambda(
            params=['y'],
            body_plan=MenaiIRReturn(value_plan=_call(_local('f'), [_const(1)])),
            sibling_free_vars=[],
            sibling_free_var_plans=[],
            outer_free_vars=['f'],
            outer_free_var_plans=[_local('f')],
            param_count=1,
            is_variadic=False,
            binding_name='inner',
            is_wrapper=False,
            lifted_helper_name=None,
        )

        ir = MenaiIRLet(
            bindings=[('f', wrapper)],
            body_plan=MenaiIRLet(
                bindings=[('inner', inner_lambda)],
                body_plan=_call(_local('inner'), [_const(2)]),
                in_tail_position=False,
            ),
            in_tail_position=False,
        )

        result = _devirt(ir)
        inner_let = result.body_plan
        assert isinstance(inner_let, MenaiIRLet)
        _, inner_result = inner_let.bindings[0]
        assert isinstance(inner_result, MenaiIRLambda)
        call = inner_result.body_plan.value_plan  # type: ignore[union-attr]
        assert isinstance(call, MenaiIRCall)
        assert isinstance(call.func_plan, MenaiIRVariable)
        assert call.func_plan.name == 'f'


# ---------------------------------------------------------------------------
# Integration tests: compile + evaluate
# ---------------------------------------------------------------------------

class TestDevirtualizerIntegration:
    """
    End-to-end tests: compile real Menai source through the full pipeline
    (including devirtualization) and verify correct results.
    """

    @pytest.fixture
    def menai(self):
        return Menai()

    def test_simple_closure_correct(self, menai):
        """A simple closure over an outer binding still produces correct results."""
        result = menai.evaluate("""
            (let ((x 10))
              (let ((add-x (lambda (n) (integer+ n x))))
                (add-x 32)))
        """)
        assert result == 42

    def test_mutual_recursion_correct(self, menai):
        """Mutually recursive letrec still produces correct results."""
        result = menai.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (list (even? 10) (odd? 7) (even? 0) (odd? 1)))
        """)
        assert result == [True, True, True, True]

    def test_mutual_recursion_tco(self, menai):
        """Devirtualized mutual recursion still handles large inputs without stack overflow."""
        result = menai.evaluate("""
            (letrec ((even? (lambda (n)
                              (if (integer=? n 0) #t (odd? (integer- n 1)))))
                     (odd?  (lambda (n)
                              (if (integer=? n 0) #f (even? (integer- n 1))))))
              (even? 100000))
        """)
        assert result is True

    def test_closure_passed_to_higher_order_function(self, menai):
        """
        Wrappers used as first-class values (not devirtualized) still work
        correctly when passed to higher-order functions.
        """
        result = menai.evaluate("""
            (let ((factor 3))
              (map-list
                (lambda (x) (integer* x factor))
                (list 1 2 3 4 5)))
        """)
        assert result == [3, 6, 9, 12, 15]

    def test_letrec_capturing_outer_binding(self, menai):
        """Letrec bindings capturing outer values work correctly after devirtualization."""
        result = menai.evaluate("""
            (let ((base 100))
              (letrec ((f (lambda (n)
                            (if (integer=? n 0)
                                base
                                (g (integer- n 1)))))
                       (g (lambda (n)
                            (if (integer=? n 0)
                                (integer+ base 1)
                                (f (integer- n 1))))))
                (list (f 0) (f 1) (f 2) (f 3))))
        """)
        assert result == [100, 101, 100, 101]

    def test_accumulator_mutual_recursion(self, menai):
        """Accumulator-style mutual recursion with captured outer values."""
        result = menai.evaluate("""
            (let ((step 1))
              (letrec ((count-a (lambda (n acc)
                                  (if (integer<=? n 0)
                                      acc
                                      (count-b (integer- n step) (integer+ acc 1)))))
                       (count-b (lambda (n acc)
                                  (if (integer<=? n 0)
                                      acc
                                      (count-a (integer- n step) (integer+ acc 1))))))
                (count-a 1000 0)))
        """)
        assert result == 1000

    def test_nested_closures_correct(self, menai):
        """Nested closures with multiple levels of capture still work."""
        result = menai.evaluate("""
            (let ((x 1))
              (let ((f (lambda (y)
                         (let ((g (lambda (z) (integer+ x (integer+ y z)))))
                           (g 10)))))
                (f 5)))
        """)
        assert result == 16

    def test_returned_closure_correct(self, menai):
        """A closure returned as a first-class value and called later still works."""
        result = menai.evaluate("""
            (let ((base 10))
              (letrec ((make-adder (lambda (n)
                                     (lambda (x) (integer+ x (integer+ n base))))))
                (let ((add5 (make-adder 5)))
                  (add5 27))))
        """)
        assert result == 42

    def test_three_way_mutual_recursion(self, menai):
        """Three-way mutual recursion works correctly after devirtualization."""
        result = menai.evaluate("""
            (letrec ((f (lambda (n) (if (integer<=? n 0) "f" (g (integer- n 1)))))
                     (g (lambda (n) (if (integer<=? n 0) "g" (h (integer- n 1)))))
                     (h (lambda (n) (if (integer<=? n 0) "h" (f (integer- n 1))))))
              (list (f 0) (f 1) (f 2) (f 3)))
        """)
        assert result == ["f", "g", "h", "f"]
