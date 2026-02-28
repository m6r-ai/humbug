"""
Tests for the boolean identity elimination in MenaiIROptimizer.

The optimizer rewrites two structural forms of MenaiIRIf:

    (if <cond> #t #f)  →  <cond>
    (if <cond> #f #t)  →  (boolean-not <cond>)

Strategy
--------
1. Unit tests on the optimizer directly, constructing IR nodes by hand.
   These verify the structural transformation without going through the
   full compiler pipeline.

2. Integration tests that compile real Menai source and verify that the
   optimized program produces the correct result for all input cases.
   These catch regressions where the optimizer silently changes semantics.
"""

from __future__ import annotations

import pytest

from menai import Menai
from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIRIf,
    MenaiIRReturn,
    MenaiIRVariable,
)
from menai.menai_ir_optimizer import MenaiIROptimizer
from menai.menai_value import MenaiBoolean, MenaiInteger


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _bool_const(value: bool) -> MenaiIRConstant:
    return MenaiIRConstant(value=MenaiBoolean(value))


def _int_const(n: int) -> MenaiIRConstant:
    return MenaiIRConstant(value=MenaiInteger(n))


def _global(name: str) -> MenaiIRVariable:
    return MenaiIRVariable(name=name, var_type='global', depth=0, index=0)


def _bool_p_call(arg: MenaiIRVariable) -> MenaiIRCall:
    """Emit (boolean? arg) as a builtin call."""
    return MenaiIRCall(
        func_plan=_global('boolean?'),
        arg_plans=[arg],
        is_tail_call=False,
        is_tail_recursive=False,
        is_builtin=True,
        builtin_name='boolean?',
    )


def _local(index: int) -> MenaiIRVariable:
    return MenaiIRVariable(
        name=f"v{index}", var_type='local', depth=0, index=index
    )


def _run(ir):
    """Run the optimizer and return (new_ir, changed)."""
    return MenaiIROptimizer().optimize(ir)


def _run_opt(ir):
    """Run the optimizer and return just the new IR."""
    new_ir, _ = _run(ir)
    return new_ir


# ---------------------------------------------------------------------------
# Unit tests: (if cond #t #f) → cond
# ---------------------------------------------------------------------------

class TestBooleanIdentityElimination:
    """(if <cond> #t #f) is replaced by <cond> directly."""

    def test_if_true_false_replaced_by_condition(self):
        """(if (boolean? v0) #t #f) → (boolean? v0)"""
        cond = _bool_p_call(_local(0))
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=cond,
            then_plan=_bool_const(True),
            else_plan=_bool_const(False),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        # The MenaiIRIf should have been eliminated entirely.
        assert isinstance(result.value_plan, MenaiIRCall)
        assert result.value_plan.builtin_name == 'boolean?'

    def test_if_true_false_sets_changed_flag(self):
        """changed is True when the pattern fires."""
        cond = _bool_p_call(_local(0))
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=cond,
            then_plan=_bool_const(True),
            else_plan=_bool_const(False),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert changed is True

    def test_no_change_when_pattern_absent(self):
        """changed is False when no boolean identity pattern is present."""
        # (if (boolean? v0) 1 2) — non-boolean branches, no transformation
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=_bool_p_call(_local(0)),
            then_plan=_int_const(1),
            else_plan=_int_const(2),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert changed is False

    def test_if_true_false_with_constant_condition(self):
        """(if #t #t #f) → #t  (condition is itself a constant)."""
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=_bool_const(True),
            then_plan=_bool_const(True),
            else_plan=_bool_const(False),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRConstant)
        assert result.value_plan.value == MenaiBoolean(True)

    def test_if_true_false_non_tail(self):
        """Transformation preserves in_tail_position=False on the condition."""
        cond = _bool_p_call(_local(0))
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=cond,
            then_plan=_bool_const(True),
            else_plan=_bool_const(False),
            in_tail_position=False,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        # The if was in non-tail position; the condition replaces it directly.
        assert isinstance(result.value_plan, MenaiIRCall)
        assert result.value_plan.builtin_name == 'boolean?'


# ---------------------------------------------------------------------------
# Unit tests: (if cond #f #t) → (boolean-not cond)
# ---------------------------------------------------------------------------

class TestBooleanNegationElimination:
    """(if <cond> #f #t) is replaced by (boolean-not <cond>)."""

    def test_if_false_true_replaced_by_boolean_not(self):
        """(if (boolean? v0) #f #t) → (boolean-not (boolean? v0))"""
        cond = _bool_p_call(_local(0))
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=cond,
            then_plan=_bool_const(False),
            else_plan=_bool_const(True),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        # Should be a call to boolean-not wrapping the original condition.
        assert isinstance(result.value_plan, MenaiIRCall)
        assert result.value_plan.builtin_name == 'boolean-not'
        assert len(result.value_plan.arg_plans) == 1
        inner = result.value_plan.arg_plans[0]
        assert isinstance(inner, MenaiIRCall)
        assert inner.builtin_name == 'boolean?'

    def test_if_false_true_sets_changed_flag(self):
        """changed is True when the negated pattern fires."""
        cond = _bool_p_call(_local(0))
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=cond,
            then_plan=_bool_const(False),
            else_plan=_bool_const(True),
            in_tail_position=True,
        ))
        _, changed = _run(ir)
        assert changed is True

    def test_if_false_true_tail_call_propagated(self):
        """The boolean-not call inherits is_tail_call from in_tail_position."""
        cond = _bool_p_call(_local(0))
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=cond,
            then_plan=_bool_const(False),
            else_plan=_bool_const(True),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result.value_plan, MenaiIRCall)
        assert result.value_plan.is_tail_call is True

    def test_if_false_true_non_tail_call_propagated(self):
        """is_tail_call is False when in_tail_position is False."""
        cond = _bool_p_call(_local(0))
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=cond,
            then_plan=_bool_const(False),
            else_plan=_bool_const(True),
            in_tail_position=False,
        ))
        result = _run_opt(ir)
        assert isinstance(result.value_plan, MenaiIRCall)
        assert result.value_plan.is_tail_call is False


# ---------------------------------------------------------------------------
# Unit tests: patterns that must NOT be transformed
# ---------------------------------------------------------------------------

class TestNoTransformation:
    """Cases that look similar but must not be rewritten."""

    def test_then_true_else_non_boolean_not_rewritten(self):
        """(if cond #t 42) — else is not a boolean constant, no change."""
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=_bool_p_call(_local(0)),
            then_plan=_bool_const(True),
            else_plan=_int_const(42),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRIf)

    def test_then_non_boolean_else_false_not_rewritten(self):
        """(if cond 42 #f) — then is not a boolean constant, no change."""
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=_bool_p_call(_local(0)),
            then_plan=_int_const(42),
            else_plan=_bool_const(False),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRIf)

    def test_then_true_else_true_not_rewritten(self):
        """(if cond #t #t) — both branches same, but not the identity pattern."""
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=_bool_p_call(_local(0)),
            then_plan=_bool_const(True),
            else_plan=_bool_const(True),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRIf)

    def test_then_false_else_false_not_rewritten(self):
        """(if cond #f #f) — both branches same, but not the negation pattern."""
        ir = MenaiIRReturn(value_plan=MenaiIRIf(
            condition_plan=_bool_p_call(_local(0)),
            then_plan=_bool_const(False),
            else_plan=_bool_const(False),
            in_tail_position=True,
        ))
        result = _run_opt(ir)
        assert isinstance(result, MenaiIRReturn)
        assert isinstance(result.value_plan, MenaiIRIf)


# ---------------------------------------------------------------------------
# Integration tests: correct runtime semantics
# ---------------------------------------------------------------------------

class TestBooleanIdentityIntegration:
    """End-to-end: optimized code must produce the same results as unoptimized."""

    @pytest.fixture
    def menai(self):
        return Menai()

    def test_identity_form_true_input(self):
        """(if (boolean? x) #t #f) returns #t when x is a boolean."""
        assert Menai().evaluate("(if (boolean? #t) #t #f)") == True

    def test_identity_form_false_input(self):
        """(if (boolean? x) #t #f) returns #f when x is not a boolean."""
        assert Menai().evaluate("(if (boolean? 42) #t #f)") == False

    def test_negation_form_true_input(self):
        """(if (boolean? x) #f #t) returns #f when x is a boolean."""
        assert Menai().evaluate("(if (boolean? #t) #f #t)") == False

    def test_negation_form_false_input(self):
        """(if (boolean? x) #f #t) returns #t when x is not a boolean."""
        assert Menai().evaluate("(if (boolean? 42) #f #t)") == True

    def test_identity_form_inside_lambda(self, menai):
        """The pattern is correctly optimized and evaluated inside a lambda."""
        result = menai.evaluate("""
            (let ((coerce-bool (lambda (x) (if (boolean? x) #t #f))))
              (list (coerce-bool #t) (coerce-bool #f) (coerce-bool 0) (coerce-bool "s")))
        """)
        assert result == [True, True, False, False]

    def test_negation_form_inside_lambda(self, menai):
        """The negation pattern is correctly optimized and evaluated inside a lambda."""
        result = menai.evaluate("""
            (let ((not-bool (lambda (x) (if (boolean? x) #f #t))))
              (list (not-bool #t) (not-bool #f) (not-bool 0) (not-bool "s")))
        """)
        assert result == [False, False, True, True]

    def test_nested_identity_forms(self, menai):
        """Nested occurrences are both eliminated."""
        result = menai.evaluate("""
            (let ((f (lambda (x)
                       (if (if (boolean? x) #t #f)
                           "was-bool"
                           "not-bool"))))
              (list (f #t) (f 42)))
        """)
        assert result == ["was-bool", "not-bool"]

    def test_identity_with_integer_predicate(self, menai):
        """Works with any predicate, not just boolean?."""
        result = menai.evaluate("""
            (let ((is-int (lambda (x) (if (integer? x) #t #f))))
              (list (is-int 42) (is-int "hi") (is-int #t)))
        """)
        assert result == [True, False, False]
