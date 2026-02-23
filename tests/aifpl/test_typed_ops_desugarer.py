"""Tests for typed arithmetic operator desugaring.

Covers the variadic desugaring of:
    integer+  integer-  integer*  integer/
    float+    float-    float*    float/
    complex+  complex-  complex*  complex/

Tests are organised into three layers:
  1. Desugarer structure — verify the AST shape produced by the desugarer.
  2. End-to-end evaluation — verify correct results via aifpl.evaluate_and_format.
  3. Error cases — verify that ill-formed calls are rejected appropriately.
"""

import cmath
import math

import pytest

from aifpl import AIFPL, AIFPLEvalError
from aifpl.aifpl_ast import (
    AIFPLASTBoolean,
    AIFPLASTComplex,
    AIFPLASTFloat,
    AIFPLASTInteger,
    AIFPLASTList,
    AIFPLASTNode,
    AIFPLASTSymbol,
)
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def parse(source: str) -> AIFPLASTNode:
    """Lex, parse, and semantically-analyse *source*, returning the AST."""
    tokens = AIFPLLexer().lex(source)
    ast = AIFPLParser().parse(tokens, source)
    return AIFPLSemanticAnalyzer().analyze(ast)


def desugar(source: str) -> AIFPLASTNode:
    """Parse and desugar *source*, returning the desugared AST."""
    return AIFPLDesugarer().desugar(parse(source))


def op_name(node: AIFPLASTNode) -> str:
    """Return the operator name of a list node."""
    assert isinstance(node, AIFPLASTList)
    first = node.first()
    assert isinstance(first, AIFPLASTSymbol)
    return first.name


# ---------------------------------------------------------------------------
# 1. Desugarer structure tests
# ---------------------------------------------------------------------------

class TestDesugarerZeroArgs:
    """Zero-argument cases produce the correct identity literal."""

    def test_integer_add_zero_args(self):
        result = desugar("(integer+)")
        assert isinstance(result, AIFPLASTInteger)
        assert result.value == 0

    def test_integer_mul_zero_args(self):
        result = desugar("(integer*)")
        assert isinstance(result, AIFPLASTInteger)
        assert result.value == 1

    def test_float_add_zero_args(self):
        result = desugar("(float+)")
        assert isinstance(result, AIFPLASTFloat)
        assert result.value == 0.0

    def test_float_mul_zero_args(self):
        result = desugar("(float*)")
        assert isinstance(result, AIFPLASTFloat)
        assert result.value == 1.0

    def test_complex_add_zero_args(self):
        result = desugar("(complex+)")
        assert isinstance(result, AIFPLASTComplex)
        assert result.value == 0+0j

    def test_complex_mul_zero_args(self):
        result = desugar("(complex*)")
        assert isinstance(result, AIFPLASTComplex)
        assert result.value == 1+0j


class TestDesugarerOneArg:
    """Single-argument cases: identity for +/*, negation for -, reciprocal for /."""

    # --- addition: single arg is the identity, return arg unchanged ---

    def test_integer_add_one_arg_is_identity(self):
        result = desugar("(integer+ 7)")
        assert isinstance(result, AIFPLASTInteger)
        assert result.value == 7

    def test_float_add_one_arg_is_identity(self):
        result = desugar("(float+ 3.5)")
        assert isinstance(result, AIFPLASTFloat)
        assert result.value == 3.5

    def test_complex_add_one_arg_is_identity(self):
        result = desugar("(complex+ (complex 1 2))")
        # The inner (complex 1 2) call is a regular function call, not a literal,
        # so the desugarer returns it as-is (a list node).
        assert isinstance(result, AIFPLASTList)

    # --- multiplication: single arg is the identity, return arg unchanged ---

    def test_integer_mul_one_arg_is_identity(self):
        result = desugar("(integer* 5)")
        assert isinstance(result, AIFPLASTInteger)
        assert result.value == 5

    def test_float_mul_one_arg_is_identity(self):
        result = desugar("(float* 2.5)")
        assert isinstance(result, AIFPLASTFloat)
        assert result.value == 2.5

    # --- subtraction: single arg desugars to (op 0 x) ---

    def test_integer_sub_one_arg_negation(self):
        result = desugar("(integer- 4)")
        assert op_name(result) == "integer-"
        assert isinstance(result, AIFPLASTList)
        assert len(result.elements) == 3
        zero = result.elements[1]
        assert isinstance(zero, AIFPLASTInteger)
        assert zero.value == 0

    def test_float_sub_one_arg_negation(self):
        result = desugar("(float- 4.0)")
        assert op_name(result) == "float-"
        zero = result.elements[1]
        assert isinstance(zero, AIFPLASTFloat)
        assert zero.value == 0.0

    def test_complex_sub_one_arg_negation(self):
        result = desugar("(complex- (complex 1 2))")
        assert op_name(result) == "complex-"
        zero = result.elements[1]
        assert isinstance(zero, AIFPLASTComplex)
        assert zero.value == 0+0j

    # --- division: reciprocal for /, float/, complex/; error path for integer/ ---

    def test_generic_div_one_arg_reciprocal(self):
        result = desugar("(/ 4)")
        assert op_name(result) == "/"
        one = result.elements[1]
        assert isinstance(one, AIFPLASTInteger)
        assert one.value == 1

    def test_float_div_one_arg_reciprocal(self):
        result = desugar("(float/ 4.0)")
        assert op_name(result) == "float/"
        one = result.elements[1]
        assert isinstance(one, AIFPLASTFloat)
        assert one.value == 1.0

    def test_complex_div_one_arg_reciprocal(self):
        result = desugar("(complex/ (complex 2 0))")
        assert op_name(result) == "complex/"
        one = result.elements[1]
        assert isinstance(one, AIFPLASTComplex)
        assert one.value == 1+0j

    def test_integer_div_one_arg_falls_through(self):
        # (integer/ x) has no sensible reciprocal (floor(1/x) == 0 for x > 1).
        # The semantic analyser enforces min_args=2 for integer/, so a 1-arg
        # call is rejected before the desugarer is reached.
        with pytest.raises(AIFPLEvalError):
            desugar("(integer/ 4)")


class TestDesugarerTwoArgs:
    """Binary calls pass through as-is (already optimal)."""

    def test_integer_add_two_args(self):
        result = desugar("(integer+ 3 4)")
        assert op_name(result) == "integer+"
        assert len(result.elements) == 3

    def test_float_sub_two_args(self):
        result = desugar("(float- 5.0 2.0)")
        assert op_name(result) == "float-"
        assert len(result.elements) == 3

    def test_complex_mul_two_args(self):
        result = desugar("(complex* (complex 1 2) (complex 3 4))")
        assert op_name(result) == "complex*"
        assert len(result.elements) == 3

    def test_integer_div_two_args(self):
        result = desugar("(integer/ 10 3)")
        assert op_name(result) == "integer/"
        assert len(result.elements) == 3

    def test_float_div_two_args(self):
        result = desugar("(float/ 10.0 4.0)")
        assert op_name(result) == "float/"
        assert len(result.elements) == 3


class TestDesugarerVariadic:
    """Three-or-more arguments fold left to right."""

    def _assert_left_fold(self, node: AIFPLASTNode, op: str, depth: int) -> None:
        """Recursively assert that *node* is a left-fold of *op* with *depth* levels."""
        assert op_name(node) == op
        assert isinstance(node, AIFPLASTList)
        if depth > 1:
            self._assert_left_fold(node.elements[1], op, depth - 1)

    def test_integer_add_three_args(self):
        result = desugar("(integer+ 1 2 3)")
        # (integer+ (integer+ 1 2) 3)
        self._assert_left_fold(result, "integer+", 2)

    def test_integer_add_four_args(self):
        result = desugar("(integer+ 1 2 3 4)")
        # (integer+ (integer+ (integer+ 1 2) 3) 4)
        self._assert_left_fold(result, "integer+", 3)

    def test_float_mul_three_args(self):
        result = desugar("(float* 2.0 3.0 4.0)")
        self._assert_left_fold(result, "float*", 2)

    def test_complex_add_three_args(self):
        result = desugar("(complex+ (complex 1 0) (complex 2 0) (complex 3 0))")
        self._assert_left_fold(result, "complex+", 2)

    def test_integer_sub_three_args(self):
        # (integer- 10 3 2) → (integer- (integer- 10 3) 2)
        result = desugar("(integer- 10 3 2)")
        self._assert_left_fold(result, "integer-", 2)

    def test_float_div_three_args(self):
        # (float/ 24.0 4.0 3.0) → (float/ (float/ 24.0 4.0) 3.0)
        result = desugar("(float/ 24.0 4.0 3.0)")
        self._assert_left_fold(result, "float/", 2)

    def test_integer_div_three_args(self):
        # (integer/ 24 4 3) → (integer/ (integer/ 24 4) 3)
        result = desugar("(integer/ 24 4 3)")
        self._assert_left_fold(result, "integer/", 2)

    def test_complex_mul_three_args(self):
        result = desugar("(complex* (complex 1 1) (complex 2 0) (complex 0 1))")
        self._assert_left_fold(result, "complex*", 2)


# ---------------------------------------------------------------------------
# 2. End-to-end evaluation tests
# ---------------------------------------------------------------------------

@pytest.fixture
def aifpl():
    return AIFPL()


class TestIntegerOpsEval:
    """integer+  integer-  integer*  integer/  evaluated end-to-end."""

    @pytest.mark.parametrize("expr,expected", [
        # zero-arg identity
        ("(integer+)", "0"),
        ("(integer*)", "1"),
        # one-arg identity
        ("(integer+ 7)", "7"),
        ("(integer* 5)", "5"),
        # unary negation
        ("(integer- 4)", "-4"),
        ("(integer- -3)", "3"),
        # binary
        ("(integer+ 3 4)", "7"),
        ("(integer- 10 3)", "7"),
        ("(integer* 6 7)", "42"),
        ("(integer/ 10 3)", "3"),   # floor division
        ("(integer/ -7 2)", "-4"),  # floor towards negative infinity
        # variadic fold
        ("(integer+ 1 2 3)", "6"),
        ("(integer+ 1 2 3 4)", "10"),
        ("(integer- 10 3 2)", "5"),
        ("(integer* 2 3 4)", "24"),
        ("(integer/ 24 4 3)", "2"),
    ])
    def test_integer_ops(self, aifpl, expr, expected):
        assert aifpl.evaluate_and_format(expr) == expected

    def test_integer_div_by_zero(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="[Dd]ivision by zero"):
            aifpl.evaluate("(integer/ 10 0)")

    def test_integer_ops_reject_float(self, aifpl):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(integer+ 1 2.0)")

    def test_integer_ops_reject_complex(self, aifpl):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(integer* 2 1j)")

    def test_integer_div_one_arg_is_error(self, aifpl):
        """(integer/ x) should be a runtime arity error, not a reciprocal."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(integer/ 4)")


class TestFloatOpsEval:
    """float+  float-  float*  float/  evaluated end-to-end."""

    @pytest.mark.parametrize("expr,expected", [
        # zero-arg identity
        ("(float+)", "0.0"),
        ("(float*)", "1.0"),
        # one-arg identity
        ("(float+ 3.5)", "3.5"),
        ("(float* 2.5)", "2.5"),
        # unary negation
        ("(float- 4.0)", "-4.0"),
        ("(float- -3.0)", "3.0"),
        # reciprocal
        ("(float/ 4.0)", "0.25"),
        ("(float/ 2.0)", "0.5"),
        # binary
        ("(float+ 1.5 2.5)", "4.0"),
        ("(float- 5.0 2.0)", "3.0"),
        ("(float* 2.0 3.5)", "7.0"),
        ("(float/ 10.0 4.0)", "2.5"),
        # variadic fold
        ("(float+ 1.0 2.0 3.0)", "6.0"),
        ("(float- 10.0 3.0 2.0)", "5.0"),
        ("(float* 2.0 3.0 4.0)", "24.0"),
        ("(float/ 24.0 4.0 3.0)", "2.0"),
    ])
    def test_float_ops(self, aifpl, expr, expected):
        assert aifpl.evaluate_and_format(expr) == expected

    def test_float_div_by_zero(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="[Dd]ivision by zero"):
            aifpl.evaluate("(float/ 1.0 0.0)")

    def test_float_ops_reject_integer(self, aifpl):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(float+ 1.0 2)")

    def test_float_ops_reject_complex(self, aifpl):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(float* 2.0 1j)")


class TestComplexOpsEval:
    """complex+  complex-  complex*  complex/  evaluated end-to-end."""

    @pytest.mark.parametrize("expr,expected", [
        # zero-arg identity
        ("(complex+)", "0j"),
        ("(complex*)", "1+0j"),
        # one-arg identity
        ("(complex+ (complex 3 4))", "3+4j"),
        ("(complex* (complex 2 1))", "2+1j"),
        # unary negation
        ("(complex- (complex 3 4))", "-3-4j"),
        ("(complex- (complex -1 -2))", "1+2j"),
        # reciprocal
        ("(complex/ (complex 2 0))", "0.5+0j"),
        # binary
        ("(complex+ (complex 1 2) (complex 3 4))", "4+6j"),
        ("(complex- (complex 5 3) (complex 2 1))", "3+2j"),
        ("(complex* (complex 1 2) (complex 3 4))", "-5+10j"),
        ("(complex/ (complex 4 2) (complex 1 1))", "3-1j"),
        # variadic fold
        ("(complex+ (complex 1 0) (complex 2 0) (complex 3 0))", "6+0j"),
        ("(complex- (complex 10 0) (complex 3 0) (complex 2 0))", "5+0j"),
        ("(complex* (complex 1 1) (complex 1 1) (complex 1 0))", "2j"),
    ])
    def test_complex_ops(self, aifpl, expr, expected):
        assert aifpl.evaluate_and_format(expr) == expected

    def test_complex_div_by_zero(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="[Dd]ivision by zero"):
            aifpl.evaluate("(complex/ (complex 1 0) (complex 0 0))")

    def test_complex_ops_reject_integer(self, aifpl):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(complex+ (complex 1 2) 3)")

    def test_complex_ops_reject_float(self, aifpl):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(complex* (complex 1 2) 3.0)")


class TestGenericDivReciprocal:
    """Generic / now supports the Lisp reciprocal convention."""

    @pytest.mark.parametrize("expr,expected", [
        ("(/ 4)", "0.25"),
        ("(/ 2)", "0.5"),
        ("(/ 1)", "1.0"),
        ("(/ 5)", "0.2"),
    ])
    def test_generic_div_reciprocal(self, aifpl, expr, expected):
        assert aifpl.evaluate_and_format(expr) == expected


class TestFirstClassUse:
    """Typed operators work as first-class values (fold, map, etc.)."""

    def test_fold_integer_add(self, aifpl):
        result = aifpl.evaluate_and_format("(fold integer+ 0 (list 1 2 3 4 5))")
        assert result == "15"

    def test_fold_float_mul(self, aifpl):
        result = aifpl.evaluate_and_format("(fold float* 1.0 (list 2.0 3.0 4.0))")
        assert result == "24.0"

    def test_map_integer_negate(self, aifpl):
        # integer- as a first-class unary negation via partial application
        result = aifpl.evaluate_and_format(
            "(map (lambda (x) (integer- x)) (list 1 2 3))"
        )
        assert result == "(-1 -2 -3)"

    def test_fold_complex_add(self, aifpl):
        result = aifpl.evaluate_and_format(
            "(fold complex+ (complex 0 0) (list (complex 1 0) (complex 0 1) (complex 1 1)))"
        )
        assert result == "2+2j"


# ---------------------------------------------------------------------------
# 3. Error cases
# ---------------------------------------------------------------------------

class TestZeroArgErrors:
    """Zero-arg subtraction and division are runtime errors."""

    @pytest.mark.parametrize("expr", [
        "(integer-)",
        "(integer/)",
        "(float-)",
        "(float/)",
        "(complex-)",
        "(complex/)",
    ])
    def test_zero_arg_sub_div_errors(self, aifpl, expr):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate(expr)


class TestTypeEnforcement:
    """Typed operators enforce strict type homogeneity."""

    @pytest.mark.parametrize("expr", [
        # integer ops with wrong types
        "(integer+ 1 1.0)",
        "(integer+ 1 1j)",
        "(integer- 1 1.0)",
        "(integer* 1 1j)",
        "(integer/ 4 2.0)",
        # float ops with wrong types
        "(float+ 1.0 1)",
        "(float+ 1.0 1j)",
        "(float- 1.0 1)",
        "(float* 1.0 1j)",
        "(float/ 4.0 2)",
        # complex ops with wrong types
        "(complex+ (complex 1 0) 1)",
        "(complex+ (complex 1 0) 1.0)",
        "(complex- (complex 1 0) 1)",
        "(complex* (complex 1 0) 1.0)",
        "(complex/ (complex 4 0) 2)",
    ])
    def test_type_errors(self, aifpl, expr):
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate(expr)
