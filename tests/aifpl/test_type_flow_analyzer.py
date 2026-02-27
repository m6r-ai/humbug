"""
Tests for AIFPLTypeFlowAnalyzer.

Two categories:

  Correctness tests — the optimizer must never change semantics.
    All existing tests implicitly cover this; these tests focus on cases
    that are most likely to be broken by an unsound optimization.

  Optimization tests — verify that redundant type-check opcodes are
    eliminated when the type is statically known.  We inspect the compiled
    bytecode directly, counting occurrences of type-check opcodes
    (NONE_P, BOOLEAN_P, INTEGER_P, FLOAT_P, COMPLEX_P, STRING_P, LIST_P,
    DICT_P, FUNCTION_P, SYMBOL_P).
"""

import pytest

from aifpl import AIFPL, AIFPLError
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_bytecode import Opcode


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

@pytest.fixture
def aifpl():
    return AIFPL()


def ev(aifpl_instance, expr: str) -> str:
    return aifpl_instance.evaluate_and_format(expr)


def compile_expr(source: str):
    """Compile *source* and return the top-level CodeObject."""
    compiler = AIFPLCompiler(optimize=True)
    return compiler.compile(source)


def count_opcodes(code, *opcodes: Opcode) -> int:
    """
    Count occurrences of any of *opcodes* in *code* and all nested CodeObjects
    reachable from it (e.g. lambda bodies).
    """
    target = set(opcodes)
    total = 0
    seen = set()
    stack = [code]
    while stack:
        co = stack.pop()
        co_id = id(co)
        if co_id in seen:
            continue
        seen.add(co_id)
        for instr in co.instructions:
            if instr.opcode in target:
                total += 1
        # Recurse into nested CodeObjects (lambda bodies, etc.)
        for nested in co.code_objects:
            stack.append(nested)
    return total


# Type-check opcodes emitted by the VM for type predicates
TYPE_CHECK_OPCODES = (
    Opcode.NONE_P,
    Opcode.BOOLEAN_P,
    Opcode.INTEGER_P,
    Opcode.FLOAT_P,
    Opcode.COMPLEX_P,
    Opcode.STRING_P,
    Opcode.LIST_P,
    Opcode.DICT_P,
    Opcode.FUNCTION_P,
    Opcode.SYMBOL_P,
)


# ---------------------------------------------------------------------------
# Correctness tests
# ---------------------------------------------------------------------------

class TestCorrectnessLiteralMatch:
    """Match on literal values — semantics must be preserved."""

    def test_boolean_match_true(self, aifpl):
        assert ev(aifpl, '(match #t (#f "false") (#t "true"))') == '"true"'

    def test_boolean_match_false(self, aifpl):
        assert ev(aifpl, '(match #f (#t "true") (#f "false"))') == '"false"'

    def test_integer_match(self, aifpl):
        assert ev(aifpl, '(match 42 (41 "wrong") (42 "right"))') == '"right"'

    def test_float_match(self, aifpl):
        assert ev(aifpl, '(match 3.14 (2.71 "e") (3.14 "pi"))') == '"pi"'

    def test_string_match(self, aifpl):
        assert ev(aifpl, '(match "hello" ("world" "world") ("hello" "hello"))') == '"hello"'

    def test_none_match(self, aifpl):
        assert ev(aifpl, '(match #none (#none "none") (_ "other"))') == '"none"'

    def test_wildcard_fallthrough(self, aifpl):
        assert ev(aifpl, '(match 99 (1 "one") (2 "two") (_ "other"))') == '"other"'

    def test_no_match_raises(self, aifpl):
        with pytest.raises(AIFPLError):
            ev(aifpl, '(match 99 (1 "one") (2 "two"))')


class TestCorrectnessLetBound:
    """Match on let-bound variables — type must be inferred from binding."""

    def test_let_bound_integer(self, aifpl):
        assert ev(aifpl, '(let ((x 42)) (match x (42 "yes") (_ "no")))') == '"yes"'

    def test_let_bound_boolean(self, aifpl):
        assert ev(aifpl, '(let ((x #f)) (match x (#t "true") (#f "false")))') == '"false"'

    def test_let_bound_none(self, aifpl):
        assert ev(aifpl, '(let ((x #none)) (match x (#none "none") (_ "other")))') == '"none"'

    def test_let_bound_string(self, aifpl):
        assert ev(aifpl, '(let ((x "hi")) (match x ("hi" "yes") (_ "no")))') == '"yes"'

    def test_let_bound_float(self, aifpl):
        assert ev(aifpl, '(let ((x 1.5)) (match x (1.5 "yes") (_ "no")))') == '"yes"'


class TestCrossTypeCorrectness:
    """Cross-type matches must fall through, not raise type errors."""

    def test_integer_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match 42 (#f "false") (_ "other"))') == '"other"'

    def test_float_vs_integer_arm(self, aifpl):
        assert ev(aifpl, '(match 1.0 (1 "one") (_ "other"))') == '"other"'

    def test_string_vs_integer_arm(self, aifpl):
        assert ev(aifpl, '(match "1" (1 "one") (_ "other"))') == '"other"'

    def test_none_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match #none (#f "false") (#none "none") (_ "other"))') == '"none"'

    def test_mixed_type_arms_integer(self, aifpl):
        expr = '(match 42 (#f "bool") ("42" "str") (42 "int") (_ "other"))'
        assert ev(aifpl, expr) == '"int"'

    def test_mixed_type_arms_string(self, aifpl):
        expr = '(match "42" (#f "bool") ("42" "str") (42 "int") (_ "other"))'
        assert ev(aifpl, expr) == '"str"'

    def test_mixed_type_arms_bool(self, aifpl):
        expr = '(match #f (#f "bool") ("42" "str") (42 "int") (_ "other"))'
        assert ev(aifpl, expr) == '"bool"'

    def test_cross_type_no_match_raises(self, aifpl):
        with pytest.raises(AIFPLError):
            ev(aifpl, '(match "hello" (1 "one") (2 "two"))')


class TestCorrectnessIfNarrowing:
    """Type narrowing through if conditions must not change semantics."""

    def test_type_predicate_branch_taken(self, aifpl):
        """When type predicate is true, then-branch executes."""
        assert ev(aifpl, '(lambda (x) (if (integer? x) "int" "other"))') != '"other"'

    def test_known_type_predicate_true(self, aifpl):
        """Let-bound integer: (integer? x) folds to #t, then-branch always taken."""
        assert ev(aifpl, '(let ((x 5)) (if (integer? x) "int" "other"))') == '"int"'

    def test_known_type_predicate_false(self, aifpl):
        """Let-bound string: (integer? x) folds to #f, else-branch always taken."""
        assert ev(aifpl, '(let ((x "hi")) (if (integer? x) "int" "other"))') == '"other"'

    def test_known_none_predicate_true(self, aifpl):
        assert ev(aifpl, '(let ((x #none)) (if (none? x) "none" "other"))') == '"none"'

    def test_known_none_predicate_false(self, aifpl):
        assert ev(aifpl, '(let ((x 42)) (if (none? x) "none" "other"))') == '"other"'

    def test_unknown_type_predicate_preserved(self, aifpl):
        """Lambda parameter has unknown type — predicate must NOT be folded."""
        fn = '(lambda (x) (if (integer? x) (integer+ x 1) 0))'
        assert ev(aifpl, f'({fn} 5)') == '6'
        assert ev(aifpl, f'({fn} "s")') == '0'


class TestCorrectnessLetStarAndLetrec:
    """let* and letrec bindings must be handled correctly."""

    def test_let_star_sequential_bindings(self, aifpl):
        assert ev(aifpl, '(let* ((x 1) (y (integer+ x 1))) y)') == '2'

    def test_letrec_recursive(self, aifpl):
        expr = '''(letrec ((fact (lambda (n)
                             (if (integer=? n 0) 1
                               (integer* n (fact (integer- n 1)))))))
                   (fact 5))'''
        assert ev(aifpl, expr) == '120'


class TestCorrectnessNested:
    """Nested match and let expressions."""

    def test_nested_match(self, aifpl):
        expr = '''(let ((x 1) (y "a"))
                   (match x
                     (1 (match y ("a" "1a") (_ "1other")))
                     (_ "other")))'''
        assert ev(aifpl, expr) == '"1a"'

    def test_match_in_lambda(self, aifpl):
        expr = '''(let ((f (lambda (x)
                             (match x
                               (1 "one")
                               (2 "two")
                               (_ "other")))))
                   (f 2))'''
        assert ev(aifpl, expr) == '"two"'


# ---------------------------------------------------------------------------
# Optimization tests — bytecode inspection
# ---------------------------------------------------------------------------

class TestOptimizationLiteralMatch:
    """
    Matching a literal value directly: the constant folder already handles
    this (folds the entire match away), so there should be zero type-check
    opcodes.
    """

    def test_boolean_literal_match_no_type_checks(self):
        code = compile_expr('(match #f (#t "true") (#f "false"))')
        assert count_opcodes(code, *TYPE_CHECK_OPCODES) == 0

    def test_integer_literal_match_no_type_checks(self):
        code = compile_expr('(match 42 (1 "one") (42 "forty-two") (_ "other"))')
        assert count_opcodes(code, *TYPE_CHECK_OPCODES) == 0


class TestOptimizationLetBound:
    """
    When the matched variable is let-bound to a literal, the type flow
    analyzer knows its type and should eliminate the type-guard opcode from
    every matching arm.
    """

    def test_let_bound_integer_no_type_check(self):
        """(let ((x 42)) (match x (42 "yes") (_ "no"))) — INTEGER_P must be absent."""
        code = compile_expr('(let ((x 42)) (match x (42 "yes") (_ "no")))')
        assert count_opcodes(code, Opcode.INTEGER_P) == 0

    def test_let_bound_boolean_no_type_check(self):
        code = compile_expr('(let ((x #f)) (match x (#t "true") (#f "false")))')
        assert count_opcodes(code, Opcode.BOOLEAN_P) == 0

    def test_let_bound_none_no_type_check(self):
        code = compile_expr('(let ((x #none)) (match x (#none "none") (_ "other")))')
        assert count_opcodes(code, Opcode.NONE_P) == 0

    def test_let_bound_string_no_type_check(self):
        code = compile_expr('(let ((x "hi")) (match x ("hi" "yes") (_ "no")))')
        assert count_opcodes(code, Opcode.STRING_P) == 0

    def test_let_bound_float_no_type_check(self):
        code = compile_expr('(let ((x 1.5)) (match x (1.5 "yes") (_ "no")))')
        assert count_opcodes(code, Opcode.FLOAT_P) == 0

    def test_let_bound_if_predicate_folds(self):
        """(let ((x 5)) (if (integer? x) ...)) — INTEGER_P must be absent."""
        code = compile_expr('(let ((x 5)) (if (integer? x) "int" "other"))')
        assert count_opcodes(code, Opcode.INTEGER_P) == 0

    def test_let_bound_none_predicate_folds(self):
        code = compile_expr('(let ((x #none)) (if (none? x) "none" "other"))')
        assert count_opcodes(code, Opcode.NONE_P) == 0


class TestOptimizationUnknownTypePreserved:
    """
    When the type is not statically known, the type-guard opcode MUST be
    present — removing it would be unsound.
    """

    def test_lambda_param_type_check_preserved(self):
        """Lambda parameter has UNKNOWN type — INTEGER_P must be emitted."""
        code = compile_expr('(lambda (x) (if (integer? x) (integer+ x 1) 0))')
        assert count_opcodes(code, Opcode.INTEGER_P) >= 1

    def test_lambda_match_type_check_preserved(self):
        """Match inside a lambda on an unknown-type parameter."""
        code = compile_expr('(lambda (x) (match x (1 "one") (2 "two") (_ "other")))')
        assert count_opcodes(code, Opcode.INTEGER_P) >= 1

    def test_unknown_var_type_check_preserved(self, aifpl):
        """Runtime: lambda with unknown arg still dispatches correctly."""
        fn = '(lambda (x) (match x (#t "bool") (1 "int") (_ "other")))'
        assert ev(aifpl, f'({fn} #t)') == '"bool"'
        assert ev(aifpl, f'({fn} 1)') == '"int"'
        assert ev(aifpl, f'({fn} "s")') == '"other"'


class TestOptimizationBuiltinReturnType:
    """
    When a variable is bound to a builtin call with a known return type,
    the type flow analyzer should infer that type and fold predicates.
    """

    def test_integer_arithmetic_result_type(self):
        """x bound to integer+ result → INTEGER_P folds away."""
        code = compile_expr(
            '(let ((x (integer+ 2 3))) (if (integer? x) "int" "other"))'
        )
        assert count_opcodes(code, Opcode.INTEGER_P) == 0

    def test_string_concat_result_type(self):
        """x bound to string-concat result → STRING_P folds away."""
        code = compile_expr(
            '(let ((x (string-concat "a" "b"))) (if (string? x) "str" "other"))'
        )
        assert count_opcodes(code, Opcode.STRING_P) == 0

    def test_list_result_type(self):
        """x bound to list call → LIST_P folds away."""
        code = compile_expr(
            '(let ((x (list 1 2 3))) (if (list? x) "list" "other"))'
        )
        assert count_opcodes(code, Opcode.LIST_P) == 0
