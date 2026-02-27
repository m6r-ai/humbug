"""Tests for cross-type literal pattern matching safety.

Verifies that matching a value of one type against a literal pattern of a
different type correctly falls through (returns #f from the arm test) rather
than raising a type error.  This is the behaviour guaranteed by the desugarer's
type-guard: each literal arm is compiled as (and (type? tmp) (type=? tmp lit))
so the equality check is only reached when the type is already confirmed.
"""

import pytest

from aifpl import AIFPL, AIFPLError


@pytest.fixture
def aifpl():
    return AIFPL()


def ev(aifpl, expr):
    return aifpl.evaluate_and_format(expr)


# ---------------------------------------------------------------------------
# Boolean literal arms against every other type
# ---------------------------------------------------------------------------

class TestBooleanPatternCrossType:
    """Boolean literal (#t / #f) arms must not throw for non-boolean values."""

    def test_integer_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match 42 (#f "false") (_ "other"))') == '"other"'

    def test_float_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match 3.14 (#t "true") (_ "other"))') == '"other"'

    def test_string_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match "hi" (#f "false") (_ "other"))') == '"other"'

    def test_none_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match #none (#f "false") (#none "none") (_ "other"))') == '"none"'

    def test_list_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match (list 1 2) (#t "true") (_ "other"))') == '"other"'

    def test_dict_vs_boolean_arm(self, aifpl):
        assert ev(aifpl, '(match (dict) (#f "false") (_ "other"))') == '"other"'

    def test_boolean_still_matches_same_type(self, aifpl):
        """Sanity check: boolean arms still match correctly."""
        assert ev(aifpl, '(match #t (#f "false") (#t "true"))') == '"true"'
        assert ev(aifpl, '(match #f (#t "true") (#f "false"))') == '"false"'


# ---------------------------------------------------------------------------
# Integer literal arms against every other type
# ---------------------------------------------------------------------------

class TestIntegerPatternCrossType:
    """Integer literal arms must not throw for non-integer values."""

    def test_float_vs_integer_arm(self, aifpl):
        assert ev(aifpl, '(match 1.0 (1 "one") (_ "other"))') == '"other"'

    def test_string_vs_integer_arm(self, aifpl):
        assert ev(aifpl, '(match "1" (1 "one") (_ "other"))') == '"other"'

    def test_boolean_vs_integer_arm(self, aifpl):
        assert ev(aifpl, '(match #t (1 "one") (_ "other"))') == '"other"'

    def test_none_vs_integer_arm(self, aifpl):
        assert ev(aifpl, '(match #none (0 "zero") (_ "other"))') == '"other"'

    def test_list_vs_integer_arm(self, aifpl):
        assert ev(aifpl, '(match (list 1) (1 "one") (_ "other"))') == '"other"'

    def test_integer_still_matches_same_type(self, aifpl):
        assert ev(aifpl, '(match 42 (41 "wrong") (42 "right"))') == '"right"'


# ---------------------------------------------------------------------------
# Float literal arms against every other type
# ---------------------------------------------------------------------------

class TestFloatPatternCrossType:
    """Float literal arms must not throw for non-float values."""

    def test_integer_vs_float_arm(self, aifpl):
        assert ev(aifpl, '(match 1 (1.0 "one float") (_ "other"))') == '"other"'

    def test_string_vs_float_arm(self, aifpl):
        assert ev(aifpl, '(match "3.14" (3.14 "pi") (_ "other"))') == '"other"'

    def test_boolean_vs_float_arm(self, aifpl):
        assert ev(aifpl, '(match #f (0.0 "zero") (_ "other"))') == '"other"'

    def test_none_vs_float_arm(self, aifpl):
        assert ev(aifpl, '(match #none (0.0 "zero") (_ "other"))') == '"other"'

    def test_float_still_matches_same_type(self, aifpl):
        assert ev(aifpl, '(match 3.14 (2.71 "e") (3.14 "pi"))') == '"pi"'


# ---------------------------------------------------------------------------
# String literal arms against every other type
# ---------------------------------------------------------------------------

class TestStringPatternCrossType:
    """String literal arms must not throw for non-string values."""

    def test_integer_vs_string_arm(self, aifpl):
        assert ev(aifpl, '(match 42 ("42" "string") (_ "other"))') == '"other"'

    def test_boolean_vs_string_arm(self, aifpl):
        assert ev(aifpl, '(match #t ("true" "string") (_ "other"))') == '"other"'

    def test_none_vs_string_arm(self, aifpl):
        assert ev(aifpl, '(match #none ("" "empty") (_ "other"))') == '"other"'

    def test_list_vs_string_arm(self, aifpl):
        assert ev(aifpl, '(match (list) ("" "empty") (_ "other"))') == '"other"'

    def test_string_still_matches_same_type(self, aifpl):
        assert ev(aifpl, '(match "hello" ("world" "world") ("hello" "hello"))') == '"hello"'


# ---------------------------------------------------------------------------
# #none literal arms against every other type
# ---------------------------------------------------------------------------

class TestNonePatternCrossType:
    """#none literal arms must not match for any non-none value."""

    def test_integer_vs_none_arm(self, aifpl):
        assert ev(aifpl, '(match 0 (#none "none") (_ "other"))') == '"other"'

    def test_false_vs_none_arm(self, aifpl):
        """#f and #none are distinct — critical disambiguation."""
        assert ev(aifpl, '(match #f (#none "none") (_ "other"))') == '"other"'

    def test_true_vs_none_arm(self, aifpl):
        assert ev(aifpl, '(match #t (#none "none") (_ "other"))') == '"other"'

    def test_empty_string_vs_none_arm(self, aifpl):
        assert ev(aifpl, '(match "" (#none "none") (_ "other"))') == '"other"'

    def test_empty_list_vs_none_arm(self, aifpl):
        assert ev(aifpl, '(match (list) (#none "none") (_ "other"))') == '"other"'

    def test_none_matches_none(self, aifpl):
        assert ev(aifpl, '(match #none (#none "none") (_ "other"))') == '"none"'


# ---------------------------------------------------------------------------
# Mixed-type arms in a single match — the real-world use case
# ---------------------------------------------------------------------------

class TestMixedTypeArms:
    """Match expressions with arms of multiple literal types must dispatch correctly."""

    def test_mixed_literal_arms_integer(self, aifpl):
        expr = '(match 42 (#f "bool") ("42" "str") (42 "int") (_ "other"))'
        assert ev(aifpl, expr) == '"int"'

    def test_mixed_literal_arms_string(self, aifpl):
        expr = '(match "42" (#f "bool") ("42" "str") (42 "int") (_ "other"))'
        assert ev(aifpl, expr) == '"str"'

    def test_mixed_literal_arms_bool(self, aifpl):
        expr = '(match #f (#f "bool") ("42" "str") (42 "int") (_ "other"))'
        assert ev(aifpl, expr) == '"bool"'

    def test_mixed_literal_arms_none(self, aifpl):
        expr = '(match #none (#f "bool") ("42" "str") (42 "int") (#none "none") (_ "other"))'
        assert ev(aifpl, expr) == '"none"'

    def test_mixed_literal_arms_wildcard(self, aifpl):
        expr = '(match 99 (#f "bool") ("42" "str") (42 "int") (#none "none") (_ "other"))'
        assert ev(aifpl, expr) == '"other"'

    def test_none_from_dict_get_in_mixed_match(self, aifpl):
        """Realistic pattern: dict-get result dispatched through mixed-type match."""
        expr = '''(let ((v (dict-get (dict) "key")))
                   (match v
                     (#none "missing")
                     (#f    "false")
                     ((? integer? n) n)
                     (_ "other")))'''
        assert ev(aifpl, expr) == '"missing"'

    def test_false_from_dict_in_mixed_match(self, aifpl):
        """Stored #f must be distinguishable from absent key."""
        expr = '''(let ((v (dict-get (dict (list "key" #f)) "key")))
                   (match v
                     (#none "missing")
                     (#f    "false")
                     ((? integer? n) n)
                     (_ "other")))'''
        assert ev(aifpl, expr) == '"false"'

    def test_value_from_dict_in_mixed_match(self, aifpl):
        expr = '''(let ((v (dict-get (dict (list "key" 42)) "key")))
                   (match v
                     (#none "missing")
                     (#f    "false")
                     ((? integer? n) n)
                     (_ "other")))'''
        assert ev(aifpl, expr) == '42'


# ---------------------------------------------------------------------------
# No-wildcard match with cross-type value — should error (no arm matches)
# ---------------------------------------------------------------------------

class TestNoMatchError:
    """A match with no wildcard arm and no matching literal should raise."""

    def test_no_match_raises(self, aifpl):
        with pytest.raises(AIFPLError):
            ev(aifpl, '(match 99 (1 "one") (2 "two"))')

    def test_cross_type_no_match_raises(self, aifpl):
        """Cross-type mismatch falls through all arms and raises — not a type error."""
        with pytest.raises(AIFPLError):
            ev(aifpl, '(match "hello" (1 "one") (2 "two"))')

    def test_none_no_match_raises(self, aifpl):
        with pytest.raises(AIFPLError):
            ev(aifpl, '(match #none (1 "one") (#f "false"))')
