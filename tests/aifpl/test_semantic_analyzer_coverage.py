"""
Tests for previously uncovered branches in AIFPLSemanticAnalyzer.
"""

import pytest

from aifpl import AIFPLEvalError

class TestLetStarTooManyElements:
    """Line 247: let* expression with more than 3 elements."""

    def test_let_star_extra_body_expression(self, aifpl):
        """(let* ((x 1)) expr1 expr2) — 4 elements, triggers 'too many elements'."""
        with pytest.raises(AIFPLEvalError, match="Let\\* expression has too many elements"):
            aifpl.evaluate("(let* ((x 1)) x x)")

    def test_let_star_multiple_extra_elements(self, aifpl):
        """(let* ((x 1)) a b c) — 5 elements."""
        with pytest.raises(AIFPLEvalError, match="Let\\* expression has too many elements"):
            aifpl.evaluate("(let* ((x 1)) x x x)")

    def test_let_star_no_bindings_two_bodies(self, aifpl):
        """(let* () expr1 expr2) — 4 elements."""
        with pytest.raises(AIFPLEvalError, match="Let\\* expression has too many elements"):
            aifpl.evaluate("(let* () 1 2)")


class TestLetStarBindingWrongCount:
    """Line 289: let* binding has != 2 elements."""

    def test_let_star_binding_missing_value(self, aifpl):
        """(let* ((x)) body) — binding has only 1 element."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* has wrong number of elements"):
            aifpl.evaluate("(let* ((x)) x)")

    def test_let_star_binding_too_many_values(self, aifpl):
        """(let* ((x 1 2)) body) — binding has 3 elements."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* has wrong number of elements"):
            aifpl.evaluate("(let* ((x 1 2)) x)")

    def test_let_star_empty_binding(self, aifpl):
        """(let* (()) body) — binding has 0 elements."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* has wrong number of elements"):
            aifpl.evaluate("(let* (()) 42)")

    def test_let_star_second_binding_wrong_count(self, aifpl):
        """Error reported for the second binding when first is fine."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* has wrong number of elements"):
            aifpl.evaluate("(let* ((x 1) (y)) (+ x y))")


class TestLetStarBindingVarNotSymbol:
    """Line 304: let* binding variable must be a symbol."""

    def test_let_star_binding_integer_variable(self, aifpl):
        """(let* ((1 5)) 1) — integer as binding variable."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* variable must be a symbol"):
            aifpl.evaluate("(let* ((1 5)) 1)")

    def test_let_star_binding_string_variable(self, aifpl):
        """(let* (("x" 5)) x) — string as binding variable."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* variable must be a symbol"):
            aifpl.evaluate('(let* (("x" 5)) "x")')

    def test_let_star_binding_boolean_variable(self, aifpl):
        """(let* ((#t 5)) #t) — boolean as binding variable."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* variable must be a symbol"):
            aifpl.evaluate("(let* ((#t 5)) #t)")

    def test_let_star_second_binding_non_symbol_variable(self, aifpl):
        """Error on the second binding's variable."""
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* variable must be a symbol"):
            aifpl.evaluate('(let* ((x 1) ("y" 2)) x)')


class TestLetrecTooFewElements:
    """Line 333: letrec expression with fewer than 3 elements."""

    def test_letrec_no_bindings_no_body(self, aifpl):
        """(letrec) — only the keyword, 1 element."""
        with pytest.raises(AIFPLEvalError, match="Letrec expression structure is incorrect"):
            aifpl.evaluate("(letrec)")

    def test_letrec_bindings_but_no_body(self, aifpl):
        """(letrec ()) — keyword + empty bindings, 2 elements, no body."""
        with pytest.raises(AIFPLEvalError, match="Letrec expression structure is incorrect"):
            aifpl.evaluate("(letrec ())")


class TestLetrecBindingsNotList:
    """Line 359: letrec binding list must be a list."""

    def test_letrec_symbol_as_bindings(self, aifpl):
        """(letrec x body) — symbol in bindings position."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding list must be a list"):
            aifpl.evaluate("(letrec x 42)")

    def test_letrec_integer_as_bindings(self, aifpl):
        """(letrec 5 body) — integer in bindings position."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding list must be a list"):
            aifpl.evaluate("(letrec 5 42)")

    def test_letrec_string_as_bindings(self, aifpl):
        """(letrec "x" body) — string in bindings position."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding list must be a list"):
            aifpl.evaluate('(letrec "x" 42)')


class TestLetrecIndividualBindingNotList:
    """Line 374: each letrec binding must be a list."""

    def test_letrec_bare_symbol_as_binding(self, aifpl):
        """(letrec (x 5) body) — 'x' is a symbol, not a (var val) list."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* must be a list"):
            aifpl.evaluate("(letrec (x 5) x)")

    def test_letrec_bare_integer_as_binding(self, aifpl):
        """(letrec (42) body) — integer as a binding slot."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* must be a list"):
            aifpl.evaluate("(letrec (42) 42)")

    def test_letrec_bare_string_as_binding(self, aifpl):
        """(letrec ("x") body) — string as a binding slot."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* must be a list"):
            aifpl.evaluate('(letrec ("x") "x")')

    def test_letrec_second_binding_not_list(self, aifpl):
        """Error reported for the second binding slot."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* must be a list"):
            aifpl.evaluate("(letrec ((x 1) y) x)")


class TestLetrecBindingWrongCount:
    """Line 386: each letrec binding must have exactly 2 elements."""

    def test_letrec_binding_missing_value(self, aifpl):
        """(letrec ((x)) body) — binding has only 1 element."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* has wrong number of elements"):
            aifpl.evaluate("(letrec ((x)) x)")

    def test_letrec_binding_too_many_values(self, aifpl):
        """(letrec ((x 1 2)) body) — binding has 3 elements."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* has wrong number of elements"):
            aifpl.evaluate("(letrec ((x 1 2)) x)")

    def test_letrec_empty_binding(self, aifpl):
        """(letrec (()) body) — binding has 0 elements."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* has wrong number of elements"):
            aifpl.evaluate("(letrec (()) 42)")

    def test_letrec_second_binding_wrong_count(self, aifpl):
        """Error reported for the second binding."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* has wrong number of elements"):
            aifpl.evaluate("(letrec ((x 1) (y)) x)")


class TestLetrecBindingVarNotSymbol:
    """Line 400: letrec binding variable must be a symbol."""

    def test_letrec_binding_integer_variable(self, aifpl):
        """(letrec ((1 5)) 1) — integer as binding variable."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* variable must be a symbol"):
            aifpl.evaluate("(letrec ((1 5)) 1)")

    def test_letrec_binding_string_variable(self, aifpl):
        """(letrec (("x" 5)) "x") — string as binding variable."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* variable must be a symbol"):
            aifpl.evaluate('(letrec (("x" 5)) "x")')

    def test_letrec_binding_boolean_variable(self, aifpl):
        """(letrec ((#t 5)) #t) — boolean as binding variable."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* variable must be a symbol"):
            aifpl.evaluate("(letrec ((#t 5)) #t)")

    def test_letrec_second_binding_non_symbol_variable(self, aifpl):
        """Error on the second binding's variable."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding .* variable must be a symbol"):
            aifpl.evaluate('(letrec ((x 1) (2 3)) x)')


class TestLetrecDuplicateBindings:
    """Line 418: letrec binding variables must be unique."""

    def test_letrec_two_identical_bindings(self, aifpl):
        """(letrec ((x 1) (x 2)) x) — duplicate 'x'."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding variables must be unique"):
            aifpl.evaluate("(letrec ((x 1) (x 2)) x)")

    def test_letrec_three_bindings_one_duplicate(self, aifpl):
        """(letrec ((x 1) (y 2) (x 3)) x) — 'x' appears twice."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding variables must be unique"):
            aifpl.evaluate("(letrec ((x 1) (y 2) (x 3)) x)")

    def test_letrec_all_bindings_same_name(self, aifpl):
        """(letrec ((x 1) (x 2) (x 3)) x) — all three are 'x'."""
        with pytest.raises(AIFPLEvalError, match="Letrec binding variables must be unique"):
            aifpl.evaluate("(letrec ((x 1) (x 2) (x 3)) x)")
