"""Tests for symbol operations: symbol?, symbol=?, symbol!=?, symbol->string.

Symbols are produced only by quote and are a distinct runtime type.
"""

import pytest

from aifpl import AIFPL, AIFPLEvalError


@pytest.fixture
def aifpl():
    return AIFPL()


# ---------------------------------------------------------------------------
# symbol? predicate
# ---------------------------------------------------------------------------

class TestSymbolP:
    def test_quoted_symbol_is_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol? 'foo)") is True

    def test_symbol_in_quoted_list(self, aifpl):
        assert aifpl.evaluate("(symbol? (list-first '(a b c)))") is True

    def test_string_is_not_symbol(self, aifpl):
        assert aifpl.evaluate('(symbol? "foo")') is False

    def test_integer_is_not_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol? 42)") is False

    def test_boolean_is_not_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol? #t)") is False

    def test_list_is_not_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol? (list 1 2 3))") is False

    def test_float_is_not_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol? 3.14)") is False

    def test_empty_list_is_not_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol? ())") is False

    def test_returns_boolean(self, aifpl):
        assert aifpl.evaluate("(boolean? (symbol? 'x))") is True

    def test_all_elements_of_quoted_list_are_symbols(self, aifpl):
        assert aifpl.evaluate("(list-all? symbol? '(a b c))") is True

    def test_quoted_list_itself_is_not_symbol(self, aifpl):
        # The list '(a b) is a list, not a symbol
        assert aifpl.evaluate("(symbol? '(a b))") is False


# ---------------------------------------------------------------------------
# symbol=? predicate
# ---------------------------------------------------------------------------

class TestSymbolEqP:
    def test_same_symbol_equal(self, aifpl):
        assert aifpl.evaluate("(symbol=? 'foo 'foo)") is True

    def test_different_symbols_not_equal(self, aifpl):
        assert aifpl.evaluate("(symbol=? 'foo 'bar)") is False

    def test_case_sensitive(self, aifpl):
        assert aifpl.evaluate("(symbol=? 'foo 'Foo)") is False
        assert aifpl.evaluate("(symbol=? 'FOO 'foo)") is False

    def test_single_char_symbols(self, aifpl):
        assert aifpl.evaluate("(symbol=? 'a 'a)") is True
        assert aifpl.evaluate("(symbol=? 'a 'b)") is False

    def test_symbols_from_quoted_list(self, aifpl):
        assert aifpl.evaluate(
            "(let ((lst '(x y z)))"
            "  (symbol=? (list-first lst) 'x))"
        ) is True

    def test_returns_boolean(self, aifpl):
        assert aifpl.evaluate("(boolean? (symbol=? 'a 'a))") is True

    def test_first_arg_non_symbol_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol=\\?.*must be symbols"):
            aifpl.evaluate('(symbol=? "foo" \'foo)')

    def test_second_arg_non_symbol_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol=\\?.*must be symbols"):
            aifpl.evaluate("(symbol=? 'foo 42)")

    def test_both_args_non_symbol_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol=\\?.*must be symbols"):
            aifpl.evaluate('(symbol=? "foo" "foo")')

    def test_wrong_arity_zero(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol=?)")

    def test_wrong_arity_one(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol=? 'foo)")

    def test_wrong_arity_three(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol=? 'foo 'foo 'foo)")


# ---------------------------------------------------------------------------
# symbol!=? predicate
# ---------------------------------------------------------------------------

class TestSymbolNeqP:
    def test_different_symbols_not_equal(self, aifpl):
        assert aifpl.evaluate("(symbol!=? 'foo 'bar)") is True

    def test_same_symbol_not_unequal(self, aifpl):
        assert aifpl.evaluate("(symbol!=? 'foo 'foo)") is False

    def test_case_difference_is_unequal(self, aifpl):
        assert aifpl.evaluate("(symbol!=? 'foo 'Foo)") is True

    def test_returns_boolean(self, aifpl):
        assert aifpl.evaluate("(boolean? (symbol!=? 'a 'b))") is True

    def test_neq_is_inverse_of_eq(self, aifpl):
        # symbol!=? should always be the inverse of symbol=?
        assert aifpl.evaluate(
            "(boolean=? (symbol!=? 'foo 'bar) (boolean-not (symbol=? 'foo 'bar)))"
        ) is True
        assert aifpl.evaluate(
            "(boolean=? (symbol!=? 'foo 'foo) (boolean-not (symbol=? 'foo 'foo)))"
        ) is True

    def test_first_arg_non_symbol_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol!=\\?.*must be symbols"):
            aifpl.evaluate("(symbol!=? 42 'foo)")

    def test_second_arg_non_symbol_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol!=\\?.*must be symbols"):
            aifpl.evaluate('(symbol!=? \'foo "bar")')

    def test_wrong_arity_zero(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol!=?)")

    def test_wrong_arity_one(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol!=? 'foo)")

    def test_wrong_arity_three(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol!=? 'foo 'bar 'baz)")


# ---------------------------------------------------------------------------
# symbol->string conversion
# ---------------------------------------------------------------------------

class TestSymbolToString:
    def test_basic_conversion(self, aifpl):
        assert aifpl.evaluate("(symbol->string 'foo)") == "foo"

    def test_multi_char_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol->string 'hello-world)") == "hello-world"

    def test_single_char_symbol(self, aifpl):
        assert aifpl.evaluate("(symbol->string 'x)") == "x"

    def test_symbol_with_special_chars(self, aifpl):
        assert aifpl.evaluate("(symbol->string 'integer+)") == "integer+"
        assert aifpl.evaluate("(symbol->string 'list?)") == "list?"
        assert aifpl.evaluate("(symbol->string 'string->number)") == "string->number"

    def test_returns_string(self, aifpl):
        assert aifpl.evaluate("(string? (symbol->string 'foo))") is True

    def test_roundtrip_via_string_ops(self, aifpl):
        # symbol->string produces a real string we can operate on
        assert aifpl.evaluate("(string-length (symbol->string 'hello))") == 5
        assert aifpl.evaluate(
            '(string=? (symbol->string \'foo) "foo")'
        ) is True

    def test_map_over_quoted_list(self, aifpl):
        assert aifpl.evaluate(
            "(list-map symbol->string '(foo bar baz))"
        ) == ["foo", "bar", "baz"]

    def test_non_symbol_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol->string.*must be a symbol"):
            aifpl.evaluate('(symbol->string "foo")')

    def test_integer_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol->string.*must be a symbol"):
            aifpl.evaluate("(symbol->string 42)")

    def test_list_raises(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="symbol->string.*must be a symbol"):
            aifpl.evaluate("(symbol->string (list 1 2))")

    def test_wrong_arity_zero(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol->string)")

    def test_wrong_arity_two(self, aifpl):
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            aifpl.evaluate("(symbol->string 'foo 'bar)")


# ---------------------------------------------------------------------------
# symbol? in match patterns
# ---------------------------------------------------------------------------

class TestSymbolMatchPattern:
    def test_match_symbol_type_pattern(self, aifpl):
        assert aifpl.evaluate(
            "(match 'hello ((? symbol? s) \"got a symbol\") (_ \"other\"))"
        ) == "got a symbol"

    def test_match_non_symbol_falls_through(self, aifpl):
        assert aifpl.evaluate(
            '(match "hello" ((? symbol? s) "got a symbol") (_ "other"))'
        ) == "other"

    def test_match_symbol_binds_variable(self, aifpl):
        assert aifpl.evaluate(
            "(match 'foo ((? symbol? s) (symbol->string s)) (_ \"none\"))"
        ) == "foo"

    def test_match_symbol_in_list_processing(self, aifpl):
        # Classify elements of a mixed quoted list
        # Build with explicit list/quote to avoid parser issues with mixed quoted lists
        result = aifpl.evaluate("""
            (list-map (lambda (x)
                   (match x
                     ((? symbol? s) (string-concat "sym:" (symbol->string s)))
                     ((? integer? n) (string-concat "int:" (integer->string n)))
                     ((? string? s) (string-concat "str:" s))
                     (_ "other")))
                 (list 'foo 42 "hello" 'bar))
        """)
        assert result == ["sym:foo", "int:42", "str:hello", "sym:bar"]


# ---------------------------------------------------------------------------
# Integration: homoiconic code inspection
# ---------------------------------------------------------------------------

class TestSymbolIntegration:
    def test_extract_operator_from_quoted_expr(self, aifpl):
        # Inspect a quoted expression: (integer+ 1 2) → operator name is "integer+"
        assert aifpl.evaluate(
            "(symbol->string (list-first '(integer+ 1 2)))"
        ) == "integer+"

    def test_filter_symbols_from_mixed_list(self, aifpl):
        result = aifpl.evaluate(
            "(list-filter symbol? '(foo 1 bar 2 baz))"
        )
        # Result is a list of symbols — check their string names
        assert aifpl.evaluate(
            "(list-map symbol->string (list-filter symbol? '(foo 1 bar 2 baz)))"
        ) == ["foo", "bar", "baz"]

    def test_symbol_equality_in_filter(self, aifpl):
        # Keep only elements equal to 'x
        result = aifpl.evaluate(
            "(list-length (list-filter (lambda (s) (symbol=? s 'x)) '(x y x z x)))"
        )
        assert result == 3

    def test_symbol_as_alist_key(self, aifpl):
        # Symbols can be used as alist keys
        assert aifpl.evaluate(
            "(let ((a (alist (list 'foo 1) (list 'bar 2))))"
            "  (alist-get a 'foo))"
        ) == 1

    def test_symbol_alist_key_lookup_miss(self, aifpl):
        assert aifpl.evaluate(
            "(let ((a (alist (list 'foo 1))))"
            "  (alist-get a 'bar \"missing\"))"
        ) == "missing"

    def test_collect_symbol_names_from_quoted_expr(self, aifpl):
        # Extract symbol names — avoid reserved keywords like 'let' in quoted list
        result = aifpl.evaluate("""
            (list-map symbol->string
                 (list-filter symbol? (list 'alpha 1 'beta 2)))
        """)
        assert result == ["alpha", "beta"]
