"""Tests for symbol operations: symbol?, symbol=?, symbol!=?, symbol->string.

Symbols are produced only by quote and are a distinct runtime type.
"""

import pytest

from menai import Menai, MenaiEvalError


@pytest.fixture
def menai():
    return Menai()


# ---------------------------------------------------------------------------
# symbol? predicate
# ---------------------------------------------------------------------------

class TestSymbolP:
    def test_quoted_symbol_is_symbol(self, menai):
        assert menai.evaluate("(symbol? 'foo)") is True

    def test_symbol_in_quoted_list(self, menai):
        assert menai.evaluate("(symbol? (list-first '(a b c)))") is True

    def test_string_is_not_symbol(self, menai):
        assert menai.evaluate('(symbol? "foo")') is False

    def test_integer_is_not_symbol(self, menai):
        assert menai.evaluate("(symbol? 42)") is False

    def test_boolean_is_not_symbol(self, menai):
        assert menai.evaluate("(symbol? #t)") is False

    def test_list_is_not_symbol(self, menai):
        assert menai.evaluate("(symbol? (list 1 2 3))") is False

    def test_float_is_not_symbol(self, menai):
        assert menai.evaluate("(symbol? 3.14)") is False

    def test_empty_list_is_not_symbol(self, menai):
        assert menai.evaluate("(symbol? ())") is False

    def test_returns_boolean(self, menai):
        assert menai.evaluate("(boolean? (symbol? 'x))") is True

    def test_all_elements_of_quoted_list_are_symbols(self, menai):
        assert menai.evaluate("(all-list? symbol? '(a b c))") is True

    def test_quoted_list_itself_is_not_symbol(self, menai):
        # The list '(a b) is a list, not a symbol
        assert menai.evaluate("(symbol? '(a b))") is False


# ---------------------------------------------------------------------------
# symbol=? predicate
# ---------------------------------------------------------------------------

class TestSymbolEqP:
    def test_same_symbol_equal(self, menai):
        assert menai.evaluate("(symbol=? 'foo 'foo)") is True

    def test_different_symbols_not_equal(self, menai):
        assert menai.evaluate("(symbol=? 'foo 'bar)") is False

    def test_case_sensitive(self, menai):
        assert menai.evaluate("(symbol=? 'foo 'Foo)") is False
        assert menai.evaluate("(symbol=? 'FOO 'foo)") is False

    def test_single_char_symbols(self, menai):
        assert menai.evaluate("(symbol=? 'a 'a)") is True
        assert menai.evaluate("(symbol=? 'a 'b)") is False

    def test_symbols_from_quoted_list(self, menai):
        assert menai.evaluate(
            "(let ((lst '(x y z)))"
            "  (symbol=? (list-first lst) 'x))"
        ) is True

    def test_returns_boolean(self, menai):
        assert menai.evaluate("(boolean? (symbol=? 'a 'a))") is True

    def test_first_arg_non_symbol_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol=\\?.*must be symbols"):
            menai.evaluate('(symbol=? "foo" \'foo)')

    def test_second_arg_non_symbol_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol=\\?.*must be symbols"):
            menai.evaluate("(symbol=? 'foo 42)")

    def test_both_args_non_symbol_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol=\\?.*must be symbols"):
            menai.evaluate('(symbol=? "foo" "foo")')

    def test_wrong_arity_zero(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol=?)")

    def test_wrong_arity_one(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol=? 'foo)")

    def test_wrong_arity_three(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol=? 'foo 'foo 'foo)")


# ---------------------------------------------------------------------------
# symbol!=? predicate
# ---------------------------------------------------------------------------

class TestSymbolNeqP:
    def test_different_symbols_not_equal(self, menai):
        assert menai.evaluate("(symbol!=? 'foo 'bar)") is True

    def test_same_symbol_not_unequal(self, menai):
        assert menai.evaluate("(symbol!=? 'foo 'foo)") is False

    def test_case_difference_is_unequal(self, menai):
        assert menai.evaluate("(symbol!=? 'foo 'Foo)") is True

    def test_returns_boolean(self, menai):
        assert menai.evaluate("(boolean? (symbol!=? 'a 'b))") is True

    def test_neq_is_inverse_of_eq(self, menai):
        # symbol!=? should always be the inverse of symbol=?
        assert menai.evaluate(
            "(boolean=? (symbol!=? 'foo 'bar) (boolean-not (symbol=? 'foo 'bar)))"
        ) is True
        assert menai.evaluate(
            "(boolean=? (symbol!=? 'foo 'foo) (boolean-not (symbol=? 'foo 'foo)))"
        ) is True

    def test_first_arg_non_symbol_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol!=\\?.*must be symbols"):
            menai.evaluate("(symbol!=? 42 'foo)")

    def test_second_arg_non_symbol_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol!=\\?.*must be symbols"):
            menai.evaluate('(symbol!=? \'foo "bar")')

    def test_wrong_arity_zero(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol!=?)")

    def test_wrong_arity_one(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol!=? 'foo)")

    def test_wrong_arity_three(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol!=? 'foo 'bar 'baz)")


# ---------------------------------------------------------------------------
# symbol->string conversion
# ---------------------------------------------------------------------------

class TestSymbolToString:
    def test_basic_conversion(self, menai):
        assert menai.evaluate("(symbol->string 'foo)") == "foo"

    def test_multi_char_symbol(self, menai):
        assert menai.evaluate("(symbol->string 'hello-world)") == "hello-world"

    def test_single_char_symbol(self, menai):
        assert menai.evaluate("(symbol->string 'x)") == "x"

    def test_symbol_with_special_chars(self, menai):
        assert menai.evaluate("(symbol->string 'integer+)") == "integer+"
        assert menai.evaluate("(symbol->string 'list?)") == "list?"
        assert menai.evaluate("(symbol->string 'string->number)") == "string->number"

    def test_returns_string(self, menai):
        assert menai.evaluate("(string? (symbol->string 'foo))") is True

    def test_roundtrip_via_string_ops(self, menai):
        # symbol->string produces a real string we can operate on
        assert menai.evaluate("(string-length (symbol->string 'hello))") == 5
        assert menai.evaluate(
            '(string=? (symbol->string \'foo) "foo")'
        ) is True

    def test_map_over_quoted_list(self, menai):
        assert menai.evaluate(
            "(map-list symbol->string '(foo bar baz))"
        ) == ["foo", "bar", "baz"]

    def test_non_symbol_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol->string.*must be a symbol"):
            menai.evaluate('(symbol->string "foo")')

    def test_integer_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol->string.*must be a symbol"):
            menai.evaluate("(symbol->string 42)")

    def test_list_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="symbol->string.*must be a symbol"):
            menai.evaluate("(symbol->string (list 1 2))")

    def test_wrong_arity_zero(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol->string)")

    def test_wrong_arity_two(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate("(symbol->string 'foo 'bar)")


# ---------------------------------------------------------------------------
# symbol? in match patterns
# ---------------------------------------------------------------------------

class TestSymbolMatchPattern:
    def test_match_symbol_type_pattern(self, menai):
        assert menai.evaluate(
            "(match 'hello ((? symbol? s) \"got a symbol\") (_ \"other\"))"
        ) == "got a symbol"

    def test_match_non_symbol_falls_through(self, menai):
        assert menai.evaluate(
            '(match "hello" ((? symbol? s) "got a symbol") (_ "other"))'
        ) == "other"

    def test_match_symbol_binds_variable(self, menai):
        assert menai.evaluate(
            "(match 'foo ((? symbol? s) (symbol->string s)) (_ \"none\"))"
        ) == "foo"

    def test_match_symbol_in_list_processing(self, menai):
        # Classify elements of a mixed quoted list
        # Build with explicit list/quote to avoid parser issues with mixed quoted lists
        result = menai.evaluate("""
            (map-list (lambda (x)
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
    def test_extract_operator_from_quoted_expr(self, menai):
        # Inspect a quoted expression: (integer+ 1 2) → operator name is "integer+"
        assert menai.evaluate(
            "(symbol->string (list-first '(integer+ 1 2)))"
        ) == "integer+"

    def test_filter_symbols_from_mixed_list(self, menai):
        result = menai.evaluate(
            "(filter-list symbol? '(foo 1 bar 2 baz))"
        )
        # Result is a list of symbols — check their string names
        assert menai.evaluate(
            "(map-list symbol->string (filter-list symbol? '(foo 1 bar 2 baz)))"
        ) == ["foo", "bar", "baz"]

    def test_symbol_equality_in_filter(self, menai):
        # Keep only elements equal to 'x
        result = menai.evaluate(
            "(list-length (filter-list (lambda (s) (symbol=? s 'x)) '(x y x z x)))"
        )
        assert result == 3

    def test_symbol_as_dict_key(self, menai):
        # Symbols can be used as dict keys
        assert menai.evaluate(
            "(let ((a (dict (list 'foo 1) (list 'bar 2))))"
            "  (dict-get a 'foo))"
        ) == 1

    def test_symbol_dict_key_lookup_miss(self, menai):
        assert menai.evaluate(
            "(let ((a (dict (list 'foo 1))))"
            "  (dict-get a 'bar \"missing\"))"
        ) == "missing"

    def test_collect_symbol_names_from_quoted_expr(self, menai):
        # Extract symbol names — avoid reserved keywords like 'let' in quoted list
        result = menai.evaluate("""
            (map-list symbol->string
                 (filter-list symbol? (list 'alpha 1 'beta 2)))
        """)
        assert result == ["alpha", "beta"]
