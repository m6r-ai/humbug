"""Tests for the #none type in AIFPL."""

import pytest
from aifpl import AIFPL, AIFPLError


@pytest.fixture
def aifpl():
    return AIFPL()


def evaluate(aifpl, expression):
    return aifpl.evaluate_and_format(expression)


# ---------------------------------------------------------------------------
# Literal and predicate
# ---------------------------------------------------------------------------

def test_none_literal(aifpl):
    assert evaluate(aifpl, "#none") == "#none"


def test_none_predicate_on_none(aifpl):
    assert evaluate(aifpl, "(none? #none)") == "#t"


def test_none_predicate_on_false(aifpl):
    assert evaluate(aifpl, "(none? #f)") == "#f"


def test_none_predicate_on_true(aifpl):
    assert evaluate(aifpl, "(none? #t)") == "#f"


def test_none_predicate_on_zero(aifpl):
    assert evaluate(aifpl, "(none? 0)") == "#f"


def test_none_predicate_on_empty_string(aifpl):
    assert evaluate(aifpl, '(none? "")') == "#f"


def test_none_predicate_on_empty_list(aifpl):
    assert evaluate(aifpl, "(none? (list))") == "#f"


def test_none_predicate_on_string(aifpl):
    assert evaluate(aifpl, '(none? "hello")') == "#f"


# ---------------------------------------------------------------------------
# #none is not a boolean — if condition must be boolean
# ---------------------------------------------------------------------------

def test_none_as_if_condition_is_type_error(aifpl):
    with pytest.raises(AIFPLError):
        evaluate(aifpl, '(if #none "yes" "no")')


# ---------------------------------------------------------------------------
# dict-get returns #none for missing keys (no default)
# ---------------------------------------------------------------------------

def test_dict_get_missing_key_returns_none(aifpl):
    assert evaluate(aifpl, '(dict-get (dict) "x")') == "#none"


def test_dict_get_missing_key_none_predicate(aifpl):
    assert evaluate(aifpl, '(none? (dict-get (dict) "x"))') == "#t"


def test_dict_get_stored_false_returns_false(aifpl):
    """Stored #f must be distinguishable from absent key."""
    assert evaluate(aifpl, '(dict-get (dict (list "x" #f)) "x")') == "#f"


def test_dict_get_stored_none_returns_none(aifpl):
    """#none can be stored as a value and retrieved."""
    assert evaluate(aifpl, '(dict-get (dict (list "x" #none)) "x")') == "#none"


def test_dict_get_with_default_still_works(aifpl):
    """Explicit default form is unchanged."""
    assert evaluate(aifpl, '(dict-get (dict) "x" "default")') == '"default"'


def test_dict_get_with_false_default(aifpl):
    """Explicit #f default is returned when key missing."""
    assert evaluate(aifpl, '(dict-get (dict) "x" #f)') == "#f"


def test_dict_get_existing_key_with_default(aifpl):
    """When key exists, value is returned not default."""
    assert evaluate(aifpl, '(dict-get (dict (list "x" 42)) "x" 0)') == "42"


# ---------------------------------------------------------------------------
# list-find returns #none when not found
# ---------------------------------------------------------------------------

def test_list_find_not_found_returns_none(aifpl):
    assert evaluate(aifpl, "(list-find (lambda (x) (integer>? x 10)) (list 1 2 3))") == "#none"


def test_list_find_empty_list_returns_none(aifpl):
    assert evaluate(aifpl, "(list-find (lambda (x) #t) (list))") == "#none"


def test_list_find_found_returns_element(aifpl):
    assert evaluate(aifpl, "(list-find (lambda (x) (integer>? x 3)) (list 1 2 3 4 5))") == "4"


def test_list_find_none_predicate(aifpl):
    assert evaluate(aifpl, "(none? (list-find (lambda (x) (integer>? x 10)) (list 1 2 3)))") == "#t"


# ---------------------------------------------------------------------------
# list-index returns #none when not found
# ---------------------------------------------------------------------------

def test_list_index_not_found_returns_none(aifpl):
    assert evaluate(aifpl, "(list-index (list 1 2 3) 99)") == "#none"


def test_list_index_found_returns_integer(aifpl):
    assert evaluate(aifpl, "(list-index (list 1 2 3) 2)") == "1"


def test_list_index_none_predicate(aifpl):
    assert evaluate(aifpl, "(none? (list-index (list 1 2 3) 99))") == "#t"


# ---------------------------------------------------------------------------
# string-index returns #none when not found
# ---------------------------------------------------------------------------

def test_string_index_not_found_returns_none(aifpl):
    assert evaluate(aifpl, '(string-index "hello" "z")') == "#none"


def test_string_index_found_returns_integer(aifpl):
    assert evaluate(aifpl, '(string-index "hello" "l")') == "2"


def test_string_index_none_predicate(aifpl):
    assert evaluate(aifpl, '(none? (string-index "hello" "z"))') == "#t"


# ---------------------------------------------------------------------------
# string->number returns #none when unparseable
# ---------------------------------------------------------------------------

def test_string_to_number_unparseable_returns_none(aifpl):
    assert evaluate(aifpl, '(string->number "hello")') == "#none"


def test_string_to_number_valid_integer(aifpl):
    assert evaluate(aifpl, '(string->number "42")') == "42"


def test_string_to_number_valid_float(aifpl):
    assert evaluate(aifpl, '(string->number "3.14")') == "3.14"


def test_string_to_number_none_predicate(aifpl):
    assert evaluate(aifpl, '(none? (string->number "hello"))') == "#t"


# ---------------------------------------------------------------------------
# string->integer returns #none when unparseable
# ---------------------------------------------------------------------------

def test_string_to_integer_unparseable_returns_none(aifpl):
    assert evaluate(aifpl, '(string->integer "xyz")') == "#none"


def test_string_to_integer_valid(aifpl):
    assert evaluate(aifpl, '(string->integer "42")') == "42"


def test_string_to_integer_hex(aifpl):
    assert evaluate(aifpl, '(string->integer "ff" 16)') == "255"


def test_string_to_integer_none_predicate(aifpl):
    assert evaluate(aifpl, '(none? (string->integer "xyz"))') == "#t"


# ---------------------------------------------------------------------------
# Pattern matching on #none
# ---------------------------------------------------------------------------

def test_match_none_literal_matches(aifpl):
    assert evaluate(aifpl, '(match #none (#none "yes") (_ "no"))') == '"yes"'


def test_match_false_does_not_match_none(aifpl):
    """#f and #none are distinct — critical disambiguation test."""
    assert evaluate(aifpl, '(match #f (#none "none") (#f "false") (_ "other"))') == '"false"'


def test_match_none_does_not_match_false(aifpl):
    assert evaluate(aifpl, '(match #none (#f "false") (#none "none") (_ "other"))') == '"none"'


def test_match_none_from_dict_get(aifpl):
    assert evaluate(aifpl, '(match (dict-get (dict) "k") (#none "missing") (_ "found"))') == '"missing"'


def test_match_value_from_dict_get(aifpl):
    assert evaluate(aifpl, '(match (dict-get (dict (list "k" 42)) "k") (#none "missing") ((? integer? n) n))') == "42"


# ---------------------------------------------------------------------------
# #none as a stored value in collections
# ---------------------------------------------------------------------------

def test_none_stored_in_list(aifpl):
    assert evaluate(aifpl, "(list-ref (list 1 #none 3) 1)") == "#none"


def test_none_stored_in_dict_value(aifpl):
    assert evaluate(aifpl, '(dict-get (dict (list "k" #none)) "k")') == "#none"


def test_none_in_list_member(aifpl):
    assert evaluate(aifpl, "(list-member? (list 1 #none 3) #none)") == "#t"


def test_none_not_confused_with_false_in_list(aifpl):
    assert evaluate(aifpl, "(list-member? (list 1 #f 3) #none)") == "#f"
