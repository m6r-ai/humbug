"""Tests for the #none type in Menai."""

import pytest
from menai import Menai, MenaiError


@pytest.fixture
def menai():
    return Menai()


def evaluate(menai, expression):
    return menai.evaluate_and_format(expression)


# ---------------------------------------------------------------------------
# Literal and predicate
# ---------------------------------------------------------------------------

def test_none_literal(menai):
    assert evaluate(menai, "#none") == "#none"


def test_none_predicate_on_none(menai):
    assert evaluate(menai, "(none? #none)") == "#t"


def test_none_predicate_on_false(menai):
    assert evaluate(menai, "(none? #f)") == "#f"


def test_none_predicate_on_true(menai):
    assert evaluate(menai, "(none? #t)") == "#f"


def test_none_predicate_on_zero(menai):
    assert evaluate(menai, "(none? 0)") == "#f"


def test_none_predicate_on_empty_string(menai):
    assert evaluate(menai, '(none? "")') == "#f"


def test_none_predicate_on_empty_list(menai):
    assert evaluate(menai, "(none? (list))") == "#f"


def test_none_predicate_on_string(menai):
    assert evaluate(menai, '(none? "hello")') == "#f"


# ---------------------------------------------------------------------------
# #none is not a boolean — if condition must be boolean
# ---------------------------------------------------------------------------

def test_none_as_if_condition_is_type_error(menai):
    with pytest.raises(MenaiError):
        evaluate(menai, '(if #none "yes" "no")')


# ---------------------------------------------------------------------------
# dict-get returns #none for missing keys (no default)
# ---------------------------------------------------------------------------

def test_dict_get_missing_key_returns_none(menai):
    assert evaluate(menai, '(dict-get (dict) "x")') == "#none"


def test_dict_get_missing_key_none_predicate(menai):
    assert evaluate(menai, '(none? (dict-get (dict) "x"))') == "#t"


def test_dict_get_stored_false_returns_false(menai):
    """Stored #f must be distinguishable from absent key."""
    assert evaluate(menai, '(dict-get (dict (list "x" #f)) "x")') == "#f"


def test_dict_get_stored_none_returns_none(menai):
    """#none can be stored as a value and retrieved."""
    assert evaluate(menai, '(dict-get (dict (list "x" #none)) "x")') == "#none"


def test_dict_get_with_default_still_works(menai):
    """Explicit default form is unchanged."""
    assert evaluate(menai, '(dict-get (dict) "x" "default")') == '"default"'


def test_dict_get_with_false_default(menai):
    """Explicit #f default is returned when key missing."""
    assert evaluate(menai, '(dict-get (dict) "x" #f)') == "#f"


def test_dict_get_existing_key_with_default(menai):
    """When key exists, value is returned not default."""
    assert evaluate(menai, '(dict-get (dict (list "x" 42)) "x" 0)') == "42"


# ---------------------------------------------------------------------------
# list-find returns #none when not found
# ---------------------------------------------------------------------------

def test_list_find_not_found_returns_none(menai):
    assert evaluate(menai, "(list-find (lambda (x) (integer>? x 10)) (list 1 2 3))") == "#none"


def test_list_find_empty_list_returns_none(menai):
    assert evaluate(menai, "(list-find (lambda (x) #t) (list))") == "#none"


def test_list_find_found_returns_element(menai):
    assert evaluate(menai, "(list-find (lambda (x) (integer>? x 3)) (list 1 2 3 4 5))") == "4"


def test_list_find_none_predicate(menai):
    assert evaluate(menai, "(none? (list-find (lambda (x) (integer>? x 10)) (list 1 2 3)))") == "#t"


# ---------------------------------------------------------------------------
# list-index returns #none when not found
# ---------------------------------------------------------------------------

def test_list_index_not_found_returns_none(menai):
    assert evaluate(menai, "(list-index (list 1 2 3) 99)") == "#none"


def test_list_index_found_returns_integer(menai):
    assert evaluate(menai, "(list-index (list 1 2 3) 2)") == "1"


def test_list_index_none_predicate(menai):
    assert evaluate(menai, "(none? (list-index (list 1 2 3) 99))") == "#t"


# ---------------------------------------------------------------------------
# string-index returns #none when not found
# ---------------------------------------------------------------------------

def test_string_index_not_found_returns_none(menai):
    assert evaluate(menai, '(string-index "hello" "z")') == "#none"


def test_string_index_found_returns_integer(menai):
    assert evaluate(menai, '(string-index "hello" "l")') == "2"


def test_string_index_none_predicate(menai):
    assert evaluate(menai, '(none? (string-index "hello" "z"))') == "#t"


# ---------------------------------------------------------------------------
# string->number returns #none when unparseable
# ---------------------------------------------------------------------------

def test_string_to_number_unparseable_returns_none(menai):
    assert evaluate(menai, '(string->number "hello")') == "#none"


def test_string_to_number_valid_integer(menai):
    assert evaluate(menai, '(string->number "42")') == "42"


def test_string_to_number_valid_float(menai):
    assert evaluate(menai, '(string->number "3.14")') == "3.14"


def test_string_to_number_none_predicate(menai):
    assert evaluate(menai, '(none? (string->number "hello"))') == "#t"


# ---------------------------------------------------------------------------
# string->integer returns #none when unparseable
# ---------------------------------------------------------------------------

def test_string_to_integer_unparseable_returns_none(menai):
    assert evaluate(menai, '(string->integer "xyz")') == "#none"


def test_string_to_integer_valid(menai):
    assert evaluate(menai, '(string->integer "42")') == "42"


def test_string_to_integer_hex(menai):
    assert evaluate(menai, '(string->integer "ff" 16)') == "255"


def test_string_to_integer_none_predicate(menai):
    assert evaluate(menai, '(none? (string->integer "xyz"))') == "#t"


# ---------------------------------------------------------------------------
# Pattern matching on #none
# ---------------------------------------------------------------------------

def test_match_none_literal_matches(menai):
    assert evaluate(menai, '(match #none (#none "yes") (_ "no"))') == '"yes"'


def test_match_false_does_not_match_none(menai):
    """#f and #none are distinct — critical disambiguation test."""
    assert evaluate(menai, '(match #f (#none "none") (#f "false") (_ "other"))') == '"false"'


def test_match_none_does_not_match_false(menai):
    assert evaluate(menai, '(match #none (#f "false") (#none "none") (_ "other"))') == '"none"'


def test_match_none_from_dict_get(menai):
    assert evaluate(menai, '(match (dict-get (dict) "k") (#none "missing") (_ "found"))') == '"missing"'


def test_match_value_from_dict_get(menai):
    assert evaluate(menai, '(match (dict-get (dict (list "k" 42)) "k") (#none "missing") ((? integer? n) n))') == "42"


# ---------------------------------------------------------------------------
# #none as a stored value in collections
# ---------------------------------------------------------------------------

def test_none_stored_in_list(menai):
    assert evaluate(menai, "(list-ref (list 1 #none 3) 1)") == "#none"


def test_none_stored_in_dict_value(menai):
    assert evaluate(menai, '(dict-get (dict (list "k" #none)) "k")') == "#none"


def test_none_in_list_member(menai):
    assert evaluate(menai, "(list-member? (list 1 #none 3) #none)") == "#t"


def test_none_not_confused_with_false_in_list(menai):
    assert evaluate(menai, "(list-member? (list 1 #f 3) #none)") == "#f"
