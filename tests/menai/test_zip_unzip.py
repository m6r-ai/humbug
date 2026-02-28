"""Tests for zip higher-order function."""

import pytest

from menai import MenaiEvalError


class TestZip:
    """Tests for the zip function."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic zip
        ('(zip-list (list 1 2 3) (list 4 5 6))', '((1 4) (2 5) (3 6))'),

        # Zip with strings
        ('(zip-list (list "a" "b" "c") (list 1 2 3))', '(("a" 1) ("b" 2) ("c" 3))'),

        # Zip with booleans
        ('(zip-list (list #t #f) (list 1 2))', '((#t 1) (#f 2))'),

        # Zip stops at shorter first list
        ('(zip-list (list 1 2) (list 4 5 6))', '((1 4) (2 5))'),

        # Zip stops at shorter second list
        ('(zip-list (list 1 2 3) (list 4 5))', '((1 4) (2 5))'),

        # Zip with empty first list
        ('(zip-list (list) (list 1 2 3))', '()'),

        # Zip with empty second list
        ('(zip-list (list 1 2 3) (list))', '()'),

        # Zip with both empty
        ('(zip-list (list) (list))', '()'),

        # Zip with single element lists
        ('(zip-list (list 1) (list 2))', '((1 2))'),
    ])
    def test_zip_basic(self, menai, expression, expected):
        """Test basic zip behaviour."""
        assert menai.evaluate_and_format(expression) == expected

    def test_zip_arity(self, menai):
        """Test that zip requires exactly 2 arguments."""
        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 1"):
            menai.evaluate('(zip-list (list 1 2 3))')

        with pytest.raises(MenaiEvalError, match=r"expects 2 arguments, got 3"):
            menai.evaluate('(zip-list (list 1 2) (list 3 4) (list 5 6))')

    def test_zip_requires_list_arguments(self, menai):
        """Test that zip requires list arguments."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(zip-list 42 (list 1 2))')

        with pytest.raises(MenaiEvalError):
            menai.evaluate('(zip-list (list 1 2) 42)')

    def test_zip_result_is_list_of_pairs(self, menai):
        """Test that each element of the result is a 2-element list."""
        result = menai.evaluate('(zip-list (list 1 2 3) (list 4 5 6))')
        assert isinstance(result, list)
        assert len(result) == 3
        for pair in result:
            assert isinstance(pair, list)
            assert len(pair) == 2

    def test_zip_with_map(self, menai, helpers):
        """Test zip used with map to process paired elements."""
        # Sum each pair
        helpers.assert_evaluates_to(
            menai,
            '(map-list (lambda (pair) (integer+ (list-first pair) (list-first (list-rest pair)))) (zip-list (list 1 2 3) (list 4 5 6)))',
            '(5 7 9)'
        )

    def test_zip_to_build_dict(self, menai, helpers):
        """Test using zip to build a dict from keys and values."""
        helpers.assert_evaluates_to(
            menai,
            '''(fold-list (lambda (acc pair)
                            (dict-set acc (list-first pair) (list-first (list-rest pair))))
                          (dict)
                          (zip-list (list "a" "b" "c") (list 1 2 3)))''',
            '{("a" 1) ("b" 2) ("c" 3)}'
        )

    def test_zip_is_first_class(self, menai, helpers):
        """Test that zip can be passed as a first-class value."""
        helpers.assert_evaluates_to(
            menai,
            '(function? zip-list)',
            '#t'
        )

    def test_zip_dot_product(self, menai, helpers):
        """Test using zip and fold to compute a dot product."""
        helpers.assert_evaluates_to(
            menai,
            '''(fold-list integer+
                        0
                        (map-list (lambda (pair)
                                    (integer* (list-first pair) (list-first (list-rest pair))))
                                  (zip-list (list 1 2 3) (list 4 5 6))))''',
            '32'  # 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
        )

    def test_zip_extract_firsts_and_lasts(self, menai, helpers):
        """Test extracting elements from zipped pairs using map-list."""
        helpers.assert_evaluates_to(
            menai,
            '(map-list list-first (zip-list (list 1 2 3) (list 4 5 6)))',
            '(1 2 3)'
        )
        helpers.assert_evaluates_to(
            menai,
            '(map-list list-last (zip-list (list 1 2 3) (list 4 5 6)))',
            '(4 5 6)'
        )
