"""Tests for zip and unzip higher-order functions."""

import pytest

from aifpl import AIFPLEvalError


class TestZip:
    """Tests for the zip function."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic zip
        ('(zip (list 1 2 3) (list 4 5 6))', '((1 4) (2 5) (3 6))'),

        # Zip with strings
        ('(zip (list "a" "b" "c") (list 1 2 3))', '(("a" 1) ("b" 2) ("c" 3))'),

        # Zip with booleans
        ('(zip (list #t #f) (list 1 2))', '((#t 1) (#f 2))'),

        # Zip stops at shorter first list
        ('(zip (list 1 2) (list 4 5 6))', '((1 4) (2 5))'),

        # Zip stops at shorter second list
        ('(zip (list 1 2 3) (list 4 5))', '((1 4) (2 5))'),

        # Zip with empty first list
        ('(zip (list) (list 1 2 3))', '()'),

        # Zip with empty second list
        ('(zip (list 1 2 3) (list))', '()'),

        # Zip with both empty
        ('(zip (list) (list))', '()'),

        # Zip with single element lists
        ('(zip (list 1) (list 2))', '((1 2))'),
    ])
    def test_zip_basic(self, aifpl, expression, expected):
        """Test basic zip behaviour."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_zip_arity(self, aifpl):
        """Test that zip requires exactly 2 arguments."""
        with pytest.raises(AIFPLEvalError, match=r"expects 2 arguments, got 1"):
            aifpl.evaluate('(zip (list 1 2 3))')

        with pytest.raises(AIFPLEvalError, match=r"expects 2 arguments, got 3"):
            aifpl.evaluate('(zip (list 1 2) (list 3 4) (list 5 6))')

    def test_zip_requires_list_arguments(self, aifpl):
        """Test that zip requires list arguments."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(zip 42 (list 1 2))')

        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(zip (list 1 2) 42)')

    def test_zip_result_is_list_of_pairs(self, aifpl):
        """Test that each element of the result is a 2-element list."""
        result = aifpl.evaluate('(zip (list 1 2 3) (list 4 5 6))')
        assert isinstance(result, list)
        assert len(result) == 3
        for pair in result:
            assert isinstance(pair, list)
            assert len(pair) == 2

    def test_zip_with_map(self, aifpl, helpers):
        """Test zip used with map to process paired elements."""
        # Sum each pair
        helpers.assert_evaluates_to(
            aifpl,
            '(map (lambda (pair) (integer+ (list-first pair) (list-first (list-rest pair)))) (zip (list 1 2 3) (list 4 5 6)))',
            '(5 7 9)'
        )

    def test_zip_to_build_alist(self, aifpl, helpers):
        """Test using zip to build an alist from keys and values."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(fold (lambda (acc pair)
                       (alist-set acc (list-first pair) (list-first (list-rest pair))))
                     (alist)
                     (zip (list "a" "b" "c") (list 1 2 3)))''',
            '{("a" 1) ("b" 2) ("c" 3)}'
        )

    def test_zip_is_first_class(self, aifpl, helpers):
        """Test that zip can be passed as a first-class value."""
        helpers.assert_evaluates_to(
            aifpl,
            '(function? zip)',
            '#t'
        )


class TestUnzip:
    """Tests for the unzip function."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic unzip
        ('(unzip (list (list 1 4) (list 2 5) (list 3 6)))', '((1 2 3) (4 5 6))'),

        # Unzip with strings
        ('(unzip (list (list "a" 1) (list "b" 2) (list "c" 3)))', '(("a" "b" "c") (1 2 3))'),

        # Unzip empty list
        ('(unzip (list))', '(() ())'),

        # Unzip single pair
        ('(unzip (list (list 1 2)))', '((1) (2))'),
    ])
    def test_unzip_basic(self, aifpl, expression, expected):
        """Test basic unzip behaviour."""
        assert aifpl.evaluate_and_format(expression) == expected

    def test_unzip_arity(self, aifpl):
        """Test that unzip requires exactly 1 argument."""
        with pytest.raises(AIFPLEvalError, match=r"expects 1 arguments, got 0"):
            aifpl.evaluate('(unzip)')

        with pytest.raises(AIFPLEvalError, match=r"expects 1 arguments, got 2"):
            aifpl.evaluate('(unzip (list (list 1 2)) (list (list 3 4)))')

    def test_unzip_requires_list_argument(self, aifpl):
        """Test that unzip requires a list argument."""
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(unzip 42)')

    def test_unzip_result_structure(self, aifpl):
        """Test that unzip returns a list of exactly two lists."""
        result = aifpl.evaluate('(unzip (list (list 1 4) (list 2 5) (list 3 6)))')
        assert isinstance(result, list)
        assert len(result) == 2
        assert result[0] == [1, 2, 3]
        assert result[1] == [4, 5, 6]

    def test_unzip_first_and_rest_access(self, aifpl, helpers):
        """Test accessing the two result lists via list-first and list-rest."""
        helpers.assert_evaluates_to(
            aifpl,
            '(list-first (unzip (list (list 1 4) (list 2 5) (list 3 6))))',
            '(1 2 3)'
        )
        helpers.assert_evaluates_to(
            aifpl,
            '(list-first (list-rest (unzip (list (list 1 4) (list 2 5) (list 3 6)))))',
            '(4 5 6)'
        )

    def test_unzip_is_first_class(self, aifpl, helpers):
        """Test that unzip can be passed as a first-class value."""
        helpers.assert_evaluates_to(
            aifpl,
            '(function? unzip)',
            '#t'
        )


class TestZipUnzipRoundtrip:
    """Tests for zip/unzip roundtrip properties."""

    def test_unzip_zip_roundtrip(self, aifpl, helpers):
        """Test that unzip(zip(a, b)) recovers the original lists."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(let* ((a (list 1 2 3))
                      (b (list 4 5 6))
                      (result (unzip (zip a b))))
                 (list (list=? (list-first result) a)
                       (list=? (list-first (list-rest result)) b)))''',
            '(#t #t)'
        )

    def test_zip_unzip_roundtrip(self, aifpl, helpers):
        """Test that zip(unzip(pairs)) recovers the original pairs."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(let* ((pairs (list (list 1 4) (list 2 5) (list 3 6)))
                      (result (unzip pairs))
                      (rezipped (zip (list-first result) (list-first (list-rest result)))))
                 (list=? rezipped pairs))''',
            '#t'
        )

    def test_zip_unzip_with_fold(self, aifpl, helpers):
        """Test using zip and unzip together in a data processing pipeline."""
        # Compute dot product of two vectors using zip + fold
        helpers.assert_evaluates_to(
            aifpl,
            '''(fold integer+
                   0
                   (map (lambda (pair)
                          (integer* (list-first pair) (list-first (list-rest pair))))
                        (zip (list 1 2 3) (list 4 5 6))))''',
            '32'  # 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
        )
