"""Tests for LISP output formatting and result display."""

import pytest

from aifpl import AIFPL


class TestFormatting:
    """Test LISP output formatting and result display consistency."""

    @pytest.mark.parametrize("expression,expected_format", [
        # Integer formatting
        ("42", "42"),
        ("0", "0"),
        ("-17", "-17"),
        ("1000000", "1000000"),

        # Different integer bases should format as decimal
        ("0xFF", "255"),
        ("0b1010", "10"),
        ("0o777", "511"),
    ])
    def test_integer_formatting(self, aifpl, expression, expected_format):
        """Test integer formatting in LISP output."""
        assert aifpl.evaluate_and_format(expression) == expected_format

    @pytest.mark.parametrize("expression,expected_format", [
        # Float formatting
        ("3.14", "3.14"),
        ("0.0", "0.0"),  # Preserves float type
        ("1.0", "1.0"),  # Preserves float type
        ("-2.5", "-2.5"),
        ("1.5e2", "150.0"),  # Scientific notation produces float
        ("1e-3", "0.001"),
    ])
    def test_float_formatting(self, aifpl, expression, expected_format):
        """Test float formatting in LISP output."""
        assert aifpl.evaluate_and_format(expression) == expected_format

    @pytest.mark.parametrize("expression,expected_format", [
        # Complex number formatting
        ("(complex 1 2)", "(1+2j)"),
        ("(complex 0 1)", "1j"),
        ("(complex 3 0)", "3.0"),  # Simplifies to float when imaginary is 0
        ("(complex -1 -2)", "(-1-2j)"),
        ("j", "1j"),
        ("(* 2 j)", "2j"),
        ("(+ 1 j)", "(1+1j)"),
    ])
    def test_complex_number_formatting(self, aifpl, expression, expected_format):
        """Test complex number formatting in LISP output."""
        assert aifpl.evaluate_and_format(expression) == expected_format

    def test_complex_number_simplification(self, aifpl_custom):
        """Test that complex numbers with negligible imaginary parts are simplified."""
        # Test with default tolerance
        aifpl_default = aifpl_custom(floating_point_tolerance=1e-10)

        # Very small imaginary part should be simplified
        result = aifpl_default.evaluate_and_format("(+ 5 (* 1e-15 j))")
        assert result == "5.0"  # Should be simplified to float (real part of complex)

        # Larger imaginary part should be preserved
        result = aifpl_default.evaluate_and_format("(+ 5 (* 1e-5 j))")
        assert "(5+" in result and "j)" in result  # Should remain complex

    @pytest.mark.parametrize("expression,expected_format", [
        # String formatting (quoted in LISP output)
        ('"hello"', '"hello"'),
        ('""', '""'),  # Empty string
        ('"hello world"', '"hello world"'),
        ('"with\\"quotes\\""', '"with\\"quotes\\""'),  # Escaped quotes preserved

        # Unicode strings
        ('"café"', '"café"'),
        ('"世界"', '"世界"'),
        ('"Hello 👋"', '"Hello 👋"'),
    ])
    def test_string_formatting(self, aifpl, expression, expected_format):
        """Test string formatting in LISP output (should be quoted)."""
        assert aifpl.evaluate_and_format(expression) == expected_format

    @pytest.mark.parametrize("expression,expected_format", [
        # Boolean formatting
        ("#t", "#t"),
        ("#f", "#f"),
        ("true", "#t"),  # Constant should format as #t
        ("false", "#f"),  # Constant should format as #f

        # Boolean operations
        ("(and #t #t)", "#t"),
        ("(or #f #f)", "#f"),
        ("(not #t)", "#f"),

        # Boolean comparisons
        ("(= 1 1)", "#t"),
        ("(> 5 3)", "#t"),
        ("(< 5 3)", "#f"),
    ])
    def test_boolean_formatting(self, aifpl, expression, expected_format):
        """Test boolean formatting in LISP output."""
        assert aifpl.evaluate_and_format(expression) == expected_format

    @pytest.mark.parametrize("expression,expected_format", [
        # Empty list
        ("(list)", "()"),

        # Single element lists
        ("(list 1)", "(1)"),
        ('(list "hello")', '("hello")'),
        ("(list #t)", "(#t)"),

        # Multiple element lists
        ("(list 1 2 3)", "(1 2 3)"),
        ('(list "a" "b" "c")', '("a" "b" "c")'),
        ("(list #t #f #t)", "(#t #f #t)"),

        # Mixed type lists
        ('(list 1 "hello" #t)', '(1 "hello" #t)'),
        ('(list 42 3.14 "world" #f)', '(42 3.14 "world" #f)'),
    ])
    def test_basic_list_formatting(self, aifpl, expression, expected_format):
        """Test basic list formatting in LISP output."""
        assert aifpl.evaluate_and_format(expression) == expected_format

    @pytest.mark.parametrize("expression,expected_format", [
        # Nested lists
        ("(list (list 1 2) (list 3 4))", "((1 2) (3 4))"),
        ("(list 1 (list 2 3) 4)", "(1 (2 3) 4)"),
        ("(list (list) (list 1) (list 2 3))", "(() (1) (2 3))"),

        # Deeply nested lists
        ("(list (list (list 1)))", "(((1)))"),
        ("(list (list 1 (list 2)) (list 3))", "((1 (2)) (3))"),

        # Mixed nesting with different types
        ('(list (list 1 "a") (list #t 3.14))', '((1 "a") (#t 3.14))'),
    ])
    def test_nested_list_formatting(self, aifpl, expression, expected_format):
        """Test nested list formatting in LISP output."""
        assert aifpl.evaluate_and_format(expression) == expected_format

    def test_list_operations_formatting(self, aifpl, helpers):
        """Test that list operations produce properly formatted results."""
        # cons operation
        helpers.assert_evaluates_to(aifpl, '(cons 1 (list 2 3))', '(1 2 3)')

        # append operation
        helpers.assert_evaluates_to(aifpl, '(append (list 1 2) (list 3 4))', '(1 2 3 4)')

        # reverse operation
        helpers.assert_evaluates_to(aifpl, '(reverse (list 1 2 3))', '(3 2 1)')

        # filter operation
        helpers.assert_evaluates_to(
            aifpl, 
            '(filter (lambda (x) (> x 2)) (list 1 2 3 4))', 
            '(3 4)'
        )

    def test_complex_data_structure_formatting(self, aifpl, helpers):
        """Test formatting of complex data structures."""
        # List containing all data types
        complex_list = '''
        (list 
          42 
          3.14 
          (complex 1 2) 
          "hello" 
          #t 
          #f 
          (list 1 2 3)
          (list "nested" "strings"))
        '''

        expected = '(42 3.14 (1+2j) "hello" #t #f (1 2 3) ("nested" "strings"))'
        helpers.assert_evaluates_to(aifpl, complex_list, expected)

    def test_arithmetic_result_formatting(self, aifpl, helpers):
        """Test that arithmetic operations produce properly formatted results."""
        # Integer results
        helpers.assert_evaluates_to(aifpl, '(+ 1 2)', '3')
        helpers.assert_evaluates_to(aifpl, '(* 3 4)', '12')

        # Float results
        helpers.assert_evaluates_to(aifpl, '(/ 7 2)', '3.5')
        helpers.assert_evaluates_to(aifpl, '(+ 1.5 2.5)', '4.0')  # Preserves float type

        # Complex results
        helpers.assert_evaluates_to(aifpl, '(+ 1 j)', '(1+1j)')
        helpers.assert_evaluates_to(aifpl, '(* j j)', '-1.0')  # Simplifies to float when imag is 0

    def test_string_operation_result_formatting(self, aifpl, helpers):
        """Test that string operations produce properly formatted results."""
        # String concatenation
        helpers.assert_evaluates_to(
            aifpl, 
            '(string-append "hello" " " "world")', 
            '"hello world"'
        )

        # String case conversion
        helpers.assert_evaluates_to(aifpl, '(string-upcase "hello")', '"HELLO"')
        helpers.assert_evaluates_to(aifpl, '(string-downcase "WORLD")', '"world"')

        # String to list conversion
        helpers.assert_evaluates_to(aifpl, '(string->list "hi")', '("h" "i")')

        # String splitting
        helpers.assert_evaluates_to(
            aifpl, 
            '(string-split "a,b,c" ",")', 
            '("a" "b" "c")'
        )

    def test_conditional_result_formatting(self, aifpl, helpers):
        """Test that conditional expressions produce properly formatted results."""
        # Different result types from conditionals
        helpers.assert_evaluates_to(aifpl, '(if #t 42 0)', '42')
        helpers.assert_evaluates_to(aifpl, '(if #f "yes" "no")', '"no"')
        helpers.assert_evaluates_to(aifpl, '(if #t (list 1 2) (list 3 4))', '(1 2)')
        helpers.assert_evaluates_to(aifpl, '(if #f #t #f)', '#f')

    def test_functional_operation_result_formatting(self, aifpl, helpers):
        """Test that functional operations produce properly formatted results."""
        # Map results
        helpers.assert_evaluates_to(
            aifpl, 
            '(map (lambda (x) (* x 2)) (list 1 2 3))', 
            '(2 4 6)'
        )

        # Filter results
        helpers.assert_evaluates_to(
            aifpl, 
            '(filter (lambda (x) (> x 0)) (list -1 2 -3 4))', 
            '(2 4)'
        )

        # Fold results
        helpers.assert_evaluates_to(
            aifpl, 
            '(fold + 0 (list 1 2 3 4))', 
            '10'
        )

        # Range results
        helpers.assert_evaluates_to(aifpl, '(range 1 5)', '(1 2 3 4)')

    def test_lambda_function_formatting(self, aifpl):
        """Test that lambda functions are formatted appropriately."""
        # Lambda functions should have a readable representation
        # Note: This tests the Python object representation, not LISP formatting
        lambda_expr = '(lambda (x) (* x 2))'
        result = aifpl.evaluate(lambda_expr)

        # When formatted for LISP output, should show as function representation
        formatted = aifpl.evaluate_and_format(lambda_expr)
        assert "lambda" in formatted.lower() or "<" in formatted

    def test_let_expression_result_formatting(self, aifpl, helpers):
        """Test that let expressions produce properly formatted results."""
        # Let expressions should format their body result
        helpers.assert_evaluates_to(aifpl, '(let ((x 5)) x)', '5')
        helpers.assert_evaluates_to(aifpl, '(let ((x 5) (y 3)) (+ x y))', '8')
        helpers.assert_evaluates_to(
            aifpl, 
            '(let ((name "hello")) (string-append name " world"))', 
            '"hello world"'
        )

    def test_whitespace_in_formatting(self, aifpl, helpers):
        """Test that formatting uses appropriate whitespace."""
        # Lists should have single spaces between elements
        helpers.assert_evaluates_to(aifpl, '(list 1 2 3 4 5)', '(1 2 3 4 5)')

        # Nested lists should not have extra spacing
        helpers.assert_evaluates_to(
            aifpl, 
            '(list (list 1 2) (list 3 4) (list 5 6))', 
            '((1 2) (3 4) (5 6))'
        )

        # Mixed types should have consistent spacing
        helpers.assert_evaluates_to(
            aifpl, 
            '(list 1 "hello" #t 3.14 (list "nested"))', 
            '(1 "hello" #t 3.14 ("nested"))'
        )

    def test_round_trip_consistency(self, aifpl):
        """Test that formatting is consistent for equivalent expressions."""
        # Different ways of creating the same list should format identically
        list1 = aifpl.evaluate_and_format('(list 1 2 3)')
        list2 = aifpl.evaluate_and_format('(cons 1 (cons 2 (cons 3 (list))))')
        list3 = aifpl.evaluate_and_format('(append (list 1) (list 2) (list 3))')

        assert list1 == list2 == list3 == '(1 2 3)'

        # Different ways of creating the same number should format identically
        num1 = aifpl.evaluate_and_format('6')
        num2 = aifpl.evaluate_and_format('(+ 2 4)')
        num3 = aifpl.evaluate_and_format('(* 2 3)')

        assert num1 == num2 == num3 == '6'

    def test_edge_case_formatting(self, aifpl, helpers):
        """Test formatting of edge cases and boundary values."""
        # Very large numbers
        helpers.assert_evaluates_to(aifpl, '(** 2 20)', '1048576')

        # Very small numbers (close to zero)
        result = aifpl.evaluate_and_format('(/ 1 1000000)')
        assert '1e-06' in result or '0.000001' in result

        # Empty structures
        helpers.assert_evaluates_to(aifpl, '(list)', '()')
        helpers.assert_evaluates_to(aifpl, '""', '""')

        # Single character strings
        helpers.assert_evaluates_to(aifpl, '"a"', '"a"')

        # Single element lists
        helpers.assert_evaluates_to(aifpl, '(list 42)', '(42)')

    def test_unicode_formatting_preservation(self, aifpl, helpers):
        """Test that Unicode characters are preserved in formatting."""
        # Basic Unicode
        helpers.assert_evaluates_to(aifpl, '"café"', '"café"')
        helpers.assert_evaluates_to(aifpl, '"世界"', '"世界"')

        # Unicode in lists
        helpers.assert_evaluates_to(aifpl, '(list "α" "β" "γ")', '("α" "β" "γ")')

        # Mixed ASCII and Unicode
        helpers.assert_evaluates_to(
            aifpl, 
            '(list "hello" "世界" "café")', 
            '("hello" "世界" "café")'
        )

    def test_special_character_formatting(self, aifpl, helpers):
        """Test formatting of strings with special characters."""
        # Strings with escape sequences should show the actual characters
        # (The input parsing handles escapes, output shows the result)
        helpers.assert_evaluates_to(aifpl, '"hello\\nworld"', '"hello\\nworld"')
        helpers.assert_evaluates_to(aifpl, '"tab\\there"', '"tab\\there"')

        # Quotes in strings
        helpers.assert_evaluates_to(aifpl, '"say \\"hello\\""', '"say \\"hello\\""')

    def test_number_precision_formatting(self, aifpl):
        """Test that number precision is maintained in formatting."""
        # Float precision should be preserved reasonably
        result = aifpl.evaluate_and_format('(/ 1 3)')
        assert '0.333333' in result  # Should show reasonable precision

        # Very precise calculations
        result = aifpl.evaluate_and_format('(+ 0.1 0.2)')
        # This might have floating point precision issues, but should be formatted consistently
        assert result.startswith('0.3')

    def test_complex_expression_formatting(self, aifpl, helpers):
        """Test formatting of results from complex expressions."""
        # Complex functional pipeline
        complex_result = '''
        (map (lambda (x) (+ x 1))
             (filter (lambda (x) (> x 0))
                     (list -1 2 -3 4 5)))
        '''
        helpers.assert_evaluates_to(aifpl, complex_result, '(3 5 6)')

        # Nested let with lambda
        nested_result = '''
        (let ((multiplier 10))
          (let ((f (lambda (x) (* x multiplier))))
            (list (f 1) (f 2) (f 3))))
        '''
        helpers.assert_evaluates_to(aifpl, nested_result, '(10 20 30)')

    def test_error_vs_success_formatting_distinction(self, aifpl):
        """Test that successful results are formatted while errors are not."""
        # Successful evaluation should return formatted string
        success_result = aifpl.evaluate_and_format('(+ 1 2)')
        assert success_result == '3'
        assert isinstance(success_result, str)

        # Error should raise exception, not return formatted error
        with pytest.raises(Exception):  # Should raise some form of error
            aifpl.evaluate_and_format('(/ 1 0)')

    def test_consistency_between_evaluation_methods(self, aifpl):
        """Test consistency between evaluate() and evaluate_and_format()."""
        test_expressions = [
            '42',
            '3.14',
            '"hello"',
            '#t',
            '#f',
            '(list 1 2 3)',
            '(+ 1 2)',
            '(string-append "hello" " " "world")',
            '(if #t "yes" "no")',
        ]

        for expr in test_expressions:
            # Both methods should succeed
            python_result = aifpl.evaluate(expr)
            formatted_result = aifpl.evaluate_and_format(expr)

            # The formatted result should be a string representation
            assert isinstance(formatted_result, str)

            # For simple values, we can verify some consistency
            # FIXED: Check for booleans FIRST since bool is a subclass of int in Python
            if isinstance(python_result, bool):
                assert formatted_result in ['#t', '#f']
            elif isinstance(python_result, (int, float)):
                assert str(python_result) == formatted_result or formatted_result == str(int(python_result))
            elif isinstance(python_result, str):
                assert formatted_result == f'"{python_result}"'
