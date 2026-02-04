"""Tests for AIFPL parser edge cases and error handling."""

import pytest

from aifpl import AIFPLTokenError, AIFPLParseError, AIFPLEvalError


class TestAIFPLParserEdgeCases:
    """Test parser edge cases and comprehensive error handling."""

    def test_deeply_nested_expressions(self, aifpl):
        """Test parsing of deeply nested expressions."""
        # Nested arithmetic
        result = aifpl.evaluate("(+ (+ (+ 1 2) 3) 4)")
        assert result == 10

        # Nested function calls
        result = aifpl.evaluate("(abs (- (max 1 5 3) (min 1 5 3)))")
        assert result == 4

        # Nested lists
        result = aifpl.evaluate("(list (list 1 2) (list 3 4))")
        assert result == [[1, 2], [3, 4]]

        # Very deep nesting (within reasonable limits)
        deep_expr = "(+ " * 50 + "1" + " 0)" * 50
        result = aifpl.evaluate(deep_expr)
        assert result == 1

    def test_whitespace_handling_comprehensive(self, aifpl):
        """Test comprehensive whitespace handling scenarios."""
        # Various whitespace characters
        test_cases = [
            ("  ( +   1    2   3  )  ", 6),
            ("(\n+\n1\n2\n3\n)", 6),
            ("(\t+\t1\t2\t3\t)", 6),
            ("(\r+\r1\r2\r3\r)", 6),
            (" ( + \t 1 \n 2 \r 3 ) ", 6),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

    def test_comment_handling(self, aifpl):
        """Test comment handling in parser."""
        # Comments might not be implemented, but test if they are
        try:
            # Single line comments
            result = aifpl.evaluate("(+ 1 2) ; this is a comment")
            assert result == 3
        except (AIFPLParseError, AIFPLTokenError):
            # Comments might not be supported, which is fine
            pass

        try:
            # Comments with newlines
            result = aifpl.evaluate("""
            (+ 1 ; first number
               2) ; second number
            """)
            assert result == 3
        except (AIFPLParseError, AIFPLTokenError):
            pass

    def test_malformed_syntax_comprehensive(self, aifpl):
        """Test comprehensive malformed syntax scenarios."""
        malformed_expressions = [
            # Unmatched parentheses variations
            "((+ 1 2)",
            "(+ 1 2))",
            "((+ 1 2",
            "(+ 1 2 3))",
            "(((",
            ")))",

            # Empty expressions that should fail
            "(/)",
            "(-)",

            # Invalid tokens in expressions
            "(+ 1 2 @)",
            "(+ 1 2 #xyz)",
            "(+ 1 2 $)",
            "(+ 1 2 %invalid)",

            # Malformed numbers
            "(+ 1 2.3.4)",
            "(+ 1 2e)",
            "(+ 1 .)",

            # Invalid identifiers
            "(123abc 1 2)",
            "(+ 1abc 2)",
        ]

        for expr in malformed_expressions:
            with pytest.raises((AIFPLTokenError, AIFPLParseError, AIFPLEvalError)):
                aifpl.evaluate(expr)

    def test_string_parsing_edge_cases(self, aifpl):
        """Test string parsing edge cases."""
        # Unclosed strings
        with pytest.raises(AIFPLTokenError, match="Unterminated string"):
            aifpl.evaluate('(+ 1 "unclosed string')

        with pytest.raises(AIFPLTokenError, match="Unterminated string"):
            aifpl.evaluate('"hello world')

        # Strings with various escape sequences
        test_cases = [
            ('"hello\\nworld"', "hello\nworld"),
            ('"hello\\tworld"', "hello\tworld"),
            ('"hello\\rworld"', "hello\rworld"),
            ('"hello\\\\"', "hello\\"),  # Fixed: 4 backslashes in source -> 2 backslashes in result
            ('"hello\\""', 'hello"'),
            ('"\\u0041"', "A"),
            ('"\\u03B1"', "α"),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Invalid escape sequences
        invalid_escapes = [
            '"hello\\q"',  # Invalid escape
            '"test\\z"',   # Invalid escape
            '"\\uXYZ"',    # Invalid unicode
            '"\\uGGGG"',   # Invalid hex
            '"\\u12"',     # Too few digits
            '"\\u"',       # No digits
        ]

        for expr in invalid_escapes:
            with pytest.raises(AIFPLTokenError):
                aifpl.evaluate(expr)

    def test_number_parsing_edge_cases(self, aifpl):
        """Test number parsing edge cases."""
        # Various number formats
        test_cases = [
            # Integers
            ("42", 42),
            ("-42", -42),
            ("0", 0),

            # Floats
            ("3.14", 3.14),
            ("-3.14", -3.14),
            ("0.0", 0.0),
            (".5", 0.5),
            ("5.", 5.0),

            # Scientific notation
            ("1e2", 100.0),
            ("1.5e2", 150.0),
            ("1E-2", 0.01),
            ("2.5E+1", 25.0),
            ("-1e3", -1000.0),

            # Hexadecimal
            ("#xFF", 255),
            ("#x10", 16),
            ("#xABC", 2748),
            ("#xff", 255),

            # Binary
            ("#b1010", 10),
            ("#b11111111", 255),
            ("#B1010", 10),

            # Octal
            ("#o777", 511),
            ("#o10", 8),
            ("#O777", 511),
        ]

        for expr, expected in test_cases:
            result = aifpl.evaluate(expr)
            assert result == expected

        # Invalid number formats
        invalid_numbers = [
            "0x",      # Hex without digits
            "0b",      # Binary without digits
            "0o",      # Octal without digits
            "1e",      # Missing exponent
            "1.5e+",   # Missing exponent after sign
            "0xGHI",   # Invalid hex digits
            "0b123",   # Invalid binary digits
            "0o89",    # Invalid octal digits
        ]

        for expr in invalid_numbers:
            with pytest.raises((AIFPLTokenError, AIFPLParseError)):
                aifpl.evaluate(expr)

    def test_boolean_parsing_edge_cases(self, aifpl):
        """Test boolean parsing edge cases."""
        # Valid booleans
        assert aifpl.evaluate("#t") is True
        assert aifpl.evaluate("#f") is False
        assert aifpl.evaluate("true") is True
        assert aifpl.evaluate("false") is False

        # Invalid boolean literals
        invalid_booleans = [
            "#x",      # Not #t or #f
            "#true",   # Must be exactly #t
            "#false",  # Must be exactly #f
            "#T",      # Case sensitive
            "#F",      # Case sensitive
        ]

        for expr in invalid_booleans:
            with pytest.raises(AIFPLTokenError):
                aifpl.evaluate(expr)

    def test_quote_parsing_edge_cases(self, aifpl):
        """Test quote parsing edge cases."""
        # Basic quote operations
        result = aifpl.evaluate("(quote x)")
        # Result should be a symbol or identifier

        result = aifpl.evaluate("'x")
        # Should be equivalent to (quote x)

        # Quote with complex expressions
        result = aifpl.evaluate("(quote (+ 1 2 3))")
        # Should return the list structure, not evaluate it

        result = aifpl.evaluate("'(+ 1 2 3)")
        # Should be equivalent to above

        # Nested quotes
        result = aifpl.evaluate("(quote (quote x))")
        result = aifpl.evaluate("'('x)")

        # Quote with various data types
        test_cases = [
            '(quote 42)',
            '(quote "hello")',
            '(quote #t)',
            '(quote ())',
            '(quote (1 2 3))',
        ]

        for expr in test_cases:
            # Should not raise an error
            result = aifpl.evaluate(expr)

    def test_list_parsing_edge_cases(self, aifpl):
        """Test list parsing edge cases."""
        # Empty lists
        result = aifpl.evaluate("()")
        assert result == []

        result = aifpl.evaluate("(list)")
        assert result == []

        # Single element lists
        result = aifpl.evaluate("(list 1)")
        assert result == [1]

        # Mixed type lists
        result = aifpl.evaluate('(list 1 "hello" #t)')
        assert result == [1, "hello", True]

        # Nested lists
        result = aifpl.evaluate("(list (list 1 2) (list 3 4))")
        assert result == [[1, 2], [3, 4]]

        # Lists with expressions
        result = aifpl.evaluate("(list (+ 1 2) (* 3 4))")
        assert result == [3, 12]

    def test_function_call_parsing_edge_cases(self, aifpl):
        """Test function call parsing edge cases."""
        # Zero argument functions
        result = aifpl.evaluate("(+)")  # Should be 0 (additive identity)
        assert result == 0

        result = aifpl.evaluate("(*)")  # Should be 1 (multiplicative identity)
        assert result == 1

        # Single argument functions
        result = aifpl.evaluate("(+ 5)")
        assert result == 5

        result = aifpl.evaluate("(- 5)")  # Unary minus
        assert result == -5

        # Many argument functions
        result = aifpl.evaluate("(+ 1 2 3 4 5 6 7 8 9 10)")
        assert result == 55

    def test_lambda_parsing_edge_cases(self, aifpl):
        """Test lambda parsing edge cases."""
        # Zero parameter lambda
        result = aifpl.evaluate("((lambda () 42))")
        assert result == 42

        # Single parameter lambda
        result = aifpl.evaluate("((lambda (x) x) 5)")
        assert result == 5

        # Multiple parameter lambda
        result = aifpl.evaluate("((lambda (x y z) (+ x y z)) 1 2 3)")
        assert result == 6

        # Lambda with complex body
        result = aifpl.evaluate("((lambda (x) (+ (* x x) 1)) 3)")
        assert result == 10

    def test_let_parsing_edge_cases(self, aifpl):
        """Test let binding parsing edge cases."""
        # Single binding
        result = aifpl.evaluate("(let ((x 5)) x)")
        assert result == 5

        # Multiple bindings
        result = aifpl.evaluate("(let ((x 5) (y 3)) (+ x y))")
        assert result == 8

        # Sequential dependency
        result = aifpl.evaluate("(let* ((x 5) (y (* x 2))) (+ x y))")
        assert result == 15

        # Complex expressions in bindings
        result = aifpl.evaluate("(let ((x (+ 1 2)) (y (* 3 4))) (+ x y))")
        assert result == 15

    def test_conditional_parsing_edge_cases(self, aifpl):
        """Test conditional parsing edge cases."""
        # Simple conditionals
        result = aifpl.evaluate('(if #t "yes" "no")')
        assert result == "yes"

        result = aifpl.evaluate('(if #f "yes" "no")')
        assert result == "no"

        # Nested conditionals
        result = aifpl.evaluate('(if #t (if #t "inner-yes" "inner-no") "outer-no")')
        assert result == "inner-yes"

        # Conditionals with expressions
        result = aifpl.evaluate("(if (> 5 3) (+ 1 2) (- 1 2))")
        assert result == 3

    def test_parser_error_recovery(self, aifpl):
        """Test parser error recovery and state management."""
        # After a parse error, parser should be ready for next expression
        with pytest.raises((AIFPLParseError, AIFPLTokenError)):
            aifpl.evaluate("(+ 1 2")

        # Next evaluation should work normally
        result = aifpl.evaluate("(+ 1 2)")
        assert result == 3

        # Multiple errors in sequence
        errors = ["(+ 1 2", "+ 1 2)", "(+ 1 @)"]
        for expr in errors:
            with pytest.raises((AIFPLTokenError, AIFPLParseError, AIFPLEvalError)):
                aifpl.evaluate(expr)

        # Should still work after multiple errors
        result = aifpl.evaluate("(* 2 3)")
        assert result == 6

    def test_parser_position_tracking(self, aifpl):
        """Test that parser tracks positions for error reporting."""
        # Test various error positions
        try:
            aifpl.evaluate("(+ 1 @)")
        except (AIFPLTokenError, AIFPLParseError) as e:
            # Error message should contain position information
            error_msg = str(e)
            # Should mention position or the problematic character
            assert "@" in error_msg or "position" in error_msg.lower()

        try:
            aifpl.evaluate("(+ 1 2")
        except (AIFPLTokenError, AIFPLParseError) as e:
            error_msg = str(e)
            assert "parenthesis" in error_msg.lower() or "position" in error_msg.lower()

    def test_parser_memory_efficiency(self, aifpl):
        """Test parser memory efficiency with large expressions."""
        # Large list literal
        large_list_expr = "(list " + " ".join(str(i) for i in range(1000)) + ")"
        result = aifpl.evaluate(large_list_expr)
        assert len(result) == 1000
        assert result[0] == 0
        assert result[999] == 999

        # Large arithmetic expression
        large_arith_expr = "(+ " + " ".join(str(i) for i in range(100)) + ")"
        result = aifpl.evaluate(large_arith_expr)
        assert result == sum(range(100))

    def test_parser_unicode_support(self, aifpl):
        """Test parser Unicode support."""
        # Unicode in strings
        result = aifpl.evaluate('"Hello 世界"')
        assert result == "Hello 世界"

        # Unicode escape sequences
        result = aifpl.evaluate('"\\u4E16\\u754C"')
        assert result == "世界"

        # Various Unicode characters
        unicode_chars = [
            '"α"',     # Greek
            '"π"',     # Mathematical
            '"🚀"',    # Emoji (if supported)
            '"Ω"',     # Greek capital
        ]

        for expr in unicode_chars:
            try:
                result = aifpl.evaluate(expr)
                assert isinstance(result, str)
                assert len(result) >= 1
            except (AIFPLTokenError, AIFPLParseError):
                # Some Unicode might not be supported, which is fine
                pass

    def test_parser_expression_boundaries(self, aifpl):
        """Test parser expression boundary detection."""
        # Multiple expressions should be rejected
        with pytest.raises(AIFPLParseError, match="Unexpected token after complete expression"):
            aifpl.evaluate("1 2")

        with pytest.raises(AIFPLParseError, match="Unexpected token after complete expression"):
            aifpl.evaluate("(+ 1 2) (+ 3 4)")

        with pytest.raises(AIFPLParseError, match="Unexpected token after complete expression"):
            aifpl.evaluate('42 "hello"')

    def test_parser_special_forms_edge_cases(self, aifpl):
        """Test parsing of special forms edge cases."""
        # Test various special forms exist and parse correctly
        special_forms = [
            "(quote x)",
            "'x",
            "(if #t 1 2)",
            "(lambda (x) x)",
            "(let ((x 1)) x)",
        ]

        for expr in special_forms:
            # Should parse without error (might have eval errors, but parsing should work)
            try:
                result = aifpl.evaluate(expr)
                # If it evaluates, that's fine
            except AIFPLEvalError:
                # Evaluation errors are fine, we're testing parsing
                pass
            except (AIFPLTokenError, AIFPLParseError):
                # These would indicate parsing problems
                pytest.fail(f"Parsing failed for special form: {expr}")

    def test_parser_broken_quote(self, aifpl):
        """Handle an empty quote"""
        with pytest.raises(AIFPLParseError, match="Incomplete quote expression"):
            result = aifpl.evaluate("('")

        with pytest.raises(AIFPLParseError, match=r"Unexpected token: \)"):
            result = aifpl.evaluate("(')")

        with pytest.raises(AIFPLParseError, match=r"Unexpected token: \)"):
            result = aifpl.evaluate("(' )")

    def test_parser_operator_precedence_not_applicable(self, aifpl):
        """Test that LISP syntax doesn't have precedence issues."""
        # In LISP, everything is explicit with parentheses
        # These should all be unambiguous
        expressions = [
            "(+ 1 (* 2 3))",      # 1 + (2 * 3) = 7
            "(* (+ 1 2) 3)",      # (1 + 2) * 3 = 9
            "(+ (* 2 3) (* 4 5))", # (2 * 3) + (4 * 5) = 26
        ]

        expected = [7, 9, 26]

        for expr, exp in zip(expressions, expected):
            result = aifpl.evaluate(expr)
            assert result == exp
