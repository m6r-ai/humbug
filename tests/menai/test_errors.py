"""Tests for error handling and exception reporting."""

import pytest

from menai import MenaiError, MenaiTokenError, MenaiParseError, MenaiEvalError


class TestErrors:
    """Test error detection and exception handling."""

    # ========== Tokenization Errors ==========

    def test_invalid_character_token_error(self, menai):
        """Test that invalid characters cause tokenization errors."""
        with pytest.raises(MenaiTokenError):
            menai.evaluate("@invalid")

        with pytest.raises(MenaiTokenError):
            menai.evaluate("hello$world")

        # FIXED: Use actually invalid character instead of & which is valid
        with pytest.raises(MenaiTokenError):
            menai.evaluate("(integer+ 1 @ 2)")

    def test_unterminated_string_token_error(self, menai):
        """Test that unterminated strings cause tokenization errors."""
        with pytest.raises(MenaiTokenError, match="Unterminated string literal"):
            menai.evaluate('"hello world')

        with pytest.raises(MenaiTokenError, match="Unterminated string literal"):
            menai.evaluate('(string-concat "hello" "world)')

    def test_invalid_escape_sequence_token_error(self, menai):
        """Test that invalid escape sequences cause tokenization errors."""
        with pytest.raises(MenaiTokenError, match="Invalid escape sequence"):
            menai.evaluate('"hello\\q"')  # Invalid escape

        with pytest.raises(MenaiTokenError, match="Invalid escape sequence"):
            menai.evaluate('"test\\z"')

    def test_invalid_unicode_escape_token_error(self, menai):
        """Test that invalid Unicode escapes cause tokenization errors."""
        with pytest.raises(MenaiTokenError, match="Invalid Unicode"):
            menai.evaluate('"\\uXYZ"')  # Not enough hex digits

        with pytest.raises(MenaiTokenError, match="Invalid Unicode"):
            menai.evaluate('"\\uGGGG"')  # Invalid hex digits

    def test_incomplete_unicode_escape_token_error(self, menai):
        """Test that incomplete Unicode escapes cause tokenization errors."""
        with pytest.raises(MenaiTokenError, match="Incomplete Unicode escape"):
            menai.evaluate('"\\u12"')  # Too few digits

        with pytest.raises(MenaiTokenError, match="Incomplete Unicode escape"):
            menai.evaluate('"\\u"')  # No digits

    def test_invalid_boolean_literal_token_error(self, menai):
        """Test that invalid boolean literals cause tokenization errors."""
        with pytest.raises(MenaiTokenError, match="Incomplete number"):
            menai.evaluate("#x")  # Not #t or #f

        with pytest.raises(MenaiTokenError, match="Invalid boolean literal"):
            menai.evaluate("#true")  # Must be exactly #t

    def test_invalid_number_format_token_error(self, menai):
        """Test that invalid number formats cause tokenization errors."""
        with pytest.raises(MenaiTokenError, match="Incomplete"):
            menai.evaluate("#x")  # Hex without digits

        with pytest.raises(MenaiTokenError, match="Incomplete"):
            menai.evaluate("#b")  # Binary without digits

        with pytest.raises(MenaiTokenError, match="Incomplete"):
            menai.evaluate("#o")  # Octal without digits

    def test_malformed_scientific_notation_token_error(self, menai):
        """Test that malformed scientific notation causes tokenization errors."""
        with pytest.raises(MenaiTokenError, match="Invalid number format"):
            menai.evaluate("1e")  # Missing exponent

        with pytest.raises(MenaiTokenError, match="Invalid number format"):
            menai.evaluate("1.5e+")  # Missing exponent after sign

    # ========== Parsing Errors ==========

    def test_empty_expression_parse_error(self, menai):
        """Test that empty expressions cause parse errors."""
        with pytest.raises(MenaiParseError, match="Empty expression"):
            menai.evaluate("")

        with pytest.raises(MenaiParseError, match="Empty expression"):
            menai.evaluate("   ")  # Whitespace only

    def test_unmatched_parentheses_parse_error(self, menai):
        """Test that unmatched parentheses cause parse errors."""
        with pytest.raises(MenaiParseError, match="Unterminated list"):
            menai.evaluate("(integer+ 1 2")

        with pytest.raises(MenaiParseError, match="Unterminated list"):
            menai.evaluate("(integer* (integer+ 1 2) 3")

        with pytest.raises(MenaiParseError, match="Unexpected token after complete expression"):
            menai.evaluate("integer+ 1 2)")

    def test_unexpected_token_after_expression_parse_error(self, menai):
        """Test that extra tokens after complete expression cause parse errors."""
        with pytest.raises(MenaiParseError, match="Unexpected token after complete expression"):
            menai.evaluate("42 43")

        with pytest.raises(MenaiParseError, match="Unexpected token after complete expression"):
            menai.evaluate("(integer+ 1 2) (integer+ 3 4)")

    def test_invalid_lambda_syntax_parse_error(self, menai):
        """Test that invalid lambda syntax causes evaluation errors (pure list approach)."""
        with pytest.raises(MenaiEvalError, match="Lambda expression structure is incorrect"):
            menai.evaluate("(lambda)")

        with pytest.raises(MenaiEvalError, match="Lambda expression structure is incorrect"):
            menai.evaluate("(lambda (x))")  # Missing body

        with pytest.raises(MenaiEvalError, match="Lambda expression structure is incorrect"):
            menai.evaluate("(lambda (x) (integer+ x 1) extra)")  # Too many elements

    def test_invalid_lambda_parameters_parse_error(self, menai):
        """Test that invalid lambda parameters cause evaluation errors (pure list approach)."""
        with pytest.raises(MenaiEvalError, match="Lambda parameter .* must be a symbol"):
            menai.evaluate("(lambda (1) (integer+ 1 1))")  # Number as parameter

        with pytest.raises(MenaiEvalError, match="Lambda parameter .* must be a symbol"):
            menai.evaluate('(lambda ("x") (integer+ x 1))')  # String as parameter

    def test_duplicate_lambda_parameters_parse_error(self, menai):
        """Test that duplicate lambda parameters cause evaluation errors (pure list approach)."""
        with pytest.raises(MenaiEvalError, match="Lambda parameters must be unique"):
            menai.evaluate("(lambda (x x) (integer+ x x))")

        with pytest.raises(MenaiEvalError, match="Lambda parameters must be unique"):
            menai.evaluate("(lambda (x y x) (integer+ x y))")

    def test_invalid_let_syntax_parse_error(self, menai):
        """Test that invalid let syntax causes evaluation errors (pure list approach)."""
        with pytest.raises(MenaiEvalError, match="Let expression structure is incorrect"):
            menai.evaluate("(let)")

        with pytest.raises(MenaiEvalError, match="Let expression structure is incorrect"):
            menai.evaluate("(let ((x 1)))")  # Missing body

    def test_invalid_let_binding_syntax_parse_error(self, menai):
        """Test that invalid let binding syntax causes evaluation errors (pure list approach)."""
        with pytest.raises(MenaiEvalError, match="Let binding .* has wrong number of elements"):
            menai.evaluate("(let ((x)) x)")  # Binding without value

        with pytest.raises(MenaiEvalError, match="Let binding .* has wrong number of elements"):
            menai.evaluate("(let ((x 1 2)) x)")  # Binding with too many elements

    def test_invalid_let_binding_variable_parse_error(self, menai):
        """Test that invalid let binding variables cause evaluation errors (pure list approach)."""
        with pytest.raises(MenaiEvalError, match="Let binding .* variable must be a symbol"):
            menai.evaluate("(let ((1 5)) 1)")  # Number as variable

        with pytest.raises(MenaiEvalError, match="Let binding .* variable must be a symbol"):
            menai.evaluate('(let (("x" 5)) x)')  # String as variable

    def test_duplicate_let_binding_variables_parse_error(self, menai):
        """Test that duplicate let binding variables cause evaluation errors (pure list approach)."""
        with pytest.raises(MenaiEvalError, match="Let binding variables must be unique"):
            menai.evaluate("(let ((x 1) (x 2)) x)")

        with pytest.raises(MenaiEvalError, match="Let binding variables must be unique"):
            menai.evaluate("(let ((x 1) (y 2) (x 3)) (integer+ x y))")

    # ========== Evaluation Errors ==========

    def test_undefined_variable_eval_error(self, menai):
        """Test that undefined variables cause evaluation errors."""
        with pytest.raises(MenaiEvalError, match="Undefined variable"):
            menai.evaluate("undefined-var")

        with pytest.raises(MenaiEvalError, match="Undefined variable"):
            menai.evaluate("(integer+ 1 undefined-var)")

    def test_undefined_function_eval_error(self, menai):
        """Test that undefined functions cause evaluation errors."""
        # Evaluator says "Unknown function", VM says "Undefined variable"
        with pytest.raises(MenaiEvalError, match="(Unknown function|Undefined variable)"):
            menai.evaluate("(unknown-op 1 2)")

        with pytest.raises(MenaiEvalError, match="(Unknown function|Undefined variable)"):
            menai.evaluate("(invalid-function)")

    def test_division_by_zero_eval_error(self, menai):
        """Test that division by zero causes evaluation errors."""
        with pytest.raises(MenaiEvalError, match="Division by zero"):
            menai.evaluate("(integer/ 1 0)")

        with pytest.raises(MenaiEvalError, match="Division by zero"):
            menai.evaluate("(integer/ 5 0)")

        with pytest.raises(MenaiEvalError, match="Division by zero"):
            menai.evaluate("(float// 1.0 0.0)")

        with pytest.raises(MenaiEvalError, match="Modulo by zero"):
            menai.evaluate("(integer% 1 0)")

        with pytest.raises(MenaiEvalError, match="Modulo by zero"):
            menai.evaluate("(float% 1.0 0.0)")

    def test_type_mismatch_eval_errors(self, menai):
        """Test that type mismatches cause evaluation errors."""
        # Arithmetic with non-numbers
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(integer+ 1 "hello")')

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer* 2 #t)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer- 5 (list 1 2))")

        # Boolean operations with non-boolean conditions
        # (and/or are lowered to if-chains; only condition args are type-checked)
        with pytest.raises(MenaiEvalError, match="must be boolean"):
            menai.evaluate('(or "hello" #f)')

        # String operations with non-strings
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(string-length 42)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate('(string-concat "hello" 42)')

    def test_arity_mismatch_eval_errors(self, menai):
        """Test that arity mismatches cause evaluation errors."""
        # Too few arguments
        with pytest.raises(MenaiEvalError, match="has wrong number of arguments"):
            menai.evaluate("(integer-abs)")

        with pytest.raises(MenaiEvalError, match="has wrong number of arguments"):
            menai.evaluate("(string-length)")

        # Too many arguments
        with pytest.raises(MenaiEvalError, match="has wrong number of arguments"):
            menai.evaluate("(integer-abs 1 2)")

        with pytest.raises(MenaiEvalError, match="has wrong number of arguments"):
            menai.evaluate("(boolean-not #t #f)")

        # Minimum argument requirements
        with pytest.raises(MenaiEvalError, match="has wrong number of arguments"):
            menai.evaluate("(integer=?)")

        with pytest.raises(MenaiEvalError, match="has wrong number of arguments"):
            menai.evaluate("(integer<?)")

    def test_lambda_function_arity_eval_errors(self, menai):
        """Test that lambda function arity mismatches cause evaluation errors."""
        # Too few arguments
        with pytest.raises(MenaiEvalError, match="expects .* arguments, got .*"):
            menai.evaluate("((lambda (x y) (integer+ x y)) 5)")

        # Too many arguments
        with pytest.raises(MenaiEvalError, match="expects .* argument.*, got .*"):
            menai.evaluate("((lambda (x) x) 1 2 3)")

        # No parameters but arguments provided
        with pytest.raises(MenaiEvalError, match="expects 0 arguments, got .*"):
            menai.evaluate("((lambda () 42) 5)")

    def test_list_operation_type_errors(self, menai):
        """Test that list operations with wrong types cause evaluation errors."""
        # List operations on non-lists
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(list-first "hello")')

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-rest 42)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(list-length #t)")

        # Operations requiring lists as specific arguments
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(list-prepend "hello" 1)')  # Second arg must be list

        with pytest.raises(MenaiEvalError):
            menai.evaluate('(list-concat (list 1 2) "hello")')  # All args must be lists

    def test_empty_list_access_errors(self, menai):
        """Test that accessing empty lists causes evaluation errors."""
        with pytest.raises(MenaiEvalError, match="Cannot get first element of empty list"):
            menai.evaluate("(list-first (list))")

        with pytest.raises(MenaiEvalError, match="Cannot get rest of empty list"):
            menai.evaluate("(list-rest (list))")

    def test_index_out_of_range_errors(self, menai):
        """Test that index out of range causes evaluation errors."""
        # List index out of range
        with pytest.raises(MenaiEvalError, match="index out of range"):
            menai.evaluate("(list-ref (list 1 2 3) 3)")

        with pytest.raises(MenaiEvalError, match="index out of range"):
            menai.evaluate("(list-ref (list 1 2 3) -1)")

        # String index out of range
        with pytest.raises(MenaiEvalError, match="index out of range"):
            menai.evaluate('(string-ref "hello" 5)')

        with pytest.raises(MenaiEvalError, match="index out of range"):
            menai.evaluate('(string-ref "hello" -1)')

    def test_conditional_type_errors(self, menai):
        """Test that conditional expressions with wrong types cause evaluation errors."""
        # If condition must be boolean
        with pytest.raises(MenaiEvalError, match="must be boolean"):
            menai.evaluate('(if 1 "yes" "no")')

        with pytest.raises(MenaiEvalError, match="must be boolean"):
            menai.evaluate('(if "hello" "yes" "no")')

        with pytest.raises(MenaiEvalError, match="must be boolean"):
            menai.evaluate("(if (list 1 2) \"yes\" \"no\")")

    def test_string_conversion_errors(self, menai):
        """Test that string conversion errors are detected."""
        # Invalid string to number conversion
        assert menai.evaluate('(string->number "hello")') == None
        assert menai.evaluate('(string->number "12.34.56")') == None
        assert menai.evaluate('(string->number "")') == None

    def test_complex_number_restriction_errors(self, menai):
        """Test that operations restricted to real numbers reject complex numbers."""
        # Rounding functions don't support complex numbers
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(round (float->complex 1 2))")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(floor j)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(ceil (float->complex 3 4))")

    def test_integer_only_operation_errors(self, menai):
        """Test that integer-only operations reject non-integers."""
        # Bitwise operations require integers
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(bit-or 1.5 2)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(bit-and 1 2.5)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(bit-xor (float->complex 1 2) 3)")

        # Base conversion requires integers
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(bin 3.14)")

        with pytest.raises(MenaiEvalError):
            menai.evaluate("(hex 2.5)")

    def test_higher_order_function_errors(self, menai):
        """Test that higher-order function errors are detected."""
        # Map/filter/fold predicates must return appropriate types
        with pytest.raises(MenaiEvalError, match="condition must be boolean"):
            menai.evaluate('(filter-list (lambda (x) x) (list 1 2 3))')

        with pytest.raises(MenaiEvalError, match="condition must be boolean"):
            menai.evaluate('(any-list? (lambda (x) "hello") (list 1 2 3))')

        # Higher-order functions require list arguments
        with pytest.raises(MenaiEvalError, match="requires list argument"):
            menai.evaluate('(map-list (lambda (x) x) 42)')

        with pytest.raises(MenaiEvalError, match="requires list argument"):
            menai.evaluate('(filter-list (lambda (x) #t) "hello")')

    def test_non_function_call_error(self, menai):
        """Test that trying to call non-functions causes evaluation errors."""
        # Can't call numbers
        with pytest.raises(MenaiEvalError, match="Cannot call non-function value"):
            menai.evaluate("(42 1 2)")

        # Can't call strings
        with pytest.raises(MenaiEvalError, match="Cannot call non-function value"):
            menai.evaluate('("hello" 1 2)')

        # Can't call booleans
        with pytest.raises(MenaiEvalError, match="Cannot call non-function value"):
            menai.evaluate("(#t 1 2)")

    def test_empty_list_evaluation_works(self, menai):
        """Test that empty list evaluation works correctly (no longer an error)."""
        # Empty list should evaluate to itself
        result = menai.evaluate("()")
        assert result == []  # Python representation of empty list

        # Empty list should format correctly
        formatted = menai.evaluate_and_format("()")
        assert formatted == "()"

        # Empty list should work with list operations
        assert menai.evaluate("(list-length ())") == 0
        assert menai.evaluate("(list-null? ())") is True
        assert menai.evaluate("(list? ())") is True

    # ========== Error Message Quality Tests ==========

    def test_error_message_includes_position_info(self, menai):
        """Test that error messages include position information where helpful."""
        # This is a sampling test for error message quality
        try:
            menai.evaluate("(integer+ 1 @)")
        except MenaiTokenError as e:
            assert "list-position" in str(e) or "@" in str(e)

        try:
            menai.evaluate("(integer+ 1 2")
        except MenaiParseError as e:
            assert "list-position" in str(e) or "parenthesis" in str(e)

    def test_error_message_context_information(self, menai):
        """Test that error messages provide helpful context."""
        # Undefined variable error should suggest available bindings
        try:
            menai.evaluate("undefined-var")
        except MenaiEvalError as e:
            error_msg = str(e)
            assert "Undefined variable" in error_msg
            # Should mention available bindings (constants, operators)
            assert "Available variables" in error_msg or "pi" in error_msg

    def test_function_call_error_context(self, menai):
        """Test that function call errors provide parameter context."""
        # Lambda arity error should show expected vs actual
        try:
            menai.evaluate("((lambda (x y) (integer+ x y)) 5)")
        except MenaiEvalError as e:
            error_msg = str(e)
            assert "expects 2 arguments" in error_msg
            assert "got 1" in error_msg

    def test_type_error_context(self, menai):
        """Test that type errors provide helpful context about expected types."""
        # String operation with wrong type
        try:
            menai.evaluate("(string-length 42)")
        except MenaiEvalError as e:
            error_msg = str(e)
            assert "string" in error_msg.lower()

        # Boolean operation with non-boolean condition
        try:
            menai.evaluate('(and "hello" #t)')
        except MenaiEvalError as e:
            error_msg = str(e)
            assert "boolean" in error_msg.lower()

    def test_nested_error_propagation(self, menai):
        """Test that errors in nested expressions are properly propagated."""
        # Error in nested arithmetic
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer+ (integer* 2 3) (integer/ 1 0))")

        # Error in nested function call
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(string-length (integer+ 1 2))")

        # Error in conditional branch (should still be caught despite lazy evaluation)
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(if #t (integer/ 1 0) 42)")

    def test_error_in_higher_order_functions(self, menai):
        """Test error handling in higher-order function contexts."""
        # Error in map function
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(map-list (lambda (x) (integer/ x 0)) (list 1 2 3))")

        # Error in filter predicate
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(filter-list (lambda (x) (integer+ x \"hello\")) (list 1 2 3))")

        # Error in fold function
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(fold-list (lambda (acc x) (integer/ acc x)) 1 (list 1 0 2))")

    def test_error_in_let_binding_evaluation(self, menai):
        """Test error handling in let binding evaluation."""
        # Error in binding expression
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(let ((x (integer/ 1 0))) x)")

        # Error in sequential binding
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(let ((x 5) (y (integer/ x 0))) y)")

    def test_error_in_lambda_closure_context(self, menai):
        """Test error handling in lambda closure contexts."""
        # Error accessing undefined variable in closure
        with pytest.raises(MenaiEvalError, match="Undefined variable"):
            menai.evaluate("(let ((f (lambda (x) (integer+ x undefined-var)))) (f 5))")

        # Type error in closure
        with pytest.raises(MenaiEvalError):
            menai.evaluate('(let ((f (lambda (x) (integer+ x "hello")))) (f 5))')

    # ========== Exception Hierarchy Tests ==========

    def test_exception_inheritance(self):
        """Test that all Menai exceptions inherit properly."""
        # All specific exceptions should inherit from MenaiError
        assert issubclass(MenaiTokenError, MenaiError)
        assert issubclass(MenaiParseError, MenaiError)
        assert issubclass(MenaiEvalError, MenaiError)

        # All Menai errors should inherit from Exception
        assert issubclass(MenaiError, Exception)
        assert issubclass(MenaiTokenError, Exception)
        assert issubclass(MenaiParseError, Exception)
        assert issubclass(MenaiEvalError, Exception)

    def test_exception_can_be_caught_generically(self, menai):
        """Test that all Menai exceptions can be caught with MenaiError."""
        # Token error
        with pytest.raises(MenaiError):
            menai.evaluate("@invalid")

        # Parse error
        with pytest.raises(MenaiError):
            menai.evaluate("(integer+ 1 2")

        # Eval error
        with pytest.raises(MenaiError):
            menai.evaluate("(integer/ 1 0)")

    def test_specific_exception_catching(self, menai):
        """Test that specific exception types can be caught individually."""
        # Catch specific token error
        with pytest.raises(MenaiTokenError):
            menai.evaluate("@invalid")

        # Catch specific parse error
        with pytest.raises(MenaiParseError):
            menai.evaluate("(integer+ 1 2")

        # Catch specific eval error
        with pytest.raises(MenaiEvalError):
            menai.evaluate("(integer/ 1 0)")

    def test_exception_chaining_preservation(self, menai):
        """Test that exception chaining is preserved where appropriate."""
        # This tests that underlying Python exceptions are properly chained
        try:
            menai.evaluate('(string->number "invalid")')
        except MenaiEvalError as e:
            # Should have a __cause__ or __context__ from the underlying ValueError
            assert e.__cause__ is not None or e.__context__ is not None

    def test_error_recovery_not_possible(self, menai):
        """Test that errors properly terminate evaluation."""
        # After an error, the evaluator should be in a clean state for next evaluation
        with pytest.raises(MenaiError):
            menai.evaluate("(integer/ 1 0)")

        # Next evaluation should work normally
        result = menai.evaluate("(integer+ 1 2)")
        assert result == 3

    def test_complex_error_scenarios(self, menai):
        """Test error handling in complex nested scenarios."""
        # Multiple levels of nesting with error deep inside
        complex_expr = '''
        (let ((x 10))
          (let ((f (lambda (y)
                              (if (integer>? y 0)
                        (integer+ x (integer/ y 0))
                        y))))
            (f 5)))
        '''

        with pytest.raises(MenaiEvalError, match="Division by zero"):
            menai.evaluate(complex_expr)

        # Error in deeply nested functional composition
        nested_functional = '''
        (fold-list integer+ 0
              (map-list (lambda (x) (integer/ x 0))
                   (filter-list (lambda (x) (integer>? x 0))
                           (list 1 2 3))))
        '''

        with pytest.raises(MenaiEvalError):
            menai.evaluate(nested_functional)
