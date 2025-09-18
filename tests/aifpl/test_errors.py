"""Tests for error handling and exception reporting."""

import pytest

from aifpl import AIFPL, AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError


class TestErrors:
    """Test error detection and exception handling."""

    # ========== Tokenization Errors ==========

    def test_invalid_character_token_error(self, aifpl):
        """Test that invalid characters cause tokenization errors."""
        with pytest.raises(AIFPLTokenError):
            aifpl.evaluate("@invalid")
        
        with pytest.raises(AIFPLTokenError):
            aifpl.evaluate("hello$world")
        
        with pytest.raises(AIFPLTokenError):
            aifpl.evaluate("(+ 1 & 2)")

    def test_unterminated_string_token_error(self, aifpl):
        """Test that unterminated strings cause tokenization errors."""
        with pytest.raises(AIFPLTokenError, match="Unterminated string literal"):
            aifpl.evaluate('"hello world')
        
        with pytest.raises(AIFPLTokenError, match="Unterminated string literal"):
            aifpl.evaluate('(string-append "hello" "world)')

    def test_invalid_escape_sequence_token_error(self, aifpl):
        """Test that invalid escape sequences cause tokenization errors."""
        with pytest.raises(AIFPLTokenError, match="Invalid escape sequence"):
            aifpl.evaluate('"hello\\q"')  # Invalid escape
        
        with pytest.raises(AIFPLTokenError, match="Invalid escape sequence"):
            aifpl.evaluate('"test\\z"')

    def test_invalid_unicode_escape_token_error(self, aifpl):
        """Test that invalid Unicode escapes cause tokenization errors."""
        with pytest.raises(AIFPLTokenError, match="Invalid Unicode"):
            aifpl.evaluate('"\\uXYZ"')  # Not enough hex digits
        
        with pytest.raises(AIFPLTokenError, match="Invalid Unicode"):
            aifpl.evaluate('"\\uGGGG"')  # Invalid hex digits

    def test_incomplete_unicode_escape_token_error(self, aifpl):
        """Test that incomplete Unicode escapes cause tokenization errors."""
        with pytest.raises(AIFPLTokenError, match="Incomplete Unicode escape"):
            aifpl.evaluate('"\\u12"')  # Too few digits
        
        with pytest.raises(AIFPLTokenError, match="Incomplete Unicode escape"):
            aifpl.evaluate('"\\u"')  # No digits

    def test_invalid_boolean_literal_token_error(self, aifpl):
        """Test that invalid boolean literals cause tokenization errors."""
        with pytest.raises(AIFPLTokenError, match="Invalid boolean literal"):
            aifpl.evaluate("#x")  # Not #t or #f
        
        with pytest.raises(AIFPLTokenError, match="Invalid boolean literal"):
            aifpl.evaluate("#true")  # Must be exactly #t

    def test_invalid_number_format_token_error(self, aifpl):
        """Test that invalid number formats cause tokenization errors."""
        with pytest.raises(AIFPLTokenError, match="Invalid"):
            aifpl.evaluate("0x")  # Hex without digits
        
        with pytest.raises(AIFPLTokenError, match="Invalid"):
            aifpl.evaluate("0b")  # Binary without digits
        
        with pytest.raises(AIFPLTokenError, match="Invalid"):
            aifpl.evaluate("0o")  # Octal without digits

    def test_malformed_scientific_notation_token_error(self, aifpl):
        """Test that malformed scientific notation causes tokenization errors."""
        with pytest.raises(AIFPLTokenError, match="Invalid scientific notation"):
            aifpl.evaluate("1e")  # Missing exponent
        
        with pytest.raises(AIFPLTokenError, match="Invalid scientific notation"):
            aifpl.evaluate("1.5e+")  # Missing exponent after sign

    # ========== Parsing Errors ==========

    def test_empty_expression_parse_error(self, aifpl):
        """Test that empty expressions cause parse errors."""
        with pytest.raises(AIFPLParseError, match="Empty expression"):
            aifpl.evaluate("")
        
        with pytest.raises(AIFPLParseError, match="Empty expression"):
            aifpl.evaluate("   ")  # Whitespace only

    def test_unmatched_parentheses_parse_error(self, aifpl):
        """Test that unmatched parentheses cause parse errors."""
        with pytest.raises(AIFPLParseError, match="Unclosed parenthesis"):
            aifpl.evaluate("(+ 1 2")
        
        with pytest.raises(AIFPLParseError, match="Unclosed parenthesis"):
            aifpl.evaluate("(* (+ 1 2) 3")
        
        with pytest.raises(AIFPLParseError, match="Expected \\("):
            aifpl.evaluate("+ 1 2)")

    def test_unexpected_token_after_expression_parse_error(self, aifpl):
        """Test that extra tokens after complete expression cause parse errors."""
        with pytest.raises(AIFPLParseError, match="Unexpected token after expression"):
            aifpl.evaluate("42 43")
        
        with pytest.raises(AIFPLParseError, match="Unexpected token after expression"):
            aifpl.evaluate("(+ 1 2) (+ 3 4)")

    def test_invalid_lambda_syntax_parse_error(self, aifpl):
        """Test that invalid lambda syntax causes parse errors."""
        with pytest.raises(AIFPLParseError, match="Lambda expression requires exactly 3 elements"):
            aifpl.evaluate("(lambda)")
        
        with pytest.raises(AIFPLParseError, match="Lambda expression requires exactly 3 elements"):
            aifpl.evaluate("(lambda (x))")  # Missing body
        
        with pytest.raises(AIFPLParseError, match="Lambda expression requires exactly 3 elements"):
            aifpl.evaluate("(lambda (x) (+ x 1) extra)")  # Too many elements

    def test_invalid_lambda_parameters_parse_error(self, aifpl):
        """Test that invalid lambda parameters cause parse errors."""
        with pytest.raises(AIFPLParseError, match="Lambda parameter must be a symbol"):
            aifpl.evaluate("(lambda (1) (+ 1 1))")  # Number as parameter
        
        with pytest.raises(AIFPLParseError, match="Lambda parameter must be a symbol"):
            aifpl.evaluate('(lambda ("x") (+ x 1))')  # String as parameter

    def test_duplicate_lambda_parameters_parse_error(self, aifpl):
        """Test that duplicate lambda parameters cause parse errors."""
        with pytest.raises(AIFPLParseError, match="Duplicate lambda parameters"):
            aifpl.evaluate("(lambda (x x) (+ x x))")
        
        with pytest.raises(AIFPLParseError, match="Duplicate lambda parameters"):
            aifpl.evaluate("(lambda (x y x) (+ x y))")

    def test_invalid_let_syntax_parse_error(self, aifpl):
        """Test that invalid let syntax causes parse errors."""
        with pytest.raises(AIFPLParseError, match="Let expression requires exactly 3 elements"):
            aifpl.evaluate("(let)")
        
        with pytest.raises(AIFPLParseError, match="Let expression requires exactly 3 elements"):
            aifpl.evaluate("(let ((x 1)))")  # Missing body

    def test_invalid_let_binding_syntax_parse_error(self, aifpl):
        """Test that invalid let binding syntax causes parse errors."""
        with pytest.raises(AIFPLParseError, match="Let binding must be a list of 2 elements"):
            aifpl.evaluate("(let ((x)) x)")  # Binding without value
        
        with pytest.raises(AIFPLParseError, match="Let binding must be a list of 2 elements"):
            aifpl.evaluate("(let ((x 1 2)) x)")  # Binding with too many elements

    def test_invalid_let_binding_variable_parse_error(self, aifpl):
        """Test that invalid let binding variables cause parse errors."""
        with pytest.raises(AIFPLParseError, match="Let binding variable must be a symbol"):
            aifpl.evaluate("(let ((1 5)) 1)")  # Number as variable
        
        with pytest.raises(AIFPLParseError, match="Let binding variable must be a symbol"):
            aifpl.evaluate('(let (("x" 5)) x)')  # String as variable

    def test_duplicate_let_binding_variables_parse_error(self, aifpl):
        """Test that duplicate let binding variables cause parse errors."""
        with pytest.raises(AIFPLParseError, match="Duplicate let binding variables"):
            aifpl.evaluate("(let ((x 1) (x 2)) x)")
        
        with pytest.raises(AIFPLParseError, match="Duplicate let binding variables"):
            aifpl.evaluate("(let ((x 1) (y 2) (x 3)) (+ x y))")

    # ========== Evaluation Errors ==========

    def test_undefined_variable_eval_error(self, aifpl):
        """Test that undefined variables cause evaluation errors."""
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("undefined-var")
        
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("(+ 1 undefined-var)")

    def test_undefined_operator_eval_error(self, aifpl):
        """Test that undefined operators cause evaluation errors."""
        with pytest.raises(AIFPLEvalError, match="Unknown operator"):
            aifpl.evaluate("(unknown-op 1 2)")
        
        with pytest.raises(AIFPLEvalError, match="Unknown operator"):
            aifpl.evaluate("(invalid-function)")

    def test_division_by_zero_eval_error(self, aifpl):
        """Test that division by zero causes evaluation errors."""
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(/ 1 0)")
        
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(/ 5 2 0)")
        
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate("(// 1 0)")
        
        with pytest.raises(AIFPLEvalError, match="Modulo by zero"):
            aifpl.evaluate("(% 1 0)")

    def test_type_mismatch_eval_errors(self, aifpl):
        """Test that type mismatches cause evaluation errors."""
        # Arithmetic with non-numbers
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(+ 1 "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(* 2 #t)")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(- 5 (list 1 2))")
        
        # Boolean operations with non-booleans
        with pytest.raises(AIFPLEvalError, match="requires boolean arguments"):
            aifpl.evaluate("(and #t 1)")
        
        with pytest.raises(AIFPLEvalError, match="requires boolean arguments"):
            aifpl.evaluate('(or "hello" #f)')
        
        # String operations with non-strings
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(string-length 42)")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(string-append "hello" 42)')

    def test_arity_mismatch_eval_errors(self, aifpl):
        """Test that arity mismatches cause evaluation errors."""
        # Too few arguments
        with pytest.raises(AIFPLEvalError, match="takes exactly .* arguments"):
            aifpl.evaluate("(abs)")
        
        with pytest.raises(AIFPLEvalError, match="takes exactly .* arguments"):
            aifpl.evaluate("(string-length)")
        
        # Too many arguments
        with pytest.raises(AIFPLEvalError, match="takes exactly .* arguments"):
            aifpl.evaluate("(abs 1 2)")
        
        with pytest.raises(AIFPLEvalError, match="takes exactly .* arguments"):
            aifpl.evaluate("(not #t #f)")
        
        # Minimum argument requirements
        with pytest.raises(AIFPLEvalError, match="requires at least .* arguments"):
            aifpl.evaluate("(=)")
        
        with pytest.raises(AIFPLEvalError, match="requires at least .* arguments"):
            aifpl.evaluate("(<)")

    def test_lambda_function_arity_eval_errors(self, aifpl):
        """Test that lambda function arity mismatches cause evaluation errors."""
        # Too few arguments
        with pytest.raises(AIFPLEvalError, match="expects .* arguments, got .*"):
            aifpl.evaluate("((lambda (x y) (+ x y)) 5)")
        
        # Too many arguments
        with pytest.raises(AIFPLEvalError, match="expects .* arguments, got .*"):
            aifpl.evaluate("((lambda (x) x) 1 2 3)")
        
        # No parameters but arguments provided
        with pytest.raises(AIFPLEvalError, match="expects 0 arguments, got .*"):
            aifpl.evaluate("((lambda () 42) 5)")

    def test_list_operation_type_errors(self, aifpl):
        """Test that list operations with wrong types cause evaluation errors."""
        # List operations on non-lists
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(first "hello")')
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(rest 42)")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(length #t)")
        
        # Operations requiring lists as specific arguments
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(cons 1 "hello")')  # Second arg must be list
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(append (list 1 2) "hello")')  # All args must be lists

    def test_empty_list_access_errors(self, aifpl):
        """Test that accessing empty lists causes evaluation errors."""
        with pytest.raises(AIFPLEvalError, match="Cannot get first element of empty list"):
            aifpl.evaluate("(first (list))")
        
        with pytest.raises(AIFPLEvalError, match="Cannot get rest of empty list"):
            aifpl.evaluate("(rest (list))")

    def test_index_out_of_range_errors(self, aifpl):
        """Test that index out of range causes evaluation errors."""
        # List index out of range
        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate("(list-ref (list 1 2 3) 3)")
        
        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate("(list-ref (list 1 2 3) -1)")
        
        # String index out of range
        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate('(string-ref "hello" 5)')
        
        with pytest.raises(AIFPLEvalError, match="index out of range"):
            aifpl.evaluate('(string-ref "hello" -1)')

    def test_conditional_type_errors(self, aifpl):
        """Test that conditional expressions with wrong types cause evaluation errors."""
        # If condition must be boolean
        with pytest.raises(AIFPLEvalError, match="requires boolean condition"):
            aifpl.evaluate('(if 1 "yes" "no")')
        
        with pytest.raises(AIFPLEvalError, match="requires boolean condition"):
            aifpl.evaluate('(if "hello" "yes" "no")')
        
        with pytest.raises(AIFPLEvalError, match="requires boolean condition"):
            aifpl.evaluate("(if (list 1 2) \"yes\" \"no\")")

    def test_string_conversion_errors(self, aifpl):
        """Test that string conversion errors are detected."""
        # Invalid string to number conversion
        with pytest.raises(AIFPLEvalError, match="Cannot convert string to number"):
            aifpl.evaluate('(string->number "hello")')
        
        with pytest.raises(AIFPLEvalError, match="Cannot convert string to number"):
            aifpl.evaluate('(string->number "12.34.56")')
        
        with pytest.raises(AIFPLEvalError, match="Cannot convert string to number"):
            aifpl.evaluate('(string->number "")')

    def test_complex_number_restriction_errors(self, aifpl):
        """Test that operations restricted to real numbers reject complex numbers."""
        # Rounding functions don't support complex numbers
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(round (complex 1 2))")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(floor j)")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(ceil (complex 3 4))")

    def test_integer_only_operation_errors(self, aifpl):
        """Test that integer-only operations reject non-integers."""
        # Bitwise operations require integers
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bit-or 1.5 2)")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bit-and 1 2.5)")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bit-xor (complex 1 2) 3)")
        
        # Base conversion requires integers
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(bin 3.14)")
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(hex 2.5)")

    def test_higher_order_function_errors(self, aifpl):
        """Test that higher-order function errors are detected."""
        # Map/filter/fold predicates must return appropriate types
        with pytest.raises(AIFPLEvalError, match="must return boolean"):
            aifpl.evaluate('(filter (lambda (x) x) (list 1 2 3))')
        
        with pytest.raises(AIFPLEvalError, match="must return boolean"):
            aifpl.evaluate('(any? (lambda (x) "hello") (list 1 2 3))')
        
        # Higher-order functions require list arguments
        with pytest.raises(AIFPLEvalError, match="requires list"):
            aifpl.evaluate('(map (lambda (x) x) 42)')
        
        with pytest.raises(AIFPLEvalError, match="requires list"):
            aifpl.evaluate('(filter (lambda (x) #t) "hello")')

    def test_recursion_depth_limit_error(self, aifpl_custom):
        """Test that recursion depth limits are enforced."""
        # Create AIFPL with very low depth limit
        aifpl_shallow = aifpl_custom(max_depth=5)
        
        # Deep recursion should cause error
        deep_expr = "(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1))))))"
        
        with pytest.raises(AIFPLEvalError, match="too deeply nested"):
            aifpl_shallow.evaluate(deep_expr)

    def test_non_function_call_error(self, aifpl):
        """Test that trying to call non-functions causes evaluation errors."""
        # Can't call numbers
        with pytest.raises(AIFPLEvalError, match="Cannot call non-function"):
            aifpl.evaluate("(42 1 2)")
        
        # Can't call strings
        with pytest.raises(AIFPLEvalError, match="Cannot call non-function"):
            aifpl.evaluate('("hello" 1 2)')
        
        # Can't call booleans
        with pytest.raises(AIFPLEvalError, match="Cannot call non-function"):
            aifpl.evaluate("(#t 1 2)")

    def test_empty_list_evaluation_error(self, aifpl):
        """Test that empty list evaluation causes error."""
        with pytest.raises(AIFPLEvalError, match="Cannot evaluate empty list"):
            aifpl.evaluate("()")

    # ========== Error Message Quality Tests ==========

    def test_error_message_includes_position_info(self, aifpl):
        """Test that error messages include position information where helpful."""
        # This is a sampling test for error message quality
        try:
            aifpl.evaluate("(+ 1 @)")
        except AIFPLTokenError as e:
            assert "position" in str(e) or "@" in str(e)
        
        try:
            aifpl.evaluate("(+ 1 2")
        except AIFPLParseError as e:
            assert "position" in str(e) or "parenthesis" in str(e)

    def test_error_message_context_information(self, aifpl):
        """Test that error messages provide helpful context."""
        # Undefined variable error should suggest available bindings
        try:
            aifpl.evaluate("undefined-var")
        except AIFPLEvalError as e:
            error_msg = str(e)
            assert "Undefined variable" in error_msg
            # Should mention available bindings (constants, operators)
            assert "Available bindings" in error_msg or "pi" in error_msg

    def test_function_call_error_context(self, aifpl):
        """Test that function call errors provide parameter context."""
        # Lambda arity error should show expected vs actual
        try:
            aifpl.evaluate("((lambda (x y) (+ x y)) 5)")
        except AIFPLEvalError as e:
            error_msg = str(e)
            assert "expects 2 arguments" in error_msg
            assert "got 1" in error_msg

    def test_type_error_context(self, aifpl):
        """Test that type errors provide helpful context about expected types."""
        # String operation with wrong type
        try:
            aifpl.evaluate("(string-length 42)")
        except AIFPLEvalError as e:
            error_msg = str(e)
            assert "string" in error_msg.lower()
        
        # Boolean operation with wrong type
        try:
            aifpl.evaluate("(and #t 1)")
        except AIFPLEvalError as e:
            error_msg = str(e)
            assert "boolean" in error_msg.lower()

    def test_nested_error_propagation(self, aifpl):
        """Test that errors in nested expressions are properly propagated."""
        # Error in nested arithmetic
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(+ (* 2 3) (/ 1 0))")
        
        # Error in nested function call
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(string-length (+ 1 2))")
        
        # Error in conditional branch (should still be caught despite lazy evaluation)
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(if #t (/ 1 0) 42)")

    def test_error_in_higher_order_functions(self, aifpl):
        """Test error handling in higher-order function contexts."""
        # Error in map function
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(map (lambda (x) (/ x 0)) (list 1 2 3))")
        
        # Error in filter predicate
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(filter (lambda (x) (+ x \"hello\")) (list 1 2 3))")
        
        # Error in fold function
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(fold (lambda (acc x) (/ acc x)) 1 (list 1 0 2))")

    def test_error_in_let_binding_evaluation(self, aifpl):
        """Test error handling in let binding evaluation."""
        # Error in binding expression
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(let ((x (/ 1 0))) x)")
        
        # Error in sequential binding
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(let ((x 5) (y (/ x 0))) y)")

    def test_error_in_lambda_closure_context(self, aifpl):
        """Test error handling in lambda closure contexts."""
        # Error accessing undefined variable in closure
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("(let ((f (lambda (x) (+ x undefined-var)))) (f 5))")
        
        # Type error in closure
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate('(let ((f (lambda (x) (+ x "hello")))) (f 5))')

    # ========== Exception Hierarchy Tests ==========

    def test_exception_inheritance(self):
        """Test that all AIFPL exceptions inherit properly."""
        # All specific exceptions should inherit from AIFPLError
        assert issubclass(AIFPLTokenError, AIFPLError)
        assert issubclass(AIFPLParseError, AIFPLError)
        assert issubclass(AIFPLEvalError, AIFPLError)
        
        # All AIFPL errors should inherit from Exception
        assert issubclass(AIFPLError, Exception)
        assert issubclass(AIFPLTokenError, Exception)
        assert issubclass(AIFPLParseError, Exception)
        assert issubclass(AIFPLEvalError, Exception)

    def test_exception_can_be_caught_generically(self, aifpl):
        """Test that all AIFPL exceptions can be caught with AIFPLError."""
        # Token error
        with pytest.raises(AIFPLError):
            aifpl.evaluate("@invalid")
        
        # Parse error
        with pytest.raises(AIFPLError):
            aifpl.evaluate("(+ 1 2")
        
        # Eval error
        with pytest.raises(AIFPLError):
            aifpl.evaluate("(/ 1 0)")

    def test_specific_exception_catching(self, aifpl):
        """Test that specific exception types can be caught individually."""
        # Catch specific token error
        with pytest.raises(AIFPLTokenError):
            aifpl.evaluate("@invalid")
        
        # Catch specific parse error
        with pytest.raises(AIFPLParseError):
            aifpl.evaluate("(+ 1 2")
        
        # Catch specific eval error
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("(/ 1 0)")

    def test_exception_chaining_preservation(self, aifpl):
        """Test that exception chaining is preserved where appropriate."""
        # This tests that underlying Python exceptions are properly chained
        try:
            aifpl.evaluate('(string->number "invalid")')
        except AIFPLEvalError as e:
            # Should have a __cause__ or __context__ from the underlying ValueError
            assert e.__cause__ is not None or e.__context__ is not None

    def test_error_recovery_not_possible(self, aifpl):
        """Test that errors properly terminate evaluation."""
        # After an error, the evaluator should be in a clean state for next evaluation
        with pytest.raises(AIFPLError):
            aifpl.evaluate("(/ 1 0)")
        
        # Next evaluation should work normally
        result = aifpl.evaluate("(+ 1 2)")
        assert result == 3

    def test_complex_error_scenarios(self, aifpl):
        """Test error handling in complex nested scenarios."""
        # Multiple levels of nesting with error deep inside
        complex_expr = '''
        (let ((x 10))
          (let ((f (lambda (y) 
                    (if (> y 0) 
                        (+ x (/ y 0))  ; Error here
                        y))))
            (f 5)))
        '''
        
        with pytest.raises(AIFPLEvalError, match="Division by zero"):
            aifpl.evaluate(complex_expr)
        
        # Error in deeply nested functional composition
        nested_functional = '''
        (fold + 0
              (map (lambda (x) (/ x 0))  ; Error in map function
                   (filter (lambda (x) (> x 0))
                           (list 1 2 3))))
        '''
        
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate(nested_functional)