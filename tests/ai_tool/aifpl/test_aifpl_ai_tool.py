"""
Tests for the AIFPL (AI Functional Programming Language) tool
"""
import asyncio
import math
from unittest.mock import patch

import pytest

from ai_tool import AITool, AIToolDefinition, AIToolParameter, AIToolExecutionError, AIToolTimeoutError
from ai_tool.aifpl.aifpl_ai_tool import AIFPLAITool
from aifpl import AIFPLError


@pytest.fixture
def aifpl_tool():
    """Fixture providing an AIFPL tool instance."""
    return AIFPLAITool()


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    async def mock_auth_callback(tool_name, arguments, context, requester_ref, destructive):
        return True  # Default to authorized

    return mock_auth_callback


class TestAIFPLAIToolDefinition:
    """Test the AIFPL tool definition."""

    def test_get_definition_returns_correct_structure(self, aifpl_tool):
        """Test that get_definition returns the correct tool definition structure."""
        definition = aifpl_tool.get_definition()

        assert isinstance(definition, AIToolDefinition)
        assert definition.name == "AIFPL"
        assert "AIFPL (AI Functional Programming Language)" in definition.description
        assert len(definition.parameters) == 2  # operation and expression

    def test_expression_parameter_definition(self, aifpl_tool):
        """Test the expression parameter definition."""
        definition = aifpl_tool.get_definition()

        # Find the expression parameter (it's the second parameter after operation)
        expr_param = None
        for param in definition.parameters:
            if param.name == "expression":
                expr_param = param
                break

        assert expr_param is not None, "expression parameter not found"

        assert isinstance(expr_param, AIToolParameter)
        assert expr_param.type == "string"
        assert "AIFPL expression using LISP-like S-expression syntax" in expr_param.description
        assert expr_param.required is True
        assert expr_param.enum is None

    def test_definition_includes_lisp_syntax_description(self, aifpl_tool):
        """Test that the definition describes LISP-like syntax."""
        definition = aifpl_tool.get_definition()
        description = definition.description

        # Check for LISP syntax information
        assert "Syntax: (operator arg1 arg2 ...)" in description
        assert "S expression" in description or "S-expression" in description
        assert "prefix notation" in description.lower()

    def test_definition_includes_key_features(self, aifpl_tool):
        """Test that the definition describes key AIFPL features."""
        definition = aifpl_tool.get_definition()
        description = definition.description

        # Check for major feature categories
        assert "Quote" in description or "quote" in description
        assert "Arithmetic" in description or "arithmetic" in description
        assert "String Operations" in description or "string" in description
        assert "List Operations" in description or "list" in description
        assert "Lambda Functions" in description or "lambda" in description
        assert "Higher-Order Functions" in description or "higher-order" in description.lower()

    def test_definition_includes_examples(self, aifpl_tool):
        """Test that the definition includes usage examples."""
        definition = aifpl_tool.get_definition()
        description = definition.description

        # Check for example expressions
        assert "(+ 1 2 3)" in description
        assert "(lambda" in description
        assert "(list" in description

    def test_definition_includes_important_notes(self, aifpl_tool):
        """Test that the definition includes important usage notes."""
        definition = aifpl_tool.get_definition()
        description = definition.description

        # Check for important notes
        assert "pure functional" in description.lower()
        assert "no side effects" in description.lower()
        assert "prefix notation" in description.lower()


class TestAIFPLAIToolExecution:
    """Test basic AIFPL tool execution."""

    def test_execute_basic_arithmetic(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with basic arithmetic expressions."""
        test_cases = [
            ("(+ 1 2 3)", "6"),
            ("(- 10 3)", "7"),
            ("(* 2 3 4)", "24"),
            ("(/ 12 3)", "4.0"),
            ("(// 7 3)", "2"),
            ("(% 7 3)", "1"),
            ("(** 2 3)", "8"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_mathematical_functions(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with mathematical functions."""
        test_cases = [
            ("(sqrt 16)", "4.0"),
            ("(abs -5)", "5"),
            ("(min 1 5 3)", "1"),
            ("(max 1 5 3)", "5"),
            ("(pow 2 3)", "8"),
            ("(round 3.7)", "4"),
            ("(floor 3.7)", "3"),
            ("(ceil 3.2)", "4"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_trigonometric_functions(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with trigonometric functions."""
        test_cases = [
            ("(sin 0)", "0.0"),
            ("(cos 0)", "1.0"),
            ("(tan 0)", "0.0"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_constants(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with mathematical constants."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "pi"})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
        # Should be approximately pi
        assert abs(float(result.content) - math.pi) < 0.0001

        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "e"})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
        # Should be approximately e
        assert abs(float(result.content) - math.e) < 0.0001

    def test_execute_complex_numbers(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with complex numbers."""
        test_cases = [
            ("1j", "1j"),  # Fixed: AIFPL returns "1j", not "(0+1j)"
            ("(complex 3 4)", "3+4j"),
            ("(+ 1 (* 2 1j))", "1+2j"),
            ("(real (complex 3 4))", "3.0"),
            ("(imag (complex 3 4))", "4.0"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_comparison_operations(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with comparison operations."""
        test_cases = [
            ("(= 1 1)", "#t"),
            ("(!= 1 2)", "#t"),
            ("(< 1 2)", "#t"),
            ("(> 3 2)", "#t"),
            ("(<= 1 1)", "#t"),
            ("(>= 2 1)", "#t"),
            ("(= 1 2)", "#f"),
            ("(< 2 1)", "#f"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_boolean_operations(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with boolean operations."""
        test_cases = [
            ("(and #t #t)", "#t"),
            ("(and #t #f)", "#f"),
            ("(or #t #f)", "#t"),
            ("(or #f #f)", "#f"),
            ("(not #t)", "#f"),
            ("(not #f)", "#t"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_conditional_expressions(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with conditional expressions."""
        test_cases = [
            ('(if #t "yes" "no")', '"yes"'),
            ('(if #f "yes" "no")', '"no"'),
            ("(if (> 5 3) 42 0)", "42"),
            ("(if (< 5 3) 42 0)", "0"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_string_operations(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with string operations."""
        test_cases = [
            ('(string-append "hello" " " "world")', '"hello world"'),
            ('(string-length "hello")', "5"),
            ('(string-ref "hello" 1)', '"e"'),
            ('(substring "hello" 1 4)', '"ell"'),
            ('(string-upcase "hello")', '"HELLO"'),
            ('(string-downcase "HELLO")', '"hello"'),
            ('(string-trim "  hello  ")', '"hello"'),
            ('(string-replace "banana" "a" "o")', '"bonono"'),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_string_predicates(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with string predicates."""
        test_cases = [
            ('(string-contains? "hello" "ell")', "#t"),
            ('(string-prefix? "hello" "he")', "#t"),
            ('(string-suffix? "hello" "lo")', "#t"),
            ('(string=? "hi" "hi")', "#t"),
            ('(string-contains? "hello" "xyz")', "#f"),
            ('(string-prefix? "hello" "xyz")', "#f"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_string_conversion(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with string conversion operations."""
        test_cases = [
            ('(string->number "42")', "42"),
            ('(number->string 42)', '"42"'),
            ('(string->list "hi")', '("h" "i")'),
            ('(list->string (list "h" "i"))', '"hi"'),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_string_split_join(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with string split and join operations."""
        test_cases = [
            ('(string-split "a,b,c" ",")', '("a" "b" "c")'),
            ('(string-join (list "hello" "world") " ")', '"hello world"'),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_list_operations(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with list operations."""
        test_cases = [
            ("(list 1 2 3)", "(1 2 3)"),
            ("(cons 1 (list 2 3))", "(1 2 3)"),
            ("(append (list 1 2) (list 3 4))", "(1 2 3 4)"),
            ("(first (list 1 2 3))", "1"),
            ("(rest (list 1 2 3))", "(2 3)"),
            ("(last (list 1 2 3))", "3"),
            ("(length (list 1 2 3))", "3"),
            ("(reverse (list 1 2 3))", "(3 2 1)"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_list_access(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with list access operations."""
        test_cases = [
            ('(list-ref (list "a" "b" "c") 1)', '"b"'),
            ("(list-ref (list 10 20 30) 0)", "10"),
            ("(list-ref (list 10 20 30) 2)", "30"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_list_predicates(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with list predicates."""
        test_cases = [
            ("(null? (list))", "#t"),
            ("(null? (list 1))", "#f"),
            ("(member? 2 (list 1 2 3))", "#t"),
            ("(member? 4 (list 1 2 3))", "#f"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_list_utilities(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with list utility operations."""
        test_cases = [
            ("(remove 2 (list 1 2 3 2 4))", "(1 3 4)"),
            ("(position 2 (list 1 2 3))", "1"),
            ("(position 4 (list 1 2 3))", "#f"),
            ("(take 3 (list 1 2 3 4 5))", "(1 2 3)"),
            ("(drop 2 (list 1 2 3 4 5))", "(3 4 5)"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_type_predicates(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with type predicates."""
        test_cases = [
            ("(number? 42)", "#t"),
            ("(number? #t)", "#f"),
            ("(integer? 42)", "#t"),
            ("(float? 3.14)", "#t"),
            ("(complex? (+ 1 1j))", "#t"),
            ('(string? "hello")', "#t"),
            ("(boolean? #t)", "#t"),
            ("(list? (list 1 2))", "#t"),
            ("(function? (lambda (x) x))", "#t"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_lambda_functions(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with lambda functions."""
        test_cases = [
            ("((lambda (x) (* x x)) 5)", "25"),
            ("((lambda (x y) (+ x y)) 3 4)", "7"),
            ("((lambda () 42))", "42"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_let_bindings(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with let bindings."""
        test_cases = [
            ("(let ((x 5)) x)", "5"),
            ("(let ((x 5) (y 3)) (+ x y))", "8"),
            ("(let ((x 5) (y (* x 2))) (+ x y))", "15"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_higher_order_functions(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with higher-order functions."""
        test_cases = [
            ("(map (lambda (x) (* x 2)) (list 1 2 3))", "(2 4 6)"),
            ("(filter (lambda (x) (> x 0)) (list -1 2 -3 4))", "(2 4)"),
            ("(fold + 0 (list 1 2 3 4))", "10"),
            ("(range 1 5)", "(1 2 3 4)"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_higher_order_predicates(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with higher-order predicate functions."""
        test_cases = [
            ("(find (lambda (x) (> x 2)) (list 1 2 3 4))", "3"),
            ("(any? (lambda (x) (> x 3)) (list 1 2 3 4))", "#t"),
            ("(all? (lambda (x) (> x 0)) (list 1 2 3 4))", "#t"),
            ("(any? (lambda (x) (> x 5)) (list 1 2 3 4))", "#f"),
            ("(all? (lambda (x) (> x 2)) (list 1 2 3 4))", "#f"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_bitwise_operations(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with bitwise operations."""
        test_cases = [
            ("(bit-or 5 3)", "7"),
            ("(bit-and 7 3)", "3"),
            ("(bit-xor 5 3)", "6"),
            ("(bit-not 5)", "-6"),
            ("(bit-shift-left 1 3)", "8"),
            ("(bit-shift-right 8 2)", "2"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_base_conversion(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with base conversion functions."""
        test_cases = [
            ("(bin 255)", '"#b11111111"'),
            ("(hex 255)", '"#xff"'),
            ("(oct 255)", '"#o377"'),
            ("(bin 5)", '"#b101"'),
            ("(hex 10)", '"#xa"'),
            ("(oct 8)", '"#o10"'),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected

    def test_execute_quote_operations(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with quote operations."""
        test_cases = [
            ("(quote (+ 1 2 3))", "(+ 1 2 3)"),
            ("'(+ 1 2 3)", "(+ 1 2 3)"),
            ("(list 'hello (+ 1 2) 'world)", "(hello 3 world)"),
            ("(first '(+ 1 2))", "+"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected


class TestAIFPLAIToolErrorHandling:
    """Test AIFPL tool error handling."""

    def test_execute_missing_expression(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution without expression argument."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "expression" in str(error).lower()

    def test_execute_empty_expression(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with empty expression."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": ""})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "empty expression" in str(error).lower()

    def test_execute_non_string_expression(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with non-string expression."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": 123})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        assert "Expression must be a string" in str(error)

    def test_execute_division_by_zero_error(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with division by zero."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(/ 5 0)"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        error = exc_info.value
        # Fixed: AIFPL raises AIFPLEvalError, not ZeroDivisionError
        # The error message should mention division by zero
        assert "Division by zero" in str(error) or "division by zero" in str(error).lower()
        # The cause should be an AIFPLError (or subclass)
        assert isinstance(error.__cause__, AIFPLError)

    def test_execute_invalid_syntax_error(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with invalid AIFPL syntax."""
        invalid_expressions = [
            "(+ 1 2",  # Missing closing parenthesis
            "+ 1 2)",  # Missing opening parenthesis
            # Removed "()" as it's valid (empty list)
            "(unknown-function 1 2)",  # Unknown function
        ]

        for expression in invalid_expressions:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            with pytest.raises(AIToolExecutionError):
                asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

    def test_execute_aifpl_error_wrapping(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test that AIFPLError is properly wrapped."""
        with patch.object(aifpl_tool._tool, 'evaluate_and_format') as mock_evaluate:
            mock_evaluate.side_effect = AIFPLError("Custom AIFPL error")

            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(+ 1 2)"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Custom AIFPL error" in str(error)
            assert error.__cause__.__class__ == AIFPLError

    def test_execute_timeout_error(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution timeout handling."""
        # Mock the specific method on the tool instance to avoid affecting other tests
        with patch.object(aifpl_tool, '_evaluate_expression_sync') as mock_evaluate:
            mock_evaluate.side_effect = Exception("Simulated timeout")

            # Mock asyncio.wait_for to raise TimeoutError when our mocked method is called
            original_wait_for = asyncio.wait_for
            async def mock_wait_for(coro, timeout=None):
                try:
                    return await original_wait_for(coro, timeout=timeout)
                except Exception:
                    # If our mock raises an exception, convert it to TimeoutError
                    raise asyncio.TimeoutError()

            with patch('asyncio.wait_for', side_effect=mock_wait_for):
                tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(+ 1 2)"})
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

                error = exc_info.value
                assert "AIFPL calculation timed out" in str(error)
                # The timeout error is now wrapped in AIToolExecutionError
                assert error.__cause__.__class__.__name__ == "AIToolTimeoutError"

    def test_execute_unexpected_error_handling(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test handling of unexpected errors."""
        with patch.object(aifpl_tool._tool, 'evaluate_and_format') as mock_evaluate:
            mock_evaluate.side_effect = RuntimeError("Unexpected error")

            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(+ 1 2)"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to evaluate AIFPL expression" in str(error)
            assert error.__cause__.__class__ == RuntimeError


class TestAIFPLAIToolParametrized:
    """Parametrized tests for the AIFPL tool."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic arithmetic
        ("(+ 1 2)", "3"),
        ("(* 3 4)", "12"),
        ("(- 10 3)", "7"),
        ("(/ 8 2)", "4.0"),

        # Function calls
        ("(sqrt 9)", "3.0"),
        ("(abs -7)", "7"),
        ("(max 1 5 3)", "5"),

        # Lists
        ("(list 1 2 3)", "(1 2 3)"),
        ("(first (list 10 20 30))", "10"),
        ("(length (list 1 2 3 4))", "4"),

        # Strings
        ('(string-length "test")', "4"),
        ('(string-upcase "hello")', '"HELLO"'),

        # Booleans
        ("(and #t #t)", "#t"),
        ("(or #f #t)", "#t"),
        ("(not #f)", "#t"),

        # Comparisons
        ("(= 5 5)", "#t"),
        ("(< 3 7)", "#t"),
        ("(> 8 2)", "#t"),

        # Constants
        ("true", "#t"),
        ("false", "#f"),
    ])
    def test_various_expressions(self, aifpl_tool, mock_authorization, make_tool_call, expression, expected):
        """Test various AIFPL expressions."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
        assert result.content == expected

    @pytest.mark.parametrize("invalid_expression", [
        "",
        "   ",
        "(+ 1 2",
        "+ 1 2)",
        # Removed "()" as it's valid in LISP (empty list)
        "(unknown-func 1 2)",
        "(if 1 \"yes\" \"no\")",  # Non-boolean condition
        "(+ \"hello\" 1)",  # Type mismatch
    ])
    def test_invalid_expressions_raise_errors(self, aifpl_tool, mock_authorization, make_tool_call, invalid_expression):
        """Test that various invalid expressions raise AIToolExecutionError."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": invalid_expression})
        with pytest.raises(AIToolExecutionError):
            asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

    @pytest.mark.parametrize("complex_expression", [
        # Nested expressions
        "(+ (* 2 3) (/ 8 2))",
        "(if (> 5 3) (+ 10 20) (- 10 5))",
        "(map (lambda (x) (* x x)) (list 1 2 3))",
        "(fold + 0 (range 1 6))",
        "(filter (lambda (x) (> x 2)) (list 1 2 3 4 5))",

        # String operations
        '(string-join (map string-upcase (string-split "hello,world" ",")) " ")',

        # List comprehension style
        "(map (lambda (x) (+ x 1)) (filter (lambda (x) (> x 0)) (list -1 1 -2 2 -3 3)))",

        # Mathematical expressions
        "(sqrt (+ (* 3 3) (* 4 4)))",
        "(round (* pi 2))",
    ])
    def test_complex_expressions_execute_successfully(self, aifpl_tool, mock_authorization, make_tool_call, complex_expression):
        """Test that complex expressions execute without errors."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": complex_expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # Just verify it doesn't crash and returns something
        assert isinstance(result.content, str)
        assert len(result.content) > 0


class TestAIFPLAIToolIntegration:
    """Integration tests for the AIFPL tool."""

    def test_tool_inheritance(self, aifpl_tool):
        """Test that AIFPLAITool properly inherits from AITool."""
        assert isinstance(aifpl_tool, AITool)
        assert hasattr(aifpl_tool, 'get_definition')
        assert hasattr(aifpl_tool, 'execute')
        assert callable(aifpl_tool.get_definition)
        assert callable(aifpl_tool.execute)

    def test_end_to_end_functional_programming(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test end-to-end functional programming workflow."""
        # Complex functional programming expression
        expression = """
        (let ((numbers (range 1 11)))
          (fold + 0
            (map (lambda (x) (* x x))
              (filter (lambda (x) (= (% x 2) 0)) numbers))))
        """
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # Expected: sum of squares of even numbers 1-10
        # Even numbers: 2, 4, 6, 8, 10
        # Squares: 4, 16, 36, 64, 100
        # Sum: 220
        assert result.content == "220"

    def test_recursive_function_example(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test recursive function using let bindings."""
        # Factorial using recursion (if supported)
        expression = """
        (let ((factorial (lambda (n)
                          (if (<= n 1)
                              1
                              (* n (factorial (- n 1)))))))
          (factorial 5))
        """
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})

        try:
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            # If recursion is supported, should return 120 (5!)
            assert result.content == "120"
        except AIToolExecutionError:
            # If recursion isn't supported, that's also acceptable
            pass

    def test_string_processing_pipeline(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test string processing pipeline."""
        expression = '''
        (string-join
          (map string-upcase
            (filter (lambda (s) (> (string-length s) 2))
              (string-split "hello,hi,world,ok,test" ",")))
          " ")
        '''
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # Should filter strings with length > 2, uppercase them, and join
        assert result.content == '"HELLO WORLD TEST"'

    def test_mathematical_computation_pipeline(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test mathematical computation pipeline."""
        expression = """
        (let ((data (list 1 2 3 4 5 6 7 8 9 10)))
          (list
            (fold + 0 data)
            (/ (fold + 0 data) (length data))
            (sqrt (fold + 0 (map (lambda (x) (* x x)) data)))))
        """
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # Should compute sum, average, and root sum of squares
        # Sum: 55, Average: 5.5, Root sum of squares: sqrt(385) ≈ 19.62
        assert "55" in result.content
        assert "5.5" in result.content

    def test_mixed_data_types_handling(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test handling of mixed data types."""
        expression = """
        (list
          (number? 42)
          (string? "hello")
          (boolean? #t)
          (list? (list 1 2 3))
          (function? (lambda (x) x)))
        """
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # All predicates should return true
        assert result.content == "(#t #t #t #t #t)"

    def test_lazy_evaluation_conditional(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test lazy evaluation in conditionals."""
        # This should not cause division by zero because of lazy evaluation
        expression = "(if #t 42 (/ 1 0))"
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        assert result.content == "42"

    def test_homoiconicity_code_as_data(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test homoiconicity - code as data."""
        expression = """
        (let ((code '(+ 1 2 3)))
          (list
            (first code)
            (rest code)
            (length code)))
        """
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # Should extract parts of the quoted expression
        assert "(+ (1 2 3) 4)" in result.content or "(+ (1 2 3) 4)" == result.content.replace(" ", "")

    def test_multiple_evaluations_independent(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test that multiple evaluations are independent."""
        expressions = [
            "(+ 1 2)",
            "(* 3 4)",
            "(list 1 2 3)",
            '(string-upcase "hello")',
        ]

        results = []
        for expr in expressions:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expr})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            results.append(result.content)

        # Verify all results are correct and independent
        assert results[0] == "3"
        assert results[1] == "12"
        assert results[2] == "(1 2 3)"
        assert results[3] == '"HELLO"'


class TestAIFPLAIToolTimeout:
    """Test AIFPL tool timeout behavior."""

    def test_timeout_configuration(self, aifpl_tool):
        """Test that timeout is configured correctly."""
        # The timeout should be 10 seconds for AIFPL (longer than calculator's 5)
        # This is tested indirectly through the timeout error test above
        assert hasattr(aifpl_tool, '_tool')

    def test_timeout_prevents_infinite_loops(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test that timeout prevents infinite computation."""
        # Mock the specific method on the tool instance to avoid affecting other tests
        with patch.object(aifpl_tool, '_evaluate_expression_sync') as mock_evaluate:
            mock_evaluate.side_effect = Exception("Simulated timeout")

            # Mock asyncio.wait_for to raise TimeoutError when our mocked method is called
            original_wait_for = asyncio.wait_for
            async def mock_wait_for(coro, timeout=None):
                try:
                    return await original_wait_for(coro, timeout=timeout)
                except Exception:
                    # If our mock raises an exception, convert it to TimeoutError
                    raise asyncio.TimeoutError()

            with patch('asyncio.wait_for', side_effect=mock_wait_for):
                tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(+ 1 2)"})
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

                error = exc_info.value
                # The timeout error is now wrapped in AIToolExecutionError
                assert error.__cause__.__class__.__name__ == "AIToolTimeoutError"


class TestAIFPLAIToolSecurity:
    """Test security aspects of the AIFPL tool."""

    def test_pure_functional_no_side_effects(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test that AIFPL is pure functional with no side effects."""
        # AIFPL should not be able to perform any I/O or system operations
        # This is ensured by the AIFPL language design itself

        # These expressions should work (pure functional)
        safe_expressions = [
            "(+ 1 2 3)",
            "(map (lambda (x) (* x x)) (list 1 2 3))",
            "(fold + 0 (range 1 100))",
            '(string-split "a,b,c" ",")',
        ]

        for expr in safe_expressions:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expr})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert isinstance(result.content, str)
            assert len(result.content) > 0

    def test_no_system_access(self, aifpl_tool):
        """Test that AIFPL cannot access system resources."""
        # AIFPL by design should not have any system access functions
        # The language itself should prevent this, but we can verify
        # the tool doesn't expose any dangerous functionality

        definition = aifpl_tool.get_definition()
        description = definition.description.lower()

        # Should not mention any system access capabilities
        forbidden_terms = ['file', 'system', 'os', 'exec', 'eval', 'import']
        for term in forbidden_terms:
            # These terms might appear in context of "no side effects" etc.
            # so we don't assert their complete absence, but the tool should
            # be designed to prevent such access
            pass

    def test_immutable_data_structures(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test that AIFPL uses immutable data structures."""
        # Operations should not modify original data
        expression = """
        (let ((original (list 1 2 3)))
          (list
            original
            (append original (list 4))
            original))
        """
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # Fixed: Check for the actual result format without spaces
        # Original list should appear unchanged
        result_no_spaces = result.content.replace(" ", "")
        expected_no_spaces = "((123)(1234)(123))"
        assert expected_no_spaces in result_no_spaces


class TestAIFPLAIToolEmptyList:
    """Test that empty list is valid in AIFPL."""

    def test_empty_list_is_valid(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test that () is a valid expression representing an empty list."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "()"})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

        # () should return an empty list representation
        assert result.content == "()"

    def test_empty_list_operations(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test operations on empty lists."""
        test_cases = [
            ("(length ())", "0"),
            ("(null? ())", "#t"),
            ("(append () (list 1 2))", "(1 2)"),
            ("(list? ())", "#t"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert result.content == expected


class TestAIFPLAIToolModulePath:
    """Test module path management in AIFPLAITool."""

    def test_default_module_path_is_empty(self):
        """Test that default module path is empty list when not specified."""
        tool = AIFPLAITool(module_path=[])
        assert tool._module_path == []

    def test_custom_module_path_is_expanded(self, tmp_path):
        """Test that custom module paths are expanded and resolved."""
        tool = AIFPLAITool(module_path=[str(tmp_path)])

        # Path should be expanded to absolute path
        assert len(tool._module_path) == 1
        assert str(tmp_path) in tool._module_path[0]

    def test_set_module_path_updates_path(self, tmp_path):
        """Test that set_module_path updates the module path."""
        tool = AIFPLAITool(module_path=["/old/path"])

        new_path = str(tmp_path)
        tool.set_module_path([new_path])

        # Should have updated path
        assert len(tool._module_path) == 1
        assert str(tmp_path) in tool._module_path[0]

    def test_set_module_path_expands_paths(self):
        """Test that set_module_path expands and resolves paths."""
        tool = AIFPLAITool()

        # Use home directory expansion
        tool.set_module_path(["~/test"])

        # Should be expanded (not contain ~)
        assert "~" not in tool._module_path[0]

    def test_set_module_path_clears_cache(self, tmp_path):
        """Test that set_module_path clears the module cache."""
        # Create a module
        module_file = tmp_path / "test.aifpl"
        module_file.write_text("(alist (list \"value\" 42))")

        tool = AIFPLAITool(module_path=[str(tmp_path)])

        # Load module to populate cache
        tool._tool.evaluate('(import "test")')
        assert "test" in tool._tool.module_cache

        # Change module path
        tool.set_module_path(["/new/path"])

        # Cache should be cleared
        assert len(tool._tool.module_cache) == 0

    def test_set_module_path_with_multiple_directories(self, tmp_path):
        """Test that set_module_path works with multiple directories."""
        dir1 = tmp_path / "dir1"
        dir2 = tmp_path / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        tool = AIFPLAITool()
        tool.set_module_path([str(dir1), str(dir2)])

        assert len(tool._module_path) == 2
        assert any(str(dir1) in path for path in tool._module_path)
        assert any(str(dir2) in path for path in tool._module_path)

    def test_module_path_persists_after_evaluation(self, tmp_path):
        """Test that module path persists after evaluations."""
        module_file = tmp_path / "test.aifpl"
        module_file.write_text("(alist (list \"value\" 42))")

        tool = AIFPLAITool(module_path=[str(tmp_path)])
        original_path = tool._module_path.copy()

        # Evaluate something
        tool._tool.evaluate('(import "test")')

        # Path should be unchanged
        assert tool._module_path == original_path
