"""
Tests for the calculator tool
"""
import asyncio
import math
import pytest
from unittest.mock import patch, MagicMock
from typing import Dict, Any

from humbug.tools.tool_calculator import ToolCalculator, SafeMathEvaluator
from humbug.ai.ai_tool_manager import AIToolDefinition, AIToolParameter, ToolExecutionError


@pytest.fixture
def calculator_tool():
    """Fixture providing a calculator tool instance."""
    return ToolCalculator()


@pytest.fixture
def safe_evaluator():
    """Fixture providing a SafeMathEvaluator instance."""
    return SafeMathEvaluator()


class TestToolCalculatorDefinition:
    """Test the calculator tool definition."""

    def test_get_definition_returns_correct_structure(self, calculator_tool):
        """Test that get_definition returns the correct tool definition structure."""
        definition = calculator_tool.get_definition()

        assert isinstance(definition, AIToolDefinition)
        assert definition.name == "calculate"
        assert "Mathematical expression evaluator" in definition.description
        assert len(definition.parameters) == 1

    def test_expression_parameter_definition(self, calculator_tool):
        """Test the expression parameter definition."""
        definition = calculator_tool.get_definition()
        expr_param = definition.parameters[0]

        assert isinstance(expr_param, AIToolParameter)
        assert expr_param.name == "expression"
        assert expr_param.type == "string"
        assert "Mathematical expression using listed operators" in expr_param.description
        assert expr_param.required is True
        assert expr_param.enum is None

    def test_definition_includes_supported_operations(self, calculator_tool):
        """Test that the definition describes supported operations."""
        definition = calculator_tool.get_definition()
        description = definition.description

        # Check for arithmetic operations
        assert "+" in description and "-" in description
        assert "*" in description and "/" in description
        assert "//" in description and "%" in description
        assert "**" in description

        # Check for functions
        assert "sin" in description and "cos" in description and "tan" in description
        assert "log" in description and "exp" in description
        assert "sqrt" in description and "abs" in description

        # Check for constants
        assert "pi" in description and "e" in description
        assert "j" in description  # imaginary unit


class TestSafeMathEvaluatorBasicArithmetic:
    """Test basic arithmetic operations in SafeMathEvaluator."""

    def test_addition(self, safe_evaluator):
        """Test addition operations."""
        assert safe_evaluator.evaluate("2 + 3") == 5
        assert safe_evaluator.evaluate("10 + 0") == 10
        assert safe_evaluator.evaluate("-5 + 8") == 3
        assert safe_evaluator.evaluate("1.5 + 2.7") == 4.2

    def test_subtraction(self, safe_evaluator):
        """Test subtraction operations."""
        assert safe_evaluator.evaluate("5 - 3") == 2
        assert safe_evaluator.evaluate("10 - 10") == 0
        assert safe_evaluator.evaluate("3 - 7") == -4
        assert safe_evaluator.evaluate("5.5 - 2.2") == 3.3

    def test_multiplication(self, safe_evaluator):
        """Test multiplication operations."""
        assert safe_evaluator.evaluate("3 * 4") == 12
        assert safe_evaluator.evaluate("7 * 0") == 0
        assert safe_evaluator.evaluate("-3 * 5") == -15
        assert safe_evaluator.evaluate("2.5 * 4") == 10.0

    def test_division(self, safe_evaluator):
        """Test division operations."""
        assert safe_evaluator.evaluate("8 / 2") == 4
        assert safe_evaluator.evaluate("7 / 2") == 3.5
        assert safe_evaluator.evaluate("-10 / 5") == -2
        assert safe_evaluator.evaluate("1 / 3") == pytest.approx(0.3333333333333333)

    def test_floor_division(self, safe_evaluator):
        """Test floor division operations."""
        assert safe_evaluator.evaluate("7 // 2") == 3
        assert safe_evaluator.evaluate("10 // 3") == 3
        assert safe_evaluator.evaluate("-7 // 2") == -4

    def test_modulo(self, safe_evaluator):
        """Test modulo operations."""
        assert safe_evaluator.evaluate("7 % 3") == 1
        assert safe_evaluator.evaluate("10 % 5") == 0
        assert safe_evaluator.evaluate("17 % 4") == 1

    def test_power(self, safe_evaluator):
        """Test power operations."""
        assert safe_evaluator.evaluate("2 ** 3") == 8
        assert safe_evaluator.evaluate("5 ** 0") == 1
        assert safe_evaluator.evaluate("4 ** 0.5") == 2.0
        assert safe_evaluator.evaluate("2 ** -1") == 0.5


class TestSafeMathEvaluatorFunctions:
    """Test mathematical functions in SafeMathEvaluator."""

    def test_sqrt_function(self, safe_evaluator):
        """Test square root function."""
        assert safe_evaluator.evaluate("sqrt(4)") == 2.0
        assert safe_evaluator.evaluate("sqrt(9)") == 3.0
        assert safe_evaluator.evaluate("sqrt(2)") == pytest.approx(1.4142135623730951)

        # Complex result for negative numbers
        result = safe_evaluator.evaluate("sqrt(-1)")
        assert isinstance(result, complex)
        assert result.real == 0.0
        assert result.imag == 1.0

    def test_trigonometric_functions(self, safe_evaluator):
        """Test trigonometric functions."""
        # Test with known values
        assert safe_evaluator.evaluate("sin(0)") == 0.0
        assert safe_evaluator.evaluate("cos(0)") == 1.0
        assert safe_evaluator.evaluate("tan(0)") == 0.0

        # Test with pi
        assert safe_evaluator.evaluate("sin(pi/2)") == pytest.approx(1.0)
        assert safe_evaluator.evaluate("cos(pi)") == pytest.approx(-1.0)

    def test_logarithmic_functions(self, safe_evaluator):
        """Test logarithmic functions."""
        assert safe_evaluator.evaluate("log(e)") == pytest.approx(1.0)
        assert safe_evaluator.evaluate("log10(10)") == pytest.approx(1.0)
        assert safe_evaluator.evaluate("log10(100)") == pytest.approx(2.0)
        assert safe_evaluator.evaluate("exp(0)") == 1.0
        assert safe_evaluator.evaluate("exp(1)") == pytest.approx(math.e)

    def test_utility_functions(self, safe_evaluator):
        """Test utility functions."""
        assert safe_evaluator.evaluate("abs(-5)") == 5
        assert safe_evaluator.evaluate("abs(3)") == 3
        assert safe_evaluator.evaluate("round(3.7)") == 4
        assert safe_evaluator.evaluate("round(3.2)") == 3
        assert safe_evaluator.evaluate("floor(3.7)") == 3
        assert safe_evaluator.evaluate("ceil(3.2)") == 4

    def test_aggregate_functions(self, safe_evaluator):
        """Test aggregate functions."""
        assert safe_evaluator.evaluate("min(3, 1, 4)") == 1
        assert safe_evaluator.evaluate("max(3, 1, 4)") == 4
        assert safe_evaluator.evaluate("sum([1, 2, 3, 4])") == 10
        assert safe_evaluator.evaluate("pow(2, 3)") == 8


class TestSafeMathEvaluatorConstants:
    """Test mathematical constants in SafeMathEvaluator."""

    def test_pi_constant(self, safe_evaluator):
        """Test pi constant."""
        result = safe_evaluator.evaluate("pi")
        assert result == pytest.approx(math.pi)

    def test_e_constant(self, safe_evaluator):
        """Test e constant."""
        result = safe_evaluator.evaluate("e")
        assert result == pytest.approx(math.e)

    def test_imaginary_unit(self, safe_evaluator):
        """Test imaginary unit j."""
        result = safe_evaluator.evaluate("j")
        assert isinstance(result, complex)
        assert result.real == 0.0
        assert result.imag == 1.0

    def test_constants_in_expressions(self, safe_evaluator):
        """Test constants used in expressions."""
        assert safe_evaluator.evaluate("2 * pi") == pytest.approx(2 * math.pi)
        assert safe_evaluator.evaluate("e ** 2") == pytest.approx(math.e ** 2)
        assert safe_evaluator.evaluate("j * j") == -1


class TestSafeMathEvaluatorComplexNumbers:
    """Test complex number operations in SafeMathEvaluator."""

    def test_complex_arithmetic(self, safe_evaluator):
        """Test complex number arithmetic."""
        assert safe_evaluator.evaluate("j + 1") == complex(1, 1)
        assert safe_evaluator.evaluate("2 * j") == complex(0, 2)
        assert safe_evaluator.evaluate("j * j") == -1
        assert safe_evaluator.evaluate("(1 + j) * (1 - j)") == 2

    def test_complex_number_simplification(self, safe_evaluator):
        """Test that complex numbers with negligible imaginary parts are simplified."""
        # Results that should be simplified to real numbers
        result = safe_evaluator.evaluate("j * j")  # Should be -1, not -1+0j
        assert result == -1
        assert isinstance(result, int)

        result = safe_evaluator.evaluate("(1 + j) * (1 - j)")  # Should be 2, not 2+0j
        assert result == 2
        assert isinstance(result, int)

    def test_complex_functions(self, safe_evaluator):
        """Test functions that can handle complex numbers."""
        # sqrt of negative number
        result = safe_evaluator.evaluate("sqrt(-4)")
        assert isinstance(result, complex)
        assert result.real == 0.0
        assert result.imag == 2.0

        # Complex trigonometry
        result = safe_evaluator.evaluate("sin(j)")
        assert isinstance(result, complex)

    def test_real_only_functions_with_complex(self, safe_evaluator):
        """Test that real-only functions reject complex numbers with significant imaginary parts."""
        with pytest.raises(ValueError, match="does not support complex numbers"):
            safe_evaluator.evaluate("floor(1 + j)")

        with pytest.raises(ValueError, match="does not support complex numbers"):
            safe_evaluator.evaluate("ceil(2 + 3*j)")

        with pytest.raises(ValueError, match="does not support complex numbers"):
            safe_evaluator.evaluate("round(1.5 + 0.1*j)")

    def test_real_only_functions_with_negligible_imaginary(self, safe_evaluator):
        """Test that real-only functions work with complex numbers having negligible imaginary parts."""
        # These should work because imaginary part is effectively zero
        # Note: We need to create these through calculation, not direct input
        result = safe_evaluator.evaluate("floor((1 + j) * (1 - j))")  # Results in real 2
        assert result == 2


class TestSafeMathEvaluatorParentheses:
    """Test parentheses and expression precedence."""

    def test_basic_parentheses(self, safe_evaluator):
        """Test basic parentheses usage."""
        assert safe_evaluator.evaluate("(2 + 3) * 4") == 20
        assert safe_evaluator.evaluate("2 + (3 * 4)") == 14
        assert safe_evaluator.evaluate("(2 + 3) * (4 + 5)") == 45

    def test_nested_parentheses(self, safe_evaluator):
        """Test nested parentheses."""
        assert safe_evaluator.evaluate("((2 + 3) * 4) + 1") == 21
        assert safe_evaluator.evaluate("2 * ((3 + 4) * (5 + 6))") == 154
        assert safe_evaluator.evaluate("(2 * (3 + (4 * 5)))") == 46

    def test_function_calls_with_parentheses(self, safe_evaluator):
        """Test function calls combined with parentheses."""
        assert safe_evaluator.evaluate("sin(pi / 2)") == pytest.approx(1.0)
        assert safe_evaluator.evaluate("sqrt((3 + 4) * (3 + 4))") == 7.0
        assert safe_evaluator.evaluate("log(exp(2))") == pytest.approx(2.0)


class TestSafeMathEvaluatorErrorHandling:
    """Test error handling in SafeMathEvaluator."""

    def test_division_by_zero(self, safe_evaluator):
        """Test division by zero error."""
        with pytest.raises(ZeroDivisionError, match="Division by zero"):
            safe_evaluator.evaluate("5 / 0")

        with pytest.raises(ZeroDivisionError):
            safe_evaluator.evaluate("10 // 0")

    def test_invalid_syntax(self, safe_evaluator):
        """Test invalid syntax errors."""
        with pytest.raises(ValueError, match="Invalid mathematical expression"):
            safe_evaluator.evaluate("2 +")

        with pytest.raises(ValueError, match="Invalid mathematical expression"):
            safe_evaluator.evaluate("* 3")

        with pytest.raises(ValueError, match="Invalid mathematical expression"):
            safe_evaluator.evaluate("((2 + 3)")

    def test_empty_expression(self, safe_evaluator):
        """Test empty expression error."""
        with pytest.raises(ValueError, match="Expression cannot be empty"):
            safe_evaluator.evaluate("")

        with pytest.raises(ValueError, match="Expression cannot be empty"):
            safe_evaluator.evaluate("   ")

    def test_undefined_variables(self, safe_evaluator):
        """Test undefined variable errors."""
        with pytest.raises(ValueError, match="Undefined variable or constant"):
            safe_evaluator.evaluate("x + 1")

        with pytest.raises(ValueError, match="Undefined variable or constant"):
            safe_evaluator.evaluate("unknown_var")

    def test_undefined_functions(self, safe_evaluator):
        """Test undefined function errors."""
        with pytest.raises(ValueError, match="Function 'undefined_func' is not allowed"):
            safe_evaluator.evaluate("undefined_func(1)")

        with pytest.raises(ValueError, match="Function 'eval' is not allowed"):
            safe_evaluator.evaluate("eval('1+1')")

    def test_unsupported_operations(self, safe_evaluator):
        """Test unsupported operation errors."""
        with pytest.raises(ValueError, match="Unsupported operation"):
            safe_evaluator.evaluate("1 & 2")  # Bitwise operations not supported

    def test_overflow_detection(self, safe_evaluator):
        """Test overflow detection."""
        with pytest.raises(OverflowError, match="too large"):
            safe_evaluator.evaluate("10 ** 1000")

    def test_deeply_nested_expressions(self, safe_evaluator):
        """Test deeply nested expression limits."""
        # Create a very deeply nested expression
        nested_expr = "1"
        for _ in range(200):  # Exceed MAX_DEPTH
            nested_expr = f"({nested_expr} + 1)"

        with pytest.raises(ValueError, match="too deeply nested"):
            safe_evaluator.evaluate(nested_expr)

    def test_function_argument_errors(self, safe_evaluator):
        """Test function argument errors."""
        with pytest.raises(ValueError, match="Error calling function"):
            safe_evaluator.evaluate("sqrt()")  # Missing argument

        with pytest.raises(ValueError, match="Error calling function"):
            safe_evaluator.evaluate("log(-1)")  # Invalid argument for real log


class TestToolCalculatorExecution:
    """Test the calculator tool execution."""

    def test_execute_basic_arithmetic(self, calculator_tool):
        """Test execution with basic arithmetic."""
        result = asyncio.run(calculator_tool.execute({"expression": "2 + 3"}))
        assert result == "5"

        result = asyncio.run(calculator_tool.execute({"expression": "10 * 5"}))
        assert result == "50"

    def test_execute_complex_expression(self, calculator_tool):
        """Test execution with complex expressions."""
        result = asyncio.run(calculator_tool.execute({"expression": "sqrt(16) + sin(0)"}))
        assert result == "4.0"

        result = asyncio.run(calculator_tool.execute({"expression": "2 * pi * 5"}))
        expected = str(2 * math.pi * 5)
        assert result == expected

    def test_execute_with_complex_numbers(self, calculator_tool):
        """Test execution with complex numbers."""
        result = asyncio.run(calculator_tool.execute({"expression": "sqrt(-1)"}))
        assert result == "1j"

        result = asyncio.run(calculator_tool.execute({"expression": "j * j"}))
        assert result == "-1"

    def test_execute_missing_expression(self, calculator_tool):
        """Test execution without expression argument."""
        with pytest.raises(ToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute({}))

        error = exc_info.value
        assert "Expression is required" in str(error)
        assert error.tool_name == "calculate"

    def test_execute_empty_expression(self, calculator_tool):
        """Test execution with empty expression."""
        with pytest.raises(ToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute({"expression": ""}))

        error = exc_info.value
        assert "Expression is required" in str(error)

    def test_execute_non_string_expression(self, calculator_tool):
        """Test execution with non-string expression."""
        with pytest.raises(ToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute({"expression": 123}))

        error = exc_info.value
        assert "Expression must be a string" in str(error)

    def test_execute_division_by_zero_error(self, calculator_tool):
        """Test execution with division by zero."""
        with pytest.raises(ToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute({"expression": "5 / 0"}))

        error = exc_info.value
        assert "Division by zero" in str(error)
        assert error.tool_name == "calculate"
        assert error.__cause__.__class__ == ZeroDivisionError

    def test_execute_invalid_expression_error(self, calculator_tool):
        """Test execution with invalid expression."""
        with pytest.raises(ToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute({"expression": "2 +"}))

        error = exc_info.value
        assert "Invalid mathematical expression" in str(error)
        assert error.__cause__.__class__ == ValueError

    def test_execute_overflow_error(self, calculator_tool):
        """Test execution with overflow."""
        with pytest.raises(ToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute({"expression": "10 ** 1000"}))

        error = exc_info.value
        assert "too large" in str(error)
        assert error.__cause__.__class__ == OverflowError

    def test_execute_unexpected_error_handling(self, calculator_tool):
        """Test handling of unexpected errors."""
        with patch.object(calculator_tool._evaluator, 'evaluate') as mock_evaluate:
            mock_evaluate.side_effect = RuntimeError("Unexpected error")

            with pytest.raises(ToolExecutionError) as exc_info:
                asyncio.run(calculator_tool.execute({"expression": "1 + 1"}))

            error = exc_info.value
            assert "Failed to calculate expression" in str(error)
            assert error.__cause__.__class__ == RuntimeError


class TestToolCalculatorLogging:
    """Test logging behavior in the calculator tool."""

    def test_successful_calculation_logging(self, calculator_tool):
        """Test that successful calculations are logged at debug level."""
        with patch('humbug.tools.tool_calculator.logger') as mock_logger:
            asyncio.run(calculator_tool.execute({"expression": "2 + 2"}))

            # Check debug logging calls
            mock_logger.debug.assert_any_call("Evaluating mathematical expression: %s", "2 + 2")
            mock_logger.debug.assert_any_call("Expression evaluation successful: %s = %s", "2 + 2", 4)

    def test_error_logging_levels(self, calculator_tool):
        """Test that different error types are logged at appropriate levels."""
        with patch('humbug.tools.tool_calculator.logger') as mock_logger:
            # Test division by zero (warning level)
            try:
                asyncio.run(calculator_tool.execute({"expression": "5 / 0"}))
            except ToolExecutionError:
                pass

            mock_logger.warning.assert_called_once()
            assert "Division by zero" in str(mock_logger.warning.call_args)

    def test_invalid_expression_logging(self, calculator_tool):
        """Test logging for invalid expressions."""
        with patch('humbug.tools.tool_calculator.logger') as mock_logger:
            try:
                asyncio.run(calculator_tool.execute({"expression": "invalid + +"}))
            except ToolExecutionError:
                pass

            mock_logger.warning.assert_called_once()
            assert "Invalid expression" in str(mock_logger.warning.call_args)

    def test_missing_expression_logging(self, calculator_tool):
        """Test logging for missing expression."""
        with patch('humbug.tools.tool_calculator.logger') as mock_logger:
            try:
                asyncio.run(calculator_tool.execute({}))
            except ToolExecutionError:
                pass

            mock_logger.error.assert_called_once()
            assert "without expression argument" in str(mock_logger.error.call_args)


class TestToolCalculatorParametrized:
    """Parametrized tests for the calculator tool."""

    @pytest.mark.parametrize("expression,expected", [
        ("1 + 1", "2"),
        ("5 * 6", "30"),
        ("10 / 2", "5.0"),
        ("2 ** 3", "8"),
        ("sqrt(9)", "3.0"),
        ("abs(-5)", "5"),
        ("round(3.7)", "4"),
        ("sin(0)", "0.0"),
        ("cos(0)", "1.0"),
        ("log(e)", "1.0"),
        ("pi", str(math.pi)),
        ("e", str(math.e)),
        ("j", "1j"),
    ])
    def test_various_expressions(self, calculator_tool, expression, expected):
        """Test various mathematical expressions."""
        result = asyncio.run(calculator_tool.execute({"expression": expression}))

        if expected.replace(".", "").replace("-", "").isdigit():
            # Numeric comparison with tolerance
            assert float(result) == pytest.approx(float(expected))
        else:
            assert result == expected

    @pytest.mark.parametrize("invalid_expression", [
        "",
        "   ",
        "2 +",
        "* 3",
        "((2 + 3)",
        "undefined_var",
        "eval('1+1')",
        "import os",
        "__import__('os')",
        "exec('print(1)')",
    ])
    def test_invalid_expressions_raise_errors(self, calculator_tool, invalid_expression):
        """Test that various invalid expressions raise ToolExecutionError."""
        with pytest.raises(ToolExecutionError):
            asyncio.run(calculator_tool.execute({"expression": invalid_expression}))

    @pytest.mark.parametrize("complex_expr,should_be_real", [
        ("j * j", True),  # Should be -1
        ("(1 + j) * (1 - j)", True),  # Should be 2
        ("sqrt(-4)", False),  # Should remain complex
        ("1 + 0*j", True),  # Should be simplified to 1
        ("sin(j)", False),  # Should remain complex
    ])
    def test_complex_number_simplification(self, calculator_tool, complex_expr, should_be_real):
        """Test that complex numbers are simplified when appropriate."""
        result = asyncio.run(calculator_tool.execute({"expression": complex_expr}))

        if should_be_real:
            # Should not contain 'j' in the result
            assert 'j' not in result
        else:
            # Should contain 'j' in the result
            assert 'j' in result or 'complex' in str(type(eval(result)))


class TestToolCalculatorSecurity:
    """Test security aspects of the calculator tool."""

    @pytest.mark.parametrize("malicious_expr", [
        "__import__('os').system('ls')",
        "exec('print(\"hacked\")')",
        "eval('__import__(\"os\").getcwd()')",
        "open('/etc/passwd').read()",
        "globals()",
        "locals()",
        "dir()",
        "vars()",
        "getattr(str, 'upper')",
        "setattr(int, 'x', 1)",
        "delattr(str, 'upper')",
        "hasattr(str, 'upper')",
        "callable(print)",
        "isinstance(1, int)",
        "issubclass(int, object)",
        "compile('1+1', '<string>', 'eval')",
    ])
    def test_malicious_expressions_blocked(self, calculator_tool, malicious_expr):
        """Test that malicious expressions are blocked."""
        with pytest.raises(ToolExecutionError):
            asyncio.run(calculator_tool.execute({"expression": malicious_expr}))

    def test_only_safe_builtins_allowed(self, calculator_tool):
        """Test that only explicitly allowed functions work."""
        # These should work
        safe_expressions = [
            "abs(-1)", "round(1.5)", "min(1, 2)", "max(1, 2)",
            "sum([1, 2, 3])", "pow(2, 3)", "sqrt(4)", "sin(0)"
        ]

        for expr in safe_expressions:
            result = asyncio.run(calculator_tool.execute({"expression": expr}))
            assert isinstance(result, str)

        # These should not work
        unsafe_expressions = [
            "len([1, 2, 3])", "str(123)", "int('123')", "float('1.5')",
            "list([1, 2])", "dict()", "tuple([1, 2])", "set([1, 2])"
        ]

        for expr in unsafe_expressions:
            with pytest.raises(ToolExecutionError):
                asyncio.run(calculator_tool.execute({"expression": expr}))


class TestToolCalculatorIntegration:
    """Integration tests for the calculator tool."""

    def test_tool_inheritance(self, calculator_tool):
        """Test that ToolCalculator properly inherits from AITool."""
        from humbug.ai.ai_tool_manager import AITool

        assert isinstance(calculator_tool, AITool)
        assert hasattr(calculator_tool, 'get_definition')
        assert hasattr(calculator_tool, 'execute')
        assert callable(calculator_tool.get_definition)
        assert callable(calculator_tool.execute)

    def test_evaluator_initialization(self, calculator_tool):
        """Test that the calculator tool properly initializes its evaluator."""
        assert hasattr(calculator_tool, '_evaluator')
        assert isinstance(calculator_tool._evaluator, SafeMathEvaluator)

    def test_end_to_end_calculation(self, calculator_tool):
        """Test end-to-end calculation workflow."""
        # Complex mathematical expression
        expression = "sqrt(pow(3, 2) + pow(4, 2)) + sin(pi/6) + log(e)"
        result = asyncio.run(calculator_tool.execute({"expression": expression}))

        # Expected: sqrt(9 + 16) + 0.5 + 1 = 5 + 0.5 + 1 = 6.5
        expected = 5.0 + 0.5 + 1.0
        assert float(result) == pytest.approx(expected)

    def test_multiple_calculations_independent(self, calculator_tool):
        """Test that multiple calculations are independent."""
        expressions = [
            "2 + 2",
            "3 * 3",
            "sqrt(16)",
            "pi / 2"
        ]

        results = []
        for expr in expressions:
            result = asyncio.run(calculator_tool.execute({"expression": expr}))
            results.append(result)

        # Verify all results are correct and independent
        assert results[0] == "4"
        assert results[1] == "9"
        assert results[2] == "4.0"
        assert float(results[3]) == pytest.approx(math.pi / 2)
