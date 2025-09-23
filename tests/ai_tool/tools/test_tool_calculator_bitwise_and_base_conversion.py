"""Tests for calculator tool bitwise operations and base conversion functions."""

import asyncio

import pytest

from ai_tool import AIToolCall, AIToolExecutionError
from ai_tool.calculator.calculator_ai_tool import CalculatorAITool


class TestCalculatorBitwiseOperations:
    """Test bitwise operations in the calculator tool."""

    @pytest.fixture
    def calculator_tool(self):
        """Create a calculator tool instance for testing."""
        return CalculatorAITool()

    @pytest.fixture
    def mock_authorization(self):
        """Fixture providing a mocked authorization callback."""
        async def mock_auth_callback(tool_name, arguments, context, destructive):
            return True  # Default to authorized

        return mock_auth_callback

    @pytest.mark.parametrize("expression,expected", [
        # Basic bitwise OR
        ("5 | 3", "7"),
        ("8 | 4", "12"),
        ("0 | 15", "15"),
        ("15 | 0", "15"),

        # Basic bitwise XOR
        ("5 ^ 3", "6"),
        ("8 ^ 4", "12"),
        ("15 ^ 15", "0"),
        ("0 ^ 7", "7"),

        # Basic bitwise AND
        ("5 & 3", "1"),
        ("8 & 4", "0"),
        ("15 & 7", "7"),
        ("0 & 15", "0"),

        # Left shift
        ("5 << 2", "20"),
        ("1 << 3", "8"),
        ("7 << 1", "14"),
        ("0 << 5", "0"),

        # Right shift
        ("20 >> 2", "5"),
        ("8 >> 3", "1"),
        ("15 >> 1", "7"),
        ("7 >> 4", "0"),

        # Bitwise NOT
        ("~5", "-6"),
        ("~0", "-1"),
        ("~(-1)", "0"),
    ])
    def test_bitwise_operations_success(self, calculator_tool, mock_authorization, expression, expected):
        """Test successful bitwise operations."""
        tool_call = AIToolCall(
            id="test_bitwise",
            name="calculator",
            arguments={"expression": expression}
        )

        result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert result.content == expected
        assert result.error is None

    @pytest.mark.parametrize("expression,expected", [
        # Complex expressions with bitwise operations
        ("(5 | 3) & 7", "7"),
        ("5 ^ 3 ^ 3", "5"),  # XOR is its own inverse
        ("(8 << 1) >> 2", "4"),
        ("~(~5)", "5"),  # Double NOT
        ("5 | (3 & 1)", "5"),
        ("(10 >> 1) << 2", "20"),
    ])
    def test_complex_bitwise_expressions(self, calculator_tool, mock_authorization, expression, expected):
        """Test complex expressions involving bitwise operations."""
        tool_call = AIToolCall(
            id="test_complex_bitwise",
            name="calculator",
            arguments={"expression": expression}
        )

        result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert result.content == expected
        assert result.error is None

    @pytest.mark.parametrize("expression", [
        # Whole number floats should work
        "5.0 | 3",
        "8.0 ^ 4.0",
        "15.0 & 7.0",
        "5.0 << 2",
        "20.0 >> 2",
        "~5.0",
    ])
    def test_bitwise_operations_with_whole_number_floats(self, calculator_tool, mock_authorization, expression):
        """Test bitwise operations with whole number floats (should work)."""
        tool_call = AIToolCall(
            id="test_bitwise_floats",
            name="calculator",
            arguments={"expression": expression}
        )

        result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        # Should not raise an error
        assert result.error is None
        assert result.content is not None

    @pytest.mark.parametrize("expression,expected_error_part", [
        # Non-integer floats should fail
        ("5.5 | 3", "requires integer values, got float: 5.5"),
        ("5 | 3.7", "requires integer values, got float: 3.7"),
        ("8.2 ^ 4", "requires integer values, got float: 8.2"),
        ("15.1 & 7", "requires integer values, got float: 15.1"),
        ("5.5 << 2", "requires integer values, got float: 5.5"),
        ("20.3 >> 2", "requires integer values, got float: 20.3"),
        ("~5.9", "requires integer values, got float: 5.9"),
    ])
    def test_bitwise_operations_with_non_integer_floats_error(self, calculator_tool, mock_authorization, expression, expected_error_part):
        """Test bitwise operations with non-integer floats (should fail)."""
        tool_call = AIToolCall(
            id="test_bitwise_float_error",
            name="calculator",
            arguments={"expression": expression}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert expected_error_part in str(exc_info.value)

    @pytest.mark.parametrize("expression,expected_error_part", [
        # Complex numbers should fail
        ("5 | (1+2j)", "does not support complex numbers"),
        ("(3+4j) ^ 2", "does not support complex numbers"),
        ("(1+1j) & 7", "does not support complex numbers"),
        ("(2+3j) << 1", "does not support complex numbers"),
        ("(5+1j) >> 2", "does not support complex numbers"),
        ("~(1+2j)", "does not support complex numbers"),
    ])
    def test_bitwise_operations_with_complex_numbers_error(self, calculator_tool, mock_authorization, expression, expected_error_part):
        """Test bitwise operations with complex numbers (should fail)."""
        tool_call = AIToolCall(
            id="test_bitwise_complex_error",
            name="calculator",
            arguments={"expression": expression}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert expected_error_part in str(exc_info.value)

    def test_bitwise_operations_with_zero_imaginary_complex_numbers(self, calculator_tool, mock_authorization):
        """Test bitwise operations with complex numbers having zero imaginary part (should work)."""
        # Complex numbers with exactly zero imaginary part
        expressions = [
            "(5+0j) | 3",
            "5 ^ (3+0j)",
            "(8+0j) & (4+0j)",
            "(5+0j) << 2",
            "(20+0j) >> 2",
            "~(5+0j)",
        ]

        for expression in expressions:
            tool_call = AIToolCall(
                id="test_bitwise_zero_imag",
                name="calculator",
                arguments={"expression": expression}
            )

            result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

            # Should not raise an error
            assert result.error is None
            assert result.content is not None

    @pytest.mark.parametrize("expression,expected_error_part", [
        # Negative shift counts
        ("5 << -1", "Shift count cannot be negative: -1"),
        ("10 >> -2", "Shift count cannot be negative: -2"),

        # Excessive shift counts
        ("5 << 65", "Shift count too large (max 64): 65"),
        ("10 >> 100", "Shift count too large (max 64): 100"),
    ])
    def test_shift_operation_validation_errors(self, calculator_tool, mock_authorization, expression, expected_error_part):
        """Test shift operation validation errors."""
        tool_call = AIToolCall(
            id="test_shift_validation",
            name="calculator",
            arguments={"expression": expression}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert expected_error_part in str(exc_info.value)

    @pytest.mark.parametrize("shift_count", [0, 1, 32, 64])
    def test_shift_operation_boundary_values(self, calculator_tool, mock_authorization, shift_count):
        """Test shift operations with boundary values (should work)."""
        expressions = [
            f"5 << {shift_count}",
            f"1000 >> {shift_count}",
        ]

        for expression in expressions:
            tool_call = AIToolCall(
                id="test_shift_boundary",
                name="calculator",
                arguments={"expression": expression}
            )

            result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

            # Should not raise an error
            assert result.error is None
            assert result.content is not None


class TestCalculatorBaseConversion:
    """Test base conversion functions in the calculator tool."""

    @pytest.fixture
    def calculator_tool(self):
        """Create a calculator tool instance for testing."""
        return CalculatorAITool()

    @pytest.fixture
    def mock_authorization(self):
        """Fixture providing a mocked authorization callback."""
        async def mock_auth_callback(tool_name, arguments, context, destructive):
            return True  # Default to authorized

        return mock_auth_callback

    @pytest.mark.parametrize("expression,expected", [
        # Binary conversion
        ("bin(0)", "0b0"),
        ("bin(1)", "0b1"),
        ("bin(5)", "0b101"),
        ("bin(42)", "0b101010"),
        ("bin(255)", "0b11111111"),

        # Hexadecimal conversion
        ("hex(0)", "0x0"),
        ("hex(10)", "0xa"),
        ("hex(15)", "0xf"),
        ("hex(255)", "0xff"),
        ("hex(4095)", "0xfff"),

        # Octal conversion
        ("oct(0)", "0o0"),
        ("oct(8)", "0o10"),
        ("oct(64)", "0o100"),
        ("oct(511)", "0o777"),
    ])
    def test_base_conversion_success(self, calculator_tool, mock_authorization, expression, expected):
        """Test successful base conversion operations."""
        tool_call = AIToolCall(
            id="test_base_conversion",
            name="calculator",
            arguments={"expression": expression}
        )

        result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert result.content == expected
        assert result.error is None

    @pytest.mark.parametrize("expression", [
        # Whole number floats should work
        "bin(5.0)",
        "hex(255.0)",
        "oct(64.0)",
    ])
    def test_base_conversion_with_whole_number_floats(self, calculator_tool, mock_authorization, expression):
        """Test base conversion with whole number floats (should work)."""
        tool_call = AIToolCall(
            id="test_base_conversion_floats",
            name="calculator",
            arguments={"expression": expression}
        )

        result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        # Should not raise an error
        assert result.error is None
        assert result.content is not None
        assert result.content.startswith(("0b", "0x", "0o"))

    @pytest.mark.parametrize("expression,expected_error_part", [
        # Non-integer floats should fail
        ("bin(5.5)", "requires integer values, got float: 5.5"),
        ("hex(255.7)", "requires integer values, got float: 255.7"),
        ("oct(64.3)", "requires integer values, got float: 64.3"),
    ])
    def test_base_conversion_with_non_integer_floats_error(self, calculator_tool, mock_authorization, expression, expected_error_part):
        """Test base conversion with non-integer floats (should fail)."""
        tool_call = AIToolCall(
            id="test_base_conversion_float_error",
            name="calculator",
            arguments={"expression": expression}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert expected_error_part in str(exc_info.value)

    @pytest.mark.parametrize("expression,expected_error_part", [
        # Complex numbers should fail
        ("bin(1+2j)", "does not support complex numbers"),
        ("hex(3+4j)", "does not support complex numbers"),
        ("oct(5+6j)", "does not support complex numbers"),
    ])
    def test_base_conversion_with_complex_numbers_error(self, calculator_tool, mock_authorization, expression, expected_error_part):
        """Test base conversion with complex numbers (should fail)."""
        tool_call = AIToolCall(
            id="test_base_conversion_complex_error",
            name="calculator",
            arguments={"expression": expression}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert expected_error_part in str(exc_info.value)

    def test_base_conversion_with_zero_imaginary_complex_numbers(self, calculator_tool, mock_authorization):
        """Test base conversion with complex numbers having zero imaginary part (should work)."""
        expressions = [
            "bin(5+0j)",
            "hex(255+0j)",
            "oct(64+0j)",
        ]

        for expression in expressions:
            tool_call = AIToolCall(
                id="test_base_conversion_zero_imag",
                name="calculator",
                arguments={"expression": expression}
            )

            result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

            # Should not raise an error
            assert result.error is None
            assert result.content is not None
            assert result.content.startswith(("0b", "0x", "0o"))

    @pytest.mark.parametrize("func_name", ["bin", "hex", "oct"])
    def test_base_conversion_wrong_argument_count(self, calculator_tool, mock_authorization, func_name):
        """Test base conversion functions with wrong argument count."""
        # Too many arguments
        tool_call = AIToolCall(
            id="test_base_conversion_args",
            name="calculator",
            arguments={"expression": f"{func_name}(5, 10)"}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert "takes exactly 1 argument" in str(exc_info.value)

    def test_base_conversion_negative_numbers(self, calculator_tool, mock_authorization):
        """Test base conversion with negative numbers."""
        expressions = [
            ("bin(-5)", "-0b101"),
            ("hex(-255)", "-0xff"),
            ("oct(-64)", "-0o100"),
        ]

        for expression, expected in expressions:
            tool_call = AIToolCall(
                id="test_base_conversion_negative",
                name="calculator",
                arguments={"expression": expression}
            )

            result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

            assert result.content == expected
            assert result.error is None


class TestCalculatorStringResultIsolation:
    """Test that string results from base conversion cannot be used in other operations."""

    @pytest.fixture
    def calculator_tool(self):
        """Create a calculator tool instance for testing."""
        return CalculatorAITool()

    @pytest.fixture
    def mock_authorization(self):
        """Fixture providing a mocked authorization callback."""
        async def mock_auth_callback(tool_name, arguments, context, destructive):
            return True  # Default to authorized

        return mock_auth_callback

    @pytest.mark.parametrize("expression,expected_error_part", [
        # Binary operations with strings should fail
        ("bin(5) + 1", "Binary operations cannot be performed on string results"),
        ("1 + bin(5)", "Binary operations cannot be performed on string results"),
        ("hex(10) * 2", "Binary operations cannot be performed on string results"),
        ("oct(8) - 1", "Binary operations cannot be performed on string results"),
        ("bin(5) | 3", "Binary operations cannot be performed on string results"),
        ("hex(15) ^ 7", "Binary operations cannot be performed on string results"),
        ("oct(8) & 4", "Binary operations cannot be performed on string results"),
        ("bin(5) << 1", "Binary operations cannot be performed on string results"),
        ("hex(16) >> 2", "Binary operations cannot be performed on string results"),

        # Unary operations with strings should fail
        ("+bin(5)", "Unary operations cannot be performed on string results"),
        ("-hex(10)", "Unary operations cannot be performed on string results"),
        ("~oct(8)", "Unary operations cannot be performed on string results"),

        # Function calls with string arguments should fail
        ("sqrt(bin(4))", "argument 1 cannot be a string result from base conversion"),
        ("sin(hex(1))", "argument 1 cannot be a string result from base conversion"),
        ("abs(oct(5))", "argument 1 cannot be a string result from base conversion"),
        ("floor(bin(10))", "argument 1 cannot be a string result from base conversion"),
    ])
    def test_string_result_isolation_errors(self, calculator_tool, mock_authorization, expression, expected_error_part):
        """Test that string results from base conversion cannot be used in other operations."""
        tool_call = AIToolCall(
            id="test_string_isolation",
            name="calculator",
            arguments={"expression": expression}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert expected_error_part in str(exc_info.value)

    def test_base_conversion_results_are_strings(self, calculator_tool, mock_authorization):
        """Test that base conversion functions return strings that can't be used in arithmetic."""
        # First verify that base conversion works
        tool_call = AIToolCall(
            id="test_base_conversion_string",
            name="calculator",
            arguments={"expression": "bin(42)"}
        )

        result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert result.content == "0b101010"
        assert result.error is None

        # Now verify that trying to use this result in arithmetic fails
        tool_call = AIToolCall(
            id="test_string_arithmetic_fail",
            name="calculator",
            arguments={"expression": "bin(42) + bin(8)"}
        )

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert "Binary operations cannot be performed on string results" in str(exc_info.value)


class TestCalculatorMixedNewFeatures:
    """Test combinations of new features together."""

    @pytest.fixture
    def calculator_tool(self):
        """Create a calculator tool instance for testing."""
        return CalculatorAITool()

    @pytest.fixture
    def mock_authorization(self):
        """Fixture providing a mocked authorization callback."""
        async def mock_auth_callback(tool_name, arguments, context, destructive):
            return True  # Default to authorized

        return mock_auth_callback

    @pytest.mark.parametrize("expression,expected", [
        # Use arithmetic to create values for base conversion
        ("bin(5 + 3)", "0b1000"),
        ("hex(15 * 16)", "0xf0"),
        ("oct(8 ** 2)", "0o100"),

        # Use arithmetic to create values for bitwise operations
        ("(2 + 3) | (1 + 1)", "7"),
        ("(8 - 3) ^ (2 * 2)", "1"),
        ("(3 * 5) & (4 + 3)", "7"),
        ("(2 ** 3) << (1 + 1)", "32"),
        ("(100 // 2) >> (3 - 1)", "12"),

        # Use bitwise results in base conversion
        ("bin(5 | 3)", "0b111"),
        ("hex(15 ^ 7)", "0x8"),
        ("oct(12 & 7)", "0o4"),
        ("bin(5 << 2)", "0b10100"),
        ("hex(32 >> 2)", "0x8"),

        # Complex combinations
        ("bin((5 | 3) + (2 ^ 1))", "0b1010"),
        ("hex((15 & 7) * (3 << 1))", "0x2a"),
        ("oct((20 >> 2) + (4 | 1))", "0o12"),
    ])
    def test_mixed_new_features_success(self, calculator_tool, mock_authorization, expression, expected):
        """Test successful combinations of new bitwise and base conversion features."""
        tool_call = AIToolCall(
            id="test_mixed_features",
            name="calculator",
            arguments={"expression": expression}
        )

        result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

        assert result.content == expected
        assert result.error is None

    def test_complex_bitwise_expression_with_precedence(self, calculator_tool, mock_authorization):
        """Test complex bitwise expressions with operator precedence."""
        # Test that bitwise operators follow correct precedence
        expressions_and_expected = [
            ("5 | 3 & 1", "5"),  # & has higher precedence than |
            ("5 ^ 3 | 1", "7"),  # ^ and | have same precedence, left-to-right
            ("2 << 1 | 1", "5"),  # << has higher precedence than |
            ("8 >> 1 ^ 2", "6"),  # >> has higher precedence than ^
        ]

        for expression, expected in expressions_and_expected:
            tool_call = AIToolCall(
                id="test_precedence",
                name="calculator",
                arguments={"expression": expression}
            )

            result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

            assert result.content == expected
            assert result.error is None

    def test_parentheses_with_bitwise_operations(self, calculator_tool, mock_authorization):
        """Test that parentheses work correctly with bitwise operations."""
        expressions_and_expected = [
            ("(5 | 3) & 7", "7"),
            ("5 | (3 & 1)", "5"),
            ("(8 >> 1) | (2 << 1)", "4"),
            ("~(5 & 3)", "-2"),
            ("(5 ^ 3) << (1 + 1)", "24"),
        ]

        for expression, expected in expressions_and_expected:
            tool_call = AIToolCall(
                id="test_parentheses_bitwise",
                name="calculator",
                arguments={"expression": expression}
            )

            result = asyncio.run(calculator_tool.execute(tool_call, None, mock_authorization))

            assert result.content == expected
            assert result.error is None
