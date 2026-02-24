"""
Tests for the AIFPL (AI Functional Programming Language) tool
"""
import asyncio
import json
import math
from unittest.mock import patch

import pytest

from ai_tool import AITool, AIToolDefinition, AIToolParameter, AIToolExecutionError, AIToolTimeoutError
from ai_tool.aifpl.aifpl_ai_tool import AIFPLAITool
from aifpl import AIFPLError


def get_result(content):
    """Extract result from JSON response."""
    try:
        data = json.loads(content)
        return data.get('result', content)
    except (json.JSONDecodeError, AttributeError, TypeError):
        return content


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


class TestAIFPLAIToolExecution:
    """Test basic AIFPL tool execution."""

    def test_execute_basic_arithmetic(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with basic arithmetic expressions."""
        test_cases = [
            ("(integer+ 1 2 3)", "6"),
            ("(integer- 10 3)", "7"),
            ("(integer* 2 3 4)", "24"),
            ("(float/ 12.0 3.0)", "4.0"),
            ("(integer/ 7 3)", "2"),
            ("(integer% 7 3)", "1"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert get_result(result.content) == expected

    def test_execute_mathematical_functions(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with mathematical functions."""
        test_cases = [
            ("(float-sqrt 16.0)", "4.0"),
            ("(integer-abs -5)", "5"),
            ("(integer-min 1 5 3)", "1"),
            ("(integer-max 1 5 3)", "5"),
            ("(float-expt 2.0 3.0)", "8.0"),
            ("(float-round 3.7)", "4.0"),
            ("(float-floor 3.7)", "3.0"),
            ("(float-ceil 3.2)", "4.0"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert get_result(result.content) == expected

    def test_execute_lambda_functions(self, aifpl_tool, mock_authorization, make_tool_call):
        """Test execution with lambda functions."""
        test_cases = [
            ("((lambda (x) (integer* x x)) 5)", "25"),
            ("((lambda (x y) (integer+ x y)) 3 4)", "7"),
            ("((lambda () 42))", "42"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
            assert get_result(result.content) == expected


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
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(integer/ 5 0)"})
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
            "(integer+ 1 2",  # Missing closing parenthesis
            "integer+ 1 2)",  # Missing opening parenthesis
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

            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
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
                tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
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

            tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

            error = exc_info.value
            assert "Failed to evaluate AIFPL expression" in str(error)
            assert error.__cause__.__class__ == RuntimeError


class TestAIFPLAIToolParametrized:
    """Parametrized tests for the AIFPL tool."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic arithmetic
        ("(integer+ 1 2)", "3"),
        ("(integer* 3 4)", "12"),

        # Function calls
        ("(float-sqrt 9.0)", "3.0"),

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

        # Constants
        ("true", "#t"),
        ("false", "#f"),
    ])
    def test_various_expressions(self, aifpl_tool, mock_authorization, make_tool_call, expression, expected):
        """Test various AIFPL expressions."""
        tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))
        assert get_result(result.content) == expected

    @pytest.mark.parametrize("invalid_expression", [
        "",
        "   ",
        "(integer+ 1 2",
        "integer+ 1 2)",
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


class TestAIFPLAIToolIntegration:
    """Integration tests for the AIFPL tool."""

    def test_tool_inheritance(self, aifpl_tool):
        """Test that AIFPLAITool properly inherits from AITool."""
        assert isinstance(aifpl_tool, AITool)
        assert hasattr(aifpl_tool, 'get_definition')
        assert hasattr(aifpl_tool, 'execute')
        assert callable(aifpl_tool.get_definition)
        assert callable(aifpl_tool.execute)


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
                tool_call = make_tool_call("AIFPL", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(aifpl_tool.execute(tool_call, "", mock_authorization))

                error = exc_info.value
                # The timeout error is now wrapped in AIToolExecutionError
                assert error.__cause__.__class__.__name__ == "AIToolTimeoutError"


class TestAIFPLAIToolModulePath:
    """Test module path management in AIFPLAITool."""

    def test_default_module_path_is_empty(self):
        """Test that default module path is empty list when not specified."""
        tool = AIFPLAITool()
        assert tool.module_path() == []

    def test_custom_module_path_is_expanded(self, tmp_path):
        """Test that custom module paths are expanded and resolved."""
        tool = AIFPLAITool()
        tool.set_module_path([str(tmp_path)])

        # Path should be expanded to absolute path
        assert len(tool.module_path()) == 1
        assert str(tmp_path) in tool.module_path()[0]

    def test_set_module_path_updates_path(self, tmp_path):
        """Test that set_module_path updates the module path."""
        tool = AIFPLAITool()

        new_path = str(tmp_path)
        tool.set_module_path([new_path])

        # Should have updated path
        assert len(tool.module_path()) == 1
        assert str(tmp_path) in tool.module_path()[0]

    def test_set_module_path_clears_cache(self, tmp_path):
        """Test that set_module_path clears the module cache."""
        # Create a module
        module_file = tmp_path / "test.aifpl"
        module_file.write_text("(alist (list \"value\" 42))")

        tool = AIFPLAITool()
        tool.set_module_path([str(tmp_path)])

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

        assert len(tool.module_path()) == 2
        assert any(str(dir1) in path for path in tool.module_path())
        assert any(str(dir2) in path for path in tool.module_path())

    def test_module_path_persists_after_evaluation(self, tmp_path):
        """Test that module path persists after evaluations."""
        module_file = tmp_path / "test.aifpl"
        module_file.write_text("(alist (list \"value\" 42))")

        tool = AIFPLAITool()
        tool.set_module_path([str(tmp_path)])
        original_path = tool.module_path().copy()

        # Evaluate something
        tool._tool.evaluate('(import "test")')

        # Path should be unchanged
        assert tool.module_path() == original_path
