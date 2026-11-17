"""
Tests for the Menai (AI Functional Programming Language) tool
"""
import asyncio
import json
import math
import time
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from ai_tool import AITool, AIToolDefinition, AIToolParameter, AIToolExecutionError, AIToolTimeoutError
from menai_ai_tool.menai_ai_tool import MenaiAITool
from menai import Menai, MenaiError
from mindspace.mindspace import Mindspace

from tests.conftest import MockRequester


def get_result(content):
    """Extract result from JSON response."""
    try:
        data = json.loads(content)
        return data.get('result', content)
    except (json.JSONDecodeError, AttributeError, TypeError):
        return content


@pytest.fixture
def menai_tool():
    """Fixture providing an Menai tool instance."""
    return MenaiAITool()


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    async def mock_auth_callback(tool_name, arguments, context, requester_ref, destructive):
        return True  # Default to authorized

    return mock_auth_callback


class TestMenaiAIToolExecution:
    """Test basic Menai tool execution."""

    def test_execute_basic_arithmetic(self, menai_tool, mock_authorization, make_tool_call):
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
            tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))
            assert get_result(result.content) == expected

    def test_execute_mathematical_functions(self, menai_tool, mock_authorization, make_tool_call):
        """Test execution with mathematical functions."""
        test_cases = [
            ("(float-sqrt 16.0)", "4.0"),
            ("(integer-abs -5)", "5"),
            ("(integer-min 1 5 3)", "1"),
            ("(integer-max 1 5 3)", "5"),
            ("(float-expn 2.0 3.0)", "8.0"),
            ("(float-round 3.7)", "4.0"),
            ("(float-floor 3.7)", "3.0"),
            ("(float-ceil 3.2)", "4.0"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))
            assert get_result(result.content) == expected

    def test_execute_lambda_functions(self, menai_tool, mock_authorization, make_tool_call):
        """Test execution with lambda functions."""
        test_cases = [
            ("((lambda (x) (integer* x x)) 5)", "25"),
            ("((lambda (x y) (integer+ x y)) 3 4)", "7"),
            ("((lambda () 42))", "42"),
        ]

        for expression, expected in test_cases:
            tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": expression})
            result = asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))
            assert get_result(result.content) == expected


class TestMenaiAIToolErrorHandling:
    """Test Menai tool error handling."""

    def test_execute_missing_expression(self, menai_tool, mock_authorization, make_tool_call):
        """Test execution without expression argument."""
        tool_call = make_tool_call("Menai", {"operation": "evaluate"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

        error = exc_info.value
        assert "expression" in str(error).lower()

    def test_execute_empty_expression(self, menai_tool, mock_authorization, make_tool_call):
        """Test execution with empty expression."""
        tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": ""})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

        error = exc_info.value
        assert "empty expression" in str(error).lower()

    def test_execute_non_string_expression(self, menai_tool, mock_authorization, make_tool_call):
        """Test execution with non-string expression."""
        tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": 123})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

        error = exc_info.value
        assert "Expression must be a string" in str(error)

    def test_execute_invalid_syntax_error(self, menai_tool, mock_authorization, make_tool_call):
        """Test execution with invalid Menai syntax."""
        invalid_expressions = [
            "(integer+ 1 2",  # Missing closing parenthesis
            "integer+ 1 2)",  # Missing opening parenthesis
            # Removed "()" as it's valid (empty list)
            "(unknown-function 1 2)",  # Unknown function
        ]

        for expression in invalid_expressions:
            tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": expression})
            with pytest.raises(AIToolExecutionError):
                asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

    def test_execute_menai_error_wrapping(self, menai_tool, mock_authorization, make_tool_call):
        """Test that MenaiError is properly wrapped."""
        with patch.object(Menai, 'evaluate_and_format') as mock_evaluate:
            mock_evaluate.side_effect = MenaiError("Custom Menai error")

            tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

            error = exc_info.value
            assert "Custom Menai error" in str(error)
            assert error.__cause__.__class__ == MenaiError

    def test_execute_timeout_error(self, menai_tool, mock_authorization, make_tool_call):
        """Test execution timeout handling."""
        # Mock the specific method on the tool instance to avoid affecting other tests
        with patch.object(menai_tool, '_evaluate_expression_sync') as mock_evaluate:
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
                tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

                error = exc_info.value
                assert "Menai calculation timed out" in str(error)
                # The timeout error is now wrapped in AIToolExecutionError
                assert error.__cause__.__class__.__name__ == "AIToolTimeoutError"

    def test_execute_unexpected_error_handling(self, menai_tool, mock_authorization, make_tool_call):
        """Test handling of unexpected errors."""
        with patch.object(Menai, 'evaluate_and_format') as mock_evaluate:
            mock_evaluate.side_effect = RuntimeError("Unexpected error")

            tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

            error = exc_info.value
            assert "Failed to evaluate Menai expression" in str(error)
            assert error.__cause__.__class__ == RuntimeError


class TestMenaiAIToolParametrized:
    """Parametrized tests for the Menai tool."""

    @pytest.mark.parametrize("expression,expected", [
        # Basic arithmetic
        ("(integer+ 1 2)", "3"),
        ("(integer* 3 4)", "12"),

        # Function calls
        ("(float-sqrt 9.0)", "3.0"),

        # Lists
        ("(list 1 2 3)", "(1 2 3)"),
        ("(list-first (list 10 20 30))", "10"),
        ("(list-length (list 1 2 3 4))", "4"),

        # Strings
        ('(string-length "test")', "4"),
        ('(string-upcase "hello")', '"HELLO"'),

        # Booleans
        ("(and #t #t)", "#t"),
        ("(or #f #t)", "#t"),
        ("(boolean-not #f)", "#t"),
    ])
    def test_various_expressions(self, menai_tool, mock_authorization, make_tool_call, expression, expected):
        """Test various Menai expressions."""
        tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": expression})
        result = asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))
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
    def test_invalid_expressions_raise_errors(self, menai_tool, mock_authorization, make_tool_call, invalid_expression):
        """Test that various invalid expressions raise AIToolExecutionError."""
        tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": invalid_expression})
        with pytest.raises(AIToolExecutionError):
            asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))


class TestMenaiAIToolHelpGate:
    """Test the Menai help-read gating mechanism."""

    def test_evaluate_blocked_without_help(self, menai_tool, mock_authorization, make_tool_call):
        """Evaluate raises an error when Menai help has not been read."""
        requester = MockRequester(menai_help_read=False)
        tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(menai_tool.execute(tool_call, requester, mock_authorization))

        assert "must read the Menai language documentation" in str(exc_info.value)

    def test_evaluate_allowed_after_help(self, menai_tool, mock_authorization, make_tool_call):
        """Evaluate succeeds when Menai help has been read."""
        requester = MockRequester(menai_help_read=True)
        tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
        result = asyncio.run(menai_tool.execute(tool_call, requester, mock_authorization))
        assert get_result(result.content) == "3"

    def test_evaluate_blocked_with_plain_requester(self, menai_tool, mock_authorization, make_tool_call):
        """Evaluate raises an error when requester has no menai_help_read method."""
        tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(menai_tool.execute(tool_call, "", mock_authorization))

        assert "must read the Menai language documentation" in str(exc_info.value)


class TestMenaiAIToolIntegration:
    """Integration tests for the Menai tool."""

    def test_tool_inheritance(self, menai_tool):
        """Test that MenaiAITool properly inherits from AITool."""
        assert isinstance(menai_tool, AITool)
        assert hasattr(menai_tool, 'get_definition')
        assert hasattr(menai_tool, 'execute')
        assert callable(menai_tool.get_definition)
        assert callable(menai_tool.execute)


class TestMenaiAIToolTimeout:
    """Test Menai tool timeout behavior."""

    def test_timeout_configuration(self, menai_tool):
        """Test that timeout is configured correctly."""
        # The timeout should be 10 seconds for Menai (longer than calculator's 5)
        # This is tested indirectly through the timeout error test above
        assert hasattr(menai_tool, '_evaluate_expression_sync')

    def test_timeout_prevents_infinite_loops(self, menai_tool, mock_authorization, make_tool_call):
        """Test that timeout prevents infinite computation."""
        # Mock the specific method on the tool instance to avoid affecting other tests
        with patch.object(menai_tool, '_evaluate_expression_sync') as mock_evaluate:
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
                tool_call = make_tool_call("Menai", {"operation": "evaluate", "expression": "(integer+ 1 2)"})
                with pytest.raises(AIToolExecutionError) as exc_info:
                    asyncio.run(menai_tool.execute(tool_call, MockRequester(), mock_authorization))

                error = exc_info.value
                # The timeout error is now wrapped in AIToolExecutionError
                assert error.__cause__.__class__.__name__ == "AIToolTimeoutError"


class TestMenaiAIToolModulePath:
    """Test module path management in MenaiAITool."""

    def test_module_path_without_mindspace_is_stdlib_only(self):
        """With no mindspace open the module path is the standard library alone."""
        tool = MenaiAITool()
        assert tool._module_path() == Menai.build_module_path(None)

    def test_module_path_places_mindspace_modules_before_stdlib(self, tmp_path):
        """The mindspace's menai-lib directory precedes the standard library."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        module_path = tool._module_path()
        assert module_path[0] == str(tmp_path / "menai-lib")
        assert module_path[-1] == str(Menai.stdlib_path())

    def test_module_path_ignores_empty_mindspace_path(self):
        """An empty mindspace path falls back to the standard library alone."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = ""

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        assert tool._module_path() == Menai.build_module_path(None)

    def test_module_path_tracks_mindspace_changes(self, tmp_path):
        """The module path reflects the currently set mindspace."""
        tool = MenaiAITool()

        first = MagicMock()
        first.mindspace_path.return_value = str(tmp_path / "one")
        tool.set_mindspace(first)
        assert tool._module_path()[0] == str(tmp_path / "one" / "menai-lib")

        second = MagicMock()
        second.mindspace_path.return_value = str(tmp_path / "two")
        tool.set_mindspace(second)
        assert tool._module_path()[0] == str(tmp_path / "two" / "menai-lib")


class TestMenaiLibInventory:
    """Tests for the menai-lib inventory shown in the Menai tool help."""

    def test_no_mindspace_means_no_inventory(self):
        """With no mindspace open the help carries no inventory section."""
        tool = MenaiAITool()

        assert tool._menai_lib_inventory() == ""

    def test_empty_menai_lib_means_no_inventory(self, tmp_path):
        """A menai-lib with no modules adds nothing to the help."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        (tmp_path / "menai-lib").mkdir()

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        assert tool._menai_lib_inventory() == ""

    def test_modules_listed_with_their_exports(self, tmp_path):
        """Each module is listed with its import name and exported names."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "csv-normalise.menai").write_text(
            "(let ((normalise (lambda (x) x)))\n  (export normalise))\n", encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        inventory = tool._menai_lib_inventory()
        assert "csv-normalise (exports: normalise)" in inventory

    def test_subdirectory_modules_use_path_import_names(self, tmp_path):
        """A module in a subdirectory is listed by its path import name."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        helpers = tmp_path / "menai-lib" / "lib"
        helpers.mkdir(parents=True)
        (helpers / "helpers.menai").write_text(
            "(let ((square (lambda (x) (integer* x x))))\n  (export square))\n", encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        inventory = tool._menai_lib_inventory()
        assert "lib/helpers (exports: square)" in inventory

    def test_help_appends_the_inventory(self, tmp_path):
        """The full help ends with the menai-lib inventory section."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "stats-extra.menai").write_text(
            "(let ((mean (lambda (xs) 1.0)))\n  (export mean))\n", encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        help_text = tool.get_detailed_help()
        assert "Mindspace library" in help_text
        assert "stats-extra (exports: mean)" in help_text

    def test_module_without_export_form_lists_name_only(self, tmp_path):
        """A module with no export form is listed without exports."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "notes.menai").write_text("(integer+ 1 2)\n", encoding="utf-8")

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        inventory = tool._menai_lib_inventory()
        assert "- notes" in inventory
        assert "exports:" not in inventory

    def test_suites_are_not_listed_as_modules(self, tmp_path):
        """A *.test.menai file is a test for a module, not a library module."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "stats.menai").write_text(
            "(let ((mean (lambda (xs) 1.0)))\n  (export mean))\n", encoding="utf-8"
        )
        (menai_lib / "stats.test.menai").write_text(
            "(let ((tests (list)))\n  (export tests))\n", encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        inventory = tool._menai_lib_inventory()
        assert "stats (exports: mean)" in inventory
        assert "stats.test" not in inventory

    def test_a_library_of_only_suites_is_not_listed(self, tmp_path):
        """Suites alone leave the help without an inventory section."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "probe.test.menai").write_text(
            "(let ((tests (list)))\n  (export tests))\n", encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        assert tool._menai_lib_inventory() == ""

    def test_module_description_comes_from_the_first_line(self, tmp_path):
        """A module's first-line comment is shown as its description."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "csv-normalise.menai").write_text(
            "; csv-normalise — normalises a CSV table's column names.\n"
            ";\n"
            "; Lowercases every header and replaces spaces with hyphens.\n"
            "(let ((normalise (lambda (table) table)))\n  (export normalise))\n",
            encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        inventory = tool._menai_lib_inventory()
        assert (
            "- csv-normalise (exports: normalise) — normalises a CSV table's column names."
            in inventory
        )

    def test_module_without_a_description_is_listed_without_one(self, tmp_path):
        """A module with no first-line comment is listed bare."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "notes.menai").write_text(
            "(let ((note (lambda () 1)))\n  (export note))\n", encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        assert "- notes (exports: note)" in tool._menai_lib_inventory()

    def test_header_comment_without_a_dash_is_not_a_description(self, tmp_path):
        """A first line that is not a `name — description` comment is left out."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "notes.menai").write_text(
            ";; internal helpers\n(let ((note (lambda () 1)))\n  (export note))\n",
            encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        assert "- notes (exports: note)\n" in tool._menai_lib_inventory()

    def test_inventory_states_the_first_line_convention(self, tmp_path):
        """The inventory tells the AI to describe its modules on the first line."""
        mindspace = MagicMock()
        mindspace.mindspace_path.return_value = str(tmp_path)
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "notes.menai").write_text(
            "(let ((note (lambda () 1)))\n  (export note))\n", encoding="utf-8"
        )

        tool = MenaiAITool()
        tool.set_mindspace(mindspace)

        inventory = tool._menai_lib_inventory()
        assert "`; <name> — <one-line description>` comment" in inventory


def make_mindspace(root: Path) -> Mindspace:
    """
    Build a Mindspace rooted at a directory without opening one for real.

    Tool path resolution needs only the root, so this avoids the journal,
    watchers and session state that opening a mindspace would start.

    Args:
        root: The directory to treat as the mindspace root

    Returns:
        A Mindspace whose path is root
    """
    mindspace = Mindspace(lambda: None, lambda: None)
    mindspace._path = str(root)  # pylint: disable=protected-access
    return mindspace


PASSING_SUITE = """
(let ((t (import "menai-test"))
      (assert-equal (:: t assert-equal))
      (expect-pass (:: t expect-pass))
      (expect-fail (:: t expect-fail))
      (tests
       (list
         (expect-pass "arithmetic" (lambda () (assert-equal (integer+ 1 2) 3)))
         (expect-fail "raises" (lambda () (error "boom")) "boom"))))
  (export tests))
"""

FAILING_SUITE = """
(let ((t (import "menai-test"))
      (assert-equal (:: t assert-equal))
      (expect-pass (:: t expect-pass))
      (tests
       (list
         (expect-pass "arithmetic" (lambda () (assert-equal (integer+ 1 2) 4)))
         (expect-pass "trivial" (lambda () (assert-equal 1 1))))))
  (export tests))
"""

BRANCHED_SUITE = """
(let ((t (import "menai-test"))
      (assert-equal (:: t assert-equal))
      (expect-pass (:: t expect-pass))
      (tests
       (list
         (list "group"
               (list
                 (expect-pass "inner" (lambda () (assert-equal 1 2)))))
         (expect-pass "top" (lambda () (assert-equal 1 1))))))
  (export tests))
"""

MALFORMED_SUITE = """
(let ((tests (list "not-a-node")))
  (export tests))
"""

BROKEN_SUITE = """
(let ((tests (integer+ 1 "two")))
  (export tests))
"""


class TestMenaiTestRunner:
    """Tests for the run_tests operation."""

    def test_suite_runs_and_reports_totals(self, tmp_path, mock_authorization, make_tool_call):
        """A passing suite reports every leaf as passed."""
        (tmp_path / "probe.test.menai").write_text(PASSING_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})
        result = asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        payload = json.loads(result.content)
        assert payload["file"] == "probe.test.menai"
        assert payload["total"] == 2
        assert payload["passed"] == 2
        assert payload["failed"] == 0
        assert payload["failures"] == []

    def test_failing_leaf_is_reported_without_raising(self, tmp_path, mock_authorization, make_tool_call):
        """A failing leaf is a result, not an error, and the run continues."""
        (tmp_path / "probe.test.menai").write_text(FAILING_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})
        result = asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        payload = json.loads(result.content)
        assert payload["total"] == 2
        assert payload["passed"] == 1
        assert payload["failed"] == 1
        assert [failure["test"] for failure in payload["failures"]] == ["arithmetic"]
        assert payload["failures"][0]["error"]

    def test_failure_path_includes_branch_names(self, tmp_path, mock_authorization, make_tool_call):
        """A failure inside a branch is named by its full path."""
        (tmp_path / "probe.test.menai").write_text(BRANCHED_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})
        result = asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        payload = json.loads(result.content)
        assert payload["total"] == 2
        assert payload["failed"] == 1
        assert payload["failures"][0]["test"] == "group > inner"

    def test_suite_imports_modules_from_menai_lib(self, tmp_path, mock_authorization, make_tool_call):
        """A suite resolves the module under test through the mindspace's menai-lib."""
        menai_lib = tmp_path / "menai-lib"
        menai_lib.mkdir()
        (menai_lib / "doubler.menai").write_text(
            "(let ((double (lambda (n) (integer* n 2))))\n  (export double))\n", encoding="utf-8"
        )
        (tmp_path / "doubler.test.menai").write_text(
            """
(let ((d (import "doubler"))
      (t (import "menai-test"))
      (assert-equal (:: t assert-equal))
      (expect-pass (:: t expect-pass))
      (double (:: d double))
      (tests (list (expect-pass "doubles" (lambda () (assert-equal (double 4) 8))))))
  (export tests))
""",
            encoding="utf-8"
        )
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "doubler.test.menai"})
        result = asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        payload = json.loads(result.content)
        assert payload["total"] == 1
        assert payload["passed"] == 1

    def test_malformed_suite_raises_execution_error(self, tmp_path, mock_authorization, make_tool_call):
        """A suite whose tests are not a node-list is rejected."""
        (tmp_path / "probe.test.menai").write_text(MALFORMED_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})
        with pytest.raises(AIToolExecutionError):
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

    def test_suite_that_fails_to_load_raises_execution_error(self, tmp_path, mock_authorization, make_tool_call):
        """A suite that fails to compile or evaluate is reported as an error."""
        (tmp_path / "probe.test.menai").write_text(BROKEN_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert isinstance(exc_info.value.__cause__, MenaiError)

    def test_timeout_raises_timeout_error(self, tmp_path, mock_authorization, make_tool_call):
        """A suite that overruns the timeout is reported as a timeout."""
        (tmp_path / "probe.test.menai").write_text(PASSING_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))
        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})

        def slow_runner(*_args, **_kwargs):
            time.sleep(0.2)
            return []

        with patch("menai_ai_tool.menai_ai_tool.run_test_suite", side_effect=slow_runner), \
                patch.object(MenaiAITool, "TEST_TIMEOUT_SECONDS", 0.01):
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert isinstance(exc_info.value.__cause__, AIToolTimeoutError)
        assert "Menai test run timed out" in str(exc_info.value)

    def test_run_tests_blocked_without_help(self, tmp_path, mock_authorization, make_tool_call):
        """The suite runner is gated on the Menai help having been read."""
        (tmp_path / "probe.test.menai").write_text(PASSING_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(menai_help_read=False), mock_authorization))

        assert "must read the Menai language documentation" in str(exc_info.value)


class TestMenaiTestRunnerPaths:
    """Tests for the confinement of the run_tests test_file argument."""

    def test_no_mindspace_is_rejected(self, mock_authorization, make_tool_call):
        """Running a suite with no mindspace open is refused."""
        tool = MenaiAITool()

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.test.menai"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert "No mindspace is open" in str(exc_info.value)

    def test_path_outside_the_mindspace_is_rejected(self, tmp_path, mock_authorization, make_tool_call):
        """A suite outside the mindspace is refused before the runner is called."""
        outside = tmp_path.parent / "outside.test.menai"
        outside.write_text(PASSING_SUITE, encoding="utf-8")
        (tmp_path / "inside").mkdir()
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path / "inside"))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": str(outside)})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert "outside mindspace boundaries" in str(exc_info.value)

    def test_path_inside_humbug_is_rejected(self, tmp_path, mock_authorization, make_tool_call):
        """A suite inside .humbug/ is refused, as for every other tool path."""
        (tmp_path / ".humbug").mkdir()
        (tmp_path / ".humbug" / "probe.test.menai").write_text(PASSING_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": ".humbug/probe.test.menai"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert ".humbug" in str(exc_info.value)

    def test_path_without_the_suite_suffix_is_rejected(self, tmp_path, mock_authorization, make_tool_call):
        """A file that is not named *.test.menai is refused."""
        (tmp_path / "probe.menai").write_text(PASSING_SUITE, encoding="utf-8")
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "probe.menai"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert ".test.menai" in str(exc_info.value)

    def test_missing_suite_is_rejected(self, tmp_path, mock_authorization, make_tool_call):
        """A suite path that names no file is refused."""
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": "absent.test.menai"})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert "No such test file" in str(exc_info.value)

    def test_empty_test_file_is_rejected(self, tmp_path, mock_authorization, make_tool_call):
        """An empty test_file argument is refused."""
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call("Menai", {"operation": "run_tests", "test_file": ""})
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert "test_file must be a non-empty string" in str(exc_info.value)


class TestMenaiTestRunnerDefinition:
    """Tests for the parameter schema the model sees."""

    def test_both_parameters_are_present_and_optional(self):
        """Neither expression nor test_file is required in the schema."""
        parameters = {parameter.name: parameter for parameter in MenaiAITool().get_definition().parameters}

        assert set(parameters) == {"operation", "expression", "test_file"}
        assert parameters["expression"].required is False
        assert parameters["test_file"].required is False
        assert parameters["operation"].required is True

    def test_run_tests_rejects_an_expression(self, tmp_path, mock_authorization, make_tool_call):
        """run_tests refuses the evaluate argument."""
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call(
            "Menai",
            {"operation": "run_tests", "test_file": "probe.test.menai", "expression": "(integer+ 1 2)"}
        )
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert "expression" in str(exc_info.value)

    def test_evaluate_rejects_a_test_file(self, tmp_path, mock_authorization, make_tool_call):
        """evaluate refuses the run_tests argument."""
        tool = MenaiAITool()
        tool.set_mindspace(make_mindspace(tmp_path))

        tool_call = make_tool_call(
            "Menai",
            {"operation": "evaluate", "expression": "(integer+ 1 2)", "test_file": "probe.test.menai"}
        )
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, MockRequester(), mock_authorization))

        assert "test_file" in str(exc_info.value)

    def test_brief_description_mentions_test_suites(self):
        """The system prompt summary tells the model the tool runs suites."""
        assert "test suites" in MenaiAITool().get_brief_description()
