"""Shared fixtures and utilities for Menai tests."""

import pytest
from typing import Any

from menai import Menai


@pytest.fixture
def menai():
    """Create a fresh Menai instance for each test."""
    return Menai()


class MenaiTestHelpers:
    """Helper utilities for Menai testing."""

    @staticmethod
    def assert_lisp_format(result: str, expected: str) -> None:
        """Assert that LISP-formatted result matches expected format."""
        assert result == expected, f"Expected LISP format '{expected}', got '{result}'"

    @staticmethod
    def assert_evaluates_to(menai: Menai, expression: str, expected: str) -> None:
        """Assert that expression evaluates to expected LISP-formatted result."""
        result = menai.evaluate_and_format(expression)
        MenaiTestHelpers.assert_lisp_format(result, expected)

    @staticmethod
    def assert_python_result(menai: Menai, expression: str, expected: Any) -> None:
        """Assert that expression evaluates to expected Python object."""
        result = menai.evaluate(expression)
        assert result == expected, f"Expected Python result {expected!r}, got {result!r}"

    @staticmethod
    def build_nested_expression(operator: str, depth: int, base_value: str = "1") -> str:
        """Build deeply nested expression for recursion testing."""
        if depth <= 0:
            return base_value

        inner = MenaiTestHelpers.build_nested_expression(operator, depth - 1, base_value)
        return f"({operator} {base_value} {inner})"

    @staticmethod
    def build_list_expression(elements: list) -> str:
        """Build a list expression from Python elements."""
        if not elements:
            return "(list)"

        element_strs = []
        for elem in elements:
            if isinstance(elem, str):
                element_strs.append(f'"{elem}"')
            elif isinstance(elem, bool):
                element_strs.append("#t" if elem else "#f")
            elif isinstance(elem, list):
                element_strs.append(MenaiTestHelpers.build_list_expression(elem))
            else:
                element_strs.append(str(elem))

        return f"(list {' '.join(element_strs)})"


@pytest.fixture
def helpers():
    """Provide test helper utilities."""
    return MenaiTestHelpers
