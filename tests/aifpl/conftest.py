"""Shared fixtures and utilities for AIFPL tests."""

import pytest
from typing import Any, Callable

from aifpl import AIFPL, AIFPLError


@pytest.fixture
def aifpl():
    """Create a fresh AIFPL instance for each test."""
    return AIFPL()


@pytest.fixture
def aifpl_custom():
    """Factory for AIFPL instances with custom configuration."""
    def _create_aifpl(max_depth: int = 100, imaginary_tolerance: float = 1e-10) -> AIFPL:
        return AIFPL(max_depth=max_depth, imaginary_tolerance=imaginary_tolerance)
    return _create_aifpl


class AIFPLTestHelpers:
    """Helper utilities for AIFPL testing."""
    
    @staticmethod
    def assert_lisp_format(result: str, expected: str) -> None:
        """Assert that LISP-formatted result matches expected format."""
        assert result == expected, f"Expected LISP format '{expected}', got '{result}'"
    
    @staticmethod
    def assert_evaluates_to(aifpl: AIFPL, expression: str, expected: str) -> None:
        """Assert that expression evaluates to expected LISP-formatted result."""
        result = aifpl.evaluate_and_format(expression)
        AIFPLTestHelpers.assert_lisp_format(result, expected)
    
    @staticmethod
    def assert_python_result(aifpl: AIFPL, expression: str, expected: Any) -> None:
        """Assert that expression evaluates to expected Python object."""
        result = aifpl.evaluate(expression)
        assert result == expected, f"Expected Python result {expected!r}, got {result!r}"
    
    @staticmethod
    def build_nested_expression(operator: str, depth: int, base_value: str = "1") -> str:
        """Build deeply nested expression for recursion testing."""
        if depth <= 0:
            return base_value
        
        inner = AIFPLTestHelpers.build_nested_expression(operator, depth - 1, base_value)
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
                element_strs.append(AIFPLTestHelpers.build_list_expression(elem))
            else:
                element_strs.append(str(elem))
        
        return f"(list {' '.join(element_strs)})"


@pytest.fixture
def helpers():
    """Provide test helper utilities."""
    return AIFPLTestHelpers