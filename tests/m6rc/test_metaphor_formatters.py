"""Unit tests for the Metaphor formatter functions."""

import pytest

from humbug.m6rc import (
    MetaphorASTNode,
    MetaphorASTNodeType,
    MetaphorParserSyntaxError,
    format_ast,
    format_errors
)


def test_format_ast_empty_root():
    """Test formatting an empty root node."""
    root = MetaphorASTNode(MetaphorASTNodeType.ROOT, "")
    assert format_ast(root) == ""


def test_format_ast_single_text():
    """Test formatting a single text node."""
    root = MetaphorASTNode(MetaphorASTNodeType.ROOT, "")
    text = MetaphorASTNode(MetaphorASTNodeType.TEXT, "Hello world")
    root.attach_child(text)
    assert format_ast(root) == "Hello world\n"


def test_format_ast_single_action():
    """Test formatting a single action node."""
    root = MetaphorASTNode(MetaphorASTNodeType.ROOT, "")
    action = MetaphorASTNode(MetaphorASTNodeType.ACTION, "Test")
    root.attach_child(action)
    assert format_ast(root) == "Action: Test\n"


def test_format_ast_nested_structure():
    """Test formatting a nested structure with multiple node types."""
    root = MetaphorASTNode(MetaphorASTNodeType.ROOT, "")
    context = MetaphorASTNode(MetaphorASTNodeType.CONTEXT, "Main")
    text1 = MetaphorASTNode(MetaphorASTNodeType.TEXT, "Context text")
    nested_context = MetaphorASTNode(MetaphorASTNodeType.CONTEXT, "Nested")
    text2 = MetaphorASTNode(MetaphorASTNodeType.TEXT, "Nested text")

    root.attach_child(context)
    context.attach_child(text1)
    context.attach_child(nested_context)
    nested_context.attach_child(text2)

    expected = (
        "Context: Main\n"
        "    Context text\n"
        "    Context: Nested\n"
        "        Nested text\n"
    )
    assert format_ast(root) == expected


def test_format_ast_all_node_types():
    """Test formatting with all possible node types."""
    root = MetaphorASTNode(MetaphorASTNodeType.ROOT, "")
    role = MetaphorASTNode(MetaphorASTNodeType.ROLE, "Expert")
    context = MetaphorASTNode(MetaphorASTNodeType.CONTEXT, "Setup")
    action = MetaphorASTNode(MetaphorASTNodeType.ACTION, "")
    text = MetaphorASTNode(MetaphorASTNodeType.TEXT, "Review")

    action.attach_child(text)
    root.attach_child(role)
    root.attach_child(context)
    root.attach_child(action)

    expected = (
        "Role: Expert\n"
        "Context: Setup\n"
        "Action:\n"
        "    Review\n"
    )
    assert format_ast(root) == expected


def test_format_errors_single_error():
    """Test formatting a single error."""
    error = MetaphorParserSyntaxError(
        message="Unexpected token",
        filename="test.m6r",
        line=1,
        column=5,
        input_text="test line"
    )

    expected = (
        "----------------\n"
        "Unexpected token: line 1, column 5, file test.m6r\n"
        "    |\n"
        "    v\n"
        "test line\n"
        "----------------\n"
    )
    assert format_errors([error]) == expected


def test_format_errors_multiple_errors():
    """Test formatting multiple errors."""
    errors = [
        MetaphorParserSyntaxError(
            message="First error",
            filename="test1.m6r",
            line=1,
            column=3,
            input_text="abc"
        ),
        MetaphorParserSyntaxError(
            message="Second error",
            filename="test2.m6r",
            line=2,
            column=4,
            input_text="xyz"
        )
    ]

    expected = (
        "----------------\n"
        "First error: line 1, column 3, file test1.m6r\n"
        "  |\n"
        "  v\n"
        "abc\n"
        "----------------\n"
        "Second error: line 2, column 4, file test2.m6r\n"
        "   |\n"
        "   v\n"
        "xyz\n"
        "----------------\n"
    )
    assert format_errors(errors) == expected


def test_format_errors_empty_list():
    """Test formatting an empty error list."""
    assert format_errors([]) == "----------------\n"
