"""Unit tests for the Metaphor formatter functions."""

import pytest

from metaphor import (
    MetaphorRootNode, MetaphorTextNode, MetaphorCodeNode,
    MetaphorRoleNode, MetaphorContextNode, MetaphorActionNode,
    MetaphorASTBuilderSyntaxError, MetaphorFormatVisitor,
    format_errors
)


def test_format_ast_empty_root():
    """Test formatting an empty root node."""
    root = MetaphorRootNode()
    formatter = MetaphorFormatVisitor()
    assert formatter.format(root) == ""


def test_format_ast_single_text():
    """Test formatting a single text node."""
    root = MetaphorRootNode()
    text = MetaphorTextNode("Hello world")
    root.add_child(text)
    formatter = MetaphorFormatVisitor()
    assert formatter.format(root) == "Hello world\n"


def test_format_ast_single_action():
    """Test formatting a single action node."""
    root = MetaphorRootNode()
    action = MetaphorActionNode("Test")
    root.add_child(action)
    formatter = MetaphorFormatVisitor()
    assert formatter.format(root) == "# Action: Test\n\n"


def test_format_ast_nested_structure():
    """Test formatting a nested structure with multiple node types."""
    root = MetaphorRootNode()
    context = MetaphorContextNode("Main")
    text1 = MetaphorTextNode("Context text")
    nested_context = MetaphorContextNode("Nested")
    text2 = MetaphorTextNode("Nested text")

    root.add_child(context)
    context.add_child(text1)
    context.add_child(nested_context)
    nested_context.add_child(text2)

    expected = (
        "# Context: Main\n\n"
        "Context text\n\n"
        "## Context: Nested\n\n"
        "Nested text\n"
    )
    formatter = MetaphorFormatVisitor()
    assert formatter.format(root) == expected


def test_format_ast_all_node_types():
    """Test formatting with all possible node types."""
    root = MetaphorRootNode()
    role = MetaphorRoleNode("Expert")
    context = MetaphorContextNode("Setup")
    action = MetaphorActionNode("")
    text = MetaphorTextNode("Review")

    action.add_child(text)
    root.add_child(role)
    root.add_child(context)
    root.add_child(action)

    expected = (
        "# Role: Expert\n\n"
        "# Context: Setup\n\n"
        "# Action:\n\n"
        "Review\n"
    )
    formatter = MetaphorFormatVisitor()
    assert formatter.format(root) == expected


def test_format_errors_single_error():
    """Test formatting a single error."""
    error = MetaphorASTBuilderSyntaxError(
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
        "----------------"
    )
    assert format_errors([error]) == expected


def test_format_errors_multiple_errors():
    """Test formatting multiple errors."""
    errors = [
        MetaphorASTBuilderSyntaxError(
            message="First error",
            filename="test1.m6r",
            line=1,
            column=3,
            input_text="abc"
        ),
        MetaphorASTBuilderSyntaxError(
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
        "----------------"
    )
    assert format_errors(errors) == expected


def test_format_errors_empty_list():
    """Test formatting an empty error list."""
    assert format_errors([]) == "----------------"


def test_format_ast_remove_blank_lines():
    """Test formatting an AST that has an unecessary blank line."""
    root = MetaphorRootNode()
    text1 = MetaphorTextNode("")
    context = MetaphorContextNode("Main")
    text2 = MetaphorTextNode("")
    text3 = MetaphorTextNode("Context text")

    root.add_child(text1)
    root.add_child(context)
    context.add_child(text2)
    context.add_child(text3)

    expected = (
        "# Context: Main\n\n"
        "Context text\n"
    )
    formatter = MetaphorFormatVisitor()
    assert formatter.format(root) == expected


def test_format_ast_with_code():
    """Test formatting AST with code blocks."""
    root = MetaphorRootNode()
    context = MetaphorContextNode("Code Example")
    text = MetaphorTextNode("Here's some code:")
    code = MetaphorCodeNode("```python\ndef hello():\n    print('Hello world')\n```")

    root.add_child(context)
    context.add_child(text)
    context.add_child(code)

    expected = (
        "# Context: Code Example\n\n"
        "Here's some code:\n"
        "```python\n"
        "def hello():\n"
        "    print('Hello world')\n"
        "```\n"
    )
    formatter = MetaphorFormatVisitor()
    assert formatter.format(root) == expected
