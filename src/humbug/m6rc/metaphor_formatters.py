"""Functions for formatting Metaphor AST nodes and error messages."""

import io
from typing import List, TextIO, Dict, Final, Type

from .metaphor_ast_node import (
    MetaphorASTNode, MetaphorRootNode, MetaphorTextNode, MetaphorCodeNode,
    MetaphorRoleNode, MetaphorContextNode, MetaphorActionNode
)
from .metaphor_parser import MetaphorParserSyntaxError


NODE_CLASS_MAP: Final[Dict[Type[MetaphorASTNode], str]] = {
    MetaphorActionNode: "Action:",
    MetaphorContextNode: "Context:",
    MetaphorRoleNode: "Role:"
}


def format_ast(node: MetaphorASTNode) -> str:
    """Format an AST node and its children as a string.

    Args:
        node: The root node to format

    Returns:
        Formatted string representation of the AST
    """
    output = io.StringIO()
    _format_node(node, 0, output)
    return output.getvalue()


def _format_node(node: MetaphorASTNode, depth: int, out: TextIO) -> None:
    """Recursively format a node and its children.

    Args:
        node: Current node being processed
        depth: Current tree depth
        out: Output buffer to write to
    """
    if not isinstance(node, MetaphorRootNode):
        if isinstance(node, MetaphorCodeNode):
            out.write(f"{node.value}\n")
            return

        value = node.value()
        if isinstance(node, MetaphorTextNode):
            if value == "":
                current_pos = out.tell()
                if current_pos <= 1:
                    return

                out.seek(current_pos - 2)
                prev_char = out.read(2)
                out.seek(current_pos)
                if prev_char == '\n\n':
                    return

            out.write(f"{value}\n")
            return

        # If we don't have a blank line before this block heading then add one
        current_pos = out.tell()
        if current_pos > 1:
            out.seek(current_pos - 2)
            prev_char = out.read(2)
            out.seek(current_pos)
            if prev_char != '\n\n':
                out.write("\n")

        indent = "#" * depth
        keyword = NODE_CLASS_MAP.get(node.__class__, "")
        out.write(f"{indent} {keyword}")
        if value:
            out.write(f" {value}")

        out.write("\n\n")

    for child in node.children:
        _format_node(child, depth + 1, out)


def format_errors(errors: List[MetaphorParserSyntaxError]) -> str:
    """Format a list of syntax errors as a string.

    Args:
        errors: List of syntax errors to format

    Returns:
        Formatted error string with each error on separate lines
    """
    output = io.StringIO()

    for error in errors:
        caret = " " * (error.column - 1)
        error_message = (
            f"{error.message}: line {error.line}, column {error.column}, "
            f"file {error.filename}\n{caret}|\n{caret}v\n{error.input_text}"
        )
        output.write(f"----------------\n{error_message}\n")

    output.write("----------------\n")
    return output.getvalue()
