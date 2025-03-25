"""Functions for formatting Metaphor AST nodes and error messages."""

import io
from typing import List, TextIO, Dict, Final

from .metaphor_ast_node import MetaphorASTNode, MetaphorASTNodeType
from .metaphor_parser import MetaphorParserSyntaxError


NODE_TYPE_MAP: Final[Dict[MetaphorASTNodeType, str]] = {
    MetaphorASTNodeType.ACTION: "Action:",
    MetaphorASTNodeType.CONTEXT: "Context:",
    MetaphorASTNodeType.ROLE: "Role:"
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
    if node.node_type != MetaphorASTNodeType.ROOT:
        indent = " " * ((depth - 1) * 4)
        if node.node_type == MetaphorASTNodeType.TEXT:
            out.write(f"{indent}{node.value}\n")
            return

        keyword = NODE_TYPE_MAP.get(node.node_type, "")
        out.write(f"{indent}{keyword}")
        if node.value:
            out.write(f" {node.value}")
        out.write("\n")

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
