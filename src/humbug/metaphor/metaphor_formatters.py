"""Functions for formatting Metaphor AST nodes and error messages."""

import io
from typing import List

from humbug.metaphor.metaphor_ast_node import MetaphorASTNode
from humbug.metaphor.metaphor_parser import MetaphorParserSyntaxError


# Import the new visitor
from .metaphor_format_visitor import MetaphorFormatVisitor


def format_ast(node: MetaphorASTNode) -> str:
    """Format an AST node and its children as a string.

    Args:
        node: The root node to format

    Returns:
        Formatted string representation of the AST
    """
    formatter = MetaphorFormatVisitor()
    return formatter.format(node)


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
