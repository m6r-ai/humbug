"""An embedded compiler for the Metaphor language."""

# Export main classes so users can import directly from m6rclib
from .metaphor_ast_node import MetaphorASTNode, MetaphorASTNodeType
from .metaphor_parser import MetaphorParser, MetaphorParserError, MetaphorParserSyntaxError
from .metaphor_formatters import format_ast, format_errors

# List what should be available when using `from m6rclib import *`
__all__ = [
    "MetaphorASTNode",
    "MetaphorASTNodeType",
    "MetaphorParser",
    "MetaphorParserError",
    "MetaphorParserSyntaxError",
    "format_ast",
    "format_errors"
]
