"""An embedded compiler for the Metaphor language."""

# Export main classes so users can import directly from m6rclib
from .metaphor_ast_node import (
    MetaphorASTNode, MetaphorRootNode, MetaphorTextNode,
    MetaphorCodeNode, MetaphorRoleNode, MetaphorContextNode, MetaphorActionNode,
    MetaphorASTVisitor
)
from .metaphor_parser import MetaphorParser, MetaphorParserError, MetaphorParserSyntaxError
from .metaphor_formatters import format_ast, format_errors


# List what should be available when using `from m6rclib import *`
__all__ = [
    "MetaphorASTNode",
    "MetaphorRootNode",
    "MetaphorTextNode",
    "MetaphorCodeNode",
    "MetaphorRoleNode",
    "MetaphorContextNode",
    "MetaphorActionNode",
    "MetaphorASTVisitor",
    "MetaphorParser",
    "MetaphorParserError",
    "MetaphorParserSyntaxError",
    "format_ast",
    "format_errors"
]
