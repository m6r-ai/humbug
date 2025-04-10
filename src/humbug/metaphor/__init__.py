"""An embedded compiler for the Metaphor language."""

# Export main classes so users can import directly from metaphor
from .metaphor_ast_node import (
    MetaphorASTNode, MetaphorRootNode, MetaphorTextNode,
    MetaphorCodeNode, MetaphorRoleNode, MetaphorContextNode, MetaphorActionNode,
    MetaphorASTVisitor
)
from .metaphor_parser import MetaphorParser, MetaphorParserError, MetaphorParserSyntaxError
from .metaphor_format_visitor import MetaphorFormatVisitor
from .metaphor_formatters import format_ast, format_errors


__all__ = [
    "MetaphorASTNode",
    "MetaphorRootNode",
    "MetaphorTextNode",
    "MetaphorCodeNode",
    "MetaphorRoleNode",
    "MetaphorContextNode",
    "MetaphorActionNode",
    "MetaphorASTVisitor",
    "MetaphorFormatVisitor",
    "MetaphorParser",
    "MetaphorParserError",
    "MetaphorParserSyntaxError",
    "format_ast",
    "format_errors"
]
