"""An embedded compiler for the Metaphor language."""

# Export main classes so users can import directly from metaphor
from .metaphor_ast_node import (
    MetaphorASTNode, MetaphorRootNode, MetaphorTextNode,
    MetaphorCodeNode, MetaphorRoleNode, MetaphorContextNode, MetaphorActionNode,
    MetaphorASTVisitor
)
from .metaphor_ast_builder import MetaphorASTBuilder, MetaphorASTBuilderError, MetaphorASTBuilderSyntaxError
from .metaphor_format_visitor import MetaphorFormatVisitor
from .metaphor_formatters import format_errors, format_preamble


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
    "MetaphorASTBuilder",
    "MetaphorASTBuilderError",
    "MetaphorASTBuilderSyntaxError",
    "format_errors",
    "format_preamble"
]
