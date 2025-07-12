"""
Visitor class to print Metaphor AST structures for debugging
"""

from typing import List, Any, Type, cast

from humbug.lib.ast.ast import ASTNode
from humbug.metaphor.metaphor_ast_node import (
    MetaphorASTVisitor, MetaphorASTNode, MetaphorTextNode, MetaphorCodeNode
)


class MetaphorASTPrinter(MetaphorASTVisitor):
    """Visitor that prints the Metaphor AST structure for debugging."""

    def __init__(self) -> None:
        """Initialize the AST printer with zero indentation."""
        super().__init__()
        self.indent_level = 0

    def _indent(self) -> str:
        """
        Get the current indentation string.

        Returns:
            A string of spaces for the current indentation level
        """
        return "  " * self.indent_level

    def generic_visit(self, node: ASTNode) -> List[Any]:
        """
        Default visit method that prints the node type.

        Args:
            node: The node to visit

        Returns:
            The results of visiting the children
        """
        # Print node information
        node_class = node.__class__.__name__
        ast_node = cast(MetaphorASTNode, node)
        print(f"{self._indent()}{node_class}: {ast_node.value()}")

        # Visit children with increased indentation
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1

        return results

    def visit_MetaphorTextNode(self, node: MetaphorTextNode) -> str:  # pylint: disable=invalid-name
        """
        Visit a text node and print its content.

        Args:
            node: The text node to visit

        Returns:
            The text content
        """
        value = node.value()
        print(f"{self._indent()}Text: '{value}'")
        return value

    def visit_MetaphorCodeNode(self, node: MetaphorCodeNode) -> str:  # pylint: disable=invalid-name
        """
        Visit a code node and print a preview of its content.

        Args:
            node: The code node to visit

        Returns:
            The code content
        """
        value = node.value()
        content_preview = value[:30] + "..." if len(value) > 30 else value
        print(f"{self._indent()}Code: '{content_preview}' ({len(value)} chars)")
        return value

    def visit_with_specific_types(
        self,
        node: MetaphorASTNode,
        types_to_show: List[Type[MetaphorASTNode]] | None = None
    ) -> List[Any]:
        """
        Visit a node and only print details for specific node classes.

        Args:
            node: The node to visit
            types_to_show: List of node classes to display, or None for all

        Returns:
            The results of visiting the node
        """
        # Check if we should display this node
        if types_to_show is None or any(isinstance(node, t) for t in types_to_show):
            node_class = node.__class__.__name__
            print(f"{self._indent()}{node_class}: {node.value()}")

        # Visit children with increased indentation
        self.indent_level += 1
        results = []
        for child in node.children:
            results.append(self.visit_with_specific_types(cast(MetaphorASTNode, child), types_to_show))

        self.indent_level -= 1

        return results
