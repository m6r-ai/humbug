"""
Visitor class to print Metaphor AST structures for debugging
"""

from humbug.m6rc.metaphor_ast_node import (
    MetaphorASTVisitor, MetaphorASTNode, MetaphorTextNode, MetaphorCodeNode,
    MetaphorRoleNode, MetaphorContextNode, MetaphorActionNode, MetaphorRootNode
)


class MetaphorASTPrinter(MetaphorASTVisitor):
    """Visitor that prints the Metaphor AST structure for debugging."""

    def __init__(self):
        """Initialize the AST printer with zero indentation."""
        super().__init__()
        self.indent_level = 0

    def _indent(self):
        """
        Get the current indentation string.

        Returns:
            A string of spaces for the current indentation level
        """
        return "  " * self.indent_level

    def generic_visit(self, node: MetaphorASTNode):
        """
        Default visit method that prints the node type.

        Args:
            node: The node to visit

        Returns:
            The results of visiting the children
        """
        # Print node information
        node_class = node.__class__.__name__
        print(f"{self._indent()}{node_class}: {node.value}")

        # Visit children with increased indentation
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1

        return results

    def visit_MetaphorTextNode(self, node: MetaphorTextNode):
        """
        Visit a text node and print its content.

        Args:
            node: The text node to visit

        Returns:
            The text content
        """
        print(f"{self._indent()}Text: '{node.value}'")
        return node.value

    def visit_MetaphorCodeNode(self, node: MetaphorCodeNode):
        """
        Visit a code node and print a preview of its content.

        Args:
            node: The code node to visit

        Returns:
            The code content
        """
        content_preview = node.value[:30] + "..." if len(node.value) > 30 else node.value
        print(f"{self._indent()}Code: '{content_preview}' ({len(node.value)} chars)")
        return node.value

    def visit_with_specific_types(self, node: MetaphorASTNode, types_to_show=None):
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
            print(f"{self._indent()}{node_class}: {node.value}")

        # Visit children with increased indentation
        self.indent_level += 1
        results = []
        for child in node.children:
            results.append(self.visit_with_specific_types(child, types_to_show))

        self.indent_level -= 1

        return results
