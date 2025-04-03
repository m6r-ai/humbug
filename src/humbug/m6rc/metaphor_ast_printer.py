"""
Visitor class to print Metaphor AST structures for debugging
"""

from humbug.m6rc.metaphor_ast_node import MetaphorASTVisitor, MetaphorASTNode, MetaphorASTNodeType


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

    def visit(self, node: MetaphorASTNode):
        """
        Visit a node and print its details.

        Args:
            node: The node to visit

        Returns:
            The results of visiting the node
        """
        # Print node information
        print(f"{self._indent()}{node.node_type.name}: {node.value}")

        # Visit children with increased indentation
        self.indent_level += 1
        results = []
        for child in node.children:
            results.append(self.visit(child))

        self.indent_level -= 1

        return results

    def visit_with_node_types(self, node: MetaphorASTNode, types_to_show=None):
        """
        Visit a node and only print details for specific node types.

        Args:
            node: The node to visit
            types_to_show: List of node types to display, or None for all

        Returns:
            The results of visiting the node
        """
        # Check if we should display this node
        if types_to_show is None or node.node_type in types_to_show:
            print(f"{self._indent()}{node.node_type.name}: {node.value}")

        # Visit children with increased indentation
        self.indent_level += 1
        results = []
        for child in node.children:
            results.append(self.visit_with_node_types(child, types_to_show))

        self.indent_level -= 1

        return results
