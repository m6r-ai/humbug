"""
Format visitor implementation for Metaphor AST.
"""

import io
from typing import Any, Dict, Final, TextIO, Type

from humbug.metaphor.metaphor_ast_node import (
    MetaphorASTNode, MetaphorASTVisitor, MetaphorActionNode, MetaphorCodeNode,
    MetaphorContextNode, MetaphorRoleNode, MetaphorRootNode, MetaphorTextNode
)


class MetaphorFormatVisitor(MetaphorASTVisitor):
    """
    Visitor that formats a Metaphor AST structure into a string representation.

    This visitor implements the visitor pattern to traverse the AST and
    generate formatted output according to the Metaphor language syntax.
    """

    # Map node types to their keyword representations
    NODE_CLASS_MAP: Final[Dict[Type[MetaphorASTNode], str]] = {
        MetaphorActionNode: "Action:",
        MetaphorContextNode: "Context:",
        MetaphorRoleNode: "Role:"
    }

    def __init__(self) -> None:
        """Initialize the format visitor with an output buffer."""
        super().__init__()
        self.output: TextIO = io.StringIO()
        self.depth: int = 0

    def format(self, node: MetaphorASTNode) -> str:
        """
        Format an AST node and its children as a string.

        Args:
            node: The root node to format

        Returns:
            Formatted string representation of the AST
        """
        self.depth = 0
        self.output = io.StringIO()
        self.visit(node)
        return self.output.getvalue()

    def generic_visit(self, node: MetaphorASTNode) -> Any:
        """
        Default visit method for node types without specific handlers.

        Args:
            node: The node to visit

        Returns:
            Implementation-specific results from visiting the node
        """
        results = []
        for child in node.children:
            results.append(self.visit(child))

        return results


    def visit_MetaphorRootNode(self, node: MetaphorRootNode) -> Any:  # pylint: disable=invalid-name
        """
        Visit the root node of the AST.

        Args:
            node: The root node to visit

        Returns:
            Implementation-specific results from visiting children
        """
        self.depth = 1
        return self.generic_visit(node)

    def visit_MetaphorTextNode(self, node: MetaphorTextNode) -> Any:  # pylint: disable=invalid-name
        """
        Visit a text node and format its content.

        Args:
            node: The text node to visit

        Returns:
            The text content
        """
        value = node.value()
        if value == "":
            current_pos = self.output.tell()
            if current_pos <= 1:
                return value

            self.output.seek(current_pos - 2)
            prev_char = self.output.read(2)
            self.output.seek(current_pos)
            if prev_char == '\n\n':
                return value

        self.output.write(f"{value}\n")
        return value

    def visit_MetaphorCodeNode(self, node: MetaphorCodeNode) -> Any:  # pylint: disable=invalid-name
        """
        Visit a code node and format its content.

        Args:
            node: The code node to visit

        Returns:
            The code content
        """
        value = node.value()
        self.output.write(f"{value}\n")
        return value

    def _format_block_node(self, node: MetaphorASTNode) -> Any:
        """
        Format a block node (Role, Context, or Action).

        Args:
            node: The block node to format

        Returns:
            Implementation-specific results from visiting the node
        """
        # If we don't have a blank line before this block heading then add one
        current_pos = self.output.tell()
        if current_pos > 1:
            self.output.seek(current_pos - 2)
            prev_char = self.output.read(2)
            self.output.seek(current_pos)
            if prev_char != '\n\n':
                self.output.write("\n")

        # Write the heading with the appropriate number of hash symbols
        indent = "#" * self.depth
        keyword = self.NODE_CLASS_MAP.get(node.__class__, "")
        self.output.write(f"{indent} {keyword}")

        # Add the optional label if present
        value = node.value()
        if value:
            self.output.write(f" {value}")

        self.output.write("\n\n")

        # Process children with increased depth
        self.depth += 1
        results = self.generic_visit(node)
        self.depth -= 1

        return results

    def visit_MetaphorRoleNode(self, node: MetaphorRoleNode) -> Any:  # pylint: disable=invalid-name
        """
        Visit a role node and format its content.

        Args:
            node: The role node to visit

        Returns:
            Implementation-specific results from visiting the node
        """
        return self._format_block_node(node)

    def visit_MetaphorContextNode(self, node: MetaphorContextNode) -> Any:  # pylint: disable=invalid-name
        """
        Visit a context node and format its content.

        Args:
            node: The context node to visit

        Returns:
            Implementation-specific results from visiting the node
        """
        return self._format_block_node(node)

    def visit_MetaphorActionNode(self, node: MetaphorActionNode) -> Any:  # pylint: disable=invalid-name
        """
        Visit an action node and format its content.

        Args:
            node: The action node to visit

        Returns:
            Implementation-specific results from visiting the node
        """
        return self._format_block_node(node)
