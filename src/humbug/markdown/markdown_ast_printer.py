"""
Visitor class to print markdown AST structures for debugging
"""
from typing import List, Any

from humbug.markdown.markdown_ast_node import (
    MarkdownASTVisitor, MarkdownTextNode, MarkdownHeadingNode, MarkdownInlineCodeNode,
    MarkdownCodeBlockNode, MarkdownASTNode, MarkdownTableNode, MarkdownTableHeaderNode,
    MarkdownTableBodyNode, MarkdownTableRowNode, MarkdownTableCellNode, MarkdownHorizontalRuleNode
)


class MarkdownASTPrinter(MarkdownASTVisitor):
    """Visitor that prints the AST structure for debugging."""
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

    def generic_visit(self, node: MarkdownASTNode) -> List[Any]:
        """
        Default visit method that prints the node type.

        Args:
            node: The node to visit

        Returns:
            The results of visiting the children
        """
        node_name = node.__class__.__name__
        line_range = ""
        if node.line_start is not None and node.line_end is not None:
            line_range = f" (lines {node.line_start}-{node.line_end})"

        print(f"{self._indent()}{node_name}{line_range}")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_MarkdownTextNode(self, node: MarkdownTextNode) -> str:  # pylint: disable=invalid-name
        """
        Visit a text node and print its content.

        Args:
            node: The text node to visit

        Returns:
            The text content
        """
        line_range = ""
        if node.line_start is not None:
            line_range = f" (line {node.line_start})"

        print(f"{self._indent()}Text{line_range}: '{node.content}'")
        return node.content

    def visit_MarkdownHeadingNode(self, node: MarkdownHeadingNode) -> List[Any]:  # pylint: disable=invalid-name
        """
        Visit a heading node and print its level.

        Args:
            node: The heading node to visit

        Returns:
            The results of visiting the children
        """
        line_range = ""
        if node.line_start is not None:
            line_range = f" (line {node.line_start})"

        print(f"{self._indent()}Heading (level {node.level}){line_range}")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_MarkdownInlineCodeNode(self, node: MarkdownInlineCodeNode) -> str:  # pylint: disable=invalid-name
        """
        Visit an inline code node and print its content.

        Args:
            node: The inline code node to visit

        Returns:
            The inline code content
        """
        line_range = ""
        if node.line_start is not None:
            line_range = f" (line {node.line_start})"

        print(f"{self._indent()}InlineCode{line_range}: '{node.content}'")
        return node.content

    def visit_MarkdownCodeBlockNode(self, node: MarkdownCodeBlockNode) -> str:  # pylint: disable=invalid-name
        """
        Visit a code block node and print its language and content.

        Args:
            node: The code block node to visit

        Returns:
            The code block content
        """
        line_range = ""
        if node.line_start is not None and node.line_end is not None:
            line_range = f" (lines {node.line_start}-{node.line_end})"

        print(f"{self._indent()}CodeBlock{line_range}: language='{node.language}'")
        self.indent_level += 1
        print(f"{self._indent()}Content: '{node.content[:30]}...' ({len(node.content)} chars)")
        self.indent_level -= 1
        return node.content

    def visit_MarkdownTableNode(self, node: MarkdownTableNode) -> List[Any]:  # pylint: disable=invalid-name
        """
        Visit a table node and print its structure.

        Args:
            node: The table node to visit

        Returns:
            The results of visiting the children
        """
        line_range = ""
        if node.line_start is not None and node.line_end is not None:
            line_range = f" (lines {node.line_start}-{node.line_end})"

        print(f"{self._indent()}Table{line_range}")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_MarkdownTableHeaderNode(self, node: MarkdownTableHeaderNode) -> List[Any]:  # pylint: disable=invalid-name
        """
        Visit a table header node and print its info.

        Args:
            node: The table header node to visit

        Returns:
            The results of visiting the children
        """
        print(f"{self._indent()}TableHeader")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_MarkdownTableBodyNode(self, node: MarkdownTableBodyNode) -> List[Any]:  # pylint: disable=invalid-name
        """
        Visit a table body node and print its info.

        Args:
            node: The table body node to visit

        Returns:
            The results of visiting the children
        """
        print(f"{self._indent()}TableBody")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_MarkdownTableRowNode(self, node: MarkdownTableRowNode) -> List[Any]:  # pylint: disable=invalid-name
        """
        Visit a table row node and print its info.

        Args:
            node: The table row node to visit

        Returns:
            The results of visiting the children
        """
        line_range = ""
        if node.line_start is not None and node.line_end is not None:
            line_range = f" (lines {node.line_start}-{node.line_end})"

        print(f"{self._indent()}TableRow{line_range}")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_MarkdownTableCellNode(self, node: MarkdownTableCellNode) -> List[Any]:  # pylint: disable=invalid-name
        """
        Visit a table cell node and print its properties.

        Args:
            node: The table cell node to visit

        Returns:
            The results of visiting the children
        """
        cell_type = "HeaderCell" if node.is_header else "Cell"
        print(f"{self._indent()}{cell_type} (alignment: {node.alignment})")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_MarkdownHorizontalRuleNode(self, node: MarkdownHorizontalRuleNode) -> None:  # pylint: disable=invalid-name
        """
        Visit a horizontal rule node and print its details.

        Args:
            node: The horizontal rule node to visit

        Returns:
            None
        """
        line_range = ""
        if node.line_start is not None:
            line_range = f" (line {node.line_start})"

        print(f"{self._indent()}HorizontalRule{line_range}")
