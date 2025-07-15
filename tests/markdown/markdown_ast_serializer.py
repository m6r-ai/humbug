"""
Visitor class for serializing and comparing markdown AST structures
"""
from typing import Dict, Any
import json

from markdown.markdown_ast_node import (
    MarkdownASTVisitor, MarkdownASTNode, MarkdownASTDocumentNode,
    MarkdownASTTextNode, MarkdownASTLineBreakNode, MarkdownASTEmphasisNode,
    MarkdownASTBoldNode, MarkdownASTHeadingNode, MarkdownASTParagraphNode,
    MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode, MarkdownASTListItemNode,
    MarkdownASTInlineCodeNode, MarkdownASTCodeBlockNode, MarkdownASTTableNode,
    MarkdownASTTableHeaderNode, MarkdownASTTableBodyNode, MarkdownASTTableRowNode,
    MarkdownASTTableCellNode, MarkdownASTHorizontalRuleNode, MarkdownASTImageNode,
    MarkdownASTLinkNode
)


class MarkdownASTSerializer(MarkdownASTVisitor):
    """Visitor that serializes the AST structure for comparison."""

    def __init__(self, include_line_numbers: bool = False) -> None:
        """
        Initialize the AST serializer.
        
        Args:
            include_line_numbers: Whether to include line numbers in the serialized output
        """
        super().__init__()
        self.include_line_numbers = include_line_numbers

    def _add_line_info(self, result: Dict[str, Any], node: MarkdownASTNode) -> None:
        """Add line information to the result if requested."""
        if not self.include_line_numbers:
            return

        if node.line_start is not None:
            result["line_start"] = node.line_start

        if node.line_end is not None:
            result["line_end"] = node.line_end

    def visit_MarkdownASTDocumentNode(self, node: MarkdownASTDocumentNode) -> Dict[str, Any]:
        """Serialize a document node."""
        result = {
            "type": "document",
            "children": []
        }

        if node.source_path:
            result["source_path"] = node.source_path

        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTTextNode(self, node: MarkdownASTTextNode) -> Dict[str, Any]:
        """Serialize a text node."""
        result = {
            "type": "text",
            "content": node.content
        }
        self._add_line_info(result, node)
        return result

    def visit_MarkdownASTLineBreakNode(self, node: MarkdownASTLineBreakNode) -> Dict[str, Any]:
        """Serialize a line break node."""
        result = {"type": "linebreak"}
        self._add_line_info(result, node)
        return result

    def visit_MarkdownASTEmphasisNode(self, node: MarkdownASTEmphasisNode) -> Dict[str, Any]:
        """Serialize an emphasis node."""
        result = {
            "type": "emphasis",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTBoldNode(self, node: MarkdownASTBoldNode) -> Dict[str, Any]:
        """Serialize a bold node."""
        result = {
            "type": "bold",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTHeadingNode(self, node: MarkdownASTHeadingNode) -> Dict[str, Any]:
        """Serialize a heading node."""
        result = {
            "type": "heading",
            "level": node.level,
            "id": node.anchor_id,
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTParagraphNode(self, node: MarkdownASTParagraphNode) -> Dict[str, Any]:
        """Serialize a paragraph node."""
        result = {
            "type": "paragraph",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTOrderedListNode(self, node: MarkdownASTOrderedListNode) -> Dict[str, Any]:
        """Serialize an ordered list node."""
        result = {
            "type": "ordered_list",
            "indent": node.indent,
            "start": node.start,
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTUnorderedListNode(self, node: MarkdownASTUnorderedListNode) -> Dict[str, Any]:
        """Serialize an unordered list node."""
        result = {
            "type": "unordered_list",
            "indent": node.indent,
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTListItemNode(self, node: MarkdownASTListItemNode) -> Dict[str, Any]:
        """Serialize a list item node."""
        result = {
            "type": "list_item",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTInlineCodeNode(self, node: MarkdownASTInlineCodeNode) -> Dict[str, Any]:
        """Serialize an inline code node."""
        result = {
            "type": "inline_code",
            "content": node.content
        }
        self._add_line_info(result, node)
        return result

    def visit_MarkdownASTCodeBlockNode(self, node: MarkdownASTCodeBlockNode) -> Dict[str, Any]:
        """Serialize a code block node."""
        result = {
            "type": "code_block",
            "language": node.language,
            "content": node.content
        }
        self._add_line_info(result, node)
        return result

    def visit_MarkdownASTTableNode(self, node: MarkdownASTTableNode) -> Dict[str, Any]:
        """Serialize a table node."""
        result = {
            "type": "table",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTTableHeaderNode(self, node: MarkdownASTTableHeaderNode) -> Dict[str, Any]:
        """Serialize a table header node."""
        result = {
            "type": "table_header",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTTableBodyNode(self, node: MarkdownASTTableBodyNode) -> Dict[str, Any]:
        """Serialize a table body node."""
        result = {
            "type": "table_body",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTTableRowNode(self, node: MarkdownASTTableRowNode) -> Dict[str, Any]:
        """Serialize a table row node."""
        result = {
            "type": "table_row",
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTTableCellNode(self, node: MarkdownASTTableCellNode) -> Dict[str, Any]:
        """Serialize a table cell node."""
        result = {
            "type": "table_cell",
            "is_header": node.is_header,
            "alignment": node.alignment,
            "children": []
        }
        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTHorizontalRuleNode(self, node: MarkdownASTHorizontalRuleNode) -> Dict[str, Any]:
        """Serialize a horizontal rule node."""
        result = {"type": "horizontal_rule"}
        self._add_line_info(result, node)
        return result

    def visit_MarkdownASTLinkNode(self, node: MarkdownASTLinkNode) -> Dict[str, Any]:
        """Serialize a link node."""
        result = {
            "type": "link",
            "url": node.url,
            "children": []
        }

        if node.title:
            result["title"] = node.title

        self._add_line_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MarkdownASTImageNode(self, node: MarkdownASTImageNode) -> Dict[str, Any]:
        """Serialize an image node."""
        result = {
            "type": "image",
            "url": node.url,
            "alt_text": node.alt_text
        }

        if node.title:
            result["title"] = node.title

        self._add_line_info(result, node)
        return result


def serialize_ast(node: MarkdownASTNode, include_line_numbers: bool = False) -> Dict[str, Any]:
    """
    Serialize an AST node to a dictionary.

    Args:
        node: The AST node to serialize
        include_line_numbers: Whether to include line numbers in the output

    Returns:
        A dictionary representation of the AST
    """
    serializer = MarkdownASTSerializer(include_line_numbers)
    return serializer.visit(node)


def save_ast_to_json(node: MarkdownASTNode, file_path: str, include_line_numbers: bool = False) -> None:
    """
    Save an AST node to a JSON file.

    Args:
        node: The AST node to serialize
        file_path: The path to save the JSON file to
        include_line_numbers: Whether to include line numbers in the output
    """
    serialized = serialize_ast(node, include_line_numbers)
    with open(file_path, 'w', encoding='utf-8') as f:
        json.dump(serialized, f, indent=2)


def load_ast_from_json(file_path: str) -> Dict[str, Any]:
    """
    Load an AST from a JSON file.

    Args:
        file_path: The path to the JSON file

    Returns:
        A dictionary representation of the AST
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return json.load(f)
