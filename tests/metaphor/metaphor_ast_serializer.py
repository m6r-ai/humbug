"""
Visitor class for serializing and comparing Metaphor AST structures.
"""

import json
from typing import Dict, Any, List

from metaphor import (
    MetaphorASTVisitor, MetaphorASTNode, MetaphorASTRootNode,
    MetaphorASTTextNode, MetaphorASTCodeNode, MetaphorASTRoleNode,
    MetaphorASTContextNode, MetaphorASTActionNode
)


class MetaphorASTSerializer(MetaphorASTVisitor):
    """Visitor that serializes the Metaphor AST structure for comparison."""

    def __init__(self, include_source_info: bool = False) -> None:
        """
        Initialize the AST serializer.

        Args:
            include_source_info: Whether to include source file information in the serialized output
        """
        super().__init__()
        self.include_source_info = include_source_info

    def _add_source_info(self, result: Dict[str, Any], node: MetaphorASTNode) -> None:
        """Add source information to the result if requested."""
        if not self.include_source_info:
            return

        # Add any source-related information that might be available
        # This could be extended based on what metadata the Metaphor nodes contain
        pass

    def visit_MetaphorASTRootNode(self, node: MetaphorASTRootNode) -> Dict[str, Any]:
        """Serialize a root node."""
        result = {
            "type": "root",
            "children": []
        }

        self._add_source_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MetaphorASTTextNode(self, node: MetaphorASTTextNode) -> Dict[str, Any]:
        """Serialize a text node."""
        result = {
            "type": "text",
            "content": node.value()
        }
        self._add_source_info(result, node)
        return result

    def visit_MetaphorASTCodeNode(self, node: MetaphorASTCodeNode) -> Dict[str, Any]:
        """Serialize a code node."""
        result = {
            "type": "code",
            "content": node.value()
        }
        self._add_source_info(result, node)
        return result

    def visit_MetaphorASTRoleNode(self, node: MetaphorASTRoleNode) -> Dict[str, Any]:
        """Serialize a role node."""
        result = {
            "type": "role",
            "label": node.value(),
            "children": []
        }
        self._add_source_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MetaphorASTContextNode(self, node: MetaphorASTContextNode) -> Dict[str, Any]:
        """Serialize a context node."""
        result = {
            "type": "context",
            "label": node.value(),
            "children": []
        }
        self._add_source_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result

    def visit_MetaphorASTActionNode(self, node: MetaphorASTActionNode) -> Dict[str, Any]:
        """Serialize an action node."""
        result = {
            "type": "action",
            "label": node.value(),
            "children": []
        }
        self._add_source_info(result, node)

        # Visit children
        children = super().generic_visit(node)
        result["children"] = children

        return result


def serialize_ast(node: MetaphorASTNode, include_source_info: bool = False) -> Dict[str, Any]:
    """
    Serialize a Metaphor AST node to a dictionary.

    Args:
        node: The AST node to serialize
        include_source_info: Whether to include source information in the output

    Returns:
        A dictionary representation of the AST
    """
    serializer = MetaphorASTSerializer(include_source_info)
    return serializer.visit(node)


def save_ast_to_json(node: MetaphorASTNode, file_path: str, include_source_info: bool = False) -> None:
    """
    Save a Metaphor AST node to a JSON file.

    Args:
        node: The AST node to serialize
        file_path: The path to save the JSON file to
        include_source_info: Whether to include source information in the output
    """
    serialized = serialize_ast(node, include_source_info)
    with open(file_path, 'w', encoding='utf-8') as f:
        json.dump(serialized, f, indent=2)


def load_ast_from_json(file_path: str) -> Dict[str, Any]:
    """
    Load a Metaphor AST from a JSON file.

    Args:
        file_path: The path to the JSON file

    Returns:
        A dictionary representation of the AST
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return json.load(f)
