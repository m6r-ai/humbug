"""
Types and classes for representing the AST (Abstract Syntax Tree)
of a Metaphor document.
"""

from typing import List
from enum import IntEnum

from humbug.ast.ast import ASTNode, ASTVisitor


class MetaphorASTNodeType(IntEnum):
    """
    Types of nodes that can appear in a Metaphor AST.
    """
    ROOT: int = 0
    TEXT: int = 1
    ROLE: int = 2
    CONTEXT: int = 3
    ACTION: int = 4
    CODE: int = 5


class MetaphorASTNode(ASTNode['MetaphorASTNode']):
    """
    Represents a node in the Metaphor Abstract Syntax Tree (AST).

    Attributes:
        node_type (MetaphorASTNodeType): The type of the token the node represents.
        value (str): The value associated with the node.
    """
    def __init__(self, node_type: MetaphorASTNodeType, value: str) -> None:
        super().__init__()
        self._node_type: MetaphorASTNodeType = node_type
        self._value: str = value

    def __str__(self, indent: int = 0) -> str:
        """
        Returns a string representation of the node and its children in a tree format.

        Args:
            indent (int): The current indentation level (used recursively)

        Returns:
            str: A formatted string showing the node's type, value, and children
        """
        # Create the indentation string
        indent_str = "    " * indent

        # Start with this node's information
        result = f"{indent_str}{self.node_type.name}: {self.value}"

        # Add all children with increased indentation
        for child in self.children:
            result += "\n" + child.__str__(indent + 1)

        return result

    def __repr__(self) -> str:
        """
        Returns a concise representation of the node for debugging.

        Returns:
            str: A string in format 'NodeType(value)[num_children]'
        """
        return f"{self.node_type.name}({self.value})[{len(self.children)}]"

    def attach_child(self, child: 'MetaphorASTNode') -> None:
        """Add a child node to this MetaphorASTNode."""
        self.add_child(child)

    def detach_child(self, child: 'MetaphorASTNode') -> None:
        """
        Detach a child node from this node in the AST.
        
        Args:
            child: The child node to detach
            
        Raises:
            ValueError: If the child is not a child of this node
        """
        self.remove_child(child)

    @property
    def node_type(self) -> MetaphorASTNodeType:
        """The type of this node."""
        return self._node_type

    @property
    def value(self) -> str:
        """The raw text value of this node."""
        return self._value

    def get_children_of_type(self, node_type: MetaphorASTNodeType) -> List['MetaphorASTNode']:
        """
        Returns a list of all immediate children that match the specified node type.

        Args:
            node_type (MetaphorASTNodeType): The type of nodes to filter for

        Returns:
            List[MetaphorASTNode]: List of child nodes matching the specified type
        """
        return [child for child in self.children if child.node_type == node_type]


class MetaphorASTVisitor(ASTVisitor['MetaphorASTNode']):
    """Base visitor class for Metaphor AST traversal."""
    pass
