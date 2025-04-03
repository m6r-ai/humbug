"""
Base classes for Abstract Syntax Trees (AST).

This module provides shared infrastructure for different AST implementations,
allowing for code reuse while maintaining separation between languages.
"""

from typing import List, Optional, TypeVar, Generic, Any

T = TypeVar('T', bound='ASTNode')

class ASTNode(Generic[T]):
    """
    Base class for all AST nodes in any language.

    This class provides common tree operations like adding/removing children
    and navigating between siblings, which are useful across different
    language implementations.
    """

    def __init__(self) -> None:
        """Initialize a base AST node with empty children list and no parent."""
        self.parent: Optional[T] = None
        self.children: List[T] = []

    def add_child(self, child: T) -> T:
        """
        Add a child node to this node.

        Args:
            child: The child node to add

        Returns:
            The added child node for method chaining
        """
        child.parent = self  # type: ignore
        self.children.append(child)
        return child

    def remove_child(self, child: T) -> None:
        """
        Remove a child node from this node.

        Args:
            child: The child node to remove
            
        Raises:
            ValueError: If the child is not a child of this node
        """
        if not child in self.children:
            raise ValueError("Node is not a child of this node")

        self.children.remove(child)
        child.parent = None

    def remove_children(self) -> None:
        """Remove all children from this node."""
        for child in self.children:
            child.parent = None

        self.children = []

    def previous_sibling(self) -> Optional[T]:
        """
        Get the previous sibling of this node, if any.

        Returns:
            The previous sibling node, or None if this is the first child or has no parent
        """
        if self.parent is None:
            return None

        index = self.parent.children.index(self)  # type: ignore
        if index > 0:
            return self.parent.children[index - 1]  # type: ignore

        return None

    def next_sibling(self) -> Optional[T]:
        """
        Get the next sibling of this node, if any.

        Returns:
            The next sibling node, or None if this is the last child or has no parent
        """
        if self.parent is None:
            return None

        index = self.parent.children.index(self)  # type: ignore
        if index < len(self.parent.children) - 1:  # type: ignore
            return self.parent.children[index + 1]  # type: ignore

        return None


class ASTVisitor(Generic[T]):
    """
    Base visitor class for AST traversal.

    This implements the visitor pattern for tree traversal, allowing
    specialized processing of different node types.
    """

    def visit(self, node: T) -> Any:
        """
        Visit a node and dispatch to the appropriate visit method.

        Args:
            node: The node to visit

        Returns:
            The result of visiting the node
        """
        method_name = f'visit_{node.__class__.__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node: T) -> List[Any]:
        """
        Default visit method for nodes without specific handlers.

        Args:
            node: The node to visit

        Returns:
            A list of results from visiting each child
        """
        results = []
        for child in node.children:
            results.append(self.visit(child))

        return results
