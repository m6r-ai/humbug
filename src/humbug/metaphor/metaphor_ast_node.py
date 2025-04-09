"""
Types and classes for representing the AST (Abstract Syntax Tree)
of a Metaphor document.
"""

from typing import List, Any

from humbug.ast.ast import ASTNode, ASTVisitor


class MetaphorASTNode(ASTNode['MetaphorASTNode']):
    """
    Base class for all Metaphor AST nodes.

    This class provides common functionality for all Metaphor nodes
    regardless of their specific type.
    """

    def __init__(self, value: str = "") -> None:
        """
        Initialize a base Metaphor AST node.

        Args:
            value: The value associated with the node
        """
        super().__init__()
        self._value: str = value

    def __str__(self, indent: int = 0) -> str:
        """
        Returns a string representation of the node and its children in a tree format.

        Args:
            indent: The current indentation level (used recursively)

        Returns:
            A formatted string showing the node's type, value, and children
        """
        # Create the indentation string
        indent_str = "    " * indent

        # Start with this node's information
        node_type_name = self.__class__.__name__
        result = f"{indent_str}{node_type_name}: {self._value}"

        # Add all children with increased indentation
        for child in self.children:
            result += "\n" + child.__str__(indent + 1)

        return result

    def __repr__(self) -> str:
        """
        Returns a concise representation of the node for debugging.

        Returns:
            A string in format 'NodeType(value)[num_children]'
        """
        node_type_name = self.__class__.__name__
        return f"{node_type_name}({self._value})[{len(self.children)}]"

    def value(self) -> str:
        """The raw text value of this node."""
        return self._value

    def get_children_of_type(self, node_class: type) -> List['MetaphorASTNode']:
        """
        Returns a list of all immediate children that are instances of the specified class.

        Args:
            node_class: The class to filter for

        Returns:
            List of child nodes matching the specified class
        """
        return [child for child in self.children if isinstance(child, node_class)]

    def accept(self, visitor: 'MetaphorASTVisitor') -> Any:
        """
        Accept a visitor to process this node.

        Args:
            visitor: The visitor to accept

        Returns:
            The result of the visitor's visit method
        """
        return visitor.visit(self)


class MetaphorRootNode(MetaphorASTNode):
    """Root node of a Metaphor AST representing an entire document."""

    def __init__(self) -> None:
        """Initialize a root node."""
        super().__init__("")


class MetaphorTextNode(MetaphorASTNode):
    """Node representing text content in a Metaphor document."""

    def __init__(self, content: str) -> None:
        """
        Initialize a text node.

        Args:
            content: The text content
        """
        super().__init__(content)


class MetaphorCodeNode(MetaphorASTNode):
    """Node representing a code block in a Metaphor document."""

    def __init__(self, content: str) -> None:
        """
        Initialize a code block node.

        Args:
            content: The code content
        """
        super().__init__(content)


class MetaphorRoleNode(MetaphorASTNode):
    """Node representing a Role block in a Metaphor document."""

    def __init__(self, label: str = "") -> None:
        """
        Initialize a role node.

        Args:
            label: Optional label for the role
        """
        super().__init__(label)


class MetaphorContextNode(MetaphorASTNode):
    """Node representing a Context block in a Metaphor document."""

    def __init__(self, label: str = "") -> None:
        """
        Initialize a context node.

        Args:
            label: Optional label for the context
        """
        super().__init__(label)


class MetaphorActionNode(MetaphorASTNode):
    """Node representing an Action block in a Metaphor document."""

    def __init__(self, label: str = "") -> None:
        """
        Initialize an action node.

        Args:
            label: Optional label for the action
        """
        super().__init__(label)


class MetaphorASTVisitor(ASTVisitor['MetaphorASTNode']):
    """Base visitor class for Metaphor AST traversal."""

    def visit(self, node: MetaphorASTNode) -> Any:
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

    def generic_visit(self, node: MetaphorASTNode) -> List[Any]:
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
