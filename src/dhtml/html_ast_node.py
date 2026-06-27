from typing import Any


class HtmlASTNode:
    """Base class for all HTML AST nodes."""

    def __init__(self) -> None:
        """Initialise a node with no parent and no children."""
        self.parent: "HtmlASTNode | None" = None
        self.children: list["HtmlASTNode"] = []

    def add_child(self, child: "HtmlASTNode") -> "HtmlASTNode":
        """
        Add a child node to this node.

        Args:
            child: The child node to add.

        Returns:
            The added child node for method chaining.
        """
        child.parent = self
        self.children.append(child)
        return child

    def remove_child(self, child: "HtmlASTNode") -> None:
        """
        Remove a child node from this node.

        Args:
            child: The child node to remove.

        Raises:
            ValueError: If the child is not a child of this node.
        """
        if child not in self.children:
            raise ValueError("Node is not a child of this node")

        self.children.remove(child)
        child.parent = None

    def remove_children(self) -> None:
        """Remove all children from this node."""
        for child in self.children:
            child.parent = None

        self.children = []


class HtmlASTVisitor:
    """Base visitor class for HTML AST traversal."""

    def visit(self, node: HtmlASTNode) -> Any:
        """
        Visit a node and dispatch to the appropriate visit method.

        Args:
            node: The node to visit.

        Returns:
            The result of visiting the node.
        """
        method_name = f"visit_{node.__class__.__name__}"
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node: HtmlASTNode) -> list[Any]:
        """
        Default visit method for nodes without specific handlers.

        Args:
            node: The node to visit.

        Returns:
            A list of results from visiting each child.
        """
        results = []
        for child in node.children:
            results.append(self.visit(child))

        return results


class HtmlASTDocumentNode(HtmlASTNode):
    """Root node representing a complete HTML document."""

    def __init__(self, source_path: str | None = None) -> None:
        """
        Initialise a document node.

        Args:
            source_path: Optional path to the source file.
        """
        super().__init__()
        self.source_path = source_path
        self.has_doctype: bool = False


class HtmlASTElementNode(HtmlASTNode):
    """
    Node representing an HTML element with a tag name and attributes.

    This single class covers all element types (div, p, h1, table, etc.).
    The tag_name is always lower-cased. Children may be HtmlASTElementNode
    or HtmlASTTextNode instances.
    """

    def __init__(self, tag_name: str, attributes: dict[str, str] | None = None) -> None:
        """
        Initialise an element node.

        Args:
            tag_name: The lower-cased HTML tag name (e.g. 'div', 'p', 'a').
            attributes: Optional mapping of attribute name to value.
        """
        super().__init__()
        self.tag_name = tag_name
        self.attributes: dict[str, str] = attributes if attributes is not None else {}


class HtmlASTTextNode(HtmlASTNode):
    """Node representing a text run within an element."""

    def __init__(self, content: str) -> None:
        """
        Initialise a text node.

        Args:
            content: The decoded text content (HTML entities already resolved).
        """
        super().__init__()
        self.content = content


class HtmlASTCommentNode(HtmlASTNode):
    """Node representing an HTML comment."""

    def __init__(self, content: str) -> None:
        """
        Initialise a comment node.

        Args:
            content: The raw comment text (between <!-- and -->).
        """
        super().__init__()
        self.content = content
