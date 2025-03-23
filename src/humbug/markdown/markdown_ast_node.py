"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

from typing import Optional


class MarkdownASTNode:
    """Base class for all AST nodes."""
    def __init__(self):
        """Initialize an AST node with an empty children list."""
        self.children = []
        self.parent = None  # Reference to parent node

        # Source range information to support incremental updating
        self.line_start: Optional[int] = None
        self.line_end: Optional[int] = None

    def add_child(self, child):
        """
        Add a child node to this node.

        Args:
            child: The child node to add

        Returns:
            The added child node for method chaining
        """
        child.parent = self
        self.children.append(child)
        return child

    def accept(self, visitor):
        """
        Accept a visitor to process this node.

        Args:
            visitor: The visitor to accept

        Returns:
            The result of the visitor's visit method
        """
        return visitor.visit(self)

    def remove_children(self):
        """Remove all children from this node."""
        for child in self.children:
            child.parent = None

        self.children = []


class MarkdownASTVisitor:
    """Base visitor class for AST traversal."""
    def visit(self, node: MarkdownASTNode):
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

    def generic_visit(self, node: MarkdownASTNode):
        """
        Default visit method for nodes without specific handlers.

        Args:
            node: The node to visit

        Returns:
            A list of results from visiting each child
        """
        results = []
        for child in node.children:
            if child:
                results.append(self.visit(child))

        return results


class MarkdownDocumentNode(MarkdownASTNode):
    """Root node representing an entire HTML document."""
    def __init__(self):
        """Initialize a document node."""
        super().__init__()


class MarkdownParagraphNode(MarkdownASTNode):
    """Node representing an HTML paragraph (<p>)."""
    def __init__(self, children=None):
        """
        Initialize a paragraph node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class MarkdownHeadingNode(MarkdownASTNode):
    """Node representing an HTML heading (<h1> through <h6>)."""
    def __init__(self, level, children=None):
        """
        Initialize a heading node.

        Args:
            level: The heading level (1-6)
            children: Optional list of child nodes to add
        """
        super().__init__()
        # Level should be 1-6
        self.level = max(1, min(6, level))
        if children:
            for child in children:
                self.add_child(child)


class MarkdownOrderedListNode(MarkdownASTNode):
    """Node representing an HTML ordered list (<ol>)."""
    def __init__(self, indent=0, start=1, children=None):
        """
        Initialize an ordered list node.

        Args:
            indent: The indentation level of this list
            children: Optional list of child nodes to add
        """
        super().__init__()
        self.indent = indent
        self.start = start

        # Content indent is typically indent + marker (e.g., "1.") + space
        # For ordered lists, we use a default of 3 characters for the marker ("1. ")
        self.content_indent = indent + 3
        if children:
            for child in children:
                self.add_child(child)


class MarkdownUnorderedListNode(MarkdownASTNode):
    """Node representing an HTML unordered list (<ul>)."""
    def __init__(self, indent=0, children=None):
        """
        Initialize an unordered list node.

        Args:
            indent: The indentation level of this list
            children: Optional list of child nodes to add
        """
        super().__init__()
        self.indent = indent
        # Content indent is typically indent + marker (e.g., "-") + space
        # For unordered lists, we use 2 characters for the marker ("- ")
        self.content_indent = indent + 2
        if children:
            for child in children:
                self.add_child(child)


class MarkdownListItemNode(MarkdownASTNode):
    """Node representing an HTML list item (<li>)."""
    def __init__(self, children=None):
        """
        Initialize a list item node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class MarkdownTextNode(MarkdownASTNode):
    """Node representing plain text content."""
    def __init__(self, content):
        """
        Initialize a text node.

        Args:
            content: The text content
        """
        super().__init__()
        self.content = content


class MarkdownBoldNode(MarkdownASTNode):
    """Node representing bold text (<b> or <strong>)."""
    def __init__(self, children=None):
        """
        Initialize a bold node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class MarkdownEmphasisNode(MarkdownASTNode):
    """Node representing emphasized text (<em> or <i>)."""
    def __init__(self, children=None):
        """
        Initialize an emphasis node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class MarkdownInlineCodeNode(MarkdownASTNode):
    """Node representing inline code (<code>)."""
    def __init__(self, content=None):
        """
        Initialize an inline code node.

        Args:
            content: The code content
        """
        super().__init__()
        self.content = content or ""


class MarkdownCodeBlockNode(MarkdownASTNode):
    """Node representing a code block (<pre><code>)."""
    def __init__(self, language=None, content=None):
        """
        Initialize a code block node.

        Args:
            language: Optional language identifier for syntax highlighting
            content: The code content
        """
        super().__init__()
        self.language = language or ""
        self.content = content or ""


class MarkdownLineBreakNode(MarkdownASTNode):
    """Node representing a line break."""
    def __init__(self):
        """Initialize a line break node."""
        super().__init__()


class MarkdownParseError(Exception):
    """Exception raised for errors during markdown parsing."""
