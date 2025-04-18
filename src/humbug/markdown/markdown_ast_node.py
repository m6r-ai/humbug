"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

from typing import Any

from humbug.ast.ast import ASTNode, ASTVisitor


class MarkdownASTNode(ASTNode['MarkdownASTNode']):
    """Base class for all Markdown AST nodes."""

    def __init__(self) -> None:
        """Initialize an AST node with common markdown properties."""
        super().__init__()

        # Source range information to support incremental updating
        self.line_start: int | None = None
        self.line_end: int | None = None

    def accept(self, visitor: 'MarkdownASTVisitor') -> Any:
        """
        Accept a visitor to process this node.

        Args:
            visitor: The visitor to accept

        Returns:
            The result of the visitor's visit method
        """
        return visitor.visit(self)


class MarkdownASTVisitor(ASTVisitor['MarkdownASTNode']):
    """Base visitor class for Markdown AST traversal."""


class MarkdownDocumentNode(MarkdownASTNode):
    """Root node representing an entire HTML document."""


class MarkdownParagraphNode(MarkdownASTNode):
    """Node representing an HTML paragraph (<p>)."""


class MarkdownHeadingNode(MarkdownASTNode):
    """Node representing an HTML heading (<h1> through <h6>)."""
    def __init__(self, level: int) -> None:
        """
        Initialize a heading node.

        Args:
            level: The heading level (1-6)
        """
        super().__init__()

        # Level should be 1-6
        self.level = max(1, min(6, level))


class MarkdownOrderedListNode(MarkdownASTNode):
    """Node representing an HTML ordered list (<ol>)."""
    def __init__(self, indent: int = 0, start: int = 1) -> None:
        """
        Initialize an ordered list node.

        Args:
            indent: The indentation level of this list
        """
        super().__init__()
        self.indent = indent
        self.start = start

        # Content indent is typically indent + marker (e.g., "1.") + space
        # For ordered lists, we use a default of 3 characters for the marker ("1. ")
        self.content_indent = indent + 3


class MarkdownUnorderedListNode(MarkdownASTNode):
    """Node representing an HTML unordered list (<ul>)."""
    def __init__(self, indent: int = 0) -> None:
        """
        Initialize an unordered list node.

        Args:
            indent: The indentation level of this list
        """
        super().__init__()
        self.indent = indent

        # Content indent is typically indent + marker (e.g., "-") + space
        # For unordered lists, we use 2 characters for the marker ("- ")
        self.content_indent = indent + 2


class MarkdownListItemNode(MarkdownASTNode):
    """Node representing an HTML list item (<li>)."""


class MarkdownTextNode(MarkdownASTNode):
    """Node representing plain text content."""
    def __init__(self, content: str) -> None:
        """
        Initialize a text node.

        Args:
            content: The text content
        """
        super().__init__()
        self.content = content


class MarkdownBoldNode(MarkdownASTNode):
    """Node representing bold text (<b> or <strong>)."""


class MarkdownEmphasisNode(MarkdownASTNode):
    """Node representing emphasized text (<em> or <i>)."""


class MarkdownInlineCodeNode(MarkdownASTNode):
    """Node representing inline code (<code>)."""
    def __init__(self, content: str = "") -> None:
        """
        Initialize an inline code node.

        Args:
            content: The code content
        """
        super().__init__()
        self.content = content


class MarkdownCodeBlockNode(MarkdownASTNode):
    """Node representing a code block (<pre><code>)."""
    def __init__(self, language: str = "", content: str = "") -> None:
        """
        Initialize a code block node.

        Args:
            language: Optional language identifier for syntax highlighting
            content: The code content
        """
        super().__init__()
        self.language = language
        self.content = content


class MarkdownLineBreakNode(MarkdownASTNode):
    """Node representing a line break."""
