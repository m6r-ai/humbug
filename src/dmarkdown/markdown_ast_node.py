"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

from typing import List

from dast import ASTNode, ASTVisitor
from syntax import ParserState, ProgrammingLanguage, Token


class MarkdownASTNode(ASTNode):
    """Base class for all Markdown AST nodes."""

    def __init__(self) -> None:
        """Initialize an AST node with common markdown properties."""
        super().__init__()

        # Source range information to support incremental updating
        self.line_start: int | None = None
        self.line_end: int | None = None


class MarkdownASTVisitor(ASTVisitor):
    """Base visitor class for Markdown AST traversal."""


class MarkdownASTDocumentNode(MarkdownASTNode):
    """Root node representing an entire HTML document."""

    def __init__(self, source_path: str | None = None) -> None:
        """
        Initialize a document node.

        Args:
            source_path: Optional path to the source file for resolving relative references
        """
        super().__init__()
        self.source_path = source_path


class MarkdownASTParagraphNode(MarkdownASTNode):
    """Node representing an HTML paragraph (<p>)."""


class MarkdownASTHeadingNode(MarkdownASTNode):
    """Node representing an HTML heading (<h1> through <h6>)."""
    def __init__(self, level: int, anchor_id: str) -> None:
        """
        Initialize a heading node.

        Args:
            level: The heading level (1-6)
        """
        super().__init__()

        # Level should be 1-6
        self.level = max(1, min(6, level))
        self.anchor_id = anchor_id


class MarkdownASTOrderedListNode(MarkdownASTNode):
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

        # Track whether this list should render tightly (without extra spacing)
        self.tight = True


class MarkdownASTUnorderedListNode(MarkdownASTNode):
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

        # Track whether this list should render tightly (without extra spacing)
        self.tight = True


class MarkdownASTListItemNode(MarkdownASTNode):
    """Node representing an HTML list item (<li>)."""


class MarkdownASTTextNode(MarkdownASTNode):
    """Node representing plain text content."""
    def __init__(self, content: str) -> None:
        """
        Initialize a text node.

        Args:
            content: The text content
        """
        super().__init__()
        self.content = content


class MarkdownASTBoldNode(MarkdownASTNode):
    """Node representing bold text (<b> or <strong>)."""


class MarkdownASTEmphasisNode(MarkdownASTNode):
    """Node representing emphasized text (<em> or <i>)."""


class MarkdownASTInlineCodeNode(MarkdownASTNode):
    """Node representing inline code (<code>)."""
    def __init__(self, content: str = "") -> None:
        """
        Initialize an inline code node.

        Args:
            content: The code content
        """
        super().__init__()
        self.content = content


class MarkdownASTLinkNode(MarkdownASTNode):
    """Node representing a link (<a>)."""
    def __init__(self, url: str = "", title: str | None = None) -> None:
        """
        Initialize a link node.

        Args:
            url: The link URL
            title: Optional title attribute
        """
        super().__init__()
        self.url = url
        self.title = title


class MarkdownASTImageNode(MarkdownASTNode):
    """Node representing an image (<img>)."""
    def __init__(self, url: str = "", alt_text: str = "", title: str | None = None) -> None:
        """
        Initialize an image node.

        Args:
            url: The image URL
            alt_text: Alternative text for the image
            title: Optional title attribute
        """
        super().__init__()
        self.url = url
        self.alt_text = alt_text
        self.title = title


class MarkdownASTCodeBlockNode(MarkdownASTNode):
    """Node representing a code block (<pre><code>)."""
    def __init__(
        self,
        language_name: str,
        content: str,
        tokens_by_line: List[List[Token]],
        states_by_line: List[ParserState | None],
        language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN,
        total_lines: int = 0
    ) -> None:
        """
        Initialize a code block node.

        Args:
            language: Optional language identifier for syntax highlighting
            content: The code content
        """
        super().__init__()
        self.language_name = language_name
        self.content = content
        self.tokens_by_line = tokens_by_line
        self.states_by_line = states_by_line
        self.language = language
        self.total_lines = total_lines


class MarkdownASTLineBreakNode(MarkdownASTNode):
    """Node representing a line break."""


class MarkdownASTTableNode(MarkdownASTNode):
    """Node representing an HTML table (<table>)."""


class MarkdownASTTableHeaderNode(MarkdownASTNode):
    """Node representing the header row section of a table (<thead>)."""


class MarkdownASTTableBodyNode(MarkdownASTNode):
    """Node representing the body section of a table (<tbody>)."""


class MarkdownASTTableRowNode(MarkdownASTNode):
    """Node representing a table row (<tr>)."""


class MarkdownASTTableCellNode(MarkdownASTNode):
    """Node representing a table cell (<td> or <th>)."""
    def __init__(self, is_header: bool = False, alignment: str = "left") -> None:
        """
        Initialize a table cell node.

        Args:
            is_header: Whether this is a header cell (<th>) or a data cell (<td>)
            alignment: Cell alignment ('left', 'center', 'right')
        """
        super().__init__()
        self.is_header = is_header
        self.alignment = alignment


class MarkdownASTHorizontalRuleNode(MarkdownASTNode):
    """Node representing a horizontal rule (<hr>)."""
