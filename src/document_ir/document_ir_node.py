from typing import Any, List


class DocumentIRNode:
    """Base class for all document IR nodes."""

    def __init__(self) -> None:
        """Initialise a node with no parent and no children."""
        self.parent: "DocumentIRNode | None" = None
        self.children: List["DocumentIRNode"] = []

    def add_child(self, child: "DocumentIRNode") -> "DocumentIRNode":
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

    def remove_child(self, child: "DocumentIRNode") -> None:
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


class DocumentIRVisitor:
    """Base visitor class for document IR tree traversal."""

    def visit(self, node: DocumentIRNode) -> Any:
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

    def generic_visit(self, node: DocumentIRNode) -> List[Any]:
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


class DocumentIRDocumentNode(DocumentIRNode):
    """Root node representing an entire document."""

    def __init__(self, source_path: str | None = None) -> None:
        """
        Initialise a document node.

        Args:
            source_path: Optional path to the source file.
        """
        super().__init__()
        self.source_path = source_path


class DocumentIRHeadingNode(DocumentIRNode):
    """Node representing a heading at a given level (1-6)."""

    def __init__(self, level: int) -> None:
        """
        Initialise a heading node.

        Args:
            level: The heading level (1-6).
        """
        super().__init__()
        self.level = max(1, min(6, level))


class DocumentIRParagraphNode(DocumentIRNode):
    """Node representing a paragraph of body text."""


class DocumentIRBlockquoteNode(DocumentIRNode):
    """Node representing a blockquote."""


class DocumentIRCodeBlockNode(DocumentIRNode):
    """Node representing a fenced code block."""

    def __init__(self, language: str, content: str) -> None:
        """
        Initialise a code block node.

        Args:
            language: The language identifier (may be empty string).
            content: The raw code content.
        """
        super().__init__()
        self.language = language
        self.content = content


class DocumentIRUnorderedListNode(DocumentIRNode):
    """Node representing an unordered (bullet) list."""

    def __init__(self, tight: bool = True) -> None:
        """
        Initialise an unordered list node.

        Args:
            tight: Whether the list is tight (no spacing between items).
        """
        super().__init__()
        self.tight = tight


class DocumentIROrderedListNode(DocumentIRNode):
    """Node representing an ordered (numbered) list."""

    def __init__(self, start: int = 1, tight: bool = True) -> None:
        """
        Initialise an ordered list node.

        Args:
            start: The starting number for the list.
            tight: Whether the list is tight (no spacing between items).
        """
        super().__init__()
        self.start = start
        self.tight = tight


class DocumentIRListItemNode(DocumentIRNode):
    """Node representing a single item within a list."""


class DocumentIRDefinitionListNode(DocumentIRNode):
    """Node representing a definition list (<dl>)."""


class DocumentIRDefinitionTermNode(DocumentIRNode):
    """Node representing a definition term (<dt>)."""


class DocumentIRDefinitionDescriptionNode(DocumentIRNode):
    """Node representing a definition description (<dd>)."""


class DocumentIRTableNode(DocumentIRNode):
    """Node representing a table."""


class DocumentIRTableHeaderNode(DocumentIRNode):
    """Node representing the header section of a table."""


class DocumentIRTableBodyNode(DocumentIRNode):
    """Node representing the body section of a table."""


class DocumentIRTableRowNode(DocumentIRNode):
    """Node representing a single row within a table."""


class DocumentIRTableCellNode(DocumentIRNode):
    """Node representing a single cell within a table row."""

    def __init__(self, is_header: bool = False, alignment: str = "left") -> None:
        """
        Initialise a table cell node.

        Args:
            is_header: Whether this cell is a header cell.
            alignment: Text alignment within the cell ('left', 'center', 'right').
        """
        super().__init__()
        self.is_header = is_header
        self.alignment = alignment


class DocumentIRHorizontalRuleNode(DocumentIRNode):
    """Node representing a horizontal rule."""


class DocumentIRTextSpanNode(DocumentIRNode):
    """
    Node representing a run of inline text with optional formatting.

    Formatting properties are held as flags rather than through nesting,
    since a single span of text may carry any combination of bold, italic,
    strikethrough, code, superscript and subscript simultaneously.
    """

    def __init__(
        self,
        content: str,
        bold: bool = False,
        italic: bool = False,
        strikethrough: bool = False,
        code: bool = False,
        superscript: bool = False,
        subscript: bool = False,
    ) -> None:
        """
        Initialise a text span node.

        Args:
            content: The text content.
            bold: Whether the text is bold.
            italic: Whether the text is italic.
            strikethrough: Whether the text has strikethrough.
            code: Whether the text is rendered as inline code.
            superscript: Whether the text is superscript.
            subscript: Whether the text is subscript.
        """
        super().__init__()
        self.content = content
        self.bold = bold
        self.italic = italic
        self.strikethrough = strikethrough
        self.code = code
        self.superscript = superscript
        self.subscript = subscript


class DocumentIRLinkNode(DocumentIRNode):
    """
    Node representing a hyperlink.

    Children are the inline nodes forming the display text.
    """

    def __init__(self, url: str, title: str | None = None) -> None:
        """
        Initialise a link node.

        Args:
            url: The link target URL.
            title: Optional tooltip title.
        """
        super().__init__()
        self.url = url
        self.title = title


class DocumentIRImageNode(DocumentIRNode):
    """Node representing an image."""

    def __init__(
        self,
        url: str,
        alt_text: str = "",
        title: str | None = None,
        data: bytes | None = None,
        mime_type: str | None = None,
    ) -> None:
        """
        Initialise an image node.

        Args:
            url: The image URL or path.
            alt_text: Alternative text for the image.
            title: Optional tooltip title.
            data: Raw image bytes, present when the image was embedded in the
                source document rather than referenced by URL.
            mime_type: MIME type of the image data (e.g. 'image/png'), or None.
        """
        super().__init__()
        self.url = url
        self.alt_text = alt_text
        self.title = title
        self.data = data
        self.mime_type = mime_type


class DocumentIRLineBreakNode(DocumentIRNode):
    """Node representing an explicit line break within a block."""
