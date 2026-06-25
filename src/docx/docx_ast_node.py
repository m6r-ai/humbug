from typing import Any, Dict, List


class DocxASTNode:
    """Base class for all DOCX AST nodes."""

    def __init__(self) -> None:
        """Initialise a node with no parent and no children."""
        self.parent: "DocxASTNode | None" = None
        self.children: List["DocxASTNode"] = []

    def add_child(self, child: "DocxASTNode") -> "DocxASTNode":
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

    def remove_child(self, child: "DocxASTNode") -> None:
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


class DocxASTVisitor:
    """Base visitor class for DOCX AST traversal."""

    def visit(self, node: DocxASTNode) -> Any:
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

    def generic_visit(self, node: DocxASTNode) -> List[Any]:
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


class DocxASTDocumentNode(DocxASTNode):
    """
    Root node representing a complete DOCX document.

    Holds the parsed body and the supporting data loaded from the ZIP
    (styles, numbering, relationships).  Children are:
      - DocxASTBodyNode
      - DocxASTStylesNode (optional)
      - DocxASTNumberingNode (optional)
    """

    def __init__(self, source_path: str | None = None) -> None:
        """
        Initialise a document node.

        Args:
            source_path: Optional path to the source .docx file.
        """
        super().__init__()
        self.source_path = source_path
        # Resolved map from relationship ID (e.g. "rId6") to target path
        # (e.g. "media/image1.png") within the ZIP.
        self.relationships: Dict[str, str] = {}


class DocxASTBodyNode(DocxASTNode):
    """
    Node representing the document body (<w:body>).

    Children are block-level nodes: paragraphs, tables, and a final
    optional DocxASTSectionPropertiesNode.
    """


class DocxASTSectionPropertiesNode(DocxASTNode):
    """
    Node representing section properties (<w:sectPr>).

    Captures page size, margins and orientation.  Stored as raw
    attribute dicts since we preserve but do not deeply interpret them.
    """

    def __init__(
        self,
        page_width: int | None = None,
        page_height: int | None = None,
        margin_top: int | None = None,
        margin_right: int | None = None,
        margin_bottom: int | None = None,
        margin_left: int | None = None,
    ) -> None:
        """
        Initialise section properties.

        All measurements are in twentieths of a point (twips).

        Args:
            page_width: Page width in twips.
            page_height: Page height in twips.
            margin_top: Top margin in twips.
            margin_right: Right margin in twips.
            margin_bottom: Bottom margin in twips.
            margin_left: Left margin in twips.
        """
        super().__init__()
        self.page_width = page_width
        self.page_height = page_height
        self.margin_top = margin_top
        self.margin_right = margin_right
        self.margin_bottom = margin_bottom
        self.margin_left = margin_left


class DocxASTParagraphNode(DocxASTNode):
    """
    Node representing a paragraph (<w:p>).

    Children may include:
      - DocxASTParagraphPropertiesNode (at most one, first child)
      - DocxASTRunNode
      - DocxASTBookmarkStartNode
      - DocxASTBookmarkEndNode
    """


class DocxASTParagraphPropertiesNode(DocxASTNode):
    """
    Node representing paragraph properties (<w:pPr>).

    Captures the semantic properties needed for conversion.  Raw XML
    attributes that are not modelled explicitly are stored in ``extra``
    for round-trip fidelity.
    """

    def __init__(
        self,
        style_id: str | None = None,
        justification: str | None = None,
        outline_level: int | None = None,
        spacing_before: int | None = None,
        spacing_after: int | None = None,
        indent_left: int | None = None,
        indent_right: int | None = None,
        indent_hanging: int | None = None,
        indent_first_line: int | None = None,
        keep_next: bool = False,
        keep_lines: bool = False,
        page_break_before: bool = False,
        shading: str | None = None,
    ) -> None:
        """
        Initialise paragraph properties.

        Args:
            style_id: The w:styleId referenced by <w:pStyle w:val="...">.
            justification: Alignment value ('left', 'center', 'right', 'both').
            outline_level: Outline level 0-8 from <w:outlineLvl w:val="...">.
            spacing_before: Space before paragraph in twips.
            spacing_after: Space after paragraph in twips.
            indent_left: Left indent in twips.
            indent_right: Right indent in twips.
            indent_hanging: Hanging indent in twips.
            indent_first_line: First-line indent in twips.
            keep_next: Whether to keep with next paragraph.
            keep_lines: Whether to keep lines together.
            page_break_before: Whether to force a page break before.
            shading: Hex fill colour for paragraph background shading, or None.
        """
        super().__init__()
        self.style_id = style_id
        self.justification = justification
        self.outline_level = outline_level
        self.spacing_before = spacing_before
        self.spacing_after = spacing_after
        self.indent_left = indent_left
        self.indent_right = indent_right
        self.indent_hanging = indent_hanging
        self.indent_first_line = indent_first_line
        self.keep_next = keep_next
        self.keep_lines = keep_lines
        self.page_break_before = page_break_before
        self.shading = shading
        # Child: DocxASTNumberingPropertiesNode (optional)


class DocxASTNumberingPropertiesNode(DocxASTNode):
    """
    Node representing list numbering properties (<w:numPr>).

    Links a paragraph to a numbering definition.
    """

    def __init__(self, num_id: str, ilvl: int) -> None:
        """
        Initialise numbering properties.

        Args:
            num_id: The w:numId value referencing a <w:num> in numbering.xml.
            ilvl: The list indent level (0-based).
        """
        super().__init__()
        self.num_id = num_id
        self.ilvl = ilvl


class DocxASTRunNode(DocxASTNode):
    """
    Node representing a text run (<w:r>).

    Children may include:
      - DocxASTRunPropertiesNode (at most one, first child)
      - DocxASTTextNode
      - DocxASTTabNode
      - DocxASTBreakNode
      - DocxASTLastRenderedPageBreakNode
    """


class DocxASTRunPropertiesNode(DocxASTNode):
    """Node representing run properties (<w:rPr>).

    Captures character-level formatting.
    """

    def __init__(
        self,
        bold: bool = False,
        italic: bool = False,
        underline: str | None = None,
        strike: bool = False,
        double_strike: bool = False,
        vertical_align: str | None = None,
        style_id: str | None = None,
        font_ascii: str | None = None,
        font_h_ansi: str | None = None,
        font_cs: str | None = None,
        sz: int | None = None,
        sz_cs: int | None = None,
        color: str | None = None,
        highlight: str | None = None,
        lang: str | None = None,
    ) -> None:
        """
        Initialise run properties.

        Args:
            bold: Whether the run is bold (<w:b/>).
            italic: Whether the run is italic (<w:i/>).
            underline: Underline style value or None (<w:u w:val="..."/>).
            strike: Whether strikethrough is applied (<w:strike/>).
            double_strike: Whether double strikethrough is applied (<w:dstrike/>).
            vertical_align: Superscript/subscript ('superscript', 'subscript').
            style_id: Character style ID from <w:rStyle w:val="...">.
            font_ascii: ASCII font name.
            font_h_ansi: High-ANSI font name.
            font_cs: Complex-script font name.
            sz: Font size in half-points.
            sz_cs: Complex-script font size in half-points.
            color: Hex colour string or 'auto'.
            highlight: Highlight colour name.
            lang: Language tag.
        """
        super().__init__()
        self.bold = bold
        self.italic = italic
        self.underline = underline
        self.strike = strike
        self.double_strike = double_strike
        self.vertical_align = vertical_align
        self.style_id = style_id
        self.font_ascii = font_ascii
        self.font_h_ansi = font_h_ansi
        self.font_cs = font_cs
        self.sz = sz
        self.sz_cs = sz_cs
        self.color = color
        self.highlight = highlight
        self.lang = lang


class DocxASTTextNode(DocxASTNode):
    """Node representing text content (<w:t>)."""

    def __init__(self, content: str, preserve_space: bool = False) -> None:
        """
        Initialise a text node.

        Args:
            content: The text content.
            preserve_space: Whether xml:space="preserve" was set.
        """
        super().__init__()
        self.content = content
        self.preserve_space = preserve_space


class DocxASTTabNode(DocxASTNode):
    """Node representing a tab character (<w:tab/>)."""


class DocxASTBreakNode(DocxASTNode):
    """
    Node representing a break (<w:br/>).

    The break type determines whether this is a line break, page break,
    or column break.
    """

    def __init__(self, break_type: str = "textWrapping") -> None:
        """Initialise a break node.

        Args:
            break_type: The w:type attribute value.  Common values are
                'textWrapping' (line break), 'page', 'column'.
                Defaults to 'textWrapping' when the attribute is absent.
        """
        super().__init__()
        self.break_type = break_type


class DocxASTLastRenderedPageBreakNode(DocxASTNode):
    """
    Node representing a last-rendered page break (<w:lastRenderedPageBreak/>).

    This is a hint from Word about where a page break was rendered and is
    preserved in the AST but ignored by converters.
    """


class DocxASTBookmarkStartNode(DocxASTNode):
    """Node representing a bookmark start (<w:bookmarkStart>)."""

    def __init__(self, bookmark_id: str, name: str) -> None:
        """Initialise a bookmark start node.

        Args:
            bookmark_id: The w:id attribute value.
            name: The w:name attribute value.
        """
        super().__init__()
        self.bookmark_id = bookmark_id
        self.name = name


class DocxASTBookmarkEndNode(DocxASTNode):
    """Node representing a bookmark end (<w:bookmarkEnd>)."""

    def __init__(self, bookmark_id: str) -> None:
        """Initialise a bookmark end node.

        Args:
            bookmark_id: The w:id attribute value matching a start node.
        """
        super().__init__()
        self.bookmark_id = bookmark_id


class DocxASTDrawingNode(DocxASTNode):
    """
    Node representing an inline or anchored drawing (<w:drawing>).

    Images in DOCX are embedded as relationships.  The relationship ID
    is resolved to a path within the ZIP by the parser and stored here.
    """

    def __init__(
        self,
        relationship_id: str | None = None,
        resolved_path: str | None = None,
        description: str | None = None,
        width_emu: int | None = None,
        height_emu: int | None = None,
        image_data: bytes | None = None,
    ) -> None:
        """
        Initialise a drawing node.

        Args:
            relationship_id: The r:embed relationship ID (e.g. 'rId6').
            resolved_path: The resolved path within the ZIP (e.g.
                'word/media/image1.png'), set by the parser.
            description: Alt text / description from the drawing properties.
            width_emu: Width in English Metric Units (914400 EMU = 1 inch).
            height_emu: Height in English Metric Units.
            image_data: Raw bytes of the image read from the ZIP, or None if
                not yet extracted.
        """
        super().__init__()
        self.relationship_id = relationship_id
        self.resolved_path = resolved_path
        self.description = description
        self.width_emu = width_emu
        self.height_emu = height_emu
        self.image_data = image_data


class DocxASTHyperlinkNode(DocxASTNode):
    """
    Node representing a hyperlink (<w:hyperlink>).

    External hyperlinks carry a URL (resolved from r:id via relationships).
    Internal hyperlinks carry an anchor (bookmark name from w:anchor).
    Children are DocxASTRunNode instances that form the visible link text.
    """

    def __init__(self, url: str = "", anchor: str = "") -> None:
        """
        Initialise a hyperlink node.

        Args:
            url: The target URL for an external hyperlink.
            anchor: The bookmark name for an internal hyperlink.
        """
        super().__init__()
        self.url = url
        self.anchor = anchor


class DocxASTTableNode(DocxASTNode):
    """
    Node representing a table (<w:tbl>).

    Children:
      - DocxASTTablePropertiesNode (optional)
      - DocxASTTableGridNode (optional)
      - DocxASTTableRowNode (one or more)
    """


class DocxASTTablePropertiesNode(DocxASTNode):
    """
    Node representing table properties (<w:tblPr>).

    Captures table-level formatting.
    """

    def __init__(
        self,
        style_id: str | None = None,
        width: int | None = None,
        width_type: str | None = None,
        indent: int | None = None,
        layout: str | None = None,
        no_borders: bool = False,
    ) -> None:
        """
        Initialise table properties.

        Args:
            style_id: Table style ID from <w:tblStyle w:val="...">.
            width: Table width value.
            width_type: Table width type ('dxa', 'pct', 'auto').
            indent: Table indent from left margin in twips.
            layout: Table layout type ('fixed', 'autofit').
            no_borders: Whether to explicitly suppress all table borders.
        """
        super().__init__()
        self.style_id = style_id
        self.width = width
        self.width_type = width_type
        self.indent = indent
        self.layout = layout
        self.no_borders = no_borders


class DocxASTTableGridNode(DocxASTNode):
    """
    Node representing the table grid (<w:tblGrid>).

    Holds the column width definitions.
    """

    def __init__(self, column_widths: List[int] | None = None) -> None:
        """
        Initialise a table grid node.

        Args:
            column_widths: List of column widths in twips, one per
                <w:gridCol w:w="..."/> element.
        """
        super().__init__()
        self.column_widths: List[int] = column_widths or []


class DocxASTTableRowNode(DocxASTNode):
    """
    Node representing a table row (<w:tr>).

    Children:
      - DocxASTTableRowPropertiesNode (optional)
      - DocxASTTableCellNode (one or more)
    """


class DocxASTTableRowPropertiesNode(DocxASTNode):
    """Node representing table row properties (<w:trPr>)."""

    def __init__(
        self,
        is_header: bool = False,
        cant_split: bool = False,
        height: int | None = None,
        height_rule: str | None = None,
    ) -> None:
        """
        Initialise table row properties.

        Args:
            is_header: Whether this row is a header row (<w:tblHeader/>).
            cant_split: Whether the row cannot be split across pages.
            height: Row height in twips.
            height_rule: Height rule ('exact', 'atLeast', 'auto').
        """
        super().__init__()
        self.is_header = is_header
        self.cant_split = cant_split
        self.height = height
        self.height_rule = height_rule


class DocxASTTableCellNode(DocxASTNode):
    """
    Node representing a table cell (<w:tc>).

    Children:
      - DocxASTTableCellPropertiesNode (optional)
      - DocxASTParagraphNode (one or more)
      - DocxASTTableNode (for nested tables)
    """


class DocxASTTableCellPropertiesNode(DocxASTNode):
    """Node representing table cell properties (<w:tcPr>)."""

    def __init__(
        self,
        width: int | None = None,
        width_type: str | None = None,
        grid_span: int = 1,
        vertical_merge: str | None = None,
        vertical_alignment: str | None = None,
        shading_fill: str | None = None,
        left_bar: bool = False,
    ) -> None:
        """
        Initialise table cell properties.

        Args:
            width: Cell width value.
            width_type: Cell width type ('dxa', 'pct', 'auto').
            grid_span: Number of columns this cell spans (<w:gridSpan w:val="..."/>).
            vertical_merge: Vertical merge type ('restart', 'continue', None).
            vertical_alignment: Vertical alignment ('top', 'center', 'bottom').
            shading_fill: Background fill colour hex string.
            left_bar: Whether to render a coloured bar on the left edge of the cell.
        """
        super().__init__()
        self.width = width
        self.width_type = width_type
        self.grid_span = grid_span
        self.vertical_merge = vertical_merge
        self.vertical_alignment = vertical_alignment
        self.shading_fill = shading_fill
        self.left_bar = left_bar


class DocxASTStylesNode(DocxASTNode):
    """
    Root node for the styles.xml content.

    Children are DocxASTStyleNode instances.
    """

    def __init__(self) -> None:
        """Initialise the styles root node."""
        super().__init__()
        # Default run and paragraph properties from <w:docDefaults>
        self.default_run_properties: DocxASTRunPropertiesNode | None = None
        self.default_paragraph_properties: DocxASTParagraphPropertiesNode | None = None


class DocxASTStyleNode(DocxASTNode):
    """
    Node representing a single style definition (<w:style>).

    Children:
      - DocxASTParagraphPropertiesNode (optional)
      - DocxASTRunPropertiesNode (optional)
    """

    def __init__(
        self,
        style_type: str,
        style_id: str,
        name: str,
        based_on: str | None = None,
        next_style: str | None = None,
        is_default: bool = False,
        is_custom: bool = False,
    ) -> None:
        """
        Initialise a style node.

        Args:
            style_type: Style type ('paragraph', 'character', 'table', 'numbering').
            style_id: The w:styleId attribute value (e.g. 'Heading1').
            name: The human-readable style name from <w:name w:val="...">.
            based_on: The styleId this style inherits from, if any.
            next_style: The styleId applied to the next paragraph, if any.
            is_default: Whether this is the default style for its type.
            is_custom: Whether this is a custom (user-defined) style.
        """
        super().__init__()
        self.style_type = style_type
        self.style_id = style_id
        self.name = name
        self.based_on = based_on
        self.next_style = next_style
        self.is_default = is_default
        self.is_custom = is_custom


class DocxASTNumberingNode(DocxASTNode):
    """
    Root node for the numbering.xml content.

    Children:
      - DocxASTAbstractNumNode (zero or more)
      - DocxASTNumNode (zero or more)
    """


class DocxASTAbstractNumNode(DocxASTNode):
    """
    Node representing an abstract numbering definition (<w:abstractNum>).

    Children are DocxASTNumLevelNode instances, one per indent level.
    """

    def __init__(self, abstract_num_id: str) -> None:
        """
        Initialise an abstract numbering definition.

        Args:
            abstract_num_id: The w:abstractNumId attribute value.
        """
        super().__init__()
        self.abstract_num_id = abstract_num_id
        self.multi_level_type: str | None = None


class DocxASTNumNode(DocxASTNode):
    """
    Node representing a numbering instance (<w:num>).

    Links a concrete numId to an abstractNum definition.
    """

    def __init__(self, num_id: str, abstract_num_id: str, start_override: int | None = None) -> None:
        """
        Initialise a numbering instance.

        Args:
            num_id: The w:numId attribute value referenced by paragraphs.
            abstract_num_id: The abstractNumId this instance references.
            start_override: If set, overrides the starting number for level 0
                via a <w:lvlOverride> element.
        """
        super().__init__()
        self.num_id = num_id
        self.abstract_num_id = abstract_num_id
        self.start_override = start_override


class DocxASTNumLevelNode(DocxASTNode):
    """
    Node representing one level of a numbering definition (<w:lvl>).

    Holds the format, text pattern and indent for one list level.
    """

    def __init__(
        self,
        ilvl: int,
        start: int = 1,
        num_fmt: str = "bullet",
        lvl_text: str = "",
        lvl_jc: str = "left",
        indent_left: int | None = None,
        indent_hanging: int | None = None,
        font_ascii: str | None = None,
        font_h_ansi: str | None = None,
    ) -> None:
        """
        Initialise a numbering level.

        Args:
            ilvl: The indent level (0-based).
            start: The starting number value.
            num_fmt: The number format ('bullet', 'decimal', 'lowerLetter',
                'lowerRoman', 'upperLetter', 'upperRoman', etc.).
            lvl_text: The level text pattern (e.g. '%1.' or '•').
            lvl_jc: Justification of the list marker.
            indent_left: Left indent in twips.
            indent_hanging: Hanging indent in twips.
            font_ascii: ASCII font for the level marker (relevant for bullet
                characters that require specific fonts like Symbol/Wingdings).
            font_h_ansi: High-ANSI font for the level marker.
        """
        super().__init__()
        self.ilvl = ilvl
        self.start = start
        self.num_fmt = num_fmt
        self.lvl_text = lvl_text
        self.lvl_jc = lvl_jc
        self.indent_left = indent_left
        self.indent_hanging = indent_hanging
        self.font_ascii = font_ascii
        self.font_h_ansi = font_h_ansi
