from document_ir.document_ir_node import (
    DocumentIRBlockquoteNode,
    DocumentIRCodeBlockNode,
    DocumentIRDefinitionDescriptionNode,
    DocumentIRDefinitionListNode,
    DocumentIRDefinitionTermNode,
    DocumentIRDocumentNode,
    DocumentIRHeadingNode,
    DocumentIRHorizontalRuleNode,
    DocumentIRImageNode,
    DocumentIRLineBreakNode,
    DocumentIRLinkNode,
    DocumentIRListItemNode,
    DocumentIRNode,
    DocumentIROrderedListNode,
    DocumentIRParagraphNode,
    DocumentIRTableBodyNode,
    DocumentIRTableCellNode,
    DocumentIRTableHeaderNode,
    DocumentIRTableNode,
    DocumentIRTableRowNode,
    DocumentIRTextSpanNode,
    DocumentIRUnorderedListNode,
)
from dhtml.html_ast_node import (
    HtmlASTCommentNode,
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTNode,
    HtmlASTTextNode,
)

# Elements whose subtrees are skipped entirely during conversion.
_SKIP_ELEMENTS = frozenset({"script", "style", "head", "nav", "footer", "aside"})

# Mapping from HTML heading tag to IR heading level.
_HEADING_TAGS = {"h1": 1, "h2": 2, "h3": 3, "h4": 4, "h5": 5, "h6": 6}

# Inline formatting tags and their IR span flags.
_INLINE_BOLD = frozenset({"b", "strong"})
_INLINE_ITALIC = frozenset({"i", "em"})
_INLINE_STRIKE = frozenset({"s", "del", "strike"})
_INLINE_CODE = frozenset({"code", "kbd", "samp", "tt"})


def html_ast_to_document_ir(
    document: HtmlASTDocumentNode,
) -> DocumentIRDocumentNode:
    """
    Convert an HTML AST document to a document_ir tree.

    Structural HTML elements (headings, paragraphs, lists, tables, blockquotes,
    code blocks) are mapped to their document_ir equivalents. Inline formatting
    (bold, italic, strikethrough, inline code) is preserved. Elements with no
    document_ir equivalent are either skipped or their children are inlined.

    Args:
        document: The root HTML AST document node.

    Returns:
        A DocumentIRDocumentNode representing the document content.
    """
    ir_doc = DocumentIRDocumentNode(source_path=document.source_path)
    _convert_children(document, ir_doc)
    return ir_doc


def _convert_children(
    html_node: HtmlASTNode,
    ir_parent: DocumentIRNode,
) -> None:
    """
    Convert all children of an HTML node into children of an IR node.

    Args:
        html_node: The HTML node whose children are to be converted.
        ir_parent: The IR node that will receive the converted children.
    """
    for child in html_node.children:
        _convert_node(child, ir_parent)


def _convert_node(
    node: HtmlASTNode,
    ir_parent: DocumentIRNode,
) -> None:
    """
    Convert a single HTML AST node and attach the result to ir_parent.

    Args:
        node: The HTML AST node to convert.
        ir_parent: The IR node that will receive the result.
    """
    if isinstance(node, HtmlASTCommentNode):
        return

    if isinstance(node, HtmlASTTextNode):

        text = node.content
        if text.strip():
            ir_parent.add_child(DocumentIRTextSpanNode(content=text))

        return

    if not isinstance(node, HtmlASTElementNode):
        return

    tag = node.tag_name

    if tag in _SKIP_ELEMENTS:
        return

    if tag in _HEADING_TAGS:
        _convert_heading(node, ir_parent)
        return

    if tag == "p":
        _convert_paragraph(node, ir_parent)
        return

    if tag == "blockquote":
        _convert_blockquote(node, ir_parent)
        return

    if tag in ("pre",):
        _convert_pre(node, ir_parent)
        return

    if tag == "ul":
        _convert_list(node, ir_parent, ordered=False)
        return

    if tag == "ol":
        _convert_list(node, ir_parent, ordered=True)
        return

    if tag == "li":
        _convert_list_item(node, ir_parent)
        return

    if tag == "table":
        _convert_table(node, ir_parent)
        return

    if tag in ("hr",):
        ir_parent.add_child(DocumentIRHorizontalRuleNode())
        return

    if tag == "br":
        ir_parent.add_child(DocumentIRLineBreakNode())
        return

    if tag == "img":
        _convert_img(node, ir_parent)
        return

    if tag == "a":
        _convert_anchor(node, ir_parent)
        return

    if tag in _INLINE_BOLD:
        _convert_inline(node, ir_parent, bold=True)
        return

    if tag in _INLINE_ITALIC:
        _convert_inline(node, ir_parent, italic=True)
        return

    if tag in _INLINE_STRIKE:
        _convert_inline(node, ir_parent, strikethrough=True)
        return

    if tag in _INLINE_CODE:
        _convert_inline_code(node, ir_parent)
        return

    if tag == "dl":
        _convert_definition_list(node, ir_parent)
        return

    if tag == "dt":
        _convert_definition_term(node, ir_parent)
        return

    if tag == "dd":
        _convert_definition_description(node, ir_parent)
        return

    if tag == "sup":
        _convert_inline(node, ir_parent, superscript=True)
        return

    if tag == "sub":
        _convert_inline(node, ir_parent, subscript=True)
        return

    # Generic container — recurse into children without adding an IR node.
    _convert_children(node, ir_parent)


def _convert_heading(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert an <h1>–<h6> element."""
    level = _HEADING_TAGS[node.tag_name]
    heading = DocumentIRHeadingNode(level=level)
    ir_parent.add_child(heading)
    _convert_inline_children(node, heading)


def _convert_paragraph(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert a <p> element."""
    para = DocumentIRParagraphNode()
    ir_parent.add_child(para)
    _convert_inline_children(node, para)


def _convert_blockquote(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert a <blockquote> element."""
    bq = DocumentIRBlockquoteNode()
    ir_parent.add_child(bq)
    _convert_children(node, bq)


def _convert_pre(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert a <pre> element, detecting an inner <code> for language hints."""
    language = ""
    content_parts: list[str] = []

    # Check for a single <code> child which may carry a language class.
    code_child: HtmlASTElementNode | None = None
    for child in node.children:
        if isinstance(child, HtmlASTElementNode) and child.tag_name == "code":
            code_child = child
            break

    source = code_child if code_child is not None else node
    class_attr = source.attributes.get("class", "")
    for cls in class_attr.split():
        if cls.startswith("language-"):
            language = cls[len("language-"):]
            break

    _collect_text(source, content_parts)
    content = "".join(content_parts)
    # Strip a single leading/trailing newline that is commonly present.
    content = content.strip("\n")

    ir_parent.add_child(DocumentIRCodeBlockNode(language=language, content=content))


def _convert_list(
    node: HtmlASTElementNode,
    ir_parent: DocumentIRNode,
    ordered: bool,
) -> None:
    """Convert a <ul> or <ol> element."""

    if ordered:
        try:
            start = int(node.attributes.get("start", "1"))

        except ValueError:
            start = 1

        ir_list: DocumentIRNode = DocumentIROrderedListNode(start=start)

    else:
        ir_list = DocumentIRUnorderedListNode()

    ir_parent.add_child(ir_list)
    _convert_children(node, ir_list)


def _convert_list_item(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert an <li> element."""
    item = DocumentIRListItemNode()
    ir_parent.add_child(item)
    _convert_children(node, item)


def _convert_table(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert a <table> element."""
    table = DocumentIRTableNode()
    ir_parent.add_child(table)

    header: DocumentIRTableHeaderNode | None = None
    body: DocumentIRTableBodyNode | None = None
    direct_tr_nodes: list[HtmlASTElementNode] = []

    for child in node.children:
        if not isinstance(child, HtmlASTElementNode):
            continue

        tag = child.tag_name

        if tag == "thead":
            header = DocumentIRTableHeaderNode()
            table.add_child(header)
            _convert_table_rows(child, header, is_header=True)

        elif tag in ("tbody", "tfoot"):
            if body is None:
                body = DocumentIRTableBodyNode()
                table.add_child(body)

            _convert_table_rows(child, body, is_header=False)

        elif tag == "tr":
            # Table rows directly inside <table> with no thead/tbody wrapper.
            if body is None:
                body = DocumentIRTableBodyNode()
                table.add_child(body)

            direct_tr_nodes.append(child)

        elif tag == "caption":
            # Captions are not modelled in document_ir; skip.
            pass

    if direct_tr_nodes:
        if body is None:
            body = DocumentIRTableBodyNode()
            table.add_child(body)

        _convert_table_row_list(direct_tr_nodes, body, is_header=False)


def _convert_table_rows(
    node: HtmlASTElementNode,
    ir_section: DocumentIRNode,
    is_header: bool,
) -> None:
    """Convert all <tr> children of a table section node."""
    tr_nodes = [
        child
        for child in node.children
        if isinstance(child, HtmlASTElementNode) and child.tag_name == "tr"
    ]
    _convert_table_row_list(tr_nodes, ir_section, is_header=is_header)


def _convert_table_row_list(
    tr_nodes: list[HtmlASTElementNode],
    ir_parent: DocumentIRNode,
    is_header: bool,
) -> None:
    """
    Convert a list of <tr> elements with shared rowspan state.

    Pending spans are scoped to this call so rowspans do not cross section
    boundaries (thead/tbody/tfoot are each processed independently).
    """
    # Maps col index → (remaining_rows, is_header, alignment)
    pending_spans: dict[int, tuple[int, bool, str]] = {}

    for tr_node in tr_nodes:
        row = DocumentIRTableRowNode()
        ir_parent.add_child(row)

        # Collect the real td/th children for this row.
        real_cells = [
            child
            for child in tr_node.children
            if isinstance(child, HtmlASTElementNode) and child.tag_name in ("td", "th")
        ]
        real_cell_iter = iter(real_cells)

        col = 0
        for real_cell in real_cell_iter:
            # Drain any pending spans that occupy columns before this real cell.
            while col in pending_spans:
                remaining, span_is_header, span_align = pending_spans[col]
                row.add_child(
                    DocumentIRTableCellNode(
                        is_header=span_is_header,
                        alignment=span_align,
                    )
                )
                remaining -= 1
                if remaining == 0:
                    del pending_spans[col]

                else:
                    pending_spans[col] = (remaining, span_is_header, span_align)

                col += 1

            # Process the real cell.
            cell_is_header = is_header or real_cell.tag_name == "th"
            align_str = real_cell.attributes.get("align", "left").lower()
            if align_str not in ("left", "center", "right"):
                align_str = "left"

            try:
                colspan = max(1, int(real_cell.attributes.get("colspan", "1")))

            except ValueError:
                colspan = 1

            try:
                rowspan = max(1, int(real_cell.attributes.get("rowspan", "1")))

            except ValueError:
                rowspan = 1

            # Create the primary cell with content.
            cell = DocumentIRTableCellNode(
                is_header=cell_is_header,
                alignment=align_str,
            )
            row.add_child(cell)
            _convert_inline_children(real_cell, cell)

            # Register rowspans and add colspan filler cells.
            for c in range(colspan):
                if rowspan > 1:
                    pending_spans[col + c] = (rowspan - 1, cell_is_header, align_str)

                if c > 0:
                    row.add_child(
                        DocumentIRTableCellNode(
                            is_header=cell_is_header,
                            alignment=align_str,
                        )
                    )

            col += colspan

        # After all real cells are consumed, inject fillers for any pending span
        # columns that lie at or beyond the current col position.
        for span_col in sorted(pending_spans.keys()):
            if span_col < col:
                continue

            remaining, span_is_header, span_align = pending_spans[span_col]
            row.add_child(
                DocumentIRTableCellNode(
                    is_header=span_is_header,
                    alignment=span_align,
                )
            )
            remaining -= 1
            if remaining == 0:
                del pending_spans[span_col]

            else:
                pending_spans[span_col] = (remaining, span_is_header, span_align)


def _convert_img(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert an <img> element."""
    url = node.attributes.get("src", "")
    alt = node.attributes.get("alt", "")
    title = node.attributes.get("title") or None
    ir_parent.add_child(DocumentIRImageNode(url=url, alt_text=alt, title=title))


def _convert_anchor(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert an <a> element."""
    url = node.attributes.get("href", "")
    title = node.attributes.get("title") or None
    link = DocumentIRLinkNode(url=url, title=title)
    ir_parent.add_child(link)
    _convert_inline_children(node, link)


def _convert_definition_list(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert a <dl> element."""
    dl = DocumentIRDefinitionListNode()
    ir_parent.add_child(dl)
    _convert_children(node, dl)


def _convert_definition_term(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert a <dt> element."""
    dt = DocumentIRDefinitionTermNode()
    ir_parent.add_child(dt)
    _convert_inline_children(node, dt)


def _convert_definition_description(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert a <dd> element."""
    dd = DocumentIRDefinitionDescriptionNode()
    ir_parent.add_child(dd)
    _convert_inline_children(node, dd)


def _convert_inline(
    node: HtmlASTElementNode,
    ir_parent: DocumentIRNode,
    bold: bool = False,
    italic: bool = False,
    strikethrough: bool = False,
    superscript: bool = False,
    subscript: bool = False,
) -> None:
    """Convert an inline formatting element by wrapping text children in spans."""
    for child in node.children:
        if isinstance(child, HtmlASTTextNode):
            ir_parent.add_child(
                DocumentIRTextSpanNode(
                    content=child.content,
                    bold=bold,
                    italic=italic,
                    strikethrough=strikethrough,
                    superscript=superscript,
                    subscript=subscript,
                )
            )

        elif isinstance(child, HtmlASTElementNode):
            _convert_node(child, ir_parent)


def _convert_inline_code(node: HtmlASTElementNode, ir_parent: DocumentIRNode) -> None:
    """Convert an inline code element (<code> outside of <pre>)."""
    parts: list[str] = []
    _collect_text(node, parts)
    content = "".join(parts)
    if content:
        ir_parent.add_child(DocumentIRTextSpanNode(content=content, code=True))


def _convert_inline_children(
    node: HtmlASTElementNode,
    ir_parent: DocumentIRNode,
) -> None:
    """Convert the children of a node as inline content."""
    for child in node.children:
        _convert_node(child, ir_parent)


def _collect_text(node: HtmlASTNode, parts: list[str]) -> None:
    """
    Recursively collect all text content from a node into parts.

    Args:
        node: The node to collect text from.
        parts: Accumulator list of text strings.
    """
    if isinstance(node, HtmlASTTextNode):
        parts.append(node.content)
        return

    for child in node.children:
        _collect_text(child, parts)
