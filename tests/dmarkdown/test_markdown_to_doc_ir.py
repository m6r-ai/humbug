"""Tests for the dmarkdown AST → doc_ir mapper."""

from dmarkdown.markdown_ast_node import (
    MarkdownASTBlockquoteNode,
    MarkdownASTBoldNode,
    MarkdownASTCodeBlockNode,
    MarkdownASTDocumentNode,
    MarkdownASTEmphasisNode,
    MarkdownASTHeadingNode,
    MarkdownASTHorizontalRuleNode,
    MarkdownASTImageNode,
    MarkdownASTInlineCodeNode,
    MarkdownASTLineBreakNode,
    MarkdownASTLinkNode,
    MarkdownASTListItemNode,
    MarkdownASTOrderedListNode,
    MarkdownASTParagraphNode,
    MarkdownASTTableBodyNode,
    MarkdownASTTableCellNode,
    MarkdownASTTableHeaderNode,
    MarkdownASTTableNode,
    MarkdownASTTableRowNode,
    MarkdownASTTextNode,
    MarkdownASTUnorderedListNode,
)
from dmarkdown.markdown_to_doc_ir import markdown_ast_to_doc_ir
from doc_ir import (
    DocIRBlockquoteNode,
    DocIRCodeBlockNode,
    DocIRDocumentNode,
    DocIRHeadingNode,
    DocIRHorizontalRuleNode,
    DocIRImageNode,
    DocIRLineBreakNode,
    DocIRLinkNode,
    DocIRListItemNode,
    DocIROrderedListNode,
    DocIRParagraphNode,
    DocIRTableBodyNode,
    DocIRTableCellNode,
    DocIRTableHeaderNode,
    DocIRTableNode,
    DocIRTableRowNode,
    DocIRTextSpanNode,
    DocIRUnorderedListNode,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _doc(*children) -> MarkdownASTDocumentNode:
    """Build a MarkdownASTDocumentNode with the given children."""
    doc = MarkdownASTDocumentNode()
    for child in children:
        doc.add_child(child)
    return doc


def _para(*children) -> MarkdownASTParagraphNode:
    """Build a paragraph with the given inline children."""
    para = MarkdownASTParagraphNode()
    for child in children:
        para.add_child(child)
    return para


def _text(content: str) -> MarkdownASTTextNode:
    return MarkdownASTTextNode(content=content)


def _bold(*children) -> MarkdownASTBoldNode:
    node = MarkdownASTBoldNode()
    for child in children:
        node.add_child(child)
    return node


def _em(*children) -> MarkdownASTEmphasisNode:
    node = MarkdownASTEmphasisNode()
    for child in children:
        node.add_child(child)
    return node


def _code(content: str) -> MarkdownASTInlineCodeNode:
    return MarkdownASTInlineCodeNode(content=content)


def _heading(level: int, *children) -> MarkdownASTHeadingNode:
    node = MarkdownASTHeadingNode(level=level, anchor_id="")
    for child in children:
        node.add_child(child)
    return node


def _map(doc: MarkdownASTDocumentNode) -> DocIRDocumentNode:
    return markdown_ast_to_doc_ir(doc)


# ---------------------------------------------------------------------------
# Document root
# ---------------------------------------------------------------------------

class TestDocumentMapping:
    def test_empty_document(self):
        result = _map(_doc())
        assert isinstance(result, DocIRDocumentNode)
        assert result.children == []

    def test_source_path_preserved(self):
        doc = MarkdownASTDocumentNode(source_path="/path/to/file.md")
        result = _map(doc)
        assert result.source_path == "/path/to/file.md"

    def test_source_path_none(self):
        result = _map(_doc())
        assert result.source_path is None


# ---------------------------------------------------------------------------
# Headings
# ---------------------------------------------------------------------------

class TestHeadingMapping:
    def test_heading_level_1(self):
        result = _map(_doc(_heading(1, _text("Title"))))
        h = result.children[0]
        assert isinstance(h, DocIRHeadingNode)
        assert h.level == 1

    def test_heading_level_3(self):
        result = _map(_doc(_heading(3, _text("Section"))))
        h = result.children[0]
        assert h.level == 3

    def test_heading_text_content(self):
        result = _map(_doc(_heading(2, _text("Hello"))))
        h = result.children[0]
        span = h.children[0]
        assert isinstance(span, DocIRTextSpanNode)
        assert span.content == "Hello"

    def test_heading_with_bold(self):
        result = _map(_doc(_heading(1, _bold(_text("Bold Title")))))
        h = result.children[0]
        span = h.children[0]
        assert isinstance(span, DocIRTextSpanNode)
        assert span.bold is True
        assert span.content == "Bold Title"

    def test_heading_with_mixed_inline(self):
        result = _map(_doc(_heading(2, _text("Hello "), _em(_text("world")))))
        h = result.children[0]
        assert len(h.children) == 2
        assert h.children[0].content == "Hello "
        assert h.children[0].italic is False
        assert h.children[1].content == "world"
        assert h.children[1].italic is True


# ---------------------------------------------------------------------------
# Paragraphs
# ---------------------------------------------------------------------------

class TestParagraphMapping:
    def test_simple_paragraph(self):
        result = _map(_doc(_para(_text("Hello"))))
        para = result.children[0]
        assert isinstance(para, DocIRParagraphNode)

    def test_paragraph_text(self):
        result = _map(_doc(_para(_text("Hello world"))))
        para = result.children[0]
        span = para.children[0]
        assert isinstance(span, DocIRTextSpanNode)
        assert span.content == "Hello world"

    def test_multiple_paragraphs(self):
        result = _map(_doc(
            _para(_text("First")),
            _para(_text("Second")),
        ))
        assert len(result.children) == 2
        assert isinstance(result.children[0], DocIRParagraphNode)
        assert isinstance(result.children[1], DocIRParagraphNode)


# ---------------------------------------------------------------------------
# Inline formatting — span flattening
# ---------------------------------------------------------------------------

class TestInlineFormatting:
    def test_plain_text_span(self):
        result = _map(_doc(_para(_text("plain"))))
        span = result.children[0].children[0]
        assert isinstance(span, DocIRTextSpanNode)
        assert span.content == "plain"
        assert span.bold is False
        assert span.italic is False
        assert span.code is False
        assert span.strikethrough is False

    def test_bold_span(self):
        result = _map(_doc(_para(_bold(_text("bold")))))
        span = result.children[0].children[0]
        assert span.bold is True
        assert span.italic is False
        assert span.content == "bold"

    def test_italic_span(self):
        result = _map(_doc(_para(_em(_text("italic")))))
        span = result.children[0].children[0]
        assert span.italic is True
        assert span.bold is False
        assert span.content == "italic"

    def test_bold_italic_combined(self):
        result = _map(_doc(_para(_bold(_em(_text("bold italic"))))))
        span = result.children[0].children[0]
        assert span.bold is True
        assert span.italic is True
        assert span.content == "bold italic"

    def test_italic_bold_combined(self):
        result = _map(_doc(_para(_em(_bold(_text("italic bold"))))))
        span = result.children[0].children[0]
        assert span.bold is True
        assert span.italic is True

    def test_inline_code(self):
        result = _map(_doc(_para(_code("some_func()"))))
        span = result.children[0].children[0]
        assert isinstance(span, DocIRTextSpanNode)
        assert span.code is True
        assert span.content == "some_func()"

    def test_bold_code(self):
        # Bold wrapping inline code: bold flag should carry through
        result = _map(_doc(_para(_bold(_code("bold_code()")))))
        span = result.children[0].children[0]
        assert span.bold is True
        assert span.code is True
        assert span.content == "bold_code()"

    def test_mixed_inline_sequence(self):
        result = _map(_doc(_para(
            _text("Hello "),
            _bold(_text("world")),
            _text("!"),
        )))
        para = result.children[0]
        assert len(para.children) == 3
        assert para.children[0].content == "Hello "
        assert para.children[0].bold is False
        assert para.children[1].content == "world"
        assert para.children[1].bold is True
        assert para.children[2].content == "!"
        assert para.children[2].bold is False

    def test_empty_text_node_omitted(self):
        result = _map(_doc(_para(_text(""))))
        para = result.children[0]
        assert para.children == []

    def test_empty_inline_code_omitted(self):
        result = _map(_doc(_para(_code(""))))
        para = result.children[0]
        assert para.children == []

    def test_line_break(self):
        lb = MarkdownASTLineBreakNode()
        result = _map(_doc(_para(_text("before"), lb, _text("after"))))
        para = result.children[0]
        assert len(para.children) == 3
        assert isinstance(para.children[1], DocIRLineBreakNode)

    def test_deeply_nested_bold_italic(self):
        # bold > italic > bold > text — all three flags should be set
        node = _bold(_em(_bold(_text("deep"))))
        result = _map(_doc(_para(node)))
        span = result.children[0].children[0]
        assert span.bold is True
        assert span.italic is True
        assert span.content == "deep"


# ---------------------------------------------------------------------------
# Links and images
# ---------------------------------------------------------------------------

class TestLinksAndImages:
    def test_link_node(self):
        link = MarkdownASTLinkNode(url="https://example.com", title="Example")
        link.add_child(_text("click here"))
        result = _map(_doc(_para(link)))
        para = result.children[0]
        link_node = para.children[0]
        assert isinstance(link_node, DocIRLinkNode)
        assert link_node.url == "https://example.com"
        assert link_node.title == "Example"

    def test_link_display_text(self):
        link = MarkdownASTLinkNode(url="https://example.com")
        link.add_child(_text("click here"))
        result = _map(_doc(_para(link)))
        link_node = result.children[0].children[0]
        assert len(link_node.children) == 1
        span = link_node.children[0]
        assert isinstance(span, DocIRTextSpanNode)
        assert span.content == "click here"

    def test_link_with_bold_text(self):
        link = MarkdownASTLinkNode(url="https://example.com")
        link.add_child(_bold(_text("bold link")))
        result = _map(_doc(_para(link)))
        link_node = result.children[0].children[0]
        span = link_node.children[0]
        assert span.bold is True
        assert span.content == "bold link"

    def test_link_no_title(self):
        link = MarkdownASTLinkNode(url="https://example.com")
        link.add_child(_text("text"))
        result = _map(_doc(_para(link)))
        link_node = result.children[0].children[0]
        assert link_node.title is None

    def test_image_node(self):
        img = MarkdownASTImageNode(
            url="image.png",
            alt_text="A photo",
            title="Photo title",
        )
        result = _map(_doc(_para(img)))
        para = result.children[0]
        img_node = para.children[0]
        assert isinstance(img_node, DocIRImageNode)
        assert img_node.url == "image.png"
        assert img_node.alt_text == "A photo"
        assert img_node.title == "Photo title"

    def test_image_no_title(self):
        img = MarkdownASTImageNode(url="image.png", alt_text="Alt")
        result = _map(_doc(_para(img)))
        img_node = result.children[0].children[0]
        assert img_node.title is None


# ---------------------------------------------------------------------------
# Blockquote
# ---------------------------------------------------------------------------

class TestBlockquoteMapping:
    def test_blockquote_node(self):
        bq = MarkdownASTBlockquoteNode()
        bq.add_child(_para(_text("Quoted text")))
        result = _map(_doc(bq))
        assert isinstance(result.children[0], DocIRBlockquoteNode)

    def test_blockquote_contains_paragraph(self):
        bq = MarkdownASTBlockquoteNode()
        bq.add_child(_para(_text("Quoted")))
        result = _map(_doc(bq))
        bq_node = result.children[0]
        assert len(bq_node.children) == 1
        assert isinstance(bq_node.children[0], DocIRParagraphNode)

    def test_blockquote_text_content(self):
        bq = MarkdownASTBlockquoteNode()
        bq.add_child(_para(_text("Quoted text")))
        result = _map(_doc(bq))
        para = result.children[0].children[0]
        assert para.children[0].content == "Quoted text"

    def test_nested_blockquote(self):
        inner = MarkdownASTBlockquoteNode()
        inner.add_child(_para(_text("Inner")))
        outer = MarkdownASTBlockquoteNode()
        outer.add_child(inner)
        result = _map(_doc(outer))
        outer_node = result.children[0]
        assert isinstance(outer_node.children[0], DocIRBlockquoteNode)


# ---------------------------------------------------------------------------
# Code block
# ---------------------------------------------------------------------------

class TestCodeBlockMapping:
    def _code_block(self, language: str, content: str) -> MarkdownASTCodeBlockNode:
        from syntax import ProgrammingLanguage
        return MarkdownASTCodeBlockNode(
            language_name=language,
            content=content,
            tokens_by_line=[],
            states_by_line=[],
            language=ProgrammingLanguage.UNKNOWN,
            total_lines=0,
        )

    def test_code_block_node(self):
        result = _map(_doc(self._code_block("python", "print('hello')")))
        assert isinstance(result.children[0], DocIRCodeBlockNode)

    def test_code_block_language(self):
        result = _map(_doc(self._code_block("python", "x = 1")))
        cb = result.children[0]
        assert cb.language == "python"

    def test_code_block_content(self):
        result = _map(_doc(self._code_block("", "some code")))
        cb = result.children[0]
        assert cb.content == "some code"

    def test_code_block_empty_language(self):
        result = _map(_doc(self._code_block("", "code")))
        cb = result.children[0]
        assert cb.language == ""

    def test_code_block_no_children(self):
        result = _map(_doc(self._code_block("python", "x = 1")))
        cb = result.children[0]
        assert cb.children == []


# ---------------------------------------------------------------------------
# Horizontal rule
# ---------------------------------------------------------------------------

class TestHorizontalRule:
    def test_horizontal_rule(self):
        hr = MarkdownASTHorizontalRuleNode()
        result = _map(_doc(hr))
        assert isinstance(result.children[0], DocIRHorizontalRuleNode)


# ---------------------------------------------------------------------------
# Unordered lists
# ---------------------------------------------------------------------------

class TestUnorderedListMapping:
    def _item(self, *inline) -> MarkdownASTListItemNode:
        item = MarkdownASTListItemNode()
        for child in inline:
            item.add_child(child)
        return item

    def test_unordered_list_node(self):
        ul = MarkdownASTUnorderedListNode()
        ul.add_child(self._item(_text("Item")))
        result = _map(_doc(ul))
        assert isinstance(result.children[0], DocIRUnorderedListNode)

    def test_list_item_node(self):
        ul = MarkdownASTUnorderedListNode()
        ul.add_child(self._item(_text("Item")))
        result = _map(_doc(ul))
        ul_node = result.children[0]
        assert len(ul_node.children) == 1
        assert isinstance(ul_node.children[0], DocIRListItemNode)

    def test_list_item_text(self):
        ul = MarkdownASTUnorderedListNode()
        ul.add_child(self._item(_text("Hello")))
        result = _map(_doc(ul))
        item = result.children[0].children[0]
        # Inline content is wrapped in an implicit paragraph
        para = item.children[0]
        assert isinstance(para, DocIRParagraphNode)
        assert para.children[0].content == "Hello"

    def test_multiple_list_items(self):
        ul = MarkdownASTUnorderedListNode()
        ul.add_child(self._item(_text("A")))
        ul.add_child(self._item(_text("B")))
        ul.add_child(self._item(_text("C")))
        result = _map(_doc(ul))
        ul_node = result.children[0]
        assert len(ul_node.children) == 3

    def test_nested_list(self):
        inner = MarkdownASTUnorderedListNode()
        inner.add_child(self._item(_text("Inner")))

        outer_item = MarkdownASTListItemNode()
        outer_item.add_child(_text("Outer"))
        outer_item.add_child(inner)

        outer = MarkdownASTUnorderedListNode()
        outer.add_child(outer_item)

        result = _map(_doc(outer))
        outer_item_node = result.children[0].children[0]
        # Should have implicit paragraph + nested list
        assert len(outer_item_node.children) == 2
        assert isinstance(outer_item_node.children[0], DocIRParagraphNode)
        assert isinstance(outer_item_node.children[1], DocIRUnorderedListNode)

    def test_list_item_bold_text(self):
        ul = MarkdownASTUnorderedListNode()
        ul.add_child(self._item(_bold(_text("Bold item"))))
        result = _map(_doc(ul))
        item = result.children[0].children[0]
        para = item.children[0]
        assert para.children[0].bold is True
        assert para.children[0].content == "Bold item"


# ---------------------------------------------------------------------------
# Ordered lists
# ---------------------------------------------------------------------------

class TestOrderedListMapping:
    def _item(self, *inline) -> MarkdownASTListItemNode:
        item = MarkdownASTListItemNode()
        for child in inline:
            item.add_child(child)
        return item

    def test_ordered_list_node(self):
        ol = MarkdownASTOrderedListNode(start=1)
        ol.add_child(self._item(_text("First")))
        result = _map(_doc(ol))
        assert isinstance(result.children[0], DocIROrderedListNode)

    def test_ordered_list_start_preserved(self):
        ol = MarkdownASTOrderedListNode(start=3)
        ol.add_child(self._item(_text("Third")))
        result = _map(_doc(ol))
        ol_node = result.children[0]
        assert ol_node.start == 3

    def test_ordered_list_default_start(self):
        ol = MarkdownASTOrderedListNode(start=1)
        ol.add_child(self._item(_text("First")))
        result = _map(_doc(ol))
        assert result.children[0].start == 1

    def test_ordered_list_items(self):
        ol = MarkdownASTOrderedListNode(start=1)
        ol.add_child(self._item(_text("A")))
        ol.add_child(self._item(_text("B")))
        result = _map(_doc(ol))
        assert len(result.children[0].children) == 2


# ---------------------------------------------------------------------------
# Tables
# ---------------------------------------------------------------------------

def _table_with_header_and_body() -> MarkdownASTTableNode:
    """Build a minimal 2-column table with one header row and one body row."""
    table = MarkdownASTTableNode()

    # Header
    header = MarkdownASTTableHeaderNode()
    header_row = MarkdownASTTableRowNode()
    cell_a = MarkdownASTTableCellNode(is_header=True, alignment="left")
    cell_a.add_child(_text("Name"))
    cell_b = MarkdownASTTableCellNode(is_header=True, alignment="right")
    cell_b.add_child(_text("Value"))
    header_row.add_child(cell_a)
    header_row.add_child(cell_b)
    header.add_child(header_row)
    table.add_child(header)

    # Body
    body = MarkdownASTTableBodyNode()
    body_row = MarkdownASTTableRowNode()
    cell_c = MarkdownASTTableCellNode(is_header=False, alignment="left")
    cell_c.add_child(_text("foo"))
    cell_d = MarkdownASTTableCellNode(is_header=False, alignment="right")
    cell_d.add_child(_text("42"))
    body_row.add_child(cell_c)
    body_row.add_child(cell_d)
    body.add_child(body_row)
    table.add_child(body)

    return table


class TestTableMapping:
    def test_table_node(self):
        result = _map(_doc(_table_with_header_and_body()))
        assert isinstance(result.children[0], DocIRTableNode)

    def test_table_has_header(self):
        result = _map(_doc(_table_with_header_and_body()))
        table = result.children[0]
        headers = [c for c in table.children if isinstance(c, DocIRTableHeaderNode)]
        assert len(headers) == 1

    def test_table_has_body(self):
        result = _map(_doc(_table_with_header_and_body()))
        table = result.children[0]
        bodies = [c for c in table.children if isinstance(c, DocIRTableBodyNode)]
        assert len(bodies) == 1

    def test_header_row(self):
        result = _map(_doc(_table_with_header_and_body()))
        header = next(
            c for c in result.children[0].children
            if isinstance(c, DocIRTableHeaderNode)
        )
        rows = [c for c in header.children if isinstance(c, DocIRTableRowNode)]
        assert len(rows) == 1

    def test_header_cells(self):
        result = _map(_doc(_table_with_header_and_body()))
        header = next(
            c for c in result.children[0].children
            if isinstance(c, DocIRTableHeaderNode)
        )
        row = header.children[0]
        cells = [c for c in row.children if isinstance(c, DocIRTableCellNode)]
        assert len(cells) == 2

    def test_header_cell_is_header_flag(self):
        result = _map(_doc(_table_with_header_and_body()))
        header = next(
            c for c in result.children[0].children
            if isinstance(c, DocIRTableHeaderNode)
        )
        cell = header.children[0].children[0]
        assert cell.is_header is True

    def test_header_cell_alignment(self):
        result = _map(_doc(_table_with_header_and_body()))
        header = next(
            c for c in result.children[0].children
            if isinstance(c, DocIRTableHeaderNode)
        )
        row = header.children[0]
        assert row.children[0].alignment == "left"
        assert row.children[1].alignment == "right"

    def test_header_cell_text(self):
        result = _map(_doc(_table_with_header_and_body()))
        header = next(
            c for c in result.children[0].children
            if isinstance(c, DocIRTableHeaderNode)
        )
        cell = header.children[0].children[0]
        assert cell.children[0].content == "Name"

    def test_body_cell_is_header_false(self):
        result = _map(_doc(_table_with_header_and_body()))
        body = next(
            c for c in result.children[0].children
            if isinstance(c, DocIRTableBodyNode)
        )
        cell = body.children[0].children[0]
        assert cell.is_header is False

    def test_body_cell_text(self):
        result = _map(_doc(_table_with_header_and_body()))
        body = next(
            c for c in result.children[0].children
            if isinstance(c, DocIRTableBodyNode)
        )
        cells = body.children[0].children
        assert cells[0].children[0].content == "foo"
        assert cells[1].children[0].content == "42"

    def test_table_cell_with_bold(self):
        table = MarkdownASTTableNode()
        header = MarkdownASTTableHeaderNode()
        row = MarkdownASTTableRowNode()
        cell = MarkdownASTTableCellNode(is_header=True, alignment="center")
        cell.add_child(_bold(_text("Bold Header")))
        row.add_child(cell)
        header.add_child(row)
        table.add_child(header)

        result = _map(_doc(table))
        ir_cell = result.children[0].children[0].children[0].children[0]
        span = ir_cell.children[0]
        assert isinstance(span, DocIRTextSpanNode)
        assert span.bold is True
        assert span.content == "Bold Header"


# ---------------------------------------------------------------------------
# Mixed document
# ---------------------------------------------------------------------------

class TestMixedDocument:
    def test_heading_then_paragraph(self):
        result = _map(_doc(
            _heading(1, _text("Title")),
            _para(_text("Body text")),
        ))
        assert isinstance(result.children[0], DocIRHeadingNode)
        assert isinstance(result.children[1], DocIRParagraphNode)

    def test_paragraph_then_code_block(self):
        from syntax import ProgrammingLanguage
        cb = MarkdownASTCodeBlockNode(
            language_name="python",
            content="x = 1",
            tokens_by_line=[],
            states_by_line=[],
            language=ProgrammingLanguage.UNKNOWN,
            total_lines=0,
        )
        result = _map(_doc(_para(_text("Intro")), cb))
        assert isinstance(result.children[0], DocIRParagraphNode)
        assert isinstance(result.children[1], DocIRCodeBlockNode)

    def test_all_block_types_present(self):
        from syntax import ProgrammingLanguage

        bq = MarkdownASTBlockquoteNode()
        bq.add_child(_para(_text("Quote")))

        ul = MarkdownASTUnorderedListNode()
        item = MarkdownASTListItemNode()
        item.add_child(_text("Item"))
        ul.add_child(item)

        cb = MarkdownASTCodeBlockNode(
            language_name="",
            content="code",
            tokens_by_line=[],
            states_by_line=[],
            language=ProgrammingLanguage.UNKNOWN,
            total_lines=0,
        )

        hr = MarkdownASTHorizontalRuleNode()

        result = _map(_doc(
            _heading(1, _text("H1")),
            _para(_text("Para")),
            bq,
            ul,
            cb,
            hr,
        ))

        assert len(result.children) == 6
        assert isinstance(result.children[0], DocIRHeadingNode)
        assert isinstance(result.children[1], DocIRParagraphNode)
        assert isinstance(result.children[2], DocIRBlockquoteNode)
        assert isinstance(result.children[3], DocIRUnorderedListNode)
        assert isinstance(result.children[4], DocIRCodeBlockNode)
        assert isinstance(result.children[5], DocIRHorizontalRuleNode)
