from dhtml.document_ir_to_html import document_ir_to_html
from document_ir import (
    DocumentIRBlockquoteNode,
    DocumentIRCodeBlockNode,
    DocumentIRDocumentNode,
    DocumentIRHeadingNode,
    DocumentIRHorizontalRuleNode,
    DocumentIRImageNode,
    DocumentIRLineBreakNode,
    DocumentIRLinkNode,
    DocumentIRListItemNode,
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


def _doc(*children) -> DocumentIRDocumentNode:
    """Build a DocumentIRDocumentNode with the given children."""
    doc = DocumentIRDocumentNode()
    for child in children:
        doc.add_child(child)
    return doc


def _span(content: str, **flags) -> DocumentIRTextSpanNode:
    """Build a DocumentIRTextSpanNode."""
    return DocumentIRTextSpanNode(content=content, **flags)


def _para(*children) -> DocumentIRParagraphNode:
    """Build a DocumentIRParagraphNode with the given inline children."""
    p = DocumentIRParagraphNode()
    for child in children:
        p.add_child(child)
    return p


def _heading(level: int, text: str) -> DocumentIRHeadingNode:
    """Build a DocumentIRHeadingNode with a single text span."""
    h = DocumentIRHeadingNode(level=level)
    h.add_child(_span(text))
    return h


def _render(doc: DocumentIRDocumentNode) -> str:
    return document_ir_to_html(doc)


class TestDocumentStructure:
    """The output is a complete HTML5 document with the required boilerplate."""

    def test_doctype_present(self) -> None:
        html = _render(_doc())
        assert html.startswith("<!DOCTYPE html>")

    def test_html_tag_present(self) -> None:
        html = _render(_doc())
        assert "<html>" in html
        assert "</html>" in html

    def test_head_tag_present(self) -> None:
        html = _render(_doc())
        assert "<head>" in html
        assert "</head>" in html

    def test_body_tag_present(self) -> None:
        html = _render(_doc())
        assert "<body>" in html
        assert "</body>" in html

    def test_charset_meta_present(self) -> None:
        html = _render(_doc())
        assert '<meta charset="utf-8">' in html

    def test_title_from_first_h1(self) -> None:
        html = _render(_doc(_heading(1, "My Title")))
        assert "<title>My Title</title>" in html

    def test_title_fallback_to_document(self) -> None:
        html = _render(_doc(_para(_span("No heading here"))))
        assert "<title>Document</title>" in html

    def test_title_not_taken_from_h2(self) -> None:
        html = _render(_doc(_heading(2, "Section")))
        assert "<title>Document</title>" in html

    def test_head_comes_before_body(self) -> None:
        html = _render(_doc())
        assert html.index("<head>") < html.index("<body>")


class TestHeadings:
    """Heading nodes are serialised with the correct tag level."""

    def test_h1(self) -> None:
        html = _render(_doc(_heading(1, "Title")))
        assert "<h1>Title</h1>" in html

    def test_h2(self) -> None:
        html = _render(_doc(_heading(2, "Section")))
        assert "<h2>Section</h2>" in html

    def test_h3(self) -> None:
        html = _render(_doc(_heading(3, "Sub")))
        assert "<h3>Sub</h3>" in html

    def test_h4(self) -> None:
        html = _render(_doc(_heading(4, "Sub-sub")))
        assert "<h4>Sub-sub</h4>" in html

    def test_h5(self) -> None:
        html = _render(_doc(_heading(5, "Deep")))
        assert "<h5>Deep</h5>" in html

    def test_h6(self) -> None:
        html = _render(_doc(_heading(6, "Deepest")))
        assert "<h6>Deepest</h6>" in html


class TestParagraph:
    """Paragraph nodes are wrapped in <p> tags."""

    def test_paragraph_tag(self) -> None:
        html = _render(_doc(_para(_span("Hello"))))
        assert "<p>Hello</p>" in html

    def test_paragraph_text_escaped(self) -> None:
        html = _render(_doc(_para(_span("<b>not bold</b>"))))
        assert "&lt;b&gt;" in html
        assert "<p>" in html


class TestBlockquote:
    """Blockquote nodes produce <blockquote> with indented children."""

    def test_blockquote_tag(self) -> None:
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quote")))
        html = _render(_doc(bq))
        assert "<blockquote>" in html
        assert "</blockquote>" in html

    def test_blockquote_contains_paragraph(self) -> None:
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quoted text")))
        html = _render(_doc(bq))
        assert "Quoted text" in html


class TestCodeBlock:
    """Code block nodes produce <pre><code> with optional language class."""

    def test_code_block_with_language(self) -> None:
        cb = DocumentIRCodeBlockNode(language="python", content="x = 1")
        html = _render(_doc(cb))
        assert '<pre><code class="language-python">x = 1</code></pre>' in html

    def test_code_block_without_language(self) -> None:
        cb = DocumentIRCodeBlockNode(language="", content="x = 1")
        html = _render(_doc(cb))
        assert "<pre><code>x = 1</code></pre>" in html

    def test_code_content_escaped(self) -> None:
        cb = DocumentIRCodeBlockNode(language="html", content="<div>")
        html = _render(_doc(cb))
        assert "&lt;div&gt;" in html


class TestLists:
    """List nodes produce ul/ol tags with li children."""

    def test_unordered_list_tag(self) -> None:
        ul = DocumentIRUnorderedListNode()
        item = DocumentIRListItemNode()
        item.add_child(_span("Item"))
        ul.add_child(item)
        html = _render(_doc(ul))
        assert "<ul>" in html
        assert "</ul>" in html

    def test_ordered_list_tag(self) -> None:
        ol = DocumentIROrderedListNode(start=1)
        item = DocumentIRListItemNode()
        item.add_child(_span("Item"))
        ol.add_child(item)
        html = _render(_doc(ol))
        assert "<ol>" in html
        assert "</ol>" in html

    def test_ordered_list_no_start_attr_when_start_is_one(self) -> None:
        ol = DocumentIROrderedListNode(start=1)
        ol.add_child(DocumentIRListItemNode())
        html = _render(_doc(ol))
        assert 'start="1"' not in html

    def test_ordered_list_start_attr_when_not_one(self) -> None:
        ol = DocumentIROrderedListNode(start=3)
        ol.add_child(DocumentIRListItemNode())
        html = _render(_doc(ol))
        assert 'start="3"' in html

    def test_list_item_inline_children(self) -> None:
        ul = DocumentIRUnorderedListNode()
        item = DocumentIRListItemNode()
        item.add_child(_span("Hello"))
        ul.add_child(item)
        html = _render(_doc(ul))
        assert "<li>Hello</li>" in html

    def test_list_item_block_children(self) -> None:
        ul = DocumentIRUnorderedListNode()
        item = DocumentIRListItemNode()
        item.add_child(_para(_span("Paragraph")))
        ul.add_child(item)
        html = _render(_doc(ul))
        assert "<li>" in html
        assert "</li>" in html
        assert "Paragraph" in html


class TestTable:
    """Table nodes produce the correct table HTML structure."""

    def _simple_table(self, header: bool = True) -> DocumentIRTableNode:
        table = DocumentIRTableNode()

        if header:
            thead = DocumentIRTableHeaderNode()
            hrow = DocumentIRTableRowNode()
            hcell = DocumentIRTableCellNode(is_header=True, alignment="left")
            hcell.add_child(_span("Header"))
            hrow.add_child(hcell)
            thead.add_child(hrow)
            table.add_child(thead)

        tbody = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_span("Data"))
        row.add_child(cell)
        tbody.add_child(row)
        table.add_child(tbody)

        return table

    def test_table_tag(self) -> None:
        html = _render(_doc(self._simple_table()))
        assert "<table>" in html
        assert "</table>" in html

    def test_thead_tag(self) -> None:
        html = _render(_doc(self._simple_table(header=True)))
        assert "<thead>" in html
        assert "</thead>" in html

    def test_tbody_tag(self) -> None:
        html = _render(_doc(self._simple_table()))
        assert "<tbody>" in html
        assert "</tbody>" in html

    def test_tr_tag(self) -> None:
        html = _render(_doc(self._simple_table()))
        assert "<tr>" in html
        assert "</tr>" in html

    def test_th_for_header_cell(self) -> None:
        html = _render(_doc(self._simple_table(header=True)))
        assert "<th>" in html or "<th " in html

    def test_td_for_data_cell(self) -> None:
        html = _render(_doc(self._simple_table()))
        assert "<td>" in html or "<td " in html

    def test_cell_alignment_center(self) -> None:
        table = DocumentIRTableNode()
        tbody = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="center")
        cell.add_child(_span("Centered"))
        row.add_child(cell)
        tbody.add_child(row)
        table.add_child(tbody)
        html = _render(_doc(table))
        assert 'align="center"' in html

    def test_cell_alignment_left_no_attr(self) -> None:
        table = DocumentIRTableNode()
        tbody = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_span("Left"))
        row.add_child(cell)
        tbody.add_child(row)
        table.add_child(tbody)
        html = _render(_doc(table))
        assert 'align="left"' not in html

    def test_cell_alignment_right(self) -> None:
        table = DocumentIRTableNode()
        tbody = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="right")
        cell.add_child(_span("Right"))
        row.add_child(cell)
        tbody.add_child(row)
        table.add_child(tbody)
        html = _render(_doc(table))
        assert 'align="right"' in html


class TestHorizontalRule:
    """Horizontal rule nodes produce <hr>."""

    def test_hr_tag(self) -> None:
        html = _render(_doc(DocumentIRHorizontalRuleNode()))
        assert "<hr>" in html


class TestTextSpans:
    """Text span nodes are wrapped in the appropriate inline tags."""

    def test_plain_text(self) -> None:
        html = _render(_doc(_para(_span("Hello"))))
        assert "Hello" in html

    def test_bold_text(self) -> None:
        html = _render(_doc(_para(_span("Bold", bold=True))))
        assert "<strong>Bold</strong>" in html

    def test_italic_text(self) -> None:
        html = _render(_doc(_para(_span("Italic", italic=True))))
        assert "<em>Italic</em>" in html

    def test_bold_and_italic(self) -> None:
        html = _render(_doc(_para(_span("Both", bold=True, italic=True))))
        assert "<strong><em>Both</em></strong>" in html

    def test_strikethrough(self) -> None:
        html = _render(_doc(_para(_span("Strike", strikethrough=True))))
        assert "<del>Strike</del>" in html

    def test_inline_code(self) -> None:
        html = _render(_doc(_para(_span("code()", code=True))))
        assert "<code>code()</code>" in html

    def test_text_escaped_lt(self) -> None:
        html = _render(_doc(_para(_span("a < b"))))
        assert "&lt;" in html

    def test_text_escaped_gt(self) -> None:
        html = _render(_doc(_para(_span("a > b"))))
        assert "&gt;" in html

    def test_text_escaped_amp(self) -> None:
        html = _render(_doc(_para(_span("a & b"))))
        assert "&amp;" in html

    def test_text_escaped_quot(self) -> None:
        html = _render(_doc(_para(_span('say "hi"'))))
        assert "&quot;" in html or "&#x27;" in html or "hi" in html


class TestLink:
    """Link nodes produce <a> with href and optional title."""

    def test_link_href(self) -> None:
        link = DocumentIRLinkNode(url="https://example.com")
        link.add_child(_span("Click"))
        html = _render(_doc(_para(link)))
        assert 'href="https://example.com"' in html

    def test_link_title_present(self) -> None:
        link = DocumentIRLinkNode(url="u", title="tooltip")
        link.add_child(_span("Click"))
        html = _render(_doc(_para(link)))
        assert 'title="tooltip"' in html

    def test_link_no_title_attr_when_none(self) -> None:
        link = DocumentIRLinkNode(url="u", title=None)
        link.add_child(_span("Click"))
        html = _render(_doc(_para(link)))
        assert "title=" not in html

    def test_link_text_content(self) -> None:
        link = DocumentIRLinkNode(url="u")
        link.add_child(_span("Link text"))
        html = _render(_doc(_para(link)))
        assert "Link text" in html

    def test_link_url_escaped(self) -> None:
        link = DocumentIRLinkNode(url="a&b")
        link.add_child(_span("Click"))
        html = _render(_doc(_para(link)))
        assert "a&amp;b" in html


class TestImage:
    """Image nodes produce <img> with src, alt, and optional title."""

    def test_img_src(self) -> None:
        img = DocumentIRImageNode(url="photo.jpg", alt_text="A photo")
        html = _render(_doc(_para(img)))
        assert 'src="photo.jpg"' in html

    def test_img_alt(self) -> None:
        img = DocumentIRImageNode(url="photo.jpg", alt_text="A photo")
        html = _render(_doc(_para(img)))
        assert 'alt="A photo"' in html

    def test_img_title_present(self) -> None:
        img = DocumentIRImageNode(url="u.png", alt_text="a", title="tip")
        html = _render(_doc(_para(img)))
        assert 'title="tip"' in html

    def test_img_no_title_attr_when_none(self) -> None:
        img = DocumentIRImageNode(url="u.png", alt_text="a", title=None)
        html = _render(_doc(_para(img)))
        assert "title=" not in html

    def test_img_alt_escaped(self) -> None:
        img = DocumentIRImageNode(url="u.png", alt_text="<special>")
        html = _render(_doc(_para(img)))
        assert "&lt;special&gt;" in html


class TestLineBreak:
    """Line break nodes produce <br>."""

    def test_br_tag(self) -> None:
        p = _para(_span("before"), DocumentIRLineBreakNode(), _span("after"))
        html = _render(_doc(p))
        assert "<br>" in html


class TestAttributeEscaping:
    """Special characters in attribute values are properly escaped."""

    def test_href_special_chars(self) -> None:
        link = DocumentIRLinkNode(url='search?q=a&b="test"')
        link.add_child(_span("Go"))
        html = _render(_doc(_para(link)))
        assert "&amp;" in html or "q=a" in html

    def test_title_special_chars(self) -> None:
        link = DocumentIRLinkNode(url="u", title='<Say "hi">')
        link.add_child(_span("Go"))
        html = _render(_doc(_para(link)))
        assert "&lt;" in html or "Say" in html


class TestDefinitionList:
    """Definition list nodes produce <dl>, <dt>, and <dd> tags."""

    def _make_dl(self) -> "DocumentIRDefinitionListNode":
        from document_ir import (
            DocumentIRDefinitionListNode,
            DocumentIRDefinitionTermNode,
            DocumentIRDefinitionDescriptionNode,
        )
        dl = DocumentIRDefinitionListNode()
        dt = DocumentIRDefinitionTermNode()
        dt.add_child(_span("Term"))
        dl.add_child(dt)
        dd = DocumentIRDefinitionDescriptionNode()
        dd.add_child(_span("Desc"))
        dl.add_child(dd)
        return dl

    def test_dl_tag(self) -> None:
        html = _render(_doc(self._make_dl()))
        assert "<dl>" in html
        assert "</dl>" in html

    def test_dt_tag(self) -> None:
        html = _render(_doc(self._make_dl()))
        assert "<dt>Term</dt>" in html

    def test_dd_tag(self) -> None:
        html = _render(_doc(self._make_dl()))
        assert "<dd>Desc</dd>" in html


class TestSuperscriptSubscript:
    """Superscript and subscript spans produce <sup> and <sub> tags."""

    def test_superscript_span(self) -> None:
        html = _render(_doc(_para(_span("2", superscript=True))))
        assert "<sup>2</sup>" in html

    def test_subscript_span(self) -> None:
        html = _render(_doc(_para(_span("2", subscript=True))))
        assert "<sub>2</sub>" in html
