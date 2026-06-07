from dhtml.html_ast_builder import parse_html
from dhtml.html_ast_node import (
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTTextNode,
)
from dhtml.html_ast_to_document_ir import html_ast_to_document_ir
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


def _convert(html: str) -> DocumentIRDocumentNode:
    """Parse html and convert it to a document IR tree."""
    return html_ast_to_document_ir(parse_html(html))


def _doc(*children: HtmlASTElementNode | HtmlASTTextNode) -> HtmlASTDocumentNode:
    """Build an HtmlASTDocumentNode with the given children."""
    doc = HtmlASTDocumentNode()
    for child in children:
        doc.add_child(child)
    return doc


def _elem(tag: str, *children, **attrs: str) -> HtmlASTElementNode:
    """Build an HtmlASTElementNode with optional children and attributes."""
    node = HtmlASTElementNode(tag_name=tag, attributes=dict(attrs))
    for child in children:
        node.add_child(child)
    return node


def _text(content: str) -> HtmlASTTextNode:
    """Build an HtmlASTTextNode."""
    return HtmlASTTextNode(content=content)


def _p_with_text(text: str) -> HtmlASTElementNode:
    """Build a <p> element containing a single text node."""
    return _elem("p", _text(text))


class TestDocumentRoot:
    """html_ast_to_document_ir returns a DocumentIRDocumentNode."""

    def test_returns_document_ir_node(self) -> None:
        result = html_ast_to_document_ir(_doc())
        assert isinstance(result, DocumentIRDocumentNode)

    def test_source_path_preserved(self) -> None:
        doc = HtmlASTDocumentNode(source_path="/path/to/file.html")
        result = html_ast_to_document_ir(doc)
        assert result.source_path == "/path/to/file.html"

    def test_empty_document_has_no_children(self) -> None:
        result = html_ast_to_document_ir(_doc())
        assert result.children == []


class TestHeadings:
    """h1–h6 elements map to DocumentIRHeadingNode with correct level."""

    def test_h1_level(self) -> None:
        result = _convert("<h1>Title</h1>")
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 1

    def test_h2_level(self) -> None:
        result = _convert("<h2>Section</h2>")
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 2

    def test_h3_level(self) -> None:
        result = _convert("<h3>Sub</h3>")
        assert result.children[0].level == 3

    def test_h4_level(self) -> None:
        result = _convert("<h4>Sub-sub</h4>")
        assert result.children[0].level == 4

    def test_h5_level(self) -> None:
        result = _convert("<h5>Deep</h5>")
        assert result.children[0].level == 5

    def test_h6_level(self) -> None:
        result = _convert("<h6>Deepest</h6>")
        assert result.children[0].level == 6

    def test_heading_text_content(self) -> None:
        result = _convert("<h1>My Heading</h1>")
        h = result.children[0]
        span = h.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.content == "My Heading"


class TestParagraph:
    """p elements map to DocumentIRParagraphNode."""

    def test_paragraph_node(self) -> None:
        result = _convert("<p>Hello</p>")
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_paragraph_text(self) -> None:
        result = _convert("<p>Hello world</p>")
        span = result.children[0].children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.content == "Hello world"

    def test_multiple_paragraphs(self) -> None:
        result = _convert("<p>A</p><p>B</p>")
        assert len(result.children) == 2
        assert all(isinstance(c, DocumentIRParagraphNode) for c in result.children)


class TestBlockquote:
    """blockquote maps to DocumentIRBlockquoteNode, preserving children."""

    def test_blockquote_node(self) -> None:
        result = _convert("<blockquote><p>Quoted</p></blockquote>")
        assert isinstance(result.children[0], DocumentIRBlockquoteNode)

    def test_blockquote_contains_paragraph(self) -> None:
        result = _convert("<blockquote><p>Text</p></blockquote>")
        bq = result.children[0]
        assert isinstance(bq.children[0], DocumentIRParagraphNode)


class TestCodeBlock:
    """pre/code maps to DocumentIRCodeBlockNode with language and content."""

    def test_pre_code_with_language(self) -> None:
        result = _convert('<pre><code class="language-python">x = 1</code></pre>')
        cb = result.children[0]
        assert isinstance(cb, DocumentIRCodeBlockNode)
        assert cb.language == "python"

    def test_pre_code_content(self) -> None:
        result = _convert('<pre><code class="language-python">x = 1</code></pre>')
        assert result.children[0].content == "x = 1"

    def test_pre_without_code_has_empty_language(self) -> None:
        result = _convert("<pre>raw code</pre>")
        cb = result.children[0]
        assert isinstance(cb, DocumentIRCodeBlockNode)
        assert cb.language == ""

    def test_pre_without_code_has_content(self) -> None:
        result = _convert("<pre>raw code</pre>")
        assert result.children[0].content == "raw code"

    def test_leading_trailing_newline_stripped(self) -> None:
        result = _convert('<pre><code class="language-js">\nfunction f() {}\n</code></pre>')
        assert not result.children[0].content.startswith("\n")
        assert not result.children[0].content.endswith("\n")

    def test_language_class_without_prefix_ignored(self) -> None:
        result = _convert('<pre><code class="python">x = 1</code></pre>')
        cb = result.children[0]
        assert cb.language == ""


class TestLists:
    """ul/ol/li map to the appropriate list IR nodes."""

    def test_unordered_list_node(self) -> None:
        result = _convert("<ul><li>Item</li></ul>")
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)

    def test_ordered_list_node(self) -> None:
        result = _convert("<ol><li>Item</li></ol>")
        assert isinstance(result.children[0], DocumentIROrderedListNode)

    def test_ordered_list_default_start(self) -> None:
        result = _convert("<ol><li>Item</li></ol>")
        assert result.children[0].start == 1

    def test_ordered_list_custom_start(self) -> None:
        result = _convert('<ol start="3"><li>Item</li></ol>')
        assert result.children[0].start == 3

    def test_list_item_node(self) -> None:
        result = _convert("<ul><li>Item</li></ul>")
        ul = result.children[0]
        assert isinstance(ul.children[0], DocumentIRListItemNode)

    def test_multiple_list_items(self) -> None:
        result = _convert("<ul><li>A</li><li>B</li><li>C</li></ul>")
        ul = result.children[0]
        assert len(ul.children) == 3

    def test_list_item_text(self) -> None:
        result = _convert("<ul><li>Hello</li></ul>")
        item = result.children[0].children[0]
        span = item.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.content == "Hello"


class TestTable:
    """table/thead/tbody/tr/td/th map to the table IR hierarchy."""

    def test_table_node(self) -> None:
        result = _convert("<table><tr><td>Cell</td></tr></table>")
        assert isinstance(result.children[0], DocumentIRTableNode)

    def test_thead_maps_to_table_header(self) -> None:
        result = _convert(
            "<table><thead><tr><th>H</th></tr></thead>"
            "<tbody><tr><td>D</td></tr></tbody></table>"
        )
        table = result.children[0]
        headers = [c for c in table.children if isinstance(c, DocumentIRTableHeaderNode)]
        assert len(headers) == 1

    def test_tbody_maps_to_table_body(self) -> None:
        result = _convert(
            "<table><thead><tr><th>H</th></tr></thead>"
            "<tbody><tr><td>D</td></tr></tbody></table>"
        )
        table = result.children[0]
        bodies = [c for c in table.children if isinstance(c, DocumentIRTableBodyNode)]
        assert len(bodies) == 1

    def test_tr_maps_to_table_row(self) -> None:
        result = _convert("<table><tbody><tr><td>Cell</td></tr></tbody></table>")
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        assert isinstance(body.children[0], DocumentIRTableRowNode)

    def test_td_is_not_header_cell(self) -> None:
        result = _convert("<table><tr><td>D</td></tr></table>")
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        row = body.children[0]
        cell = row.children[0]
        assert isinstance(cell, DocumentIRTableCellNode)
        assert cell.is_header is False

    def test_th_is_header_cell(self) -> None:
        result = _convert("<table><tr><th>H</th></tr></table>")
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        row = body.children[0]
        cell = row.children[0]
        assert cell.is_header is True

    def test_thead_rows_have_header_cells(self) -> None:
        result = _convert("<table><thead><tr><th>H</th></tr></thead></table>")
        table = result.children[0]
        header = next(c for c in table.children if isinstance(c, DocumentIRTableHeaderNode))
        row = header.children[0]
        assert row.children[0].is_header is True

    def test_cell_alignment_from_attribute(self) -> None:
        result = _convert('<table><tr><td align="center">C</td></tr></table>')
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        cell = body.children[0].children[0]
        assert cell.alignment == "center"

    def test_cell_default_alignment_left(self) -> None:
        result = _convert("<table><tr><td>D</td></tr></table>")
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        cell = body.children[0].children[0]
        assert cell.alignment == "left"

    def test_colspan_expands_cells(self) -> None:
        """A th with colspan=3 produces 3 cells in the row."""
        result = _convert(
            "<table><thead><tr><th colspan=\"3\">Header</th></tr></thead></table>"
        )
        table = result.children[0]
        header = next(c for c in table.children if isinstance(c, DocumentIRTableHeaderNode))
        row = header.children[0]
        cells = [c for c in row.children if isinstance(c, DocumentIRTableCellNode)]
        assert len(cells) == 3

    def test_colspan_1_is_normal(self) -> None:
        """A td with colspan=1 produces exactly 1 cell in the row."""
        result = _convert(
            "<table><tr><td colspan=\"1\">Data</td></tr></table>"
        )
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        row = body.children[0]
        cells = [c for c in row.children if isinstance(c, DocumentIRTableCellNode)]
        assert len(cells) == 1

    def test_colspan_extra_cells_empty(self) -> None:
        """The 2nd and 3rd cells expanded from colspan=3 have no children."""
        result = _convert(
            "<table><thead><tr><th colspan=\"3\">Header</th></tr></thead></table>"
        )
        table = result.children[0]
        header = next(c for c in table.children if isinstance(c, DocumentIRTableHeaderNode))
        row = header.children[0]
        cells = [c for c in row.children if isinstance(c, DocumentIRTableCellNode)]
        assert cells[1].children == []
        assert cells[2].children == []


class TestHorizontalRule:
    """hr maps to DocumentIRHorizontalRuleNode."""

    def test_hr_node(self) -> None:
        result = _convert("<hr>")
        assert isinstance(result.children[0], DocumentIRHorizontalRuleNode)


class TestLineBreak:
    """br maps to DocumentIRLineBreakNode."""

    def test_br_node(self) -> None:
        result = _convert("<p>line1<br>line2</p>")
        p = result.children[0]
        breaks = [c for c in p.children if isinstance(c, DocumentIRLineBreakNode)]
        assert len(breaks) == 1


class TestImage:
    """img maps to DocumentIRImageNode with correct attributes."""

    def test_image_node(self) -> None:
        result = _convert('<img src="photo.jpg" alt="A photo">')
        assert isinstance(result.children[0], DocumentIRImageNode)

    def test_image_url(self) -> None:
        result = _convert('<img src="photo.jpg" alt="A photo">')
        assert result.children[0].url == "photo.jpg"

    def test_image_alt_text(self) -> None:
        result = _convert('<img src="photo.jpg" alt="A photo">')
        assert result.children[0].alt_text == "A photo"

    def test_image_title(self) -> None:
        result = _convert('<img src="u.png" alt="a" title="tooltip">')
        assert result.children[0].title == "tooltip"

    def test_image_no_title_is_none(self) -> None:
        result = _convert('<img src="u.png" alt="a">')
        assert result.children[0].title is None


class TestLink:
    """a maps to DocumentIRLinkNode with inline children."""

    def test_link_node(self) -> None:
        result = _convert('<a href="https://example.com">Click</a>')
        assert isinstance(result.children[0], DocumentIRLinkNode)

    def test_link_url(self) -> None:
        result = _convert('<a href="https://example.com">Click</a>')
        assert result.children[0].url == "https://example.com"

    def test_link_title(self) -> None:
        result = _convert('<a href="u" title="tip">Click</a>')
        assert result.children[0].title == "tip"

    def test_link_no_title_is_none(self) -> None:
        result = _convert('<a href="u">Click</a>')
        assert result.children[0].title is None

    def test_link_inline_children(self) -> None:
        result = _convert('<a href="u">text</a>')
        link = result.children[0]
        assert any(isinstance(c, DocumentIRTextSpanNode) for c in link.children)


class TestInlineFormatting:
    """Inline formatting tags produce DocumentIRTextSpanNode with correct flags."""

    def test_strong_bold(self) -> None:
        result = _convert("<p><strong>Bold</strong></p>")
        p = result.children[0]
        span = p.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.bold is True

    def test_b_bold(self) -> None:
        result = _convert("<p><b>Bold</b></p>")
        span = result.children[0].children[0]
        assert span.bold is True

    def test_em_italic(self) -> None:
        result = _convert("<p><em>Italic</em></p>")
        span = result.children[0].children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.italic is True

    def test_i_italic(self) -> None:
        result = _convert("<p><i>Italic</i></p>")
        span = result.children[0].children[0]
        assert span.italic is True

    def test_s_strikethrough(self) -> None:
        result = _convert("<p><s>Strike</s></p>")
        span = result.children[0].children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.strikethrough is True

    def test_del_strikethrough(self) -> None:
        result = _convert("<p><del>Strike</del></p>")
        span = result.children[0].children[0]
        assert span.strikethrough is True

    def test_strike_strikethrough(self) -> None:
        result = _convert("<p><strike>Strike</strike></p>")
        span = result.children[0].children[0]
        assert span.strikethrough is True

    def test_inline_code_outside_pre(self) -> None:
        result = _convert("<p><code>func()</code></p>")
        p = result.children[0]
        span = p.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.code is True

    def test_kbd_code(self) -> None:
        result = _convert("<p><kbd>Ctrl+C</kbd></p>")
        span = result.children[0].children[0]
        assert span.code is True


class TestSkippedElements:
    """script, style, head, nav, footer, and aside are skipped entirely."""

    def test_script_skipped(self) -> None:
        result = _convert("<script>alert('x')</script><p>Visible</p>")
        assert len(result.children) == 1
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_style_skipped(self) -> None:
        result = _convert("<style>.cls {}</style><p>Visible</p>")
        assert len(result.children) == 1

    def test_head_skipped(self) -> None:
        result = _convert("<head><title>T</title></head><p>Body</p>")
        paras = [c for c in result.children if isinstance(c, DocumentIRParagraphNode)]
        assert len(paras) == 1

    def test_nav_skipped(self) -> None:
        result = _convert("<nav><a href='/'>Home</a></nav><p>Content</p>")
        assert len(result.children) == 1
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_footer_skipped(self) -> None:
        result = _convert("<footer>Footer text</footer><p>Main</p>")
        assert len(result.children) == 1

    def test_aside_skipped(self) -> None:
        result = _convert("<aside>Sidebar</aside><p>Main</p>")
        assert len(result.children) == 1


class TestTransparentContainers:
    """div, section, main, article, span inline their children."""

    def test_div_transparent(self) -> None:
        result = _convert("<div><p>Text</p></div>")
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_section_transparent(self) -> None:
        result = _convert("<section><p>Text</p></section>")
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_main_transparent(self) -> None:
        result = _convert("<main><p>Text</p></main>")
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_article_transparent(self) -> None:
        result = _convert("<article><p>Text</p></article>")
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_span_transparent(self) -> None:
        result = _convert("<p><span>text</span></p>")
        p = result.children[0]
        spans = [c for c in p.children if isinstance(c, DocumentIRTextSpanNode)]
        assert any(s.content == "text" for s in spans)


class TestCommentsIgnored:
    """HTML comments produce no IR nodes."""

    def test_comment_ignored(self) -> None:
        result = _convert("<!-- a comment --><p>Visible</p>")
        assert len(result.children) == 1
        assert isinstance(result.children[0], DocumentIRParagraphNode)


class TestDefinitionList:
    """dl/dt/dd elements map to the definition list IR hierarchy."""

    def test_dl_produces_definition_list_node(self) -> None:
        from document_ir import DocumentIRDefinitionListNode
        result = _convert("<dl><dt>Term</dt><dd>Desc</dd></dl>")
        assert isinstance(result.children[0], DocumentIRDefinitionListNode)

    def test_dt_produces_definition_term_node(self) -> None:
        from document_ir import DocumentIRDefinitionTermNode
        result = _convert("<dl><dt>Term</dt></dl>")
        dl = result.children[0]
        assert isinstance(dl.children[0], DocumentIRDefinitionTermNode)

    def test_dd_produces_definition_description_node(self) -> None:
        from document_ir import DocumentIRDefinitionDescriptionNode
        result = _convert("<dl><dt>Term</dt><dd>Desc</dd></dl>")
        dl = result.children[0]
        assert isinstance(dl.children[1], DocumentIRDefinitionDescriptionNode)

    def test_definition_list_structure(self) -> None:
        from document_ir import (
            DocumentIRDefinitionListNode,
            DocumentIRDefinitionTermNode,
            DocumentIRDefinitionDescriptionNode,
        )
        result = _convert("<dl><dt>Term</dt><dd>Desc</dd></dl>")
        dl = result.children[0]
        assert isinstance(dl, DocumentIRDefinitionListNode)
        assert isinstance(dl.children[0], DocumentIRDefinitionTermNode)
        assert isinstance(dl.children[1], DocumentIRDefinitionDescriptionNode)

    def test_dt_text_content(self) -> None:
        result = _convert("<dl><dt>My Term</dt></dl>")
        dl = result.children[0]
        dt = dl.children[0]
        span = dt.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.content == "My Term"

    def test_dd_text_content(self) -> None:
        result = _convert("<dl><dt>Term</dt><dd>My Description</dd></dl>")
        dl = result.children[0]
        dd = dl.children[1]
        span = dd.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.content == "My Description"


class TestSuperscriptSubscript:
    """sup/sub elements map to DocumentIRTextSpanNode with correct flags."""

    def test_sup_produces_superscript_span(self) -> None:
        result = _convert("<p><sup>2</sup></p>")
        p = result.children[0]
        span = p.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.superscript is True

    def test_sub_produces_subscript_span(self) -> None:
        result = _convert("<p><sub>2</sub></p>")
        p = result.children[0]
        span = p.children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.subscript is True
