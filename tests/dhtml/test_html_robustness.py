from dhtml.html_ast_builder import parse_html
from dhtml.html_ast_node import (
    HtmlASTCommentNode,
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTTextNode,
)
from dhtml.html_ast_to_document_ir import html_ast_to_document_ir
from document_ir import (
    DocumentIRCodeBlockNode,
    DocumentIRHeadingNode,
    DocumentIROrderedListNode,
    DocumentIRParagraphNode,
    DocumentIRTableBodyNode,
    DocumentIRTableCellNode,
    DocumentIRTableHeaderNode,
    DocumentIRTableNode,
    DocumentIRTextSpanNode,
    DocumentIRUnorderedListNode,
)


def _elems(node: HtmlASTDocumentNode | HtmlASTElementNode) -> list[HtmlASTElementNode]:
    return [c for c in node.children if isinstance(c, HtmlASTElementNode)]


def _texts(node: HtmlASTDocumentNode | HtmlASTElementNode) -> list[HtmlASTTextNode]:
    return [c for c in node.children if isinstance(c, HtmlASTTextNode)]


def _convert(html: str):
    return html_ast_to_document_ir(parse_html(html))


class TestUnclosedTags:
    """Unclosed tags at end of input are silently accepted; content is still reachable."""

    def test_unclosed_p_at_eof(self) -> None:
        doc = parse_html("<p>Hello")
        p = _elems(doc)[0]
        assert p.tag_name == "p"
        assert _texts(p)[0].content == "Hello"

    def test_unclosed_div_containing_p(self) -> None:
        doc = parse_html("<div><p>Text</p>")
        div = _elems(doc)[0]
        assert div.tag_name == "div"
        p = _elems(div)[0]
        assert p.tag_name == "p"

    def test_deeply_unclosed_nesting(self) -> None:
        doc = parse_html("<ul><li><span>item")
        ul = _elems(doc)[0]
        li = _elems(ul)[0]
        span = _elems(li)[0]
        assert span.tag_name == "span"
        assert _texts(span)[0].content == "item"

    def test_unclosed_produces_no_exception(self) -> None:
        parse_html("<div><p><strong>no close tags anywhere")


class TestEmptyAndMinimalInput:
    """Edge cases around empty or near-empty input."""

    def test_empty_string(self) -> None:
        doc = parse_html("")
        assert doc.children == []

    def test_whitespace_only(self) -> None:
        doc = parse_html("   \n\t  ")
        assert doc.children == []

    def test_empty_element(self) -> None:
        doc = parse_html("<p></p>")
        p = _elems(doc)[0]
        assert p.tag_name == "p"
        assert p.children == []

    def test_doctype_only(self) -> None:
        doc = parse_html("<!DOCTYPE html>")
        assert doc.has_doctype is True
        assert doc.children == []

    def test_comment_only(self) -> None:
        doc = parse_html("<!-- nothing here -->")
        comments = [c for c in doc.children if isinstance(c, HtmlASTCommentNode)]
        assert len(comments) == 1
        assert doc.children == comments


class TestMalformedTags:
    """Malformed tag syntax is handled gracefully without raising exceptions."""

    def test_tag_with_no_close_angle(self) -> None:
        doc = parse_html("<p")
        assert isinstance(doc, HtmlASTDocumentNode)

    def test_close_tag_with_no_close_angle(self) -> None:
        doc = parse_html("<p>text</p")
        assert isinstance(doc, HtmlASTDocumentNode)

    def test_invalid_tag_name_starting_with_digit(self) -> None:
        doc = parse_html("<123>text</123>")
        texts = [c for c in doc.children if isinstance(c, HtmlASTTextNode)]
        combined = "".join(t.content for t in texts)
        assert "text" in combined

    def test_empty_close_tag(self) -> None:
        doc = parse_html("<p>text</>more</p>")
        assert isinstance(doc, HtmlASTDocumentNode)
        p = _elems(doc)[0]
        assert p.tag_name == "p"

    def test_close_tag_with_no_name(self) -> None:
        doc = parse_html("<p>text</  >more</p>")
        assert isinstance(doc, HtmlASTDocumentNode)

    def test_stray_angle_bracket_in_text(self) -> None:
        doc = parse_html("<p>1 < 2 and 3 > 2</p>")
        p = _elems(doc)[0]
        combined = "".join(t.content for t in _texts(p))
        assert "1 " in combined

    def test_attribute_with_equals_but_no_value(self) -> None:
        doc = parse_html("<div class=>text</div>")
        div = _elems(doc)[0]
        assert "class" in div.attributes

    def test_attribute_value_with_no_closing_quote(self) -> None:
        doc = parse_html('<div class="unclosed>text</div>')
        assert isinstance(doc, HtmlASTDocumentNode)

    def test_attribute_with_extra_equals(self) -> None:
        doc = parse_html('<div class=="foo">text</div>')
        assert isinstance(doc, HtmlASTDocumentNode)


class TestMisnestingAndStackRecovery:
    """Misnested and overlapping tags are recovered from correctly."""

    def test_overlapping_inline_tags(self) -> None:
        doc = parse_html("<p><b>bold <i>both</b> italic</i></p>")
        assert isinstance(doc, HtmlASTDocumentNode)
        p = _elems(doc)[0]
        assert p.tag_name == "p"

    def test_close_tag_with_no_matching_open(self) -> None:
        doc = parse_html("<p>text</span></p>")
        p = _elems(doc)[0]
        assert p.tag_name == "p"
        assert _texts(p)[0].content == "text"

    def test_multiple_unmatched_close_tags(self) -> None:
        doc = parse_html("</div></span></p><p>real</p>")
        ps = [c for c in doc.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "p"]
        assert len(ps) == 1
        assert _texts(ps[0])[0].content == "real"

    def test_deeply_misnested(self) -> None:
        html = "<div><table><tr><td><p>cell</div>"
        doc = parse_html(html)
        assert isinstance(doc, HtmlASTDocumentNode)


class TestImpliedCloseEdgeCases:
    """Implied-close rules work correctly in realistic real-world patterns."""

    def test_p_implied_close_chain(self) -> None:
        doc = parse_html("<p>one<p>two<p>three")
        ps = [c for c in doc.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "p"]
        assert len(ps) == 3
        assert _texts(ps[0])[0].content == "one"
        assert _texts(ps[1])[0].content == "two"
        assert _texts(ps[2])[0].content == "three"

    def test_li_implied_close_preserves_content(self) -> None:
        doc = parse_html("<ul><li>a<li>b<li>c</ul>")
        ul = _elems(doc)[0]
        lis = _elems(ul)
        assert len(lis) == 3
        assert _texts(lis[0])[0].content == "a"
        assert _texts(lis[1])[0].content == "b"
        assert _texts(lis[2])[0].content == "c"

    def test_td_implied_close_in_row(self) -> None:
        doc = parse_html("<table><tr><td>A<td>B<td>C</tr></table>")
        table = _elems(doc)[0]
        tr = _elems(table)[0]
        tds = [c for c in tr.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "td"]
        assert len(tds) == 3

    def test_dt_dd_alternation(self) -> None:
        doc = parse_html("<dl><dt>Term1<dd>Def1<dt>Term2<dd>Def2</dl>")
        dl = _elems(doc)[0]
        tags = [c.tag_name for c in dl.children if isinstance(c, HtmlASTElementNode)]
        assert tags == ["dt", "dd", "dt", "dd"]

    def test_optgroup_closes_option(self) -> None:
        doc = parse_html("<select><option>A<optgroup label='G'><option>B</optgroup></select>")
        select = _elems(doc)[0]
        children = [c for c in select.children if isinstance(c, HtmlASTElementNode)]
        assert children[0].tag_name == "option"
        assert children[1].tag_name == "optgroup"

    def test_tfoot_after_tbody(self) -> None:
        doc = parse_html(
            "<table>"
            "<tbody><tr><td>body</td></tr></tbody>"
            "<tfoot><tr><td>foot</td></tr></tfoot>"
            "</table>"
        )
        table = _elems(doc)[0]
        section_tags = [c.tag_name for c in table.children if isinstance(c, HtmlASTElementNode)]
        assert "tbody" in section_tags
        assert "tfoot" in section_tags


class TestScriptAndStyleEdgeCases:
    """script and style raw-text handling is robust to tricky content."""

    def test_script_with_html_like_content(self) -> None:
        doc = parse_html("<script>var x = '<div>not html</div>';</script>")
        script = _elems(doc)[0]
        assert script.tag_name == "script"
        assert _elems(script) == []

    def test_script_without_close_tag(self) -> None:
        doc = parse_html("<script>var x = 1;")
        assert isinstance(doc, HtmlASTDocumentNode)
        script = _elems(doc)[0]
        assert script.tag_name == "script"

    def test_style_with_angle_brackets(self) -> None:
        doc = parse_html("<style>a > b, c < d { color: red; }</style>")
        style = _elems(doc)[0]
        assert style.tag_name == "style"
        assert _elems(style) == []

    def test_script_uppercase_close_tag(self) -> None:
        doc = parse_html("<script>var x = 1;</SCRIPT><p>visible</p>")
        assert isinstance(doc, HtmlASTDocumentNode)
        elems = _elems(doc)
        assert any(e.tag_name == "p" for e in elems)


class TestEntityEdgeCases:
    """Entity decoding handles boundary conditions correctly."""

    def test_unknown_entity_passed_through(self) -> None:
        doc = parse_html("<p>&nosuchentity;</p>")
        p = _elems(doc)[0]
        text = _texts(p)[0].content
        assert "nosuchentity" in text

    def test_bare_ampersand_in_text(self) -> None:
        doc = parse_html("<p>a & b</p>")
        p = _elems(doc)[0]
        text = _texts(p)[0].content
        assert "&" in text

    def test_numeric_entity_zero(self) -> None:
        doc = parse_html("<p>&#0;</p>")
        assert isinstance(doc, HtmlASTDocumentNode)

    def test_large_numeric_entity(self) -> None:
        doc = parse_html("<p>&#128512;</p>")
        p = _elems(doc)[0]
        text = _texts(p)[0].content
        assert "\U0001f600" in text

    def test_hex_entity_uppercase(self) -> None:
        doc = parse_html("<p>&#X41;</p>")
        p = _elems(doc)[0]
        text = _texts(p)[0].content
        assert "A" in text

    def test_entity_at_start_of_text(self) -> None:
        doc = parse_html("<p>&lt;start</p>")
        p = _elems(doc)[0]
        text = _texts(p)[0].content
        assert text.startswith("<")

    def test_entity_at_end_of_text(self) -> None:
        doc = parse_html("<p>end&gt;</p>")
        p = _elems(doc)[0]
        text = _texts(p)[0].content
        assert text.endswith(">")

    def test_consecutive_entities(self) -> None:
        doc = parse_html("<p>&lt;&gt;&amp;</p>")
        p = _elems(doc)[0]
        text = _texts(p)[0].content
        assert "<" in text
        assert ">" in text
        assert "&" in text


class TestRealWorldHtmlPatterns:
    """Patterns commonly found in real-world HTML, tested via the IR converter."""

    def test_table_with_no_thead(self) -> None:
        html = "<table><tr><td>R1C1</td><td>R1C2</td></tr><tr><td>R2C1</td></tr></table>"
        ir = _convert(html)
        table = ir.children[0]
        assert isinstance(table, DocumentIRTableNode)
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        assert len(body.children) == 2

    def test_table_with_tfoot_goes_to_body(self) -> None:
        html = (
            "<table>"
            "<thead><tr><th>H</th></tr></thead>"
            "<tfoot><tr><td>F</td></tr></tfoot>"
            "<tbody><tr><td>B</td></tr></tbody>"
            "</table>"
        )
        ir = _convert(html)
        table = ir.children[0]
        assert isinstance(table, DocumentIRTableNode)
        headers = [c for c in table.children if isinstance(c, DocumentIRTableHeaderNode)]
        bodies = [c for c in table.children if isinstance(c, DocumentIRTableBodyNode)]
        assert len(headers) == 1
        assert len(bodies) == 1

    def test_table_rows_directly_in_table(self) -> None:
        html = "<table><tr><td>A</td></tr><tr><td>B</td></tr></table>"
        ir = _convert(html)
        table = ir.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        assert len(body.children) == 2

    def test_inline_elements_within_heading(self) -> None:
        html = "<h2>A <strong>bold</strong> heading</h2>"
        ir = _convert(html)
        h = ir.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 2
        spans = [c for c in h.children if isinstance(c, DocumentIRTextSpanNode)]
        bold_spans = [s for s in spans if s.bold]
        assert any(s.content == "bold" for s in bold_spans)

    def test_paragraph_with_mixed_inline(self) -> None:
        html = "<p>Normal <b>bold</b> and <i>italic</i> text.</p>"
        ir = _convert(html)
        p = ir.children[0]
        assert isinstance(p, DocumentIRParagraphNode)
        spans = [c for c in p.children if isinstance(c, DocumentIRTextSpanNode)]
        assert any(s.bold for s in spans)
        assert any(s.italic for s in spans)

    def test_pre_code_with_multiple_language_classes(self) -> None:
        html = '<pre><code class="language-python highlight">x = 1</code></pre>'
        ir = _convert(html)
        cb = ir.children[0]
        assert isinstance(cb, DocumentIRCodeBlockNode)
        assert cb.language == "python"

    def test_pre_code_content_preserves_internal_newlines(self) -> None:
        html = "<pre><code>line1\nline2\nline3</code></pre>"
        ir = _convert(html)
        cb = ir.children[0]
        assert "line1\nline2\nline3" == cb.content


class TestOrderedListEdgeCases:
    """ol start attribute is parsed robustly."""

    def test_valid_start_attribute(self) -> None:
        ir = _convert('<ol start="5"><li>Item</li></ol>')
        ol = ir.children[0]
        assert isinstance(ol, DocumentIROrderedListNode)
        assert ol.start == 5

    def test_invalid_start_attribute_defaults_to_one(self) -> None:
        ir = _convert('<ol start="abc"><li>Item</li></ol>')
        ol = ir.children[0]
        assert isinstance(ol, DocumentIROrderedListNode)
        assert ol.start == 1

    def test_empty_start_attribute_defaults_to_one(self) -> None:
        ir = _convert('<ol start=""><li>Item</li></ol>')
        ol = ir.children[0]
        assert ol.start == 1

    def test_float_start_attribute_defaults_to_one(self) -> None:
        ir = _convert('<ol start="1.5"><li>Item</li></ol>')
        ol = ir.children[0]
        assert ol.start == 1

    def test_negative_start_attribute(self) -> None:
        ir = _convert('<ol start="-3"><li>Item</li></ol>')
        ol = ir.children[0]
        assert ol.start == -3

    def test_missing_start_attribute_defaults_to_one(self) -> None:
        ir = _convert("<ol><li>Item</li></ol>")
        ol = ir.children[0]
        assert ol.start == 1


class TestTableCellAlignmentEdgeCases:
    """td/th alignment attribute is normalised robustly."""

    def test_bogus_alignment_defaults_to_left(self) -> None:
        ir = _convert('<table><tr><td align="middle">X</td></tr></table>')
        table = ir.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        cell = body.children[0].children[0]
        assert isinstance(cell, DocumentIRTableCellNode)
        assert cell.alignment == "left"

    def test_uppercase_alignment_normalised(self) -> None:
        ir = _convert('<table><tr><td align="CENTER">X</td></tr></table>')
        table = ir.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        cell = body.children[0].children[0]
        assert cell.alignment == "center"

    def test_right_alignment_preserved(self) -> None:
        ir = _convert('<table><tr><td align="right">X</td></tr></table>')
        table = ir.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        cell = body.children[0].children[0]
        assert cell.alignment == "right"

    def test_missing_alignment_defaults_to_left(self) -> None:
        ir = _convert("<table><tr><td>X</td></tr></table>")
        table = ir.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        cell = body.children[0].children[0]
        assert cell.alignment == "left"
