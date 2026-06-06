from dhtml.html_ast_builder import parse_html
from dhtml.html_ast_node import (
    HtmlASTCommentNode,
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTTextNode,
)


def _first_elem(doc: HtmlASTDocumentNode) -> HtmlASTElementNode:
    """Return the first element child of the document."""
    for child in doc.children:
        if isinstance(child, HtmlASTElementNode):
            return child
    raise AssertionError("No element child found in document")


def _elem_children(node: HtmlASTElementNode) -> list[HtmlASTElementNode]:
    """Return all element children of a node."""
    return [c for c in node.children if isinstance(c, HtmlASTElementNode)]


def _text_children(node: HtmlASTElementNode) -> list[HtmlASTTextNode]:
    """Return all text children of a node."""
    return [c for c in node.children if isinstance(c, HtmlASTTextNode)]


def _comment_children(node: HtmlASTDocumentNode | HtmlASTElementNode) -> list[HtmlASTCommentNode]:
    """Return all comment children of a node."""
    return [c for c in node.children if isinstance(c, HtmlASTCommentNode)]


class TestDocumentRoot:
    """parse_html returns an HtmlASTDocumentNode with correct metadata."""

    def test_returns_document_node(self) -> None:
        doc = parse_html("<p>Hello</p>")
        assert isinstance(doc, HtmlASTDocumentNode)

    def test_source_path_stored(self) -> None:
        doc = parse_html("<p>Hi</p>", source_path="/tmp/test.html")
        assert doc.source_path == "/tmp/test.html"

    def test_source_path_none_when_not_given(self) -> None:
        doc = parse_html("<p>Hi</p>")
        assert doc.source_path is None

    def test_empty_string_produces_empty_document(self) -> None:
        doc = parse_html("")
        assert doc.children == []

    def test_whitespace_only_suppressed_at_root(self) -> None:
        doc = parse_html("   \n\t  ")
        assert doc.children == []


class TestDoctype:
    """DOCTYPE declaration sets has_doctype on the document node."""

    def test_doctype_sets_flag(self) -> None:
        doc = parse_html("<!DOCTYPE html><p>Hello</p>")
        assert doc.has_doctype is True

    def test_no_doctype_flag_false(self) -> None:
        doc = parse_html("<p>Hello</p>")
        assert doc.has_doctype is False

    def test_doctype_case_insensitive(self) -> None:
        doc = parse_html("<!doctype html><p>x</p>")
        assert doc.has_doctype is True


class TestTagNameLowercasing:
    """Tag names are always stored in lower case."""

    def test_uppercase_tag_lowercased(self) -> None:
        doc = parse_html("<DIV>content</DIV>")
        elem = _first_elem(doc)
        assert elem.tag_name == "div"

    def test_mixed_case_tag_lowercased(self) -> None:
        doc = parse_html("<H1>Title</H1>")
        elem = _first_elem(doc)
        assert elem.tag_name == "h1"


class TestVoidElements:
    """Void elements are never pushed onto the stack and have no children."""

    def test_br_is_leaf(self) -> None:
        doc = parse_html("<div><br></div>")
        div = _first_elem(doc)
        br = _elem_children(div)[0]
        assert br.tag_name == "br"
        assert br.children == []

    def test_img_is_leaf(self) -> None:
        doc = parse_html("<img src='x.png'>")
        img = _first_elem(doc)
        assert img.tag_name == "img"
        assert img.children == []

    def test_hr_is_leaf(self) -> None:
        doc = parse_html("<hr>")
        hr = _first_elem(doc)
        assert hr.tag_name == "hr"
        assert hr.children == []

    def test_input_is_leaf(self) -> None:
        doc = parse_html("<input type='text'>")
        inp = _first_elem(doc)
        assert inp.children == []

    def test_meta_is_leaf(self) -> None:
        doc = parse_html("<meta charset='utf-8'>")
        meta = _first_elem(doc)
        assert meta.children == []

    def test_link_is_leaf(self) -> None:
        doc = parse_html("<link rel='stylesheet' href='a.css'>")
        link = _first_elem(doc)
        assert link.children == []

    def test_subsequent_content_not_child_of_void(self) -> None:
        doc = parse_html("<div><br>text</div>")
        div = _first_elem(doc)
        texts = _text_children(div)
        assert any(t.content == "text" for t in texts)


class TestSelfClosingTags:
    """Explicitly self-closing tags produce leaf elements with no children."""

    def test_explicit_self_closing_is_leaf(self) -> None:
        doc = parse_html("<div><span /></div>")
        div = _first_elem(doc)
        span = _elem_children(div)[0]
        assert span.tag_name == "span"
        assert span.children == []

    def test_self_closing_foreign_element(self) -> None:
        doc = parse_html("<svg:path d='M 0 0' />")
        # Element is present as a leaf
        elem = _first_elem(doc)
        assert elem.children == []


class TestAttributes:
    """Attributes are parsed correctly in all syntactic forms."""

    def test_double_quoted_attribute(self) -> None:
        doc = parse_html('<a href="https://example.com">link</a>')
        a = _first_elem(doc)
        assert a.attributes["href"] == "https://example.com"

    def test_single_quoted_attribute(self) -> None:
        doc = parse_html("<a href='https://example.com'>link</a>")
        a = _first_elem(doc)
        assert a.attributes["href"] == "https://example.com"

    def test_unquoted_attribute(self) -> None:
        doc = parse_html("<div class=highlight>text</div>")
        div = _first_elem(doc)
        assert div.attributes["class"] == "highlight"

    def test_boolean_attribute(self) -> None:
        doc = parse_html("<input disabled>")
        inp = _first_elem(doc)
        assert "disabled" in inp.attributes
        assert inp.attributes["disabled"] == ""

    def test_multiple_attributes(self) -> None:
        doc = parse_html('<img src="a.png" alt="A" title="T">')
        img = _first_elem(doc)
        assert img.attributes["src"] == "a.png"
        assert img.attributes["alt"] == "A"
        assert img.attributes["title"] == "T"

    def test_attribute_names_lowercased(self) -> None:
        doc = parse_html("<div CLASS='x'>text</div>")
        div = _first_elem(doc)
        assert "class" in div.attributes


class TestEntityDecoding:
    """HTML entities are decoded in text content and attribute values."""

    def test_amp_in_text(self) -> None:
        doc = parse_html("<p>a &amp; b</p>")
        p = _first_elem(doc)
        text = _text_children(p)[0]
        assert "&" in text.content

    def test_lt_in_text(self) -> None:
        doc = parse_html("<p>&lt;tag&gt;</p>")
        p = _first_elem(doc)
        text = _text_children(p)[0]
        assert "<" in text.content
        assert ">" in text.content

    def test_quot_in_text(self) -> None:
        doc = parse_html("<p>&quot;quoted&quot;</p>")
        p = _first_elem(doc)
        text = _text_children(p)[0]
        assert '"' in text.content

    def test_nbsp_in_text(self) -> None:
        doc = parse_html("<p>a&nbsp;b</p>")
        p = _first_elem(doc)
        text = _text_children(p)[0]
        assert "\u00a0" in text.content

    def test_numeric_entity_in_text(self) -> None:
        doc = parse_html("<p>&#65;</p>")
        p = _first_elem(doc)
        text = _text_children(p)[0]
        assert "A" in text.content

    def test_hex_entity_in_text(self) -> None:
        doc = parse_html("<p>&#x41;</p>")
        p = _first_elem(doc)
        text = _text_children(p)[0]
        assert "A" in text.content

    def test_entity_in_attribute_value(self) -> None:
        doc = parse_html('<a href="a&amp;b">link</a>')
        a = _first_elem(doc)
        assert a.attributes["href"] == "a&b"


class TestComments:
    """HTML comments produce HtmlASTCommentNode children."""

    def test_comment_node_created(self) -> None:
        doc = parse_html("<!-- a comment -->")
        comments = _comment_children(doc)
        assert len(comments) == 1

    def test_comment_content_stored(self) -> None:
        doc = parse_html("<!-- hello world -->")
        comment = _comment_children(doc)[0]
        assert "hello world" in comment.content

    def test_comment_inside_element(self) -> None:
        doc = parse_html("<div><!-- inner --></div>")
        div = _first_elem(doc)
        inner_comments = [c for c in div.children if isinstance(c, HtmlASTCommentNode)]
        assert len(inner_comments) == 1


class TestRawTextElements:
    """script and style content is consumed as opaque text, not parsed as HTML."""

    def test_script_content_not_parsed_as_elements(self) -> None:
        doc = parse_html("<script>if (a < b) { x = 1; }</script>")
        script = _first_elem(doc)
        assert script.tag_name == "script"
        nested_elems = _elem_children(script)
        assert nested_elems == []

    def test_script_content_stored_as_text(self) -> None:
        doc = parse_html("<script>var x = 1;</script>")
        script = _first_elem(doc)
        texts = _text_children(script)
        assert len(texts) == 1
        assert "var x = 1;" in texts[0].content

    def test_style_content_not_parsed_as_elements(self) -> None:
        doc = parse_html("<style>body { color: red; }</style>")
        style = _first_elem(doc)
        assert style.tag_name == "style"
        assert _elem_children(style) == []

    def test_style_content_stored_as_text(self) -> None:
        doc = parse_html("<style>.cls { margin: 0; }</style>")
        style = _first_elem(doc)
        texts = _text_children(style)
        assert len(texts) == 1
        assert ".cls" in texts[0].content


class TestImpliedCloseRules:
    """Opening certain tags implicitly closes currently open same-type tags."""

    def test_p_closes_open_p(self) -> None:
        doc = parse_html("<p>first<p>second</p>")
        ps = [c for c in doc.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "p"]
        assert len(ps) == 2

    def test_li_closes_open_li(self) -> None:
        doc = parse_html("<ul><li>a<li>b<li>c</ul>")
        ul = _first_elem(doc)
        lis = _elem_children(ul)
        assert len(lis) == 3

    def test_tr_closes_open_tr(self) -> None:
        doc = parse_html("<table><tr><td>A</td><tr><td>B</td></table>")
        table = _first_elem(doc)
        trs = [c for c in table.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "tr"]
        assert len(trs) == 2

    def test_td_closes_open_td(self) -> None:
        doc = parse_html("<table><tr><td>A<td>B</tr></table>")
        table = _first_elem(doc)
        tr = _elem_children(table)[0]
        tds = [c for c in tr.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "td"]
        assert len(tds) == 2

    def test_option_closes_open_option(self) -> None:
        doc = parse_html("<select><option>A<option>B</select>")
        select = _first_elem(doc)
        options = [c for c in select.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "option"]
        assert len(options) == 2

    def test_dt_closes_open_dt(self) -> None:
        doc = parse_html("<dl><dt>Term1<dt>Term2</dl>")
        dl = _first_elem(doc)
        dts = [c for c in dl.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "dt"]
        assert len(dts) == 2

    def test_dd_closes_open_dt(self) -> None:
        doc = parse_html("<dl><dt>Term<dd>Def</dl>")
        dl = _first_elem(doc)
        tags = [c.tag_name for c in dl.children if isinstance(c, HtmlASTElementNode)]
        assert tags == ["dt", "dd"]

    def test_thead_closes_tbody(self) -> None:
        doc = parse_html("<table><tbody><tr><td>B</td></tr><thead><tr><th>H</th></tr></thead></table>")
        table = _first_elem(doc)
        sections = [c for c in table.children if isinstance(c, HtmlASTElementNode)]
        section_tags = [s.tag_name for s in sections]
        assert "thead" in section_tags


class TestMisnesting:
    """Misnested close tags are handled by walking the stack."""

    def test_misnested_close_tag_discards_intervening(self) -> None:
        doc = parse_html("<div><span><p>text</div>")
        div = _first_elem(doc)
        assert div.tag_name == "div"
        assert len(div.children) > 0

    def test_unmatched_close_tag_is_ignored(self) -> None:
        doc = parse_html("<p>text</p></span>")
        ps = [c for c in doc.children if isinstance(c, HtmlASTElementNode) and c.tag_name == "p"]
        assert len(ps) == 1


class TestBareAngle:
    """A bare '<' that is not a valid tag start is emitted as a text node."""

    def test_bare_lt_becomes_text(self) -> None:
        doc = parse_html("a < b")
        texts = [c for c in doc.children if isinstance(c, HtmlASTTextNode)]
        combined = "".join(t.content for t in texts)
        assert "<" in combined

    def test_bare_lt_does_not_consume_following_text(self) -> None:
        doc = parse_html("<p>1 < 2</p>")
        p = _first_elem(doc)
        texts = _text_children(p)
        combined = "".join(t.content for t in texts)
        assert "1 " in combined or "< 2" in combined


class TestNestedStructure:
    """The parser builds correctly nested parent/child trees."""

    def test_nested_elements(self) -> None:
        doc = parse_html("<div><p>Hello</p></div>")
        div = _first_elem(doc)
        assert div.tag_name == "div"
        p = _elem_children(div)[0]
        assert p.tag_name == "p"
        text = _text_children(p)[0]
        assert text.content == "Hello"

    def test_sibling_elements(self) -> None:
        doc = parse_html("<p>A</p><p>B</p>")
        ps = [c for c in doc.children if isinstance(c, HtmlASTElementNode)]
        assert len(ps) == 2

    def test_text_and_element_siblings(self) -> None:
        doc = parse_html("<div>before<span>inside</span>after</div>")
        div = _first_elem(doc)
        assert len(div.children) == 3
