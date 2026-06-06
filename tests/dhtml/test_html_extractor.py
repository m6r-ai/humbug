from dhtml.html_ast_builder import parse_html
from dhtml.html_extractor import extract_text


def _extract(html: str) -> str:
    return extract_text(parse_html(html))


class TestSkippedElements:
    """script, style, and head content is excluded from the extracted text."""

    def test_script_content_excluded(self) -> None:
        result = _extract("<script>alert('x');</script>")
        assert "alert" not in result

    def test_style_content_excluded(self) -> None:
        result = _extract("<style>body { color: red; }</style>")
        assert "color" not in result
        assert "red" not in result

    def test_head_content_excluded(self) -> None:
        result = _extract("<html><head><title>My Page</title></head><body><p>Body</p></body></html>")
        assert "My Page" not in result
        assert "Body" in result

    def test_body_content_included(self) -> None:
        result = _extract("<body><p>Visible</p></body>")
        assert "Visible" in result


class TestBlockElements:
    """Block-level elements are separated by newlines in the output."""

    def test_p_followed_by_newline(self) -> None:
        result = _extract("<p>First</p><p>Second</p>")
        assert "First" in result
        assert "Second" in result
        assert result.index("First") < result.index("Second")
        assert "\n" in result

    def test_heading_followed_by_newline(self) -> None:
        result = _extract("<h1>Title</h1><p>Body</p>")
        assert "\n" in result

    def test_div_followed_by_newline(self) -> None:
        result = _extract("<div>A</div><div>B</div>")
        assert "\n" in result

    def test_li_items_separated(self) -> None:
        result = _extract("<ul><li>Item 1</li><li>Item 2</li></ul>")
        assert "Item 1" in result
        assert "Item 2" in result

    def test_td_produces_newline(self) -> None:
        result = _extract("<table><tr><td>Cell</td></tr></table>")
        assert "Cell" in result

    def test_blockquote_separated(self) -> None:
        result = _extract("<p>before</p><blockquote>quote</blockquote><p>after</p>")
        assert "before" in result
        assert "quote" in result
        assert "after" in result

    def test_pre_content_included(self) -> None:
        result = _extract("<pre>code here</pre>")
        assert "code here" in result


class TestLineBreaks:
    """br and hr each produce a newline."""

    def test_br_produces_newline(self) -> None:
        result = _extract("<p>line1<br>line2</p>")
        assert "line1" in result
        assert "line2" in result
        assert "\n" in result

    def test_hr_produces_newline(self) -> None:
        result = _extract("before<hr>after")
        assert "\n" in result
        assert "before" in result
        assert "after" in result


class TestInlineElements:
    """Inline elements contribute text inline without adding newlines."""

    def test_strong_inline(self) -> None:
        result = _extract("<p>Hello <strong>world</strong>!</p>")
        assert "Hello" in result
        assert "world" in result
        assert "!" in result

    def test_em_inline(self) -> None:
        result = _extract("<p>Some <em>italic</em> text</p>")
        assert "Some" in result
        assert "italic" in result

    def test_a_inline(self) -> None:
        result = _extract('<p>See <a href="#">here</a> for details</p>')
        assert "here" in result

    def test_code_inline(self) -> None:
        result = _extract("<p>Call <code>func()</code> now</p>")
        assert "func()" in result

    def test_span_inline(self) -> None:
        result = _extract("<p><span>word</span></p>")
        assert "word" in result


class TestNewlineCollapsing:
    """Runs of more than two consecutive newlines are collapsed to two."""

    def test_triple_newline_collapsed(self) -> None:
        result = _extract("<p>A</p><p>B</p><p>C</p>")
        assert "\n\n\n" not in result

    def test_many_block_elements(self) -> None:
        html = "".join(f"<p>P{i}</p>" for i in range(10))
        result = _extract(html)
        assert "\n\n\n" not in result


class TestLeadingTrailingWhitespace:
    """The result has leading and trailing whitespace stripped."""

    def test_no_leading_whitespace(self) -> None:
        result = _extract("<p>Hello</p>")
        assert result == result.strip()

    def test_no_trailing_whitespace(self) -> None:
        result = _extract("<p>Hello</p>")
        assert not result.endswith("\n")
        assert not result.endswith(" ")


class TestTextContent:
    """Plain text content is correctly extracted end-to-end."""

    def test_simple_paragraph(self) -> None:
        result = _extract("<p>Hello, world!</p>")
        assert result == "Hello, world!"

    def test_entities_decoded(self) -> None:
        result = _extract("<p>a &amp; b</p>")
        assert "&" in result

    def test_multiple_headings_and_paragraphs(self) -> None:
        html = "<h1>Title</h1><p>Intro</p><h2>Section</h2><p>Body</p>"
        result = _extract(html)
        assert "Title" in result
        assert "Intro" in result
        assert "Section" in result
        assert "Body" in result

    def test_full_page(self) -> None:
        html = """<!DOCTYPE html>
<html>
<head><title>Test</title></head>
<body>
<h1>Heading</h1>
<p>Paragraph text</p>
</body>
</html>"""
        result = _extract(html)
        assert "Heading" in result
        assert "Paragraph text" in result
        assert "Test" not in result
