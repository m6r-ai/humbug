"""Tests for the document_ir → Markdown serialiser."""

from dmarkdown.document_ir_to_markdown import document_ir_to_markdown
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


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _doc(*children) -> DocumentIRDocumentNode:
    doc = DocumentIRDocumentNode()
    for child in children:
        doc.add_child(child)
    return doc


def _para(*children) -> DocumentIRParagraphNode:
    p = DocumentIRParagraphNode()
    for child in children:
        p.add_child(child)
    return p


def _span(text, bold=False, italic=False, strike=False, code=False) -> DocumentIRTextSpanNode:
    return DocumentIRTextSpanNode(content=text, bold=bold, italic=italic, strikethrough=strike, code=code)


def _heading(level, *children) -> DocumentIRHeadingNode:
    h = DocumentIRHeadingNode(level=level)
    for child in children:
        h.add_child(child)
    return h


def _item(*children) -> DocumentIRListItemNode:
    item = DocumentIRListItemNode()
    for child in children:
        item.add_child(child)
    return item


def _md(doc: DocumentIRDocumentNode) -> str:
    return document_ir_to_markdown(doc)


# ---------------------------------------------------------------------------
# Document structure
# ---------------------------------------------------------------------------

class TestDocumentStructure:
    def test_empty_document(self):
        assert _md(_doc()) == ""

    def test_single_paragraph(self):
        assert _md(_doc(_para(_span("Hello")))) == "Hello\n"

    def test_two_paragraphs_separated_by_blank_line(self):
        result = _md(_doc(_para(_span("First")), _para(_span("Second"))))
        assert result == "First\n\nSecond\n"

    def test_trailing_newline(self):
        result = _md(_doc(_para(_span("text"))))
        assert result.endswith("\n")


# ---------------------------------------------------------------------------
# Headings
# ---------------------------------------------------------------------------

class TestHeadings:
    def test_heading1(self):
        assert _md(_doc(_heading(1, _span("Title")))) == "# Title\n"

    def test_heading2(self):
        assert _md(_doc(_heading(2, _span("Section")))) == "## Section\n"

    def test_heading6(self):
        assert _md(_doc(_heading(6, _span("Deep")))) == "###### Deep\n"

    def test_heading_with_bold(self):
        result = _md(_doc(_heading(1, _span("Bold", bold=True))))
        assert result == "# **Bold**\n"


# ---------------------------------------------------------------------------
# Inline formatting — individual flags
# ---------------------------------------------------------------------------

class TestInlineFormatting:
    def test_plain_text(self):
        assert _md(_doc(_para(_span("plain")))) == "plain\n"

    def test_bold(self):
        assert _md(_doc(_para(_span("bold", bold=True)))) == "**bold**\n"

    def test_italic(self):
        assert _md(_doc(_para(_span("italic", italic=True)))) == "*italic*\n"

    def test_strikethrough(self):
        assert _md(_doc(_para(_span("struck", strike=True)))) == "~~struck~~\n"

    def test_inline_code(self):
        assert _md(_doc(_para(_span("func()", code=True)))) == "`func()`\n"


# ---------------------------------------------------------------------------
# Inline formatting — combinations
# ---------------------------------------------------------------------------

class TestInlineFormattingCombinations:
    def test_bold_and_italic(self):
        assert _md(_doc(_para(_span("bi", bold=True, italic=True)))) == "***bi***\n"

    def test_bold_and_strikethrough(self):
        result = _md(_doc(_para(_span("bs", bold=True, strike=True))))
        assert result == "~~**bs**~~\n"

    def test_italic_and_strikethrough(self):
        result = _md(_doc(_para(_span("is", italic=True, strike=True))))
        assert result == "~~*is*~~\n"

    def test_bold_italic_and_strikethrough(self):
        result = _md(_doc(_para(_span("bis", bold=True, italic=True, strike=True))))
        assert result == "~~***bis***~~\n"

    def test_code_takes_precedence_over_other_flags(self):
        # Code spans are returned as-is regardless of other flags
        result = _md(_doc(_para(_span("x", bold=True, code=True))))
        assert result == "`x`\n"


# ---------------------------------------------------------------------------
# Adjacent span coalescing
# ---------------------------------------------------------------------------

class TestAdjacentSpanCoalescing:
    def test_adjacent_bold_spans_coalesced(self):
        result = _md(_doc(_para(_span("Hello", bold=True), _span(" World", bold=True))))
        assert result == "**Hello World**\n"

    def test_adjacent_italic_spans_coalesced(self):
        result = _md(_doc(_para(_span("Hello", italic=True), _span(" World", italic=True))))
        assert result == "*Hello World*\n"

    def test_adjacent_plain_spans_coalesced(self):
        result = _md(_doc(_para(_span("Hello"), _span(" World"))))
        assert result == "Hello World\n"

    def test_adjacent_bold_mid_word_coalesced(self):
        result = _md(_doc(_para(_span("R", bold=True), _span("edemption", bold=True))))
        assert result == "**Redemption**\n"

    def test_different_formatting_not_coalesced(self):
        result = _md(_doc(_para(_span("Hello", bold=True), _span(" World", italic=True))))
        assert result == "**Hello*** World*\n"

    def test_non_span_node_breaks_coalescing(self):
        para = DocumentIRParagraphNode()
        para.add_child(_span("Before", bold=True))
        para.add_child(DocumentIRLineBreakNode())
        para.add_child(_span("After", bold=True))
        result = _md(_doc(para))
        assert result == "**Before**  \n**After**\n"

    def test_three_adjacent_bold_spans_coalesced(self):
        result = _md(_doc(_para(
            _span("Launch Phase", bold=True),
            _span(":", bold=True),
            _span(" 2", bold=True),
        )))
        assert result == "**Launch Phase: 2**\n"


# ---------------------------------------------------------------------------
# Markdown escaping
# ---------------------------------------------------------------------------

class TestMarkdownEscaping:
    def test_backslash_escaped(self):
        result = _md(_doc(_para(_span("a\\b"))))
        assert "a\\\\b" in result

    def test_backtick_escaped(self):
        result = _md(_doc(_para(_span("a`b"))))
        assert "a\\`b" in result

    def test_asterisk_escaped(self):
        result = _md(_doc(_para(_span("a*b"))))
        assert "a\\*b" in result

    def test_underscore_escaped(self):
        result = _md(_doc(_para(_span("a_b"))))
        assert "a\\_b" in result

    def test_tilde_escaped_in_plain_text(self):
        result = _md(_doc(_para(_span("a~b"))))
        assert "a\\~b" in result

    def test_code_span_not_escaped(self):
        # Content inside code spans must not be escaped
        result = _md(_doc(_para(_span("a*b`c", code=True))))
        assert result == "`a*b`c`\n"


# ---------------------------------------------------------------------------
# Horizontal rule
# ---------------------------------------------------------------------------

class TestHorizontalRule:
    def test_horizontal_rule(self):
        assert _md(_doc(DocumentIRHorizontalRuleNode())) == "---\n"

    def test_horizontal_rule_between_paragraphs(self):
        result = _md(_doc(
            _para(_span("before")),
            DocumentIRHorizontalRuleNode(),
            _para(_span("after")),
        ))
        assert "---" in result
        assert "before" in result
        assert "after" in result


# ---------------------------------------------------------------------------
# Code blocks
# ---------------------------------------------------------------------------

class TestCodeBlocks:
    def test_fenced_code_block_no_language(self):
        result = _md(_doc(DocumentIRCodeBlockNode(language="", content="x = 1")))
        assert result == "```\nx = 1\n```\n"

    def test_fenced_code_block_with_language(self):
        result = _md(_doc(DocumentIRCodeBlockNode(language="python", content="x = 1")))
        assert result == "```python\nx = 1\n```\n"

    def test_multiline_code_block(self):
        result = _md(_doc(DocumentIRCodeBlockNode(language="", content="a\nb\nc")))
        assert result == "```\na\nb\nc\n```\n"


# ---------------------------------------------------------------------------
# Blockquotes
# ---------------------------------------------------------------------------

class TestBlockquotes:
    def test_simple_blockquote(self):
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quoted")))
        result = _md(_doc(bq))
        assert result == "> Quoted\n"

    def test_multiline_blockquote(self):
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("First")))
        bq.add_child(_para(_span("Second")))
        result = _md(_doc(bq))
        lines = result.splitlines()
        assert all(line.startswith(">") for line in lines if line)


# ---------------------------------------------------------------------------
# Unordered lists
# ---------------------------------------------------------------------------

class TestUnorderedLists:
    def test_simple_list(self):
        ul = DocumentIRUnorderedListNode()
        ul.add_child(_item(_para(_span("Alpha"))))
        ul.add_child(_item(_para(_span("Beta"))))
        result = _md(_doc(ul))
        assert "- Alpha" in result
        assert "- Beta" in result

    def test_nested_list(self):
        inner = DocumentIRUnorderedListNode()
        inner.add_child(_item(_para(_span("Inner"))))
        outer_item = DocumentIRListItemNode()
        outer_item.add_child(_para(_span("Outer")))
        outer_item.add_child(inner)
        ul = DocumentIRUnorderedListNode()
        ul.add_child(outer_item)
        result = _md(_doc(ul))
        lines = result.splitlines()
        outer_line = next(l for l in lines if "Outer" in l)
        inner_line = next(l for l in lines if "Inner" in l)
        assert not outer_line.startswith(" ")
        assert inner_line.startswith("    ")


# ---------------------------------------------------------------------------
# Ordered lists
# ---------------------------------------------------------------------------

class TestOrderedLists:
    def test_simple_ordered_list(self):
        ol = DocumentIROrderedListNode(start=1)
        ol.add_child(_item(_para(_span("First"))))
        ol.add_child(_item(_para(_span("Second"))))
        result = _md(_doc(ol))
        assert "1. First" in result
        assert "2. Second" in result

    def test_ordered_list_custom_start(self):
        ol = DocumentIROrderedListNode(start=3)
        ol.add_child(_item(_para(_span("Three"))))
        result = _md(_doc(ol))
        assert "3. Three" in result


# ---------------------------------------------------------------------------
# Links and images
# ---------------------------------------------------------------------------

class TestLinksAndImages:
    def test_link(self):
        link = DocumentIRLinkNode(url="https://example.com")
        link.add_child(_span("click here"))
        result = _md(_doc(_para(link)))
        assert "[click here](https://example.com)" in result

    def test_image(self):
        img = DocumentIRImageNode(url="photo.png", alt_text="A photo")
        result = _md(_doc(_para(img)))
        assert "![A photo](photo.png)" in result

    def test_line_break(self):
        result = _md(_doc(_para(_span("before"), DocumentIRLineBreakNode(), _span("after"))))
        assert "before" in result
        assert "after" in result


# ---------------------------------------------------------------------------
# Tables
# ---------------------------------------------------------------------------

def _table_with_header() -> DocumentIRTableNode:
    table = DocumentIRTableNode()
    header = DocumentIRTableHeaderNode()
    hrow = DocumentIRTableRowNode()
    for text, alignment in [("Name", "left"), ("Score", "right")]:
        cell = DocumentIRTableCellNode(is_header=True, alignment=alignment)
        cell.add_child(_para(_span(text)))
        hrow.add_child(cell)
    header.add_child(hrow)
    table.add_child(header)
    body = DocumentIRTableBodyNode()
    brow = DocumentIRTableRowNode()
    for text, alignment in [("Alice", "left"), ("42", "right")]:
        cell = DocumentIRTableCellNode(is_header=False, alignment=alignment)
        cell.add_child(_para(_span(text)))
        brow.add_child(cell)
    body.add_child(brow)
    table.add_child(body)
    return table


class TestTables:
    def test_table_has_header_row(self):
        result = _md(_doc(_table_with_header()))
        lines = result.strip().splitlines()
        assert "Name" in lines[0]
        assert "Score" in lines[0]

    def test_table_has_separator_row(self):
        result = _md(_doc(_table_with_header()))
        lines = result.strip().splitlines()
        assert "---" in lines[1]

    def test_table_right_alignment_separator(self):
        result = _md(_doc(_table_with_header()))
        lines = result.strip().splitlines()
        assert lines[1].endswith(":|") or "---:" in lines[1]

    def test_table_body_row(self):
        result = _md(_doc(_table_with_header()))
        assert "Alice" in result
        assert "42" in result

    def test_table_pipe_delimited(self):
        result = _md(_doc(_table_with_header()))
        for line in result.strip().splitlines():
            assert line.startswith("|") and line.endswith("|")

    def test_cell_with_inline_children(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_span("inline"))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _md(_doc(table))
        assert "inline" in result

    def test_pipe_in_cell_escaped(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_para(_span("a|b")))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _md(_doc(table))
        assert "a\\|b" in result

    def test_table_col_count_from_widest_row(self):
        """All columns appear when a body row has more cells than the header row."""
        table = DocumentIRTableNode()

        header = DocumentIRTableHeaderNode()
        hrow = DocumentIRTableRowNode()
        for text in ("A", "B"):
            cell = DocumentIRTableCellNode(is_header=True, alignment="left")
            cell.add_child(_span(text))
            hrow.add_child(cell)
        header.add_child(hrow)
        table.add_child(header)

        body = DocumentIRTableBodyNode()
        brow = DocumentIRTableRowNode()
        for text in ("1", "2", "3", "4"):
            cell = DocumentIRTableCellNode(is_header=False, alignment="left")
            cell.add_child(_span(text))
            brow.add_child(cell)
        body.add_child(brow)
        table.add_child(body)

        result = _md(_doc(table))
        assert "1" in result
        assert "2" in result
        assert "3" in result
        assert "4" in result

    def test_multi_row_header_uses_max_col_count(self):
        """All columns appear when a later header row has more cells than the first."""
        table = DocumentIRTableNode()

        header = DocumentIRTableHeaderNode()
        hrow1 = DocumentIRTableRowNode()
        for text in ("A", "B"):
            cell = DocumentIRTableCellNode(is_header=True, alignment="left")
            cell.add_child(_span(text))
            hrow1.add_child(cell)
        header.add_child(hrow1)
        hrow2 = DocumentIRTableRowNode()
        for text in ("C", "D", "E", "F"):
            cell = DocumentIRTableCellNode(is_header=True, alignment="left")
            cell.add_child(_span(text))
            hrow2.add_child(cell)
        header.add_child(hrow2)
        table.add_child(header)

        body = DocumentIRTableBodyNode()
        brow = DocumentIRTableRowNode()
        for text in ("1", "2", "3", "4"):
            cell = DocumentIRTableCellNode(is_header=False, alignment="left")
            cell.add_child(_span(text))
            brow.add_child(cell)
        body.add_child(brow)
        table.add_child(body)

        result = _md(_doc(table))
        for text in ("C", "D", "E", "F", "1", "2", "3", "4"):
            assert text in result


# ---------------------------------------------------------------------------
# Definition lists
# ---------------------------------------------------------------------------

class TestDefinitionLists:
    def test_definition_term_rendered_as_bold(self):
        from document_ir import (
            DocumentIRDefinitionListNode,
            DocumentIRDefinitionTermNode,
        )
        dl = DocumentIRDefinitionListNode()
        dt = DocumentIRDefinitionTermNode()
        dt.add_child(_span("My Term"))
        dl.add_child(dt)
        result = _md(_doc(dl))
        assert "**My Term**" in result

    def test_definition_description_rendered_with_colon(self):
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
        dd.add_child(_span("My Description"))
        dl.add_child(dd)
        result = _md(_doc(dl))
        assert ":   My Description" in result

    def test_full_definition_list_structure(self):
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
        dd.add_child(_span("Description"))
        dl.add_child(dd)
        result = _md(_doc(dl))
        assert "**Term**" in result
        assert ":   Description" in result


# ---------------------------------------------------------------------------
# Superscript and subscript
# ---------------------------------------------------------------------------

class TestSuperscriptSubscript:
    def test_superscript_renders_as_plain_text(self):
        span = DocumentIRTextSpanNode(content="2", superscript=True)
        result = _md(_doc(_para(span)))
        assert "<sup>" not in result
        assert "2" in result

    def test_subscript_renders_as_plain_text(self):
        span = DocumentIRTextSpanNode(content="2", subscript=True)
        result = _md(_doc(_para(span)))
        assert "<sub>" not in result
        assert "2" in result
