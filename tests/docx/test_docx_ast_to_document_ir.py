"""Tests for the DOCX AST → document_ir mapper."""

import pytest

from docx import docx_ast_to_document_ir
from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
    DocxASTBodyNode,
    DocxASTBreakNode,
    DocxASTDocumentNode,
    DocxASTDrawingNode,
    DocxASTLastRenderedPageBreakNode,
    DocxASTNumLevelNode,
    DocxASTNumNode,
    DocxASTNumberingNode,
    DocxASTNumberingPropertiesNode,
    DocxASTParagraphNode,
    DocxASTParagraphPropertiesNode,
    DocxASTRunNode,
    DocxASTRunPropertiesNode,
    DocxASTStyleNode,
    DocxASTStylesNode,
    DocxASTTabNode,
    DocxASTTableCellNode,
    DocxASTTableCellPropertiesNode,
    DocxASTTableNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTextNode,
)
from document_ir import (
    DocumentIRBlockquoteNode,
    DocumentIRCodeBlockNode,
    DocumentIRDocumentNode,
    DocumentIRHeadingNode,
    DocumentIRImageNode,
    DocumentIRLineBreakNode,
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
# Fixture builders
# ---------------------------------------------------------------------------

def _doc(*children) -> DocxASTDocumentNode:
    """Build a DocxASTDocumentNode with the given children."""
    doc = DocxASTDocumentNode()
    for child in children:
        doc.add_child(child)
    return doc


def _body(*children) -> DocxASTBodyNode:
    body = DocxASTBodyNode()
    for child in children:
        body.add_child(child)
    return body


def _para(
    text: str = "",
    style_id: str = "",
    bold: bool = False,
    italic: bool = False,
    sz: int = 0,
    font: str = "",
    num_id: str = "",
    ilvl: int = 0,
    justification: str = "",
    outline_level: int = -1,
) -> DocxASTParagraphNode:
    """Build a paragraph node with optional properties."""
    para = DocxASTParagraphNode()

    # Paragraph properties
    if style_id or num_id or justification or outline_level >= 0:
        ppr = DocxASTParagraphPropertiesNode(
            style_id=style_id or None,
            justification=justification or None,
            outline_level=outline_level if outline_level >= 0 else None,
        )
        if num_id:
            ppr.add_child(DocxASTNumberingPropertiesNode(num_id=num_id, ilvl=ilvl))
        para.add_child(ppr)

    # Run with text
    if text:
        run = DocxASTRunNode()
        if bold or italic or sz or font:
            rpr = DocxASTRunPropertiesNode(
                bold=bold,
                italic=italic,
                sz=sz if sz else None,
                font_ascii=font if font else None,
            )
            run.add_child(rpr)
        run.add_child(DocxASTTextNode(content=text))
        para.add_child(run)

    return para


def _style(
    style_id: str,
    name: str,
    style_type: str = "paragraph",
    based_on: str = "",
    bold: bool = False,
    italic: bool = False,
    sz: int = 0,
    font: str = "",
    outline_level: int = -1,
    is_default: bool = False,
) -> DocxASTStyleNode:
    """Build a style node."""
    node = DocxASTStyleNode(
        style_type=style_type,
        style_id=style_id,
        name=name,
        based_on=based_on or None,
        is_default=is_default,
    )
    if bold or italic or sz or font:
        rpr = DocxASTRunPropertiesNode(
            bold=bold,
            italic=italic,
            sz=sz if sz else None,
            font_ascii=font if font else None,
        )
        node.add_child(rpr)
    if outline_level >= 0:
        ppr = DocxASTParagraphPropertiesNode(outline_level=outline_level)
        node.add_child(ppr)
    return node


def _styles_node(*styles) -> DocxASTStylesNode:
    node = DocxASTStylesNode()
    for s in styles:
        node.add_child(s)
    return node


def _numbering_node(
    num_id: str = "1",
    abstract_id: str = "0",
    num_fmt: str = "bullet",
    start: int = 1,
    ilvl: int = 0,
) -> DocxASTNumberingNode:
    """Build a minimal numbering node with one abstract num and one instance."""
    numbering = DocxASTNumberingNode()

    abstract = DocxASTAbstractNumNode(abstract_num_id=abstract_id)
    lvl = DocxASTNumLevelNode(
        ilvl=ilvl,
        start=start,
        num_fmt=num_fmt,
        lvl_text="\u2022" if num_fmt == "bullet" else "%1.",
    )
    abstract.add_child(lvl)
    numbering.add_child(abstract)

    num = DocxASTNumNode(num_id=num_id, abstract_num_id=abstract_id)
    numbering.add_child(num)

    return numbering


def _map(doc: DocxASTDocumentNode) -> DocumentIRDocumentNode:
    return docx_ast_to_document_ir(doc)


# ---------------------------------------------------------------------------
# Document root
# ---------------------------------------------------------------------------

class TestDocumentRoot:
    def test_returns_document_node(self):
        result = _map(_doc(_body()))
        assert isinstance(result, DocumentIRDocumentNode)

    def test_source_path_preserved(self):
        doc = DocxASTDocumentNode(source_path="/path/to/doc.docx")
        doc.add_child(_body())
        result = _map(doc)
        assert result.source_path == "/path/to/doc.docx"

    def test_empty_body(self):
        result = _map(_doc(_body()))
        assert result.children == []

    def test_no_body(self):
        result = _map(_doc())
        assert result.children == []


# ---------------------------------------------------------------------------
# Plain paragraphs
# ---------------------------------------------------------------------------

class TestParagraphMapping:
    def test_simple_paragraph(self):
        result = _map(_doc(_body(_para("Hello"))))
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_paragraph_text(self):
        result = _map(_doc(_body(_para("Hello world"))))
        para = result.children[0]
        assert para.children[0].content == "Hello world"

    def test_empty_paragraph_omitted(self):
        result = _map(_doc(_body(_para(""))))
        assert result.children == []

    def test_multiple_paragraphs(self):
        result = _map(_doc(_body(_para("A"), _para("B"), _para("C"))))
        assert len(result.children) == 3

    def test_bold_text(self):
        result = _map(_doc(_body(_para("Bold", bold=True))))
        span = result.children[0].children[0]
        assert isinstance(span, DocumentIRTextSpanNode)
        assert span.bold is True

    def test_italic_text(self):
        result = _map(_doc(_body(_para("Italic", italic=True))))
        span = result.children[0].children[0]
        assert span.italic is True

    def test_strikethrough_text(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTRunPropertiesNode(strike=True))
        run.add_child(DocxASTTextNode("struck"))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        span = result.children[0].children[0]
        assert span.strikethrough is True


# ---------------------------------------------------------------------------
# Heading detection — via style
# ---------------------------------------------------------------------------

class TestHeadingViaStyle:
    def test_heading1_by_style_id(self):
        styles = _styles_node(_style("Heading1", "heading 1", outline_level=0))
        result = _map(_doc(styles, _body(_para("Title", style_id="Heading1"))))
        assert isinstance(result.children[0], DocumentIRHeadingNode)
        assert result.children[0].level == 1

    def test_heading2_by_style_id(self):
        styles = _styles_node(_style("Heading2", "heading 2", outline_level=1))
        result = _map(_doc(styles, _body(_para("Section", style_id="Heading2"))))
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 2

    def test_heading_text_preserved(self):
        styles = _styles_node(_style("Heading1", "heading 1", outline_level=0))
        result = _map(_doc(styles, _body(_para("My Title", style_id="Heading1"))))
        h = result.children[0]
        assert h.children[0].content == "My Title"

    def test_heading_via_style_name_match(self):
        # Style ID is custom but name matches "heading 1"
        styles = _styles_node(_style("MyH1", "heading 1"))
        result = _map(_doc(styles, _body(_para("Title", style_id="MyH1"))))
        assert isinstance(result.children[0], DocumentIRHeadingNode)
        assert result.children[0].level == 1

    def test_heading_via_style_inheritance(self):
        # Heading1 based on Normal; child style based on Heading1
        styles = _styles_node(
            _style("Normal", "Normal", is_default=True),
            _style("Heading1", "heading 1", based_on="Normal", outline_level=0),
            _style("MyHeading", "My Heading", based_on="Heading1"),
        )
        result = _map(_doc(styles, _body(_para("Title", style_id="MyHeading"))))
        assert isinstance(result.children[0], DocumentIRHeadingNode)
        assert result.children[0].level == 1


# ---------------------------------------------------------------------------
# Heading detection — via outline_level in paragraph properties
# ---------------------------------------------------------------------------

class TestHeadingViaOutlineLevel:
    def test_outline_level_0_is_h1(self):
        result = _map(_doc(_body(_para("Title", outline_level=0))))
        assert isinstance(result.children[0], DocumentIRHeadingNode)
        assert result.children[0].level == 1

    def test_outline_level_1_is_h2(self):
        result = _map(_doc(_body(_para("Section", outline_level=1))))
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 2

    def test_outline_level_5_is_h6(self):
        result = _map(_doc(_body(_para("Deep", outline_level=5))))
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 6


# ---------------------------------------------------------------------------
# Heading detection — via direct formatting heuristic
# ---------------------------------------------------------------------------

class TestHeadingViaDirectFormatting:
    def test_bold_large_font_is_heading(self):
        # sz=40 half-points (20pt) + bold → H1
        result = _map(_doc(_body(_para("Big Title", bold=True, sz=40))))
        assert isinstance(result.children[0], DocumentIRHeadingNode)

    def test_bold_small_font_is_not_heading(self):
        # sz=24 half-points (12pt) + bold → body text
        result = _map(_doc(_body(_para("Body", bold=True, sz=24))))
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_large_font_not_bold_is_not_heading(self):
        # Large but not bold → not a heading
        result = _map(_doc(_body(_para("Large", bold=False, sz=40))))
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_bold_40pt_is_h1(self):
        result = _map(_doc(_body(_para("H1", bold=True, sz=40))))
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 1

    def test_bold_32pt_is_h2(self):
        result = _map(_doc(_body(_para("H2", bold=True, sz=32))))
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 2

    def test_bold_28pt_is_h3(self):
        result = _map(_doc(_body(_para("H3", bold=True, sz=28))))
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 3


# ---------------------------------------------------------------------------
# Code block detection
# ---------------------------------------------------------------------------

class TestCodeBlockDetection:
    def test_code_style_name(self):
        styles = _styles_node(_style("CodeBlock", "Code Block"))
        result = _map(_doc(styles, _body(_para("x = 1", style_id="CodeBlock"))))
        assert isinstance(result.children[0], DocumentIRCodeBlockNode)

    def test_code_style_keyword_in_name(self):
        styles = _styles_node(_style("Pre", "preformatted"))
        result = _map(_doc(styles, _body(_para("code here", style_id="Pre"))))
        assert isinstance(result.children[0], DocumentIRCodeBlockNode)

    def test_code_content_preserved(self):
        styles = _styles_node(_style("Code", "code"))
        result = _map(_doc(styles, _body(_para("print('hello')", style_id="Code"))))
        cb = result.children[0]
        assert cb.content == "print('hello')"

    def test_monospace_font_heuristic(self):
        result = _map(_doc(_body(_para("x = 1", font="Courier New"))))
        assert isinstance(result.children[0], DocumentIRCodeBlockNode)

    def test_consolas_font_heuristic(self):
        result = _map(_doc(_body(_para("code", font="Consolas"))))
        assert isinstance(result.children[0], DocumentIRCodeBlockNode)

    def test_non_monospace_font_is_not_code(self):
        result = _map(_doc(_body(_para("text", font="Arial"))))
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_inline_code_via_monospace_run(self):
        # A paragraph where all runs use a monospace font becomes a code block.
        # Inline code spans (code=True) apply when a run uses monospace within
        # a mixed paragraph — tested via the run mapper directly here.
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTRunPropertiesNode(font_ascii="Courier New"))
        run.add_child(DocxASTTextNode("func()"))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        # The whole paragraph is monospace → classified as a code block
        assert isinstance(result.children[0], DocumentIRCodeBlockNode)
        assert result.children[0].content == "func()"

    def test_inline_code_span_in_mixed_paragraph(self):
        # When only one run in a paragraph is monospace, the whole paragraph
        # is NOT classified as a code block.  The monospace run gets code=True.
        para = DocxASTParagraphNode()
        run1 = DocxASTRunNode()
        run1.add_child(DocxASTTextNode("Call "))
        run2 = DocxASTRunNode()
        run2.add_child(DocxASTRunPropertiesNode(font_ascii="Courier New"))
        run2.add_child(DocxASTTextNode("func()"))
        para.add_child(run1)
        para.add_child(run2)
        result = _map(_doc(_body(para)))
        # Mixed paragraph → DocumentIRParagraphNode, not code block
        assert isinstance(result.children[0], DocumentIRParagraphNode)
        spans = result.children[0].children
        assert spans[0].code is False
        assert spans[1].code is True


# ---------------------------------------------------------------------------
# Blockquote detection
# ---------------------------------------------------------------------------

class TestBlockquoteDetection:
    def test_blockquote_style_name(self):
        styles = _styles_node(_style("Blockquote", "Blockquote"))
        result = _map(_doc(styles, _body(_para("Quoted", style_id="Blockquote"))))
        assert isinstance(result.children[0], DocumentIRBlockquoteNode)

    def test_quote_style_keyword(self):
        styles = _styles_node(_style("Q", "quote"))
        result = _map(_doc(styles, _body(_para("Quoted text", style_id="Q"))))
        assert isinstance(result.children[0], DocumentIRBlockquoteNode)

    def test_blockquote_contains_paragraph(self):
        styles = _styles_node(_style("BQ", "blockquote"))
        result = _map(_doc(styles, _body(_para("Quoted", style_id="BQ"))))
        bq = result.children[0]
        assert len(bq.children) == 1
        assert isinstance(bq.children[0], DocumentIRParagraphNode)

    def test_blockquote_text(self):
        styles = _styles_node(_style("BQ", "blockquote"))
        result = _map(_doc(styles, _body(_para("Quote text", style_id="BQ"))))
        bq = result.children[0]
        assert bq.children[0].children[0].content == "Quote text"


# ---------------------------------------------------------------------------
# Unordered list grouping
# ---------------------------------------------------------------------------

class TestUnorderedListGrouping:
    def test_single_bullet_item(self):
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(numbering, _body(
            _para("Item", num_id="1", ilvl=0),
        )))
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)

    def test_multiple_bullet_items(self):
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(numbering, _body(
            _para("A", num_id="1", ilvl=0),
            _para("B", num_id="1", ilvl=0),
            _para("C", num_id="1", ilvl=0),
        )))
        ul = result.children[0]
        assert isinstance(ul, DocumentIRUnorderedListNode)
        assert len(ul.children) == 3

    def test_list_item_text(self):
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(numbering, _body(
            _para("Hello", num_id="1", ilvl=0),
        )))
        item = result.children[0].children[0]
        assert isinstance(item, DocumentIRListItemNode)
        para = item.children[0]
        assert isinstance(para, DocumentIRParagraphNode)
        assert para.children[0].content == "Hello"

    def test_list_followed_by_paragraph(self):
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(numbering, _body(
            _para("Item", num_id="1", ilvl=0),
            _para("After list"),
        )))
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)
        assert isinstance(result.children[1], DocumentIRParagraphNode)

    def test_paragraph_before_list(self):
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(numbering, _body(
            _para("Before"),
            _para("Item", num_id="1", ilvl=0),
        )))
        assert isinstance(result.children[0], DocumentIRParagraphNode)
        assert isinstance(result.children[1], DocumentIRUnorderedListNode)

    def test_nested_list(self):
        # Build numbering with two levels
        numbering = DocxASTNumberingNode()
        abstract = DocxASTAbstractNumNode(abstract_num_id="0")
        for i, fmt in enumerate(["bullet", "bullet"]):
            lvl = DocxASTNumLevelNode(ilvl=i, num_fmt=fmt, start=1, lvl_text="\u2022")
            abstract.add_child(lvl)
        numbering.add_child(abstract)
        numbering.add_child(DocxASTNumNode(num_id="1", abstract_num_id="0"))

        result = _map(_doc(numbering, _body(
            _para("Outer", num_id="1", ilvl=0),
            _para("Inner", num_id="1", ilvl=1),
            _para("Outer 2", num_id="1", ilvl=0),
        )))

        ul = result.children[0]
        assert isinstance(ul, DocumentIRUnorderedListNode)
        # Should have 2 top-level items
        assert len(ul.children) == 2
        # First item should contain a nested list
        first_item = ul.children[0]
        nested_lists = [c for c in first_item.children if isinstance(c, DocumentIRUnorderedListNode)]
        assert len(nested_lists) == 1


def _style_with_num(
    style_id: str,
    name: str,
    num_id: str,
    ilvl: int = 0,
    based_on: str = "",
) -> DocxASTStyleNode:
    """Build a style node that carries numbering properties in its pPr."""
    node = DocxASTStyleNode(
        style_type="paragraph",
        style_id=style_id,
        name=name,
        based_on=based_on or None,
    )
    ppr = DocxASTParagraphPropertiesNode()
    ppr.add_child(DocxASTNumberingPropertiesNode(num_id=num_id, ilvl=ilvl))
    node.add_child(ppr)
    return node


class TestStyleBasedNumbering:
    """Tests for list detection when numPr comes from the paragraph style."""

    def test_bullet_via_style_numpr(self):
        """A paragraph using a style that carries numPr is detected as a list item."""
        styles = _styles_node(_style_with_num("ListBullet", "List Bullet", num_id="1"))
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(styles, numbering, _body(
            _para("Item A", style_id="ListBullet"),
            _para("Item B", style_id="ListBullet"),
        )))
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)
        assert len(result.children[0].children) == 2

    def test_single_bullet_via_style_numpr(self):
        styles = _styles_node(_style_with_num("ListBullet", "List Bullet", num_id="1"))
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(styles, numbering, _body(
            _para("Only item", style_id="ListBullet"),
        )))
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)

    def test_ordered_via_style_numpr(self):
        styles = _styles_node(_style_with_num("ListNumber", "List Number", num_id="1"))
        numbering = _numbering_node(num_id="1", num_fmt="decimal")
        result = _map(_doc(styles, numbering, _body(
            _para("First", style_id="ListNumber"),
            _para("Second", style_id="ListNumber"),
        )))
        assert isinstance(result.children[0], DocumentIROrderedListNode)
        assert len(result.children[0].children) == 2

    def test_style_numpr_inherited_via_based_on(self):
        """A child style that inherits from a style with numPr also gets numbering."""
        styles = _styles_node(
            _style_with_num("ListBullet", "List Bullet", num_id="1"),
            _style("MyBullet", "My Bullet", based_on="ListBullet"),
        )
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(styles, numbering, _body(
            _para("Item", style_id="MyBullet"),
        )))
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)

    def test_direct_numpr_takes_precedence_over_style(self):
        """When both the paragraph and its style carry numPr, the direct one wins."""
        styles = _styles_node(_style_with_num("ListBullet", "List Bullet", num_id="2"))
        # numbering 1 = bullet, numbering 2 = ordered
        numbering = DocxASTNumberingNode()
        abstract = DocxASTAbstractNumNode(abstract_num_id="0")
        abstract.add_child(DocxASTNumLevelNode(ilvl=0, num_fmt="bullet", start=1, lvl_text="\u2022"))
        numbering.add_child(abstract)
        abstract2 = DocxASTAbstractNumNode(abstract_num_id="1")
        abstract2.add_child(DocxASTNumLevelNode(ilvl=0, num_fmt="decimal", start=1, lvl_text="%1."))
        numbering.add_child(abstract2)
        numbering.add_child(DocxASTNumNode(num_id="1", abstract_num_id="0"))
        numbering.add_child(DocxASTNumNode(num_id="2", abstract_num_id="1"))

        result = _map(_doc(styles, numbering, _body(
            _para("Item", style_id="ListBullet", num_id="1", ilvl=0),
        )))
        # Direct numId="1" (bullet) should win over style's numId="2" (ordered)
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)

    def test_style_numpr_followed_by_plain_paragraph(self):
        styles = _styles_node(_style_with_num("ListBullet", "List Bullet", num_id="1"))
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(styles, numbering, _body(
            _para("Item", style_id="ListBullet"),
            _para("After list"),
        )))
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)
        assert isinstance(result.children[1], DocumentIRParagraphNode)

    def test_style_numpr_item_text(self):
        styles = _styles_node(_style_with_num("ListBullet", "List Bullet", num_id="1"))
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(styles, numbering, _body(
            _para("Hello", style_id="ListBullet"),
        )))
        item = result.children[0].children[0]
        assert isinstance(item, DocumentIRListItemNode)
        para = item.children[0]
        assert isinstance(para, DocumentIRParagraphNode)
        assert para.children[0].content == "Hello"

    def test_mixed_style_and_direct_numpr_in_one_list(self):
        """Paragraphs with direct numPr and style-based numPr using the same
        numId should be grouped into a single list."""
        styles = _styles_node(_style_with_num("ListBullet", "List Bullet", num_id="1"))
        numbering = _numbering_node(num_id="1", num_fmt="bullet")
        result = _map(_doc(styles, numbering, _body(
            _para("Direct", num_id="1", ilvl=0),
            _para("Via style", style_id="ListBullet"),
        )))
        assert isinstance(result.children[0], DocumentIRUnorderedListNode)
        assert len(result.children[0].children) == 2


class TestNonContiguousListLevels:
    """Tests for nesting when ilvl values skip levels (e.g. 0 then 2)."""

    def _three_level_numbering(self) -> DocxASTNumberingNode:
        numbering = DocxASTNumberingNode()
        abstract = DocxASTAbstractNumNode(abstract_num_id="0")
        for i in range(3):
            abstract.add_child(DocxASTNumLevelNode(
                ilvl=i, num_fmt="bullet", start=1, lvl_text="\u2022",
            ))
        numbering.add_child(abstract)
        numbering.add_child(DocxASTNumNode(num_id="1", abstract_num_id="0"))
        return numbering

    def test_skip_from_0_to_2(self):
        """Jumping from ilvl=0 to ilvl=2 nests one level, placing L2 as a
        direct child of L0's item since intermediate levels can't be
        represented in a list tree."""
        numbering = self._three_level_numbering()
        result = _map(_doc(numbering, _body(
            _para("Level 0", num_id="1", ilvl=0),
            _para("Level 2", num_id="1", ilvl=2),
            _para("Back to 0", num_id="1", ilvl=0),
        )))
        ul = result.children[0]
        assert isinstance(ul, DocumentIRUnorderedListNode)
        assert len(ul.children) == 2  # L0 item and "Back to 0" item
        first_item = ul.children[0]
        # first_item should contain one nested list for the L2 content
        nested1 = [c for c in first_item.children if isinstance(c, DocumentIRUnorderedListNode)]
        assert len(nested1) == 1
        # The nested list contains one item for L2
        assert len(nested1[0].children) == 1
        assert isinstance(nested1[0].children[0], DocumentIRListItemNode)

    def test_start_at_non_zero_ilvl(self):
        """A list that starts at ilvl=1 (no ilvl=0) should still work."""
        numbering = self._three_level_numbering()
        result = _map(_doc(numbering, _body(
            _para("Item A", num_id="1", ilvl=1),
            _para("Item B", num_id="1", ilvl=1),
        )))
        ul = result.children[0]
        assert isinstance(ul, DocumentIRUnorderedListNode)
        assert len(ul.children) == 2

    def test_pop_to_non_contiguous_level(self):
        """Going 0, 2, 1: L1 pops out of the L2 sublist and becomes a
        second child list of the L0 item, since there is no ilvl=1
        list on the stack to rejoin."""
        numbering = self._three_level_numbering()
        result = _map(_doc(numbering, _body(
            _para("L0", num_id="1", ilvl=0),
            _para("L2", num_id="1", ilvl=2),
            _para("L1", num_id="1", ilvl=1),
        )))
        ul = result.children[0]
        assert isinstance(ul, DocumentIRUnorderedListNode)
        assert len(ul.children) == 1
        first_item = ul.children[0]
        # L0 item contains two child lists: one for L2, one for L1
        nested1 = [c for c in first_item.children if isinstance(c, DocumentIRUnorderedListNode)]
        assert len(nested1) == 2


# ---------------------------------------------------------------------------
# Ordered list grouping
# ---------------------------------------------------------------------------

class TestOrderedListGrouping:
    def test_ordered_list_node(self):
        numbering = _numbering_node(num_id="1", num_fmt="decimal", start=1)
        result = _map(_doc(numbering, _body(
            _para("First", num_id="1", ilvl=0),
        )))
        assert isinstance(result.children[0], DocumentIROrderedListNode)

    def test_ordered_list_start(self):
        numbering = _numbering_node(num_id="1", num_fmt="decimal", start=3)
        result = _map(_doc(numbering, _body(
            _para("Third", num_id="1", ilvl=0),
        )))
        assert result.children[0].start == 3

    def test_lower_letter_is_ordered(self):
        numbering = _numbering_node(num_id="1", num_fmt="lowerLetter")
        result = _map(_doc(numbering, _body(
            _para("a", num_id="1", ilvl=0),
        )))
        assert isinstance(result.children[0], DocumentIROrderedListNode)

    def test_lower_roman_is_ordered(self):
        numbering = _numbering_node(num_id="1", num_fmt="lowerRoman")
        result = _map(_doc(numbering, _body(
            _para("i", num_id="1", ilvl=0),
        )))
        assert isinstance(result.children[0], DocumentIROrderedListNode)

    def test_multiple_ordered_items(self):
        numbering = _numbering_node(num_id="1", num_fmt="decimal")
        result = _map(_doc(numbering, _body(
            _para("One", num_id="1", ilvl=0),
            _para("Two", num_id="1", ilvl=0),
        )))
        ol = result.children[0]
        assert len(ol.children) == 2


# ---------------------------------------------------------------------------
# Numbered headings
# ---------------------------------------------------------------------------

class TestNumberedHeadings:
    def test_heading_with_numpr_is_not_a_list(self):
        styles = _styles_node(_style("heading1", "heading 1", outline_level=0))
        numbering = _numbering_node(num_id="1", num_fmt="decimal")
        result = _map(_doc(styles, numbering, _body(
            _para("Introduction", style_id="heading1", num_id="1", ilvl=0),
        )))
        assert isinstance(result.children[0], DocumentIRHeadingNode)

    def test_heading_with_numpr_correct_level(self):
        styles = _styles_node(_style("heading1", "heading 1", outline_level=0))
        numbering = _numbering_node(num_id="1", num_fmt="decimal")
        result = _map(_doc(styles, numbering, _body(
            _para("Introduction", style_id="heading1", num_id="1", ilvl=0),
        )))
        assert result.children[0].level == 1

    def test_heading_with_numpr_correct_text(self):
        styles = _styles_node(_style("heading1", "heading 1", outline_level=0))
        numbering = _numbering_node(num_id="1", num_fmt="decimal")
        result = _map(_doc(styles, numbering, _body(
            _para("Introduction", style_id="heading1", num_id="1", ilvl=0),
        )))
        assert result.children[0].children[0].content == "Introduction"

    def test_heading_does_not_break_adjacent_list(self):
        styles = _styles_node(_style("heading1", "heading 1", outline_level=0))
        numbering = _numbering_node(num_id="1", num_fmt="decimal")
        result = _map(_doc(styles, numbering, _body(
            _para("Section", style_id="heading1", num_id="1", ilvl=0),
            _para("Item one", num_id="1", ilvl=0),
            _para("Item two", num_id="1", ilvl=0),
        )))
        assert isinstance(result.children[0], DocumentIRHeadingNode)
        assert isinstance(result.children[1], DocumentIROrderedListNode)
        assert len(result.children[1].children) == 2


# ---------------------------------------------------------------------------
# Table mapping
# ---------------------------------------------------------------------------

def _simple_table(rows: list, header_row: bool = False) -> DocxASTTableNode:
    """Build a table from a list of lists of cell texts."""
    table = DocxASTTableNode()
    for row_idx, row_texts in enumerate(rows):
        row = DocxASTTableRowNode()
        if row_idx == 0 and header_row:
            rpr = DocxASTTableRowPropertiesNode(is_header=True)
            row.add_child(rpr)
        for text in row_texts:
            cell = DocxASTTableCellNode()
            cell.add_child(DocxASTTableCellPropertiesNode())
            para = DocxASTParagraphNode()
            run = DocxASTRunNode()
            run.add_child(DocxASTTextNode(text))
            para.add_child(run)
            cell.add_child(para)
            row.add_child(cell)
        table.add_child(row)
    return table


class TestTableMapping:
    def test_table_node(self):
        result = _map(_doc(_body(_simple_table([["A", "B"]]))))
        assert isinstance(result.children[0], DocumentIRTableNode)

    def test_table_body_present(self):
        result = _map(_doc(_body(_simple_table([["A", "B"]]))))
        table = result.children[0]
        bodies = [c for c in table.children if isinstance(c, DocumentIRTableBodyNode)]
        assert len(bodies) == 1

    def test_table_with_header_row(self):
        result = _map(_doc(_body(_simple_table([["H1", "H2"], ["D1", "D2"]], header_row=True))))
        table = result.children[0]
        headers = [c for c in table.children if isinstance(c, DocumentIRTableHeaderNode)]
        bodies = [c for c in table.children if isinstance(c, DocumentIRTableBodyNode)]
        assert len(headers) == 1
        assert len(bodies) == 1

    def test_header_row_count(self):
        result = _map(_doc(_body(_simple_table([["H1", "H2"], ["D1", "D2"]], header_row=True))))
        table = result.children[0]
        header = next(c for c in table.children if isinstance(c, DocumentIRTableHeaderNode))
        assert len(header.children) == 1  # one header row

    def test_body_row_count(self):
        result = _map(_doc(_body(_simple_table([["H1"], ["D1"], ["D2"]], header_row=True))))
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        assert len(body.children) == 2  # two body rows

    def test_cell_count(self):
        result = _map(_doc(_body(_simple_table([["A", "B", "C"]]))))
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        row = body.children[0]
        assert len(row.children) == 3

    def test_cell_text(self):
        result = _map(_doc(_body(_simple_table([["Hello", "World"]]))))
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        cell = body.children[0].children[0]
        assert isinstance(cell, DocumentIRTableCellNode)
        para = cell.children[0]
        assert para.children[0].content == "Hello"

    def test_table_row_node(self):
        result = _map(_doc(_body(_simple_table([["A", "B"]]))))
        table = result.children[0]
        body = next(c for c in table.children if isinstance(c, DocumentIRTableBodyNode))
        assert isinstance(body.children[0], DocumentIRTableRowNode)

    def test_cell_alignment_from_justification(self):
        table = DocxASTTableNode()
        row = DocxASTTableRowNode()
        cell = DocxASTTableCellNode()
        extra_cell = DocxASTTableCellNode()
        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(justification="center")
        para.add_child(ppr)
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode("Centered"))
        para.add_child(run)
        cell.add_child(para)
        extra_para = DocxASTParagraphNode()
        extra_run = DocxASTRunNode()
        extra_run.add_child(DocxASTTextNode("Other"))
        extra_para.add_child(extra_run)
        extra_cell.add_child(extra_para)
        row.add_child(cell)
        row.add_child(extra_cell)
        table.add_child(row)

        result = _map(_doc(_body(table)))
        ir_table = result.children[0]
        body = next(c for c in ir_table.children if isinstance(c, DocumentIRTableBodyNode))
        ir_cell = body.children[0].children[0]
        assert ir_cell.alignment == "center"

    def test_no_header_rows_all_go_to_body(self):
        result = _map(_doc(_body(_simple_table([["A"], ["B"]]))))
        table = result.children[0]
        headers = [c for c in table.children if isinstance(c, DocumentIRTableHeaderNode)]
        bodies = [c for c in table.children if isinstance(c, DocumentIRTableBodyNode)]
        assert len(headers) == 0
        assert len(bodies) == 1
        assert len(bodies[0].children) == 2

    def test_single_cell_table_with_text_is_unwrapped(self):
        result = _map(_doc(_body(_simple_table([["Hello"]]))))
        assert len(result.children) == 1
        assert isinstance(result.children[0], DocumentIRParagraphNode)
        assert result.children[0].children[0].content == "Hello"

    def test_single_cell_table_with_image_is_unwrapped(self):
        table = DocxASTTableNode()
        row = DocxASTTableRowNode()
        cell = DocxASTTableCellNode()
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        drawing = DocxASTDrawingNode(
            relationship_id="rId1",
            resolved_path=None,
            description="logo",
            width_emu=None,
            height_emu=None,
            image_data=None,
        )
        run.add_child(drawing)
        para.add_child(run)
        cell.add_child(para)
        row.add_child(cell)
        table.add_child(row)
        result = _map(_doc(_body(table)))
        assert len(result.children) == 0

    def test_single_cell_table_two_rows_is_not_unwrapped(self):
        result = _map(_doc(_body(_simple_table([["A"], ["B"]]))))
        assert isinstance(result.children[0], DocumentIRTableNode)


# ---------------------------------------------------------------------------
# Inline content
# ---------------------------------------------------------------------------

class TestInlineContent:
    def test_tab_becomes_tab_character(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode("Before"))
        run.add_child(DocxASTTabNode())
        run.add_child(DocxASTTextNode("After"))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        children = result.children[0].children
        assert children[0].content == "Before"
        assert children[1].content == "\t"
        assert children[2].content == "After"

    def test_line_break_preserved(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode("Line 1"))
        run.add_child(DocxASTBreakNode(break_type="textWrapping"))
        run.add_child(DocxASTTextNode("Line 2"))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        children = result.children[0].children
        assert isinstance(children[1], DocumentIRLineBreakNode)

    def test_page_break_dropped(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTBreakNode(break_type="page"))
        run.add_child(DocxASTTextNode("After page"))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        # Page break dropped; only text span remains
        children = result.children[0].children
        assert len(children) == 1
        assert children[0].content == "After page"

    def test_last_rendered_page_break_dropped(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTLastRenderedPageBreakNode())
        run.add_child(DocxASTTextNode("Text"))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        children = result.children[0].children
        assert len(children) == 1
        assert children[0].content == "Text"

    def test_empty_text_omitted(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode(""))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        assert result.children == []

    def test_drawing_becomes_image(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTDrawingNode(
            resolved_path="word/media/image1.png",
            description="A photo",
            image_data=b"fakepng",
        ))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        para_node = result.children[0]
        img = para_node.children[0]
        assert isinstance(img, DocumentIRImageNode)
        assert img.url == "word/media/image1.png"
        assert img.alt_text == "A photo"
        assert img.data == b"fakepng"

    def test_drawing_without_path_uses_rel_id(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTDrawingNode(
            relationship_id="rId6",
            resolved_path=None,
            image_data=b"fakejpeg",
        ))
        para.add_child(run)
        result = _map(_doc(_body(para)))
        img = result.children[0].children[0]
        assert isinstance(img, DocumentIRImageNode)
        assert img.url == "rId6"
        assert img.data == b"fakejpeg"

    def test_drawing_without_any_url_omitted(self):
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTDrawingNode())
        para.add_child(run)
        result = _map(_doc(_body(para)))
        assert result.children == []

    def test_multiple_runs_in_paragraph(self):
        para = DocxASTParagraphNode()
        for text in ["Hello ", "world", "!"]:
            run = DocxASTRunNode()
            run.add_child(DocxASTTextNode(text))
            para.add_child(run)
        result = _map(_doc(_body(para)))
        spans = result.children[0].children
        assert len(spans) == 3
        assert "".join(s.content for s in spans) == "Hello world!"


# ---------------------------------------------------------------------------
# Style inheritance edge cases
# ---------------------------------------------------------------------------

class TestStyleInheritance:
    def test_style_not_found_falls_back_to_paragraph(self):
        # No styles node — unknown style_id → paragraph
        result = _map(_doc(_body(_para("Text", style_id="UnknownStyle"))))
        assert isinstance(result.children[0], DocumentIRParagraphNode)

    def test_deep_inheritance_chain(self):
        # A → B → C where C is heading 3
        styles = _styles_node(
            _style("A", "heading 3", outline_level=2),
            _style("B", "B Style", based_on="A"),
            _style("C", "C Style", based_on="B"),
        )
        result = _map(_doc(styles, _body(_para("Deep", style_id="C"))))
        h = result.children[0]
        assert isinstance(h, DocumentIRHeadingNode)
        assert h.level == 3

    def test_cycle_guard(self):
        # Circular basedOn reference should not crash
        styles = _styles_node(
            _style("A", "A Style", based_on="B"),
            _style("B", "B Style", based_on="A"),
        )
        # Should not raise
        result = _map(_doc(styles, _body(_para("Cyclic", style_id="A"))))
        assert result is not None
