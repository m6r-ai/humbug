"""Tests for the document_ir → DOCX AST mapper."""

from docx import document_ir_to_docx_ast
from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
    DocxASTBodyNode,
    DocxASTBreakNode,
    DocxASTDocumentNode,
    DocxASTDrawingNode,
    DocxASTHyperlinkNode,
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
    DocxASTTableCellNode,
    DocxASTTableCellPropertiesNode,
    DocxASTTableNode,
    DocxASTTablePropertiesNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTextNode,
)
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


def _para(*spans) -> DocumentIRParagraphNode:
    p = DocumentIRParagraphNode()
    for s in spans:
        p.add_child(s)
    return p


def _span(text: str, bold=False, italic=False, strike=False, code=False) -> DocumentIRTextSpanNode:
    return DocumentIRTextSpanNode(
        content=text, bold=bold, italic=italic, strikethrough=strike, code=code
    )


def _heading(level: int, *spans) -> DocumentIRHeadingNode:
    h = DocumentIRHeadingNode(level=level)
    for s in spans:
        h.add_child(s)
    return h


def _map(doc: DocumentIRDocumentNode) -> DocxASTDocumentNode:
    return document_ir_to_docx_ast(doc)


def _body(doc: DocxASTDocumentNode) -> DocxASTBodyNode:
    return next(c for c in doc.children if isinstance(c, DocxASTBodyNode))


def _styles(doc: DocxASTDocumentNode) -> DocxASTStylesNode:
    return next(c for c in doc.children if isinstance(c, DocxASTStylesNode))


def _numbering(doc: DocxASTDocumentNode) -> DocxASTNumberingNode:
    return next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))


def _first_para(doc: DocxASTDocumentNode) -> DocxASTParagraphNode:
    return next(c for c in _body(doc).children if isinstance(c, DocxASTParagraphNode))


def _ppr(para: DocxASTParagraphNode) -> DocxASTParagraphPropertiesNode:
    return next(c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode))


def _runs(para: DocxASTParagraphNode):
    return [c for c in para.children if isinstance(c, DocxASTRunNode)]


def _text_of(para: DocxASTParagraphNode) -> str:
    parts = []
    for run in _runs(para):
        for child in run.children:
            if isinstance(child, DocxASTTextNode):
                parts.append(child.content)
    return "".join(parts)


def _content_paras(doc: DocxASTDocumentNode) -> list:
    """Return body paragraphs that are not zero-height spacers.

    Spacer paragraphs have both spacing_before=0 and spacing_after=0 set
    explicitly; content paragraphs either have no spacing override or only
    one of the two set.
    """
    def _is_spacer(para: DocxASTParagraphNode) -> bool:
        ppr_node = next((c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode)), None)
        return (ppr_node is not None
                and ppr_node.spacing_before == 0
                and ppr_node.spacing_after == 0)
    return [c for c in _body(doc).children
            if isinstance(c, DocxASTParagraphNode) and not _is_spacer(c)]


# ---------------------------------------------------------------------------
# Document structure
# ---------------------------------------------------------------------------

class TestDocumentStructure:
    def test_returns_document_node(self):
        result = _map(_doc())
        assert isinstance(result, DocxASTDocumentNode)

    def test_source_path_preserved(self):
        doc = DocumentIRDocumentNode(source_path="/path/to/file.md")
        result = _map(doc)
        assert result.source_path == "/path/to/file.md"

    def test_always_has_styles(self):
        result = _map(_doc())
        styles_nodes = [c for c in result.children if isinstance(c, DocxASTStylesNode)]
        assert len(styles_nodes) == 1

    def test_always_has_numbering(self):
        result = _map(_doc())
        num_nodes = [c for c in result.children if isinstance(c, DocxASTNumberingNode)]
        assert len(num_nodes) == 1

    def test_always_has_body(self):
        result = _map(_doc())
        bodies = [c for c in result.children if isinstance(c, DocxASTBodyNode)]
        assert len(bodies) == 1

    def test_empty_document_empty_body(self):
        result = _map(_doc())
        assert _body(result).children == []


# ---------------------------------------------------------------------------
# Built-in styles
# ---------------------------------------------------------------------------

class TestBuiltInStyles:
    def test_normal_style_present(self):
        result = _map(_doc())
        style_ids = [c.style_id for c in _styles(result).children
                     if isinstance(c, DocxASTStyleNode)]
        assert "Normal" in style_ids

    def test_normal_style_is_default(self):
        result = _map(_doc())
        normal = next(c for c in _styles(result).children
                      if isinstance(c, DocxASTStyleNode) and c.style_id == "Normal")
        assert normal.is_default is True

    def test_heading_styles_present(self):
        result = _map(_doc())
        style_ids = {c.style_id for c in _styles(result).children
                     if isinstance(c, DocxASTStyleNode)}
        for i in range(1, 7):
            assert f"Heading{i}" in style_ids

    def test_code_block_style_present(self):
        result = _map(_doc())
        style_ids = {c.style_id for c in _styles(result).children
                     if isinstance(c, DocxASTStyleNode)}
        assert "CodeBlock" in style_ids

    def test_blockquote_style_present(self):
        result = _map(_doc())
        style_ids = {c.style_id for c in _styles(result).children
                     if isinstance(c, DocxASTStyleNode)}
        assert "Blockquote" in style_ids

    def test_heading1_has_outline_level_0(self):
        result = _map(_doc())
        h1 = next(c for c in _styles(result).children
                  if isinstance(c, DocxASTStyleNode) and c.style_id == "Heading1")
        ppr = next((c for c in h1.children if isinstance(c, DocxASTParagraphPropertiesNode)), None)
        assert ppr is not None
        assert ppr.outline_level == 0

    def test_heading6_has_outline_level_5(self):
        result = _map(_doc())
        h6 = next(c for c in _styles(result).children
                  if isinstance(c, DocxASTStyleNode) and c.style_id == "Heading6")
        ppr = next((c for c in h6.children if isinstance(c, DocxASTParagraphPropertiesNode)), None)
        assert ppr.outline_level == 5


# ---------------------------------------------------------------------------
# Built-in numbering
# ---------------------------------------------------------------------------

class TestBuiltInNumbering:
    def test_two_abstract_nums(self):
        result = _map(_doc())
        num = _numbering(result)
        abstract = [c for c in num.children if isinstance(c, DocxASTAbstractNumNode)]
        assert len(abstract) == 2

    def test_empty_doc_has_one_num_instance(self):
        # Only the bullet instance is pre-created; ordered instances are
        # allocated dynamically when ordered lists are encountered.
        result = _map(_doc())
        num = _numbering(result)
        instances = [c for c in num.children if isinstance(c, DocxASTNumNode)]
        assert len(instances) == 1
        assert instances[0].num_id == "1"

    def test_each_ordered_list_gets_own_num_instance(self):
        # Two separate ordered lists must each get a distinct numId so Word
        # resets the counter independently for each.
        def _ol():
            ol = DocumentIROrderedListNode(start=1)
            item = DocumentIRListItemNode()
            item.add_child(_para(_span("item")))
            ol.add_child(item)
            return ol
        result = _map(_doc(_ol(), _ol()))
        num = _numbering(result)
        instances = [c for c in num.children if isinstance(c, DocxASTNumNode)]
        ordered = [i for i in instances if i.abstract_num_id == "1"]
        assert len(ordered) == 2
        assert ordered[0].num_id != ordered[1].num_id

    def test_bullet_abstract_has_levels(self):
        result = _map(_doc())
        num = _numbering(result)
        bullet = next(c for c in num.children
                      if isinstance(c, DocxASTAbstractNumNode) and c.abstract_num_id == "0")
        levels = [c for c in bullet.children if isinstance(c, DocxASTNumLevelNode)]
        assert len(levels) == 9  # _MAX_LIST_DEPTH

    def test_bullet_level_0_is_bullet(self):
        result = _map(_doc())
        num = _numbering(result)
        bullet = next(c for c in num.children
                      if isinstance(c, DocxASTAbstractNumNode) and c.abstract_num_id == "0")
        lvl0 = next(c for c in bullet.children
                    if isinstance(c, DocxASTNumLevelNode) and c.ilvl == 0)
        assert lvl0.num_fmt == "bullet"

    def test_ordered_abstract_level_0_is_decimal(self):
        result = _map(_doc())
        num = _numbering(result)
        ordered = next(c for c in num.children
                       if isinstance(c, DocxASTAbstractNumNode) and c.abstract_num_id == "1")
        lvl0 = next(c for c in ordered.children
                    if isinstance(c, DocxASTNumLevelNode) and c.ilvl == 0)
        assert lvl0.num_fmt == "decimal"

    def test_ordered_abstract_all_levels_decimal(self):
        result = _map(_doc())
        num = _numbering(result)
        ordered = next(c for c in num.children
                       if isinstance(c, DocxASTAbstractNumNode) and c.abstract_num_id == "1")
        levels = [c for c in ordered.children if isinstance(c, DocxASTNumLevelNode)]
        for lvl in levels:
            assert lvl.num_fmt == "decimal"

    def test_ordered_abstract_lvl_text_references_own_level(self):
        result = _map(_doc())
        num = _numbering(result)
        ordered = next(c for c in num.children
                       if isinstance(c, DocxASTAbstractNumNode) and c.abstract_num_id == "1")
        levels = [c for c in ordered.children if isinstance(c, DocxASTNumLevelNode)]
        for lvl in levels:
            assert lvl.lvl_text == f"%{lvl.ilvl + 1}."


# ---------------------------------------------------------------------------
# Paragraph mapping
# ---------------------------------------------------------------------------

class TestParagraphMapping:
    def test_paragraph_style_normal(self):
        result = _map(_doc(_para(_span("Hello"))))
        assert _ppr(_first_para(result)).style_id == "Normal"

    def test_paragraph_text(self):
        result = _map(_doc(_para(_span("Hello world"))))
        assert _text_of(_first_para(result)) == "Hello world"

    def test_empty_paragraph_omitted(self):
        result = _map(_doc(_para()))
        assert _body(result).children == []

    def test_multiple_paragraphs(self):
        result = _map(_doc(_para(_span("A")), _para(_span("B"))))
        paras = [c for c in _body(result).children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 2

    def test_bold_span(self):
        result = _map(_doc(_para(_span("Bold", bold=True))))
        run = _runs(_first_para(result))[0]
        rpr = next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.bold is True

    def test_italic_span(self):
        result = _map(_doc(_para(_span("Italic", italic=True))))
        run = _runs(_first_para(result))[0]
        rpr = next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.italic is True

    def test_strikethrough_span(self):
        result = _map(_doc(_para(_span("Struck", strike=True))))
        run = _runs(_first_para(result))[0]
        rpr = next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.strike is True

    def test_code_span_uses_courier(self):
        result = _map(_doc(_para(_span("func()", code=True))))
        run = _runs(_first_para(result))[0]
        rpr = next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.font_ascii == "Courier New"

    def test_plain_span_no_rpr(self):
        result = _map(_doc(_para(_span("plain"))))
        run = _runs(_first_para(result))[0]
        rprs = [c for c in run.children if isinstance(c, DocxASTRunPropertiesNode)]
        assert len(rprs) == 0

    def test_consecutive_same_format_runs_merged(self):
        result = _map(_doc(_para(_span("Hello "), _span("world"))))
        runs = _runs(_first_para(result))
        assert len(runs) == 1
        assert _text_of(_first_para(result)) == "Hello world"

    def test_different_format_runs_not_merged(self):
        result = _map(_doc(_para(_span("plain"), _span("bold", bold=True))))
        runs = _runs(_first_para(result))
        assert len(runs) == 2


# ---------------------------------------------------------------------------
# Heading mapping
# ---------------------------------------------------------------------------

class TestHeadingMapping:
    def test_heading1_style(self):
        result = _map(_doc(_heading(1, _span("Title"))))
        para = _first_para(result)
        assert _ppr(para).style_id == "Heading1"

    def test_heading3_style(self):
        result = _map(_doc(_heading(3, _span("Section"))))
        para = _first_para(result)
        assert _ppr(para).style_id == "Heading3"

    def test_heading6_style(self):
        result = _map(_doc(_heading(6, _span("Deep"))))
        para = _first_para(result)
        assert _ppr(para).style_id == "Heading6"

    def test_heading_text(self):
        result = _map(_doc(_heading(2, _span("My Heading"))))
        assert _text_of(_first_para(result)) == "My Heading"

    def test_heading_with_bold_span(self):
        result = _map(_doc(_heading(1, _span("Bold", bold=True))))
        run = _runs(_first_para(result))[0]
        rpr = next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.bold is True


# ---------------------------------------------------------------------------
# Code block mapping
# ---------------------------------------------------------------------------

class TestCodeBlockMapping:
    def test_single_line_code(self):
        result = _map(_doc(DocumentIRCodeBlockNode(language="python", content="x = 1")))
        body = _body(result)
        paras = _content_paras(result)
        assert len(paras) == 1
        assert _ppr(paras[0]).style_id == "CodeBlock"

    def test_multiline_code_one_para_per_line(self):
        result = _map(_doc(DocumentIRCodeBlockNode(language="", content="line1\nline2\nline3")))
        body = _body(result)
        paras = _content_paras(result)
        assert len(paras) == 3

    def test_code_content_preserved(self):
        result = _map(_doc(DocumentIRCodeBlockNode(language="", content="print('hello')")))
        body = _body(result)
        para = next(c for c in body.children if isinstance(c, DocxASTParagraphNode))
        assert _text_of(para) == "print('hello')"

    def test_empty_code_block_emits_one_para(self):
        result = _map(_doc(DocumentIRCodeBlockNode(language="", content="")))
        body = _body(result)
        paras = _content_paras(result)
        assert len(paras) == 1

    def test_trailing_newline_not_extra_para(self):
        result = _map(_doc(DocumentIRCodeBlockNode(language="", content="code\n")))
        body = _body(result)
        paras = _content_paras(result)
        assert len(paras) == 1

    def test_code_block_last_line_has_standard_spacing(self):
        result = _map(_doc(DocumentIRCodeBlockNode(language="", content="x = 1")))
        body_children = list(_body(result).children)
        last_code = next(c for c in reversed(body_children)
                         if isinstance(c, DocxASTParagraphNode)
                         and _ppr(c).style_id == "CodeBlock")
        assert _ppr(last_code).spacing_after == 200

    def test_code_block_mid_list_item_has_standard_spacing(self):
        # Code block followed by more content within the same item: last code
        # line gets spacing_after=200 to separate it from the continuation.
        item = DocumentIRListItemNode()
        item.add_child(_para(_span("Intro")))
        item.add_child(DocumentIRCodeBlockNode(language="", content="x = 1"))
        item.add_child(_para(_span("Continuation")))
        ul = DocumentIRUnorderedListNode()
        ul.add_child(item)
        result = _map(_doc(ul))
        body_children = list(_body(result).children)
        code_para = next(c for c in body_children
                         if isinstance(c, DocxASTParagraphNode)
                         and _ppr(c).style_id == "CodeBlock")
        assert _ppr(code_para).spacing_after == 200

    def test_code_block_at_end_of_tight_list_item_has_standard_spacing(self):
        # Code block as the last child of a tight list item: last line gets
        # spacing_after=200 via _apply_list_trailing_spacing.
        item = DocumentIRListItemNode()
        item.add_child(_para(_span("Intro")))
        item.add_child(DocumentIRCodeBlockNode(language="", content="x = 1"))
        ul = DocumentIRUnorderedListNode()
        ul.add_child(item)
        result = _map(_doc(ul))
        body_children = list(_body(result).children)
        last_code = next(c for c in reversed(body_children)
                         if isinstance(c, DocxASTParagraphNode)
                         and _ppr(c).style_id == "CodeBlock")
        assert _ppr(last_code).spacing_after == 200

    def test_code_block_at_end_of_loose_list_item_has_standard_spacing(self):
        # Code block as the last child of a loose list item: last line gets
        # spacing_after=200 via _apply_list_trailing_spacing.
        item = DocumentIRListItemNode()
        item.add_child(_para(_span("Intro")))
        item.add_child(DocumentIRCodeBlockNode(language="", content="x = 1"))
        ul = DocumentIRUnorderedListNode(tight=False)
        ul.add_child(item)
        result = _map(_doc(ul))
        body_children = list(_body(result).children)
        last_code = next(c for c in reversed(body_children)
                         if isinstance(c, DocxASTParagraphNode)
                         and _ppr(c).style_id == "CodeBlock")
        assert _ppr(last_code).spacing_after == 200


# ---------------------------------------------------------------------------
# Blockquote mapping
# ---------------------------------------------------------------------------

class TestBlockquoteMapping:
    def test_blockquote_style(self):
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quoted")))
        result = _map(_doc(bq))
        # Blockquote maps to a table wrapping a shaded cell; paragraphs live
        # inside the cell, not directly in the body.
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in cell.children if isinstance(c, DocxASTParagraphNode))
        assert _ppr(para).style_id == "Blockquote"

    def test_blockquote_text(self):
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quote text")))
        result = _map(_doc(bq))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in cell.children if isinstance(c, DocxASTParagraphNode))
        assert _text_of(para) == "Quote text"

    def test_blockquote_shading(self):
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quoted")))
        result = _map(_doc(bq))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        tcp = next(c for c in cell.children if isinstance(c, DocxASTTableCellPropertiesNode))
        assert tcp.shading_fill == "E8F0F8"

    def test_blockquote_multiple_paragraphs(self):
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("First")))
        bq.add_child(_para(_span("Second")))
        result = _map(_doc(bq))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        paras = [c for c in cell.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 2
        for para in paras:
            assert _ppr(para).style_id == "Blockquote"

    def test_blockquote_followed_by_spacer(self):
        # Blockquotes are implemented as tables, so the general table-spacer
        # rule applies: a zero-height Normal paragraph is emitted immediately
        # after the table to provide visible separation from subsequent content.
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quoted")))
        result = _map(_doc(bq))
        body_children = list(_body(result).children)
        table_idx = next(i for i, c in enumerate(body_children) if isinstance(c, DocxASTTableNode))
        spacer = body_children[table_idx + 1]
        assert isinstance(spacer, DocxASTParagraphNode)
        assert _ppr(spacer).style_id == "Normal"
        assert _ppr(spacer).spacing_before == 0
        assert _ppr(spacer).spacing_after == 0

    def test_blockquote_at_end_of_tight_list_item_no_extra_spacer(self):
        # Blockquote as the last child of a tight list item: _map_block adds a
        # spacer after the table, but it must be removed so the list's own
        # trailing spacer is the only one.
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quoted")))
        item = DocumentIRListItemNode()
        item.add_child(_para(_span("Intro")))
        item.add_child(bq)
        ul = DocumentIRUnorderedListNode(tight=True)
        ul.add_child(item)
        result = _map(_doc(ul))
        body_children = list(_body(result).children)
        table_idx = next(i for i, c in enumerate(body_children) if isinstance(c, DocxASTTableNode))
        after_table = body_children[table_idx + 1]
        assert isinstance(after_table, DocxASTParagraphNode)
        assert _ppr(after_table).spacing_before == 0
        assert _ppr(after_table).spacing_after == 0
        # Must be exactly one spacer, not two.
        assert not (table_idx + 2 < len(body_children)
                    and isinstance(body_children[table_idx + 2], DocxASTParagraphNode)
                    and _ppr(body_children[table_idx + 2]).spacing_before == 0
                    and _ppr(body_children[table_idx + 2]).spacing_after == 0)

    def test_blockquote_at_end_of_loose_list_item_has_spacer(self):
        # Blockquote as the last child of a loose list item: the spacer from
        # _map_block is kept (not removed) to provide the inter-item gap.
        bq = DocumentIRBlockquoteNode()
        bq.add_child(_para(_span("Quoted")))
        item = DocumentIRListItemNode()
        item.add_child(_para(_span("Intro")))
        item.add_child(bq)
        ul = DocumentIRUnorderedListNode(tight=False)
        ul.add_child(item)
        result = _map(_doc(ul))
        body_children = list(_body(result).children)
        table_idx = next(i for i, c in enumerate(body_children) if isinstance(c, DocxASTTableNode))
        spacer = body_children[table_idx + 1]
        assert isinstance(spacer, DocxASTParagraphNode)
        assert _ppr(spacer).spacing_before == 0
        assert _ppr(spacer).spacing_after == 0


# ---------------------------------------------------------------------------
# Unordered list mapping
# ---------------------------------------------------------------------------

class TestUnorderedListMapping:
    def _item(self, text: str) -> DocumentIRListItemNode:
        item = DocumentIRListItemNode()
        item.add_child(_para(_span(text)))
        return item

    def test_bullet_list_paragraph(self):
        ul = DocumentIRUnorderedListNode()
        ul.add_child(self._item("Item"))
        result = _map(_doc(ul))
        para = _first_para(result)
        ppr = _ppr(para)
        num_pr = next(
            (c for c in ppr.children if isinstance(c, DocxASTNumberingPropertiesNode)),
            None,
        )
        assert num_pr is not None
        assert num_pr.num_id == "1"  # bullet numId

    def test_bullet_list_ilvl_0(self):
        ul = DocumentIRUnorderedListNode()
        ul.add_child(self._item("Item"))
        result = _map(_doc(ul))
        ppr = _ppr(_first_para(result))
        num_pr = next(c for c in ppr.children if isinstance(c, DocxASTNumberingPropertiesNode))
        assert num_pr.ilvl == 0

    def test_multiple_bullet_items(self):
        ul = DocumentIRUnorderedListNode()
        for text in ["A", "B", "C"]:
            ul.add_child(self._item(text))
        result = _map(_doc(ul))
        paras = _content_paras(result)
        assert len(paras) == 3

    def test_bullet_item_text(self):
        ul = DocumentIRUnorderedListNode()
        ul.add_child(self._item("Hello"))
        result = _map(_doc(ul))
        assert _text_of(_first_para(result)) == "Hello"

    def test_unordered_list_last_item_has_standard_spacing(self):
        ul = DocumentIRUnorderedListNode()
        ul.add_child(self._item("Item"))
        result = _map(_doc(ul))
        assert _ppr(_content_paras(result)[-1]).spacing_after == 200

    def test_nested_list_ilvl(self):
        inner_item = DocumentIRListItemNode()
        inner_item.add_child(_para(_span("Inner")))

        inner_ul = DocumentIRUnorderedListNode()
        inner_ul.add_child(inner_item)

        outer_item = DocumentIRListItemNode()
        outer_item.add_child(_para(_span("Outer")))
        outer_item.add_child(inner_ul)

        outer_ul = DocumentIRUnorderedListNode()
        outer_ul.add_child(outer_item)

        result = _map(_doc(outer_ul))
        paras = _content_paras(result)
        assert len(paras) == 2

        # First para: ilvl 0
        ppr0 = _ppr(paras[0])
        np0 = next(c for c in ppr0.children if isinstance(c, DocxASTNumberingPropertiesNode))
        assert np0.ilvl == 0

        # Second para: ilvl 1
        ppr1 = _ppr(paras[1])
        np1 = next(c for c in ppr1.children if isinstance(c, DocxASTNumberingPropertiesNode))
        assert np1.ilvl == 1


# ---------------------------------------------------------------------------
# Ordered list mapping
# ---------------------------------------------------------------------------

class TestOrderedListMapping:
    def _item(self, text: str) -> DocumentIRListItemNode:
        item = DocumentIRListItemNode()
        item.add_child(_para(_span(text)))
        return item

    def test_ordered_list_num_id(self):
        ol = DocumentIROrderedListNode(start=1)
        ol.add_child(self._item("First"))
        result = _map(_doc(ol))
        ppr = _ppr(_first_para(result))
        num_pr = next(c for c in ppr.children if isinstance(c, DocxASTNumberingPropertiesNode))
        assert num_pr.num_id == "2"  # ordered numId

    def test_ordered_list_ilvl_0(self):
        ol = DocumentIROrderedListNode(start=1)
        ol.add_child(self._item("First"))
        result = _map(_doc(ol))
        ppr = _ppr(_first_para(result))
        num_pr = next(c for c in ppr.children if isinstance(c, DocxASTNumberingPropertiesNode))
        assert num_pr.ilvl == 0

    def test_multiple_ordered_items(self):
        ol = DocumentIROrderedListNode(start=1)
        for text in ["One", "Two", "Three"]:
            ol.add_child(self._item(text))
        result = _map(_doc(ol))
        paras = _content_paras(result)
        assert len(paras) == 3

    def test_tight_ordered_list_items_have_spacing_after_zero(self):
        ol = DocumentIROrderedListNode(start=1, tight=True)
        for text in ["One", "Two", "Three"]:
            ol.add_child(self._item(text))
        result = _map(_doc(ol))
        paras = _content_paras(result)
        for para in paras[:-1]:
            assert _ppr(para).spacing_after == 0
        assert _ppr(paras[-1]).spacing_after == 200

    def test_loose_ordered_list_items_have_default_spacing(self):
        ol = DocumentIROrderedListNode(start=1, tight=False)
        for text in ["One", "Two", "Three"]:
            ol.add_child(self._item(text))
        result = _map(_doc(ol))
        paras = _content_paras(result)
        for para in paras[:-1]:
            assert _ppr(para).spacing_after is None
        assert _ppr(paras[-1]).spacing_after == 200

    def test_ordered_list_last_item_has_standard_spacing(self):
        ol = DocumentIROrderedListNode(start=1)
        ol.add_child(self._item("Item"))
        result = _map(_doc(ol))
        assert _ppr(_content_paras(result)[-1]).spacing_after == 200


# ---------------------------------------------------------------------------
# Loose/tight list spacing interactions
# ---------------------------------------------------------------------------

class TestListSpacingInteractions:
    def test_loose_bullet_item_ending_with_tight_nested_ordered_has_loose_spacing(self):
        """Last para of a tight nested ordered list inside a loose bullet item
        gets spacing_after=200 from _apply_list_trailing_spacing on the nested
        list, providing the inter-item gap."""
        nested_ol = DocumentIROrderedListNode(start=1, tight=True)
        for text in ["a", "b", "c"]:
            ni = DocumentIRListItemNode()
            ni.add_child(_para(_span(text)))
            nested_ol.add_child(ni)

        outer_item = DocumentIRListItemNode()
        outer_item.add_child(_para(_span("Outer")))
        outer_item.add_child(nested_ol)

        outer_ul = DocumentIRUnorderedListNode(tight=False)
        outer_ul.add_child(outer_item)

        result = _map(_doc(outer_ul))
        paras = _content_paras(result)
        # Last paragraph emitted belongs to the nested list; it gets 200 from
        # _apply_list_trailing_spacing, which the outer list then also patches to 200.
        assert _ppr(paras[-1]).spacing_after == 200

    def test_loose_bullet_item_ending_with_tight_nested_bullet_has_loose_spacing(self):
        """Same as above but the nested list is also unordered."""
        nested_ul = DocumentIRUnorderedListNode(tight=True)
        for text in ["x", "y"]:
            ni = DocumentIRListItemNode()
            ni.add_child(_para(_span(text)))
            nested_ul.add_child(ni)

        outer_item = DocumentIRListItemNode()
        outer_item.add_child(_para(_span("Outer")))
        outer_item.add_child(nested_ul)

        outer_ul = DocumentIRUnorderedListNode(tight=False)
        outer_ul.add_child(outer_item)

        result = _map(_doc(outer_ul))
        paras = _content_paras(result)
        assert _ppr(paras[-1]).spacing_after == 200

    def test_tight_outer_item_ending_with_tight_nested_list_stays_tight(self):
        """When the outer item is itself tight, the last para of the nested list
        gets spacing_after=200 from _apply_list_trailing_spacing, which the
        outer tight list also patches to 200."""
        nested_ol = DocumentIROrderedListNode(start=1, tight=True)
        ni = DocumentIRListItemNode()
        ni.add_child(_para(_span("inner")))
        nested_ol.add_child(ni)

        outer_item = DocumentIRListItemNode()
        outer_item.add_child(_para(_span("Outer")))
        outer_item.add_child(nested_ol)

        outer_ul = DocumentIRUnorderedListNode(tight=True)
        outer_ul.add_child(outer_item)

        result = _map(_doc(outer_ul))
        paras = _content_paras(result)
        assert _ppr(paras[-1]).spacing_after == 200


# ---------------------------------------------------------------------------
# Table mapping
# ---------------------------------------------------------------------------

def _ir_table(rows: list, header_count: int = 0) -> DocumentIRTableNode:
    """Build a DocumentIRTableNode from a list of lists of cell texts."""
    table = DocumentIRTableNode()

    if header_count > 0:
        header = DocumentIRTableHeaderNode()
        for row_texts in rows[:header_count]:
            row = DocumentIRTableRowNode()
            for text in row_texts:
                cell = DocumentIRTableCellNode(is_header=True, alignment="left")
                cell.add_child(_para(_span(text)))
                row.add_child(cell)
            header.add_child(row)
        table.add_child(header)

    body = DocumentIRTableBodyNode()
    for row_texts in rows[header_count:]:
        row = DocumentIRTableRowNode()
        for text in row_texts:
            cell = DocumentIRTableCellNode(is_header=False, alignment="left")
            cell.add_child(_para(_span(text)))
            row.add_child(cell)
        body.add_child(row)
    table.add_child(body)

    return table


class TestTableMapping:
    def test_table_node(self):
        result = _map(_doc(_ir_table([["A", "B"]])))
        tables = [c for c in _body(result).children if isinstance(c, DocxASTTableNode)]
        assert len(tables) == 1

    def test_table_row_count(self):
        result = _map(_doc(_ir_table([["A"], ["B"], ["C"]])))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        rows = [c for c in table.children if isinstance(c, DocxASTTableRowNode)]
        assert len(rows) == 3

    def test_table_cell_count(self):
        result = _map(_doc(_ir_table([["A", "B", "C"]])))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cells = [c for c in row.children if isinstance(c, DocxASTTableCellNode)]
        assert len(cells) == 3

    def test_cell_text(self):
        result = _map(_doc(_ir_table([["Hello"]])))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in cell.children if isinstance(c, DocxASTParagraphNode))
        assert _text_of(para) == "Hello"

    def test_header_row_has_tbl_header(self):
        result = _map(_doc(_ir_table([["H1", "H2"], ["D1", "D2"]], header_count=1)))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        first_row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        rpr = next(
            (c for c in first_row.children if isinstance(c, DocxASTTableRowPropertiesNode)),
            None,
        )
        assert rpr is not None
        assert rpr.is_header is True

    def test_body_row_no_tbl_header(self):
        result = _map(_doc(_ir_table([["D1"]])))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        rpr = next(
            (c for c in row.children if isinstance(c, DocxASTTableRowPropertiesNode)),
            None,
        )
        # Body rows have cant_split but not is_header
        assert rpr is None or rpr.is_header is False

    def test_cell_always_has_paragraph(self):
        # Even an empty cell must have at least one paragraph
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        # No children added to cell
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _map(_doc(table))
        docx_table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        docx_row = next(c for c in docx_table.children if isinstance(c, DocxASTTableRowNode))
        docx_cell = next(c for c in docx_row.children if isinstance(c, DocxASTTableCellNode))
        paras = [c for c in docx_cell.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) >= 1

    def test_cell_alignment_center(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="center")
        cell.add_child(_para(_span("Centered")))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _map(_doc(table))
        docx_table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        docx_row = next(c for c in docx_table.children if isinstance(c, DocxASTTableRowNode))
        docx_cell = next(c for c in docx_row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in docx_cell.children if isinstance(c, DocxASTParagraphNode))
        ppr = next(c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode))
        assert ppr.justification == "center"

    def test_table_has_properties_node(self):
        result = _map(_doc(_ir_table([["A"]])))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        tbl_pr = next(
            (c for c in table.children if isinstance(c, DocxASTTablePropertiesNode)),
            None,
        )
        assert tbl_pr is not None

    def test_table_width_is_full_page(self):
        result = _map(_doc(_ir_table([["A"]])))
        table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        tbl_pr = next(c for c in table.children if isinstance(c, DocxASTTablePropertiesNode))
        assert tbl_pr.width == 5000
        assert tbl_pr.width_type == "pct"

    def test_table_followed_by_spacer(self):
        result = _map(_doc(_ir_table([["A"]])))
        body_children = list(_body(result).children)
        table_idx = next(i for i, c in enumerate(body_children) if isinstance(c, DocxASTTableNode))
        spacer = body_children[table_idx + 1]
        assert isinstance(spacer, DocxASTParagraphNode)
        assert _ppr(spacer).style_id == "Normal"
        assert _ppr(spacer).spacing_before == 0
        assert _ppr(spacer).spacing_after == 0

    def test_cell_with_inline_children_has_paragraph(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_span("Hello"))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _map(_doc(table))
        docx_table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        docx_row = next(c for c in docx_table.children if isinstance(c, DocxASTTableRowNode))
        docx_cell = next(c for c in docx_row.children if isinstance(c, DocxASTTableCellNode))
        paras = [c for c in docx_cell.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 1

    def test_cell_with_inline_children_has_correct_text(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_span("Hello"))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _map(_doc(table))
        docx_table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        docx_row = next(c for c in docx_table.children if isinstance(c, DocxASTTableRowNode))
        docx_cell = next(c for c in docx_row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in docx_cell.children if isinstance(c, DocxASTParagraphNode))
        assert _text_of(para) == "Hello"

    def test_cell_with_inline_bold_span(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_span("Bold", bold=True))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _map(_doc(table))
        docx_table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        docx_row = next(c for c in docx_table.children if isinstance(c, DocxASTTableRowNode))
        docx_cell = next(c for c in docx_row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in docx_cell.children if isinstance(c, DocxASTParagraphNode))
        run = next(c for c in para.children if isinstance(c, DocxASTRunNode))
        rpr = next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.bold is True

    def test_cell_with_mixed_inline_and_block_children(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="left")
        cell.add_child(_span("Inline"))
        cell.add_child(_para(_span("Block")))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _map(_doc(table))
        docx_table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        docx_row = next(c for c in docx_table.children if isinstance(c, DocxASTTableRowNode))
        docx_cell = next(c for c in docx_row.children if isinstance(c, DocxASTTableCellNode))
        paras = [c for c in docx_cell.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 2

    def test_cell_alignment_respected_for_inline_children(self):
        table = DocumentIRTableNode()
        body = DocumentIRTableBodyNode()
        row = DocumentIRTableRowNode()
        cell = DocumentIRTableCellNode(is_header=False, alignment="right")
        cell.add_child(_span("Right"))
        row.add_child(cell)
        body.add_child(row)
        table.add_child(body)
        result = _map(_doc(table))
        docx_table = next(c for c in _body(result).children if isinstance(c, DocxASTTableNode))
        docx_row = next(c for c in docx_table.children if isinstance(c, DocxASTTableRowNode))
        docx_cell = next(c for c in docx_row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in docx_cell.children if isinstance(c, DocxASTParagraphNode))
        ppr = next(c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode))
        assert ppr.justification == "right"


# ---------------------------------------------------------------------------
# Inline special content
# ---------------------------------------------------------------------------

class TestInlineSpecialContent:
    def test_line_break(self):
        p = DocumentIRParagraphNode()
        p.add_child(_span("Before"))
        p.add_child(DocumentIRLineBreakNode())
        p.add_child(_span("After"))
        result = _map(_doc(p))
        runs = _runs(_first_para(result))
        # Should have: text run, break run, text run
        assert len(runs) == 3
        break_run = runs[1]
        breaks = [c for c in break_run.children if isinstance(c, DocxASTBreakNode)]
        assert len(breaks) == 1
        assert breaks[0].break_type == "textWrapping"

    def test_image(self):
        p = DocumentIRParagraphNode()
        img = DocumentIRImageNode(url="image.png", alt_text="A photo")
        p.add_child(img)
        result = _map(_doc(p))
        runs = _runs(_first_para(result))
        assert len(runs) == 1
        drawing = next(c for c in runs[0].children if isinstance(c, DocxASTDrawingNode))
        assert drawing.resolved_path == "image.png"
        assert drawing.description == "A photo"

    def test_link_text_underlined(self):
        p = DocumentIRParagraphNode()
        link = DocumentIRLinkNode(url="https://example.com")
        link.add_child(_span("click here"))
        p.add_child(link)
        result = _map(_doc(p))
        # Links map to a DocxASTHyperlinkNode, not plain runs.  The hyperlink
        # carries runs inside it; each run uses the "Hyperlink" character style.
        para = _first_para(result)
        hyperlink = next(c for c in para.children if isinstance(c, DocxASTHyperlinkNode))
        assert hyperlink.url == "https://example.com"
        run = next(c for c in hyperlink.children if isinstance(c, DocxASTRunNode))
        rpr = next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.style_id == "Hyperlink"

    def test_link_url_appended(self):
        p = DocumentIRParagraphNode()
        link = DocumentIRLinkNode(url="https://example.com")
        link.add_child(_span("text"))
        p.add_child(link)
        result = _map(_doc(p))
        # The URL is carried on the hyperlink node itself; the paragraph child
        # is a DocxASTHyperlinkNode, not a separate URL run.
        para = _first_para(result)
        hyperlink = next(c for c in para.children if isinstance(c, DocxASTHyperlinkNode))
        assert hyperlink.url == "https://example.com"
        run = next(c for c in hyperlink.children if isinstance(c, DocxASTRunNode))
        text_node = next(c for c in run.children if isinstance(c, DocxASTTextNode))
        assert text_node.content == "text"

    def test_link_no_url_no_url_run(self):
        p = DocumentIRParagraphNode()
        link = DocumentIRLinkNode(url="")
        link.add_child(_span("text"))
        p.add_child(link)
        result = _map(_doc(p))
        # Even with an empty URL the hyperlink node is still emitted; the run
        # inside it carries the link text.
        para = _first_para(result)
        hyperlink = next(c for c in para.children if isinstance(c, DocxASTHyperlinkNode))
        run = next(c for c in hyperlink.children if isinstance(c, DocxASTRunNode))
        text_node = next(c for c in run.children if isinstance(c, DocxASTTextNode))
        assert text_node.content == "text"


# ---------------------------------------------------------------------------
# Horizontal rule
# ---------------------------------------------------------------------------

class TestHorizontalRule:
    def test_horizontal_rule_emits_paragraph(self):
        result = _map(_doc(DocumentIRHorizontalRuleNode()))
        paras = [c for c in _body(result).children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 1

    def test_horizontal_rule_uses_normal_style(self):
        result = _map(_doc(DocumentIRHorizontalRuleNode()))
        para = _first_para(result)
        assert _ppr(para).style_id == "Normal"


# ---------------------------------------------------------------------------
# Mixed document
# ---------------------------------------------------------------------------

class TestMixedDocument:
    def test_heading_then_paragraph(self):
        result = _map(_doc(
            _heading(1, _span("Title")),
            _para(_span("Body text")),
        ))
        paras = [c for c in _body(result).children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 2
        assert _ppr(paras[0]).style_id == "Heading1"
        assert _ppr(paras[1]).style_id == "Normal"

    def test_paragraph_then_list_then_paragraph(self):
        ul = DocumentIRUnorderedListNode()
        item = DocumentIRListItemNode()
        item.add_child(_para(_span("Item")))
        ul.add_child(item)

        result = _map(_doc(
            _para(_span("Before")),
            ul,
            _para(_span("After")),
        ))
        paras = _content_paras(result)
        assert len(paras) == 3
        # Middle para has numPr
        ppr1 = _ppr(paras[1])
        num_prs = [c for c in ppr1.children if isinstance(c, DocxASTNumberingPropertiesNode)]
        assert len(num_prs) == 1
