"""Tests for the DOCX AST serialiser.

Verifies that serialise_docx() produces valid ZIP archives containing
well-formed XML that round-trips correctly through the parser.
"""

import io
import xml.etree.ElementTree as ET
import zipfile

import pytest

from docx import serialise_docx, parse_docx, doc_ir_to_docx_ast
from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
    DocxASTBodyNode,
    DocxASTBreakNode,
    DocxASTDocumentNode,
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
from doc_ir import (
    DocIRDocumentNode,
    DocIRHeadingNode,
    DocIRParagraphNode,
    DocIRTextSpanNode,
    DocIRCodeBlockNode,
    DocIRUnorderedListNode,
    DocIROrderedListNode,
    DocIRListItemNode,
    DocIRTableNode,
    DocIRTableBodyNode,
    DocIRTableRowNode,
    DocIRTableCellNode,
    DocIRBlockquoteNode,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"


def _build_minimal_docx_ast(text: str = "Hello") -> DocxASTDocumentNode:
    """Build a minimal DocxASTDocumentNode with one paragraph."""
    doc = DocxASTDocumentNode()

    styles = DocxASTStylesNode()
    normal = DocxASTStyleNode(
        style_type="paragraph", style_id="Normal", name="Normal", is_default=True
    )
    styles.add_child(normal)
    doc.add_child(styles)

    body = DocxASTBodyNode()
    para = DocxASTParagraphNode()
    ppr = DocxASTParagraphPropertiesNode(style_id="Normal")
    para.add_child(ppr)
    run = DocxASTRunNode()
    run.add_child(DocxASTTextNode(content=text))
    para.add_child(run)
    body.add_child(para)
    doc.add_child(body)

    return doc


def _open_zip(data: bytes) -> zipfile.ZipFile:
    return zipfile.ZipFile(io.BytesIO(data))


def _parse_xml(data: bytes, path: str) -> ET.Element:
    with _open_zip(data) as zf:
        return ET.fromstring(zf.read(path))


def _w(local: str) -> str:
    return f"{{{_W}}}{local}"


# ---------------------------------------------------------------------------
# ZIP structure
# ---------------------------------------------------------------------------

class TestZIPStructure:
    def test_returns_bytes(self):
        result = serialise_docx(_build_minimal_docx_ast())
        assert isinstance(result, bytes)
        assert len(result) > 0

    def test_is_valid_zip(self):
        result = serialise_docx(_build_minimal_docx_ast())
        assert zipfile.is_zipfile(io.BytesIO(result))

    def test_contains_content_types(self):
        result = serialise_docx(_build_minimal_docx_ast())
        with _open_zip(result) as zf:
            assert "[Content_Types].xml" in zf.namelist()

    def test_contains_rels(self):
        result = serialise_docx(_build_minimal_docx_ast())
        with _open_zip(result) as zf:
            assert "_rels/.rels" in zf.namelist()

    def test_contains_document_xml(self):
        result = serialise_docx(_build_minimal_docx_ast())
        with _open_zip(result) as zf:
            assert "word/document.xml" in zf.namelist()

    def test_contains_document_rels(self):
        result = serialise_docx(_build_minimal_docx_ast())
        with _open_zip(result) as zf:
            assert "word/_rels/document.xml.rels" in zf.namelist()

    def test_contains_styles(self):
        result = serialise_docx(_build_minimal_docx_ast())
        with _open_zip(result) as zf:
            assert "word/styles.xml" in zf.namelist()

    def test_no_numbering_when_absent(self):
        result = serialise_docx(_build_minimal_docx_ast())
        with _open_zip(result) as zf:
            assert "word/numbering.xml" not in zf.namelist()

    def test_numbering_present_when_included(self):
        doc = _build_minimal_docx_ast()
        numbering = DocxASTNumberingNode()
        abstract = DocxASTAbstractNumNode(abstract_num_id="0")
        lvl = DocxASTNumLevelNode(ilvl=0, num_fmt="bullet", lvl_text="\u2022")
        abstract.add_child(lvl)
        numbering.add_child(abstract)
        numbering.add_child(DocxASTNumNode(num_id="1", abstract_num_id="0"))
        doc.add_child(numbering)

        result = serialise_docx(doc)
        with _open_zip(result) as zf:
            assert "word/numbering.xml" in zf.namelist()


# ---------------------------------------------------------------------------
# XML validity
# ---------------------------------------------------------------------------

class TestXMLValidity:
    def test_document_xml_is_valid(self):
        result = serialise_docx(_build_minimal_docx_ast())
        # Should not raise
        root = _parse_xml(result, "word/document.xml")
        assert root is not None

    def test_styles_xml_is_valid(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/styles.xml")
        assert root is not None

    def test_content_types_is_valid(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "[Content_Types].xml")
        assert root is not None

    def test_rels_is_valid(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "_rels/.rels")
        assert root is not None

    def test_document_rels_is_valid(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/_rels/document.xml.rels")
        assert root is not None


# ---------------------------------------------------------------------------
# document.xml structure
# ---------------------------------------------------------------------------

class TestDocumentXML:
    def test_has_body(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/document.xml")
        body = root.find(_w("body"))
        assert body is not None

    def test_has_paragraph(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/document.xml")
        body = root.find(_w("body"))
        paras = body.findall(_w("p"))
        assert len(paras) >= 1

    def test_paragraph_text(self):
        result = serialise_docx(_build_minimal_docx_ast("Hello world"))
        root = _parse_xml(result, "word/document.xml")
        body = root.find(_w("body"))
        texts = [t.text or "" for t in body.iter(_w("t"))]
        assert "Hello world" in texts

    def test_paragraph_style(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/document.xml")
        body = root.find(_w("body"))
        para = body.find(_w("p"))
        ppr = para.find(_w("pPr"))
        assert ppr is not None
        pstyle = ppr.find(_w("pStyle"))
        assert pstyle is not None
        assert pstyle.get(_w("val")) == "Normal"

    def test_sect_pr_present(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/document.xml")
        body = root.find(_w("body"))
        sect_pr = body.find(_w("sectPr"))
        assert sect_pr is not None

    def test_bold_run(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTRunPropertiesNode(bold=True))
        run.add_child(DocxASTTextNode("Bold"))
        para.add_child(run)
        body.add_child(para)
        doc.add_child(body)

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        b_elements = list(root.iter(_w("b")))
        assert len(b_elements) >= 1

    def test_italic_run(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTRunPropertiesNode(italic=True))
        run.add_child(DocxASTTextNode("Italic"))
        para.add_child(run)
        body.add_child(para)
        doc.add_child(body)

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        i_elements = list(root.iter(_w("i")))
        assert len(i_elements) >= 1

    def test_line_break(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode("Before"))
        run.add_child(DocxASTBreakNode(break_type="textWrapping"))
        run.add_child(DocxASTTextNode("After"))
        para.add_child(run)
        body.add_child(para)
        doc.add_child(body)

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        br_elements = list(root.iter(_w("br")))
        assert len(br_elements) >= 1

    def test_numbering_properties_in_paragraph(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(style_id="Normal")
        ppr.add_child(DocxASTNumberingPropertiesNode(num_id="1", ilvl=0))
        para.add_child(ppr)
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode("Item"))
        para.add_child(run)
        body.add_child(para)
        doc.add_child(body)

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        num_id_elements = list(root.iter(_w("numId")))
        assert len(num_id_elements) >= 1
        assert num_id_elements[0].get(_w("val")) == "1"

    def test_table_in_document(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()

        table = DocxASTTableNode()
        row = DocxASTTableRowNode()
        cell = DocxASTTableCellNode()
        cell.add_child(DocxASTTableCellPropertiesNode(width_type="auto"))
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode("Cell"))
        para.add_child(run)
        cell.add_child(para)
        row.add_child(cell)
        table.add_child(row)
        body.add_child(table)
        doc.add_child(body)

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        tables = list(root.iter(_w("tbl")))
        assert len(tables) == 1

    def test_table_row_header(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()

        table = DocxASTTableNode()
        row = DocxASTTableRowNode()
        rpr = DocxASTTableRowPropertiesNode(is_header=True)
        row.add_child(rpr)
        cell = DocxASTTableCellNode()
        cell.add_child(DocxASTTableCellPropertiesNode(width_type="auto"))
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        run.add_child(DocxASTTextNode("Header"))
        para.add_child(run)
        cell.add_child(para)
        row.add_child(cell)
        table.add_child(row)
        body.add_child(table)
        doc.add_child(body)

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        tbl_header_elements = list(root.iter(_w("tblHeader")))
        assert len(tbl_header_elements) >= 1

    def test_table_with_properties_has_tbl_pr(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        table = DocxASTTableNode()
        table.add_child(DocxASTTablePropertiesNode(width=5000, width_type="pct"))
        row = DocxASTTableRowNode()
        cell = DocxASTTableCellNode()
        cell.add_child(DocxASTTableCellPropertiesNode(width_type="auto"))
        cell.add_child(DocxASTParagraphNode())
        row.add_child(cell)
        table.add_child(row)
        body.add_child(table)
        doc.add_child(body)
        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        tbl = root.find(f".//{_w('tbl')}")
        tbl_pr = tbl.find(_w("tblPr"))
        assert tbl_pr is not None

    def test_table_with_properties_has_correct_width(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        table = DocxASTTableNode()
        table.add_child(DocxASTTablePropertiesNode(width=5000, width_type="pct"))
        row = DocxASTTableRowNode()
        cell = DocxASTTableCellNode()
        cell.add_child(DocxASTTableCellPropertiesNode(width_type="auto"))
        cell.add_child(DocxASTParagraphNode())
        row.add_child(cell)
        table.add_child(row)
        body.add_child(table)
        doc.add_child(body)
        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        tbl_w = root.find(f".//{_w('tblW')}")
        assert tbl_w.get(_w("w")) == "5000"
        assert tbl_w.get(_w("type")) == "pct"

    def test_table_with_properties_has_borders(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        table = DocxASTTableNode()
        table.add_child(DocxASTTablePropertiesNode(width=5000, width_type="pct"))
        row = DocxASTTableRowNode()
        cell = DocxASTTableCellNode()
        cell.add_child(DocxASTTableCellPropertiesNode(width_type="auto"))
        cell.add_child(DocxASTParagraphNode())
        row.add_child(cell)
        table.add_child(row)
        body.add_child(table)
        doc.add_child(body)
        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        tbl_borders = root.find(f".//{_w('tblBorders')}")
        assert tbl_borders is not None
        border_vals = [el.get(_w("val")) for el in tbl_borders]
        assert all(v == "single" for v in border_vals)

    def test_table_no_borders_suppresses_borders(self):
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())
        body = DocxASTBodyNode()
        table = DocxASTTableNode()
        table.add_child(DocxASTTablePropertiesNode(width=5000, width_type="pct", no_borders=True))
        row = DocxASTTableRowNode()
        cell = DocxASTTableCellNode()
        cell.add_child(DocxASTTableCellPropertiesNode(width_type="auto"))
        cell.add_child(DocxASTParagraphNode())
        row.add_child(cell)
        table.add_child(row)
        body.add_child(table)
        doc.add_child(body)
        result = serialise_docx(doc)
        root = _parse_xml(result, "word/document.xml")
        tbl_borders = root.find(f".//{_w('tblBorders')}")
        assert tbl_borders is not None
        border_vals = [el.get(_w("val")) for el in tbl_borders]
        assert all(v == "none" for v in border_vals)


# ---------------------------------------------------------------------------
# styles.xml
# ---------------------------------------------------------------------------

class TestStylesXML:
    def test_styles_root_element(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/styles.xml")
        assert root.tag == _w("styles")

    def test_normal_style_present(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/styles.xml")
        style_ids = [s.get(_w("styleId")) for s in root.findall(_w("style"))]
        assert "Normal" in style_ids

    def test_normal_style_is_default(self):
        result = serialise_docx(_build_minimal_docx_ast())
        root = _parse_xml(result, "word/styles.xml")
        normal = next(
            s for s in root.findall(_w("style"))
            if s.get(_w("styleId")) == "Normal"
        )
        assert normal.get(_w("default")) == "1"

    def test_heading1_style(self):
        doc = DocxASTDocumentNode()
        styles = DocxASTStylesNode()
        h1 = DocxASTStyleNode(
            style_type="paragraph", style_id="Heading1",
            name="heading 1", based_on="Normal",
        )
        h1_ppr = DocxASTParagraphPropertiesNode(outline_level=0)
        h1.add_child(h1_ppr)
        h1_rpr = DocxASTRunPropertiesNode(bold=True)
        h1.add_child(h1_rpr)
        styles.add_child(h1)
        doc.add_child(styles)
        doc.add_child(DocxASTBodyNode())

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/styles.xml")
        style_ids = [s.get(_w("styleId")) for s in root.findall(_w("style"))]
        assert "Heading1" in style_ids

    def test_heading1_has_based_on(self):
        doc = DocxASTDocumentNode()
        styles = DocxASTStylesNode()
        h1 = DocxASTStyleNode(
            style_type="paragraph", style_id="Heading1",
            name="heading 1", based_on="Normal",
        )
        styles.add_child(h1)
        doc.add_child(styles)
        doc.add_child(DocxASTBodyNode())

        result = serialise_docx(doc)
        root = _parse_xml(result, "word/styles.xml")
        h1_elem = next(
            s for s in root.findall(_w("style"))
            if s.get(_w("styleId")) == "Heading1"
        )
        based_on = h1_elem.find(_w("basedOn"))
        assert based_on is not None
        assert based_on.get(_w("val")) == "Normal"


# ---------------------------------------------------------------------------
# numbering.xml
# ---------------------------------------------------------------------------

class TestNumberingXML:
    def _doc_with_numbering(self) -> DocxASTDocumentNode:
        doc = DocxASTDocumentNode()
        doc.add_child(DocxASTStylesNode())

        numbering = DocxASTNumberingNode()
        abstract = DocxASTAbstractNumNode(abstract_num_id="0")
        abstract.multi_level_type = "hybridMultilevel"
        lvl = DocxASTNumLevelNode(
            ilvl=0, start=1, num_fmt="bullet",
            lvl_text="\u2022", lvl_jc="left",
            indent_left=720, indent_hanging=360,
        )
        abstract.add_child(lvl)
        numbering.add_child(abstract)
        numbering.add_child(DocxASTNumNode(num_id="1", abstract_num_id="0"))
        doc.add_child(numbering)
        doc.add_child(DocxASTBodyNode())
        return doc

    def test_numbering_root_element(self):
        result = serialise_docx(self._doc_with_numbering())
        root = _parse_xml(result, "word/numbering.xml")
        assert root.tag == _w("numbering")

    def test_abstract_num_present(self):
        result = serialise_docx(self._doc_with_numbering())
        root = _parse_xml(result, "word/numbering.xml")
        abstract_nums = root.findall(_w("abstractNum"))
        assert len(abstract_nums) == 1

    def test_abstract_num_id(self):
        result = serialise_docx(self._doc_with_numbering())
        root = _parse_xml(result, "word/numbering.xml")
        abstract_num = root.find(_w("abstractNum"))
        assert abstract_num.get(_w("abstractNumId")) == "0"

    def test_level_present(self):
        result = serialise_docx(self._doc_with_numbering())
        root = _parse_xml(result, "word/numbering.xml")
        levels = list(root.iter(_w("lvl")))
        assert len(levels) == 1

    def test_level_num_fmt(self):
        result = serialise_docx(self._doc_with_numbering())
        root = _parse_xml(result, "word/numbering.xml")
        num_fmt = root.find(f".//{_w('numFmt')}")
        assert num_fmt.get(_w("val")) == "bullet"

    def test_num_instance_present(self):
        result = serialise_docx(self._doc_with_numbering())
        root = _parse_xml(result, "word/numbering.xml")
        nums = root.findall(_w("num"))
        assert len(nums) == 1
        assert nums[0].get(_w("numId")) == "1"

    def test_level_indent(self):
        result = serialise_docx(self._doc_with_numbering())
        root = _parse_xml(result, "word/numbering.xml")
        ind = root.find(f".//{_w('ind')}")
        assert ind is not None
        assert ind.get(_w("left")) == "720"
        assert ind.get(_w("hanging")) == "360"


# ---------------------------------------------------------------------------
# Round-trip: doc_ir → DOCX AST → serialise → parse → doc_ir
# ---------------------------------------------------------------------------

class TestRoundTrip:
    """Verify that serialised output can be parsed back by the DOCX parser."""

    def _round_trip(self, ir_doc: DocIRDocumentNode) -> DocxASTDocumentNode:
        """Convert doc_ir → DOCX AST → bytes → parsed DOCX AST."""
        docx_ast = doc_ir_to_docx_ast(ir_doc)
        docx_bytes = serialise_docx(docx_ast)
        return parse_docx(docx_bytes)

    def test_simple_paragraph_round_trips(self):
        ir = DocIRDocumentNode()
        p = DocIRParagraphNode()
        p.add_child(DocIRTextSpanNode("Hello world"))
        ir.add_child(p)

        parsed = self._round_trip(ir)
        body = next(c for c in parsed.children if isinstance(c, __import__('docx').docx_ast_node.DocxASTBodyNode))
        texts = [t.content for p in body.children
                 if isinstance(p, __import__('docx').docx_ast_node.DocxASTParagraphNode)
                 for r in p.children
                 if isinstance(r, __import__('docx').docx_ast_node.DocxASTRunNode)
                 for t in r.children
                 if isinstance(t, __import__('docx').docx_ast_node.DocxASTTextNode)]
        assert "Hello world" in texts

    def test_heading_round_trips(self):
        ir = DocIRDocumentNode()
        h = DocIRHeadingNode(level=1)
        h.add_child(DocIRTextSpanNode("My Title"))
        ir.add_child(h)

        parsed = self._round_trip(ir)
        from docx.docx_ast_node import (
            DocxASTBodyNode, DocxASTParagraphNode,
            DocxASTParagraphPropertiesNode,
        )
        body = next(c for c in parsed.children if isinstance(c, DocxASTBodyNode))
        para = next(c for c in body.children if isinstance(c, DocxASTParagraphNode))
        ppr = next(c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode))
        assert ppr.style_id == "Heading1"

    def test_code_block_round_trips(self):
        ir = DocIRDocumentNode()
        ir.add_child(DocIRCodeBlockNode(language="python", content="x = 1"))

        parsed = self._round_trip(ir)
        from docx.docx_ast_node import (
            DocxASTBodyNode, DocxASTParagraphNode,
            DocxASTParagraphPropertiesNode, DocxASTRunNode, DocxASTTextNode,
        )
        body = next(c for c in parsed.children if isinstance(c, DocxASTBodyNode))
        para = next(c for c in body.children if isinstance(c, DocxASTParagraphNode))
        ppr = next(c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode))
        assert ppr.style_id == "CodeBlock"
        texts = [t.content for r in para.children
                 if isinstance(r, DocxASTRunNode)
                 for t in r.children
                 if isinstance(t, DocxASTTextNode)]
        assert "x = 1" in texts

    def test_table_round_trips(self):
        ir = DocIRDocumentNode()
        table = DocIRTableNode()
        body_section = DocIRTableBodyNode()
        row = DocIRTableRowNode()
        cell = DocIRTableCellNode(is_header=False, alignment="left")
        p = DocIRParagraphNode()
        p.add_child(DocIRTextSpanNode("CellText"))
        cell.add_child(p)
        row.add_child(cell)
        body_section.add_child(row)
        table.add_child(body_section)
        ir.add_child(table)

        parsed = self._round_trip(ir)
        from docx.docx_ast_node import DocxASTBodyNode, DocxASTTableNode
        body = next(c for c in parsed.children if isinstance(c, DocxASTBodyNode))
        tables = [c for c in body.children if isinstance(c, DocxASTTableNode)]
        assert len(tables) == 1

    def test_bullet_list_round_trips(self):
        ir = DocIRDocumentNode()
        ul = DocIRUnorderedListNode()
        item = DocIRListItemNode()
        p = DocIRParagraphNode()
        p.add_child(DocIRTextSpanNode("List item"))
        item.add_child(p)
        ul.add_child(item)
        ir.add_child(ul)

        parsed = self._round_trip(ir)
        from docx.docx_ast_node import (
            DocxASTBodyNode, DocxASTParagraphNode,
            DocxASTParagraphPropertiesNode, DocxASTNumberingPropertiesNode,
        )
        body = next(c for c in parsed.children if isinstance(c, DocxASTBodyNode))
        para = next(c for c in body.children if isinstance(c, DocxASTParagraphNode))
        ppr = next(c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode))
        num_pr = next(
            (c for c in ppr.children if isinstance(c, DocxASTNumberingPropertiesNode)),
            None,
        )
        assert num_pr is not None
        assert num_pr.num_id == "1"

    def test_xml_special_characters_escaped(self):
        ir = DocIRDocumentNode()
        p = DocIRParagraphNode()
        p.add_child(DocIRTextSpanNode("a < b & c > d"))
        ir.add_child(p)

        # Should not raise (would fail if XML is malformed)
        parsed = self._round_trip(ir)
        assert parsed is not None
