"""Tests for DocxASTParser using synthetic DOCX fixtures only."""

import io
import zipfile

import pytest

from docx import (
    DocxASTParser,
    DocxParseError,
    DocxUnsupportedError,
    DocxExtractionError,
    parse_docx,
    DocxASTDocumentNode,
    DocxASTBodyNode,
    DocxASTHyperlinkNode,
    DocxASTStylesNode,
    DocxASTStyleNode,
    DocxASTNumberingNode,
    DocxASTAbstractNumNode,
    DocxASTNumNode,
    DocxASTNumLevelNode,
    DocxASTParagraphNode,
    DocxASTParagraphPropertiesNode,
    DocxASTNumberingPropertiesNode,
    DocxASTRunNode,
    DocxASTRunPropertiesNode,
    DocxASTTextNode,
    DocxASTTabNode,
    DocxASTBreakNode,
    DocxASTLastRenderedPageBreakNode,
    DocxASTBookmarkStartNode,
    DocxASTBookmarkEndNode,
    DocxASTTableNode,
    DocxASTTablePropertiesNode,
    DocxASTTableGridNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTableCellNode,
    DocxASTTableCellPropertiesNode,
    DocxASTSectionPropertiesNode,
)
from tests.docx.conftest import (
    make_docx,
    make_encrypted_docx,
    make_docx_missing_document,
    make_docx_bad_xml,
    make_docx_no_body,
)


# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

_W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

_CONTENT_TYPES_MINIMAL = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/word/document.xml"
    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
</Types>"""

_RELS_MINIMAL = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
    Target="word/document.xml"/>
</Relationships>"""

_WORD_RELS_EMPTY = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
</Relationships>"""


def _build_docx(
    body_xml: str,
    styles_xml: str = "",
    numbering_xml: str = "",
    word_rels_extra: str = "",
) -> bytes:
    """Build a minimal but valid DOCX from raw body XML and optional extras.

    Args:
        body_xml: XML content to place inside <w:body>.
        styles_xml: Full styles.xml content (empty string = omit).
        numbering_xml: Full numbering.xml content (empty string = omit).
        word_rels_extra: Extra <Relationship> elements for word/_rels/document.xml.rels.

    Returns:
        Raw DOCX bytes.
    """
    document_xml = (
        f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        f'<w:document xmlns:w="{_W}">'
        f'<w:body>{body_xml}</w:body>'
        f'</w:document>'
    )

    word_rels = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
        + word_rels_extra
        + '</Relationships>'
    )

    content_types = _CONTENT_TYPES_MINIMAL
    if styles_xml:
        content_types = content_types.replace(
            "</Types>",
            '  <Override PartName="/word/styles.xml"'
            ' ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>'
            "\n</Types>",
        )
    if numbering_xml:
        content_types = content_types.replace(
            "</Types>",
            '  <Override PartName="/word/numbering.xml"'
            ' ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"/>'
            "\n</Types>",
        )

    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as zf:
        zf.writestr("[Content_Types].xml", content_types)
        zf.writestr("_rels/.rels", _RELS_MINIMAL)
        zf.writestr("word/_rels/document.xml.rels", word_rels)
        zf.writestr("word/document.xml", document_xml)
        if styles_xml:
            zf.writestr("word/styles.xml", styles_xml)
        if numbering_xml:
            zf.writestr("word/numbering.xml", numbering_xml)

    return buf.getvalue()


def _para(text: str, style_id: str = "") -> str:
    """Return a minimal <w:p> XML string."""
    ppr = f"<w:pPr><w:pStyle w:val=\"{style_id}\"/></w:pPr>" if style_id else ""
    return (
        f'<w:p xmlns:w="{_W}">'
        f'{ppr}'
        f'<w:r><w:t>{text}</w:t></w:r>'
        f'</w:p>'
    )


def _run_with_rpr(rpr_inner_xml: str, text: str) -> str:
    """Return a <w:p> containing a run with the given rPr content."""
    return (
        f'<w:p xmlns:w="{_W}">'
        f'<w:r>'
        f'<w:rPr>{rpr_inner_xml}</w:rPr>'
        f'<w:t>{text}</w:t>'
        f'</w:r>'
        f'</w:p>'
    )


# ---------------------------------------------------------------------------
# Helpers for navigating the parsed tree
# ---------------------------------------------------------------------------

def _body(doc: DocxASTDocumentNode) -> DocxASTBodyNode:
    return next(c for c in doc.children if isinstance(c, DocxASTBodyNode))


def _first_para(doc: DocxASTDocumentNode) -> DocxASTParagraphNode:
    return next(
        c for c in _body(doc).children if isinstance(c, DocxASTParagraphNode)
    )


def _first_run(para: DocxASTParagraphNode) -> DocxASTRunNode:
    return next(c for c in para.children if isinstance(c, DocxASTRunNode))


def _ppr(para: DocxASTParagraphNode) -> DocxASTParagraphPropertiesNode:
    return next(
        c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode)
    )


def _rpr(run: DocxASTRunNode) -> DocxASTRunPropertiesNode:
    return next(c for c in run.children if isinstance(c, DocxASTRunPropertiesNode))


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

class TestParserErrors:
    def test_not_a_zip(self):
        with pytest.raises(DocxParseError, match="ZIP open failed"):
            parse_docx(b"not a zip file")

    def test_encrypted(self):
        with pytest.raises(DocxUnsupportedError, match="encrypted"):
            parse_docx(make_encrypted_docx())

    def test_missing_document_xml(self):
        with pytest.raises(DocxParseError, match="missing"):
            parse_docx(make_docx_missing_document())

    def test_malformed_xml(self):
        with pytest.raises(DocxParseError, match="Malformed XML"):
            parse_docx(make_docx_bad_xml())

    def test_no_body(self):
        with pytest.raises(DocxExtractionError, match="w:body"):
            parse_docx(make_docx_no_body())


# ---------------------------------------------------------------------------
# Document structure
# ---------------------------------------------------------------------------

class TestDocumentStructure:
    def test_returns_document_node(self):
        doc = parse_docx(make_docx(["Hello"]))
        assert isinstance(doc, DocxASTDocumentNode)

    def test_source_path_stored(self):
        doc = parse_docx(make_docx(["Hello"]), source_path="/tmp/test.docx")
        assert doc.source_path == "/tmp/test.docx"

    def test_source_path_none_by_default(self):
        doc = parse_docx(make_docx(["Hello"]))
        assert doc.source_path is None

    def test_body_is_child(self):
        doc = parse_docx(make_docx(["Hello"]))
        bodies = [c for c in doc.children if isinstance(c, DocxASTBodyNode)]
        assert len(bodies) == 1

    def test_parse_convenience_function(self):
        doc = parse_docx(make_docx(["Hello"]))
        assert isinstance(doc, DocxASTDocumentNode)

    def test_parser_class_directly(self):
        parser = DocxASTParser()
        doc = parser.parse(make_docx(["Hello"]))
        assert isinstance(doc, DocxASTDocumentNode)

    def test_no_styles_when_absent(self):
        doc = parse_docx(make_docx(["Hello"]))
        styles = [c for c in doc.children if isinstance(c, DocxASTStylesNode)]
        assert len(styles) == 0

    def test_no_numbering_when_absent(self):
        doc = parse_docx(make_docx(["Hello"]))
        numbering = [c for c in doc.children if isinstance(c, DocxASTNumberingNode)]
        assert len(numbering) == 0


# ---------------------------------------------------------------------------
# Paragraph parsing
# ---------------------------------------------------------------------------

class TestParagraphParsing:
    def test_single_paragraph(self):
        doc = parse_docx(make_docx(["Hello world"]))
        body = _body(doc)
        paras = [c for c in body.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 1

    def test_multiple_paragraphs(self):
        doc = parse_docx(make_docx(["First", "Second", "Third"]))
        body = _body(doc)
        paras = [c for c in body.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 3

    def test_empty_paragraph(self):
        doc = parse_docx(make_docx([""]))
        body = _body(doc)
        paras = [c for c in body.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 1

    def test_text_content(self):
        doc = parse_docx(make_docx(["Hello world"]))
        para = _first_para(doc)
        run = _first_run(para)
        texts = [c for c in run.children if isinstance(c, DocxASTTextNode)]
        assert texts[0].content == "Hello world"

    def test_paragraph_with_style(self):
        data = _build_docx(_para("My Heading", style_id="Heading1"))
        doc = parse_docx(data)
        para = _first_para(doc)
        ppr_node = _ppr(para)
        assert ppr_node.style_id == "Heading1"

    def test_paragraph_justification(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:pPr><w:jc w:val="center"/></w:pPr>'
            f'<w:r><w:t>Centred</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ppr_node = _ppr(para)
        assert ppr_node.justification == "center"

    def test_paragraph_outline_level(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:pPr><w:outlineLvl w:val="1"/></w:pPr>'
            f'<w:r><w:t>Level 2</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ppr_node = _ppr(para)
        assert ppr_node.outline_level == 1

    def test_paragraph_spacing(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:pPr><w:spacing w:before="240" w:after="120"/></w:pPr>'
            f'<w:r><w:t>Spaced</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ppr_node = _ppr(para)
        assert ppr_node.spacing_before == 240
        assert ppr_node.spacing_after == 120

    def test_paragraph_indent(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:pPr><w:ind w:left="720" w:hanging="360"/></w:pPr>'
            f'<w:r><w:t>Indented</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ppr_node = _ppr(para)
        assert ppr_node.indent_left == 720
        assert ppr_node.indent_hanging == 360

    def test_paragraph_keep_next(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:pPr><w:keepNext/></w:pPr>'
            f'<w:r><w:t>Keep</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ppr_node = _ppr(para)
        assert ppr_node.keep_next is True

    def test_paragraph_page_break_before(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:pPr><w:pageBreakBefore/></w:pPr>'
            f'<w:r><w:t>New page</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ppr_node = _ppr(para)
        assert ppr_node.page_break_before is True

    def test_numbering_properties_on_paragraph(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:pPr>'
            f'<w:numPr>'
            f'<w:ilvl w:val="0"/>'
            f'<w:numId w:val="1"/>'
            f'</w:numPr>'
            f'</w:pPr>'
            f'<w:r><w:t>Item</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ppr_node = _ppr(para)
        num_pr = next(
            (c for c in ppr_node.children if isinstance(c, DocxASTNumberingPropertiesNode)),
            None,
        )
        assert num_pr is not None
        assert num_pr.num_id == "1"
        assert num_pr.ilvl == 0


# ---------------------------------------------------------------------------
# Run and inline content parsing
# ---------------------------------------------------------------------------

class TestRunParsing:
    def test_bold(self):
        doc = parse_docx(_build_docx(_run_with_rpr("<w:b/>", "Bold")))
        assert _rpr(_first_run(_first_para(doc))).bold is True

    def test_bold_val_zero_not_bold(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:b w:val="0"/>', "Not bold")))
        assert _rpr(_first_run(_first_para(doc))).bold is False

    def test_italic(self):
        doc = parse_docx(_build_docx(_run_with_rpr("<w:i/>", "Italic")))
        assert _rpr(_first_run(_first_para(doc))).italic is True

    def test_bcs_sets_bold(self):
        doc = parse_docx(_build_docx(_run_with_rpr("<w:bCs/>", "Bold CS")))
        assert _rpr(_first_run(_first_para(doc))).bold is True

    def test_ics_sets_italic(self):
        doc = parse_docx(_build_docx(_run_with_rpr("<w:iCs/>", "Italic CS")))
        assert _rpr(_first_run(_first_para(doc))).italic is True

    def test_underline(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:u w:val="single"/>', "Underlined")))
        assert _rpr(_first_run(_first_para(doc))).underline == "single"

    def test_strikethrough(self):
        doc = parse_docx(_build_docx(_run_with_rpr("<w:strike/>", "Struck")))
        assert _rpr(_first_run(_first_para(doc))).strike is True

    def test_double_strike(self):
        doc = parse_docx(_build_docx(_run_with_rpr("<w:dstrike/>", "DStruck")))
        assert _rpr(_first_run(_first_para(doc))).double_strike is True

    def test_superscript(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:vertAlign w:val="superscript"/>', "sup")))
        assert _rpr(_first_run(_first_para(doc))).vertical_align == "superscript"

    def test_subscript(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:vertAlign w:val="subscript"/>', "sub")))
        assert _rpr(_first_run(_first_para(doc))).vertical_align == "subscript"

    def test_font_size(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:sz w:val="48"/>', "Large")))
        assert _rpr(_first_run(_first_para(doc))).sz == 48

    def test_color(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:color w:val="FF0000"/>', "Red")))
        assert _rpr(_first_run(_first_para(doc))).color == "FF0000"

    def test_highlight(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:highlight w:val="yellow"/>', "Highlighted")))
        assert _rpr(_first_run(_first_para(doc))).highlight == "yellow"

    def test_rStyle(self):
        doc = parse_docx(_build_docx(_run_with_rpr('<w:rStyle w:val="CodeChar"/>', "Code")))
        assert _rpr(_first_run(_first_para(doc))).style_id == "CodeChar"

    def test_font_ascii(self):
        doc = parse_docx(_build_docx(
            _run_with_rpr('<w:rFonts w:ascii="Courier New" w:hAnsi="Courier New"/>', "Mono")
        ))
        rpr = _rpr(_first_run(_first_para(doc)))
        assert rpr.font_ascii == "Courier New"
        assert rpr.font_h_ansi == "Courier New"

    def test_tab_node(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:r><w:t>Before</w:t><w:tab/><w:t>After</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        run = _first_run(_first_para(doc))
        tabs = [c for c in run.children if isinstance(c, DocxASTTabNode)]
        assert len(tabs) == 1

    def test_page_break(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:r><w:br w:type="page"/></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        run = _first_run(_first_para(doc))
        breaks = [c for c in run.children if isinstance(c, DocxASTBreakNode)]
        assert len(breaks) == 1
        assert breaks[0].break_type == "page"

    def test_line_break_default_type(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:r><w:br/></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        run = _first_run(_first_para(doc))
        breaks = [c for c in run.children if isinstance(c, DocxASTBreakNode)]
        assert breaks[0].break_type == "textWrapping"

    def test_last_rendered_page_break(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:r><w:lastRenderedPageBreak/><w:t>Text</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        run = _first_run(_first_para(doc))
        lrpb = [c for c in run.children if isinstance(c, DocxASTLastRenderedPageBreakNode)]
        assert len(lrpb) == 1

    def test_preserve_space(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:r><w:t xml:space="preserve"> spaced </w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        run = _first_run(_first_para(doc))
        texts = [c for c in run.children if isinstance(c, DocxASTTextNode)]
        assert texts[0].preserve_space is True

    def test_no_preserve_space_by_default(self):
        # A <w:t> without xml:space="preserve" should have preserve_space=False
        body_xml = f'<w:p xmlns:w="{_W}"><w:r><w:t>Hello</w:t></w:r></w:p>'
        doc = parse_docx(_build_docx(body_xml))
        run = _first_run(_first_para(doc))
        texts = [c for c in run.children if isinstance(c, DocxASTTextNode)]
        assert texts[0].preserve_space is False

    def test_multiple_runs_in_paragraph(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:r><w:t>First</w:t></w:r>'
            f'<w:r><w:t>Second</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        runs = [c for c in para.children if isinstance(c, DocxASTRunNode)]
        assert len(runs) == 2


# ---------------------------------------------------------------------------
# Bookmark parsing
# ---------------------------------------------------------------------------

class TestBookmarkParsing:
    def test_bookmark_start(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:bookmarkStart w:id="0" w:name="intro"/>'
            f'<w:r><w:t>Text</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        starts = [c for c in para.children if isinstance(c, DocxASTBookmarkStartNode)]
        assert len(starts) == 1
        assert starts[0].bookmark_id == "0"
        assert starts[0].name == "intro"

    def test_bookmark_end(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:r><w:t>Text</w:t></w:r>'
            f'<w:bookmarkEnd w:id="0"/>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        ends = [c for c in para.children if isinstance(c, DocxASTBookmarkEndNode)]
        assert len(ends) == 1
        assert ends[0].bookmark_id == "0"

    def test_bookmark_start_and_end_together(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}">'
            f'<w:bookmarkStart w:id="1" w:name="section-a"/>'
            f'<w:r><w:t>Content</w:t></w:r>'
            f'<w:bookmarkEnd w:id="1"/>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        starts = [c for c in para.children if isinstance(c, DocxASTBookmarkStartNode)]
        ends = [c for c in para.children if isinstance(c, DocxASTBookmarkEndNode)]
        assert starts[0].name == "section-a"
        assert ends[0].bookmark_id == "1"


# ---------------------------------------------------------------------------
# Hyperlink parsing
# ---------------------------------------------------------------------------

_R_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"


class TestHyperlinkParsing:
    def test_internal_hyperlink_anchor(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}" xmlns:r="{_R_NS}">'
            f'<w:hyperlink w:anchor="_Toc123">'
            f'<w:r><w:t>link text</w:t></w:r>'
            f'</w:hyperlink>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        links = [c for c in para.children if isinstance(c, DocxASTHyperlinkNode)]
        assert len(links) == 1
        assert links[0].anchor == "_Toc123"
        assert links[0].url == ""
        runs = [c for c in links[0].children if isinstance(c, DocxASTRunNode)]
        assert len(runs) == 1

    def test_external_hyperlink_resolves_url(self):
        word_rels = (
            '<Relationship Id="rId5"'
            ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"'
            ' Target="https://example.com" TargetMode="External"/>'
        )
        body_xml = (
            f'<w:p xmlns:w="{_W}" xmlns:r="{_R_NS}">'
            f'<w:hyperlink r:id="rId5">'
            f'<w:r><w:t>click here</w:t></w:r>'
            f'</w:hyperlink>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml, word_rels_extra=word_rels))
        para = _first_para(doc)
        links = [c for c in para.children if isinstance(c, DocxASTHyperlinkNode)]
        assert len(links) == 1
        assert links[0].url == "https://example.com"

    def test_hyperlink_with_bookmark_discards_url(self):
        word_rels = (
            '<Relationship Id="rId5"'
            ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"'
            ' Target="http://f.pp.pt/" TargetMode="External"/>'
        )
        body_xml = (
            f'<w:p xmlns:w="{_W}" xmlns:r="{_R_NS}">'
            f'<w:hyperlink r:id="rId5">'
            f'<w:bookmarkStart w:id="1" w:name="_Toc999"/>'
            f'<w:r><w:t>F.PP.PT</w:t></w:r>'
            f'<w:bookmarkEnd w:id="1"/>'
            f'</w:hyperlink>'
            f'<w:r><w:t>.008 Title</w:t></w:r>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml, word_rels_extra=word_rels))
        para = _first_para(doc)
        links = [c for c in para.children if isinstance(c, DocxASTHyperlinkNode)]
        assert len(links) == 1
        assert links[0].url == ""
        runs = [c for c in links[0].children if isinstance(c, DocxASTRunNode)]
        assert len(runs) == 1

    def test_hyperlink_text_extracted_in_heading(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}" xmlns:r="{_R_NS}">'
            f'<w:pPr><w:pStyle w:val="Heading5"/></w:pPr>'
            f'<w:hyperlink r:id="rId5">'
            f'<w:bookmarkStart w:id="1" w:name="_Toc999"/>'
            f'<w:r><w:t>F.PP.PT</w:t></w:r>'
            f'</w:hyperlink>'
            f'<w:r><w:t>.008 \u2013 Title</w:t></w:r>'
            f'</w:p>'
        )
        word_rels = (
            '<Relationship Id="rId5"'
            ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"'
            ' Target="http://f.pp.pt/" TargetMode="External"/>'
        )
        doc = parse_docx(_build_docx(body_xml, word_rels_extra=word_rels))
        para = _first_para(doc)
        links = [c for c in para.children if isinstance(c, DocxASTHyperlinkNode)]
        assert len(links) == 1
        runs = [c for c in links[0].children if isinstance(c, DocxASTRunNode)]
        text_nodes = [c for c in runs[0].children if isinstance(c, DocxASTTextNode)]
        assert text_nodes[0].content == "F.PP.PT"

    def test_hyperlink_with_multiple_runs(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}" xmlns:r="{_R_NS}">'
            f'<w:hyperlink w:anchor="_Toc1">'
            f'<w:r><w:t>part 1</w:t></w:r>'
            f'<w:r><w:t>part 2</w:t></w:r>'
            f'</w:hyperlink>'
            f'</w:p>'
        )
        doc = parse_docx(_build_docx(body_xml))
        para = _first_para(doc)
        links = [c for c in para.children if isinstance(c, DocxASTHyperlinkNode)]
        assert len(links) == 1
        runs = [c for c in links[0].children if isinstance(c, DocxASTRunNode)]
        assert len(runs) == 2


# ---------------------------------------------------------------------------
# Table parsing
# ---------------------------------------------------------------------------

def _simple_table_xml(cell_texts: list) -> str:
    """Build a single-row table with one cell per text entry."""
    cells = ""
    for text in cell_texts:
        cells += (
            f'<w:tc>'
            f'<w:tcPr><w:tcW w:w="2000" w:type="dxa"/></w:tcPr>'
            f'<w:p><w:r><w:t>{text}</w:t></w:r></w:p>'
            f'</w:tc>'
        )
    return (
        f'<w:tbl xmlns:w="{_W}">'
        f'<w:tblPr><w:tblW w:w="0" w:type="auto"/></w:tblPr>'
        f'<w:tblGrid><w:gridCol w:w="2000"/></w:tblGrid>'
        f'<w:tr><w:trPr><w:cantSplit/></w:trPr>{cells}</w:tr>'
        f'</w:tbl>'
    )


class TestTableParsing:
    def test_table_node_present(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["A", "B"])))
        body = _body(doc)
        tables = [c for c in body.children if isinstance(c, DocxASTTableNode)]
        assert len(tables) == 1

    def test_table_has_properties(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["A"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        props = [c for c in table.children if isinstance(c, DocxASTTablePropertiesNode)]
        assert len(props) == 1
        assert props[0].width_type == "auto"

    def test_table_grid(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["A"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        grid = next(c for c in table.children if isinstance(c, DocxASTTableGridNode))
        assert grid.column_widths == [2000]

    def test_table_has_row(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["A", "B"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        rows = [c for c in table.children if isinstance(c, DocxASTTableRowNode)]
        assert len(rows) == 1

    def test_row_properties_cant_split(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["A"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        rpr = next(
            (c for c in row.children if isinstance(c, DocxASTTableRowPropertiesNode)),
            None,
        )
        assert rpr is not None
        assert rpr.cant_split is True

    def test_two_cells(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["A", "B"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cells = [c for c in row.children if isinstance(c, DocxASTTableCellNode)]
        assert len(cells) == 2

    def test_cell_properties(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["A"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        cpr = next(c for c in cell.children if isinstance(c, DocxASTTableCellPropertiesNode))
        assert cpr.width == 2000
        assert cpr.width_type == "dxa"

    def test_cell_contains_paragraph(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["Hello"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        paras = [c for c in cell.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 1

    def test_cell_text_content(self):
        doc = parse_docx(_build_docx(_simple_table_xml(["CellText"])))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        para = next(c for c in cell.children if isinstance(c, DocxASTParagraphNode))
        run = _first_run(para)
        texts = [c for c in run.children if isinstance(c, DocxASTTextNode)]
        assert texts[0].content == "CellText"

    def test_grid_span(self):
        body_xml = (
            f'<w:tbl xmlns:w="{_W}">'
            f'<w:tr>'
            f'<w:tc>'
            f'<w:tcPr>'
            f'<w:tcW w:w="6000" w:type="dxa"/>'
            f'<w:gridSpan w:val="3"/>'
            f'</w:tcPr>'
            f'<w:p><w:r><w:t>Spanning</w:t></w:r></w:p>'
            f'</w:tc>'
            f'</w:tr>'
            f'</w:tbl>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        cpr = next(c for c in cell.children if isinstance(c, DocxASTTableCellPropertiesNode))
        assert cpr.grid_span == 3

    def test_vertical_merge_restart(self):
        body_xml = (
            f'<w:tbl xmlns:w="{_W}">'
            f'<w:tr>'
            f'<w:tc>'
            f'<w:tcPr><w:vMerge w:val="restart"/></w:tcPr>'
            f'<w:p><w:r><w:t>Merged</w:t></w:r></w:p>'
            f'</w:tc>'
            f'</w:tr>'
            f'</w:tbl>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        cpr = next(c for c in cell.children if isinstance(c, DocxASTTableCellPropertiesNode))
        assert cpr.vertical_merge == "restart"

    def test_vertical_merge_continue(self):
        body_xml = (
            f'<w:tbl xmlns:w="{_W}">'
            f'<w:tr>'
            f'<w:tc>'
            f'<w:tcPr><w:vMerge/></w:tcPr>'
            f'<w:p><w:r><w:t></w:t></w:r></w:p>'
            f'</w:tc>'
            f'</w:tr>'
            f'</w:tbl>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        cpr = next(c for c in cell.children if isinstance(c, DocxASTTableCellPropertiesNode))
        assert cpr.vertical_merge == "continue"

    def test_multiple_rows(self):
        body_xml = (
            f'<w:tbl xmlns:w="{_W}">'
            f'<w:tr><w:tc><w:p><w:r><w:t>R1</w:t></w:r></w:p></w:tc></w:tr>'
            f'<w:tr><w:tc><w:p><w:r><w:t>R2</w:t></w:r></w:p></w:tc></w:tr>'
            f'<w:tr><w:tc><w:p><w:r><w:t>R3</w:t></w:r></w:p></w:tc></w:tr>'
            f'</w:tbl>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        rows = [c for c in table.children if isinstance(c, DocxASTTableRowNode)]
        assert len(rows) == 3

    def test_nested_table(self):
        body_xml = (
            f'<w:tbl xmlns:w="{_W}">'
            f'<w:tr>'
            f'<w:tc>'
            f'<w:p><w:r><w:t>Outer</w:t></w:r></w:p>'
            f'<w:tbl>'
            f'<w:tr><w:tc><w:p><w:r><w:t>Inner</w:t></w:r></w:p></w:tc></w:tr>'
            f'</w:tbl>'
            f'</w:tc>'
            f'</w:tr>'
            f'</w:tbl>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        outer = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in outer.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        inner_tables = [c for c in cell.children if isinstance(c, DocxASTTableNode)]
        assert len(inner_tables) == 1

    def test_cell_multiple_paragraphs(self):
        body_xml = (
            f'<w:tbl xmlns:w="{_W}">'
            f'<w:tr>'
            f'<w:tc>'
            f'<w:p><w:r><w:t>Para 1</w:t></w:r></w:p>'
            f'<w:p><w:r><w:t>Para 2</w:t></w:r></w:p>'
            f'</w:tc>'
            f'</w:tr>'
            f'</w:tbl>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        row = next(c for c in table.children if isinstance(c, DocxASTTableRowNode))
        cell = next(c for c in row.children if isinstance(c, DocxASTTableCellNode))
        paras = [c for c in cell.children if isinstance(c, DocxASTParagraphNode)]
        assert len(paras) == 2

    def test_table_style_id(self):
        body_xml = (
            f'<w:tbl xmlns:w="{_W}">'
            f'<w:tblPr><w:tblStyle w:val="TableGrid"/></w:tblPr>'
            f'<w:tr><w:tc><w:p><w:r><w:t>A</w:t></w:r></w:p></w:tc></w:tr>'
            f'</w:tbl>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        table = next(c for c in body.children if isinstance(c, DocxASTTableNode))
        props = next(c for c in table.children if isinstance(c, DocxASTTablePropertiesNode))
        assert props.style_id == "TableGrid"


# ---------------------------------------------------------------------------
# Section properties
# ---------------------------------------------------------------------------

class TestSectionProperties:
    def test_section_properties_parsed(self):
        body_xml = (
            f'<w:p xmlns:w="{_W}"><w:r><w:t>Text</w:t></w:r></w:p>'
            f'<w:sectPr xmlns:w="{_W}">'
            f'<w:pgSz w:w="12240" w:h="15840"/>'
            f'<w:pgMar w:top="1440" w:right="1800" w:bottom="1440" w:left="1800"'
            f' w:header="720" w:footer="720" w:gutter="0"/>'
            f'</w:sectPr>'
        )
        doc = parse_docx(_build_docx(body_xml))
        body = _body(doc)
        sect = next(
            (c for c in body.children if isinstance(c, DocxASTSectionPropertiesNode)),
            None,
        )
        assert sect is not None
        assert sect.page_width == 12240
        assert sect.page_height == 15840
        assert sect.margin_top == 1440
        assert sect.margin_right == 1800
        assert sect.margin_bottom == 1440
        assert sect.margin_left == 1800

    def test_section_properties_absent(self):
        doc = parse_docx(make_docx(["Hello"]))
        body = _body(doc)
        sects = [c for c in body.children if isinstance(c, DocxASTSectionPropertiesNode)]
        assert len(sects) == 0


# ---------------------------------------------------------------------------
# Styles parsing
# ---------------------------------------------------------------------------

def _styles_xml(style_defs: str, doc_defaults: str = "") -> str:
    defaults_block = (
        f'<w:docDefaults xmlns:w="{_W}">'
        f'{doc_defaults}'
        f'</w:docDefaults>'
        if doc_defaults
        else ""
    )
    return (
        f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        f'<w:styles xmlns:w="{_W}">'
        f'{defaults_block}'
        f'{style_defs}'
        f'</w:styles>'
    )


class TestStylesParsing:
    def test_styles_node_present(self):
        styles = _styles_xml(
            f'<w:style w:type="paragraph" w:default="1" w:styleId="Normal">'
            f'<w:name w:val="Normal"/>'
            f'</w:style>'
        )
        doc = parse_docx(_build_docx(_para("Hello"), styles_xml=styles))
        nodes = [c for c in doc.children if isinstance(c, DocxASTStylesNode)]
        assert len(nodes) == 1

    def test_default_style_flag(self):
        styles = _styles_xml(
            f'<w:style w:type="paragraph" w:default="1" w:styleId="Normal">'
            f'<w:name w:val="Normal"/>'
            f'</w:style>'
        )
        doc = parse_docx(_build_docx(_para("Hello"), styles_xml=styles))
        styles_node = next(c for c in doc.children if isinstance(c, DocxASTStylesNode))
        normal = next(c for c in styles_node.children if isinstance(c, DocxASTStyleNode))
        assert normal.is_default is True

    def test_style_inheritance(self):
        styles = _styles_xml(
            f'<w:style w:type="paragraph" w:styleId="Normal">'
            f'<w:name w:val="Normal"/>'
            f'</w:style>'
            f'<w:style w:type="paragraph" w:styleId="Heading1">'
            f'<w:name w:val="heading 1"/>'
            f'<w:basedOn w:val="Normal"/>'
            f'<w:next w:val="Normal"/>'
            f'</w:style>'
        )
        doc = parse_docx(_build_docx(_para("Hello"), styles_xml=styles))
        styles_node = next(c for c in doc.children if isinstance(c, DocxASTStylesNode))
        h1 = next(
            c for c in styles_node.children
            if isinstance(c, DocxASTStyleNode) and c.style_id == "Heading1"
        )
        assert h1.based_on == "Normal"
        assert h1.next_style == "Normal"

    def test_style_run_properties(self):
        styles = _styles_xml(
            f'<w:style w:type="paragraph" w:styleId="Heading1">'
            f'<w:name w:val="heading 1"/>'
            f'<w:rPr><w:b/><w:sz w:val="32"/></w:rPr>'
            f'</w:style>'
        )
        doc = parse_docx(_build_docx(_para("Hello"), styles_xml=styles))
        styles_node = next(c for c in doc.children if isinstance(c, DocxASTStylesNode))
        h1 = next(
            c for c in styles_node.children
            if isinstance(c, DocxASTStyleNode) and c.style_id == "Heading1"
        )
        rpr = next(c for c in h1.children if isinstance(c, DocxASTRunPropertiesNode))
        assert rpr.bold is True
        assert rpr.sz == 32

    def test_style_paragraph_properties(self):
        styles = _styles_xml(
            f'<w:style w:type="paragraph" w:styleId="Heading1">'
            f'<w:name w:val="heading 1"/>'
            f'<w:pPr><w:outlineLvl w:val="0"/></w:pPr>'
            f'</w:style>'
        )
        doc = parse_docx(_build_docx(_para("Hello"), styles_xml=styles))
        styles_node = next(c for c in doc.children if isinstance(c, DocxASTStylesNode))
        h1 = next(
            c for c in styles_node.children
            if isinstance(c, DocxASTStyleNode) and c.style_id == "Heading1"
        )
        ppr = next(c for c in h1.children if isinstance(c, DocxASTParagraphPropertiesNode))
        assert ppr.outline_level == 0

    def test_default_run_properties_from_doc_defaults(self):
        doc_defaults = (
            f'<w:rPrDefault xmlns:w="{_W}">'
            f'<w:rPr>'
            f'<w:rFonts w:ascii="Arial" w:hAnsi="Arial"/>'
            f'<w:sz w:val="24"/>'
            f'</w:rPr>'
            f'</w:rPrDefault>'
        )
        styles = _styles_xml("", doc_defaults=doc_defaults)
        doc = parse_docx(_build_docx(_para("Hello"), styles_xml=styles))
        styles_node = next(c for c in doc.children if isinstance(c, DocxASTStylesNode))
        assert styles_node.default_run_properties is not None
        assert styles_node.default_run_properties.font_ascii == "Arial"
        assert styles_node.default_run_properties.sz == 24

    def test_character_style(self):
        styles = _styles_xml(
            f'<w:style w:type="character" w:styleId="CodeChar">'
            f'<w:name w:val="Code Char"/>'
            f'<w:rPr>'
            f'<w:rFonts w:ascii="Courier New" w:hAnsi="Courier New"/>'
            f'</w:rPr>'
            f'</w:style>'
        )
        doc = parse_docx(_build_docx(_para("Hello"), styles_xml=styles))
        styles_node = next(c for c in doc.children if isinstance(c, DocxASTStylesNode))
        code_char = next(
            c for c in styles_node.children
            if isinstance(c, DocxASTStyleNode) and c.style_id == "CodeChar"
        )
        assert code_char.style_type == "character"
        assert code_char.name == "Code Char"


# ---------------------------------------------------------------------------
# Numbering parsing
# ---------------------------------------------------------------------------

def _numbering_xml(num_fmt: str = "bullet", lvl_text: str = "\u2022") -> str:
    return (
        f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        f'<w:numbering xmlns:w="{_W}">'
        f'<w:abstractNum w:abstractNumId="0">'
        f'<w:multiLevelType w:val="hybridMultilevel"/>'
        f'<w:lvl w:ilvl="0">'
        f'<w:start w:val="1"/>'
        f'<w:numFmt w:val="{num_fmt}"/>'
        f'<w:lvlText w:val="{lvl_text}"/>'
        f'<w:lvlJc w:val="left"/>'
        f'<w:pPr><w:ind w:left="720" w:hanging="360"/></w:pPr>'
        f'</w:lvl>'
        f'</w:abstractNum>'
        f'<w:num w:numId="1">'
        f'<w:abstractNumId w:val="0"/>'
        f'</w:num>'
        f'</w:numbering>'
    )


class TestNumberingParsing:
    def test_numbering_node_present(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml()))
        nodes = [c for c in doc.children if isinstance(c, DocxASTNumberingNode)]
        assert len(nodes) == 1

    def test_abstract_num_id(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml()))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_nums = [c for c in numbering.children if isinstance(c, DocxASTAbstractNumNode)]
        assert len(abstract_nums) == 1
        assert abstract_nums[0].abstract_num_id == "0"

    def test_multi_level_type(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml()))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_num = next(c for c in numbering.children if isinstance(c, DocxASTAbstractNumNode))
        assert abstract_num.multi_level_type == "hybridMultilevel"

    def test_level_ilvl(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml()))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_num = next(c for c in numbering.children if isinstance(c, DocxASTAbstractNumNode))
        lvl = next(c for c in abstract_num.children if isinstance(c, DocxASTNumLevelNode))
        assert lvl.ilvl == 0

    def test_level_start(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml()))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_num = next(c for c in numbering.children if isinstance(c, DocxASTAbstractNumNode))
        lvl = next(c for c in abstract_num.children if isinstance(c, DocxASTNumLevelNode))
        assert lvl.start == 1

    def test_level_num_fmt_bullet(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml("bullet")))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_num = next(c for c in numbering.children if isinstance(c, DocxASTAbstractNumNode))
        lvl = next(c for c in abstract_num.children if isinstance(c, DocxASTNumLevelNode))
        assert lvl.num_fmt == "bullet"

    def test_level_num_fmt_decimal(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml("decimal", "%1.")))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_num = next(c for c in numbering.children if isinstance(c, DocxASTAbstractNumNode))
        lvl = next(c for c in abstract_num.children if isinstance(c, DocxASTNumLevelNode))
        assert lvl.num_fmt == "decimal"
        assert lvl.lvl_text == "%1."

    def test_level_indent(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml()))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_num = next(c for c in numbering.children if isinstance(c, DocxASTAbstractNumNode))
        lvl = next(c for c in abstract_num.children if isinstance(c, DocxASTNumLevelNode))
        assert lvl.indent_left == 720
        assert lvl.indent_hanging == 360

    def test_num_node(self):
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=_numbering_xml()))
        numbering = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        num_nodes = [c for c in numbering.children if isinstance(c, DocxASTNumNode)]
        assert len(num_nodes) == 1
        assert num_nodes[0].num_id == "1"
        assert num_nodes[0].abstract_num_id == "0"

    def test_level_font_for_bullet(self):
        numbering = (
            f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            f'<w:numbering xmlns:w="{_W}">'
            f'<w:abstractNum w:abstractNumId="0">'
            f'<w:lvl w:ilvl="0">'
            f'<w:numFmt w:val="bullet"/>'
            f'<w:lvlText w:val=""/>'
            f'<w:lvlJc w:val="left"/>'
            f'<w:rPr>'
            f'<w:rFonts w:ascii="Symbol" w:hAnsi="Symbol"/>'
            f'</w:rPr>'
            f'</w:lvl>'
            f'</w:abstractNum>'
            f'<w:num w:numId="1"><w:abstractNumId w:val="0"/></w:num>'
            f'</w:numbering>'
        )
        doc = parse_docx(_build_docx(_para("Item"), numbering_xml=numbering))
        numbering_node = next(c for c in doc.children if isinstance(c, DocxASTNumberingNode))
        abstract_num = next(c for c in numbering_node.children if isinstance(c, DocxASTAbstractNumNode))
        lvl = next(c for c in abstract_num.children if isinstance(c, DocxASTNumLevelNode))
        assert lvl.font_ascii == "Symbol"
        assert lvl.font_h_ansi == "Symbol"
