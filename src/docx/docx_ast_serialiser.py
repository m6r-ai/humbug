"""
Serialises a DocxASTDocumentNode to a .docx file (ZIP/OOXML).

The serialiser walks the AST and produces XML strings for each component,
then assembles them into a valid .docx ZIP archive using only the Python
standard library.
"""

import io
from pathlib import Path
from xml.sax import saxutils
import struct
import zipfile

from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
    DocxASTBodyNode,
    DocxASTBreakNode,
    DocxASTDocumentNode,
    DocxASTDrawingNode,
    DocxASTHyperlinkNode,
    DocxASTLastRenderedPageBreakNode,
    DocxASTNode,
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
    DocxASTTablePropertiesNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTextNode,
)

# ZIP entry paths
_CONTENT_TYPES_PATH = "[Content_Types].xml"
_RELS_PATH = "_rels/.rels"
_DOCUMENT_PATH = "word/document.xml"
_DOCUMENT_RELS_PATH = "word/_rels/document.xml.rels"
_STYLES_PATH = "word/styles.xml"
_NUMBERING_PATH = "word/numbering.xml"
_SETTINGS_PATH = "word/settings.xml"

# XML namespace URIs
_W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
_R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
_WP = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
_A = "http://schemas.openxmlformats.org/drawingml/2006/main"
_PIC = "http://schemas.openxmlformats.org/drawingml/2006/picture"
_REL_STYLES = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"
_REL_NUMBERING = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering"
_REL_SETTINGS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings"
_REL_OFFICE_DOC = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
_REL_HYPERLINK = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"
_REL_IMAGE = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
_CT_RELS = "application/vnd.openxmlformats-package.relationships+xml"
_CT_XML = "application/xml"
_CT_DOCUMENT = "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"
_CT_STYLES = "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"
_CT_NUMBERING = "application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"
_CT_SETTINGS = "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"

# Default image dimensions used when the source drawing carries no size information.
# 3 inches × 2 inches expressed in EMU (914400 EMU = 1 inch).
_DEFAULT_WIDTH_EMU = 2_743_200
_DEFAULT_HEIGHT_EMU = 1_828_800

# Conversion factor: 1 twip = 635 EMU (914400 EMU/inch ÷ 1440 twips/inch).
_EMU_PER_TWIP = 635
# Full text area width in EMU (9360 twips × 635).
_TEXT_WIDTH_EMU = 9360 * _EMU_PER_TWIP

# Maps lowercase image file extensions to OOXML content-type strings.
_IMAGE_CONTENT_TYPES: dict[str, str] = {
    "png": "image/png",
    "jpg": "image/jpeg",
    "jpeg": "image/jpeg",
    "gif": "image/gif",
    "webp": "image/webp",
    "bmp": "image/bmp",
    "tiff": "image/tiff",
    "tif": "image/tiff",
    "emf": "image/x-emf",
    "wmf": "image/x-wmf",
}

# Word 2013+ compatibility mode value — prevents MS Word from opening the
# document in compatibility mode.
_COMPAT_MODE_VAL = "15"

# Per-style visual overrides: style_id → dict of extra XML to inject into
# the serialised <w:style> element.  These carry the visual presentation
# that isn't modelled in the AST (fonts, spacing, shading).
_STYLE_VISUALS = {
    "Normal": {
        "ppr_extra": '<w:spacing w:after="200"/>',
        "rpr_extra": '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="24"/>',
    },
    "Heading1": {
        "ppr_extra": '<w:spacing w:before="480"/>',
        "rpr_extra": '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="40"/>',
    },
    "Heading2": {
        "ppr_extra": '<w:spacing w:before="360"/>',
        "rpr_extra": '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="32"/>',
    },
    "Heading3": {
        "ppr_extra": '<w:spacing w:before="280"/>',
        "rpr_extra": '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="28"/>',
    },
    "Heading4": {
        "ppr_extra": '<w:spacing w:before="240"/>',
        "rpr_extra": '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="24"/>',
    },
    "Heading5": {
        "ppr_extra": '<w:spacing w:before="200"/>',
        "rpr_extra": '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="22"/>',
    },
    "Heading6": {
        "ppr_extra": '<w:spacing w:before="200"/>',
        "rpr_extra": '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/><w:sz w:val="22"/>',
    },
    "CodeBlock": {
        "ppr_extra": (
            '<w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="exact"/>'
        ),
        "rpr_extra": (
            '<w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" w:cs="Consolas"/>'
            '<w:sz w:val="20"/>'
        ),
    },
    "Blockquote": {
        "ppr_extra": (
            '<w:shd w:val="clear" w:color="auto" w:fill="E8F0F8"/>'
        ),
        "rpr_extra": (
            '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>'
            '<w:i/>'
            '<w:color w:val="444444"/>'
        ),
    },
    "HorizontalRule": {
        "ppr_extra": (
            '<w:spacing w:before="200" w:after="200" w:line="1" w:lineRule="exact"/>'
            '<w:pBdr>'
            '<w:bottom w:val="single" w:sz="6" w:space="1" w:color="AAAAAA"/>'
            '</w:pBdr>'
        ),
    },
}

# Hyperlink character style visual definition (not in _STYLE_VISUALS since it
# is a character style emitted separately in _serialise_styles)
_HYPERLINK_STYLE_XML = (
    '<w:style w:type="character" w:styleId="Hyperlink">'
    '<w:name w:val="Hyperlink"/>'
    '<w:rPr><w:color w:val="0563C1"/><w:u w:val="single"/></w:rPr>'
    '</w:style>'
)


def _esc(text: str) -> str:
    """Escape a string for safe embedding in XML character data."""
    return saxutils.escape(text)


def _read_image_dimensions(data: bytes) -> tuple[int, int] | None:
    """
    Read the intrinsic pixel dimensions from image bytes.

    Supports PNG, JPEG, GIF, and WEBP using only the Python standard library.
    Returns (width_px, height_px), or None if the format is unrecognised or
    the data is too short to contain a valid header.

    Args:
        data: Raw image bytes.

    Returns:
        A (width, height) tuple in pixels, or None.
    """
    if len(data) < 12:
        return None

    # PNG: 8-byte signature + 4-byte length + 4-byte 'IHDR' + 4-byte w + 4-byte h
    if data[:8] == b'\x89PNG\r\n\x1a\n':
        if len(data) < 24:
            return None

        w, h = struct.unpack('>II', data[16:24])
        return w, h

    # GIF: 'GIF87a' or 'GIF89a' + 2-byte w + 2-byte h (little-endian)
    if data[:6] in (b'GIF87a', b'GIF89a'):
        if len(data) < 10:
            return None

        w, h = struct.unpack('<HH', data[6:10])
        return w, h

    # WEBP: 'RIFF' + 4-byte size + 'WEBP' + 'VP8 '/'VP8L'/'VP8X'
    if data[:4] == b'RIFF' and data[8:12] == b'WEBP':
        chunk = data[12:16]
        if chunk == b'VP8 ' and len(data) >= 30:
            # Lossy: width/height in bits 0-13 of bytes 26-27 and 28-29
            w = (struct.unpack('<H', data[26:28])[0] & 0x3FFF) + 1
            h = (struct.unpack('<H', data[28:30])[0] & 0x3FFF) + 1
            return w, h

        if chunk == b'VP8L' and len(data) >= 25:
            # Lossless: packed bits in bytes 21-24
            bits = struct.unpack('<I', data[21:25])[0]
            w = (bits & 0x3FFF) + 1
            h = ((bits >> 14) & 0x3FFF) + 1
            return w, h

        if chunk == b'VP8X' and len(data) >= 30:
            # Extended: flags(4) then 3-byte canvas width/height minus 1 at bytes 24 and 27
            w = (data[24] | (data[25] << 8) | (data[26] << 16)) + 1
            h = (data[27] | (data[28] << 8) | (data[29] << 16)) + 1
            return w, h

        return None

    # JPEG: scan for SOF markers (0xFF 0xC0..0xC3, 0xC5..0xC7, 0xC9..0xCB, 0xCD..0xCF)
    if data[:2] == b'\xff\xd8':
        i = 2
        while i + 3 < len(data):
            if data[i] != 0xFF:
                break

            marker = data[i + 1]
            if marker in range(0xC0, 0xD0) and marker not in (0xC4, 0xC8, 0xCC):
                if i + 9 <= len(data):
                    h, w = struct.unpack('>HH', data[i + 5:i + 9])
                    return w, h

            # Skip this segment: 2-byte marker + 2-byte length (length includes itself)
            if i + 3 >= len(data):
                break

            seg_len = struct.unpack('>H', data[i + 2:i + 4])[0]
            i += 2 + seg_len

        return None

    return None


# EMU per inch, and assumed screen resolution for images without explicit DPI.
_EMU_PER_INCH = 914_400
_DEFAULT_DPI = 96


def serialise_docx(document: DocxASTDocumentNode) -> bytes:
    """
    Serialise a DocxASTDocumentNode to raw .docx bytes.

    Args:
        document: The DocxASTDocumentNode to serialise.  Should contain
            a DocxASTStylesNode, DocxASTNumberingNode, and DocxASTBodyNode
            as children (as produced by doc_ir_to_docx_ast).

    Returns:
        Raw bytes of a valid .docx file.
    """
    serialiser = _DocxASTSerialiser(document)
    return serialiser.serialise()


class _DocxASTSerialiser:
    """Walks a DocxASTDocumentNode and produces a .docx ZIP archive."""

    def __init__(self, document: DocxASTDocumentNode) -> None:
        self._document = document
        self._has_numbering = False
        # Maps URL → rId, populated during document body serialisation
        self._hyperlink_rels: dict[str, str] = {}
        # Maps rId → (zip_path, image_bytes) for embedded images
        self._image_rels: dict[str, tuple[str, bytes]] = {}
        # Fixed relationship IDs: rId1=styles, rId2=numbering, rId3=settings
        self._next_rel_id: int = 4
        # Available width for images in the current serialisation context, in EMU.
        self._available_width_emu: int = _TEXT_WIDTH_EMU

    def serialise(self) -> bytes:
        """Perform the full serialisation and return raw bytes."""
        styles_node = next(
            (c for c in self._document.children if isinstance(c, DocxASTStylesNode)),
            None,
        )
        numbering_node = next(
            (c for c in self._document.children if isinstance(c, DocxASTNumberingNode)),
            None,
        )
        body_node = next(
            (c for c in self._document.children if isinstance(c, DocxASTBodyNode)),
            None,
        )

        self._has_numbering = numbering_node is not None

        styles_xml = self._serialise_styles(styles_node)
        numbering_xml = self._serialise_numbering(numbering_node) if numbering_node else None
        document_xml = self._serialise_document(body_node)
        settings_xml = self._serialise_settings()

        return self._build_zip(
            document_xml=document_xml,
            styles_xml=styles_xml,
            numbering_xml=numbering_xml,
            settings_xml=settings_xml,
        )

    def _build_zip(
        self,
        document_xml: str,
        styles_xml: str,
        numbering_xml: str | None,
        settings_xml: str,
    ) -> bytes:
        """Assemble all XML parts into a .docx ZIP archive."""
        buf = io.BytesIO()
        with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as zf:
            zf.writestr(_CONTENT_TYPES_PATH, self._content_types_xml(
                numbering_xml is not None, bool(self._image_rels)
            ))
            zf.writestr(_RELS_PATH, self._root_rels_xml())
            zf.writestr(_DOCUMENT_PATH, document_xml)
            zf.writestr(_DOCUMENT_RELS_PATH, self._document_rels_xml(numbering_xml is not None))
            zf.writestr(_STYLES_PATH, styles_xml)
            if numbering_xml is not None:
                zf.writestr(_NUMBERING_PATH, numbering_xml)

            zf.writestr(_SETTINGS_PATH, settings_xml)
            for _rid, (zip_path, img_bytes) in self._image_rels.items():
                zf.writestr(zip_path, img_bytes)

        return buf.getvalue()

    def _content_types_xml(self, include_numbering: bool, include_images: bool) -> str:
        numbering_override = (
            f'\n  <Override PartName="/{_NUMBERING_PATH}" ContentType="{_CT_NUMBERING}"/>'
            if include_numbering
            else ""
        )
        image_defaults = ""
        if include_images:
            exts_seen: set[str] = set()
            for _rid, (zip_path, _data) in self._image_rels.items():
                ext = zip_path.rsplit(".", 1)[-1].lower() if "." in zip_path else ""
                if ext and ext not in exts_seen:
                    exts_seen.add(ext)
                    mime = _IMAGE_CONTENT_TYPES.get(ext, f"image/{ext}")
                    image_defaults += f'\n  <Default Extension="{ext}" ContentType="{mime}"/>'

        return (
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
            '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">\n'
            f'  <Default Extension="rels" ContentType="{_CT_RELS}"/>\n'
            f'  <Default Extension="xml" ContentType="{_CT_XML}"/>\n'
            f'  <Override PartName="/{_DOCUMENT_PATH}" ContentType="{_CT_DOCUMENT}"/>\n'
            f'  <Override PartName="/{_STYLES_PATH}" ContentType="{_CT_STYLES}"/>'
            f'{numbering_override}\n'
            f'  <Override PartName="/{_SETTINGS_PATH}" ContentType="{_CT_SETTINGS}"/>'
            f'{image_defaults}\n'
            '</Types>'
        )

    def _root_rels_xml(self) -> str:
        return (
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">\n'
            f'  <Relationship Id="rId1" Type="{_REL_OFFICE_DOC}" Target="word/document.xml"/>\n'
            '</Relationships>'
        )

    def _document_rels_xml(self, include_numbering: bool) -> str:
        numbering_rel = (
            f'\n  <Relationship Id="rId2" Type="{_REL_NUMBERING}" Target="numbering.xml"/>'
            if include_numbering
            else ""
        )
        hyperlink_rels = "".join(
            f'\n  <Relationship Id="{rid}" Type="{_REL_HYPERLINK}"'
            f' Target="{_esc(url)}" TargetMode="External"/>'
            for url, rid in self._hyperlink_rels.items()
        )
        image_rels = "".join(
            f'\n  <Relationship Id="{rid}" Type="{_REL_IMAGE}"'
            f' Target="{_esc(zip_path[len("word/"):])}" />'
            for rid, (zip_path, _data) in self._image_rels.items()
        )
        return (
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">\n'
            f'  <Relationship Id="rId1" Type="{_REL_STYLES}" Target="styles.xml"/>'
            f'{numbering_rel}'
            f'\n  <Relationship Id="rId3" Type="{_REL_SETTINGS}" Target="settings.xml"/>'
            f'{hyperlink_rels}'
            f'{image_rels}\n'
            '</Relationships>'
        )

    def _serialise_document(self, body_node: DocxASTBodyNode | None) -> str:
        """Produce the full document.xml string."""
        body_parts: list[str] = []
        if body_node is not None:
            for child in body_node.children:
                body_parts.append(self._serialise_block(child))

        body_content = "\n".join(body_parts)

        return (
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
            f'<w:document xmlns:w="{_W}" xmlns:r="{_R}">\n'
            '  <w:body>\n'
            f'{body_content}\n'
            '    <w:sectPr>\n'
            '      <w:pgSz w:w="12240" w:h="15840"/>\n'
            '      <w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440"'
            ' w:header="720" w:footer="720" w:gutter="0"/>\n'
            '    </w:sectPr>\n'
            '  </w:body>\n'
            '</w:document>'
        )

    def _serialise_block(self, node: DocxASTNode) -> str:
        """Dispatch a block-level AST node to its XML serialiser."""
        if isinstance(node, DocxASTParagraphNode):
            return self._serialise_paragraph(node)

        if isinstance(node, DocxASTTableNode):
            return self._serialise_table(node)

        return ""

    def _serialise_paragraph(self, node: DocxASTParagraphNode) -> str:
        """Serialise a <w:p> element."""
        ppr_xml = ""
        run_parts: list[str] = []

        for child in node.children:
            if isinstance(child, DocxASTParagraphPropertiesNode):
                ppr_xml = self._serialise_ppr(child)

            elif isinstance(child, DocxASTRunNode):
                run_parts.append(self._serialise_run(child))

            elif isinstance(child, DocxASTHyperlinkNode):
                run_parts.append(self._serialise_hyperlink(child))

        runs_xml = "".join(run_parts)
        return f"<w:p>{ppr_xml}{runs_xml}</w:p>"

    def _serialise_hyperlink(self, node: DocxASTHyperlinkNode) -> str:
        """Serialise a <w:hyperlink> element, registering the URL as a relationship."""
        rid = self._hyperlink_rels.get(node.url)
        if rid is None:
            rid = f"rId{self._next_rel_id}"
            self._next_rel_id += 1
            self._hyperlink_rels[node.url] = rid

        runs_xml = "".join(self._serialise_run(c) for c in node.children if isinstance(c, DocxASTRunNode))
        return f'<w:hyperlink r:id="{_esc(rid)}">{runs_xml}</w:hyperlink>'

    def _serialise_ppr(self, node: DocxASTParagraphPropertiesNode) -> str:
        """Serialise a <w:pPr> element."""
        parts: list[str] = []

        if node.style_id:
            parts.append(f'<w:pStyle w:val="{_esc(node.style_id)}"/>')

        if node.justification:
            parts.append(f'<w:jc w:val="{_esc(node.justification)}"/>')

        if node.outline_level is not None:
            parts.append(f'<w:outlineLvl w:val="{node.outline_level}"/>')

        if node.keep_next:
            parts.append("<w:keepNext/>")

        if node.keep_lines:
            parts.append("<w:keepLines/>")

        if node.page_break_before:
            parts.append("<w:pageBreakBefore/>")

        if node.shading:
            parts.append(
                f'<w:shd w:val="clear" w:color="auto" w:fill="{_esc(node.shading)}"/>'
            )

        if node.spacing_before is not None or node.spacing_after is not None:
            before = f' w:before="{node.spacing_before}"' if node.spacing_before is not None else ""
            after = f' w:after="{node.spacing_after}"' if node.spacing_after is not None else ""
            parts.append(f"<w:spacing{before}{after}/>")

        if any(v is not None for v in (
            node.indent_left, node.indent_right,
            node.indent_hanging, node.indent_first_line,
        )):
            left = f' w:left="{node.indent_left}"' if node.indent_left is not None else ""
            right = f' w:right="{node.indent_right}"' if node.indent_right is not None else ""
            hanging = f' w:hanging="{node.indent_hanging}"' if node.indent_hanging is not None else ""
            first = f' w:firstLine="{node.indent_first_line}"' if node.indent_first_line is not None else ""
            parts.append(f"<w:ind{left}{right}{hanging}{first}/>")

        # Numbering properties
        num_pr = next(
            (c for c in node.children if isinstance(c, DocxASTNumberingPropertiesNode)),
            None,
        )
        if num_pr is not None:
            parts.append(
                f'<w:numPr>'
                f'<w:ilvl w:val="{num_pr.ilvl}"/>'
                f'<w:numId w:val="{_esc(num_pr.num_id)}"/>'
                f'</w:numPr>'
            )

        if not parts:
            return ""

        return "<w:pPr>" + "".join(parts) + "</w:pPr>"

    def _serialise_run(self, node: DocxASTRunNode) -> str:
        """Serialise a <w:r> element."""
        rpr_xml = ""
        content_parts: list[str] = []

        for child in node.children:
            if isinstance(child, DocxASTRunPropertiesNode):
                rpr_xml = self._serialise_rpr(child)

            elif isinstance(child, DocxASTTextNode):
                content_parts.append(self._serialise_text(child))

            elif isinstance(child, DocxASTTabNode):
                content_parts.append("<w:tab/>")

            elif isinstance(child, DocxASTBreakNode):
                if child.break_type and child.break_type != "textWrapping":
                    content_parts.append(f'<w:br w:type="{_esc(child.break_type)}"/>')

                else:
                    content_parts.append("<w:br/>")

            elif isinstance(child, DocxASTLastRenderedPageBreakNode):
                content_parts.append("<w:lastRenderedPageBreak/>")

            elif isinstance(child, DocxASTDrawingNode):
                content_parts.append(self._serialise_drawing(child))

        if not content_parts and not rpr_xml:
            return ""

        content_xml = "".join(content_parts)
        return f"<w:r>{rpr_xml}{content_xml}</w:r>"

    def _serialise_rpr(self, node: DocxASTRunPropertiesNode) -> str:
        """Serialise a <w:rPr> element."""
        parts: list[str] = []

        if node.style_id:
            parts.append(f'<w:rStyle w:val="{_esc(node.style_id)}"/>')

        if node.bold:
            parts.append("<w:b/>")

        if node.italic:
            parts.append("<w:i/>")

        if node.underline:
            parts.append(f'<w:u w:val="{_esc(node.underline)}"/>')

        if node.strike:
            parts.append("<w:strike/>")

        if node.double_strike:
            parts.append("<w:dstrike/>")

        if node.vertical_align:
            parts.append(f'<w:vertAlign w:val="{_esc(node.vertical_align)}"/>')

        if node.font_ascii or node.font_h_ansi or node.font_cs:
            ascii_attr = f' w:ascii="{_esc(node.font_ascii)}"' if node.font_ascii else ""
            hansi_attr = f' w:hAnsi="{_esc(node.font_h_ansi)}"' if node.font_h_ansi else ""
            cs_attr = f' w:cs="{_esc(node.font_cs)}"' if node.font_cs else ""
            parts.append(f"<w:rFonts{ascii_attr}{hansi_attr}{cs_attr}/>")

        if node.sz is not None:
            parts.append(f'<w:sz w:val="{node.sz}"/>')

        if node.sz_cs is not None:
            parts.append(f'<w:szCs w:val="{node.sz_cs}"/>')

        if node.color:
            parts.append(f'<w:color w:val="{_esc(node.color)}"/>')

        if node.highlight:
            parts.append(f'<w:highlight w:val="{_esc(node.highlight)}"/>')

        if node.lang:
            parts.append(f'<w:lang w:val="{_esc(node.lang)}"/>')

        if not parts:
            return ""

        return "<w:rPr>" + "".join(parts) + "</w:rPr>"

    def _serialise_text(self, node: DocxASTTextNode) -> str:
        """Serialise a <w:t> element."""
        space_attr = ' xml:space="preserve"' if node.preserve_space else ""
        return f"<w:t{space_attr}>{_esc(node.content)}</w:t>"

    def _serialise_drawing(self, node: DocxASTDrawingNode) -> str:
        """
        Serialise a <w:drawing> element, embedding the image into the ZIP.

        Image bytes are sourced from, in order of preference:
        1. ``node.image_data`` — bytes already in memory (from a parsed DOCX).
        2. ``node.resolved_path`` — a filesystem path relative to the document
           source, read from disk (from a Markdown or HTML import with sidecar
           images).

        If no image bytes can be obtained, a plain-text placeholder is emitted
        so content is not silently lost.
        """
        image_bytes = self._resolve_image_bytes(node)
        if image_bytes is None:
            alt = node.description or node.resolved_path or node.relationship_id or "image"
            return f'<w:t xml:space="preserve">[Image: {_esc(alt)}]</w:t>'

        rid = self._register_image(node.resolved_path or "", image_bytes)
        dims = _read_image_dimensions(image_bytes)
        if dims is not None:
            width_px, height_px = dims
            width_emu = round(width_px * _EMU_PER_INCH / _DEFAULT_DPI)
            height_emu = round(height_px * _EMU_PER_INCH / _DEFAULT_DPI)

        else:
            width_emu = _DEFAULT_WIDTH_EMU
            height_emu = _DEFAULT_HEIGHT_EMU

        if width_emu > self._available_width_emu:
            scale = self._available_width_emu / width_emu
            width_emu = self._available_width_emu
            height_emu = round(height_emu * scale)

        alt = _esc(node.description or "")
        doc_pr_id = len(self._image_rels)

        return (
            f'<w:drawing>'
            f'<wp:inline xmlns:wp="{_WP}">'
            f'<wp:extent cx="{width_emu}" cy="{height_emu}"/>'
            f'<wp:docPr id="{doc_pr_id}" name="Image{doc_pr_id}" descr="{alt}"/>'
            f'<a:graphic xmlns:a="{_A}">'
            f'<a:graphicData uri="{_PIC}">'
            f'<pic:pic xmlns:pic="{_PIC}">'
            f'<pic:nvPicPr>'
            f'<pic:cNvPr id="{doc_pr_id}" name="Image{doc_pr_id}"/>'
            f'<pic:cNvPicPr/>'
            f'</pic:nvPicPr>'
            f'<pic:blipFill>'
            f'<a:blip r:embed="{rid}" xmlns:r="{_R}"/>'
            f'<a:stretch><a:fillRect/></a:stretch>'
            f'</pic:blipFill>'
            f'<pic:spPr>'
            f'<a:xfrm>'
            f'<a:off x="0" y="0"/>'
            f'<a:ext cx="{width_emu}" cy="{height_emu}"/>'
            f'</a:xfrm>'
            f'<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
            f'</pic:spPr>'
            f'</pic:pic>'
            f'</a:graphicData>'
            f'</a:graphic>'
            f'</wp:inline>'
            f'</w:drawing>'
        )

    def _resolve_image_bytes(self, node: DocxASTDrawingNode) -> bytes | None:
        """Return image bytes for a drawing node, or None if unavailable."""
        if node.image_data is not None:
            return node.image_data

        if node.resolved_path and self._document.source_path:
            source_dir = Path(self._document.source_path).parent
            image_path = source_dir / node.resolved_path
            if image_path.is_file():
                try:
                    return image_path.read_bytes()

                except OSError:
                    pass

        return None

    def _register_image(self, hint_path: str, image_bytes: bytes) -> str:
        """
        Register image bytes for ZIP embedding and return the relationship ID.

        If the same bytes have already been registered (e.g. the same image
        used twice), a new entry is still created so each drawing has its own
        relationship ID, which is the simplest correct approach.
        """
        rid = f"rId{self._next_rel_id}"
        self._next_rel_id += 1
        ext = hint_path.rsplit(".", 1)[-1].lower() if "." in hint_path else "bin"
        zip_path = f"word/media/image{len(self._image_rels) + 1}.{ext}"
        self._image_rels[rid] = (zip_path, image_bytes)
        return rid

    def _serialise_table(self, node: DocxASTTableNode) -> str:
        """Serialise a <w:tbl> element."""
        tbl_pr_xml = ""
        row_parts: list[str] = []

        for child in node.children:
            if isinstance(child, DocxASTTablePropertiesNode):
                tbl_pr_xml = self._serialise_tbl_pr(child)

            elif isinstance(child, DocxASTTableRowNode):
                row_parts.append(self._serialise_table_row(child))

        # Default table properties if none provided
        if not tbl_pr_xml:
            tbl_pr_xml = (
                "<w:tblPr>"
                '<w:tblW w:w="0" w:type="auto"/>'
                "<w:tblBorders>"
                '<w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
                '<w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
                '<w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
                '<w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
                '<w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
                '<w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
                "</w:tblBorders>"
                "</w:tblPr>"
            )

        rows_xml = "".join(row_parts)
        return f"<w:tbl>{tbl_pr_xml}{rows_xml}</w:tbl>"

    def _serialise_tbl_pr(self, node: DocxASTTablePropertiesNode) -> str:
        """Serialise <w:tblPr>."""
        parts: list[str] = []

        if node.style_id:
            parts.append(f'<w:tblStyle w:val="{_esc(node.style_id)}"/>')

        if node.width is not None and node.width_type:
            parts.append(f'<w:tblW w:w="{node.width}" w:type="{_esc(node.width_type)}"/>')

        else:
            parts.append('<w:tblW w:w="0" w:type="auto"/>')

        if node.indent is not None:
            parts.append(f'<w:tblInd w:w="{node.indent}" w:type="dxa"/>')

        if node.layout:
            parts.append(f'<w:tblLayout w:type="{_esc(node.layout)}"/>')

        if node.no_borders:
            border_parts = "".join(
                f'<w:{side} w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                for side in ("top", "left", "bottom", "right", "insideH", "insideV")
            )
            parts.append(f'<w:tblBorders>{border_parts}</w:tblBorders>')

        else:
            border_parts = "".join(
                f'<w:{side} w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
                for side in ("top", "left", "bottom", "right", "insideH", "insideV")
            )
            parts.append(f'<w:tblBorders>{border_parts}</w:tblBorders>')

        return "<w:tblPr>" + "".join(parts) + "</w:tblPr>"

    def _serialise_table_row(self, node: DocxASTTableRowNode) -> str:
        """Serialise a <w:tr> element."""
        trpr_xml = ""
        cell_parts: list[str] = []

        for child in node.children:
            if isinstance(child, DocxASTTableRowPropertiesNode):
                trpr_xml = self._serialise_trpr(child)

            elif isinstance(child, DocxASTTableCellNode):
                cell_parts.append(self._serialise_table_cell(child))

        cells_xml = "".join(cell_parts)
        return f"<w:tr>{trpr_xml}{cells_xml}</w:tr>"

    def _serialise_trpr(self, node: DocxASTTableRowPropertiesNode) -> str:
        """Serialise <w:trPr>."""
        parts: list[str] = []

        if node.is_header:
            parts.append("<w:tblHeader/>")

        if node.cant_split:
            parts.append("<w:cantSplit/>")

        if node.height is not None:
            rule = f' w:hRule="{_esc(node.height_rule)}"' if node.height_rule else ""
            parts.append(f'<w:trHeight w:val="{node.height}"{rule}/>')

        if not parts:
            return ""

        return "<w:trPr>" + "".join(parts) + "</w:trPr>"

    def _serialise_table_cell(self, node: DocxASTTableCellNode) -> str:
        """Serialise a <w:tc> element."""
        tcpr_xml = ""
        content_parts: list[str] = []

        # Determine available image width from the cell's explicit dxa width, if set.
        cell_width_dxa: int | None = None
        for child in node.children:
            if isinstance(child, DocxASTTableCellPropertiesNode):
                tcpr_xml = self._serialise_tcpr(child)
                if child.width is not None and child.width_type == "dxa":
                    cell_width_dxa = child.width

        saved_width = self._available_width_emu
        if cell_width_dxa is not None:
            # Subtract cell margins (120 twips each side) before converting.
            inner_dxa = max(0, cell_width_dxa - 2 * 120)
            self._available_width_emu = inner_dxa * _EMU_PER_TWIP

        for child in node.children:
            if isinstance(child, DocxASTTableCellPropertiesNode):
                pass  # already handled above

            elif isinstance(child, DocxASTParagraphNode):
                content_parts.append(self._serialise_paragraph(child))

            elif isinstance(child, DocxASTTableNode):
                content_parts.append(self._serialise_table(child))

        self._available_width_emu = saved_width

        # OOXML requires at least one paragraph in every cell
        if not any("<w:p" in p for p in content_parts):
            content_parts.append("<w:p><w:pPr><w:spacing w:before=\"0\" w:after=\"0\"/></w:pPr></w:p>")

        # OOXML also requires the last child of a cell to be a paragraph
        elif not content_parts[-1].startswith("<w:p"):
            content_parts.append("<w:p><w:pPr><w:spacing w:before=\"0\" w:after=\"0\"/></w:pPr></w:p>")

        # Default tcPr if none provided
        if not tcpr_xml:
            tcpr_xml = '<w:tcPr><w:tcW w:w="0" w:type="auto"/></w:tcPr>'

        content_xml = "".join(content_parts)
        return f"<w:tc>{tcpr_xml}{content_xml}</w:tc>"

    def _serialise_tcpr(self, node: DocxASTTableCellPropertiesNode) -> str:
        """Serialise <w:tcPr>."""
        parts: list[str] = []

        if node.width is not None and node.width_type:
            parts.append(f'<w:tcW w:w="{node.width}" w:type="{_esc(node.width_type)}"/>')

        else:
            parts.append('<w:tcW w:w="0" w:type="auto"/>')

        if node.grid_span > 1:
            parts.append(f'<w:gridSpan w:val="{node.grid_span}"/>')

        if node.vertical_merge:
            val_attr = f' w:val="{_esc(node.vertical_merge)}"' if node.vertical_merge != "continue" else ""
            parts.append(f"<w:vMerge{val_attr}/>")

        if node.vertical_alignment:
            parts.append(f'<w:vAlign w:val="{_esc(node.vertical_alignment)}"/>')

        if node.shading_fill:
            parts.append(
                f'<w:shd w:val="clear" w:color="auto" w:fill="{_esc(node.shading_fill)}"/>'
            )

        if node.left_bar:
            parts.append(
                '<w:tcBorders>'
                '<w:left w:val="single" w:sz="24" w:space="0" w:color="4A90D9"/>'
                '</w:tcBorders>'
            )

        # Uniform cell padding of 120 twips (~half a line height) on all sides
        parts.append(
            '<w:tcMar>'
            '<w:top w:w="120" w:type="dxa"/>'
            '<w:left w:w="120" w:type="dxa"/>'
            '<w:bottom w:w="120" w:type="dxa"/>'
            '<w:right w:w="120" w:type="dxa"/>'
            '</w:tcMar>'
        )

        return "<w:tcPr>" + "".join(parts) + "</w:tcPr>"

    def _serialise_settings(self) -> str:
        """Produce the settings.xml string declaring Word 2013+ compatibility."""
        return (
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
            f'<w:settings xmlns:w="{_W}">\n'
            '  <w:compat>\n'
            '    <w:compatSetting'
            ' w:name="compatibilityMode"'
            ' w:uri="http://schemas.microsoft.com/office/word"'
            f' w:val="{_COMPAT_MODE_VAL}"/>\n'
            '  </w:compat>\n'
            '</w:settings>'
        )

    def _serialise_styles(self, node: DocxASTStylesNode | None) -> str:
        """Produce the styles.xml string."""
        style_parts: list[str] = []

        if node is not None:
            # Default run properties from docDefaults
            if node.default_run_properties is not None:
                rpr_xml = self._serialise_rpr(node.default_run_properties)
                if rpr_xml:
                    style_parts.append(
                        f"<w:docDefaults>"
                        f"<w:rPrDefault><w:rPr>{rpr_xml[len('<w:rPr>'):-len('</w:rPr>')]}</w:rPr></w:rPrDefault>"
                        f"</w:docDefaults>"
                    )

            for child in node.children:
                if isinstance(child, DocxASTStyleNode):
                    style_parts.append(self._serialise_style(child))

            # Always include the Hyperlink character style
            style_parts.append(_HYPERLINK_STYLE_XML)

        else:
            # Emit a minimal Normal style so the document is valid
            style_parts.append(
                '<w:style w:type="paragraph" w:default="1" w:styleId="Normal">'
                '<w:name w:val="Normal"/>'
                "</w:style>"
            )

        styles_xml = "\n".join(style_parts)
        return (
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
            f'<w:styles xmlns:w="{_W}">\n'
            f'{styles_xml}\n'
            '</w:styles>'
        )

    def _serialise_style(self, node: DocxASTStyleNode) -> str:
        """Serialise a single <w:style> element with visual defaults applied."""
        attrs = f'w:type="{_esc(node.style_type)}" w:styleId="{_esc(node.style_id)}"'
        if node.is_default:
            attrs += ' w:default="1"'

        if node.is_custom:
            attrs += ' w:customStyle="1"'

        parts: list[str] = [f'<w:name w:val="{_esc(node.name)}"/>']

        if node.based_on:
            parts.append(f'<w:basedOn w:val="{_esc(node.based_on)}"/>')

        if node.next_style:
            parts.append(f'<w:next w:val="{_esc(node.next_style)}"/>')

        # Get visual overrides for this style
        visuals = _STYLE_VISUALS.get(node.style_id, {})
        ppr_extra = visuals.get("ppr_extra", "")
        rpr_extra = visuals.get("rpr_extra", "")

        # Paragraph properties
        ppr_node = next(
            (c for c in node.children if isinstance(c, DocxASTParagraphPropertiesNode)),
            None,
        )
        ppr_parts: list[str] = []
        if ppr_node:
            ppr_inner = self._serialise_ppr(ppr_node)
            # Strip outer <w:pPr>...</w:pPr> tags to get the inner content
            if ppr_inner.startswith("<w:pPr>") and ppr_inner.endswith("</w:pPr>"):
                ppr_parts.append(ppr_inner[len("<w:pPr>"):-len("</w:pPr>")])

        if ppr_extra:
            ppr_parts.append(ppr_extra)

        if ppr_parts:
            parts.append("<w:pPr>" + "".join(ppr_parts) + "</w:pPr>")

        # Run properties
        rpr_node = next(
            (c for c in node.children if isinstance(c, DocxASTRunPropertiesNode)),
            None,
        )
        rpr_parts: list[str] = []
        if rpr_node:
            rpr_inner = self._serialise_rpr(rpr_node)
            if rpr_inner.startswith("<w:rPr>") and rpr_inner.endswith("</w:rPr>"):
                rpr_parts.append(rpr_inner[len("<w:rPr>"):-len("</w:rPr>")])

        if rpr_extra:
            rpr_parts.append(rpr_extra)

        if rpr_parts:
            parts.append("<w:rPr>" + "".join(rpr_parts) + "</w:rPr>")

        inner_xml = "".join(parts)
        return f"<w:style {attrs}>{inner_xml}</w:style>"

    def _serialise_numbering(self, node: DocxASTNumberingNode) -> str:
        """Produce the numbering.xml string."""
        parts: list[str] = []

        for child in node.children:
            if isinstance(child, DocxASTAbstractNumNode):
                parts.append(self._serialise_abstract_num(child))

            elif isinstance(child, DocxASTNumNode):
                override_xml = ""
                if child.start_override is not None:
                    override_xml = (
                        f'<w:lvlOverride w:ilvl="0">'
                        f'<w:startOverride w:val="{child.start_override}"/>'
                        f'</w:lvlOverride>'
                    )

                parts.append(
                    f'<w:num w:numId="{_esc(child.num_id)}">'
                    f'<w:abstractNumId w:val="{_esc(child.abstract_num_id)}"/>'
                    f'{override_xml}'
                    f'</w:num>'
                )

        inner_xml = "\n".join(parts)
        return (
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
            f'<w:numbering xmlns:w="{_W}">\n'
            f'{inner_xml}\n'
            '</w:numbering>'
        )

    def _serialise_abstract_num(self, node: DocxASTAbstractNumNode) -> str:
        """Serialise a <w:abstractNum> element."""
        level_parts: list[str] = []

        for child in node.children:
            if isinstance(child, DocxASTNumLevelNode):
                level_parts.append(self._serialise_num_level(child))

        multi_level = ""
        if node.multi_level_type:
            multi_level = f'<w:multiLevelType w:val="{_esc(node.multi_level_type)}"/>\n'

        levels_xml = "\n".join(level_parts)
        return (
            f'<w:abstractNum w:abstractNumId="{_esc(node.abstract_num_id)}">\n'
            f'{multi_level}'
            f'{levels_xml}\n'
            f'</w:abstractNum>'
        )

    def _serialise_num_level(self, node: DocxASTNumLevelNode) -> str:
        """Serialise a <w:lvl> element."""
        parts: list[str] = [
            f'<w:start w:val="{node.start}"/>',
            f'<w:numFmt w:val="{_esc(node.num_fmt)}"/>',
            f'<w:lvlText w:val="{_esc(node.lvl_text)}"/>',
            f'<w:lvlJc w:val="{_esc(node.lvl_jc)}"/>',
        ]

        # Indent properties
        if node.indent_left is not None or node.indent_hanging is not None:
            left = f' w:left="{node.indent_left}"' if node.indent_left is not None else ""
            hanging = f' w:hanging="{node.indent_hanging}"' if node.indent_hanging is not None else ""
            parts.append(f"<w:pPr><w:ind{left}{hanging}/></w:pPr>")

        # Font for bullet character
        if node.font_ascii or node.font_h_ansi:
            ascii_attr = f' w:ascii="{_esc(node.font_ascii)}"' if node.font_ascii else ""
            hansi_attr = f' w:hAnsi="{_esc(node.font_h_ansi)}"' if node.font_h_ansi else ""
            parts.append(f"<w:rPr><w:rFonts{ascii_attr}{hansi_attr}/></w:rPr>")

        inner_xml = "".join(parts)
        return f'<w:lvl w:ilvl="{node.ilvl}">{inner_xml}</w:lvl>'
