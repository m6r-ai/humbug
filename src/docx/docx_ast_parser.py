import io
import zipfile
import xml.etree.ElementTree as ET
from typing import Dict, List

from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
    DocxASTBodyNode,
    DocxASTBookmarkEndNode,
    DocxASTBookmarkStartNode,
    DocxASTBreakNode,
    DocxASTDocumentNode,
    DocxASTDrawingNode,
    DocxASTHyperlinkNode,
    DocxASTLastRenderedPageBreakNode,
    DocxASTNumLevelNode,
    DocxASTNumNode,
    DocxASTNumberingNode,
    DocxASTNumberingPropertiesNode,
    DocxASTParagraphNode,
    DocxASTParagraphPropertiesNode,
    DocxASTRunNode,
    DocxASTRunPropertiesNode,
    DocxASTSectionPropertiesNode,
    DocxASTStyleNode,
    DocxASTStylesNode,
    DocxASTTabNode,
    DocxASTTableCellNode,
    DocxASTTableCellPropertiesNode,
    DocxASTTableGridNode,
    DocxASTTableNode,
    DocxASTTablePropertiesNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTextNode,
)
from docx.docx_errors import DocxExtractionError, DocxParseError, DocxUnsupportedError

# XML namespace constants
_W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
_R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
_R_PKG = "http://schemas.openxmlformats.org/package/2006/relationships"
_WP = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
_A = "http://schemas.openxmlformats.org/drawingml/2006/main"
_PIC = "http://schemas.openxmlformats.org/drawingml/2006/picture"

# Relationship type suffixes used to identify image and hyperlink relationships
_REL_IMAGE = "relationships/image"
_REL_HYPERLINK = "relationships/hyperlink"

# ZIP paths
_DOCUMENT_PATH = "word/document.xml"
_DOCUMENT_RELS_PATH = "word/_rels/document.xml.rels"
_STYLES_PATH = "word/styles.xml"
_NUMBERING_PATH = "word/numbering.xml"
_ENCRYPTED_ENTRY = "EncryptedPackage"


def _tag(ns: str, local: str) -> str:
    """Return a Clark-notation tag string."""
    return f"{{{ns}}}{local}"


def _w(local: str) -> str:
    """Return a Clark-notation tag in the w: namespace."""
    return f"{{{_W}}}{local}"


def _attr(element: ET.Element, ns: str, local: str) -> str | None:
    """Return a namespaced attribute value or None."""
    return element.get(f"{{{ns}}}{local}")


def _wattr(element: ET.Element, local: str) -> str | None:
    """Return a w: namespaced attribute value or None."""
    return element.get(f"{{{_W}}}{local}")


def _int_or_none(value: str | None) -> int | None:
    """Convert a string to int, returning None if conversion fails or value is None."""
    if value is None:
        return None

    try:
        return int(value)

    except (ValueError, TypeError):
        return None


class DocxASTParser:
    """
    Parses a DOCX file from raw bytes into a DocxASTDocumentNode.

    Usage::

        parser = DocxASTParser()
        document = parser.parse(data, source_path="/path/to/file.docx")

    The parser faithfully captures the OOXML structure without semantic
    interpretation.  Interpretation (e.g. resolving style inheritance,
    inferring heading levels from direct formatting) is the responsibility
    of the mapper that converts the AST to doc_ir.
    """

    def __init__(self) -> None:
        """Initialise the parser."""
        self._zf: zipfile.ZipFile | None = None

    def parse(
        self,
        data: bytes,
        source_path: str | None = None,
    ) -> DocxASTDocumentNode:
        """
        Parse DOCX bytes into a document AST.

        Args:
            data: Raw bytes of the .docx file.
            source_path: Optional path to the source file for error messages
                and image resolution.

        Returns:
            A fully populated DocxASTDocumentNode.

        Raises:
            DocxUnsupportedError: If the file is encrypted.
            DocxParseError: If the ZIP or XML is malformed.
            DocxExtractionError: If required XML structure is missing.
        """
        try:
            zf = zipfile.ZipFile(io.BytesIO(data))
        except zipfile.BadZipFile as e:
            raise DocxParseError(f"Not a valid DOCX file (ZIP open failed): {e}") from e

        with zf:
            self._zf = zf
            names = zf.namelist()

            if _ENCRYPTED_ENTRY in names:
                raise DocxUnsupportedError("File is encrypted and cannot be read")

            if _DOCUMENT_PATH not in names:
                raise DocxParseError(
                    f"Not a valid DOCX file (missing {_DOCUMENT_PATH!r})"
                )

            document = DocxASTDocumentNode(source_path=source_path)

            # Relationships
            if _DOCUMENT_RELS_PATH in names:
                rels_xml = self._read_xml(zf, _DOCUMENT_RELS_PATH)
                document.relationships = self._parse_relationships(rels_xml)

            # Styles
            if _STYLES_PATH in names:
                styles_xml = self._read_xml(zf, _STYLES_PATH)
                styles_node = self._parse_styles(styles_xml)
                document.add_child(styles_node)

            # Numbering
            if _NUMBERING_PATH in names:
                numbering_xml = self._read_xml(zf, _NUMBERING_PATH)
                numbering_node = self._parse_numbering(numbering_xml)
                document.add_child(numbering_node)

            # Document body
            doc_xml = self._read_xml(zf, _DOCUMENT_PATH)
            body_node = self._parse_document_body(doc_xml, document.relationships)
            document.add_child(body_node)

            self._zf = None

        return document

    def _read_xml(self, zf: zipfile.ZipFile, path: str) -> ET.Element:
        """
        Read and parse an XML entry from the ZIP.

        Args:
            zf: Open ZipFile.
            path: Entry path within the ZIP.

        Returns:
            Parsed root Element.

        Raises:
            DocxParseError: If the entry cannot be read or parsed.
        """
        try:
            xml_bytes = zf.read(path)
        except KeyError as e:
            raise DocxParseError(f"Could not read {path!r} from ZIP: {e}") from e

        try:
            return ET.fromstring(xml_bytes)
        except ET.ParseError as e:
            raise DocxParseError(f"Malformed XML in {path!r}: {e}") from e

    def _parse_relationships(self, root: ET.Element) -> Dict[str, str]:
        """
        Parse a .rels file and return a map from Id to Target.

        Image relationships are stored as ZIP paths (e.g. 'word/media/image1.png').
        Hyperlink relationships are stored as URLs verbatim.  Other relationship
        types are silently skipped.

        Args:
            root: Root element of the .rels XML.

        Returns:
            Dict mapping relationship Id (e.g. 'rId6') to target.  Image
            targets are normalised to paths within the ZIP; hyperlink targets
            are the raw URL.
        """
        relationships: Dict[str, str] = {}

        for rel in root:
            rel_type = rel.get("Type", "")
            is_image = _REL_IMAGE in rel_type
            is_hyperlink = _REL_HYPERLINK in rel_type
            if not is_image and not is_hyperlink:
                continue

            rel_id = rel.get("Id")
            target = rel.get("Target")
            if rel_id and target:
                if is_image:
                    # Targets are relative to word/, e.g. "media/image1.png"
                    # Normalise to a path within the ZIP: "word/media/image1.png"
                    if not target.startswith("/"):
                        target = f"word/{target}"

                relationships[rel_id] = target

        return relationships

    def _parse_styles(self, root: ET.Element) -> DocxASTStylesNode:
        """
        Parse styles.xml into a DocxASTStylesNode.

        Args:
            root: Root <w:styles> element.

        Returns:
            Populated DocxASTStylesNode.
        """
        styles_node = DocxASTStylesNode()

        # Default run/paragraph properties from <w:docDefaults>
        doc_defaults = root.find(_w("docDefaults"))
        if doc_defaults is not None:
            rpr_default = doc_defaults.find(f".//{_w('rPr')}")
            if rpr_default is not None:
                styles_node.default_run_properties = self._parse_run_properties(
                    rpr_default
                )

            ppr_default = doc_defaults.find(f".//{_w('pPr')}")
            if ppr_default is not None:
                styles_node.default_paragraph_properties = (
                    self._parse_paragraph_properties(ppr_default)
                )

        # Individual style definitions
        for style_elem in root.findall(_w("style")):
            style_node = self._parse_style(style_elem)
            styles_node.add_child(style_node)

        return styles_node

    def _parse_style(self, elem: ET.Element) -> DocxASTStyleNode:
        """
        Parse a single <w:style> element.

        Args:
            elem: The <w:style> element.

        Returns:
            A DocxASTStyleNode.
        """
        style_type = _wattr(elem, "type") or "paragraph"
        style_id = _wattr(elem, "styleId") or ""
        is_default = _wattr(elem, "default") == "1"
        is_custom = _wattr(elem, "customStyle") == "1"

        name_elem = elem.find(_w("name"))
        name = _wattr(name_elem, "val") if name_elem is not None else style_id

        based_on_elem = elem.find(_w("basedOn"))
        based_on = (
            _wattr(based_on_elem, "val") if based_on_elem is not None else None
        )

        next_elem = elem.find(_w("next"))
        next_style = _wattr(next_elem, "val") if next_elem is not None else None

        style_node = DocxASTStyleNode(
            style_type=style_type,
            style_id=style_id,
            name=name or style_id,
            based_on=based_on,
            next_style=next_style,
            is_default=is_default,
            is_custom=is_custom,
        )

        ppr_elem = elem.find(_w("pPr"))
        if ppr_elem is not None:
            style_node.add_child(self._parse_paragraph_properties(ppr_elem))

        rpr_elem = elem.find(_w("rPr"))
        if rpr_elem is not None:
            style_node.add_child(self._parse_run_properties(rpr_elem))

        return style_node

    def _parse_numbering(self, root: ET.Element) -> DocxASTNumberingNode:
        """
        Parse numbering.xml into a DocxASTNumberingNode.

        Args:
            root: Root <w:numbering> element.

        Returns:
            Populated DocxASTNumberingNode.
        """
        numbering_node = DocxASTNumberingNode()

        for abstract_elem in root.findall(_w("abstractNum")):
            abstract_node = self._parse_abstract_num(abstract_elem)
            numbering_node.add_child(abstract_node)

        for num_elem in root.findall(_w("num")):
            num_node = self._parse_num(num_elem)
            numbering_node.add_child(num_node)

        return numbering_node

    def _parse_abstract_num(self, elem: ET.Element) -> DocxASTAbstractNumNode:
        """
        Parse a <w:abstractNum> element.

        Args:
            elem: The <w:abstractNum> element.

        Returns:
            A DocxASTAbstractNumNode with level children.
        """
        abstract_num_id = _wattr(elem, "abstractNumId") or "0"
        node = DocxASTAbstractNumNode(abstract_num_id=abstract_num_id)

        multi_level_elem = elem.find(_w("multiLevelType"))
        if multi_level_elem is not None:
            node.multi_level_type = _wattr(multi_level_elem, "val")

        for lvl_elem in elem.findall(_w("lvl")):
            lvl_node = self._parse_num_level(lvl_elem)
            node.add_child(lvl_node)

        return node

    def _parse_num_level(self, elem: ET.Element) -> DocxASTNumLevelNode:
        """
        Parse a <w:lvl> element.

        Args:
            elem: The <w:lvl> element.

        Returns:
            A DocxASTNumLevelNode.
        """
        ilvl = _int_or_none(_wattr(elem, "ilvl")) or 0

        start_elem = elem.find(_w("start"))
        start = _int_or_none(_wattr(start_elem, "val")) if start_elem is not None else 1
        if start is None:
            start = 1

        num_fmt_elem = elem.find(_w("numFmt"))
        num_fmt = (
            _wattr(num_fmt_elem, "val") if num_fmt_elem is not None else "bullet"
        ) or "bullet"

        lvl_text_elem = elem.find(_w("lvlText"))
        lvl_text = (
            _wattr(lvl_text_elem, "val") if lvl_text_elem is not None else ""
        ) or ""

        lvl_jc_elem = elem.find(_w("lvlJc"))
        lvl_jc = (
            _wattr(lvl_jc_elem, "val") if lvl_jc_elem is not None else "left"
        ) or "left"

        # Indent properties from <w:pPr><w:ind>
        indent_left: int | None = None
        indent_hanging: int | None = None
        ppr_elem = elem.find(_w("pPr"))
        if ppr_elem is not None:
            ind_elem = ppr_elem.find(_w("ind"))
            if ind_elem is not None:
                indent_left = _int_or_none(_wattr(ind_elem, "left"))
                indent_hanging = _int_or_none(_wattr(ind_elem, "hanging"))

        # Font for bullet character from <w:rPr><w:rFonts>
        font_ascii: str | None = None
        font_h_ansi: str | None = None
        rpr_elem = elem.find(_w("rPr"))
        if rpr_elem is not None:
            fonts_elem = rpr_elem.find(_w("rFonts"))
            if fonts_elem is not None:
                font_ascii = _wattr(fonts_elem, "ascii")
                font_h_ansi = _wattr(fonts_elem, "hAnsi")

        return DocxASTNumLevelNode(
            ilvl=ilvl,
            start=start,
            num_fmt=num_fmt,
            lvl_text=lvl_text,
            lvl_jc=lvl_jc,
            indent_left=indent_left,
            indent_hanging=indent_hanging,
            font_ascii=font_ascii,
            font_h_ansi=font_h_ansi,
        )

    def _parse_num(self, elem: ET.Element) -> DocxASTNumNode:
        """
        Parse a <w:num> element.

        Args:
            elem: The <w:num> element.

        Returns:
            A DocxASTNumNode.
        """
        num_id = _wattr(elem, "numId") or "0"

        abstract_num_id_elem = elem.find(_w("abstractNumId"))
        abstract_num_id = (
            _wattr(abstract_num_id_elem, "val")
            if abstract_num_id_elem is not None
            else "0"
        ) or "0"

        return DocxASTNumNode(num_id=num_id, abstract_num_id=abstract_num_id)

    def _parse_document_body(
        self,
        root: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTBodyNode:
        """
        Parse the document.xml root into a DocxASTBodyNode.

        Args:
            root: Root <w:document> element.
            relationships: Resolved relationship map for image lookup.

        Returns:
            A DocxASTBodyNode containing the document content.

        Raises:
            DocxExtractionError: If the <w:body> element is missing.
        """
        body_elem = root.find(_w("body"))
        if body_elem is None:
            raise DocxExtractionError("document.xml has no w:body element")

        body_node = DocxASTBodyNode()

        for child in body_elem:
            local = child.tag.split("}")[-1]

            if local == "p":
                body_node.add_child(self._parse_paragraph(child, relationships))

            elif local == "tbl":
                body_node.add_child(self._parse_table(child, relationships))

            elif local == "sectPr":
                body_node.add_child(self._parse_section_properties(child))

            # Other block-level elements (sdt, etc.) are silently skipped

        return body_node

    def _parse_paragraph(
        self,
        elem: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTParagraphNode:
        """
        Parse a <w:p> element.

        Args:
            elem: The <w:p> element.
            relationships: Relationship map for image resolution.

        Returns:
            A DocxASTParagraphNode.
        """
        para_node = DocxASTParagraphNode()

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "pPr":
                para_node.add_child(self._parse_paragraph_properties(child))

            elif local == "r":
                para_node.add_child(self._parse_run(child, relationships))

            elif local == "bookmarkStart":
                para_node.add_child(self._parse_bookmark_start(child))

            elif local == "bookmarkEnd":
                para_node.add_child(self._parse_bookmark_end(child))

            elif local == "hyperlink":
                para_node.add_child(self._parse_hyperlink(child, relationships))

            # sdts and other elements are skipped at this stage

        return para_node

    def _parse_hyperlink(
        self,
        elem: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTHyperlinkNode:
        """
        Parse a <w:hyperlink> element.

        Handles both external hyperlinks (r:id referencing a URL via
        relationships) and internal hyperlinks (w:anchor referencing a
        bookmark).  Runs inside the hyperlink are parsed as children.

        Args:
            elem: The <w:hyperlink> element.
            relationships: Relationship map for URL resolution.

        Returns:
            A DocxASTHyperlinkNode with run children.
        """
        anchor = _wattr(elem, "anchor") or ""
        rel_id = elem.get(f"{{{_R}}}id")
        url = ""
        if rel_id and rel_id in relationships:
            url = relationships[rel_id]

        # Word sometimes wraps heading text in an external hyperlink that also
        # contains bookmark elements.  These are cross-reference structures
        # (the bookmark is the TOC anchor) and the external URL is an autoformat
        # artefact (e.g. F.PP.PT -> http://f.pp.pt/).  Discard the URL so the
        # text is emitted without a link wrapper.
        has_bookmark = elem.find(_w("bookmarkStart")) is not None
        if has_bookmark:
            url = ""

        node = DocxASTHyperlinkNode(url=url, anchor=anchor)

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "r":
                node.add_child(self._parse_run(child, relationships))

            # bookmarkStart, bookmarkEnd, proofErr, etc. are skipped

        return node

    def _parse_paragraph_properties(
        self, elem: ET.Element
    ) -> DocxASTParagraphPropertiesNode:
        """
        Parse a <w:pPr> element.

        Args:
            elem: The <w:pPr> element.

        Returns:
            A DocxASTParagraphPropertiesNode.
        """
        style_id: str | None = None
        justification: str | None = None
        outline_level: int | None = None
        spacing_before: int | None = None
        spacing_after: int | None = None
        indent_left: int | None = None
        indent_right: int | None = None
        indent_hanging: int | None = None
        indent_first_line: int | None = None
        keep_next = False
        keep_lines = False
        page_break_before = False

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "pStyle":
                style_id = _wattr(child, "val")

            elif local == "jc":
                justification = _wattr(child, "val")

            elif local == "outlineLvl":
                outline_level = _int_or_none(_wattr(child, "val"))

            elif local == "spacing":
                spacing_before = _int_or_none(_wattr(child, "before"))
                spacing_after = _int_or_none(_wattr(child, "after"))

            elif local == "ind":
                indent_left = _int_or_none(_wattr(child, "left"))
                indent_right = _int_or_none(_wattr(child, "right"))
                indent_hanging = _int_or_none(_wattr(child, "hanging"))
                indent_first_line = _int_or_none(_wattr(child, "firstLine"))

            elif local == "keepNext":
                keep_next = _wattr(child, "val") != "0"

            elif local == "keepLines":
                keep_lines = _wattr(child, "val") != "0"

            elif local == "pageBreakBefore":
                page_break_before = _wattr(child, "val") != "0"

        ppr_node = DocxASTParagraphPropertiesNode(
            style_id=style_id,
            justification=justification,
            outline_level=outline_level,
            spacing_before=spacing_before,
            spacing_after=spacing_after,
            indent_left=indent_left,
            indent_right=indent_right,
            indent_hanging=indent_hanging,
            indent_first_line=indent_first_line,
            keep_next=keep_next,
            keep_lines=keep_lines,
            page_break_before=page_break_before,
        )

        # Numbering properties
        num_pr_elem = elem.find(_w("numPr"))
        if num_pr_elem is not None:
            num_id_elem = num_pr_elem.find(_w("numId"))
            ilvl_elem = num_pr_elem.find(_w("ilvl"))
            num_id = _wattr(num_id_elem, "val") if num_id_elem is not None else "0"
            ilvl = _int_or_none(_wattr(ilvl_elem, "val")) if ilvl_elem is not None else 0
            ppr_node.add_child(
                DocxASTNumberingPropertiesNode(
                    num_id=num_id or "0",
                    ilvl=ilvl or 0,
                )
            )

        return ppr_node

    def _parse_run(
        self,
        elem: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTRunNode:
        """
        Parse a <w:r> element.

        Args:
            elem: The <w:r> element.
            relationships: Relationship map for image resolution.

        Returns:
            A DocxASTRunNode.
        """
        run_node = DocxASTRunNode()

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "rPr":
                run_node.add_child(self._parse_run_properties(child))

            elif local == "t":
                text = child.text or ""
                preserve = child.get("{http://www.w3.org/XML/1998/namespace}space") == "preserve"
                run_node.add_child(DocxASTTextNode(content=text, preserve_space=preserve))

            elif local == "tab":
                run_node.add_child(DocxASTTabNode())

            elif local == "br":
                break_type = _wattr(child, "type") or "textWrapping"
                run_node.add_child(DocxASTBreakNode(break_type=break_type))

            elif local == "lastRenderedPageBreak":
                run_node.add_child(DocxASTLastRenderedPageBreakNode())

            elif local == "drawing":
                drawing_node = self._parse_drawing(child, relationships)
                if drawing_node is not None:
                    run_node.add_child(drawing_node)

            # delText, instrText, fldChar, etc. are silently skipped

        return run_node

    def _parse_run_properties(self, elem: ET.Element) -> DocxASTRunPropertiesNode:
        """
        Parse a <w:rPr> element.

        Args:
            elem: The <w:rPr> element.

        Returns:
            A DocxASTRunPropertiesNode.
        """
        bold = False
        italic = False
        underline: str | None = None
        strike = False
        double_strike = False
        vertical_align: str | None = None
        style_id: str | None = None
        font_ascii: str | None = None
        font_h_ansi: str | None = None
        font_cs: str | None = None
        sz: int | None = None
        sz_cs: int | None = None
        color: str | None = None
        highlight: str | None = None
        lang: str | None = None

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "b":
                bold = _wattr(child, "val") != "0"

            elif local == "bCs":
                # bCs applies bold to complex scripts; we treat it as bold
                if _wattr(child, "val") != "0":
                    bold = True

            elif local == "i":
                italic = _wattr(child, "val") != "0"

            elif local == "iCs":
                if _wattr(child, "val") != "0":
                    italic = True

            elif local == "u":
                underline = _wattr(child, "val")

            elif local == "strike":
                strike = _wattr(child, "val") != "0"

            elif local == "dstrike":
                double_strike = _wattr(child, "val") != "0"

            elif local == "vertAlign":
                vertical_align = _wattr(child, "val")

            elif local == "rStyle":
                style_id = _wattr(child, "val")

            elif local == "rFonts":
                font_ascii = _wattr(child, "ascii")
                font_h_ansi = _wattr(child, "hAnsi")
                font_cs = _wattr(child, "cs")

            elif local == "sz":
                sz = _int_or_none(_wattr(child, "val"))

            elif local == "szCs":
                sz_cs = _int_or_none(_wattr(child, "val"))

            elif local == "color":
                color = _wattr(child, "val")

            elif local == "highlight":
                highlight = _wattr(child, "val")

            elif local == "lang":
                lang = _wattr(child, "val")

        return DocxASTRunPropertiesNode(
            bold=bold,
            italic=italic,
            underline=underline,
            strike=strike,
            double_strike=double_strike,
            vertical_align=vertical_align,
            style_id=style_id,
            font_ascii=font_ascii,
            font_h_ansi=font_h_ansi,
            font_cs=font_cs,
            sz=sz,
            sz_cs=sz_cs,
            color=color,
            highlight=highlight,
            lang=lang,
        )

    def _parse_bookmark_start(self, elem: ET.Element) -> DocxASTBookmarkStartNode:
        """Parse a <w:bookmarkStart> element."""
        bookmark_id = _wattr(elem, "id") or ""
        name = _wattr(elem, "name") or ""
        return DocxASTBookmarkStartNode(bookmark_id=bookmark_id, name=name)

    def _parse_bookmark_end(self, elem: ET.Element) -> DocxASTBookmarkEndNode:
        """Parse a <w:bookmarkEnd> element."""
        bookmark_id = _wattr(elem, "id") or ""
        return DocxASTBookmarkEndNode(bookmark_id=bookmark_id)

    def _parse_drawing(
        self,
        elem: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTDrawingNode | None:
        """
        Parse a <w:drawing> element and return a DocxASTDrawingNode.

        Extracts the relationship ID (for image lookup), dimensions, and
        description from the drawing's inline or anchor child.

        Args:
            elem: The <w:drawing> element.
            relationships: Relationship map for resolving image paths.

        Returns:
            A DocxASTDrawingNode, or None if no image relationship is found.
        """
        relationship_id: str | None = None
        description: str | None = None
        width_emu: int | None = None
        height_emu: int | None = None

        # Look in both <wp:inline> and <wp:anchor>
        for container_tag in (
            _tag(_WP, "inline"),
            _tag(_WP, "anchor"),
        ):
            container = elem.find(container_tag)
            if container is None:
                continue

            # Extent (dimensions)
            extent = container.find(_tag(_WP, "extent"))
            if extent is not None:
                width_emu = _int_or_none(extent.get("cx"))
                height_emu = _int_or_none(extent.get("cy"))

            # Description from docPr
            doc_pr = container.find(_tag(_WP, "docPr"))
            if doc_pr is not None:
                description = doc_pr.get("descr") or doc_pr.get("title")

            # Navigate to the blipFill to get the relationship ID
            # Path: graphic > graphicData > pic:pic > pic:blipFill > a:blip r:embed
            graphic = container.find(_tag(_A, "graphic"))
            if graphic is None:
                continue

            graphic_data = graphic.find(_tag(_A, "graphicData"))
            if graphic_data is None:
                continue

            pic_elem = graphic_data.find(_tag(_PIC, "pic"))
            if pic_elem is None:
                continue

            blip_fill = pic_elem.find(_tag(_PIC, "blipFill"))
            if blip_fill is None:
                continue

            blip = blip_fill.find(_tag(_A, "blip"))
            if blip is not None:
                relationship_id = blip.get(f"{{{_R}}}embed")

            break

        if relationship_id is None:
            return None

        resolved_path = relationships.get(relationship_id)

        image_data: bytes | None = None
        if resolved_path is not None and self._zf is not None:
            try:
                image_data = self._zf.read(resolved_path)

            except KeyError:
                pass

        return DocxASTDrawingNode(
            relationship_id=relationship_id,
            resolved_path=resolved_path,
            description=description,
            width_emu=width_emu,
            height_emu=height_emu,
            image_data=image_data,
        )

    def _parse_table(
        self,
        elem: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTTableNode:
        """
        Parse a <w:tbl> element.

        Args:
            elem: The <w:tbl> element.
            relationships: Relationship map for image resolution.

        Returns:
            A DocxASTTableNode.
        """
        table_node = DocxASTTableNode()

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "tblPr":
                table_node.add_child(self._parse_table_properties(child))

            elif local == "tblGrid":
                table_node.add_child(self._parse_table_grid(child))

            elif local == "tr":
                table_node.add_child(self._parse_table_row(child, relationships))

        return table_node

    def _parse_table_properties(self, elem: ET.Element) -> DocxASTTablePropertiesNode:
        """Parse a <w:tblPr> element."""
        style_id: str | None = None
        width: int | None = None
        width_type: str | None = None
        indent: int | None = None
        layout: str | None = None

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "tblStyle":
                style_id = _wattr(child, "val")

            elif local == "tblW":
                width = _int_or_none(_wattr(child, "w"))
                width_type = _wattr(child, "type")

            elif local == "tblInd":
                indent = _int_or_none(_wattr(child, "w"))

            elif local == "tblLayout":
                layout = _wattr(child, "type")

        return DocxASTTablePropertiesNode(
            style_id=style_id,
            width=width,
            width_type=width_type,
            indent=indent,
            layout=layout,
        )

    def _parse_table_grid(self, elem: ET.Element) -> DocxASTTableGridNode:
        """Parse a <w:tblGrid> element."""
        column_widths: List[int] = []

        for child in elem:
            local = child.tag.split("}")[-1]
            if local == "gridCol":
                w = _int_or_none(_wattr(child, "w"))
                if w is not None:
                    column_widths.append(w)

        return DocxASTTableGridNode(column_widths=column_widths)

    def _parse_table_row(
        self,
        elem: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTTableRowNode:
        """Parse a <w:tr> element."""
        row_node = DocxASTTableRowNode()

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "trPr":
                row_node.add_child(self._parse_table_row_properties(child))

            elif local == "tc":
                row_node.add_child(self._parse_table_cell(child, relationships))
            # tblPrEx (row-level table property overrides) is skipped

        return row_node

    def _parse_table_row_properties(
        self, elem: ET.Element
    ) -> DocxASTTableRowPropertiesNode:
        """Parse a <w:trPr> element."""
        is_header = False
        cant_split = False
        height: int | None = None
        height_rule: str | None = None

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "tblHeader":
                is_header = _wattr(child, "val") != "0"

            elif local == "cantSplit":
                cant_split = _wattr(child, "val") != "0"

            elif local == "trHeight":
                height = _int_or_none(_wattr(child, "val"))
                height_rule = _wattr(child, "hRule")

        return DocxASTTableRowPropertiesNode(
            is_header=is_header,
            cant_split=cant_split,
            height=height,
            height_rule=height_rule,
        )

    def _parse_table_cell(
        self,
        elem: ET.Element,
        relationships: Dict[str, str],
    ) -> DocxASTTableCellNode:
        """Parse a <w:tc> element."""
        cell_node = DocxASTTableCellNode()

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "tcPr":
                cell_node.add_child(self._parse_table_cell_properties(child))

            elif local == "p":
                cell_node.add_child(self._parse_paragraph(child, relationships))

            elif local == "tbl":
                # Nested table
                cell_node.add_child(self._parse_table(child, relationships))

        return cell_node

    def _parse_table_cell_properties(
        self, elem: ET.Element
    ) -> DocxASTTableCellPropertiesNode:
        """Parse a <w:tcPr> element."""
        width: int | None = None
        width_type: str | None = None
        grid_span = 1
        vertical_merge: str | None = None
        vertical_alignment: str | None = None
        shading_fill: str | None = None

        for child in elem:
            local = child.tag.split("}")[-1]

            if local == "tcW":
                width = _int_or_none(_wattr(child, "w"))
                width_type = _wattr(child, "type")

            elif local == "gridSpan":
                grid_span = _int_or_none(_wattr(child, "val")) or 1

            elif local == "vMerge":
                val = _wattr(child, "val")
                # Absence of val means "continue"; val="restart" means start
                vertical_merge = val if val else "continue"

            elif local == "vAlign":
                vertical_alignment = _wattr(child, "val")

            elif local == "shd":
                shading_fill = _wattr(child, "fill")

        return DocxASTTableCellPropertiesNode(
            width=width,
            width_type=width_type,
            grid_span=grid_span,
            vertical_merge=vertical_merge,
            vertical_alignment=vertical_alignment,
            shading_fill=shading_fill,
        )

    def _parse_section_properties(
        self, elem: ET.Element
    ) -> DocxASTSectionPropertiesNode:
        """Parse a <w:sectPr> element."""
        page_width: int | None = None
        page_height: int | None = None
        margin_top: int | None = None
        margin_right: int | None = None
        margin_bottom: int | None = None
        margin_left: int | None = None

        pg_sz = elem.find(_w("pgSz"))
        if pg_sz is not None:
            page_width = _int_or_none(_wattr(pg_sz, "w"))
            page_height = _int_or_none(_wattr(pg_sz, "h"))

        pg_mar = elem.find(_w("pgMar"))
        if pg_mar is not None:
            margin_top = _int_or_none(_wattr(pg_mar, "top"))
            margin_right = _int_or_none(_wattr(pg_mar, "right"))
            margin_bottom = _int_or_none(_wattr(pg_mar, "bottom"))
            margin_left = _int_or_none(_wattr(pg_mar, "left"))

        return DocxASTSectionPropertiesNode(
            page_width=page_width,
            page_height=page_height,
            margin_top=margin_top,
            margin_right=margin_right,
            margin_bottom=margin_bottom,
            margin_left=margin_left,
        )


def parse_docx(
    data: bytes,
    source_path: str | None = None,
) -> DocxASTDocumentNode:
    """
    Parse a DOCX file from raw bytes into a document AST.

    Convenience wrapper around DocxASTParser.

    Args:
        data: Raw bytes of the .docx file.
        source_path: Optional path to the source file.

    Returns:
        A DocxASTDocumentNode.

    Raises:
        DocxUnsupportedError: If the file is encrypted.
        DocxParseError: If the ZIP or XML is malformed.
        DocxExtractionError: If required XML structure is missing.
    """
    return DocxASTParser().parse(data, source_path=source_path)
