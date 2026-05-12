"""Shared fixtures and helpers for docx module tests."""

import io
import zipfile


_CONTENT_TYPES = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/word/document.xml"
    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
</Types>"""

_RELS = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
    Target="word/document.xml"/>
</Relationships>"""

_WORD_RELS = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
</Relationships>"""

_W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"


def _build_document_xml(paragraphs: list[str]) -> str:
    """Build a minimal word/document.xml containing the given paragraphs."""
    para_xml_parts = []
    for para in paragraphs:
        if para:
            runs = f'<w:r><w:t xml:space="preserve">{_escape_xml(para)}</w:t></w:r>'

        else:
            runs = ""
        para_xml_parts.append(f"<w:p>{runs}</w:p>")

    paras = "\n    ".join(para_xml_parts)
    return f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:w="{_W_NS}">
  <w:body>
    {paras}
  </w:body>
</w:document>"""


def _escape_xml(text: str) -> str:
    """Escape special XML characters."""
    return (
        text
        .replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
    )


def make_docx(paragraphs: list[str]) -> bytes:
    """Build a minimal but valid DOCX ZIP containing the given paragraphs.

    Each string in `paragraphs` becomes one paragraph in the document body.
    """
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", compression=zipfile.ZIP_DEFLATED) as zf:
        zf.writestr("[Content_Types].xml", _CONTENT_TYPES)
        zf.writestr("_rels/.rels", _RELS)
        zf.writestr("word/_rels/document.xml.rels", _WORD_RELS)
        zf.writestr("word/document.xml", _build_document_xml(paragraphs))

    return buf.getvalue()


def make_encrypted_docx() -> bytes:
    """Build a ZIP that looks like an encrypted DOCX (contains EncryptedPackage)."""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        zf.writestr("EncryptedPackage", b"\x00" * 16)
        zf.writestr("EncryptionInfo", b"\x00" * 16)

    return buf.getvalue()


def make_docx_missing_document() -> bytes:
    """Build a ZIP that is a valid ZIP but lacks word/document.xml."""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        zf.writestr("[Content_Types].xml", _CONTENT_TYPES)
        zf.writestr("_rels/.rels", _RELS)

    return buf.getvalue()


def make_docx_bad_xml() -> bytes:
    """Build a DOCX ZIP whose word/document.xml contains malformed XML."""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        zf.writestr("[Content_Types].xml", _CONTENT_TYPES)
        zf.writestr("_rels/.rels", _RELS)
        zf.writestr("word/_rels/document.xml.rels", _WORD_RELS)
        zf.writestr("word/document.xml", b"<<< this is not xml >>>")

    return buf.getvalue()


def make_docx_no_body() -> bytes:
    """Build a DOCX whose document.xml has no w:body element."""
    xml = f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:w="{_W_NS}">
</w:document>"""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        zf.writestr("[Content_Types].xml", _CONTENT_TYPES)
        zf.writestr("_rels/.rels", _RELS)
        zf.writestr("word/_rels/document.xml.rels", _WORD_RELS)
        zf.writestr("word/document.xml", xml)

    return buf.getvalue()
