import io
import zipfile
import xml.etree.ElementTree as ET

from docx.docx_errors import DocxExtractionError, DocxParseError, DocxUnsupportedError

_W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
_DOCUMENT_PATH = "word/document.xml"
_ENCRYPTED_ENTRY = "EncryptedPackage"


def extract_text(data: bytes) -> str:
    """
    Extract body text from a DOCX file given its raw bytes.

    Paragraphs are extracted in document order and joined with newlines.
    Headers, footers, and text boxes are ignored.

    Raises:
        DocxUnsupportedError: if the file is encrypted.
        DocxParseError: if the data is not a valid DOCX ZIP or the XML is malformed.
        DocxExtractionError: if the document body cannot be located in the XML.
    """
    try:
        zf = zipfile.ZipFile(io.BytesIO(data))

    except zipfile.BadZipFile as e:
        raise DocxParseError(f"Not a valid DOCX file (ZIP open failed): {e}") from e

    with zf:
        names = zf.namelist()

        if _ENCRYPTED_ENTRY in names:
            raise DocxUnsupportedError("File is encrypted and cannot be read")

        if _DOCUMENT_PATH not in names:
            raise DocxParseError(f"Not a valid DOCX file (missing {_DOCUMENT_PATH!r})")

        try:
            xml_bytes = zf.read(_DOCUMENT_PATH)

        except KeyError as e:
            raise DocxParseError(f"Could not read {_DOCUMENT_PATH!r}: {e}") from e

    try:
        root = ET.fromstring(xml_bytes)

    except ET.ParseError as e:
        raise DocxParseError(f"Malformed XML in {_DOCUMENT_PATH!r}: {e}") from e

    body = root.find(f"{{{_W}}}body")
    if body is None:
        raise DocxExtractionError("document.xml has no w:body element")

    return _extract_body_text(body)


def _extract_body_text(body: ET.Element) -> str:
    """Extract text from a w:body element, returning paragraphs joined by newlines."""
    paragraphs: list[str] = []

    for para in body.iter(f"{{{_W}}}p"):
        text = _extract_paragraph_text(para)
        paragraphs.append(text)

    # Strip trailing blank lines while preserving internal ones
    while paragraphs and not paragraphs[-1]:
        paragraphs.pop()

    return "\n".join(paragraphs)


def _extract_paragraph_text(para: ET.Element) -> str:
    """Extract the plain text from a single w:p element."""
    parts: list[str] = []

    for elem in para.iter(f"{{{_W}}}t"):
        if elem.text:
            parts.append(elem.text)

    return "".join(parts)
