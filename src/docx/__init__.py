from docx.docx_errors import DocxError, DocxExtractionError, DocxParseError, DocxUnsupportedError
from docx.docx_extractor import extract_text

__all__ = [
    # Errors
    "DocxError",
    "DocxParseError",
    "DocxUnsupportedError",
    "DocxExtractionError",
    # API
    "extract_text",
]
