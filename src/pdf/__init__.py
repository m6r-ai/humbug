"""
PDF text extraction using only the Python standard library.
"""

from pdf.pdf_errors import PDFError, PDFExtractionError, PDFParseError, PDFUnsupportedError
from pdf.pdf_extractor import extract_text
from pdf.pdf_parser import parse
from pdf.pdf_types import PDFDocument, PDFObjectRef, PDFStream, PDFXRefEntry

__all__ = [
    # Errors
    "PDFError",
    "PDFParseError",
    "PDFUnsupportedError",
    "PDFExtractionError",
    # Types
    "PDFObjectRef",
    "PDFStream",
    "PDFXRefEntry",
    "PDFDocument",
    # API
    "parse",
    "extract_text",
]
