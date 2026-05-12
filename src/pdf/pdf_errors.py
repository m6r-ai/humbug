class PDFError(Exception):
    """Base exception for all PDF module errors."""

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self.message = message

    def __str__(self) -> str:
        return self.message


class PDFParseError(PDFError):
    """Raised when the PDF structure is malformed or cannot be parsed."""


class PDFUnsupportedError(PDFError):
    """Raised when the PDF is valid but uses a feature not supported by this implementation."""


class PDFExtractionError(PDFError):
    """Raised when text extraction fails."""
