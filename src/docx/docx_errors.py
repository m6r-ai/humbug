class DocxError(Exception):
    """Base exception for all DOCX module errors."""

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self.message = message

    def __str__(self) -> str:
        return self.message


class DocxParseError(DocxError):
    """Raised when the DOCX structure is malformed or cannot be parsed."""


class DocxUnsupportedError(DocxError):
    """Raised when the DOCX is valid but uses a feature not supported by this implementation."""


class DocxExtractionError(DocxError):
    """Raised when text extraction fails."""
