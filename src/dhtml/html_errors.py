class HtmlError(Exception):
    """Base class for all HTML processing errors."""


class HtmlParseError(HtmlError):
    """Raised when the HTML input cannot be parsed."""


class HtmlExtractionError(HtmlError):
    """Raised when text extraction from an HTML DOM fails."""
