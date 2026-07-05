class HtmlError(Exception):
    """Base class for all HTML processing errors."""


class HtmlParseError(HtmlError):
    """Raised when the HTML source cannot be parsed."""
