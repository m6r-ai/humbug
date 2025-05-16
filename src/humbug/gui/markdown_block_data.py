"""User data classes for QTextBlock to support extended markdown functionality."""

from PySide6.QtGui import QTextBlockUserData


class MarkdownBlockData(QTextBlockUserData):
    """Base class for all markdown-specific text block user data."""
    
    def __init__(self) -> None:
        """Initialize the base class."""
        super().__init__()


class HeadingBlockData(MarkdownBlockData):
    """Class to store heading identifier data for text blocks."""

    def __init__(self, element_id: str) -> None:
        """
        Initialize heading block data.

        Args:
            element_id: The identifier for this heading
        """
        super().__init__()
        self.element_id = element_id

