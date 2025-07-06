"""Editor highlighter."""

import logging

from PySide6.QtGui import QSyntaxHighlighter, QTextDocument
from humbug.gui.style_manager import StyleManager
from humbug.gui.tab.shell.shell_command_parser import ShellCommandParser


class ShellCommandHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for system command lines."""

    def __init__(self, parent: QTextDocument) -> None:
        """Initialize the highlighter."""
        super().__init__(parent)

        # Consistent font family fallback sequence for all code formats
        self._style_manager = StyleManager()
        self._logger = logging.getLogger("ShellCommandHighlighter")

    def highlightBlock(self, text: str) -> None:
        """Apply highlighting to the given block of text."""
        try:
            # Parse the input
            parser = ShellCommandParser()
            parser.parse(None, text)

            # Apply syntax highlighting based on token types
            while True:
                token = parser.get_next_token()
                if token is None:
                    break

                self.setFormat(
                    token.start,
                    len(token.value),
                    self._style_manager.get_highlight(token.type)
                )

        except Exception:
            self._logger.exception("highlighting exception")
