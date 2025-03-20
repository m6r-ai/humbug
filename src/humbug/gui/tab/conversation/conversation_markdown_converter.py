"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

import logging

from humbug.gui.tab.conversation.conversation_markdown_ast_builder import (
    ASTBuilder, MarkdownParseError
)
from humbug.gui.tab.conversation.conversation_markdown_ast_node import (
    HTMLRenderer, ASTPrinter
)


class ConversationMarkdownConverter:
    """
    Converts simplified markdown to HTML using an AST-based approach.

    This class handles the incremental conversion of simplified markdown to HTML,
    focusing on headings, unordered lists, ordered lists, bold, and italic formatting
    with proper handling of block-level vs inline elements.
    """

    def __init__(self):
        """Initialize the markdown converter with an AST builder and HTML renderer."""
        self.ast_builder = ASTBuilder()
        self.renderer = HTMLRenderer()
        self.ast_printer = ASTPrinter()  # For debugging

        self._logger = logging.getLogger("ConversationMarkdownConverter")

        # Keep track of the current text for incremental updates
        self.current_text = ""

    def convert_incremental(self, new_text: str) -> str:
        """
        Convert markdown to HTML incrementally as text is being added.

        Args:
            new_text: The updated text to convert

        Returns:
            HTML converted text

        Raises:
            None
        """
        try:
            # Update the AST based on the changes
            self.ast_builder.update_ast(new_text, self.current_text)

            # Update the current text
            self.current_text = new_text

            # For debugging: print the AST structure
            # self.ast_printer.visit(self.ast_builder.document)

            # Render the AST to HTML
            html = self.renderer.visit(self.ast_builder.document)
            return html

        except MarkdownParseError as e:
            self._logger.exception("Error converting markdown")
            # Return a fallback HTML message for the error
            return f"<p>Error converting markdown: {e}</p>"

    def reset(self):
        """Reset the converter state to initial values."""
        self.ast_builder = ASTBuilder()
        self.current_text = ""
