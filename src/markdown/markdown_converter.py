"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

import logging
from typing import List, Tuple, cast

from syntax.programming_language import ProgrammingLanguage
from syntax.programming_language_utils import ProgrammingLanguageUtils

from markdown.markdown_ast_builder import MarkdownASTBuilder
from markdown.markdown_ast_node import MarkdownASTCodeBlockNode, MarkdownASTNode, MarkdownASTDocumentNode


class MarkdownConverter:
    """
    Converts simplified markdown to HTML using an AST-based approach.

    This class handles the incremental parsing of simplified markdown into an
    Abstract Syntax Tree (AST) representation and extraction of content sections for rendering.
    """

    def __init__(self) -> None:
        """
        Initialize the markdown converter with an AST builder and HTML renderer.

        Args:
            source_path: Optional path to the source markdown file
        """
        self.ast_builder = MarkdownASTBuilder(True)

        self._logger = logging.getLogger("ConversationMarkdownConverter")

        # Keep track of the current text for incremental updates
        self.current_text = ""

        # Store builder state for preservation during reset
        self.builder_state = None

        self._source_path: str | None = None

    def extract_sections(self, text: str, path: str | None) -> List[Tuple[MarkdownASTNode, ProgrammingLanguage | None]]:
        """
        Process markdown text and extract content sections from it.

        Args:
            text: The markdown text to process
            path: Optional path to the source markdown file

        Returns:
            List of (node, language) tuples where language is None for markdown content
            and a ProgrammingLanguage enum for code blocks

        Raises:
            None
        """
        self._source_path = path

        # Update the AST based on the changes
        self.ast_builder.update_ast(text, self.current_text, path)
        self.current_text = text

        # Extract content sections from the AST document
        return self._extract_sections_from_ast(self.ast_builder.document())

    def _extract_sections_from_ast(self, document: MarkdownASTNode) -> List[Tuple[MarkdownASTNode, ProgrammingLanguage | None]]:
        """
        Extract content sections from the AST document.

        Args:
            document: The AST document node

        Returns:
            List of (node, language) tuples where language is None for markdown content
            and a ProgrammingLanguage enum for code blocks
        """
        sections: List[tuple[MarkdownASTNode, ProgrammingLanguage | None]] = []
        current_markdown_nodes: List[MarkdownASTNode] = []

        # Helper function to add accumulated markdown content as a section
        def add_markdown_section() -> None:
            if current_markdown_nodes:
                # Create a container node for these markdown nodes
                container = MarkdownASTDocumentNode(self._source_path)
                for node in current_markdown_nodes:
                    container.add_child(node)

                # Add as a markdown section
                sections.append((container, None))
                current_markdown_nodes.clear()

        # Process all nodes in the document
        for node in document.children:
            if isinstance(node, MarkdownASTCodeBlockNode):
                # Add any accumulated markdown before this code block
                add_markdown_section()

                # Process the code block
                language = ProgrammingLanguageUtils.from_name(node.language) if node.language else ProgrammingLanguage.TEXT

                # Add the code block node as a section with its language
                sections.append((node, language))

            else:
                # Add to current markdown content
                current_markdown_nodes.append(cast(MarkdownASTNode, node))

        # Add any remaining markdown content
        add_markdown_section()

        return sections
