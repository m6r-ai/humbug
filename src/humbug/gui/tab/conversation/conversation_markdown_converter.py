"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

import re
from typing import List, Tuple, Dict, Optional


class ConversationMarkdownConverter:
    """
    Converts simplified markdown to HTML.

    This class handles the incremental conversion of simplified markdown to HTML,
    focusing on headings, unordered lists, bold, and italic formatting.
    """

    def __init__(self):
        """Initialize the markdown converter."""
        # Track list state for proper list generation
        self._in_list = False
        self._list_depth = 0

        # Regular expressions for markdown elements
        self._heading_pattern = re.compile(r'^(#{1,6})\s+(.*?)(?:\s+#{1,6})?$', re.MULTILINE)
        self._bold_pattern = re.compile(r'\*\*(.*?)\*\*|\b__(.*?)__\b')
        self._italic_pattern = re.compile(r'\*([^*]+)\*|\b_([^_]+)_\b')
        self._list_pattern = re.compile(r'^(\s*)[*+-]\s+(.*?)$', re.MULTILINE)

        # Track block state for incremental conversion
        self._current_blocks: List[str] = []

        # For each block, track if it's been converted to HTML
        self._block_converted: List[bool] = []

        # For each block, store its HTML version
        self._block_html: List[str] = []

        # Track the part of the last block we've processed so far
        self._partial_last_block = ""

    def _convert_headings(self, text: str) -> str:
        """
        Convert markdown headings to HTML.

        Args:
            text: The markdown text to convert

        Returns:
            Text with headings converted to HTML
        """
        def replace_heading(match):
            level = len(match.group(1))
            content = match.group(2).strip()
            return f"<h{level}>{content}</h{level}>"

        return self._heading_pattern.sub(replace_heading, text)

    def _convert_bold(self, text: str) -> str:
        """
        Convert markdown bold to HTML.

        Args:
            text: The markdown text to convert

        Returns:
            Text with bold formatting converted to HTML
        """
        def replace_bold(match):
            content = match.group(1) or match.group(2)
            return f"<strong>{content}</strong>"

        return self._bold_pattern.sub(replace_bold, text)

    def _convert_italic(self, text: str) -> str:
        """
        Convert markdown italic to HTML.

        Args:
            text: The markdown text to convert

        Returns:
            Text with italic formatting converted to HTML
        """
        def replace_italic(match):
            content = match.group(1) or match.group(2)
            return f"<em>{content}</em>"

        return self._italic_pattern.sub(replace_italic, text)

    def _handle_list_blocks(self, blocks: List[str]) -> List[str]:
        """
        Process and convert list items to HTML, maintaining proper list nesting.

        Args:
            blocks: List of text blocks

        Returns:
            List of blocks with list items converted to HTML
        """
        result = []
        in_list = False
        list_stack = []
        current_indent = 0

        for block in blocks:
            list_match = self._list_pattern.match(block)

            if not list_match:
                # Not a list item, close any open lists
                if in_list:
                    while list_stack:
                        result.append(f"</ul>")
                        list_stack.pop()
                    in_list = False
                    current_indent = 0

                # Add the normal block
                result.append(block)
                continue

            # Handle list item
            indent = len(list_match.group(1))
            content = list_match.group(2)

            if not in_list:
                # Start a new list
                result.append(f"<ul>")
                list_stack.append(indent)
                in_list = True
                current_indent = indent
            elif indent > current_indent:
                # Deeper nesting level
                result.append(f"<ul>")
                list_stack.append(indent)
                current_indent = indent
            elif indent < current_indent:
                # Going back up the nesting levels
                while list_stack and indent < current_indent:
                    result.append(f"</ul>")
                    list_stack.pop()
                    if list_stack:
                        current_indent = list_stack[-1]
                    else:
                        current_indent = 0

            # Add the list item
            result.append(f"<li>{content}</li>")

        # Close any open lists
        while list_stack:
            result.append(f"</ul>")
            list_stack.pop()

        return result

    def _split_into_blocks(self, text: str) -> List[str]:
        """
        Split text into blocks based on blank lines.

        Args:
            text: The text to split

        Returns:
            List of text blocks
        """
        blocks = []
        current_block = []

        lines = text.split('\n')
        for line in lines:
            if line.strip() == "":
                if current_block:
                    blocks.append('\n'.join(current_block))
                    current_block = []
            else:
                current_block.append(line)

        if current_block:
            blocks.append('\n'.join(current_block))

        return blocks

    def _convert_block(self, block: str) -> str:
        """
        Convert a single block of markdown to HTML.

        Args:
            block: The block of text to convert

        Returns:
            HTML converted text
        """
        # Check if it matches a heading pattern
        if self._heading_pattern.match(block):
            return self._convert_headings(block)

        # Check if it matches a list pattern
        if self._list_pattern.match(block):
            # Individual list items are handled in _handle_list_blocks
            return block

        # Apply inline formatting
        formatted = self._convert_bold(block)
        formatted = self._convert_italic(formatted)

        # Wrap in paragraph if it doesn't start with HTML tag
        if not formatted.startswith('<'):
            return f"<p>{formatted}</p>"

        return formatted

    def convert_incremental(self, new_text: str) -> str:
        """
        Convert markdown to HTML incrementally as text is being added.

        Args:
            new_text: The updated text to convert

        Returns:
            HTML converted text
        """
        # Split into blocks
        new_blocks = self._split_into_blocks(new_text)

        # If we have no previous blocks, process everything
        if not self._current_blocks:
            self._current_blocks = new_blocks
            self._block_converted = [False] * len(new_blocks)
            self._block_html = [""] * len(new_blocks)
            self._partial_last_block = new_blocks[-1] if new_blocks else ""
        else:
            # Check if blocks have been added or modified
            common_prefix_len = 0

            # Find common prefix length
            for i, (old, new) in enumerate(zip(self._current_blocks, new_blocks)):
                if old == new:
                    common_prefix_len = i + 1
                else:
                    break

            # Update blocks
            if len(new_blocks) > len(self._current_blocks):
                # New blocks added
                self._current_blocks = new_blocks
                # Mark added blocks as not converted
                self._block_converted.extend([False] * (len(new_blocks) - len(self._block_converted)))
                self._block_html.extend([""] * (len(new_blocks) - len(self._block_html)))
                self._partial_last_block = new_blocks[-1]
            elif len(new_blocks) <= len(self._current_blocks) and common_prefix_len < len(new_blocks):
                # Last common block changed
                self._current_blocks = new_blocks
                self._block_converted[common_prefix_len:] = [False] * (len(new_blocks) - common_prefix_len)
                self._block_html[common_prefix_len:] = [""] * (len(new_blocks) - common_prefix_len)
                self._partial_last_block = new_blocks[-1]
            else:
                # Weird case - same or fewer blocks with existing content unchanged
                # Just reset to be safe
                self._current_blocks = new_blocks
                self._block_converted = [False] * len(new_blocks)
                self._block_html = [""] * len(new_blocks)
                self._partial_last_block = new_blocks[-1] if new_blocks else ""

        # Convert each unconverted block
        for i, block in enumerate(self._current_blocks):
            if not self._block_converted[i]:
                # Last block might be still in progress, so don't mark it as converted
                is_last_block = (i == len(self._current_blocks) - 1)

                # Convert the block
                converted = self._convert_block(block)
                self._block_html[i] = converted

                # Mark as converted (except last block)
                if not is_last_block:
                    self._block_converted[i] = True

        # Handle lists - process all blocks to ensure proper list structure
        converted_blocks = self._handle_list_blocks(self._block_html)

        # Join all blocks and return
        return '\n'.join(converted_blocks)

    def reset(self):
        """Reset the converter state."""
        self._current_blocks = []
        self._block_converted = []
        self._block_html = []
        self._partial_last_block = ""
        self._in_list = False
        self._list_depth = 0
