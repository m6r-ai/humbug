"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

import re
from typing import List, Tuple, Any


class ConversationMarkdownConverter:
    """
    Converts simplified markdown to HTML.

    This class handles the incremental conversion of simplified markdown to HTML,
    focusing on headings, unordered lists, ordered lists, bold, and italic formatting
    with proper handling of block-level vs inline elements.
    """

    def __init__(self):
        """Initialize the markdown converter."""
        # Regular expressions for markdown elements
        self._heading_pattern = re.compile(r'^(#{1,6})\s+(.*?)(?:\s+#{1,6})?$', re.MULTILINE)
        self._bold_pattern = re.compile(r'\*\*(.*?)\*\*|\b__(.*?)__\b')
        self._italic_pattern = re.compile(r'\*([^*]+)\*|\b_([^_]+)_\b')

        # List item patterns - capture leading spaces, list marker, and content
        self._unordered_list_pattern = re.compile(r'^(\s*)([*+-])\s+(.*?)$', re.MULTILINE)
        self._ordered_list_pattern = re.compile(r'^(\s*)(\d+)\.[ \t]+(.*?)$', re.MULTILINE)

        # Track block state for incremental conversion
        self._current_blocks: List[str] = []
        self._block_converted: List[bool] = []
        self._block_html: List[str] = []

        # Track the part of the last block we've processed so far
        self._partial_last_block = ""

    def _is_list_item(self, line: str) -> bool:
        """
        Check if a line is a list item.

        Args:
            line: The line to check

        Returns:
            True if the line is a list item, False otherwise
        """
        return bool(self._unordered_list_pattern.match(line) or self._ordered_list_pattern.match(line))

    def _split_into_blocks(self, text: str) -> List[str]:
        """
        Split text into logical blocks for processing.

        This method separates text at:
        1. Blank lines (unless between list items)
        2. Headings
        3. Transitions between block types (headings, lists, paragraphs)

        It also keeps list items with blank lines between them in the same block.

        Args:
            text: The text to split

        Returns:
            List of text blocks
        """
        blocks = []
        current_block = []
        in_list = False
        current_block_type = None  # Can be 'heading', 'list', 'paragraph', or None

        lines = text.split('\n')
        i = 0
        while i < len(lines):
            line = lines[i]
            # Check the type of the current line
            is_heading = self._heading_pattern.match(line)
            is_list_item = self._is_list_item(line)

            # Determine the line type
            if is_heading:
                line_type = 'heading'
            elif is_list_item:
                line_type = 'list'
            elif line.strip():
                line_type = 'paragraph'
            else:
                line_type = 'blank'

            # Handle transitions between different block types
            if line_type == 'heading':
                # Always start a new block for headings
                if current_block:
                    blocks.append('\n'.join(current_block))

                current_block = [line]
                current_block_type = 'heading'
                in_list = False
            elif line_type == 'list':
                # Start a new block if transitioning from heading to list
                if current_block_type == 'heading':
                    blocks.append('\n'.join(current_block))
                    current_block = [line]
                    current_block_type = 'list'
                    in_list = True
                # Continue the current block for nested lists or lists following paragraphs
                else:
                    if not in_list and current_block:
                        # Transitioning from paragraph to list
                        blocks.append('\n'.join(current_block))
                        current_block = [line]
                    else:
                        # Already in a list or starting a new one
                        current_block.append(line)

                    current_block_type = 'list'
                    in_list = True
            elif line_type == 'blank':
                if current_block:
                    # If we're in a list, look ahead to see if the list continues
                    if in_list:
                        j = i + 1
                        next_line = ""
                        while j < len(lines):
                            next_line = lines[j]
                            if next_line.strip():
                                break

                            j += 1
                            if j < len(lines):
                                next_line = lines[j]

                        # If next non-blank line is a list item, add blank line to current block
                        if j < len(lines) and self._is_list_item(next_line):
                            current_block.append(line)
                        else:
                            # End of list
                            blocks.append('\n'.join(current_block))
                            current_block = []
                            current_block_type = None
                            in_list = False
                    else:
                        # Not in a list, treat blank line as block separator
                        blocks.append('\n'.join(current_block))
                        current_block = []
                        current_block_type = None
            else:  # paragraph line
                if current_block_type == 'heading':
                    # If coming from a heading, start a new block
                    blocks.append('\n'.join(current_block))
                    current_block = [line]
                    current_block_type = 'paragraph'
                elif in_list:
                    # If in a list, this is probably a continuation or indented content
                    current_block.append(line)
                else:
                    # Regular paragraph content
                    if not current_block:
                        # Starting a new paragraph
                        current_block_type = 'paragraph'

                    current_block.append(line)

            i += 1

        # Add the last block if there is one
        if current_block:
            blocks.append('\n'.join(current_block))

        return blocks

    def _apply_inline_formatting(self, text: str) -> str:
        """
        Apply inline formatting (bold, italic) to text.

        Args:
            text: The text to format

        Returns:
            Text with inline formatting applied
        """
        # First, escape special HTML characters
        def escape_html(text):
            # Replace < with &lt; and > with &gt;
            text = text.replace('&', '&amp;')  # Must come first to avoid double-escaping
            text = text.replace('<', '&lt;')
            text = text.replace('>', '&gt;')
            return text

        # Escape the text first
        text = escape_html(text)

        # Apply bold formatting
        def replace_bold(match):
            content = match.group(1) or match.group(2)
            return f"<strong>{content}</strong>" if content else "**"

        # Apply italic formatting
        def replace_italic(match):
            content = match.group(1) or match.group(2)
            return f"<em>{content}</em>" if content else "*"

        formatted = self._bold_pattern.sub(replace_bold, text)
        return self._italic_pattern.sub(replace_italic, formatted)

    def _handle_line_breaks(self, text: str) -> str:
        """
        Process text to convert lines ending with two spaces to line breaks.

        Args:
            text: The text to process

        Returns:
            Text with appropriate line breaks added
        """
        lines = text.split('\n')
        processed_lines = []

        for line in lines:
            if line.endswith('  '):  # Line ends with exactly two spaces
                processed_lines.append(line.rstrip() + '<br />')
            else:
                processed_lines.append(line)

        return '\n'.join(processed_lines)

    def _identify_line_type(self, line: str) -> Tuple[str, any]:
        """
        Identify the type of a single line of text.

        Args:
            line: The line to identify

        Returns:
            Tuple of (line_type, content) where line_type is one of:
            'heading', 'unordered_list_item', 'ordered_list_item', 'blank', 'text'
        """
        if not line.strip():
            return 'blank', None

        # Check for heading
        heading_match = self._heading_pattern.match(line)
        if heading_match:
            level = len(heading_match.group(1))
            content = heading_match.group(2).strip()
            return 'heading', (level, content)

        # Check for unordered list item
        unordered_match = self._unordered_list_pattern.match(line)
        if unordered_match:
            indent = len(unordered_match.group(1))
            marker = unordered_match.group(2)
            content = unordered_match.group(3)
            return 'unordered_list_item', (indent, marker, content)

        # Check for ordered list item
        ordered_match = self._ordered_list_pattern.match(line)
        if ordered_match:
            indent = len(ordered_match.group(1))
            number = ordered_match.group(2)
            content = ordered_match.group(3)
            return 'ordered_list_item', (indent, number, content)

        # Default to regular text
        return 'text', line

    def _convert_block(self, block: str) -> str:
        """
        Convert a single block of markdown to HTML.

        This simplified approach handles block conversion directly.

        Args:
            block: The block of text to convert

        Returns:
            HTML converted text
        """
        lines = block.split('\n')

        # Check for heading block (simple case)
        if lines and self._heading_pattern.match(lines[0]):
            _line_type, content = self._identify_line_type(lines[0])
            level, heading_text = content
            return self._convert_heading(level, heading_text)

        # Check if this is a list block
        has_list_items = any(
            self._identify_line_type(line)[0] in ('ordered_list_item', 'unordered_list_item')
            for line in lines
        )

        if has_list_items:
            return self._handle_list_block(lines)

        # Otherwise, treat as paragraph
        paragraph_text = self._handle_line_breaks('\n'.join(lines))
        formatted_text = self._apply_inline_formatting(paragraph_text)
        return f"<p>{formatted_text}</p>"

    def _convert_heading(self, level: int, content: str) -> str:
        """
        Convert a heading to HTML.

        Args:
            level: Heading level (1-6)
            content: Heading content

        Returns:
            HTML heading
        """
        # Apply inline formatting within the heading
        formatted_content = self._apply_inline_formatting(content)
        return f"<h{level}>{formatted_content}</h{level}>"

    def _handle_list_block(self, lines: List[str]) -> str:
        """
        Handle a block of text containing list items.

        Args:
            lines: Lines of text that include list items

        Returns:
            HTML string representing the list structure
        """
        # Parse each line into a list item structure
        list_items = []
        i = 0
        while i < len(lines):
            line_type, content = self._identify_line_type(lines[i])

            if line_type in ('ordered_list_item', 'unordered_list_item'):
                indent, marker, text = content

                # Check for continuation lines
                continuation_text = []
                j = i + 1
                while j < len(lines):
                    next_line_type, _ = self._identify_line_type(lines[j])
                    # Include continuation text and blank lines
                    if next_line_type == 'text':
                        continuation_text.append(lines[j])
                        j += 1
                    elif next_line_type == 'blank':
                        # Check if there's a list item after this blank line
                        if j + 1 < len(lines):
                            after_blank_type, _ = self._identify_line_type(lines[j + 1])
                            if after_blank_type in ('ordered_list_item', 'unordered_list_item'):
                                # This is a blank line between list items
                                break
                        # Add blank line as part of content
                        continuation_text.append(lines[j])
                        j += 1
                    else:
                        break

                # Append continuation lines to text if any
                if continuation_text:
                    text += '\n' + '\n'.join(continuation_text)
                    i = j - 1  # Skip these lines

                # Add list item with its processed content
                list_items.append((line_type, (indent, marker, text)))
            elif line_type == 'blank':
                # Skip blank lines between list items
                pass
            else:
                # Non-empty, non-list line - treat as a paragraph
                formatted_text = self._apply_inline_formatting(self._handle_line_breaks(lines[i]))
                list_items.append(('paragraph', formatted_text))

            i += 1

        # Now process the list items
        return self._process_list_items(list_items)

    def _process_list_items(self, items: List[Tuple[str, Any]]) -> str:
        """
        Process list items into proper HTML using a simplified stack approach.

        Args:
            items: List of parsed items (line_type, content)

        Returns:
            HTML string
        """
        # Filter only actual list items
        list_items = [item for item in items if item[0] in ('ordered_list_item', 'unordered_list_item')]

        # If no list items, just return paragraphs
        if not list_items:
            html_parts = []
            for item_type, content in items:
                if item_type == 'paragraph':
                    html_parts.append(f"<p>{content}</p>")
            return "\n".join(html_parts)

        # Start with paragraphs if there are any before the first list
        html_parts = []
        for item in items:
            if item[0] == 'paragraph':
                html_parts.append(f"<p>{item[1]}</p>")
            else:
                break

        # Each stack entry: [indent, is_ordered, in_item, item_content]
        list_stack = []

        # Process each list item
        for item_type, (indent, _marker, content) in list_items:
            # Format content
            formatted_content = self._apply_inline_formatting(content)
            is_ordered = (item_type == 'ordered_list_item')

            # If stack is empty, start first list
            if not list_stack:
                list_tag = "ol" if is_ordered else "ul"
                html_parts.append(f"<{list_tag}>")
                html_parts.append(f"<li>{formatted_content}")
                list_stack.append([indent, is_ordered, True, formatted_content])
                continue

            # Check relationship with top of stack
            top_indent, top_ordered, in_item, _ = list_stack[-1]

            # Same level as top of stack
            if indent == top_indent:
                # Close previous item if needed
                if in_item:
                    html_parts.append("</li>")

                # If different list type at same level, close old and start new
                if is_ordered != top_ordered:
                    # Close all nested lists up to this level
                    while list_stack and list_stack[-1][0] >= indent:
                        if list_stack[-1][2]:  # in_item
                            html_parts.append("</li>")
                        list_tag = "ol" if list_stack[-1][1] else "ul"
                        html_parts.append(f"</{list_tag}>")
                        list_stack.pop()

                    # Start new list of correct type
                    list_tag = "ol" if is_ordered else "ul"
                    html_parts.append(f"<{list_tag}>")

                # Add new item
                html_parts.append(f"<li>{formatted_content}")

                # Update stack
                list_stack[-1] = [indent, is_ordered, True, formatted_content]

            # Nested deeper than top of stack
            elif indent > top_indent:
                # Start a nested list
                list_tag = "ol" if is_ordered else "ul"
                html_parts.append(f"<{list_tag}>")
                html_parts.append(f"<li>{formatted_content}")

                # Add to stack
                list_stack.append([indent, is_ordered, True, formatted_content])

            # Less indented than top of stack - moving out
            else:
                # Close lists until we find appropriate level
                while list_stack and list_stack[-1][0] > indent:
                    if list_stack[-1][2]:  # in_item
                        html_parts.append("</li>")
                    list_tag = "ol" if list_stack[-1][1] else "ul"
                    html_parts.append(f"</{list_tag}>")
                    list_stack.pop()

                # At this point, we should be at the same level or still deeper
                if list_stack and list_stack[-1][0] == indent:
                    # Same level - close current item if any
                    if list_stack[-1][2]:
                        html_parts.append("</li>")

                    # If different list type, close and start new
                    if list_stack[-1][1] != is_ordered:
                        list_tag = "ol" if list_stack[-1][1] else "ul"
                        html_parts.append(f"</{list_tag}>")
                        list_stack.pop()

                        # Start new list
                        list_tag = "ol" if is_ordered else "ul"
                        html_parts.append(f"<{list_tag}>")
                        html_parts.append(f"<li>{formatted_content}")
                        list_stack.append([indent, is_ordered, True, formatted_content])
                    else:
                        # Same type - continue list
                        html_parts.append(f"<li>{formatted_content}")
                        list_stack[-1] = [indent, is_ordered, True, formatted_content]
                else:
                    # We've closed too many lists - start a new one
                    list_tag = "ol" if is_ordered else "ul"
                    html_parts.append(f"<{list_tag}>")
                    html_parts.append(f"<li>{formatted_content}")
                    list_stack.append([indent, is_ordered, True, formatted_content])

        # Close any remaining open lists
        while list_stack:
            if list_stack[-1][2]:  # in_item
                html_parts.append("</li>")
            list_tag = "ol" if list_stack[-1][1] else "ul"
            html_parts.append(f"</{list_tag}>")
            list_stack.pop()

        # Add paragraphs after the list if any
        paragraph_found = False
        for item in items:
            if paragraph_found and item[0] == 'paragraph':
                html_parts.append(f"<p>{item[1]}</p>")
            elif item[0] != 'paragraph':
                paragraph_found = True

        return "\n".join(html_parts)

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

        # Handle empty input
        if not new_blocks:
            return ""

        # Initialize if this is the first conversion
        if not self._current_blocks:
            self._current_blocks = new_blocks
            self._block_converted = [False] * len(new_blocks)
            self._block_html = [""] * len(new_blocks)
            self._partial_last_block = new_blocks[-1]
        else:
            # Find how many blocks match from the beginning
            common_prefix_len = 0
            for i, (old, new) in enumerate(zip(self._current_blocks, new_blocks)):
                if old == new:
                    common_prefix_len = i + 1
                else:
                    break

            # Update our tracking arrays
            if len(new_blocks) > len(self._current_blocks):
                # New blocks added
                self._current_blocks = new_blocks
                # Keep converted status for existing blocks
                self._block_converted = (
                    self._block_converted +
                    [False] * (len(new_blocks) - len(self._block_converted))
                )
                # Keep HTML for existing blocks
                self._block_html = (
                    self._block_html +
                    [""] * (len(new_blocks) - len(self._block_html))
                )
                # Always consider the last block as "in progress"
                self._block_converted[-1] = False
            elif common_prefix_len < len(new_blocks):
                # Update with changed blocks
                self._current_blocks = new_blocks
                # Mark changed blocks as not converted
                for i in range(common_prefix_len, len(new_blocks)):
                    self._block_converted[i] = False
                    self._block_html[i] = ""
                # Trim arrays if blocks were removed
                if len(new_blocks) < len(self._block_converted):
                    self._block_converted = self._block_converted[:len(new_blocks)]
                    self._block_html = self._block_html[:len(new_blocks)]

            self._partial_last_block = new_blocks[-1]

        # Process all unconverted blocks
        for i, block in enumerate(self._current_blocks):
            if not self._block_converted[i]:
                self._block_html[i] = self._convert_block(block)
                # Mark all blocks except the last one as converted
                if i < len(self._current_blocks) - 1:
                    self._block_converted[i] = True

        # Combine all processed blocks
        print("\n".join(self._block_html))
        return "\n".join(self._block_html)

    def reset(self):
        """Reset the converter state."""
        self._current_blocks = []
        self._block_converted = []
        self._block_html = []
        self._partial_last_block = ""
