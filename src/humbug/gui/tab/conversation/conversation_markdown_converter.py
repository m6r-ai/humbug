"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

from enum import Enum, auto
import re
from typing import List, Tuple, Any


class ConverterState(Enum):
    NONE = auto()
    PARAGRAPH = auto()
    LIST = auto()


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

        # Track line state for incremental conversion
        self._current_lines: List[str] = []
        self._last_line_converted: int = -1
        self._line_html: List[str] = []
        self._state: ConverterState = ConverterState.NONE

        # Each list stack entry: [indent, is_ordered, in_item, item_content]
        self._list_stack = []

        # Track the part of the last line we've processed so far
        self._partial_last_line = ""

    def _is_list_item(self, line: str) -> bool:
        """
        Check if a line is a list item.

        Args:
            line: The line to check

        Returns:
            True if the line is a list item, False otherwise
        """
        return bool(self._unordered_list_pattern.match(line) or self._ordered_list_pattern.match(line))

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
        processed_lines = []

        if text.endswith('  '):  # Line ends with exactly two spaces
            processed_lines.append(text.rstrip() + '<br />')
        else:
            processed_lines.append(text)

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

        # Start with paragraphs if there are any before the first list
        html_parts = []
        for item in items:
            if item[0] == 'paragraph':
                print(f"how did we get a paragraph? {item[1]}")
                html_parts.append(f"<p>{item[1]}</p>")
            else:
                break

        # If we have previously had a list stack then restore it and put ourselves
        # to the correct list indentation to match.
        list_stack = self._list_stack.copy()
        for list_item in list_stack:
            list_tag = "ol" if list_item[1] else "ul"
            html_parts.append(f"<{list_tag}>")

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
                if list_stack:  # We shouldn't need this!
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

        # Record any remaining open lists.  If this is the last line in a message section
        # then we'll need this list to start the next message section at the correct
        # list indentation level.
        if list_stack:
            self._list_stack = list_stack.copy()

        # Close any remaining open lists
        while list_stack:
            if list_stack[-1][2]:  # in_item
                html_parts.append("</li>")

            list_tag = "ol" if list_stack[-1][1] else "ul"
            html_parts.append(f"</{list_tag}>")
            list_stack.pop()

        return "\n".join(html_parts)

    def _handle_list_line(self, line: str) -> str:
        """
        Handle a line of text containing list items.

        Args:
            lines: Lines of text that include list items

        Returns:
            HTML string representing the list structure
        """
        # Parse each line into a list item structure
        list_items = []
        i = 0
        while i < 1:
            line_type, content = self._identify_line_type(line)

            # Skip past blank lines
            if line_type == 'blank':
                i += 1
                continue

            if line_type in ('ordered_list_item', 'unordered_list_item'):
                indent, marker, text = content

                # Check for continuation lines
                continuation_text = []
                j = i + 1
                while j < 1: # was line lenght
# This logic is wrong - it's only a text continuation if the text lines up with list item text and not the bullet!
                    next_line_type, _ = self._identify_line_type(line)
                    # Include continuation text and blank lines
                    if next_line_type == 'text':
                        continuation_text.append(line)
                        j += 1
                    elif next_line_type == 'blank':
# If we have a blank line followed by more indented text then we've got paragraphs inside the list bullet!
                        # Check if there's a list item after this blank line
                        if j + 1 < 1: # was line legnth
                            after_blank_type, _ = self._identify_line_type(lines[j + 1])
                            if after_blank_type in ('ordered_list_item', 'unordered_list_item'):
                                # This is a blank line between list items
                                break

                        # Add blank line as part of content
                        continuation_text.append(line)
                        j += 1
                    else:
                        break

                # Append continuation lines to text if any
                if continuation_text:
                    text += '\n' + '\n'.join(continuation_text)
                    i = j - 1  # Skip these lines

                # Add list item with its processed content
                list_items.append((line_type, (indent, marker, text)))

                i += 1
                continue

            # Non-empty, non-list line - treat as a paragraph
            formatted_text = self._apply_inline_formatting(self._handle_line_breaks(line))
            print(f"annotate para: {formatted_text}")
            list_items.append(('paragraph', formatted_text))

            i += 1

        # Now process the list items
        return self._process_list_items(list_items)

    def _process_unconverted_lines(self) -> None:
        """Convert any previously unconverted lines to HTML."""
        for i, line in enumerate(self._current_lines):
            if i > self._last_line_converted:
                # Check for heading line (simple case)
                line_type, content = self._identify_line_type(line)
                if line_type == 'heading':
                    level, heading_text = content
                    self._line_html[i] = self._convert_heading(level, heading_text)
                    self._state = ConverterState.NONE
                    self._last_line_converted = i - 1
                elif line_type in ('ordered_list_item', 'unordered_list_item'):
                    self._line_html[i] = self._handle_list_line(line)
                    self._last_line_converted = i - 1

                elif line_type == 'blank':
                    self._last_line_converted = i - 1

                else:
                    # Otherwise, treat as paragraph
                    paragraph_text = self._handle_line_breaks(line)
                    formatted_text = self._apply_inline_formatting(paragraph_text)
                    self._line_html[i] = f"<p>{formatted_text}</p>"
                    self._last_line_converted = i - 1

    def convert_incremental(self, new_text: str) -> str:
        """
        Convert markdown to HTML incrementally as text is being added.

        Args:
            new_text: The updated text to convert

        Returns:
            HTML converted text
        """
        # Split into lines
        new_lines = new_text.split('\n')

        # Handle empty input
        if not new_lines:
            return ""

        # Initialize if this is the first conversion
        if not self._current_lines:
            self._current_lines = new_lines
            self._last_line_converted = -1
            self._line_html = [""] * len(new_lines)
            self._partial_last_line = new_lines[-1]
        else:
            # Find how many lines match from the beginning
            common_prefix_len = 0
            for i, (old, new) in enumerate(zip(self._current_lines, new_lines)):
                if old == new:
                    common_prefix_len = i + 1
                else:
                    break

            # Update our tracking arrays
            if len(new_lines) > len(self._current_lines):
                # New lines added
                self._current_lines = new_lines

                # Keep HTML for existing liness
                self._line_html = (
                    self._line_html +
                    [""] * (len(new_lines) - len(self._line_html))
                )

            elif common_prefix_len < len(new_lines):
                # Update with changed lines
                self._current_lines = new_lines

                for i in range(common_prefix_len, len(new_lines)):
                    self._line_html[i] = ""

            self._partial_last_line = new_lines[-1]

        self._process_unconverted_lines()

        # Combine all processed lines
        return "\n".join(self._line_html)

    def reset(self):
        """Reset the converter state."""
        self._current_lines = []
        self._last_line_converted = -1
        self._line_html = []
        self._state = ConverterState.NONE
        self._partial_last_line = ""
