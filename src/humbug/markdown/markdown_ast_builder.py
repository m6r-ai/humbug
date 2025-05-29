"""
Parser to construct an AST from Markdown.
"""

import logging
import re
from typing import Dict, List, Tuple, Any, cast

from humbug.markdown.markdown_ast_node import (
    MarkdownASTNode, MarkdownDocumentNode, MarkdownTextNode, MarkdownLineBreakNode,
    MarkdownEmphasisNode, MarkdownBoldNode, MarkdownHeadingNode,
    MarkdownParagraphNode, MarkdownOrderedListNode, MarkdownUnorderedListNode,
    MarkdownListItemNode, MarkdownInlineCodeNode, MarkdownCodeBlockNode,
    MarkdownTableNode, MarkdownTableHeaderNode, MarkdownTableBodyNode,
    MarkdownTableRowNode, MarkdownTableCellNode, MarkdownHorizontalRuleNode,
    MarkdownImageNode, MarkdownLinkNode
)
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.programming_language_utils import ProgrammingLanguageUtils


class MarkdownASTBuilderError(Exception):
    """Exception raised for errors during markdown parsing."""


class TableBufferState:
    """Class to track and buffer table elements during parsing."""

    def __init__(self) -> None:
        """Initialize the table buffer state."""
        # Buffer for table rows before commitment
        self.header_rows: List[List[str]] = []
        self.separator_row: List[str] = []
        self.body_rows: List[List[str]] = []

        # Table state
        self.is_in_potential_table: bool = False
        self.is_confirmed_table: bool = False
        self.alignments: List[str] = []

        # Line tracking for AST nodes
        self.start_line: int = -1
        self.current_line: int = -1

    def reset(self) -> None:
        """Reset the table buffer state."""
        self.header_rows = []
        self.separator_row = []
        self.body_rows = []
        self.is_in_potential_table = False
        self.is_confirmed_table = False
        self.alignments = []
        self.start_line = -1
        self.current_line = -1

    def is_valid_table(self) -> bool:
        """
        Check if we have sufficient elements for a valid table.

        Returns:
            True if we have a header row, separator row, and at least one body row
        """
        return (len(self.header_rows) > 0 and
                len(self.separator_row) > 0 and
                len(self.body_rows) > 0)


class ListState:
    """Tracks the state of a list and its items during parsing."""

    def __init__(self, list_node: MarkdownOrderedListNode | MarkdownUnorderedListNode, indent: int) -> None:
        """
        Initialize a list state tracker.

        Args:
            list_node: The list node (MarkdownOrderedListNode or MarkdownUnorderedListNode)
            indent: The indentation level of the list marker
        """
        self.list_node = list_node
        self.indent = indent
        self.marker_length = 0
        self.last_item: MarkdownListItemNode | None = None
        self.contains_blank_line = False


class MarkdownASTBuilder:
    """
    Builder class for constructing an AST from markdown text.

    This class handles the incremental parsing of markdown text into an
    Abstract Syntax Tree (AST) representation.
    """

    def __init__(self, no_underscores: bool):
        """Initialize the AST builder with regex patterns for markdown elements."""
        self._no_underscores = no_underscores
        self._source_path: str | None = None

        # Regular expressions for markdown elements
        self._heading_pattern = re.compile(r'^(#{1,10})\s+(.*?)(?:\s+#{1,10})?$', re.MULTILINE)
        self._unordered_list_pattern = re.compile(r'^(\s*)([*+-])\s+(.*?)$', re.MULTILINE)
        self._ordered_list_pattern = re.compile(r'^(\s*)(\d+)\.[ \t]+(.*?)$', re.MULTILINE)
        self._code_block_pattern = re.compile(r'^```(?:([\w\-#+./*():\s]+))?$')
        self._table_row_pattern = re.compile(r'^(\|.+\|)$', re.MULTILINE)
        self._table_separator_pattern = re.compile(r'^(\|[\s:-]+\|)$', re.MULTILINE)
        self._horizontal_rule_pattern = re.compile(r'^(?:\s*(?:[-*_])\s*){3,}$')

        self._logger = logging.getLogger("MarkdownASTBuilder")

        # Initialize an empty document
        self._document = MarkdownDocumentNode()

        # Mapping from line numbers to nodes for incremental updates
        self._line_to_node_map: Dict[int, List[MarkdownASTNode]] = {}

        # Track header IDs to allow us to create unique link anchors for headings with the same text
        self._used_header_ids: Dict[str, int] = {}

        # List state tracking
        self._list_stack: List[ListState] = []

        # Text continuation tracking
        self._last_paragraph: MarkdownParagraphNode | None = None
        self._last_processed_line_type: str = ""
        self._blank_line_count: int = 0

        # Code block state tracking
        self._in_code_block = False
        self._code_block_language = ""
        self._code_block_content: List[str] = []
        self._code_block_start_line = -1
        self._code_block_nesting_level = 0
        self._code_block_indents: List[int] = []
        self._embedded_parser_state: Any = None
        self._embedded_language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN

        # Table state tracking using the new buffer approach
        self._table_buffer = TableBufferState()

    def document(self) -> MarkdownDocumentNode:
        """
        Get the current document node.

        Returns:
            The document node
        """
        return self._document

    def _should_parse_code_block_as_continuation(
        self,
        language: ProgrammingLanguage,
        line_content: str
    ) -> bool:
        """
        Check if a line should be parsed as a continuation by the embedded parser.

        Args:
            language: The programming language of the code block
            line_content: The content of the line to check

        Returns:
            True if this line should be treated as a continuation
        """
        if language == ProgrammingLanguage.TEXT:
            return False

        # Get or create parser for this language
        parser = ParserRegistry.create_parser(language)
        if not parser:
            return False

        # If language changed, reset state
        if language != self._embedded_language:
            self._embedded_parser_state = None
            self._embedded_language = language

        try:
            # Parse the line and check if we're in a continuation
            new_state = parser.parse(self._embedded_parser_state, line_content)

            # Update stored state
            self._embedded_parser_state = new_state

            # Return whether we're in a continuation
            return new_state is not None and new_state.parsing_continuation

        except Exception as e:
            self._logger.warning(
                "Error checking continuation for %s: %s",
                language.name,
                str(e)
            )
            return False

    def identify_line_type(self, line: str) -> Tuple[str, Any]:
        """
        Identify the type of a markdown line.

        Args:
            line: The line to identify

        Returns:
            A tuple of (line_type, content) where line_type is one of:
            'heading', 'unordered_list_item', 'ordered_list_item', 'blank', 'text',
            'code_block_start', 'code_block_end', 'code_block_content',
            'table_row', 'table_separator', 'line_break', 'horizontal_rule'

        Raises:
            None
        """
        # Handle code block state
        lstripped_line = line.lstrip()
        indent = len(line) - len(lstripped_line)

        if self._in_code_block:
            # Check if our embedded parser indicates this should be a continuation
            if self._code_block_language:
                language = ProgrammingLanguageUtils.from_name(self._code_block_language)
                if self._should_parse_code_block_as_continuation(language, line):
                    return 'code_block_content', line

            # Check for code fence.  If we have one then we're either closing
            # this block or nesting another.
            code_block_match = self._code_block_pattern.match(lstripped_line)
            if code_block_match:
                language = code_block_match.group(1)
                if language is not None or indent > self._code_block_indents[-1]:
                    self._code_block_nesting_level += 1
                    self._code_block_indents.append(indent)
                    return 'code_block_content', line

                self._code_block_indents.pop()
                self._code_block_nesting_level -= 1
                if self._code_block_nesting_level == 0:
                    return 'code_block_end', None

            return 'code_block_content', line

        # Check for code block start
        code_block_match = self._code_block_pattern.match(lstripped_line)
        if code_block_match:
            language = code_block_match.group(1) or ""
            self._code_block_nesting_level = 1
            self._code_block_indents.append(indent)
            return 'code_block_start', language

        # Check for a line break
        if indent >= 2 and not lstripped_line:
            return 'line_break', None

        # Check for blank line
        stripped_line = line.strip()
        if not stripped_line:
            return 'blank', None

        # Check for heading
        heading_match = self._heading_pattern.match(line)
        if heading_match:
            level = len(heading_match.group(1))
            content = heading_match.group(2).strip()
            return 'heading', (level, content)

        # Check for table separator row
        if stripped_line.startswith('|') and stripped_line.endswith('|'):
            # Check if it's a separator row with at least one colon or dash
            if '-' in line and (':-' in line or '-:' in line or '--' in line):
                return 'table_separator', stripped_line

            # Regular table row
            return 'table_row', stripped_line

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

        # Check for horizontal rule
        if self._horizontal_rule_pattern.match(line):
            return 'horizontal_rule', None

        # Default to regular text
        return 'text', line

    def _find_closing_parenthesis(self, text: str, start_pos: int) -> int:
        """
        Find the closing parenthesis that matches an opening one, handling nested parentheses.

        Args:
            text: The text to search in
            start_pos: The position after the opening parenthesis

        Returns:
            The position of the closing parenthesis, or -1 if not found
        """
        paren_count = 1
        pos = start_pos

        while pos < len(text):
            if text[pos] == '(':
                paren_count += 1

            elif text[pos] == ')':
                paren_count -= 1
                if paren_count == 0:
                    return pos
            pos += 1

        return -1

    def _parse_url_and_title(self, url_title: str) -> Tuple[str, str | None]:
        """
        Parse a URL string that may contain a title in quotes.

        Args:
            url_title: The string containing URL and optional title

        Returns:
            A tuple of (url, title) where title may be None
        """
        # Look for title in quotes
        title_match = re.search(r'\s+[\'"](.+?)[\'"]$', url_title)
        if title_match:
            title = title_match.group(1)
            url = url_title[:title_match.start()].strip()
            return url, title

        return url_title.strip(), None

    def parse_inline_formatting(self, text: str) -> List[MarkdownASTNode]:
        """
        Parse inline formatting (bold, italic, inline code) in text and create appropriate AST nodes.

        Args:
            text: The text to parse

        Returns:
            A list of AST nodes representing the formatted text

        Raises:
            None
        """
        # Check if text has trailing line break
        has_line_break = text.endswith('  ')
        if has_line_break:
            text = text[:-2]

        # Simple state machine for inline formatting
        i = 0
        nodes: List[MarkdownASTNode] = []
        current_text = ""

        while i < len(text):
            # Check for image (highest precedence due to the '!' prefix)
            if text[i] == '!' and i + 1 < len(text) and text[i+1] == '[':
                # Look for the closing '](' pattern
                bracket_end = text.find('](', i + 2)
                if bracket_end != -1:
                    # Find closing parenthesis
                    paren_end = self._find_closing_parenthesis(text, bracket_end + 2)
                    if paren_end != -1:
                        # Add any accumulated text before this image
                        if current_text:
                            nodes.append(MarkdownTextNode(current_text))
                            current_text = ""

                        # Extract the alt text and URL
                        alt_text = text[i+2:bracket_end]
                        url_title = text[bracket_end+2:paren_end]
                        url, title = self._parse_url_and_title(url_title)

                        # Create image node
                        image_node = MarkdownImageNode(url, alt_text, title)
                        nodes.append(image_node)

                        # Move past the closing parenthesis
                        i = paren_end + 1
                        continue

            # Check for link
            elif text[i] == '[':
                # Look for the closing '](' pattern
                bracket_end = text.find('](', i + 1)
                if bracket_end != -1:
                    # Find closing parenthesis
                    paren_end = self._find_closing_parenthesis(text, bracket_end + 2)
                    if paren_end != -1:
                        # Add any accumulated text before this link
                        if current_text:
                            nodes.append(MarkdownTextNode(current_text))
                            current_text = ""

                        # Extract the link text and URL
                        link_text = text[i+1:bracket_end]
                        url_title = text[bracket_end+2:paren_end]
                        url, title = self._parse_url_and_title(url_title)

                        # Create link node
                        link_node = MarkdownLinkNode(url, title)

                        # Process the content inside the link text recursively
                        for child_node in self.parse_inline_formatting(link_text):
                            link_node.add_child(child_node)

                        nodes.append(link_node)

                        # Move past the closing parenthesis
                        i = paren_end + 1
                        continue

            # Check for inline code (high precedence)
            elif text[i] == '`':
                # Look for the closing backtick
                end_pos = text.find('`', i + 1)
                if end_pos != -1:
                    # Add any accumulated text before this code block
                    if current_text:
                        nodes.append(MarkdownTextNode(current_text))
                        current_text = ""

                    # Extract the code content (excluding backticks)
                    code_content = text[i+1:end_pos]
                    nodes.append(MarkdownInlineCodeNode(code_content))

                    # Move past the closing backtick
                    i = end_pos + 1
                    continue

            # Check for bold formatting
            elif (i + 1 < len(text) and
                    ((text[i:i+2] == '**') or (text[i:i+2] == '__' and not self._no_underscores))):
                # Determine which marker we're using
                marker = text[i:i+2]

                # Look for the closing marker
                end_pos = text.find(marker, i + 2)
                if end_pos != -1:
                    # Add any accumulated text before this bold block
                    if current_text:
                        nodes.append(MarkdownTextNode(current_text))
                        current_text = ""

                    # Extract the bold content (excluding markers)
                    bold_content = text[i+2:end_pos]

                    # Create bold node and process its content recursively
                    bold_node = MarkdownBoldNode()

                    # Process the content inside the bold
                    for child_node in self.parse_inline_formatting(bold_content):
                        bold_node.add_child(child_node)

                    nodes.append(bold_node)

                    # Move past the closing marker
                    i = end_pos + 2
                    continue

            # Check for italic formatting
            elif (text[i] == '*' or (text[i] == '_' and not self._no_underscores)) and (
                    i == 0 or text[i-1] != text[i]):  # Avoid mistaking ** as *

                # Determine which marker we're using
                marker = text[i]

                # Look for the closing marker
                end_pos = text.find(marker, i + 1)
                if end_pos != -1 and (end_pos + 1 >= len(text) or text[end_pos+1] != marker):  # Avoid **
                    # Add any accumulated text before this italic block
                    if current_text:
                        nodes.append(MarkdownTextNode(current_text))
                        current_text = ""

                    # Extract the italic content (excluding markers)
                    italic_content = text[i+1:end_pos]

                    # Create emphasis node and process its content recursively
                    emphasis_node = MarkdownEmphasisNode()

                    # Process the content inside the emphasis
                    for child_node in self.parse_inline_formatting(italic_content):
                        emphasis_node.add_child(child_node)

                    nodes.append(emphasis_node)

                    # Move past the closing marker
                    i = end_pos + 1
                    continue

            # No formatting found, accumulate normal text
            current_text += text[i]
            i += 1

        # Add any remaining accumulated text
        if current_text:
            nodes.append(MarkdownTextNode(current_text))

        # Append a line break node if needed
        if has_line_break:
            nodes.append(MarkdownLineBreakNode())

        return nodes

    def _create_id_from_text(self, text: str) -> str:
        """
        Create a simplified ID from text suitable for HTML anchor links.

        Args:
            text: The text to convert to an ID

        Returns:
            A simplified string suitable for use as an element ID
        """
        # Convert to lowercase
        text = text.lower()

        # Replace spaces with hyphens
        text = text.replace(' ', '-')

        # Remove special characters
        text = re.sub(r'[^a-z0-9-]', '', text)

        # Ensure it doesn't start with a number
        if text and text[0].isdigit():
            text = 'h-' + text

        return text

    def parse_heading(self, level: int, content: str, line_num: int) -> None:
        """
        Parse a heading line and create a heading node.

        Args:
            level: The heading level (1-6)
            content: The heading content
            line_num: The line number
        """
        anchor_id = self._create_id_from_text(content)

        # Check if this ID already exists.  If it doesn't, then record we've seen it.   If it does,
        # then add a suffix to it to make it unique.
        if anchor_id not in self._used_header_ids:
            self._used_header_ids[anchor_id] = 0

        else:
            # This ID already exists, increment the counter and append it
            self._used_header_ids[anchor_id] += 1
            anchor_id = f"{anchor_id}-{self._used_header_ids[anchor_id]}"

        heading = MarkdownHeadingNode(level, anchor_id)
        for node in self.parse_inline_formatting(content):
            heading.add_child(node)

        heading.line_start = line_num
        heading.line_end = line_num
        self.register_node_line(heading, line_num)

        self._document.add_child(heading)

    def parse_text(self, text: str, line_num: int) -> MarkdownParagraphNode:
        """
        Parse a text line and create a paragraph node.

        Args:
            text: The text content
            line_num: The line number
        """
        paragraph = MarkdownParagraphNode()
        for node in self.parse_inline_formatting(text):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        self.register_node_line(paragraph, line_num)

        self._document.add_child(paragraph)
        return paragraph

    def _current_list_state(self) -> ListState | None:
        """
        Return the current list state or None if not in a list.

        Returns:
            The current list state or None
        """
        return self._list_stack[-1] if self._list_stack else None

    def _parent_list_state(self) -> ListState | None:
        """
        Return the parent list state or None if there's no parent.

        Returns:
            The parent list state or None
        """
        return self._list_stack[-2] if len(self._list_stack) > 1 else None

    def _close_lists_at_indent(self, indent: int, consider_marker: bool) -> bool:
        """
        Close all lists deeper than the given indent level.

        Args:
            indent: The indentation level
            consider_marker: Whether to consider the marker length for closing lists

        Returns:
            True if lists were closed, False otherwise
        """
        if not self._list_stack:
            return False

        closed_lists = False

        while self._list_stack:
            marker_adjust = self._list_stack[-1].marker_length if consider_marker else 0
            if (self._list_stack[-1].indent + marker_adjust) <= indent:
                break

            self._list_stack.pop()
            closed_lists = True

        return closed_lists

    def _find_parent_for_list(self) -> MarkdownASTNode:
        """
        Find the appropriate parent for a new list.

        Returns:
            The parent node
        """
        if not self._list_stack:
            return self._document

        # Find the last list item of the deepest list to be the parent
        current_list = self._list_stack[-1]
        if current_list.last_item:
            return current_list.last_item

        # If there's no list item yet, go up the stack
        for i in range(len(self._list_stack) - 2, -1, -1):
            list_state = self._list_stack[i]
            if list_state.last_item:
                return list_state.last_item

        # If no list items found, use the document
        return self._document

    def find_or_create_ordered_list(self, indent: int, start_number: int) -> MarkdownOrderedListNode:
        """
        Find or create an ordered list at the given indent level with specified start number.

        Args:
            indent: The indentation level
            start_number: The starting number

        Returns:
            The created ordered list node
        """
        # Check if we already have an ordered list at this level
        if self._list_stack and self._list_stack[-1].indent == indent:
            list_state = self._list_stack[-1]
            if isinstance(list_state.list_node, MarkdownOrderedListNode):
                return list_state.list_node

            # Different list type, close it
            self._list_stack.pop()

        # Find parent
        parent = self._find_parent_for_list()

        # Create new ordered list
        new_list = MarkdownOrderedListNode(indent, start_number)
        parent.add_child(new_list)

        # Create and add list state
        list_state = ListState(new_list, indent)
        self._list_stack.append(list_state)

        return new_list

    def find_or_create_unordered_list(self, indent: int) -> MarkdownUnorderedListNode:
        """
        Find or create an unordered list at the given indent level.

        Args:
            indent: The indentation level

        Returns:
            The created unordered list node
        """
        # Check if we already have an unordered list at this level
        if self._list_stack and self._list_stack[-1].indent == indent:
            list_state = self._list_stack[-1]
            if isinstance(list_state.list_node, MarkdownUnorderedListNode):
                return list_state.list_node

            # Different list type, close it
            self._list_stack.pop()

        # Find parent
        parent = self._find_parent_for_list()

        # Create new unordered list
        new_list = MarkdownUnorderedListNode(indent)
        parent.add_child(new_list)

        # Create and add list state
        list_state = ListState(new_list, indent)
        self._list_stack.append(list_state)

        return new_list

    def parse_ordered_list_item(self, indent: int, number: str, content: str, line_num: int) -> None:
        """
        Parse an ordered list item and create a list item node.

        Args:
            indent: The indentation level
            number: The list item number (as a string)
            content: The item content
            line_num: The line number
        """
        # Extract the starting number
        try:
            start_number = int(number)

        except ValueError:
            start_number = 1

        # Close deeper lists
        self._close_lists_at_indent(indent, False)

        # If we're in a list, and we've seen a blank line, then mark the list as loose
        if self._list_stack and self._blank_line_count > 0:
            self._list_stack[-1].contains_blank_line = True
            self._list_stack[-1].list_node.tight = False

        list_node = self.find_or_create_ordered_list(indent, start_number)
        current_list = cast(ListState, self._current_list_state())

        # Create the list item
        item = MarkdownListItemNode()
        list_node.add_child(item)

        # Calculate the actual content indentation for this specific marker
        current_list.marker_length = len(number) + 2  # +2 for the "." and space after number

        # Create a paragraph for the content
        paragraph = MarkdownParagraphNode()
        for node in self.parse_inline_formatting(content):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        item.add_child(paragraph)
        self.register_node_line(paragraph, line_num)

        item.line_start = line_num
        item.line_end = line_num
        self.register_node_line(item, line_num)

        # Update tracking variables
        current_list.last_item = item
        self._last_processed_line_type = 'ordered_list_item'

    def parse_unordered_list_item(self, indent: int, marker: str, content: str, line_num: int) -> None:
        """
        Parse an unordered list item and create a list item node.

        Args:
            indent: The indentation level
            marker: The bullet marker (-, *, +)
            content: The item content
            line_num: The line number
        """
        # Close deeper lists
        self._close_lists_at_indent(indent, False)

        # If we're in a list, and we've seen a blank line, then mark the list as loose
        if self._list_stack and self._blank_line_count > 0:
            self._list_stack[-1].contains_blank_line = True
            self._list_stack[-1].list_node.tight = False

        list_node = self.find_or_create_unordered_list(indent)
        current_list = cast(ListState, self._current_list_state())

        # Create the list item
        item = MarkdownListItemNode()
        list_node.add_child(item)

        # Calculate the actual content indentation for this specific marker
        current_list.marker_length = len(marker) + 1  # +1 for the space after marker

        # Create a paragraph for the content
        paragraph = MarkdownParagraphNode()
        for node in self.parse_inline_formatting(content):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        item.add_child(paragraph)
        self.register_node_line(paragraph, line_num)

        item.line_start = line_num
        item.line_end = line_num
        self.register_node_line(item, line_num)

        # Update tracking variables
        current_list.last_item = item
        self._last_processed_line_type = 'unordered_list_item'

    def parse_horizontal_rule(self, line_num: int) -> MarkdownHorizontalRuleNode:
        """
        Parse a horizontal rule line and create a horizontal rule node.

        Args:
            line_num: The line number

        Returns:
            A horizontal rule node
        """
        horizontal_rule = MarkdownHorizontalRuleNode()
        horizontal_rule.line_start = line_num
        horizontal_rule.line_end = line_num
        self.register_node_line(horizontal_rule, line_num)

        return horizontal_rule

    def _parse_table_separator(self, separator_line: str) -> List[str]:
        """
        Parse a table separator line to determine column alignments.

        Args:
            separator_line: A line containing the table column separators (e.g., |---|:---:|--:|)

        Returns:
            List of alignment strings ('left', 'center', 'right') for each column
        """
        cells = [cell.strip() for cell in separator_line.split('|')[1:-1]]
        alignments = []

        for cell in cells:
            if cell.startswith(':') and cell.endswith(':'):
                alignments.append('center')

            elif cell.endswith(':'):
                alignments.append('right')

            else:
                alignments.append('left')

        return alignments

    def _handle_table_row(self, line: str, line_num: int) -> None:
        """
        Handle a table row line in the parsing process.

        Args:
            line: The table row line
            line_num: The line number

        Returns:
            None
        """
        # If this is the first potential table row
        if not self._table_buffer.is_in_potential_table:
            self._table_buffer.is_in_potential_table = True
            self._table_buffer.start_line = line_num

            # Store this as a header row
            cells = [cell.strip() for cell in line.split('|')[1:-1]]
            self._table_buffer.header_rows.append(cells)

        elif self._table_buffer.is_confirmed_table:
            # This is a body row
            cells = [cell.strip() for cell in line.split('|')[1:-1]]
            self._table_buffer.body_rows.append(cells)

        else:
            # This is another potential header row
            cells = [cell.strip() for cell in line.split('|')[1:-1]]
            self._table_buffer.header_rows.append(cells)

        self._table_buffer.current_line = line_num

    def _handle_table_separator(self, line: str, line_num: int) -> None:
        """
        Handle a table separator line in the parsing process.

        Args:
            line: The table separator line
            line_num: The line number

        Returns:
            None
        """
        # If we're not already in a potential table, ignore it
        if not self._table_buffer.is_in_potential_table:
            # Treat as normal text
            self.parse_text(line, line_num)
            return

        # Store the separator row
        cells = [cell.strip() for cell in line.split('|')[1:-1]]
        self._table_buffer.separator_row = cells

        # Parse alignments
        self._table_buffer.alignments = self._parse_table_separator(line)
        self._table_buffer.current_line = line_num

        # Mark as a confirmed table now that we have a separator
        self._table_buffer.is_confirmed_table = True

    def _create_table_from_buffer(self) -> None:
        """
        Create a table node from the buffered content.

        Returns:
            None
        """
        # Create the table structure
        table_node = MarkdownTableNode()
        header_node = MarkdownTableHeaderNode()
        body_node = MarkdownTableBodyNode()

        # Set line information
        table_node.line_start = self._table_buffer.start_line
        table_node.line_end = self._table_buffer.current_line
        header_node.line_start = self._table_buffer.start_line

        # Add header and body to table
        table_node.add_child(header_node)
        table_node.add_child(body_node)

        # Process header rows
        for i, row_cells in enumerate(self._table_buffer.header_rows):
            row_node = MarkdownTableRowNode()
            row_line = self._table_buffer.start_line + i
            row_node.line_start = row_line
            row_node.line_end = row_line

            for j, cell_content in enumerate(row_cells):
                # Determine alignment
                alignment = 'left'
                if j < len(self._table_buffer.alignments):
                    alignment = self._table_buffer.alignments[j]

                # Create cell
                cell_node = MarkdownTableCellNode(is_header=True, alignment=alignment)

                # Add content to cell
                for text_node in self.parse_inline_formatting(cell_content):
                    cell_node.add_child(text_node)

                row_node.add_child(cell_node)

            header_node.add_child(row_node)
            self.register_node_line(row_node, row_line)

        # Set header end line
        header_node.line_end = self._table_buffer.start_line + len(self._table_buffer.header_rows) - 1

        # Process body rows
        body_start_line = self._table_buffer.start_line + len(self._table_buffer.header_rows) + 1
        body_node.line_start = body_start_line

        for i, row_cells in enumerate(self._table_buffer.body_rows):
            row_node = MarkdownTableRowNode()
            row_line = body_start_line + i
            row_node.line_start = row_line
            row_node.line_end = row_line

            for j, cell_content in enumerate(row_cells):
                # Determine alignment
                alignment = 'left'
                if j < len(self._table_buffer.alignments):
                    alignment = self._table_buffer.alignments[j]

                # Create cell
                cell_node = MarkdownTableCellNode(is_header=False, alignment=alignment)

                # Add content to cell
                for text_node in self.parse_inline_formatting(cell_content):
                    cell_node.add_child(text_node)

                row_node.add_child(cell_node)

            body_node.add_child(row_node)
            self.register_node_line(row_node, row_line)

        body_node.line_end = self._table_buffer.current_line

        # Add the table to the document
        self._document.add_child(table_node)

        # Register table with line mappings
        for i in range(self._table_buffer.start_line, self._table_buffer.current_line + 1):
            self.register_node_line(table_node, i)

        # Reset table buffer
        self._table_buffer.reset()

    def _handle_incomplete_table(self) -> None:
        """
        Handle buffered table content that doesn't form a complete table.
        Convert it to regular paragraphs.

        Returns:
            None
        """
        # Render header rows as regular text
        for i, row in enumerate(self._table_buffer.header_rows):
            line_num = self._table_buffer.start_line + i
            # Convert back to text line
            line_text = '|' + '|'.join(row) + '|'
            self.parse_text(line_text, line_num)

        # Handle separator if present
        if self._table_buffer.separator_row:
            line_num = self._table_buffer.start_line + len(self._table_buffer.header_rows)
            # Reconstruct separator line
            parts = []
            for alignment in self._table_buffer.alignments:
                if alignment == 'center':
                    parts.append(':---:')

                elif alignment == 'right':
                    parts.append('---:')

                else:
                    parts.append('---')

            line_text = '|' + '|'.join(parts) + '|'
            self.parse_text(line_text, line_num)

        # Handle any body rows
        body_start = self._table_buffer.start_line + len(self._table_buffer.header_rows)
        if self._table_buffer.separator_row:
            body_start += 1

        for i, row in enumerate(self._table_buffer.body_rows):
            line_num = body_start + i
            # Convert back to text line
            line_text = '|' + '|'.join(row) + '|'
            self.parse_text(line_text, line_num)

        # Reset table buffer
        self._table_buffer.reset()

    def register_node_line(self, node: MarkdownASTNode, line_num: int) -> None:
        """
        Register a node with a line number for later reference.

        Args:
            node: The AST node
            line_num: The line number

        Returns:
            None

        Raises:
            None
        """
        if line_num not in self._line_to_node_map:
            self._line_to_node_map[line_num] = []

        self._line_to_node_map[line_num].append(node)

    def _finalize_code_block(self, end_line: int) -> None:
        """
        Finalize a code block that might be unclosed at document end.

        Args:
            end_line: The last line number in the document

        Returns:
            None
        """
        # Create a code block node for the unclosed block
        code_block = MarkdownCodeBlockNode(
            language=self._code_block_language,
            content='\n'.join(self._code_block_content)
        )
        code_block.line_start = self._code_block_start_line
        code_block.line_end = end_line

        # Add to document
        self._document.add_child(code_block)

        # Register code block with all lines it spans
        for i in range(self._code_block_start_line, end_line + 1):
            self.register_node_line(code_block, i)

        # Reset code block state
        self._in_code_block = False
        self._code_block_language = ""
        self._code_block_content = []
        self._code_block_start_line = -1
        self._code_block_nesting_level = 0
        self._code_block_indents = []
        self._embedded_parser_state = None
        self._embedded_language = ProgrammingLanguage.UNKNOWN

    def _handle_text_continuation(self, text: str, line_num: int) -> bool:
        """
        Handle text as a continuation of the previous paragraph or list item.
        A text line is a continuation of a list item if it is indented
        by at least the same amount as the list item's content indent.

        Args:
            text: The text content
            line_num: The line number

        Returns:
            True if handled as a continuation, False otherwise
        """
        # Case 1: Continue a paragraph
        if self._last_paragraph and self._last_processed_line_type == 'text':
            # Add a space between the continued text as long as we didn't just have a line break
            if not isinstance(self._last_paragraph.children[-1], MarkdownLineBreakNode):
                self._last_paragraph.add_child(MarkdownTextNode(" "))

            for node in self.parse_inline_formatting(text):
                self._last_paragraph.add_child(node)

            self._last_paragraph.line_end = line_num
            self.register_node_line(self._last_paragraph, line_num)
            return True

        # Case 2: Continue a list item paragraph
        if self._list_stack and self._last_processed_line_type in ('unordered_list_item', 'ordered_list_item', 'blank', 'text'):
            # Get the indentation of the current line
            current_indent = len(text) - len(text.lstrip())

            if self._close_lists_at_indent(current_indent, True):
                # We closed one or more lists, we should treat it as if a blank line was encountered
                # This will ensure proper formatting when returning to an outer list
                if self._list_stack:
                    self._last_processed_line_type = 'blank'
                    self._blank_line_count = 1

                # Reset the paragraph continuity as well
                self._last_paragraph = None

            # If we're continuing a list item and we've seen a blank line, mark the list as loose
            if self._list_stack and self._blank_line_count > 0:
                self._list_stack[-1].contains_blank_line = True
                self._list_stack[-1].list_node.tight = False

            list_state = self._current_list_state()
            if not list_state or not list_state.last_item:
                return False

            formatted_text = text.lstrip()

            # Find the paragraph within the last list item and continue it
            last_item = list_state.last_item
            paragraph = cast(MarkdownParagraphNode, last_item.children[-1])

            # If we weren't just preceded by a line break then add a space
            if paragraph.children and not isinstance(paragraph.children[-1], MarkdownLineBreakNode):
                paragraph.add_child(MarkdownTextNode(" "))

            for node in self.parse_inline_formatting(formatted_text):
                paragraph.add_child(node)

            paragraph.line_end = line_num
            self.register_node_line(paragraph, line_num)

            last_item.line_end = line_num
            self.register_node_line(last_item, line_num)
            return True

        return False

    def _reset_list_state(self) -> None:
        """Reset all list tracking state."""
        self._list_stack = []

    def parse_line(self, line: str, line_num: int) -> None:
        """
        Parse a single line and add the resulting nodes to the AST.

        Args:
            line: The line to parse
            line_num: The line number

        Returns:
            None

        Raises:
            MarkdownASTBuilderError: If there's an error parsing the line
        """
        try:
            line_type, content = self.identify_line_type(line)

            # Reset paragraph tracking if not continuing text
            if line_type not in ('text', 'blank', 'line_break'):
                self._last_paragraph = None

            # Handle table ends when a non-table line is encountered
            if self._table_buffer.is_in_potential_table and line_type not in ('table_row', 'table_separator'):
                # Check if we have a complete table to create
                if self._table_buffer.is_valid_table():
                    self._create_table_from_buffer()

                else:
                    # Not a valid table, render as regular text
                    self._handle_incomplete_table()

            if line_type == 'line_break':
                self._document.add_child(MarkdownLineBreakNode())
                return

            if line_type == 'blank':
                self._blank_line_count += 1
                self._last_processed_line_type = line_type
                return

            if line_type == 'unordered_list_item':
                indent, marker, text = content
                self.parse_unordered_list_item(indent, marker, text, line_num)
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            if line_type == 'ordered_list_item':
                indent, number, text = content
                self.parse_ordered_list_item(indent, number, text, line_num)
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            if line_type == 'code_block_start':
                self._in_code_block = True
                self._code_block_language = content
                self._code_block_content = []
                self._code_block_start_line = line_num
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            if line_type == 'code_block_content':
                self._code_block_content.append(content)
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            if line_type == 'code_block_end':
                self._finalize_code_block(line_num)
                self._reset_list_state()
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            # Handle table-related lines
            if line_type == 'table_row':
                self._handle_table_row(content, line_num)
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            if line_type == 'table_separator':
                self._handle_table_separator(content, line_num)
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            # Process other line types
            if line_type == 'heading':
                level, heading_text = content
                self.parse_heading(level, heading_text, line_num)
                self._reset_list_state()
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            if line_type == 'horizontal_rule':
                horizontal_rule = self.parse_horizontal_rule(line_num)
                self._document.add_child(horizontal_rule)
                self._reset_list_state()
                self._last_processed_line_type = line_type
                self._blank_line_count = 0
                return

            # We have text left
            # Try to handle as a continuation first
            if self._handle_text_continuation(content, line_num):
                self._last_processed_line_type = line_type
                return

            # Regular paragraph
            paragraph = self.parse_text(content, line_num)
            self._last_paragraph = paragraph
            self._reset_list_state()
            self._last_processed_line_type = line_type
            self._blank_line_count = 0

        except Exception as e:
            self._logger.exception("Error parsing line %d: %s", line_num, line)
            raise MarkdownASTBuilderError(f"Failed to parse line {line_num}: {e}") from e

    def build_ast(self, text: str) -> MarkdownDocumentNode:
        """
        Build a complete AST from the given text.

        Args:
            text: The markdown text to parse

        Returns:
            The document root node

        Raises:
            MarkdownASTBuilderError: If there's an error parsing the text
        """
        self._document = MarkdownDocumentNode(self._source_path)
        self._line_to_node_map = {}
        self._reset_list_state()
        self._last_paragraph = None
        self._last_processed_line_type = ""
        self._blank_line_count = 0
        self._in_code_block = False
        self._code_block_language = ""
        self._code_block_content = []
        self._code_block_start_line = -1
        self._code_block_nesting_level = 0
        self._code_block_indents = []

        # Reset table buffer
        self._table_buffer.reset()

        # Parse line by line
        lines = text.split('\n')
        for i, line in enumerate(lines):
            self.parse_line(line, i)

        # Check for any buffered table content at the end of the document
        if self._table_buffer.is_in_potential_table:
            if self._table_buffer.is_valid_table():
                self._create_table_from_buffer()

            else:
                self._handle_incomplete_table()

        # Handle case where document ends while still in a code block
        if self._in_code_block:
            self._finalize_code_block(len(lines) - 1)

        return self._document

    def update_ast(self, text: str, previous_text: str, path: str | None = None) -> MarkdownDocumentNode:
        """
        Update the AST incrementally based on changes between previous_text and text.

        Args:
            text: The new markdown text
            previous_text: The previous markdown text, or None if this is the first update
            path: Optional path to the source markdown file

        Returns:
            The updated document root node

        Raises:
            MarkdownASTBuilderError: If there's an error updating the AST
        """
        self._source_path = path
        if previous_text is None or not self._document.children:
            # First update or empty document, build from scratch
            return self.build_ast(text)

        # Split into lines
        new_lines = text.split('\n')
        old_lines = previous_text.split('\n')

        # Find common prefix length (unchanged lines)
        common_prefix_len = 0
        for i, (old, new) in enumerate(zip(old_lines, new_lines)):
            if old == new:
                common_prefix_len = i + 1

            else:
                break

        # Find common suffix length (unchanged lines from the end)
        common_suffix_len = 0
        for i, (old, new) in enumerate(zip(reversed(old_lines), reversed(new_lines))):
            if old == new and common_prefix_len + common_suffix_len < min(len(old_lines), len(new_lines)):
                common_suffix_len = i + 1

            else:
                break

        # Calculate changed region
        start = common_prefix_len
        old_end = len(old_lines) - common_suffix_len
        new_end = len(new_lines) - common_suffix_len

        # If nothing changed, return existing document
        if start >= old_end and start >= new_end:
            return self._document

        # For now, just rebuild the entire AST
        # In the future we might implement incremental updates
        return self.build_ast(text)
