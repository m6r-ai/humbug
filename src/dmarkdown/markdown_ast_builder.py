"""
Parser to construct an AST from Markdown.
"""

import logging
import re
from typing import Dict, List, Tuple, Any, cast

from syntax import ParserRegistry, ProgrammingLanguage, ProgrammingLanguageUtils, Token

from dmarkdown.markdown_ast_node import (
    MarkdownASTNode, MarkdownASTDocumentNode, MarkdownASTTextNode, MarkdownASTLineBreakNode,
    MarkdownASTEmphasisNode, MarkdownASTBoldNode, MarkdownASTHeadingNode,
    MarkdownASTParagraphNode, MarkdownASTListNode, MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode,
    MarkdownASTListItemNode, MarkdownASTInlineCodeNode, MarkdownASTCodeBlockNode,
    MarkdownASTTableNode, MarkdownASTTableHeaderNode, MarkdownASTTableBodyNode,
    MarkdownASTTableRowNode, MarkdownASTTableCellNode, MarkdownASTHorizontalRuleNode,
    MarkdownASTImageNode, MarkdownASTLinkNode, MarkdownASTBlockquoteNode
)


class TableBufferState:
    """Class to track and buffer table elements during parsing."""

    def __init__(self) -> None:
        """Initialize the table buffer state."""
        # Buffer for table rows before commitment
        self.header_row: List[str] = []
        self.header_line: str = ""
        self.separator_row: List[str] = []
        self.separator_line: str = ""
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
        self.header_row = []
        self.header_line = ""
        self.separator_row = []
        self.separator_line = ""
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
        return (len(self.header_row) > 0 and
                len(self.separator_row) > 0 and
                len(self.body_rows) > 0)



class ContainerContext:
    """
    Represents a container that can hold block elements.

    This tracks the current nesting context during parsing, allowing proper
    handling of block elements within lists, blockquotes, and other containers.
    """

    def __init__(
        self,
        node: MarkdownASTNode,
        indent_level: int,
        container_type: str,
        can_contain_blocks: bool = True,
        lazy_continuation: bool = False,
        marker_length: int = 0,
        is_tight_list: bool = True
    ) -> None:
        """
        Initialize a container context.

        Args:
            node: The AST node representing this container
            indent_level: Required indentation level for content in this container
            container_type: Type identifier ('document', 'list_item', 'blockquote', etc.)
            can_contain_blocks: Whether this container can hold block-level elements
            lazy_continuation: Whether lazy continuation is allowed (for lists/blockquotes)
            marker_length: Length of list marker (for list items)
            is_tight_list: Whether the list is tight (for list containers)
        """
        self.node = node
        self.indent_level = indent_level
        self.container_type = container_type
        self.can_contain_blocks = can_contain_blocks
        self.lazy_continuation = lazy_continuation
        self.marker_length = marker_length
        self.is_tight_list = is_tight_list


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
        self._heading_pattern = re.compile(r'^(\s{0,3})(#{1,10})\s+(.*?)(?:\s+#{1,10})?$', re.MULTILINE)
        self._unordered_list_pattern = re.compile(r'^(\s*)([*+-])\s+(.*?)$', re.MULTILINE)
        self._ordered_list_pattern = re.compile(r'^(\s*)(\d+)\.[ \t]+(.*?)$', re.MULTILINE)
        self._code_block_pattern = re.compile(r'^```(?:([\w\-#+./*():\s]+))?$')
        self._table_row_pattern = re.compile(r'^(\|.+\|)$', re.MULTILINE)
        self._table_separator_pattern = re.compile(r'^(\|[\s:-]+\|)$', re.MULTILINE)
        self._horizontal_rule_pattern = re.compile(r'^(?:\s*(?:[-*_])\s*){3,}$')

        self._logger = logging.getLogger("MarkdownASTBuilder")

        # Initialize an empty document
        self._document = MarkdownASTDocumentNode()

        # Mapping from line numbers to nodes for incremental updates
        self._line_to_node_map: Dict[int, List[MarkdownASTNode]] = {}

        # Track header IDs to allow us to create unique link anchors for headings with the same text
        self._used_header_ids: Dict[str, int] = {}

        # Container stack for tracking nesting context
        # Initialize with document as root container
        self._container_stack: List[ContainerContext] = []

        # Text continuation tracking
        self._last_paragraph: MarkdownASTParagraphNode | None = None
        self._last_processed_line_type: str = ""
        self._blank_line_count: int = 0

        # Code block state tracking
        self._in_code_block = False
        self._code_block_language_name = ""
        self._code_block_content: List[str] = []
        self._code_block_start_line = -1
        self._code_block_nesting_level = 0
        self._code_block_indents: List[int] = []
        self._embedded_parser_state: Any = None
        self._embedded_language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN
        self._tokens_by_line: List[List[Token]] = []
        self._states_by_line: List[Any] = []

        # Table state tracking using the new buffer approach
        self._table_buffer = TableBufferState()

        # Track where code blocks should be added
        self._code_block_start_container: MarkdownASTNode | None = None

        # Track current line number for blockquote handling
        self._current_line_num: int = 0

    def document(self) -> MarkdownASTDocumentNode:
        """
        Get the current document node.

        Returns:
            The document node
        """
        return self._document

    def _current_container(self) -> MarkdownASTNode:
        """
        Get the current container for adding block elements.

        Returns:
            The AST node that should receive new block elements
        """
        assert self._container_stack, "Container stack should never be empty"
        return self._container_stack[-1].node

    def _initialize_container_stack(self) -> None:
        """Initialize the container stack with the document as root."""
        self._container_stack = [
            ContainerContext(
                node=self._document,
                indent_level=0,
                container_type='document',
                can_contain_blocks=True,
                lazy_continuation=False
            )
        ]

    def _reset_container_stack(self) -> None:
        """Reset the container stack to just the document root."""
        assert self._container_stack, "Container stack should be initialized before reset"
        self._container_stack = [self._container_stack[0]]

    def _parse_code_line(
        self,
        line_content: str
    ) -> bool:
        """
        Parse the next line of code in a code block.  This saves the parse result, and returns a status
        indicating whether the line ends with a continuation to another line.

        Args:
            language: The programming language of the code block
            line_content: The content of the line to check

        Returns:
            True if this line should be treated as a continuation
        """
        parser = ParserRegistry.create_parser(self._embedded_language)
        assert parser is not None, f"No parser registered for language: {self._embedded_language.name}"

        # Parse the line and check if we're in a continuation
        new_state = parser.parse(self._embedded_parser_state, line_content)

        # Extract tokens from the parser and cache them
        line_tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break

            line_tokens.append(token)

        # Store results
        self._tokens_by_line.append(line_tokens)
        self._states_by_line.append(new_state)

        # Update stored state
        self._embedded_parser_state = new_state

        # Return whether we're in a continuation
        return new_state is not None and new_state.parsing_continuation

    def _identify_line_type(self, line: str) -> Tuple[str, List[int], Dict[str, Any]]:
        """
        Identify the type of a markdown line (stateless).

        For lines with blockquote markers (>), this strips all markers and returns
        the innermost content type along with the blockquote nesting information.

        Args:
            line: The line to identify

        Returns:
            A tuple of (line_type, blockquote_indents, line_data) where:
            - line_type is one of:
            'heading', 'unordered_list_item', 'ordered_list_item', 'blank', 'text',
            'code_block_fence',
            'table_row', 'table_separator', 'horizontal_rule'
            - blockquote_indents is a list of column positions where > markers were found
            - line_data is a dict with line-type-specific data
        """
        # Strip all blockquote markers (>) and track their positions
        blockquote_indents = []
        remaining = line
        current_col = 0

        while True:
            stripped = remaining.lstrip()
            spaces_before = len(remaining) - len(stripped)

            if stripped.startswith('>'):
                # Record the column position of this >
                blockquote_indents.append(current_col + spaces_before)

                # Strip the > and optional following space
                remaining = stripped[1:]
                if remaining.startswith(' '):
                    remaining = remaining[1:]
                    current_col += spaces_before + 2  # "> "

                else:
                    current_col += spaces_before + 1  # ">"

            else:
                break

        # Now remaining contains the actual content after all > markers
        # Calculate the final indent of the content
        content_stripped = remaining.lstrip()
        content_indent = current_col + (len(remaining) - len(content_stripped))

        # Check for blank line (after stripping blockquotes)
        if not content_stripped:
            return 'blank', blockquote_indents, {}

        # Check for code block fence (after stripping blockquotes)
        code_block_match = self._code_block_pattern.match(content_stripped)
        if code_block_match:
            language_name = code_block_match.group(1) or ""
            return 'code_block_fence', blockquote_indents, {
                'language': language_name,
                'indent': content_indent,
                'raw': remaining
            }

        # Check for heading
        heading_match = self._heading_pattern.match(remaining)
        if heading_match:
            level = len(heading_match.group(2))
            heading_content = heading_match.group(3).strip()
            return 'heading', blockquote_indents, {
                'level': level,
                'text': heading_content,
                'raw': remaining
            }

        # Check for table separator row
        if content_stripped.startswith('|') and content_stripped.endswith('|'):
            # Check if it's a separator row with at least one colon or dash
            if '-' in remaining and (':-' in remaining or '-:' in remaining or '--' in remaining):
                return 'table_separator', blockquote_indents, {
                    'content': content_stripped,
                    'raw': remaining
                }

            # Regular table row
            return 'table_row', blockquote_indents, {
                'content': content_stripped,
                'raw': remaining
            }

        # Check for unordered list item
        unordered_match = self._unordered_list_pattern.match(remaining)
        if unordered_match:
            list_indent = len(unordered_match.group(1))
            marker = unordered_match.group(2)
            list_content = unordered_match.group(3)
            return 'unordered_list_item', blockquote_indents, {
                'indent': current_col + list_indent,
                'marker': marker,
                'text': list_content,
                'raw': remaining
            }

        # Check for ordered list item
        ordered_match = self._ordered_list_pattern.match(remaining)
        if ordered_match:
            list_indent = len(ordered_match.group(1))
            number = ordered_match.group(2)
            list_content = ordered_match.group(3)
            return 'ordered_list_item', blockquote_indents, {
                'indent': current_col + list_indent,
                'number': number,
                'text': list_content,
                'raw': remaining
            }

        # Check for horizontal rule
        if self._horizontal_rule_pattern.match(remaining):
            return 'horizontal_rule', blockquote_indents, {'raw': remaining}

        # Default to regular text
        return 'text', blockquote_indents, {
            'indent': content_indent,
            'text': remaining,
            'raw': remaining
        }

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

    def _parse_inline_formatting(self, text: str) -> List[MarkdownASTNode]:
        """
        Parse inline formatting (bold, italic, inline code) in text and create appropriate AST nodes.

        Args:
            text: The text to parse

        Returns:
            A list of AST nodes representing the formatted text
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
                            nodes.append(MarkdownASTTextNode(current_text))
                            current_text = ""

                        # Extract the alt text and URL
                        alt_text = text[i+2:bracket_end]
                        url_title = text[bracket_end+2:paren_end]
                        url, title = self._parse_url_and_title(url_title)

                        # Create image node
                        image_node = MarkdownASTImageNode(url, alt_text, title)
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
                            nodes.append(MarkdownASTTextNode(current_text))
                            current_text = ""

                        # Extract the link text and URL
                        link_text = text[i+1:bracket_end]
                        url_title = text[bracket_end+2:paren_end]
                        url, title = self._parse_url_and_title(url_title)

                        # Create link node
                        link_node = MarkdownASTLinkNode(url, title)

                        # Process the content inside the link text recursively
                        for child_node in self._parse_inline_formatting(link_text):
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
                        nodes.append(MarkdownASTTextNode(current_text))
                        current_text = ""

                    # Extract the code content (excluding backticks)
                    code_content = text[i+1:end_pos]
                    nodes.append(MarkdownASTInlineCodeNode(code_content))

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
                        nodes.append(MarkdownASTTextNode(current_text))
                        current_text = ""

                    # Extract the bold content (excluding markers)
                    bold_content = text[i+2:end_pos]

                    # Create bold node and process its content recursively
                    bold_node = MarkdownASTBoldNode()

                    # Process the content inside the bold
                    for child_node in self._parse_inline_formatting(bold_content):
                        bold_node.add_child(child_node)

                    nodes.append(bold_node)

                    # Move past the closing marker
                    i = end_pos + 2
                    continue

            # Check for italic formatting
            elif (text[i] == '*' or (text[i] == '_' and not self._no_underscores)) and (
                    i == 0 or text[i-1] != text[i]):
                # Determine which marker we're using
                marker = text[i]

                # Look for the closing marker
                end_pos = text.find(marker, i + 1)
                if end_pos != -1 and (end_pos + 1 >= len(text) or text[end_pos+1] != marker):  # Avoid **
                    # Add any accumulated text before this italic block
                    if current_text:
                        nodes.append(MarkdownASTTextNode(current_text))
                        current_text = ""

                    # Extract the italic content (excluding markers)
                    italic_content = text[i+1:end_pos]

                    # Create emphasis node and process its content recursively
                    emphasis_node = MarkdownASTEmphasisNode()

                    # Process the content inside the emphasis
                    for child_node in self._parse_inline_formatting(italic_content):
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
            nodes.append(MarkdownASTTextNode(current_text))

        # Append a line break node if needed
        if has_line_break:
            nodes.append(MarkdownASTLineBreakNode())

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

    def _register_node_line(self, node: MarkdownASTNode, line_num: int) -> None:
        """
        Register a node with a line number for later reference.

        Args:
            node: The AST node
            line_num: The line number

        Returns:
            None
        """
        if line_num not in self._line_to_node_map:
            self._line_to_node_map[line_num] = []

        self._line_to_node_map[line_num].append(node)

    def _parse_heading(self, level: int, content: str, line_num: int) -> None:
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

        heading = MarkdownASTHeadingNode(level, anchor_id)
        for node in self._parse_inline_formatting(content):
            heading.add_child(node)

        heading.line_start = line_num
        heading.line_end = line_num
        self._register_node_line(heading, line_num)

        # Add heading to current container
        self._current_container().add_child(heading)

    def _parse_text(self, text: str, line_num: int) -> MarkdownASTParagraphNode:
        """
        Parse a text line and create a paragraph node.

        Args:
            text: The text content
            line_num: The line number
        """
        paragraph = MarkdownASTParagraphNode()
        for node in self._parse_inline_formatting(text):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        self._register_node_line(paragraph, line_num)

        # Add paragraph to current container
        self._current_container().add_child(paragraph)
        return paragraph

    def _find_or_create_ordered_list(self, indent: int, start_number: int) -> ContainerContext:
        """
        Find or create an ordered list at the given indent level with specified start number.

        Args:
            indent: The indentation level
            start_number: The starting number

        Returns:
            The ContainerContext for the list
        """
        # Search the container stack for an ordered list at this indent level
        for context in reversed(self._container_stack):
            if isinstance(context.node, MarkdownASTListNode) and context.indent_level == indent:
                # Found a list at this level
                if context.container_type == 'ordered_list':
                    # Same type, reuse it
                    return context

                # Different type - we need to close it and create new
                self._container_stack.pop()
                break

        # No suitable list found, create a new one
        # The parent is the current container after adjustment
        parent = self._current_container()

        # Create new ordered list
        new_list = MarkdownASTOrderedListNode(indent, start_number)
        parent.add_child(new_list)

        # Push list as a container
        # List containers don't require additional indentation themselves
        # (list items will have their own indentation requirements)
        list_context = ContainerContext(
            node=new_list,
            indent_level=indent,
            container_type='ordered_list',
            can_contain_blocks=True,
            lazy_continuation=False,
            is_tight_list=True  # Start as tight, will be set to False if we see blank lines
        )
        self._container_stack.append(list_context)

        return list_context

    def _parse_ordered_list_item(self, indent: int, number: str, content: str, line_num: int) -> None:
        """
        Parse an ordered list item and create a list item node.

        Args:
            indent: The indentation level
            number: The list item number (as a string)
            content: The item content
            line_num: The line number
        """
        # Extract the starting number
        start_number = int(number)

        # Calculate marker length
        marker_length = len(number) + 2  # +2 for the "." and space after number

        # Find or create the list at this indent level
        list_context = self._find_or_create_ordered_list(indent, start_number)
        list_node = cast(MarkdownASTOrderedListNode, list_context.node)

        # If we've seen a blank line, mark the list as loose
        # (but only if the list already has items - blank lines before first item don't count)
        if self._blank_line_count > 0 and len(list_node.children) > 0:
            list_context.is_tight_list = False
            list_node.tight = False

        # Create the list item
        item = MarkdownASTListItemNode()
        list_node.add_child(item)

        # Create a paragraph for the content
        paragraph = MarkdownASTParagraphNode()
        for node in self._parse_inline_formatting(content):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        item.add_child(paragraph)
        self._register_node_line(paragraph, line_num)

        item.line_start = line_num
        item.line_end = line_num
        self._register_node_line(item, line_num)

        # Update tracking
        self._last_processed_line_type = 'ordered_list_item'

        # Push list item as a container so subsequent indented blocks go inside it
        content_indent = indent + marker_length
        context = ContainerContext(
            node=item,
            indent_level=content_indent,
            container_type='list_item',
            can_contain_blocks=True,
            lazy_continuation=True,  # List items allow lazy continuation
            marker_length=marker_length
        )
        self._container_stack.append(context)

    def _find_or_create_unordered_list(self, indent: int) -> ContainerContext:
        """
        Find or create an unordered list at the given indent level.

        Args:
            indent: The indentation level

        Returns:
            The ContainerContext for the list
        """
        # Search the container stack for an unordered list at this indent level
        for context in reversed(self._container_stack):
            if isinstance(context.node, MarkdownASTListNode) and context.indent_level == indent:
                # Found a list at this level
                if context.container_type == 'unordered_list':
                    # Same type, reuse it
                    return context

                # Different type - we need to close it and create new
                self._container_stack.pop()
                break

        # No suitable list found, create a new one
        # The parent is the current container after adjustment
        parent = self._current_container()

        # Create new unordered list
        new_list = MarkdownASTUnorderedListNode(indent)
        parent.add_child(new_list)

        # Push list as a container
        # List containers don't require additional indentation themselves
        # (list items will have their own indentation requirements)
        list_context = ContainerContext(
            node=new_list,
            indent_level=indent,
            container_type='unordered_list',
            can_contain_blocks=True,
            lazy_continuation=False,
            is_tight_list=True  # Start as tight, will be set to False if we see blank lines
        )
        self._container_stack.append(list_context)

        return list_context

    def _parse_unordered_list_item(self, indent: int, marker: str, content: str, line_num: int) -> None:
        """
        Parse an unordered list item and create a list item node.

        Args:
            indent: The indentation level
            marker: The bullet marker (-, *, +)
            content: The item content
            line_num: The line number
        """
        # Calculate marker length
        marker_length = len(marker) + 1  # +1 for the space after marker

        # Find or create the list at this indent level
        list_context = self._find_or_create_unordered_list(indent)
        list_node = cast(MarkdownASTUnorderedListNode, list_context.node)

        # If we've seen a blank line, mark the list as loose
        # (but only if the list already has items - blank lines before first item don't count)
        if self._blank_line_count > 0 and len(list_node.children) > 0:
            list_context.is_tight_list = False
            list_node.tight = False

        # Create the list item
        item = MarkdownASTListItemNode()
        list_node.add_child(item)

        # Create a paragraph for the content
        paragraph = MarkdownASTParagraphNode()
        for node in self._parse_inline_formatting(content):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        item.add_child(paragraph)
        self._register_node_line(paragraph, line_num)

        item.line_start = line_num
        item.line_end = line_num
        self._register_node_line(item, line_num)

        # Update tracking
        self._last_processed_line_type = 'unordered_list_item'

        # Push list item as a container so subsequent indented blocks go inside it
        content_indent = indent + marker_length
        context = ContainerContext(
            node=item,
            indent_level=content_indent,
            container_type='list_item',
            can_contain_blocks=True,
            lazy_continuation=True,  # List items allow lazy continuation
            marker_length=marker_length
        )
        self._container_stack.append(context)

    def _parse_horizontal_rule(self, line_num: int) -> MarkdownASTHorizontalRuleNode:
        """
        Parse a horizontal rule line and create a horizontal rule node.

        Args:
            line_num: The line number

        Returns:
            A horizontal rule node
        """
        horizontal_rule = MarkdownASTHorizontalRuleNode()
        horizontal_rule.line_start = line_num
        horizontal_rule.line_end = line_num
        self._register_node_line(horizontal_rule, line_num)

        return horizontal_rule

    def _enter_blockquote(self, indent: int, line_num: int) -> None:
        """
        Enter a blockquote context.

        Args:
            indent: The indentation level of the '>' marker
            line_num: The line number
        """
        blockquote = MarkdownASTBlockquoteNode()

        blockquote.line_start = line_num
        blockquote.line_end = None  # Will be set when we exit

        # Add to current container
        self._current_container().add_child(blockquote)

        # Clear last paragraph so text in the new blockquote starts fresh
        self._last_paragraph = None

        # Push blockquote onto container stack
        context = ContainerContext(
            node=blockquote,
            indent_level=indent,
            container_type='blockquote',
            can_contain_blocks=True,
            lazy_continuation=True  # Blockquotes allow lazy continuation
        )
        self._container_stack.append(context)

        # Track for line mapping
        self._register_node_line(blockquote, line_num)

    def _exit_blockquote(self, line_num: int) -> None:
        """
        Exit the current blockquote context.

        Args:
            line_num: The line number where the blockquote ends
        """
        blockquote = self._container_stack[-1].node
        blockquote.line_end = line_num

        # Clear the last paragraph so that text after exiting the blockquote
        # starts a new paragraph instead of continuing the previous one
        self._last_paragraph = None

        self._container_stack.pop()

    def _is_in_blockquote(self) -> bool:
        """
        Check if we're currently inside a blockquote.

        Returns:
            True if inside a blockquote
        """
        assert self._container_stack, "Container stack should never be empty"
        return self._container_stack[-1].container_type == 'blockquote'

    def _adjust_blockquote_contexts(self, blockquote_indents: List[int], line_num: int) -> None:
        """
        Adjust the container stack to match the required blockquote nesting.

        This ensures we're in the right blockquote contexts based on the > markers
        found in the current line.

        Args:
            blockquote_indents: List of column positions where > markers were found
            line_num: The current line number
        """
        # Collect current blockquote contexts from the stack
        current_blockquotes = []
        for ctx in self._container_stack:
            if ctx.container_type == 'blockquote':
                current_blockquotes.append(ctx)

        # Iterate backwards through the blockquote contexts and exit blockquotes that are no longer present or
        # at wrong indent levels.  Work backwards to maintain stack integrity
        for i in range(len(current_blockquotes) - 1, -1, -1):
            if i >= len(blockquote_indents) or current_blockquotes[i].indent_level != blockquote_indents[i]:
                # Need to exit this blockquote and all nested ones
                while True:
                    if self._container_stack[-1] is current_blockquotes[i]:
                        self._exit_blockquote(line_num - 1)
                        break

                    # Pop non-blockquote containers above this blockquote (e.g., list_item, list)
                    self._container_stack.pop()

        # Enter new blockquotes that we need
        for i in range(len(current_blockquotes), len(blockquote_indents)):
            self._enter_blockquote(blockquote_indents[i], line_num)

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
        # Is this a body row?
        if self._table_buffer.is_confirmed_table:
            cells = [cell.strip() for cell in line.split('|')[1:-1]]
            self._table_buffer.body_rows.append(cells)
            self._table_buffer.current_line = line_num
            return

        # Is this another header row?  If yes, we can't have two, so emit the last one as text
        if self._table_buffer.is_in_potential_table:
            self._parse_text(self._table_buffer.header_line, self._table_buffer.start_line)

        else:
            self._table_buffer.is_in_potential_table = True
            self._table_buffer.start_line = line_num

        cells = [cell.strip() for cell in line.split('|')[1:-1]]
        self._table_buffer.header_row = cells
        self._table_buffer.header_line = line
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
            self._parse_text(line, line_num)
            return

        # Store the separator row
        cells = [cell.strip() for cell in line.split('|')[1:-1]]
        self._table_buffer.separator_row = cells
        self._table_buffer.separator_line = line

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
        # Calculate the maximum number of columns across all rows
        max_columns = len(self._table_buffer.header_row)
        max_body_columns = max(len(row) for row in self._table_buffer.body_rows)
        max_columns = max(max_columns, max_body_columns)

        # Normalize header row to have max_columns
        while len(self._table_buffer.header_row) < max_columns:
            self._table_buffer.header_row.append("")

        # Normalize alignments to have max_columns (default to "left" for additional columns)
        while len(self._table_buffer.alignments) < max_columns:
            self._table_buffer.alignments.append("left")

        # Normalize all body rows to have max_columns
        for row in self._table_buffer.body_rows:
            while len(row) < max_columns:
                row.append("")

        # Create the table structure
        table_node = MarkdownASTTableNode()
        header_node = MarkdownASTTableHeaderNode()
        body_node = MarkdownASTTableBodyNode()

        # Set line information
        table_node.line_start = self._table_buffer.start_line
        table_node.line_end = self._table_buffer.current_line
        header_node.line_start = self._table_buffer.start_line

        # Add header and body to table
        table_node.add_child(header_node)
        table_node.add_child(body_node)

        # Process header rows
        row_node = MarkdownASTTableRowNode()
        row_line = self._table_buffer.start_line
        row_node.line_start = row_line
        row_node.line_end = row_line

        for j, cell_content in enumerate(self._table_buffer.header_row):
            # Determine alignment
            alignment = self._table_buffer.alignments[j]

            # Create cell
            cell_node = MarkdownASTTableCellNode(is_header=True, alignment=alignment)

            # Add content to cell
            for text_node in self._parse_inline_formatting(cell_content):
                cell_node.add_child(text_node)

            row_node.add_child(cell_node)

        header_node.add_child(row_node)
        self._register_node_line(row_node, row_line)

        # Set header end line
        header_node.line_end = self._table_buffer.start_line

        # Process body rows
        body_start_line = self._table_buffer.start_line + 1
        body_node.line_start = body_start_line

        for i, row_cells in enumerate(self._table_buffer.body_rows):
            row_node = MarkdownASTTableRowNode()
            row_line = body_start_line + i
            row_node.line_start = row_line
            row_node.line_end = row_line

            for j, cell_content in enumerate(row_cells):
                # Determine alignment
                alignment = self._table_buffer.alignments[j]

                # Create cell
                cell_node = MarkdownASTTableCellNode(is_header=False, alignment=alignment)

                # Add content to cell
                for text_node in self._parse_inline_formatting(cell_content):
                    cell_node.add_child(text_node)

                row_node.add_child(cell_node)

            body_node.add_child(row_node)
            self._register_node_line(row_node, row_line)

        body_node.line_end = self._table_buffer.current_line

        # Add the table to the current container
        self._current_container().add_child(table_node)

        # Register table with line mappings
        for i in range(self._table_buffer.start_line, self._table_buffer.current_line + 1):
            self._register_node_line(table_node, i)

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
        self._parse_text(self._table_buffer.header_line, self._table_buffer.start_line)

        # Handle separator if present
        if self._table_buffer.separator_row:
            self._parse_text(self._table_buffer.separator_line, self._table_buffer.start_line + 1)

        # Reset table buffer
        self._table_buffer.reset()

    def _finalize_code_block(self, end_line: int) -> None:
        """
        Finalize a code block that might be unclosed at document end.

        Args:
            end_line: The last line number in the document

        Returns:
            None
        """
        # Create a code block node for the unclosed block
        code_block = MarkdownASTCodeBlockNode(
            language_name=self._code_block_language_name,
            content='\n'.join(self._code_block_content),
            tokens_by_line=self._tokens_by_line,
            states_by_line=self._states_by_line,
            language=self._embedded_language,
            total_lines=len(self._code_block_content)
        )
        code_block.line_start = self._code_block_start_line
        code_block.line_end = end_line

        # Add to the container where the code block started
        assert self._code_block_start_container, "Code block should have a start container"
        self._code_block_start_container.add_child(code_block)

        # Register code block with all lines it spans
        for i in range(self._code_block_start_line, end_line + 1):
            self._register_node_line(code_block, i)

        # Reset code block state
        self._in_code_block = False
        self._code_block_language_name = ""
        self._code_block_content = []
        self._code_block_start_line = -1
        self._code_block_nesting_level = 0
        self._code_block_indents = []
        self._embedded_parser_state = None
        self._embedded_language = ProgrammingLanguage.UNKNOWN
        self._tokens_by_line = []
        self._states_by_line = []

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
            if not isinstance(self._last_paragraph.children[-1], MarkdownASTLineBreakNode):
                self._last_paragraph.add_child(MarkdownASTTextNode(" "))

            for node in self._parse_inline_formatting(text):
                self._last_paragraph.add_child(node)

            self._last_paragraph.line_end = line_num
            self._register_node_line(self._last_paragraph, line_num)
            return True

        # Case 2: Continue a list item paragraph
        # Check if we're in a list item container
        if not self._container_stack or self._last_processed_line_type not in (
            'unordered_list_item', 'ordered_list_item', 'blank', 'text'
        ):
            return False

        # Find the current list item container
        list_item_context = None
        list_item_index = None
        for i, context in enumerate(reversed(self._container_stack)):
            if context.container_type == 'list_item':
                list_item_context = context
                list_item_index = len(self._container_stack) - 1 - i
                break

        if not list_item_context:
            return False

        # Check if text is unindented (at column 0)
        current_indent = len(text) - len(text.lstrip())

        # If we have unindented text after a blank line, don't continue the list
        # This closes the list and allows the text to become a standalone paragraph
        if current_indent == 0 and self._blank_line_count > 0:
            return False

        # The list container is the parent of the list_item in the stack
        # (immediately before the list_item when going forward through the stack)
        assert list_item_index is not None and list_item_index > 0, "List item must have a parent list"
        list_context = self._container_stack[list_item_index - 1]
        assert isinstance(list_context.node, MarkdownASTListNode), \
            f"Parent of list_item must be a list, got {type(list_context.node).__name__}"

        # If we've seen a blank line, mark the list as loose
        if self._blank_line_count > 0:
            list_context.is_tight_list = False
            list_context.node.tight = False

        formatted_text = text.lstrip()
        last_item = cast(MarkdownASTListItemNode, list_item_context.node)

        # Determine if we need a new paragraph
        needs_new_paragraph = False

        # Need new paragraph if we had a blank line before this
        if self._blank_line_count > 0:
            needs_new_paragraph = True

        # Need new paragraph if last child is not a paragraph
        if last_item.children and not isinstance(last_item.children[-1], MarkdownASTParagraphNode):
            needs_new_paragraph = True

        assert last_item.children, "List items should always have at least one paragraph"
        if needs_new_paragraph:
            # Create a new paragraph in this list item
            paragraph = MarkdownASTParagraphNode()
            for node in self._parse_inline_formatting(formatted_text):
                paragraph.add_child(node)

            paragraph.line_start = line_num
            paragraph.line_end = line_num
            last_item.add_child(paragraph)
            self._register_node_line(paragraph, line_num)

        else:
            # Continue the existing paragraph
            paragraph = cast(MarkdownASTParagraphNode, last_item.children[-1])

            # If we weren't just preceded by a line break then add a space
            if paragraph.children and not isinstance(paragraph.children[-1], MarkdownASTLineBreakNode):
                paragraph.add_child(MarkdownASTTextNode(" "))

            for node in self._parse_inline_formatting(formatted_text):
                paragraph.add_child(node)

            paragraph.line_end = line_num
            self._register_node_line(paragraph, line_num)

        last_item.line_end = line_num
        self._register_node_line(last_item, line_num)
        return True

    def _adjust_containers_for_line(
        self,
        line_type: str,
        blockquote_indents: List[int],
        effective_indent: int,
        line_num: int
    ) -> None:
        """
        Adjust the container stack for the current line.

        This handles:
        1. Blockquote context adjustments based on > markers
        2. Exiting blockquotes when they end
        3. Closing containers based on indentation
        4. Special handling for headings (which close most containers)
        5. Closing lists after blank lines

        Args:
            line_type: The type of line being processed
            blockquote_indents: List of column positions where > markers were found
            effective_indent: The effective indentation level for this line
            line_num: The line number
        """
        # 1. Adjust blockquote contexts based on > markers in this line
        if blockquote_indents:
            self._adjust_blockquote_contexts(blockquote_indents, line_num)

        # 2. For non-blockquote lines, we might need to exit blockquotes
        # (unless it's a blank line which allows lazy continuation)
        elif self._is_in_blockquote() and line_type != 'blank':
            # Exit all blockquotes
            while self._is_in_blockquote():
                self._exit_blockquote(line_num - 1)

        # 3. Headings close all containers except document and blockquotes
        if line_type == 'heading':
            if not self._is_in_blockquote():
                self._reset_container_stack()
            return

        # 4. Close containers that require more indentation than we have
        while len(self._container_stack) > 1:
            current_context = self._container_stack[-1]
            required_indent = current_context.indent_level

            # If we have enough indentation, keep this container
            if effective_indent >= required_indent:
                break

            # Check if lazy continuation is allowed
            if current_context.lazy_continuation:
                # For lazy continuation, we allow less indentation for text
                # BUT only if there wasn't a blank line before (which breaks continuation)
                if line_type == 'text' and self._blank_line_count == 0:
                    # Allow lazy continuation for text without preceding blank
                    break

                if line_type == 'blank':
                    # Blank lines don't close containers
                    break

            # Close this container
            self._container_stack.pop()

        # 5. After a blank line, close list containers for block-level elements
        if (self._blank_line_count > 0 and line_type not in ('blank', 'unordered_list_item', 'ordered_list_item')):
            # Close list containers at or above the element's indentation
            while True:
                top = self._container_stack[-1]

                # Break if we hit a non-list container (e.g., list_item, document)
                if not isinstance(top.node, MarkdownASTListNode):
                    break

                # Close this list
                self._container_stack.pop()

    def _parse_line(self, line: str, line_num: int) -> None:
        """
        Parse a single line and add the resulting nodes to the AST.

        Args:
            line: The line to parse
            line_num: The line number

        Returns:
            None
        """
        # Store current line number for blockquote tracking
        self._current_line_num = line_num

        line_type, blockquote_indents, line_data = self._identify_line_type(line)

        # Calculate indentation for container management (from original line)
        stripped = line.lstrip()
        indent = len(line) - len(stripped)

        # Handle code block state (stateful logic)
        if self._in_code_block:
            # We're in a code block - check if this line ends it or is content
            if line_type == 'code_block_fence':
                # We saw a fence - need to decide if it's closing or nested
                language_name = line_data.get('language', '')
                code_indent = line_data.get('indent', indent)

                # Check if embedded parser says we're in a string/comment
                if (self._embedded_parser_state and self._embedded_parser_state.parsing_continuation):
                    # ``` is inside a string/comment, treat as content
                    # Extract the text after stripping blockquotes
                    text_content = line_data.get('raw', line)
                    self._code_block_content.append(text_content)
                    self._parse_code_line(text_content)

                    self._last_processed_line_type = 'code_block_content'
                    self._blank_line_count = 0
                    return

                # Check if this is a nested fence (has language or more indented)
                if language_name or code_indent > self._code_block_indents[-1]:
                    # Nested fence
                    self._code_block_nesting_level += 1
                    self._code_block_indents.append(code_indent)
                    text_content = line_data.get('raw', line)
                    self._code_block_content.append(text_content)
                    self._last_processed_line_type = 'code_block_content'
                    self._blank_line_count = 0
                    return

                # Check if this closes the outermost fence
                if self._code_block_nesting_level == 1:
                    # Close the code block
                    self._finalize_code_block(line_num)
                    self._last_processed_line_type = 'code_block_end'
                    self._blank_line_count = 0
                    return

                # Close a nested fence
                self._code_block_indents.pop()
                self._code_block_nesting_level -= 1
                text_content = line_data.get('raw', line)
                self._code_block_content.append(text_content)
                self._last_processed_line_type = 'code_block_content'
                self._blank_line_count = 0
                return

            # Not a fence - accumulate as content
            text_content = line_data.get('raw', line)

            self._code_block_content.append(text_content)
            self._parse_code_line(text_content)

            self._last_processed_line_type = 'code_block_content'
            self._blank_line_count = 0
            return

        # Use the indent from line_data if available (which accounts for blockquote stripping),
        # otherwise fall back to the indent calculated from the original line
        effective_indent = line_data.get('indent', indent)

        # Adjust containers based on line type, blockquotes, and indentation
        self._adjust_containers_for_line(line_type, blockquote_indents, effective_indent, line_num)

        # Handle table ends when a non-table line is encountered
        if self._table_buffer.is_in_potential_table and line_type not in ('table_row', 'table_separator'):
            # Check if we have a complete table to create
            if self._table_buffer.is_valid_table():
                self._create_table_from_buffer()

            else:
                # Not a valid table, render as regular text
                self._handle_incomplete_table()

        if line_type == 'blank':
            self._blank_line_count += 1
            self._last_paragraph = None
            self._last_processed_line_type = line_type
            return

        if line_type == 'unordered_list_item':
            self._parse_unordered_list_item(line_data['indent'], line_data['marker'], line_data['text'], line_num)
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            self._last_paragraph = None
            return

        if line_type == 'ordered_list_item':
            self._parse_ordered_list_item(line_data['indent'], line_data['number'], line_data['text'], line_num)
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            self._last_paragraph = None
            return

        if line_type == 'code_block_fence':
            # Starting a new code block
            self._in_code_block = True
            self._code_block_start_container = self._current_container()
            self._code_block_language_name = line_data['language']
            self._embedded_language = ProgrammingLanguageUtils.from_name(line_data['language'])
            self._code_block_content = []
            self._code_block_start_line = line_num

            # Initialize nesting tracking
            self._code_block_nesting_level = 1
            self._code_block_indents = [line_data['indent']]
            self._embedded_parser_state = None  # Will be initialized on first line
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            self._last_paragraph = None
            return

        # Handle table-related lines
        if line_type == 'table_row':
            self._handle_table_row(line_data['content'], line_num)
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            self._last_paragraph = None
            return

        if line_type == 'table_separator':
            self._handle_table_separator(line_data['content'], line_num)
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            self._last_paragraph = None
            return

        # Process other line types
        if line_type == 'heading':
            self._parse_heading(line_data['level'], line_data['text'], line_num)
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            self._last_paragraph = None
            return

        if line_type == 'horizontal_rule':
            horizontal_rule = self._parse_horizontal_rule(line_num)
            self._current_container().add_child(horizontal_rule)
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            self._last_paragraph = None
            return

        # We have text left
        text_content = line_data['text']

        # Try to handle as a continuation first
        if self._handle_text_continuation(text_content, line_num):
            self._last_processed_line_type = line_type
            self._blank_line_count = 0
            return

        # Regular paragraph
        paragraph = self._parse_text(text_content, line_num)
        self._last_paragraph = paragraph
        self._last_processed_line_type = line_type
        self._blank_line_count = 0

    def build_ast(self, text: str) -> MarkdownASTDocumentNode:
        """
        Build a complete AST from the given text.

        Args:
            text: The markdown text to parse

        Returns:
            The document root node
        """
        self._document = MarkdownASTDocumentNode(self._source_path)
        self._line_to_node_map = {}
        self._initialize_container_stack()
        self._last_paragraph = None
        self._last_processed_line_type = ""
        self._blank_line_count = 0
        self._in_code_block = False
        self._code_block_language_name = ""
        self._code_block_content = []
        self._code_block_start_line = -1
        self._code_block_nesting_level = 0
        self._code_block_indents = []

        # Reset table buffer
        self._table_buffer.reset()

        # Parse line by line
        lines = text.split('\n')
        for i, line in enumerate(lines):
            self._parse_line(line, i)

        # Check for any buffered table content at the end of the document
        if self._table_buffer.is_in_potential_table:
            if self._table_buffer.is_valid_table():
                self._create_table_from_buffer()

            else:
                self._handle_incomplete_table()

        # Handle case where document ends while still in a code block
        if self._in_code_block:
            self._finalize_code_block(len(lines) - 1)

        # Close any open blockquotes at the end of the document
        while self._is_in_blockquote():
            self._exit_blockquote(len(lines) - 1)

        return self._document

    def update_ast(self, text: str, previous_text: str, path: str | None = None) -> MarkdownASTDocumentNode:
        """
        Update the AST incrementally based on changes between previous_text and text.

        Args:
            text: The new markdown text
            previous_text: The previous markdown text, or None if this is the first update
            path: Optional path to the source markdown file

        Returns:
            The updated document root node
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
