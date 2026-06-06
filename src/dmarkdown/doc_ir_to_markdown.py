from typing import List, Optional, Sequence

from doc_ir import (
    DocIRBlockquoteNode,
    DocIRCodeBlockNode,
    DocIRDocumentNode,
    DocIRHeadingNode,
    DocIRHorizontalRuleNode,
    DocIRImageNode,
    DocIRLineBreakNode,
    DocIRLinkNode,
    DocIRListItemNode,
    DocIRNode,
    DocIROrderedListNode,
    DocIRParagraphNode,
    DocIRTableBodyNode,
    DocIRTableCellNode,
    DocIRTableHeaderNode,
    DocIRTableNode,
    DocIRTableRowNode,
    DocIRTextSpanNode,
    DocIRUnorderedListNode,
)


def doc_ir_to_markdown(document: DocIRDocumentNode) -> str:
    """Convert a doc_ir document tree to a Markdown string.

    Args:
        document: The root DocIRDocumentNode to serialise.

    Returns:
        A Markdown string representing the document content.
    """
    serialiser = _DocIRToMarkdownSerialiser()
    return serialiser.serialise(document)


class _DocIRToMarkdownSerialiser:
    """Walks a doc_ir tree and emits Markdown text."""

    def serialise(self, document: DocIRDocumentNode) -> str:
        """Serialise a document to a Markdown string.

        Args:
            document: The root document node.

        Returns:
            The Markdown text.
        """
        blocks: List[str] = []
        for child in document.children:
            block = self._serialise_block(child, depth=0, ordered_index=None)
            if block is not None:
                blocks.append(block)

        return "\n\n".join(blocks) + "\n" if blocks else ""

    def _serialise_block(
        self,
        node: DocIRNode,
        depth: int,
        ordered_index: Optional[int],  # pylint: disable=unused-argument
    ) -> Optional[str]:
        """Serialise a single block node to a Markdown string.

        Args:
            node: The block node to serialise.
            depth: Current list nesting depth (0 = top level).
            ordered_index: If inside an ordered list item, the item number;
                otherwise None.

        Returns:
            The Markdown string for this block, or None if nothing to emit.
        """
        if isinstance(node, DocIRHeadingNode):
            return self._serialise_heading(node)

        if isinstance(node, DocIRParagraphNode):
            return self._serialise_paragraph(node)

        if isinstance(node, DocIRBlockquoteNode):
            return self._serialise_blockquote(node)

        if isinstance(node, DocIRCodeBlockNode):
            return self._serialise_code_block(node)

        if isinstance(node, DocIRUnorderedListNode):
            return self._serialise_unordered_list(node, depth=depth)

        if isinstance(node, DocIROrderedListNode):
            return self._serialise_ordered_list(node, depth=depth)

        if isinstance(node, DocIRTableNode):
            return self._serialise_table(node)

        if isinstance(node, DocIRHorizontalRuleNode):
            return "---"

        return None

    def _serialise_heading(self, node: DocIRHeadingNode) -> str:
        """Serialise a heading node."""
        prefix = "#" * node.level
        text = self._serialise_inline_children(node.children)
        return f"{prefix} {text}"

    def _serialise_paragraph(self, node: DocIRParagraphNode) -> Optional[str]:
        """Serialise a paragraph node."""
        text = self._serialise_inline_children(node.children)
        return text if text else None

    def _serialise_blockquote(self, node: DocIRBlockquoteNode) -> Optional[str]:
        """Serialise a blockquote by prefixing each block line with '> '."""
        inner_blocks: List[str] = []
        for child in node.children:
            block = self._serialise_block(child, depth=0, ordered_index=None)
            if block is not None:
                inner_blocks.append(block)

        if not inner_blocks:
            return None

        inner = "\n\n".join(inner_blocks)
        prefixed = "\n".join(f"> {line}" if line else ">" for line in inner.splitlines())
        return prefixed

    def _serialise_code_block(self, node: DocIRCodeBlockNode) -> str:
        """Serialise a fenced code block."""
        lang = node.language or ""
        return f"```{lang}\n{node.content}\n```"

    def _serialise_unordered_list(
        self, node: DocIRUnorderedListNode, depth: int
    ) -> Optional[str]:
        """Serialise an unordered list."""
        lines: List[str] = []
        for child in node.children:
            if isinstance(child, DocIRListItemNode):
                item_lines = self._serialise_list_item(child, depth=depth, ordered_index=None)
                lines.extend(item_lines)

        return "\n".join(lines) if lines else None

    def _serialise_ordered_list(
        self, node: DocIROrderedListNode, depth: int
    ) -> Optional[str]:
        """Serialise an ordered list."""
        lines: List[str] = []
        index = node.start
        for child in node.children:
            if isinstance(child, DocIRListItemNode):
                item_lines = self._serialise_list_item(child, depth=depth, ordered_index=index)
                lines.extend(item_lines)
                index += 1

        return "\n".join(lines) if lines else None

    def _serialise_list_item(
        self,
        item: DocIRListItemNode,
        depth: int,
        ordered_index: Optional[int],
    ) -> List[str]:
        """Serialise a list item, returning a list of output lines.

        The first paragraph of the item is emitted on the marker line.
        Continuation paragraphs and nested lists are indented to align
        with the text start.

        Args:
            item: The list item node.
            depth: Current nesting depth (0 = top level).
            ordered_index: Item number for ordered lists, None for unordered.

        Returns:
            A list of strings, one per output line.
        """
        indent = "    " * depth
        if ordered_index is not None:
            marker = f"{ordered_index}."
        else:
            marker = "-"

        # Width of "marker + space" for continuation indent alignment
        continuation_indent = indent + " " * (len(marker) + 1)

        lines: List[str] = []
        is_first_para = True

        for child in item.children:
            if isinstance(child, DocIRParagraphNode):
                text = self._serialise_inline_children(child.children)
                if is_first_para:
                    lines.append(f"{indent}{marker} {text}")
                    is_first_para = False
                else:
                    lines.append(f"{continuation_indent}{text}")

            elif isinstance(child, DocIRUnorderedListNode):
                is_first_para = False
                nested = self._serialise_unordered_list(child, depth=depth + 1)
                if nested:
                    lines.extend(nested.splitlines())

            elif isinstance(child, DocIROrderedListNode):
                is_first_para = False
                nested = self._serialise_ordered_list(child, depth=depth + 1)
                if nested:
                    lines.extend(nested.splitlines())

            elif isinstance(child, DocIRCodeBlockNode):
                is_first_para = False
                code = self._serialise_code_block(child)
                for code_line in code.splitlines():
                    lines.append(f"{continuation_indent}{code_line}")

        return lines

    def _serialise_table(self, node: DocIRTableNode) -> Optional[str]:
        """Serialise a table to GFM pipe-table syntax.

        If a header section is present it is emitted first with a separator
        row.  If there is no header, the first body row is treated as the
        header (required by GFM table syntax).
        """
        header_rows: List[DocIRTableRowNode] = []
        body_rows: List[DocIRTableRowNode] = []

        for child in node.children:
            if isinstance(child, DocIRTableHeaderNode):
                for row in child.children:
                    if isinstance(row, DocIRTableRowNode):
                        header_rows.append(row)

            elif isinstance(child, DocIRTableBodyNode):
                for row in child.children:
                    if isinstance(row, DocIRTableRowNode):
                        body_rows.append(row)

        if not header_rows and not body_rows:
            return None

        # Determine the effective header and data rows
        if header_rows:
            effective_header = header_rows[0]
            effective_body = header_rows[1:] + body_rows
        else:
            effective_header = body_rows[0]
            effective_body = body_rows[1:]

        # Collect cell alignments from the header row
        alignments = self._row_alignments(effective_header)
        col_count = len(alignments)

        # Render all rows to lists of cell strings
        header_cells = self._render_row_cells(effective_header, col_count)
        body_cell_rows = [
            self._render_row_cells(row, col_count) for row in effective_body
        ]

        # Compute column widths for alignment
        col_widths = [max(3, len(header_cells[c])) for c in range(col_count)]
        for row_cells in body_cell_rows:
            for c, cell in enumerate(row_cells):
                col_widths[c] = max(col_widths[c], len(cell))

        # Build separator row based on alignment
        sep_cells: List[str] = []
        for c, alignment in enumerate(alignments):
            w = col_widths[c]
            if alignment == "center":
                sep_cells.append(":" + "-" * (w - 2) + ":")
            elif alignment == "right":
                sep_cells.append("-" * (w - 1) + ":")
            else:
                sep_cells.append("-" * w)

        lines: List[str] = []
        lines.append(self._format_row(header_cells, col_widths))
        lines.append(self._format_row(sep_cells, col_widths))
        for row_cells in body_cell_rows:
            lines.append(self._format_row(row_cells, col_widths))

        return "\n".join(lines)

    def _row_alignments(self, row: DocIRTableRowNode) -> List[str]:
        """Return a list of alignment strings for each cell in a row."""
        alignments: List[str] = []
        for child in row.children:
            if isinstance(child, DocIRTableCellNode):
                alignments.append(child.alignment or "left")
        return alignments

    def _render_row_cells(
        self, row: DocIRTableRowNode, col_count: int
    ) -> List[str]:
        """Render all cells in a row to strings, padding to col_count."""
        cells: List[str] = []
        for child in row.children:
            if isinstance(child, DocIRTableCellNode):
                cells.append(self._serialise_cell(child))

        # Pad with empty cells if row is short
        while len(cells) < col_count:
            cells.append("")

        return cells[:col_count]

    def _serialise_cell(self, cell: DocIRTableCellNode) -> str:
        """Serialise a table cell's content to a single-line string.

        Inline children are serialised directly; block children (paragraphs,
        etc.) have their text extracted and joined with spaces.
        """
        parts: List[str] = []

        # Inline children directly on the cell (from markdown_to_doc_ir path)
        inline_types = (DocIRTextSpanNode, DocIRLinkNode, DocIRImageNode, DocIRLineBreakNode)
        inline_children = [c for c in cell.children if isinstance(c, inline_types)]
        if inline_children:
            parts.append(self._serialise_inline_children(inline_children))

        # Block children (from docx_ast_to_doc_ir path)
        for child in cell.children:
            if isinstance(child, DocIRParagraphNode):
                text = self._serialise_inline_children(child.children)
                if text:
                    parts.append(text)

            elif isinstance(child, DocIRCodeBlockNode):
                # A code block in a cell (e.g. from a monospace-font paragraph)
                # is rendered as an inline code span rather than a fenced block.
                if child.content:
                    parts.append(f"`{child.content}`")

        # Escape pipe characters so they don't break table structure
        return " ".join(parts).replace("|", "\\|")

    def _format_row(self, cells: List[str], col_widths: List[int]) -> str:
        """Format a list of cell strings into a padded pipe-table row."""
        padded = [cell.ljust(col_widths[i]) for i, cell in enumerate(cells)]
        return "| " + " | ".join(padded) + " |"

    def _serialise_inline_children(self, children: Sequence[DocIRNode]) -> str:
        """Serialise a list of inline nodes to a Markdown string."""
        parts: List[str] = []
        for child in children:
            parts.append(self._serialise_inline(child))

        return "".join(parts)

    def _serialise_inline(self, node: DocIRNode) -> str:
        """Serialise a single inline node."""
        if isinstance(node, DocIRTextSpanNode):
            return self._serialise_text_span(node)

        if isinstance(node, DocIRLinkNode):
            inner = self._serialise_inline_children(node.children)
            return f"[{inner}]({node.url})"

        if isinstance(node, DocIRImageNode):
            return f"![{node.alt_text}]({node.url})"

        if isinstance(node, DocIRLineBreakNode):
            return "  \n"

        return ""

    def _serialise_text_span(self, node: DocIRTextSpanNode) -> str:
        """Serialise a text span, wrapping with Markdown formatting markers."""
        if node.code:
            # Code spans are returned as-is — no escaping or further wrapping.
            return f"`{node.content}`"

        # Escape characters that would be misinterpreted as Markdown syntax.
        text = self._escape_markdown(node.content)

        # Apply markers from innermost to outermost so combinations are valid.
        # bold+italic share a single *** wrapper; other flags compose independently.
        if node.bold and node.italic:
            text = f"***{text}***"

        elif node.bold:
            text = f"**{text}**"

        elif node.italic:
            text = f"*{text}*"

        if node.strikethrough:
            text = f"~~{text}~~"

        return text

    @staticmethod
    def _escape_markdown(text: str) -> str:
        """Escape Markdown special characters in plain text spans."""
        # Escape only characters that are special in all inline contexts.
        # Characters like '.', '-', '(' are only special at line-start or in
        # specific constructs and do not need escaping in running prose.
        for ch in ("\\", "`", "*", "_", "~"):
            text = text.replace(ch, "\\" + ch)

        return text
