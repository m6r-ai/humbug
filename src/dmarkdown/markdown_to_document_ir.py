from typing import List

from dmarkdown.markdown_ast_node import (
    MarkdownASTBlockquoteNode,
    MarkdownASTBoldNode,
    MarkdownASTCodeBlockNode,
    MarkdownASTStrikethroughNode,
    MarkdownASTDocumentNode,
    MarkdownASTEmphasisNode,
    MarkdownASTHeadingNode,
    MarkdownASTHorizontalRuleNode,
    MarkdownASTImageNode,
    MarkdownASTInlineCodeNode,
    MarkdownASTLineBreakNode,
    MarkdownASTLinkNode,
    MarkdownASTListItemNode,
    MarkdownASTNode,
    MarkdownASTOrderedListNode,
    MarkdownASTParagraphNode,
    MarkdownASTTableBodyNode,
    MarkdownASTTableCellNode,
    MarkdownASTTableHeaderNode,
    MarkdownASTTableNode,
    MarkdownASTTableRowNode,
    MarkdownASTTextNode,
    MarkdownASTUnorderedListNode,
)
from document_ir import (
    DocumentIRBlockquoteNode,
    DocumentIRCodeBlockNode,
    DocumentIRDocumentNode,
    DocumentIRHeadingNode,
    DocumentIRHorizontalRuleNode,
    DocumentIRImageNode,
    DocumentIRLineBreakNode,
    DocumentIRLinkNode,
    DocumentIRListItemNode,
    DocumentIRNode,
    DocumentIROrderedListNode,
    DocumentIRParagraphNode,
    DocumentIRTableBodyNode,
    DocumentIRTableCellNode,
    DocumentIRTableHeaderNode,
    DocumentIRTableNode,
    DocumentIRTableRowNode,
    DocumentIRTextSpanNode,
    DocumentIRUnorderedListNode,
)


def markdown_ast_to_document_ir(
    document: MarkdownASTDocumentNode,
) -> DocumentIRDocumentNode:
    """
    Convert a dmarkdown AST document into a document_ir document.

    This is the public entry point for the mapper.

    Args:
        document: The root MarkdownASTDocumentNode produced by the dmarkdown parser.

    Returns:
        A DocumentIRDocumentNode containing the format-agnostic intermediate
        representation of the document content.
    """
    mapper = _MarkdownToDocumentIRMapper()
    return mapper.map_document(document)


class _MarkdownToDocumentIRMapper:
    """
    Maps a dmarkdown AST to a document_ir tree.

    Implements the visitor pattern manually rather than using
    MarkdownASTVisitor, because several mapping decisions require
    knowing the formatting context (bold/italic accumulation) rather
    than just the node type.
    """

    def map_document(self, node: MarkdownASTDocumentNode) -> DocumentIRDocumentNode:
        """
        Map the root document node.

        Args:
            node: The MarkdownASTDocumentNode to map.

        Returns:
            A DocumentIRDocumentNode with all block children mapped.
        """
        document_ir = DocumentIRDocumentNode(source_path=node.source_path)
        for child in node.children:
            mapped = self._map_block(child)
            if mapped is not None:
                document_ir.add_child(mapped)

        return document_ir

    def _map_block(self, node: MarkdownASTNode) -> DocumentIRNode | None:
        """
        Dispatch a block-level markdown node to the appropriate mapper.

        Args:
            node: A block-level MarkdownASTNode.

        Returns:
            The corresponding DocumentIRNode, or None if the node type is not
            mapped (silently skipped).
        """
        if isinstance(node, MarkdownASTHeadingNode):
            return self._map_heading(node)

        if isinstance(node, MarkdownASTParagraphNode):
            return self._map_paragraph(node)

        if isinstance(node, MarkdownASTBlockquoteNode):
            return self._map_blockquote(node)

        if isinstance(node, MarkdownASTCodeBlockNode):
            return self._map_code_block(node)

        if isinstance(node, MarkdownASTUnorderedListNode):
            return self._map_unordered_list(node)

        if isinstance(node, MarkdownASTOrderedListNode):
            return self._map_ordered_list(node)

        if isinstance(node, MarkdownASTTableNode):
            return self._map_table(node)

        if isinstance(node, MarkdownASTHorizontalRuleNode):
            return DocumentIRHorizontalRuleNode()

        if isinstance(node, MarkdownASTDocumentNode):
            # Nested document (e.g. from extract_sections container) — flatten
            container = DocumentIRParagraphNode()
            for child in node.children:
                mapped = self._map_block(child)
                if mapped is not None:
                    container.add_child(mapped)

            # Return the first child directly if there's only one, otherwise
            # wrap — but in practice this path produces a paragraph-like container
            return container

        return None

    def _map_heading(self, node: MarkdownASTHeadingNode) -> DocumentIRHeadingNode:
        """
        Map a heading node.

        Args:
            node: The MarkdownASTHeadingNode.

        Returns:
            A DocumentIRHeadingNode with inline children mapped as text spans.
        """
        heading = DocumentIRHeadingNode(level=node.level)
        self._append_inline_children(node, heading)
        return heading

    def _map_paragraph(self, node: MarkdownASTParagraphNode) -> DocumentIRParagraphNode:
        """
        Map a paragraph node.

        Args:
            node: The MarkdownASTParagraphNode.

        Returns:
            A DocumentIRParagraphNode with inline children mapped as text spans.
        """
        para = DocumentIRParagraphNode()
        self._append_inline_children(node, para)
        return para

    def _map_blockquote(self, node: MarkdownASTBlockquoteNode) -> DocumentIRBlockquoteNode:
        """
        Map a blockquote node.

        Blockquote children are block-level nodes (paragraphs, nested
        blockquotes, etc.) and are mapped recursively.

        Args:
            node: The MarkdownASTBlockquoteNode.

        Returns:
            A DocumentIRBlockquoteNode with block children mapped.
        """
        blockquote = DocumentIRBlockquoteNode()
        for child in node.children:
            mapped = self._map_block(child)
            if mapped is not None:
                blockquote.add_child(mapped)

        return blockquote

    def _map_code_block(self, node: MarkdownASTCodeBlockNode) -> DocumentIRCodeBlockNode:
        """
        Map a fenced code block.

        The syntax token information (tokens_by_line, states_by_line) is
        intentionally dropped here — it is rendering metadata specific to
        the dmarkdown AST and has no place in the format-agnostic IR.

        Args:
            node: The MarkdownASTCodeBlockNode.

        Returns:
            A DocumentIRCodeBlockNode with language and raw content.
        """
        return DocumentIRCodeBlockNode(
            language=node.language_name,
            content=node.content,
        )

    def _map_unordered_list(
        self, node: MarkdownASTUnorderedListNode
    ) -> DocumentIRUnorderedListNode:
        """
        Map an unordered list.

        Args:
            node: The MarkdownASTUnorderedListNode.

        Returns:
            A DocumentIRUnorderedListNode with list item children mapped.
        """
        ul = DocumentIRUnorderedListNode(tight=node.tight)
        for child in node.children:
            if isinstance(child, MarkdownASTListItemNode):
                ul.add_child(self._map_list_item(child))

            elif isinstance(child, MarkdownASTTableNode) and ul.children:
                last_item = ul.children[-1]
                assert isinstance(last_item, DocumentIRListItemNode)
                last_item.add_child(self._map_table(child))

        return ul

    def _map_ordered_list(
        self, node: MarkdownASTOrderedListNode
    ) -> DocumentIROrderedListNode:
        """
        Map an ordered list.

        Args:
            node: The MarkdownASTOrderedListNode.

        Returns:
            A DocumentIROrderedListNode with list item children mapped.
        """
        ol = DocumentIROrderedListNode(start=node.start, tight=node.tight)
        for child in node.children:
            if isinstance(child, MarkdownASTListItemNode):
                ol.add_child(self._map_list_item(child))

            elif isinstance(child, MarkdownASTTableNode) and ol.children:
                last_item = ol.children[-1]
                assert isinstance(last_item, DocumentIRListItemNode)
                last_item.add_child(self._map_table(child))

        return ol

    def _map_list_item(self, node: MarkdownASTListItemNode) -> DocumentIRListItemNode:
        """
        Map a list item.

        A list item may contain inline content and/or nested lists.
        Inline content is gathered into an implicit paragraph; nested
        lists are mapped recursively as block children.

        Args:
            node: The MarkdownASTListItemNode.

        Returns:
            A DocumentIRListItemNode with its content mapped.
        """
        item = DocumentIRListItemNode()

        # Separate inline children from nested block children (sub-lists,
        # paragraphs, blockquotes, etc.)
        inline_nodes: List[MarkdownASTNode] = []
        block_nodes: List[MarkdownASTNode] = []

        for child in node.children:
            if isinstance(
                child,
                (
                    MarkdownASTUnorderedListNode,
                    MarkdownASTOrderedListNode,
                    MarkdownASTParagraphNode,
                    MarkdownASTBlockquoteNode,
                    MarkdownASTCodeBlockNode,
                    MarkdownASTTableNode,
                ),
            ):
                block_nodes.append(child)

            else:
                inline_nodes.append(child)

        # If there are inline nodes, wrap them in an implicit paragraph
        if inline_nodes:
            para = DocumentIRParagraphNode()
            for inline in inline_nodes:
                spans = self._collect_spans(inline, bold=False, italic=False, strikethrough=False, code=False)
                for span in spans:
                    para.add_child(span)

            item.add_child(para)

        # Map block children (nested lists, paragraphs, etc.)
        for block in block_nodes:
            mapped = self._map_block(block)
            if mapped is not None:
                item.add_child(mapped)

        return item

    def _map_table(self, node: MarkdownASTTableNode) -> DocumentIRTableNode:
        """
        Map a table node.

        Args:
            node: The MarkdownASTTableNode.

        Returns:
            A DocumentIRTableNode with header and body sections mapped.
        """
        table = DocumentIRTableNode()
        for child in node.children:
            if isinstance(child, MarkdownASTTableHeaderNode):
                table.add_child(self._map_table_header(child))

            elif isinstance(child, MarkdownASTTableBodyNode):
                table.add_child(self._map_table_body(child))

        return table

    def _map_table_header(
        self, node: MarkdownASTTableHeaderNode
    ) -> DocumentIRTableHeaderNode:
        """Map a table header section."""
        header = DocumentIRTableHeaderNode()
        for child in node.children:
            if isinstance(child, MarkdownASTTableRowNode):
                header.add_child(self._map_table_row(child))

        return header

    def _map_table_body(self, node: MarkdownASTTableBodyNode) -> DocumentIRTableBodyNode:
        """Map a table body section."""
        body = DocumentIRTableBodyNode()
        for child in node.children:
            if isinstance(child, MarkdownASTTableRowNode):
                body.add_child(self._map_table_row(child))

        return body

    def _map_table_row(self, node: MarkdownASTTableRowNode) -> DocumentIRTableRowNode:
        """Map a table row."""
        row = DocumentIRTableRowNode()
        for child in node.children:
            if isinstance(child, MarkdownASTTableCellNode):
                row.add_child(self._map_table_cell(child))

        return row

    def _map_table_cell(self, node: MarkdownASTTableCellNode) -> DocumentIRTableCellNode:
        """Map a table cell, collecting its inline content as text spans."""
        cell = DocumentIRTableCellNode(
            is_header=node.is_header,
            alignment=node.alignment,
        )
        self._append_inline_children(node, cell)
        return cell

    def _append_inline_children(
        self, source: MarkdownASTNode, target: DocumentIRNode
    ) -> None:
        """
        Walk the inline children of source and append spans/links/images
        to target.

        This is the bridge between the nested bold/italic/code structure of
        the markdown AST and the flat DocumentIRTextSpanNode model.

        Args:
            source: The markdown AST node whose children are inline content.
            target: The document_ir node to append the mapped inline nodes to.
        """
        for child in source.children:
            nodes = self._collect_inline(child, bold=False, italic=False, strikethrough=False, code=False)
            for node in nodes:
                target.add_child(node)

    def _collect_inline(
        self,
        node: MarkdownASTNode,
        bold: bool,
        italic: bool,
        strikethrough: bool,
        code: bool,
    ) -> List[DocumentIRNode]:
        """
        Recursively collect inline document_ir nodes from a markdown AST node.

        Formatting flags are accumulated as we descend through Bold/Emphasis/
        InlineCode wrappers, so that a bold-italic run correctly produces a
        DocumentIRTextSpanNode with both flags set.

        Link and image nodes are returned as DocumentIRLinkNode/DocIRImageNode
        respectively.  Line breaks become DocumentIRLineBreakNode.

        Args:
            node: The markdown AST inline node to process.
            bold: Whether bold formatting is currently active.
            italic: Whether italic formatting is currently active.
            strikethrough: Whether strikethrough formatting is currently active.
            code: Whether inline-code formatting is currently active.

        Returns:
            A list of DocumentIRNode instances representing the inline content.
        """
        if isinstance(node, MarkdownASTTextNode):
            if not node.content:
                return []

            return [DocumentIRTextSpanNode(
                content=node.content,
                bold=bold,
                italic=italic,
                strikethrough=strikethrough,
                code=code,
            )]

        if isinstance(node, MarkdownASTBoldNode):
            result: List[DocumentIRNode] = []
            for child in node.children:
                result.extend(self._collect_inline(child, bold=True, italic=italic, strikethrough=strikethrough, code=code))

            return result

        if isinstance(node, MarkdownASTEmphasisNode):
            result = []
            for child in node.children:
                result.extend(self._collect_inline(child, bold=bold, italic=True, strikethrough=strikethrough, code=code))

            return result

        if isinstance(node, MarkdownASTStrikethroughNode):
            result = []
            for child in node.children:
                result.extend(self._collect_inline(child, bold=bold, italic=italic, strikethrough=True, code=code))
            return result

        if isinstance(node, MarkdownASTInlineCodeNode):
            if not node.content:
                return []

            return [DocumentIRTextSpanNode(
                content=node.content,
                bold=bold,
                italic=italic,
                code=True,
            )]

        if isinstance(node, MarkdownASTLinkNode):
            link = DocumentIRLinkNode(url=node.url, title=node.title)
            for child in node.children:
                result = self._collect_inline(child, bold=bold, italic=italic, strikethrough=strikethrough, code=code)
                for item in result:
                    link.add_child(item)

            return [link]

        if isinstance(node, MarkdownASTImageNode):
            return [DocumentIRImageNode(
                url=node.url,
                alt_text=node.alt_text,
                title=node.title,
            )]

        if isinstance(node, MarkdownASTLineBreakNode):
            return [DocumentIRLineBreakNode()]

        # Any other node type: recurse into children with current formatting
        result = []
        for child in node.children:
            result.extend(self._collect_inline(child, bold=bold, italic=italic, strikethrough=strikethrough, code=code))

        return result

    def _collect_spans(
        self,
        node: MarkdownASTNode,
        bold: bool,
        italic: bool,
        strikethrough: bool,
        code: bool,
    ) -> List[DocumentIRNode]:
        """Alias for _collect_inline used in list item mapping for clarity."""
        return self._collect_inline(node, bold=bold, italic=italic, strikethrough=strikethrough, code=code)
