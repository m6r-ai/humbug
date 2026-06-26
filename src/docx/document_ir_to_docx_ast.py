from typing import List, Sequence, Tuple

from document_ir import (
    DocumentIRBlockquoteNode,
    DocumentIRCodeBlockNode,
    DocumentIRDefinitionDescriptionNode,
    DocumentIRDefinitionListNode,
    DocumentIRDefinitionTermNode,
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
from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
    DocxASTNode,
    DocxASTBodyNode,
    DocxASTBreakNode,
    DocxASTDocumentNode,
    DocxASTDrawingNode,
    DocxASTHyperlinkNode,
    DocxASTNumLevelNode,
    DocxASTNumNode,
    DocxASTNumberingNode,
    DocxASTNumberingPropertiesNode,
    DocxASTParagraphNode,
    DocxASTParagraphPropertiesNode,
    DocxASTRunNode,
    DocxASTRunPropertiesNode,
    DocxASTStyleNode,
    DocxASTStylesNode,
    DocxASTTableCellNode,
    DocxASTTableCellPropertiesNode,
    DocxASTTableNode,
    DocxASTTablePropertiesNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTextNode,
)

_STYLE_NORMAL = "Normal"
_STYLE_HEADINGS = {
    1: "Heading1",
    2: "Heading2",
    3: "Heading3",
    4: "Heading4",
    5: "Heading5",
    6: "Heading6"
}
_STYLE_CODE_BLOCK = "CodeBlock"
_STYLE_BLOCKQUOTE = "Blockquote"
_STYLE_HORIZONTAL_RULE = "HorizontalRule"

# numId values for bullet and ordered lists (referenced in numPr)
_NUM_ID_BULLET = "1"

# Maximum nesting depth supported for lists
_MAX_LIST_DEPTH = 9

# Background shading colour for blockquote paragraphs
_BLOCKQUOTE_SHADING = "E8F0F8"

# Background shading colour for code block cells
_CODE_BLOCK_SHADING = "F2F2F2"

# Background shading colour for table header cells
_TABLE_HEADER_SHADING = "D0D0D0"

# Standard inter-block spacing_after in twips, matching Normal style's spacing_after.
_STANDARD_SPACING = 200

# Text area width in twips: page width (12240) minus left+right margins (1440 each).
# Used to compute dxa widths for block elements so they shrink correctly when indented.
_TEXT_WIDTH_DXA = 9360

# Cell margin in twips applied to all four sides of blockquote and code block cells.
_CELL_MARGIN_DXA = 120


def document_ir_to_docx_ast(document: DocumentIRDocumentNode) -> DocxASTDocumentNode:
    """
    Convert a document_ir document into a DOCX AST document.

    This is the public entry point for the write-side mapper.

    The returned DocxASTDocumentNode contains:
    - A DocxASTStylesNode with all styles referenced by the content
    - A DocxASTNumberingNode with bullet and ordered list definitions
    - A DocxASTBodyNode with the full document content

    The AST can then be serialised to a .docx file by the DOCX writer.

    Args:
        document: The DocumentIRDocumentNode to convert.

    Returns:
        A DocxASTDocumentNode ready for serialisation.
    """
    mapper = _DocumentIRToDocxASTMapper()
    return mapper.map(document)


class _DocumentIRToDocxASTMapper:
    """
    Maps a document_ir tree to a DOCX AST tree.

    Generates the styles and numbering definitions required by the content,
    then maps each document_ir node to the appropriate DOCX AST structure.
    """

    def __init__(self) -> None:
        """Initialise the mapper with a fresh numbering state."""
        # Holds the numbering node so ordered lists can register new numId
        # instances dynamically during mapping.
        self._numbering_node: DocxASTNumberingNode | None = None
        # Next available numId; 1 is reserved for bullets, so ordered lists
        # start from 2.
        self._next_num_id: int = 2

    def map(self, document: DocumentIRDocumentNode) -> DocxASTDocumentNode:
        """
        Perform the full mapping.

        Args:
            document: The source DocumentIRDocumentNode.

        Returns:
            A populated DocxASTDocumentNode.
        """
        docx_doc = DocxASTDocumentNode(source_path=document.source_path)

        # Always include styles and numbering so the output is self-contained
        docx_doc.add_child(self._build_styles_node())
        self._numbering_node = self._build_numbering_node()
        docx_doc.add_child(self._numbering_node)

        body = DocxASTBodyNode()
        for child in document.children:
            self._map_block(child, body)
        docx_doc.add_child(body)

        return docx_doc

    def _build_styles_node(self) -> DocxASTStylesNode:
        """Build a DocxASTStylesNode with all styles used by generated content."""
        styles = DocxASTStylesNode()

        # Normal (default paragraph style)
        normal = DocxASTStyleNode(
            style_type="paragraph",
            style_id=_STYLE_NORMAL,
            name="Normal",
            is_default=True,
        )
        styles.add_child(normal)

        # Headings 1–6
        for level in range(1, 7):
            style_id = _STYLE_HEADINGS[level]
            h = DocxASTStyleNode(
                style_type="paragraph",
                style_id=style_id,
                name=f"heading {level}",
                based_on=_STYLE_NORMAL,
                next_style=_STYLE_NORMAL,
            )
            ppr = DocxASTParagraphPropertiesNode(outline_level=level - 1)
            h.add_child(ppr)
            rpr = DocxASTRunPropertiesNode(bold=True)
            h.add_child(rpr)
            styles.add_child(h)

        # Code block
        code = DocxASTStyleNode(
            style_type="paragraph",
            style_id=_STYLE_CODE_BLOCK,
            name="Code Block",
            based_on=_STYLE_NORMAL,
        )
        code_rpr = DocxASTRunPropertiesNode(font_ascii="Consolas", font_h_ansi="Consolas")
        code.add_child(code_rpr)
        styles.add_child(code)

        # Blockquote
        bq = DocxASTStyleNode(
            style_type="paragraph",
            style_id=_STYLE_BLOCKQUOTE,
            name="Blockquote",
            based_on=_STYLE_NORMAL,
        )
        bq_ppr = DocxASTParagraphPropertiesNode(spacing_after=0)
        bq.add_child(bq_ppr)
        bq_rpr = DocxASTRunPropertiesNode(italic=True)
        bq.add_child(bq_rpr)
        styles.add_child(bq)

        # Horizontal rule
        hr = DocxASTStyleNode(
            style_type="paragraph",
            style_id=_STYLE_HORIZONTAL_RULE,
            name="Horizontal Rule",
            based_on=_STYLE_NORMAL,
        )
        hr_ppr = DocxASTParagraphPropertiesNode()
        hr.add_child(hr_ppr)
        styles.add_child(hr)

        return styles

    def _build_numbering_node(self) -> DocxASTNumberingNode:
        """
        Build a DocxASTNumberingNode with bullet and ordered list definitions.

        Defines two abstractNum entries (bullet and decimal).  A single bullet
        num instance (numId=1) is pre-created.  Ordered list num instances are
        allocated dynamically by _allocate_ordered_num_id as lists are mapped,
        so each ordered list gets its own numId and can start at any number.
        """
        numbering = DocxASTNumberingNode()

        # abstractNum 0: bullet list
        bullet_abstract = DocxASTAbstractNumNode(abstract_num_id="0")
        bullet_abstract.multi_level_type = "hybridMultilevel"
        bullet_chars = ["\u2022", "\u25e6", "\u25aa"]  # •, ◦, ▪
        for i in range(_MAX_LIST_DEPTH):
            lvl = DocxASTNumLevelNode(
                ilvl=i,
                start=1,
                num_fmt="bullet",
                lvl_text=bullet_chars[i % 3],
                lvl_jc="left",
            )
            bullet_abstract.add_child(lvl)

        numbering.add_child(bullet_abstract)

        # abstractNum 1: decimal (ordered) list
        ordered_abstract = DocxASTAbstractNumNode(abstract_num_id="1")
        ordered_abstract.multi_level_type = "multilevel"
        for i in range(_MAX_LIST_DEPTH):
            lvl = DocxASTNumLevelNode(
                ilvl=i,
                start=1,
                num_fmt="decimal",
                lvl_text=f"%{i + 1}.",
                lvl_jc="left",
            )
            ordered_abstract.add_child(lvl)

        numbering.add_child(ordered_abstract)

        # num 1: bullet instance
        numbering.add_child(DocxASTNumNode(num_id=_NUM_ID_BULLET, abstract_num_id="0"))

        return numbering

    def _allocate_ordered_num_id(self, start: int) -> str:
        """
        Allocate a fresh numId for an ordered list and register it.

        Each ordered list gets its own <w:num> instance so that Word resets
        the counter independently for each list.  A lvlOverride startOverride
        is used to honour non-default start values.

        Args:
            start: The starting number for the list (typically 1).

        Returns:
            The allocated numId string.
        """
        num_id = str(self._next_num_id)
        self._next_num_id += 1
        assert self._numbering_node is not None
        self._numbering_node.add_child(DocxASTNumNode(
            num_id=num_id,
            abstract_num_id="1",
            start_override=start,
        ))
        return num_id

    def _map_block(self, node: DocumentIRNode, parent: DocxASTBodyNode) -> None:
        """
        Map a block-level document_ir node and append result(s) to parent.

        Args:
            node: The document_ir block node to map.
            parent: The DocxASTBodyNode to append generated paragraphs/tables to.
        """
        if isinstance(node, DocumentIRHeadingNode):
            parent.add_child(self._map_heading(node))

        elif isinstance(node, DocumentIRParagraphNode):
            para = self._map_paragraph_node(node, style_id=_STYLE_NORMAL)
            if para is not None:
                parent.add_child(para)

        elif isinstance(node, DocumentIRBlockquoteNode):
            self._map_blockquote(node, parent)
            parent.add_child(self._make_spacer_para())

        elif isinstance(node, DocumentIRCodeBlockNode):
            self._map_code_block(node, parent)

        elif isinstance(node, DocumentIRUnorderedListNode):
            self._map_list(node, parent, num_id=_NUM_ID_BULLET, depth=0, tight=node.tight)

        elif isinstance(node, DocumentIROrderedListNode):
            num_id = self._allocate_ordered_num_id(node.start)
            self._map_list(node, parent, num_id=num_id, depth=0, tight=node.tight)

        elif isinstance(node, DocumentIRTableNode):
            parent.add_child(self._map_table(node))
            parent.add_child(self._make_spacer_para())

        elif isinstance(node, DocumentIRHorizontalRuleNode):
            parent.add_child(self._make_horizontal_rule_para())

        elif isinstance(node, DocumentIRDefinitionListNode):
            self._map_definition_list(node, parent)

        # Unknown block types are silently skipped

    def _map_heading(
        self, node: DocumentIRHeadingNode, indent_left: int | None = None
    ) -> DocxASTParagraphNode:
        """Map a heading to a styled paragraph."""
        style_id = _STYLE_HEADINGS.get(node.level, _STYLE_HEADINGS[6])
        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(style_id=style_id, indent_left=indent_left)
        para.add_child(ppr)
        self._append_inline_children(node.children, para)
        return para

    def _map_paragraph_node(
        self,
        node: DocumentIRParagraphNode,
        style_id: str = _STYLE_NORMAL,
        shading: str | None = None,
        indent_left: int | None = None,
    ) -> DocxASTParagraphNode | None:
        """
        Map a paragraph to a styled paragraph.

        Returns None if the paragraph would be empty.
        """
        runs = self._build_runs(node.children)
        if not runs:
            return None

        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(style_id=style_id, shading=shading,
                                             indent_left=indent_left)
        para.add_child(ppr)
        for run in runs:
            para.add_child(run)

        return para

    def _map_blockquote(
        self, node: DocumentIRBlockquoteNode, parent: DocxASTBodyNode,
        suppress_trailing_spacing: bool = True,
        indent_base: int = 0,
        available_width: int = _TEXT_WIDTH_DXA,
    ) -> None:
        """
        Map a blockquote as a borderless table with a shaded cell.

        Using a table cell background gives a full-width fill across the entire
        blockquote region regardless of content indentation, which paragraph-level
        shading cannot achieve.
        """
        # Map content into a temporary body so we can transfer children to the cell
        cell_body = DocxASTBodyNode()
        # Compute the cell width up front so inner block calls can use it.
        width = available_width - indent_base
        # Inner blocks must fit within the cell's content area (cell margins on left and right).
        inner_width = width - 2 * _CELL_MARGIN_DXA

        for child in node.children:
            if isinstance(child, DocumentIRParagraphNode):
                para = self._map_paragraph_node(child, style_id=_STYLE_BLOCKQUOTE,
                                                indent_left=None)
                if para is not None:
                    cell_body.add_child(para)

            elif isinstance(child, DocumentIRHeadingNode):
                cell_body.add_child(self._map_heading(child, indent_left=None))

            elif isinstance(child, DocumentIRUnorderedListNode):
                self._map_list(child, cell_body, num_id=_NUM_ID_BULLET, depth=0,
                               indent_base=0, tight=child.tight, available_width=inner_width)

            elif isinstance(child, DocumentIROrderedListNode):
                num_id = self._allocate_ordered_num_id(child.start)
                self._map_list(child, cell_body, num_id=num_id, depth=0,
                               indent_base=0, tight=child.tight, available_width=inner_width)

            elif isinstance(child, DocumentIRCodeBlockNode):
                self._map_code_block(child, cell_body, available_width=inner_width)

            elif isinstance(child, DocumentIRTableNode):
                cell_body.add_child(self._map_table(child, available_width=inner_width))
                cell_body.add_child(self._make_spacer_para())

            else:
                if isinstance(child, DocumentIRBlockquoteNode):
                    self._map_blockquote(child, cell_body, indent_base=indent_base + 720,
                                         available_width=inner_width)

                else:
                    self._map_block(child, cell_body)

        # The mandatory trailing paragraph in every Word table cell must not add
        # visible height.  Suppress spacing_after on the last content paragraph
        # when in a tight context (or at top level).  In a loose context, leave
        # it so the cell's spacing bleeds out and separates it from the next item.
        if suppress_trailing_spacing:
            self._suppress_list_trailing_spacing(cell_body)

        # Build the table wrapper
        table = DocxASTTableNode()

        tbl_pr = DocxASTTablePropertiesNode(width=width, width_type="dxa", no_borders=True, indent=indent_base)
        table.add_child(tbl_pr)

        row = DocxASTTableRowNode()
        table.add_child(row)

        cell = DocxASTTableCellNode()
        tcp = DocxASTTableCellPropertiesNode(
            width=width,
            width_type="dxa",
            shading_fill=_BLOCKQUOTE_SHADING,
            left_bar=True,
        )
        cell.add_child(tcp)

        for content_node in cell_body.children:
            cell.add_child(content_node)

        row.add_child(cell)
        parent.add_child(table)

    def _map_code_block(
        self, node: DocumentIRCodeBlockNode, parent: DocxASTBodyNode,
        indent_left: int | None = None,
        trailing_spacing: bool = True,
        available_width: int = _TEXT_WIDTH_DXA,
    ) -> None:
        """
        Map a code block as a borderless single-cell table with shaded background.

        The table provides full-width shading independent of text indentation.
        Cell margins supply the padding.  trailing_spacing controls whether a
        post-block spacer paragraph is appended after the table.
        """
        lines = node.content.split("\n")
        # Remove trailing empty line that split() often produces
        if lines and lines[-1] == "":
            lines = lines[:-1]
        if not lines:
            lines = [""]

        cell_body = DocxASTBodyNode()
        for line in lines:
            para = DocxASTParagraphNode()
            ppr = DocxASTParagraphPropertiesNode(style_id=_STYLE_CODE_BLOCK)
            para.add_child(ppr)
            if line:
                run = DocxASTRunNode()
                run.add_child(DocxASTTextNode(content=line, preserve_space=True))
                para.add_child(run)
            cell_body.add_child(para)

        table = DocxASTTableNode()
        _indent = indent_left or 0
        width = available_width - _indent
        tbl_pr = DocxASTTablePropertiesNode(width=width, width_type="dxa", no_borders=True,
                                            indent=indent_left)
        table.add_child(tbl_pr)
        row = DocxASTTableRowNode()
        table.add_child(row)
        cell = DocxASTTableCellNode()
        tcp = DocxASTTableCellPropertiesNode(width=width, width_type="dxa",
                                             shading_fill=_CODE_BLOCK_SHADING)
        cell.add_child(tcp)
        for content_node in cell_body.children:
            cell.add_child(content_node)

        row.add_child(cell)
        parent.add_child(table)

        if trailing_spacing:
            parent.add_child(self._make_spacer_para())

    def _map_list(
        self,
        node: DocumentIRNode,
        parent: DocxASTBodyNode,
        num_id: str,
        depth: int,
        indent_base: int = 0,
        shading: str | None = None,
        tight: bool = True,
        suppress_trailing_spacing: bool = False,
        available_width: int = _TEXT_WIDTH_DXA,
    ) -> None:
        """
        Recursively map a list node, emitting paragraphs with numPr.

        Args:
            node: DocumentIRUnorderedListNode or DocumentIROrderedListNode.
            parent: The body node to append paragraphs to.
            num_id: The numId to reference in numPr.
            depth: Current nesting depth (0-based ilvl).
            indent_base: Extra left indent in twips added to all levels (used
                when the list is nested inside a blockquote).
            shading: Background shading hex colour, or None.
            tight: Whether to suppress inter-item spacing.
            suppress_trailing_spacing: When True, skip the trailing spacing
                patch.  Used when this list is a non-final child inside a tight
                outer item, where the outer tight context owns the spacing.
        """
        for child in node.children:
            if isinstance(child, DocumentIRListItemNode):
                self._map_list_item(child, parent, num_id=num_id, depth=depth,
                                    indent_base=indent_base, shading=shading, tight=tight,
                                    available_width=available_width)

        if not suppress_trailing_spacing:
            self._apply_list_trailing_spacing(parent)

        else:
            self._suppress_list_trailing_spacing(parent)

    def _map_list_item(
        self,
        item: DocumentIRListItemNode,
        parent: DocxASTBodyNode,
        num_id: str,
        depth: int,
        indent_base: int = 0,
        shading: str | None = None,
        tight: bool = True,
        available_width: int = _TEXT_WIDTH_DXA,
    ) -> None:
        """Map a list item, emitting all its content paragraphs."""
        is_first_para = True
        indent_left = indent_base + 720 * (depth + 1)  # text/continuation indent

        for child in item.children:
            if isinstance(child, DocumentIRParagraphNode):
                runs = self._build_runs(child.children)
                para = DocxASTParagraphNode()
                if is_first_para:
                    # First paragraph: carries the bullet/number marker.
                    # The numbering definition carries no indent; paragraph-level
                    # indent is the sole source of positioning.
                    # In a loose list, non-first items need spacing_before so
                    # that a gap appears even when the previous item ended with
                    # a blockquote table (which has no trailing paragraph to
                    # carry spacing_after).
                    ppr = DocxASTParagraphPropertiesNode(
                        style_id=_STYLE_NORMAL,
                        indent_left=indent_left,
                        indent_hanging=360,
                        shading=shading,
                        spacing_after=0 if tight else None,
                        spacing_before=_STANDARD_SPACING if (
                            not tight and item.parent is not None and item.parent.children[0] is not item
                        ) else None,
                    )
                    num_pr = DocxASTNumberingPropertiesNode(
                        num_id=num_id,
                        ilvl=min(depth, _MAX_LIST_DEPTH - 1),
                    )
                    ppr.add_child(num_pr)
                    is_first_para = False

                else:
                    # Continuation paragraph: indented to match the list level,
                    # no bullet/number marker
                    ppr = DocxASTParagraphPropertiesNode(
                        style_id=_STYLE_NORMAL,
                        indent_left=indent_left,
                        shading=shading,
                        spacing_after=0 if tight else None,
                    )
                para.add_child(ppr)
                for run in runs:
                    para.add_child(run)

                parent.add_child(para)

            elif isinstance(child, DocumentIRUnorderedListNode):
                is_first_para = False
                self._map_list(child, parent, num_id=_NUM_ID_BULLET, depth=depth + 1,
                               indent_base=indent_base, shading=shading,
                               tight=child.tight,
                               suppress_trailing_spacing=tight,
                               available_width=available_width)

            elif isinstance(child, DocumentIROrderedListNode):
                is_first_para = False
                num_id = self._allocate_ordered_num_id(child.start)
                self._map_list(child, parent, num_id=num_id, depth=depth + 1,
                               indent_base=indent_base, shading=shading,
                               tight=child.tight,
                               suppress_trailing_spacing=tight,
                               available_width=available_width)

            else:
                # Other block children (code blocks, blockquotes, tables) inside
                # a list item — handle with list context so indentation is preserved.
                is_first_para = False
                if isinstance(child, DocumentIRCodeBlockNode):
                    is_last = child is item.children[-1]
                    self._map_code_block(child, parent, indent_left=indent_left,
                                         trailing_spacing=not (tight and is_last),
                                         available_width=available_width)
                    # Code blocks now emit a table + optional spacer — apply the
                    # same spacer removal logic as blockquotes.
                    if ((tight or child is item.children[-1])
                            and parent.children
                            and self._is_spacer_para(parent.children[-1])):
                        parent.remove_child(parent.children[-1])

                elif isinstance(child, DocumentIRBlockquoteNode):
                    self._map_blockquote(child, parent, suppress_trailing_spacing=True,
                                         indent_base=indent_left,
                                         available_width=available_width)

                elif isinstance(child, DocumentIRTableNode):
                    self._map_table_in_list(child, parent, indent_left=indent_left, tight=tight,
                                            available_width=available_width)

                else:
                    self._map_block(child, parent)
                    # _map_block adds a trailing spacer for tables.  Remove it
                    # in tight lists or when it's the last child.
                    if ((tight or child is item.children[-1])
                            and parent.children
                            and self._is_spacer_para(parent.children[-1])):
                        parent.remove_child(parent.children[-1])

    def _map_table_in_list(
        self,
        node: DocumentIRTableNode,
        parent: DocxASTBodyNode,
        indent_left: int,
        tight: bool,
        available_width: int = _TEXT_WIDTH_DXA,
    ) -> None:
        """
        Map a table that is a direct child of a list item.

        Word supports <w:tblInd> on a top-level table to shift it right from
        the page margin, so we emit the table directly with the list-level
        indent applied via tblInd.  Percentage widths cannot be used on nested
        tables, so the table uses auto width here.

        Args:
            node: The DocumentIRTableNode to render.
            parent: The body node to append the wrapper table to.
            indent_left: Left indent in twips matching the list level.
            tight: Whether the enclosing list is tight (suppresses trailing gap).
        """
        table = self._map_table(node, indent=indent_left, available_width=available_width)
        parent.add_child(table)

        if not tight:
            parent.add_child(self._make_spacer_para())

    def _map_table(self, node: DocumentIRTableNode, indent: int = 0,
                   available_width: int = _TEXT_WIDTH_DXA) -> DocxASTTableNode:
        """
        Map a table node.

        Args:
            node: The DocumentIRTableNode to map.
            indent: Left indent in twips (for tables inside list items).  The
                table width is reduced by this amount so it does not overflow
                the right margin.
            available_width: The width in twips available to this table.  Defaults
                to the full text area width.  Pass a smaller value when the table
                is inside a blockquote or other constrained container.
        """
        table = DocxASTTableNode()

        width = available_width - indent
        tbl_pr = DocxASTTablePropertiesNode(width=width, width_type="dxa",
                                            indent=indent if indent else None)
        table.add_child(tbl_pr)

        for child in node.children:
            if isinstance(child, DocumentIRTableHeaderNode):
                for row_node in child.children:
                    if isinstance(row_node, DocumentIRTableRowNode):
                        table.add_child(self._map_table_row(row_node, is_header=True))

            elif isinstance(child, DocumentIRTableBodyNode):
                for row_node in child.children:
                    if isinstance(row_node, DocumentIRTableRowNode):
                        table.add_child(self._map_table_row(row_node, is_header=False))

        return table

    def _map_table_row(
        self, node: DocumentIRTableRowNode, is_header: bool
    ) -> DocxASTTableRowNode:
        """Map a table row."""
        row = DocxASTTableRowNode()
        if is_header:
            rpr = DocxASTTableRowPropertiesNode(is_header=True, cant_split=True)
            row.add_child(rpr)

        else:
            rpr = DocxASTTableRowPropertiesNode(cant_split=True)
            row.add_child(rpr)

        for child in node.children:
            if isinstance(child, DocumentIRTableCellNode):
                row.add_child(self._map_table_cell(child, is_header))

        return row

    def _map_table_cell(
        self, node: DocumentIRTableCellNode, is_header: bool
    ) -> DocxASTTableCellNode:
        """Map a table cell."""
        cell = DocxASTTableCellNode()
        cpr = DocxASTTableCellPropertiesNode(
            width_type="auto",
            shading_fill=_TABLE_HEADER_SHADING if is_header else None,
        )
        cell.add_child(cpr)

        # Separate inline children (text spans, links, images, line breaks) from
        # block children.  Cells produced by markdown_to_document_ir carry inline nodes
        # directly as children of the cell rather than wrapped in a paragraph, so
        # we collect them here and emit a single paragraph for the cell.
        inline_types = (DocumentIRTextSpanNode, DocumentIRLinkNode, DocumentIRImageNode, DocumentIRLineBreakNode)
        inline_children = [c for c in node.children if isinstance(c, inline_types)]
        if inline_children:
            justification = self._alignment_to_jc(node.alignment)
            para = DocxASTParagraphNode()
            ppr = DocxASTParagraphPropertiesNode(
                style_id=_STYLE_NORMAL,
                justification=justification,
                spacing_after=0,
            )
            para.add_child(ppr)
            for run in self._build_runs(inline_children):
                para.add_child(run)
            cell.add_child(para)

        # Map block-level cell content
        for child in node.children:
            if isinstance(child, DocumentIRParagraphNode):
                justification = self._alignment_to_jc(node.alignment)
                para = DocxASTParagraphNode()
                ppr = DocxASTParagraphPropertiesNode(
                    style_id=_STYLE_NORMAL,
                    justification=justification,
                    spacing_after=0,
                )
                para.add_child(ppr)
                runs = self._build_runs(child.children)
                for run in runs:
                    para.add_child(run)

                cell.add_child(para)

            elif isinstance(child, DocumentIRTableNode):
                cell.add_child(self._map_table(child))

            elif isinstance(child, DocumentIRHeadingNode):
                cell.add_child(self._map_heading(child))

            elif isinstance(child, DocumentIRCodeBlockNode):
                # Inline code block in cell: emit as single para
                para = DocxASTParagraphNode()
                ppr = DocxASTParagraphPropertiesNode(style_id=_STYLE_CODE_BLOCK)
                ppr.spacing_after = 0
                para.add_child(ppr)
                run = DocxASTRunNode()
                run.add_child(DocxASTTextNode(content=child.content, preserve_space=True))
                para.add_child(run)
                cell.add_child(para)

        # Ensure cell always has at least one paragraph (OOXML requirement)
        has_para = any(isinstance(c, DocxASTParagraphNode) for c in cell.children)
        if not has_para:
            empty_para = DocxASTParagraphNode()
            empty_para.add_child(DocxASTParagraphPropertiesNode(style_id=_STYLE_NORMAL))
            cell.add_child(empty_para)

        return cell

    @staticmethod
    def _alignment_to_jc(alignment: str) -> str | None:
        """Convert a document_ir alignment string to a DOCX justification value."""
        mapping = {"left": "left", "center": "center", "right": "right"}
        return mapping.get(alignment)

    def _make_horizontal_rule_para(self) -> DocxASTParagraphNode:
        """Create a paragraph with a bottom border to represent a horizontal rule."""
        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(style_id=_STYLE_HORIZONTAL_RULE)
        para.add_child(ppr)
        return para

    def _make_spacer_para(self) -> DocxASTParagraphNode:
        """
        Create a zero-height spacer paragraph to follow a table.

        OOXML provides no table-level spacing_after equivalent, so a spacer
        paragraph is inserted after every table.  It uses Normal style with
        both spacing_before and spacing_after suppressed so it contributes no
        extra visual gap beyond the Normal style's own spacing_after.
        """
        para = DocxASTParagraphNode()
        para.add_child(DocxASTParagraphPropertiesNode(
            style_id=_STYLE_NORMAL,
            spacing_before=0,
            spacing_after=0,
        ))
        return para

    def _apply_list_trailing_spacing(self, parent: DocxASTBodyNode) -> None:
        """
        Apply the standard trailing gap after a list.

        Walks back through the paragraphs most recently added to parent and
        applies the appropriate end-of-list gap:

        - If the last child is a paragraph (Normal or CodeBlock), set its
          spacing_after to _STANDARD_SPACING explicitly.  This overrides both
          the tight list's explicit 0 and CodeBlock's style-level 0.
        - If the last child is a table (blockquote or content table), there is
          no paragraph to patch, so a spacer paragraph is appended instead.
        - Spacer paragraphs from mid-list blockquotes are skipped — they are
          not the end of the list.
        """
        if not parent.children:
            return

        last = parent.children[-1]

        if isinstance(last, DocxASTParagraphNode) and not self._is_spacer_para(last):
            # Patch spacing_after on the last content paragraph.
            ppr = next(
                (c for c in last.children if isinstance(c, DocxASTParagraphPropertiesNode)),
                None,
            )
            if ppr is not None:
                ppr.spacing_after = _STANDARD_SPACING

        elif isinstance(last, DocxASTTableNode):
            # Last child is a table (blockquote/table) — no paragraph to patch.
            parent.add_child(self._make_spacer_para())

        elif self._is_spacer_para(last):
            # Last child is already a spacer (from a trailing blockquote/table
            # inside the last item) — it provides the gap, nothing to do.
            pass

    def _suppress_list_trailing_spacing(self, parent: DocxASTBodyNode) -> None:
        """
        Suppress the trailing gap on the last paragraph emitted by a list.

        Called when a nested list is inside a tight outer item.  The outer
        tight context owns all spacing decisions, so the last paragraph of
        the inner list must have spacing_after=0 regardless of what the inner
        list's own items set (e.g. a loose inner list sets spacing_after=None
        which inherits 200 from Normal — that must be overridden to 0 here).
        """
        if not parent.children:
            return

        last = parent.children[-1]

        if isinstance(last, DocxASTParagraphNode) and not self._is_spacer_para(last):
            ppr = next(
                (c for c in last.children if isinstance(c, DocxASTParagraphPropertiesNode)),
                None,
            )
            if ppr is not None:
                ppr.spacing_after = 0

    def _is_spacer_para(self, node: DocxASTNode) -> bool:
        """Return True if node is a zero-height spacer paragraph."""
        if not isinstance(node, DocxASTParagraphNode):
            return False
        ppr = next((c for c in node.children if isinstance(c, DocxASTParagraphPropertiesNode)), None)
        return (ppr is not None
                and ppr.spacing_before == 0
                and ppr.spacing_after == 0)

    def _map_definition_list(
        self, node: DocumentIRDefinitionListNode, parent: DocxASTBodyNode
    ) -> None:
        """
        Map a definition list to a sequence of Normal paragraphs.

        Definition terms are emitted as bold Normal paragraphs.  Definition
        descriptions are emitted as indented Normal paragraphs (indent_left=360).
        """
        for child in node.children:
            if isinstance(child, DocumentIRDefinitionTermNode):
                para = DocxASTParagraphNode()
                ppr = DocxASTParagraphPropertiesNode(style_id=_STYLE_NORMAL)
                para.add_child(ppr)
                runs = self._build_runs(child.children)
                if not runs:
                    run = DocxASTRunNode()
                    rpr = DocxASTRunPropertiesNode(bold=True)
                    run.add_child(rpr)
                    runs = [run]

                else:
                    for r in runs:
                        rpr_node = next(
                            (c for c in r.children if isinstance(c, DocxASTRunPropertiesNode)),
                            None,
                        )
                        if rpr_node is not None:
                            rpr_node.bold = True

                        else:
                            r.children.insert(0, DocxASTRunPropertiesNode(bold=True))

                for r in runs:
                    para.add_child(r)

                parent.add_child(para)

            elif isinstance(child, DocumentIRDefinitionDescriptionNode):
                para = DocxASTParagraphNode()
                ppr = DocxASTParagraphPropertiesNode(
                    style_id=_STYLE_NORMAL,
                    indent_left=360,
                )
                para.add_child(ppr)
                for r in self._build_runs(child.children):
                    para.add_child(r)

                parent.add_child(para)

    def _append_inline_children(
        self,
        children: Sequence[DocumentIRNode],
        para: DocxASTParagraphNode,
    ) -> None:
        """Build runs from inline children and append to a paragraph."""
        for run in self._build_runs(children):
            para.add_child(run)

    def _build_runs(self, children: Sequence[DocumentIRNode]) -> List[DocxASTNode]:
        """
        Convert a list of inline document_ir nodes to DocxASTRunNode instances.

        Consecutive text spans with identical formatting are merged into a
        single run for cleaner output.

        Args:
            children: Inline document_ir nodes (spans, links, images, line breaks).

        Returns:
            List of DocxASTNode instances (runs and hyperlinks interleaved).
        """
        # Collect (bold, italic, strike, code, content_or_special) tuples
        # where content_or_special is either a string or a special marker
        items: List[Tuple] = []
        self._collect_run_items(children, items)

        return self._items_to_runs(items)

    def _collect_run_items(
        self,
        children: Sequence[DocumentIRNode],
        items: List[Tuple],
    ) -> None:
        """
        Recursively collect run items from inline nodes.

        Each item is a tuple:
          ('text', bold, italic, strike, code, superscript, subscript, content)
          ('break',)
          ('image', url, alt_text)
          ('link_text', bold, italic, strike, code, content, url)
        """
        for child in children:
            if isinstance(child, DocumentIRTextSpanNode):
                if child.content:
                    items.append((
                        "text",
                        child.bold,
                        child.italic,
                        child.strikethrough,
                        child.code,
                        child.superscript,
                        child.subscript,
                        child.content,
                    ))

            elif isinstance(child, DocumentIRLineBreakNode):
                items.append(("break",))

            elif isinstance(child, DocumentIRImageNode):
                items.append(("image", child.url, child.alt_text or ""))

            elif isinstance(child, DocumentIRLinkNode):
                # Build a hyperlink node carrying its display runs
                hyperlink = DocxASTHyperlinkNode(url=child.url)
                for link_child in child.children:
                    if isinstance(link_child, DocumentIRTextSpanNode) and link_child.content:
                        run = DocxASTRunNode()
                        rpr = DocxASTRunPropertiesNode(
                            bold=link_child.bold,
                            italic=link_child.italic,
                            strike=link_child.strikethrough,
                            style_id="Hyperlink",
                        )
                        run.add_child(rpr)
                        run.add_child(DocxASTTextNode(content=link_child.content))
                        hyperlink.add_child(run)

                items.append(("hyperlink", hyperlink))

            # Other inline types are silently skipped

    def _items_to_runs(self, items: List[Tuple]) -> List[DocxASTNode]:
        """
        Convert collected run items to DocxASTRunNode instances.

        Consecutive text items with the same formatting are merged.
        """
        runs: List[DocxASTNode] = []

        i = 0
        while i < len(items):
            item = items[i]
            kind = item[0]

            if kind == "text":
                _, bold, italic, strike, code, superscript, subscript, content = item
                # Merge consecutive text items with same formatting
                merged = content
                while (
                    i + 1 < len(items)
                    and items[i + 1][0] == "text"
                    and items[i + 1][1] == bold
                    and items[i + 1][2] == italic
                    and items[i + 1][3] == strike
                    and items[i + 1][4] == code
                    and items[i + 1][5] == superscript
                    and items[i + 1][6] == subscript
                ):
                    i += 1
                    merged += items[i][7]

                run = DocxASTRunNode()
                has_formatting = bold or italic or strike or code or superscript or subscript
                if has_formatting:
                    vert_align: str | None = None
                    if superscript:
                        vert_align = "superscript"

                    elif subscript:
                        vert_align = "subscript"

                    rpr = DocxASTRunPropertiesNode(
                        bold=bold,
                        italic=italic,
                        strike=strike,
                        font_ascii="Consolas" if code else None,
                        font_h_ansi="Consolas" if code else None,
                        vertical_align=vert_align,
                    )
                    run.add_child(rpr)

                run.add_child(DocxASTTextNode(
                    content=merged,
                    preserve_space=merged.startswith(" ") or merged.endswith(" "),
                ))
                runs.append(run)

            elif kind == "hyperlink":
                _, hyperlink_node = item
                runs.append(hyperlink_node)

            elif kind == "break":
                run = DocxASTRunNode()
                run.add_child(DocxASTBreakNode(break_type="textWrapping"))
                runs.append(run)

            elif kind == "image":
                _, url, alt_text = item
                run = DocxASTRunNode()
                run.add_child(DocxASTDrawingNode(
                    resolved_path=url,
                    description=alt_text,
                ))
                runs.append(run)

            i += 1

        return runs
