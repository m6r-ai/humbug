from typing import Dict, List, Optional, Tuple

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
from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
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
    DocxASTTableNode,
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

# numId values for bullet and ordered lists (referenced in numPr)
_NUM_ID_BULLET = "1"
_NUM_ID_ORDERED = "2"

# Maximum nesting depth supported for lists
_MAX_LIST_DEPTH = 9


def doc_ir_to_docx_ast(document: DocIRDocumentNode) -> DocxASTDocumentNode:
    """Convert a doc_ir document into a DOCX AST document.

    This is the public entry point for the write-side mapper.

    The returned DocxASTDocumentNode contains:
    - A DocxASTStylesNode with all styles referenced by the content
    - A DocxASTNumberingNode with bullet and ordered list definitions
    - A DocxASTBodyNode with the full document content

    The AST can then be serialised to a .docx file by the DOCX writer.

    Args:
        document: The DocIRDocumentNode to convert.

    Returns:
        A DocxASTDocumentNode ready for serialisation.
    """
    mapper = _DocIRToDocxASTMapper()
    return mapper.map(document)


class _DocIRToDocxASTMapper:
    """Maps a doc_ir tree to a DOCX AST tree.

    Generates the styles and numbering definitions required by the content,
    then maps each doc_ir node to the appropriate DOCX AST structure.
    """

    def map(self, document: DocIRDocumentNode) -> DocxASTDocumentNode:
        """Perform the full mapping.

        Args:
            document: The source DocIRDocumentNode.

        Returns:
            A populated DocxASTDocumentNode.
        """
        docx_doc = DocxASTDocumentNode(source_path=document.source_path)

        # Always include styles and numbering so the output is self-contained
        docx_doc.add_child(self._build_styles_node())
        docx_doc.add_child(self._build_numbering_node())

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
        code_rpr = DocxASTRunPropertiesNode(font_ascii="Courier New", font_hAnsi="Courier New")
        code.add_child(code_rpr)
        styles.add_child(code)

        # Blockquote
        bq = DocxASTStyleNode(
            style_type="paragraph",
            style_id=_STYLE_BLOCKQUOTE,
            name="Blockquote",
            based_on=_STYLE_NORMAL,
        )
        bq_ppr = DocxASTParagraphPropertiesNode(indent_left=720)
        bq.add_child(bq_ppr)
        bq_rpr = DocxASTRunPropertiesNode(italic=True)
        bq.add_child(bq_rpr)
        styles.add_child(bq)

        return styles

    def _build_numbering_node(self) -> DocxASTNumberingNode:
        """Build a DocxASTNumberingNode with bullet and ordered list definitions.

        Defines two abstractNum entries (bullet and decimal) and two num
        instances referencing them.  Each definition covers _MAX_LIST_DEPTH
        indent levels.
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
                indent_left=720 * (i + 1),
                indent_hanging=360,
            )
            bullet_abstract.add_child(lvl)

        numbering.add_child(bullet_abstract)

        # abstractNum 1: decimal (ordered) list
        ordered_abstract = DocxASTAbstractNumNode(abstract_num_id="1")
        ordered_abstract.multi_level_type = "multilevel"
        ordered_fmts = [
            ("decimal", "%1."),
            ("lowerLetter", "%2."),
            ("lowerRoman", "%3."),
        ]
        for i in range(_MAX_LIST_DEPTH):
            fmt, text = ordered_fmts[i % 3]
            lvl = DocxASTNumLevelNode(
                ilvl=i,
                start=1,
                num_fmt=fmt,
                lvl_text=text,
                lvl_jc="left",
                indent_left=720 * (i + 1),
                indent_hanging=360,
            )
            ordered_abstract.add_child(lvl)

        numbering.add_child(ordered_abstract)

        # num 1: bullet instance

        numbering.add_child(DocxASTNumNode(num_id=_NUM_ID_BULLET, abstract_num_id="0"))
        # num 2: ordered instance
        numbering.add_child(DocxASTNumNode(num_id=_NUM_ID_ORDERED, abstract_num_id="1"))

        return numbering

    def _map_block(self, node: DocIRNode, parent: DocxASTBodyNode) -> None:
        """Map a block-level doc_ir node and append result(s) to parent.

        Args:
            node: The doc_ir block node to map.
            parent: The DocxASTBodyNode to append generated paragraphs/tables to.
        """
        if isinstance(node, DocIRHeadingNode):
            parent.add_child(self._map_heading(node))

        elif isinstance(node, DocIRParagraphNode):
            para = self._map_paragraph_node(node, style_id=_STYLE_NORMAL)
            if para is not None:
                parent.add_child(para)

        elif isinstance(node, DocIRBlockquoteNode):
            self._map_blockquote(node, parent)

        elif isinstance(node, DocIRCodeBlockNode):
            self._map_code_block(node, parent)

        elif isinstance(node, DocIRUnorderedListNode):
            self._map_list(node, parent, num_id=_NUM_ID_BULLET, depth=0)

        elif isinstance(node, DocIROrderedListNode):
            self._map_list(node, parent, num_id=_NUM_ID_ORDERED, depth=0)

        elif isinstance(node, DocIRTableNode):
            parent.add_child(self._map_table(node))

        elif isinstance(node, DocIRHorizontalRuleNode):
            parent.add_child(self._make_horizontal_rule_para())

        # Unknown block types are silently skipped

    def _map_heading(self, node: DocIRHeadingNode) -> DocxASTParagraphNode:
        """Map a heading to a styled paragraph."""
        style_id = _STYLE_HEADINGS.get(node.level, _STYLE_HEADINGS[6])
        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(style_id=style_id)
        para.add_child(ppr)
        self._append_inline_children(node.children, para)
        return para

    def _map_paragraph_node(
        self,
        node: DocIRParagraphNode,
        style_id: str = _STYLE_NORMAL,
    ) -> Optional[DocxASTParagraphNode]:
        """Map a paragraph to a styled paragraph.

        Returns None if the paragraph would be empty.
        """
        runs = self._build_runs(node.children)
        if not runs:
            return None

        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(style_id=style_id)
        para.add_child(ppr)
        for run in runs:
            para.add_child(run)

        return para

    def _map_blockquote(
        self, node: DocIRBlockquoteNode, parent: DocxASTBodyNode
    ) -> None:
        """Map a blockquote — each child paragraph gets the Blockquote style."""
        for child in node.children:
            if isinstance(child, DocIRParagraphNode):
                para = self._map_paragraph_node(child, style_id=_STYLE_BLOCKQUOTE)
                if para is not None:
                    parent.add_child(para)

            else:
                # Nested blocks inside blockquote: recurse
                self._map_block(child, parent)

    def _map_code_block(
        self, node: DocIRCodeBlockNode, parent: DocxASTBodyNode,
        indent_left: Optional[int] = None,
    ) -> None:
        """Map a code block — one paragraph per line, all with CodeBlock style."""
        lines = node.content.split("\n")
        # Remove trailing empty line that split() often produces
        if lines and lines[-1] == "":
            lines = lines[:-1]

        if not lines:
            # Emit one empty code paragraph to preserve the block
            lines = [""]

        paras: List[DocxASTParagraphNode] = []
        for line in lines:
            para = DocxASTParagraphNode()
            ppr = DocxASTParagraphPropertiesNode(
                style_id=_STYLE_CODE_BLOCK,
                indent_left=indent_left,
            )
            para.add_child(ppr)
            if line:
                run = DocxASTRunNode()
                run.add_child(DocxASTTextNode(content=line, preserve_space=True))
                para.add_child(run)
            paras.append(para)

        # Add spacing after the last line so there is a visible gap between the
        # code block and whatever follows it (the CodeBlock style sets after=0)
        last_ppr = next(c for c in paras[-1].children if isinstance(c, DocxASTParagraphPropertiesNode))
        last_ppr.spacing_after = 160

        for para in paras:
            parent.add_child(para)

    def _map_list(
        self,
        node: DocIRNode,
        parent: DocxASTBodyNode,
        num_id: str,
        depth: int,
    ) -> None:
        """Recursively map a list node, emitting paragraphs with numPr.

        Args:
            node: DocIRUnorderedListNode or DocIROrderedListNode.
            parent: The body node to append paragraphs to.
            num_id: The numId to reference in numPr.
            depth: Current nesting depth (0-based ilvl).
        """
        for child in node.children:
            if isinstance(child, DocIRListItemNode):
                self._map_list_item(child, parent, num_id=num_id, depth=depth)

    def _map_list_item(
        self,
        item: DocIRListItemNode,
        parent: DocxASTBodyNode,
        num_id: str,
        depth: int,
    ) -> None:
        """Map a list item.

        Inline content (wrapped in an implicit paragraph) is emitted as a
        numbered paragraph.  Subsequent paragraphs within the same list item
        (continuation paragraphs) are indented to the same level but carry no
        list marker.  Nested lists are recursed with depth+1.
        """
        is_first_para = True
        indent_left = 720 * (depth + 1)

        for child in item.children:
            if isinstance(child, DocIRParagraphNode):
                runs = self._build_runs(child.children)
                para = DocxASTParagraphNode()
                if is_first_para:
                    # First paragraph: carries the bullet/number marker
                    ppr = DocxASTParagraphPropertiesNode(style_id=_STYLE_NORMAL)
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
                    )
                para.add_child(ppr)
                for run in runs:
                    para.add_child(run)

                parent.add_child(para)

            elif isinstance(child, DocIRUnorderedListNode):
                is_first_para = False
                self._map_list(child, parent, num_id=_NUM_ID_BULLET, depth=depth + 1)

            elif isinstance(child, DocIROrderedListNode):
                is_first_para = False
                self._map_list(child, parent, num_id=_NUM_ID_ORDERED, depth=depth + 1)

            else:
                # Other block children inside a list item — handle with list
                # context so indentation is preserved
                is_first_para = False
                if isinstance(child, DocIRCodeBlockNode):
                    # Use the list text indent directly — explicit indent_left
                    # overrides the CodeBlock style's own indent entirely
                    self._map_code_block(child, parent, indent_left=indent_left)

                else:
                    self._map_block(child, parent)

    def _map_table(self, node: DocIRTableNode) -> DocxASTTableNode:
        """Map a table node."""
        table = DocxASTTableNode()

        for child in node.children:
            if isinstance(child, DocIRTableHeaderNode):
                for row_node in child.children:
                    if isinstance(row_node, DocIRTableRowNode):
                        table.add_child(self._map_table_row(row_node, is_header=True))

            elif isinstance(child, DocIRTableBodyNode):
                for row_node in child.children:
                    if isinstance(row_node, DocIRTableRowNode):
                        table.add_child(self._map_table_row(row_node, is_header=False))

        return table

    def _map_table_row(
        self, node: DocIRTableRowNode, is_header: bool
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
            if isinstance(child, DocIRTableCellNode):
                row.add_child(self._map_table_cell(child, is_header=is_header))

        return row

    def _map_table_cell(
        self, node: DocIRTableCellNode, is_header: bool
    ) -> DocxASTTableCellNode:
        """Map a table cell."""
        from docx.docx_ast_node import DocxASTTableCellPropertiesNode
        cell = DocxASTTableCellNode()
        cpr = DocxASTTableCellPropertiesNode(width_type="auto")
        cell.add_child(cpr)

        # Map cell content
        for child in node.children:
            if isinstance(child, DocIRParagraphNode):
                justification = self._alignment_to_jc(node.alignment)
                para = DocxASTParagraphNode()
                ppr = DocxASTParagraphPropertiesNode(
                    style_id=_STYLE_NORMAL,
                    justification=justification,
                )
                para.add_child(ppr)
                runs = self._build_runs(child.children)
                for run in runs:
                    para.add_child(run)

                cell.add_child(para)

            elif isinstance(child, DocIRTableNode):
                cell.add_child(self._map_table(child))

            elif isinstance(child, DocIRHeadingNode):
                cell.add_child(self._map_heading(child))

            elif isinstance(child, DocIRCodeBlockNode):
                # Inline code block in cell: emit as single para
                para = DocxASTParagraphNode()
                ppr = DocxASTParagraphPropertiesNode(style_id=_STYLE_CODE_BLOCK)
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
    def _alignment_to_jc(alignment: str) -> Optional[str]:
        """Convert a doc_ir alignment string to a DOCX justification value."""
        mapping = {"left": "left", "center": "center", "right": "right"}
        return mapping.get(alignment)

    def _make_horizontal_rule_para(self) -> DocxASTParagraphNode:
        """Create a paragraph with a bottom border to represent a horizontal rule."""
        para = DocxASTParagraphNode()
        ppr = DocxASTParagraphPropertiesNode(style_id=_STYLE_NORMAL)
        para.add_child(ppr)
        return para

    def _append_inline_children(
        self,
        children: List[DocIRNode],
        para: DocxASTParagraphNode,
    ) -> None:
        """Build runs from inline children and append to a paragraph."""
        for run in self._build_runs(children):
            para.add_child(run)

    def _build_runs(self, children: List[DocIRNode]) -> List[DocxASTNode]:
        """Convert a list of inline doc_ir nodes to DocxASTRunNode instances.

        Consecutive text spans with identical formatting are merged into a
        single run for cleaner output.

        Args:
            children: Inline doc_ir nodes (spans, links, images, line breaks).

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
        children: List[DocIRNode],
        items: List[Tuple],
    ) -> None:
        """Recursively collect run items from inline nodes.

        Each item is a tuple:
          ('text', bold, italic, strike, code, content)
          ('break',)
          ('image', url, alt_text)
          ('link_text', bold, italic, strike, code, content, url)
        """
        for child in children:
            if isinstance(child, DocIRTextSpanNode):
                if child.content:
                    items.append((
                        "text",
                        child.bold,
                        child.italic,
                        child.strikethrough,
                        child.code,
                        child.content,
                    ))

            elif isinstance(child, DocIRLineBreakNode):
                items.append(("break",))

            elif isinstance(child, DocIRImageNode):
                items.append(("image", child.url, child.alt_text or ""))

            elif isinstance(child, DocIRLinkNode):
                # Build a hyperlink node carrying its display runs
                hyperlink = DocxASTHyperlinkNode(url=child.url)
                for link_child in child.children:
                    if isinstance(link_child, DocIRTextSpanNode) and link_child.content:
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
        """Convert collected run items to DocxASTRunNode instances.

        Consecutive text items with the same formatting are merged.
        """
        runs: List[DocxASTNode] = []

        i = 0
        while i < len(items):
            item = items[i]
            kind = item[0]

            if kind == "text":
                _, bold, italic, strike, code, content = item
                # Merge consecutive text items with same formatting
                merged = content
                while (
                    i + 1 < len(items)
                    and items[i + 1][0] == "text"
                    and items[i + 1][1] == bold
                    and items[i + 1][2] == italic
                    and items[i + 1][3] == strike
                    and items[i + 1][4] == code
                ):
                    i += 1
                    merged += items[i][5]

                run = DocxASTRunNode()
                if bold or italic or strike or code:
                    rpr = DocxASTRunPropertiesNode(
                        bold=bold,
                        italic=italic,
                        strike=strike,
                        font_ascii="Courier New" if code else None,
                        font_hAnsi="Courier New" if code else None,
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
