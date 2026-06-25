from typing import Dict, List, Tuple

from docx.docx_ast_node import (
    DocxASTAbstractNumNode,
    DocxASTBodyNode,
    DocxASTBreakNode,
    DocxASTDocumentNode,
    DocxASTDrawingNode,
    DocxASTLastRenderedPageBreakNode,
    DocxASTNode,
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
    DocxASTTabNode,
    DocxASTTableCellNode,
    DocxASTTableNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTextNode,
)
from document_ir import (
    DocumentIRBlockquoteNode,
    DocumentIRCodeBlockNode,
    DocumentIRDocumentNode,
    DocumentIRHeadingNode,
    DocumentIRImageNode,
    DocumentIRLineBreakNode,
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
    mime_type_for_extension,
)

# Style IDs / names that map to heading levels 1-6.
# Keys are lowercased style names or IDs; values are heading levels.
_HEADING_STYLE_NAMES: Dict[str, int] = {
    "heading 1": 1, "heading1": 1, "h1": 1,
    "heading 2": 2, "heading2": 2, "h2": 2,
    "heading 3": 3, "heading3": 3, "h3": 3,
    "heading 4": 4, "heading4": 4, "h4": 4,
    "heading 5": 5, "heading5": 5, "h5": 5,
    "heading 6": 6, "heading6": 6, "h6": 6,
}

# Style names (lowercased) that indicate code / preformatted content.
_CODE_STYLE_KEYWORDS = {"code", "codeblock", "code block", "preformatted", "verbatim"}

# Style names (lowercased) that indicate blockquote content.
_BLOCKQUOTE_STYLE_KEYWORDS = {"blockquote", "block quote", "quote", "quotation"}

# Monospace font names — used as a heuristic for code paragraphs when
# no explicit code style is present.
_MONOSPACE_FONTS = {
    "courier new", "courier", "consolas", "lucida console",
    "monaco", "menlo", "source code pro", "fira code",
}

# Font size threshold (in half-points) above which a bold paragraph with
# no style is treated as a heading.  24 half-points = 12pt (body text).
_HEADING_SIZE_THRESHOLD = 24

# Mapping from approximate half-point size to heading level for the
# direct-formatting heuristic.  Sizes are approximate — we use >=.
_SIZE_TO_HEADING_LEVEL: List[Tuple[int, int]] = [
    (40, 1),  # >= 40 half-points (20pt) → H1
    (32, 2),  # >= 32 half-points (16pt) → H2
    (28, 3),  # >= 28 half-points (14pt) → H3
    (24, 4),  # >= 24 half-points (12pt) → H4
]


def docx_ast_to_document_ir(document: DocxASTDocumentNode) -> DocumentIRDocumentNode:
    """Convert a DOCX AST document into a document_ir document.

    This is the public entry point for the mapper.

    Args:
        document: The root DocxASTDocumentNode produced by DocxASTParser.

    Returns:
        A DocumentIRDocumentNode containing the format-agnostic intermediate
        representation of the document content.
    """
    mapper = _DocxToDocumentIRMapper(document)
    return mapper.map()


class _ResolvedStyle:
    """Fully resolved properties for a single style after inheritance."""

    __slots__ = (
        "style_id", "style_type", "name",
        "heading_level",
        "is_code", "is_blockquote", "num_id", "ilvl",
        "bold", "italic", "sz",
        "font_ascii",
    )

    def __init__(self) -> None:
        self.style_id: str = ""
        self.style_type: str = "paragraph"
        self.name: str = ""
        self.heading_level: int | None = None
        self.is_code: bool = False
        self.is_blockquote: bool = False
        self.num_id: str | None = None
        self.ilvl: int = 0
        self.bold: bool = False
        self.italic: bool = False
        self.sz: int | None = None
        self.font_ascii: str | None = None


class _NumLevel:
    """Resolved numbering level for a specific numId + ilvl combination."""

    __slots__ = ("num_fmt", "start")

    def __init__(self, num_fmt: str = "bullet", start: int = 1) -> None:
        self.num_fmt = num_fmt
        self.start = start


class _DocxToDocumentIRMapper:
    """Maps a DOCX AST to a document_ir tree.

    Handles:
    - Style resolution with inheritance chains
    - Numbering resolution (numId → abstractNum → level)
    - Semantic classification of paragraphs (heading, code, blockquote, list)
    - Grouping of consecutive list paragraphs into list trees
    - Inline content assembly with formatting flag accumulation
    """

    def __init__(self, document: DocxASTDocumentNode) -> None:
        self._document = document
        # Built during map(): style_id → _ResolvedStyle
        self._styles: Dict[str, _ResolvedStyle] = {}

        # Built during map(): (num_id, ilvl) → _NumLevel
        self._num_levels: Dict[Tuple[str, int], _NumLevel] = {}

    def map(self) -> DocumentIRDocumentNode:
        """Perform the full mapping and return the document_ir document."""
        # Build supporting indexes from styles and numbering children
        for child in self._document.children:
            if isinstance(child, DocxASTStylesNode):
                self._build_style_index(child)

            elif isinstance(child, DocxASTNumberingNode):
                self._build_numbering_index(child)

        document_ir = DocumentIRDocumentNode(source_path=self._document.source_path)

        body = next(
            (c for c in self._document.children if isinstance(c, DocxASTBodyNode)),
            None,
        )
        if body is None:
            return document_ir

        self._map_block_sequence(list(body.children), document_ir)
        return document_ir

    def _build_style_index(self, styles_node: DocxASTStylesNode) -> None:
        """Build a flat map of style_id → _ResolvedStyle from the styles tree.

        Resolves inheritance chains so each entry holds the effective
        properties after applying basedOn ancestors.
        """
        # First pass: collect raw DocxASTStyleNode objects by style_id
        raw: Dict[str, DocxASTStyleNode] = {}
        for child in styles_node.children:
            if isinstance(child, DocxASTStyleNode):
                raw[child.style_id] = child

        # Second pass: resolve each style with inheritance
        for style_id in raw:
            if style_id not in self._styles:
                self._resolve_style(style_id, raw, set())

    def _resolve_style(
        self,
        style_id: str,
        raw: Dict[str, DocxASTStyleNode],
        visiting: set,
    ) -> _ResolvedStyle:
        """Resolve a style by walking its basedOn chain.

        Args:
            style_id: The style to resolve.
            raw: Map of all raw style nodes.
            visiting: Set of style IDs currently being resolved (cycle guard).

        Returns:
            The resolved _ResolvedStyle, also stored in self._styles.
        """
        if style_id in self._styles:
            return self._styles[style_id]

        resolved = _ResolvedStyle()
        resolved.style_id = style_id

        node = raw.get(style_id)
        if node is None:
            self._styles[style_id] = resolved
            return resolved

        resolved.style_type = node.style_type
        resolved.name = node.name

        # Resolve parent first (cycle-safe)
        if node.based_on and node.based_on not in visiting:
            parent = self._resolve_style(node.based_on, raw, visiting | {style_id})
            # Inherit from parent
            resolved.heading_level = parent.heading_level
            resolved.is_code = parent.is_code
            resolved.is_blockquote = parent.is_blockquote
            resolved.num_id = parent.num_id
            resolved.ilvl = parent.ilvl
            resolved.bold = parent.bold
            resolved.italic = parent.italic
            resolved.sz = parent.sz
            resolved.font_ascii = parent.font_ascii

        # Apply this style's own properties (override parent)
        name_lower = node.name.lower()
        id_lower = node.style_id.lower()

        # Heading level from name/id
        for key, level in _HEADING_STYLE_NAMES.items():
            if key in (name_lower, id_lower):
                resolved.heading_level = level
                break

        # Code / blockquote from name keywords
        for kw in _CODE_STYLE_KEYWORDS:
            if kw in name_lower or kw in id_lower:
                resolved.is_code = True
                break

        for kw in _BLOCKQUOTE_STYLE_KEYWORDS:
            if kw in name_lower or kw in id_lower:
                resolved.is_blockquote = True
                break

        # Run properties from this style's <w:rPr>
        rpr = next((c for c in node.children if isinstance(c, DocxASTRunPropertiesNode)), None)
        if rpr is not None:
            if rpr.bold:
                resolved.bold = True

            if rpr.italic:
                resolved.italic = True

            if rpr.sz is not None:
                resolved.sz = rpr.sz

            if rpr.font_ascii is not None:
                resolved.font_ascii = rpr.font_ascii

        # Paragraph outline_level from <w:pPr>
        ppr = next((c for c in node.children if isinstance(c, DocxASTParagraphPropertiesNode)), None)
        if ppr is not None:
            if ppr.outline_level is not None:
                # outline_level 0-5 → heading 1-6
                resolved.heading_level = ppr.outline_level + 1

            # Numbering properties from this style's <w:pPr><w:numPr>
            num_pr = next(
                (c for c in ppr.children if isinstance(c, DocxASTNumberingPropertiesNode)),
                None,
            )
            if num_pr is not None:
                resolved.num_id = num_pr.num_id
                resolved.ilvl = num_pr.ilvl

        self._styles[style_id] = resolved
        return resolved

    def _get_style(self, style_id: str | None) -> _ResolvedStyle | None:
        """Look up a resolved style by ID, returning None if not found."""
        if not style_id:
            return None
        return self._styles.get(style_id)

    def _build_numbering_index(self, numbering_node: DocxASTNumberingNode) -> None:
        """Build a map from (num_id, ilvl) → _NumLevel."""
        # abstract_num_id → {ilvl → DocxASTNumLevelNode}
        abstract_levels: Dict[str, Dict[int, DocxASTNumLevelNode]] = {}

        for child in numbering_node.children:
            if isinstance(child, DocxASTAbstractNumNode):
                levels: Dict[int, DocxASTNumLevelNode] = {}
                for lvl in child.children:
                    if isinstance(lvl, DocxASTNumLevelNode):
                        levels[lvl.ilvl] = lvl

                abstract_levels[child.abstract_num_id] = levels

        # num_id → abstract_num_id
        num_to_abstract: Dict[str, str] = {}
        for child in numbering_node.children:
            if isinstance(child, DocxASTNumNode):
                num_to_abstract[child.num_id] = child.abstract_num_id

        # Build final index
        for num_id, abstract_id in num_to_abstract.items():
            levels = abstract_levels.get(abstract_id, {})
            for ilvl, lvl_node in levels.items():
                self._num_levels[(num_id, ilvl)] = _NumLevel(
                    num_fmt=lvl_node.num_fmt,
                    start=lvl_node.start,
                )

    def _get_num_level(
        self, num_id: str, ilvl: int
    ) -> _NumLevel | None:
        """Look up a resolved numbering level."""
        return self._num_levels.get((num_id, ilvl))

    def _map_block_sequence(
        self,
        nodes: List[DocxASTNode],
        target: DocumentIRNode,
    ) -> None:
        """Map a sequence of block-level DOCX nodes into target.

        Handles list grouping: consecutive paragraphs sharing the same
        numId are collected and built into a list tree before appending.

        Args:
            nodes: Sequence of DocxASTNode (paragraphs, tables, etc.).
            target: The document_ir node to append mapped children to.
        """
        i = 0
        while i < len(nodes):
            node = nodes[i]

            if isinstance(node, DocxASTParagraphNode):
                num_pr = self._get_num_pr(node)
                if num_pr is not None and not self._para_is_heading(node):
                    # Start of a list — consume all consecutive list paragraphs
                    list_paras: List[DocxASTParagraphNode] = []
                    while i < len(nodes):
                        next_node = nodes[i]
                        if not isinstance(next_node, DocxASTParagraphNode):
                            break

                        np = self._get_num_pr(next_node)
                        if np is None or self._para_is_heading(next_node):
                            break

                        list_paras.append(next_node)
                        i += 1

                    list_nodes = self._build_list_tree(list_paras)
                    for list_node in list_nodes:
                        target.add_child(list_node)

                    continue

                if self._para_is_code(node):
                    # Start of a code block — consume all consecutive code paragraphs
                    code_paras: List[DocxASTParagraphNode] = []
                    while i < len(nodes):
                        next_node = nodes[i]
                        if not isinstance(next_node, DocxASTParagraphNode):
                            break

                        if self._get_num_pr(next_node) is not None:
                            break

                        if not self._para_is_code(next_node):
                            break

                        code_paras.append(next_node)
                        i += 1

                    target.add_child(self._merge_code_paragraphs(code_paras))
                    continue

                mapped = self._map_paragraph(node)
                if mapped is not None:
                    target.add_child(mapped)

            elif isinstance(node, DocxASTTableNode):
                if self._is_single_cell_table(node):
                    self._unwrap_single_cell_table(node, target)

                else:
                    target.add_child(self._map_table(node))

            # Section properties and other nodes are silently skipped
            i += 1

    def _para_is_code(self, para: DocxASTParagraphNode) -> bool:
        """Return True if the paragraph is classified as a code block.

        Mirrors the classification logic in _map_paragraph: a paragraph is
        code if its resolved style has is_code set, or if all its runs use
        a monospace font.
        """
        ppr = next(
            (c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode)),
            None,
        )
        resolved = self._get_style(ppr.style_id if ppr else None)

        if resolved and resolved.is_code:
            return True

        return self._para_is_monospace(para, resolved)

    def _merge_code_paragraphs(
        self, paras: List[DocxASTParagraphNode],
    ) -> DocumentIRCodeBlockNode:
        """Merge a sequence of code paragraphs into a single code block node.

        Each paragraph represents one line of the original code block.  The
        lines are joined with newlines to reconstruct the multi-line content.
        """
        lines = [self._extract_plain_text(p) for p in paras]
        return DocumentIRCodeBlockNode(
            language="",
            content="\n".join(lines),
        )

    def _get_num_pr(
        self, para: DocxASTParagraphNode
    ) -> DocxASTNumberingPropertiesNode | None:
        """Return the numbering properties of a paragraph, or None.

        Checks the paragraph's own ``<w:numPr>`` first.  If the paragraph
        does not carry numbering directly, falls back to the numbering
        inherited from its paragraph style.  This mirrors Word's behaviour
        where a style such as "List Bullet" can supply ``numPr`` that
        applies to every paragraph using that style.
        """
        ppr = next(
            (c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode)),
            None,
        )
        if ppr is not None:
            direct = next(
                (c for c in ppr.children if isinstance(c, DocxASTNumberingPropertiesNode)),
                None,
            )
            if direct is not None:
                return direct

        # Fall back to numbering inherited from the paragraph's style
        resolved = self._get_style(ppr.style_id if ppr else None)
        if resolved and resolved.num_id is not None:
            return DocxASTNumberingPropertiesNode(
                num_id=resolved.num_id, ilvl=resolved.ilvl,
            )

        return None

    def _para_is_heading(self, para: DocxASTParagraphNode) -> bool:
        """Return True if the paragraph resolves to a heading via style or outline level.

        Used to prevent numbered heading paragraphs from being consumed as list
        items: Word documents commonly attach auto-numbering to heading styles,
        but the semantic intent is a heading, not a list.
        """
        ppr = next(
            (c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode)),
            None,
        )
        if ppr is None:
            return False

        resolved = self._get_style(ppr.style_id)
        if resolved and resolved.heading_level is not None:
            return True

        return ppr.outline_level is not None

    def _build_list_tree(
        self, paras: List[DocxASTParagraphNode]
    ) -> List[DocumentIRNode]:
        """Build a nested list tree from a flat sequence of list paragraphs.

        The paragraphs are grouped by ilvl (indent level).  When ilvl
        increases we start a nested list; when it decreases we close
        the nested list and return to the parent.

        Args:
            paras: Flat list of paragraphs all having numPr set.

        Returns:
            A list of root list nodes.  Normally a single list, but when
            ilvl values drop below the root level mid-sequence, multiple
            separate root lists are produced.
        """
        if not paras:
            return []

        # Stack of (list_node, current_item_node) pairs.
        # list_node is the DocumentIRUnorderedListNode/DocumentIROrderedListNode.
        # current_item_node is the DocumentIRListItemNode currently being built.
        # The third element is the ilvl at which this list lives, so that
        # nesting decisions are based on actual ilvl values rather than
        # assuming they are contiguous and zero-based.
        stack: List[Tuple[DocumentIRNode, DocumentIRListItemNode, int]] = []

        # Completed root lists — normally just one, but if ilvl drops below
        # the root mid-sequence, we save the old root and start a new one.
        roots: List[DocumentIRNode] = []

        def _make_list(num_id: str, ilvl: int) -> DocumentIRNode:
            num_level = self._get_num_level(num_id, ilvl)
            if num_level and num_level.num_fmt == "decimal":
                return DocumentIROrderedListNode(start=num_level.start)

            if num_level and num_level.num_fmt in (
                "lowerLetter", "upperLetter", "lowerRoman", "upperRoman",
            ):
                return DocumentIROrderedListNode(start=num_level.start)

            return DocumentIRUnorderedListNode()

        for para in paras:
            num_pr = self._get_num_pr(para)
            if num_pr is None:
                continue

            num_id = num_pr.num_id
            ilvl = num_pr.ilvl

            if not stack:
                # Start the root list
                root_list = _make_list(num_id, ilvl)
                roots.append(root_list)
                item = DocumentIRListItemNode()
                root_list.add_child(item)
                stack.append((root_list, item, ilvl))
                self._fill_list_item(item, para)
                continue

            current_ilvl = stack[-1][2]

            if ilvl > current_ilvl:
                # Nest deeper: create a new sub-list inside the current item
                _, parent_item, _ = stack[-1]
                sub_list = _make_list(num_id, ilvl)
                parent_item.add_child(sub_list)
                item = DocumentIRListItemNode()
                sub_list.add_child(item)
                stack.append((sub_list, item, ilvl))
                self._fill_list_item(item, para)

            elif ilvl < current_ilvl:
                # Pop back up to the right level
                while stack and stack[-1][2] > ilvl:
                    stack.pop()

                # If we've popped back to exactly the right ilvl, add a sibling.
                # If the target ilvl doesn't exist on the stack (non-contiguous
                # levels), create a new sub-list at this level under the
                # nearest shallower ancestor.  If the stack is empty (the
                # current ilvl is shallower than the root), start a new root
                # list — this happens when consecutive list paragraphs are
                # grouped together but their ilvl values don't start at the
                # shallowest level seen so far.
                if not stack:
                    root_list = _make_list(num_id, ilvl)
                    roots.append(root_list)
                    item = DocumentIRListItemNode()
                    root_list.add_child(item)
                    stack.append((root_list, item, ilvl))
                    self._fill_list_item(item, para)

                elif stack[-1][2] == ilvl:
                    current_list, _, _ = stack[-1]
                    item = DocumentIRListItemNode()
                    current_list.add_child(item)
                    stack[-1] = (current_list, item, ilvl)
                    self._fill_list_item(item, para)

                else:
                    _, parent_item, _ = stack[-1]
                    sub_list = _make_list(num_id, ilvl)
                    parent_item.add_child(sub_list)
                    item = DocumentIRListItemNode()
                    sub_list.add_child(item)
                    stack.append((sub_list, item, ilvl))
                    self._fill_list_item(item, para)

            else:
                # Same level: add a new sibling item
                current_list, _, _ = stack[-1]
                item = DocumentIRListItemNode()
                current_list.add_child(item)
                stack[-1] = (current_list, item, ilvl)
                self._fill_list_item(item, para)

        return roots

    def _fill_list_item(
        self, item: DocumentIRListItemNode, para: DocxASTParagraphNode
    ) -> None:
        """Fill a DocumentIRListItemNode with the inline content of a paragraph."""
        para_node = DocumentIRParagraphNode()
        self._append_inline_content(para, para_node)
        if para_node.children:
            item.add_child(para_node)

    def _map_paragraph(
        self, para: DocxASTParagraphNode
    ) -> DocumentIRNode | None:
        """Map a single paragraph to the appropriate document_ir node.

        Classification order:
        1. Style-based: heading, code, blockquote
        2. Outline level from paragraph properties
        3. Direct formatting heuristics (bold + large font → heading,
           monospace font → code)
        4. Default: paragraph

        Args:
            para: The DocxASTParagraphNode to map.

        Returns:
            A document_ir node, or None if the paragraph is empty and would
            produce no content.
        """
        ppr = next(
            (c for c in para.children if isinstance(c, DocxASTParagraphPropertiesNode)),
            None,
        )

        style_id = ppr.style_id if ppr else None
        resolved = self._get_style(style_id)

        # Heading via style
        if resolved and resolved.heading_level is not None:
            heading = DocumentIRHeadingNode(level=resolved.heading_level)
            self._append_inline_content(para, heading)
            return heading if heading.children else None

        # Heading via outline_level in paragraph properties
        if ppr and ppr.outline_level is not None:
            level = ppr.outline_level + 1  # outline 0 → H1
            heading = DocumentIRHeadingNode(level=level)
            self._append_inline_content(para, heading)
            return heading if heading.children else None

        # Code block via style
        if resolved and resolved.is_code:
            content = self._extract_plain_text(para)
            return DocumentIRCodeBlockNode(language="", content=content)

        # Blockquote via style
        if resolved and resolved.is_blockquote:
            bq = DocumentIRBlockquoteNode()
            inner = DocumentIRParagraphNode()
            self._append_inline_content(para, inner)
            if inner.children:
                bq.add_child(inner)
            return bq if bq.children else None

        # Heuristic: monospace font → code
        if self._para_is_monospace(para, resolved):
            content = self._extract_plain_text(para)
            return DocumentIRCodeBlockNode(language="", content=content)

        # Heuristic: bold + large font → heading
        heading_level = self._infer_heading_level(para, resolved)
        if heading_level is not None:
            heading = DocumentIRHeadingNode(level=heading_level)
            self._append_inline_content(para, heading)
            return heading if heading.children else None

        # Default: paragraph
        node = DocumentIRParagraphNode()
        self._append_inline_content(para, node)
        return node if node.children else None

    def _para_is_monospace(
        self,
        para: DocxASTParagraphNode,
        resolved: _ResolvedStyle | None,
    ) -> bool:
        """Return True if all text runs in the paragraph use a monospace font."""
        # Check style-level font first
        if resolved and resolved.font_ascii:
            if resolved.font_ascii.lower() in _MONOSPACE_FONTS:
                return True

        # Check direct run-level fonts
        runs = [c for c in para.children if isinstance(c, DocxASTRunNode)]
        if not runs:
            return False

        for run in runs:
            rpr = next(
                (c for c in run.children if isinstance(c, DocxASTRunPropertiesNode)),
                None,
            )
            if rpr is None:
                return False

            font = (rpr.font_ascii or "").lower()
            if font not in _MONOSPACE_FONTS:
                return False

        return True

    def _infer_heading_level(
        self,
        para: DocxASTParagraphNode,
        resolved: _ResolvedStyle | None,
    ) -> int | None:
        """Infer a heading level from direct formatting (bold + large font).

        Returns None if the paragraph doesn't look like a heading.
        """
        # Collect run properties across all runs
        all_bold = True
        max_sz: int | None = None

        runs = [c for c in para.children if isinstance(c, DocxASTRunNode)]
        if not runs:
            return None

        for run in runs:
            # Skip runs with no text content
            has_text = any(isinstance(c, DocxASTTextNode) for c in run.children)
            if not has_text:
                continue

            rpr = next(
                (c for c in run.children if isinstance(c, DocxASTRunPropertiesNode)),
                None,
            )
            if rpr is None:
                # No explicit properties — check if style provides bold
                if resolved and not resolved.bold:
                    all_bold = False
                continue

            if not rpr.bold:
                # Check style fallback
                if not (resolved and resolved.bold):
                    all_bold = False

            sz = rpr.sz
            if sz is None and resolved:
                sz = resolved.sz

            if sz is not None:
                if max_sz is None or sz > max_sz:
                    max_sz = sz

        if not all_bold:
            return None

        if max_sz is None or max_sz <= _HEADING_SIZE_THRESHOLD:
            return None

        # Map size to heading level
        for threshold, level in _SIZE_TO_HEADING_LEVEL:
            if max_sz >= threshold:
                return level

        return None

    def _is_single_cell_table(self, table: DocxASTTableNode) -> bool:
        """Return True if the table has exactly one row containing exactly one cell."""
        rows = [c for c in table.children if isinstance(c, DocxASTTableRowNode)]
        if len(rows) != 1:
            return False

        cells = [c for c in rows[0].children if isinstance(c, DocxASTTableCellNode)]
        return len(cells) == 1

    def _unwrap_single_cell_table(
        self, table: DocxASTTableNode, target: DocumentIRNode
    ) -> None:
        """Emit the content of a 1×1 table's cell directly into target.

        A single-cell table is a Word layout artefact (used for image frames,
        text boxes, callouts, etc.) with no semantic table meaning.  Rather
        than emitting a pointless one-cell Markdown/HTML table we discard the
        table wrapper and let the cell's block content flow into the surrounding
        document as normal block-level nodes.
        """
        rows = [c for c in table.children if isinstance(c, DocxASTTableRowNode)]
        cells = [c for c in rows[0].children if isinstance(c, DocxASTTableCellNode)]
        cell = cells[0]

        block_nodes: List[DocxASTNode] = [
            c for c in cell.children
            if isinstance(c, (DocxASTParagraphNode, DocxASTTableNode))
        ]
        self._map_block_sequence(block_nodes, target)

    def _map_table(self, table: DocxASTTableNode) -> DocumentIRTableNode:
        """Map a DOCX table to a DocumentIRTableNode.

        Rows marked as header rows (via tblHeader) go into a
        DocumentIRTableHeaderNode; all others go into a DocumentIRTableBodyNode.
        """
        ir_table = DocumentIRTableNode()
        header_rows: List[DocxASTTableRowNode] = []
        body_rows: List[DocxASTTableRowNode] = []

        for child in table.children:
            if isinstance(child, DocxASTTableRowNode):
                rpr = next(
                    (c for c in child.children if isinstance(c, DocxASTTableRowPropertiesNode)),
                    None,
                )
                if rpr and rpr.is_header:
                    header_rows.append(child)

                else:
                    body_rows.append(child)

        if header_rows:
            ir_header = DocumentIRTableHeaderNode()
            for row in header_rows:
                ir_header.add_child(self._map_table_row(row, is_header=True))

            ir_table.add_child(ir_header)

        if body_rows:
            ir_body = DocumentIRTableBodyNode()
            for row in body_rows:
                ir_body.add_child(self._map_table_row(row, is_header=False))

            ir_table.add_child(ir_body)

        # If no rows were classified as header, put everything in body
        if not header_rows and not body_rows:
            ir_body = DocumentIRTableBodyNode()
            ir_table.add_child(ir_body)

        return ir_table

    def _map_table_row(
        self, row: DocxASTTableRowNode, is_header: bool
    ) -> DocumentIRTableRowNode:
        """Map a table row."""
        ir_row = DocumentIRTableRowNode()
        for child in row.children:
            if isinstance(child, DocxASTTableCellNode):
                ir_row.add_child(self._map_table_cell(child, is_header=is_header))

        return ir_row

    def _map_table_cell(
        self, cell: DocxASTTableCellNode, is_header: bool
    ) -> DocumentIRTableCellNode:
        """Map a table cell.

        Cell alignment is taken from the first paragraph's justification,
        or from the cell properties if available.
        """
        # Determine alignment from cell properties or first paragraph
        alignment = "left"

        # Try to get alignment from first paragraph's pPr
        first_para = next(
            (c for c in cell.children if isinstance(c, DocxASTParagraphNode)),
            None,
        )
        if first_para:
            ppr = next(
                (c for c in first_para.children
                 if isinstance(c, DocxASTParagraphPropertiesNode)),
                None,
            )
            if ppr and ppr.justification:
                jc = ppr.justification
                if jc in ("center", "right", "left"):
                    alignment = jc

        ir_cell = DocumentIRTableCellNode(is_header=is_header, alignment=alignment)

        # Map cell content — paragraphs and nested tables
        for child in cell.children:
            if isinstance(child, DocxASTParagraphNode):
                mapped = self._map_paragraph(child)
                if mapped is not None:
                    ir_cell.add_child(mapped)

            elif isinstance(child, DocxASTTableNode):
                if self._is_single_cell_table(child):
                    self._unwrap_single_cell_table(child, ir_cell)

                else:
                    ir_cell.add_child(self._map_table(child))

        return ir_cell

    def _append_inline_content(
        self, para: DocxASTParagraphNode, target: DocumentIRNode
    ) -> None:
        """Collect inline nodes from a paragraph's runs and append to target."""
        for child in para.children:
            if isinstance(child, DocxASTRunNode):
                for ir_node in self._map_run(child):
                    target.add_child(ir_node)
            # Bookmarks are silently skipped

    def _map_run(self, run: DocxASTRunNode) -> List[DocumentIRNode]:
        """Map a single run to a list of document_ir inline nodes.

        A run may contain text, tabs, breaks, and drawings.  Text content
        is emitted as DocumentIRTextSpanNode with formatting flags from the rPr.
        """
        rpr = next(
            (c for c in run.children if isinstance(c, DocxASTRunPropertiesNode)),
            None,
        )

        bold = rpr.bold if rpr else False
        italic = rpr.italic if rpr else False
        strike = rpr.strike if rpr else False
        code = False

        superscript = False
        subscript = False
        if rpr and rpr.vertical_align == "superscript":
            superscript = True

        elif rpr and rpr.vertical_align == "subscript":
            subscript = True

        # A run styled with a code character style is treated as inline code
        if rpr and rpr.style_id:
            style = self._get_style(rpr.style_id)
            if style and style.is_code:
                code = True

        # A run using a monospace font is treated as inline code
        if rpr and rpr.font_ascii:
            if rpr.font_ascii.lower() in _MONOSPACE_FONTS:
                code = True

        result: List[DocumentIRNode] = []

        for child in run.children:
            if isinstance(child, DocxASTRunPropertiesNode):
                continue

            if isinstance(child, DocxASTTextNode):
                if child.content:
                    result.append(DocumentIRTextSpanNode(
                        content=child.content,
                        bold=bold,
                        italic=italic,
                        strikethrough=strike,
                        code=code,
                        superscript=superscript,
                        subscript=subscript,
                    ))

            elif isinstance(child, DocxASTTabNode):
                # Represent tabs as a space span — they are layout artefacts
                # in most prose contexts
                result.append(DocumentIRTextSpanNode(
                    content="\t",
                    bold=bold,
                    italic=italic,
                    strikethrough=strike,
                    code=code,
                    superscript=superscript,
                    subscript=subscript,
                ))

            elif isinstance(child, DocxASTBreakNode):
                if child.break_type == "textWrapping":
                    result.append(DocumentIRLineBreakNode())
                # Page and column breaks are dropped — they are layout-only

            elif isinstance(child, DocxASTLastRenderedPageBreakNode):
                pass  # Rendering hint only — always dropped

            elif isinstance(child, DocxASTDrawingNode):
                ir_img = self._map_drawing(child)
                if ir_img is not None:
                    result.append(ir_img)

        return result

    def _map_drawing(self, drawing: DocxASTDrawingNode) -> DocumentIRNode | None:
        """Map a drawing node to a DocumentIRImageNode."""
        if drawing.image_data is None:
            return None

        url = drawing.resolved_path or drawing.relationship_id or ""
        mime_type: str | None = None
        if drawing.resolved_path:
            ext = drawing.resolved_path.rsplit(".", 1)[-1] if "." in drawing.resolved_path else ""
            mime_type = mime_type_for_extension(ext)

        return DocumentIRImageNode(
            url=url,
            alt_text=drawing.description or "",
            title=None,
            data=drawing.image_data,
            mime_type=mime_type,
        )

    def _extract_plain_text(self, para: DocxASTParagraphNode) -> str:
        """Extract the raw text content of a paragraph, ignoring formatting."""
        parts: List[str] = []
        for child in para.children:
            if isinstance(child, DocxASTRunNode):
                for item in child.children:
                    if isinstance(item, DocxASTTextNode):
                        parts.append(item.content)

                    elif isinstance(item, DocxASTTabNode):
                        parts.append("\t")

        return "".join(parts)
