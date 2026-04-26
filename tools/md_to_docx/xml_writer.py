"""
Visitor that walks a dmarkdown AST and produces Open XML body element strings.

Each public method returns a list of XML strings that can be concatenated into
the <w:body> of a Word document.
"""

import xml.sax.saxutils as saxutils
from typing import List

from dmarkdown.markdown_ast_node import (
    MarkdownASTBoldNode,
    MarkdownASTBlockquoteNode,
    MarkdownASTCodeBlockNode,
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
    MarkdownASTVisitor,
)

# numId values defined in numbering.xml
_NUM_ID_BULLET = "1"
_NUM_ID_DECIMAL = "2"

# Map heading level → Word style ID
_HEADING_STYLE = {
    1: "Heading1",
    2: "Heading2",
    3: "Heading3",
    4: "Heading4",
    5: "Heading5",
    6: "Heading6",
}

# Table cell alignment → jc value
_ALIGN_MAP = {
    "left": "left",
    "center": "center",
    "right": "right",
}


def _esc(text: str) -> str:
    """Escape a string for embedding in XML character data."""
    return saxutils.escape(text)


class DocxXmlVisitor(MarkdownASTVisitor):
    """
    Walks a dmarkdown AST and accumulates Open XML strings for the document body.

    Usage::

        visitor = DocxXmlVisitor()
        visitor.visit(ast_document_node)
        xml_parts = visitor.body_parts
    """

    def __init__(self) -> None:
        """Initialise the visitor with an empty body and list depth counter."""
        super().__init__()
        self.body_parts: List[str] = []
        self._list_depth = 0

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _inline_runs(self, node: MarkdownASTNode) -> str:
        """
        Render the children of a block node as a sequence of inline <w:r> runs.

        Returns a string of XML run elements suitable for embedding inside a
        <w:p> element.
        """
        collector = _InlineCollector()
        for child in node.children:
            collector.visit(child)

        return collector.result()

    def _make_paragraph(self, runs_xml: str, style_id: str = "Normal",
                        num_id: str | None = None, ilvl: int = 0) -> str:
        """
        Wrap inline run XML in a <w:p> with the given paragraph style.

        Args:
            runs_xml: The inline run XML to embed.
            style_id: The Word paragraph style ID.
            num_id: If set, adds list numbering properties.
            ilvl: List indent level (0-based).

        Returns:
            A complete <w:p> XML string.
        """
        ppr_parts = [f'<w:pStyle w:val="{style_id}"/>']
        if num_id is not None:
            ppr_parts.append(
                f'<w:numPr>'
                f'<w:ilvl w:val="{ilvl}"/>'
                f'<w:numId w:val="{num_id}"/>'
                f'</w:numPr>'
            )

        ppr = "<w:pPr>" + "".join(ppr_parts) + "</w:pPr>"
        return f"<w:p>{ppr}{runs_xml}</w:p>"

    # ------------------------------------------------------------------
    # Block-level visitors
    # ------------------------------------------------------------------

    def visit_MarkdownASTDocumentNode(self, node: MarkdownASTDocumentNode) -> None:  # pylint: disable=invalid-name
        """Visit the root document node and process all children."""
        for child in node.children:
            self.visit(child)

    def visit_MarkdownASTParagraphNode(self, node: MarkdownASTParagraphNode) -> None:  # pylint: disable=invalid-name
        """Emit a Normal-style paragraph."""
        runs = self._inline_runs(node)
        self.body_parts.append(self._make_paragraph(runs))

    def visit_MarkdownASTHeadingNode(self, node: MarkdownASTHeadingNode) -> None:  # pylint: disable=invalid-name
        """Emit a heading paragraph with the appropriate heading style."""
        style = _HEADING_STYLE.get(node.level, "Heading1")
        runs = self._inline_runs(node)
        self.body_parts.append(self._make_paragraph(runs, style))

    def visit_MarkdownASTCodeBlockNode(self, node: MarkdownASTCodeBlockNode) -> None:  # pylint: disable=invalid-name
        """Emit one CodeBlock paragraph per line of the code block."""
        lines = node.content.splitlines()
        if not lines:
            self.body_parts.append(self._make_paragraph("", "CodeBlock"))
            return

        for line in lines:
            run = f'<w:r><w:rPr><w:rStyle w:val="CodeChar"/></w:rPr><w:t xml:space="preserve">{_esc(line)}</w:t></w:r>'
            self.body_parts.append(self._make_paragraph(run, "CodeBlock"))

    def visit_MarkdownASTUnorderedListNode(self, node: MarkdownASTUnorderedListNode) -> None:  # pylint: disable=invalid-name
        """Visit an unordered list, incrementing the depth counter."""
        self._list_depth += 1
        for child in node.children:
            self.visit(child)

        self._list_depth -= 1

    def visit_MarkdownASTOrderedListNode(self, node: MarkdownASTOrderedListNode) -> None:  # pylint: disable=invalid-name
        """Visit an ordered list, incrementing the depth counter."""
        self._list_depth += 1
        for child in node.children:
            self.visit(child)

        self._list_depth -= 1

    def visit_MarkdownASTListItemNode(self, node: MarkdownASTListItemNode) -> None:  # pylint: disable=invalid-name
        """
        Emit a list item paragraph.

        Determines bullet vs. numbered from the parent list type and uses the
        current depth for the indent level.
        """
        ilvl = max(0, self._list_depth - 1)

        # Determine numId from the parent list type
        from dmarkdown.markdown_ast_node import MarkdownASTOrderedListNode as OL
        if isinstance(node.parent, OL):
            num_id = _NUM_ID_DECIMAL
        else:
            num_id = _NUM_ID_BULLET

        # Collect inline content from direct inline children; recurse into
        # nested lists separately.
        inline_collector = _InlineCollector()
        for child in node.children:
            if isinstance(child, (MarkdownASTUnorderedListNode, MarkdownASTOrderedListNode)):
                # Flush current item first, then recurse into sub-list
                runs = inline_collector.result()
                self.body_parts.append(self._make_paragraph(runs, "Normal", num_id, ilvl))
                inline_collector = _InlineCollector()
                self.visit(child)
            else:
                inline_collector.visit(child)

        remaining = inline_collector.result()
        if remaining or not any(
            isinstance(c, (MarkdownASTUnorderedListNode, MarkdownASTOrderedListNode))
            for c in node.children
        ):
            self.body_parts.append(self._make_paragraph(remaining, "Normal", num_id, ilvl))

    def visit_MarkdownASTTableNode(self, node: MarkdownASTTableNode) -> None:  # pylint: disable=invalid-name
        """Emit a <w:tbl> element for a Markdown table."""
        rows: List[str] = []
        for child in node.children:
            rows.extend(self._table_section(child))

        tbl_props = (
            '<w:tblPr>'
            '<w:tblStyle w:val="TableGrid"/>'
            '<w:tblW w:w="0" w:type="auto"/>'
            '<w:tblBorders>'
            '<w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
            '<w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
            '<w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
            '<w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
            '<w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
            '<w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
            '</w:tblBorders>'
            '</w:tblPr>'
        )
        self.body_parts.append(f"<w:tbl>{tbl_props}{''.join(rows)}</w:tbl>")

    def _table_section(self, node: MarkdownASTNode) -> List[str]:
        """Return a list of <w:tr> strings for a table header or body section."""
        rows: List[str] = []
        if isinstance(node, (MarkdownASTTableHeaderNode, MarkdownASTTableBodyNode)):
            for child in node.children:
                if isinstance(child, MarkdownASTTableRowNode):
                    rows.append(self._table_row(child))

        return rows

    def _table_row(self, node: MarkdownASTTableRowNode) -> str:
        """Return a <w:tr> string for a single table row."""
        cells = ""
        for child in node.children:
            if isinstance(child, MarkdownASTTableCellNode):
                cells += self._table_cell(child)

        return f"<w:tr>{cells}</w:tr>"

    def _table_cell(self, node: MarkdownASTTableCellNode) -> str:
        """Return a <w:tc> string for a single table cell."""
        jc = _ALIGN_MAP.get(node.alignment, "left")
        inline_collector = _InlineCollector()
        for child in node.children:
            inline_collector.visit(child)

        runs = inline_collector.result()
        rpr = ""
        if node.is_header:
            rpr = "<w:rPr><w:b/></w:rPr>"

        p = (
            f'<w:p>'
            f'<w:pPr><w:jc w:val="{jc}"/></w:pPr>'
            f'<w:r>{rpr}<w:t xml:space="preserve">{runs}</w:t></w:r>'
            f'</w:p>'
        )
        tcp = '<w:tcPr><w:tcW w:w="0" w:type="auto"/></w:tcPr>'
        return f"<w:tc>{tcp}{p}</w:tc>"

    def visit_MarkdownASTHorizontalRuleNode(self, node: MarkdownASTHorizontalRuleNode) -> None:  # pylint: disable=invalid-name
        """Emit an empty paragraph with a bottom border to represent a horizontal rule."""
        hr = (
            '<w:p>'
            '<w:pPr>'
            '<w:pBdr>'
            '<w:bottom w:val="single" w:sz="6" w:space="1" w:color="auto"/>'
            '</w:pBdr>'
            '</w:pPr>'
            '</w:p>'
        )
        self.body_parts.append(hr)

    def visit_MarkdownASTBlockquoteNode(self, node: MarkdownASTBlockquoteNode) -> None:  # pylint: disable=invalid-name
        """Emit blockquote children using the Blockquote paragraph style."""
        # We render each child paragraph with the Blockquote style
        for child in node.children:
            if isinstance(child, MarkdownASTParagraphNode):
                runs = self._inline_runs(child)
                self.body_parts.append(self._make_paragraph(runs, "Blockquote"))
            else:
                # Fallback: visit normally
                self.visit(child)


# ------------------------------------------------------------------
# Inline content collector
# ------------------------------------------------------------------

class _InlineCollector(MarkdownASTVisitor):
    """
    Collects inline AST nodes and renders them as a sequence of <w:r> run strings.

    This is kept separate from DocxXmlVisitor so that inline rendering can be
    used recursively without polluting the top-level body_parts list.
    """

    def __init__(self) -> None:
        """Initialise with an empty run buffer."""
        super().__init__()
        self._parts: List[str] = []

    def result(self) -> str:
        """Return the accumulated run XML as a single string."""
        return "".join(self._parts)

    def visit_MarkdownASTTextNode(self, node: MarkdownASTTextNode) -> None:  # pylint: disable=invalid-name
        """Emit a plain text run."""
        self._parts.append(
            f'<w:r><w:t xml:space="preserve">{_esc(node.content)}</w:t></w:r>'
        )

    def visit_MarkdownASTBoldNode(self, node: MarkdownASTBoldNode) -> None:  # pylint: disable=invalid-name
        """Emit bold runs for all children."""
        for child in node.children:
            child_collector = _InlineCollector()
            child_collector.visit(child)
            inner = child_collector.result()
            # Re-wrap each run with bold rPr
            self._parts.append(self._rewrap(inner, bold=True))

    def visit_MarkdownASTEmphasisNode(self, node: MarkdownASTEmphasisNode) -> None:  # pylint: disable=invalid-name
        """Emit italic runs for all children."""
        for child in node.children:
            child_collector = _InlineCollector()
            child_collector.visit(child)
            inner = child_collector.result()
            self._parts.append(self._rewrap(inner, italic=True))

    def visit_MarkdownASTInlineCodeNode(self, node: MarkdownASTInlineCodeNode) -> None:  # pylint: disable=invalid-name
        """Emit an inline code run using the CodeChar character style."""
        self._parts.append(
            f'<w:r>'
            f'<w:rPr><w:rStyle w:val="CodeChar"/></w:rPr>'
            f'<w:t xml:space="preserve">{_esc(node.content)}</w:t>'
            f'</w:r>'
        )

    def visit_MarkdownASTLinkNode(self, node: MarkdownASTLinkNode) -> None:  # pylint: disable=invalid-name
        """
        Emit link text as an underlined run.

        Full hyperlink relationships require per-document relationship IDs which
        are complex to manage here; we render the display text with underline
        formatting and append the URL in parentheses so the information is not lost.
        """
        child_collector = _InlineCollector()
        for child in node.children:
            child_collector.visit(child)

        link_text = child_collector.result()
        self._parts.append(self._rewrap(link_text, underline=True))
        if node.url:
            self._parts.append(
                f'<w:r><w:t xml:space="preserve"> ({_esc(node.url)})</w:t></w:r>'
            )

    def visit_MarkdownASTImageNode(self, node: MarkdownASTImageNode) -> None:  # pylint: disable=invalid-name
        """
        Emit image alt text as a plain run.

        Embedding binary image data requires image relationship handling beyond
        the scope of a simple converter; we emit the alt text instead.
        """
        alt = node.alt_text or node.url
        self._parts.append(
            f'<w:r><w:t xml:space="preserve">[Image: {_esc(alt)}]</w:t></w:r>'
        )

    def visit_MarkdownASTLineBreakNode(self, node: MarkdownASTLineBreakNode) -> None:  # pylint: disable=invalid-name
        """Emit a line break run."""
        self._parts.append('<w:r><w:br/></w:r>')

    def _rewrap(self, runs_xml: str, bold: bool = False,
                italic: bool = False, underline: bool = False) -> str:
        """
        Take a plain <w:r>...<w:t>text</w:t></w:r> string and return a new run
        with the given character properties applied.

        This is a simple approach: we extract the text content and build a new
        run rather than trying to parse and modify existing XML.
        """
        # Extract text content between <w:t ...> and </w:t> tags
        import re
        texts = re.findall(r'<w:t[^>]*>(.*?)</w:t>', runs_xml, re.DOTALL)
        combined = "".join(texts)

        rpr_parts = []
        if bold:
            rpr_parts.append("<w:b/>")
        if italic:
            rpr_parts.append("<w:i/>")
        if underline:
            rpr_parts.append('<w:u w:val="single"/>')

        rpr = f"<w:rPr>{''.join(rpr_parts)}</w:rPr>" if rpr_parts else ""
        return f'<w:r>{rpr}<w:t xml:space="preserve">{combined}</w:t></w:r>'
