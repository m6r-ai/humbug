from html import escape as _html_escape

from document_ir.document_ir_node import (
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
    DocumentIRVisitor,
)


def document_ir_to_html(document: DocumentIRDocumentNode) -> str:
    """
    Serialise a document_ir tree to a complete HTML document string.

    The output is a well-formed HTML5 document with a minimal <head> (charset
    meta tag and a title derived from the first H1, if present) and a <body>
    containing the document content.

    Args:
        document: The root document IR node to serialise.

    Returns:
        A UTF-8 HTML5 document as a string.
    """
    title = _find_title(document)
    body_lines: list[str] = []
    visitor = _HtmlSerialiser(body_lines)
    visitor.generic_visit(document)

    head_lines = [
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        '<meta charset="utf-8">',
        f"<title>{_esc(title)}</title>",
        "</head>",
        "<body>",
    ]
    tail_lines = [
        "</body>",
        "</html>",
        "",
    ]
    return "\n".join(head_lines + body_lines + tail_lines)


def _find_title(document: DocumentIRDocumentNode) -> str:
    """Return the text content of the first H1 node, or 'Document' if absent."""
    for child in document.children:
        if isinstance(child, DocumentIRHeadingNode) and child.level == 1:
            return _collect_text(child)

    return "Document"


def _collect_text(node: DocumentIRNode) -> str:
    """Recursively collect plain text from an IR node's descendants."""
    parts: list[str] = []
    for child in node.children:
        if isinstance(child, DocumentIRTextSpanNode):
            parts.append(child.content)

        else:
            parts.append(_collect_text(child))

    return "".join(parts)


def _esc(text: str) -> str:
    """HTML-escape a plain text string."""
    return _html_escape(text)


class _HtmlSerialiser(DocumentIRVisitor):
    """Visitor that serialises document_ir nodes to HTML lines."""

    def __init__(self, lines: list[str]) -> None:
        """
        Initialise the serialiser.

        Args:
            lines: The list to which output lines are appended.
        """
        self._lines = lines
        self._indent = 0

    def _emit(self, text: str) -> None:
        """Append an indented line of HTML to the output list."""
        self._lines.append("  " * self._indent + text)

    def _emit_inline(self, node: DocumentIRNode) -> str:
        """Serialise all children of node as an inline HTML string."""
        parts: list[str] = []
        for child in node.children:
            parts.append(self._inline(child))

        return "".join(parts)

    def _inline(self, node: DocumentIRNode) -> str:
        """Return the inline HTML representation of a single node."""
        if isinstance(node, DocumentIRTextSpanNode):
            text = _esc(node.content)
            if node.code:
                return f"<code>{text}</code>"

            if node.bold and node.italic:
                return f"<strong><em>{text}</em></strong>"

            if node.bold:
                return f"<strong>{text}</strong>"

            if node.italic:
                return f"<em>{text}</em>"

            if node.strikethrough:
                return f"<del>{text}</del>"

            if node.superscript:
                return f"<sup>{text}</sup>"

            if node.subscript:
                return f"<sub>{text}</sub>"

            return text

        if isinstance(node, DocumentIRLinkNode):
            inner = self._emit_inline(node)
            url = _esc(node.url)
            if node.title:
                return f'<a href="{url}" title="{_esc(node.title)}">{inner}</a>'

            return f'<a href="{url}">{inner}</a>'

        if isinstance(node, DocumentIRImageNode):
            url = _esc(node.url)
            alt = _esc(node.alt_text)
            if node.title:
                return f'<img src="{url}" alt="{alt}" title="{_esc(node.title)}">'

            return f'<img src="{url}" alt="{alt}">'

        if isinstance(node, DocumentIRLineBreakNode):
            return "<br>"

        # Fallback: recurse into children.
        parts: list[str] = []
        for child in node.children:
            parts.append(self._inline(child))

        return "".join(parts)

    def visit_DocumentIRHeadingNode(self, node: DocumentIRHeadingNode) -> None:  # pylint: disable=invalid-name
        """Serialise a heading node."""
        tag = f"h{node.level}"
        inner = self._emit_inline(node)
        self._emit(f"<{tag}>{inner}</{tag}>")

    def visit_DocumentIRParagraphNode(self, node: DocumentIRParagraphNode) -> None:  # pylint: disable=invalid-name
        """Serialise a paragraph node."""
        inner = self._emit_inline(node)
        self._emit(f"<p>{inner}</p>")

    def visit_DocumentIRBlockquoteNode(self, node: DocumentIRBlockquoteNode) -> None:  # pylint: disable=invalid-name
        """Serialise a blockquote node."""
        self._emit("<blockquote>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</blockquote>")

    def visit_DocumentIRCodeBlockNode(self, node: DocumentIRCodeBlockNode) -> None:  # pylint: disable=invalid-name
        """Serialise a fenced code block."""
        lang_attr = f' class="language-{_esc(node.language)}"' if node.language else ""
        self._emit(f"<pre><code{lang_attr}>{_esc(node.content)}</code></pre>")

    def visit_DocumentIRUnorderedListNode(self, node: DocumentIRUnorderedListNode) -> None:  # pylint: disable=invalid-name
        """Serialise an unordered list."""
        self._emit("<ul>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</ul>")

    def visit_DocumentIROrderedListNode(self, node: DocumentIROrderedListNode) -> None:  # pylint: disable=invalid-name
        """Serialise an ordered list."""
        start_attr = f' start="{node.start}"' if node.start != 1 else ""
        self._emit(f"<ol{start_attr}>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</ol>")

    def visit_DocumentIRListItemNode(self, node: DocumentIRListItemNode) -> None:  # pylint: disable=invalid-name
        """Serialise a list item."""
        inner = self._emit_inline(node)
        if inner:
            self._emit(f"<li>{inner}</li>")

        else:
            self._emit("<li>")
            self._indent += 1
            self.generic_visit(node)
            self._indent -= 1
            self._emit("</li>")

    def visit_DocumentIRTableNode(self, node: DocumentIRTableNode) -> None:  # pylint: disable=invalid-name
        """Serialise a table."""
        self._emit("<table>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</table>")

    def visit_DocumentIRTableHeaderNode(self, node: DocumentIRTableHeaderNode) -> None:  # pylint: disable=invalid-name
        """Serialise a table header section."""
        self._emit("<thead>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</thead>")

    def visit_DocumentIRTableBodyNode(self, node: DocumentIRTableBodyNode) -> None:  # pylint: disable=invalid-name
        """Serialise a table body section."""
        self._emit("<tbody>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</tbody>")

    def visit_DocumentIRTableRowNode(self, node: DocumentIRTableRowNode) -> None:  # pylint: disable=invalid-name
        """Serialise a table row."""
        self._emit("<tr>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</tr>")

    def visit_DocumentIRTableCellNode(self, node: DocumentIRTableCellNode) -> None:  # pylint: disable=invalid-name
        """Serialise a table cell."""
        tag = "th" if node.is_header else "td"
        align_attr = f' align="{node.alignment}"' if node.alignment != "left" else ""
        inner = self._emit_inline(node)
        self._emit(f"<{tag}{align_attr}>{inner}</{tag}>")

    def visit_DocumentIRHorizontalRuleNode(self, node: DocumentIRHorizontalRuleNode) -> None:  # pylint: disable=invalid-name,unused-argument
        """Serialise a horizontal rule."""
        self._emit("<hr>")

    def visit_DocumentIRTextSpanNode(self, node: DocumentIRTextSpanNode) -> None:  # pylint: disable=invalid-name
        """Serialise a top-level text span (outside any block container)."""
        self._emit(self._inline(node))

    def visit_DocumentIRLinkNode(self, node: DocumentIRLinkNode) -> None:  # pylint: disable=invalid-name
        """Serialise a top-level link (outside any block container)."""
        self._emit(self._inline(node))

    def visit_DocumentIRImageNode(self, node: DocumentIRImageNode) -> None:  # pylint: disable=invalid-name
        """Serialise a top-level image."""
        self._emit(self._inline(node))

    def visit_DocumentIRLineBreakNode(self, node: DocumentIRLineBreakNode) -> None:  # pylint: disable=invalid-name,unused-argument
        """Serialise a line break."""
        self._emit("<br>")

    def visit_DocumentIRDefinitionListNode(self, node: DocumentIRDefinitionListNode) -> None:  # pylint: disable=invalid-name
        """Serialise a definition list."""
        self._emit("<dl>")
        self._indent += 1
        self.generic_visit(node)
        self._indent -= 1
        self._emit("</dl>")

    def visit_DocumentIRDefinitionTermNode(self, node: DocumentIRDefinitionTermNode) -> None:  # pylint: disable=invalid-name
        """Serialise a definition term."""
        inner = self._emit_inline(node)
        self._emit(f"<dt>{inner}</dt>")

    def visit_DocumentIRDefinitionDescriptionNode(self, node: DocumentIRDefinitionDescriptionNode) -> None:  # pylint: disable=invalid-name
        """Serialise a definition description."""
        inner = self._emit_inline(node)
        self._emit(f"<dd>{inner}</dd>")
