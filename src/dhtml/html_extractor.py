import re

from dhtml.html_ast_node import (
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTNode,
    HtmlASTTextNode,
)

# Elements whose text content should be skipped entirely.
_SKIP_ELEMENTS = frozenset({"script", "style", "head"})

# Block-level elements that should be followed by a newline in plain text output.
_BLOCK_ELEMENTS = frozenset({
    "address", "article", "aside", "blockquote", "caption", "dd", "details",
    "dialog", "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer",
    "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hgroup", "hr",
    "legend", "li", "main", "menu", "nav", "ol", "p", "pre", "section",
    "summary", "table", "tbody", "td", "tfoot", "th", "thead", "tr", "ul",
})


def extract_text(document: HtmlASTDocumentNode) -> str:
    """Extract plain text from an HTML AST document.

    Block-level elements are separated by newlines. Inline elements contribute
    their text inline. Script, style, and head content is skipped.

    Args:
        document: The root document node produced by parse_html.

    Returns:
        Plain text content of the document.

    Raises:
        HtmlExtractionError: If an unexpected node type is encountered.
    """
    parts: list[str] = []
    _extract_node(document, parts)

    text = re.sub(r"\n{3,}", "\n\n", "".join(parts))
    return text.strip()


def _extract_node(node: HtmlASTNode, parts: list[str]) -> None:
    """Recursively extract text from a node into parts.

    Args:
        node: The current node to process.
        parts: Accumulator list of text fragments.
    """
    if isinstance(node, HtmlASTTextNode):
        parts.append(node.content)
        return

    if isinstance(node, HtmlASTElementNode):
        if node.tag_name in _SKIP_ELEMENTS:
            return

        is_block = node.tag_name in _BLOCK_ELEMENTS

        if node.tag_name == "br":
            parts.append("\n")
            return

        if node.tag_name == "hr":
            parts.append("\n")
            return

        if is_block and parts and not parts[-1].endswith("\n"):
            parts.append("\n")

        for child in node.children:
            _extract_node(child, parts)

        if is_block and parts and not parts[-1].endswith("\n"):
            parts.append("\n")

        return

    # Document node or comment node — just recurse into children.
    for child in node.children:
        _extract_node(child, parts)
