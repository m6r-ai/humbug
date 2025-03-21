"""
Markdown AST visitor to render the AST as HTML.
"""

from humbug.markdown.markdown_ast_node import MarkdownASTVisitor


class MarkdownHTMLRenderer(MarkdownASTVisitor):
    """Visitor that renders the AST back to HTML."""
    def visit_MarkdownDocumentNode(self, node):  # pylint: disable=invalid-name
        """
        Render a document node to HTML.

        Args:
            node: The document node to render

        Returns:
            The HTML string representation of the document
        """
        html_parts = []
        for child in node.children:
            html_parts.append(self.visit(child))

        return "".join(html_parts)

    def visit_MarkdownParagraphNode(self, node):  # pylint: disable=invalid-name
        """
        Render a paragraph node to HTML.

        Args:
            node: The paragraph node to render

        Returns:
            The HTML string representation of the paragraph
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<p>{inner_html}</p>"

    def visit_MarkdownHeadingNode(self, node):  # pylint: disable=invalid-name
        """
        Render a heading node to HTML.

        Args:
            node: The heading node to render

        Returns:
            The HTML string representation of the heading
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<h{node.level}>{inner_html}</h{node.level}>"

    def visit_MarkdownOrderedListNode(self, node):  # pylint: disable=invalid-name
        """
        Render an ordered list node to HTML.

        Args:
            node: The ordered list node to render

        Returns:
            The HTML string representation of the ordered list
        """
        list_items = "".join(self.visit(child) for child in node.children)
        return f"<ol>{list_items}</ol>"

    def visit_MarkdownUnorderedListNode(self, node):  # pylint: disable=invalid-name
        """
        Render an unordered list node to HTML.

        Args:
            node: The unordered list node to render

        Returns:
            The HTML string representation of the unordered list
        """
        list_items = "".join(self.visit(child) for child in node.children)
        return f"<ul>{list_items}</ul>"

    def visit_MarkdownListItemNode(self, node):  # pylint: disable=invalid-name
        """
        Render a list item node to HTML.

        Args:
            node: The list item node to render

        Returns:
            The HTML string representation of the list item
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<li>{inner_html}</li>"

    def visit_MarkdownTextNode(self, node):  # pylint: disable=invalid-name
        """
        Render a text node to HTML.

        Args:
            node: The text node to render

        Returns:
            The text content
        """
        # Text content should already be HTML-escaped at creation time
        return node.content

    def visit_MarkdownBoldNode(self, node):  # pylint: disable=invalid-name
        """
        Render a bold node to HTML.

        Args:
            node: The bold node to render

        Returns:
            The HTML string representation of the bold text
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<strong>{inner_html}</strong>"

    def visit_MarkdownEmphasisNode(self, node):  # pylint: disable=invalid-name
        """
        Render an emphasis node to HTML.

        Args:
            node: The emphasis node to render

        Returns:
            The HTML string representation of the emphasized text
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<em>{inner_html}</em>"

    def visit_MarkdownInlineCodeNode(self, node):  # pylint: disable=invalid-name
        """
        Render an inline code node to HTML.

        Args:
            node: The inline code node to render

        Returns:
            The HTML string representation of the inline code
        """
        # Inline code is already HTML-escaped at creation time
        return f"<code>{node.content}</code>"

    def visit_MarkdownCodeBlockNode(self, node):  # pylint: disable=invalid-name
        """
        Render a code block node to HTML.

        Args:
            node: The code block node to render

        Returns:
            The HTML string representation of the code block
        """
        language_class = f" class=\"language-{node.language}\"" if node.language else ""
        return f"<pre><code{language_class}>{node.content}</code></pre>"

    def visit_MarkdownLineBreakNode(self, _node):  # pylint: disable=invalid-name
        """
        Render a line break node to HTML.

        Args:
            node: The line break node to render

        Returns:
            The HTML string representation of the line break
        """
        return "<br />"
