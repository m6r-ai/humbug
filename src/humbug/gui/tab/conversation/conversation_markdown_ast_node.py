"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

from typing import Optional


class ASTNode:
    """Base class for all AST nodes."""
    def __init__(self):
        """Initialize an AST node with an empty children list."""
        self.children = []
        # Source range information to support incremental updating
        self.line_start: Optional[int] = None
        self.line_end: Optional[int] = None

    def add_child(self, child):
        """
        Add a child node to this node.

        Args:
            child: The child node to add

        Returns:
            The added child node for method chaining
        """
        self.children.append(child)
        return child

    def accept(self, visitor):
        """
        Accept a visitor to process this node.

        Args:
            visitor: The visitor to accept

        Returns:
            The result of the visitor's visit method
        """
        return visitor.visit(self)

    def remove_children(self):
        """Remove all children from this node."""
        self.children = []


class Document(ASTNode):
    """Root node representing an entire HTML document."""
    def __init__(self):
        """Initialize a document node."""
        super().__init__()


class Paragraph(ASTNode):
    """Node representing an HTML paragraph (<p>)."""
    def __init__(self, children=None):
        """
        Initialize a paragraph node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class Heading(ASTNode):
    """Node representing an HTML heading (<h1> through <h6>)."""
    def __init__(self, level, children=None):
        """
        Initialize a heading node.

        Args:
            level: The heading level (1-6)
            children: Optional list of child nodes to add
        """
        super().__init__()
        # Level should be 1-6
        self.level = max(1, min(6, level))
        if children:
            for child in children:
                self.add_child(child)


class OrderedList(ASTNode):
    """Node representing an HTML ordered list (<ol>)."""
    def __init__(self, indent=0, children=None):
        """
        Initialize an ordered list node.

        Args:
            indent: The indentation level of this list
            children: Optional list of child nodes to add
        """
        super().__init__()
        self.indent = indent
        if children:
            for child in children:
                self.add_child(child)


class UnorderedList(ASTNode):
    """Node representing an HTML unordered list (<ul>)."""
    def __init__(self, indent=0, children=None):
        """
        Initialize an unordered list node.

        Args:
            indent: The indentation level of this list
            children: Optional list of child nodes to add
        """
        super().__init__()
        self.indent = indent
        if children:
            for child in children:
                self.add_child(child)


class ListItem(ASTNode):
    """Node representing an HTML list item (<li>)."""
    def __init__(self, children=None):
        """
        Initialize a list item node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class Text(ASTNode):
    """Node representing plain text content."""
    def __init__(self, content):
        """
        Initialize a text node.

        Args:
            content: The text content
        """
        super().__init__()
        self.content = content


class Bold(ASTNode):
    """Node representing bold text (<b> or <strong>)."""
    def __init__(self, children=None):
        """
        Initialize a bold node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class Emphasis(ASTNode):
    """Node representing emphasized text (<em> or <i>)."""
    def __init__(self, children=None):
        """
        Initialize an emphasis node.

        Args:
            children: Optional list of child nodes to add
        """
        super().__init__()
        if children:
            for child in children:
                self.add_child(child)


class CodeBlock(ASTNode):
    """Node representing a code block (<pre><code>)."""
    def __init__(self, language=None, content=None):
        """
        Initialize a code block node.

        Args:
            language: Optional language identifier for syntax highlighting
            content: The code content
        """
        super().__init__()
        self.language = language or ""
        self.content = content or ""


class MarkdownParseError(Exception):
    """Exception raised for errors during markdown parsing."""


class ASTVisitor:
    """Base visitor class for AST traversal."""
    def visit(self, node):
        """
        Visit a node and dispatch to the appropriate visit method.

        Args:
            node: The node to visit

        Returns:
            The result of visiting the node
        """
        method_name = f'visit_{node.__class__.__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        """
        Default visit method for nodes without specific handlers.

        Args:
            node: The node to visit

        Returns:
            A list of results from visiting each child
        """
        results = []
        for child in node.children:
            if child:
                results.append(self.visit(child))
        return results


class HTMLRenderer(ASTVisitor):
    """Visitor that renders the AST back to HTML."""
    def visit_Document(self, node):
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

    def visit_Paragraph(self, node):
        """
        Render a paragraph node to HTML.

        Args:
            node: The paragraph node to render

        Returns:
            The HTML string representation of the paragraph
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<p>{inner_html}</p>"

    def visit_Heading(self, node):
        """
        Render a heading node to HTML.

        Args:
            node: The heading node to render

        Returns:
            The HTML string representation of the heading
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<h{node.level}>{inner_html}</h{node.level}>"

    def visit_OrderedList(self, node):
        """
        Render an ordered list node to HTML.

        Args:
            node: The ordered list node to render

        Returns:
            The HTML string representation of the ordered list
        """
        list_items = "".join(self.visit(child) for child in node.children)
        return f"<ol>{list_items}</ol>"

    def visit_UnorderedList(self, node):
        """
        Render an unordered list node to HTML.

        Args:
            node: The unordered list node to render

        Returns:
            The HTML string representation of the unordered list
        """
        list_items = "".join(self.visit(child) for child in node.children)
        return f"<ul>{list_items}</ul>"

    def visit_ListItem(self, node):
        """
        Render a list item node to HTML.

        Args:
            node: The list item node to render

        Returns:
            The HTML string representation of the list item
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<li>{inner_html}</li>"

    def visit_Text(self, node):
        """
        Render a text node to HTML.

        Args:
            node: The text node to render

        Returns:
            The text content
        """
        # Text content should already be HTML-escaped at creation time
        return node.content

    def visit_Bold(self, node):
        """
        Render a bold node to HTML.

        Args:
            node: The bold node to render

        Returns:
            The HTML string representation of the bold text
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<strong>{inner_html}</strong>"

    def visit_Emphasis(self, node):
        """
        Render an emphasis node to HTML.

        Args:
            node: The emphasis node to render

        Returns:
            The HTML string representation of the emphasized text
        """
        inner_html = "".join(self.visit(child) for child in node.children)
        return f"<em>{inner_html}</em>"

    def visit_CodeBlock(self, node):
        """
        Render a code block node to HTML.

        Args:
            node: The code block node to render

        Returns:
            The HTML string representation of the code block
        """
        language_class = f" class=\"language-{node.language}\"" if node.language else ""
        return f"<pre><code{language_class}>{node.content}</code></pre>"


class ASTPrinter(ASTVisitor):
    """Visitor that prints the AST structure for debugging."""
    def __init__(self):
        """Initialize the AST printer with zero indentation."""
        self.indent_level = 0

    def _indent(self):
        """
        Get the current indentation string.

        Returns:
            A string of spaces for the current indentation level
        """
        return "  " * self.indent_level

    def generic_visit(self, node):
        """
        Default visit method that prints the node type.

        Args:
            node: The node to visit

        Returns:
            The results of visiting the children
        """
        node_name = node.__class__.__name__
        line_range = ""
        if node.line_start is not None and node.line_end is not None:
            line_range = f" (lines {node.line_start}-{node.line_end})"
        print(f"{self._indent()}{node_name}{line_range}")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_Text(self, node):
        """
        Visit a text node and print its content.

        Args:
            node: The text node to visit

        Returns:
            The text content
        """
        line_range = ""
        if node.line_start is not None:
            line_range = f" (line {node.line_start})"
        print(f"{self._indent()}Text{line_range}: '{node.content}'")
        return node.content

    def visit_Heading(self, node):
        """
        Visit a heading node and print its level.

        Args:
            node: The heading node to visit

        Returns:
            The results of visiting the children
        """
        line_range = ""
        if node.line_start is not None:
            line_range = f" (line {node.line_start})"
        print(f"{self._indent()}Heading (level {node.level}){line_range}")
        self.indent_level += 1
        results = super().generic_visit(node)
        self.indent_level -= 1
        return results

    def visit_CodeBlock(self, node):
        """
        Visit a code block node and print its language and content.

        Args:
            node: The code block node to visit

        Returns:
            The code block content
        """
        line_range = ""
        if node.line_start is not None and node.line_end is not None:
            line_range = f" (lines {node.line_start}-{node.line_end})"
        print(f"{self._indent()}CodeBlock{line_range}: language='{node.language}'")
        self.indent_level += 1
        print(f"{self._indent()}Content: '{node.content[:30]}...' ({len(node.content)} chars)")
        self.indent_level -= 1
        return node.content
