"""
Tests for the markdown AST builder
"""
import os
import pytest

from humbug.markdown.markdown_ast_builder import MarkdownASTBuilder, MarkdownASTBuilderError
from test_utils import (
    find_test_files,
    parse_and_compare
)


@pytest.fixture
def ast_builder():
    """Fixture providing a markdown AST builder instance."""
    return MarkdownASTBuilder(no_underscores=False)


@pytest.fixture
def ast_builder_no_underscores():
    """Fixture providing a markdown AST builder instance with underscores disabled."""
    return MarkdownASTBuilder(no_underscores=True)


class TestMarkdownASTBuilder:
    """Tests for the MarkdownASTBuilder class."""

    @pytest.mark.parametrize("markdown_path,expected_json_path", find_test_files())
    def test_parse_fixture_files(self, markdown_path, expected_json_path):
        """Test parsing markdown files against expected JSON outputs."""
        is_match, diff = parse_and_compare(markdown_path, expected_json_path)
        assert is_match, f"AST mismatch for {os.path.basename(markdown_path)}:\n{diff}"

    def test_empty_document(self, ast_builder):
        """Test parsing an empty document."""
        doc = ast_builder.build_ast("")
        assert doc is not None
        assert len(doc.children) == 0

    def test_simple_paragraph(self, ast_builder):
        """Test parsing a simple paragraph."""
        doc = ast_builder.build_ast("This is a paragraph.")
        assert doc is not None
        assert len(doc.children) == 1
        assert doc.children[0].__class__.__name__ == "MarkdownParagraphNode"
        assert len(doc.children[0].children) == 1
        assert doc.children[0].children[0].__class__.__name__ == "MarkdownTextNode"
        assert doc.children[0].children[0].content == "This is a paragraph."

    def test_heading(self, ast_builder):
        """Test parsing a heading."""
        doc = ast_builder.build_ast("# Heading 1")
        assert doc is not None
        assert len(doc.children) == 1
        assert doc.children[0].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[0].level == 1
        assert len(doc.children[0].children) == 1
        assert doc.children[0].children[0].__class__.__name__ == "MarkdownTextNode"
        assert doc.children[0].children[0].content == "Heading 1"

    def test_bold_text(self, ast_builder):
        """Test parsing bold text."""
        doc = ast_builder.build_ast("This is **bold** text.")
        assert doc is not None
        assert len(doc.children) == 1
        paragraph = doc.children[0]
        assert paragraph.__class__.__name__ == "MarkdownParagraphNode"
        assert len(paragraph.children) == 3
        assert paragraph.children[0].__class__.__name__ == "MarkdownTextNode"
        assert paragraph.children[0].content == "This is "
        assert paragraph.children[1].__class__.__name__ == "MarkdownBoldNode"
        assert paragraph.children[1].children[0].__class__.__name__ == "MarkdownTextNode"
        assert paragraph.children[1].children[0].content == "bold"
        assert paragraph.children[2].__class__.__name__ == "MarkdownTextNode"
        assert paragraph.children[2].content == " text."

    def test_underscore_formatting(self, ast_builder, ast_builder_no_underscores):
        """Test underscore formatting behavior."""
        # With underscores enabled
        doc = ast_builder.build_ast("_italic_ and __bold__")
        assert doc.children[0].children[0].__class__.__name__ == "MarkdownEmphasisNode"
        assert doc.children[0].children[2].__class__.__name__ == "MarkdownBoldNode"

        # With underscores disabled
        doc = ast_builder_no_underscores.build_ast("_italic_ and __bold__")
        assert doc.children[0].children[0].__class__.__name__ == "MarkdownTextNode"
        assert doc.children[0].children[0].content == "_italic_ and __bold__"

    def test_nested_formatting(self, ast_builder):
        """Test nested formatting."""
        doc = ast_builder.build_ast("This is **bold with *italic* inside**.")
        paragraph = doc.children[0]
        bold_node = paragraph.children[1]
        assert bold_node.__class__.__name__ == "MarkdownBoldNode"
        assert len(bold_node.children) == 3
        assert bold_node.children[0].__class__.__name__ == "MarkdownTextNode"
        assert bold_node.children[0].content == "bold with "
        assert bold_node.children[1].__class__.__name__ == "MarkdownEmphasisNode"

    def test_unordered_list(self, ast_builder):
        """Test parsing an unordered list."""
        markdown = """
- Item 1
- Item 2
- Item 3
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        list_node = doc.children[0]
        assert list_node.__class__.__name__ == "MarkdownUnorderedListNode"
        assert len(list_node.children) == 3
        for item in list_node.children:
            assert item.__class__.__name__ == "MarkdownListItemNode"

    def test_ordered_list(self, ast_builder):
        """Test parsing an ordered list."""
        markdown = """
1. First item
2. Second item
3. Third item
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        list_node = doc.children[0]
        assert list_node.__class__.__name__ == "MarkdownOrderedListNode"
        assert len(list_node.children) == 3
        assert list_node.start == 1

    def test_nested_list(self, ast_builder):
        """Test parsing a nested list."""
        markdown = """
- Item 1
  - Nested 1.1
  - Nested 1.2
- Item 2
- Item 3
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        list_node = doc.children[0]
        assert len(list_node.children) == 3

        # Check first item has a nested list
        first_item = list_node.children[0]
        assert len(first_item.children) == 2
        nested_list = first_item.children[0]
        assert nested_list.__class__.__name__ == "MarkdownParagraphNode"
        nested_list = first_item.children[1]
        assert nested_list.__class__.__name__ == "MarkdownUnorderedListNode"
        assert len(nested_list.children) == 2

    def test_code_block(self, ast_builder):
        """Test parsing a code block."""
        markdown = """```python
def hello():
    print("Hello, world!")
```"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        code_block = doc.children[0]
        assert code_block.__class__.__name__ == "MarkdownCodeBlockNode"
        assert code_block.language == "python"
        assert "def hello():" in code_block.content
        assert "print(\"Hello, world!\")" in code_block.content

    def test_table(self, ast_builder):
        """Test parsing a table."""
        markdown = """
| Header 1 | Header 2 |
|----------|----------|
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        table = doc.children[0]
        assert table.__class__.__name__ == "MarkdownTableNode"
        assert len(table.children) == 2

        # Check header
        header = table.children[0]
        assert header.__class__.__name__ == "MarkdownTableHeaderNode"
        assert len(header.children) == 1  # One row

        # Check body
        body = table.children[1]
        assert body.__class__.__name__ == "MarkdownTableBodyNode"
        assert len(body.children) == 2  # Two rows

    def test_horizontal_rule(self, ast_builder):
        """Test parsing a horizontal rule."""
        markdown = """
Before rule

---

After rule
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 3
        assert doc.children[0].__class__.__name__ == "MarkdownParagraphNode"
        assert doc.children[1].__class__.__name__ == "MarkdownHorizontalRuleNode"
        assert doc.children[2].__class__.__name__ == "MarkdownParagraphNode"

    def test_link(self, ast_builder):
        """Test parsing a link."""
        doc = ast_builder.build_ast("Here is a [link](https://example.com) to a website.")
        paragraph = doc.children[0]
        assert len(paragraph.children) == 3
        link = paragraph.children[1]
        assert link.__class__.__name__ == "MarkdownLinkNode"
        assert link.url == "https://example.com"
        assert len(link.children) == 1
        assert link.children[0].content == "link"

    def test_image(self, ast_builder):
        """Test parsing an image."""
        doc = ast_builder.build_ast("![Alt text](image.jpg \"Image title\")")
        paragraph = doc.children[0]
        image = paragraph.children[0]
        assert image.__class__.__name__ == "MarkdownImageNode"
        assert image.url == "image.jpg"
        assert image.alt_text == "Alt text"
        assert image.title == "Image title"

    def test_line_to_node_mapping(self, ast_builder):
        """Test the line to node mapping functionality."""
        markdown = """# Heading

Paragraph 1.

Paragraph 2.
"""
        ast_builder.build_ast(markdown)

        # Check mapping exists for each line
        assert 0 in ast_builder._line_to_node_map  # Heading
        assert 2 in ast_builder._line_to_node_map  # Paragraph 1
        assert 4 in ast_builder._line_to_node_map  # Paragraph 2

    def test_update_ast(self, ast_builder):
        """Test updating an AST incrementally."""
        original = "# Original heading\n\nOriginal paragraph."
        updated = "# Updated heading\n\nUpdated paragraph."

        # Build original AST
        ast_builder.build_ast(original)

        # Update AST
        doc = ast_builder.update_ast(updated, original)

        # Check that the AST was updated
        assert doc.children[0].children[0].content == "Updated heading"
        assert doc.children[1].children[0].content == "Updated paragraph."

    def test_ast_builder_error(self):
        """Test that ast_builder errors are raised appropriately."""
        # Create a ast_builder with a broken regex to force an error
        broken_ast_builder = MarkdownASTBuilder(no_underscores=False)
        broken_ast_builder._heading_pattern = None  # type: ignore

        with pytest.raises(MarkdownASTBuilderError):
            broken_ast_builder.parse_line("# This should fail", 0)


class TestEdgeCases:
    """Tests for edge cases and error conditions."""

    def test_unclosed_code_block(self, ast_builder):
        """Test handling of unclosed code blocks."""
        markdown = """```python
def unclosed_function():
    print("This code block is not closed")
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        code_block = doc.children[0]
        assert code_block.__class__.__name__ == "MarkdownCodeBlockNode"
        assert code_block.language == "python"

    def test_nested_code_blocks(self, ast_builder):
        """Test handling of nested code blocks."""
        markdown = """```
Outer code block
  ```
  Nested code block
  ```
Outer code block continues
```
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        code_block = doc.children[0]
        assert code_block.__class__.__name__ == "MarkdownCodeBlockNode"
        assert "Nested code block" in code_block.content

    def test_incomplete_table(self, ast_builder):
        """Test handling of incomplete tables."""
        markdown = """
| Header 1 | Header 2 |
|----------|----------|
"""
        doc = ast_builder.build_ast(markdown)
        # The AST builder should not create a table without any body rows
        assert len(doc.children) == 2
        assert doc.children[0].__class__.__name__ == "MarkdownParagraphNode"
        assert doc.children[1].__class__.__name__ == "MarkdownParagraphNode"

    def test_invalid_table_separator(self, ast_builder):
        """Test handling of invalid table separators."""
        markdown = """
| Header 1 | Header 2 |
| Invalid | Separator |
| Cell 1   | Cell 2   |
"""
        doc = ast_builder.build_ast(markdown)
        # Should be treated as paragraphs
        assert len(doc.children) == 3
        assert all(child.__class__.__name__ == "MarkdownParagraphNode" for child in doc.children)
