"""
Tests for the markdown AST builder
"""
import os
import pytest

from humbug.markdown.markdown_ast_builder import MarkdownASTBuilder

# pylint: disable=unused-import
import humbug.syntax.parser_imports
# pylint: enable=unused-import

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

    def test_document_property(self, ast_builder):
        """Test the document property returns the current document."""
        # Test with empty document
        initial_doc = ast_builder.document()
        assert initial_doc is not None
        assert len(initial_doc.children) == 0

        # Test after parsing content
        ast_builder.build_ast("# Test Heading")
        updated_doc = ast_builder.document()
        assert updated_doc is not None
        assert len(updated_doc.children) == 1
        assert updated_doc.children[0].__class__.__name__ == "MarkdownHeadingNode"

        # Verify same document instance is returned
        assert updated_doc is ast_builder.document()

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

    def test_headings(self, ast_builder):
        """Test parsing different levels of headings."""
        markdown = """
# Heading A
## Heading 1
# Heading B
## Heading 1
### 3rd level
### Another 3rd level
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 6
        assert doc.children[0].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[0].level == 1
        assert doc.children[1].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[1].level == 2
        assert doc.children[2].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[2].level == 1
        assert doc.children[3].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[3].level == 2
        assert doc.children[4].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[4].level == 3
        assert doc.children[5].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[5].level == 3

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

    def test_nested_list2(self, ast_builder):
        """Test parsing a nested list."""
        markdown = """
- Item 1
  - Nested 1.1
  - Nested 1.2
  1. Nested n.1
  2. Nested n.2
- Item 2
- Item 3
"""
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        list_node = doc.children[0]
        assert len(list_node.children) == 3

        # Check first item has a nested list
        first_item = list_node.children[0]
        assert len(first_item.children) == 3
        nested_list = first_item.children[0]
        assert nested_list.__class__.__name__ == "MarkdownParagraphNode"
        first_nested_list = first_item.children[1]
        assert first_nested_list.__class__.__name__ == "MarkdownUnorderedListNode"
        assert len(first_nested_list.children) == 2
        second_nested_list = first_item.children[2]
        assert second_nested_list.__class__.__name__ == "MarkdownOrderedListNode"
        assert len(second_nested_list.children) == 2

    def test_nested_list3(self, ast_builder):
        """Test parsing a nested list."""
        markdown = """
- Item 1
  1. Nested n.1
  2. Nested n.2
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
        assert len(first_item.children) == 3
        nested_list = first_item.children[0]
        assert nested_list.__class__.__name__ == "MarkdownParagraphNode"
        first_nested_list = first_item.children[1]
        assert first_nested_list.__class__.__name__ == "MarkdownOrderedListNode"
        assert len(first_nested_list.children) == 2
        second_nested_list = first_item.children[2]
        assert second_nested_list.__class__.__name__ == "MarkdownUnorderedListNode"
        assert len(second_nested_list.children) == 2

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


class TestInlineFormatting:
    """Comprehensive tests for inline formatting parsing."""

    def test_complex_nested_formatting(self, ast_builder):
        """Test deeply nested and complex formatting combinations."""
        test_cases = [
            # Nested formatting
            "**Bold with *italic* and `code` inside**",
            "*Italic with **bold** inside*",
            "`Code with **bold** inside`",

            # Adjacent formatting
            "**Bold**_italic_`code`",
            "*italic*`code`**bold**",

            # Formatting with links
            "**Bold [link](url) text**",
            "*Italic ![image](url) text*",
            "[Link with **bold** text](url)",

            # Complex nesting
            "***Bold italic*** text",
            "**Bold *with italic* and more bold**",
            "*Italic **with bold** and more italic*",
        ]

        for markdown in test_cases:
            doc = ast_builder.build_ast(markdown)
            # Verify document parses without errors
            assert doc is not None
            assert len(doc.children) >= 1
            paragraph = doc.children[0]
            assert paragraph.__class__.__name__ == "MarkdownParagraphNode"

    def test_malformed_formatting_edge_cases(self, ast_builder):
        """Test handling of malformed or incomplete formatting."""
        test_cases = [
            # Unclosed formatting
            "**Unclosed bold",
            "*Unclosed italic",
            "`Unclosed code",
            "__Unclosed underscore bold",
            "_Unclosed underscore italic",

            # Empty formatting
            "****",
            "**",
            "``",
            "____",
            "__",

            # Mismatched formatting
            "**Bold with *italic**",
            "*Italic with **bold*",

            # Formatting at boundaries
            "**start bold",
            "end bold**",
            "*start italic",
            "end italic*",
        ]

        for markdown in test_cases:
            doc = ast_builder.build_ast(markdown)
            # Verify document parses without errors
            assert doc is not None
            assert len(doc.children) >= 1

    def test_complex_link_parentheses(self, ast_builder):
        """Test links with nested parentheses in URLs (tests _find_closing_parenthesis)."""
        test_cases = [
            "[Link](http://example.com/path(with)parentheses)",
            "[Link](http://example.com/path(nested(deep)parentheses))",
            "[Link](url(a(b(c)b)a))",
            "![Image](image(1).jpg)",
            "![Image](path/to/image(version)(2).png)",
            "[Complex](http://site.com/api/func(param1(nested)param2))",
        ]

        for markdown in test_cases:
            doc = ast_builder.build_ast(markdown)
            paragraph = doc.children[0]

            if markdown.startswith('!'):
                # Image
                image = paragraph.children[0]
                assert image.__class__.__name__ == "MarkdownImageNode"
                assert '(' in image.url and ')' in image.url

            else:
                # Link
                link = paragraph.children[0]
                assert link.__class__.__name__ == "MarkdownLinkNode"
                assert '(' in link.url and ')' in link.url

    def test_inline_code_with_formatting_inside(self, ast_builder):
        """Test inline code that contains formatting characters."""
        test_cases = [
            "`code with **asterisks**`",
            "`code with *single asterisk*`",
            "`code with __underscores__`",
            "`code with _single underscore_`",
            "`code with [brackets](url)`",
            "`code with ![image](url)`",
        ]

        for markdown in test_cases:
            doc = ast_builder.build_ast(markdown)
            paragraph = doc.children[0]
            code_node = paragraph.children[0]
            assert code_node.__class__.__name__ == "MarkdownInlineCodeNode"
            # Content should be preserved as-is without formatting
            assert "**" in code_node.content or "*" in code_node.content or "__" in code_node.content or "_" in code_node.content or "[" in code_node.content

    def test_incomplete_link_info(self, ast_builder):
        """Test handling of links with incomplete or malformed information."""
        test_cases = [
            "[Incomplete link](http://example.com",
            "[Incomplete link(http://example.com",
            "[Link with empty URL](",
            "![Incomplete image](image.jpg",
            "![Incomplete image(image.jpg",
        ]

        for markdown in test_cases:
            doc = ast_builder.build_ast(markdown)
            paragraph = doc.children[0]
            assert len(paragraph.children) == 1
            image = paragraph.children[0]
            assert image.__class__.__name__ == "MarkdownTextNode"


class TestCodeBlockContinuation:
    """Tests for code block continuation logic."""

    def test_code_block_with_programming_languages(self, ast_builder):
        """Test code blocks with various programming languages."""
        # Test Python with incomplete statements that should continue
        python_code = '''```python
def incomplete_function(
    param1,
    param2
):
    str = """
This is a multi-line string that should trigger a continuation
"""
    if condition:
        return value
```'''

        # Test JavaScript with objects
        js_code = '''```javascript
/*
const obj = {
    property1: value1,
    property2: {
        nested: true
    }
*/
};
```'''

        # Test C++ with templates
        cpp_code = '''```cpp
template<typename T>
class MyClass {
public:
    T getValue() {
        return value;
    }

/*
 * This is a continued comment.
 */
};
```'''

        # Test for text.
        text_code = '''```text
this is text
and more text
```'''

        for code in [python_code, js_code, cpp_code, text_code]:
            doc = ast_builder.build_ast(code)
            assert len(doc.children) == 1
            code_block = doc.children[0]
            assert code_block.__class__.__name__ == "MarkdownCodeBlockNode"
            assert len(code_block.content.strip()) > 0

    def test_code_block_continuation_edge_cases(self, ast_builder):
        """Test edge cases in code block continuation parsing."""
        # Code block with nested fences
        nested_fences = '''```
Outer code block
  ```
  Nested code block
  ```
Outer code block continues
```'''

        doc = ast_builder.build_ast(nested_fences)
        assert len(doc.children) == 1
        code_block = doc.children[0]
        assert code_block.__class__.__name__ == "MarkdownCodeBlockNode"
        assert "Nested code block" in code_block.content

    def test_code_block_with_mixed_content(self, ast_builder):
        """Test code blocks containing markdown-like content."""
        code_with_markdown = '''```
# This looks like a heading
- This looks like a list
**This looks like bold**
[This looks like a link](url)
```'''

        doc = ast_builder.build_ast(code_with_markdown)
        assert len(doc.children) == 1
        code_block = doc.children[0]
        assert code_block.__class__.__name__ == "MarkdownCodeBlockNode"
        # Content should be preserved as-is
        assert "# This looks like a heading" in code_block.content
        assert "- This looks like a list" in code_block.content


class TestListHandling:
    """Tests for advanced list functionality."""

    def test_loose_vs_tight_ordered_lists(self, ast_builder):
        """Test detection of loose vs tight ordered lists."""
        # Tight list (no blank lines)
        tight_list = """1. Item 1
2. Item 2
3. Item 3"""

        # Loose list (with blank lines)
        loose_list = """1. Item 1

2. Item 2

3. Item 3"""

        # Mixed scenario - becomes loose due to blank line
        mixed_list = """1. Item 1
2. Item 2

3. Item 3"""

        # Test tight list
        doc = ast_builder.build_ast(tight_list)
        assert len(doc.children) == 1
        list_node = doc.children[0]
        assert list_node.__class__.__name__ == "MarkdownOrderedListNode"
        assert list_node.tight is True

        # Test loose list
        doc = ast_builder.build_ast(loose_list)
        list_node = doc.children[0]
        assert list_node.tight is False

        # Test mixed list
        doc = ast_builder.build_ast(mixed_list)
        list_node = doc.children[0]
        assert list_node.tight is False

    def test_loose_vs_tight_unordered_lists(self, ast_builder):
        """Test detection of loose vs tight unordered lists."""
        # Tight list (no blank lines)
        tight_list = """- Item 1
- Item 2
- Item 3"""

        # Loose list (with blank lines)
        loose_list = """- Item 1

- Item 2

- Item 3"""

        # Mixed scenario - becomes loose due to blank line
        mixed_list = """- Item 1
- Item 2

- Item 3"""

        # Test tight list
        doc = ast_builder.build_ast(tight_list)
        assert len(doc.children) == 1
        list_node = doc.children[0]
        assert list_node.__class__.__name__ == "MarkdownUnorderedListNode"
        assert list_node.tight is True

        # Test loose list
        doc = ast_builder.build_ast(loose_list)
        list_node = doc.children[0]
        assert list_node.tight is False

        # Test mixed list
        doc = ast_builder.build_ast(mixed_list)
        list_node = doc.children[0]
        assert list_node.tight is False

    def test_complex_nested_list_structures(self, ast_builder):
        """Test complex nested list scenarios."""
        complex_nested = """- Level 1 Item 1
  - Level 2 Item 1
    - Level 3 Item 1
    - Level 3 Item 2
  - Level 2 Item 2
- Level 1 Item 2
  1. Ordered nested item 1
  2. Ordered nested item 2
- Level 1 Item 3"""

        doc = ast_builder.build_ast(complex_nested)
        assert len(doc.children) == 1
        main_list = doc.children[0]
        assert main_list.__class__.__name__ == "MarkdownUnorderedListNode"
        assert len(main_list.children) == 3

        # Check first item has nested unordered list
        first_item = main_list.children[0]
        assert len(first_item.children) == 2  # paragraph + nested list
        nested_list = first_item.children[1]
        assert nested_list.__class__.__name__ == "MarkdownUnorderedListNode"

        # Check second item has nested ordered list
        second_item = main_list.children[1]
        assert len(second_item.children) == 2  # paragraph + nested list
        ordered_nested = second_item.children[1]
        assert ordered_nested.__class__.__name__ == "MarkdownOrderedListNode"

    def test_list_continuation_and_interruption(self, ast_builder):
        """Test text continuation within lists and list interruption."""
        # List with continued text
        list_with_continuation = """- First list item
  continues on this line
  and this line too
- Second list item
  also continues"""

        doc = ast_builder.build_ast(list_with_continuation)
        assert len(doc.children) == 1
        list_node = doc.children[0]

        # First item should have continued text
        first_item = list_node.children[0]
        first_paragraph = first_item.children[0]
        assert "continues on this line" in first_paragraph.children[2].content

        # List interrupted by other content
        interrupted_list = """- Item 1
- Item 2

Not a list item

- Item 3"""

        doc = ast_builder.build_ast(interrupted_list)
        # Should create two separate lists with paragraph in between
        assert len(doc.children) == 3
        assert doc.children[0].__class__.__name__ == "MarkdownUnorderedListNode"
        assert doc.children[1].__class__.__name__ == "MarkdownParagraphNode"
        assert doc.children[2].__class__.__name__ == "MarkdownUnorderedListNode"

    def test_ordered_list_with_custom_start(self, ast_builder):
        """Test ordered lists with non-standard start numbers."""
        custom_start = """5. Fifth item
6. Sixth item
7. Seventh item"""

        doc = ast_builder.build_ast(custom_start)
        assert len(doc.children) == 1
        list_node = doc.children[0]
        assert list_node.__class__.__name__ == "MarkdownOrderedListNode"
        assert list_node.start == 5

        complex_lists_with_continuation = """- First list item
  continues on this line
  and this line too
  1. Indented
  2. Indented
  And back here
- Second list item  
  also continues"""

        doc = ast_builder.build_ast(complex_lists_with_continuation)
        assert len(doc.children) == 1
        list_node = doc.children[0]

        # First item should have continued text
        first_item = list_node.children[0]
        first_paragraph = first_item.children[0]
        assert "continues on this line" in first_paragraph.children[2].content


class TestTableFormatting:
    """Tests for table alignment and formatting."""

    def test_table_alignment_variations(self, ast_builder):
        """Test different table column alignments."""
        table_markdown = """| Left | Center | Right |
|:-----|:------:|------:|
| L1   | C1     | R1    |
| L2   | C2     | R2    |"""

        doc = ast_builder.build_ast(table_markdown)
        assert len(doc.children) == 1
        table = doc.children[0]
        assert table.__class__.__name__ == "MarkdownTableNode"

        # Check header row for alignment
        header = table.children[0]
        header_row = header.children[0]

        # Verify alignment is set correctly on cells
        left_cell = header_row.children[0]
        center_cell = header_row.children[1]
        right_cell = header_row.children[2]

        assert left_cell.alignment == "left"
        assert center_cell.alignment == "center"
        assert right_cell.alignment == "right"

        # Check body row alignment matches
        body = table.children[1]
        body_row = body.children[0]

        body_left_cell = body_row.children[0]
        body_center_cell = body_row.children[1]
        body_right_cell = body_row.children[2]

        assert body_left_cell.alignment == "left"
        assert body_center_cell.alignment == "center"
        assert body_right_cell.alignment == "right"

    def test_double_header_row(self, ast_builder):
        """Test different table column alignments."""
        table_markdown = """| Left | Center | Right |
| Lef2 | Cen2   | Rig2  |
|------|--------|-------|
| L1   | C1     | R1    |
| L2   | C2     | R2    |"""

        doc = ast_builder.build_ast(table_markdown)
        assert len(doc.children) == 2
        text = doc.children[0]
        assert text.__class__.__name__ == "MarkdownParagraphNode"

        table = doc.children[1]
        assert table.__class__.__name__ == "MarkdownTableNode"

        # Check header row for alignment
        header = table.children[0]
        header_row = header.children[0]

        # Verify alignment is set correctly on cells
        left_cell = header_row.children[0]
        center_cell = header_row.children[1]
        right_cell = header_row.children[2]

        assert left_cell.children[0].content == "Lef2"
        assert center_cell.children[0].content == "Cen2"
        assert right_cell.children[0].content == "Rig2"

    def test_incomplete_table_alignment_variations(self, ast_builder):
        """Test different table column alignments."""
        table_markdown = """| Left | Center | Right |
|:-----|:------:|------:|"""

        doc = ast_builder.build_ast(table_markdown)
        assert len(doc.children) == 2
        line0 = doc.children[0]
        assert line0.__class__.__name__ == "MarkdownParagraphNode"
        assert line0.children[0].content == "| Left | Center | Right |"
        line1 = doc.children[1]
        assert line1.__class__.__name__ == "MarkdownParagraphNode"
        assert line1.children[0].content == "|:-----|:------:|------:|"

    def test_solitary_table_separator(self, ast_builder):
        """Test different table column alignments."""
        table_markdown = """|:-----|:------:|------:|"""

        doc = ast_builder.build_ast(table_markdown)
        assert len(doc.children) == 1
        line0 = doc.children[0]
        assert line0.__class__.__name__ == "MarkdownParagraphNode"
        assert line0.children[0].content == "|:-----|:------:|------:|"

    def test_table_with_complex_content(self, ast_builder):
        """Test tables containing formatted text."""
        complex_table = """| **Bold** | *Italic* | `Code` |
|----------|----------|--------|
| **B1**   | *I1*     | `C1`   |
| [Link](url) | ![Image](img.jpg) | Normal |"""

        doc = ast_builder.build_ast(complex_table)
        assert len(doc.children) == 1
        table = doc.children[0]
        assert table.__class__.__name__ == "MarkdownTableNode"

        # Check header contains formatted content
        header = table.children[0]
        header_row = header.children[0]

        # First header cell should contain bold text
        bold_cell = header_row.children[0]
        assert len(bold_cell.children) == 1
        assert bold_cell.children[0].__class__.__name__ == "MarkdownBoldNode"

        # Second header cell should contain italic text
        italic_cell = header_row.children[1]
        assert len(italic_cell.children) == 1
        assert italic_cell.children[0].__class__.__name__ == "MarkdownEmphasisNode"

        # Third header cell should contain code
        code_cell = header_row.children[2]
        assert len(code_cell.children) == 1
        assert code_cell.children[0].__class__.__name__ == "MarkdownInlineCodeNode"

    def test_table_edge_cases(self, ast_builder):
        """Test table parsing edge cases."""
        # Table with missing cells
        uneven_table = """| Col1 | Col2 | Col3 | Col4 |
|------|------|------|
| A1   | A2   |
| B1   | B2   | B3   | B4 |"""

        doc = ast_builder.build_ast(uneven_table)
        assert len(doc.children) == 1
        table = doc.children[0]
        assert table.__class__.__name__ == "MarkdownTableNode"

        # Should handle uneven rows gracefully
        body = table.children[1]
        assert len(body.children) == 2  # Two body rows


class TestTextContinuation:
    """Tests for text continuation logic."""

    def test_paragraph_continuation(self, ast_builder):
        """Test paragraph continuation across multiple lines."""
        paragraph_continuation = """This is the first line
and this continues the same paragraph
with even more text on this line."""

        doc = ast_builder.build_ast(paragraph_continuation)
        assert len(doc.children) == 1
        paragraph = doc.children[0]
        assert paragraph.__class__.__name__ == "MarkdownParagraphNode"

        # Should contain all text with spaces between lines
        full_text = ""
        for child in paragraph.children:
            if child.__class__.__name__ == "MarkdownTextNode":
                full_text += child.content

        assert "This is the first line" in full_text
        assert "and this continues" in full_text
        assert "with even more text" in full_text

    def test_list_item_continuation(self, ast_builder):
        """Test text continuation within list items."""
        list_continuation = """- First list item
  continues on this line
  and continues further
- Second list item
  also continues here"""

        doc = ast_builder.build_ast(list_continuation)
        assert len(doc.children) == 1
        list_node = doc.children[0]

        # First item should contain continued text
        first_item = list_node.children[0]
        first_paragraph = first_item.children[0]

        # Extract all text content
        full_text = ""
        for child in first_paragraph.children:
            if child.__class__.__name__ == "MarkdownTextNode":
                full_text += child.content

        assert "First list item" in full_text
        assert "continues on this line" in full_text
        assert "and continues further" in full_text

    def test_continuation_with_line_breaks(self, ast_builder):
        """Test text continuation with explicit line breaks."""
        text_with_breaks = """First line  
Second line with break
Third line continues normally"""

        doc = ast_builder.build_ast(text_with_breaks)
        assert len(doc.children) == 1
        paragraph = doc.children[0]

        # Should contain line break node
        has_line_break = False
        for child in paragraph.children:
            if child.__class__.__name__ == "MarkdownLineBreakNode":
                has_line_break = True
                break

        assert has_line_break

    def test_continuation_edge_cases(self, ast_builder):
        """Test edge cases in text continuation."""
        # Continuation after different elements
        mixed_continuation = """# Heading

Paragraph starts here
and continues on next line

- List item
  continues here
  
Another paragraph
continues normally"""

        doc = ast_builder.build_ast(mixed_continuation)
        # Should have heading, paragraph, list, line break, and another paragraph
        assert len(doc.children) == 5
        assert doc.children[0].__class__.__name__ == "MarkdownHeadingNode"
        assert doc.children[1].__class__.__name__ == "MarkdownParagraphNode"
        assert doc.children[2].__class__.__name__ == "MarkdownUnorderedListNode"
        assert doc.children[3].__class__.__name__ == "MarkdownLineBreakNode"
        assert doc.children[4].__class__.__name__ == "MarkdownParagraphNode"


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

    def test_mixed_list_markers(self, ast_builder):
        """Test lists with mixed markers."""
        mixed_markers = """- Item with dash
* Item with asterisk
+ Item with plus
- Back to dash"""

        doc = ast_builder.build_ast(mixed_markers)
        # Should create separate lists for different markers
        assert len(doc.children) >= 1
        # All should be unordered lists
        for child in doc.children:
            assert child.__class__.__name__ == "MarkdownUnorderedListNode"

    def test_empty_elements(self, ast_builder):
        """Test handling of empty elements."""
        empty_elements = """


# 

**

[]()

![]()

```
```

| |
|-|
| |
"""
        doc = ast_builder.build_ast(empty_elements)
        # Should handle empty elements gracefully without crashing
        assert doc is not None
        assert len(doc.children) >= 0
