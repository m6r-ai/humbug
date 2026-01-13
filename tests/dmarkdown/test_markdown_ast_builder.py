"""
Tests for the markdown AST builder
"""
import os
import pytest

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from syntax import ProgrammingLanguage
from dmarkdown import MarkdownASTBuilder

from markdown_test_utils import (
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


@pytest.mark.parametrize("markdown_path,expected_json_path", find_test_files())
def test_parse_fixture_files(markdown_path, expected_json_path):
    """Test parsing markdown files against expected JSON outputs."""
    is_match, diff = parse_and_compare(markdown_path, expected_json_path)
    assert is_match, f"AST mismatch for {os.path.basename(markdown_path)}:\n{diff}"


def test_empty_document(ast_builder):
    """Test parsing an empty document."""
    doc = ast_builder.build_ast("")
    assert doc is not None
    assert len(doc.children) == 0


def test_document_property(ast_builder):
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
    assert updated_doc.children[0].__class__.__name__ == "MarkdownASTHeadingNode"

    # Verify same document instance is returned
    assert updated_doc is ast_builder.document()


def test_simple_paragraph(ast_builder):
    """Test parsing a simple paragraph."""
    doc = ast_builder.build_ast("This is a paragraph.")
    assert doc is not None
    assert len(doc.children) == 1
    assert doc.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert len(doc.children[0].children) == 1
    assert doc.children[0].children[0].__class__.__name__ == "MarkdownASTTextNode"
    assert doc.children[0].children[0].content == "This is a paragraph."


def test_heading(ast_builder):
    """Test parsing a heading."""
    doc = ast_builder.build_ast("# Heading 1")
    assert doc is not None
    assert len(doc.children) == 1
    assert doc.children[0].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[0].level == 1
    assert len(doc.children[0].children) == 1
    assert doc.children[0].children[0].__class__.__name__ == "MarkdownASTTextNode"
    assert doc.children[0].children[0].content == "Heading 1"


def test_heading_with_leading_spaces(ast_builder):
    """Test headings with 0-3 leading spaces."""
    test_cases = [
        ("# No spaces", 1, "No spaces"),
        (" # One space", 1, "One space"),
        ("  ## Two spaces", 2, "Two spaces"),
        ("   ### Three spaces", 3, "Three spaces"),
    ]

    for markdown, expected_level, expected_content in test_cases:
        doc = ast_builder.build_ast(markdown)
        assert len(doc.children) == 1
        heading = doc.children[0]
        assert heading.__class__.__name__ == "MarkdownASTHeadingNode"
        assert heading.level == expected_level
        assert heading.children[0].content == expected_content


def test_heading_with_too_many_spaces(ast_builder):
    """Test that 4+ spaces prevent heading recognition."""
    markdown = "    # Four spaces should not be a heading"
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    # Should be treated as a paragraph, not a heading
    assert doc.children[0].__class__.__name__ == "MarkdownASTParagraphNode"


def test_mixed_heading_indentation(ast_builder):
    """Test document with mixed heading indentation."""
    markdown = """# Level 1 no spaces
 ## Level 2 one space
  ### Level 3 two spaces
   #### Level 4 three spaces"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 4

    levels_and_content = [
        (1, "Level 1 no spaces"),
        (2, "Level 2 one space"),
        (3, "Level 3 two spaces"),
        (4, "Level 4 three spaces")
    ]

    for i, (expected_level, expected_content) in enumerate(levels_and_content):
        heading = doc.children[i]
        assert heading.__class__.__name__ == "MarkdownASTHeadingNode"
        assert heading.level == expected_level
        assert heading.children[0].content == expected_content


def test_bold_text(ast_builder):
    """Test parsing bold text."""
    doc = ast_builder.build_ast("This is **bold** text.")
    assert doc is not None
    assert len(doc.children) == 1
    paragraph = doc.children[0]
    assert paragraph.__class__.__name__ == "MarkdownASTParagraphNode"
    assert len(paragraph.children) == 3
    assert paragraph.children[0].__class__.__name__ == "MarkdownASTTextNode"
    assert paragraph.children[0].content == "This is "
    assert paragraph.children[1].__class__.__name__ == "MarkdownASTBoldNode"
    assert paragraph.children[1].children[0].__class__.__name__ == "MarkdownASTTextNode"
    assert paragraph.children[1].children[0].content == "bold"
    assert paragraph.children[2].__class__.__name__ == "MarkdownASTTextNode"
    assert paragraph.children[2].content == " text."


def test_underscore_formatting(ast_builder, ast_builder_no_underscores):
    """Test underscore formatting behavior."""
    # With underscores enabled
    doc = ast_builder.build_ast("_italic_ and __bold__")
    assert doc.children[0].children[0].__class__.__name__ == "MarkdownASTEmphasisNode"
    assert doc.children[0].children[2].__class__.__name__ == "MarkdownASTBoldNode"

    # With underscores disabled
    doc = ast_builder_no_underscores.build_ast("_italic_ and __bold__")
    assert doc.children[0].children[0].__class__.__name__ == "MarkdownASTTextNode"
    assert doc.children[0].children[0].content == "_italic_ and __bold__"


def test_nested_formatting(ast_builder):
    """Test nested formatting."""
    doc = ast_builder.build_ast("This is **bold with *italic* inside**.")
    paragraph = doc.children[0]
    bold_node = paragraph.children[1]
    assert bold_node.__class__.__name__ == "MarkdownASTBoldNode"
    assert len(bold_node.children) == 3
    assert bold_node.children[0].__class__.__name__ == "MarkdownASTTextNode"
    assert bold_node.children[0].content == "bold with "
    assert bold_node.children[1].__class__.__name__ == "MarkdownASTEmphasisNode"


def test_headings(ast_builder):
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
    assert doc.children[0].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[0].level == 1
    assert doc.children[1].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[1].level == 2
    assert doc.children[2].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[2].level == 1
    assert doc.children[3].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[3].level == 2
    assert doc.children[4].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[4].level == 3
    assert doc.children[5].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[5].level == 3


def test_unordered_list(ast_builder):
    """Test parsing an unordered list."""
    markdown = """
- Item 1
- Item 2
- Item 3
"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(list_node.children) == 3
    for item in list_node.children:
        assert item.__class__.__name__ == "MarkdownASTListItemNode"


def test_ordered_list(ast_builder):
    """Test parsing an ordered list."""
    markdown = """
1. First item
2. Second item
3. Third item
"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTOrderedListNode"
    assert len(list_node.children) == 3
    assert list_node.start == 1


def test_nested_list(ast_builder):
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
    assert nested_list.__class__.__name__ == "MarkdownASTParagraphNode"
    nested_list = first_item.children[1]
    assert nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(nested_list.children) == 2


def test_nested_list2(ast_builder):
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
    assert nested_list.__class__.__name__ == "MarkdownASTParagraphNode"
    first_nested_list = first_item.children[1]
    assert first_nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(first_nested_list.children) == 2
    second_nested_list = first_item.children[2]
    assert second_nested_list.__class__.__name__ == "MarkdownASTOrderedListNode"
    assert len(second_nested_list.children) == 2


def test_nested_list3(ast_builder):
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
    assert nested_list.__class__.__name__ == "MarkdownASTParagraphNode"
    first_nested_list = first_item.children[1]
    assert first_nested_list.__class__.__name__ == "MarkdownASTOrderedListNode"
    assert len(first_nested_list.children) == 2
    second_nested_list = first_item.children[2]
    assert second_nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(second_nested_list.children) == 2


def test_list_with_continuation1(ast_builder):
    """Test parsing a list with continuation."""
    markdown = """
- Item 1
  continues on this line
- Item 2
- Item 3
"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert len(list_node.children) == 3
    first_item = list_node.children[0]
    assert first_item.__class__.__name__ == "MarkdownASTListItemNode"
    paragraph = first_item.children[0]
    assert paragraph.__class__.__name__ == "MarkdownASTParagraphNode"
    assert len(paragraph.children) == 3


def test_list_with_continuation2(ast_builder):
    """Test parsing a list with continuation."""
    markdown = """
- Item 1
- Item 2
    continues on this line
- Item 3
"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert len(list_node.children) == 3
    second_item = list_node.children[1]
    assert second_item.__class__.__name__ == "MarkdownASTListItemNode"
    paragraph = second_item.children[0]
    assert paragraph.__class__.__name__ == "MarkdownASTParagraphNode"
    assert len(paragraph.children) == 3


def test_list_with_continuation3(ast_builder):
    """Test parsing a list with continuation."""
    markdown = """
- Item 1
- Item 2
continues on this line
- Item 3
"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert len(list_node.children) == 3
    second_item = list_node.children[1]
    assert second_item.__class__.__name__ == "MarkdownASTListItemNode"
    paragraph = second_item.children[0]
    assert paragraph.__class__.__name__ == "MarkdownASTParagraphNode"
    assert len(paragraph.children) == 3


def test_list_item_multi_line_paragraph_continuation(ast_builder):
    """
    Test that consecutive lines in a list item form a single paragraph.

    This tests the fix for the bug where _blank_line_count was not being
    reset after handling text continuation, causing each line to be treated
    as a separate paragraph.
    """
    markdown = """- First item with bold text

  Line 1 of paragraph.
  Line 2 of paragraph.
  Line 3 of paragraph.

- Second item

  Line A of paragraph.
  Line B of paragraph.
  Line C of paragraph.
  Line D of paragraph."""

    doc = ast_builder.build_ast(markdown)

    # Should have one unordered list
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"

    # Should have two list items
    assert len(list_node.children) == 2

    # First item should have two paragraphs (the item text and the indented paragraph)
    first_item = list_node.children[0]
    assert len(first_item.children) == 2

    # First paragraph is the item text
    first_para = first_item.children[0]
    assert first_para.__class__.__name__ == "MarkdownASTParagraphNode"

    # Second paragraph should contain all three lines as ONE paragraph
    second_para = first_item.children[1]
    assert second_para.__class__.__name__ == "MarkdownASTParagraphNode"

    # Verify it spans lines 2-4 (the three consecutive lines)
    assert second_para.line_start == 2
    assert second_para.line_end == 4

    # Should contain text nodes with spaces between them
    text_content = []
    for child in second_para.children:
        if child.__class__.__name__ == "MarkdownASTTextNode":
            text_content.append(child.content)

    # Should have: "Line 1 of paragraph." + " " + "Line 2 of paragraph." + " " + "Line 3 of paragraph."
    full_text = "".join(text_content)
    assert "Line 1 of paragraph." in full_text
    assert "Line 2 of paragraph." in full_text
    assert "Line 3 of paragraph." in full_text

    # Second item should also have two paragraphs
    second_item = list_node.children[1]
    assert len(second_item.children) == 2

    # The indented paragraph should contain all four lines as ONE paragraph
    second_item_para = second_item.children[1]
    assert second_item_para.__class__.__name__ == "MarkdownASTParagraphNode"

    # Verify it spans lines 8-11 (the four consecutive lines)
    assert second_item_para.line_start == 8
    assert second_item_para.line_end == 11

    # Should contain all four lines
    text_content = []
    for child in second_item_para.children:
        if child.__class__.__name__ == "MarkdownASTTextNode":
            text_content.append(child.content)

    full_text = "".join(text_content)
    assert "Line A of paragraph." in full_text
    assert "Line B of paragraph." in full_text
    assert "Line C of paragraph." in full_text
    assert "Line D of paragraph." in full_text


def test_list_item_paragraph_with_formatting(ast_builder):
    """
    Test multi-line paragraphs in list items with inline formatting.

    Ensures that paragraph continuation works correctly even when
    the text contains bold, italic, or other formatting.
    """
    markdown = """- **Item with bold header**

  This is the first line of the paragraph.
  This is the second line with **bold text** in it.
  This is the third line with *italic text* too.
  And a fourth line with `inline code`."""

    doc = ast_builder.build_ast(markdown)

    list_node = doc.children[0]
    first_item = list_node.children[0]

    # Should have two children: the bold header paragraph and the multi-line paragraph
    assert len(first_item.children) == 2

    # The second paragraph should be a single paragraph spanning all four lines
    paragraph = first_item.children[1]
    assert paragraph.__class__.__name__ == "MarkdownASTParagraphNode"
    assert paragraph.line_start == 2
    assert paragraph.line_end == 5

    # Should contain various formatting nodes
    has_bold = False
    has_italic = False
    has_code = False

    for child in paragraph.children:
        if child.__class__.__name__ == "MarkdownASTBoldNode":
            has_bold = True
        elif child.__class__.__name__ == "MarkdownASTEmphasisNode":
            has_italic = True
        elif child.__class__.__name__ == "MarkdownASTInlineCodeNode":
            has_code = True

    assert has_bold, "Should contain bold formatting"
    assert has_italic, "Should contain italic formatting"
    assert has_code, "Should contain inline code"


def test_nested_list_paragraph_continuation(ast_builder):
    """
    Test paragraph continuation in nested list items.

    Ensures the fix works correctly for nested list structures.
    """
    markdown = """- Outer item

  First line of outer paragraph.
  Second line of outer paragraph.

  - Nested item

    First line of nested paragraph.
    Second line of nested paragraph.
    Third line of nested paragraph."""

    doc = ast_builder.build_ast(markdown)

    list_node = doc.children[0]
    outer_item = list_node.children[0]

    # Outer item should have: paragraph (item text), paragraph (multi-line), nested list
    assert len(outer_item.children) == 3

    # Check outer multi-line paragraph
    outer_para = outer_item.children[1]
    assert outer_para.__class__.__name__ == "MarkdownASTParagraphNode"
    assert outer_para.line_start == 2
    assert outer_para.line_end == 3

    # Check nested list
    nested_list = outer_item.children[2]
    assert nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"

    nested_item = nested_list.children[0]

    # Nested item should have two paragraphs
    assert len(nested_item.children) == 2

    # Check nested multi-line paragraph
    nested_para = nested_item.children[1]
    assert nested_para.__class__.__name__ == "MarkdownASTParagraphNode"
    assert nested_para.line_start == 7
    assert nested_para.line_end == 9


def test_ordered_tight_list_with_multi_line_items(ast_builder):
    """
    Test that tight lists remain tight even with multi-line items.

    A tight list should not have blank lines between list items,
    but can have multi-line content within each item.
    """
    markdown = """1. First item line 1
   continues on line 2
2. Second item line 1
   continues on line 2
3. Third item"""

    doc = ast_builder.build_ast(markdown)

    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTOrderedListNode"

    # Should be a tight list (no blank lines between items)
    assert list_node.tight is True

    # First item should have a single paragraph with both lines
    first_item = list_node.children[0]
    first_para = first_item.children[0]
    assert first_para.line_start == 0
    assert first_para.line_end == 1

    # Second item should also have a single paragraph with both lines
    second_item = list_node.children[1]
    second_para = second_item.children[0]
    assert second_para.line_start == 2
    assert second_para.line_end == 3


def test_ordered_loose_list_with_multi_line_paragraphs(ast_builder):
    """
    Test that loose lists with multi-line paragraphs are handled correctly.

    A loose list has blank lines between items, and each item can have
    multi-line paragraphs.
    """
    markdown = """1. First item

   First paragraph line 1.
   First paragraph line 2.
   
   Second paragraph line 1.
   Second paragraph line 2.

2. Second item

   Another paragraph line 1.
   Another paragraph line 2."""

    doc = ast_builder.build_ast(markdown)

    list_node = doc.children[0]

    # Should be a loose list (has blank lines between items)
    assert list_node.tight is False

    # First item should have three paragraphs:
    # 1. The item text ("First item")
    # 2. First multi-line paragraph
    # 3. Second multi-line paragraph
    first_item = list_node.children[0]
    assert len(first_item.children) == 3

    # Check first multi-line paragraph
    first_para = first_item.children[1]
    assert first_para.line_start == 2
    assert first_para.line_end == 3

    # Check second multi-line paragraph
    second_para = first_item.children[2]
    assert second_para.line_start == 5
    assert second_para.line_end == 6

    # Second item should have two paragraphs
    second_item = list_node.children[1]
    assert len(second_item.children) == 2

    # Check the multi-line paragraph
    second_item_para = second_item.children[1]
    assert second_item_para.line_start == 10
    assert second_item_para.line_end == 11


def test_unordered_tight_list_with_multi_line_items(ast_builder):
    """
    Test that tight lists remain tight even with multi-line items.

    A tight list should not have blank lines between list items,
    but can have multi-line content within each item.
    """
    markdown = """- First item line 1
  continues on line 2
- Second item line 1
  continues on line 2
- Third item"""

    doc = ast_builder.build_ast(markdown)

    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"

    # Should be a tight list (no blank lines between items)
    assert list_node.tight is True

    # First item should have a single paragraph with both lines
    first_item = list_node.children[0]
    first_para = first_item.children[0]
    assert first_para.line_start == 0
    assert first_para.line_end == 1

    # Second item should also have a single paragraph with both lines
    second_item = list_node.children[1]
    second_para = second_item.children[0]
    assert second_para.line_start == 2
    assert second_para.line_end == 3


def test_unordered_loose_list_with_multi_line_paragraphs(ast_builder):
    """
    Test that loose lists with multi-line paragraphs are handled correctly.

    A loose list has blank lines between items, and each item can have
    multi-line paragraphs.
    """
    markdown = """- First item

  First paragraph line 1.
  First paragraph line 2.
  
  Second paragraph line 1.
  Second paragraph line 2.

- Second item

  Another paragraph line 1.
  Another paragraph line 2."""

    doc = ast_builder.build_ast(markdown)

    list_node = doc.children[0]

    # Should be a loose list (has blank lines between items)
    assert list_node.tight is False

    # First item should have three paragraphs:
    # 1. The item text ("First item")
    # 2. First multi-line paragraph
    # 3. Second multi-line paragraph
    first_item = list_node.children[0]
    assert len(first_item.children) == 3

    # Check first multi-line paragraph
    first_para = first_item.children[1]
    assert first_para.line_start == 2
    assert first_para.line_end == 3

    # Check second multi-line paragraph
    second_para = first_item.children[2]
    assert second_para.line_start == 5
    assert second_para.line_end == 6

    # Second item should have two paragraphs
    second_item = list_node.children[1]
    assert len(second_item.children) == 2

    # Check the multi-line paragraph
    second_item_para = second_item.children[1]
    assert second_item_para.line_start == 10
    assert second_item_para.line_end == 11


def test_code_block(ast_builder):
    """Test parsing a code block."""
    markdown = """```python
def hello():
print("Hello, world!")
```"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    code_block = doc.children[0]
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert code_block.language == ProgrammingLanguage.PYTHON
    assert "def hello():" in code_block.content
    assert "print(\"Hello, world!\")" in code_block.content


def test_table(ast_builder):
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
    assert table.__class__.__name__ == "MarkdownASTTableNode"
    assert len(table.children) == 2

    # Check header
    header = table.children[0]
    assert header.__class__.__name__ == "MarkdownASTTableHeaderNode"
    assert len(header.children) == 1  # One row

    # Check body
    body = table.children[1]
    assert body.__class__.__name__ == "MarkdownASTTableBodyNode"
    assert len(body.children) == 2  # Two rows

def test_horizontal_rule(ast_builder):
    """Test parsing a horizontal rule."""
    markdown = """
Before rule

---

After rule
"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 3
    assert doc.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTHorizontalRuleNode"
    assert doc.children[2].__class__.__name__ == "MarkdownASTParagraphNode"


def test_link(ast_builder):
    """Test parsing a link."""
    doc = ast_builder.build_ast("Here is a [link](https://example.com) to a website.")
    paragraph = doc.children[0]
    assert len(paragraph.children) == 3
    link = paragraph.children[1]
    assert link.__class__.__name__ == "MarkdownASTLinkNode"
    assert link.url == "https://example.com"
    assert len(link.children) == 1
    assert link.children[0].content == "link"


def test_image(ast_builder):
    """Test parsing an image."""
    doc = ast_builder.build_ast("![Alt text](image.jpg \"Image title\")")
    paragraph = doc.children[0]
    image = paragraph.children[0]
    assert image.__class__.__name__ == "MarkdownASTImageNode"
    assert image.url == "image.jpg"
    assert image.alt_text == "Alt text"
    assert image.title == "Image title"


def test_line_to_node_mapping(ast_builder):
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


def test_update_ast(ast_builder):
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


def test_complex_nested_formatting(ast_builder):
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
        assert paragraph.__class__.__name__ == "MarkdownASTParagraphNode"


def test_malformed_formatting_edge_cases(ast_builder):
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


def test_complex_link_parentheses(ast_builder):
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
            assert image.__class__.__name__ == "MarkdownASTImageNode"
            assert '(' in image.url and ')' in image.url

        else:
            # Link
            link = paragraph.children[0]
            assert link.__class__.__name__ == "MarkdownASTLinkNode"
            assert '(' in link.url and ')' in link.url


def test_inline_code_with_formatting_inside(ast_builder):
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
        assert code_node.__class__.__name__ == "MarkdownASTInlineCodeNode"
        # Content should be preserved as-is without formatting
        assert "**" in code_node.content or "*" in code_node.content or "__" in code_node.content or "_" in code_node.content or "[" in code_node.content


def test_incomplete_link_info(ast_builder):
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
        assert image.__class__.__name__ == "MarkdownASTTextNode"


def test_code_block_with_programming_languages(ast_builder):
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
        assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
        assert len(code_block.content.strip()) > 0


def test_code_block_continuation_edge_cases(ast_builder):
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
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert "Nested code block" in code_block.content


def test_code_block_with_mixed_content(ast_builder):
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
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    # Content should be preserved as-is
    assert "# This looks like a heading" in code_block.content
    assert "- This looks like a list" in code_block.content


def test_loose_vs_tight_ordered_lists(ast_builder):
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
    assert list_node.__class__.__name__ == "MarkdownASTOrderedListNode"
    assert list_node.tight is True

    # Test loose list
    doc = ast_builder.build_ast(loose_list)
    list_node = doc.children[0]
    assert list_node.tight is False

    # Test mixed list
    doc = ast_builder.build_ast(mixed_list)
    list_node = doc.children[0]
    assert list_node.tight is False


def test_loose_vs_tight_unordered_lists(ast_builder):
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
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert list_node.tight is True

    # Test loose list
    doc = ast_builder.build_ast(loose_list)
    list_node = doc.children[0]
    assert list_node.tight is False

    # Test mixed list
    doc = ast_builder.build_ast(mixed_list)
    list_node = doc.children[0]
    assert list_node.tight is False


def test_blank_line_before_list_should_not_affect_tightness(ast_builder):
    """Test that blank lines BEFORE a list do not affect tight/loose status.

    According to CommonMark spec, only blank lines BETWEEN list items
    should affect whether a list is tight or loose. Blank lines before
    the list (e.g., after a heading) should not make the list loose.
    """
    # Blank line after heading, no blank lines between items -> should be TIGHT
    list_after_heading_with_blank = """## Features

- **Pure list representation**: Everything is data
- **Pure functional**: No side effects
- **S-expression syntax**: `(function-or-operator arg1 arg2 ...)`
"""

    # No blank line after heading, no blank lines between items -> should be TIGHT
    list_after_heading_no_blank = """## Features
- **Pure list representation**: Everything is data
- **Pure functional**: No side effects
- **S-expression syntax**: `(function-or-operator arg1 arg2 ...)`
"""

    # Test list with blank line before it
    doc = ast_builder.build_ast(list_after_heading_with_blank)
    assert len(doc.children) == 2
    list_node = doc.children[1]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert list_node.tight is True, "List should be tight - blank line before list should not affect tightness"

    # Test list without blank line before it
    doc = ast_builder.build_ast(list_after_heading_no_blank)
    assert len(doc.children) == 2
    list_node = doc.children[1]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert list_node.tight is True, "List should be tight - no blank lines between items"

def test_complex_nested_list_structures(ast_builder):
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
    assert main_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(main_list.children) == 3

    # Check first item has nested unordered list
    first_item = main_list.children[0]
    assert len(first_item.children) == 2  # paragraph + nested list
    nested_list = first_item.children[1]
    assert nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"

    # Check second item has nested ordered list
    second_item = main_list.children[1]
    assert len(second_item.children) == 2  # paragraph + nested list
    ordered_nested = second_item.children[1]
    assert ordered_nested.__class__.__name__ == "MarkdownASTOrderedListNode"

def test_list_continuation_and_interruption(ast_builder):
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
    assert doc.children[0].__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[2].__class__.__name__ == "MarkdownASTUnorderedListNode"


def test_ordered_list_with_custom_start(ast_builder):
    """Test ordered lists with non-standard start numbers."""
    custom_start = """5. Fifth item
6. Sixth item
7. Seventh item"""

    doc = ast_builder.build_ast(custom_start)
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTOrderedListNode"
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


def test_table_alignment_variations(ast_builder):
    """Test different table column alignments."""
    table_markdown = """| Left | Center | Right |
|:-----|:------:|------:|
| L1   | C1     | R1    |
| L2   | C2     | R2    |"""

    doc = ast_builder.build_ast(table_markdown)
    assert len(doc.children) == 1
    table = doc.children[0]
    assert table.__class__.__name__ == "MarkdownASTTableNode"

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


def test_double_header_row(ast_builder):
    """Test different table column alignments."""
    table_markdown = """| Left | Center | Right |
| Lef2 | Cen2   | Rig2  |
|------|--------|-------|
| L1   | C1     | R1    |
| L2   | C2     | R2    |"""

    doc = ast_builder.build_ast(table_markdown)
    assert len(doc.children) == 2
    text = doc.children[0]
    assert text.__class__.__name__ == "MarkdownASTParagraphNode"

    table = doc.children[1]
    assert table.__class__.__name__ == "MarkdownASTTableNode"

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


def test_incomplete_table_alignment_variations(ast_builder):
    """Test different table column alignments."""
    table_markdown = """| Left | Center | Right |
|:-----|:------:|------:|"""

    doc = ast_builder.build_ast(table_markdown)
    assert len(doc.children) == 2
    line0 = doc.children[0]
    assert line0.__class__.__name__ == "MarkdownASTParagraphNode"
    assert line0.children[0].content == "| Left | Center | Right |"
    line1 = doc.children[1]
    assert line1.__class__.__name__ == "MarkdownASTParagraphNode"
    assert line1.children[0].content == "|:-----|:------:|------:|"


def test_solitary_table_separator(ast_builder):
    """Test different table column alignments."""
    table_markdown = """|:-----|:------:|------:|"""

    doc = ast_builder.build_ast(table_markdown)
    assert len(doc.children) == 1
    line0 = doc.children[0]
    assert line0.__class__.__name__ == "MarkdownASTParagraphNode"
    assert line0.children[0].content == "|:-----|:------:|------:|"


def test_table_with_complex_content(ast_builder):
    """Test tables containing formatted text."""
    complex_table = """| **Bold** | *Italic* | `Code` |
|----------|----------|--------|
| **B1**   | *I1*     | `C1`   |
| [Link](url) | ![Image](img.jpg) | Normal |"""

    doc = ast_builder.build_ast(complex_table)
    assert len(doc.children) == 1
    table = doc.children[0]
    assert table.__class__.__name__ == "MarkdownASTTableNode"

    # Check header contains formatted content
    header = table.children[0]
    header_row = header.children[0]

    # First header cell should contain bold text
    bold_cell = header_row.children[0]
    assert len(bold_cell.children) == 1
    assert bold_cell.children[0].__class__.__name__ == "MarkdownASTBoldNode"

    # Second header cell should contain italic text
    italic_cell = header_row.children[1]
    assert len(italic_cell.children) == 1
    assert italic_cell.children[0].__class__.__name__ == "MarkdownASTEmphasisNode"

    # Third header cell should contain code
    code_cell = header_row.children[2]
    assert len(code_cell.children) == 1
    assert code_cell.children[0].__class__.__name__ == "MarkdownASTInlineCodeNode"


def test_table_edge_cases(ast_builder):
    """Test table parsing edge cases."""
    # Table with missing cells
    uneven_table = """| Col1 | Col2 | Col3 | Col4 |
|------|------|------|
| A1   | A2   |
| B1   | B2   | B3   | B4 |"""

    doc = ast_builder.build_ast(uneven_table)
    assert len(doc.children) == 1
    table = doc.children[0]
    assert table.__class__.__name__ == "MarkdownASTTableNode"

    # Should handle uneven rows gracefully
    body = table.children[1]
    assert len(body.children) == 2  # Two body rows


def test_paragraph_continuation(ast_builder):
    """Test paragraph continuation across multiple lines."""
    paragraph_continuation = """This is the first line
and this continues the same paragraph
with even more text on this line."""

    doc = ast_builder.build_ast(paragraph_continuation)
    assert len(doc.children) == 1
    paragraph = doc.children[0]
    assert paragraph.__class__.__name__ == "MarkdownASTParagraphNode"

    # Should contain all text with spaces between lines
    full_text = ""
    for child in paragraph.children:
        if child.__class__.__name__ == "MarkdownASTTextNode":
            full_text += child.content

    assert "This is the first line" in full_text
    assert "and this continues" in full_text
    assert "with even more text" in full_text


def test_list_item_continuation(ast_builder):
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
        if child.__class__.__name__ == "MarkdownASTTextNode":
            full_text += child.content

    assert "First list item" in full_text
    assert "continues on this line" in full_text
    assert "and continues further" in full_text


def test_continuation_with_line_breaks(ast_builder):
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
        if child.__class__.__name__ == "MarkdownASTLineBreakNode":
            has_line_break = True
            break

    assert has_line_break


def test_continuation_edge_cases(ast_builder):
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
    # Should have heading, paragraph, list, and another paragraph
    assert len(doc.children) == 4
    assert doc.children[0].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[2].__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert doc.children[3].__class__.__name__ == "MarkdownASTParagraphNode"


def test_unclosed_code_block(ast_builder):
    """Test handling of unclosed code blocks."""
    markdown = """```python
def unclosed_function():
    print("This code block is not closed")
"""
    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    code_block = doc.children[0]
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert code_block.language == ProgrammingLanguage.PYTHON


def test_nested_code_blocks(ast_builder):
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
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert "Nested code block" in code_block.content


def test_incomplete_table(ast_builder):
    """Test handling of incomplete tables."""
    markdown = """
| Header 1 | Header 2 |
|----------|----------|
"""
    doc = ast_builder.build_ast(markdown)
    # The AST builder should not create a table without any body rows
    assert len(doc.children) == 2
    assert doc.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"


def test_invalid_table_separator(ast_builder):
    """Test handling of invalid table separators."""
    markdown = """
| Header 1 | Header 2 |
| Invalid | Separator |
| Cell 1   | Cell 2   |
"""
    doc = ast_builder.build_ast(markdown)
    # Should be treated as paragraphs
    assert len(doc.children) == 3
    assert all(child.__class__.__name__ == "MarkdownASTParagraphNode" for child in doc.children)


def test_mixed_list_markers(ast_builder):
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
        assert child.__class__.__name__ == "MarkdownASTUnorderedListNode"


def test_empty_elements(ast_builder):
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


@pytest.fixture
def ast_builder():
    """Fixture providing a markdown AST builder instance."""
    return MarkdownASTBuilder(no_underscores=False)


def test_first_update_with_none_previous(ast_builder):
    """Test update_ast when previous_text is None (first update)."""
    text = "# Initial heading\n\nInitial paragraph."

    # First update with None previous text should build from scratch
    doc = ast_builder.update_ast(text, None)

    assert doc is not None
    assert len(doc.children) == 2
    assert doc.children[0].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[0].children[0].content == "Initial heading"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].children[0].content == "Initial paragraph."


def test_update_with_empty_document(ast_builder):
    """Test update_ast when document is empty."""
    previous = ""
    new_text = "# New heading"

    # Build empty document first
    ast_builder.build_ast(previous)

    # Update with new content
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 1
    assert doc.children[0].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[0].children[0].content == "New heading"


def test_update_to_empty_document(ast_builder):
    """Test update_ast when updating to empty content."""
    previous = "# Heading to remove\n\nParagraph to remove."
    new_text = ""

    # Build initial document
    ast_builder.build_ast(previous)

    # Update to empty
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 0


def test_simple_text_change(ast_builder):
    """Test simple text content changes."""
    previous = "# Original heading\n\nOriginal paragraph."
    new_text = "# Updated heading\n\nUpdated paragraph."

    # Build original
    ast_builder.build_ast(previous)

    # Update
    doc = ast_builder.update_ast(new_text, previous)

    assert doc.children[0].children[0].content == "Updated heading"
    assert doc.children[1].children[0].content == "Updated paragraph."


def test_heading_level_change(ast_builder):
    """Test changing heading levels."""
    previous = "# Level 1\n## Level 2\n### Level 3"
    new_text = "## Level 2\n# Level 1\n#### Level 4"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 3
    assert doc.children[0].level == 2
    assert doc.children[1].level == 1
    assert doc.children[2].level == 4


def test_structure_type_change(ast_builder):
    """Test changing element types (heading to paragraph, etc.)."""
    previous = "# Heading\n\nParagraph\n\n- List item"
    new_text = "Regular text\n\n## New heading\n\nNew paragraph"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 3
    assert doc.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[1].level == 2
    assert doc.children[2].__class__.__name__ == "MarkdownASTParagraphNode"


def test_add_content_beginning(ast_builder):
    """Test adding content at the beginning."""
    previous = "# Existing heading\n\nExisting paragraph."
    new_text = "# New first heading\n\nNew paragraph.\n\n# Existing heading\n\nExisting paragraph."

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 4
    assert doc.children[0].children[0].content == "New first heading"
    assert doc.children[2].children[0].content == "Existing heading"


def test_add_content_middle(ast_builder):
    """Test adding content in the middle."""
    previous = "# First\n\n# Third"
    new_text = "# First\n\n# Second\n\n# Third"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 3
    assert doc.children[0].children[0].content == "First"
    assert doc.children[1].children[0].content == "Second"
    assert doc.children[2].children[0].content == "Third"


def test_add_content_end(ast_builder):
    """Test adding content at the end."""
    previous = "# Existing heading"
    new_text = "# Existing heading\n\n# New ending"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 2
    assert doc.children[0].children[0].content == "Existing heading"
    assert doc.children[1].children[0].content == "New ending"


def test_remove_content_beginning(ast_builder):
    """Test removing content from the beginning."""
    previous = "# Remove me\n\n# Keep me\n\nKeep this paragraph."
    new_text = "# Keep me\n\nKeep this paragraph."

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 2
    assert doc.children[0].children[0].content == "Keep me"
    assert doc.children[1].children[0].content == "Keep this paragraph."


def test_remove_content_middle(ast_builder):
    """Test removing content from the middle."""
    previous = "# First\n\n# Remove me\n\n# Last"
    new_text = "# First\n\n# Last"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 2
    assert doc.children[0].children[0].content == "First"
    assert doc.children[1].children[0].content == "Last"


def test_remove_content_end(ast_builder):
    """Test removing content from the end."""
    previous = "# Keep me\n\n# Remove me"
    new_text = "# Keep me"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 1
    assert doc.children[0].children[0].content == "Keep me"


def test_list_modifications(ast_builder):
    """Test modifications to list structures."""
    previous = """- Item 1
- Item 2
- Item 3"""

    new_text = """- Modified Item 1
- Item 2
- Item 3
- New Item 4"""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(list_node.children) == 4

    # Check first item was modified
    first_item = list_node.children[0]
    first_paragraph = first_item.children[0]
    assert first_paragraph.children[0].content == "Modified Item 1"


def test_list_type_change(ast_builder):
    """Test changing list types (unordered to ordered)."""
    previous = """- Item 1
- Item 2
- Item 3"""

    new_text = """1. Item 1
2. Item 2
3. Item 3"""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTOrderedListNode"
    assert len(list_node.children) == 3


def test_nested_list_modifications(ast_builder):
    """Test modifications to nested list structures."""
    previous = """- Item 1
  - Nested 1
  - Nested 2
- Item 2"""

    new_text = """- Item 1
  - Nested 1
  - Modified Nested 2
  - New Nested 3
- Item 2
- New Item 3"""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    main_list = doc.children[0]
    assert len(main_list.children) == 3

    # Check nested list was modified
    first_item = main_list.children[0]
    nested_list = first_item.children[1]
    assert len(nested_list.children) == 3


def test_table_modifications(ast_builder):
    """Test modifications to table structures."""
    previous = """| Col1 | Col2 |
|------|------|
| A1   | A2   |
| B1   | B2   |"""

    new_text = """| Col1 | Col2 | Col3 |
|------|------|------|
| A1   | A2   | A3   |
| B1   | B2   | B3   |
| C1   | C2   | C3   |"""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    table = doc.children[0]
    assert table.__class__.__name__ == "MarkdownASTTableNode"

    # Check header has 3 columns
    header = table.children[0]
    header_row = header.children[0]
    assert len(header_row.children) == 3

    # Check body has 3 rows
    body = table.children[1]
    assert len(body.children) == 3


def test_table_to_paragraph_conversion(ast_builder):
    """Test converting table to paragraphs by removing separator."""
    previous = """| Col1 | Col2 |
|------|------|
| A1   | A2   |"""

    new_text = """| Col1 | Col2 |
| A1   | A2   |"""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    # Should be converted to paragraphs
    assert len(doc.children) == 2
    assert doc.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"


def test_code_block_modifications(ast_builder):
    """Test modifications to code blocks."""
    previous = """```python
def old_function():
    return "old"
```"""

    new_text = """```python
def new_function():
    return "new"

def another_function():
    return "another"
```"""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    code_block = doc.children[0]
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert "new_function" in code_block.content
    assert "another_function" in code_block.content


def test_code_block_language_change(ast_builder):
    """Test changing code block language."""
    previous = """```python
print("hello")
```"""

    new_text = """```javascript
console.log("hello");
```"""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    code_block = doc.children[0]
    assert code_block.language == ProgrammingLanguage.JAVASCRIPT
    assert code_block.content.strip() == 'console.log("hello");'


def test_inline_formatting_changes(ast_builder):
    """Test changes to inline formatting."""
    previous = "This has **bold** text."
    new_text = "This has *italic* text."

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    paragraph = doc.children[0]
    formatting_node = paragraph.children[1]
    assert formatting_node.__class__.__name__ == "MarkdownASTEmphasisNode"


def test_link_modifications(ast_builder):
    """Test modifications to links."""
    previous = "Visit [old site](http://old.com) for info."
    new_text = "Visit [new site](http://new.com) for info."

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    paragraph = doc.children[0]
    link = paragraph.children[1]
    assert link.__class__.__name__ == "MarkdownASTLinkNode"
    assert link.url == "http://new.com"
    assert link.children[0].content == "new site"


def test_image_modifications(ast_builder):
    """Test modifications to images."""
    previous = "![old alt](old.jpg)"
    new_text = "![new alt](new.jpg \"New title\")"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    paragraph = doc.children[0]
    image = paragraph.children[0]
    assert image.__class__.__name__ == "MarkdownASTImageNode"
    assert image.url == "new.jpg"
    assert image.alt_text == "new alt"
    assert image.title == "New title"


def test_horizontal_rule_changes(ast_builder):
    """Test changes involving horizontal rules."""
    previous = "Before\n\n---\n\nAfter"
    new_text = "Before\n\nMiddle\n\nAfter"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    assert len(doc.children) == 3
    assert doc.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].children[0].content == "Middle"
    assert doc.children[2].__class__.__name__ == "MarkdownASTParagraphNode"


def test_multiple_updates_sequence(ast_builder):
    """Test a sequence of multiple updates."""
    # Start with simple content
    content1 = "# Initial"
    doc = ast_builder.update_ast(content1, None)
    assert len(doc.children) == 1

    # Add content
    content2 = "# Initial\n\nAdded paragraph."
    doc = ast_builder.update_ast(content2, content1)
    assert len(doc.children) == 2

    # Modify content
    content3 = "# Modified\n\nAdded paragraph."
    doc = ast_builder.update_ast(content3, content2)
    assert doc.children[0].children[0].content == "Modified"

    # Remove content
    content4 = "# Modified"
    doc = ast_builder.update_ast(content4, content3)
    assert len(doc.children) == 1


def test_line_break_modifications(ast_builder):
    """Test modifications involving line breaks."""
    previous = "Line 1  \nLine 2\nLine 3"
    new_text = "Line 1\nLine 2  \nLine 3"

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    paragraph = doc.children[0]
    # Should have line break in different position
    has_line_break = any(
        child.__class__.__name__ == "MarkdownASTLineBreakNode"
        for child in paragraph.children
    )
    assert has_line_break


def test_source_path_preservation(ast_builder):
    """Test that source path is preserved during updates."""
    previous = "# Heading"
    new_text = "# Updated heading"

    # Build with source path
    doc = ast_builder.update_ast(previous, None, path="/test/path.md")
    assert doc.source_path == "/test/path.md"

    # Update should preserve path
    doc = ast_builder.update_ast(new_text, previous, path="/test/path.md")
    assert doc.source_path == "/test/path.md"


def test_complex_document_update(ast_builder):
    """Test updating a complex document with multiple element types."""
    previous = """# Document Title

Introduction paragraph with **bold** and *italic* text.

## Section 1

- List item 1
- List item 2
  - Nested item

Here's a [link](http://example.com) and an image:
![Alt text](image.jpg)

```python
def hello():
    print("world")
```

| Col1 | Col2 |
|------|------|
| A    | B    |

---

Final paragraph."""

    new_text = """# Updated Document Title

Modified introduction paragraph with **bold** text only.

## Modified Section 1

1. Ordered item 1
2. Ordered item 2
   - Mixed nested item

Here's an updated [link](http://newexample.com):

```javascript
function hello() {
    console.log("world");
}
```

| Col1 | Col2 | Col3 |
|------|------|------|
| A    | B    | C    |
| D    | E    | F    |

Updated final paragraph."""

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    # Verify structure changes
    assert len(doc.children) > 5  # Should have multiple elements

    # Check title was updated
    title = doc.children[0]
    assert title.children[0].content == "Updated Document Title"

    # Check list type changed to ordered
    list_element = None
    for child in doc.children:
        if child.__class__.__name__ == "MarkdownASTOrderedListNode":
            list_element = child
            break
    assert list_element is not None

    # Check code block language changed
    code_element = None
    for child in doc.children:
        if child.__class__.__name__ == "MarkdownASTCodeBlockNode":
            code_element = child
            break
    assert code_element is not None
    assert code_element.language == ProgrammingLanguage.JAVASCRIPT


def test_whitespace_only_changes(ast_builder):
    """Test updates that only change whitespace."""
    previous = "#Heading\nParagraph."
    new_text = "# Heading\n\nParagraph."

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    # Structure should be the same but properly formatted
    assert len(doc.children) == 2
    assert doc.children[0].__class__.__name__ == "MarkdownASTHeadingNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"


def test_identical_content_update(ast_builder):
    """Test updating with identical content."""
    content = "# Same heading\n\nSame paragraph."

    ast_builder.build_ast(content)
    original_doc = ast_builder.document()

    doc = ast_builder.update_ast(content, content)

    # Should return same structure
    assert len(doc.children) == len(original_doc.children)
    assert doc.children[0].children[0].content == "Same heading"


def test_update_with_malformed_content(ast_builder):
    """Test updates with malformed markdown content."""
    previous = "# Valid heading"
    new_text = "**Unclosed bold\n`Unclosed code\n[Unclosed link("

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    # Should handle malformed content gracefully
    assert doc is not None
    assert len(doc.children) >= 1


def test_very_large_content_change(ast_builder):
    """Test updating with very large content changes."""
    # Create large previous content
    previous_lines = []
    for i in range(100):
        previous_lines.append(f"# Heading {i}")
        previous_lines.append(f"Paragraph {i} content.")
        previous_lines.append("")
    previous = "\n".join(previous_lines)

    # Create completely different large content
    new_lines = []
    for i in range(150):
        new_lines.append(f"## New Heading {i}")
        new_lines.append(f"- List item {i}")
        new_lines.append("")
    new_text = "\n".join(new_lines)

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    # Should handle large changes without error
    assert doc is not None
    assert len(doc.children) > 100


def test_update_preserves_line_mapping(ast_builder):
    """Test that line mapping is properly updated."""
    previous = "# Line 0\n\nLine 2 paragraph."
    new_text = "# Line 0\n\n## Line 2 heading\n\nLine 4 paragraph."

    ast_builder.build_ast(previous)
    doc = ast_builder.update_ast(new_text, previous)

    # Verify line mapping exists and has correct entries
    assert hasattr(ast_builder, '_line_to_node_map')
    assert len(ast_builder._line_to_node_map) > 0

    # Check specific line mappings
    assert 0 in ast_builder._line_to_node_map  # First heading
    assert 2 in ast_builder._line_to_node_map  # Second heading
    assert 4 in ast_builder._line_to_node_map  # Paragraph



def test_simple_blockquote(ast_builder):
    """Test parsing a simple blockquote."""
    markdown = """> This is a quote
> It continues here"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1

    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Should have one paragraph with both lines merged (CommonMark behavior)
    assert len(blockquote.children) == 1
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"


def test_blockquote_with_multiple_paragraphs(ast_builder):
    """Test blockquote with multiple paragraphs separated by blank line."""
    markdown = """> First paragraph
>
> Second paragraph"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1

    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Should have two paragraphs
    assert len(blockquote.children) == 2
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTParagraphNode"


def test_blockquote_with_list(ast_builder):
    """Test blockquote containing a list."""
    markdown = """> Quote with list:
>
> - Item 1
> - Item 2"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1

    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Should have paragraph and list
    assert len(blockquote.children) == 2
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTUnorderedListNode"

    # List should have 2 items
    list_node = blockquote.children[1]
    assert len(list_node.children) == 2


def test_nested_blockquotes(ast_builder):
    """Test nested blockquotes."""
    markdown = """> Outer quote
>
> > Nested quote
>
> Back to outer"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1

    outer_blockquote = doc.children[0]
    assert outer_blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Outer should have: paragraph, nested blockquote, paragraph
    assert len(outer_blockquote.children) == 3
    assert outer_blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert outer_blockquote.children[1].__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert outer_blockquote.children[2].__class__.__name__ == "MarkdownASTParagraphNode"

    # Nested blockquote should have one paragraph
    nested_blockquote = outer_blockquote.children[1]
    assert len(nested_blockquote.children) == 1
    assert nested_blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"


def test_blockquote_interruption(ast_builder):
    """Test that blank line interrupts blockquote."""
    markdown = """> Quote line 1
> Quote line 2

Not in quote

> New quote"""

    doc = ast_builder.build_ast(markdown)

    # Should have: blockquote, paragraph, blockquote
    assert len(doc.children) == 3
    assert doc.children[0].__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[2].__class__.__name__ == "MarkdownASTBlockquoteNode"

    # First blockquote should have 1 paragraph (consecutive lines merge)
    first_blockquote = doc.children[0]
    assert len(first_blockquote.children) == 1


def test_blockquote_with_code_block(ast_builder):
    """Test blockquote containing a code block."""
    markdown = """> Quote with code:
>
> ```python
> print("hello")
> ```"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1

    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Should have paragraph and code block
    assert len(blockquote.children) == 2
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"


def test_empty_blockquote(ast_builder):
    """Test empty blockquote line."""
    markdown = """>"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1

    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Should have no children (empty blockquote)
    assert len(blockquote.children) == 0


def test_blockquote_with_heading(ast_builder):
    """Test that heading stays inside blockquote (CommonMark behavior)."""
    markdown = """> # Heading in quote
> 
> Paragraph after heading"""

    doc = ast_builder.build_ast(markdown)
    # Heading stays inside blockquote (CommonMark allows headings in blockquotes)
    assert len(doc.children) == 1

    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert len(blockquote.children) == 2  # Heading and paragraph
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTHeadingNode"
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTParagraphNode"

def test_blockquote_with_list_and_continuation(ast_builder):
    """Test blockquote containing a list followed by more blockquote content."""
    markdown = """> This is a quote
>
> - List in quote
> - Another item
>
> Another paragraph in quote"""

    doc = ast_builder.build_ast(markdown)

    # Should have one blockquote at document level
    assert len(doc.children) == 1
    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Blockquote should have 3 children: paragraph, list, paragraph
    assert len(blockquote.children) == 3

    # First child: paragraph
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[0].children[0].content == "This is a quote"

    # Second child: unordered list
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTUnorderedListNode"
    list_node = blockquote.children[1]
    assert len(list_node.children) == 2

    # First list item should have only one child (the paragraph)
    first_item = list_node.children[0]
    assert len(first_item.children) == 1
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[0].children[0].content == "List in quote"

    # Second list item should have only one child (the paragraph)
    second_item = list_node.children[1]
    assert len(second_item.children) == 1
    assert second_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert second_item.children[0].children[0].content == "Another item"

    # Third child: paragraph (back to blockquote level)
    assert blockquote.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[2].children[0].content == "Another paragraph in quote"


def test_table_with_uneven_columns(ast_builder):
    """Test table where body rows have more columns than header."""
    markdown = """| A |
|---|---|
| B | C |"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    assert doc.children[0].__class__.__name__ == "MarkdownASTTableNode"

    table = doc.children[0]
    assert len(table.children) == 2  # header and body

    # Check header
    header = table.children[0]
    assert header.__class__.__name__ == "MarkdownASTTableHeaderNode"
    assert len(header.children) == 1  # one row

    header_row = header.children[0]
    assert len(header_row.children) == 2  # Should be padded to 2 columns

    # First cell should have "A"
    assert header_row.children[0].children[0].content == "A"

    # Second cell should be empty (padded) - empty strings produce no children
    # This is correct behavior: _parse_inline_formatting("") returns []
    assert len(header_row.children[1].children) == 0

    # Check body
    body = table.children[1]
    assert body.__class__.__name__ == "MarkdownASTTableBodyNode"
    assert len(body.children) == 1  # one row

    body_row = body.children[0]
    assert len(body_row.children) == 2
    assert body_row.children[0].children[0].content == "B"
    assert body_row.children[1].children[0].content == "C"


def test_table_with_multiple_uneven_rows(ast_builder):
    """Test table with varying column counts across rows."""
    markdown = """| A | B |
|---|---|---|
| C | D | E |
| F |"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 1
    table = doc.children[0]

    # All rows should be normalized to 3 columns (max from separator)
    header_row = table.children[0].children[0]
    assert len(header_row.children) == 3
    assert header_row.children[0].children[0].content == "A"
    assert header_row.children[1].children[0].content == "B"
    assert len(header_row.children[2].children) == 0  # padded, empty

    body = table.children[1]
    # First body row
    assert len(body.children[0].children) == 3
    assert body.children[0].children[0].children[0].content == "C"
    assert body.children[0].children[1].children[0].content == "D"
    assert body.children[0].children[2].children[0].content == "E"

    # Second body row (padded)
    assert len(body.children[1].children) == 3
    assert body.children[1].children[0].children[0].content == "F"
    assert len(body.children[1].children[1].children) == 0  # padded, empty
    assert len(body.children[1].children[2].children) == 0  # padded, empty


def test_blockquote_reentry_same_level(ast_builder):
    """Test re-entering a blockquote at the same level after exiting."""
    markdown = """> Quote 1
> 
> Quote 2

Regular text

> Quote 3"""

    doc = ast_builder.build_ast(markdown)

    # Should have 3 top-level children: blockquote, paragraph, blockquote
    assert len(doc.children) == 3

    # First blockquote
    assert doc.children[0].__class__.__name__ == "MarkdownASTBlockquoteNode"
    blockquote1 = doc.children[0]
    assert len(blockquote1.children) == 2  # Two paragraphs
    assert blockquote1.children[0].children[0].content == "Quote 1"
    assert blockquote1.children[1].children[0].content == "Quote 2"

    # Regular paragraph
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].children[0].content == "Regular text"

    # Second blockquote (re-entered at same level)
    assert doc.children[2].__class__.__name__ == "MarkdownASTBlockquoteNode"
    blockquote2 = doc.children[2]
    assert len(blockquote2.children) == 1
    assert blockquote2.children[0].children[0].content == "Quote 3"


def test_deep_nested_lists_with_blank_lines(ast_builder):
    """Test deeply nested lists with blank lines and content at various levels."""
    markdown = """- Level 1
  - Level 2
    - Level 3

      Text in level 3

    Back to level 3

  Back to level 2

Back to level 1"""

    doc = ast_builder.build_ast(markdown)
    assert len(doc.children) == 2  # List and final paragraph

    # Outer list
    assert doc.children[0].__class__.__name__ == "MarkdownASTUnorderedListNode"
    outer_list = doc.children[0]
    assert len(outer_list.children) == 1  # One item

    level1_item = outer_list.children[0]
    # Level 1 item should have: paragraph + nested list + paragraph
    assert len(level1_item.children) == 3
    assert level1_item.children[0].children[0].content == "Level 1"

    # Level 2 list
    level2_list = level1_item.children[1]
    assert level2_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    level2_item = level2_list.children[0]

    # Level 2 item should have: paragraph + nested list + paragraph
    assert len(level2_item.children) == 3
    assert level2_item.children[0].children[0].content == "Level 2"

    # Level 3 list
    level3_list = level2_item.children[1]
    assert level3_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    level3_item = level3_list.children[0]

    # Level 3 item should have multiple paragraphs due to blank lines
    assert len(level3_item.children) >= 2
    assert level3_item.children[0].children[0].content == "Level 3"
    # Find the "Text in level 3" paragraph
    text_found = any("Text in level 3" in child.children[0].content 
                     for child in level3_item.children 
                     if child.__class__.__name__ == "MarkdownASTParagraphNode")
    assert text_found, "Should find 'Text in level 3' in level 3 item"

    # Check level 2 continuation
    assert level2_item.children[2].children[0].content == "Back to level 3"

    # Check level 1 continuation
    assert level1_item.children[2].children[0].content == "Back to level 2"

    # Final paragraph outside all lists
    assert doc.children[1].__class__.__name__ == "MarkdownASTParagraphNode"
    assert doc.children[1].children[0].content == "Back to level 1"



def test_list_type_change_with_nested_list(ast_builder):
    """Test changing list type at parent level while nested list exists."""
    markdown = """- Item 1
  - Nested A
  - Nested B
1. Item 2
2. Item 3"""

    doc = ast_builder.build_ast(markdown)

    # Should have two lists at root level: unordered then ordered
    assert len(doc.children) == 2

    # First should be unordered list with one item containing nested list
    first_list = doc.children[0]
    assert first_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(first_list.children) == 1

    first_item = first_list.children[0]
    assert len(first_item.children) == 2  # Text + nested list

    nested_list = first_item.children[1]
    assert nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(nested_list.children) == 2

    # Second should be ordered list with two items
    second_list = doc.children[1]
    assert second_list.__class__.__name__ == "MarkdownASTOrderedListNode"
    assert len(second_list.children) == 2


def test_ordered_to_unordered_list_type_change_with_nesting(ast_builder):
    """Test changing from ordered to unordered list with nested content.

    Tests the opposite direction of list type change.
    """
    markdown = """1. Item 1
   - Nested A
   - Nested B
- Item 2
- Item 3"""

    doc = ast_builder.build_ast(markdown)

    # Should have two lists at root level: ordered then unordered
    assert len(doc.children) == 2

    # First should be ordered list
    first_list = doc.children[0]
    assert first_list.__class__.__name__ == "MarkdownASTOrderedListNode"
    assert len(first_list.children) == 1

    # Second should be unordered list
    second_list = doc.children[1]
    assert second_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(second_list.children) == 2


def test_blockquote_with_nested_list_complex(ast_builder):
    """Test blockquotes containing lists with re-entry to blockquote."""
    markdown = """> Quote start
> - List item 1
>   - Nested item A
>   - Nested item B
> - List item 2
> 
> Back to quote paragraph"""

    doc = ast_builder.build_ast(markdown)

    assert len(doc.children) == 1
    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Should contain: paragraph, list, paragraph
    assert len(blockquote.children) == 3

    # First paragraph
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[0].children[0].content == "Quote start"

    # List with nested items
    list_node = blockquote.children[1]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(list_node.children) == 2

    # First list item should have nested list
    first_item = list_node.children[0]
    assert len(first_item.children) == 2  # Text + nested list
    nested_list = first_item.children[1]
    assert nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(nested_list.children) == 2

    # Back to quote paragraph
    assert blockquote.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[2].children[0].content == "Back to quote paragraph"


def test_nested_blockquotes_with_lists(ast_builder):
    """
    Test nested blockquotes containing lists.

    This tests the blockquote nesting adjustment logic when exiting
    and re-entering blockquote contexts.
    """
    markdown = """> Level 1 quote
> > Level 2 quote
> > - List in level 2
> > - Another item
> Back to level 1"""

    doc = ast_builder.build_ast(markdown)

    assert len(doc.children) == 1
    outer_blockquote = doc.children[0]
    assert outer_blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Should contain: paragraph, nested blockquote, paragraph
    assert len(outer_blockquote.children) == 3

    # First paragraph in outer blockquote
    assert outer_blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert outer_blockquote.children[0].children[0].content == "Level 1 quote"

    # Nested blockquote
    inner_blockquote = outer_blockquote.children[1]
    assert inner_blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Inner blockquote should have paragraph and list
    assert len(inner_blockquote.children) == 2
    assert inner_blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert inner_blockquote.children[0].children[0].content == "Level 2 quote"

    list_node = inner_blockquote.children[1]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(list_node.children) == 2

    # Back to level 1 paragraph
    assert outer_blockquote.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    assert outer_blockquote.children[2].children[0].content == "Back to level 1"


def test_blockquote_nesting_level_changes(ast_builder):
    """
    Test changing blockquote nesting levels dynamically.

    This should trigger the blockquote context adjustment logic when
    the nesting level changes.
    """
    markdown = """> Level 1
> > Level 2
> > > Level 3
> > Back to level 2
> Back to level 1"""

    doc = ast_builder.build_ast(markdown)

    assert len(doc.children) == 1
    level1 = doc.children[0]
    assert level1.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Level 1 should contain: paragraph, level 2 blockquote, paragraph
    assert len(level1.children) == 3
    assert level1.children[0].children[0].content == "Level 1"

    # Level 2 blockquote
    level2 = level1.children[1]
    assert level2.__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert len(level2.children) == 3
    assert level2.children[0].children[0].content == "Level 2"

    # Level 3 blockquote
    level3 = level2.children[1]
    assert level3.__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert len(level3.children) == 1
    assert level3.children[0].children[0].content == "Level 3"

    # Back to level 2
    assert level2.children[2].children[0].content == "Back to level 2"

    # Back to level 1
    assert level1.children[2].children[0].content == "Back to level 1"



def test_blockquote_exit_with_list_item_on_stack(ast_builder):
    """
    Test exiting a blockquote when list_item container is on stack.

    This specifically tests line 1054 in markdown_ast_builder.py where we need
    to pop non-blockquote containers (like list_item) when exiting a blockquote.

    The scenario: We have a nested blockquote containing a list. When we reduce
    the nesting level, the list_item container is still on the stack and must be
    popped before we can exit the inner blockquote.
    """
    markdown = """> Outer quote
> > Inner quote
> > - List item
> Back to outer"""

    doc = ast_builder.build_ast(markdown)

    # Should have one blockquote
    assert len(doc.children) == 1

    outer = doc.children[0]
    assert outer.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Outer should have: paragraph, nested blockquote, paragraph
    assert len(outer.children) == 3

    # Inner blockquote with list
    inner = outer.children[1]
    assert inner.__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert len(inner.children) == 2  # paragraph and list

    # Verify the list exists
    list_node = inner.children[1]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"


def test_nested_blockquote_exit_with_list(ast_builder):
    """
    Test exiting nested blockquote with list on stack.

    This is a more complex scenario that also triggers line 1054, where we
    exit from a nested blockquote level that contains a list.
    """
    markdown = """> Outer
> > Inner with list:
> > - Item A
> > - Item B
> Back to outer"""

    doc = ast_builder.build_ast(markdown)

    assert len(doc.children) == 1
    outer = doc.children[0]
    assert outer.__class__.__name__ == "MarkdownASTBlockquoteNode"

    # Outer should have: paragraph, nested blockquote, paragraph
    assert len(outer.children) == 3

    # Check nested blockquote with list
    inner = outer.children[1]
    assert inner.__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert len(inner.children) == 2

    # Inner has paragraph and list
    assert inner.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert inner.children[0].children[0].content == "Inner with list:"

    inner_list = inner.children[1]
    assert inner_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(inner_list.children) == 2

    # Back to outer paragraph
    assert outer.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    assert outer.children[2].children[0].content == "Back to outer"


def test_fenced_code_block_in_loose_list(ast_builder):
    """
    Test parsing a fenced code block within a loose list item.

    This test verifies that:
    1. A fenced code block indented within a list item is recognized as a code block
    2. The code block is properly associated with its list item
    3. Content after the list is correctly parsed as a separate paragraph
    4. The list is correctly identified as loose (has blank lines between items)
    """
    markdown = """# Hello

Here is a doc

- This is a list

- It's a loose list

- ```python
  for in in range(10):
      print("hello")
  ```

More!"""

    doc = ast_builder.build_ast(markdown)

    # Should have 4 top-level children: heading, paragraph, list, paragraph
    assert len(doc.children) == 4, f"Expected 4 children, got {len(doc.children)}"

    # First child: Heading
    heading = doc.children[0]
    assert heading.__class__.__name__ == "MarkdownASTHeadingNode"
    assert heading.level == 1
    assert heading.children[0].content == "Hello"

    # Second child: Paragraph
    para1 = doc.children[1]
    assert para1.__class__.__name__ == "MarkdownASTParagraphNode"
    assert para1.children[0].content == "Here is a doc"

    # Third child: Unordered list (loose)
    list_node = doc.children[2]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert len(list_node.children) == 3, f"Expected 3 list items, got {len(list_node.children)}"

    # Third list item should contain a code block
    third_item = list_node.children[2]
    assert third_item.__class__.__name__ == "MarkdownASTListItemNode"
    assert len(third_item.children) == 1, f"Expected 1 child (code block), got {len(third_item.children)}"

    # The code block should be the only child of the third list item
    code_block = third_item.children[0]
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode", \
        f"Expected MarkdownASTCodeBlockNode, got {code_block.__class__.__name__}"
    assert code_block.language == ProgrammingLanguage.PYTHON
    assert "for in in range(10):" in code_block.content
    assert 'print("hello")' in code_block.content

    # Fourth child: Paragraph with "More!"
    para2 = doc.children[3]
    assert para2.__class__.__name__ == "MarkdownASTParagraphNode"
    assert para2.children[0].content == "More!"
