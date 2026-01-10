"""
Tests for complex nesting scenarios in the markdown parser.

These tests verify that the container stack architecture properly handles
nested block elements like code blocks in lists, tables in lists, etc.
"""
import pytest

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from syntax import ProgrammingLanguage
from dmarkdown import MarkdownASTBuilder


@pytest.fixture
def ast_builder():
    """Fixture providing a markdown AST builder instance."""
    return MarkdownASTBuilder(no_underscores=False)


def test_code_block_in_list_item(ast_builder):
    """Test that code blocks properly nest inside list items."""
    markdown = """- First item with code:

  ```python
  def hello():
      return "world"
  ```

- Second item"""

    doc = ast_builder.build_ast(markdown)
    
    # Should have one list
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    
    # Should have two list items
    assert len(list_node.children) == 2
    
    # First item should contain paragraph and code block
    first_item = list_node.children[0]
    assert first_item.__class__.__name__ == "MarkdownASTListItemNode"
    assert len(first_item.children) == 2
    
    # First child is the paragraph with "First item with code:"
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[0].children[0].content == "First item with code:"
    
    # Second child is the code block
    code_block = first_item.children[1]
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert code_block.language == ProgrammingLanguage.PYTHON
    assert "def hello():" in code_block.content
    assert "return \"world\"" in code_block.content
    
    # Second item should just have paragraph
    second_item = list_node.children[1]
    assert second_item.__class__.__name__ == "MarkdownASTListItemNode"
    assert len(second_item.children) == 1
    assert second_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert second_item.children[0].children[0].content == "Second item"


def test_multiple_code_blocks_in_list(ast_builder):
    """Test multiple code blocks in the same list item."""
    markdown = """- Item with multiple code blocks:

  ```python
  first_block()
  ```
  
  Some text between blocks.
  
  ```javascript
  secondBlock();
  ```

- Next item"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_item = list_node.children[0]
    
    # Should have: paragraph, code block, paragraph, code block
    assert len(first_item.children) == 4
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert first_item.children[1].language == ProgrammingLanguage.PYTHON
    assert first_item.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[3].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert first_item.children[3].language == ProgrammingLanguage.JAVASCRIPT


def test_table_in_list_item(ast_builder):
    """Test that tables properly nest inside list items."""
    markdown = """- First item with table:

  | Col1 | Col2 |
  |------|------|
  | A    | B    |
  | C    | D    |

- Second item"""

    doc = ast_builder.build_ast(markdown)
    
    # Should have one list
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTUnorderedListNode"
    
    # Should have two list items
    assert len(list_node.children) == 2
    
    # First item should contain paragraph and table
    first_item = list_node.children[0]
    assert first_item.__class__.__name__ == "MarkdownASTListItemNode"
    assert len(first_item.children) == 2
    
    # First child is the paragraph
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    
    # Second child is the table
    table = first_item.children[1]
    assert table.__class__.__name__ == "MarkdownASTTableNode"
    
    # Verify table structure
    assert len(table.children) == 2  # header and body
    header = table.children[0]
    body = table.children[1]
    assert header.__class__.__name__ == "MarkdownASTTableHeaderNode"
    assert body.__class__.__name__ == "MarkdownASTTableBodyNode"
    assert len(body.children) == 2  # Two rows
    
    # Second item should just have paragraph
    second_item = list_node.children[1]
    assert len(second_item.children) == 1
    assert second_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"


def test_nested_list_with_code_block(ast_builder):
    """Test code block in nested list item."""
    markdown = """- Outer item
  - Nested item with code:
  
    ```python
    nested_code()
    ```
  
  - Another nested item

- Outer item 2"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_outer_item = list_node.children[0]
    
    # First outer item should have paragraph and nested list
    assert len(first_outer_item.children) == 2
    nested_list = first_outer_item.children[1]
    assert nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    
    # First nested item should have paragraph and code block
    first_nested_item = nested_list.children[0]
    assert len(first_nested_item.children) == 2
    assert first_nested_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_nested_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert "nested_code()" in first_nested_item.children[1].content


def test_blockquote_with_table(ast_builder):
    """Test table inside blockquote."""
    markdown = """> Quote with table:
> 
> | Col1 | Col2 |
> |------|------|
> | A    | B    |
>
> More quote text"""

    doc = ast_builder.build_ast(markdown)
    
    # Should have one blockquote
    assert len(doc.children) == 1
    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"
    
    # Blockquote should contain: paragraph, table, paragraph
    assert len(blockquote.children) == 3
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTTableNode"
    assert blockquote.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    
    # Verify table
    table = blockquote.children[1]
    assert len(table.children) == 2  # header and body


def test_blockquote_with_nested_list_and_code(ast_builder):
    """Test complex nesting: blockquote containing list with code block."""
    markdown = """> This is a quote
> 
> - List item in quote
>   
>   ```python
>   code_in_list_in_quote()
>   ```
> 
> - Second list item
>
> Back to quote text"""

    doc = ast_builder.build_ast(markdown)
    
    # Should have one blockquote
    assert len(doc.children) == 1
    blockquote = doc.children[0]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"
    
    # Blockquote should contain: paragraph, list, paragraph
    assert len(blockquote.children) == 3
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTUnorderedListNode"
    assert blockquote.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    
    # Check the list
    list_node = blockquote.children[1]
    assert len(list_node.children) == 2
    
    # First list item should have paragraph and code block
    first_item = list_node.children[0]
    assert len(first_item.children) == 2
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert "code_in_list_in_quote()" in first_item.children[1].content


def test_list_with_blockquote(ast_builder):
    """Test blockquote inside list item."""
    markdown = """- First item

  > Blockquote in list
  > Second line
  
  Back to list item

- Second item"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_item = list_node.children[0]
    
    # First item should have: paragraph, blockquote, paragraph
    assert len(first_item.children) == 3
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[1].__class__.__name__ == "MarkdownASTBlockquoteNode"
    assert first_item.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    
    # Verify blockquote content
    blockquote = first_item.children[1]
    assert len(blockquote.children) == 1
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"


def test_horizontal_rule_in_list(ast_builder):
    """Test horizontal rule inside list item."""
    markdown = """- First item

  ---
  
  After the rule

- Second item"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_item = list_node.children[0]
    
    # First item should have: paragraph, hr, paragraph
    assert len(first_item.children) == 3
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[1].__class__.__name__ == "MarkdownASTHorizontalRuleNode"
    assert first_item.children[2].__class__.__name__ == "MarkdownASTParagraphNode"


def test_deeply_nested_mixed_blocks(ast_builder):
    """Test deeply nested mixed block elements."""
    markdown = """- Level 1 list
  
  > Level 1 blockquote
  > 
  > - Level 2 list in quote
  >   
  >   ```python
  >   level_2_code()
  >   ```
  >   
  >   > Level 2 nested quote
  >   > in the list
  > 
  > Back to level 1 quote

- Level 1 list item 2"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_item = list_node.children[0]
    
    # First item has paragraph and blockquote
    assert len(first_item.children) == 2
    blockquote = first_item.children[1]
    assert blockquote.__class__.__name__ == "MarkdownASTBlockquoteNode"
    
    # Blockquote should have: paragraph, list, paragraph
    assert len(blockquote.children) == 3
    nested_list = blockquote.children[1]
    assert nested_list.__class__.__name__ == "MarkdownASTUnorderedListNode"
    
    # Nested list item should have: paragraph, code block, blockquote
    nested_item = nested_list.children[0]
    assert len(nested_item.children) == 3
    assert nested_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert nested_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert nested_item.children[2].__class__.__name__ == "MarkdownASTBlockquoteNode"


def test_ordered_list_with_code_block(ast_builder):
    """Test code block in ordered list item."""
    markdown = """1. First item with code:

   ```python
   first()
   ```

2. Second item with code:

   ```javascript
   second();
   ```"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTOrderedListNode"
    
    # Both items should have paragraph and code block
    assert len(list_node.children) == 2
    
    first_item = list_node.children[0]
    assert len(first_item.children) == 2
    assert first_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert first_item.children[1].language == ProgrammingLanguage.PYTHON
    
    second_item = list_node.children[1]
    assert len(second_item.children) == 2
    assert second_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert second_item.children[1].language == ProgrammingLanguage.JAVASCRIPT


def test_mixed_list_types_with_blocks(ast_builder):
    """Test code blocks in mixed ordered/unordered lists."""
    markdown = """- Unordered item
  
  1. Nested ordered with code:
  
     ```python
     nested_ordered()
     ```
  
  2. Another nested ordered

- Unordered item 2"""

    doc = ast_builder.build_ast(markdown)
    outer_list = doc.children[0]
    first_item = outer_list.children[0]
    
    # First item has paragraph and nested ordered list
    assert len(first_item.children) == 2
    nested_list = first_item.children[1]
    assert nested_list.__class__.__name__ == "MarkdownASTOrderedListNode"
    
    # First nested item has paragraph and code block
    nested_item = nested_list.children[0]
    assert len(nested_item.children) == 2
    assert nested_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"


def test_table_after_code_block_in_list(ast_builder):
    """Test that multiple block elements work in sequence."""
    markdown = """- Item with multiple blocks:

  ```python
  code_first()
  ```
  
  Then some text.
  
  | Col1 | Col2 |
  |------|------|
  | A    | B    |
  
  Final text.

- Next item"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_item = list_node.children[0]
    
    # Should have: paragraph, code block, paragraph, table, paragraph
    assert len(first_item.children) == 5
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[1].__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert first_item.children[2].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[3].__class__.__name__ == "MarkdownASTTableNode"
    assert first_item.children[4].__class__.__name__ == "MarkdownASTParagraphNode"


def test_empty_code_block_in_list(ast_builder):
    """Test empty code block in list."""
    markdown = """- Item with empty code:

  ```python
  ```

- Next item"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_item = list_node.children[0]
    
    # Should have paragraph and code block
    assert len(first_item.children) == 2
    code_block = first_item.children[1]
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    assert code_block.content == ""


def test_unclosed_code_block_in_list(ast_builder):
    """Test unclosed code block in list item."""
    markdown = """- Item with unclosed code:

  ```python
  unclosed_code()
  
- This should still be in the code block"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]
    first_item = list_node.children[0]
    
    # Should have paragraph and code block
    assert len(first_item.children) == 2
    code_block = first_item.children[1]
    assert code_block.__class__.__name__ == "MarkdownASTCodeBlockNode"
    # The unclosed code block should capture everything until end
    assert "unclosed_code()" in code_block.content
