"""
Tests for the MarkdownConverter class.
"""
import pytest

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from syntax import ProgrammingLanguage
from dmarkdown import MarkdownConverter, MarkdownASTCodeBlockNode, MarkdownASTDocumentNode


@pytest.fixture
def converter():
    """Fixture providing a markdown converter instance."""
    return MarkdownConverter()


def test_converter_initialization(converter):
    """Test that converter initializes properly."""
    assert converter is not None
    assert converter.current_text == ""
    assert converter.ast_builder is not None


def test_extract_sections_simple_markdown(converter):
    """Test extracting sections from simple markdown text."""
    text = """# Heading

This is a paragraph.

Another paragraph."""
    
    sections = converter.extract_sections(text, None)
    
    # Should have one markdown section
    assert len(sections) == 1
    node, language = sections[0]
    assert language is None  # Markdown section
    assert isinstance(node, MarkdownASTDocumentNode)
    assert len(node.children) == 3  # Heading + 2 paragraphs


def test_extract_sections_with_code_block(converter):
    """Test extracting sections with a code block."""
    text = """# Heading

Some text before code.

```python
def hello():
    print("world")
```

Text after code."""
    
    sections = converter.extract_sections(text, None)
    
    # Should have 3 sections: markdown, code, markdown
    assert len(sections) == 3
    
    # First section: markdown (heading + paragraph)
    node1, lang1 = sections[0]
    assert lang1 is None
    assert isinstance(node1, MarkdownASTDocumentNode)
    assert len(node1.children) == 2
    
    # Second section: code block
    node2, lang2 = sections[1]
    assert isinstance(node2, MarkdownASTCodeBlockNode)
    assert lang2 == ProgrammingLanguage.PYTHON
    assert "def hello():" in node2.content
    
    # Third section: markdown (paragraph)
    node3, lang3 = sections[2]
    assert lang3 is None
    assert isinstance(node3, MarkdownASTDocumentNode)
    assert len(node3.children) == 1


def test_extract_sections_multiple_code_blocks(converter):
    """Test extracting sections with multiple code blocks."""
    text = """Text 1

```python
code1()
```

Text 2

```javascript
code2();
```

Text 3"""
    
    sections = converter.extract_sections(text, None)
    
    # Should have 5 sections: md, code, md, code, md
    assert len(sections) == 5
    
    # Check alternating pattern
    assert sections[0][1] is None  # Markdown
    assert sections[1][1] == ProgrammingLanguage.PYTHON  # Code
    assert sections[2][1] is None  # Markdown
    assert sections[3][1] == ProgrammingLanguage.JAVASCRIPT  # Code
    assert sections[4][1] is None  # Markdown


def test_extract_sections_only_code_block(converter):
    """Test extracting sections from text with only a code block."""
    text = """```python
print("hello")
```"""
    
    sections = converter.extract_sections(text, None)
    
    # Should have one code section
    assert len(sections) == 1
    node, language = sections[0]
    assert isinstance(node, MarkdownASTCodeBlockNode)
    assert language == ProgrammingLanguage.PYTHON


def test_extract_sections_empty_text(converter):
    """Test extracting sections from empty text."""
    sections = converter.extract_sections("", None)
    
    # Should have no sections
    assert len(sections) == 0


def test_extract_sections_with_path(converter):
    """Test that source path is stored."""
    text = "# Test"
    path = "/path/to/file.md"
    
    sections = converter.extract_sections(text, path)
    
    assert len(sections) == 1
    node, _ = sections[0]
    assert isinstance(node, MarkdownASTDocumentNode)
    assert node.source_path == path


def test_extract_sections_incremental_update(converter):
    """Test that incremental updates work correctly."""
    initial_text = "# Initial"
    updated_text = "# Updated"
    
    # First extraction
    sections1 = converter.extract_sections(initial_text, None)
    assert len(sections1) == 1
    
    # Update with new text
    sections2 = converter.extract_sections(updated_text, None)
    assert len(sections2) == 1
    
    # Verify current_text is updated
    assert converter.current_text == updated_text


def test_extract_sections_preserves_code_block_language(converter):
    """Test that code block languages are correctly identified."""
    test_cases = [
        ("```python\ncode\n```", ProgrammingLanguage.PYTHON),
        ("```javascript\ncode\n```", ProgrammingLanguage.JAVASCRIPT),
        ("```java\ncode\n```", ProgrammingLanguage.JAVA),
        ("```cpp\ncode\n```", ProgrammingLanguage.CPP),
        ("```\ncode\n```", ProgrammingLanguage.TEXT),
    ]
    
    for markdown, expected_lang in test_cases:
        converter = MarkdownConverter()  # Fresh converter for each test
        sections = converter.extract_sections(markdown, None)
        
        assert len(sections) == 1
        node, language = sections[0]
        assert isinstance(node, MarkdownASTCodeBlockNode)
        assert language == expected_lang, f"Expected {expected_lang} for {markdown}"


def test_extract_sections_code_block_between_lists(converter):
    """Test code blocks appearing between list items."""
    text = """- Item 1
- Item 2

```python
code()
```

- Item 3"""
    
    sections = converter.extract_sections(text, None)
    
    # Should have 3 sections: md (list), code, md (list)
    assert len(sections) == 3
    
    assert sections[0][1] is None  # Markdown
    assert sections[1][1] == ProgrammingLanguage.PYTHON  # Code
    assert sections[2][1] is None  # Markdown
