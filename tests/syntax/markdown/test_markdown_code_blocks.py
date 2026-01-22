"""
Tests for markdown code block syntax highlighting.
"""
import pytest
import sys
sys.path.insert(0, 'src')

import syntax.parser_imports
from syntax.markdown.markdown_parser import MarkdownParser
from syntax.programming_language import ProgrammingLanguage
from syntax.lexer import TokenType


def test_python_code_block_highlighting():
    """Test that Python code blocks are highlighted with Python syntax."""
    markdown_lines = [
        "```python",
        "def hello():",
        "    print('Hello, world!')",
        "```",
    ]

    parser = MarkdownParser()
    parser_state = None
    all_tokens = []

    for line in markdown_lines:
        parser_state = parser.parse(parser_state, line)
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            all_tokens.append(token)

    # Check that we have the expected tokens
    token_types = [t.type for t in all_tokens]
    token_values = [t.value for t in all_tokens]

    # Should have fence markers
    assert TokenType.FENCE_START in token_types
    assert TokenType.FENCE_END in token_types
    assert TokenType.LANGUAGE in token_types

    # Should have Python-specific tokens
    assert TokenType.KEYWORD in token_types  # 'def'
    assert TokenType.FUNCTION_OR_METHOD in token_types  # 'hello' and 'print'
    assert TokenType.STRING in token_types  # 'Hello, world!'

    # Check specific token values
    assert 'def' in token_values
    assert 'hello' in token_values
    assert 'print' in token_values
    assert "'Hello, world!'" in token_values


def test_javascript_code_block_highlighting():
    """Test that JavaScript code blocks are highlighted with JavaScript syntax."""
    markdown_lines = [
        "```javascript",
        "function greet() {",
        "    console.log('Hello');",
        "}",
        "```",
    ]

    parser = MarkdownParser()
    parser_state = None
    all_tokens = []

    for line in markdown_lines:
        parser_state = parser.parse(parser_state, line)
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            all_tokens.append(token)

    token_types = [t.type for t in all_tokens]
    token_values = [t.value for t in all_tokens]

    # Should have JavaScript-specific tokens
    assert TokenType.KEYWORD in token_types  # 'function'
    assert TokenType.FUNCTION_OR_METHOD in token_types  # 'greet'

    # Check specific values
    assert 'function' in token_values
    assert 'greet' in token_values


def test_code_block_fence_closing():
    """Test that code block fences close properly."""
    markdown_lines = [
        "```python",
        "x = 1",
        "```",
        "Regular text after code block",
    ]

    parser = MarkdownParser()
    parser_state = None
    states_after_each_line = []

    for line in markdown_lines:
        parser_state = parser.parse(parser_state, line)
        states_after_each_line.append({
            'in_fence': parser_state.in_fence_block,
            'language': parser_state.language
        })
        # Consume tokens
        while parser.get_next_token() is not None:
            pass

    # After line 1 (```python): should be in fence
    assert states_after_each_line[0]['in_fence'] == True
    assert states_after_each_line[0]['language'] == ProgrammingLanguage.PYTHON

    # After line 2 (x = 1): should still be in fence
    assert states_after_each_line[1]['in_fence'] == True

    # After line 3 (```): should be out of fence
    assert states_after_each_line[2]['in_fence'] == False
    assert states_after_each_line[2]['language'] == ProgrammingLanguage.UNKNOWN

    # After line 4 (regular text): should still be out of fence
    assert states_after_each_line[3]['in_fence'] == False


def test_multiple_code_blocks():
    """Test that multiple code blocks in sequence work correctly."""
    markdown_lines = [
        "```python",
        "x = 1",
        "```",
        "Some text",
        "```javascript",
        "var y = 2;",
        "```",
    ]

    parser = MarkdownParser()
    parser_state = None
    all_tokens = []

    for line in markdown_lines:
        parser_state = parser.parse(parser_state, line)
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            all_tokens.append((token.type, token.value))

    # Should have 2 fence starts and 2 fence ends
    fence_starts = [t for t in all_tokens if t[0] == TokenType.FENCE_START]
    fence_ends = [t for t in all_tokens if t[0] == TokenType.FENCE_END]
    assert len(fence_starts) == 2
    assert len(fence_ends) == 2

    # Should have both language markers
    languages = [t[1] for t in all_tokens if t[0] == TokenType.LANGUAGE]
    assert 'python' in languages
    assert 'javascript' in languages


def test_empty_code_block():
    """Test that empty code blocks are handled correctly."""
    markdown_lines = [
        "```python",
        "```",
    ]

    parser = MarkdownParser()
    parser_state = None
    all_tokens = []

    for line in markdown_lines:
        parser_state = parser.parse(parser_state, line)
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            all_tokens.append(token)

    token_types = [t.type for t in all_tokens]

    # Should have fence markers
    assert TokenType.FENCE_START in token_types
    assert TokenType.FENCE_END in token_types
    assert TokenType.LANGUAGE in token_types


def test_nested_code_blocks_in_markdown():
    """Test that code blocks nested inside a markdown code block are handled correctly."""
    markdown_lines = [
        "Hello",
        "",
        "```markdown",
        "# AIFPL Bytecode",
        "",
        "A `CodeObject` represents a compiled AIFPL expression:",
        "",
        "```python",
        "@dataclass",
        "class CodeObject:",
        "    instructions: List[Instruction]",
        "```",
        "",
        "### Instruction",
        "",
        "Each instruction consists of an opcode:",
        "",
        "```python",
        "@dataclass",
        "class Instruction:",
        "    opcode: Opcode",
        "```",
        "```",
        "",
        "Bye"
    ]

    parser = MarkdownParser()
    parser_state = None
    all_tokens = []
    states_after_each_line = []

    for line in markdown_lines:
        parser_state = parser.parse(parser_state, line)
        states_after_each_line.append({
            'in_fence': parser_state.in_fence_block,
            'language': parser_state.language,
            'line': line
        })
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            all_tokens.append((token.type, token.value))

    # After line 3 (```markdown): should be in fence with markdown language
    assert states_after_each_line[2]['in_fence'] == True
    assert states_after_each_line[2]['language'] == ProgrammingLanguage.MARKDOWN

    # After line 8 (```python inside markdown block): should STILL be in markdown fence
    assert states_after_each_line[7]['in_fence'] == True
    assert states_after_each_line[7]['language'] == ProgrammingLanguage.MARKDOWN

    # After line 23 (closing ``` for markdown block): should be out of fence
    assert states_after_each_line[22]['in_fence'] == False
    assert states_after_each_line[22]['language'] == ProgrammingLanguage.UNKNOWN


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
