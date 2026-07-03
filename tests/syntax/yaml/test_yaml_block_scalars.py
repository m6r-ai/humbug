"""
Tests for YAML block scalar handling across lines.
"""
from syntax.yaml.yaml_parser import YAMLParser
from syntax.lexer import TokenType


class TestYAMLBlockScalars:
    """Test YAML block scalar (literal | and folded >) handling."""

    def test_literal_block_scalar(self):
        """Test a literal block scalar consumes indented lines as strings."""
        lines = [
            'script: |',
            '  echo hello',
            '  echo world',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        content_tokens = [
            t for t in all_tokens
            if t.type == TokenType.STRING and t.value.startswith('  echo')
        ]
        assert len(content_tokens) == 2, "Both content lines should be STRING tokens"
        assert content_tokens[0].value == '  echo hello'
        assert content_tokens[1].value == '  echo world'

    def test_folded_block_scalar(self):
        """Test a folded block scalar (> indicator)."""
        lines = [
            'description: >',
            '  This is',
            '  folded text.',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        content_tokens = [
            t for t in all_tokens
            if t.type == TokenType.STRING and 'folded' in t.value
        ]
        assert len(content_tokens) == 1

    def test_block_scalar_with_chomping(self):
        """Test block scalar with chomping indicator (|-)."""
        lines = [
            'text: |-',
            '  line one',
            '  line two',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        content_tokens = [
            t for t in all_tokens
            if t.type == TokenType.STRING and 'line' in t.value
        ]
        assert len(content_tokens) == 2

    def test_block_scalar_exit_on_indent(self):
        """Test that block scalar ends when indentation drops."""
        lines = [
            'text: |',
            '  scalar content',
            'next_key: value',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'text' in key_values
        assert 'next_key' in key_values, "Key after block scalar should be classified normally"

    def test_block_scalar_with_blank_lines(self):
        """Test block scalar with blank lines inside."""
        lines = [
            'text: |',
            '  first paragraph',
            '',
            '  second paragraph',
            'after: true',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'text' in key_values
        assert 'after' in key_values, "Key after block scalar with blank lines should be normal"

    def test_block_scalar_indicator_on_key_line(self):
        """Test that block scalar indicator is on the same line as the key."""
        parser = YAMLParser()
        parser.parse(None, 'script: |')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'script'

    def test_nested_block_scalar(self):
        """Test block scalar inside a nested mapping."""
        lines = [
            'config:',
            '  script: |',
            '    echo nested',
            '    echo deeper',
            '  done: true',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'config' in key_values
        assert 'script' in key_values
        assert 'done' in key_values, "Key after nested block scalar should be normal"

    def test_state_carries_block_scalar_flag(self):
        """Test that parser state correctly tracks block scalar mode."""
        parser = YAMLParser()
        state = parser.parse(None, 'script: |')

        assert state.in_block_scalar, "State should be in_block_scalar after seeing |"

    def test_state_clears_block_scalar_on_exit(self):
        """Test that parser state exits block scalar mode on dedent."""
        lines = [
            'script: |',
            '  content',
            'next: value',
        ]
        state = None

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)

        assert not state.in_block_scalar, "State should exit block scalar after dedent"

    def test_folded_scalar_with_chomping(self):
        """Test folded scalar with chomping indicator (>-)."""
        lines = [
            'text: >-',
            '  folded',
            '  content',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        content_tokens = [
            t for t in all_tokens
            if t.type == TokenType.STRING and ('folded' in t.value or 'content' in t.value)
        ]
        assert len(content_tokens) == 2
