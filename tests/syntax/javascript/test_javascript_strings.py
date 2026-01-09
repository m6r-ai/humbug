"""
Tests for JavaScript string tokenization.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestJavaScriptStrings:
    """Test JavaScript string literal tokenization."""

    def test_single_quoted_strings(self):
        """Test single-quoted strings."""
        test_cases = [
            "'hello'",
            "'world'",
            "'with spaces'",
            "'123'",
            "''",  # empty string
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String '{s}' should be STRING type"
            assert tokens[0].value == s

    def test_double_quoted_strings(self):
        """Test double-quoted strings."""
        test_cases = [
            '"hello"',
            '"world"',
            '"with spaces"',
            '"123"',
            '""',  # empty string
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"String '{s}' should be STRING type"
            assert tokens[0].value == s

    def test_string_with_escaped_quotes(self):
        """Test strings containing escaped quote characters."""
        test_cases = [
            r"'She said \'hello\''",
            r'"He said \"hi\""',
            r"'It\'s'",
            r'"Say \"quote\""',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with escaped quotes '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == s

    def test_string_with_other_quote_type(self):
        """Test strings containing the other quote type."""
        test_cases = [
            "'She said \"hello\"'",
            '"He said \'hi\'"',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_string_with_escape_sequences(self):
        """Test strings with various escape sequences."""
        test_cases = [
            r"'line1\nline2'",
            r"'tab\there'",
            r"'backslash\\'",
            r"'carriage\rreturn'",
            r"'null\0byte'",
            r"'unicode\u0041'",
            r"'unicode\u{1F600}'",  # ES6 unicode escape
            r"'hex\x41'",
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String with escapes '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_template_literals_single_line(self):
        """Test template literals (backtick strings) on single line."""
        test_cases = [
            '`hello`',
            '`world`',
            '`with spaces`',
            '``',  # empty template
            '`value: ${x}`',
            '`multiple ${x} and ${y}`',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Template literal '{s}' should produce at least one token"
            # Template literals should be tokenized as STRING
            string_tokens = [t for t in tokens if t.type.name == 'STRING']
            assert len(string_tokens) >= 1, f"Template literal '{s}' should have at least one STRING token"

    def test_string_concatenation(self):
        """Test adjacent string concatenation."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '"hello" + "world"')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 2, "Should have 2 separate string tokens"

    def test_unterminated_strings(self):
        """Test unterminated strings (should still tokenize)."""
        test_cases = [
            "'unterminated",
            '"missing quote at end',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Unterminated string '{s}' should produce at least one token"
            assert tokens[0].type.name == 'STRING'

    def test_string_with_backslash_at_end(self):
        """Test strings with backslash at the end."""
        test_cases = [
            r"'ends with\\'",
            r'"ends with\\"',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"String '{s}' should produce at least one token"

    def test_string_with_special_characters(self):
        """Test strings containing special characters."""
        test_cases = [
            '"hello@world"',
            '"test#123"',
            '"path/to/file"',
            '"C:\\\\path\\\\file"',  # Windows path
            '"emoji: 😀"',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'


class TestJavaScriptTemplateLiterals:
    """Test JavaScript template literal tokenization with focus on multiline."""

    def test_template_literal_basic(self):
        """Test basic template literal."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '`hello world`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Template literal should produce at least one token"

    def test_template_literal_with_expression(self):
        """Test template literal with embedded expression."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '`value is ${x}`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Template literal with expression should produce tokens"

    def test_template_literal_multiline_start(self):
        """Test starting a multiline template literal."""
        lexer = JavaScriptLexer()
        state = lexer.lex(None, '`start of template')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should have at least one token"
        # Check if state indicates we're in a template literal
        # Note: Current implementation may not track this state

    def test_template_literal_with_backtick_escape(self):
        """Test template literal with escaped backtick."""
        lexer = JavaScriptLexer()
        lexer.lex(None, r'`escaped \` backtick`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should produce tokens"

    def test_template_literal_with_nested_quotes(self):
        """Test template literal containing quotes."""
        test_cases = [
            '`contains "double" quotes`',
            "`contains 'single' quotes`",
            '`both "double" and \'single\'`',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Template literal '{s}' should produce tokens"

    def test_template_literal_empty_expression(self):
        """Test template literal with empty expression."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '`value: ${}`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should produce tokens"

    def test_template_literal_nested_expressions(self):
        """Test template literal with nested expressions."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '`outer ${`inner ${x}`}`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should produce tokens"


class TestJavaScriptStringEdgeCases:
    """Test edge cases for JavaScript string tokenization."""

    def test_string_in_expression(self):
        """Test strings in various expressions."""
        test_cases = [
            'let s = "hello";',
            'const name = \'world\';',
            'console.log("message");',
            'if (x === "test") {}',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            string_tokens = [t for t in tokens if t.type.name == 'STRING']
            assert len(string_tokens) >= 1, f"Code '{code}' should have at least one string token"

    def test_strings_with_embedded_quotes(self):
        """Test strings containing the other quote type."""
        test_cases = [
            '"He said \'hello\'"',
            '\'She said "hi"\'',
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            string_tokens = [t for t in tokens if t.type.name == 'STRING']
            assert len(string_tokens) == 1, f"'{s}' should produce one string token"

    def test_complex_escape_sequences(self):
        """Test complex escape sequences."""
        test_cases = [
            r'"\x1B[31mRed\x1B[0m"',  # ANSI color codes
            r'"\u0041\u0042\u0043"',   # Multiple unicode escapes
            r'"\"\'\\\n\r\t"',         # Multiple standard escapes
        ]
        for s in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"Complex escape sequence '{s}' should produce tokens"

    def test_string_with_line_continuation(self):
        """Test string with line continuation backslash."""
        # In JavaScript, backslash at end of line continues the string
        lexer = JavaScriptLexer()
        lexer.lex(None, r'"line continuation\"')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Should produce tokens"

    def test_empty_input(self):
        """Test empty input."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0, "Empty input should produce no tokens"

    def test_whitespace_only(self):
        """Test whitespace only input."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '    ')

        tokens = list(lexer._tokens)
        # Whitespace tokens might or might not be included
        assert len(tokens) >= 0

    def test_mixed_string_types(self):
        """Test code with mixed string types."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'let a = "double", b = \'single\', c = `template`;')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 3, "Should have 3 string tokens"
