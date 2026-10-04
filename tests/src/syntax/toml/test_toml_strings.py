"""
Tests for TOML string tokenization.
"""
from syntax.toml.toml_lexer import TOMLLexer


class TestTOMLStrings:
    """Test TOML string tokenization."""

    def test_double_quoted_strings(self):
        """Test basic double-quoted strings."""
        for s in ['"hello"', '"with spaces"', '"123"', '""']:
            lexer = TOMLLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == s

    def test_single_quoted_strings(self):
        """Test basic single-quoted strings (literal strings in TOML)."""
        for s in ["'hello'", "'with spaces'", "'123'", "''"]:
            lexer = TOMLLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"String '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'
            assert tokens[0].value == s

    def test_double_quoted_with_escapes(self):
        """Test double-quoted strings with escape sequences."""
        for s in [r'"line\nbreak"', r'"tab\there"', r'"quote\"inside"', r'"backslash\\here"']:
            lexer = TOMLLexer()
            lexer.lex(None, s)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Escaped string '{s}' should produce one token"
            assert tokens[0].type.name == 'STRING'

    def test_single_quoted_no_escapes(self):
        """Test that single-quoted strings do not process escapes."""
        lexer = TOMLLexer()
        lexer.lex(None, r"'no \n escape'")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == r"'no \n escape'"

    def test_string_containing_other_quote(self):
        """Test strings containing the other quote type."""
        lexer = TOMLLexer()
        lexer.lex(None, "'she said \"hi\"'")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'


class TestTOMLMultilineStrings:
    """Test TOML multi-line (triple-quoted) string tokenization."""

    def test_triple_double_quoted_single_line(self):
        """Test triple-double-quoted string on a single line."""
        lexer = TOMLLexer()
        lexer.lex(None, '"""hello"""')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert tokens[0].value == '"""hello"""'

    def test_triple_single_quoted_single_line(self):
        """Test triple-single-quoted string on a single line."""
        lexer = TOMLLexer()
        lexer.lex(None, "'''hello'''")

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'

    def test_multiline_string_across_lines(self):
        """Test a multi-line string spanning multiple lines."""
        lexer = TOMLLexer()
        state = lexer.lex(None, '"""start')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert state.in_multiline_string

        lexer2 = TOMLLexer()
        state2 = lexer2.lex(state, 'middle')

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type.name == 'STRING'
        assert state2.in_multiline_string

        lexer3 = TOMLLexer()
        state3 = lexer3.lex(state2, 'end"""')

        tokens3 = list(lexer3._tokens)
        assert len(tokens3) == 1
        assert tokens3[0].type.name == 'STRING'
        assert not state3.in_multiline_string

    def test_multiline_single_quoted_across_lines(self):
        """Test triple-single-quoted multi-line string."""
        lexer = TOMLLexer()
        state = lexer.lex(None, "'''start")

        assert state.in_multiline_string

        lexer2 = TOMLLexer()
        state2 = lexer2.lex(state, "end'''")

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type.name == 'STRING'
        assert not state2.in_multiline_string

    def test_multiline_string_with_content_on_first_line(self):
        """Test multi-line string with content on the first line."""
        lexer = TOMLLexer()
        state = lexer.lex(None, '"""some content')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert state.in_multiline_string

    def test_multiline_string_empty(self):
        """Test an empty triple-quoted string."""
        lexer = TOMLLexer()
        lexer.lex(None, '""""""')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
