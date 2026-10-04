"""
Tests for Python multiline and state continuation scenarios.
"""
import pytest

from syntax.python.python_lexer import PythonLexer
from syntax.python.python_parser import PythonParser


class TestPythonMultiline:
    """Test Python multiline tokenization and state management."""

    def test_multiline_docstring_start(self):
        """Test starting a multiline docstring."""
        lexer = PythonLexer()
        state = lexer.lex(None, '"""Start of docstring')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'STRING'
        assert state.in_docstring, "Should be in docstring state"
        assert state.docstring_quote == '"'

    def test_multiline_docstring_middle(self):
        """Test middle lines of multiline docstring."""
        lexer1 = PythonLexer()
        state1 = lexer1.lex(None, '"""Start')

        lexer2 = PythonLexer()
        state2 = lexer2.lex(state1, 'Middle line')

        tokens = list(lexer2._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'STRING'
        assert state2.in_docstring, "Should still be in docstring state"

    def test_multiline_docstring_end(self):
        """Test ending a multiline docstring."""
        lexer1 = PythonLexer()
        state1 = lexer1.lex(None, '"""Start')

        lexer2 = PythonLexer()
        state2 = lexer2.lex(state1, 'Middle')

        lexer3 = PythonLexer()
        state3 = lexer3.lex(state2, 'End"""')

        tokens = list(lexer3._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'STRING'
        assert not state3.in_docstring, "Should no longer be in docstring state"

    def test_multiline_docstring_single_quotes(self):
        """Test multiline docstring with single quotes."""
        lexer1 = PythonLexer()
        state1 = lexer1.lex(None, "'''Start")

        assert state1.in_docstring, "Should be in docstring state"
        assert state1.docstring_quote == "'"

        lexer2 = PythonLexer()
        state2 = lexer2.lex(state1, "End'''")

        assert not state2.in_docstring, "Should no longer be in docstring state"

    def test_docstring_with_code_after(self):
        """Test docstring on same line as closing quotes with code."""
        lexer = PythonLexer()
        lexer.lex(None, '"""Docstring""" and code')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 1, "Should have docstring"

        # Should have other tokens after the docstring
        assert len(tokens) > 1, "Should have tokens after docstring"

    def test_empty_multiline_docstring(self):
        """Test empty multiline docstring."""
        lexer = PythonLexer()
        state = lexer.lex(None, '""""""')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should have one token"
        assert tokens[0].type.name == 'STRING'
        assert not state.in_docstring, "Should not be in docstring state"

    def test_multiline_docstring_with_quotes_inside(self):
        """Test multiline docstring containing quote characters."""
        lexer1 = PythonLexer()
        state1 = lexer1.lex(None, '"""Start with "quotes"')

        lexer2 = PythonLexer()
        state2 = lexer2.lex(state1, 'More "quotes" here')

        lexer3 = PythonLexer()
        state3 = lexer3.lex(state2, 'End"""')

        assert not state3.in_docstring, "Should close despite internal quotes"

    def test_multiline_with_parser_state(self):
        """Test multiline with parser state continuation."""
        parser1 = PythonParser()
        state1 = parser1.parse(None, '"""Start')

        assert state1.parsing_continuation, "Should be in continuation state"

        parser2 = PythonParser()
        state2 = parser2.parse(state1, 'End"""')

        assert not state2.parsing_continuation, "Should no longer be in continuation"

    def test_regular_string_not_multiline(self):
        """Test that regular strings don't trigger multiline state."""
        lexer = PythonLexer()
        state = lexer.lex(None, '"regular string"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'STRING'
        assert not state.in_docstring, "Should not be in docstring state"

    def test_line_continuation_with_backslash(self):
        """Test line continuation with backslash."""
        lexer = PythonLexer()
        lexer.lex(None, 'x = 1 + \\')

        tokens = list(lexer._tokens)
        backslash_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '\\']
        assert len(backslash_tokens) == 1, "Should have backslash operator"

    def test_implicit_line_continuation_in_parens(self):
        """Test implicit line continuation in parentheses."""
        lines = [
            'result = (',
            '    1 + 2',
            ')',
        ]

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)
            # Should not crash
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_implicit_line_continuation_in_brackets(self):
        """Test implicit line continuation in brackets."""
        lines = [
            'my_list = [',
            '    1, 2, 3,',
            ']',
        ]

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_implicit_line_continuation_in_braces(self):
        """Test implicit line continuation in braces."""
        lines = [
            'my_dict = {',
            '    "key": "value",',
            '}',
        ]

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_multiline_function_definition(self):
        """Test function definition across multiple lines."""
        lines = [
            'def long_function(',
            '        arg1,',
            '        arg2):',
        ]

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)
            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_multiline_function_call(self):
        """Test function call across multiple lines."""
        lines = [
            'result = function(',
            '    arg1,',
            '    arg2,',
            ')',
        ]

        for line in lines:
            parser = PythonParser()
            parser.parse(None, line)
            tokens = list(parser._tokens)
            assert len(tokens) >= 1

    def test_multiline_list_comprehension(self):
        """Test list comprehension across multiple lines."""
        lines = [
            'result = [',
            '    x * 2',
            '    for x in range(10)',
            '    if x % 2 == 0',
            ']',
        ]

        for line in lines:
            parser = PythonParser()
            parser.parse(None, line)
            tokens = list(parser._tokens)
            assert len(tokens) >= 1

    def test_docstring_in_function(self):
        """Test docstring inside function definition."""
        lines = [
            'def func():',
            '    """',
            '    This is a docstring',
            '    """',
        ]

        state = None
        for line in lines:
            parser = PythonParser()
            state = parser.parse(state, line)
            tokens = list(parser._tokens)
            assert len(tokens) >= 1

    def test_docstring_in_class(self):
        """Test docstring inside class definition."""
        lines = [
            'class MyClass:',
            '    """',
            '    This is a class docstring',
            '    """',
        ]

        state = None
        for line in lines:
            parser = PythonParser()
            state = parser.parse(state, line)
            tokens = list(parser._tokens)
            assert len(tokens) >= 1

    def test_nested_docstrings(self):
        """Test nested structures with docstrings."""
        lines = [
            'class MyClass:',
            '    """Class docstring"""',
            '    def method(self):',
            '        """Method docstring"""',
        ]

        for line in lines:
            parser = PythonParser()
            parser.parse(None, line)
            tokens = list(parser._tokens)
            assert len(tokens) >= 1

    def test_docstring_not_closed_at_end_of_line(self):
        """Test docstring that spans multiple lines without closing."""
        lexer1 = PythonLexer()
        state1 = lexer1.lex(None, '"""This docstring')

        assert state1.in_docstring, "Should be in docstring state"

        lexer2 = PythonLexer()
        state2 = lexer2.lex(state1, 'continues here')

        assert state2.in_docstring, "Should still be in docstring state"

        lexer3 = PythonLexer()
        state3 = lexer3.lex(state2, 'and finally closes"""')

        assert not state3.in_docstring, "Should be closed"

    def test_triple_quotes_in_regular_string(self):
        """Test that triple quotes inside regular strings don't trigger docstring."""
        lexer = PythonLexer()
        lexer.lex(None, 'text = "not a \\"\\"\\" docstring"')

        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        # Should be one regular string, not a docstring
        assert len(string_tokens) >= 1

    def test_comment_after_docstring_start(self):
        """Test comment on line with docstring start."""
        lexer = PythonLexer()
        state = lexer.lex(None, '"""Docstring # not a comment')

        assert state.in_docstring, "Should be in docstring, comment symbol ignored"

    def test_multiple_statements_one_line(self):
        """Test multiple statements on one line separated by semicolon."""
        parser = PythonParser()
        parser.parse(None, 'x = 1; y = 2; z = 3')

        tokens = list(parser._tokens)
        # Should have multiple identifiers and assignments
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 3, "Should have three identifiers"

    def test_empty_line(self):
        """Test empty line."""
        lexer = PythonLexer()
        lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0, "Empty line should produce no tokens"

    def test_whitespace_only_line(self):
        """Test line with only whitespace."""
        lexer = PythonLexer()
        lexer.lex(None, '    ')

        tokens = list(lexer._tokens)
        # Whitespace tokens might or might not be included
        # Just checking it doesn't crash
        assert len(tokens) >= 0

    def test_indented_code(self):
        """Test indented code (like inside a function)."""
        lexer = PythonLexer()
        lexer.lex(None, '    x = 42')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 1, "Should have identifier despite indentation"
