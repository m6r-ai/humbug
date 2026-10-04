"""
Tests for Python comment tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer


class TestPythonComments:
    """Test Python comment tokenization."""

    def test_simple_comment(self):
        """Test simple single-line comments."""
        test_cases = [
            '# This is a comment',
            '# Comment with numbers 123',
            '# Comment with symbols !@#$%',
            '#NoSpaceComment',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment '{comment}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"Should be COMMENT type"
            assert tokens[0].value == comment

    def test_empty_comment(self):
        """Test empty comment (just the # symbol)."""
        lexer = PythonLexer()
        lexer.lex(None, '#')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Empty comment should produce one token"
        assert tokens[0].type.name == 'COMMENT'

    def test_comment_after_code(self):
        """Test comments that appear after code on the same line."""
        lexer = PythonLexer()
        lexer.lex(None, 'x = 42  # This is a comment')

        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type.name == 'COMMENT']
        assert len(comment_tokens) == 1, "Should have one comment token"
        assert comment_tokens[0].value == '# This is a comment'

    def test_comment_with_string_quotes(self):
        """Test comments containing quote characters."""
        test_cases = [
            '# Comment with "double quotes"',
            "# Comment with 'single quotes'",
            '# Mixed "quotes" and \'quotes\'',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment with quotes should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_comment_with_operators(self):
        """Test comments containing operators."""
        test_cases = [
            '# x = 1 + 2',
            '# if x > 5:',
            '# array[0] = value',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment with operators should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_comment_with_keywords(self):
        """Test comments containing Python keywords."""
        test_cases = [
            '# if this then that',
            '# def function_name():',
            '# class MyClass:',
            '# import module',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment with keywords should produce one token"
            assert tokens[0].type.name == 'COMMENT'
            # Keywords in comments should not be tokenized as keywords
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) == 0, "Keywords in comments should not be tokenized"

    def test_comment_with_urls(self):
        """Test comments containing URLs."""
        test_cases = [
            '# See https://example.com',
            '# Reference: http://docs.python.org',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment with URL should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_comment_with_unicode(self):
        """Test comments containing Unicode characters."""
        test_cases = [
            '# Comment with emoji 😀',
            '# Comment with Chinese 中文',
            '# Comment with accents café',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Comment with Unicode should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_multiple_comments(self):
        """Test multiple comment lines."""
        # Note: This tests line-by-line parsing
        lines = [
            '# First comment',
            '# Second comment',
            '# Third comment',
        ]
        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, "Each line should produce one comment token"
            assert tokens[0].type.name == 'COMMENT'

    def test_comment_vs_string_hash(self):
        """Test that hash inside strings is not a comment."""
        lexer = PythonLexer()
        lexer.lex(None, '"string with # hash"')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be a single string token"
        assert tokens[0].type.name == 'STRING', "Should be STRING, not COMMENT"

    def test_shebang_line(self):
        """Test shebang line (which starts with #)."""
        lexer = PythonLexer()
        lexer.lex(None, '#!/usr/bin/env python3')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Shebang should produce one token"
        assert tokens[0].type.name == 'COMMENT', "Shebang should be treated as comment"

    def test_coding_declaration(self):
        """Test coding declaration comment."""
        test_cases = [
            '# -*- coding: utf-8 -*-',
            '# coding: utf-8',
            '# coding=utf-8',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Coding declaration should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_todo_comment(self):
        """Test TODO/FIXME/NOTE style comments."""
        test_cases = [
            '# TODO: Fix this',
            '# FIXME: Bug here',
            '# NOTE: Important',
            '# XXX: Hack',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"TODO comment should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_commented_out_code(self):
        """Test commented-out code."""
        test_cases = [
            '# x = 42',
            '# def foo():',
            '#     return True',
        ]
        for comment in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, comment)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Commented code should produce one token"
            assert tokens[0].type.name == 'COMMENT'

    def test_comment_consumes_rest_of_line(self):
        """Test that comment consumes everything to the end of line."""
        lexer = PythonLexer()
        lexer.lex(None, '# Comment with code after: x = 42; y = 10')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Comment should consume entire rest of line"
        assert tokens[0].type.name == 'COMMENT'
        # There should be no other tokens like identifiers or operators
        other_tokens = [t for t in tokens if t.type.name != 'COMMENT']
        assert len(other_tokens) == 0, "No non-comment tokens should exist"
