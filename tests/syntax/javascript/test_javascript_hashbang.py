"""
Tests for JavaScript hashbang (shebang) tokenization.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestJavaScriptHashbang:
    """Test JavaScript hashbang/shebang tokenization."""

    def test_hashbang_basic(self):
        """Test basic hashbang at start of file."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '#!/usr/bin/env node')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Hashbang should produce one token"
        assert tokens[0].type.name == 'PREPROCESSOR', "Hashbang should be PREPROCESSOR type"
        assert tokens[0].value == '#!/usr/bin/env node'

    def test_hashbang_variations(self):
        """Test various hashbang formats."""
        test_cases = [
            '#!/usr/bin/node',
            '#!/usr/bin/env node',
            '#!/usr/local/bin/node',
            '#!node',
            '#!/bin/sh',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'PREPROCESSOR', f"'{code}' should be PREPROCESSOR type"
            assert tokens[0].value == code

    def test_hashbang_with_options(self):
        """Test hashbang with command-line options."""
        test_cases = [
            '#!/usr/bin/env node --experimental-modules',
            '#!/usr/bin/node --harmony',
            '#!/usr/bin/env node --trace-warnings',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'PREPROCESSOR'
            assert tokens[0].value == code

    def test_hashbang_consumes_entire_line(self):
        """Test that hashbang consumes the entire line."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '#!/usr/bin/env node // this is all part of hashbang')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Hashbang should consume entire line"
        assert tokens[0].type.name == 'PREPROCESSOR'
        assert '// this is all part of hashbang' in tokens[0].value

    def test_hashbang_only_first_line(self):
        """Test that hashbang only works on first line."""
        lines = [
            '#!/usr/bin/env node',
            'console.log("hello");',
        ]

        state = None
        for i, line in enumerate(lines):
            lexer = JavaScriptLexer()
            state = lexer.lex(state, line)
            tokens = list(lexer._tokens)

            if i == 0:
                # First line should have hashbang
                assert len(tokens) == 1
                assert tokens[0].type.name == 'PREPROCESSOR'
            else:
                # Second line should have regular code
                assert len(tokens) > 1


class TestJavaScriptHashError:
    """Test standalone hash character (error case)."""

    def test_standalone_hash(self):
        """Test standalone # character (not followed by !)."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '#')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Standalone # should produce one token"
        assert tokens[0].type.name == 'ERROR', "Standalone # should be ERROR type"
        assert tokens[0].value == '#'

    def test_hash_in_expression(self):
        """Test # appearing in code (should be error)."""
        test_cases = [
            'let x = #;',
            'const # = 5;',
            'function test() { # }',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have an ERROR token for the #
            error_tokens = [t for t in tokens if t.type.name == 'ERROR']
            assert len(error_tokens) >= 1, f"'{code}' should have at least one ERROR token"
            assert any(t.value == '#' for t in error_tokens), f"'{code}' should have ERROR token with value '#'"

    def test_hash_followed_by_non_exclamation(self):
        """Test # followed by characters other than !."""
        test_cases = [
            '#x',
            '#123',
            '#abc',
            '# comment?',  # This looks like a comment but isn't valid JS
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # First token should be ERROR for the #
            assert len(tokens) >= 1, f"'{code}' should produce tokens"
            assert tokens[0].type.name == 'ERROR', f"First token of '{code}' should be ERROR"
            assert tokens[0].value == '#'

    def test_hash_vs_hashbang(self):
        """Test distinguishing between # and #!."""
        # Hash alone
        lexer1 = JavaScriptLexer()
        lexer1.lex(None, '#')
        tokens1 = list(lexer1._tokens)
        assert tokens1[0].type.name == 'ERROR'

        # Hashbang
        lexer2 = JavaScriptLexer()
        lexer2.lex(None, '#!/usr/bin/env node')
        tokens2 = list(lexer2._tokens)
        assert tokens2[0].type.name == 'PREPROCESSOR'


class TestJavaScriptPrivateFields:
    """Test private class fields (which use # in modern JavaScript)."""

    def test_private_field_syntax(self):
        """Test private field syntax in classes."""
        # Note: Private fields like #privateField are valid ES2022+ syntax
        # The lexer currently treats # as ERROR, which may need updating
        # for full ES2022+ support
        test_cases = [
            'class MyClass { #privateField; }',
            '#count = 0;',
            'this.#value',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Currently # is treated as ERROR
            # This test documents current behavior
            error_tokens = [t for t in tokens if t.type.name == 'ERROR']
            assert len(error_tokens) >= 1, f"'{code}' produces ERROR tokens (current behavior)"


class TestJavaScriptHashEdgeCases:
    """Test edge cases for hash character handling."""

    def test_hash_in_string(self):
        """Test # inside strings (should not be tokenized separately)."""
        test_cases = [
            '"#hashtag"',
            "'#value'",
            '`#template`',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one STRING token"
            assert tokens[0].type.name == 'STRING'
            # No ERROR tokens should be present
            error_tokens = [t for t in tokens if t.type.name == 'ERROR']
            assert len(error_tokens) == 0, f"'{code}' should not have ERROR tokens"

    def test_hash_in_comment(self):
        """Test # inside comments (should not be tokenized separately)."""
        test_cases = [
            '// # this is a comment',
            '/* # also a comment */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one COMMENT token"
            assert tokens[0].type.name == 'COMMENT'
            # No ERROR tokens should be present
            error_tokens = [t for t in tokens if t.type.name == 'ERROR']
            assert len(error_tokens) == 0, f"'{code}' should not have ERROR tokens"

    def test_hash_in_regex(self):
        """Test # inside regex (should be part of regex)."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '/#hashtag/')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should produce one REGEXP token"
        assert tokens[0].type.name == 'REGEXP'
        assert '#' in tokens[0].value

    def test_hashbang_not_at_start(self):
        """Test that #! in middle of code is not treated as hashbang."""
        # When #! appears not at the start of a file, it should be # (ERROR) + ! (OPERATOR)
        lexer = JavaScriptLexer()
        lexer.lex(None, 'let x = #!')

        tokens = list(lexer._tokens)
        # Should have multiple tokens including ERROR for #
        error_tokens = [t for t in tokens if t.type.name == 'ERROR']
        assert len(error_tokens) >= 1, "Should have ERROR token for #"

    def test_empty_hashbang(self):
        """Test hashbang with nothing after it."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '#!')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Empty hashbang should produce one token"
        assert tokens[0].type.name == 'PREPROCESSOR'
        assert tokens[0].value == '#!'

    def test_hashbang_with_spaces(self):
        """Test hashbang with various spacing."""
        test_cases = [
            '#! /usr/bin/env node',  # Space after #!
            '#!/usr/bin/env  node',   # Double space
            '#!/usr/bin/env node ',   # Trailing space
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'PREPROCESSOR'
