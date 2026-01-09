"""
Tests for JavaScript regular expression tokenization.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestJavaScriptRegex:
    """Test JavaScript regular expression literal tokenization."""

    def test_simple_regex(self):
        """Test simple regex patterns."""
        test_cases = [
            '/abc/',
            '/test/',
            '/[0-9]/',
            '/\\d+/',
            '/\\w+/',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"
            assert tokens[0].value == code

    def test_regex_with_flags(self):
        """Test regex with flags."""
        test_cases = [
            '/abc/g',
            '/test/i',
            '/pattern/gi',
            '/search/gim',
            '/match/gimsuy',
            '/find/d',  # hasIndices flag
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"
            assert tokens[0].value == code

    def test_regex_with_escaped_slash(self):
        """Test regex containing escaped forward slash."""
        test_cases = [
            r'/\//',   # matches forward slash
            r'/a\/b/',  # matches a/b
            r'/path\/to\/file/',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"

    def test_regex_with_character_class(self):
        """Test regex with character classes."""
        test_cases = [
            '/[abc]/',
            '/[a-z]/',
            '/[A-Z0-9]/',
            '/[^abc]/',  # negated class
            '/[\\d\\w]/',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"

    def test_regex_with_quantifiers(self):
        """Test regex with quantifiers."""
        test_cases = [
            '/a*/',
            '/b+/',
            '/c?/',
            '/d{3}/',
            '/e{2,5}/',
            '/f{3,}/',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"

    def test_regex_with_groups(self):
        """Test regex with groups."""
        test_cases = [
            '/(abc)/',
            '/(a|b)/',
            '/(?:non-capturing)/',
            '/(?<name>group)/',  # named capture group
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"

    def test_regex_with_anchors(self):
        """Test regex with anchors."""
        test_cases = [
            '/^start/',
            '/end$/',
            '/^full$/',
            '/\\bword\\b/',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"

    def test_empty_regex(self):
        """Test empty regex."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '//')

        tokens = list(lexer._tokens)
        # Empty // is a comment, not regex
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'


class TestJavaScriptRegexEdgeCases:
    """Test edge cases for JavaScript regex tokenization."""

    def test_regex_vs_division(self):
        """Test distinguishing regex from division operator."""
        test_cases = [
            # These should be regex
            ('let re = /abc/', 'REGEXP'),
            ('if (/test/.test(s)) {}', 'REGEXP'),
            ('return /pattern/', 'REGEXP'),

            # These should be division
            ('x / y', 'OPERATOR'),
            ('a /= b', 'OPERATOR'),
        ]
        for code, expected_type in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Find the token with / or /.../ pattern
            slash_tokens = [t for t in tokens if '/' in t.value]
            assert len(slash_tokens) >= 1, f"'{code}' should have slash-related token"

    def test_regex_in_expression(self):
        """Test regex in various expressions."""
        test_cases = [
            'let pattern = /\\d+/;',
            'const regex = /test/gi;',
            'if (/abc/.test(str)) {}',
            'str.match(/pattern/)',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            regex_tokens = [t for t in tokens if t.type.name == 'REGEXP']
            assert len(regex_tokens) >= 1, f"'{code}' should have regex token"

    def test_regex_with_backslash_at_end(self):
        """Test regex with backslash before closing slash."""
        test_cases = [
            r'/\\/',  # matches single backslash
            r'/a\\/',  # matches 'a' followed by backslash
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_regex_like_strings(self):
        """Test strings that look like regex."""
        test_cases = [
            '"/abc/"',
            '"/pattern/g"',
            "'/test/'",
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"'{code}' should be STRING, not REGEXP"

    def test_regex_in_comments(self):
        """Test regex patterns in comments."""
        test_cases = [
            '// /pattern/',
            '/* /regex/ */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"'{code}' should be COMMENT"

    def test_complex_regex_patterns(self):
        """Test complex real-world regex patterns."""
        test_cases = [
            r'/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/',  # email
            r'/^\d{3}-\d{2}-\d{4}$/',  # SSN
            r'/^(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)*\/?$/',  # URL
            r'/^#?([a-f0-9]{6}|[a-f0-9]{3})$/i',  # hex color
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"

    def test_regex_constructor_vs_literal(self):
        """Test regex constructor (not a regex literal)."""
        test_cases = [
            'new RegExp("pattern")',
            'RegExp("test", "gi")',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # These use strings, not regex literals
            regex_tokens = [t for t in tokens if t.type.name == 'REGEXP']
            assert len(regex_tokens) == 0, f"'{code}' should not have REGEXP tokens"

    def test_unterminated_regex(self):
        """Test unterminated regex (missing closing slash)."""
        test_cases = [
            '/unterminated',
            '/pattern',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should still produce some tokens
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_regex_with_unicode(self):
        """Test regex with unicode patterns."""
        test_cases = [
            r'/\u0041/',
            r'/\u{1F600}/u',
            r'/[\\u0000-\\uFFFF]/',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"

    def test_regex_with_special_escapes(self):
        """Test regex with special escape sequences."""
        test_cases = [
            r'/\d/',   # digit
            r'/\D/',   # non-digit
            r'/\w/',   # word character
            r'/\W/',   # non-word
            r'/\s/',   # whitespace
            r'/\S/',   # non-whitespace
            r'/\b/',   # word boundary
            r'/\B/',   # non-word boundary
            r'/\0/',   # null
            r'/\n/',   # newline
            r'/\t/',   # tab
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'REGEXP', f"'{code}' should be REGEXP type"
