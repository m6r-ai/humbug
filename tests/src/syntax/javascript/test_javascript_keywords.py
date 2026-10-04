"""
Tests for JavaScript keyword tokenization.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestJavaScriptKeywords:
    """Test JavaScript keyword tokenization."""

    def test_variable_declaration_keywords(self):
        """Test variable declaration keywords."""
        test_cases = [
            'var',
            'let',
            'const',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_function_keywords(self):
        """Test function-related keywords."""
        test_cases = [
            'function',
            'return',
            'async',
            'await',
            'yield',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_control_flow_keywords(self):
        """Test control flow keywords."""
        test_cases = [
            'if',
            'else',
            'switch',
            'case',
            'default',
            'break',
            'continue',
            'while',
            'do',
            'for',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_exception_handling_keywords(self):
        """Test exception handling keywords."""
        test_cases = [
            'try',
            'catch',
            'finally',
            'throw',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_class_keywords(self):
        """Test class-related keywords."""
        test_cases = [
            'class',
            'extends',
            'super',
            'static',
            'new',
            'this',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_module_keywords(self):
        """Test module-related keywords."""
        test_cases = [
            'import',
            'export',
            'from',
            'default',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_operator_keywords(self):
        """Test operator keywords."""
        test_cases = [
            'typeof',
            'instanceof',
            'in',
            'of',
            'delete',
            'void',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_boolean_keywords(self):
        """Test boolean literal keywords."""
        test_cases = [
            'true',
            'false',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_null_keyword(self):
        """Test null keyword."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'null')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'
        assert tokens[0].value == 'null'

    def test_debugger_keyword(self):
        """Test debugger keyword."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'debugger')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'
        assert tokens[0].value == 'debugger'

    def test_with_keyword(self):
        """Test with keyword (deprecated but still a keyword)."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'with')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'
        assert tokens[0].value == 'with'

    def test_reserved_keywords(self):
        """Test reserved keywords (future use)."""
        test_cases = [
            'enum',
            'implements',
            'interface',
            'package',
            'private',
            'protected',
            'public',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"


class TestJavaScriptKeywordEdgeCases:
    """Test edge cases for JavaScript keyword tokenization."""

    def test_keywords_in_context(self):
        """Test keywords in code context."""
        test_cases = [
            'let x = 5;',
            'const name = "test";',
            'function test() {}',
            'class MyClass {}',
            'if (true) {}',
            'for (let i = 0; i < 10; i++) {}',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 1, f"'{code}' should have at least one keyword"

    def test_keyword_like_identifiers(self):
        """Test identifiers that contain keywords but aren't keywords."""
        test_cases = [
            'letter',  # contains 'let'
            'constant',  # contains 'const'
            'variables',  # contains 'var'
            'classify',  # contains 'class'
            'functions',  # contains 'function'
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"'{code}' should be IDENTIFIER, not KEYWORD"

    def test_keywords_as_object_properties(self):
        """Test keywords used as object property names."""
        test_cases = [
            'obj.class',
            'obj.function',
            'obj.var',
            'obj.let',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # The property name should still be recognized as keyword
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 1, f"'{code}' should have keyword token"

    def test_keywords_case_sensitivity(self):
        """Test that keywords are case-sensitive."""
        test_cases = [
            ('let', 'KEYWORD'),
            ('Let', 'IDENTIFIER'),
            ('LET', 'IDENTIFIER'),
            ('function', 'KEYWORD'),
            ('Function', 'IDENTIFIER'),
            ('FUNCTION', 'IDENTIFIER'),
        ]
        for code, expected_type in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == expected_type, f"'{code}' should be {expected_type}"

    def test_contextual_keywords(self):
        """Test contextual keywords (keywords in some contexts)."""
        test_cases = [
            'async',
            'await',
            'from',
            'of',
            'get',
            'set',
        ]
        for keyword in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            # These should be recognized as keywords

    def test_keywords_with_numbers(self):
        """Test that keywords followed by numbers become identifiers."""
        test_cases = [
            'let1',
            'const2',
            'var3',
            'function4',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"'{code}' should be IDENTIFIER"

    def test_keywords_with_underscores(self):
        """Test that keywords with underscores become identifiers."""
        test_cases = [
            '_let',
            'let_',
            '_const_',
            'my_var',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"'{code}' should be IDENTIFIER"

    def test_keywords_in_strings(self):
        """Test that keywords in strings are not tokenized as keywords."""
        test_cases = [
            '"let"',
            '"const"',
            '"function"',
            "'class'",
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'STRING', f"'{code}' should be STRING"

    def test_keywords_in_comments(self):
        """Test that keywords in comments are not separately tokenized."""
        test_cases = [
            '// let const var',
            '/* function class */',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{code}' should produce one token"
            assert tokens[0].type.name == 'COMMENT', f"'{code}' should be COMMENT"
