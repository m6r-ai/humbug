"""
Tests for Python keyword and boolean literal tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer


class TestPythonKeywords:
    """Test Python keyword tokenization."""

    def test_control_flow_keywords(self):
        """Test control flow keywords."""
        keywords = ['if', 'elif', 'else', 'while', 'for', 'break', 'continue', 'pass']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_function_keywords(self):
        """Test function-related keywords."""
        keywords = ['def', 'return', 'lambda', 'yield', 'async', 'await']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_class_keywords(self):
        """Test class-related keywords."""
        keywords = ['class']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_exception_keywords(self):
        """Test exception-related keywords."""
        keywords = ['try', 'except', 'finally', 'raise', 'assert']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_import_keywords(self):
        """Test import-related keywords."""
        keywords = ['import', 'from', 'as']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_logical_keywords(self):
        """Test logical operator keywords."""
        keywords = ['and', 'or', 'not', 'is', 'in']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_scope_keywords(self):
        """Test scope-related keywords."""
        keywords = ['global', 'nonlocal', 'del']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_context_keywords(self):
        """Test context manager keywords."""
        keywords = ['with']
        for keyword in keywords:
            lexer = PythonLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Keyword '{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Keyword '{keyword}' not recognized"

    def test_boolean_and_none_literals(self):
        """Test boolean and None literal tokenization."""
        literals = ['True', 'False', 'None']
        for literal in literals:
            lexer = PythonLexer()
            lexer.lex(None, literal)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Literal '{literal}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"Literal '{literal}' should be KEYWORD type"

    def test_keywords_case_sensitivity(self):
        """Test that keywords are case-sensitive."""
        test_cases = [
            ('true', 'IDENTIFIER'),  # lowercase should be identifier
            ('TRUE', 'IDENTIFIER'),  # uppercase should be identifier
            ('True', 'KEYWORD'),     # proper case is keyword
            ('If', 'IDENTIFIER'),    # wrong case
            ('if', 'KEYWORD'),       # correct case
        ]
        for test_input, expected_type in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, test_input)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{test_input}' should produce one token"
            assert tokens[0].type.name == expected_type, \
                f"'{test_input}' should be {expected_type}, got {tokens[0].type.name}"

    def test_keywords_with_whitespace(self):
        """Test keywords surrounded by whitespace."""
        lexer = PythonLexer()
        lexer.lex(None, 'if x and y or z')

        tokens = list(lexer._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keywords) == 3, "Should find 3 keywords (if, and, or)"

    def test_keyword_as_identifier_prefix(self):
        """Test identifiers that start with keywords."""
        test_cases = [
            'ifconfig',
            'forloop',
            'classical',
            'import_module',
        ]
        for test_input in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, test_input)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{test_input}' should be a single identifier token"
            assert tokens[0].type.name == 'IDENTIFIER', \
                f"'{test_input}' should be an identifier, not split"
            assert tokens[0].value == test_input, f"Value should be '{test_input}'"

    def test_keyword_as_identifier_suffix(self):
        """Test identifiers that end with keywords."""
        test_cases = [
            'myif',
            'get_class',
            'is_none',
        ]
        for test_input in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, test_input)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{test_input}' should be a single identifier token"
            assert tokens[0].type.name == 'IDENTIFIER', \
                f"'{test_input}' should be an identifier"

    def test_multiple_keywords_in_statement(self):
        """Test multiple keywords in a single statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'if x is not None and y in list')

        tokens = list(lexer._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keywords]
        assert 'if' in keyword_values
        assert 'is' in keyword_values
        assert 'not' in keyword_values
        assert 'None' in keyword_values
        assert 'and' in keyword_values
        assert 'in' in keyword_values
