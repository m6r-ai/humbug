"""
Tests for Python identifier tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer


class TestPythonIdentifiers:
    """Test Python identifier tokenization."""

    def test_simple_identifiers(self):
        """Test simple variable names."""
        test_cases = ['x', 'y', 'var', 'my_var', 'myVar', 'MyVar', 'VAR']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', f"'{identifier}' should be IDENTIFIER type"
            assert tokens[0].value == identifier

    def test_identifiers_with_underscores(self):
        """Test identifiers with underscores."""
        test_cases = ['_private', '__private', '__dunder__', '_', '__', '___']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'
            assert tokens[0].value == identifier

    def test_identifiers_with_numbers(self):
        """Test identifiers containing numbers."""
        test_cases = ['var1', 'var2', 'my_var_123', 'test2var', 'x1y2z3']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'
            assert tokens[0].value == identifier

    def test_identifiers_cannot_start_with_number(self):
        """Test that identifiers starting with numbers are tokenized as number + identifier."""
        lexer = PythonLexer()
        lexer.lex(None, '123var')

        tokens = list(lexer._tokens)
        # Should be number followed by identifier (or error)
        assert len(tokens) >= 1, "Should produce at least one token"
        assert tokens[0].type.name == 'NUMBER', "First token should be NUMBER"

    def test_camel_case_identifiers(self):
        """Test CamelCase and camelCase identifiers."""
        test_cases = ['CamelCase', 'camelCase', 'MyClassName', 'myFunctionName']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'

    def test_snake_case_identifiers(self):
        """Test snake_case identifiers."""
        test_cases = ['snake_case', 'my_variable_name', 'get_user_data', 'is_valid']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'

    def test_uppercase_identifiers(self):
        """Test UPPERCASE identifiers (typically constants)."""
        test_cases = ['MAX_SIZE', 'PI', 'DEFAULT_VALUE', 'API_KEY']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'

    def test_single_letter_identifiers(self):
        """Test single letter identifiers."""
        test_cases = ['a', 'b', 'x', 'y', 'z', 'i', 'j', 'k', 'n']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'

    def test_underscore_only_identifier(self):
        """Test underscore-only identifiers."""
        lexer = PythonLexer()
        lexer.lex(None, '_')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Single underscore should produce one token"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == '_'

    def test_identifiers_in_assignment(self):
        """Test identifiers in assignment statements."""
        lexer = PythonLexer()
        lexer.lex(None, 'my_var = 42')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 1, "Should have one identifier"
        assert identifier_tokens[0].value == 'my_var'

    def test_multiple_identifiers(self):
        """Test multiple identifiers in one line."""
        lexer = PythonLexer()
        lexer.lex(None, 'x = y + z')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 3, "Should have three identifiers"
        identifier_values = [t.value for t in identifier_tokens]
        assert 'x' in identifier_values
        assert 'y' in identifier_values
        assert 'z' in identifier_values

    def test_identifiers_with_keywords_as_substrings(self):
        """Test identifiers that contain keywords as substrings."""
        test_cases = ['classify', 'defrost', 'import_data', 'for_loop', 'if_statement']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Identifier '{identifier}' should be single token"
            assert tokens[0].type.name == 'IDENTIFIER', \
                f"'{identifier}' should be IDENTIFIER, not split into keywords"

    def test_builtin_names_as_identifiers(self):
        """Test Python builtin names used as identifiers (allowed but not recommended)."""
        test_cases = ['list', 'dict', 'str', 'int', 'float', 'len', 'range', 'print']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Builtin '{identifier}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER', \
                f"Builtin names should be IDENTIFIER type, not KEYWORD"

    def test_long_identifiers(self):
        """Test very long identifier names."""
        long_name = 'this_is_a_very_long_identifier_name_that_someone_might_use'
        lexer = PythonLexer()
        lexer.lex(None, long_name)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long identifier should produce one token"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == long_name

    def test_identifier_with_unicode(self):
        """Test identifiers with Unicode characters (Python 3 supports this)."""
        # Note: Python 3 allows Unicode identifiers
        test_cases = ['café', 'naïve', 'π', '中文']
        for identifier in test_cases:
            lexer = PythonLexer()
            lexer.lex(None, identifier)

            tokens = list(lexer._tokens)
            # The lexer might or might not support Unicode identifiers
            # We're just checking it doesn't crash
            assert len(tokens) >= 1, f"Unicode identifier '{identifier}' should produce tokens"

    def test_identifiers_separated_by_operators(self):
        """Test identifiers separated by operators."""
        lexer = PythonLexer()
        lexer.lex(None, 'a+b-c*d/e')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 5, "Should have 5 identifiers"

    def test_identifiers_in_function_call(self):
        """Test identifiers in function calls."""
        lexer = PythonLexer()
        lexer.lex(None, 'func(arg1, arg2)')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        # func might be FUNCTION_OR_METHOD if parser is involved
        # arg1 and arg2 should be identifiers
        assert len(identifier_tokens) >= 2, "Should have at least 2 identifiers"

    def test_identifiers_in_attribute_access(self):
        """Test identifiers in attribute access."""
        lexer = PythonLexer()
        lexer.lex(None, 'obj.attr')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 2, "Should have 2 identifiers"

    def test_identifiers_in_import(self):
        """Test identifiers in import statements."""
        lexer = PythonLexer()
        lexer.lex(None, 'import module')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 1, "Should have 1 identifier (module)"

    def test_class_name_identifier(self):
        """Test class names as identifiers."""
        lexer = PythonLexer()
        lexer.lex(None, 'class MyClass:')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 1, "Should have 1 identifier (MyClass)"
        assert identifier_tokens[0].value == 'MyClass'

    def test_function_name_identifier(self):
        """Test function names as identifiers."""
        lexer = PythonLexer()
        lexer.lex(None, 'def my_function():')

        tokens = list(lexer._tokens)
        identifier_tokens = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifier_tokens) == 1, "Should have 1 identifier (my_function)"
        assert identifier_tokens[0].value == 'my_function'
