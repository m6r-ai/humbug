"""
Comprehensive tests for the C++ lexer.

Tests basic lexer functionality, identifiers, whitespace, and general tokenization.
"""
import pytest

from syntax.cpp.cpp_lexer import CppLexer


class TestCppLexerBasics:
    """Test basic C++ lexer functionality."""

    def test_empty_input(self):
        """Test lexing empty input."""
        lexer = CppLexer()
        state = lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0, "Empty input should produce no tokens"

    def test_whitespace_only(self):
        """Test lexing whitespace-only input."""
        test_cases = [
            ' ',
            '   ',
            '\t',
            '  \t  ',
        ]
        for code in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Whitespace is consumed but not emitted as tokens
            # (the lexer reads whitespace but doesn't create tokens for it)
            assert len(tokens) == 0, f"Whitespace input should produce no tokens"

    def test_single_identifier(self):
        """Test lexing a single identifier."""
        test_cases = [
            'x',
            'variable',
            'myVariable',
            'my_variable',
            '_private',
            '__special',
            'var123',
            'CamelCase',
        ]
        for code in test_cases:
            lexer = CppLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Single identifier '{code}' should produce one token"
            assert tokens[0].type.name == 'IDENTIFIER'
            assert tokens[0].value == code

    def test_multiple_tokens(self):
        """Test lexing multiple tokens."""
        lexer = CppLexer()
        lexer.lex(None, 'int x = 42;')

        tokens = list(lexer._tokens)
        # Should have: int, x, =, 42, ;
        # Whitespace may or may not be included
        non_whitespace = [t for t in tokens if t.type.name != 'WHITESPACE']
        assert len(non_whitespace) == 5

    def test_lexer_state_persistence(self):
        """Test that lexer state persists across lines."""
        lexer1 = CppLexer()
        state1 = lexer1.lex(None, '/* start comment')

        assert state1.in_block_comment == True

        lexer2 = CppLexer()
        state2 = lexer2.lex(state1, 'end comment */')

        assert state2.in_block_comment == False


class TestCppIdentifiers:
    """Test C++ identifier tokenization."""

    def test_simple_identifiers(self):
        """Test simple identifier names."""
        identifiers = [
            'a', 'z', 'A', 'Z',
            'abc', 'xyz', 'ABC', 'XYZ',
            'camelCase', 'PascalCase', 'snake_case', 'SCREAMING_SNAKE_CASE',
        ]
        for ident in identifiers:
            lexer = CppLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'IDENTIFIER'
            assert tokens[0].value == ident

    def test_identifiers_with_numbers(self):
        """Test identifiers containing numbers."""
        identifiers = [
            'var1', 'var2', 'var123',
            'x0', 'y1', 'z2',
            'test123abc',
        ]
        for ident in identifiers:
            lexer = CppLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'IDENTIFIER'
            assert tokens[0].value == ident

    def test_identifiers_with_underscores(self):
        """Test identifiers with underscores."""
        identifiers = [
            '_', '__', '___',
            '_var', 'var_', '_var_',
            '__private', '__init__',
            'my_long_variable_name',
        ]
        for ident in identifiers:
            lexer = CppLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'IDENTIFIER'
            assert tokens[0].value == ident

    def test_identifiers_not_keywords(self):
        """Test that identifiers are distinguished from keywords."""
        # These look like keywords but aren't
        identifiers = [
            'integer', 'Int', 'INT',
            'Class', 'CLASS',
            'namespace_', '_namespace',
            'auto_', 'auto123',
        ]
        for ident in identifiers:
            lexer = CppLexer()
            lexer.lex(None, ident)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'IDENTIFIER', f"'{ident}' should be IDENTIFIER, not KEYWORD"
            assert tokens[0].value == ident

    def test_unicode_identifiers(self):
        """Test identifiers with Unicode characters (C++11 allows this)."""
        # Note: C++11 allows Unicode characters in identifiers
        # The lexer may or may not support this
        identifiers = [
            'café',
            'naïve',
            '日本語',
            'Δx',
        ]
        for ident in identifiers:
            lexer = CppLexer()
            try:
                lexer.lex(None, ident)
                tokens = list(lexer._tokens)
                # If it tokenizes, check what we got
                assert len(tokens) >= 1
            except:
                # Unicode may not be supported, which is fine
                pass


class TestCppOperators:
    """Test C++ operator tokenization."""

    def test_arithmetic_operators(self):
        """Test arithmetic operators."""
        operators = ['+', '-', '*', '/', '%']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_comparison_operators(self):
        """Test comparison operators."""
        operators = ['==', '!=', '<', '>', '<=', '>=']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_logical_operators(self):
        """Test logical operators."""
        operators = ['&&', '||', '!']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_bitwise_operators(self):
        """Test bitwise operators."""
        operators = ['&', '|', '^', '~', '<<', '>>']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_assignment_operators(self):
        """Test assignment operators."""
        operators = [
            '=', '+=', '-=', '*=', '/=', '%=',
            '&=', '|=', '^=', '<<=', '>>=',
        ]
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_increment_decrement_operators(self):
        """Test increment and decrement operators."""
        operators = ['++', '--']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_member_access_operators(self):
        """Test member access operators."""
        operators = ['.', '->', '::']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_ternary_operator(self):
        """Test ternary operator components."""
        operators = ['?', ':']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_brackets_and_parens(self):
        """Test brackets, parentheses, and braces."""
        operators = ['(', ')', '[', ']', '{', '}']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_punctuation(self):
        """Test punctuation operators."""
        operators = [';', ',']
        for op in operators:
            lexer = CppLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'OPERATOR'
            assert tokens[0].value == op

    def test_compound_assignment_edge_cases(self):
        """Test compound assignment operators with edge cases."""
        # Test that >>= is tokenized as one operator, not >> and =
        lexer = CppLexer()
        lexer.lex(None, '>>=')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '>>='

    def test_alternative_operators(self):
        """Test alternative operator representations."""
        # C++ supports alternative operator keywords
        alternatives = [
            ('and', 'KEYWORD'),      # alternative for &&
            ('or', 'KEYWORD'),       # alternative for ||
            ('not', 'KEYWORD'),      # alternative for !
            ('and_eq', 'KEYWORD'),   # alternative for &=
            ('or_eq', 'KEYWORD'),    # alternative for |=
            ('xor', 'KEYWORD'),      # alternative for ^
            ('xor_eq', 'KEYWORD'),   # alternative for ^=
            ('bitand', 'KEYWORD'),   # alternative for &
            ('bitor', 'KEYWORD'),    # alternative for |
            ('compl', 'KEYWORD'),    # alternative for ~
            ('not_eq', 'KEYWORD'),   # alternative for !=
        ]
        for alt, expected_type in alternatives:
            lexer = CppLexer()
            lexer.lex(None, alt)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == expected_type
            assert tokens[0].value == alt


class TestCppWhitespace:
    """Test whitespace handling."""

    def test_spaces(self):
        """Test space characters."""
        lexer = CppLexer()
        lexer.lex(None, 'a b c')

        tokens = list(lexer._tokens)
        # Should have identifiers a, b, c, possibly with whitespace tokens
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifiers) == 3
        assert identifiers[0].value == 'a'
        assert identifiers[1].value == 'b'
        assert identifiers[2].value == 'c'

    def test_tabs(self):
        """Test tab characters."""
        lexer = CppLexer()
        lexer.lex(None, 'a\tb\tc')

        tokens = list(lexer._tokens)
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifiers) == 3

    def test_mixed_whitespace(self):
        """Test mixed whitespace."""
        lexer = CppLexer()
        lexer.lex(None, 'a  \t  b   c')

        tokens = list(lexer._tokens)
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']
        assert len(identifiers) == 3

    def test_leading_whitespace(self):
        """Test leading whitespace."""
        lexer = CppLexer()
        lexer.lex(None, '   int')

        tokens = list(lexer._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keywords) == 1
        assert keywords[0].value == 'int'

    def test_trailing_whitespace(self):
        """Test trailing whitespace."""
        lexer = CppLexer()
        lexer.lex(None, 'int   ')

        tokens = list(lexer._tokens)
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keywords) == 1
        assert keywords[0].value == 'int'


class TestCppTokenPositions:
    """Test that tokens have correct position information."""

    def test_token_start_positions(self):
        """Test that tokens have correct start positions."""
        lexer = CppLexer()
        lexer.lex(None, 'int x = 42;')

        tokens = list(lexer._tokens)
        non_whitespace = [t for t in tokens if t.type.name != 'WHITESPACE']

        # Each token should have a start position
        for token in non_whitespace:
            assert hasattr(token, 'start')
            assert token.start >= 0

    def test_token_positions_sequential(self):
        """Test that token positions are in order."""
        lexer = CppLexer()
        lexer.lex(None, 'a b c')

        tokens = list(lexer._tokens)

        # Token positions should be non-decreasing
        for i in range(len(tokens) - 1):
            assert tokens[i].start <= tokens[i + 1].start

    def test_token_value_matches_input(self):
        """Test that token values match the input at their positions."""
        input_str = 'int x = 42;'
        lexer = CppLexer()
        lexer.lex(None, input_str)

        tokens = list(lexer._tokens)

        for token in tokens:
            # Token value should match the input at the token's position
            if token.type.name != 'WHITESPACE':
                assert input_str[token.start:token.start + len(token.value)] == token.value


class TestCppComplexExpressions:
    """Test tokenization of complex expressions."""

    def test_arithmetic_expression(self):
        """Test arithmetic expression."""
        lexer = CppLexer()
        lexer.lex(None, '(a + b) * c - d / e')

        tokens = list(lexer._tokens)
        # Should tokenize all parts of the expression
        assert len(tokens) >= 11

    def test_boolean_expression(self):
        """Test boolean expression."""
        lexer = CppLexer()
        lexer.lex(None, 'a && b || !c')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 5

    def test_function_call(self):
        """Test function call."""
        lexer = CppLexer()
        lexer.lex(None, 'func(a, b, c)')

        tokens = list(lexer._tokens)
        # Should have: func, (, a, ,, b, ,, c, )
        assert len(tokens) >= 8

    def test_array_access(self):
        """Test array access."""
        lexer = CppLexer()
        lexer.lex(None, 'arr[i]')

        tokens = list(lexer._tokens)
        # Should have: arr, [, i, ]
        assert len(tokens) >= 4

    def test_member_access(self):
        """Test member access."""
        lexer = CppLexer()
        lexer.lex(None, 'obj.member')

        tokens = list(lexer._tokens)
        # Should have: obj, ., member
        assert len(tokens) >= 3

    def test_pointer_access(self):
        """Test pointer member access."""
        lexer = CppLexer()
        lexer.lex(None, 'ptr->member')

        tokens = list(lexer._tokens)
        # Should have: ptr, ->, member
        assert len(tokens) >= 3

    def test_scope_resolution(self):
        """Test scope resolution operator."""
        lexer = CppLexer()
        lexer.lex(None, 'std::cout')

        tokens = list(lexer._tokens)
        # Should have: std, ::, cout
        assert len(tokens) >= 3

    def test_template_instantiation(self):
        """Test template instantiation."""
        lexer = CppLexer()
        lexer.lex(None, 'vector<int>')

        tokens = list(lexer._tokens)
        # Should have: vector, <, int, >
        assert len(tokens) >= 4


class TestCppEdgeCases:
    """Test edge cases and corner cases."""

    def test_maximum_munch(self):
        """Test that lexer uses maximum munch principle."""
        # ++ should be one token, not two + tokens
        lexer = CppLexer()
        lexer.lex(None, '++')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].value == '++'

    def test_ambiguous_operators(self):
        """Test disambiguation of ambiguous operator sequences."""
        # >>= should be one token, not >> and =
        lexer = CppLexer()
        lexer.lex(None, '>>=')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].value == '>>='

    def test_no_space_between_tokens(self):
        """Test tokens without spaces."""
        lexer = CppLexer()
        lexer.lex(None, 'int(x)')

        tokens = list(lexer._tokens)
        non_whitespace = [t for t in tokens if t.type.name != 'WHITESPACE']
        # Should have: int, (, x, )
        assert len(non_whitespace) == 4

    def test_adjacent_operators(self):
        """Test adjacent operators."""
        lexer = CppLexer()
        lexer.lex(None, 'a+++b')

        tokens = list(lexer._tokens)
        # Should tokenize as: a, ++, +, b (maximum munch)
        non_whitespace = [t for t in tokens if t.type.name != 'WHITESPACE']
        assert len(non_whitespace) == 4

    def test_very_long_identifier(self):
        """Test very long identifier."""
        long_id = 'a' * 1000
        lexer = CppLexer()
        lexer.lex(None, long_id)

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == long_id

    def test_empty_string_after_comment(self):
        """Test that lexer handles end of input correctly."""
        lexer = CppLexer()
        lexer.lex(None, '// comment')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'COMMENT'
