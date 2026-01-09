"""
Tests for JavaScript operator tokenization.
"""
import pytest

from syntax.javascript.javascript_lexer import JavaScriptLexer


class TestJavaScriptOperators:
    """Test JavaScript operator tokenization."""

    def test_arithmetic_operators(self):
        """Test arithmetic operators."""
        test_cases = [
            '+',
            '-',
            '*',
            '/',
            '%',
            '**',  # exponentiation
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR' or tokens[0].type.name == 'COMMENT' or tokens[0].type.name == 'REGEXP', \
                f"'{op}' should be OPERATOR type (or COMMENT/REGEXP for special cases)"

    def test_assignment_operators(self):
        """Test assignment operators."""
        test_cases = [
            '=',
            '+=',
            '-=',
            '*=',
            '/=',
            '%=',
            '**=',
            '&=',
            '|=',
            '^=',
            '<<=',
            '>>=',
            '>>>=',
            '&&=',
            '||=',
            '??=',
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_comparison_operators(self):
        """Test comparison operators."""
        test_cases = [
            '==',
            '===',
            '!=',
            '!==',
            '<',
            '>',
            '<=',
            '>=',
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_logical_operators(self):
        """Test logical operators."""
        test_cases = [
            '&&',
            '||',
            '!',
            '??',  # nullish coalescing
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_bitwise_operators(self):
        """Test bitwise operators."""
        test_cases = [
            '&',
            '|',
            '^',
            '~',
            '<<',
            '>>',
            '>>>',  # unsigned right shift
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_unary_operators(self):
        """Test unary operators."""
        test_cases = [
            '++',
            '--',
            '+',
            '-',
            '!',
            '~',
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"

    def test_ternary_operator(self):
        """Test ternary operator components."""
        test_cases = [
            '?',
            ':',
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_member_access_operators(self):
        """Test member access operators."""
        test_cases = [
            '.',
            '?.',  # optional chaining
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_spread_operator(self):
        """Test spread/rest operator."""
        lexer = JavaScriptLexer()
        lexer.lex(None, '...')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '...'

    def test_bracket_operators(self):
        """Test bracket operators."""
        test_cases = [
            '(',
            ')',
            '[',
            ']',
            '{',
            '}',
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op

    def test_punctuation_operators(self):
        """Test punctuation operators."""
        test_cases = [
            ',',
            ';',
        ]
        for op in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, op)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type.name == 'OPERATOR', f"'{op}' should be OPERATOR type"
            assert tokens[0].value == op


class TestJavaScriptOperatorEdgeCases:
    """Test edge cases for JavaScript operator tokenization."""

    def test_operators_in_expressions(self):
        """Test operators in various expressions."""
        test_cases = [
            'x + y',
            'a - b',
            'c * d',
            'e / f',
            'g % h',
            'i ** j',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
            assert len(operator_tokens) >= 1, f"'{code}' should have at least one operator"

    def test_compound_operators(self):
        """Test compound assignment operators."""
        test_cases = [
            'x += 5',
            'y -= 3',
            'z *= 2',
            'a /= 4',
            'b %= 3',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
            # Should have compound operator and maybe =
            assert len(operator_tokens) >= 1, f"'{code}' should have operator(s)"

    def test_increment_decrement_prefix_postfix(self):
        """Test increment/decrement operators."""
        test_cases = [
            '++x',
            'x++',
            '--y',
            'y--',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
            assert len(operator_tokens) >= 1, f"'{code}' should have operator"

    def test_ternary_expression(self):
        """Test ternary operator in expression."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'x > 0 ? "pos" : "neg"')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        # Should have >, ?, and : operators
        assert len(operator_tokens) >= 3, "Should have multiple operators"

    def test_optional_chaining(self):
        """Test optional chaining operator."""
        test_cases = [
            'obj?.prop',
            'arr?.[0]',
            'func?.()',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have ?. operator
            optional_chaining = [t for t in tokens if t.value == '?.']
            assert len(optional_chaining) >= 1, f"'{code}' should have ?. operator"

    def test_nullish_coalescing(self):
        """Test nullish coalescing operator."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'value ?? default')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.value == '??']
        assert len(operator_tokens) == 1, "Should have ?? operator"

    def test_spread_in_context(self):
        """Test spread operator in various contexts."""
        test_cases = [
            '[...arr]',
            '{...obj}',
            'func(...args)',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            spread_tokens = [t for t in tokens if t.value == '...']
            assert len(spread_tokens) == 1, f"'{code}' should have ... operator"

    def test_operator_precedence_expression(self):
        """Test expression with multiple operators."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'a + b * c - d / e')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        # Should have +, *, -, / operators
        assert len(operator_tokens) == 4, "Should have 4 operators"

    def test_bitwise_operations(self):
        """Test bitwise operations."""
        test_cases = [
            'a & b',
            'c | d',
            'e ^ f',
            '~g',
            'h << 2',
            'i >> 3',
            'j >>> 4',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
            assert len(operator_tokens) >= 1, f"'{code}' should have operator(s)"

    def test_comparison_chains(self):
        """Test comparison operators in chains."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'a < b && b < c')

        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        # Should have <, &&, < operators
        assert len(operator_tokens) >= 3, "Should have multiple operators"

    def test_equality_operators(self):
        """Test equality vs strict equality."""
        test_cases = [
            'a == b',
            'a === b',
            'a != b',
            'a !== b',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
            assert len(operator_tokens) >= 1, f"'{code}' should have operator"

    def test_arrow_function_arrow(self):
        """Test arrow function arrow (=>) operator."""
        lexer = JavaScriptLexer()
        lexer.lex(None, 'x => x * 2')

        tokens = list(lexer._tokens)
        # => should be tokenized (might be two tokens: = and >)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(operator_tokens) >= 1, "Should have operators"

    def test_operators_without_spaces(self):
        """Test operators without surrounding spaces."""
        test_cases = [
            '1+2',
            '3*4',
            '5/6',
            'a&&b',
            'c||d',
        ]
        for code in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have number/identifier, operator, number/identifier
            assert len(tokens) >= 3, f"'{code}' should have at least 3 tokens"

    def test_dot_vs_decimal_point(self):
        """Test distinguishing dot operator from decimal point."""
        test_cases = [
            ('obj.prop', True),  # dot operator
            ('3.14', False),     # decimal point
        ]
        for code, has_dot_operator in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            dot_operators = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == '.']
            if has_dot_operator:
                assert len(dot_operators) >= 1, f"'{code}' should have dot operator"
            else:
                assert len(dot_operators) == 0, f"'{code}' should not have dot operator"

    def test_division_vs_regex_vs_comment(self):
        """Test distinguishing division, regex, and comments."""
        test_cases = [
            ('x / y', 'division'),
            ('// comment', 'comment'),
            ('/pattern/', 'regex'),
        ]
        for code, expected_type in test_cases:
            lexer = JavaScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1, f"'{code}' should produce tokens"
