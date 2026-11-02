"""
Tests for the Pascal lexer.
"""
from syntax.lexer import TokenType
from syntax.pascal.pascal_lexer import PascalLexer, PascalLexerState


def lex_all(source: str) -> list:
    """Lex the whole source and return the token list."""
    lexer = PascalLexer()
    lexer.lex(None, source)
    tokens = []
    while True:
        token = lexer.get_next_token()
        if token is None:
            break

        tokens.append(token)

    return tokens


def token_types(source: str) -> list[TokenType]:
    """Return the token types for the given source."""
    return [t.type for t in lex_all(source)]


class TestPascalLexerKeywords:
    """Test Pascal keyword and identifier lexing."""

    def test_keywords_are_case_insensitive(self):
        """Test that keywords are recognised regardless of case."""
        for value in ('begin', 'BEGIN', 'Begin'):
            tokens = lex_all(value)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD
            assert tokens[0].value == value

    def test_common_keywords(self):
        """Test that common Pascal keywords are recognised."""
        keywords = ('program', 'unit', 'interface', 'implementation', 'uses', 'var',
                    'const', 'type', 'procedure', 'function', 'begin', 'end', 'if',
                    'then', 'else', 'while', 'do', 'for', 'to', 'downto', 'repeat',
                    'until', 'case', 'of', 'record', 'array', 'set', 'with', 'nil')
        for keyword in keywords:
            tokens = lex_all(keyword)
            assert len(tokens) == 1, keyword
            assert tokens[0].type == TokenType.KEYWORD, keyword

    def test_booleans(self):
        """Test that boolean literals are recognised case-insensitively."""
        for value in ('true', 'false', 'True', 'False'):
            tokens = lex_all(value)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.BOOLEAN

    def test_identifier(self):
        """Test that a plain identifier is recognised."""
        tokens = lex_all('MyVariable')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_identifier_with_underscore_and_digits(self):
        """Test identifiers containing underscores and digits."""
        tokens = lex_all('_foo_bar1')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER


class TestPascalLexerComments:
    """Test Pascal comment lexing."""

    def test_line_comment(self):
        """Test a // line comment."""
        tokens = lex_all('// hello')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == '// hello'

    def test_brace_comment_single_line(self):
        """Test a { ... } comment on a single line."""
        tokens = lex_all('{ hello }')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == '{ hello }'

    def test_paren_comment_single_line(self):
        """Test a (* ... *) comment on a single line."""
        tokens = lex_all('(* hello *)')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == '(* hello *)'

    def test_brace_comment_spans_lines(self):
        """Test that a { ... } comment continues across lines."""
        lexer = PascalLexer()
        state = lexer.lex(None, '{ start')
        assert isinstance(state, PascalLexerState)
        assert state.in_brace_comment is True

        lexer2 = PascalLexer()
        state2 = lexer2.lex(state, 'still comment }')
        assert state2.in_brace_comment is False

    def test_paren_comment_spans_lines(self):
        """Test that a (* ... *) comment continues across lines."""
        lexer = PascalLexer()
        state = lexer.lex(None, '(* start')
        assert state.in_paren_comment is True

        lexer2 = PascalLexer()
        state2 = lexer2.lex(state, 'still comment *)')
        assert state2.in_paren_comment is False

    def test_lparen_not_comment(self):
        """Test that a plain left parenthesis is an operator."""
        tokens = lex_all('(x)')
        assert tokens[0].type == TokenType.LPAREN


class TestPascalLexerStrings:
    """Test Pascal string lexing."""

    def test_simple_string(self):
        """Test a simple single-quoted string."""
        tokens = lex_all("'hello'")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == "'hello'"

    def test_escaped_quote(self):
        """Test that a doubled quote is treated as an escaped quote."""
        tokens = lex_all("'it''s'")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == "'it''s'"

    def test_string_spans_lines(self):
        """Test that a string literal continues across lines."""
        lexer = PascalLexer()
        state = lexer.lex(None, "s := 'line one")
        assert state.in_string is True

        lexer2 = PascalLexer()
        state2 = lexer2.lex(state, "line two'")
        assert state2.in_string is False


class TestPascalLexerNumbers:
    """Test Pascal number lexing."""

    def test_decimal_integer(self):
        """Test a decimal integer."""
        tokens = lex_all('42')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER

    def test_float(self):
        """Test a floating-point number."""
        tokens = lex_all('3.14')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER

    def test_scientific_notation(self):
        """Test scientific notation."""
        tokens = lex_all('1.5e10')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER

    def test_hex_number(self):
        """Test a hexadecimal number."""
        tokens = lex_all('$FF')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER

    def test_binary_number(self):
        """Test a binary number."""
        tokens = lex_all('%1010')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER

    def test_octal_number(self):
        """Test an octal number."""
        tokens = lex_all('&77')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER

    def test_character_code(self):
        """Test a #NN character code."""
        tokens = lex_all('#65')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER

    def test_hex_character_code(self):
        """Test a #$NN character code."""
        tokens = lex_all('#$41')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.CHARACTER

    def test_range_operator_not_number(self):
        """Test that .. is an operator, not part of a number."""
        tokens = lex_all('1..10')
        types = [t.type for t in tokens]
        assert types == [TokenType.NUMBER, TokenType.OPERATOR, TokenType.NUMBER]


class TestPascalLexerOperators:
    """Test Pascal operator lexing."""

    def test_assignment_operator(self):
        """Test the := assignment operator."""
        tokens = lex_all(':=')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.OPERATOR
        assert tokens[0].value == ':='

    def test_range_operator(self):
        """Test the .. range operator."""
        tokens = lex_all('..')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.OPERATOR
        assert tokens[0].value == '..'

    def test_not_equal_operator(self):
        """Test the <> not-equal operator."""
        tokens = lex_all('<>')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.OPERATOR
        assert tokens[0].value == '<>'

    def test_comparison_operators(self):
        """Test <= and >= operators."""
        tokens = lex_all('<= >=')
        op_tokens = [t for t in tokens if t.type == TokenType.OPERATOR]
        assert [t.value for t in op_tokens] == ['<=', '>=']

    def test_parentheses(self):
        """Test that parentheses produce LPAREN and RPAREN tokens."""
        tokens = lex_all('()')
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN

    def test_pointer_operator(self):
        """Test the ^ pointer operator."""
        tokens = lex_all('^')
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.OPERATOR
        assert tokens[0].value == '^'


class TestPascalLexerState:
    """Test Pascal lexer state handling."""

    def test_state_type_assertion(self):
        """Test that the lexer asserts the correct state type."""
        lexer = PascalLexer()
        state = lexer.lex(None, 'begin')
        assert isinstance(state, PascalLexerState)

    def test_fresh_state_has_no_continuation(self):
        """Test that a fresh lex sets no continuation flags."""
        lexer = PascalLexer()
        state = lexer.lex(None, 'begin end')
        assert state.in_brace_comment is False
        assert state.in_paren_comment is False
        assert state.in_string is False
