"""
Tests for Menai function-call classification.

These tests verify that identifiers in operator position (the head of an
ordinary form) are tokenized as FUNCTION_OR_METHOD, while identifiers in
binding, parameter, field, export, pattern, namespace, and quoted positions
remain IDENTIFIER.
"""
from syntax.lexer import TokenType
from syntax.menai.menai_parser import MenaiParser


def parse_line(source: str) -> list:
    """
    Parse a single line of Menai source and return its tokens.

    Args:
        source: The Menai source line

    Returns:
        The list of tokens produced by the parser
    """
    parser = MenaiParser()
    parser.parse(None, source)
    return list(parser._tokens)


def parse_lines(lines: list[str]) -> list:
    """
    Parse multiple lines of Menai source, preserving parser state.

    Args:
        lines: The Menai source lines

    Returns:
        The list of tokens produced by the parser
    """
    parser = MenaiParser()
    state = None
    for line in lines:
        state = parser.parse(state, line)

    return list(parser._tokens)


def token_values(tokens: list, token_type: TokenType) -> list[str]:
    """
    Extract the values of all tokens of a given type.

    Args:
        tokens: The tokens to filter
        token_type: The token type to match

    Returns:
        The values of matching tokens, in order
    """
    return [t.value for t in tokens if t.type == token_type]


class TestMenaiCallContext:
    """Test Menai function-call classification."""

    def test_simple_call_head_is_function(self):
        """Test that the head of a simple call is a function."""
        tokens = parse_line('(foo x y)')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']
        assert token_values(tokens, TokenType.IDENTIFIER) == ['x', 'y']

    def test_nested_call_heads_are_functions(self):
        """Test that nested call heads are functions."""
        tokens = parse_line('(foo (bar x) (baz y))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo', 'bar', 'baz']

    def test_operator_symbols_are_functions(self):
        """Test that symbolic operators in head position are functions."""
        tokens = parse_line('(+ 1 2)')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['+']

    def test_arguments_are_not_functions(self):
        """Test that identifiers in argument position are not functions."""
        tokens = parse_line('(foo bar baz)')

        assert token_values(tokens, TokenType.IDENTIFIER) == ['bar', 'baz']

    def test_if_head_is_keyword_not_function(self):
        """Test that the if keyword is not classified as a function."""
        tokens = parse_line('(if #t (foo) (bar))')

        assert token_values(tokens, TokenType.KEYWORD) == ['if']
        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo', 'bar']

    def test_and_or_arguments_are_functions(self):
        """Test that calls inside and/or are classified."""
        tokens = parse_line('(and (foo) (or (bar) (baz)))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo', 'bar', 'baz']

    def test_let_binding_names_are_not_functions(self):
        """Test that let binding names are not classified as functions."""
        tokens = parse_line('(let ((x 5) (y 6)) (foo x y))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']
        assert token_values(tokens, TokenType.IDENTIFIER) == ['x', 'y', 'x', 'y']

    def test_let_binding_values_are_functions(self):
        """Test that calls in let binding values are classified."""
        tokens = parse_line('(let ((x (foo)) (y (bar))) x)')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo', 'bar']

    def test_let_star_binding_names_are_not_functions(self):
        """Test that let* binding names are not classified as functions."""
        tokens = parse_line('(let* ((x 1) (y (foo x))) (bar y))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo', 'bar']

    def test_letrec_binding_names_are_not_functions(self):
        """Test that letrec binding names are not classified as functions."""
        tokens = parse_line('(letrec ((f (lambda (n) (g n)))) (f 1))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['g', 'f']

    def test_lambda_parameters_are_not_functions(self):
        """Test that lambda parameters are not classified as functions."""
        tokens = parse_line('(lambda (x y) (foo x y))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']
        assert token_values(tokens, TokenType.IDENTIFIER) == ['x', 'y', 'x', 'y']

    def test_lambda_body_is_function(self):
        """Test that a call in a lambda body is classified."""
        tokens = parse_line('(lambda (x) (foo x))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']

    def test_variadic_lambda_parameters_are_not_functions(self):
        """Test that variadic lambda parameters are not classified as functions."""
        tokens = parse_line('(lambda (a . rest) (foo a rest))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']
        assert 'rest' in token_values(tokens, TokenType.IDENTIFIER)

    def test_struct_field_names_are_not_functions(self):
        """Test that struct field names are not classified as functions."""
        tokens = parse_line('(struct (x y))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['x', 'y']

    def test_struct_as_binding_value(self):
        """Test a struct definition used as a letrec binding value."""
        tokens = parse_line('(letrec ((point (struct (x y)))) point)')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert 'point' in token_values(tokens, TokenType.IDENTIFIER)

    def test_export_names_are_not_functions(self):
        """Test that export names are not classified as functions."""
        tokens = parse_line('(export square cube)')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['square', 'cube']

    def test_export_head_is_keyword(self):
        """Test that export is recognized as a keyword."""
        tokens = parse_line('(export foo)')

        assert token_values(tokens, TokenType.KEYWORD) == ['export']

    def test_namespace_access_operands_are_not_functions(self):
        """Test that namespace access operands are not classified as functions."""
        tokens = parse_line('(:: math square)')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['math', 'square']

    def test_namespace_access_head_is_keyword(self):
        """Test that the :: operator is recognized as a keyword."""
        tokens = parse_line('(:: math square)')

        assert token_values(tokens, TokenType.KEYWORD) == ['::']

    def test_namespace_member_call_is_function(self):
        """Test that a call to a namespace member is classified."""
        tokens = parse_line('((:: math square) 5)')

        assert token_values(tokens, TokenType.KEYWORD) == ['::']
        assert token_values(tokens, TokenType.IDENTIFIER) == ['math', 'square']

    def test_import_module_name_is_not_function(self):
        """Test that an import module name is not classified as a function."""
        tokens = parse_line('(import "math_utils")')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.KEYWORD) == ['import']

    def test_match_subject_is_not_function(self):
        """Test that the match subject is not classified as a function."""
        tokens = parse_line('(match x (1 "one") (_ "other"))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []

    def test_match_result_expressions_are_functions(self):
        """Test that calls in match results are classified."""
        tokens = parse_line('(match x ((? integer? n) (foo n)) (_ (bar)))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo', 'bar']

    def test_match_pattern_names_are_not_functions(self):
        """Test that names in match patterns are not classified as functions."""
        tokens = parse_line('(match x ((a b c) (foo a b c)) (_ 0))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']
        assert token_values(tokens, TokenType.IDENTIFIER) == ['x', 'a', 'b', 'c', 'a', 'b', 'c', '_']

    def test_match_predicate_pattern_is_not_function(self):
        """Test that a predicate in a pattern is not classified as a function."""
        tokens = parse_line('(match x ((? string? s) s) (_ 0))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert 'string?' in token_values(tokens, TokenType.IDENTIFIER)

    def test_match_cons_pattern_is_not_function(self):
        """Test that a cons pattern is not classified as a function."""
        tokens = parse_line('(match lst ((head . tail) (foo head tail)) (_ 0))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']

    def test_match_subject_call_is_function(self):
        """Test that a call in the match subject is classified."""
        tokens = parse_line('(match (foo x) (1 "one") (_ "other"))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']

    def test_quoted_symbol_is_not_function(self):
        """Test that a quoted symbol is not classified as a function."""
        tokens = parse_line("'foo")

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['foo']

    def test_quoted_list_head_is_not_function(self):
        """Test that the head of a quoted list is not classified as a function."""
        tokens = parse_line("'(foo bar)")

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['foo', 'bar']

    def test_quote_form_contents_are_not_functions(self):
        """Test that the contents of a quote form are not classified as functions."""
        tokens = parse_line('(quote (foo bar))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['foo', 'bar']

    def test_quoted_argument_is_not_function(self):
        """Test that quoted arguments are not classified as functions."""
        tokens = parse_line("(list 'foo 'bar)")

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['list']
        assert token_values(tokens, TokenType.IDENTIFIER) == ['foo', 'bar']

    def test_apply_arguments_are_functions(self):
        """Test that calls inside apply arguments are classified."""
        tokens = parse_line('(apply foo (list (bar)))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['list', 'bar']
        assert 'foo' in token_values(tokens, TokenType.IDENTIFIER)

    def test_top_level_atom_is_not_function(self):
        """Test that a top-level atom is not classified as a function."""
        tokens = parse_line('foo')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['foo']

    def test_multiple_top_level_forms(self):
        """Test classification across multiple top-level forms."""
        tokens = parse_line('(foo) (bar) baz')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo', 'bar']
        assert token_values(tokens, TokenType.IDENTIFIER) == ['baz']

    def test_call_spanning_multiple_lines(self):
        """Test classification of a call spanning multiple lines."""
        tokens = parse_lines(['(let ((x 1))', '  (foo x))'])

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']

    def test_binding_list_spanning_multiple_lines(self):
        """Test that binding names spanning lines are not functions."""
        tokens = parse_lines(['(let ((x 1)', '      (y 2))', '  (foo x y))'])

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['foo']

    def test_unclosed_form_does_not_crash(self):
        """Test that an unclosed form is handled without raising."""
        tokens = parse_line('(let ((x')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []

    def test_unmatched_close_paren_does_not_crash(self):
        """Test that an unmatched close paren is handled without raising."""
        tokens = parse_line(')) foo')

        assert token_values(tokens, TokenType.IDENTIFIER) == ['foo']

    def test_quote_spanning_a_line_break_quotes_next_form(self):
        """Test that a quote before a line break quotes the following form."""
        tokens = parse_lines(["'", '(foo bar)'])

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []
        assert token_values(tokens, TokenType.IDENTIFIER) == ['foo', 'bar']

    def test_keyword_head_is_not_function(self):
        """Test that a special-form keyword head is not a function."""
        tokens = parse_line('(if x y z)')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == []

    def test_deeply_nested_calls(self):
        """Test classification of deeply nested calls."""
        tokens = parse_line('(a (b (c (d x))))')

        assert token_values(tokens, TokenType.FUNCTION_OR_METHOD) == ['a', 'b', 'c', 'd']

    def test_paren_depth_preserved_with_calls(self):
        """Test that parenthesis depth tracking still works."""
        parser = MenaiParser()
        state = parser.parse(None, '((foo')

        assert state.paren_depth == 2


class TestMenaiCallContextStateIsolation:
    """Test that saved call-context state is isolated between parser runs."""

    def test_saved_state_is_not_mutated_by_later_parsing(self):
        """Test that parsing a later line leaves an earlier saved state intact."""
        parser = MenaiParser()
        first_state = parser.parse(None, '(letrec')
        parser.parse(first_state, '  ((pi 3.14)')
        second_state = parser.parse(first_state, '  ((pi 3.14)')

        frames = second_state.call_context_state.frames
        assert [frame.kind.name for frame in frames] == ['APPLICATION', 'BINDING_LIST']

    def test_reparsing_a_line_does_not_corrupt_its_predecessor_state(self):
        """Test that re-parsing a line from a saved state leaves that state unchanged."""
        parser = MenaiParser()
        first_state = parser.parse(None, '(let')
        parser.parse(first_state, '  ((x 1)')
        second_state = parser.parse(first_state, '  ((x 1)')

        parser.parse(first_state, '  ((x 1)')
        reparsed_state = parser.parse(first_state, '  ((x 1)')

        assert (
            [frame.kind.name for frame in second_state.call_context_state.frames]
            == [frame.kind.name for frame in reparsed_state.call_context_state.frames]
        )

    def test_restoring_state_does_not_alias_it(self):
        """Test that a restored state shares no mutable frames with its source."""
        parser = MenaiParser()
        first_state = parser.parse(None, '(let')

        parser.parse(first_state, '((x (foo)))')

        frames = first_state.call_context_state.frames
        assert len(frames) == 1
        assert frames[0].kind.name == 'APPLICATION'
