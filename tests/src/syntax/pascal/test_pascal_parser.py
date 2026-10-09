"""
Tests for the Pascal parser.
"""
from syntax.lexer import TokenType
from syntax.pascal.pascal_parser import PascalParser, PascalParserState


class TestPascalParser:
    """Test Pascal parser."""

    def test_parse_simple_assignment(self):
        """Test parsing a simple assignment."""
        parser = PascalParser()
        state = parser.parse(None, 'x := 42;')

        assert isinstance(state, PascalParserState)
        assert state.parsing_continuation is False
        assert state.continuation_state == 0
        assert state.in_element is False

    def test_parse_function_call(self):
        """Test parsing a function call converts identifier to FUNCTION_OR_METHOD."""
        parser = PascalParser()
        parser.parse(None, 'Writeln(x);')

        tokens = list(parser._tokens)
        writeln_tokens = [t for t in tokens if t.value == 'Writeln']
        assert len(writeln_tokens) == 1
        assert writeln_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_parse_element_access_dot(self):
        """Test parsing element access with dot."""
        parser = PascalParser()
        parser.parse(None, 'obj.field')

        tokens = list(parser._tokens)
        field_tokens = [t for t in tokens if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_parse_chained_element_access(self):
        """Test parsing chained element access."""
        parser = PascalParser()
        parser.parse(None, 'obj.field1.field2')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type == TokenType.ELEMENT]
        assert len(element_tokens) == 2
        assert element_tokens[0].value == 'field1'
        assert element_tokens[1].value == 'field2'

    def test_parse_element_with_method_call(self):
        """Test parsing element access followed by method call."""
        parser = PascalParser()
        parser.parse(None, 'obj.Method()')

        tokens = list(parser._tokens)
        method_tokens = [t for t in tokens if t.value == 'Method']
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_parse_keywords_not_converted(self):
        """Test that keywords followed by parentheses remain keywords."""
        parser = PascalParser()
        parser.parse(None, 'if (x > 0) then')

        tokens = list(parser._tokens)
        if_tokens = [t for t in tokens if t.value == 'if']
        assert len(if_tokens) == 1
        assert if_tokens[0].type == TokenType.KEYWORD

    def test_parse_identifier_not_function(self):
        """Test that a plain identifier is not converted."""
        parser = PascalParser()
        parser.parse(None, 'x := y;')

        tokens = list(parser._tokens)
        y_tokens = [t for t in tokens if t.value == 'y']
        assert len(y_tokens) == 1
        assert y_tokens[0].type == TokenType.IDENTIFIER

    def test_parse_brace_comment_start(self):
        """Test parsing the start of a brace block comment."""
        parser = PascalParser()
        state = parser.parse(None, '{ comment')

        assert state.parsing_continuation is True
        assert state.continuation_state == 1
        assert state.lexer_state.in_brace_comment is True

    def test_parse_brace_comment_continuation(self):
        """Test parsing the continuation of a brace block comment."""
        parser1 = PascalParser()
        state1 = parser1.parse(None, '{ start')

        parser2 = PascalParser()
        state2 = parser2.parse(state1, 'middle')

        assert state2.parsing_continuation is True
        assert state2.lexer_state.in_brace_comment is True

    def test_parse_brace_comment_end(self):
        """Test parsing the end of a brace block comment."""
        parser1 = PascalParser()
        state1 = parser1.parse(None, '{ start')

        parser2 = PascalParser()
        state2 = parser2.parse(state1, 'end }')

        assert state2.parsing_continuation is False
        assert state2.continuation_state == 0
        assert state2.lexer_state.in_brace_comment is False

    def test_parse_paren_comment_start(self):
        """Test parsing the start of a paren block comment."""
        parser = PascalParser()
        state = parser.parse(None, '(* comment')

        assert state.parsing_continuation is True
        assert state.continuation_state == 2
        assert state.lexer_state.in_paren_comment is True

    def test_parse_paren_comment_end(self):
        """Test parsing the end of a paren block comment."""
        parser1 = PascalParser()
        state1 = parser1.parse(None, '(* start')

        parser2 = PascalParser()
        state2 = parser2.parse(state1, 'end *)')

        assert state2.parsing_continuation is False
        assert state2.continuation_state == 0
        assert state2.lexer_state.in_paren_comment is False

    def test_parse_multiline_string_start(self):
        """Test parsing the start of a multi-line string."""
        parser = PascalParser()
        state = parser.parse(None, "s := 'line one")

        assert state.parsing_continuation is True
        assert state.continuation_state == 3
        assert state.lexer_state.in_string is True

    def test_parse_multiline_string_end(self):
        """Test parsing the end of a multi-line string."""
        parser1 = PascalParser()
        state1 = parser1.parse(None, "s := 'line one")

        parser2 = PascalParser()
        state2 = parser2.parse(state1, "line two';")

        assert state2.parsing_continuation is False
        assert state2.continuation_state == 0
        assert state2.lexer_state.in_string is False

    def test_parse_empty_input(self):
        """Test parsing empty input."""
        parser = PascalParser()
        state = parser.parse(None, '')

        assert isinstance(state, PascalParserState)
        assert state.parsing_continuation is False
        assert state.in_element is False

    def test_parse_preserves_element_state(self):
        """Test that parser preserves in_element state across lines."""
        parser1 = PascalParser()
        state1 = parser1.parse(None, 'obj.')

        assert state1.in_element is True

        parser2 = PascalParser()
        state2 = parser2.parse(state1, 'field')

        tokens2 = list(parser2._tokens)
        field_tokens = [t for t in tokens2 if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_parse_function_call_resets_element_state(self):
        """Test that a function call resets the element state."""
        parser = PascalParser()
        state = parser.parse(None, 'obj.Func()')

        assert state.in_element is False

    def test_parse_complex_expression(self):
        """Test parsing a complex expression."""
        parser = PascalParser()
        parser.parse(None, 'obj.field.Method(arg).result')

        tokens = list(parser._tokens)

        field_tokens = [t for t in tokens if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

        method_tokens = [t for t in tokens if t.value == 'Method']
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD

        result_tokens = [t for t in tokens if t.value == 'result']
        assert len(result_tokens) == 1
        assert result_tokens[0].type == TokenType.ELEMENT

    def test_parse_array_access_not_function(self):
        """Test that array access does not mark the identifier as a function."""
        parser = PascalParser()
        parser.parse(None, 'arr[0]')

        tokens = list(parser._tokens)
        arr_tokens = [t for t in tokens if t.value == 'arr']
        assert len(arr_tokens) == 1
        assert arr_tokens[0].type == TokenType.IDENTIFIER

    def test_parse_nested_function_calls(self):
        """Test parsing nested function calls."""
        parser = PascalParser()
        parser.parse(None, 'Outer(Inner())')

        tokens = list(parser._tokens)
        func_tokens = [t for t in tokens if t.type == TokenType.FUNCTION_OR_METHOD]
        assert len(func_tokens) == 2

    def test_parse_line_comment(self):
        """Test that a line comment is not converted."""
        parser = PascalParser()
        parser.parse(None, '// comment')

        tokens = list(parser._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

    def test_parse_multiple_statements(self):
        """Test parsing multiple statements."""
        parser = PascalParser()
        parser.parse(None, 'Foo(); Bar();')

        tokens = list(parser._tokens)
        func_tokens = [t for t in tokens if t.type == TokenType.FUNCTION_OR_METHOD]
        assert len(func_tokens) == 2

    def test_parse_multiline_with_element_continuation(self):
        """Test multiline parsing with element continuation."""
        parser1 = PascalParser()
        state1 = parser1.parse(None, 'obj.')

        assert state1.in_element is True

        parser2 = PascalParser()
        state2 = parser2.parse(state1, 'field1.')

        assert state2.in_element is True

        parser3 = PascalParser()
        parser3.parse(state2, 'field2')

        tokens3 = list(parser3._tokens)
        field2_tokens = [t for t in tokens3 if t.value == 'field2']
        assert len(field2_tokens) == 1
        assert field2_tokens[0].type == TokenType.ELEMENT
