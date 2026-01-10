"""
Tests for C parser.
"""
import pytest

from syntax.c.c_parser import CParser, CParserState
from syntax.lexer import TokenType
from syntax.programming_language import ProgrammingLanguage


class TestCParser:
    """Test C parser."""

    def test_parse_simple_expression(self):
        """Test parsing a simple expression."""
        parser = CParser()
        state = parser.parse(None, 'int x = 42;')

        assert isinstance(state, CParserState)
        assert state.parsing_continuation is False
        assert state.continuation_state == 0
        assert state.in_element is False

    def test_parse_function_call(self):
        """Test parsing function call converts identifier to FUNCTION_OR_METHOD."""
        parser = CParser()
        state = parser.parse(None, 'printf("hello");')

        tokens = list(parser._tokens)
        # Find the printf token
        printf_tokens = [t for t in tokens if t.value == 'printf']
        assert len(printf_tokens) == 1
        assert printf_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_parse_method_call(self):
        """Test parsing method call."""
        parser = CParser()
        state = parser.parse(None, 'obj.method();')

        tokens = list(parser._tokens)
        method_tokens = [t for t in tokens if t.value == 'method']
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_parse_element_access_dot(self):
        """Test parsing element access with dot."""
        parser = CParser()
        state = parser.parse(None, 'obj.field')

        tokens = list(parser._tokens)
        field_tokens = [t for t in tokens if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_parse_element_access_arrow(self):
        """Test parsing element access with arrow."""
        parser = CParser()
        state = parser.parse(None, 'ptr->field')

        tokens = list(parser._tokens)
        field_tokens = [t for t in tokens if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_parse_chained_element_access(self):
        """Test parsing chained element access."""
        parser = CParser()
        state = parser.parse(None, 'obj.field1.field2')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type == TokenType.ELEMENT]
        assert len(element_tokens) == 2
        assert element_tokens[0].value == 'field1'
        assert element_tokens[1].value == 'field2'

    def test_parse_element_with_method_call(self):
        """Test parsing element access followed by method call."""
        parser = CParser()
        state = parser.parse(None, 'obj.method()')

        tokens = list(parser._tokens)
        method_tokens = [t for t in tokens if t.value == 'method']
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_parse_multiline_block_comment_start(self):
        """Test parsing start of multiline block comment."""
        parser = CParser()
        state = parser.parse(None, '/* comment')

        assert isinstance(state, CParserState)
        assert state.parsing_continuation is True
        assert state.continuation_state == 1
        assert state.lexer_state.in_block_comment is True

    def test_parse_multiline_block_comment_continuation(self):
        """Test parsing continuation of multiline block comment."""
        parser1 = CParser()
        state1 = parser1.parse(None, '/* start')

        parser2 = CParser()
        state2 = parser2.parse(state1, 'middle')

        assert state2.parsing_continuation is True
        assert state2.lexer_state.in_block_comment is True

    def test_parse_multiline_block_comment_end(self):
        """Test parsing end of multiline block comment."""
        parser1 = CParser()
        state1 = parser1.parse(None, '/* start')

        parser2 = CParser()
        state2 = parser2.parse(state1, 'end */')

        assert state2.parsing_continuation is False
        assert state2.continuation_state == 0
        assert state2.lexer_state.in_block_comment is False

    def test_parse_empty_input(self):
        """Test parsing empty input."""
        parser = CParser()
        state = parser.parse(None, '')

        assert isinstance(state, CParserState)
        assert state.parsing_continuation is False
        assert state.in_element is False

    def test_parse_identifier_not_function(self):
        """Test parsing identifier that's not a function."""
        parser = CParser()
        state = parser.parse(None, 'int x;')

        tokens = list(parser._tokens)
        x_tokens = [t for t in tokens if t.value == 'x']
        assert len(x_tokens) == 1
        assert x_tokens[0].type == TokenType.IDENTIFIER

    def test_parse_function_with_args(self):
        """Test parsing function call with arguments."""
        parser = CParser()
        state = parser.parse(None, 'foo(x, y)')

        tokens = list(parser._tokens)
        foo_tokens = [t for t in tokens if t.value == 'foo']
        assert len(foo_tokens) == 1
        assert foo_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_parse_nested_function_calls(self):
        """Test parsing nested function calls."""
        parser = CParser()
        state = parser.parse(None, 'outer(inner())')

        tokens = list(parser._tokens)
        func_tokens = [t for t in tokens if t.type == TokenType.FUNCTION_OR_METHOD]
        assert len(func_tokens) == 2

    def test_parse_struct_member_access(self):
        """Test parsing struct member access."""
        parser = CParser()
        state = parser.parse(None, 'struct_var.member')

        tokens = list(parser._tokens)
        member_tokens = [t for t in tokens if t.value == 'member']
        assert len(member_tokens) == 1
        assert member_tokens[0].type == TokenType.ELEMENT

    def test_parse_pointer_member_access(self):
        """Test parsing pointer member access."""
        parser = CParser()
        state = parser.parse(None, 'ptr->member')

        tokens = list(parser._tokens)
        member_tokens = [t for t in tokens if t.value == 'member']
        assert len(member_tokens) == 1
        assert member_tokens[0].type == TokenType.ELEMENT

    def test_parse_mixed_access_and_calls(self):
        """Test parsing mixed element access and function calls."""
        parser = CParser()
        state = parser.parse(None, 'obj.method().field')

        tokens = list(parser._tokens)
        method_tokens = [t for t in tokens if t.value == 'method']
        field_tokens = [t for t in tokens if t.value == 'field']
        
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_parse_array_access_not_function(self):
        """Test that array access doesn't mark identifier as function."""
        parser = CParser()
        state = parser.parse(None, 'arr[0]')

        tokens = list(parser._tokens)
        arr_tokens = [t for t in tokens if t.value == 'arr']
        assert len(arr_tokens) == 1
        # Should remain IDENTIFIER, not FUNCTION_OR_METHOD
        assert arr_tokens[0].type == TokenType.IDENTIFIER

    def test_parser_state_type_assertion(self):
        """Test that parser asserts correct state type."""
        parser1 = CParser()
        state1 = parser1.parse(None, 'int x;')

        # Create a parser with the correct state type
        parser2 = CParser()
        state2 = parser2.parse(state1, 'int y;')

        assert isinstance(state2, CParserState)

    def test_parse_preserves_element_state(self):
        """Test that parser preserves in_element state across lines."""
        parser1 = CParser()
        state1 = parser1.parse(None, 'obj.')

        # State should indicate we're in element context
        assert state1.in_element is True

        parser2 = CParser()
        state2 = parser2.parse(state1, 'field')

        tokens2 = list(parser2._tokens)
        field_tokens = [t for t in tokens2 if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_parse_arrow_followed_by_element(self):
        """Test arrow operator followed by element."""
        parser1 = CParser()
        state1 = parser1.parse(None, 'ptr->')

        assert state1.in_element is True

        parser2 = CParser()
        state2 = parser2.parse(state1, 'member')

        tokens2 = list(parser2._tokens)
        member_tokens = [t for t in tokens2 if t.value == 'member']
        assert len(member_tokens) == 1
        assert member_tokens[0].type == TokenType.ELEMENT

    def test_parse_function_call_resets_element_state(self):
        """Test that function call resets element state."""
        parser = CParser()
        state = parser.parse(None, 'obj.func()')

        # After function call, should not be in element context
        assert state.in_element is False

    def test_parse_complex_expression(self):
        """Test parsing complex expression."""
        parser = CParser()
        state = parser.parse(None, 'obj->field.method(arg).result')

        tokens = list(parser._tokens)
        
        # field should be ELEMENT
        field_tokens = [t for t in tokens if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT
        
        # method should be FUNCTION_OR_METHOD
        method_tokens = [t for t in tokens if t.value == 'method']
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD
        
        # result should be ELEMENT
        result_tokens = [t for t in tokens if t.value == 'result']
        assert len(result_tokens) == 1
        assert result_tokens[0].type == TokenType.ELEMENT

    def test_parse_keywords_not_converted(self):
        """Test that keywords are not converted to functions."""
        parser = CParser()
        state = parser.parse(None, 'if (condition)')

        tokens = list(parser._tokens)
        # 'if' should remain a KEYWORD, not become FUNCTION_OR_METHOD
        if_tokens = [t for t in tokens if t.value == 'if']
        assert len(if_tokens) == 1
        assert if_tokens[0].type == TokenType.KEYWORD

    def test_parse_numbers_not_converted(self):
        """Test that numbers are not affected by parser."""
        parser = CParser()
        state = parser.parse(None, '42 + 3.14')

        tokens = list(parser._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 2

    def test_parse_strings_not_converted(self):
        """Test that strings are not affected by parser."""
        parser = CParser()
        state = parser.parse(None, '"hello"')

        tokens = list(parser._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1

    def test_parse_operators_not_converted(self):
        """Test that operators are not affected by parser."""
        parser = CParser()
        state = parser.parse(None, '+ - * /')

        tokens = list(parser._tokens)
        op_tokens = [t for t in tokens if t.type == TokenType.OPERATOR]
        assert len(op_tokens) == 4

    def test_parse_comments_not_converted(self):
        """Test that comments are not affected by parser."""
        parser = CParser()
        state = parser.parse(None, '// comment')

        tokens = list(parser._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1

    def test_parse_preprocessor_not_converted(self):
        """Test that preprocessor directives are not affected by parser."""
        parser = CParser()
        state = parser.parse(None, '#include <stdio.h>')

        tokens = list(parser._tokens)
        directive_tokens = [t for t in tokens if t.type == TokenType.PREPROCESSOR]
        assert len(directive_tokens) == 1

    def test_parse_multiple_statements(self):
        """Test parsing multiple statements."""
        parser = CParser()
        state = parser.parse(None, 'foo(); bar();')

        tokens = list(parser._tokens)
        func_tokens = [t for t in tokens if t.type == TokenType.FUNCTION_OR_METHOD]
        assert len(func_tokens) == 2

    def test_parse_function_pointer_call(self):
        """Test parsing function pointer call."""
        parser = CParser()
        state = parser.parse(None, '(*func_ptr)()')

        tokens = list(parser._tokens)
        # func_ptr should remain IDENTIFIER since it's not directly followed by (
        func_ptr_tokens = [t for t in tokens if t.value == 'func_ptr']
        assert len(func_ptr_tokens) == 1
        assert func_ptr_tokens[0].type == TokenType.IDENTIFIER

    def test_parse_cast_expression(self):
        """Test parsing cast expression."""
        parser = CParser()
        state = parser.parse(None, '(int)x')

        tokens = list(parser._tokens)
        # int should be KEYWORD
        int_tokens = [t for t in tokens if t.value == 'int']
        assert len(int_tokens) == 1
        assert int_tokens[0].type == TokenType.KEYWORD

    def test_parse_sizeof_operator(self):
        """Test parsing sizeof operator."""
        parser = CParser()
        state = parser.parse(None, 'sizeof(int)')

        tokens = list(parser._tokens)
        # sizeof is a KEYWORD, not a function
        sizeof_tokens = [t for t in tokens if t.value == 'sizeof']
        assert len(sizeof_tokens) == 1
        assert sizeof_tokens[0].type == TokenType.KEYWORD

    def test_parse_typedef_struct(self):
        """Test parsing typedef struct."""
        parser = CParser()
        state = parser.parse(None, 'typedef struct { int x; } MyStruct;')

        tokens = list(parser._tokens)
        # Should have various token types
        assert len(tokens) > 0

    def test_parse_multiline_with_element_continuation(self):
        """Test multiline parsing with element continuation."""
        parser1 = CParser()
        state1 = parser1.parse(None, 'obj.')
        
        assert state1.in_element is True
        
        parser2 = CParser()
        state2 = parser2.parse(state1, 'field1.')
        
        # After field1., should still be in element context
        assert state2.in_element is True
        
        parser3 = CParser()
        state3 = parser3.parse(state2, 'field2')
        
        # field2 should be marked as ELEMENT
        tokens3 = list(parser3._tokens)
        field2_tokens = [t for t in tokens3 if t.value == 'field2']
        assert len(field2_tokens) == 1
        assert field2_tokens[0].type == TokenType.ELEMENT
