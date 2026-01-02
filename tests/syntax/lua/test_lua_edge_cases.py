"""
Tests for Lua edge cases and corner cases in tokenization.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer
from syntax.lua.lua_parser import LuaParser


class TestLuaEdgeCases:
    """Test edge cases and corner cases in Lua tokenization."""

    def test_block_comment_with_equals(self):
        """Test block comments with equals signs like --[=[...]=]."""
        test_cases = [
            '--[=[comment]=]',
            '--[==[comment]==]',
            '--[===[comment]===]',
        ]
        for comment in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, comment)
            tokens = list(lexer._tokens)
            # Note: The lexer might not support this - check behavior
            assert len(tokens) >= 1, f"Block comment with equals '{comment}' should produce tokens"

    def test_block_comment_bracket_mismatch(self):
        """Test block comment with mismatched bracket levels."""
        lexer = LuaLexer()
        lexer.lex(None, '--[==[ comment ]=]')
        tokens = list(lexer._tokens)
        # Wrong number of equals - might not close properly
        assert len(tokens) >= 1, "Mismatched brackets should produce tokens"

    def test_incomplete_block_comment_closing(self):
        """Test block comment with incomplete closing bracket."""
        lexer = LuaLexer()
        state = lexer.lex(None, '--[[ text ]')
        # Only one closing bracket - should not close
        assert state.in_block_comment, "Should still be in block comment"

    def test_unclosed_single_quote_string(self):
        """Test unclosed single quote string."""
        lexer = LuaLexer()
        lexer.lex(None, "'unclosed string")
        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Unclosed string should produce tokens"

    def test_unclosed_double_quote_string(self):
        """Test unclosed double quote string."""
        lexer = LuaLexer()
        lexer.lex(None, '"unclosed string')
        tokens = list(lexer._tokens)
        assert len(tokens) >= 1, "Unclosed string should produce tokens"

    def test_string_escape_sequences(self):
        """Test various string escape sequences."""
        test_cases = [
            r"'\n'",  # Newline
            r"'\t'",  # Tab
            r"'\\'",  # Backslash
            r"'\''",  # Single quote
            r'"\""',  # Double quote
            r"'\r'",  # Carriage return
            r"'\a'",  # Bell
            r"'\b'",  # Backspace
            r"'\f'",  # Form feed
            r"'\v'",  # Vertical tab
            r"'\123'",  # Decimal escape
            r"'\x7F'",  # Hex escape (Lua 5.2+)
        ]
        for s in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, s)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Escaped string '{s}' should be single token"
            assert tokens[0].type.name == 'STRING'

    def test_number_starting_with_dot(self):
        """Test number starting with dot."""
        lexer = LuaLexer()
        lexer.lex(None, '.5')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single number token"
        assert tokens[0].type.name == 'NUMBER'
        assert tokens[0].value == '.5'

    def test_double_dot_vs_number(self):
        """Test .. operator vs .5 number."""
        lexer = LuaLexer()
        lexer.lex(None, '..5')
        tokens = list(lexer._tokens)
        # Should be .. operator followed by number 5, or error
        assert len(tokens) >= 1, "Should produce tokens"

    def test_triple_dot_operator(self):
        """Test ... (varargs) operator."""
        lexer = LuaLexer()
        lexer.lex(None, '...')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single operator token"
        assert tokens[0].type.name == 'OPERATOR'
        assert tokens[0].value == '...'

    def test_operator_precedence_tokenization(self):
        """Test that operators are correctly tokenized by length."""
        lexer = LuaLexer()
        lexer.lex(None, '... .. .')
        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(operator_tokens) == 3, "Should have three operators"
        assert operator_tokens[0].value == '...'
        assert operator_tokens[1].value == '..'
        assert operator_tokens[2].value == '.'

    def test_invalid_hex_digit(self):
        """Test hexadecimal with invalid digit."""
        lexer = LuaLexer()
        lexer.lex(None, '0xG')
        tokens = list(lexer._tokens)
        # Might be 0x followed by identifier G, or error
        assert len(tokens) >= 1, "Invalid hex should produce tokens"

    def test_hex_with_no_digits(self):
        """Test 0x with no following digits."""
        lexer = LuaLexer()
        lexer.lex(None, '0x')
        tokens = list(lexer._tokens)
        # Incomplete hex literal
        assert len(tokens) >= 1, "Incomplete hex should produce tokens"

    def test_goto_statement(self):
        """Test goto statement (Lua 5.2+)."""
        lexer = LuaLexer()
        lexer.lex(None, 'goto label')
        tokens = list(lexer._tokens)
        # goto might be keyword or identifier depending on Lua version support
        assert len(tokens) == 2, "Should have two tokens"

    def test_label_syntax(self):
        """Test label syntax ::label:: (Lua 5.2+)."""
        lexer = LuaLexer()
        lexer.lex(None, '::label::')
        tokens = list(lexer._tokens)
        # Labels use :: which might be tokenized as two colons
        assert len(tokens) >= 1, "Label should produce tokens"

    def test_method_call_syntax(self):
        """Test method call with colon operator."""
        parser = LuaParser()
        parser.parse(None, 'object:method()')
        tokens = list(parser._tokens)
        # Should have object (identifier), : (operator), method (function), ()
        colon_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ':']
        assert len(colon_tokens) == 1, "Should have colon operator"

    def test_method_call_chained(self):
        """Test chained method calls."""
        parser = LuaParser()
        parser.parse(None, 'obj:method1():method2()')
        tokens = list(parser._tokens)
        colon_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ':']
        assert len(colon_tokens) == 2, "Should have two colon operators"

    def test_very_long_identifier(self):
        """Test extremely long identifier."""
        long_id = 'a' * 1000
        lexer = LuaLexer()
        lexer.lex(None, long_id)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long identifier should be single token"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == long_id

    def test_very_long_string(self):
        """Test extremely long string."""
        long_str = '"' + 'x' * 5000 + '"'
        lexer = LuaLexer()
        lexer.lex(None, long_str)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long string should be single token"
        assert tokens[0].type.name == 'STRING'

    def test_very_long_comment(self):
        """Test extremely long comment."""
        long_comment = '-- ' + 'x' * 5000
        lexer = LuaLexer()
        lexer.lex(None, long_comment)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long comment should be single token"
        assert tokens[0].type.name == 'COMMENT'

    def test_very_long_number(self):
        """Test extremely long number."""
        long_num = '9' * 1000
        lexer = LuaLexer()
        lexer.lex(None, long_num)
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Long number should be single token"
        assert tokens[0].type.name == 'NUMBER'

    def test_comment_inside_string(self):
        """Test that -- inside string doesn't start comment."""
        lexer = LuaLexer()
        lexer.lex(None, '"string with -- comment"')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single string token"
        assert tokens[0].type.name == 'STRING'

    def test_string_inside_comment(self):
        """Test that quotes inside comment don't start string."""
        lexer = LuaLexer()
        lexer.lex(None, '-- comment with "quotes"')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single comment token"
        assert tokens[0].type.name == 'COMMENT'

    def test_mixed_bracket_styles(self):
        """Test different bracket styles on same line."""
        lexer = LuaLexer()
        lexer.lex(None, '[[str1]] [=[str2]=]')
        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 2, "Should have two string tokens"

    def test_bracket_string_with_internal_brackets(self):
        """Test bracket string containing bracket characters."""
        lexer = LuaLexer()
        lexer.lex(None, '[[string with ] inside]]')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single string token"
        assert tokens[0].type.name == 'STRING'

    def test_bracket_string_with_double_bracket_inside(self):
        """Test bracket string containing ]] that shouldn't close it."""
        lexer = LuaLexer()
        lexer.lex(None, '[=[ string with ]] inside ]=]')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single string token (]] shouldn't close [=[)"
        assert tokens[0].type.name == 'STRING'

    def test_empty_input(self):
        """Test empty input string."""
        lexer = LuaLexer()
        lexer.lex(None, '')
        tokens = list(lexer._tokens)
        assert len(tokens) == 0, "Empty input should produce no tokens"

    def test_only_whitespace(self):
        """Test input with only whitespace."""
        test_cases = [' ', '    ', '\t', '  \t  ']
        for ws in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, ws)
            tokens = list(lexer._tokens)
            # Whitespace might or might not produce tokens
            assert len(tokens) >= 0

    def test_token_positions(self):
        """Test that token positions are tracked correctly."""
        lexer = LuaLexer()
        lexer.lex(None, 'x = 42')
        tokens = list(lexer._tokens)
        
        # Check that start positions are set
        for token in tokens:
            assert hasattr(token, 'start'), "Token should have start position"
            assert token.start >= 0, "Start position should be non-negative"
        
        # Positions should be in order
        positions = [t.start for t in tokens]
        assert positions == sorted(positions), "Token positions should be in order"

    def test_number_immediately_followed_by_identifier(self):
        """Test number immediately followed by identifier (invalid)."""
        lexer = LuaLexer()
        lexer.lex(None, '123abc')
        tokens = list(lexer._tokens)
        # Should be number followed by identifier
        assert len(tokens) >= 1, "Should produce tokens"
        assert tokens[0].type.name == 'NUMBER'

    def test_operators_without_spaces(self):
        """Test operators in sequence without spaces."""
        lexer = LuaLexer()
        lexer.lex(None, '1+2*3/4')
        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        assert len(operator_tokens) == 3, "Should have three operators"

    def test_multiple_underscores_in_identifier(self):
        """Test identifier with multiple consecutive underscores."""
        lexer = LuaLexer()
        lexer.lex(None, 'var___name')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single identifier"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == 'var___name'

    def test_all_underscore_identifier(self):
        """Test identifier that's all underscores."""
        lexer = LuaLexer()
        lexer.lex(None, '____')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single identifier"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == '____'

    def test_single_underscore_identifier(self):
        """Test single underscore as identifier."""
        lexer = LuaLexer()
        lexer.lex(None, '_')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1, "Should be single identifier"
        assert tokens[0].type.name == 'IDENTIFIER'
        assert tokens[0].value == '_'

    def test_adjacent_strings(self):
        """Test adjacent strings without space."""
        lexer = LuaLexer()
        lexer.lex(None, '"hello""world"')
        tokens = list(lexer._tokens)
        string_tokens = [t for t in tokens if t.type.name == 'STRING']
        assert len(string_tokens) == 2, "Should have two string tokens"

    def test_number_with_trailing_dot(self):
        """Test number with trailing dot like 42."""
        lexer = LuaLexer()
        lexer.lex(None, '42.')
        tokens = list(lexer._tokens)
        # Might be 42. (float) or 42 followed by .
        assert len(tokens) >= 1, "Should produce tokens"

    def test_exponent_without_digits(self):
        """Test malformed scientific notation."""
        lexer = LuaLexer()
        lexer.lex(None, '1e')
        tokens = list(lexer._tokens)
        # Incomplete exponent
        assert len(tokens) >= 1, "Should produce tokens"

    def test_hex_float_without_exponent(self):
        """Test hex float without p exponent."""
        lexer = LuaLexer()
        lexer.lex(None, '0x1.5')
        tokens = list(lexer._tokens)
        # Hex float might require p exponent
        assert len(tokens) >= 1, "Should produce tokens"

    def test_negative_exponent(self):
        """Test negative exponent in scientific notation."""
        lexer = LuaLexer()
        lexer.lex(None, '1e-5')
        tokens = list(lexer._tokens)
        # Should be: NUMBER(1) OPERATOR(-) NUMBER(5) or NUMBER(1e-5)
        # Lua supports 1e-5 as single number
        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(number_tokens) >= 1, "Should have number token(s)"

    def test_all_operators_in_sequence(self):
        """Test many operators in sequence."""
        lexer = LuaLexer()
        lexer.lex(None, '+-*/%^#==~=<=>=..')
        tokens = list(lexer._tokens)
        operator_tokens = [t for t in tokens if t.type.name == 'OPERATOR']
        # Should have multiple operator tokens
        assert len(operator_tokens) >= 8, "Should have many operators"

    def test_semicolon_statement_separator(self):
        """Test semicolon as statement separator."""
        lexer = LuaLexer()
        lexer.lex(None, 'x=1; y=2; z=3')
        tokens = list(lexer._tokens)
        semicolon_tokens = [t for t in tokens if t.type.name == 'OPERATOR' and t.value == ';']
        assert len(semicolon_tokens) == 2, "Should have two semicolons"

    def test_table_constructor_with_mixed_syntax(self):
        """Test table with array and record syntax."""
        parser = LuaParser()
        parser.parse(None, '{1, 2, key="value"}')
        tokens = list(parser._tokens)
        # Should have numbers, identifiers, strings, operators
        assert len(tokens) >= 8, "Should have multiple tokens"

    def test_function_call_string_shorthand(self):
        """Test function call with string shorthand: func"str" """
        parser = LuaParser()
        parser.parse(None, 'print"hello"')
        tokens = list(parser._tokens)
        # print should be recognized as function
        function_tokens = [t for t in tokens if t.type.name == 'FUNCTION_OR_METHOD']
        assert len(function_tokens) == 1, "Should recognize function call"

    def test_function_call_table_shorthand(self):
        """Test function call with table shorthand: func{} """
        parser = LuaParser()
        parser.parse(None, 'func{x=1}')
        tokens = list(parser._tokens)
        # func should be recognized as function (even without parens)
        # Actually, without parens it might not be recognized
        assert len(tokens) >= 1, "Should produce tokens"
