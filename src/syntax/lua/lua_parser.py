from dataclasses import dataclass

from syntax.lexer import TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.lua.lua_lexer import LuaLexer


@dataclass
class LuaParserState(ParserState):
    """
    State information for the Lua parser.

    Attributes:
        in_table_access: Indicates if we're currently in a table field access chain
    """
    in_table_access: bool = False


@ParserRegistry.register_parser(ProgrammingLanguage.LUA)
class LuaParser(Parser):
    """
    Parser for Lua code.

    This parser processes tokens from the Lua lexer and handles special cases
    like function calls, method calls, and table field access.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> LuaParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser converts identifier tokens to:
            - FUNCTION_OR_METHOD tokens when followed by '(' (function call)
            - FUNCTION_OR_METHOD tokens when followed by ':' (method call on object)
            - ELEMENT tokens when part of a dotted table access chain
        """
        in_table_access = False
        prev_lexer_state = None
        if prev_parser_state:
            assert isinstance(prev_parser_state, LuaParserState), \
                f"Expected LuaParserState, got {type(prev_parser_state).__name__}"
            in_table_access = prev_parser_state.in_table_access
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = LuaLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            # Skip non-identifier tokens
            if token.type != TokenType.IDENTIFIER:
                # Check for keywords that might start special constructs
                if token.type == TokenType.KEYWORD and token.value == 'function':
                    # Function keyword, next identifier might be function name
                    pass

                self._tokens.append(token)
                continue

            # Look at the next token to determine the context
            next_token = lexer.peek_next_token()
            cur_in_table_access = in_table_access
            next_in_table_access = False

            if next_token and next_token.type == TokenType.OPERATOR:
                # Function call: identifier followed by '('
                if next_token.value == '(':
                    in_table_access = False
                    token.type = TokenType.FUNCTION_OR_METHOD
                    self._tokens.append(token)
                    continue

                # Method call: identifier followed by ':'
                # This is for the object part: object:method()
                if next_token.value == ':':
                    in_table_access = False
                    self._tokens.append(token)
                    continue

                # Table field access: identifier followed by '.'
                if next_token.value == '.':
                    next_in_table_access = True

                # Function call with table literal: identifier followed by '{'
                # Lua allows: func{x=1} as shorthand for func({x=1})
                if next_token.value == '{':
                    in_table_access = False
                    token.type = TokenType.FUNCTION_OR_METHOD
                    self._tokens.append(token)
                    continue

            # Function call with string literal: identifier followed by string
            # Lua allows: func"str" as shorthand for func("str")
            if next_token and next_token.type == TokenType.STRING:
                in_table_access = False
                token.type = TokenType.FUNCTION_OR_METHOD
                self._tokens.append(token)
                continue

            # Table indexing: identifier followed by '['
            if next_token and next_token.type == TokenType.OPERATOR and next_token.value == '[':
                # This could be table indexing: table[key]
                in_table_access = False
                self._tokens.append(token)
                continue

            in_table_access = next_in_table_access

            # If we're in a table access chain, mark this as ELEMENT
            if cur_in_table_access:
                token.type = TokenType.ELEMENT
                self._tokens.append(token)
                continue

            self._tokens.append(token)

        parser_state = LuaParserState()
        parser_state.continuation_state = 0
        parser_state.parsing_continuation = False
        parser_state.lexer_state = lexer_state
        parser_state.in_table_access = in_table_access
        return parser_state
