from dataclasses import dataclass
from enum import Enum

from humbug.lib.syntax.json.json_lexer import JSONLexer
from humbug.lib.syntax.lexer import TokenType
from humbug.lib.syntax.parser import Parser, ParserState
from humbug.lib.syntax.parser_registry import ParserRegistry
from humbug.lib.syntax.programming_language import ProgrammingLanguage


class JSONContext(Enum):
    """Context within JSON structure."""
    ROOT = "root"
    OBJECT = "object"
    ARRAY = "array"


class JSONState(Enum):
    """State within JSON parsing."""
    EXPECTING_KEY = "expecting_key"
    EXPECTING_VALUE = "expecting_value"
    EXPECTING_KEY_OR_END = "expecting_key_or_end"
    EXPECTING_VALUE_OR_END = "expecting_value_or_end"


@dataclass
class JSONParserState(ParserState):
    """
    State information for the JSON parser.

    Attributes:
        context_stack: Stack of JSON contexts (object/array)
        current_state: Current parsing state
    """
    context_stack: list[JSONContext] | None = None
    current_state: JSONState = JSONState.EXPECTING_VALUE

    def __post_init__(self) -> None:
        """Initialize mutable default attributes."""
        if self.context_stack is None:
            self.context_stack = [JSONContext.ROOT]


@ParserRegistry.register_parser(ProgrammingLanguage.JSON)
class JSONParser(Parser):
    """
    Parser for JSON.

    This parser processes tokens from the JSON lexer and converts string tokens
    to JSON_KEY tokens when they appear in object key positions.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> JSONParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        # Initialize state
        context_stack = [JSONContext.ROOT]
        current_state = JSONState.EXPECTING_VALUE
        prev_lexer_state = None

        if prev_parser_state:
            assert isinstance(prev_parser_state, JSONParserState), \
                f"Expected JSONParserState, got {type(prev_parser_state).__name__}"
            assert isinstance(prev_parser_state.context_stack, list), \
                f"Expected context_stack to be a list, got {type(prev_parser_state.context_stack).__name__}"
            context_stack = prev_parser_state.context_stack.copy()
            current_state = prev_parser_state.current_state
            prev_lexer_state = prev_parser_state.lexer_state

        lexer = JSONLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            # Skip whitespace tokens for state tracking
            if token.type == TokenType.OPERATOR and token.value in (' ', '\t', '\n', '\r'):
                self._tokens.append(token)
                continue

            # Handle structural tokens
            if token.type == TokenType.OPERATOR:
                if token.value == '{':
                    context_stack.append(JSONContext.OBJECT)
                    current_state = JSONState.EXPECTING_KEY_OR_END
                    self._tokens.append(token)
                    continue

                if token.value == '}':
                    if context_stack and context_stack[-1] == JSONContext.OBJECT:
                        context_stack.pop()
                    current_state = self._get_state_after_value(context_stack)
                    self._tokens.append(token)
                    continue

                if token.value == '[':
                    context_stack.append(JSONContext.ARRAY)
                    current_state = JSONState.EXPECTING_VALUE_OR_END
                    self._tokens.append(token)
                    continue

                if token.value == ']':
                    if context_stack and context_stack[-1] == JSONContext.ARRAY:
                        context_stack.pop()
                    current_state = self._get_state_after_value(context_stack)
                    self._tokens.append(token)
                    continue

                if token.value == ':':
                    current_state = JSONState.EXPECTING_VALUE
                    self._tokens.append(token)
                    continue

                if token.value == ',':
                    if context_stack and context_stack[-1] == JSONContext.OBJECT:
                        current_state = JSONState.EXPECTING_KEY

                    elif context_stack and context_stack[-1] == JSONContext.ARRAY:
                        current_state = JSONState.EXPECTING_VALUE

                    else:
                        current_state = JSONState.EXPECTING_VALUE

                    self._tokens.append(token)
                    continue

            # Handle string tokens - convert to JSON_KEY if in key position
            if token.type == TokenType.STRING:
                if current_state in (JSONState.EXPECTING_KEY, JSONState.EXPECTING_KEY_OR_END):
                    token.type = TokenType.JSON_KEY
                    current_state = JSONState.EXPECTING_VALUE  # After key, expect colon then value

                else:
                    # It's a value, update state accordingly
                    current_state = self._get_state_after_value(context_stack)

                self._tokens.append(token)
                continue

            # Handle other value tokens (numbers, booleans, null)
            if token.type in (TokenType.NUMBER, TokenType.KEYWORD):
                current_state = self._get_state_after_value(context_stack)
                self._tokens.append(token)
                continue

            # Handle any other tokens
            self._tokens.append(token)

        # Create and return parser state
        parser_state = JSONParserState()
        parser_state.lexer_state = lexer_state
        parser_state.context_stack = context_stack
        parser_state.current_state = current_state
        return parser_state

    def _get_state_after_value(self, context_stack: list[JSONContext]) -> JSONState:
        """
        Determine the next expected state after processing a value.

        Args:
            context_stack: Current context stack

        Returns:
            The next expected parsing state
        """
        if not context_stack or context_stack[-1] == JSONContext.ROOT:
            return JSONState.EXPECTING_VALUE

        if context_stack[-1] == JSONContext.OBJECT:
            return JSONState.EXPECTING_KEY_OR_END

        if context_stack[-1] == JSONContext.ARRAY:
            return JSONState.EXPECTING_VALUE_OR_END

        return JSONState.EXPECTING_VALUE
