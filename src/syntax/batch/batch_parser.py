from dataclasses import dataclass

from syntax.lexer import Token, TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.batch.batch_lexer import BatchLexer


@dataclass
class BatchParserState(ParserState):
    """
    State information for the Batch parser.

    Batch has no multi-line constructs, so no cross-line state is needed
    beyond what the base ``ParserState`` provides.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.BATCH)
class BatchParser(Parser):
    """
    Parser for Windows Batch scripts (.bat, .cmd).

    Processes tokens from the Batch lexer and performs two reclassification
    passes:

    1. ``REM`` in command position consumes the rest of the line as a comment.
    2. Bare IDENTIFIER tokens in command position (first token, after ``@``,
       or after a separator ``&`` / ``&&`` / ``||``) are retyped to COMMAND.
    """

    _SEPARATOR_OPS = {'&', '&&', '||'}

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> BatchParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse (one line)

        Returns:
            The updated parser state after parsing
        """
        self._tokens = []
        self._next_token = 0

        prev_lexer_state = None
        if prev_parser_state is not None:
            assert isinstance(prev_parser_state, BatchParserState), \
                f"Expected BatchParserState, got {type(prev_parser_state).__name__}"

            prev_lexer_state = prev_parser_state.lexer_state

        lexer = BatchLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        raw_tokens: list[Token] = []
        while True:
            token = lexer.get_next_token()
            if token is None:
                break

            raw_tokens.append(token)

        rem_handled = self._handle_rem(raw_tokens)
        for token in self._classify_commands(rem_handled):
            self._tokens.append(token)

        parser_state = BatchParserState()
        parser_state.lexer_state = lexer_state
        return parser_state

    def _handle_rem(self, tokens: list[Token]) -> list[Token]:
        """
        Collapse ``REM`` in command position into a single COMMENT token.

        When REM appears as the first token on a line (or after ``@`` or a
        separator), all remaining tokens on that line are merged into a
        COMMENT token whose value is reconstructed from the original input
        span.

        Args:
            tokens: The raw lexer tokens

        Returns:
            Tokens with REM comments collapsed
        """
        if not tokens:
            return tokens

        for i, token in enumerate(tokens):
            is_command_pos = False

            if i == 0:
                is_command_pos = True

            elif i >= 1:
                prev = tokens[i - 1]
                if prev.type == TokenType.OPERATOR and (
                        prev.value == '@' or prev.value in self._SEPARATOR_OPS):
                    is_command_pos = True

            if (is_command_pos and
                    token.type == TokenType.IDENTIFIER and
                    token.value.lower() == 'rem' and
                    i + 1 < len(tokens)):
                comment_text = ' '.join(t.value for t in tokens[i:])
                return tokens[:i] + [Token(
                    type=TokenType.COMMENT,
                    value=comment_text,
                    start=token.start
                )]

        return tokens

    def _classify_commands(self, tokens: list[Token]) -> list[Token]:
        """
        Reclassify IDENTIFIER tokens in command position as COMMAND.

        A token is in command position if it is the first token on the line,
        or if the previous non-whitespace token was a command separator
        (``&``, ``&&``, ``||``).  The ``@`` operator also puts the following
        token in command position.

        Args:
            tokens: The raw lexer tokens

        Returns:
            Tokens with command-position identifiers retyped to COMMAND
        """
        result: list[Token] = []
        in_command_position = True

        for token in tokens:
            if token.type == TokenType.OPERATOR and token.value == '@':
                result.append(token)
                in_command_position = True

            elif token.type == TokenType.IDENTIFIER and in_command_position:
                result.append(Token(
                    type=TokenType.COMMAND,
                    value=token.value,
                    start=token.start
                ))
                in_command_position = False

            elif token.type == TokenType.OPERATOR and token.value in self._SEPARATOR_OPS:
                result.append(token)
                in_command_position = True

            else:
                result.append(token)
                if token.type != TokenType.COMMENT:
                    in_command_position = False

        return result
