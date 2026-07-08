from dataclasses import dataclass

from syntax.lexer import Token, TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.bash.bash_lexer import BashLexer


@dataclass
class BashParserState(ParserState):
    """
    State information for the Bash parser.

    Cross-line state (heredoc tracking) is carried entirely by the lexer's
    state, accessible via ``lexer_state``.  No additional parser-level state
    is needed.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.BASH)
class BashParser(Parser):
    """
    Parser for shell scripts (Bash, sh, Zsh, fish).

    Processes tokens from the Bash lexer and reclassifies bare identifiers
    that appear in command position (first non-keyword token after a
    ``;``, ``|``, ``&&``, ``||``, or at line start) as COMMAND tokens.
    """

    _SEPARATOR_OPS = {';', '|', '&&', '||', '\n'}

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> BashParserState:
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
            assert isinstance(prev_parser_state, BashParserState), \
                f"Expected BashParserState, got {type(prev_parser_state).__name__}"

            prev_lexer_state = prev_parser_state.lexer_state

        lexer = BashLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        raw_tokens: list = []
        while True:
            token = lexer.get_next_token()
            if token is None:
                break

            raw_tokens.append(token)

        for token in self._classify_commands(raw_tokens):
            self._tokens.append(token)

        parser_state = BashParserState()
        parser_state.lexer_state = lexer_state
        return parser_state

    def _classify_commands(self, tokens: list) -> list:
        """
        Reclassify IDENTIFIER tokens in command position as COMMAND.

        A token is in command position if it is the first non-whitespace
        token on the line, or if the previous non-whitespace token was a
        command separator (``;``, ``|``, ``&&``, ``||``).

        Args:
            tokens: The raw lexer tokens

        Returns:
            Tokens with command-position identifiers retyped to COMMAND
        """
        result: list[Token] = []
        in_command_position = True

        for token in tokens:
            if token.type == TokenType.IDENTIFIER and in_command_position:
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
                if token.type not in (TokenType.COMMENT,):
                    in_command_position = False

        return result
