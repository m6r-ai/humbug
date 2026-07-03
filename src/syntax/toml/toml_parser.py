from dataclasses import dataclass

from syntax.lexer import Token, TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.toml.toml_lexer import TOMLLexer


@dataclass
class TOMLParserState(ParserState):
    """
    State information for the TOML parser.

    The TOML parser's only cross-line state is carried by the lexer's
    multi-line string tracking (accessible via ``lexer_state``).  No
    additional parser-level state is needed.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.TOML)
class TOMLParser(Parser):
    """
    Parser for TOML.

    This parser processes tokens from the TOML lexer and classifies bare
    keys: a TEXT token immediately followed by ``=`` is retyped to JSON_KEY.
    For dotted keys (``a.b.c = value``), each TEXT segment in the key path
    is also retyped to JSON_KEY.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> TOMLParserState:
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
            assert isinstance(prev_parser_state, TOMLParserState), \
                f"Expected TOMLParserState, got {type(prev_parser_state).__name__}"

            prev_lexer_state = prev_parser_state.lexer_state

        lexer = TOMLLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        raw_tokens: list[Token] = []
        while True:
            token = lexer.get_next_token()
            if token is None:
                break

            raw_tokens.append(token)

        for token in self._classify_keys(raw_tokens):
            self._tokens.append(token)

        parser_state = TOMLParserState()
        parser_state.lexer_state = lexer_state
        return parser_state

    def _classify_keys(self, tokens: list[Token]) -> list[Token]:
        """
        Retype TEXT tokens that are part of a key to JSON_KEY.

        A TEXT token is a key segment if it is immediately followed by ``=``
        (``key = value``) or by a ``.`` operator that is part of a dotted key
        path leading to ``=`` (``a.b.c = value``).

        Quoted STRING tokens followed by ``=`` or ``.`` are also retyped, to
        handle quoted keys like ``"my key" = value``.

        Since the lexer does not emit whitespace tokens, adjacency in the
        token list means the tokens were adjacent in the source.

        Args:
            tokens: The raw lexer tokens

        Returns:
            Tokens with keys retyped to JSON_KEY
        """
        key_types = {TokenType.TEXT, TokenType.STRING}
        result: list[Token] = []

        for i, token in enumerate(tokens):
            if token.type in key_types and i + 1 < len(tokens):
                nxt = tokens[i + 1]
                if nxt.type == TokenType.OPERATOR and nxt.value == '=':
                    result.append(Token(
                        type=TokenType.JSON_KEY,
                        value=token.value,
                        start=token.start
                    ))
                    continue

                if nxt.type == TokenType.OPERATOR and nxt.value == '.':
                    result.append(Token(
                        type=TokenType.JSON_KEY,
                        value=token.value,
                        start=token.start
                    ))
                    continue

            result.append(token)

        return result
