from dataclasses import dataclass

from syntax.lexer import LexerState, Token, TokenType
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage
from syntax.yaml.yaml_lexer import YAMLLexer


@dataclass
class YAMLParserState(ParserState):
    """
    State information for the YAML parser.

    Attributes:
        in_block_scalar: Whether we are currently consuming block scalar
            content lines.
        block_scalar_indent: The indentation level of block scalar content.
            A value of -1 means the indent has not yet been determined (we're
            on the first line after the indicator and need to measure the
            content's indentation).
    """
    in_block_scalar: bool = False
    block_scalar_indent: int = -1


@ParserRegistry.register_parser(ProgrammingLanguage.YAML)
class YAMLParser(Parser):
    """
    Parser for YAML.

    This parser processes tokens from the YAML lexer and adds context-dependent
    retyping:

    - Bareword/STRING tokens followed by ``:`` are retyped to JSON_KEY.
    - ``-`` at the start of a content line (followed by whitespace or EOL) is
      retyped to LIST_MARKER.
    - Block scalar lines (``|`` or ``>``) consume subsequent lines as STRING
      content until indentation drops below the scalar's indent level.
    - ``true``/``false``/``null`` values are tagged as KEYWORD by the lexer
      and left as-is.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> YAMLParserState:
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

        in_block_scalar = False
        block_scalar_indent = -1
        prev_lexer_state = None

        if prev_parser_state is not None:
            assert isinstance(prev_parser_state, YAMLParserState), \
                f"Expected YAMLParserState, got {type(prev_parser_state).__name__}"

            in_block_scalar = prev_parser_state.in_block_scalar
            block_scalar_indent = prev_parser_state.block_scalar_indent
            prev_lexer_state = prev_parser_state.lexer_state

        if in_block_scalar:
            return self._parse_block_scalar_line(
                input_str, block_scalar_indent, prev_lexer_state
            )

        return self._parse_normal_line(input_str, prev_lexer_state)

    def _parse_block_scalar_line(
        self,
        input_str: str,
        block_scalar_indent: int,
        prev_lexer_state: LexerState | None
    ) -> YAMLParserState:
        """
        Parse a line that may be block scalar content.

        If the indentation is deep enough (or the line is blank), the entire
        line is emitted as STRING.  Otherwise we've exited the block scalar
        and fall through to normal parsing.

        Args:
            input_str: The input line
            block_scalar_indent: The minimum indentation for block scalar content,
                or -1 if not yet determined
            prev_lexer_state: Previous lexer state

        Returns:
            The updated parser state
        """
        stripped = input_str.lstrip(' ')
        indent = len(input_str) - len(stripped)

        if block_scalar_indent == -1:
            if stripped == '':
                self._tokens.append(Token(
                    type=TokenType.STRING,
                    value=input_str,
                    start=0
                ))
                return self._make_state(True, -1, prev_lexer_state)

            block_scalar_indent = indent

        if stripped == '':
            self._tokens.append(Token(
                type=TokenType.STRING,
                value=input_str,
                start=0
            ))
            return self._make_state(True, block_scalar_indent, prev_lexer_state)

        if indent >= block_scalar_indent:
            self._tokens.append(Token(
                type=TokenType.STRING,
                value=input_str,
                start=0
            ))
            return self._make_state(True, block_scalar_indent, prev_lexer_state)

        return self._parse_normal_line(input_str, prev_lexer_state)

    def _parse_normal_line(
        self,
        input_str: str,
        prev_lexer_state: LexerState | None
    ) -> YAMLParserState:
        """
        Parse a line in normal (non-block-scalar) mode.

        Args:
            input_str: The input line
            prev_lexer_state: Previous lexer state

        Returns:
            The updated parser state
        """
        lexer = YAMLLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)

        raw_tokens: list[Token] = []
        while True:
            token = lexer.get_next_token()
            if token is None:
                break

            raw_tokens.append(token)

        processed = self._classify_list_markers(raw_tokens)
        processed = self._classify_keys(processed)

        in_block_scalar = False
        for token in processed:
            self._tokens.append(token)

            if (token.type == TokenType.STRING and
                    token.value.lstrip(' ') in ('|', '>', '|-', '|+', '>-', '>+')):
                in_block_scalar = True

        return self._make_state(in_block_scalar, -1, lexer_state)

    def _make_state(
        self,
        in_block_scalar: bool,
        block_scalar_indent: int,
        lexer_state: LexerState | None
    ) -> YAMLParserState:
        """
        Create a YAMLParserState from individual fields.

        Args:
            in_block_scalar: Whether we are in a block scalar
            block_scalar_indent: The block scalar indentation level
            lexer_state: The lexer state

        Returns:
            The new parser state
        """
        parser_state = YAMLParserState()
        parser_state.in_block_scalar = in_block_scalar
        parser_state.block_scalar_indent = block_scalar_indent
        parser_state.lexer_state = lexer_state
        return parser_state

    def _classify_list_markers(self, tokens: list[Token]) -> list[Token]:
        """
        Retype leading ``-`` operators at content boundaries to LIST_MARKER.

        A ``-`` OPERATOR token that is the first content token on the line
        and is either at the end of the token stream or immediately followed
        by another token (which in the source text is preceded by whitespace)
        is a YAML block sequence marker.

        Since whitespace is not emitted as tokens, a ``-`` followed by another
        token means there was a space between them in the source.

        Args:
            tokens: The raw lexer tokens

        Returns:
            Tokens with list markers retyped
        """
        if not tokens:
            return tokens

        result: list[Token] = []

        for i, token in enumerate(tokens):
            if (token.type == TokenType.OPERATOR and token.value == '-' and i == 0):
                if i + 1 < len(tokens):
                    result.append(Token(
                        type=TokenType.LIST_MARKER,
                        value='- ',
                        start=token.start
                    ))
                    continue

                result.append(Token(
                    type=TokenType.LIST_MARKER,
                    value='-',
                    start=token.start
                ))
                continue

            result.append(token)

        return result

    def _classify_keys(self, tokens: list[Token]) -> list[Token]:
        """
        Retype TEXT and STRING tokens that are immediately followed by a ``:``
        operator to JSON_KEY.

        A YAML mapping key is a TEXT, STRING, or KEYWORD token that is followed
        immediately by a ``:`` OPERATOR, where the ``:`` is either the last
        token on the line or is followed by another token (which in the source
        text is preceded by whitespace).  Since the lexer does not emit
        whitespace tokens, adjacency in the token list means the two tokens
        were adjacent in the source.

        Args:
            tokens: The token list (after list marker classification)

        Returns:
            Tokens with keys retyped to JSON_KEY
        """
        result: list[Token] = []

        for i, token in enumerate(tokens):
            if token.type in (TokenType.TEXT, TokenType.STRING, TokenType.KEYWORD):
                if (i + 1 < len(tokens) and
                        tokens[i + 1].type == TokenType.OPERATOR and
                        tokens[i + 1].value == ':'):
                    result.append(Token(
                        type=TokenType.JSON_KEY,
                        value=token.value,
                        start=token.start
                    ))
                    continue

            result.append(token)

        return result
