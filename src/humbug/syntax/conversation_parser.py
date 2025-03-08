from dataclasses import dataclass
from typing import Optional

from humbug.syntax.lexer import Token, TokenType
from humbug.syntax.conversation_lexer import ConversationLexer
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.programming_language_utils import ProgrammingLanguageUtils


@dataclass
class ConversationParserState(ParserState):
    """
    State information for the Conversation parser.

    Attributes:
        in_fence_block: Indicates if we're currently in a code fence block
        fence_depth: Indentation of the current fence block (if we are in one)
        language: The current programming language being parsed
        embedded_parser_state: State of the embedded language parser
    """
    in_fence_block: bool = False
    fence_depth: int = 0
    language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN
    embedded_parser_state: ParserState = None


class ConversationParser(Parser):
    """
    Parser for conversation content with embedded code blocks.

    This parser processes conversation content and delegates embedded code blocks to
    appropriate language-specific parsers.
    """

    def _embedded_parse(
            self,
            language: ProgrammingLanguage,
            prev_embedded_parser_state: ParserState,
            input_str: str
    ) -> ParserState:
        """
        Parse embedded code content using an appropriate language parser.

        Args:
            language: The programming language to use for parsing
            prev_embedded_parser_state: Previous parser state if any
            input_str: The input string to parse

        Returns:
            Updated parser state after parsing

        Note:
            Uses ParserFactory to instantiate appropriate parser for the language.
            Returns None if no parser is available for the language.
        """
        embedded_parser = ParserRegistry.create_parser(language)
        if not embedded_parser:
            return None

        # We apply a per-parser offset to any continuation value in case we switched language!
        continuation_offset = int(language) * 0x1000
        embedded_parser_state = embedded_parser.parse(prev_embedded_parser_state, input_str)
        embedded_parser_state.continuation_state += continuation_offset

        while True:
            token = embedded_parser.get_next_token()
            if token is None:
                break

            self._tokens.append(token)

        return embedded_parser_state

    def parse(self, prev_parser_state: Optional[ConversationParserState], input_str: str) -> ConversationParserState:
        """
        Parse conversation content including embedded code blocks.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            Handles transitions between regular conversation content and code fence blocks,
            delegating code blocks to appropriate language parsers.
        """
        in_fence_block = False
        fence_depth = 0
        language = ProgrammingLanguage.UNKNOWN
        embedded_parser_state = None
        parsing_continuation = False
        if prev_parser_state:
            in_fence_block = prev_parser_state.in_fence_block
            fence_depth = prev_parser_state.fence_depth
            language = prev_parser_state.language
            embedded_parser_state = prev_parser_state.embedded_parser_state
            parsing_continuation = prev_parser_state.parsing_continuation

        parse_embedded = language != ProgrammingLanguage.UNKNOWN

        if not parsing_continuation:
            lexer = ConversationLexer()
            lexer.lex(None, input_str)

            seen_text = False

            while True:
                lex_token = lexer.get_next_token()
                if not lex_token:
                    break

                # If we've already processed something interesting on this line then run to the
                # end of the line.
                if seen_text:
                    self._tokens.append(lex_token)
                    continue

                if lex_token.type == TokenType.WHITESPACE:
                    peek_token = lexer.peek_next_token()
                    if (peek_token is None or peek_token.type != TokenType.FENCE) and parse_embedded:
                        break

                    self._tokens.append(lex_token)
                    continue

                seen_text = True

                if lex_token.type == TokenType.FENCE:
                    if in_fence_block:
                        self._tokens.append(Token(type=TokenType.FENCE_END, value='```', start=lex_token.start))
                        in_fence_block = False
                        fence_depth = 0
                        language = ProgrammingLanguage.UNKNOWN
                        embedded_parser_state = None
                        parse_embedded = False
                        continue

                    in_fence_block = True
                    fence_depth = lex_token.start
                    embedded_parser_state = None
                    self._tokens.append(Token(type=TokenType.FENCE_START, value='```', start=lex_token.start))

                    next_token = lexer.peek_next_token([TokenType.WHITESPACE])
                    if next_token and (next_token.type == TokenType.TEXT):
                        next_token = lexer.get_next_token([TokenType.WHITESPACE])
                        self._tokens.append(Token(type=TokenType.LANGUAGE, value=next_token.value, start=next_token.start))
                        self._tokens.append(Token(type=TokenType.NEWLINE, value='\n', start=(next_token.start + len(next_token.value))))
                        language = ProgrammingLanguageUtils.from_name(next_token.value)
                        break

                    self._tokens.append(Token(type=TokenType.NEWLINE, value='\n', start=(lex_token.start + 3)))
                    language = ProgrammingLanguage.TEXT
                    break

                if parse_embedded:
                    break

                self._tokens.append(lex_token)

        parser_state = ConversationParserState()
        parser_state.in_fence_block = in_fence_block
        parser_state.fence_depth = fence_depth
        parser_state.language = language
        if parse_embedded:
            if input_str.startswith(' ' * fence_depth):
                input_str = input_str[fence_depth:]
            else:
                if input_str.strip():
                    input_str = ""
                else:
                    self._tokens.append(Token(type=TokenType.ERROR, value='[Invalid indent]', start=0))

            new_embedded_parser_state = self._embedded_parse(parser_state.language, embedded_parser_state, input_str)
            parser_state.embedded_parser_state = new_embedded_parser_state
            if new_embedded_parser_state:
                parser_state.continuation_state = new_embedded_parser_state.continuation_state
                parser_state.parsing_continuation = new_embedded_parser_state.parsing_continuation

        return parser_state
