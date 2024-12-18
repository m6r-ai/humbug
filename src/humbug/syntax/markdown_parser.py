from dataclasses import dataclass
from enum import IntEnum

from humbug.syntax.markdown_lexer import MarkdownLexer
from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.python_parser import PythonParser
from humbug.syntax.text_parser import TextParser


class ProgrammingLanguage(IntEnum):
    UNKNOWN = -1
    PYTHON = 0
    TEXT = 1


# Mapping from lowercase language names to enum members
language_mapping = {
    "c": ProgrammingLanguage.TEXT,
    "c++": ProgrammingLanguage.TEXT,
    "css": ProgrammingLanguage.TEXT,
    "html": ProgrammingLanguage.TEXT,
    "javascript": ProgrammingLanguage.TEXT,
    "metaphor": ProgrammingLanguage.TEXT,
    "python": ProgrammingLanguage.PYTHON,
    "typescript": ProgrammingLanguage.TEXT,
}


@dataclass
class MarkdownParserState(ParserState):
    in_code_block: bool
    language: ProgrammingLanguage
    embedded_parser_state: ParserState


class MarkdownParser(Parser):
    def __init__(self) -> MarkdownParserState:
        super().__init__()

        self._parser_state = MarkdownParserState(
            in_code_block=False,
            language=ProgrammingLanguage.UNKNOWN,
            embedded_parser_state=None
        )

    def _embedded_parse(self, input_str: str) -> None:
        embedded_parser = None
        match self._parser_state.language:
            case ProgrammingLanguage.PYTHON:
                embedded_parser = PythonParser()

            case ProgrammingLanguage.TEXT:
                embedded_parser = TextParser()

        print(f"embedded parse {input_str}, {embedded_parser}")

        embedded_parser_state = embedded_parser.parse(self._parser_state.embedded_parser_state, input_str)
        while True:
            token = embedded_parser.get_next_token()
            if token is None:
                break

            print(f"token: {token}")
            self._tokens.append(Token(type=token.type, value=token.value, start=token.start))

        self._parser_state.embedded_parser_state = embedded_parser_state

    def parse(self, parser_state: MarkdownParserState, input_str: str) -> None:
        if not parser_state:
            self._parser_state.in_code_block = False
            self._parser_state.language = ProgrammingLanguage.UNKNOWN
            self._parser_state.embedded_parser_state = None
        else:
            self._parser_state.in_code_block = parser_state.in_code_block
            self._parser_state.language = parser_state.language
            self._parser_state.embedded_parser_state = parser_state.embedded_parser_state

        lexer = MarkdownLexer(input_str)
        lexer.lex()

        seen_text = False

        while True:
            lex_token = lexer.get_next_token()
            if not lex_token:
                break

            if lex_token.type == 'WHITESPACE':
                self._tokens.append(Token(type=lex_token.type, value=lex_token.value, start=lex_token.start))
                continue

            if (not seen_text) and (lex_token.type == 'FENCE'):
                seen_text = True
                if self._parser_state.in_code_block:
                    self._tokens.append(Token(type='FENCE_END', value='```', start=lex_token.start))
                    self._parser_state.in_code_block = False
                    self._parser_state.language = ProgrammingLanguage.UNKNOWN
                    self._parser_state.embedded_parser_state = None
                    print("*** FENCE END")
                    continue

                print("*** FENCE START")
                self._parser_state.in_code_block = True
                self._parser_state.embedded_parser_state = None
                self._tokens.append(Token(type='FENCE_START', value='```', start=lex_token.start))

                next_token = lexer.peek_next_token('WHITESPACE')
                if next_token and (next_token.type == 'TEXT'):
                    next_token = lexer.get_next_token('WHITESPACE')
                    self._tokens.append(Token(type='LANGUAGE', value=next_token.value, start=next_token.start))

                    input_normalized = next_token.value.strip().lower()
                    self._parser_state.language = language_mapping.get(input_normalized, ProgrammingLanguage.TEXT)
                    continue

                self._parser_state.language = language_mapping.get('', ProgrammingLanguage.TEXT)
                continue

            seen_text = True

            if self._parser_state.language != ProgrammingLanguage.UNKNOWN:
                self._embedded_parse(input_str)
                break

            self._tokens.append(Token(type=lex_token.type, value=lex_token.value, start=lex_token.start))

        new_parser_state = MarkdownParserState(
            in_code_block=self._parser_state.in_code_block,
            language=self._parser_state.language,
            embedded_parser_state=self._parser_state.embedded_parser_state
        )
        return new_parser_state
