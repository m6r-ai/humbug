from dataclasses import dataclass
from enum import IntEnum

from humbug.syntax.markdown_lexer import MarkdownLexer
from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser


class ProgrammingLanguage(IntEnum):
    UNKNOWN = 0
    PYTHON = 1


# Mapping from lowercase language names to enum members
language_mapping = {
    "c": ProgrammingLanguage.UNKNOWN,
    "c++": ProgrammingLanguage.UNKNOWN,
    "css": ProgrammingLanguage.UNKNOWN,
    "html": ProgrammingLanguage.UNKNOWN,
    "javascript": ProgrammingLanguage.UNKNOWN,
    "metaphor": ProgrammingLanguage.UNKNOWN,
    "python": ProgrammingLanguage.PYTHON,
    "typescript": ProgrammingLanguage.UNKNOWN,
}


@dataclass
class MarkdownParserState:
    in_code_block: bool
    language: ProgrammingLanguage


class MarkdownParser(Parser):
    def __init__(self) -> MarkdownParserState:
        super().__init__()

        self._parser_state = MarkdownParserState(
            in_code_block=False,
            language=ProgrammingLanguage.UNKNOWN
        )

    def parse(self, parser_state: MarkdownParserState, input_str: str) -> None:
        if not parser_state:
            self._parser_state.in_code_block = False
            self._parser_state.language = None
        else:
            self._parser_state.in_code_block = parser_state.in_code_block
            self._parser_state.language = parser_state.language

        lexer = MarkdownLexer(input_str)
        lexer.lex()

        seen_text = False

        while True:
            lex_token = lexer.get_next_token(['WHITESPACE'])
            if not lex_token:
                break

            if (not seen_text) and (lex_token.type == 'FENCE'):
                seen_text = True
                if self._parser_state.in_code_block:
                    self._parser_state.in_code_block = False
                    self._tokens.append(Token(type='FENCE_END', value='```', start=lex_token.start))
                    self._parser_state.language = None
                    continue

                self._parser_state.in_code_block = True
                self._tokens.append(Token(type='FENCE_START', value='```', start=lex_token.start))

                next_token = lexer.peek_next_token('WHITESPACE')
                if next_token and (next_token.type == 'TEXT'):
                    next_token = lexer.get_next_token('WHITESPACE')
                    self._tokens.append(Token(type='LANGUAGE', value=next_token.value, start=next_token.start))

                    input_normalized = next_token.value.strip().lower()
                    self._parser_state.language = language_mapping.get(input_normalized, ProgrammingLanguage.UNKNOWN)
                    print(f"language {next_token.value}, {self._parser_state.language}")

                continue

            seen_text = True
            self._tokens.append(Token(type=lex_token.type, value=lex_token.value, start=lex_token.start))

        new_parser_state = MarkdownParserState(
            in_code_block=self._parser_state.in_code_block,
            language=self._parser_state.language
        )
        return new_parser_state
