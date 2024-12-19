from dataclasses import dataclass
from enum import IntEnum

from humbug.syntax.markdown_lexer import MarkdownLexer
from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser, ParserState
from humbug.syntax.c_parser import CParser
from humbug.syntax.java_script_parser import JavaScriptParser
from humbug.syntax.metaphor_parser import MetaphorParser
from humbug.syntax.python_parser import PythonParser
from humbug.syntax.text_parser import TextParser


class ProgrammingLanguage(IntEnum):
    UNKNOWN = -1
    C = 0
    JAVASCRIPT = 1
    METAPHOR = 2
    PYTHON = 3
    TEXT = 4


# Mapping from lowercase language names to enum members
language_mapping = {
    "c": ProgrammingLanguage.C,
    "c++": ProgrammingLanguage.TEXT,
    "css": ProgrammingLanguage.TEXT,
    "html": ProgrammingLanguage.TEXT,
    "javascript": ProgrammingLanguage.JAVASCRIPT,
    "metaphor": ProgrammingLanguage.METAPHOR,
    "python": ProgrammingLanguage.PYTHON,
    "typescript": ProgrammingLanguage.TEXT,
}


@dataclass
class MarkdownParserState(ParserState):
    in_fence_block: bool
    language: ProgrammingLanguage
    embedded_parser_state: ParserState


class MarkdownParser(Parser):
    def _embedded_parse(
            self,
            language: ProgrammingLanguage,
            prev_embedded_parser_state: ParserState,
            input_str: str
    ) -> ParserState:
        embedded_parser = None
        match language:
            case ProgrammingLanguage.C:
                embedded_parser = CParser()

            case ProgrammingLanguage.JAVASCRIPT:
                embedded_parser = JavaScriptParser()

            case ProgrammingLanguage.METAPHOR:
                embedded_parser = MetaphorParser()

            case ProgrammingLanguage.PYTHON:
                embedded_parser = PythonParser()

            case ProgrammingLanguage.TEXT:
                embedded_parser = TextParser()

        embedded_parser_state = embedded_parser.parse(prev_embedded_parser_state, input_str)
        while True:
            token = embedded_parser.get_next_token()
            if token is None:
                break

            self._tokens.append(Token(type=token.type, value=token.value, start=token.start))

        return embedded_parser_state

    def parse(self, prev_parser_state: MarkdownParserState, input_str: str) -> MarkdownParserState:
        parser_state = MarkdownParserState(
            in_fence_block=False,
            language=ProgrammingLanguage.UNKNOWN,
            embedded_parser_state=None
        )

        if prev_parser_state:
            parser_state.in_fence_block = prev_parser_state.in_fence_block
            parser_state.language = prev_parser_state.language
            parser_state.embedded_parser_state = prev_parser_state.embedded_parser_state

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
                if parser_state.in_fence_block:
                    self._tokens.append(Token(type='FENCE_END', value='```', start=lex_token.start))
                    parser_state.in_fence_block = False
                    parser_state.language = ProgrammingLanguage.UNKNOWN
                    parser_state.embedded_parser_state = None
                    continue

                parser_state.in_fence_block = True
                parser_state.embedded_parser_state = None
                self._tokens.append(Token(type='FENCE_START', value='```', start=lex_token.start))

                next_token = lexer.peek_next_token('WHITESPACE')
                if next_token and (next_token.type == 'TEXT'):
                    next_token = lexer.get_next_token('WHITESPACE')
                    self._tokens.append(Token(type='LANGUAGE', value=next_token.value, start=next_token.start))

                    input_normalized = next_token.value.strip().lower()
                    parser_state.language = language_mapping.get(input_normalized, ProgrammingLanguage.TEXT)
                    continue

                parser_state.language = language_mapping.get('', ProgrammingLanguage.TEXT)
                continue

            seen_text = True

            if parser_state.language != ProgrammingLanguage.UNKNOWN:
                embedded_parser_state = self._embedded_parse(parser_state.language, parser_state.embedded_parser_state, input_str)
                parser_state.embedded_parser_state = embedded_parser_state
                break

            self._tokens.append(Token(type=lex_token.type, value=lex_token.value, start=lex_token.start))

        return parser_state
