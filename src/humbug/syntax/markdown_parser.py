from dataclasses import dataclass

from humbug.syntax.markdown_lexer import MarkdownLexer
from humbug.syntax.lexer import Token
from humbug.syntax.parser import Parser


@dataclass
class MarkdownParserState:
    in_code_block: bool


class MarkdownParser(Parser):
    def __init__(self) -> MarkdownParserState:
        super().__init__()

        self._parser_state = MarkdownParserState(in_code_block=False)

    def parse(self, parser_state: MarkdownParserState, input_str: str) -> None:
        self._parser_state.in_code_block = parser_state.in_code_block if parser_state else False

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
                    continue

                self._parser_state.in_code_block = True
                self._tokens.append(Token(type='FENCE_START', value='```', start=lex_token.start))

                next_token = lexer.peek_next_token('WHITESPACE')
                if next_token and (next_token.type == 'TEXT'):
                    next_token = lexer.get_next_token('WHITESPACE')
                    self._tokens.append(Token(type='LANGUAGE', value=next_token.value, start=next_token.start))

                continue

            seen_text = True
            self._tokens.append(Token(type=lex_token.type, value=lex_token.value, start=lex_token.start))

        new_parser_state = MarkdownParserState(in_code_block=self._parser_state.in_code_block)
        return new_parser_state
