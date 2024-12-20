from dataclasses import dataclass
from typing import Optional

from humbug.syntax.javascript_parser import JavaScriptParser
from humbug.syntax.css_parser import CSSParser
from humbug.syntax.html_lexer import HTMLLexer
from humbug.syntax.parser import Parser, ParserState


@dataclass
class HTMLParserState(ParserState):
    """
    State information for the HTML parser.

    Attributes:
        js_parser: Optional JavaScript parser for script content
        css_parser: Optional CSS parser for style content
    """
#    js_parser: Optional[JavaScriptParser] = None
#    css_parser: Optional[CSSParser] = None


class HTMLParser(Parser):
    """
    Parser for HTML code.

    This parser processes tokens from the HTML lexer and handles special cases
    like embedded JavaScript and CSS content.
    """

    def parse(self, prev_parser_state: Optional[HTMLParserState], input_str: str) -> HTMLParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing

        Note:
            The parser handles embedded JavaScript and CSS content by delegating to
            specialized parsers for those languages.
        """
        parser_state = HTMLParserState()
        prev_lexer_state = None

        if prev_parser_state:
            prev_lexer_state = prev_parser_state.lexer_state

#        if prev_parser_state:
#            parser_state.js_parser = prev_parser_state.js_parser
#            parser_state.css_parser = prev_parser_state.css_parser

        lexer = HTMLLexer()
        lexer_state = lexer.lex(prev_lexer_state, input_str)
        parser_state.continuation_state = 1 if lexer_state.in_comment else 0
        parser_state.lexer_state = lexer_state

        while True:
            # If we're using a JavaScript parser, use that until we've completed
            # processing the JavaScript
#            if parser_state.js_parser:
#                token = parser_state.js_parser.get_next_token()
#                if token:
#                    self._tokens.append(token)
#                    continue

#                parser_state.js_parser = None

            # If we're using a CSS parser, use that until we've completed
            # processing the CSS
#            if parser_state.css_parser:
#                token = parser_state.css_parser.get_next_token()
#                if token:
#                    self._tokens.append(token)
#                    continue

#                parser_state.css_parser = None

            token = lexer.get_next_token()
            if not token:
                break

#            if token.type == 'SCRIPT':
#                parser_state.js_parser = JavaScriptParser(token.value)
#                js_token = parser_state.js_parser.get_next_token()
#                if js_token:
#                    self._tokens.append(js_token)
#                    continue

#                parser_state.js_parser = None

#            if token.type == 'STYLE':
#                parser_state.css_parser = CSSParser(token.value)
#                css_token = parser_state.css_parser.get_next_token()
#                if css_token:
#                    self._tokens.append(css_token)
#                    continue

#                parser_state.css_parser = None

            self._tokens.append(token)

        return parser_state
