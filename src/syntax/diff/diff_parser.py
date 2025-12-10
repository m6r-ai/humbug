"""
Diff/Patch Parser

This module implements a parser for diff/patch files, extending the functionality
of the base parser.
"""

from dataclasses import dataclass

from syntax.diff.diff_lexer import DiffLexer
from syntax.parser import Parser, ParserState
from syntax.parser_registry import ParserRegistry
from syntax.programming_language import ProgrammingLanguage


@dataclass
class DiffParserState(ParserState):
    """
    State information for the Diff parser.

    Since diff parsing is stateless (each line is independently classified),
    no additional state beyond the base ParserState is needed.
    """


@ParserRegistry.register_parser(ProgrammingLanguage.DIFF)
class DiffParser(Parser):
    """
    Parser for diff/patch files.

    This parser processes tokens from the Diff lexer. Since diff files are
    line-oriented and stateless, the parser simply passes through the lexer
    tokens without additional processing.
    """

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> DiffParserState:
        """
        Parse the input string using the provided parser state.

        Args:
            prev_parser_state: Optional previous parser state (unused for diff)
            input_str: The input string to parse

        Returns:
            The updated parser state after parsing
        """
        lexer = DiffLexer()
        lexer_state = lexer.lex(None, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            self._tokens.append(token)

        parser_state = DiffParserState()
        parser_state.lexer_state = lexer_state
        return parser_state
