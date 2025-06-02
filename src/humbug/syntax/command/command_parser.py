"""
System shell command parser

This module implements a parser for command lines, extending the functionality of the base parser.
"""

from humbug.mindspace.system.system_command_registry import SystemCommandRegistry
from humbug.syntax.command.command_lexer import CommandLexer
from humbug.syntax.lexer import TokenType
from humbug.syntax.parser import Parser, ParserState


class CommandParser(Parser):
    """
    Parser for command lines in the system shell.

    This parser processes tokens from the command lexer.
    """
    def __init__(self) -> None:
        super().__init__()
        self._command_registry = SystemCommandRegistry()

    def parse(self, prev_parser_state: ParserState | None, input_str: str) -> None:
        """
        Parse the input string using the provided parser state.

        Args:
            input_str: The input string to parse
        """
        lexer = CommandLexer()
        lexer.lex(None, input_str)

        while True:
            token = lexer.get_next_token()
            if not token:
                break

            self._tokens.append(token)

        # Perform semantic analysis to identify option values
        command_name = None
        for token in self._tokens:
            if token.type == TokenType.COMMAND:
                command_name = token.value
                break

        # Get the command if available
        command = self._command_registry.get_command(command_name) if command_name else None
        if command is None:
            return

        i = 0

        while i < len(self._tokens):
            token = self._tokens[i]
            i += 1

            if token.type == TokenType.OPTION:
                # Ask the command how many values this option takes
                value_count = command.get_option_value_count(token.value)

                # Process option values
                values_processed = 0
                while (value_count != 0 and
                       i < len(self._tokens) and
                       self._tokens[i].type == TokenType.ARGUMENT and
                       (value_count < 0 or values_processed < value_count)):
                    self._tokens[i].type = TokenType.OPTION_VALUE
                    i += 1
                    values_processed += 1
