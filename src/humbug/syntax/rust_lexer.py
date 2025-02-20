from dataclasses import dataclass
from typing import Callable, Optional, Set

from humbug.syntax.lexer import Lexer, LexerState, Token


@dataclass
class RustLexerState(LexerState):
    """
    State information for the Rust lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        block_comment_depth: Tracks nested block comment depth
    """
    in_block_comment: bool = False
    block_comment_depth: int = 0


class RustLexer(Lexer):
    """
    Lexer for Rust code.

    This lexer handles Rust-specific syntax including:
    - Raw string literals (r#"..."#)
    - Raw identifiers (r#ident#)
    - Generic type parameters
    - Standard language tokens (keywords, operators, etc.)
    """

    def __init__(self):
        super().__init__()
        self._in_block_comment = False
        self._block_comment_depth = 0

    def lex(self, prev_lexer_state: Optional[RustLexerState], input_str: str) -> RustLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing
        """
        self._input = input_str
        if prev_lexer_state:
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._block_comment_depth = prev_lexer_state.block_comment_depth

        if self._in_block_comment:
            self._read_block_comment(0)

        if not self._in_block_comment:
            self._inner_lex()

        lexer_state = RustLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.block_comment_depth = self._block_comment_depth
        return lexer_state

    def _get_lexing_function(self, ch: str) -> Callable[[], None]:
        """
        Get the lexing function that matches a given start character.

        Args:
            ch: The start character

        Returns:
            The appropriate lexing function for the character
        """
        if ch == '\n':
            return self._read_newline

        if self._is_whitespace(ch):
            return self._read_whitespace

        if ch == 'r':
            return self._read_raw_token

        if self._is_letter(ch) or ch == '_':
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch == '"':
            return self._read_string

        if ch == '/':
            return self._read_forward_slash

        if ch == '<':
            return self._read_angle_bracket

        return self._read_operator

    def _read_raw_token(self) -> None:
        """
        Read a raw token, which could be:
        - A raw string literal (r#"..."#)
        - A raw identifier (r#ident#)
        - A regular identifier starting with 'r'
        """
        start = self._position
        hash_count = 0

        # Check if this is a raw token
        if (self._position + 1 < len(self._input) and
                self._input[self._position + 1] == '#'):
            self._position += 2
            hash_count = 1

            # Count additional # symbols
            while (self._position < len(self._input) and
                   self._input[self._position] == '#'):
                hash_count += 1
                self._position += 1

            if self._position < len(self._input):
                if self._input[self._position] == '"':
                    self._read_raw_string(hash_count, start)
                    return

                # Raw identifier
                self._read_raw_identifier(hash_count, start)
                return

        # Not a raw token, handle as regular identifier
        self._position = start
        self._read_identifier_or_keyword()

    def _read_raw_string(self, hash_count: int, start: int) -> None:
        """
        Read a raw string literal token.
        """
        self._position += 1  # Skip the quote
        end_sequence = '"' + '#' * hash_count

        while self._position < len(self._input):
            if (self._position + len(end_sequence) <= len(self._input) and
                    self._input[self._position:self._position + len(end_sequence)] == end_sequence):
                self._position += len(end_sequence)
                break
            self._position += 1

        self._tokens.append(Token(
            type='STRING',
            value=self._input[start:self._position],
            start=start
        ))

    def _read_raw_identifier(self, hash_count: int, start: int) -> None:
        """
        Read a raw identifier token.
        """
        while self._position < len(self._input):
            ch = self._input[self._position]
            if (not self._is_letter_or_digit(ch) and
                    ch != '_' and ch != '#'):
                break
            self._position += 1

        # Ensure proper closing with matching number of #
        closing_hashes = 0
        while (self._position > 0 and
               self._position - 1 < len(self._input) and
               self._input[self._position - 1] == '#'):
            closing_hashes += 1
            self._position -= 1

        if closing_hashes != hash_count:
            # Invalid raw identifier, treat as error
            self._tokens.append(Token(
                type='ERROR',
                value=self._input[start:self._position + closing_hashes],
                start=start
            ))
            return

        self._position += closing_hashes
        value = self._input[start:self._position]

        # Check if the raw identifier is actually a keyword
        inner_value = value[2:-hash_count]  # Strip r# and trailing #
        if self._is_keyword(inner_value):
            self._tokens.append(Token(type='KEYWORD', value=value, start=start))
            return

        self._tokens.append(Token(type='IDENTIFIER', value=value, start=start))

    def _read_angle_bracket(self) -> None:
        """
        Read an angle bracket, which could be:
        - A generic type parameter
        - A comparison operator
        - A bit shift operator
        """
        start = self._position
        self._position += 1

        # Check for <= or <<
        if self._position < len(self._input):
            ch = self._input[self._position]
            if ch == '=':
                self._position += 1
                self._tokens.append(Token(
                    type='OPERATOR',
                    value='<=',
                    start=start
                ))
                return
            if ch == '<':
                self._position += 1
                # Check for <<=
                if (self._position < len(self._input) and
                        self._input[self._position] == '='):
                    self._position += 1
                    self._tokens.append(Token(
                        type='OPERATOR',
                        value='<<=',
                        start=start
                    ))
                    return
                self._tokens.append(Token(
                    type='OPERATOR',
                    value='<<',
                    start=start
                ))
                return

        # Single < for generic parameter or comparison
        self._tokens.append(Token(
            type='OPERATOR',
            value='<',
            start=start
        ))

    def _read_identifier_or_keyword(self) -> None:
        """
        Read an identifier or keyword token.
        """
        start = self._position
        self._position += 1
        while (self._position < len(self._input) and
               (self._is_letter_or_digit(self._input[self._position]) or
                self._input[self._position] == '_')):
            self._position += 1

        value = self._input[start:self._position]
        if self._is_keyword(value):
            self._tokens.append(Token(type='KEYWORD', value=value, start=start))
            return

        self._tokens.append(Token(type='IDENTIFIER', value=value, start=start))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a Rust keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a Rust keyword, False otherwise
        """
        return value in self._get_keywords()

    def _get_keywords(self) -> Set[str]:
        """
        Get the set of Rust keywords.

        Returns:
            Set of Rust keywords
        """
        return {
            # Reserved keywords
            'as', 'break', 'const', 'continue', 'crate', 'else', 'enum',
            'extern', 'false', 'fn', 'for', 'if', 'impl', 'in', 'let',
            'loop', 'match', 'mod', 'move', 'mut', 'pub', 'ref', 'return',
            'self', 'Self', 'static', 'struct', 'super', 'trait', 'true',
            'type', 'unsafe', 'use', 'where', 'while',

            # Reserved for future use
            'abstract', 'async', 'become', 'box', 'do', 'final', 'macro',
            'override', 'priv', 'try', 'typeof', 'unsized', 'virtual',
            'yield'
        }
