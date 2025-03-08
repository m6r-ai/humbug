from dataclasses import dataclass
from typing import Callable, Optional, Set

from humbug.syntax.lexer import Lexer, LexerState, Token, TokenType


@dataclass
class CSharpLexerState(LexerState):
    """
    State information for the C# lexer.

    Attributes:
        in_block_comment: Indicates if we're currently parsing a block comment
        in_xml_doc: Indicates if we're currently parsing an XML documentation comment
        in_verbatim_string: Indicates if we're currently parsing a verbatim string literal
    """
    in_block_comment: bool = False
    in_xml_doc: bool = False
    in_verbatim_string: bool = False


class CSharpLexer(Lexer):
    """
    Lexer for C# code.

    This lexer handles C#-specific syntax including:
    - Keywords and modifiers
    - String literals (including verbatim strings)
    - Numbers with underscores and suffixes
    - Comments (including XML documentation comments)
    - Attributes
    - Generics
    - Lambda expressions
    - Preprocessor directives
    - Nullable types
    """

    def __init__(self):
        super().__init__()
        self._in_block_comment = False
        self._in_xml_doc = False
        self._in_verbatim_string = False

    def lex(self, prev_lexer_state: Optional[CSharpLexerState], input_str: str) -> CSharpLexerState:
        """
        Lex all the tokens in the input.

        Args:
            prev_lexer_state: Optional previous lexer state
            input_str: The input string to parse

        Returns:
            The updated lexer state after processing

        Raises:
            None
        """
        self._input = input_str
        self._input_len = len(input_str)
        if prev_lexer_state:
            self._in_block_comment = prev_lexer_state.in_block_comment
            self._in_xml_doc = prev_lexer_state.in_xml_doc
            self._in_verbatim_string = prev_lexer_state.in_verbatim_string

        if self._in_block_comment or self._in_xml_doc:
            self._read_block_comment(0)

        if self._in_verbatim_string:
            self._read_verbatim_string(0)

        if not (self._in_block_comment or self._in_xml_doc or
                self._in_verbatim_string):
            self._inner_lex()

        lexer_state = CSharpLexerState()
        lexer_state.in_block_comment = self._in_block_comment
        lexer_state.in_xml_doc = self._in_xml_doc
        lexer_state.in_verbatim_string = self._in_verbatim_string
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

        if ch == '[':
            return self._read_attribute_or_operator

        if ch == '_' or self._is_letter(ch):
            return self._read_identifier_or_keyword

        if self._is_digit(ch):
            return self._read_number

        if ch == '"':
            return self._read_string

        if ch == '@':
            return self._read_verbatim_prefix

        if ch == '$':
            return self._read_interpolated_prefix

        if ch == '\'':
            return self._read_character

        if ch == '.':
            return self._read_dot

        if ch == '/':
            return self._read_forward_slash

        if ch == '#':
            return self._read_preprocessor_directive

        return self._read_operator

    def _read_interpolated_prefix(self) -> None:
        """
        Read an interpolated string prefix ($).
        """
        start = self._position
        self._position += 1
        if self._position < self._input_len:
            next_char = self._input[self._position]
            if next_char == '"':
                self._tokens.append(Token(
                    type=TokenType.STRING,
                    value=self._input[start:self._position],
                    start=start
                ))
                self._read_string()
                return

        self._tokens.append(Token(
            type=TokenType.ERROR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_verbatim_prefix(self) -> None:
        """
        Read a verbatim string prefix (@) or identifier starting with @.
        """
        start = self._position
        self._position += 1
        if self._position < self._input_len:
            next_char = self._input[self._position]
            if next_char == '"':
                # @"string" - verbatim string
                self._read_verbatim_string(2)
                return
            elif next_char == '$' and self._position + 1 < self._input_len and self._input[self._position + 1] == '"':
                # @$"string" - verbatim interpolated string
                self._read_verbatim_string(3)
                return
            elif self._is_letter(next_char) or next_char == '_':
                # @identifier - verbatim identifier (allows using reserved keywords as identifiers)
                self._position += 1
                while (self._position < self._input_len and
                      (self._is_letter_or_digit(self._input[self._position]) or
                       self._input[self._position] == '_')):
                    self._position += 1

                self._tokens.append(Token(
                    type=TokenType.IDENTIFIER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        # Just @ by itself (unlikely but handle it as an error)
        self._tokens.append(Token(
            type=TokenType.ERROR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_string(self) -> None:
        """
        Read a regular string literal token.
        """
        start = self._position
        self._position += 1  # Skip opening quote

        while self._position < self._input_len and self._input[self._position] != '"':
            if self._input[self._position] == '\\':
                self._position += 2  # Skip escape sequence
                continue
            self._position += 1

        if self._position < self._input_len:
            self._position += 1  # Include closing quote

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_verbatim_string(self, skip_chars: int) -> None:
        """
        Read a verbatim string literal token (@"...").

        In verbatim strings, backslashes are treated as literal characters,
        and double quotes are escaped by doubling them.

        Args:
            skip_chars: Number of characters to skip at start
        """
        self._in_verbatim_string = True
        start = self._position
        self._position += skip_chars

        while self._position < self._input_len:
            if self._input[self._position] == '"':
                # Check if it's a doubled quote (escaped)
                if (self._position + 1 < self._input_len and
                    self._input[self._position + 1] == '"'):
                    self._position += 2  # Skip both quotes
                    continue
                else:
                    # End of verbatim string
                    self._in_verbatim_string = False
                    self._position += 1  # Include closing quote
                    break
            self._position += 1

        if self._in_verbatim_string:
            # Reached end of input without closing the string
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.STRING,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_character(self) -> None:
        """
        Read a character literal token ('x').
        """
        start = self._position
        self._position += 1  # Skip opening quote

        if self._position < self._input_len:
            if self._input[self._position] == '\\':
                self._position += 2  # Skip escape sequence
            else:
                self._position += 1  # Regular character

        if self._position < self._input_len and self._input[self._position] == '\'':
            self._position += 1  # Include closing quote

        self._tokens.append(Token(
            type=TokenType.CHARACTER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_attribute_or_operator(self) -> None:
        """
        Read an attribute token ([Attribute]) or bracket operator.
        """
        start = self._position
        self._position += 1  # Skip opening bracket

        # Check if this might be an attribute
        # Look ahead for an identifier followed by optional parameters and closing bracket
        saved_position = self._position

        # Skip whitespace
        while (self._position < self._input_len and
               self._is_whitespace(self._input[self._position])):
            self._position += 1

        # Check if next token could be an identifier (attribute name)
        is_attribute = False
        if self._position < self._input_len:
            ch = self._input[self._position]
            if self._is_letter(ch) or ch == '_':
                is_attribute = True

                # Skip the identifier
                self._position += 1
                while (self._position < self._input_len and
                      (self._is_letter_or_digit(self._input[self._position]) or
                       self._input[self._position] == '_')):
                    self._position += 1

                # Look for closing bracket or parameters
                while self._position < self._input_len and self._input[self._position] != ']':
                    self._position += 1

                # If we didn't find a closing bracket, it's not an attribute
                if self._position >= self._input_len or self._input[self._position] != ']':
                    is_attribute = False

        # Reset position
        self._position = saved_position

        if is_attribute:
            # Find the closing bracket
            bracket_depth = 1
            while self._position < self._input_len and bracket_depth > 0:
                ch = self._input[self._position]
                self._position += 1

                if ch == '[':
                    bracket_depth += 1
                elif ch == ']':
                    bracket_depth -= 1

            self._tokens.append(Token(
                type=TokenType.ATTRIBUTE,
                value=self._input[start:self._position],
                start=start
            ))
        else:
            # Just a bracket operator
            self._tokens.append(Token(
                type=TokenType.OPERATOR,
                value='[',
                start=start
            ))

    def _read_number(self) -> None:
        """
        Read a numeric literal token.

        Handles:
        - Decimal integers with optional underscores
        - Hexadecimal numbers (0x prefix)
        - Binary numbers (0b prefix)
        - Floating point numbers
        - Number suffixes (UL, F, M, etc.)
        """
        start = self._position

        # Check for hex or binary prefix
        if (self._input[self._position] == '0' and
            self._position + 1 < self._input_len):
            next_char = self._input[self._position + 1].lower()

            if next_char == 'x':  # Hexadecimal
                self._position += 2
                while (self._position < self._input_len and
                      (self._is_hex_digit(self._input[self._position]) or
                       self._input[self._position] == '_')):
                    self._position += 1

                # Check for suffix
                self._read_number_suffix()

                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

            elif next_char == 'b':  # Binary
                self._position += 2
                while (self._position < self._input_len and
                      (self._is_binary_digit(self._input[self._position]) or
                       self._input[self._position] == '_')):
                    self._position += 1

                # Check for suffix
                self._read_number_suffix()

                self._tokens.append(Token(
                    type=TokenType.NUMBER,
                    value=self._input[start:self._position],
                    start=start
                ))
                return

        # Read decimal integer or floating point
        self._read_decimal_number()

        # Check for suffix
        self._read_number_suffix()

        self._tokens.append(Token(
            type=TokenType.NUMBER,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_decimal_number(self) -> None:
        """
        Read a decimal number (integer or floating-point).
        """
        # Read integer part
        while (self._position < self._input_len and
              (self._is_digit(self._input[self._position]) or
               self._input[self._position] == '_')):
            self._position += 1

        # Check for decimal point
        if (self._position < self._input_len and
            self._input[self._position] == '.'):
            self._position += 1

            # Read fractional part
            while (self._position < self._input_len and
                  (self._is_digit(self._input[self._position]) or
                   self._input[self._position] == '_')):
                self._position += 1

        # Check for exponent
        if (self._position < self._input_len and
            self._input[self._position].lower() == 'e'):
            self._position += 1

            # Optional sign
            if (self._position < self._input_len and
                self._input[self._position] in ('+', '-')):
                self._position += 1

            # Exponent digits
            while (self._position < self._input_len and
                  (self._is_digit(self._input[self._position]) or
                   self._input[self._position] == '_')):
                self._position += 1

    def _read_number_suffix(self) -> None:
        """
        Read numeric literal suffix.

        C# number suffixes include:
        - Integers: u, l, ul, lu
        - Floating-point: f, d, m

        All suffixes are case-insensitive.
        """
        if self._position >= self._input_len:
            return

        suffix_char = self._input[self._position].lower()

        if suffix_char in ('u', 'l', 'f', 'd', 'm'):
            start_pos = self._position
            self._position += 1

            # For ul/lu combinations
            if (self._position < self._input_len and
                suffix_char in ('u', 'l')):
                next_char = self._input[self._position].lower()

                if ((suffix_char == 'u' and next_char == 'l') or
                    (suffix_char == 'l' and next_char == 'u')):
                    self._position += 1

            # Check if the suffix is valid
            suffix = self._input[start_pos:self._position].lower()
            valid_suffixes = {'u', 'l', 'ul', 'lu', 'f', 'd', 'm'}

            if suffix not in valid_suffixes:
                self._position = start_pos  # Reset position if invalid

    def _read_dot(self) -> None:
        """
        Read a dot operator or decimal point.
        Handles:
        - Member access operator (.)
        - Start of float literal (.123)
        - Range operator (..)
        """
        # Check for range operator
        if (self._position + 1 < self._input_len and
            self._input[self._position + 1] == '.'):
            start = self._position
            self._position += 2  # Skip both dots
            self._tokens.append(Token(
                type=TokenType.OPERATOR,
                value='..',
                start=start
            ))
            return

        # Check if this is the start of a float literal
        if (self._position + 1 < self._input_len and
            self._is_digit(self._input[self._position + 1])):
            self._read_number()
            return

        # Just a regular dot operator
        self._read_operator()

    def _read_forward_slash(self) -> None:
        """
        Read a forward slash token, which could be:
        - Start of a single-line comment (//)
        - Start of a block comment (/*)
        - Start of an XML documentation comment (///)
        - Division operator (/)
        """
        if self._position + 1 >= self._input_len:
            self._read_operator()
            return

        next_char = self._input[self._position + 1]

        if next_char == '/':
            # Could be a line comment (//) or XML doc comment (///)
            if (self._position + 2 < self._input_len and
                self._input[self._position + 2] == '/'):
                self._read_xml_doc_comment()
            else:
                self._read_line_comment()

        elif next_char == '*':
            self._read_block_comment(2)

        else:
            self._read_operator()

    def _read_line_comment(self) -> None:
        """
        Read a single-line comment token (//).
        """
        start = self._position
        self._position += 2  # Skip //

        while self._position < self._input_len and self._input[self._position] != '\n':
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_xml_doc_comment(self) -> None:
        """
        Read an XML documentation comment token (///).
        """
        start = self._position
        self._position += 3  # Skip ///

        while self._position < self._input_len and self._input[self._position] != '\n':
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.XML_DOC,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_block_comment(self, skip_chars: int) -> None:
        """
        Read a block comment token (/* ... */).

        Args:
            skip_chars: Number of characters to skip at start
        """
        self._in_block_comment = True
        start = self._position
        self._position += skip_chars

        while self._position + 1 < self._input_len:
            if (self._input[self._position] == '*' and
                self._input[self._position + 1] == '/'):
                self._in_block_comment = False
                self._position += 2  # Skip closing */
                break

            self._position += 1

        if self._in_block_comment:
            # Reached end of input without closing the comment
            self._position = self._input_len

        self._tokens.append(Token(
            type=TokenType.COMMENT,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_preprocessor_directive(self) -> None:
        """
        Read a preprocessor directive token (#if, #region, etc.).
        """
        start = self._position
        self._position += 1  # Skip #

        # Read the directive name
        while (self._position < self._input_len and
               self._is_letter(self._input[self._position])):
            self._position += 1

        # Read the rest of the line
        while self._position < self._input_len and self._input[self._position] != '\n':
            self._position += 1

        self._tokens.append(Token(
            type=TokenType.PREPROCESSOR,
            value=self._input[start:self._position],
            start=start
        ))

    def _read_identifier_or_keyword(self) -> None:
        """
        Read an identifier or keyword token.
        """
        start = self._position
        self._position += 1

        while (self._position < self._input_len and
              (self._is_letter_or_digit(self._input[self._position]) or
               self._input[self._position] == '_')):
            self._position += 1

        # Check for contextual keywords (that can also be identifiers)
        value = self._input[start:self._position]

        # Handle generics and nullable types
        if (value and self._position < self._input_len and
            self._input[self._position] == '?'):
            # This is a nullable type (e.g., int?)
            self._position += 1
            value = self._input[start:self._position]

        if self._is_keyword(value):
            self._tokens.append(Token(
                type=TokenType.KEYWORD,
                value=value,
                start=start
            ))
        else:
            self._tokens.append(Token(
                type=TokenType.IDENTIFIER,
                value=value,
                start=start
            ))

    def _read_operator(self) -> None:
        """
        Read an operator token.

        Handles all C# operators including:
        - Arithmetic operators
        - Comparison operators
        - Logical operators
        - Bitwise operators
        - Assignment operators
        - Null-conditional operators (?. and ?[)
        - Null-coalescing operator (??)
        - Lambda operator (=>)
        """
        operators = [
            '??=', '<<=', '>>=', '&&=', '||=', '??', '?.', '?[', '=>', '++', '--',
            '&&', '||', '==', '!=', '<=', '>=', '+=', '-=', '*=', '/=', '%=', '&=',
            '|=', '^=', '<<', '>>', '::', '->', '+', '-', '*', '/', '%', '&', '|',
            '^', '!', '~', '=', '<', '>', '?', ':', '.', ',', ';', '(', ')', '{',
            '}', '[', ']'
        ]

        for operator in operators:
            if self._input[self._position:].startswith(operator):
                start = self._position
                self._position += len(operator)
                self._tokens.append(Token(
                    type=TokenType.OPERATOR,
                    value=operator,
                    start=start
                ))
                return

        # If we get here, we have an unknown character
        start = self._position
        self._position += 1
        self._tokens.append(Token(
            type=TokenType.ERROR,
            value=self._input[start:self._position],
            start=start
        ))

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a C# keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a C# keyword, False otherwise
        """
        return value in self._get_keywords()

    def _get_keywords(self) -> Set[str]:
        """
        Get the set of C# keywords.

        Returns:
            Set of C# keywords
        """
        return {
            # Regular keywords
            'abstract', 'as', 'base', 'bool', 'break', 'byte', 'case', 'catch',
            'char', 'checked', 'class', 'const', 'continue', 'decimal', 'default',
            'delegate', 'do', 'double', 'else', 'enum', 'event', 'explicit', 'extern',
            'false', 'finally', 'fixed', 'float', 'for', 'foreach', 'goto', 'if',
            'implicit', 'in', 'int', 'interface', 'internal', 'is', 'lock', 'long',
            'namespace', 'new', 'null', 'object', 'operator', 'out', 'override',
            'params', 'private', 'protected', 'public', 'readonly', 'ref', 'return',
            'sbyte', 'sealed', 'short', 'sizeof', 'stackalloc', 'static', 'string',
            'struct', 'switch', 'this', 'throw', 'true', 'try', 'typeof', 'uint',
            'ulong', 'unchecked', 'unsafe', 'ushort', 'using', 'virtual', 'void',
            'volatile', 'while',

            # Contextual keywords
            'add', 'and', 'alias', 'ascending', 'async', 'await', 'by', 'descending',
            'dynamic', 'equals', 'from', 'get', 'global', 'group', 'init', 'into',
            'join', 'let', 'managed', 'nameof', 'not', 'notnull', 'on', 'or',
            'orderby', 'partial', 'record', 'remove', 'required', 'select', 'set',
            'unmanaged', 'value', 'var', 'when', 'where', 'with', 'yield'
        }
