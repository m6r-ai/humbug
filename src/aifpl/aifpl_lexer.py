"""Tokenizer for AIFPL expressions with detailed error messages."""

from typing import Callable, List, Union

from aifpl.aifpl_error import AIFPLTokenError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


class AIFPLLexer:
    """Lexes AIFPL expressions into tokens with detailed error messages."""

    def __init__(self) -> None:
        """Initialize the lexer with empty state."""
        self._expression = ""
        self._tokens: List[AIFPLToken] = []
        self._position = 0
        self._line = 1
        self._column = 1
        self._preserve_comments = False

        # Build jump table for ASCII characters (0-127)
        # This provides O(1) lookup instead of O(n) if/elif chain
        self._jump_table: List[Callable[[], None]] = [self._handle_invalid_char] * 128

        # Whitespace characters (except newline which is handled separately)
        for code in [9, 11, 12, 13, 32]:  # \t, \v, \f, \r, space
            self._jump_table[code] = self._handle_whitespace

        # Newline
        self._jump_table[10] = self._handle_newline  # \n

        # Special single-character tokens
        self._jump_table[ord('(')] = self._handle_lparen
        self._jump_table[ord(')')] = self._handle_rparen
        self._jump_table[ord("'")] = self._handle_quote
        self._jump_table[ord('"')] = self._handle_string
        self._jump_table[ord(';')] = self._handle_comment
        self._jump_table[ord('#')] = self._handle_hash

        # Digits - all lead to number handling
        for code in range(ord('0'), ord('9') + 1):
            self._jump_table[code] = self._handle_number

        # Period - could be decimal number or symbol
        self._jump_table[ord('.')] = self._handle_dot

        # Plus and minus - could be number or symbol
        self._jump_table[ord('+')] = self._handle_plus
        self._jump_table[ord('-')] = self._handle_minus

        # Letters - all lead to symbol handling
        for code in range(ord('a'), ord('z') + 1):
            self._jump_table[code] = self._handle_symbol

        for code in range(ord('A'), ord('Z') + 1):
            self._jump_table[code] = self._handle_symbol

        # Other symbol-starting characters
        for char in '*/%<>=!&|^~_':
            self._jump_table[ord(char)] = self._handle_symbol

    def lex(self, expression: str, preserve_comments: bool = False) -> List[AIFPLToken]:
        """
        Lex an AIFPL expression with detailed error reporting.

        Args:
            expression: The expression string to lex
            preserve_comments: If True, emit COMMENT tokens instead of skipping them

        Returns:
            List of tokens

        Raises:
            AIFPLTokenError: If tokenization fails with detailed context
        """
        self._expression = expression
        self._tokens = []
        self._position = 0
        self._line = 1
        self._column = 1
        self._preserve_comments = preserve_comments

        while self._position < len(self._expression):
            char = self._expression[self._position]

            # Use jump table for ASCII, fallback to slow path for non-ASCII
            char_code = ord(char)
            if char_code < 128:
                self._jump_table[char_code]()
                continue

            # Non-ASCII characters - use slow path
            if char.isalpha():
                self._handle_symbol()
                continue

            self._handle_invalid_char()

        return self._tokens

    def _handle_dot(self) -> None:
        """Handle period - could be start of decimal number (.5) or symbol (.)."""
        # Check if this is a decimal number like .5
        if self._position + 1 < len(self._expression) and self._expression[self._position + 1].isdigit():
            self._handle_number()
            return

        self._handle_symbol()

    def _handle_plus(self) -> None:
        """Handle + - could be start of number or symbol."""
        # Check if this is a number like +42, or +3.14
        expression = self._expression
        pos = self._position

        # Positive numbers (explicit + sign)
        if pos + 1 < len(expression):
            next_char = expression[pos + 1]
            if next_char.isdigit():
                self._handle_number()
                return

            if next_char == '.' and pos + 2 < len(expression) and expression[pos + 2].isdigit():
                self._handle_number()
                return

        self._handle_symbol()

    def _handle_minus(self) -> None:
        """Handle - - could be start of number or symbol."""
        # Check if this is a number like -3.14 or -#xFF
        expression = self._expression
        pos = self._position

        # Negative numbers
        if pos + 1 < len(expression):
            next_char = expression[pos + 1]
            if next_char.isdigit():
                self._handle_number()
                return

            if next_char == '.' and pos + 2 < len(expression) and expression[pos + 2].isdigit():
                self._handle_number()
                return

            if next_char == '#' and pos + 2 < len(expression) and expression[pos + 2] in 'xXbBoO':
                self._handle_number()
                return

        self._handle_symbol()

    def _handle_newline(self) -> None:
        """Handle newline character."""
        self._line += 1
        self._column = 1
        self._position += 1

    def _handle_whitespace(self) -> None:
        """Handle whitespace character (not newline)."""
        self._column += 1
        self._position += 1

    def _handle_comment(self) -> None:
        """Handle comment - skip or preserve based on mode."""
        if not self._preserve_comments:
            # Skip comment
            while self._position < len(self._expression) and self._expression[self._position] != '\n':
                self._column += 1
                self._position += 1

            return

        # Preserve the comment as a token
        start_line = self._line
        start_col = self._column
        start_pos = self._position

        # Read until end of line
        while self._position < len(self._expression) and self._expression[self._position] != '\n':
            self._column += 1
            self._position += 1

        comment_text = self._expression[start_pos:self._position]
        length = self._position - start_pos
        self._tokens.append(AIFPLToken(AIFPLTokenType.COMMENT, comment_text, length, start_line, start_col))

    def _handle_lparen(self) -> None:
        """Handle left parenthesis."""
        char = self._expression[self._position]
        self._tokens.append(AIFPLToken(AIFPLTokenType.LPAREN, char, 1, self._line, self._column))
        self._column += 1
        self._position += 1

    def _handle_rparen(self) -> None:
        """Handle right parenthesis."""
        char = self._expression[self._position]
        self._tokens.append(AIFPLToken(AIFPLTokenType.RPAREN, char, 1, self._line, self._column))
        self._column += 1
        self._position += 1

    def _handle_quote(self) -> None:
        """Handle quote character."""
        self._tokens.append(AIFPLToken(AIFPLTokenType.QUOTE, "'", 1, self._line, self._column))
        self._column += 1
        self._position += 1

    def _handle_string(self) -> None:
        """Handle string literal."""
        start_line = self._line
        start_col = self._column
        start_pos = self._position

        try:
            string_value, length = self._read_string(self._expression, self._position)
            self._tokens.append(AIFPLToken(AIFPLTokenType.STRING, string_value, length, start_line, start_col))

            # Advance position for each character in the string
            for j in range(length):
                if self._expression[self._position + j] == '\n':
                    self._line += 1
                    self._column = 1

                else:
                    self._column += 1

            self._position += length

        except AIFPLTokenError as e:
            # Convert to detailed error
            if "Unterminated string" in str(e):
                raise AIFPLTokenError(
                    message="Unterminated string literal",
                    line=self._line,
                    column=self._column,
                    received=f"String starting with: {self._expression[start_pos:start_pos+10]}...",
                    expected="Closing quote \" at end of string",
                    example='Correct: "hello world"\\nIncorrect: "hello world',
                    suggestion="Add closing quote \" at the end of the string",
                    context="String literals must be enclosed in double quotes"
                ) from e

            if "Invalid escape sequence" in str(e):
                # Find the escape position
                escape_pos = start_pos + 1
                escape_line = start_line
                escape_col = start_col
                while escape_pos < len(self._expression) and self._expression[escape_pos] != '\\':
                    if self._expression[escape_pos] == '\n':
                        escape_line += 1
                        escape_col = 1

                    else:
                        escape_col += 1

                    escape_pos += 1

                bad_escape = self._expression[escape_pos:escape_pos+2]
                raise AIFPLTokenError(
                    message=f"Invalid escape sequence: {bad_escape}",
                    line=escape_line,
                    column=escape_col,
                    received=f"Escape sequence: {bad_escape}",
                    expected="Valid escape: \\n, \\t, \\r, \\\", \\\\, or \\uXXXX",
                    example='Valid: "line1\\nline2" or "tab\\there"\\nInvalid: "bad\\qsequence"',
                    suggestion="Use valid escape sequences or remove backslash",
                    context="Only specific escape sequences are supported in strings"
                ) from e

            raise  # Re-raise if not handled

    def _handle_hash(self) -> None:
        """Handle hash literals: booleans (#t, #f) or based numbers (#xFF, #b1010, #o755)."""
        if self._position + 1 >= len(self._expression):
            # Lone # at end of input - invalid
            self._handle_invalid_hash_sequence()
            return

        next_char = self._expression[self._position + 1]

        # Boolean literals
        if next_char in 'tf':
            self._handle_boolean()
            return

        # Based numbers (hex, binary, octal)
        if next_char in 'xXbBoO':
            start_line = self._line
            start_col = self._column

            try:
                number_value, length, token_type = self._read_hash_number(
                    self._expression, self._position, start_line, start_col
                )
                self._tokens.append(AIFPLToken(token_type, number_value, length, start_line, start_col))
                self._column += length
                self._position += length

            except AIFPLTokenError as e:
                raise e

            return

        # Invalid # sequence
        self._handle_invalid_hash_sequence()

    def _handle_boolean(self) -> None:
        """Handle boolean literals (#t, #f)."""
        # Check if this is part of a longer invalid sequence like #true or #false
        if (self._position + 2 < len(self._expression) and not self._is_delimiter(self._expression[self._position + 2])):
            # Find end of the invalid sequence
            end = self._position + 2
            while end < len(self._expression) and not self._is_delimiter(self._expression[end]):
                end += 1

            invalid_literal = self._expression[self._position:end]
            raise AIFPLTokenError(
                message=f"Invalid boolean literal: {invalid_literal}",
                line=self._line,
                column=self._column,
                received=f"Boolean literal: {invalid_literal}",
                expected="Valid boolean: #t or #f",
                example="Correct: #t, #f\\nIncorrect: #true, #false, #T, #F",
                suggestion="Use #t for true or #f for false",
                context="AIFPL uses #t and #f for boolean values"
            )

        boolean_value = self._expression[self._position + 1] == 't'
        self._tokens.append(AIFPLToken(AIFPLTokenType.BOOLEAN, boolean_value, 2, self._line, self._column))
        self._column += 2
        self._position += 2

    def _handle_invalid_hash_sequence(self) -> None:
        """Handle invalid # sequence with helpful error message."""
        invalid_char = self._expression[self._position + 1] if self._position + 1 < len(self._expression) else ''

        # Check if it looks like they tried Python-style 0x/0b/0o
        suggestion = "Use #t for true or #f for false"
        if invalid_char.isdigit():
            suggestion = "For hex/binary/octal use #x, #b, or #o prefix (e.g., #xFF, #b1010, #o755)"

        elif invalid_char in 'xXbBoO':
            suggestion = f"Use #{invalid_char} followed by digits (e.g., #xFF, #b1010, #o755)"

        raise AIFPLTokenError(
            message=f"Invalid # literal: #{invalid_char}",
            line=self._line,
            column=self._column,
            received=f"Found: #{invalid_char}",
            expected="Valid # literal: #t, #f, #xFF, #b1010, #o755",
            example="Correct: #t, #f, #xFF, #b1010, #o755\\nIncorrect: #true, #1, 0xFF",
            suggestion=suggestion,
            context="# must be followed by: 't'/'f' (boolean), 'x'/'X' (hex), 'b'/'B' (binary), or 'o'/'O' (octal)"
        )

    def _handle_number(self) -> None:
        """Handle number literals (including complex, scientific notation)."""
        start_line = self._line
        start_col = self._column

        number_value, length, token_type = self._read_number(
            self._expression, self._position, start_line, start_col
        )
        self._tokens.append(AIFPLToken(token_type, number_value, length, start_line, start_col))
        self._column += length
        self._position += length

    def _handle_symbol(self) -> None:
        """Handle symbols (variables, parameters, functions, constants)."""
        start_line = self._line
        start_col = self._column

        i = self._position

        while i < len(self._expression):
            char = self._expression[i]

            # Symbol characters: letters, digits, hyphens, and operator chars
            if not char.isalnum() and char not in '-+*/%<>=!&|^~?_.':
                break

            i += 1

        if i < len(self._expression):
            char = self._expression[i]
            if not self._is_delimiter(char):
                char_code = ord(char)
                if char_code < 32:
                    char_display = f"\\u{char_code:04x}"
                    raise AIFPLTokenError(
                        message=f"Invalid control character in source code: {char_display}",
                        line=start_line,
                        column=start_col,
                        received=f"Control character: {char_display} (code {char_code})",
                        expected="Valid AIFPL characters or escape sequences in strings",
                        example='Valid: "hello\\nworld" (newline in string)\\nInvalid: hello<ctrl-char>world',
                        suggestion="Remove the control character or use escape sequences like \\n, \\t, or \\uXXXX in strings",
                        context="Control characters are not allowed in source code. Use escape "
                            "sequences like \\n, \\t, or \\uXXXX in strings."
                    )

                raise AIFPLTokenError(
                    message=f"Invalid character in symbol: {char}",
                    line=self._line,
                    column=i,
                    received=f"Expression: {self._expression[self._position:i+1]}",
                    expected="Valid symbol characters: letters, digits, and specific symbols",
                    suggestion=f"Remove or replace invalid character '{char}' in symbol"
                )

        symbol = self._expression[self._position:i]
        length = i - self._position
        self._tokens.append(AIFPLToken(AIFPLTokenType.SYMBOL, symbol, length, start_line, start_col))
        self._column += length
        self._position += length

    def _handle_invalid_char(self) -> None:
        """Handle invalid character with helpful error message."""
        char = self._expression[self._position]
        char_code = ord(char)

        # Control characters
        if char_code < 32:
            char_display = f"\\u{char_code:04x}"
            raise AIFPLTokenError(
                message=f"Invalid control character in source code: {char_display}",
                line=self._line,
                column=self._column,
                received=f"Control character: {char_display} (code {char_code})",
                expected="Valid AIFPL characters or escape sequences in strings",
                example='Valid: "hello\\nworld" (newline in string)\\nInvalid: hello<ctrl-char>world',
                suggestion="Remove the control character or use escape sequences like \\n, \\t, or \\uXXXX in strings",
                context="Control characters are not allowed in source code. Use escape "
                    "sequences like \\n, \\t, or \\uXXXX in strings."
            )

        # Other invalid characters - provide helpful suggestions
        suggestions = {
            '@': "@ is not valid in AIFPL - use symbols like 'at' or 'email'",
            '$': "$ is not valid in AIFPL - use symbols like 'dollar' or 'var'",
            '&': "Use 'and' for boolean operations, not &",
            '|': "Use 'or' for boolean operations, not |",
            '[': "Use parentheses ( ) for lists, not brackets [ ]",
            ']': "Use parentheses ( ) for lists, not brackets [ ]",
            '{': "Use parentheses ( ) for all grouping, not braces { }",
            '}': "Use parentheses ( ) for all grouping, not braces { }",
        }

        suggestion = suggestions.get(char, f"'{char}' is not a valid character in AIFPL")
        context = "Only letters, digits, and specific symbols are allowed"

        raise AIFPLTokenError(
            message=f"Invalid character: {char}",
            line=self._line,
            column=self._column,
            received=f"Character: {char} (code {char_code})",
            expected="Valid AIFPL characters: letters, digits, +, -, *, /, etc.",
            example="Valid: (+ 1 2), my-var, func?\\nInvalid: @var, $value, [list]",
            suggestion=suggestion,
            context=context
        )

    def _read_string(self, expression: str, start: int) -> tuple[str, int]:
        """
        Read a string literal from the expression.

        Returns:
            Tuple of (string_value, length_consumed)

        Raises:
            AIFPLTokenError: If string is malformed
        """
        i = start + 1  # Skip opening quote
        result: list[str] = []

        while i < len(expression):
            char = expression[i]

            # End of string
            if char == '"':
                i += 1  # Skip closing quote
                return ''.join(result), i - start

            # Escape sequences
            if char == '\\':
                if i + 1 >= len(expression):
                    raise AIFPLTokenError(f"Unterminated escape sequence at position {i}")

                next_char = expression[i + 1]

                if next_char == '"':
                    result.append('"')

                elif next_char == '\\':
                    result.append('\\')

                elif next_char == 'n':
                    result.append('\n')

                elif next_char == 't':
                    result.append('\t')

                elif next_char == 'r':
                    result.append('\r')

                elif next_char == 'u':
                    # Unicode escape sequence \uXXXX
                    if i + 5 >= len(expression):
                        raise AIFPLTokenError(f"Incomplete Unicode escape sequence at position {i}")

                    hex_digits = expression[i + 2:i + 6]
                    if not all(c in '0123456789abcdefABCDEF' for c in hex_digits):
                        raise AIFPLTokenError(f"Invalid Unicode escape sequence at position {i}: \\u{hex_digits}")

                    code_point = int(hex_digits, 16)
                    result.append(chr(code_point))
                    i += 4  # Skip the extra 4 characters (uXXXX)

                else:
                    raise AIFPLTokenError(f"Invalid escape sequence at position {i}: \\{next_char}")

                i += 2  # Skip escape sequence
                continue

            # Regular character
            result.append(char)
            i += 1

        raise AIFPLTokenError(f"Unterminated string literal starting at position {start}")

    def _read_hash_number(
        self,
        expression: str,
        start: int,
        start_line: int,
        start_col: int
    ) -> tuple[int, int, AIFPLTokenType]:
        """
        Read a Scheme-style hex/binary/octal number literal (#xFF, #b1010, #o755).

        Returns:
            Tuple of (number_value, length_consumed, token_type)

        Raises:
            AIFPLTokenError: If the token is not a valid number
        """
        # Must start with #
        if expression[start] != '#':
            raise AIFPLTokenError(
                message="Internal error: _read_hash_number called without #",
                line=start_line,
                column=start_col,
                received=expression[start],
                expected="#"
            )

        # Need at least 3 characters: #, format char, and one digit
        if start + 2 >= len(expression):
            raise AIFPLTokenError(
                message=f"Incomplete number literal: {expression[start:]}",
                line=start_line,
                column=start_col,
                received=expression[start:],
                expected="Complete hex/binary/octal literal",
                example="Valid: #xFF, #b1010, #o755"
            )

        format_char = expression[start + 1].lower()

        # Read digits until delimiter
        i = start + 2
        while i < len(expression) and not self._is_delimiter(expression[i]):
            i += 1

        digits = expression[start + 2:i]

        if not digits:
            raise AIFPLTokenError(
                message=f"Missing digits after #{format_char}",
                line=start_line,
                column=start_col,
                received=expression[start:i],
                expected=f"Digits after #{format_char}",
                example=f"Valid: #{format_char}FF, -#{format_char}FF (negative)"
            )

        # Parse based on format
        try:
            if format_char == 'x':
                # Hexadecimal
                if not all(c in '0123456789abcdefABCDEF' for c in digits):
                    raise AIFPLTokenError(
                        message=f"Invalid hexadecimal digits: {digits}",
                        line=start_line,
                        column=start_col,
                        received=f"#{format_char}{digits}",
                        expected="Hexadecimal digits (0-9, A-F)",
                        example="Valid: #xFF, -#xFF, #x2A, #xDEADBEEF"
                    )
                value = int(digits, 16)

            elif format_char == 'b':
                # Binary
                if not all(c in '01' for c in digits):
                    raise AIFPLTokenError(
                        message=f"Invalid binary digits: {digits}",
                        line=start_line,
                        column=start_col,
                        received=f"#{format_char}{digits}",
                        expected="Binary digits (0-1)",
                        example="Valid: #b1010, -#b1010, #b11111111"
                    )
                value = int(digits, 2)

            elif format_char == 'o':
                # Octal
                if not all(c in '01234567' for c in digits):
                    raise AIFPLTokenError(
                        message=f"Invalid octal digits: {digits}",
                        line=start_line,
                        column=start_col,
                        received=f"#{format_char}{digits}",
                        expected="Octal digits (0-7)",
                        example="Valid: #o755, -#o755, #o644"
                    )
                value = int(digits, 8)

            else:
                raise AIFPLTokenError(
                    message=f"Invalid number format: #{format_char}",
                    line=start_line,
                    column=start_col,
                    received=f"#{format_char}",
                    expected="#x (hex), #b (binary), or #o (octal)",
                    example="Valid: #xFF, #b1010, #o755"
                )

        except ValueError as e:
            raise AIFPLTokenError(
                message=f"Invalid number literal: #{format_char}{digits}",
                line=start_line,
                column=start_col,
                received=f"#{format_char}{digits}",
                expected="Valid number digits",
                context=str(e)
            ) from e

        length = i - start
        return value, length, AIFPLTokenType.INTEGER

    def _read_number(
        self,
        expression: str,
        start: int,
        start_line: int,
        start_col: int
    ) -> tuple[Union[int, float, complex], int, AIFPLTokenType]:
        """
        Read a number literal (including complex) from the expression.

        Returns:
            Tuple of (number_value, length_consumed, token_type)

        Raises:
            AIFPLTokenError: If the token is not a valid number
        """
        # Get the complete token until delimiter (this will check for control characters)
        i = start
        curr_col = start_col

        # Consume characters until we hit a delimiter
        while i < len(expression):
            char = expression[i]

            # Check for control characters before processing
            char_code = ord(char)

            # Control characters are ASCII < 32 (excluding whitespace which is handled separately)
            if char_code < 32 and not char.isspace():
                char_display = f"\\u{char_code:04x}"
                raise AIFPLTokenError(
                    message=f"Invalid control character in source code: {char_display}",
                    line=start_line,
                    column=curr_col,
                    received=f"Control character: {char_display} (code {char_code})",
                    expected="Valid AIFPL characters or escape sequences in strings",
                    example='Valid: "hello\\nworld" (newline in string)\\nInvalid: hello<ctrl-char>world',
                    suggestion="Remove the control character or use escape sequences like \\n, \\t, or \\uXXXX in strings",
                    context="Control characters are not allowed in source code. Use escape "
                        "sequences like \\n, \\t, or \\uXXXX in strings."
                )

            if self._is_delimiter(char):
                break

            i += 1
            curr_col += 1

        complete_token = expression[start:i]

        # Check if this is a Scheme-style hex/bin/oct literal (#xFF, -#xFF, etc.)
        # This handles both positive (#xFF) and negative (-#xFF) cases
        hash_pos = complete_token.find('#')
        if hash_pos != -1:
            # Found a # in the token - it should be a Scheme-style number
            # The # should be at position 0 (positive) or 1 (negative with -)
            if hash_pos <= 1 and hash_pos + 1 < len(complete_token):
                format_char = complete_token[hash_pos + 1]
                if format_char in 'xXbBoO':
                    # Delegate to _read_hash_number, adjusting for potential negative sign
                    hash_start = start + hash_pos
                    value, _, token_type = self._read_hash_number(expression, hash_start, start_line, start_col + hash_pos)

                    # Apply negative sign if present
                    if hash_pos == 1 and complete_token[0] == '-':
                        value = -value

                    return value, len(complete_token), token_type

        # Check if this is a complex number literal - must have 'j' or 'J' at the end
        last_char = complete_token[-1]
        if last_char in ('j', 'J'):
            complex_value = self._parse_complex_literal(complete_token, start_line, start_col)
            return complex_value, len(complete_token), AIFPLTokenType.COMPLEX

        # Validate that this token is a valid real number
        try:
            float(complete_token)

        except ValueError:
            # Not a valid number but the ValueError exception isn't interesting to propagate
            raise AIFPLTokenError(
                message=f"Invalid number format: {complete_token}",
                line=start_line,
                column=start_col,
                received=f"Malformed number token: {complete_token}",
                expected="Valid number format",
                suggestion=f"Fix the number format: {complete_token}",
                context="Token appears to be a number but contains invalid characters",
                example="Valid: 1.23, .5, 42, 1e-10, 3+4j. For hex/binary/octal use: #xFF, #b1010, #o755"
            ) from None

        # Parse the valid number
        number_value: int | float
        if '.' in complete_token or 'e' in complete_token.lower():
            number_value = float(complete_token)
            token_type = AIFPLTokenType.FLOAT

        else:
            number_value = int(complete_token)
            token_type = AIFPLTokenType.INTEGER

        return number_value, len(complete_token), token_type

    def _is_delimiter(self, char: str) -> bool:
        """Check if character is a token delimiter."""
        return char.isspace() or char in "()'\";,"

    def _parse_complex_literal(self, token: str, start_line: int, start_col: int) -> complex:
        """
        Parse a complex number literal.

        Supported formats:
        - 4j → 4j
        - -5j → -5j
        - 3+4j → (3+4j)
        - 3-4j → (3-4j)
        - 1.5e2+3.7e-1j → (150+0.37j)

        Args:
            token: The complete token string
            start_line: Line number where token starts
            start_col: Column number where token starts

        Returns:
            Complex number value

        Raises:
            AIFPLTokenError: If the complex literal is malformed
        """
        token_without_j = token[:-1]

        # Try to find the + or - that separates real and imaginary parts
        separator_pos = -1
        i = 0
        if token and token[0] in '+-':
            i = 1  # Skip leading sign

        while i < len(token):
            char = token[i]

            # Check if this is a separator (not part of scientific notation)
            if char in '+-':
                # It's a separator if it's not immediately after 'e' or 'E'
                if i > 0 and token[i-1].lower() != 'e':
                    separator_pos = i
                    break

            i += 1

        if separator_pos == -1:
            # Pure imaginary number (e.g., "4j", "-5j")
            try:
                imag_part = float(token_without_j)
                return complex(0, imag_part)

            except ValueError as e:
                raise AIFPLTokenError(
                    message=f"Invalid imaginary part: {token_without_j}",
                    line=start_line,
                    column=start_col,
                    received=f"Imaginary part: {token_without_j}",
                    expected="Valid number format",
                    example="Valid: 4j, -5j, 1.5e2j",
                    suggestion="Check the number format before 'j'",
                    context="The imaginary part must be a valid number"
                ) from e

        # Complex number with both real and imaginary parts
        real_part_str = token_without_j[:separator_pos]
        imag_part_str = token_without_j[separator_pos:]  # Includes the +/- sign

        try:
            real_part = float(real_part_str) if real_part_str else 0
            imag_part = float(imag_part_str)
            return complex(real_part, imag_part)

        except ValueError as e:
            raise AIFPLTokenError(
                message=f"Invalid complex literal: {token}",
                line=start_line,
                column=start_col,
                received=f"Token: {token}",
                expected="Valid complex number format",
                example="Valid: 3+4j, 2-5j, 1.5e2+3.7e-1j",
                suggestion="Check both real and imaginary parts are valid numbers",
                context=f"Parse error: {str(e)}"
            ) from e
