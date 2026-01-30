"""Tokenizer for AIFPL expressions with detailed error messages."""

from typing import List, Union

from aifpl.aifpl_error import AIFPLTokenError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


class AIFPLLexer:
    """Lexes AIFPL expressions into tokens with detailed error messages."""

    def lex(self, expression: str) -> List[AIFPLToken]:
        """
        Lex an AIFPL expression with detailed error reporting.

        Args:
            expression: The expression string to lex

        Returns:
            List of tokens

        Raises:
            AIFPLTokenError: If tokenization fails with detailed context
        """
        tokens = []
        i = 0
        line = 1  # Current line number (1-indexed)
        column = 1  # Current column number (1-indexed)

        while i < len(expression):
            next_char = expression[i]

            # Skip newlines
            if next_char == '\n':
                line += 1
                column = 1
                i += 1
                continue

            # Skip whitespace
            if next_char.isspace():
                column += 1
                i += 1
                continue

            # Comments - skip from ';' to end of line
            if next_char == ';':
                while i < len(expression) and expression[i] != '\n':
                    column += 1
                    i += 1

                continue

            # Parentheses
            if next_char == '(':
                tokens.append(AIFPLToken(AIFPLTokenType.LPAREN, '(', 1, line, column))
                column += 1
                i += 1
                continue

            if next_char == ')':
                tokens.append(AIFPLToken(AIFPLTokenType.RPAREN, ')', 1, line, column))
                column += 1
                i += 1
                continue

            # Quote character
            if next_char == "'":
                tokens.append(AIFPLToken(AIFPLTokenType.QUOTE, "'", 1, line, column))
                column += 1
                i += 1
                continue

            # String literals
            if next_char == '"':
                try:
                    # Save line/column at start of string
                    string_line = line
                    string_col = column
                    string_value, length = self._read_string(expression, i)
                    tokens.append(AIFPLToken(AIFPLTokenType.STRING, string_value, length, string_line, string_col))

                    # Advance position for each character in the string
                    for j in range(length):
                        if expression[i + j] == '\n':
                            line += 1
                            column = 1

                        else:
                            column += 1

                    i += length
                    continue

                except AIFPLTokenError as e:
                    # Convert to detailed error
                    if "Unterminated string" in str(e):
                        raise AIFPLTokenError(
                            message="Unterminated string literal",
                            line=line,
                            column=column,
                            received=f"String starting with: {expression[i:i+10]}...",
                            expected="Closing quote \" at end of string",
                            example='Correct: "hello world"\\nIncorrect: "hello world',
                            suggestion="Add closing quote \" at the end of the string",
                            context="String literals must be enclosed in double quotes"
                        ) from e

                    if "Invalid escape sequence" in str(e):
                        # Find the escape position
                        escape_pos = i + 1
                        escape_line = line
                        escape_col = column
                        while escape_pos < len(expression) and expression[escape_pos] != '\\':
                            if expression[escape_pos] == '\\n':
                                escape_line += 1
                                escape_col = 1

                            else:
                                escape_col += 1

                            escape_pos += 1

                        bad_escape = expression[escape_pos:escape_pos+2]
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

            # Boolean literals (#t, #f) with validation for invalid patterns like #true, #false
            if next_char == '#' and i + 1 < len(expression):
                next_after_hash = expression[i + 1]
                if next_after_hash in 'tf':
                    # Check if this is part of a longer invalid sequence like #true or #false
                    if i + 2 < len(expression) and not self._is_delimiter(expression[i + 2]):
                        # Find end of the invalid sequence
                        end = i + 2
                        while end < len(expression) and expression[end].isalnum():
                            end += 1

                        invalid_literal = expression[i:end]
                        raise AIFPLTokenError(
                            message=f"Invalid boolean literal: {invalid_literal}",
                            line=line,
                            column=column,
                            received=f"Boolean literal: {invalid_literal}",
                            expected="Valid boolean: #t or #f",
                            example="Correct: #t, #f\\nIncorrect: #true, #false, #T, #F",
                            suggestion="Use #t for true or #f for false",
                            context="AIFPL uses #t and #f for boolean values"
                        )

                    boolean_value = expression[i + 1] == 't'
                    tokens.append(AIFPLToken(AIFPLTokenType.BOOLEAN, boolean_value, 2, line, column))
                    column += 2
                    i += 2
                    continue

                # Hexadecimal, binary, or octal literals (#xFF, #b1010, #o755)
                if next_after_hash in 'xXbBoO':
                    try:
                        # Save line/column at start of number
                        number_line = line
                        number_col = column
                        number_value, length, token_type = self._read_hash_number(expression, i, number_line, number_col)
                        tokens.append(AIFPLToken(token_type, number_value, length, number_line, number_col))
                        column += length
                        i += length
                        continue

                    except AIFPLTokenError as e:
                        raise e

                # Invalid # sequence - provide helpful error message
                invalid_char = next_after_hash

                # Check if it looks like they tried Python-style 0x/0b/0o
                suggestion = "Use #t for true or #f for false"
                if invalid_char.isdigit():
                    suggestion = "For hex/binary/octal use #x, #b, or #o prefix (e.g., #xFF, #b1010, #o755)"

                elif invalid_char in 'xXbBoO':
                    suggestion = f"Use #{invalid_char} followed by digits (e.g., #xFF, #b1010, #o755)"

                raise AIFPLTokenError(
                    message=f"Invalid # literal: #{invalid_char}",
                    line=line,
                    column=column,
                    received=f"Found: #{invalid_char}",
                    expected="Valid # literal: #t, #f, #xFF, #b1010, #o755",
                    example="Correct: #t, #f, #xFF, #b1010, #o755\\nIncorrect: #true, #1, 0xFF",
                    suggestion=suggestion,
                    context="# must be followed by: 't'/'f' (boolean), 'x'/'X' (hex), 'b'/'B' (binary), or 'o'/'O' (octal)"
                )

            # Numbers (including complex, hex, binary, octal, scientific notation)
            # Check numbers BEFORE symbols to handle .5 correctly
            if self._is_number_start(expression, i):
                try:
                    # Save line/column at start of number
                    number_line = line
                    number_col = column
                    number_value, length, token_type = self._read_number(expression, i, number_line, number_col)
                    tokens.append(AIFPLToken(token_type, number_value, length, number_line, number_col))
                    column += length
                    i += length
                    continue

                except AIFPLTokenError as e:
                    raise e

            # Symbols (variables, parameters, functions, constants)
            if self._is_symbol_start(next_char):
                # Save line/column at start of symbol
                symbol_line = line
                symbol_col = column
                symbol, length = self._read_symbol(expression, i, symbol_line, symbol_col)
                tokens.append(AIFPLToken(AIFPLTokenType.SYMBOL, symbol, length, symbol_line, symbol_col))
                column += length
                i += length
                continue

            # Invalid character - check for control characters first
            char_code = ord(next_char)

            if char_code < 32:
                # Control character - provide specific error
                char_display = f"\\u{char_code:04x}"
                raise AIFPLTokenError(
                    message=f"Invalid control character in source code: {char_display}",
                    line=line,
                    column=column,
                    received=f"Control character: {char_display} (code {char_code})",
                    expected="Valid AIFPL characters or escape sequences in strings",
                    example='Valid: "hello\\nworld" (newline in string)\\nInvalid: hello<ctrl-char>world',
                    suggestion="Remove the control character or use escape sequences like \\n, \\t, or \\uXXXX in strings",
                    context="Control characters are not allowed in source code. Use escape "
                        "sequences like \\n, \\t, or \\uXXXX in strings."
                )

            # Other invalid characters
            # Provide helpful suggestions for common mistakes
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

            suggestion = suggestions.get(next_char, f"'{next_char}' is not a valid character in AIFPL")
            context = "Only letters, digits, and specific symbols are allowed"

            raise AIFPLTokenError(
                message=f"Invalid character: {next_char}",
                line=line,
                column=column,
                received=f"Character: {next_char} (code {char_code})",
                expected="Valid AIFPL characters: letters, digits, +, -, *, /, etc.",
                example="Valid: (+ 1 2), my-var, func?\\nInvalid: @var, $value, [list]",
                suggestion=suggestion,
                context=context
            )

        return tokens

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

    def _is_number_start(self, expression: str, pos: int) -> bool:
        """Check if position starts a number literal."""
        char = expression[pos]

        # Standard number starts
        if char.isdigit():
            return True

        # Decimal numbers starting with a dot (like .5) - ONLY if followed by digit
        if char == '.' and pos + 1 < len(expression) and expression[pos + 1].isdigit():
            return True

        # Negative numbers
        if char == '-' and pos + 1 < len(expression):
            next_char = expression[pos + 1]
            # Negative digit, negative decimal, or negative Scheme-style hex/bin/oct (-#xFF, -#b1010, -#o755)
            if next_char.isdigit():
                return True
            if next_char == '.' and pos + 2 < len(expression) and expression[pos + 2].isdigit():
                return True
            if next_char == '#' and pos + 2 < len(expression) and expression[pos + 2] in 'xXbBoO':
                return True

        # Positive numbers (explicit + sign)
        if char == '+' and pos + 1 < len(expression):
            next_char = expression[pos + 1]
            # Positive digit or positive decimal starting with dot (only if followed by digit)
            if next_char.isdigit() or (next_char == '.' and pos + 2 < len(expression) and expression[pos + 2].isdigit()):
                return True

        return False

    def _is_delimiter(self, char: str) -> bool:
        """Check if character is a LISP token delimiter."""
        return char.isspace() or char in "()'\";,"

    def _is_valid_number(self, token: str) -> bool:
        """
        Check if a complete token is a valid number format.

        Args:
            token: The complete token string to validate

        Returns:
            True if the token represents a valid number
        """
        # Handle negative numbers
        first_char = token[0]
        check_token = token
        if first_char == '-':
            check_token = token[1:]

        # Decimal numbers (int, float, scientific notation)
        try:
            float(check_token)
            return True

        except ValueError:
            return False

    def _parse_number_value(self, token: str) -> int | float:
        """
        Parse a valid number token into its numeric value.

        Args:
            token: The complete valid number token

        Returns:
            The numeric value
        """
        # Handle negative numbers
        first_char = token[0]
        check_token = token
        negative = first_char == '-'
        if negative:
            check_token = token[1:]

        # Decimal number - use float if it contains . or e/E, otherwise int
        value: int | float
        if '.' in check_token or 'e' in check_token.lower():
            value = float(check_token)

        else:
            value = int(check_token)

        return -value if negative else value

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
            )

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
                format_char = complete_token[hash_pos + 1].lower()
                if format_char in 'xbo':
                    # Delegate to _read_hash_number, adjusting for potential negative sign
                    hash_start = start + hash_pos
                    value, hash_length, token_type = self._read_hash_number(expression, hash_start, start_line, start_col + hash_pos)

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
        if not self._is_valid_number(complete_token):
            raise AIFPLTokenError(
                message=f"Invalid number format: {complete_token}",
                line=start_line,
                column=start_col,
                received=f"Malformed number token: {complete_token}",
                expected="Valid number format",
                suggestion=f"Fix the number format: {complete_token}",
                context="Token appears to be a number but contains invalid characters",
                example="Valid: 1.23, .5, 42, 1e-10, 3+4j. For hex/binary/octal use: #xFF, #b1010, #o755"
            )

        # Parse the valid number
        number_value = self._parse_number_value(complete_token)

        # Determine token type based on Python type
        if isinstance(number_value, int):
            token_type = AIFPLTokenType.INTEGER

        else:
            token_type = AIFPLTokenType.FLOAT

        return number_value, len(complete_token), token_type

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
        separator_pos = self._find_complex_separator(token_without_j)

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

    def _find_complex_separator(self, token: str) -> int:
        """
        Find the position of + or - that separates real and imaginary parts.

        Must handle scientific notation correctly (e.g., "1e-10+2" should find the +, not the -)

        Examples:
        - "3+4" → 1 (position of +)
        - "3-4" → 1 (position of -)
        - "1e-10+2" → 5 (position of +, not the - in e-10)
        - "1.5e2+3.7e-1" → 5 (position of +)
        - "4" → -1 (no separator, pure imaginary)
        - "-5" → -1 (leading -, not separator)

        Args:
            token: Token without the trailing 'j'

        Returns:
            Position of separator, or -1 if not found (pure imaginary)
        """
        # Scan from left to right, skip over scientific notation
        i = 0
        if token and token[0] in '+-':
            i = 1  # Skip leading sign

        while i < len(token):
            char = token[i]

            # Check if this is a separator (not part of scientific notation)
            if char in '+-':
                # It's a separator if it's not immediately after 'e' or 'E'
                if i > 0 and token[i-1].lower() != 'e':
                    return i

            i += 1

        return -1  # No separator found

    def _is_symbol_start(self, char: str) -> bool:
        """Check if character can start a symbol."""
        return char.isalpha() or char in '+-*/%<>=!&|^~_.'

    def _read_symbol(self, expression: str, start: int, start_line: int, start_col: int) -> tuple[str, int]:
        """
        Read a symbol from the expression.

        Validates that no control characters are present in the symbol.

        Returns:
            Tuple of (symbol_string, length_consumed)

        Raises:
            AIFPLTokenError: If an invalid character is encountered
        """
        i = start

        while i < len(expression):
            char = expression[i]

            # Symbol characters: letters, digits, hyphens, and operator chars
            if not char.isalnum() and char not in '-+*/%<>=!&|^~?_.':
                break

            i += 1

        if i < len(expression):
            char = expression[i]
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
                    line=start_line,
                    column=i,
                    received=f"Expression: {expression[start:i+1]}",
                    expected="Valid symbol characters: letters, digits, and specific symbols",
                    suggestion=f"Remove or replace invalid character '{char}' in symbol"
                )

        symbol = expression[start:i]
        return symbol, i - start
