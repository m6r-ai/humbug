"""Tokenizer for AIFPL expressions with detailed error messages."""

from typing import List, Union, Any

from aifpl.aifpl_error import AIFPLTokenError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


class AIFPLTokenizer:
    """Tokenizes AIFPL expressions into tokens with detailed error messages."""

    def tokenize(self, expression: str) -> List[AIFPLToken]:
        """
        Tokenize an AIFPL expression with detailed error reporting.

        Args:
            expression: The expression string to tokenize

        Returns:
            List of tokens

        Raises:
            AIFPLTokenError: If tokenization fails with detailed context
        """
        tokens = []
        i = 0
        line = 1  # Current line number (1-indexed)
        column = 1  # Current column number (1-indexed)

        def make_token(
            token_type: AIFPLTokenType,
            value: Any,
            length: int = 1,
            token_line: int | None = None,
            token_col: int | None = None
        ) -> AIFPLToken:
            """Helper to create token with line/column info."""
            # Use current line/column if not explicitly provided
            tl = token_line if token_line is not None else line
            tc = token_col if token_col is not None else column
            return AIFPLToken(token_type, value, length, tl, tc)

        def advance_position(char: str) -> None:
            """Update line/column based on character."""
            nonlocal line, column
            if char == '\n':
                line += 1
                column = 1

            else:
                column += 1

        while i < len(expression):
            next_char = expression[i]

            # Skip whitespace
            if next_char.isspace():
                advance_position(next_char)
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
                tokens.append(make_token(AIFPLTokenType.LPAREN, '('))
                column += 1
                i += 1
                continue

            if next_char == ')':
                tokens.append(make_token(AIFPLTokenType.RPAREN, ')'))
                column += 1
                i += 1
                continue

            # Quote character
            if next_char == "'":
                tokens.append(make_token(AIFPLTokenType.QUOTE, "'"))
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
                    tokens.append(make_token(AIFPLTokenType.STRING, string_value, length, string_line, string_col))
                    # Advance position for each character in the string
                    for j in range(length):
                        advance_position(expression[i + j])

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
                if expression[i + 1] in 'tf':
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
                    tokens.append(make_token(AIFPLTokenType.BOOLEAN, boolean_value, 2))
                    column += 2
                    i += 2
                    continue

                # Invalid # sequence
                invalid_char = expression[i + 1] if i + 1 < len(expression) else ""
                raise AIFPLTokenError(
                    message=f"Invalid boolean literal: #{invalid_char}",
                    line=line,
                    column=column,
                    received=f"Found: #{invalid_char}",
                    expected="Valid boolean: #t or #f",
                    example="Correct: #t (true), #f (false)\\nIncorrect: #x, #1, #true",
                    suggestion="Use #t for true or #f for false",
                    context="# symbol must be followed by 't' or 'f' for booleans"
                )

            # Numbers (including complex, hex, binary, octal, scientific notation)
            # Check numbers BEFORE symbols to handle .5 correctly
            if self._is_number_start(expression, i):
                try:
                    # Save line/column at start of number
                    number_line = line
                    number_col = column
                    number_value, length, token_type = self._read_number(expression, i, number_line, number_col)
                    tokens.append(make_token(token_type, number_value, length, number_line, number_col))
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
                tokens.append(make_token(AIFPLTokenType.SYMBOL, symbol, length, symbol_line, symbol_col))
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
            # Negative digit or negative decimal starting with dot (only if followed by digit)
            if next_char.isdigit() or (next_char == '.' and pos + 2 < len(expression) and expression[pos + 2].isdigit()):
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

    def _check_for_control_character(self, char: str, line: int, column: int) -> None:
        """
        Check if a character is a control character and raise an error if so.

        Args:
            char: Character to check
            line: Line number in the expression
            column: Column number in the expression

        Raises:
            AIFPLTokenError: If the character is a control character
        """
        char_code = ord(char)

        # Control characters are ASCII < 32 (excluding whitespace which is handled separately)
        if char_code < 32 and not char.isspace():
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

    def _read_complete_token(self, expression: str, start: int, start_line: int, start_col: int) -> str:
        """
        Read a complete token until delimiter, following LISP tokenization rules.

        Validates that no control characters are present in the token.

        Returns:
            The complete token string

        Raises:
            AIFPLTokenError: If a control character is encountered
        """
        i = start
        curr_line = start_line
        curr_col = start_col

        # Consume characters until we hit a delimiter
        while i < len(expression):
            char = expression[i]

            # Check for control characters before processing
            self._check_for_control_character(char, curr_line, curr_col)

            # Update line/column for next iteration
            if char == '\n':
                curr_line += 1
                curr_col = 1

            else:
                curr_col += 1

            if self._is_delimiter(char):
                break

            i += 1

        return expression[start:i]

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
            # We don't need to worry about just '-' being a number, as that would be caught earlier
            check_token = token[1:]

        # Try different number formats

        # Hexadecimal
        if first_char == '0' and len(check_token) > 1:
            second_char = check_token[1]
            if second_char in ('x', 'X'):
                if len(check_token) <= 2:
                    return False

                hex_part = check_token[2:]
                return all(c in '0123456789abcdefABCDEF' for c in hex_part)

            # Binary
            if second_char in ('b', 'B'):
                if len(check_token) <= 2:
                    return False

                bin_part = check_token[2:]
                return all(c in '01' for c in bin_part)

            # Octal
            if second_char in ('o', 'O'):
                if len(check_token) <= 2:
                    return False

                oct_part = check_token[2:]
                return all(c in '01234567' for c in oct_part)

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

        # Parse different formats
        value: int | float
        if first_char == '0' and len(check_token) > 1:
            second_char = check_token[1]
            if second_char in ('x', 'X'):
                value = int(check_token, 16)
                return -value if negative else value

            if second_char in ('b', 'B'):
                value = int(check_token, 2)
                return -value if negative else value

            if second_char in ('o', 'O'):
                value = int(check_token, 8)
                return -value if negative else value

        # Decimal number - use float if it contains . or e/E, otherwise int
        if '.' in check_token or 'e' in check_token.lower():
            value = float(check_token)

        else:
            value = int(check_token)

        return -value if negative else value

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
        complete_token = self._read_complete_token(expression, start, start_line, start_col)

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
                example="Valid: 1.23, .5, 42, 1e-10, 0xFF, 3+4j"
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
            AIFPLTokenError: If a control character is encountered
        """
        i = start
        curr_line = start_line
        curr_col = start_col

        while i < len(expression):
            char = expression[i]

            # Symbol characters: letters, digits, hyphens, and operator chars
            if char.isalnum() or char in '-+*/%<>=!&|^~?_.':
                # Check for control characters before adding to symbol
                self._check_for_control_character(char, curr_line, curr_col)

                # Update line/column for next iteration
                if char == '\n':
                    curr_line += 1
                    curr_col = 1

                else:
                    curr_col += 1

                i += 1

            else:
                break

        symbol = expression[start:i]
        return symbol, i - start
