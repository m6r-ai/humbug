"""Tokenizer for AIFPL expressions with detailed error messages."""

from typing import List, Union
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

        while i < len(expression):
            # Skip whitespace
            if expression[i].isspace():
                i += 1
                continue

            # Comments - skip from ';' to end of line
            if expression[i] == ';':
                while i < len(expression) and expression[i] != '\n':
                    i += 1

                continue

            # Parentheses
            if expression[i] == '(':
                tokens.append(AIFPLToken(AIFPLTokenType.LPAREN, '(', i))
                i += 1
                continue

            if expression[i] == ')':
                tokens.append(AIFPLToken(AIFPLTokenType.RPAREN, ')', i))
                i += 1
                continue

            # Quote character
            if expression[i] == "'":
                tokens.append(AIFPLToken(AIFPLTokenType.QUOTE, "'", i))
                i += 1
                continue

            # String literals
            if expression[i] == '"':
                try:
                    string_value, length = self._read_string(expression, i)
                    tokens.append(AIFPLToken(AIFPLTokenType.STRING, string_value, i, length))
                    i += length
                    continue

                except AIFPLTokenError as e:
                    # Convert to detailed error
                    if "Unterminated string" in str(e):
                        raise AIFPLTokenError(
                            message="Unterminated string literal",
                            position=i,
                            received=f"String starting with: {expression[i:i+10]}...",
                            expected="Closing quote \" at end of string",
                            example='Correct: "hello world"\\nIncorrect: "hello world',
                            suggestion="Add closing quote \" at the end of the string",
                            context="String literals must be enclosed in double quotes"
                        ) from e

                    if "Invalid escape sequence" in str(e):
                        # Find the escape position
                        escape_pos = i + 1
                        while escape_pos < len(expression) and expression[escape_pos] != '\\':
                            escape_pos += 1

                        bad_escape = expression[escape_pos:escape_pos+2]
                        raise AIFPLTokenError(
                            message=f"Invalid escape sequence: {bad_escape}",
                            position=escape_pos,
                            received=f"Escape sequence: {bad_escape}",
                            expected="Valid escape: \\n, \\t, \\r, \\\", \\\\, or \\uXXXX",
                            example='Valid: "line1\\nline2" or "tab\\there"\\nInvalid: "bad\\qsequence"',
                            suggestion="Use valid escape sequences or remove backslash",
                            context="Only specific escape sequences are supported in strings"
                        ) from e

                    raise  # Re-raise if not handled

            # Boolean literals (#t, #f) with validation for invalid patterns like #true, #false
            if expression[i] == '#' and i + 1 < len(expression):
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
                            position=i,
                            received=f"Boolean literal: {invalid_literal}",
                            expected="Valid boolean: #t or #f",
                            example="Correct: #t, #f\\nIncorrect: #true, #false, #T, #F",
                            suggestion="Use #t for true or #f for false",
                            context="AIFPL uses #t and #f for boolean values"
                        )

                    boolean_value = expression[i + 1] == 't'
                    tokens.append(AIFPLToken(AIFPLTokenType.BOOLEAN, boolean_value, i, 2))
                    i += 2
                    continue

                # Invalid # sequence
                invalid_char = expression[i + 1] if i + 1 < len(expression) else ""
                raise AIFPLTokenError(
                    message=f"Invalid boolean literal: #{invalid_char}",
                    position=i,
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
                    number, length = self._read_number(expression, i)
                    tokens.append(AIFPLToken(AIFPLTokenType.NUMBER, number, i, length))
                    i += length
                    continue

                except AIFPLTokenError as e:
                    raise e

            # Symbols (variables, parameters, functions, constants)
            if self._is_symbol_start(expression[i]):
                symbol, length = self._read_symbol(expression, i)
                tokens.append(AIFPLToken(AIFPLTokenType.SYMBOL, symbol, i, length))
                i += length
                continue

            # Invalid character - check for control characters first
            char = expression[i]
            char_code = ord(char)

            if char_code < 32:
                # Control character - provide specific error
                char_display = f"\\u{char_code:04x}"
                raise AIFPLTokenError(
                    message=f"Invalid control character in source code: {char_display}",
                    position=i,
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

            suggestion = suggestions.get(char, f"'{char}' is not a valid character in AIFPL")
            context = "Only letters, digits, and specific symbols are allowed"

            raise AIFPLTokenError(
                message=f"Invalid character: {char}",
                position=i,
                received=f"Character: {char} (code {char_code})",
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

        return False

    def _is_delimiter(self, char: str) -> bool:
        """Check if character is a LISP token delimiter."""
        return char.isspace() or char in "()'\";,"

    def _check_for_control_character(self, char: str, position: int) -> None:
        """
        Check if a character is a control character and raise an error if so.

        Args:
            char: Character to check
            position: Position in the expression

        Raises:
            AIFPLTokenError: If the character is a control character
        """
        char_code = ord(char)

        # Control characters are ASCII < 32 (excluding whitespace which is handled separately)
        if char_code < 32 and not char.isspace():
            char_display = f"\\u{char_code:04x}"
            raise AIFPLTokenError(
                message=f"Invalid control character in source code: {char_display}",
                position=position,
                received=f"Control character: {char_display} (code {char_code})",
                expected="Valid AIFPL characters or escape sequences in strings",
                example='Valid: "hello\\nworld" (newline in string)\\nInvalid: hello<ctrl-char>world',
                suggestion="Remove the control character or use escape sequences like \\n, \\t, or \\uXXXX in strings",
                context="Control characters are not allowed in source code. Use escape "
                    "sequences like \\n, \\t, or \\uXXXX in strings."
            )

    def _read_complete_token(self, expression: str, start: int) -> str:
        """
        Read a complete token until delimiter, following LISP tokenization rules.

        Validates that no control characters are present in the token.

        Returns:
            The complete token string

        Raises:
            AIFPLTokenError: If a control character is encountered
        """
        i = start

        # Consume characters until we hit a delimiter
        while i < len(expression):
            char = expression[i]

            # Check for control characters before processing
            self._check_for_control_character(char, i)

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
        check_token = token
        if token.startswith('-'):
            # We don't need to worry about just '-' being a number, as that would be caught earlier
            check_token = token[1:]

        # Try different number formats

        # Hexadecimal
        if check_token.startswith('0x') or check_token.startswith('0X'):
            if len(check_token) <= 2:
                return False

            hex_part = check_token[2:]
            return all(c in '0123456789abcdefABCDEF' for c in hex_part)

        # Binary
        if check_token.startswith('0b') or check_token.startswith('0B'):
            if len(check_token) <= 2:
                return False

            bin_part = check_token[2:]
            return all(c in '01' for c in bin_part)

        # Octal
        if check_token.startswith('0o') or check_token.startswith('0O'):
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
        negative = token.startswith('-')
        if negative:
            token = token[1:]

        # Parse different formats
        if token.startswith('0x') or token.startswith('0X'):
            value: int | float = int(token, 16)

        elif token.startswith('0b') or token.startswith('0B'):
            value = int(token, 2)

        elif token.startswith('0o') or token.startswith('0O'):
            value = int(token, 8)

        else:
            # Decimal number - use float if it contains . or e/E, otherwise int
            if '.' in token or 'e' in token.lower():
                value = float(token)

            else:
                value = int(token)

        return -value if negative else value

    def _read_number(self, expression: str, start: int) -> tuple[Union[int, float, complex], int]:
        """
        Read a number literal from the expression using robust token boundary detection.

        Returns:
            Tuple of (number_value, length_consumed)

        Raises:
            AIFPLTokenError: If the token is not a valid number
        """
        # Get the complete token until delimiter (this will check for control characters)
        complete_token = self._read_complete_token(expression, start)

        # Validate that this token is a valid number
        if not self._is_valid_number(complete_token):
            raise AIFPLTokenError(
                message=f"Invalid number format: {complete_token}",
                position=start,
                received=f"Malformed number token: {complete_token}",
                expected="Valid number format",
                suggestion=f"Fix the number format: {complete_token}",
                context="Token appears to be a number but contains invalid characters",
                example="Valid: 1.23, .5, 42, 1e-10, 0xFF"
            )

        # Parse the valid number
        number_value = self._parse_number_value(complete_token)
        return number_value, len(complete_token)

    def _is_symbol_start(self, char: str) -> bool:
        """Check if character can start a symbol."""
        return char.isalpha() or char in '+-*/%<>=!&|^~_.'

    def _read_symbol(self, expression: str, start: int) -> tuple[str, int]:
        """
        Read a symbol from the expression.

        Validates that no control characters are present in the symbol.

        Returns:
            Tuple of (symbol_string, length_consumed)

        Raises:
            AIFPLTokenError: If a control character is encountered
        """
        i = start

        while i < len(expression):
            char = expression[i]

            # Symbol characters: letters, digits, hyphens, and operator chars
            if char.isalnum() or char in '-+*/%<>=!&|^~?_.':
                # Check for control characters before adding to symbol
                self._check_for_control_character(char, i)
                i += 1

            else:
                break

        symbol = expression[start:i]
        return symbol, i - start
