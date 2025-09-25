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

                        if escape_pos + 1 < len(expression):
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
                    if i + 2 < len(expression) and expression[i + 2].isalpha():
                        # Find end of the invalid sequence
                        end = i + 2
                        while end < len(expression) and expression[end].isalnum():
                            end += 1

                        invalid_literal = expression[i:end]

                        suggestion = ""
                        if invalid_literal.lower() in ["#true", "#false"]:
                            correct = "#t" if "true" in invalid_literal.lower() else "#f"
                            suggestion = f"Use {correct} instead of {invalid_literal}"

                        else:
                            suggestion = "Use #t for true or #f for false"

                        raise AIFPLTokenError(
                            message=f"Invalid boolean literal: {invalid_literal}",
                            position=i,
                            received=f"Boolean literal: {invalid_literal}",
                            expected="Valid boolean: #t or #f",
                            example="Correct: #t, #f\\nIncorrect: #true, #false, #T, #F",
                            suggestion=suggestion,
                            context="AIFPL uses #t and #f for boolean values (not #true/#false)"
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
            if self._is_number_start(expression, i):
                try:
                    number, length = self._read_number(expression, i)
                    tokens.append(AIFPLToken(AIFPLTokenType.NUMBER, number, i, length))
                    i += length
                    continue

                except AIFPLTokenError as e:
                    error_msg = str(e)
                    if "Invalid hexadecimal" in error_msg:
                        raise AIFPLTokenError(
                            message="Invalid hexadecimal number",
                            position=i,
                            received=f"Number format: {expression[i:i+6]}...",
                            expected="Hexadecimal digits after 0x: 0x1A2B",
                            example="Correct: 0xFF, 0x123, 0xABCD\\nIncorrect: 0x, 0xGHI",
                            suggestion="Add hex digits (0-9, A-F) after 0x",
                            context="Hexadecimal numbers need digits after the 0x prefix"
                        ) from e

                    if "Invalid binary" in error_msg:
                        raise AIFPLTokenError(
                            message="Invalid binary number",
                            position=i,
                            received=f"Number format: {expression[i:i+6]}...",
                            expected="Binary digits after 0b: 0b1010",
                            example="Correct: 0b101, 0b1111\\nIncorrect: 0b, 0b123",
                            suggestion="Add binary digits (0 or 1) after 0b",
                            context="Binary numbers need digits after the 0b prefix"
                        ) from e

                    if "Invalid scientific notation" in error_msg:
                        raise AIFPLTokenError(
                            message="Invalid scientific notation",
                            position=i,
                            received=f"Number format: {expression[i:i+8]}...",
                            expected="Digits after e/E: 1.5e10 or 2E-3",
                            example="Correct: 1e5, 2.5E-3, 1.0e+10\\nIncorrect: 1e, 2E+",
                            suggestion="Add digits after the exponent marker (e/E)",
                            context="Scientific notation needs digits after e/E"
                        ) from e

                    raise  # Re-raise if not handled

            # Symbols (variables, parameters, functions, constants)
            if self._is_symbol_start(expression[i]):
                try:
                    symbol, length = self._read_symbol(expression, i)
                    tokens.append(AIFPLToken(AIFPLTokenType.SYMBOL, symbol, i, length))
                    i += length
                    continue

                except AIFPLTokenError as e:
                    if "looks like malformed number" in str(e):
                        symbol_text = ""
                        j = i
                        while j < len(expression) and not expression[j].isspace() and expression[j] not in "()":
                            symbol_text += expression[j]
                            j += 1

                        raise AIFPLTokenError(
                            message=f"Invalid symbol: {symbol_text}",
                            position=i,
                            received=f"Symbol: {symbol_text}",
                            expected="Valid symbol name or number",
                            example="Valid symbols: +, my-var, func?\\nValid numbers: 42, -3.14",
                            suggestion="If this should be a number, check the format. If a symbol, don't start with digits.",
                            context="Symbols cannot look like malformed numbers"
                        ) from e

                    raise  # Re-raise if not handled

            # Invalid character
            char = expression[i]
            char_code = ord(char)

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

            if char_code < 32:
                char_display = f"\\u{char_code:04x}"
                context = "Control characters are not allowed except in strings"

            else:
                char_display = char
                context = "Only letters, digits, and specific symbols are allowed"

            raise AIFPLTokenError(
                message=f"Invalid character: {char_display}",
                position=i,
                received=f"Character: {char_display} (code {char_code})",
                expected="Valid AIFPL characters: letters, digits, +, -, *, /, etc.",
                example="Valid: (+ 1 2), my-var, func?\\nInvalid: @var, $value, [list]",
                suggestion=suggestion,
                context=context
            )

        tokens.append(AIFPLToken(AIFPLTokenType.EOF, None, len(expression)))
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

                    try:
                        code_point = int(hex_digits, 16)
                        result.append(chr(code_point))
                        i += 4  # Skip the extra 4 characters (uXXXX)

                    except ValueError as e:
                        raise AIFPLTokenError(f"Invalid Unicode code point at position {i}: \\u{hex_digits}") from e

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

        # Negative numbers
        if char == '-' and pos + 1 < len(expression) and expression[pos + 1].isdigit():
            return True

        # Hex, binary, octal prefixes
        if char == '0' and pos + 1 < len(expression):
            next_char = expression[pos + 1].lower()
            if next_char in 'xbo':
                return True

        return False

    def _read_number(self, expression: str, start: int) -> tuple[Union[int, float, complex], int]:
        """
        Read a number literal from the expression.

        Returns:
            Tuple of (number_value, length_consumed)
        """
        i = start

        # Handle negative sign
        negative = False
        if expression[i] == '-':
            negative = True
            i += 1

        # Handle different number formats
        if expression[i] == '0' and i + 1 < len(expression):
            next_char = expression[i + 1].lower()

            # Hexadecimal
            if next_char == 'x':
                i += 2
                hex_start = i
                while i < len(expression) and expression[i].lower() in '0123456789abcdef':
                    i += 1

                if i == hex_start:
                    raise AIFPLTokenError(f"Invalid hexadecimal number at position {start}")

                hex_value = int(expression[hex_start:i], 16)
                return (-hex_value if negative else hex_value), i - start

            # Binary
            if next_char == 'b':
                i += 2
                bin_start = i
                while i < len(expression) and expression[i] in '01':
                    i += 1

                if i == bin_start:
                    raise AIFPLTokenError(f"Invalid binary number at position {start}")

                bin_value = int(expression[bin_start:i], 2)
                return (-bin_value if negative else bin_value), i - start

            # Octal
            if next_char == 'o':
                i += 2
                oct_start = i
                while i < len(expression) and expression[i] in '01234567':
                    i += 1

                if i == oct_start:
                    raise AIFPLTokenError(f"Invalid octal number at position {start}")

                oct_value = int(expression[oct_start:i], 8)
                return (-oct_value if negative else oct_value), i - start

        # Regular decimal number (int, float, or scientific notation)
        num_start = i
        has_dot = False

        # Read the base number (digits and optional decimal point)
        while i < len(expression):
            char = expression[i]
            if char.isdigit():
                i += 1

            elif char == '.' and not has_dot:
                has_dot = True
                i += 1

            else:
                break

        if i == num_start:
            raise AIFPLTokenError(f"Invalid number at position {start}")

        # Check for scientific notation (e or E)
        if i < len(expression) and expression[i].lower() == 'e':
            i += 1  # consume 'e' or 'E'

            # Optional sign after 'e'
            if i < len(expression) and expression[i] in '+-':
                i += 1  # consume '+' or '-'

            # Must have digits after 'e' (and optional sign)
            exponent_start = i
            while i < len(expression) and expression[i].isdigit():
                i += 1

            if i == exponent_start:
                raise AIFPLTokenError(f"Invalid scientific notation: missing exponent digits at position {i}")

        number_str = expression[num_start:i]

        try:
            # Always use float for scientific notation or numbers with decimal points
            if 'e' in number_str.lower() or has_dot:
                decimal_value: Union[int, float] = float(number_str)

            else:
                decimal_value = int(number_str)

            return (-decimal_value if negative else decimal_value), i - start

        except ValueError as e:
            raise AIFPLTokenError(f"Invalid number format at position {start}: {e}") from e

    def _is_symbol_start(self, char: str) -> bool:
        """Check if character can start a symbol."""
        return char.isalpha() or char in '+-*/%<>=!&|^~_.'

    def _read_symbol(self, expression: str, start: int) -> tuple[str, int]:
        """
        Read a symbol from the expression.

        Returns:
            Tuple of (symbol_string, length_consumed)
        """
        i = start

        while i < len(expression):
            char = expression[i]
            # Symbol characters: letters, digits, hyphens, and operator chars
            if char.isalnum() or char in '-+*/%<>=!&|^~?_.':
                i += 1

            else:
                break

        if i == start:
            raise AIFPLTokenError(f"Invalid symbol at position {start}")

        symbol = expression[start:i]

        # Validate that symbols don't look like malformed numbers
        if symbol[0].isdigit() or (symbol.startswith('-') and len(symbol) > 1 and symbol[1].isdigit()):
            raise AIFPLTokenError(f"Invalid symbol '{symbol}' at position {start} (looks like malformed number)")

        return symbol, i - start
