"""Tokenizer for AIFPL expressions."""

from typing import List, Union

from aifpl.aifpl_error import AIFPLTokenError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


class AIFPLTokenizer:
    """Tokenizes AIFPL expressions into tokens."""

    def tokenize(self, expression: str) -> List[AIFPLToken]:
        """
        Tokenize an AIFPL expression.

        Args:
            expression: The expression string to tokenize

        Returns:
            List of tokens

        Raises:
            AIFPLTokenError: If tokenization fails
        """
        tokens = []
        i = 0

        while i < len(expression):
            # Skip whitespace
            if expression[i].isspace():
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

            # Numbers (including complex, hex, binary, octal, scientific notation)
            if self._is_number_start(expression, i):
                number, length = self._read_number(expression, i)
                tokens.append(AIFPLToken(AIFPLTokenType.NUMBER, number, i, length))
                i += length
                continue

            # Symbols (operators, functions, constants)
            if self._is_symbol_start(expression[i]):
                symbol, length = self._read_symbol(expression, i)
                tokens.append(AIFPLToken(AIFPLTokenType.SYMBOL, symbol, i, length))
                i += length
                continue

            raise AIFPLTokenError(f"Invalid character '{expression[i]}' at position {i}")

        tokens.append(AIFPLToken(AIFPLTokenType.EOF, None, len(expression)))
        return tokens

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
        return char.isalpha() or char in '+-*/%<>=!&|^~'

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
            if char.isalnum() or char in '-+*/%<>=!&|^~':
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
