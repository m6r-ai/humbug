"""AIFPL (AI Functional Programming Language) calculator tool with LISP-like syntax."""

import asyncio
import cmath
import logging
import math
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Union

from ai_tool import (
    AITool, AIToolCall, AIToolDefinition, AIToolParameter, AIToolResult,
    AIToolExecutionError, AIToolTimeoutError, AIToolAuthorizationCallback
)


class TokenType(Enum):
    """Token types for AIFPL expressions."""
    LPAREN = "("
    RPAREN = ")"
    SYMBOL = "SYMBOL"
    NUMBER = "NUMBER"
    EOF = "EOF"


@dataclass
class Token:
    """Represents a single token in an AIFPL expression."""
    type: TokenType
    value: Any
    position: int
    length: int = 1

    def __repr__(self) -> str:
        return f"Token({self.type.name}, {self.value!r}, pos={self.position})"


# S-Expression types
Atom = Union[int, float, complex, str]
SExpression = Union[Atom, List['SExpression']]


@dataclass
class ParsedExpression:
    """Wrapper to track position info for error reporting."""
    expr: SExpression
    start_pos: int
    end_pos: int


class AIFPLError(Exception):
    """Base exception for AIFPL errors."""


class AIFPLTokenError(AIFPLError):
    """Tokenization errors."""


class AIFPLParseError(AIFPLError):
    """Parsing errors."""


class AIFPLEvalError(AIFPLError):
    """Evaluation errors."""


class AIFPLTokenizer:
    """Tokenizes AIFPL expressions into tokens."""

    def tokenize(self, expression: str) -> List[Token]:
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
                tokens.append(Token(TokenType.LPAREN, '(', i))
                i += 1
            elif expression[i] == ')':
                tokens.append(Token(TokenType.RPAREN, ')', i))
                i += 1

            # Numbers (including complex, hex, binary, octal)
            elif self._is_number_start(expression, i):
                number, length = self._read_number(expression, i)
                tokens.append(Token(TokenType.NUMBER, number, i, length))
                i += length

            # Symbols (operators, functions, constants)
            elif self._is_symbol_start(expression[i]):
                symbol, length = self._read_symbol(expression, i)
                tokens.append(Token(TokenType.SYMBOL, symbol, i, length))
                i += length

            else:
                raise AIFPLTokenError(f"Invalid character '{expression[i]}' at position {i}")

        tokens.append(Token(TokenType.EOF, None, len(expression)))
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

        # Regular decimal number (int or float)
        num_start = i
        has_dot = False

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

        number_str = expression[num_start:i]

        try:
            if has_dot:
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


class AIFPLParser:
    """Parses tokens into an Abstract Syntax Tree."""

    def __init__(self, tokens: List[Token]):
        """
        Initialize parser with tokens.

        Args:
            tokens: List of tokens to parse
        """
        self.tokens = tokens
        self.pos = 0
        self.current_token: Token | None = tokens[0] if tokens else None

    def parse(self) -> ParsedExpression:
        """
        Parse tokens into AST.

        Returns:
            Parsed expression tree

        Raises:
            AIFPLParseError: If parsing fails
        """
        if self.current_token is None or self.current_token.type == TokenType.EOF:
            raise AIFPLParseError("Empty expression")

        expr = self._parse_expression()

        if self.current_token is None or self.current_token.type != TokenType.EOF:
            current_value = self.current_token.value if self.current_token else "EOF"
            current_pos = self.current_token.position if self.current_token else "end"
            raise AIFPLParseError(f"Unexpected token after expression: {current_value} at position {current_pos}")

        return expr

    def _parse_expression(self) -> ParsedExpression:
        """Parse a single expression (atom or list)."""
        if self.current_token is None:
            raise AIFPLParseError("Unexpected end of input, expected expression")

        start_pos = self.current_token.position

        if self.current_token.type == TokenType.LPAREN:
            return self._parse_list(start_pos)

        if self.current_token.type in (TokenType.NUMBER, TokenType.SYMBOL):
            return self._parse_atom(start_pos)

        raise AIFPLParseError(f"Unexpected token: {self.current_token.value} at position {self.current_token.position}")

    def _parse_list(self, start_pos: int) -> ParsedExpression:
        """Parse (operator arg1 arg2 ...)"""
        self._consume(TokenType.LPAREN)

        elements = []
        while self.current_token is not None and self.current_token.type != TokenType.RPAREN:
            if self.current_token.type == TokenType.EOF:
                raise AIFPLParseError(f"Unclosed parenthesis starting at position {start_pos}")
            elements.append(self._parse_expression().expr)

        if self.current_token is None:
            raise AIFPLParseError(f"Unterminated list starting at position {start_pos}, expected ')'")

        end_pos = self.current_token.position
        self._consume(TokenType.RPAREN)

        if not elements:
            raise AIFPLParseError(f"Empty list at position {start_pos}")

        return ParsedExpression(elements, start_pos, end_pos)

    def _parse_atom(self, start_pos: int) -> ParsedExpression:
        """Parse an atomic value (number or symbol)."""
        assert self.current_token is not None, "_parse_atom called with None token"

        token = self.current_token
        self._advance()

        end_pos = start_pos + token.length
        return ParsedExpression(token.value, start_pos, end_pos)

    def _consume(self, expected_type: TokenType) -> None:
        """Consume a token of the expected type."""
        if self.current_token is None:
            raise AIFPLParseError(f"Unexpected end of input, expected {expected_type.value}")

        if self.current_token.type != expected_type:
            raise AIFPLParseError(
                f"Expected {expected_type.value}, got {self.current_token.value} at position {self.current_token.position}"
            )

        self._advance()

    def _advance(self) -> None:
        """Move to the next token."""
        self.pos += 1
        if self.pos < len(self.tokens):
            self.current_token = self.tokens[self.pos]
        else:
            self.current_token = None


class AIFPLEvaluator:
    """Evaluates AIFPL Abstract Syntax Trees."""

    # Mathematical constants
    CONSTANTS = {
        'pi': math.pi,
        'e': math.e,
        'j': 1j,
    }

    # Operator and function definitions
    OPERATORS: Dict[str, Dict[str, Any]] = {
        # Arithmetic operators
        '+': {'type': 'variadic', 'min_args': 0, 'identity': 0},
        '-': {'type': 'variadic', 'min_args': 1},
        '*': {'type': 'variadic', 'min_args': 0, 'identity': 1},
        '/': {'type': 'variadic', 'min_args': 2},
        '//': {'type': 'binary'},
        '%': {'type': 'binary'},
        '**': {'type': 'binary'},

        # Bitwise operators
        'bit-or': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-and': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-xor': {'type': 'variadic', 'min_args': 2, 'bitwise': True},
        'bit-not': {'type': 'unary', 'bitwise': True},
        'bit-shift-left': {'type': 'binary', 'bitwise': True},
        'bit-shift-right': {'type': 'binary', 'bitwise': True},

        # Mathematical functions
        'sin': {'type': 'unary'},
        'cos': {'type': 'unary'},
        'tan': {'type': 'unary'},
        'log': {'type': 'unary'},
        'log10': {'type': 'unary'},
        'exp': {'type': 'unary'},
        'sqrt': {'type': 'unary'},
        'abs': {'type': 'unary'},
        'round': {'type': 'unary', 'real_only': True},
        'floor': {'type': 'unary', 'real_only': True},
        'ceil': {'type': 'unary', 'real_only': True},
        'min': {'type': 'variadic', 'min_args': 1},
        'max': {'type': 'variadic', 'min_args': 1},
        'pow': {'type': 'binary'},

        # Base conversion functions
        'bin': {'type': 'unary', 'integer_only': True, 'returns_string': True},
        'hex': {'type': 'unary', 'integer_only': True, 'returns_string': True},
        'oct': {'type': 'unary', 'integer_only': True, 'returns_string': True},

        # Complex number constructor
        'complex': {'type': 'binary'},
    }

    # Tolerance for considering imaginary part as zero
    IMAGINARY_TOLERANCE = 1e-10

    def __init__(self, max_depth: int = 100):
        """
        Initialize evaluator.

        Args:
            max_depth: Maximum recursion depth
        """
        self.max_depth = max_depth

    def evaluate(self, expr: SExpression, depth: int = 0) -> Union[int, float, complex, str]:
        """
        Recursively evaluate AST.

        Args:
            expr: Expression to evaluate
            depth: Current recursion depth

        Returns:
            Evaluation result

        Raises:
            AIFPLEvalError: If evaluation fails
        """
        if depth > self.max_depth:
            raise AIFPLEvalError(f"Expression too deeply nested (max depth: {self.max_depth})")

        # Atom evaluation
        if isinstance(expr, (int, float, complex)):
            return expr

        if isinstance(expr, str):
            # Symbol lookup (constants)
            if expr in self.CONSTANTS:
                return self.CONSTANTS[expr]

            raise AIFPLEvalError(f"Unknown symbol: '{expr}'")

        # List evaluation
        if isinstance(expr, list):
            if not expr:
                raise AIFPLEvalError("Cannot evaluate empty list")

            operator = expr[0]
            args = expr[1:]

            if not isinstance(operator, str):
                raise AIFPLEvalError(f"Operator must be a symbol, got {type(operator).__name__}")

            return self._apply_operator(operator, args, depth + 1)

        raise AIFPLEvalError(f"Invalid expression type: {type(expr).__name__}")

    def _apply_operator(self, operator: str, args: List[SExpression], depth: int) -> Union[int, float, complex, str]:
        """Apply an operator to its arguments."""
        if operator not in self.OPERATORS:
            raise AIFPLEvalError(f"Unknown operator: '{operator}'")

        op_def: Dict[str, Any] = self.OPERATORS[operator]

        # Evaluate arguments
        evaluated_args = [self.evaluate(arg, depth) for arg in args]

        # Check argument count
        self._validate_arity(operator, op_def, evaluated_args)

        # Handle special cases that return strings
        if op_def.get('returns_string'):
            return self._apply_string_function(operator, evaluated_args)

        # Filter out string arguments for mathematical operations
        for arg in evaluated_args:
            if isinstance(arg, str):
                raise AIFPLEvalError(f"Operator '{operator}' cannot operate on string arguments")

        # Handle bitwise operations (require integers)
        if op_def.get('bitwise'):
            return self._apply_bitwise_operator(operator, evaluated_args)

        # Handle real-only operations
        if op_def.get('real_only'):
            return self._apply_real_only_function(operator, evaluated_args)

        # Handle integer-only operations
        if op_def.get('integer_only'):
            return self._apply_integer_only_function(operator, evaluated_args)

        # Handle regular mathematical operations
        return self._apply_mathematical_operator(operator, op_def, evaluated_args)

    def _validate_arity(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> None:
        """Validate argument count for an operator."""
        op_type = op_def['type']
        arg_count = len(args)

        if op_type == 'unary' and arg_count != 1:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 1 argument, got {arg_count}")

        if op_type == 'binary' and arg_count != 2:
            raise AIFPLEvalError(f"Operator '{operator}' takes exactly 2 arguments, got {arg_count}")

        if op_type == 'variadic':
            min_args = op_def.get('min_args', 0)
            if arg_count < min_args:
                raise AIFPLEvalError(f"Operator '{operator}' requires at least {min_args} arguments, got {arg_count}")

    def _apply_string_function(self, operator: str, args: List[Any]) -> str:
        """Apply functions that return strings."""
        arg = args[0]

        # Convert to integer for base conversion
        int_arg = self._to_integer(arg, operator)

        if operator == 'bin':
            return bin(int_arg)

        if operator == 'hex':
            return hex(int_arg)

        if operator == 'oct':
            return oct(int_arg)

        raise AIFPLEvalError(f"Unknown string function: '{operator}'")

    def _apply_bitwise_operator(self, operator: str, args: List[Any]) -> int:
        """Apply bitwise operators (require integer arguments)."""
        # Convert all arguments to integers
        int_args = [self._to_integer(arg, operator) for arg in args]

        if operator == 'bit-or':
            result = int_args[0]
            for arg in int_args[1:]:
                result |= arg

            return result

        if operator == 'bit-and':
            result = int_args[0]
            for arg in int_args[1:]:
                result &= arg

            return result

        if operator == 'bit-xor':
            result = int_args[0]
            for arg in int_args[1:]:
                result ^= arg

            return result

        if operator == 'bit-not':
            return ~int_args[0]

        if operator == 'bit-shift-left':
            left, right = int_args
            if right < 0:
                raise AIFPLEvalError("Shift count cannot be negative")

            if right > 64:
                raise AIFPLEvalError("Shift count too large (max 64)")

            return left << right

        if operator == 'bit-shift-right':
            left, right = int_args
            if right < 0:
                raise AIFPLEvalError("Shift count cannot be negative")

            if right > 64:
                raise AIFPLEvalError("Shift count too large (max 64)")

            return left >> right

        raise AIFPLEvalError(f"Unknown bitwise operator: '{operator}'")

    def _apply_real_only_function(self, operator: str, args: List[Any]) -> Union[int, float]:
        """Apply functions that only work with real numbers."""
        arg = args[0]

        # Convert complex to real if imaginary part is negligible
        if isinstance(arg, complex):
            if abs(arg.imag) >= self.IMAGINARY_TOLERANCE:
                raise AIFPLEvalError(f"Function '{operator}' does not support complex numbers")
            arg = arg.real

        if operator == 'round':
            return round(arg)

        if operator == 'floor':
            return math.floor(arg)

        if operator == 'ceil':
            return math.ceil(arg)

        raise AIFPLEvalError(f"Unknown real-only function: '{operator}'")

    def _apply_integer_only_function(self, operator: str, args: List[Any]) -> int:
        """Apply functions that only work with integers."""
        arg = args[0]
        return self._to_integer(arg, operator)

    def _apply_mathematical_operator(self, operator: str, op_def: Dict[str, Any], args: List[Any]) -> Union[int, float, complex]:
        """Apply regular mathematical operators with type promotion."""
        if not args and 'identity' in op_def:
            return op_def['identity']

        # Promote types to common type
        promoted_args = self._promote_types(*args)

        try:
            if operator == '+':
                return sum(promoted_args)

            if operator == '-':
                if len(promoted_args) == 1:
                    return -promoted_args[0]

                result = promoted_args[0]
                for arg in promoted_args[1:]:
                    result -= arg

                return result

            if operator == '*':
                result = promoted_args[0] if promoted_args else 1
                for arg in promoted_args[1:]:
                    result *= arg

                return result

            if operator == '/':
                result = promoted_args[0]
                for arg in promoted_args[1:]:
                    if arg == 0:
                        raise ZeroDivisionError("Division by zero")

                    result /= arg

                return result

            if operator == '//':
                a, b = promoted_args
                if b == 0:
                    raise ZeroDivisionError("Division by zero")

                return a // b

            if operator == '%':
                a, b = promoted_args
                if b == 0:
                    raise ZeroDivisionError("Modulo by zero")

                return a % b

            if operator == '**':
                a, b = promoted_args
                return a ** b

            if operator == 'pow':
                a, b = promoted_args
                return pow(a, b)

            if operator == 'sin':
                return cmath.sin(promoted_args[0])

            if operator == 'cos':
                return cmath.cos(promoted_args[0])

            if operator == 'tan':
                return cmath.tan(promoted_args[0])

            if operator == 'log':
                return cmath.log(promoted_args[0])

            if operator == 'log10':
                return cmath.log10(promoted_args[0])

            if operator == 'exp':
                return cmath.exp(promoted_args[0])

            if operator == 'sqrt':
                return cmath.sqrt(promoted_args[0])

            if operator == 'abs':
                return abs(promoted_args[0])

            if operator == 'min':
                return min(promoted_args)

            if operator == 'max':
                return max(promoted_args)

            if operator == 'complex':
                real, imag = promoted_args
                return complex(real, imag)

            raise AIFPLEvalError(f"Unknown mathematical operator: '{operator}'")

        except ZeroDivisionError as e:
            raise AIFPLEvalError(f"Division by zero error in '{operator}': {e}") from e

        except (ValueError, OverflowError) as e:
            raise AIFPLEvalError(f"Mathematical error in '{operator}': {e}") from e

    def _promote_types(self, *values: Any) -> tuple:
        """Promote arguments to common type: int → float → complex."""
        has_complex = any(isinstance(v, complex) for v in values)
        has_float = any(isinstance(v, float) for v in values)

        if has_complex:
            return tuple(complex(v) for v in values)

        if has_float:
            return tuple(float(v) for v in values)

        return values  # All integers

    def _to_integer(self, value: Union[int, float, complex], operation: str) -> int:
        """Convert a numeric value to integer for operations that require it."""
        if isinstance(value, complex):
            if abs(value.imag) >= self.IMAGINARY_TOLERANCE:
                raise AIFPLEvalError(f"Operation '{operation}' does not support complex numbers")

            value = value.real

        if isinstance(value, float):
            if not value.is_integer():
                raise AIFPLEvalError(f"Operation '{operation}' requires integer values, got float: {value}")

            value = int(value)

        return value

    def simplify_result(self, result: Union[int, float, complex, str]) -> Union[int, float, complex, str]:
        """Simplify complex results to real numbers when imaginary part is negligible."""
        if isinstance(result, complex):
            # If imaginary part is effectively zero, return just the real part
            if abs(result.imag) < self.IMAGINARY_TOLERANCE:
                real_part = result.real
                # Convert to int if it's a whole number
                if isinstance(real_part, float) and real_part.is_integer():
                    return int(real_part)

                return real_part

        # For real numbers, convert float to int if it's a whole number
        if isinstance(result, float) and result.is_integer():
            return int(result)

        return result


class AIFPLAITool(AITool):
    """AIFPL calculator tool with LISP-like syntax."""

    def __init__(self) -> None:
        """Initialize the AIFPL tool."""
        self._tokenizer = AIFPLTokenizer()
        self._evaluator = AIFPLEvaluator()
        self._logger = logging.getLogger("AIFPLAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return AIToolDefinition(
            name="AIFPL",
            description=(
                "The AIFPL (AI Functional Programming Language) calculator uses LISP-like (S expression) syntax for "
                "mathematical expressions. "
                "Syntax: (operator arg1 arg2 ...)\n\n"
                "Supported operations:\n"
                "- Arithmetic: (+ 1 2 3), (- 10 3), (* 2 3 4), (/ 12 3), (// 7 3), (% 7 3), (** 2 3)\n"
                "- Trigonometry: (sin (* pi 0.5)), (cos 0), (tan (* pi 0.25))\n"
                "- Logarithms: (log e), (log10 100), (exp 1)\n"
                "- Other math: (sqrt 16), (abs -5), (round 3.7), (floor 3.7), (ceil 3.2)\n"
                "- Aggregation: (min 1 5 3), (max 1 5 3), (pow 2 3)\n"
                "- Bitwise: (bit-or 5 3), (bit-and 7 3), (bit-xor 5 3), (bit-not 5)\n"
                "- Bit shifts: (bit-shift-left 1 3), (bit-shift-right 8 2)\n"
                "- Base conversion: (bin 255), (hex 255), (oct 255)\n"
                "- Complex numbers: (complex 3 4), (+ 1 (* 2 j))\n"
                "- Constants: pi, e, j\n"
                "- Number literals: 42, 3.14, 0xFF, 0b1010, 0o755\n\n"
                "- Nested expressions\n"
                "Important:\n"
                "- All operators use prefix notation: (+ 1 2) not (1 + 2)\n"
                "- Whitespace is required between all tokens\n"
                "- Arithmetic operators are variadic where sensible: (+ 1 2 3 4) = 10\n"
                "- Results are simplified to real numbers when imaginary part is negligible\n"
                "- Bitwise operations only work on integers\n"
                "- Do not use it for anything other than mathematical calculations of the types described\n"
            ),
            parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="A valid AIFPL expression using LISP-like S-expression syntax",
                    required=True
                )
            ]
        )

    def _evaluate_expression(self, expression: str) -> str:
        """
        Synchronous helper for expression evaluation.

        Args:
            expression: AIFPL expression to evaluate

        Returns:
            String representation of the result

        Raises:
            Various AIFPL-related exceptions
        """
        # Phase 1: Tokenization
        tokens = self._tokenizer.tokenize(expression)

        # Phase 2: Parsing
        parser = AIFPLParser(tokens)
        parsed = parser.parse()

        # Phase 3: Evaluation
        result = self._evaluator.evaluate(parsed.expr)

        # Simplify the result
        simplified = self._evaluator.simplify_result(result)

        return str(simplified)

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the AIFPL tool with timeout protection.

        Args:
            tool_call: Tool call containing the expression to evaluate
            requester_ref: Reference to the requester
            request_authorization: Function to call if we need to request authorization

        Returns:
            AIToolResult containing the calculation result

        Raises:
            AIToolExecutionError: If calculation fails or expression is invalid
            AIToolTimeoutError: If calculation takes too long
        """
        arguments = tool_call.arguments
        expression = arguments.get("expression", "")

        # Validate expression is provided
        if not expression:
            self._logger.error("AIFPL tool called without expression argument")
            raise AIToolExecutionError("Expression is required")

        # Validate expression is a string
        if not isinstance(expression, str):
            self._logger.error("AIFPL tool called with non-string expression: %s", type(expression).__name__)
            raise AIToolExecutionError("Expression must be a string")

        try:
            self._logger.debug("Evaluating AIFPL expression: %s", expression)

            # Run calculation with timeout protection
            try:
                result = await asyncio.wait_for(
                    asyncio.to_thread(self._evaluate_expression, expression),
                    timeout=5.0  # 5 seconds should be plenty for mathematical calculations
                )
            except asyncio.TimeoutError as e:
                self._logger.warning("AIFPL expression evaluation timed out: %s", expression)
                raise AIToolTimeoutError("AIFPL calculation timed out", 5.0) from e

            self._logger.debug("AIFPL evaluation successful: %s = %s", expression, result)

            return AIToolResult(
                id=tool_call.id,
                name="AIFPL",
                content=result
            )

        except AIToolTimeoutError:
            # Re-raise timeout errors
            raise

        except (AIFPLTokenError, AIFPLParseError, AIFPLEvalError) as e:
            self._logger.warning("AIFPL error in expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(str(e)) from e

        except ZeroDivisionError as e:
            self._logger.warning("Division by zero in AIFPL expression '%s'", expression, exc_info=True)
            raise AIToolExecutionError("Division by zero") from e

        except Exception as e:
            self._logger.error("Unexpected error evaluating AIFPL expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to evaluate AIFPL expression: {str(e)}") from e
