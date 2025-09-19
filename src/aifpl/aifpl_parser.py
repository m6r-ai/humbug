"""Parser for AIFPL expressions."""

from dataclasses import dataclass
from typing import List, Union, Tuple

from aifpl.aifpl_error import AIFPLParseError
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


# S-Expression types
AIFPLAtom = Union[int, float, complex, str, bool]


@dataclass
class AIFPLStringLiteral:
    """Wrapper to distinguish string literals from symbols."""
    value: str

    def __str__(self) -> str:
        return self.value

    def __repr__(self) -> str:
        return f'AIFPLStringLiteral({self.value!r})'


@dataclass
class AIFPLLambdaExpr:
    """Lambda expression AST node."""
    parameters: List[str]
    body: 'AIFPLSExpression'
    position: int = 0


@dataclass
class AIFPLLetExpr:
    """Let expression AST node."""
    bindings: List[Tuple[str, 'AIFPLSExpression']]
    body: 'AIFPLSExpression'
    position: int = 0


@dataclass
class AIFPLFunctionCall:
    """Function call AST node."""
    function: 'AIFPLSExpression'
    arguments: List['AIFPLSExpression']
    position: int = 0


# Updated S-Expression type to include new nodes and AIFPLStringLiteral
AIFPLSExpression = Union[AIFPLAtom, AIFPLStringLiteral, List['AIFPLSExpression'], AIFPLLambdaExpr, AIFPLLetExpr, AIFPLFunctionCall]


@dataclass
class AIFPLParsedExpression:
    """Wrapper to track position info for error reporting."""
    expr: AIFPLSExpression
    start_pos: int
    end_pos: int


class AIFPLParser:
    """Parses tokens into an Abstract Syntax Tree."""

    def __init__(self, tokens: List[AIFPLToken]):
        """
        Initialize parser with tokens.

        Args:
            tokens: List of tokens to parse
        """
        self.tokens = tokens
        self.pos = 0
        self.current_token: AIFPLToken | None = tokens[0] if tokens else None

    def parse(self) -> AIFPLParsedExpression:
        """
        Parse tokens into AST.

        Returns:
            Parsed expression tree

        Raises:
            AIFPLParseError: If parsing fails
        """
        if self.current_token is None or self.current_token.type == AIFPLTokenType.EOF:
            raise AIFPLParseError("Empty expression")

        expr = self._parse_expression()

        if self.current_token is None or self.current_token.type != AIFPLTokenType.EOF:
            current_value = self.current_token.value if self.current_token else "EOF"
            current_pos = self.current_token.position if self.current_token else "end"
            raise AIFPLParseError(f"Unexpected token after expression: {current_value} at position {current_pos}")

        return expr

    def _parse_expression(self) -> AIFPLParsedExpression:
        """Parse a single expression (atom or list)."""
        if self.current_token is None:
            raise AIFPLParseError("Unexpected end of input, expected expression")

        start_pos = self.current_token.position

        if self.current_token.type == AIFPLTokenType.LPAREN:
            return self._parse_list(start_pos)

        if self.current_token.type in (AIFPLTokenType.NUMBER, AIFPLTokenType.SYMBOL,
                                       AIFPLTokenType.STRING, AIFPLTokenType.BOOLEAN):
            return self._parse_atom(start_pos)

        raise AIFPLParseError(f"Unexpected token: {self.current_token.value} at position {self.current_token.position}")

    def _parse_list(self, start_pos: int) -> AIFPLParsedExpression:
        """Parse (operator arg1 arg2 ...) or special forms."""
        self._consume(AIFPLTokenType.LPAREN)

        elements = []
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            if self.current_token.type == AIFPLTokenType.EOF:
                raise AIFPLParseError(f"Unclosed parenthesis starting at position {start_pos}")

            elements.append(self._parse_expression().expr)

        if self.current_token is None:
            raise AIFPLParseError(f"Unterminated list starting at position {start_pos}, expected ')'")

        end_pos = self.current_token.position
        self._consume(AIFPLTokenType.RPAREN)

        # Handle empty lists - return as empty Python list
        if not elements:
            return AIFPLParsedExpression([], start_pos, end_pos)

        # Check for special forms first
        if isinstance(elements[0], str):
            if elements[0] == "lambda":
                lambda_expr = self._parse_lambda_form(elements, start_pos)
                return AIFPLParsedExpression(lambda_expr, start_pos, end_pos)

            if elements[0] == "let":
                let_expr = self._parse_let_form(elements, start_pos)
                return AIFPLParsedExpression(let_expr, start_pos, end_pos)

        # Regular function call - create AIFPLFunctionCall object
        func_call = AIFPLFunctionCall(
            function=elements[0],
            arguments=elements[1:],
            position=start_pos
        )
        return AIFPLParsedExpression(func_call, start_pos, end_pos)

    def _parse_lambda_form(self, elements: List[AIFPLSExpression], start_pos: int) -> AIFPLLambdaExpr:
        """Parse (lambda (param1 param2 ...) body)."""
        if len(elements) != 3:
            raise AIFPLParseError(
                f"Lambda expression requires exactly 3 elements: (lambda (params...) body) at position {start_pos}"
            )

        # Parse parameter list - handle the case where it might be a AIFPLFunctionCall or empty list
        param_expr = elements[1]

        # Extract parameters and ensure they're all strings
        raw_parameters: List[AIFPLSExpression] = []

        # Handle different parameter list formats
        if isinstance(param_expr, AIFPLFunctionCall):
            # For lambda parameters, we expect (param1 param2 ...) which becomes AIFPLFunctionCall(param1, [param2, ...])
            raw_parameters = [param_expr.function] + param_expr.arguments

        elif isinstance(param_expr, list):
            # This handles empty parameter lists: () -> []
            raw_parameters = param_expr

        else:
            # Single parameter without parentheses (not standard but handle gracefully)
            if isinstance(param_expr, str):
                raw_parameters = [param_expr]

            else:
                raise AIFPLParseError(
                    f"Lambda parameter list must be a list or symbol, got {type(param_expr).__name__} at position {start_pos}"
                )

        # Validate parameters are all strings and convert them
        parameters: List[str] = []
        for param in raw_parameters:
            if not isinstance(param, str):
                raise AIFPLParseError(f"Lambda parameter must be a symbol, got {type(param).__name__} at position {start_pos}")

            parameters.append(param)

        # Check for duplicate parameters
        if len(parameters) != len(set(parameters)):
            duplicates = [p for p in parameters if parameters.count(p) > 1]
            raise AIFPLParseError(f"Duplicate lambda parameters: {duplicates} at position {start_pos}")

        body = elements[2]

        return AIFPLLambdaExpr(parameters=parameters, body=body, position=start_pos)

    def _parse_let_form(self, elements: List[AIFPLSExpression], start_pos: int) -> AIFPLLetExpr:
        """Parse (let ((var1 val1) (var2 val2) ...) body)."""
        if len(elements) != 3:
            raise AIFPLParseError(f"Let expression requires exactly 3 elements: (let ((bindings...)) body) at position {start_pos}")

        # Parse binding list - handle the case where it might be a AIFPLFunctionCall
        binding_expr = elements[1]

        # Convert AIFPLFunctionCall back to list for binding lists
        if isinstance(binding_expr, AIFPLFunctionCall):
            # For let bindings, we expect ((var1 val1) (var2 val2) ...)
            # which becomes AIFPLFunctionCall((var1 val1), [(var2 val2), ...])
            binding_list = [binding_expr.function] + binding_expr.arguments

        elif isinstance(binding_expr, list):
            binding_list = binding_expr

        else:
            raise AIFPLParseError(f"Let binding list must be a list, got {type(binding_expr).__name__} at position {start_pos}")

        bindings = []
        for binding in binding_list:
            # Each binding might also be a AIFPLFunctionCall
            if isinstance(binding, AIFPLFunctionCall):
                # (var val) becomes AIFPLFunctionCall(var, [val])
                if len(binding.arguments) != 1:
                    raise AIFPLParseError(f"Let binding must be a list of 2 elements: (var value) at position {start_pos}")

                var_name = binding.function
                var_value = binding.arguments[0]

            elif isinstance(binding, list) and len(binding) == 2:
                var_name, var_value = binding

            else:
                raise AIFPLParseError(f"Let binding must be a list of 2 elements: (var value) at position {start_pos}")

            if not isinstance(var_name, str):
                raise AIFPLParseError(
                    f"Let binding variable must be a symbol, got {type(var_name).__name__} at position {start_pos}"
                )

            bindings.append((var_name, var_value))

        # Check for duplicate binding names
        var_names = [name for name, _ in bindings]
        if len(var_names) != len(set(var_names)):
            duplicates = [name for name in var_names if var_names.count(name) > 1]
            raise AIFPLParseError(f"Duplicate let binding variables: {duplicates} at position {start_pos}")

        body = elements[2]

        return AIFPLLetExpr(bindings=bindings, body=body, position=start_pos)

    def _parse_atom(self, start_pos: int) -> AIFPLParsedExpression:
        """Parse an atomic value (number, string, boolean, or symbol)."""
        assert self.current_token is not None, "_parse_atom called with None token"

        token = self.current_token
        self._advance()

        end_pos = start_pos + token.length

        # Wrap string tokens in AIFPLStringLiteral to distinguish from symbols
        if token.type == AIFPLTokenType.STRING:
            return AIFPLParsedExpression(AIFPLStringLiteral(token.value), start_pos, end_pos)

        return AIFPLParsedExpression(token.value, start_pos, end_pos)

    def _consume(self, expected_type: AIFPLTokenType) -> None:
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
