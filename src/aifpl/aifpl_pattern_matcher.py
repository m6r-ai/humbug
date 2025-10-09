"""Pattern matching system for AIFPL expressions."""

from typing import Callable

from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol,
    AIFPLList, AIFPLFunction, AIFPLBuiltinFunction
)


class AIFPLPatternMatcher:
    """Handles pattern matching operations for AIFPL match expressions."""

    def __init__(self, format_result_func: Callable[[AIFPLValue], str]):
        """
        Initialize pattern matcher.

        Args:
            format_result_func: Function to format AIFPLValue for display
        """
        self.format_result = format_result_func

    def evaluate_match_form(
        self,
        match_list: AIFPLList,
        env: AIFPLEnvironment,
        depth: int,
        evaluate_expression_func: Callable[[AIFPLValue, AIFPLEnvironment, int], AIFPLValue]
    ) -> AIFPLValue:
        """
        Evaluate (match value (pattern1 result1) (pattern2 result2) ...) form.

        Args:
            match_list: List representing match expression
            env: Current environment
            depth: Current recursion depth
            evaluate_expression_func: Function to evaluate expressions

        Returns:
            Result of the first matching pattern
        """
        if match_list.length() < 3:
            raise AIFPLEvalError(
                message="Match expression has wrong number of arguments",
                received=f"Got {match_list.length() - 1} arguments: {self.format_result(match_list)}",
                expected="At least 2 arguments: (match value (pattern1 result1) ...)",
                example="(match x ((42) \"found forty-two\") (_ \"something else\"))",
                suggestion="Match needs a value to match and at least one pattern clause"
            )

        # Validate ALL pattern clauses upfront before any matching begins
        for i in range(2, match_list.length()):
            clause = match_list.get(i)
            clause_num = i - 1

            # Validate clause structure: (pattern result)
            if not isinstance(clause, AIFPLList) or clause.length() != 2:
                if isinstance(clause, AIFPLList):
                    raise AIFPLEvalError(
                        message=f"Match clause {clause_num} has wrong number of elements",
                        received=f"Clause {clause_num}: {self.format_result(clause)} (has {clause.length()} elements)",
                        expected="Each clause needs exactly 2 elements: (pattern result)",
                        example="(match x ((42) \"found\") ((string? s) s))",
                        suggestion="Each clause: (pattern result-expression)"
                    )

                raise AIFPLEvalError(
                    message=f"Match clause {clause_num} must be a list",
                    received=f"Clause {clause_num}: {self.format_result(clause)} ({clause.type_name()})",
                    expected="List with pattern and result: (pattern result)",
                    example="(match x ((42) \"found\") (_ \"default\"))",
                    suggestion="Wrap each clause in parentheses: (pattern result)"
                )

            # Validate pattern syntax
            pattern = clause.get(0)
            try:
                self._validate_pattern_syntax(pattern)

            except AIFPLEvalError as e:
                raise AIFPLEvalError(
                    message=f"Invalid pattern in clause {clause_num}",
                    context=str(e),
                    received=f"Pattern: {self.format_result(pattern)}",
                    suggestion="Fix the pattern syntax before matching"
                ) from e

        # Evaluate the value to match against
        value_to_match = evaluate_expression_func(match_list.get(1), env, depth + 1)

        # Try each pattern clause in order
        for i in range(2, match_list.length()):
            clause = match_list.get(i)
            assert isinstance(clause, AIFPLList) and clause.length() == 2, "Clause structure validated earlier"
            pattern = clause.get(0)
            result_expr = clause.get(1)

            # Try to match the pattern
            match_result = self._try_match_pattern(pattern, value_to_match, env)
            if match_result is not None:
                # Evaluate result in the match environment
                return evaluate_expression_func(result_expr, match_result, depth + 1)

        # No patterns matched
        raise AIFPLEvalError(
            message="No patterns matched in match expression",
            received=f"Value: {self.format_result(value_to_match)}",
            expected="At least one pattern should match, or add a wildcard pattern",
            example="(match x ((42) \"found\") (_ \"default\"))",
            suggestion="Add a wildcard pattern (_ result) as the last clause to catch all cases"
        )

    def _try_match_pattern(
        self,
        pattern: AIFPLValue,
        value: AIFPLValue,
        env: AIFPLEnvironment
    ) -> AIFPLEnvironment | None:
        """
        Try to match a pattern against a value.
        Assumes pattern is syntactically valid (validation done upfront).

        Args:
            pattern: Pattern to match
            value: Value to match against
            env: Current environment

        Returns:
            new_env_with_bindings if match succeeds, None if no match
        """
        if isinstance(pattern, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            if pattern.to_python() == value.to_python():
                return env

            return None

        if isinstance(pattern, AIFPLSymbol):
            if pattern.name == "_":  # Wildcard - always matches, no binding
                return env

            # Variable binding - bind the symbol to the value
            new_env = env.define(pattern.name, value)
            return new_env

        assert isinstance(pattern, AIFPLList), "Pattern type checked earlier"
        return self._try_match_list_pattern(pattern, value, env)

    def _try_match_list_pattern(
        self,
        pattern: AIFPLList,
        value: AIFPLValue,
        env: AIFPLEnvironment
    ) -> AIFPLEnvironment | None:
        """
        Try to match a list pattern against a value.
        Assumes pattern is syntactically valid (validation done upfront).

        Args:
            pattern: List pattern to match
            value: Value to match against
            env: Current environment

        Returns:
            new_env_with_bindings if match succeeds, None if no match
        """
        # Type patterns
        type_predicate = self._is_type_pattern(pattern)
        if type_predicate is not None:
            var_pattern = pattern.get(1)

            # Check if value matches the type predicate
            if self._matches_type_predicate(value, type_predicate):
                # If type matches, try to match the variable pattern
                return self._try_match_pattern(var_pattern, value, env)

            return None

        if not isinstance(value, AIFPLList):
            return None

        # Empty list pattern
        if pattern.is_empty():
            if value.is_empty():
                return env

            return None

        # Head/tail patterns: Support both (head . tail) and (a b c . rest)
        for dot_position in range(pattern.length()):
            element = pattern.get(dot_position)
            if isinstance(element, AIFPLSymbol) and element.name == ".":
                return self._match_head_tail_pattern(pattern, value, env, dot_position)

        # Fixed-length list pattern: (p1 p2 p3 ...)
        if pattern.length() != value.length():
            return None

        # Match each element in sequence
        current_env = env
        for i in range(pattern.length()):
            element_result = self._try_match_pattern(pattern.get(i), value.get(i), current_env)
            if element_result is None:
                return None

            current_env = element_result

        return current_env

    def _matches_type_predicate(self, value: AIFPLValue, type_pred: str) -> bool:
        """
        Check if value matches a type predicate.

        Args:
            value: Value to check
            type_pred: Type predicate name (e.g., "number?", "string?")

        Returns:
            True if value matches the predicate
        """
        type_checks = {
            'number?': lambda v: isinstance(v, AIFPLNumber),
            'integer?': lambda v: isinstance(v, AIFPLNumber) and v.is_integer(),
            'float?': lambda v: isinstance(v, AIFPLNumber) and v.is_float(),
            'complex?': lambda v: isinstance(v, AIFPLNumber) and v.is_complex(),
            'string?': lambda v: isinstance(v, AIFPLString),
            'boolean?': lambda v: isinstance(v, AIFPLBoolean),
            'list?': lambda v: isinstance(v, AIFPLList),
            'function?': lambda v: isinstance(v, (AIFPLFunction, AIFPLBuiltinFunction)),
            'symbol?': lambda v: isinstance(v, AIFPLSymbol),
        }

        checker = type_checks.get(type_pred)
        return checker(value) if checker else False

    def _match_head_tail_pattern(
        self,
        pattern: AIFPLList,
        value: AIFPLList,
        env: AIFPLEnvironment,
        dot_position: int
    ) -> AIFPLEnvironment | None:
        """
        Match a head/tail pattern like (a b . rest) against a list value.
        Assumes pattern is syntactically valid (validation done upfront).

        Args:
            pattern: Pattern containing dot (assumed valid)
            value: List value to match
            env: Current environment
            dot_position: Index of the dot in the pattern

        Returns:
            new_env if match succeeds, None if no match
        """
        # Pattern structure is assumed valid - no validation needed here
        num_head_elements = dot_position

        # Check if we have enough elements in the value (semantic check)
        if value.length() < num_head_elements:
            return None

        # Match head elements
        current_env = env
        for i in range(num_head_elements):
            head_result = self._try_match_pattern(pattern.get(i), value.get(i), current_env)
            if head_result is None:
                return None

            current_env = head_result

        # Match tail (remaining elements as a list)
        tail_pattern = pattern.get(dot_position + 1)
        tail_elements = value.elements[num_head_elements:]
        tail_value = AIFPLList(tail_elements)

        tail_result = self._try_match_pattern(tail_pattern, tail_value, current_env)
        if tail_result is None:
            return None

        return tail_result

    def _is_valid_type_predicate(self, type_pred: str) -> bool:
        """
        Check if a type predicate is valid.

        Args:
            type_pred: Type predicate to validate

        Returns:
            True if valid type predicate
        """
        valid_predicates = {
            'number?', 'integer?', 'float?', 'complex?',
            'string?', 'boolean?', 'list?', 'function?', 'symbol?'
        }
        return type_pred in valid_predicates

    def _validate_pattern_syntax(self, pattern: AIFPLValue) -> None:
        """
        Validate pattern syntax completely upfront before matching begins.
        This is the single source of truth for pattern syntax validation.

        Args:
            pattern: Pattern to validate

        Raises:
            AIFPLEvalError: If pattern syntax is invalid
        """
        # Literals are always valid
        if isinstance(pattern, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            return

        # Variable patterns (symbols)
        if isinstance(pattern, AIFPLSymbol):
            return

        assert isinstance(pattern, AIFPLList), "Pattern type checked earlier"
        self._validate_list_pattern_syntax(pattern)

    def _validate_list_pattern_syntax(self, pattern: AIFPLList) -> None:
        """
        Validate list pattern syntax.

        Args:
            pattern: List pattern to validate

        Raises:
            AIFPLEvalError: If pattern syntax is invalid
        """
        # Empty list is valid
        if pattern.is_empty():
            return

        # Check for type patterns: (type? var)
        type_predicate = self._is_type_pattern(pattern)
        if type_predicate is not None:
            var_pattern = pattern.get(1)

            # Validate type predicate is known
            if not self._is_valid_type_predicate(type_predicate):
                raise AIFPLEvalError(
                    message="Invalid type pattern",
                    received=f"Type pattern: ({type_predicate} {self.format_result(var_pattern)})",
                    expected="Valid type predicate like number?, string?, list?, etc.",
                    example="(number? n) or (string? s)",
                    suggestion="Use a valid type predicate ending with ?"
                )

            # Variable in type pattern must be a symbol
            if not isinstance(var_pattern, AIFPLSymbol):
                raise AIFPLEvalError(
                    message="Pattern variable must be a symbol",
                    received=f"Variable in type pattern: {self.format_result(var_pattern)} ({var_pattern.type_name()})",
                    expected="Symbol (variable name)",
                    example="(number? x) not (number? 42) or (number? \"x\")",
                    suggestion="Use unquoted variable names in type patterns"
                )

            return

        # Check for malformed type patterns (wrong number of arguments)
        first_elem = pattern.get(0)
        if isinstance(first_elem, AIFPLSymbol) and self._is_valid_type_predicate(first_elem.name):
            type_predicate = first_elem.name

            # Are we missing a variable?  e.g. (number?)
            if pattern.length() == 1:
                raise AIFPLEvalError(
                    message="Invalid type pattern",
                    received=f"Type pattern: ({type_predicate}) - missing variable",
                    expected="Type pattern with variable: (type? var)",
                    example="(number? x) not (number?)",
                    suggestion="Add a variable name after the type predicate"
                )

            # Do we have too many variables?  e.g. (number? x y)
            raise AIFPLEvalError(
                message="Invalid type pattern",
                received=f"Type pattern: {self.format_result(pattern)} - too many variables",
                expected="Type pattern with one variable: (type? var)",
                example="(number? x) not (number? x y)",
                suggestion="Use only one variable in type patterns"
            )

        # Comprehensive dot pattern validation
        dot_positions = []
        for i in range(pattern.length()):
            element = pattern.get(i)
            if isinstance(element, AIFPLSymbol) and element.name == ".":
                dot_positions.append(i)

        if len(dot_positions) > 1:
            # Multiple dots: (a . b . c)
            raise AIFPLEvalError(
                message="Invalid cons pattern",
                received=f"Pattern: {self.format_result(pattern)} - multiple dots",
                expected="At most one dot in cons pattern",
                example="(head . tail) or (a b . rest) not (a . b . c)",
                suggestion="Use only one dot to separate head from tail"
            )

        if len(dot_positions) == 1:
            dot_pos = dot_positions[0]

            # Dot at beginning: (. a b)
            if dot_pos == 0:
                raise AIFPLEvalError(
                    message="Invalid cons pattern",
                    received=f"Pattern: {self.format_result(pattern)} - dot at beginning",
                    expected="Elements before dot in cons pattern",
                    example="(head . tail) or (a b . rest) not (. a b)",
                    suggestion="Put at least one element before the dot"
                )

            # Dot at end: (a b .)
            if dot_pos == pattern.length() - 1:
                raise AIFPLEvalError(
                    message="Invalid cons pattern",
                    received=f"Pattern: {self.format_result(pattern)} - dot at end",
                    expected="Pattern like (head . tail) or (a b . rest)",
                    example="(head . tail) or (first second . rest)",
                    suggestion="Put a tail pattern after the dot"
                )

            # Multiple elements after dot: (a . b c)
            if dot_pos != pattern.length() - 2:
                raise AIFPLEvalError(
                    message="Invalid cons pattern",
                    received=f"Pattern: {self.format_result(pattern)} - multiple elements after dot",
                    expected="Pattern like (head . tail) or (a b . rest)",
                    example="(head . tail) or (first second . rest)",
                    suggestion="Use only one tail variable after the dot"
                )

        # Recursively validate all elements (except dots which are structural)
        for i in range(pattern.length()):
            element = pattern.get(i)
            # Skip dot symbols - they're structural, not patterns
            if isinstance(element, AIFPLSymbol) and element.name == ".":
                continue

            self._validate_pattern_syntax(element)

    def _is_type_pattern(self, pattern: AIFPLValue) -> str | None:
        """Check if pattern is a type pattern like (number? x), return predicate name or None."""
        if (isinstance(pattern, AIFPLList) and pattern.length() == 2):
            first_elem = pattern.get(0)
            if (isinstance(first_elem, AIFPLSymbol) and first_elem.name.endswith('?')):
                return first_elem.name

        return None
