"""AIFPL Desugarer - transforms complex constructs into core language.

The desugarer takes a full AST (including match, and, or) and transforms it
into a core AST containing only:
- Literals (numbers, strings, booleans)
- Variables (symbols)
- If expressions
- Let expressions
- Lambda expressions
- Function calls

This simplifies the compiler and enables better optimization.
"""

from typing import List, Tuple, Any, cast
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLSymbol, AIFPLList, AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLString, AIFPLBoolean
)
from aifpl.aifpl_error import AIFPLEvalError


class AIFPLDesugarer:
    """Transforms complex AIFPL constructs into core language."""

    def __init__(self) -> None:
        self.temp_counter = 0  # For generating unique temp variable names

    def desugar(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Desugar an expression recursively.

        Args:
            expr: AST to desugar

        Returns:
            Desugared AST (core language only)
        """
        # Lists need inspection - anything else does not
        if not isinstance(expr, AIFPLList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()
        if isinstance(first, AIFPLSymbol):
            name = first.name

            # Match expression - desugar it!
            if name == 'match':
                return self._desugar_match(expr)

            # Core constructs - desugar children only
            if name == 'if':
                return self._desugar_if(expr)

            if name == 'let':
                return self._desugar_let(expr)

            if name == 'lambda':
                return self._desugar_lambda(expr)

            if name == 'quote':
                # Quote: don't desugar the quoted expression
                return expr

            if name == 'trace':
                # Trace is a special form - handle it
                return self._desugar_trace(expr)

        # Regular function call - desugar all elements
        return self._desugar_call(expr)

    def _desugar_if(self, expr: AIFPLList) -> AIFPLValue:
        """Desugar if expression by desugaring its subexpressions."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 4, "If expression should have exactly 4 elements (validated by semantic analyzer)"

        _, condition, then_expr, else_expr = expr.elements

        # Desugar all subexpressions
        desugared_condition = self.desugar(condition)
        desugared_then = self.desugar(then_expr)
        desugared_else = self.desugar(else_expr)

        return AIFPLList((
            AIFPLSymbol('if'),
            desugared_condition,
            desugared_then,
            desugared_else
        ))

    def _desugar_let(self, expr: AIFPLList) -> AIFPLValue:
        """Desugar let expression by desugaring its subexpressions."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements (validated by semantic analyzer)"

        let_symbol = expr.elements[0]
        bindings_list = expr.elements[1]
        body = expr.elements[2]

        assert isinstance(bindings_list, AIFPLList), "Binding list should be a list (validated by semantic analyzer)"

        # Desugar each binding value
        desugared_bindings = []
        for i, binding in enumerate(bindings_list.elements):
            assert isinstance(binding, AIFPLList) and len(binding.elements) == 2, \
                f"Binding {i+1} should be a list with 2 elements (validated by semantic analyzer)"

            var_name, value_expr = binding.elements
            desugared_value = self.desugar(value_expr)
            desugared_bindings.append(AIFPLList((var_name, desugared_value)))

        # Desugar body
        desugared_body = self.desugar(body)

        return AIFPLList((
            let_symbol,
            AIFPLList(tuple(desugared_bindings)),
            desugared_body
        ))

    def _desugar_lambda(self, expr: AIFPLList) -> AIFPLValue:
        """Desugar lambda expression by desugaring its body."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Lambda expression should have exactly 3 elements (validated by semantic analyzer)"

        lambda_symbol, params_list, body = expr.elements

        # Desugar body
        desugared_body = self.desugar(body)

        return AIFPLList((lambda_symbol, params_list, desugared_body))

    def _desugar_trace(self, expr: AIFPLList) -> AIFPLValue:
        """
        Desugar trace special form.

        (trace msg1 msg2 ... msgN expr)

        Trace is kept as-is but with desugared subexpressions.
        The IR builder will handle creating the trace IR node.
        """
        if expr.length() < 3:  # (trace msg expr) minimum
            raise AIFPLEvalError(
                message="trace requires at least 2 arguments (message, expr)",
                suggestion="Usage: (trace \"message\" expression)"
            )

        # Desugar all subexpressions
        desugared_elements: List[AIFPLValue] = [AIFPLSymbol('trace')]
        for elem in expr.elements[1:]:  # Skip 'trace' symbol
            desugared_elements.append(self.desugar(elem))

        return AIFPLList(tuple(desugared_elements))

    def _desugar_call(self, expr: AIFPLList) -> AIFPLValue:
        """Desugar function call by desugaring all elements."""
        desugared_elements = []
        for elem in expr.elements:
            desugared_elements.append(self.desugar(elem))

        return AIFPLList(tuple(desugared_elements))

    def _desugar_match(self, expr: AIFPLList) -> AIFPLValue:
        """
        Transform match expression into if/let expressions.

        Args:
            expr: Match expression AST

        Returns:
            Desugared if/let AST
        """
        # Validation already done by semantic analyzer
        assert len(expr.elements) >= 3, "Match expression should have at least 3 elements (validated by semantic analyzer)"

        value_expr = expr.elements[1]
        clauses = list(expr.elements[2:])

        # All clauses already validated by semantic analyzer
        for i, clause in enumerate(clauses):
            assert isinstance(clause, AIFPLList) and len(clause.elements) == 2, \
                f"Clause {i+1} should be a list with 2 elements (validated by semantic analyzer)"

        # Generate temp variable for match value
        temp_var = self._gen_temp()

        # Desugar the value expression
        desugared_value = self.desugar(value_expr)

        # Build the match logic as nested if/let expressions
        match_logic = self._build_match_clauses(temp_var, clauses)

        # Wrap in let to bind the temp variable
        # (let ((temp value)) match-logic)
        result = AIFPLList((
            AIFPLSymbol('let'),
            AIFPLList((
                AIFPLList((AIFPLSymbol(temp_var), desugared_value)),
            )),
            match_logic
        ))

        return result

    def _build_match_clauses(self, temp_var: str, clauses: List[AIFPLValue]) -> AIFPLValue:
        """
        Build nested if/let structure for match clauses.

        Args:
            temp_var: Name of temp variable holding the match value
            clauses: List of (pattern, result) clauses

        Returns:
            Nested if/let AST
        """
        if not clauses:
            # No clauses - this shouldn't happen, but handle it
            raise AIFPLEvalError(
                message="Match expression must have at least one clause"
            )

        # Process clauses in reverse order to build nested structure
        result: AIFPLValue | None = None

        for i in range(len(clauses) - 1, -1, -1):
            clause = clauses[i]
            if not isinstance(clause, AIFPLList):
                raise AIFPLEvalError("Clause must be a list")

            pattern = clause.elements[0]
            result_expr = clause.elements[1]

            # Desugar the result expression
            desugared_result = self.desugar(result_expr)

            # Desugar the pattern into (test_expr, bindings)
            try:
                test_expr, bindings = self._desugar_pattern(pattern, temp_var)

            except AIFPLEvalError as e:
                # Add clause context to error message
                clause_num = i + 1
                raise AIFPLEvalError(
                    message=f"Invalid pattern in clause {clause_num}",
                    context=str(e)
                ) from e

            if i == len(clauses) - 1:
                # Last clause - if it fails, raise an error
                if result is None:
                    # This is the only clause, or we're building from the last
                    result = self._build_clause_with_bindings(
                        test_expr,
                        bindings,
                        desugared_result,
                        self._build_no_match_error()
                    )

            else:
                # Not the last clause - chain with else
                # result is guaranteed to be set here because we process in reverse
                assert result is not None
                result = self._build_clause_with_bindings(
                    test_expr,
                    bindings,
                    desugared_result,
                    result
                )

        assert result is not None
        return result

    def _build_clause_with_bindings(
        self,
        test_expr: AIFPLValue,
        bindings: List[Tuple[str, Any]],  # All bindings (temp + pattern vars or special markers)
        result_expr: AIFPLValue,
        else_expr: AIFPLValue
    ) -> AIFPLValue:
        """
        Build if/let structure for a single clause.

        Args:
            test_expr: Expression that tests if pattern matches
            bindings: List of (var_name, value_expr) to bind if pattern matches
            result_expr: Expression to evaluate if pattern matches
            else_expr: Expression to evaluate if pattern doesn't match

        Returns:
            (if test_expr
                (let (bindings...) result_expr)
                else_expr)

        Note: If bindings include temp variables that are referenced in test_expr,
        we need to wrap the entire if in a let for those temp bindings.
        """
        # Check if this is a list pattern (special marker)
        if (bindings and len(bindings) == 1 and bindings[0][0].startswith('__LIST_PATTERN_')):
            # This is a list pattern - use special building logic
            element_info = bindings[0][1]
            return self._build_list_pattern_clause(
                test_expr,
                element_info,
                result_expr,
                else_expr
            )

        # Check if this is a cons pattern (special marker)
        if (bindings and len(bindings) == 1 and bindings[0][0].startswith('__CONS_PATTERN_')):
            # This is a cons pattern - use same building logic as list pattern
            element_info = bindings[0][1]
            return self._build_list_pattern_clause(
                test_expr,
                element_info,
                result_expr,
                else_expr
            )

        # Separate different types of bindings:
        # 1. Element extraction bindings (list-ref) - must go inside then branch
        # 2. Other temp bindings - can go outside
        # 3. Pattern bindings - go inside then branch
        element_extraction_bindings: List[Tuple[str, AIFPLValue]] = []
        temp_bindings: List[Tuple[str, Any]] = []
        pattern_bindings: List[Tuple[str, AIFPLValue]] = []

        for var_name, value_expr in bindings:
            if var_name.startswith('#:match-tmp-'):
                # Check if this is an element extraction (list-ref)
                if isinstance(value_expr, AIFPLList) and not value_expr.is_empty():
                    first = value_expr.first()
                    if isinstance(first, AIFPLSymbol) and first.name == 'list-ref':
                        element_extraction_bindings.append((var_name, value_expr))
                    else:
                        temp_bindings.append((var_name, value_expr))
                else:
                    temp_bindings.append((var_name, value_expr))
            else:
                pattern_bindings.append((var_name, value_expr))

        # Element extraction and pattern bindings go inside the then branch
        # (they should only be evaluated after the test passes)
        if element_extraction_bindings or pattern_bindings:
            binding_list = []
            # Add element extractions first
            binding_list.extend([AIFPLList((AIFPLSymbol(vn), ve)) for vn, ve in element_extraction_bindings])

            # Then add pattern bindings
            binding_list.extend([AIFPLList((AIFPLSymbol(vn), ve)) for vn, ve in pattern_bindings])

            then_expr: AIFPLValue = AIFPLList((
                AIFPLSymbol('let'),
                AIFPLList(tuple(binding_list)),
                result_expr
            ))

        else:
            then_expr = result_expr

        # Build if expression
        if_expr = AIFPLList((
            AIFPLSymbol('if'),
            test_expr,
            then_expr,
            else_expr
        ))

        # Other temp bindings (not element extractions) go outside the if
        # (they're safe to evaluate before the test)
        if temp_bindings:
            binding_list = []
            for var_name, value_expr in temp_bindings:
                binding_list.append(AIFPLList((AIFPLSymbol(var_name), value_expr)))

            return AIFPLList((
                AIFPLSymbol('let'),
                AIFPLList(tuple(binding_list)),
                if_expr
            ))

        return if_expr

    def _build_no_match_error(self) -> AIFPLValue:
        """
        Build an expression that raises a no-match error.

        We need to produce the standard error message:
        "No patterns matched in match expression"

        We'll generate a call to a special form that the IR builder recognizes,
        or generate code that will produce the right error message.
        """
        # Generate: (error "No patterns matched in match expression")
        # The compiler should recognize this and emit RAISE_ERROR
        return AIFPLList((
            AIFPLSymbol('error'),
            AIFPLString("No patterns matched in match expression")
        ))

    def _desugar_pattern(
        self,
        pattern: AIFPLValue,
        temp_var: str
    ) -> Tuple[AIFPLValue, List[Tuple[str, AIFPLValue]]]:
        """
        Desugar a pattern into (test_expr, bindings).

        Args:
            pattern: Pattern AST
            temp_var: Name of temp variable holding the match value

        Returns:
            (test_expression, [(var_name, value_expr), ...])

        Example:
            Pattern: (number? n)
            Temp: "#:match-tmp-1"
            Returns: ((number? #:match-tmp-1), [("n", #:match-tmp-1)])
        """
        # Literal patterns: numbers, strings, booleans
        # Phase 1: Accept both old and new numeric types as literals
        if isinstance(pattern, (AIFPLInteger, AIFPLFloat, AIFPLComplex, AIFPLString, AIFPLBoolean)):
            # Test: (= temp_var literal)
            test_expr = AIFPLList((
                AIFPLSymbol('='),
                AIFPLSymbol(temp_var),
                pattern
            ))
            return (test_expr, [])

        # Variable pattern: binds the value
        if isinstance(pattern, AIFPLSymbol):
            if pattern.name == '_':
                # Wildcard - always matches, no binding
                return (AIFPLBoolean(True), [])

            # Variable binding - always matches, binds variable
            return (
                AIFPLBoolean(True),
                [(pattern.name, AIFPLSymbol(temp_var))]
            )

        # List patterns
        if isinstance(pattern, AIFPLList):
            return self._desugar_list_pattern(pattern, temp_var)

        # Should never reach here - semantic analyzer validates patterns
        assert False, f"Unknown pattern type: {type(pattern).__name__} (should be validated by semantic analyzer)"

    def _desugar_list_pattern(
        self,
        pattern: AIFPLList,
        temp_var: str
    ) -> Tuple[AIFPLValue, List[Tuple[str, AIFPLValue]]]:
        """
        Desugar a list pattern.

        Args:
            pattern: List pattern AST
            temp_var: Name of temp variable holding the match value

        Returns:
            (test_expression, bindings)
        """
        # Empty list pattern: ()
        if pattern.is_empty():
            # Test: (null? temp_var)
            test_expr = AIFPLList((
                AIFPLSymbol('null?'),
                AIFPLSymbol(temp_var)
            ))
            return (test_expr, [])

        # Check for type pattern: (type? var)
        if (len(pattern.elements) == 2 and
            isinstance(pattern.elements[0], AIFPLSymbol) and
            pattern.elements[0].name.endswith('?')):

            type_pred = pattern.elements[0].name
            var_pattern = pattern.elements[1]

            # Validate type predicate
            valid_predicates = {
                'number?', 'integer?', 'float?', 'complex?',
                'string?', 'boolean?', 'list?', 'alist?', 'function?'
            }

            if type_pred in valid_predicates:
                # Variable pattern already validated by semantic analyzer
                assert isinstance(var_pattern, AIFPLSymbol), \
                    "Type pattern variable should be a symbol (validated by semantic analyzer)"

                # Test: (type? temp_var)
                test_expr = AIFPLList((
                    AIFPLSymbol(type_pred),
                    AIFPLSymbol(temp_var)
                ))

                # Binding: if var_pattern is a variable, bind it
                bindings: List[Tuple[str, AIFPLValue]] = []
                if isinstance(var_pattern, AIFPLSymbol) and var_pattern.name != '_':
                    bindings.append((var_pattern.name, AIFPLSymbol(temp_var)))

                return (test_expr, bindings)

            # Unknown type predicate - should have been caught by semantic analyzer
            assert False, f"Unknown type predicate: {type_pred} (should be validated by semantic analyzer)"

        # Malformed type patterns should have been caught by semantic analyzer
        # Continue with other pattern types

        # Check for cons pattern: (head . tail) or (a b . rest)
        dot_positions = []
        for i, elem in enumerate(pattern.elements):
            if isinstance(elem, AIFPLSymbol) and elem.name == '.':
                dot_positions.append(i)

        # Validation already done by semantic analyzer
        assert len(dot_positions) <= 1, "Pattern should have at most one dot (validated by semantic analyzer)"

        # If we have a dot, use cons pattern
        if dot_positions:
            dot_position = dot_positions[0]
            return self._desugar_cons_pattern(pattern, temp_var, dot_position)

        # Fixed-length list pattern: (p1 p2 p3)
        return self._desugar_fixed_list_pattern(pattern, temp_var)

    def _desugar_fixed_list_pattern(
        self,
        pattern: AIFPLList,
        temp_var: str
    ) -> Tuple[AIFPLValue, List[Tuple[str, Any]]]:
        """
        Desugar a fixed-length list pattern like (a b c).

        Args:
            pattern: List pattern AST
            temp_var: Name of temp variable holding the match value

        Returns:
            (test_expression, bindings)
        """
        num_elements = len(pattern.elements)

        # Test: (and (list? temp_var) (= (length temp_var) num_elements))
        # We'll build this as nested ifs for simplicity

        # First test: (list? temp_var)
        list_test = AIFPLList((
            AIFPLSymbol('list?'),
            AIFPLSymbol(temp_var)
        ))

        # Second test: (= (length temp_var) num_elements)
        length_test = AIFPLList((
            AIFPLSymbol('='),
            AIFPLList((
                AIFPLSymbol('length'),
                AIFPLSymbol(temp_var)
            )),
            AIFPLInteger(num_elements)
        ))

        # Combine with and
        combined_test = AIFPLList((
            AIFPLSymbol('and'),
            list_test,
            length_test
        ))

        # For fixed-length list patterns, we need a special structure:
        # (and (list? x) (= (length x) n))  <- basic test
        # Then INSIDE the then branch:
        #   - Extract elements
        #   - Test element patterns
        #   - Bind pattern variables
        #
        # We can't return this as (test, bindings) because element tests
        # reference element temp vars which must be bound after length check.
        #
        # Solution: Return a special marker that tells the caller to use
        # a different building strategy.

        # Collect element pattern info
        element_info: List[Tuple[AIFPLValue, str, AIFPLValue]] = []  # List of (pattern, temp_var, extraction_expr)

        for i, elem_pattern in enumerate(pattern.elements):
            # Generate temp var for this element
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = AIFPLList((
                AIFPLSymbol('list-ref'),
                AIFPLSymbol(temp_var),
                AIFPLInteger(i)
            ))

            element_info.append((elem_pattern, elem_temp, elem_value))

        # Return the basic test and a special marker with element info
        # The caller will use this to build the proper nested structure
        # Use a unique marker name to avoid duplicates in nested patterns
        marker_name = f'__LIST_PATTERN_{self._gen_temp()}__'
        return (combined_test, [(marker_name, element_info)])

    def _flatten_nested_pattern(
        self,
        pattern: AIFPLValue,
        temp_var: str,
        extraction_bindings: List[Tuple[str, AIFPLValue]],
        element_tests: List[AIFPLValue],
        pattern_bindings: List[Tuple[str, AIFPLValue]]
    ) -> None:
        """
        Recursively flatten a nested pattern into the given lists.

        Args:
            pattern: Pattern to flatten
            temp_var: Temp variable holding the value to match
            extraction_bindings: List to append extraction bindings to
            element_tests: List to append tests to
            pattern_bindings: List to append pattern variable bindings to
        """
        # Desugar the pattern
        test, bindings = self._desugar_pattern(pattern, temp_var)

        # Check if this is a list/cons pattern (special marker)
        if (bindings and len(bindings) == 1 and
            (bindings[0][0].startswith('__LIST_PATTERN_') or
             bindings[0][0].startswith('__CONS_PATTERN_'))):
            # This is a nested list/cons pattern - flatten it
            # Cast to the expected type for element_info
            nested_element_info = cast(List[Tuple[AIFPLValue, str, AIFPLValue]], bindings[0][1])

            # Add the length/type test
            if not (isinstance(test, AIFPLBoolean) and test.value):
                element_tests.append(test)

            # Recursively flatten each element
            for elem_pattern, elem_temp, elem_value in nested_element_info:
                # Add extraction binding
                extraction_bindings.append((elem_temp, elem_value))

                # Recursively flatten this element
                self._flatten_nested_pattern(
                    elem_pattern,
                    elem_temp,
                    extraction_bindings,
                    element_tests,
                    pattern_bindings
                )

        else:
            # Regular pattern - add test and bindings
            if not (isinstance(test, AIFPLBoolean) and test.value):
                element_tests.append(test)
            pattern_bindings.extend(bindings)

    def _build_list_pattern_clause(
        self,
        length_test: AIFPLValue,
        element_info: List,
        result_expr: AIFPLValue,
        else_expr: AIFPLValue
    ) -> AIFPLValue:
        """
        Build a clause for a list pattern with proper nesting.

        Args:
            length_test: Test for list? and length
            element_info: List of (pattern, temp_var, extraction_expr)
            result_expr: Result expression to evaluate if pattern matches
            else_expr: Expression to evaluate if pattern doesn't match

        Returns:
            Properly nested if/let structure
        """
        # Build: (if length_test
        #          (let ((#:tmp-2 (list-ref x 0)) ...)
        #            (if (and elem-test-1 elem-test-2 ...)
        #                (let ((a #:tmp-2) (b #:tmp-3) ...)
        #                  result)
        #                else))
        #          else)

        # Extract elements
        extraction_bindings: List[Tuple[str, AIFPLValue]] = []
        element_tests: List[AIFPLValue] = []
        pattern_bindings: List[Tuple[str, AIFPLValue]] = []

        for elem_pattern, elem_temp, elem_value in element_info:
            # Add extraction binding
            extraction_bindings.append((elem_temp, elem_value))

            # Desugar element pattern
            elem_test, elem_bindings = self._desugar_pattern(elem_pattern, elem_temp)

            # Check if element pattern is itself a list/cons pattern (special marker)
            if (elem_bindings and len(elem_bindings) == 1 and
                    (elem_bindings[0][0].startswith('__LIST_PATTERN_') or
                     elem_bindings[0][0].startswith('__CONS_PATTERN_'))):
                # Nested list/cons pattern - use helper to recursively flatten it
                self._flatten_nested_pattern(
                    elem_pattern,
                    elem_temp,
                    extraction_bindings,
                    element_tests,
                    pattern_bindings
                )

            else:
                # Regular pattern
                # Collect element test (unless it's just #t)
                if not (isinstance(elem_test, AIFPLBoolean) and elem_test.value):
                    element_tests.append(elem_test)

                # Collect pattern bindings
                pattern_bindings.extend(elem_bindings)

        # Build the inner structure (after element extraction)
        if element_tests:
            # We have element tests - need nested if
            elem_test_combined = (
                AIFPLList((AIFPLSymbol('and'),) + tuple(element_tests)) if len(element_tests) > 1 else element_tests[0]
            )

            # Build pattern bindings let
            if pattern_bindings:
                binding_list = [AIFPLList((AIFPLSymbol(vn), ve)) for vn, ve in pattern_bindings]
                pattern_let: AIFPLValue = AIFPLList((
                    AIFPLSymbol('let'),
                    AIFPLList(tuple(binding_list)),
                    result_expr
                ))

            else:
                pattern_let = result_expr

            # Build element test if
            inner_if: AIFPLValue = AIFPLList((
                AIFPLSymbol('if'),
                elem_test_combined,
                pattern_let,
                else_expr
            ))

        else:
            # No element tests - just bind pattern vars
            if pattern_bindings:
                binding_list = [AIFPLList((AIFPLSymbol(vn), ve)) for vn, ve in pattern_bindings]
                inner_if = AIFPLList((
                    AIFPLSymbol('let'),
                    AIFPLList(tuple(binding_list)),
                    result_expr
                ))

            else:
                inner_if = result_expr

        # Wrap in element extraction let
        extraction_binding_list = [AIFPLList((AIFPLSymbol(vn), ve)) for vn, ve in extraction_bindings]
        extraction_let = AIFPLList((
            AIFPLSymbol('let'),
            AIFPLList(tuple(extraction_binding_list)),
            inner_if
        ))

        # Build outer if with length test
        return AIFPLList((
            AIFPLSymbol('if'),
            length_test,
            extraction_let,
            else_expr
        ))

    def _desugar_cons_pattern(
        self,
        pattern: AIFPLList,
        temp_var: str,
        dot_position: int
    ) -> Tuple[AIFPLValue, List[Tuple[str, Any]]]:
        """
        Desugar a cons pattern like (head . tail) or (a b . rest).

        Args:
            pattern: List pattern AST
            temp_var: Name of temp variable holding the match value
            dot_position: Index of the dot in the pattern

        Returns:
            (test_expression, bindings)
        """
        # Validation already done by semantic analyzer
        assert dot_position > 0, "Dot should not be at beginning (validated by semantic analyzer)"
        assert dot_position < len(pattern.elements) - 1, "Dot should not be at end (validated by semantic analyzer)"
        assert dot_position == len(pattern.elements) - 2, \
            "Should have exactly one element after dot (validated by semantic analyzer)"

        # Test: (and (list? temp_var) (>= (length temp_var) dot_position))
        list_test = AIFPLList((
            AIFPLSymbol('list?'),
            AIFPLSymbol(temp_var)
        ))

        if dot_position > 0:
            length_test = AIFPLList((
                AIFPLSymbol('>='),
                AIFPLList((
                    AIFPLSymbol('length'),
                    AIFPLSymbol(temp_var)
                )),
                AIFPLInteger(dot_position)
            ))
            combined_test = AIFPLList((
                AIFPLSymbol('and'),
                list_test,
                length_test
            ))

        else:
            # (. tail) pattern - just need non-empty list
            non_empty_test = AIFPLList((
                AIFPLSymbol('not'),
                AIFPLList((
                    AIFPLSymbol('null?'),
                    AIFPLSymbol(temp_var)
                ))
            ))
            combined_test = AIFPLList((
                AIFPLSymbol('and'),
                list_test,
                non_empty_test
            ))

        # Collect head element info
        head_elements: List[Tuple[AIFPLValue, str, AIFPLValue]] = []

        for i in range(dot_position):
            elem_pattern = pattern.elements[i]
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = AIFPLList((
                AIFPLSymbol('list-ref'),
                AIFPLSymbol(temp_var),
                AIFPLInteger(i)
            ))

            head_elements.append((elem_pattern, elem_temp, elem_value))

        # Build binding for tail
        tail_pattern = pattern.elements[dot_position + 1]
        tail_temp = self._gen_temp()

        # Extract tail: (drop dot_position temp_var)
        tail_value = AIFPLList((
            AIFPLSymbol('drop'),
            AIFPLInteger(dot_position),
            AIFPLSymbol(temp_var)
        ))

        # Add tail to element info
        all_elements = head_elements + [(tail_pattern, tail_temp, tail_value)]

        # Return special marker for cons pattern
        # Use a unique marker name to avoid duplicates in nested patterns
        marker_name = f'__CONS_PATTERN_{self._gen_temp()}__'
        return (combined_test, [(marker_name, all_elements)])

    def _gen_temp(self) -> str:
        """Generate unique temporary variable name."""
        self.temp_counter += 1
        return f"#:match-tmp-{self.temp_counter}"
