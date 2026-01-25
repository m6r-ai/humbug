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

from typing import List, Tuple, Set
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLSymbol, AIFPLList, AIFPLNumber, AIFPLString, AIFPLBoolean
)
from aifpl.aifpl_error import AIFPLEvalError


class AIFPLDesugarer:
    """Transforms complex AIFPL constructs into core language."""

    def __init__(self):
        self.temp_counter = 0  # For generating unique temp variable names

    def desugar(self, expr: AIFPLValue) -> AIFPLValue:
        """Desugar an expression recursively.

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

        # Regular function call - desugar all elements
        return self._desugar_call(expr)

    def _desugar_if(self, expr: AIFPLList) -> AIFPLValue:
        """Desugar if expression by desugaring its subexpressions."""
        if len(expr.elements) != 4:
            raise AIFPLEvalError(
                message="If expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments",
                expected="Exactly 3 arguments: (if condition then else)"
            )

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
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Let expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="At least 3 elements: (let ((bindings...)) body)"
            )

        let_symbol = expr.elements[0]
        bindings_list = expr.elements[1]
        body = expr.elements[2]

        if not isinstance(bindings_list, AIFPLList):
            raise AIFPLEvalError(
                message="Let binding list must be a list",
                received=f"Binding list: {bindings_list.type_name()}"
            )

        # Desugar each binding value
        desugared_bindings = []
        for i, binding in enumerate(bindings_list.elements):
            # Check if binding is a list
            if not isinstance(binding, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} must be a list",
                    received=f"Binding: {binding}"
                )
            
            # Check if binding has exactly 2 elements
            if len(binding.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} has wrong number of elements",
                    received=f"Binding: {binding}",
                    expected="Exactly 2 elements: (variable value)",
                    example="(x 5) not (x) or (x 1 2)"
                )

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
        if len(expr.elements) != 3:
            raise AIFPLEvalError(
                message="Lambda expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (lambda (params...) body)"
            )

        lambda_symbol, params_list, body = expr.elements

        # Desugar body
        desugared_body = self.desugar(body)

        return AIFPLList((lambda_symbol, params_list, desugared_body))

    def _desugar_call(self, expr: AIFPLList) -> AIFPLValue:
        """Desugar function call by desugaring all elements."""
        desugared_elements = []
        for elem in expr.elements:
            desugared_elements.append(self.desugar(elem))

        return AIFPLList(tuple(desugared_elements))

    def _desugar_match(self, expr: AIFPLList) -> AIFPLValue:
        """Transform match expression into if/let expressions.

        Args:
            expr: Match expression AST

        Returns:
            Desugared if/let AST
        """
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Match expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments",
                expected="At least 2 arguments: (match value (pattern1 result1) ...)"
            )

        value_expr = expr.elements[1]
        clauses = expr.elements[2:]

        # Validate all clauses
        for i, clause in enumerate(clauses):
            if not isinstance(clause, AIFPLList) or len(clause.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Match clause {i+1} has wrong number of elements",
                    received=f"Clause {i+1}: {clause}",
                    expected="Each clause needs exactly 2 elements: (pattern result)"
                )

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
        """Build nested if/let structure for match clauses.

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
        result = None

        for i in range(len(clauses) - 1, -1, -1):
            clause = clauses[i]
            if not isinstance(clause, AIFPLList):
                raise AIFPLEvalError(f"Clause must be a list")

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
                result = self._build_clause_with_bindings(
                    test_expr,
                    bindings,
                    desugared_result,
                    result
                )

        return result

    def _build_clause_with_bindings(
        self,
        test_expr: AIFPLValue,
        bindings: List[Tuple[str, AIFPLValue]],  # All bindings (temp + pattern vars)
        result_expr: AIFPLValue,
        else_expr: AIFPLValue
    ) -> AIFPLValue:
        """Build if/let structure for a single clause.

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
        if (bindings and len(bindings) == 1 and 
            bindings[0][0].startswith('__LIST_PATTERN_')):
            # This is a list pattern - use special building logic
            element_info = bindings[0][1]
            return self._build_list_pattern_clause(
                test_expr,
                element_info,
                result_expr,
                else_expr
            )

        # Check if this is a cons pattern (special marker)
        if (bindings and len(bindings) == 1 and 
            bindings[0][0].startswith('__CONS_PATTERN_')):
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
        element_extraction_bindings = []
        temp_bindings = []
        pattern_bindings = []

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

            then_expr = AIFPLList((
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
        if bindings:
            binding_list = []
            for var_name, value_expr in bindings:
                binding_list.append(AIFPLList((AIFPLSymbol(var_name), value_expr)))

            then_expr = AIFPLList((
                AIFPLSymbol('let'),
                AIFPLList(tuple(binding_list)),
                result_expr
            ))
        else:
            then_expr = result_expr

        # Build if expression
        return AIFPLList((
            AIFPLSymbol('if'),
            test_expr,
            then_expr,
            else_expr
        ))

    def _build_no_match_error(self) -> AIFPLValue:
        """Build an expression that raises a no-match error.

        We need to match the error message from the interpreter:
        "No patterns matched in match expression"

        We'll generate a call to a special form that the compiler recognizes,
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
        """Desugar a pattern into (test_expr, bindings).

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
        if isinstance(pattern, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
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
            else:
                # Variable binding - always matches, binds variable
                return (
                    AIFPLBoolean(True),
                    [(pattern.name, AIFPLSymbol(temp_var))]
                )

        # List patterns
        if isinstance(pattern, AIFPLList):
            return self._desugar_list_pattern(pattern, temp_var)

        raise AIFPLEvalError(
            message=f"Unknown pattern type: {type(pattern).__name__}",
            received=f"Pattern: {pattern}"
        )

    def _desugar_list_pattern(
        self,
        pattern: AIFPLList,
        temp_var: str
    ) -> Tuple[AIFPLValue, List[Tuple[str, AIFPLValue]]]:
        """Desugar a list pattern.

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
                # Validate variable pattern
                if not isinstance(var_pattern, AIFPLSymbol):
                    raise AIFPLEvalError(
                        message="Pattern variable must be a symbol",
                        received=f"Variable in type pattern: {var_pattern}",
                        expected="Symbol (variable name)",
                        example="(number? x) not (number? 42)",
                        suggestion="Use unquoted variable names in type patterns"
                    )

                # Test: (type? temp_var)
                test_expr = AIFPLList((
                    AIFPLSymbol(type_pred),
                    AIFPLSymbol(temp_var)
                ))

                # Binding: if var_pattern is a variable, bind it
                bindings = []
                if isinstance(var_pattern, AIFPLSymbol) and var_pattern.name != '_':
                    bindings.append((var_pattern.name, AIFPLSymbol(temp_var)))

                return (test_expr, bindings)
            else:
                # Unknown type predicate
                raise AIFPLEvalError(
                    message="Invalid type pattern",
                    received=f"Type pattern: ({type_pred} {var_pattern})",
                    expected="Valid type predicate like number?, string?, list?, etc.",
                    example="(number? x) or (string? s)",
                    suggestion="Use a valid type predicate ending with ?"
                )
                if not isinstance(var_pattern, AIFPLSymbol):
                    raise AIFPLEvalError(
                        message="Pattern variable must be a symbol",
                        received=f"Variable in type pattern: {var_pattern}",
                        expected="Symbol (variable name)",
                        example="(number? x) not (number? 42)",
                        suggestion="Use unquoted variable names in type patterns"
                    )

                # Test: (type? temp_var)
                test_expr = AIFPLList((
                    AIFPLSymbol(type_pred),
                    AIFPLSymbol(temp_var)
                ))

                # Binding: if var_pattern is a variable, bind it
                bindings = []
                if isinstance(var_pattern, AIFPLSymbol) and var_pattern.name != '_':
                    bindings.append((var_pattern.name, AIFPLSymbol(temp_var)))

                return (test_expr, bindings)

        # Check for malformed type patterns
        if (len(pattern.elements) >= 1 and
            isinstance(pattern.elements[0], AIFPLSymbol) and
            pattern.elements[0].name.endswith('?')):

            type_pred = pattern.elements[0].name
            valid_predicates = {
                'number?', 'integer?', 'float?', 'complex?',
                'string?', 'boolean?', 'list?', 'alist?', 'function?'
            }

            if type_pred in valid_predicates:
                # Type predicate but wrong number of arguments
                if len(pattern.elements) == 1:
                    raise AIFPLEvalError(
                        message="Invalid type pattern",
                        received=f"Type pattern: ({type_pred}) - missing variable",
                        expected="Type pattern with variable: (type? var)",
                        example="(number? x) not (number?)",
                        suggestion="Add a variable name after the type predicate"
                    )
                else:  # len > 2
                    raise AIFPLEvalError(
                        message="Invalid type pattern",
                        received=f"Type pattern: {pattern} - too many variables",
                        expected="Type pattern with one variable: (type? var)",
                        example="(number? x) not (number? x y)",
                        suggestion="Use only one variable in type patterns"
                    )

        # Check for cons pattern: (head . tail) or (a b . rest)
        dot_positions = []
        for i, elem in enumerate(pattern.elements):
            if isinstance(elem, AIFPLSymbol) and elem.name == '.':
                dot_positions.append(i)
        
        # Validate: at most one dot
        if len(dot_positions) > 1:
            raise AIFPLEvalError(
                message="Invalid cons pattern",
                received=f"Pattern: {pattern} - multiple dots"
            )
        
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
    ) -> Tuple[AIFPLValue, List[Tuple[str, AIFPLValue]]]:
        """Desugar a fixed-length list pattern like (a b c).

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
            AIFPLNumber(num_elements)
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
        element_info = []  # List of (pattern, temp_var, extraction_expr)

        for i, elem_pattern in enumerate(pattern.elements):
            # Generate temp var for this element
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = AIFPLList((
                AIFPLSymbol('list-ref'),
                AIFPLSymbol(temp_var),
                AIFPLNumber(i)
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
        extraction_bindings: List,
        element_tests: List,
        pattern_bindings: List
    ) -> None:
        """Recursively flatten a nested pattern into the given lists.

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
            nested_element_info = bindings[0][1]

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
        """Build a clause for a list pattern with proper nesting.

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
        extraction_bindings = []
        element_tests = []
        pattern_bindings = []

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
            elem_test_combined = AIFPLList((AIFPLSymbol('and'),) + tuple(element_tests)) if len(element_tests) > 1 else element_tests[0]

            # Build pattern bindings let
            if pattern_bindings:
                binding_list = [AIFPLList((AIFPLSymbol(vn), ve)) for vn, ve in pattern_bindings]
                pattern_let = AIFPLList((
                    AIFPLSymbol('let'),
                    AIFPLList(tuple(binding_list)),
                    result_expr
                ))
            else:
                pattern_let = result_expr

            # Build element test if
            inner_if = AIFPLList((
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
    ) -> Tuple[AIFPLValue, List[Tuple[str, AIFPLValue]]]:
        """Desugar a cons pattern like (head . tail) or (a b . rest).

        Args:
            pattern: List pattern AST
            temp_var: Name of temp variable holding the match value
            dot_position: Index of the dot in the pattern

        Returns:
            (test_expression, bindings)
        """
        # Validate dot position
        if dot_position == 0:
            raise AIFPLEvalError(
                message="Invalid cons pattern",
                received=f"Pattern: {pattern} - dot at beginning"
            )

        if dot_position == len(pattern.elements) - 1:
            raise AIFPLEvalError(
                message="Invalid cons pattern",
                received=f"Pattern: {pattern} - dot at end"
            )

        if dot_position != len(pattern.elements) - 2:
            raise AIFPLEvalError(
                message="Invalid cons pattern",
                received=f"Pattern: {pattern} - multiple elements after dot"
            )

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
                AIFPLNumber(dot_position)
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
        head_elements = []

        for i in range(dot_position):
            elem_pattern = pattern.elements[i]
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = AIFPLList((
                AIFPLSymbol('list-ref'),
                AIFPLSymbol(temp_var),
                AIFPLNumber(i)
            ))

            head_elements.append((elem_pattern, elem_temp, elem_value))

        # Build binding for tail
        tail_pattern = pattern.elements[dot_position + 1]
        tail_temp = self._gen_temp()

        # Extract tail: (drop dot_position temp_var)
        tail_value = AIFPLList((
            AIFPLSymbol('drop'),
            AIFPLNumber(dot_position),
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
