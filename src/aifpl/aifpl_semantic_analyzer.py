"""AIFPL Semantic Analyzer - validates AST structure and semantics.

This module performs semantic validation after parsing but before desugaring.
It checks:
- Special form arity (correct number of arguments)
- Binding structure validity (let, letrec)
- Lambda parameter validity
- Pattern validity (for match expressions)
- Duplicate binding detection

All validation happens in a single pass to provide clear, precise error messages
before any transformations occur.
"""

from typing import List, cast

from aifpl.aifpl_value import AIFPLValue, AIFPLSymbol, AIFPLList, AIFPLString
from aifpl.aifpl_error import AIFPLEvalError


class AIFPLSemanticAnalyzer:
    """
    Validates AIFPL AST structure and semantics.

    This analyzer runs after parsing to check that all special forms
    are well-formed before any transformations (desugaring, compilation).
    """

    def analyze(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Analyze an expression recursively, validating all special forms.

        Args:
            expr: AST to analyze

        Returns:
            The same AST (unmodified) if validation passes

        Raises:
            AIFPLEvalError: If validation fails with detailed error message
        """
        # Lists need inspection
        if isinstance(expr, AIFPLList):
            return self._analyze_list(expr)

        # Self-evaluating values need no validation
        return expr

    def _analyze_list(self, expr: AIFPLList) -> AIFPLList:
        """Analyze a list expression (special form or function call)."""
        if expr.is_empty():
            # Empty list is valid (though semantically meaningless)
            return expr

        first = expr.first()

        # Check for special forms
        if isinstance(first, AIFPLSymbol):
            name = first.name

            if name == 'if':
                return self._analyze_if(expr)

            if name == 'let':
                return self._analyze_let(expr)

            if name == 'let*':
                return self._analyze_let_star(expr)

            if name == 'letrec':
                return self._analyze_letrec(expr)

            if name == 'lambda':
                return self._analyze_lambda(expr)

            if name == 'quote':
                return self._analyze_quote(expr)

            if name == 'match':
                return self._analyze_match(expr)

            if name == 'and':
                return self._analyze_and(expr)

            if name == 'or':
                return self._analyze_or(expr)

            if name == 'import':
                return self._analyze_import(expr)

        # Regular function call - recursively analyze all subexpressions
        return self._analyze_call(expr)

    def _analyze_if(self, expr: AIFPLList) -> AIFPLList:
        """Validate if expression: (if condition then else)"""
        if len(expr.elements) != 4:
            raise AIFPLEvalError(
                message="If expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments: {expr.describe()}",
                expected="Exactly 3 arguments: (if condition then else)",
                example="(if (> x 0) \"positive\" \"negative\")",
                suggestion="If needs condition, then-branch, and else-branch"
            )

        _, condition, then_expr, else_expr = expr.elements

        # Recursively analyze subexpressions
        self.analyze(condition)
        self.analyze(then_expr)
        self.analyze(else_expr)

        return expr

    def _analyze_let(self, expr: AIFPLList) -> AIFPLList:
        """Validate let expression: (let ((var val) ...) body)"""
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Let expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (let ((bindings...)) body)",
                example="(let ((x 5) (y 10)) (+ x y))",
                suggestion="Let needs binding list and body: (let ((var1 val1) (var2 val2) ...) body)"
            )

        if len(expr.elements) > 3:
            raise AIFPLEvalError(
                message="Let expression has too many elements",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (let ((bindings...)) body)",
                example="(let ((x 5) (y 10)) (+ x y))",
                suggestion="Let takes only bindings and one body expression. "
                    "Use (let (...) (begin expr1 expr2)) for multiple expressions"
            )

        _, bindings_list, body = expr.elements

        if not isinstance(bindings_list, AIFPLList):
            raise AIFPLEvalError(
                message="Let binding list must be a list",
                received=f"Binding list: {bindings_list.type_name()}",
                expected="List of bindings: ((var1 val1) (var2 val2) ...)",
                example="(let ((x 5) (y (* x 2))) (+ x y))",
                suggestion="Wrap bindings in parentheses: ((var val) (var val) ...)"
            )

        # Validate each binding
        var_names: List[str] = []
        for i, binding in enumerate(bindings_list.elements):
            if not isinstance(binding, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} must be a list",
                    received=f"Binding {i+1}: {binding.type_name()}",
                    expected="Each binding: (variable value-expression)",
                    example='(x 5)',
                    suggestion="Wrap each binding in parentheses: (variable-name value-expression)"
                )

            if len(binding.elements) != 2:
                binding_str = f"{len(binding.elements)} elements"
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} has wrong number of elements",
                    received=f"Binding {i+1}: {binding_str}",
                    expected="Each binding: (variable value-expression)",
                    example='Correct: (x 5)\nIncorrect: (x) or (x 1 2)',
                    suggestion="Each binding needs exactly variable name and value: (var value)"
                )

            name_expr, value_expr = binding.elements

            if not isinstance(name_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Let binding {i+1} variable must be a symbol",
                    received=f"Variable: {name_expr.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (x 5)\nIncorrect: ("x" 5)',
                    suggestion="Use unquoted variable names in bindings"
                )

            var_names.append(name_expr.name)

            # Recursively analyze the value expression
            self.analyze(value_expr)

        # Check for duplicate binding names
        if len(var_names) != len(set(var_names)):
            duplicates = [name for name in var_names if var_names.count(name) > 1]
            raise AIFPLEvalError(
                message="Let binding variables must be unique",
                received=f"Duplicate variables: {duplicates}",
                expected="All variable names should be different",
                example='Correct: (let ((x 1) (y 2)) ...)\nIncorrect: (let ((x 1) (x 2)) ...)',
                suggestion="Use different names for each variable"
            )

        # Analyze body
        self.analyze(body)

        return expr

    def _analyze_let_star(self, expr: AIFPLList) -> AIFPLList:
        """Validate let* expression: (let* ((var val) ...) body)"""
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Let* expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (let* ((bindings...)) body)",
                example="(let* ((x 5) (y (* x 2))) (+ x y))",
                suggestion="Let* needs binding list and body: (let* ((var1 val1) (var2 val2) ...) body)"
            )

        if len(expr.elements) > 3:
            raise AIFPLEvalError(
                message="Let* expression has too many elements",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (let* ((bindings...)) body)",
                example="(let* ((x 5) (y (* x 2))) (+ x y))",
                suggestion="Let* takes only bindings and one body expression. "
                    "Use (let* (...) (begin expr1 expr2)) for multiple expressions"
            )

        _, bindings_list, body = expr.elements

        if not isinstance(bindings_list, AIFPLList):
            raise AIFPLEvalError(
                message="Let* binding list must be a list",
                received=f"Binding list: {bindings_list.type_name()}",
                expected="List of bindings: ((var1 val1) (var2 val2) ...)",
                example="(let* ((x 5) (y (* x 2))) (+ x y))",
                suggestion="Wrap bindings in parentheses: ((var val) (var val) ...)"
            )

        # Validate each binding
        var_names: List[str] = []
        for i, binding in enumerate(bindings_list.elements):
            if not isinstance(binding, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Let* binding {i+1} must be a list",
                    received=f"Binding {i+1}: {binding.type_name()}",
                    expected="Each binding: (variable value-expression)",
                    example='(x 5)',
                    suggestion="Wrap each binding in parentheses: (variable-name value-expression)"
                )

            if len(binding.elements) != 2:
                binding_str = f"{len(binding.elements)} elements"
                raise AIFPLEvalError(
                    message=f"Let* binding {i+1} has wrong number of elements",
                    received=f"Binding {i+1}: {binding_str}",
                    expected="Each binding: (variable value-expression)",
                    example='Correct: (x 5)\nIncorrect: (x) or (x 1 2)',
                    suggestion="Each binding needs exactly variable name and value: (var value)"
                )

            name_expr, value_expr = binding.elements

            if not isinstance(name_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Let* binding {i+1} variable must be a symbol",
                    received=f"Variable: {name_expr.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (x 5)\nIncorrect: ("x" 5)',
                    suggestion="Use unquoted variable names in bindings"
                )

            var_names.append(name_expr.name)

            # Recursively analyze the value expression
            self.analyze(value_expr)

        # Note: Unlike 'let', we allow duplicate binding names (shadowing) in let*.
        # This is because let* has sequential semantics where later bindings
        # can shadow earlier ones, and we also don't check if later bindings reference earlier ones.
        # That's the whole point of let* - sequential bindings are allowed and expected.

        # Analyze body
        self.analyze(body)

        return expr

    def _analyze_letrec(self, expr: AIFPLList) -> AIFPLList:
        """Validate letrec expression: (letrec ((var val) ...) body)"""
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Letrec expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (letrec ((bindings...)) body)",
                example="(letrec ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 5))",
                suggestion="Letrec needs binding list and body: (letrec ((var1 val1) (var2 val2) ...) body)"
            )

        if len(expr.elements) > 3:
            raise AIFPLEvalError(
                message="Letrec expression has too many elements",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (letrec ((bindings...)) body)",
                example="(letrec ((fact (lambda (n) ...))) (fact 5))",
                suggestion="Letrec takes only bindings and one body expression"
            )

        _, bindings_list, body = expr.elements

        if not isinstance(bindings_list, AIFPLList):
            raise AIFPLEvalError(
                message="Letrec binding list must be a list",
                received=f"Binding list: {bindings_list.type_name()}",
                expected="List of bindings: ((var1 val1) (var2 val2) ...)",
                example="(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))",
                suggestion="Wrap bindings in parentheses: ((var val) (var val) ...)"
            )

        # Validate each binding
        var_names: List[str] = []
        for i, binding in enumerate(bindings_list.elements):
            if not isinstance(binding, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Letrec binding {i+1} must be a list",
                    received=f"Binding {i+1}: {binding.type_name()}",
                    expected="List with variable and value: (var val)",
                    example='Correct: (x 5)\nIncorrect: x or "x"',
                    suggestion="Wrap each binding in parentheses: (variable value)"
                )

            if len(binding.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Letrec binding {i+1} has wrong number of elements",
                    received=f"Binding {i+1}: has {len(binding.elements)} elements",
                    expected="Each binding needs exactly 2 elements: (variable value)",
                    example='Correct: (x 5)\nIncorrect: (x) or (x 5 6)',
                    suggestion="Each binding: (variable-name value-expression)"
                )

            name_expr, value_expr = binding.elements

            if not isinstance(name_expr, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Letrec binding {i+1} variable must be a symbol",
                    received=f"Variable: {name_expr.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (x 5)\nIncorrect: ("x" 5) or (1 5)',
                    suggestion='Use unquoted variable names: x, not "x"'
                )

            var_names.append(name_expr.name)

            # Recursively analyze the value expression
            self.analyze(value_expr)

        # Check for duplicate binding names
        if len(var_names) != len(set(var_names)):
            duplicates = [name for name in var_names if var_names.count(name) > 1]
            raise AIFPLEvalError(
                message="Letrec binding variables must be unique",
                received=f"Duplicate variables: {duplicates}",
                expected="All variable names should be different",
                example='Correct: (letrec ((x 1) (y 2)) ...)\nIncorrect: (letrec ((x 1) (x 2)) ...)',
                suggestion="Use different names for each variable"
            )

        # Analyze body
        self.analyze(body)

        return expr

    def _analyze_lambda(self, expr: AIFPLList) -> AIFPLList:
        """Validate lambda expression: (lambda (params...) body)"""
        if len(expr.elements) != 3:
            raise AIFPLEvalError(
                message="Lambda expression structure is incorrect",
                received=f"Got {len(expr.elements)} elements",
                expected="Exactly 3 elements: (lambda (params...) body)",
                example="(lambda (x y) (+ x y))",
                suggestion="Lambda needs parameter list and body: (lambda (param1 param2 ...) body-expression)"
            )

        _, params_list, body = expr.elements

        if not isinstance(params_list, AIFPLList):
            raise AIFPLEvalError(
                message="Lambda parameters must be a list",
                received=f"Parameter list: {params_list.type_name()}",
                expected="List of symbols: (param1 param2 ...)",
                example="(lambda (x y z) (+ x y z))",
                suggestion="Parameters should be unquoted variable names"
            )

        # Validate each parameter
        param_names: List[str] = []
        for i, param in enumerate(params_list.elements):
            if not isinstance(param, AIFPLSymbol):
                raise AIFPLEvalError(
                    message=f"Lambda parameter {i+1} must be a symbol",
                    received=f"Parameter {i+1}: {param.type_name()}",
                    expected="Unquoted symbol (variable name)",
                    example='Correct: (lambda (x y) (+ x y))\nIncorrect: (lambda ("x" 1) ...)',
                    suggestion='Use unquoted names: x, not "x" or 1'
                )
            param_names.append(param.name)

        # Check for duplicate parameters
        if len(param_names) != len(set(param_names)):
            duplicates = [p for p in param_names if param_names.count(p) > 1]
            raise AIFPLEvalError(
                message="Lambda parameters must be unique",
                received=f"Duplicate parameters: {duplicates}",
                expected="All parameter names should be different",
                example='Correct: (lambda (x y z) ...)\nIncorrect: (lambda (x y x) ...)',
                suggestion="Use different names for each parameter"
            )

        # Analyze body
        self.analyze(body)

        return expr

    def _analyze_quote(self, expr: AIFPLList) -> AIFPLList:
        """Validate quote expression: (quote expr)"""
        if len(expr.elements) != 2:
            raise AIFPLEvalError(
                message="Quote expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments: {expr.describe()}",
                expected="Exactly 1 argument",
                example="(quote expr) or 'expr",
                suggestion="Quote requires exactly one expression to quote"
            )

        # Note: We don't recursively analyze the quoted expression
        # because it's data, not code to be evaluated
        return expr

    def _analyze_match(self, expr: AIFPLList) -> AIFPLList:
        """Validate match expression: (match value (pattern result) ...)"""
        if len(expr.elements) < 3:
            raise AIFPLEvalError(
                message="Match expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments",
                expected="At least 2 arguments: (match value (pattern1 result1) ...)",
                example="(match x ((number? n) (* n 2)) (_ \"not a number\"))",
                suggestion="Match needs a value and at least one pattern clause"
            )

        value_expr = expr.elements[1]
        clauses = list(expr.elements[2:])

        # Analyze the value expression
        self.analyze(value_expr)

        # Validate all clauses
        for i, clause in enumerate(clauses):
            if not isinstance(clause, AIFPLList):
                raise AIFPLEvalError(
                    message=f"Match clause {i+1} must be a list",
                    received=f"Clause {i+1}: {clause.type_name()}",
                    expected="Each clause: (pattern result-expression)",
                    example="((number? n) (* n 2))",
                    suggestion="Wrap each clause in parentheses: (pattern result)"
                )

            if len(clause.elements) != 2:
                raise AIFPLEvalError(
                    message=f"Match clause {i+1} has wrong number of elements",
                    received=f"Clause {i+1}: {clause}",
                    expected="Each clause needs exactly 2 elements: (pattern result)",
                    example="Correct: ((number? n) (* n 2))\nIncorrect: ((number? n)) or ((number? n) result1 result2)",
                    suggestion="Each clause: (pattern result-expression)"
                )

            pattern, result_expr = clause.elements

            # Validate the pattern
            self._analyze_match_pattern(pattern, i + 1)

            # Analyze the result expression
            self.analyze(result_expr)

        return expr

    def _analyze_match_pattern(self, pattern: AIFPLValue, clause_num: int) -> None:
        """
        Validate a match pattern.

        Args:
            pattern: Pattern to validate
            clause_num: Clause number (for error messages)
        """
        if not isinstance(pattern, AIFPLList):
            return

        # Empty list pattern is valid
        if pattern.is_empty():
            return

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
                        message=f"Pattern variable must be a symbol in clause {clause_num}",
                        received=f"Variable in type pattern: {var_pattern}",
                        expected="Symbol (variable name)",
                        example="(number? x) not (number? 42)",
                        suggestion="Use unquoted variable names in type patterns"
                    )
                return

            # Unknown type predicate but looks like one
            raise AIFPLEvalError(
                message=f"Invalid type pattern in clause {clause_num}",
                received=f"Type pattern: ({type_pred} {var_pattern})",
                expected="Valid type predicate like number?, string?, list?, etc.",
                example="(number? x) or (string? s)",
                suggestion="Use a valid type predicate ending with ?"
            )

        # Check for malformed type patterns (wrong arity)
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
                        message=f"Invalid type pattern in clause {clause_num}",
                        received=f"Type pattern: ({type_pred}) - missing variable",
                        expected="Type pattern with variable: (type? var)",
                        example="(number? x) not (number?)",
                        suggestion="Add a variable name after the type predicate"
                    )

                raise AIFPLEvalError(
                    message=f"Invalid type pattern in clause {clause_num}",
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
                message=f"Invalid pattern in clause {clause_num}",
                received=f"Pattern: {pattern} - multiple dots",
                expected="At most one dot in cons pattern",
                example="(head . tail) or (a b . rest)",
                suggestion="Use only one dot to separate head from tail"
            )

        # If we have a dot, validate cons pattern structure
        if dot_positions:
            dot_position = dot_positions[0]

            if dot_position == 0:
                raise AIFPLEvalError(
                    message=f"Invalid pattern in clause {clause_num}",
                    received=f"Pattern: {pattern} - dot at beginning",
                    expected="Dot must come after at least one element",
                    example="(head . tail) not (. tail)",
                    suggestion="Put at least one pattern before the dot"
                )

            if dot_position == len(pattern.elements) - 1:
                raise AIFPLEvalError(
                    message=f"Invalid pattern in clause {clause_num}",
                    received=f"Pattern: {pattern} - dot at end",
                    expected="Dot must be followed by tail pattern",
                    example="(head . tail) not (head .)",
                    suggestion="Add a tail pattern after the dot"
                )

            if dot_position != len(pattern.elements) - 2:
                raise AIFPLEvalError(
                    message=f"Invalid pattern in clause {clause_num}",
                    received=f"Pattern: {pattern} - multiple elements after dot",
                    expected="Exactly one tail pattern after dot",
                    example="(a b . rest) not (a . b c)",
                    suggestion="Use only one pattern after the dot for the tail"
                )

            # Recursively validate head patterns
            for i in range(dot_position):
                self._analyze_match_pattern(pattern.elements[i], clause_num)

            # Validate tail pattern
            tail_pattern = cast(AIFPLValue, pattern.elements[dot_position + 1])
            self._analyze_match_pattern(tail_pattern, clause_num)

            return

        # Fixed-length list pattern: (p1 p2 p3)
        # Recursively validate each element pattern
        for elem_pattern in pattern.elements:
            self._analyze_match_pattern(elem_pattern, clause_num)

    def _analyze_and(self, expr: AIFPLList) -> AIFPLList:
        """Validate and expression: (and arg1 arg2 ...)"""
        # 'and' can have any number of arguments (including zero)
        # Just recursively analyze all arguments
        for arg in expr.elements[1:]:
            self.analyze(arg)

        return expr

    def _analyze_or(self, expr: AIFPLList) -> AIFPLList:
        """Validate or expression: (or arg1 arg2 ...)"""
        # 'or' can have any number of arguments (including zero)
        # Just recursively analyze all arguments
        for arg in expr.elements[1:]:
            self.analyze(arg)

        return expr

    def _analyze_import(self, expr: AIFPLList) -> AIFPLList:
        """Validate import expression: (import "module-name")"""
        if len(expr.elements) != 2:
            raise AIFPLEvalError(
                message="Import expression has wrong number of arguments",
                received=f"Got {len(expr.elements) - 1} arguments: {expr.describe()}",
                expected="Exactly 1 argument: (import \"module-name\")",
                example='(import "calendar") or (import "lib/validation")',
                suggestion="Import needs exactly one module name as a string"
            )

        _, module_name_expr = expr.elements

        if not isinstance(module_name_expr, AIFPLString):
            raise AIFPLEvalError(
                message="Import module name must be a string literal",
                received=f"Module name: {module_name_expr.type_name()}",
                expected="String literal with module name",
                example='(import "calendar") not (import calendar)',
                suggestion="Module names must be string literals in double quotes"
            )

        # Validate module name is not empty
        if not module_name_expr.value:
            raise AIFPLEvalError(
                message="Import module name cannot be empty",
                example='(import "calendar")',
                suggestion="Provide a valid module name"
            )

        return expr

    def _analyze_call(self, expr: AIFPLList) -> AIFPLList:
        """Validate function call by recursively analyzing all subexpressions."""
        # Recursively analyze all elements (function and arguments)
        for elem in expr.elements:
            self.analyze(elem)

        return expr
