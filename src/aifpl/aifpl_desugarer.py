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

from aifpl.aifpl_ast import (
    AIFPLASTNode, AIFPLASTSymbol, AIFPLASTList, AIFPLASTInteger,
    AIFPLASTFloat, AIFPLASTComplex, AIFPLASTString, AIFPLASTBoolean
)
from aifpl.aifpl_error import AIFPLEvalError


class AIFPLDesugarer:
    """Transforms complex AIFPL constructs into core language."""

    def __init__(self) -> None:
        self.temp_counter = 0  # For generating unique temp variable names

    def _make_symbol(self, name: str, source_node: AIFPLASTNode) -> AIFPLASTSymbol:
        """Create a symbol with source location from another node."""
        return AIFPLASTSymbol(
            name,
            line=source_node.line,
            column=source_node.column,
            source_file=source_node.source_file
        )

    def _make_list(self, elements: tuple, source_node: AIFPLASTNode) -> AIFPLASTList:
        """Create a list with source location from another node."""
        return AIFPLASTList(
            elements,
            line=source_node.line,
            column=source_node.column,
            source_file=source_node.source_file
        )

    def desugar(self, expr: AIFPLASTNode) -> AIFPLASTNode:
        """
        Desugar an expression recursively.

        Args:
            expr: AST to desugar

        Returns:
            Desugared AST (core language only)
        """
        # Lists need inspection - anything else does not
        if not isinstance(expr, AIFPLASTList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()
        if isinstance(first, AIFPLASTSymbol):
            name = first.name

            # Match expression - desugar it!
            if name == 'match':
                return self._desugar_match(expr)

            # Core constructs - desugar children only
            if name == 'if':
                return self._desugar_if(expr)

            if name == 'let':
                return self._desugar_let(expr)

            if name == 'let*':
                # Let* desugars to nested lets
                return self._desugar_let_star(expr)

            if name == 'lambda':
                return self._desugar_lambda(expr)

            if name == 'quote':
                # Quote: don't desugar the quoted expression
                return expr

            if name == 'trace':
                # Trace is a special form - handle it
                return self._desugar_trace(expr)

            # Check for typed variadic arithmetic operations
            if name in [
                'integer+', 'integer-', 'integer*', 'integer/',
                'float+', 'float-', 'float*', 'float/',
                'complex+', 'complex-', 'complex*', 'complex/',
            ]:
                return self._desugar_variadic_arithmetic(expr)

            # Fold-reducible variadic operations
            if name in ['bit-or', 'bit-and', 'bit-xor', 'append', 'string-append', 'min', 'max']:
                return self._desugar_fold_variadic(expr)

            # Variadic comparison chains (short-circuit with 'and' is correct)
            if name in ['<', '>', '<=', '>=']:
                return self._desugar_comparison_chain(expr)

            # Strict equality predicates
            if name in [
                'boolean=?', 'integer=?', 'float=?', 'complex=?', 'string=?', 'list=?', 'alist=?'
            ]:
                return self._desugar_strict_equality(expr)

            # Strict inequality predicates
            if name in [
                'boolean!=?', 'integer!=?', 'float!=?', 'complex!=?', 'string!=?', 'list!=?', 'alist!=?'
            ]:
                return self._desugar_strict_inequality(expr)

        # Regular function call - desugar all elements
        return self._desugar_call(expr)

    def _desugar_if(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Desugar if expression by desugaring its subexpressions."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 4, "If expression should have exactly 4 elements (validated by semantic analyzer)"

        _, condition, then_expr, else_expr = expr.elements

        # Desugar all subexpressions
        desugared_condition = self.desugar(condition)
        desugared_then = self.desugar(then_expr)
        desugared_else = self.desugar(else_expr)

        return self._make_list((
            self._make_symbol('if', expr),
            desugared_condition, desugared_then, desugared_else
        ), expr)

    def _desugar_let(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Desugar let expression by desugaring its subexpressions."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements (validated by semantic analyzer)"

        let_symbol = expr.elements[0]
        bindings_list = expr.elements[1]
        body = expr.elements[2]

        assert isinstance(bindings_list, AIFPLASTList), "Binding list should be a list (validated by semantic analyzer)"

        # Desugar each binding value
        desugared_bindings = []
        for i, binding in enumerate(bindings_list.elements):
            assert isinstance(binding, AIFPLASTList) and len(binding.elements) == 2, \
                f"Binding {i+1} should be a list with 2 elements (validated by semantic analyzer)"

            var_name, value_expr = binding.elements
            desugared_value = self.desugar(value_expr)
            desugared_bindings.append(self._make_list((var_name, desugared_value), binding))

        # Desugar body
        desugared_body = self.desugar(body)

        return self._make_list((
            let_symbol, self._make_list(tuple(desugared_bindings), bindings_list), desugared_body
        ), expr)

    def _desugar_let_star(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Desugar let* expression to nested let expressions.

        (let* ((x 1) (y (+ x 1)) (z (* y 2))) body)
        =>
        (let ((x 1))
          (let ((y (+ x 1)))
            (let ((z (* y 2)))
              body)))
        """
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Let* expression should have exactly 3 elements (validated by semantic analyzer)"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, AIFPLASTList), "Binding list should be a list (validated by semantic analyzer)"

        # If no bindings, just return the body
        if len(bindings_list.elements) == 0:
            return self.desugar(body)

        # Build nested lets from the inside out
        # Start with the body
        result = self.desugar(body)

        # Wrap in nested lets, processing bindings in reverse order
        for binding in reversed(bindings_list.elements):
            assert isinstance(binding, AIFPLASTList) and len(binding.elements) == 2, \
                "Binding should be a list with 2 elements (validated by semantic analyzer)"

            var_name, value_expr = binding.elements

            # Desugar the value expression
            desugared_value = self.desugar(value_expr)

            # Wrap result in a let with this binding
            result = self._make_list((
                self._make_symbol('let', expr),
                self._make_list((self._make_list((var_name, desugared_value), binding),), binding),
                result
            ), expr)

        return result

    def _desugar_lambda(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Desugar lambda expression by desugaring its body."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Lambda expression should have exactly 3 elements (validated by semantic analyzer)"

        lambda_symbol, params_list, body = expr.elements

        # Desugar body
        desugared_body = self.desugar(body)

        return self._make_list((lambda_symbol, params_list, desugared_body), expr)

    def _desugar_trace(self, expr: AIFPLASTList) -> AIFPLASTNode:
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
        desugared_elements: List[AIFPLASTNode] = [self._make_symbol('trace', expr)]
        for elem in expr.elements[1:]:  # Skip 'trace' symbol
            desugared_elements.append(self.desugar(elem))

        return self._make_list(tuple(desugared_elements), expr)

    def _desugar_call(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Desugar function call by desugaring all elements."""
        desugared_elements = []
        for elem in expr.elements:
            desugared_elements.append(self.desugar(elem))

        return self._make_list(tuple(desugared_elements), expr)

    def _desugar_variadic_arithmetic(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Desugar variadic arithmetic operations to nested binary operations.

        Examples:
            (+ 1 2 3) → (+ (+ 1 2) 3)
            (- 5) → (- 0 5)  [unary negation]
            (* 2 3 4) → (* (* 2 3) 4)
            (+ 1) → 1  [identity]
            (+) → 0  [zero-arg identity]

        Args:
            expr: Arithmetic expression to desugar

        Returns:
            Desugared expression (binary operations only)
        """
        op_symbol = expr.first()
        assert isinstance(op_symbol, AIFPLASTSymbol)
        op_name = op_symbol.name

        args = list(expr.elements[1:])

        # Determine identity elements and sub operator for this family.
        if op_name in ('integer+', 'integer-', 'integer*', 'integer/'):
            zero: AIFPLASTNode = AIFPLASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)
            one: AIFPLASTNode = AIFPLASTInteger(1, line=expr.line, column=expr.column, source_file=expr.source_file)
            sub_op = 'integer-'
        elif op_name in ('float+', 'float-', 'float*', 'float/'):
            zero = AIFPLASTFloat(0.0, line=expr.line, column=expr.column, source_file=expr.source_file)
            one = AIFPLASTFloat(1.0, line=expr.line, column=expr.column, source_file=expr.source_file)
            sub_op = 'float-'
        elif op_name in ('complex+', 'complex-', 'complex*', 'complex/'):
            zero = AIFPLASTComplex(0+0j, line=expr.line, column=expr.column, source_file=expr.source_file)
            one = AIFPLASTComplex(1+0j, line=expr.line, column=expr.column, source_file=expr.source_file)
            sub_op = 'complex-'
        else:
            assert False, f"Unexpected operator in _desugar_variadic_arithmetic: {op_name!r}"

        is_add = op_name in ('integer+', 'float+', 'complex+')
        is_mul = op_name in ('integer*', 'float*', 'complex*')
        is_sub = op_name in ('integer-', 'float-', 'complex-')
        # integer/ is binary-only: (integer/ x) has no sensible reciprocal
        # (floor(1/x) == 0 for all x > 1).  float/, complex/ support the
        # Lisp reciprocal convention: (op x) → (op identity x).
        is_div_error = op_name == 'integer/'
        is_div_reciprocal = op_name in ('float/', 'complex/')

        # Handle zero-argument cases
        if len(args) == 0:
            if is_add:
                # (+) / (integer+) / (float+) / (complex+) → identity element
                return zero

            if is_mul:
                # (*) / (integer*) / (float*) / (complex*) → identity element
                return one

            # subtraction/division with no args are errors — fall through to runtime
            return self._desugar_call(expr)

        # Handle single-argument cases
        if len(args) == 1:
            desugared_arg = self.desugar(args[0])

            if is_sub:
                # (- x) → (- 0 x), (integer- x) → (integer- 0 x), etc.
                return self._make_list((self._make_symbol(sub_op, expr), zero, desugared_arg), expr)

            if is_div_reciprocal:
                # (/ x) → (/ 1 x), (float/ x) → (float/ 1.0 x), (complex/ x) → (complex/ 1+0j x)
                return self._make_list((self._make_symbol(op_name, expr), one, desugared_arg), expr)

            if is_div_error:
                # (integer/ x) is an error — floor(1/x) == 0 for x > 1, almost certainly not intended
                return self._desugar_call(expr)

            # (+ x), (* x), (integer+ x), (float* x), (complex+ x), etc. → x [identity]
            return desugared_arg

        # Handle binary case (already optimal)
        if len(args) == 2:
            desugared_args = [self.desugar(arg) for arg in args]
            return self._make_list((op_symbol,) + tuple(desugared_args), expr)

        # Handle variadic case (3+ arguments) - fold left to right
        # (+ 1 2 3 4) → (+ (+ (+ 1 2) 3) 4)
        desugared_args = [self.desugar(arg) for arg in args]

        # Start with first two arguments
        result = self._make_list((
            self._make_symbol(op_name, expr),
            desugared_args[0],
            desugared_args[1]
        ), expr)

        # Fold remaining arguments left-to-right
        for arg in desugared_args[2:]:
            result = self._make_list((
                self._make_symbol(op_name, expr),
                result,
                arg
            ), expr)

        return result

    def _desugar_fold_variadic(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Desugar fold-reducible variadic operations to nested binary operations.

        These are operations where (f a b c) means (f (f a b) c) — a left fold.
        Each operation has a natural identity element for the 0-arg case.

        Handles:
            bit-or, bit-and, bit-xor  — bitwise ops, identity: 0
            append                    — list concatenation, identity: ()
            string-append             — string concatenation, identity: ""
            min, max                  — numeric reduction (1+ args required)
        """
        op_symbol = expr.first()
        assert isinstance(op_symbol, AIFPLASTSymbol)
        op_name = op_symbol.name

        args = list(expr.elements[1:])

        # 0-arg identity cases
        if len(args) == 0:
            if op_name == 'bit-or':
                return AIFPLASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)

            if op_name == 'bit-and':
                return AIFPLASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)

            if op_name == 'bit-xor':
                return AIFPLASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)

            if op_name == 'append':
                return self._make_list((self._make_symbol('quote', expr), self._make_list((), expr)), expr)

            if op_name == 'string-append':
                return AIFPLASTString("", line=expr.line, column=expr.column, source_file=expr.source_file)

            # min/max with 0 args: let runtime raise the error
            return self._desugar_call(expr)

        # 1-arg: identity — return the single argument as-is
        if len(args) == 1:
            return self.desugar(args[0])

        # 2-arg: already binary, emit directly
        if len(args) == 2:
            return self._make_list(
                (op_symbol, self.desugar(args[0]), self.desugar(args[1])), expr
            )

        # 3+ args: left-fold
        desugared_args = [self.desugar(arg) for arg in args]
        result = self._make_list(
            (self._make_symbol(op_name, expr), desugared_args[0], desugared_args[1]), expr
        )
        for arg in desugared_args[2:]:
            result = self._make_list(
                (self._make_symbol(op_name, expr), result, arg), expr
            )

        return result

    def _desugar_comparison_chain(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Desugar variadic comparison/equality operations to pairwise binary comparisons.

        (op a b c) means (and (op a b) (op b c)), not a fold.
        Each argument is evaluated once; the desugared form uses let bindings to
        avoid double-evaluation when there are 3+ arguments.

        The 2-arg case emits the binary opcode directly. The 3+-arg case wraps
        intermediate values in let bindings and chains with 'and'.
        For != the pairwise connector is 'or' rather than 'and'.
        """
        op_symbol = expr.first()
        assert isinstance(op_symbol, AIFPLASTSymbol)
        op_name = op_symbol.name

        args = list(expr.elements[1:])

        # 2-arg: emit binary opcode directly (most common case)
        if len(args) == 2:
            return self._make_list(
                (op_symbol, self.desugar(args[0]), self.desugar(args[1])), expr
            )

        # 3+ args: (op a b c d) → (and (op a b) (op b c) (op c d))
        # For != the connector is 'or' instead of 'and': any consecutive pair
        # differing is sufficient to make the whole expression true.
        # Bind each arg to a temp to avoid double-evaluation, then chain pairwise.
        desugared_args = [self.desugar(arg) for arg in args]
        temps = [self._gen_temp() for _ in args]

        # Build pairwise comparisons
        pairs = [
            self._make_list((self._make_symbol(op_name, expr),
                             self._make_symbol(temps[i], expr),
                             self._make_symbol(temps[i + 1], expr)), expr)
            for i in range(len(temps) - 1)
        ]

        # Chain with 'and' for ordered comparisons
        body: AIFPLASTNode = self._make_list(
            tuple([self._make_symbol('and', expr)] + pairs), expr
        )

        # Wrap in let* bindings from innermost outward
        for temp, desugared_arg in reversed(list(zip(temps, desugared_args))):
            body = self._make_list((
                self._make_symbol('let*', expr),
                self._make_list((
                    self._make_list((self._make_symbol(temp, expr), desugared_arg), expr),
                ), expr),
                body
            ), expr)

        return self.desugar(body)

    def _desugar_strict_equality_chain(self, expr: AIFPLASTList, check_eq: bool) -> AIFPLASTNode:
        """
        Desugar strict type-specific equality predicates.

        The 2-arg case is desugared to a binary opcode directly.

        The 3+-arg case is desugared to a let*-bound sequence of pairwise binary
        opcode calls, combined with 'and'. Crucially all pair evaluations are
        bound in let* before the 'and' is evaluated, so no short-circuiting
        occurs — every consecutive pair is always checked, and a type mismatch
        on any argument raises an error regardless of position.

        For example, (integer=? a b c) becomes:
            (let* ((t0 (integer=? a b))
                   (t1 (integer=? b c)))
              (and t0 t1))

        The 0-arg and 1-arg cases fall through to a regular call, which
        resolves to the prelude lambda that raises the arity error.
        """
        op_symbol = expr.first()
        assert isinstance(op_symbol, AIFPLASTSymbol)
        op_name = op_symbol.name

        args = list(expr.elements[1:])

        if len(args) == 2:
            return self._make_list(
                (op_symbol, self.desugar(args[0]), self.desugar(args[1])), expr
            )

        if len(args) >= 3:
            # Desugar all args first
            desugared_args = [self.desugar(arg) for arg in args]

            # Generate temps for each arg to avoid double-evaluation
            temps = [self._gen_temp() for _ in args]

            # Generate temps for each pairwise result
            pair_temps = [self._gen_temp() for _ in range(len(args) - 1)]

            # Build pairwise binary calls: (op ti ti+1)
            pairs = [
                self._make_list((self._make_symbol(op_name, expr),
                                 self._make_symbol(temps[i], expr),
                                 self._make_symbol(temps[i + 1], expr)), expr)
                for i in range(len(args) - 1)
            ]

            # Body: (and pair_temp0 pair_temp1 ...)
            connector = 'and' if check_eq else 'or'
            body: AIFPLASTNode = self._make_list(
                tuple([self._make_symbol(connector, expr)] + [self._make_symbol(pt, expr) for pt in pair_temps]),
                expr
            )

            # Wrap pair results in let* bindings (innermost first)
            for pt, pair in reversed(list(zip(pair_temps, pairs))):
                body = self._make_list((
                    self._make_symbol('let*', expr),
                    self._make_list((
                        self._make_list((self._make_symbol(pt, expr), pair), expr),
                    ), expr),
                    body
                ), expr)

            # Wrap arg values in let* bindings (innermost first)
            for temp, desugared_arg in reversed(list(zip(temps, desugared_args))):
                body = self._make_list((
                    self._make_symbol('let*', expr),
                    self._make_list((
                        self._make_list((self._make_symbol(temp, expr), desugared_arg), expr),
                    ), expr),
                    body
                ), expr)

            return self.desugar(body)

        # 0-arg or 1-arg: fall through to regular call → prelude lambda raises arity error
        return self._desugar_call(expr)

    def _desugar_strict_equality(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Desugar strict type-specific equality predicates."""
        return self._desugar_strict_equality_chain(expr, check_eq=True)

    def _desugar_strict_inequality(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Desugar strict type-specific inequality predicates."""
        return self._desugar_strict_equality_chain(expr, check_eq=False)

    def _desugar_match(self, expr: AIFPLASTList) -> AIFPLASTNode:
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
            assert isinstance(clause, AIFPLASTList) and len(clause.elements) == 2, \
                f"Clause {i+1} should be a list with 2 elements (validated by semantic analyzer)"

        # Generate temp variable for match value
        temp_var = self._gen_temp()

        # Desugar the value expression
        desugared_value = self.desugar(value_expr)

        # Build the match logic as nested if/let expressions
        match_logic = self._build_match_clauses(temp_var, clauses)

        # Wrap in let to bind the temp variable
        # (let ((temp value)) match-logic)
        result = AIFPLASTList((
            AIFPLASTSymbol('let*'),
            AIFPLASTList((
                AIFPLASTList((AIFPLASTSymbol(temp_var), desugared_value)),
            )),
            match_logic
        ))

        # Recursively desugar the result to handle let* -> nested let transformation
        return self.desugar(result)

    def _build_match_clauses(self, temp_var: str, clauses: List[AIFPLASTNode]) -> AIFPLASTNode:
        """
        Build nested if/let structure for match clauses.

        Args:
            temp_var: Name of temp variable holding the match value
            clauses: Non-empty list of (pattern, result) clauses (validated by semantic analyzer)

        Returns:
            Nested if/let AST
        """
        # Process clauses in reverse order to build nested structure
        result: AIFPLASTNode | None = None

        for i in range(len(clauses) - 1, -1, -1):
            clause = clauses[i]
            assert isinstance(clause, AIFPLASTList), "Clause must be a list (validated by semantic analyzer)"
            pattern = clause.elements[0]
            result_expr = clause.elements[1]

            # Desugar the result expression
            desugared_result = self.desugar(result_expr)

            # Desugar the pattern into (test_expr, bindings)
            # All patterns are validated by semantic analyzer, so this cannot raise
            test_expr, bindings = self._desugar_pattern(pattern, temp_var)

            # If this is the last clause, the else branch is the no-match error
            if i == len(clauses) - 1:
                no_match_error = AIFPLASTList((
                    AIFPLASTSymbol('error'),
                    AIFPLASTString("No patterns matched in match expression")
                ))
                result = self._build_clause_with_bindings(
                    test_expr,
                    bindings,
                    desugared_result,
                    no_match_error
                )
                continue

            result = self._build_clause_with_bindings(
                test_expr,
                bindings,
                desugared_result,
                cast(AIFPLASTNode, result)
            )

        assert result is not None
        return result

    def _build_clause_with_bindings(
        self,
        test_expr: AIFPLASTNode,
        bindings: List[Tuple[str, Any]],  # Pattern variable bindings or special markers
        result_expr: AIFPLASTNode,
        else_expr: AIFPLASTNode
    ) -> AIFPLASTNode:
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

        # All bindings here are pattern variable bindings (user-defined names).
        # They must go inside the then branch (only evaluated after test passes).
        if bindings:
            binding_list = [AIFPLASTList((AIFPLASTSymbol(vn), ve)) for vn, ve in bindings]

            then_expr: AIFPLASTNode = AIFPLASTList((
                AIFPLASTSymbol('let*'),
                AIFPLASTList(tuple(binding_list)),
                result_expr
            ))
        else:
            then_expr = result_expr

        # Build if expression
        return AIFPLASTList((
            AIFPLASTSymbol('if'),
            test_expr,
            then_expr,
            else_expr
        ))

    def _desugar_pattern(
        self,
        pattern: AIFPLASTNode,
        temp_var: str
    ) -> Tuple[AIFPLASTNode, List[Tuple[str, AIFPLASTNode]]]:
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
        if isinstance(pattern, AIFPLASTBoolean):
            # Test: (= temp_var literal)
            test_expr = AIFPLASTList((
                AIFPLASTSymbol('boolean=?'),
                AIFPLASTSymbol(temp_var),
                pattern
            ))
            return (test_expr, [])

        if isinstance(pattern, AIFPLASTInteger):
            # Test: (= temp_var literal)
            test_expr = AIFPLASTList((
                AIFPLASTSymbol('integer=?'),
                AIFPLASTSymbol(temp_var),
                pattern
            ))
            return (test_expr, [])

        if isinstance(pattern, AIFPLASTFloat):
            # Test: (= temp_var literal)
            test_expr = AIFPLASTList((
                AIFPLASTSymbol('float=?'),
                AIFPLASTSymbol(temp_var),
                pattern
            ))
            return (test_expr, [])

        if isinstance(pattern, AIFPLASTComplex):
            # Test: (= temp_var literal)
            test_expr = AIFPLASTList((
                AIFPLASTSymbol('complex=?'),
                AIFPLASTSymbol(temp_var),
                pattern
            ))
            return (test_expr, [])

        if isinstance(pattern, AIFPLASTString):
            # Test: (= temp_var literal)
            test_expr = AIFPLASTList((
                AIFPLASTSymbol('string=?'),
                AIFPLASTSymbol(temp_var),
                pattern
            ))
            return (test_expr, [])

        # Variable pattern: binds the value
        if isinstance(pattern, AIFPLASTSymbol):
            if pattern.name == '_':
                # Wildcard - always matches, no binding
                return (AIFPLASTBoolean(True), [])

            # Variable binding - always matches, binds variable
            return (
                AIFPLASTBoolean(True),
                [(pattern.name, AIFPLASTSymbol(temp_var))]
            )

        # List patterns
        if isinstance(pattern, AIFPLASTList):
            return self._desugar_list_pattern(pattern, temp_var)

        # Should never reach here - semantic analyzer validates patterns
        assert False, f"Unknown pattern type: {type(pattern).__name__} (should be validated by semantic analyzer)"

    def _desugar_list_pattern(
        self,
        pattern: AIFPLASTList,
        temp_var: str
    ) -> Tuple[AIFPLASTNode, List[Tuple[str, AIFPLASTNode]]]:
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
            test_expr = AIFPLASTList((
                AIFPLASTSymbol('null?'),
                AIFPLASTSymbol(temp_var)
            ))
            return (test_expr, [])

        # Check for type pattern: (type? var)
        if (len(pattern.elements) == 2 and
            isinstance(pattern.elements[0], AIFPLASTSymbol) and
            pattern.elements[0].name.endswith('?')):

            type_pred = pattern.elements[0].name
            var_pattern = pattern.elements[1]

            # Validate type predicate
            valid_predicates = {
                'number?', 'integer?', 'float?', 'complex?',
                'string?', 'boolean?', 'list?', 'alist?', 'function?'
            }

            assert type_pred in valid_predicates, f"Unknown type predicate: {type_pred} (should be validated by semantic analyzer)"
            assert isinstance(var_pattern, AIFPLASTSymbol), \
                "Type pattern variable should be a symbol (validated by semantic analyzer)"

            # Test: (type? temp_var)
            test_expr = AIFPLASTList((
                AIFPLASTSymbol(type_pred),
                AIFPLASTSymbol(temp_var)
            ))

            # Binding: if var_pattern is a variable, bind it
            bindings: List[Tuple[str, AIFPLASTNode]] = []
            if isinstance(var_pattern, AIFPLASTSymbol) and var_pattern.name != '_':
                bindings.append((var_pattern.name, AIFPLASTSymbol(temp_var)))

            return (test_expr, bindings)

        # Malformed type patterns should have been caught by semantic analyzer
        # Continue with other pattern types

        # Check for cons pattern: (head . tail) or (a b . rest)
        dot_positions = []
        for i, elem in enumerate(pattern.elements):
            if isinstance(elem, AIFPLASTSymbol) and elem.name == '.':
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
        pattern: AIFPLASTList,
        temp_var: str
    ) -> Tuple[AIFPLASTNode, List[Tuple[str, Any]]]:
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
        list_test = AIFPLASTList((
            AIFPLASTSymbol('list?'),
            AIFPLASTSymbol(temp_var)
        ))

        # Second test: (= (length temp_var) num_elements)
        length_test = AIFPLASTList((
            AIFPLASTSymbol('integer=?'),
            AIFPLASTList((
                AIFPLASTSymbol('length'),
                AIFPLASTSymbol(temp_var)
            )),
            AIFPLASTInteger(num_elements)
        ))

        # Combine with and
        combined_test = AIFPLASTList((
            AIFPLASTSymbol('and'),
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
        element_info: List[Tuple[AIFPLASTNode, str, AIFPLASTNode]] = []  # List of (pattern, temp_var, extraction_expr)

        for i, elem_pattern in enumerate(pattern.elements):
            # Generate temp var for this element
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = AIFPLASTList((
                AIFPLASTSymbol('list-ref'),
                AIFPLASTSymbol(temp_var),
                AIFPLASTInteger(i)
            ))

            element_info.append((elem_pattern, elem_temp, elem_value))

        # Return the basic test and a special marker with element info
        # The caller will use this to build the proper nested structure
        # Use a unique marker name to avoid duplicates in nested patterns
        marker_name = f'__LIST_PATTERN_{self._gen_temp()}__'
        return (combined_test, [(marker_name, element_info)])

    def _flatten_nested_pattern(
        self,
        pattern: AIFPLASTNode,
        temp_var: str,
        extraction_bindings: List[Tuple[str, AIFPLASTNode]],
        element_tests: List[AIFPLASTNode],
        pattern_bindings: List[Tuple[str, AIFPLASTNode]]
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
            nested_element_info = cast(List[Tuple[AIFPLASTNode, str, AIFPLASTNode]], bindings[0][1])

            # Add the length/type test
            if not (isinstance(test, AIFPLASTBoolean) and test.value):
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
            if not (isinstance(test, AIFPLASTBoolean) and test.value):
                element_tests.append(test)
            pattern_bindings.extend(bindings)

    def _build_list_pattern_clause(
        self,
        length_test: AIFPLASTNode,
        element_info: List,
        result_expr: AIFPLASTNode,
        else_expr: AIFPLASTNode
    ) -> AIFPLASTNode:
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
        extraction_bindings: List[Tuple[str, AIFPLASTNode]] = []
        element_tests: List[AIFPLASTNode] = []
        pattern_bindings: List[Tuple[str, AIFPLASTNode]] = []

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
                if not (isinstance(elem_test, AIFPLASTBoolean) and elem_test.value):
                    element_tests.append(elem_test)

                # Collect pattern bindings
                pattern_bindings.extend(elem_bindings)

        # Build the inner structure (after element extraction)
        if element_tests:
            # We have element tests - need nested if
            elem_test_combined = (
                AIFPLASTList((AIFPLASTSymbol('and'),) + tuple(element_tests)) if len(element_tests) > 1 else element_tests[0]
            )

            # Build pattern bindings let
            if pattern_bindings:
                binding_list = [AIFPLASTList((AIFPLASTSymbol(vn), ve)) for vn, ve in pattern_bindings]
                pattern_let: AIFPLASTNode = AIFPLASTList((
                    AIFPLASTSymbol('let*'),
                    AIFPLASTList(tuple(binding_list)),
                    result_expr
                ))

            else:
                pattern_let = result_expr

            # Build element test if
            inner_if: AIFPLASTNode = AIFPLASTList((
                AIFPLASTSymbol('if'),
                elem_test_combined,
                pattern_let,
                else_expr
            ))

        else:
            # No element tests - just bind pattern vars
            if pattern_bindings:
                binding_list = [AIFPLASTList((AIFPLASTSymbol(vn), ve)) for vn, ve in pattern_bindings]
                inner_if = AIFPLASTList((
                    AIFPLASTSymbol('let*'),
                    AIFPLASTList(tuple(binding_list)),
                    result_expr
                ))

            else:
                inner_if = result_expr

        # Wrap in element extraction let
        extraction_binding_list = [AIFPLASTList((AIFPLASTSymbol(vn), ve)) for vn, ve in extraction_bindings]
        extraction_let = AIFPLASTList((
            AIFPLASTSymbol('let*'),
            AIFPLASTList(tuple(extraction_binding_list)),
            inner_if
        ))

        # Build outer if with length test
        return AIFPLASTList((
            AIFPLASTSymbol('if'),
            length_test,
            extraction_let,
            else_expr
        ))

    def _desugar_cons_pattern(
        self,
        pattern: AIFPLASTList,
        temp_var: str,
        dot_position: int
    ) -> Tuple[AIFPLASTNode, List[Tuple[str, Any]]]:
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
        list_test = AIFPLASTList((
            AIFPLASTSymbol('list?'),
            AIFPLASTSymbol(temp_var)
        ))

        length_test = AIFPLASTList((
            AIFPLASTSymbol('>='),
            AIFPLASTList((
                AIFPLASTSymbol('length'),
                AIFPLASTSymbol(temp_var)
            )),
            AIFPLASTInteger(dot_position)
        ))
        combined_test = AIFPLASTList((
            AIFPLASTSymbol('and'),
            list_test,
            length_test
        ))

        # Collect head element info
        head_elements: List[Tuple[AIFPLASTNode, str, AIFPLASTNode]] = []

        for i in range(dot_position):
            elem_pattern = pattern.elements[i]
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = AIFPLASTList((
                AIFPLASTSymbol('list-ref'),
                AIFPLASTSymbol(temp_var),
                AIFPLASTInteger(i)
            ))

            head_elements.append((elem_pattern, elem_temp, elem_value))

        # Build binding for tail
        tail_pattern = pattern.elements[dot_position + 1]
        tail_temp = self._gen_temp()

        # Extract tail: (drop dot_position temp_var)
        tail_value = AIFPLASTList((
            AIFPLASTSymbol('drop'),
            AIFPLASTInteger(dot_position),
            AIFPLASTSymbol(temp_var)
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
