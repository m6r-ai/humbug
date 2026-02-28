"""Menai Desugarer - transforms complex constructs into core language.

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

from menai.menai_ast import (
    MenaiASTNode, MenaiASTSymbol, MenaiASTList, MenaiASTInteger,
    MenaiASTFloat, MenaiASTComplex, MenaiASTString, MenaiASTBoolean, MenaiASTNone
)
from menai.menai_error import MenaiEvalError


class MenaiDesugarer:
    """Transforms complex Menai constructs into core language."""

    def __init__(self) -> None:
        self.temp_counter = 0  # For generating unique temp variable names

    def _make_symbol(self, name: str, source_node: MenaiASTNode) -> MenaiASTSymbol:
        """Create a symbol with source location from another node."""
        return MenaiASTSymbol(
            name,
            line=source_node.line,
            column=source_node.column,
            source_file=source_node.source_file
        )

    def _make_list(self, elements: tuple, source_node: MenaiASTNode) -> MenaiASTList:
        """Create a list with source location from another node."""
        return MenaiASTList(
            elements,
            line=source_node.line,
            column=source_node.column,
            source_file=source_node.source_file
        )

    def _make_and(self, exprs: List[MenaiASTNode], source_node: MenaiASTNode) -> MenaiASTNode:
        """
        Construct (and exprs...) as a lowered if-chain without going through desugar().

        Used internally when building AST nodes that must already be in desugared
        form (e.g. inside _desugar_pattern, _desugar_fixed_list_pattern).

        (and)      -> #t
        (and A)    -> A
        (and A B+) -> (if A (and B+) #f)
        """
        if not exprs:
            return MenaiASTBoolean(
                True,
                line=source_node.line,
                column=source_node.column,
                source_file=source_node.source_file
            )

        if len(exprs) == 1:
            return exprs[0]

        return self._make_list((
            self._make_symbol('if', source_node),
            exprs[0],
            self._make_and(exprs[1:], source_node),
            MenaiASTBoolean(
                False,
                line=source_node.line,
                column=source_node.column,
                source_file=source_node.source_file
            ),
        ), source_node)

    def _make_or(self, exprs: List[MenaiASTNode], source_node: MenaiASTNode) -> MenaiASTNode:
        """
        Construct (or exprs...) as a lowered if-chain without going through desugar().

        Used internally when building AST nodes that must already be in desugared
        form (e.g. inside _desugar_strict_equality_chain).

        (or)      -> #f
        (or A)    -> A
        (or A B+) -> (if A #t (or B+))
        """
        if not exprs:
            return MenaiASTBoolean(
                False,
                line=source_node.line,
                column=source_node.column,
                source_file=source_node.source_file
            )

        if len(exprs) == 1:
            return exprs[0]

        return self._make_list((
            self._make_symbol('if', source_node),
            exprs[0],
            MenaiASTBoolean(
                True,
                line=source_node.line,
                column=source_node.column,
                source_file=source_node.source_file
            ),
            self._make_or(exprs[1:], source_node),
        ), source_node)

    def _desugar_and(self, expr: MenaiASTList) -> MenaiASTNode:
        """
        Desugar (and A B ...) to a right-folded if-chain.

        (and)      -> #t
        (and A)    -> (desugar A)
        (and A B+) -> (if (desugar A) (desugar (and B+)) #f)
        """
        args = list(expr.elements[1:])
        if not args:
            return MenaiASTBoolean(True, line=expr.line, column=expr.column, source_file=expr.source_file)

        if len(args) == 1:
            return self.desugar(args[0])

        desugared_first = self.desugar(args[0])
        rest_expr = self._make_list((self._make_symbol('and', expr),) + tuple(args[1:]), expr)
        desugared_rest = self._desugar_and(rest_expr)
        return self._make_list((
            self._make_symbol('if', expr),
            desugared_first,
            desugared_rest,
            MenaiASTBoolean(False, line=expr.line, column=expr.column, source_file=expr.source_file),
        ), expr)

    def _desugar_or(self, expr: MenaiASTList) -> MenaiASTNode:
        """
        Desugar (or A B ...) to a right-folded if-chain.

        (or)      -> #f
        (or A)    -> (desugar A)
        (or A B+) -> (if (desugar A) #t (desugar (or B+)))
        """
        args = list(expr.elements[1:])
        if not args:
            return MenaiASTBoolean(False, line=expr.line, column=expr.column, source_file=expr.source_file)

        if len(args) == 1:
            return self.desugar(args[0])

        desugared_first = self.desugar(args[0])
        rest_expr = self._make_list((self._make_symbol('or', expr),) + tuple(args[1:]), expr)
        desugared_rest = self._desugar_or(rest_expr)
        return self._make_list((
            self._make_symbol('if', expr),
            desugared_first,
            MenaiASTBoolean(True, line=expr.line, column=expr.column, source_file=expr.source_file),
            desugared_rest,
        ), expr)

    def desugar(self, expr: MenaiASTNode) -> MenaiASTNode:
        """
        Desugar an expression recursively.

        Args:
            expr: AST to desugar

        Returns:
            Desugared AST (core language only)
        """
        # Lists need inspection - anything else does not
        if not isinstance(expr, MenaiASTList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()
        if isinstance(first, MenaiASTSymbol):
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

            if name == 'and':
                return self._desugar_and(expr)

            if name == 'or':
                return self._desugar_or(expr)

            # Check for typed variadic arithmetic operations
            if name in [
                'integer+', 'integer-', 'integer*', 'integer/',
                'float+', 'float-', 'float*', 'float/',
                'complex+', 'complex-', 'complex*', 'complex/',
            ]:
                return self._desugar_variadic_arithmetic(expr)

            # Fold-reducible variadic operations
            if name in [
                'bit-or', 'bit-and', 'bit-xor',
                'integer-min', 'integer-max',
                'float-min', 'float-max',
                'list-concat',
                'string-concat',
            ]:
                return self._desugar_fold_variadic(expr)

            # Variadic comparison chains (short-circuit with 'and' is correct)
            if name in [
                'integer<?', 'integer>?', 'integer<=?', 'integer>=?',
                'float<?',   'float>?',   'float<=?',   'float>=?',
                'string<?',  'string>?',  'string<=?',  'string>=?',
            ]:
                return self._desugar_comparison_chain(expr)

            # Strict equality predicates
            if name in [
                'boolean=?', 'integer=?', 'float=?', 'complex=?', 'string=?', 'list=?', 'dict=?'
            ]:
                return self._desugar_strict_equality(expr)

            # Strict inequality predicates
            if name in [
                'boolean!=?', 'integer!=?', 'float!=?', 'complex!=?', 'string!=?', 'list!=?', 'dict!=?'
            ]:
                return self._desugar_strict_inequality(expr)

        # Regular function call - desugar all elements
        return self._desugar_call(expr)

    def _desugar_if(self, expr: MenaiASTList) -> MenaiASTNode:
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

    def _desugar_let(self, expr: MenaiASTList) -> MenaiASTNode:
        """Desugar let expression by desugaring its subexpressions."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements (validated by semantic analyzer)"

        let_symbol = expr.elements[0]
        bindings_list = expr.elements[1]
        body = expr.elements[2]

        assert isinstance(bindings_list, MenaiASTList), "Binding list should be a list (validated by semantic analyzer)"

        # Desugar each binding value
        desugared_bindings = []
        for i, binding in enumerate(bindings_list.elements):
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2, \
                f"Binding {i+1} should be a list with 2 elements (validated by semantic analyzer)"

            var_name, value_expr = binding.elements
            desugared_value = self.desugar(value_expr)
            desugared_bindings.append(self._make_list((var_name, desugared_value), binding))

        # Desugar body
        desugared_body = self.desugar(body)

        return self._make_list((
            let_symbol, self._make_list(tuple(desugared_bindings), bindings_list), desugared_body
        ), expr)

    def _desugar_let_star(self, expr: MenaiASTList) -> MenaiASTNode:
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
        assert isinstance(bindings_list, MenaiASTList), "Binding list should be a list (validated by semantic analyzer)"

        # If no bindings, just return the body
        if len(bindings_list.elements) == 0:
            return self.desugar(body)

        # Build nested lets from the inside out
        # Start with the body
        result = self.desugar(body)

        # Wrap in nested lets, processing bindings in reverse order
        for binding in reversed(bindings_list.elements):
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2, \
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

    def _desugar_lambda(self, expr: MenaiASTList) -> MenaiASTNode:
        """Desugar lambda expression by desugaring its body."""
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 3, "Lambda expression should have exactly 3 elements (validated by semantic analyzer)"

        lambda_symbol, params_list, body = expr.elements

        # Desugar body
        desugared_body = self.desugar(body)

        return self._make_list((lambda_symbol, params_list, desugared_body), expr)

    def _desugar_trace(self, expr: MenaiASTList) -> MenaiASTNode:
        """
        Desugar trace special form.

        (trace msg1 msg2 ... msgN expr)

        Trace is kept as-is but with desugared subexpressions.
        The IR builder will handle creating the trace IR node.
        """
        if expr.length() < 3:  # (trace msg expr) minimum
            raise MenaiEvalError(
                message="trace requires at least 2 arguments (message, expr)",
                suggestion="Usage: (trace \"message\" expression)"
            )

        # Desugar all subexpressions
        desugared_elements: List[MenaiASTNode] = [self._make_symbol('trace', expr)]
        for elem in expr.elements[1:]:  # Skip 'trace' symbol
            desugared_elements.append(self.desugar(elem))

        return self._make_list(tuple(desugared_elements), expr)

    def _desugar_call(self, expr: MenaiASTList) -> MenaiASTNode:
        """Desugar function call by desugaring all elements."""
        desugared_elements = []
        for elem in expr.elements:
            desugared_elements.append(self.desugar(elem))

        return self._make_list(tuple(desugared_elements), expr)

    def _desugar_variadic_arithmetic(self, expr: MenaiASTList) -> MenaiASTNode:
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
        assert isinstance(op_symbol, MenaiASTSymbol)
        op_name = op_symbol.name

        args = list(expr.elements[1:])

        # Determine identity elements and sub operator for this family.
        if op_name in ('integer+', 'integer-', 'integer*', 'integer/'):
            zero: MenaiASTNode = MenaiASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)
            one: MenaiASTNode = MenaiASTInteger(1, line=expr.line, column=expr.column, source_file=expr.source_file)

        elif op_name in ('float+', 'float-', 'float*', 'float/'):
            zero = MenaiASTFloat(0.0, line=expr.line, column=expr.column, source_file=expr.source_file)
            one = MenaiASTFloat(1.0, line=expr.line, column=expr.column, source_file=expr.source_file)

        elif op_name in ('complex+', 'complex-', 'complex*', 'complex/'):
            zero = MenaiASTComplex(0+0j, line=expr.line, column=expr.column, source_file=expr.source_file)
            one = MenaiASTComplex(1+0j, line=expr.line, column=expr.column, source_file=expr.source_file)

        else:
            assert False, f"Unexpected operator in _desugar_variadic_arithmetic: {op_name!r}"

        is_add = op_name in ('integer+', 'float+', 'complex+')
        is_mul = op_name in ('integer*', 'float*', 'complex*')

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

            if is_add or is_mul:
                # (+ x) → x, (* x) → x [identity]
                return desugared_arg

            # subtraction/division with one arg is an error - fall through to runtime
            return self._desugar_call(expr)

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

    def _desugar_fold_variadic(self, expr: MenaiASTList) -> MenaiASTNode:
        """
        Desugar fold-reducible variadic operations to nested binary operations.

        These are operations where (f a b c) means (f (f a b) c) — a left fold.
        Each operation has a natural identity element for the 0-arg case.

        Handles:
            bit-or, bit-and, bit-xor  — bitwise ops, identity: 0
            list-concat               — list concatenation, identity: ()
            string-append             — string concatenation, identity: ""
            min, max                  — numeric reduction (1+ args required)
        """
        op_symbol = expr.first()
        assert isinstance(op_symbol, MenaiASTSymbol)
        op_name = op_symbol.name

        args = list(expr.elements[1:])

        # 0-arg identity cases
        if len(args) == 0:
            if op_name == 'bit-or':
                return MenaiASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)

            if op_name == 'bit-and':
                return MenaiASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)

            if op_name == 'bit-xor':
                return MenaiASTInteger(0, line=expr.line, column=expr.column, source_file=expr.source_file)

            if op_name == 'string-concat':
                return MenaiASTString("", line=expr.line, column=expr.column, source_file=expr.source_file)

            if op_name == 'list-concat':
                return self._make_list((self._make_symbol('quote', expr), self._make_list((), expr)), expr)

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

    def _desugar_comparison_chain(self, expr: MenaiASTList) -> MenaiASTNode:
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
        assert isinstance(op_symbol, MenaiASTSymbol)
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
        pairs: List[MenaiASTNode] = [
            self._make_list((self._make_symbol(op_name, expr),
                             self._make_symbol(temps[i], expr),
                             self._make_symbol(temps[i + 1], expr)), expr)
            for i in range(len(temps) - 1)
        ]

        # Chain with 'and' for ordered comparisons
        body: MenaiASTNode = self._make_and(pairs, expr)

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

    def _desugar_strict_equality_chain(self, expr: MenaiASTList, check_eq: bool) -> MenaiASTNode:
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
        assert isinstance(op_symbol, MenaiASTSymbol)
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

            # Body: and/or of pair temps, lowered to if-chain
            connector = 'and' if check_eq else 'or'
            pair_temp_syms: List[MenaiASTNode] = [self._make_symbol(pt, expr) for pt in pair_temps]
            body: MenaiASTNode = (self._make_and(pair_temp_syms, expr) if connector == 'and'
                                  else self._make_or(pair_temp_syms, expr))

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

    def _desugar_strict_equality(self, expr: MenaiASTList) -> MenaiASTNode:
        """Desugar strict type-specific equality predicates."""
        return self._desugar_strict_equality_chain(expr, check_eq=True)

    def _desugar_strict_inequality(self, expr: MenaiASTList) -> MenaiASTNode:
        """Desugar strict type-specific inequality predicates."""
        return self._desugar_strict_equality_chain(expr, check_eq=False)

    def _desugar_match(self, expr: MenaiASTList) -> MenaiASTNode:
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
            assert isinstance(clause, MenaiASTList) and len(clause.elements) == 2, \
                f"Clause {i+1} should be a list with 2 elements (validated by semantic analyzer)"

        # Generate temp variable for match value
        temp_var = self._gen_temp()

        # Desugar the value expression
        desugared_value = self.desugar(value_expr)

        # Build the match logic as nested if/let expressions
        match_logic = self._build_match_clauses(temp_var, clauses)

        # Wrap in let to bind the temp variable
        # (let ((temp value)) match-logic)
        result = MenaiASTList((
            MenaiASTSymbol('let*'),
            MenaiASTList((
                MenaiASTList((MenaiASTSymbol(temp_var), desugared_value)),
            )),
            match_logic
        ))

        # Recursively desugar the result to handle let* -> nested let transformation
        return self.desugar(result)

    def _build_match_clauses(self, temp_var: str, clauses: List[MenaiASTNode]) -> MenaiASTNode:
        """
        Build nested if/let structure for match clauses.

        Args:
            temp_var: Name of temp variable holding the match value
            clauses: Non-empty list of (pattern, result) clauses (validated by semantic analyzer)

        Returns:
            Nested if/let AST
        """
        # Partition clauses into groups.  A group is either:
        #   - A maximal contiguous run of literal arms sharing the same type
        #     (integer, float, complex, boolean, string).  These can share a
        #     single hoisted type guard, with bare equality checks per arm.
        #   - A single non-literal arm (wildcard, variable binding, predicate
        #     pattern, list/cons pattern) processed individually as before.
        #
        # We build the result right-to-left so each group's else branch is the
        # already-built remainder.

        no_match_error: MenaiASTNode = MenaiASTList((
            MenaiASTSymbol('error'),
            MenaiASTString("No patterns matched in match expression")
        ))

        # Partition into groups: list of (group_type, [clause, ...])
        # group_type is the Python AST class for literal groups, or None for singles.
        groups: List[Tuple[Any, List[MenaiASTNode]]] = []
        i = 0
        while i < len(clauses):
            clause = clauses[i]
            assert isinstance(clause, MenaiASTList)
            pattern = clause.elements[0]
            lit_type = self._literal_pattern_type(pattern)

            if lit_type is not None:
                # Start or extend a literal group of this type
                group: List[MenaiASTNode] = [clause]
                j = i + 1
                while j < len(clauses):
                    next_clause = clauses[j]
                    assert isinstance(next_clause, MenaiASTList)
                    if not self._literal_pattern_type(next_clause.elements[0]) is lit_type:
                        break

                    group.append(next_clause)
                    j += 1

                groups.append((lit_type, group))
                i = j

            else:
                groups.append((None, [clause]))
                i += 1

        # Build right-to-left: start with the no-match error as the base.
        result: MenaiASTNode = no_match_error

        for group_type, group_clauses in reversed(groups):
            if group_type is not None:
                # Literal group: hoist a single type guard over all arms.
                result = self._build_literal_group(temp_var, group_type, group_clauses, result)

            else:
                # Single non-literal clause: use the original per-arm path.
                clause = group_clauses[0]
                assert isinstance(clause, MenaiASTList)
                pattern = clause.elements[0]
                desugared_result = self.desugar(clause.elements[1])
                test_expr, bindings = self._desugar_pattern(pattern, temp_var)
                result = self._build_clause_with_bindings(
                    test_expr, bindings, desugared_result, result
                )

        return result

    def _literal_pattern_type(self, pattern: MenaiASTNode) -> type | None:
        """
        Return the Python AST class for a literal pattern, or None if the
        pattern is not a simple literal (i.e. it is a wildcard, variable,
        predicate pattern, or list/cons pattern).

        Only the types that have a safe typed equality operator are included:
        boolean, integer, float, complex, string.  MenaiASTNone is excluded
        because (none? tmp) is already a singleton predicate with no equality
        check needed — grouping adds no benefit there.
        """
        if isinstance(pattern, (MenaiASTBoolean, MenaiASTInteger,
                                MenaiASTFloat, MenaiASTComplex, MenaiASTString)):
            return type(pattern)

        return None

    def _build_literal_group(
        self,
        temp_var: str,
        lit_type: type,
        clauses: List[MenaiASTNode],
        else_expr: MenaiASTNode,
    ) -> MenaiASTNode:
        """
        Build a type-guarded block for a run of same-type literal arms.

        Emits:
            (if (type? tmp)
                (if (type=? tmp lit0) result0
                (if (type=? tmp lit1) result1
                    ...
                    else_expr))
                else_expr)

        The type guard is emitted once.  Each arm uses only the bare equality
        check, with no per-arm type predicate.
        """
        # Map AST literal class → (type-predicate name, equality-op name)
        _type_info: dict[type, Tuple[str, str]] = {
            MenaiASTBoolean: ('boolean?', 'boolean=?'),
            MenaiASTInteger: ('integer?', 'integer=?'),
            MenaiASTFloat:   ('float?',   'float=?'),
            MenaiASTComplex: ('complex?', 'complex=?'),
            MenaiASTString:  ('string?',  'string=?'),
        }
        type_pred, eq_op = _type_info[lit_type]
        tmp_sym = MenaiASTSymbol(temp_var)

        # Build the inner equality chain right-to-left, falling through to else_expr.
        inner: MenaiASTNode = else_expr
        for clause in reversed(clauses):
            assert isinstance(clause, MenaiASTList)
            pattern = clause.elements[0]
            desugared_result = self.desugar(clause.elements[1])
            eq_test = MenaiASTList((MenaiASTSymbol(eq_op), tmp_sym, pattern))
            inner = MenaiASTList((MenaiASTSymbol('if'), eq_test, desugared_result, inner))

        # Wrap in the single type guard.
        type_test = MenaiASTList((MenaiASTSymbol(type_pred), tmp_sym))
        return MenaiASTList((MenaiASTSymbol('if'), type_test, inner, else_expr))

    def _build_clause_with_bindings(
        self,
        test_expr: MenaiASTNode,
        bindings: List[Tuple[str, Any]],  # Pattern variable bindings or special markers
        result_expr: MenaiASTNode,
        else_expr: MenaiASTNode
    ) -> MenaiASTNode:
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
            binding_list = [MenaiASTList((MenaiASTSymbol(vn), ve)) for vn, ve in bindings]

            then_expr: MenaiASTNode = MenaiASTList((
                MenaiASTSymbol('let*'),
                MenaiASTList(tuple(binding_list)),
                result_expr
            ))
        else:
            then_expr = result_expr

        # Build if expression
        return MenaiASTList((
            MenaiASTSymbol('if'),
            test_expr,
            then_expr,
            else_expr
        ))

    def _desugar_pattern(
        self,
        pattern: MenaiASTNode,
        temp_var: str
    ) -> Tuple[MenaiASTNode, List[Tuple[str, MenaiASTNode]]]:
        """
        Desugar a pattern into (test_expr, bindings).

        Args:
            pattern: Pattern AST
            temp_var: Name of temp variable holding the match value

        Returns:
            (test_expression, [(var_name, value_expr), ...])

        Example:
            Pattern: (? number? n)
            Temp: "#:match-tmp-1"
            Returns: ((number? #:match-tmp-1), [("n", #:match-tmp-1)])  ; pred called directly
        """
        # Literal patterns: #none, booleans, numbers, strings
        #
        # Each literal pattern is compiled as:
        #   (and (type? tmp) (type=? tmp literal))
        #
        # The type-guard is essential for correctness: without it, presenting a
        # value of the wrong type to the equality operator raises a type error
        # instead of simply not matching.  The guard short-circuits via `and`
        # so the equality check is only reached when the type is known correct.
        #
        # #none is the sole exception — (none? tmp) is already a safe predicate
        # that returns #f for any non-none value, so no separate equality check
        # is needed.

        if isinstance(pattern, MenaiASTNone):
            # (none? tmp) — singleton predicate, inherently type-safe
            test_expr: MenaiASTNode = MenaiASTList((
                MenaiASTSymbol('none?'),
                MenaiASTSymbol(temp_var),
            ))
            return (test_expr, [])

        if isinstance(pattern, MenaiASTBoolean):
            # (and (boolean? tmp) (boolean=? tmp literal))
            test_expr = self._make_and([
                MenaiASTList((MenaiASTSymbol('boolean?'), MenaiASTSymbol(temp_var))),
                MenaiASTList((MenaiASTSymbol('boolean=?'), MenaiASTSymbol(temp_var), pattern)),
            ], pattern)
            return (test_expr, [])

        if isinstance(pattern, MenaiASTInteger):
            # (and (integer? tmp) (integer=? tmp literal))
            test_expr = self._make_and([
                MenaiASTList((MenaiASTSymbol('integer?'), MenaiASTSymbol(temp_var))),
                MenaiASTList((MenaiASTSymbol('integer=?'), MenaiASTSymbol(temp_var), pattern)),
            ], pattern)
            return (test_expr, [])

        if isinstance(pattern, MenaiASTFloat):
            # (and (float? tmp) (float=? tmp literal))
            test_expr = self._make_and([
                MenaiASTList((MenaiASTSymbol('float?'), MenaiASTSymbol(temp_var))),
                MenaiASTList((MenaiASTSymbol('float=?'), MenaiASTSymbol(temp_var), pattern)),
            ], pattern)
            return (test_expr, [])

        if isinstance(pattern, MenaiASTComplex):
            # (and (complex? tmp) (complex=? tmp literal))
            test_expr = self._make_and([
                MenaiASTList((MenaiASTSymbol('complex?'), MenaiASTSymbol(temp_var))),
                MenaiASTList((MenaiASTSymbol('complex=?'), MenaiASTSymbol(temp_var), pattern)),
            ], pattern)
            return (test_expr, [])

        if isinstance(pattern, MenaiASTString):
            # (and (string? tmp) (string=? tmp literal))
            test_expr = self._make_and([
                MenaiASTList((MenaiASTSymbol('string?'), MenaiASTSymbol(temp_var))),
                MenaiASTList((MenaiASTSymbol('string=?'), MenaiASTSymbol(temp_var), pattern)),
            ], pattern)
            return (test_expr, [])

        # Variable pattern: binds the value
        if isinstance(pattern, MenaiASTSymbol):
            if pattern.name == '_':
                # Wildcard - always matches, no binding
                return (MenaiASTBoolean(True), [])

            # Variable binding - always matches, binds variable
            return (
                MenaiASTBoolean(True),
                [(pattern.name, MenaiASTSymbol(temp_var))]
            )

        # List patterns
        if isinstance(pattern, MenaiASTList):
            return self._desugar_list_pattern(pattern, temp_var)

        # Should never reach here - semantic analyzer validates patterns
        assert False, f"Unknown pattern type: {type(pattern).__name__} (should be validated by semantic analyzer)"

    def _desugar_list_pattern(
        self,
        pattern: MenaiASTList,
        temp_var: str
    ) -> Tuple[MenaiASTNode, List[Tuple[str, MenaiASTNode]]]:
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
            test_expr = MenaiASTList((
                MenaiASTSymbol('list-null?'),
                MenaiASTSymbol(temp_var)
            ))
            return (test_expr, [])

        # Check for predicate test pattern: (? pred var)
        if (len(pattern.elements) == 3 and
            isinstance(pattern.elements[0], MenaiASTSymbol) and
            pattern.elements[0].name == '?'):

            pred_expr = pattern.elements[1]
            var_pattern = pattern.elements[2]

            assert isinstance(var_pattern, MenaiASTSymbol), \
                "Predicate pattern variable should be a symbol (validated by semantic analyzer)"

            # Test: (pred temp_var) — pred can be any expression
            test_expr = MenaiASTList((
                pred_expr,
                MenaiASTSymbol(temp_var)
            ))

            # Binding: bind the variable to temp_var (unless wildcard)
            bindings: List[Tuple[str, MenaiASTNode]] = []
            if var_pattern.name != '_':
                bindings.append((var_pattern.name, MenaiASTSymbol(temp_var)))

            return (test_expr, bindings)

        # Check for cons pattern: (head . tail) or (a b . rest)
        dot_positions = []
        for i, elem in enumerate(pattern.elements):
            if isinstance(elem, MenaiASTSymbol) and elem.name == '.':
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
        pattern: MenaiASTList,
        temp_var: str
    ) -> Tuple[MenaiASTNode, List[Tuple[str, Any]]]:
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
        list_test = MenaiASTList((
            MenaiASTSymbol('list?'),
            MenaiASTSymbol(temp_var)
        ))

        # Second test: (= (length temp_var) num_elements)
        length_test = MenaiASTList((
            MenaiASTSymbol('integer=?'),
            MenaiASTList((
                MenaiASTSymbol('list-length'),
                MenaiASTSymbol(temp_var)
            )),
            MenaiASTInteger(num_elements)
        ))

        # Combine with and (lowered to if-chain)
        combined_test = self._make_and([list_test, length_test], MenaiASTList(()))

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
        element_info: List[Tuple[MenaiASTNode, str, MenaiASTNode]] = []  # List of (pattern, temp_var, extraction_expr)

        for i, elem_pattern in enumerate(pattern.elements):
            # Generate temp var for this element
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = MenaiASTList((
                MenaiASTSymbol('list-ref'),
                MenaiASTSymbol(temp_var),
                MenaiASTInteger(i)
            ))

            element_info.append((elem_pattern, elem_temp, elem_value))

        # Return the basic test and a special marker with element info
        # The caller will use this to build the proper nested structure
        # Use a unique marker name to avoid duplicates in nested patterns
        marker_name = f'__LIST_PATTERN_{self._gen_temp()}__'
        return (combined_test, [(marker_name, element_info)])

    def _flatten_nested_pattern(
        self,
        pattern: MenaiASTNode,
        temp_var: str,
        extraction_bindings: List[Tuple[str, MenaiASTNode]],
        element_tests: List[MenaiASTNode],
        pattern_bindings: List[Tuple[str, MenaiASTNode]]
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
            nested_element_info = cast(List[Tuple[MenaiASTNode, str, MenaiASTNode]], bindings[0][1])

            # Add the length/type test
            if not (isinstance(test, MenaiASTBoolean) and test.value):
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
            if not (isinstance(test, MenaiASTBoolean) and test.value):
                element_tests.append(test)
            pattern_bindings.extend(bindings)

    def _build_list_pattern_clause(
        self,
        length_test: MenaiASTNode,
        element_info: List,
        result_expr: MenaiASTNode,
        else_expr: MenaiASTNode
    ) -> MenaiASTNode:
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
        extraction_bindings: List[Tuple[str, MenaiASTNode]] = []
        element_tests: List[MenaiASTNode] = []
        pattern_bindings: List[Tuple[str, MenaiASTNode]] = []

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
                if not (isinstance(elem_test, MenaiASTBoolean) and elem_test.value):
                    element_tests.append(elem_test)

                # Collect pattern bindings
                pattern_bindings.extend(elem_bindings)

        # Build the inner structure (after element extraction)
        if element_tests:
            # We have element tests - need nested if
            elem_test_combined = self._make_and(element_tests, MenaiASTList(()))

            # Build pattern bindings let
            if pattern_bindings:
                binding_list = [MenaiASTList((MenaiASTSymbol(vn), ve)) for vn, ve in pattern_bindings]
                pattern_let: MenaiASTNode = MenaiASTList((
                    MenaiASTSymbol('let*'),
                    MenaiASTList(tuple(binding_list)),
                    result_expr
                ))

            else:
                pattern_let = result_expr

            # Build element test if
            inner_if: MenaiASTNode = MenaiASTList((
                MenaiASTSymbol('if'),
                elem_test_combined,
                pattern_let,
                else_expr
            ))

        else:
            # No element tests - just bind pattern vars
            if pattern_bindings:
                binding_list = [MenaiASTList((MenaiASTSymbol(vn), ve)) for vn, ve in pattern_bindings]
                inner_if = MenaiASTList((
                    MenaiASTSymbol('let*'),
                    MenaiASTList(tuple(binding_list)),
                    result_expr
                ))

            else:
                inner_if = result_expr

        # Wrap in element extraction let
        extraction_binding_list = [MenaiASTList((MenaiASTSymbol(vn), ve)) for vn, ve in extraction_bindings]
        extraction_let = MenaiASTList((
            MenaiASTSymbol('let*'),
            MenaiASTList(tuple(extraction_binding_list)),
            inner_if
        ))

        # Build outer if with length test
        return MenaiASTList((
            MenaiASTSymbol('if'),
            length_test,
            extraction_let,
            else_expr
        ))

    def _desugar_cons_pattern(
        self,
        pattern: MenaiASTList,
        temp_var: str,
        dot_position: int
    ) -> Tuple[MenaiASTNode, List[Tuple[str, Any]]]:
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
        list_test = MenaiASTList((
            MenaiASTSymbol('list?'),
            MenaiASTSymbol(temp_var)
        ))

        length_test = MenaiASTList((
            MenaiASTSymbol('integer>=?'),
            MenaiASTList((
                MenaiASTSymbol('list-length'),
                MenaiASTSymbol(temp_var)
            )),
            MenaiASTInteger(dot_position)
        ))
        combined_test = self._make_and([list_test, length_test], MenaiASTList(()))

        # Collect head element info
        head_elements: List[Tuple[MenaiASTNode, str, MenaiASTNode]] = []

        for i in range(dot_position):
            elem_pattern = pattern.elements[i]
            elem_temp = self._gen_temp()

            # Extract element: (list-ref temp_var i)
            elem_value = MenaiASTList((
                MenaiASTSymbol('list-ref'),
                MenaiASTSymbol(temp_var),
                MenaiASTInteger(i)
            ))

            head_elements.append((elem_pattern, elem_temp, elem_value))

        # Build binding for tail
        tail_pattern = pattern.elements[dot_position + 1]
        tail_temp = self._gen_temp()

        # Extract tail: (list-slice temp-var dot_position)
        tail_value = MenaiASTList((
            MenaiASTSymbol('list-slice'),
            MenaiASTSymbol(temp_var),
            MenaiASTInteger(dot_position)
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
