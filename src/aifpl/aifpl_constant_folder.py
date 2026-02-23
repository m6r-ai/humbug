"""
AIFPL AST Optimizer - Optimizes AST before bytecode compilation.

This module provides a framework for AST optimization passes that run after
desugaring but before bytecode compilation. Optimizations transform the AST
while preserving runtime semantics.
"""

import cmath
import math
from typing import List

from aifpl.aifpl_ast import (
    AIFPLASTNode, AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex,
    AIFPLASTBoolean, AIFPLASTSymbol, AIFPLASTList, AIFPLASTString
)
from aifpl.aifpl_optimization_pass import AIFPLOptimizationPass


class AIFPLConstantFolder(AIFPLOptimizationPass):
    """
    Fold constant expressions at compile time.

    This pass evaluates expressions that contain only compile-time constants,
    replacing them with their computed values. This reduces bytecode size and
    improves runtime performance.

    Examples:
        (integer+ 1 2) → 3
        (integer* 2 3) → 6
        (integer+ (integer* 2 3) (integer* 4 5)) → 26
        (integer< 5 10) → #t
        (and #t #t) → #t
    """

    # Builtin operations we can fold
    FOLDABLE_BUILTINS = {
        '//', '%',
        '<', '>', '<=', '>=',
        'min', 'max',
        'round', 'floor', 'ceil',
        'number=?',

        'boolean=?',
        'not',
        'integer=?',
        'integer+',
        'integer-',
        'integer*',
        'integer/',
        'integer-negate',
        'integer-abs',
        'bit-or',
        'bit-and',
        'bit-xor',
        'bit-not',
        'bit-shift-left',
        'bit-shift-right',
        'float=?',
        'float+',
        'float-',
        'float*',
        'float/',
        'float-negate',
        'float-expt',
        'float-sin',
        'float-cos',
        'float-tan',
        'float-log',
        'float-log10',
        'float-exp',
        'float-sqrt',
        'float-abs',
        'complex',
        'complex=?',
        'complex+',
        'complex-',
        'complex*',
        'complex/',
        'complex-negate',
        'complex-expt',
        'complex-sin',
        'complex-cos',
        'complex-tan',
        'complex-log',
        'complex-log10',
        'complex-exp',
        'complex-sqrt',
        'complex-abs',
        'real',
        'imag',
        'string=?',
    }

    def __init__(self) -> None:
        """
        Initialize jump tables for fast operation dispatch.

        This is called once during initialization to build a dictionary mapping
        operation names to their corresponding fold/optimize methods. This replaces
        expensive if-elif chains with O(1) dictionary lookup.
        """
        # Build jump table for foldable builtin operations
        self._builtin_jump_table = {
            'boolean=?': self._fold_boolean_eq,
            'boolean!=?': self._fold_boolean_neq,
            'not': self._fold_not,
            'integer=?': self._fold_integer_eq,
            'integer!=?': self._fold_integer_neq,
            'integer+': self._fold_integer_add,
            'integer-': self._fold_integer_sub,
            'integer*': self._fold_integer_mul,
            'integer/': self._fold_integer_div,
            'integer-negate': self._fold_integer_negate,
            'integer-abs': self._fold_integer_abs,
            'bit-or': self._fold_bit_or,
            'bit-and': self._fold_bit_and,
            'bit-xor': self._fold_bit_xor,
            'bit-not': self._fold_bit_not,
            'bit-shift-left': self._fold_bit_shift_left,
            'bit-shift-right': self._fold_bit_shift_right,
            'float=?': self._fold_float_eq,
            'float!=?': self._fold_float_neq,
            'float+': self._fold_float_add,
            'float-': self._fold_float_sub,
            'float*': self._fold_float_mul,
            'float/': self._fold_float_div,
            'float-negate': self._fold_float_negate,
            'float-expt': self._fold_float_expt,
            'float-sin': self._fold_float_sin,
            'float-cos': self._fold_float_cos,
            'float-tan': self._fold_float_tan,
            'float-log': self._fold_float_log,
            'float-log10': self._fold_float_log10,
            'float-exp': self._fold_float_exp,
            'float-sqrt': self._fold_float_sqrt,
            'float-abs': self._fold_float_abs,
            'complex=?': self._fold_complex_eq,
            'complex!=?': self._fold_complex_neq,
            'complex': self._fold_complex,
            'complex+': self._fold_complex_add,
            'complex-': self._fold_complex_sub,
            'complex*': self._fold_complex_mul,
            'complex/': self._fold_complex_div,
            'real': self._fold_real,
            'imag': self._fold_imag,
            'complex-negate': self._fold_complex_negate,
            'complex-expt': self._fold_complex_expt,
            'complex-sin': self._fold_complex_sin,
            'complex-cos': self._fold_complex_cos,
            'complex-tan': self._fold_complex_tan,
            'complex-log': self._fold_complex_log,
            'complex-log10': self._fold_complex_log10,
            'complex-exp': self._fold_complex_exp,
            'complex-sqrt': self._fold_complex_sqrt,
            'complex-abs': self._fold_complex_abs,
            'string=?': self._fold_string_eq,
            'string!=?': self._fold_string_neq,

            # Arithmetic operations
            '//': self._fold_floor_divide,
            '%': self._fold_modulo,

            # Comparison operations
            '<': self._fold_less_than,
            '>': self._fold_greater_than,
            '<=': self._fold_less_equal,
            '>=': self._fold_greater_equal,
            'min': self._fold_min,
            'max': self._fold_max,
            'round': self._fold_round,
            'floor': self._fold_floor,
            'ceil': self._fold_ceil,

            # Strict type-specific equality predicates
            'number=?': self._fold_number_eq,
        }

        # Build jump table for special form optimization.  Note we don't include any special forms that were
        # removed by desugaring.
        self._special_form_jump_table = {
            'and': self._optimize_and,
            'or': self._optimize_or,
            'if': self._optimize_if,
            'let': self._optimize_let,
            'letrec': self._optimize_let,
            'lambda': self._optimize_lambda,
            'quote': self._optimize_quote,
            'error': self._optimize_error,
        }

    def optimize(self, expr: AIFPLASTNode) -> AIFPLASTNode:
        """
        Recursively fold constants in expression tree.

        Args:
            expr: Input expression

        Returns:
            Optimized expression (may be same as input if no folding possible)
        """
        # We're only interested in lists.  Anything else we allow to pass through.
        if not isinstance(expr, AIFPLASTList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()

        # Check if this is a foldable builtin call
        if isinstance(first, AIFPLASTSymbol):
            op_name = first.name

            # Check if it's a special form (use jump table)
            if op_name in self._special_form_jump_table:
                optimizer = self._special_form_jump_table[op_name]
                assert optimizer is not None
                return optimizer(expr)

            # Check if it's a foldable builtin
            if op_name in self.FOLDABLE_BUILTINS:
                return self._try_fold_builtin(op_name, list(expr.elements[1:]))

        # Not a foldable call - recursively optimize arguments
        optimized_elements = [self.optimize(elem) for elem in expr.elements]
        return AIFPLASTList(tuple(optimized_elements))

    def _optimize_error(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Optimize 'error' special form: (error message)

        Currently, we just recursively optimize all elements.
        """
        optimized_elements = [self.optimize(elem) for elem in expr.elements]
        return AIFPLASTList(tuple(optimized_elements))

    def _optimize_and(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Optimize 'and' special form with short-circuit evaluation.

        (and) → #t
        (and #f anything) → #f (short-circuit - don't optimize 'anything')
        (and #t #t #t) → #t
        (and #t x) → (and x) (remove constant, keep wrapper for type checking)
        (and #t #t x y) → (and x y) (remove all true constants)
        """
        if expr.is_empty() or len(expr.elements) == 1:
            # (and) → #t
            return AIFPLASTBoolean(True)

        # Get arguments (skip the 'and' symbol)
        args = expr.elements[1:]

        # Optimize arguments one at a time, short-circuiting when possible
        folded_args = []
        for arg in args:
            opt_arg = self.optimize(arg)

            # If we hit a false constant, short-circuit immediately
            if isinstance(opt_arg, AIFPLASTBoolean) and not opt_arg.value:
                return AIFPLASTBoolean(False)

            # If it's a true constant, we can skip it (doesn't affect result)
            if isinstance(opt_arg, AIFPLASTBoolean) and opt_arg.value:
                continue

            # Non-constant argument - keep it
            folded_args.append(opt_arg)

        # If all args were true constants, result is true
        if len(folded_args) == 0:
            return AIFPLASTBoolean(True)

        # One or more non-constant args remain - keep 'and' wrapper for runtime type checking
        return AIFPLASTList((AIFPLASTSymbol('and'),) + tuple(folded_args))

    def _optimize_or(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Optimize 'or' special form with short-circuit evaluation.

        (or) → #f
        (or #t anything) → #t (short-circuit - don't optimize 'anything')
        (or #f #f #f) → #f
        (or #f x) → (or x) (remove constant, keep wrapper for type checking)
        (or #f #f x y) → (or x y) (remove all false constants)
        """
        if expr.is_empty() or len(expr.elements) == 1:
            # (or) → #f
            return AIFPLASTBoolean(False)

        # Get arguments (skip the 'or' symbol)
        args = expr.elements[1:]

        # Optimize arguments one at a time, short-circuiting when possible
        folded_args = []
        for arg in args:
            opt_arg = self.optimize(arg)

            # If we hit a true constant, short-circuit immediately
            if isinstance(opt_arg, AIFPLASTBoolean) and opt_arg.value:
                return AIFPLASTBoolean(True)

            # If it's a false constant, we can skip it (doesn't affect result)
            if isinstance(opt_arg, AIFPLASTBoolean) and not opt_arg.value:
                continue

            # Non-constant argument - keep it
            folded_args.append(opt_arg)

        # If all args were false constants, result is false
        if len(folded_args) == 0:
            return AIFPLASTBoolean(False)

        # One or more non-constant args remain - keep 'or' wrapper for runtime type checking
        return AIFPLASTList((AIFPLASTSymbol('or'),) + tuple(folded_args))

    def _optimize_if(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Optimize 'if' special form: (if condition then else)

        Can eliminate branches if condition is a constant boolean.
        """
        assert len(expr.elements) == 4  # Earlier semantic analysis should ensure this
        _, condition, then_expr, else_expr = expr.elements

        # Optimize the condition
        opt_condition = self.optimize(condition)

        # If condition is a constant boolean, we can eliminate branches
        if isinstance(opt_condition, AIFPLASTBoolean):
            if opt_condition.value:
                # Condition is true, return optimized then branch
                return self.optimize(then_expr)

            # Condition is false, return optimized else branch
            return self.optimize(else_expr)

        # Can't eliminate, but optimize all branches
        opt_then = self.optimize(then_expr)
        opt_else = self.optimize(else_expr)
        return AIFPLASTList((expr.elements[0], opt_condition, opt_then, opt_else))

    def _optimize_let(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Optimize 'let'/'letrec' special form: (let ((var val) ...) body)

        Optimizes binding values and body.
        """
        assert len(expr.elements) == 3  # Earlier semantic analysis should ensure this
        form_symbol, bindings_list, body = expr.elements

        # Optimize binding values
        opt_bindings_list: AIFPLASTNode
        assert isinstance(bindings_list, AIFPLASTList)
        opt_bindings: List[AIFPLASTNode] = []
        for binding in bindings_list.elements:
            assert isinstance(binding, AIFPLASTList) and len(binding.elements) == 2
            var, val = binding.elements
            opt_val = self.optimize(val)
            opt_bindings.append(AIFPLASTList((var, opt_val)))

        opt_bindings_list = AIFPLASTList(tuple(opt_bindings))

        # Optimize body
        opt_body = self.optimize(body)
        return AIFPLASTList((form_symbol, opt_bindings_list, opt_body))

    def _optimize_lambda(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Optimize 'lambda' special form: (lambda (params) body)"""
        assert len(expr.elements) == 3  # Earlier semantic analysis should ensure this
        lambda_symbol, params, body = expr.elements
        opt_body = self.optimize(body)
        return AIFPLASTList((lambda_symbol, params, opt_body))

    def _optimize_quote(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """Optimize 'quote' special form - quoted expressions are not evaluated."""
        return expr

    def _try_fold_builtin(self, op_name: str, args: List[AIFPLASTNode]) -> AIFPLASTNode:
        """
        Try to fold a builtin operation.

        Args:
            op_name: Name of the builtin operation
            args: Argument expressions

        Returns:
            Folded constant value, or original expression if folding not possible
        """
        # Optimize arguments and check if all are constants in a single pass
        all_constants = True
        opt_args = []
        for arg in args:
            opt_arg = self.optimize(arg)
            opt_args.append(opt_arg)

            # Check if this arg is a constant (only if we haven't already determined it's not all constants)
            if not all_constants:
                continue

            # Symbols are not constants (variables)
            # Lists with symbol as first element are not constants (function calls)
            # Everything else is a constant (literals, empty lists, data lists)
            if isinstance(opt_arg, AIFPLASTSymbol):
                all_constants = False
                continue

            if isinstance(opt_arg, AIFPLASTList) and not opt_arg.is_empty() and isinstance(opt_arg.first(), AIFPLASTSymbol):
                all_constants = False

        if all_constants:
            # Try to evaluate the builtin
            try:
                fold_func = self._builtin_jump_table.get(op_name)
                assert fold_func is not None
                result = fold_func(opt_args)
                if result is not None:
                    return result

            except Exception:
                # Evaluation failed - preserve runtime error by not folding
                pass

        # Couldn't fold - return expression with optimized arguments
        return AIFPLASTList((AIFPLASTSymbol(op_name),) + tuple(opt_args))

    def _fold_boolean_eq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold boolean=?: all args must be booleans."""
        # Check all are booleans - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, AIFPLASTBoolean) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_boolean_neq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """
        Fold inequality: (boolean!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, AIFPLASTBoolean) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_not(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold not: (not a) → boolean"""
        if not isinstance(args[0], AIFPLASTBoolean):
            return None

        return AIFPLASTBoolean(not args[0].value)

    def _fold_integer_eq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer=?: all args must be integers."""
        # Check all are integers - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, AIFPLASTInteger) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_integer_neq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """
        Fold inequality: (integer!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, AIFPLASTInteger) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_integer_add(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer+: all args must be integers, returns integer."""
        if not all(isinstance(a, AIFPLASTInteger) for a in args):
            return None

        result = sum(a.value for a in args)  # type: ignore[union-attr]
        return AIFPLASTInteger(result)

    def _fold_integer_sub(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-: all args must be integers, returns integer."""
        if not all(isinstance(a, AIFPLASTInteger) for a in args):
            return None

        # By the time we reach the folder, desugaring has already reduced this
        # to a binary call, so len(args) == 2 always.  Guard anyway.
        result = args[0].value  # type: ignore[union-attr]
        for a in args[1:]:
            result -= a.value  # type: ignore[union-attr]

        return AIFPLASTInteger(result)

    def _fold_integer_mul(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer*: all args must be integers, returns integer."""
        if not all(isinstance(a, AIFPLASTInteger) for a in args):
            return None

        result = 1
        for a in args:
            result *= a.value  # type: ignore[union-attr]

        return AIFPLASTInteger(result)

    def _fold_integer_div(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer/: all args must be integers, floor division, returns integer."""
        if not all(isinstance(a, AIFPLASTInteger) for a in args):
            return None

        result = args[0].value  # type: ignore[union-attr]
        for a in args[1:]:
            divisor = a.value  # type: ignore[union-attr]
            if divisor == 0:
                return None  # Division by zero — let runtime raise the error

            result //= divisor

        return AIFPLASTInteger(result)

    def _fold_integer_negate(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-negate: arg must be integer, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        return AIFPLASTInteger(-args[0].value)

    def _fold_integer_abs(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-abs: arg must be integer, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        return AIFPLASTInteger(abs(args[0].value))

    def _fold_bit_or(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-or: (bit-or a b ...) → bitwise OR"""
        first_arg = args[0]
        if not isinstance(first_arg, AIFPLASTInteger):
            return None

        result = first_arg.value
        for arg in args[1:]:
            if not isinstance(arg, AIFPLASTInteger):
                return None

            result = result | arg.value

        return AIFPLASTInteger(result)

    def _fold_bit_and(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-and: (bit-and a b ...) → bitwise AND"""
        first_arg = args[0]
        if not isinstance(first_arg, AIFPLASTInteger):
            return None

        result = first_arg.value
        for arg in args[1:]:
            if not isinstance(arg, AIFPLASTInteger):
                return None

            result = result & arg.value

        return AIFPLASTInteger(result)

    def _fold_bit_xor(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-xor: (bit-xor a b ...) → bitwise XOR"""
        first_arg = args[0]
        if not isinstance(first_arg, AIFPLASTInteger):
            return None

        result = first_arg.value
        for arg in args[1:]:
            if not isinstance(arg, AIFPLASTInteger):
                return None

            result = result ^ arg.value

        return AIFPLASTInteger(result)

    def _fold_bit_not(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-not: (bit-not a) → bitwise NOT"""
        arg = args[0]
        if not isinstance(arg, AIFPLASTInteger):
            return None

        result = ~arg.value
        return AIFPLASTInteger(result)

    def _fold_bit_shift_left(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-shift-left: (bit-shift-left a b) → a << b"""
        arg0 = args[0]
        if not isinstance(arg0, AIFPLASTInteger):
            return None

        arg1 = args[1]
        if not isinstance(arg1, AIFPLASTInteger):
            return None

        result = arg0.value << arg1.value
        return AIFPLASTInteger(result)

    def _fold_bit_shift_right(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-shift-right: (bit-shift-right a b) → a >> b"""
        arg0 = args[0]
        if not isinstance(arg0, AIFPLASTInteger):
            return None

        arg1 = args[1]
        if not isinstance(arg1, AIFPLASTInteger):
            return None

        result = arg0.value >> arg1.value
        return AIFPLASTInteger(result)

    def _fold_float_eq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float=?: all args must be floats."""
        # Check all are floats - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, AIFPLASTFloat) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_float_neq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """
        Fold inequality: (float!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, AIFPLASTFloat) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_float_add(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float+: all args must be floats, returns float."""
        if not all(isinstance(a, AIFPLASTFloat) for a in args):
            return None

        result = sum(a.value for a in args)  # type: ignore[union-attr]
        return AIFPLASTFloat(result)

    def _fold_float_sub(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-: all args must be floats, returns float."""
        if not all(isinstance(a, AIFPLASTFloat) for a in args):
            return None

        result = args[0].value  # type: ignore[union-attr]
        for a in args[1:]:
            result -= a.value  # type: ignore[union-attr]

        return AIFPLASTFloat(result)

    def _fold_float_mul(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float*: all args must be floats, returns float."""
        if not all(isinstance(a, AIFPLASTFloat) for a in args):
            return None

        result = 1.0
        for a in args:
            result *= a.value  # type: ignore[union-attr]

        return AIFPLASTFloat(result)

    def _fold_float_div(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float/: all args must be floats, true division, returns float."""
        if not all(isinstance(a, AIFPLASTFloat) for a in args):
            return None

        result = args[0].value  # type: ignore[union-attr]
        for a in args[1:]:
            divisor = a.value  # type: ignore[union-attr]
            if divisor == 0.0:
                return None  # Division by zero — let runtime raise the error

            result /= divisor

        return AIFPLASTFloat(result)

    def _fold_float_negate(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-negate: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(-args[0].value)

    def _fold_float_expt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-expt: both args must be floats, returns float."""
        if not all(isinstance(a, AIFPLASTFloat) for a in args):
            return None

        result = args[0].value ** args[1].value  # type: ignore[union-attr]
        return AIFPLASTFloat(result)

    def _fold_float_sin(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-sin: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(math.sin(args[0].value))

    def _fold_float_cos(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-cos: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(math.cos(args[0].value))

    def _fold_float_tan(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-tan: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(math.tan(args[0].value))

    def _fold_float_log(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-log: arg must be float, returns float. Zero → -inf, negative → don't fold."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        if val == 0.0:
            return AIFPLASTFloat(float('-inf'))

        return AIFPLASTFloat(math.log(val))

    def _fold_float_log10(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-log10: arg must be float, returns float. Zero → -inf, negative → don't fold."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        if val == 0.0:
            return AIFPLASTFloat(float('-inf'))

        return AIFPLASTFloat(math.log10(val))

    def _fold_float_exp(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-exp: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(math.exp(args[0].value))

    def _fold_float_sqrt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-sqrt: arg must be non-negative float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        return AIFPLASTFloat(math.sqrt(val))

    def _fold_float_abs(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-abs: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(abs(args[0].value))

    def _fold_complex(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex: (complex real imag) → complex number"""
        real, imag = self._to_python_number(args[0]), self._to_python_number(args[1])

        # Don't fold if arguments are complex - runtime will raise error
        if isinstance(real, complex):
            return None

        if isinstance(imag, complex):
            return None

        result = complex(real, imag)
        return AIFPLASTComplex(result)

    def _fold_complex_eq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex=?: all args must be complex."""
        # Check all are complex - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, AIFPLASTComplex) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_complex_neq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """
        Fold inequality: (complex!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, AIFPLASTComplex) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_complex_add(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex+: all args must be complex, returns complex."""
        if not all(isinstance(a, AIFPLASTComplex) for a in args):
            return None

        result = sum((a.value for a in args), 0j)  # type: ignore[union-attr]
        return AIFPLASTComplex(result)

    def _fold_complex_sub(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-: all args must be complex, returns complex."""
        if not all(isinstance(a, AIFPLASTComplex) for a in args):
            return None

        result = args[0].value  # type: ignore[union-attr]
        for a in args[1:]:
            result -= a.value  # type: ignore[union-attr]

        return AIFPLASTComplex(result)

    def _fold_complex_mul(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex*: all args must be complex, returns complex."""
        if not all(isinstance(a, AIFPLASTComplex) for a in args):
            return None

        result = complex(1, 0)
        for a in args:
            result *= a.value  # type: ignore[union-attr]

        return AIFPLASTComplex(result)

    def _fold_complex_div(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex/: all args must be complex, returns complex."""
        if not all(isinstance(a, AIFPLASTComplex) for a in args):
            return None

        result = args[0].value  # type: ignore[union-attr]
        for a in args[1:]:
            divisor = a.value  # type: ignore[union-attr]
            if divisor == 0j:
                return None  # Division by zero — let runtime raise the error

            result /= divisor

        return AIFPLASTComplex(result)

    def _fold_complex_negate(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-negate: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(-args[0].value)

    def _fold_real(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold real: (real a) → real part"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return AIFPLASTFloat(val.real)

        # Convert to float to match runtime behavior
        return AIFPLASTFloat(float(val))

    def _fold_imag(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold imag: (imag a) → imaginary part"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return AIFPLASTFloat(val.imag)

        # Real numbers have 0.0 imaginary part (float, not int)
        return AIFPLASTFloat(0.0)

    def _fold_complex_expt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-expt: both args must be complex, returns complex."""
        if not all(isinstance(a, AIFPLASTComplex) for a in args):
            return None

        result = args[0].value ** args[1].value  # type: ignore[union-attr]
        return AIFPLASTComplex(result)

    def _fold_complex_sin(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-sin: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.sin(args[0].value))

    def _fold_complex_cos(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-cos: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.cos(args[0].value))

    def _fold_complex_tan(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-tan: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.tan(args[0].value))

    def _fold_complex_log(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-log: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.log(args[0].value))

    def _fold_complex_log10(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-log10: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.log10(args[0].value))

    def _fold_complex_exp(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-exp: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.exp(args[0].value))

    def _fold_complex_sqrt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-sqrt: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.sqrt(args[0].value))

    def _fold_complex_abs(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-abs: arg must be complex, returns float (magnitude)."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTFloat(abs(args[0].value))

    def _fold_string_eq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold string=?: all args must be strings."""
        # Check all are strings - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, AIFPLASTString) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_string_neq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """
        Fold inequality: (string!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, AIFPLASTString) for arg in args):
            return None

        first = args[0]
        return AIFPLASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_floor_divide(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold floor division: (// a b) → floor quotient"""
        a, b = self._to_python_number(args[0]), self._to_python_number(args[1])

        if b == 0:
            return None  # Division by zero

        # Floor division only works on real numbers
        if isinstance(a, complex) or isinstance(b, complex):
            return None

        result = a // b
        return self._from_python_number(result)

    def _fold_modulo(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold modulo: (% a b) → remainder"""
        a, b = self._to_python_number(args[0]), self._to_python_number(args[1])

        if b == 0:
            return None  # Division by zero

        # Modulo only works on real numbers
        if isinstance(a, complex) or isinstance(b, complex):
            return None

        result = a % b
        return self._from_python_number(result)

    # Comparison operations

    def _fold_less_than(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold less than: (< a b c ...) → boolean"""
        # Check if arguments are in strictly increasing order
        prev = self._to_python_number(args[0])

        # Complex numbers don't support ordering - can't fold
        if isinstance(prev, complex):
            return None

        for arg in args[1:]:
            curr = self._to_python_number(arg)
            if isinstance(curr, complex):
                return None

            if prev >= curr:
                return AIFPLASTBoolean(False)

            prev = curr

        return AIFPLASTBoolean(True)

    def _fold_greater_than(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold greater than: (> a b c ...) → boolean"""
        # Check if arguments are in strictly decreasing order
        prev = self._to_python_number(args[0])

        # Complex numbers don't support ordering - can't fold
        if isinstance(prev, complex):
            return None

        for arg in args[1:]:
            curr = self._to_python_number(arg)
            if isinstance(curr, complex):
                return None

            if prev <= curr:
                return AIFPLASTBoolean(False)

            prev = curr

        return AIFPLASTBoolean(True)

    def _fold_less_equal(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold less than or equal: (<= a b c ...) → boolean"""
        prev = self._to_python_number(args[0])

        # Complex numbers don't support ordering - can't fold
        if isinstance(prev, complex):
            return None

        for arg in args[1:]:
            curr = self._to_python_number(arg)
            if isinstance(curr, complex):
                return None

            if prev > curr:
                return AIFPLASTBoolean(False)

            prev = curr

        return AIFPLASTBoolean(True)

    def _fold_greater_equal(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold greater than or equal: (>= a b c ...) → boolean"""
        prev = self._to_python_number(args[0])

        # Complex numbers don't support ordering - can't fold
        if isinstance(prev, complex):
            return None

        for arg in args[1:]:
            curr = self._to_python_number(arg)
            if isinstance(curr, complex):
                return None

            if prev < curr:
                return AIFPLASTBoolean(False)

            prev = curr

        return AIFPLASTBoolean(True)

    def _fold_sqrt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold sqrt: (sqrt a) → square root"""
        val = self._to_python_number(args[0])

        # Only fold if input is complex OR non-negative real
        # Negative real/integer should raise error at runtime, not compile time
        if isinstance(val, complex):
            # Complex input: use cmath
            result = cmath.sqrt(val)

        elif val < 0:
            # Negative real/integer: don't fold, let runtime handle error
            return None

        else:
            # Non-negative real: use math.sqrt
            result = math.sqrt(val)

        return self._from_python_number(result)

    def _fold_abs(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold abs: (abs a) → absolute value"""
        val = self._to_python_number(args[0])
        result = abs(val)

        return self._from_python_number(result)

    def _fold_min(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold min: (min a b c ...) → minimum"""
        vals = [self._to_python_number(arg) for arg in args]

        # min/max don't work with complex numbers - can't fold
        if any(isinstance(v, complex) for v in vals):
            return None

        # Type narrowing: we've excluded complex, so only int | float remain
        # Cast to help mypy understand this
        real_vals = [v for v in vals if not isinstance(v, complex)]
        result = min(real_vals)

        return self._from_python_number(result)

    def _fold_max(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold max: (max a b c ...) → maximum"""
        vals = [self._to_python_number(arg) for arg in args]

        # min/max don't work with complex numbers - can't fold
        if any(isinstance(v, complex) for v in vals):
            return None

        # Type narrowing: we've excluded complex, so only int | float remain
        # Cast to help mypy understand this
        real_vals = [v for v in vals if not isinstance(v, complex)]
        result = max(real_vals)

        return self._from_python_number(result)

    def _fold_expt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold expt: (expt a b) → a^b"""
        base, exponent = self._to_python_number(args[0]), self._to_python_number(args[1])
        result = base ** exponent
        return self._from_python_number(result)

    def _fold_sin(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold sin: (sin a) → sine"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.sin(val)

        else:
            result = math.sin(val)

        return self._from_python_number(result)

    def _fold_cos(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold cos: (cos a) → cosine"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.cos(val)

        else:
            result = math.cos(val)

        return self._from_python_number(result)

    def _fold_tan(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold tan: (tan a) → tangent"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.tan(val)

        else:
            result = math.tan(val)

        return self._from_python_number(result)

    def _fold_log(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold log: (log a) → natural logarithm"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex) or val <= 0:
            result = cmath.log(val)

        else:
            result = math.log(val)

        return self._from_python_number(result)

    def _fold_log10(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold log10: (log10 a) → base-10 logarithm"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex) or val <= 0:
            result = cmath.log10(val)

        else:
            result = math.log10(val)

        return self._from_python_number(result)

    def _fold_exp(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold exp: (exp a) → e^a"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.exp(val)

        else:
            result = math.exp(val)

        return self._from_python_number(result)

    def _fold_round(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold round: (round a) → rounded integer"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return None  # Can't round complex numbers

        result = round(val)
        return AIFPLASTInteger(int(result))

    def _fold_floor(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold floor: (floor a) → floor integer"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return None  # Can't floor complex numbers

        result = math.floor(val)
        return AIFPLASTInteger(int(result))

    def _fold_ceil(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold ceil: (ceil a) → ceiling integer"""
        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return None  # Can't ceil complex numbers

        result = math.ceil(val)
        return AIFPLASTInteger(int(result))

    # Strict type-specific equality predicates
    def _fold_number_eq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold number=?: all args must be numbers, allows cross-type comparison."""
        # Check all are numbers
        if not all(isinstance(arg, (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex)) for arg in args):
            return None  # Not all numbers, can't fold (will error at runtime)

        # Compare using Python equality (which allows cross-type)
        first = args[0]
        return AIFPLASTBoolean(all(first == arg for arg in args[1:]))

    def _to_python_number(self, value: AIFPLASTNode) -> int | float | complex:
        """Convert AIFPL numeric value to Python number."""
        if isinstance(value, (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex)):
            return value.value

        raise ValueError(f"Expected numeric value, got {type(value).__name__}")

    def _from_python_number(self, value: int | float | complex) -> AIFPLASTNode:
        """Convert Python number to AIFPL value with proper type."""
        if isinstance(value, bool):
            # bool is a subclass of int in Python, handle it first
            return AIFPLASTBoolean(value)

        if isinstance(value, int):
            return AIFPLASTInteger(value)

        if isinstance(value, float):
            return AIFPLASTFloat(value)

        if isinstance(value, complex):
            return AIFPLASTComplex(value)

        raise ValueError(f"Unsupported numeric type: {type(value)}")
