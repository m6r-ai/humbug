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
        'boolean=?',
        'boolean-not',
        'integer=?',
        'integer-abs',
        'integer+',
        'integer-',
        'integer*',
        'integer/',
        'integer%',
        'integer-neg',
        'integer-expn',
        'integer-bit-or',
        'integer-bit-and',
        'integer-bit-xor',
        'integer-bit-not',
        'integer-bit-shift-left',
        'integer-bit-shift-right',
        'integer-min',
        'integer-max',
        'integer->complex',
        'float=?',
        'float-abs',
        'float+',
        'float-',
        'float*',
        'float/',
        'float//',
        'float%',
        'float-neg',
        'float-exp',
        'float-expn',
        'float-log',
        'float-log10',
        'float-sin',
        'float-cos',
        'float-tan',
        'float-sqrt',
        'float-floor',
        'float-ceil',
        'float-round',
        'float-min',
        'float-max',
        'float->complex',
        'complex=?',
        'complex-real',
        'complex-imag',
        'complex-abs',
        'complex+',
        'complex-',
        'complex*',
        'complex/',
        'complex-neg',
        'complex-exp',
        'complex-expn',
        'complex-log',
        'complex-log10',
        'complex-sin',
        'complex-cos',
        'complex-tan',
        'complex-sqrt',
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
            'boolean-not': self._fold_not,
            'integer=?': self._fold_integer_eq,
            'integer!=?': self._fold_integer_neq,
            'integer-abs': self._fold_integer_abs,
            'integer+': self._fold_integer_add,
            'integer-': self._fold_integer_sub,
            'integer*': self._fold_integer_mul,
            'integer/': self._fold_integer_div,
            'integer%': self._fold_integer_mod,
            'integer-neg': self._fold_integer_neg,
            'integer-expn': self._fold_integer_expn,
            'integer-bit-or': self._fold_integer_bit_or,
            'integer-bit-and': self._fold_integer_bit_and,
            'integer-bit-xor': self._fold_integer_bit_xor,
            'integer-bit-not': self._fold_integer_bit_not,
            'integer-bit-shift-left': self._fold_integer_bit_shift_left,
            'integer-bit-shift-right': self._fold_integer_bit_shift_right,
            'integer-min': self._fold_integer_min,
            'integer-max': self._fold_integer_max,
            'integer->complex': self._fold_integer_to_complex,
            'float=?': self._fold_float_eq,
            'float!=?': self._fold_float_neq,
            'float-abs': self._fold_float_abs,
            'float+': self._fold_float_add,
            'float-': self._fold_float_sub,
            'float*': self._fold_float_mul,
            'float/': self._fold_float_div,
            'float//': self._fold_float_floor_div,
            'float%': self._fold_float_mod,
            'float-neg': self._fold_float_neg,
            'float-exp': self._fold_float_exp,
            'float-expn': self._fold_float_expn,
            'float-log': self._fold_float_log,
            'float-log10': self._fold_float_log10,
            'float-sin': self._fold_float_sin,
            'float-cos': self._fold_float_cos,
            'float-tan': self._fold_float_tan,
            'float-sqrt': self._fold_float_sqrt,
            'float-floor': self._fold_float_floor,
            'float-ceil': self._fold_float_ceil,
            'float-round': self._fold_float_round,
            'float-min': self._fold_float_min,
            'float-max': self._fold_float_max,
            'float->complex': self._fold_float_to_complex,
            'complex=?': self._fold_complex_eq,
            'complex!=?': self._fold_complex_neq,
            'complex-abs': self._fold_complex_abs,
            'complex+': self._fold_complex_add,
            'complex-': self._fold_complex_sub,
            'complex*': self._fold_complex_mul,
            'complex/': self._fold_complex_div,
            'complex-real': self._fold_complex_real,
            'complex-imag': self._fold_complex_imag,
            'complex-neg': self._fold_complex_neg,
            'complex-exp': self._fold_complex_exp,
            'complex-expn': self._fold_complex_expn,
            'complex-log': self._fold_complex_log,
            'complex-log10': self._fold_complex_log10,
            'complex-sin': self._fold_complex_sin,
            'complex-cos': self._fold_complex_cos,
            'complex-tan': self._fold_complex_tan,
            'complex-sqrt': self._fold_complex_sqrt,
            'string=?': self._fold_string_eq,
            'string!=?': self._fold_string_neq,
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

    def _fold_integer_abs(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-abs: arg must be integer, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        return AIFPLASTInteger(abs(args[0].value))

    def _fold_integer_add(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer+: all args must be integers, returns integer."""
        result = 0
        for a in args:
            if not isinstance(a, AIFPLASTInteger):
                return None

            result += a.value

        return AIFPLASTInteger(result)

    def _fold_integer_sub(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-: all args must be integers, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        # By the time we reach the folder, desugaring has already reduced this
        # to a binary call, so len(args) == 2 always.  Guard anyway.
        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, AIFPLASTInteger):
                return None

            result -= a.value

        return AIFPLASTInteger(result)

    def _fold_integer_mul(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer*: all args must be integers, returns integer."""
        result = 1
        for a in args:
            if not isinstance(a, AIFPLASTInteger):
                return None

            result *= a.value

        return AIFPLASTInteger(result)

    def _fold_integer_div(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer/: all args must be integers, floor division, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, AIFPLASTInteger):
                return None

            divisor = a.value
            if divisor == 0:
                return None  # Division by zero — let runtime raise the error

            result //= divisor

        return AIFPLASTInteger(result)

    def _fold_integer_mod(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer%: all args must be integers, modulo, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, AIFPLASTInteger):
                return None

            divisor = a.value
            if divisor == 0:
                return None  # Division by zero — let runtime raise the error

            result %= divisor

        return AIFPLASTInteger(result)

    def _fold_integer_neg(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-neg: arg must be integer, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        return AIFPLASTInteger(-args[0].value)

    def _fold_integer_expn(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-expn: both args must be integers, returns integer."""
        if not isinstance(args[0], AIFPLASTInteger) or not isinstance(args[1], AIFPLASTInteger):
            return None

        arg1 = args[1].value
        if arg1 < 0:
            return None  # Negative exponent - let runtime raise the error

        result = args[0].value ** arg1
        return AIFPLASTInteger(result)

    def _fold_integer_bit_or(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
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

    def _fold_integer_bit_and(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
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

    def _fold_integer_bit_xor(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
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

    def _fold_integer_bit_not(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-not: (bit-not a) → bitwise NOT"""
        arg = args[0]
        if not isinstance(arg, AIFPLASTInteger):
            return None

        result = ~arg.value
        return AIFPLASTInteger(result)

    def _fold_integer_bit_shift_left(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-shift-left: (bit-shift-left a b) → a << b"""
        arg0 = args[0]
        if not isinstance(arg0, AIFPLASTInteger):
            return None

        arg1 = args[1]
        if not isinstance(arg1, AIFPLASTInteger):
            return None

        result = arg0.value << arg1.value
        return AIFPLASTInteger(result)

    def _fold_integer_bit_shift_right(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold bit-shift-right: (bit-shift-right a b) → a >> b"""
        arg0 = args[0]
        if not isinstance(arg0, AIFPLASTInteger):
            return None

        arg1 = args[1]
        if not isinstance(arg1, AIFPLASTInteger):
            return None

        result = arg0.value >> arg1.value
        return AIFPLASTInteger(result)

    def _fold_integer_min(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-min: (integer-min a b) → smaller integer"""
        if not isinstance(args[0], AIFPLASTInteger) or not isinstance(args[1], AIFPLASTInteger):
            return None

        return AIFPLASTInteger(args[0].value if args[0].value <= args[1].value else args[1].value)

    def _fold_integer_max(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer-max: (integer-max a b) → larger integer"""
        if not isinstance(args[0], AIFPLASTInteger) or not isinstance(args[1], AIFPLASTInteger):
            return None

        return AIFPLASTInteger(args[0].value if args[0].value >= args[1].value else args[1].value)

    def _fold_integer_to_complex(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold integer->complex: (integer->complex real [imag]) → complex number"""
        if not isinstance(args[0], AIFPLASTInteger):
            return None

        real = args[0].value

        if len(args) == 1:
            imag = 0.0

        else:
            if not isinstance(args[1], AIFPLASTInteger):
                return None

            imag = args[1].value

        result = complex(real, imag)
        return AIFPLASTComplex(result)

    def _fold_float_eq(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float=?: all args must be floats."""
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

    def _fold_float_abs(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-abs: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(abs(args[0].value))

    def _fold_float_add(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float+: all args must be floats, returns float."""
        result = 0.0
        for a in args:
            if not isinstance(a, AIFPLASTFloat):
                return None

            result += a.value

        return AIFPLASTFloat(result)

    def _fold_float_sub(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-: all args must be floats, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, AIFPLASTFloat):
                return None

            result -= a.value

        return AIFPLASTFloat(result)

    def _fold_float_mul(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float*: all args must be floats, returns float."""
        result = 1.0
        for a in args:
            if not isinstance(a, AIFPLASTFloat):
                return None

            result *= a.value

        return AIFPLASTFloat(result)

    def _fold_float_div(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float/: all args must be floats, true division, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, AIFPLASTFloat):
                return None

            divisor = a.value
            if divisor == 0.0:
                return None  # Division by zero — let runtime raise the error

            result /= divisor

        return AIFPLASTFloat(result)

    def _fold_float_neg(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-neg: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(-args[0].value)

    def _fold_float_exp(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-exp: arg must be float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(math.exp(args[0].value))

    def _fold_float_expn(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-expn: both args must be floats, returns float."""
        if not isinstance(args[0], AIFPLASTFloat) or not isinstance(args[1], AIFPLASTFloat):
            return None

        result = args[0].value ** args[1].value
        return AIFPLASTFloat(result)

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

    def _fold_float_sqrt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-sqrt: arg must be non-negative float, returns float."""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        return AIFPLASTFloat(math.sqrt(val))

    def _fold_float_to_complex(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float->complex: (float->complex real [imag]) → complex number"""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        real = args[0].value

        if len(args) == 1:
            imag = 0.0

        else:
            if not isinstance(args[1], AIFPLASTFloat):
                return None

            imag = args[1].value

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

    def _fold_complex_real(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-real: (complex-real a) → real part"""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        val = args[0].value
        return AIFPLASTFloat(val.real)

    def _fold_complex_imag(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-imag: (complex-imag a) → imaginary part"""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        val = args[0].value
        return AIFPLASTFloat(val.imag)

    def _fold_complex_abs(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-abs: arg must be complex, returns float (magnitude)."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTFloat(abs(args[0].value))

    def _fold_complex_add(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex+: all args must be complex, returns complex."""
        result = complex(0, 0)
        for a in args:
            if not isinstance(a, AIFPLASTComplex):
                return None

            result += a.value

        return AIFPLASTComplex(result)

    def _fold_complex_sub(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-: all args must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, AIFPLASTComplex):
                return None

            result -= a.value  # type: ignore[union-attr]

        return AIFPLASTComplex(result)

    def _fold_complex_mul(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex*: all args must be complex, returns complex."""
        result = complex(1, 0)
        for a in args:
            if not isinstance(a, AIFPLASTComplex):
                return None

            result *= a.value  # type: ignore[union-attr]

        return AIFPLASTComplex(result)

    def _fold_complex_div(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex/: all args must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, AIFPLASTComplex):
                return None

            divisor = a.value
            if divisor == 0j:
                return None  # Division by zero — let runtime raise the error

            result /= divisor

        return AIFPLASTComplex(result)

    def _fold_complex_neg(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-neg: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(-args[0].value)

    def _fold_complex_exp(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-exp: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.exp(args[0].value))

    def _fold_complex_expn(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-expn: both args must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex) or not isinstance(args[1], AIFPLASTComplex):
            return None

        result = args[0].value ** args[1].value
        return AIFPLASTComplex(result)

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

    def _fold_complex_sqrt(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold complex-sqrt: arg must be complex, returns complex."""
        if not isinstance(args[0], AIFPLASTComplex):
            return None

        return AIFPLASTComplex(cmath.sqrt(args[0].value))

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

    def _fold_float_floor_div(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float// floor division: (float// a b) → float floor quotient"""
        if not isinstance(args[0], AIFPLASTFloat) or not isinstance(args[1], AIFPLASTFloat):
            return None

        a, b = args[0].value, args[1].value
        if b == 0:
            return None  # Division by zero

        return AIFPLASTFloat(float(a // b))

    def _fold_float_mod(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float% modulo: (float% a b) → float remainder"""
        if not isinstance(args[0], AIFPLASTFloat) or not isinstance(args[1], AIFPLASTFloat):
            return None

        a, b = args[0].value, args[1].value
        if b == 0:
            return None  # Division by zero

        return AIFPLASTFloat(a % b)

    def _fold_float_floor(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-floor: (float-floor a) → float floor"""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(float(math.floor(args[0].value)))

    def _fold_float_ceil(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-ceil: (float-ceil a) → float ceiling"""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(float(math.ceil(args[0].value)))

    def _fold_float_round(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-round: (float-round a) → float rounded"""
        if not isinstance(args[0], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(float(round(args[0].value)))

    def _fold_float_min(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-min: (float-min a b) → smaller float"""
        if not isinstance(args[0], AIFPLASTFloat) or not isinstance(args[1], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(args[0].value if args[0].value <= args[1].value else args[1].value)

    def _fold_float_max(self, args: List[AIFPLASTNode]) -> AIFPLASTNode | None:
        """Fold float-max: (float-max a b) → larger float"""
        if not isinstance(args[0], AIFPLASTFloat) or not isinstance(args[1], AIFPLASTFloat):
            return None

        return AIFPLASTFloat(args[0].value if args[0].value >= args[1].value else args[1].value)
