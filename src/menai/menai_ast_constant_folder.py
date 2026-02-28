"""
Menai AST Optimizer - Optimizes AST before bytecode compilation.

This module provides a framework for AST optimization passes that run after
desugaring but before bytecode compilation. Optimizations transform the AST
while preserving runtime semantics.
"""

import cmath
import math
from typing import List

from menai.menai_ast import (
    MenaiASTNode, MenaiASTInteger, MenaiASTFloat, MenaiASTComplex,
    MenaiASTBoolean, MenaiASTSymbol, MenaiASTList, MenaiASTString
)
from menai.menai_ast_optimization_pass import MenaiASTOptimizationPass


class MenaiASTConstantFolder(MenaiASTOptimizationPass):
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
        'float-log2',
        'float-logn',
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
        'complex-logn',
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
            'float-log2': self._fold_float_log2,
            'float-logn': self._fold_float_logn,
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
            'complex-logn': self._fold_complex_logn,
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
            'if': self._optimize_if,
            'let': self._optimize_let,
            'letrec': self._optimize_let,
            'lambda': self._optimize_lambda,
            'quote': self._optimize_quote,
            'error': self._optimize_error,
        }

    def optimize(self, expr: MenaiASTNode) -> MenaiASTNode:
        """
        Recursively fold constants in expression tree.

        Args:
            expr: Input expression

        Returns:
            Optimized expression (may be same as input if no folding possible)
        """
        # We're only interested in lists.  Anything else we allow to pass through.
        if not isinstance(expr, MenaiASTList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()

        # Check if this is a foldable builtin call
        if isinstance(first, MenaiASTSymbol):
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
        return MenaiASTList(tuple(optimized_elements))

    def _optimize_error(self, expr: MenaiASTList) -> MenaiASTNode:
        """
        Optimize 'error' special form: (error message)

        Currently, we just recursively optimize all elements.
        """
        optimized_elements = [self.optimize(elem) for elem in expr.elements]
        return MenaiASTList(tuple(optimized_elements))

    def _optimize_if(self, expr: MenaiASTList) -> MenaiASTNode:
        """
        Optimize 'if' special form: (if condition then else)

        Can eliminate branches if condition is a constant boolean.
        """
        assert len(expr.elements) == 4  # Earlier semantic analysis should ensure this
        _, condition, then_expr, else_expr = expr.elements

        # Optimize the condition
        opt_condition = self.optimize(condition)

        # If condition is a constant boolean, we can eliminate branches
        if isinstance(opt_condition, MenaiASTBoolean):
            if opt_condition.value:
                # Condition is true, return optimized then branch
                return self.optimize(then_expr)

            # Condition is false, return optimized else branch
            return self.optimize(else_expr)

        # Can't eliminate, but optimize all branches
        opt_then = self.optimize(then_expr)
        opt_else = self.optimize(else_expr)
        return MenaiASTList((expr.elements[0], opt_condition, opt_then, opt_else))

    def _optimize_let(self, expr: MenaiASTList) -> MenaiASTNode:
        """
        Optimize 'let'/'letrec' special form: (let ((var val) ...) body)

        Optimizes binding values and body.
        """
        assert len(expr.elements) == 3  # Earlier semantic analysis should ensure this
        form_symbol, bindings_list, body = expr.elements

        # Optimize binding values
        opt_bindings_list: MenaiASTNode
        assert isinstance(bindings_list, MenaiASTList)
        opt_bindings: List[MenaiASTNode] = []
        for binding in bindings_list.elements:
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
            var, val = binding.elements
            opt_val = self.optimize(val)
            opt_bindings.append(MenaiASTList((var, opt_val)))

        opt_bindings_list = MenaiASTList(tuple(opt_bindings))

        # Optimize body
        opt_body = self.optimize(body)
        return MenaiASTList((form_symbol, opt_bindings_list, opt_body))

    def _optimize_lambda(self, expr: MenaiASTList) -> MenaiASTNode:
        """Optimize 'lambda' special form: (lambda (params) body)"""
        assert len(expr.elements) == 3  # Earlier semantic analysis should ensure this
        lambda_symbol, params, body = expr.elements
        opt_body = self.optimize(body)
        return MenaiASTList((lambda_symbol, params, opt_body))

    def _optimize_quote(self, expr: MenaiASTList) -> MenaiASTNode:
        """Optimize 'quote' special form - quoted expressions are not evaluated."""
        return expr

    def _try_fold_builtin(self, op_name: str, args: List[MenaiASTNode]) -> MenaiASTNode:
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
            if isinstance(opt_arg, MenaiASTSymbol):
                all_constants = False
                continue

            if isinstance(opt_arg, MenaiASTList) and not opt_arg.is_empty() and isinstance(opt_arg.first(), MenaiASTSymbol):
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
        return MenaiASTList((MenaiASTSymbol(op_name),) + tuple(opt_args))

    def _fold_boolean_eq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold boolean=?: all args must be booleans."""
        # Check all are booleans - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, MenaiASTBoolean) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_boolean_neq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """
        Fold inequality: (boolean!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, MenaiASTBoolean) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_not(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold not: (not a) → boolean"""
        if not isinstance(args[0], MenaiASTBoolean):
            return None

        return MenaiASTBoolean(not args[0].value)

    def _fold_integer_eq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer=?: all args must be integers."""
        # Check all are integers - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, MenaiASTInteger) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_integer_neq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """
        Fold inequality: (integer!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, MenaiASTInteger) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_integer_abs(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer-abs: arg must be integer, returns integer."""
        if not isinstance(args[0], MenaiASTInteger):
            return None

        return MenaiASTInteger(abs(args[0].value))

    def _fold_integer_add(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer+: all args must be integers, returns integer."""
        result = 0
        for a in args:
            if not isinstance(a, MenaiASTInteger):
                return None

            result += a.value

        return MenaiASTInteger(result)

    def _fold_integer_sub(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer-: all args must be integers, returns integer."""
        if not isinstance(args[0], MenaiASTInteger):
            return None

        # By the time we reach the folder, desugaring has already reduced this
        # to a binary call, so len(args) == 2 always.  Guard anyway.
        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTInteger):
                return None

            result -= a.value

        return MenaiASTInteger(result)

    def _fold_integer_mul(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer*: all args must be integers, returns integer."""
        result = 1
        for a in args:
            if not isinstance(a, MenaiASTInteger):
                return None

            result *= a.value

        return MenaiASTInteger(result)

    def _fold_integer_div(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer/: all args must be integers, floor division, returns integer."""
        if not isinstance(args[0], MenaiASTInteger):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTInteger):
                return None

            divisor = a.value
            if divisor == 0:
                return None  # Division by zero — let runtime raise the error

            result //= divisor

        return MenaiASTInteger(result)

    def _fold_integer_mod(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer%: all args must be integers, modulo, returns integer."""
        if not isinstance(args[0], MenaiASTInteger):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTInteger):
                return None

            divisor = a.value
            if divisor == 0:
                return None  # Division by zero — let runtime raise the error

            result %= divisor

        return MenaiASTInteger(result)

    def _fold_integer_neg(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer-neg: arg must be integer, returns integer."""
        if not isinstance(args[0], MenaiASTInteger):
            return None

        return MenaiASTInteger(-args[0].value)

    def _fold_integer_expn(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer-expn: both args must be integers, returns integer."""
        if not isinstance(args[0], MenaiASTInteger) or not isinstance(args[1], MenaiASTInteger):
            return None

        arg1 = args[1].value
        if arg1 < 0:
            return None  # Negative exponent - let runtime raise the error

        result = args[0].value ** arg1
        return MenaiASTInteger(result)

    def _fold_integer_bit_or(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold bit-or: (bit-or a b ...) → bitwise OR"""
        first_arg = args[0]
        if not isinstance(first_arg, MenaiASTInteger):
            return None

        result = first_arg.value
        for arg in args[1:]:
            if not isinstance(arg, MenaiASTInteger):
                return None

            result = result | arg.value

        return MenaiASTInteger(result)

    def _fold_integer_bit_and(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold bit-and: (bit-and a b ...) → bitwise AND"""
        first_arg = args[0]
        if not isinstance(first_arg, MenaiASTInteger):
            return None

        result = first_arg.value
        for arg in args[1:]:
            if not isinstance(arg, MenaiASTInteger):
                return None

            result = result & arg.value

        return MenaiASTInteger(result)

    def _fold_integer_bit_xor(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold bit-xor: (bit-xor a b ...) → bitwise XOR"""
        first_arg = args[0]
        if not isinstance(first_arg, MenaiASTInteger):
            return None

        result = first_arg.value
        for arg in args[1:]:
            if not isinstance(arg, MenaiASTInteger):
                return None

            result = result ^ arg.value

        return MenaiASTInteger(result)

    def _fold_integer_bit_not(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold bit-not: (bit-not a) → bitwise NOT"""
        arg = args[0]
        if not isinstance(arg, MenaiASTInteger):
            return None

        result = ~arg.value
        return MenaiASTInteger(result)

    def _fold_integer_bit_shift_left(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold bit-shift-left: (bit-shift-left a b) → a << b"""
        arg0 = args[0]
        if not isinstance(arg0, MenaiASTInteger):
            return None

        arg1 = args[1]
        if not isinstance(arg1, MenaiASTInteger):
            return None

        result = arg0.value << arg1.value
        return MenaiASTInteger(result)

    def _fold_integer_bit_shift_right(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold bit-shift-right: (bit-shift-right a b) → a >> b"""
        arg0 = args[0]
        if not isinstance(arg0, MenaiASTInteger):
            return None

        arg1 = args[1]
        if not isinstance(arg1, MenaiASTInteger):
            return None

        result = arg0.value >> arg1.value
        return MenaiASTInteger(result)

    def _fold_integer_min(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer-min: (integer-min a b) → smaller integer"""
        if not isinstance(args[0], MenaiASTInteger) or not isinstance(args[1], MenaiASTInteger):
            return None

        return MenaiASTInteger(args[0].value if args[0].value <= args[1].value else args[1].value)

    def _fold_integer_max(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer-max: (integer-max a b) → larger integer"""
        if not isinstance(args[0], MenaiASTInteger) or not isinstance(args[1], MenaiASTInteger):
            return None

        return MenaiASTInteger(args[0].value if args[0].value >= args[1].value else args[1].value)

    def _fold_integer_to_complex(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold integer->complex: (integer->complex real [imag]) → complex number"""
        if not isinstance(args[0], MenaiASTInteger):
            return None

        real = args[0].value

        if len(args) == 1:
            imag = 0.0

        else:
            if not isinstance(args[1], MenaiASTInteger):
                return None

            imag = args[1].value

        result = complex(real, imag)
        return MenaiASTComplex(result)

    def _fold_float_eq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float=?: all args must be floats."""
        if not all(isinstance(arg, MenaiASTFloat) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_float_neq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """
        Fold inequality: (float!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, MenaiASTFloat) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_float_abs(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-abs: arg must be float, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(abs(args[0].value))

    def _fold_float_add(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float+: all args must be floats, returns float."""
        result = 0.0
        for a in args:
            if not isinstance(a, MenaiASTFloat):
                return None

            result += a.value

        return MenaiASTFloat(result)

    def _fold_float_sub(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-: all args must be floats, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTFloat):
                return None

            result -= a.value

        return MenaiASTFloat(result)

    def _fold_float_mul(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float*: all args must be floats, returns float."""
        result = 1.0
        for a in args:
            if not isinstance(a, MenaiASTFloat):
                return None

            result *= a.value

        return MenaiASTFloat(result)

    def _fold_float_div(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float/: all args must be floats, true division, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTFloat):
                return None

            divisor = a.value
            if divisor == 0.0:
                return None  # Division by zero — let runtime raise the error

            result /= divisor

        return MenaiASTFloat(result)

    def _fold_float_neg(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-neg: arg must be float, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(-args[0].value)

    def _fold_float_exp(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-exp: arg must be float, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(math.exp(args[0].value))

    def _fold_float_expn(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-expn: all args must be floats, left-associative reduction, returns float."""
        if len(args) < 2:
            return None

        if not isinstance(args[0], MenaiASTFloat):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTFloat):
                return None

            result = result ** a.value

        return MenaiASTFloat(result)

    def _fold_float_log(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-log: arg must be float, returns float. Zero → -inf, negative → don't fold."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        if val == 0.0:
            return MenaiASTFloat(float('-inf'))

        return MenaiASTFloat(math.log(val))

    def _fold_float_log10(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-log10: arg must be float, returns float. Zero → -inf, negative → don't fold."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        if val == 0.0:
            return MenaiASTFloat(float('-inf'))

        return MenaiASTFloat(math.log10(val))

    def _fold_float_log2(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-log2: arg must be float, returns float. Zero → -inf, negative → don't fold."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        if val == 0.0:
            return MenaiASTFloat(float('-inf'))

        return MenaiASTFloat(math.log2(val))

    def _fold_float_logn(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-logn: (float-logn x base) → float. Zero x → -inf, invalid base → don't fold."""
        if not isinstance(args[0], MenaiASTFloat) or not isinstance(args[1], MenaiASTFloat):
            return None

        val = args[0].value
        base = args[1].value

        if base <= 0.0 or base == 1.0:
            return None  # Invalid base is a runtime error — don't fold

        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        if val == 0.0:
            return MenaiASTFloat(float('-inf'))

        return MenaiASTFloat(math.log(val, base))

    def _fold_float_sin(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-sin: arg must be float, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(math.sin(args[0].value))

    def _fold_float_cos(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-cos: arg must be float, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(math.cos(args[0].value))

    def _fold_float_tan(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-tan: arg must be float, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(math.tan(args[0].value))

    def _fold_float_sqrt(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-sqrt: arg must be non-negative float, returns float."""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        val = args[0].value
        if val < 0.0:
            return None  # Negative arg is a runtime error — don't fold

        return MenaiASTFloat(math.sqrt(val))

    def _fold_float_to_complex(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float->complex: (float->complex real [imag]) → complex number"""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        real = args[0].value

        if len(args) == 1:
            imag = 0.0

        else:
            if not isinstance(args[1], MenaiASTFloat):
                return None

            imag = args[1].value

        result = complex(real, imag)
        return MenaiASTComplex(result)

    def _fold_complex_eq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex=?: all args must be complex."""
        # Check all are complex - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, MenaiASTComplex) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_complex_neq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """
        Fold inequality: (complex!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, MenaiASTComplex) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_complex_real(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-real: (complex-real a) → real part"""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        val = args[0].value
        return MenaiASTFloat(val.real)

    def _fold_complex_imag(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-imag: (complex-imag a) → imaginary part"""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        val = args[0].value
        return MenaiASTFloat(val.imag)

    def _fold_complex_abs(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-abs: arg must be complex, returns float (magnitude)."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTFloat(abs(args[0].value))

    def _fold_complex_add(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex+: all args must be complex, returns complex."""
        result = complex(0, 0)
        for a in args:
            if not isinstance(a, MenaiASTComplex):
                return None

            result += a.value

        return MenaiASTComplex(result)

    def _fold_complex_sub(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-: all args must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTComplex):
                return None

            result -= a.value  # type: ignore[union-attr]

        return MenaiASTComplex(result)

    def _fold_complex_mul(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex*: all args must be complex, returns complex."""
        result = complex(1, 0)
        for a in args:
            if not isinstance(a, MenaiASTComplex):
                return None

            result *= a.value  # type: ignore[union-attr]

        return MenaiASTComplex(result)

    def _fold_complex_div(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex/: all args must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTComplex):
                return None

            divisor = a.value
            if divisor == 0j:
                return None  # Division by zero — let runtime raise the error

            result /= divisor

        return MenaiASTComplex(result)

    def _fold_complex_neg(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-neg: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(-args[0].value)

    def _fold_complex_exp(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-exp: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(cmath.exp(args[0].value))

    def _fold_complex_expn(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-expn: all args must be complex, left-associative reduction, returns complex."""
        if len(args) < 2:
            return None

        if not isinstance(args[0], MenaiASTComplex):
            return None

        result = args[0].value
        for a in args[1:]:
            if not isinstance(a, MenaiASTComplex):
                return None

            result = result ** a.value

        return MenaiASTComplex(result)

    def _fold_complex_log(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-log: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(cmath.log(args[0].value))

    def _fold_complex_log10(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-log10: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(cmath.log10(args[0].value))

    def _fold_complex_logn(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-logn: (complex-logn x base) → complex. Zero base → don't fold."""
        if not isinstance(args[0], MenaiASTComplex) or not isinstance(args[1], MenaiASTComplex):
            return None

        base = args[1].value
        if base == 0j:
            return None  # Zero base is a runtime error — don't fold

        return MenaiASTComplex(cmath.log(args[0].value, base))

    def _fold_complex_sin(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-sin: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(cmath.sin(args[0].value))

    def _fold_complex_cos(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-cos: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(cmath.cos(args[0].value))

    def _fold_complex_tan(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-tan: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(cmath.tan(args[0].value))

    def _fold_complex_sqrt(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold complex-sqrt: arg must be complex, returns complex."""
        if not isinstance(args[0], MenaiASTComplex):
            return None

        return MenaiASTComplex(cmath.sqrt(args[0].value))

    def _fold_string_eq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold string=?: all args must be strings."""
        # Check all are strings - if not, can't fold (will error at runtime)
        if not all(isinstance(arg, MenaiASTString) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(all(first == arg for arg in args[1:]))

    def _fold_string_neq(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """
        Fold inequality: (string!= a b c ...) → boolean

        Semantics: "not all arguments are equal" — True if any pair differs.
        This is equivalent to (not (= a b c ...)).
        """
        if not all(isinstance(arg, MenaiASTString) for arg in args):
            return None

        first = args[0]
        return MenaiASTBoolean(not all(first == arg for arg in args[1:]))

    def _fold_float_floor_div(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float// floor division: (float// a b) → float floor quotient"""
        if not isinstance(args[0], MenaiASTFloat) or not isinstance(args[1], MenaiASTFloat):
            return None

        a, b = args[0].value, args[1].value
        if b == 0:
            return None  # Division by zero

        return MenaiASTFloat(float(a // b))

    def _fold_float_mod(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float% modulo: (float% a b) → float remainder"""
        if not isinstance(args[0], MenaiASTFloat) or not isinstance(args[1], MenaiASTFloat):
            return None

        a, b = args[0].value, args[1].value
        if b == 0:
            return None  # Division by zero

        return MenaiASTFloat(a % b)

    def _fold_float_floor(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-floor: (float-floor a) → float floor"""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(float(math.floor(args[0].value)))

    def _fold_float_ceil(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-ceil: (float-ceil a) → float ceiling"""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(float(math.ceil(args[0].value)))

    def _fold_float_round(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-round: (float-round a) → float rounded"""
        if not isinstance(args[0], MenaiASTFloat):
            return None

        return MenaiASTFloat(float(round(args[0].value)))

    def _fold_float_min(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-min: (float-min a b) → smaller float"""
        if not isinstance(args[0], MenaiASTFloat) or not isinstance(args[1], MenaiASTFloat):
            return None

        return MenaiASTFloat(args[0].value if args[0].value <= args[1].value else args[1].value)

    def _fold_float_max(self, args: List[MenaiASTNode]) -> MenaiASTNode | None:
        """Fold float-max: (float-max a b) → larger float"""
        if not isinstance(args[0], MenaiASTFloat) or not isinstance(args[1], MenaiASTFloat):
            return None

        return MenaiASTFloat(args[0].value if args[0].value >= args[1].value else args[1].value)
