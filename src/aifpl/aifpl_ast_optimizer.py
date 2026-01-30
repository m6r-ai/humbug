"""AIFPL AST Optimizer - Optimizes AST before bytecode compilation.

This module provides a framework for AST optimization passes that run after
desugaring but before bytecode compilation. Optimizations transform the AST
while preserving runtime semantics.

Architecture:
    ASTOptimizationPass - Base class for optimization passes
    ConstantFoldingPass - Folds constant expressions at compile time
    ASTOptimizer - Orchestrates multiple optimization passes
"""

from typing import List
import math
import cmath

from aifpl.aifpl_value import (
    AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex,
    AIFPLBoolean, AIFPLSymbol, AIFPLList
)


class ASTOptimizationPass:
    """Base class for AST optimization passes."""

    def optimize(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Transform AST, returning optimized version.

        Args:
            expr: Input AST expression

        Returns:
            Optimized AST expression
        """
        raise NotImplementedError


class ConstantFoldingPass(ASTOptimizationPass):
    """
    Fold constant expressions at compile time.

    This pass evaluates expressions that contain only compile-time constants,
    replacing them with their computed values. This reduces bytecode size and
    improves runtime performance.

    Examples:
        (+ 1 2) → 3
        (* 2 3) → 6
        (+ (* 2 3) (* 4 5)) → 26
        (< 5 10) → #t
        (and #t #t) → #t
    """

    # Builtin operations we can fold
    FOLDABLE_ARITHMETIC = {'+', '-', '*', '/', '//', '%', '**'}
    FOLDABLE_COMPARISON = {'=', '!=', '<', '>', '<=', '>='}
    FOLDABLE_LOGIC = {'and', 'or', 'not'}
    FOLDABLE_MATH = {
        'sqrt', 'abs', 'min', 'max', 'pow',
        'sin', 'cos', 'tan', 'log', 'log10', 'exp',
        'round', 'floor', 'ceil',
        'real', 'imag', 'complex'
    }
    FOLDABLE_BITWISE = {
        'bit-or', 'bit-and', 'bit-xor', 'bit-not',
        'bit-shift-left', 'bit-shift-right'
    }

    def __init__(self) -> None:
        """
        Initialize the jump table for fast builtin operation dispatch.

        This is called once during initialization to build a dictionary mapping
        operation names to their corresponding fold methods. This replaces the
        expensive if-elif chain with O(1) dictionary lookup.
        """
        # Build jump table mapping operation names to fold methods
        self._builtin_jump_table = {
            # Arithmetic operations
            '+': self._fold_add,
            '-': self._fold_subtract,
            '*': self._fold_multiply,
            '/': self._fold_divide,
            '//': self._fold_floor_divide,
            '%': self._fold_modulo,
            '**': self._fold_power,

            # Comparison operations
            '=': self._fold_equal,
            '!=': self._fold_not_equal,
            '<': self._fold_less_than,
            '>': self._fold_greater_than,
            '<=': self._fold_less_equal,
            '>=': self._fold_greater_equal,

            # Boolean logic
            'not': self._fold_not,

            # Math functions
            'sqrt': self._fold_sqrt,
            'abs': self._fold_abs,
            'min': self._fold_min,
            'max': self._fold_max,
            'pow': self._fold_pow,
            'sin': self._fold_sin,
            'cos': self._fold_cos,
            'tan': self._fold_tan,
            'log': self._fold_log,
            'log10': self._fold_log10,
            'exp': self._fold_exp,
            'round': self._fold_round,
            'floor': self._fold_floor,
            'ceil': self._fold_ceil,
            'real': self._fold_real,
            'imag': self._fold_imag,
            'complex': self._fold_complex,

            # Bitwise operations
            'bit-or': self._fold_bit_or,
            'bit-and': self._fold_bit_and,
            'bit-xor': self._fold_bit_xor,
            'bit-not': self._fold_bit_not,
            'bit-shift-left': self._fold_bit_shift_left,
            'bit-shift-right': self._fold_bit_shift_right,
        }

    def optimize(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Recursively fold constants in expression tree.

        Args:
            expr: Input expression

        Returns:
            Optimized expression (may be same as input if no folding possible)
        """
        # Literals are already optimal
        if self._is_constant(expr):
            return expr

        # Symbols (variables) cannot be folded
        if isinstance(expr, AIFPLSymbol):
            return expr

        # Lists may be foldable function calls
        if isinstance(expr, AIFPLList):
            return self._optimize_list(expr)

        # Other types pass through unchanged
        return expr

    def _optimize_list(self, expr: AIFPLList) -> AIFPLValue:
        """Optimize a list expression (potential function call)."""
        if expr.is_empty():
            return expr

        first = expr.first()

        # Check if this is a foldable builtin call
        if isinstance(first, AIFPLSymbol):
            op_name = first.name

            # Special forms should not be folded (if, let, lambda, etc.)
            if op_name in {'if', 'let', 'letrec', 'lambda', 'quote', 'match', 'error'}:
                # Recursively optimize their subexpressions
                return self._optimize_special_form(expr, op_name)

            # Check if it's a foldable builtin
            if op_name in (self.FOLDABLE_ARITHMETIC | self.FOLDABLE_COMPARISON |
                          self.FOLDABLE_LOGIC | self.FOLDABLE_MATH | self.FOLDABLE_BITWISE):
                return self._try_fold_builtin(op_name, list(expr.elements[1:]))

        # Not a foldable call - recursively optimize arguments
        optimized_elements = [self.optimize(elem) for elem in expr.elements]
        return AIFPLList(tuple(optimized_elements))

    def _optimize_special_form(self, expr: AIFPLList, form_name: str) -> AIFPLValue:
        """
        Optimize special forms by recursively optimizing their subexpressions.

        We don't fold special forms themselves, but we can optimize their parts.
        """
        if form_name == 'if':
            # (if condition then else)
            if len(expr.elements) == 4:
                _, condition, then_expr, else_expr = expr.elements

                # Optimize the condition
                opt_condition = self.optimize(condition)

                # If condition is a constant boolean, we can eliminate branches
                if isinstance(opt_condition, AIFPLBoolean):
                    if opt_condition.value:
                        # Condition is true, return optimized then branch
                        return self.optimize(then_expr)

                    # Condition is false, return optimized else branch
                    return self.optimize(else_expr)

                # Can't eliminate, but optimize all branches
                opt_then = self.optimize(then_expr)
                opt_else = self.optimize(else_expr)
                return AIFPLList((expr.elements[0], opt_condition, opt_then, opt_else))

        if form_name in {'let', 'letrec'}:
            # (let ((var val) ...) body)
            if len(expr.elements) == 3:
                form_symbol, bindings_list, body = expr.elements

                # Optimize binding values
                opt_bindings_list: AIFPLValue
                if isinstance(bindings_list, AIFPLList):
                    opt_bindings: List[AIFPLValue] = []
                    for binding in bindings_list.elements:
                        if isinstance(binding, AIFPLList) and len(binding.elements) == 2:
                            var, val = binding.elements
                            opt_val = self.optimize(val)
                            opt_bindings.append(AIFPLList((var, opt_val)))

                        else:
                            opt_bindings.append(binding)

                    opt_bindings_list = AIFPLList(tuple(opt_bindings))

                else:
                    opt_bindings_list = bindings_list

                # Optimize body
                opt_body = self.optimize(body)

                return AIFPLList((form_symbol, opt_bindings_list, opt_body))

        if form_name == 'lambda':
            # (lambda (params) body)
            if len(expr.elements) == 3:
                lambda_symbol, params, body = expr.elements
                # Optimize lambda body
                opt_body = self.optimize(body)
                return AIFPLList((lambda_symbol, params, opt_body))

        if form_name == 'quote':
            # Quoted expressions are not evaluated, don't optimize
            return expr

        # Default: recursively optimize all elements
        optimized_elements = [self.optimize(elem) for elem in expr.elements]
        return AIFPLList(tuple(optimized_elements))

    def _try_fold_builtin(self, op_name: str, args: List[AIFPLValue]) -> AIFPLValue:
        """
        Try to fold a builtin operation.

        Args:
            op_name: Name of the builtin operation
            args: Argument expressions

        Returns:
            Folded constant value, or original expression if folding not possible
        """
        # First, recursively optimize all arguments
        opt_args = [self.optimize(arg) for arg in args]

        # Special handling for short-circuit operators
        if op_name == 'and':
            return self._fold_and(opt_args)

        if op_name == 'or':
            return self._fold_or(opt_args)

        # For other operations, all arguments must be constants
        if not all(self._is_constant(arg) for arg in opt_args):
            # Can't fold - return expression with optimized arguments
            return AIFPLList((AIFPLSymbol(op_name),) + tuple(opt_args))

        # Try to evaluate the builtin
        try:
            fold_func = self._builtin_jump_table.get(op_name)
            if fold_func is not None:
                result = fold_func(opt_args)
                if result is not None:
                    return result

        except Exception:
            # Evaluation failed - preserve runtime error by not folding
            pass

        # Couldn't fold - return expression with optimized arguments
        return AIFPLList((AIFPLSymbol(op_name),) + tuple(opt_args))

    def _fold_and(self, args: List[AIFPLValue]) -> AIFPLValue:
        """
        Fold 'and' with short-circuit evaluation.

        (and) → #t
        (and #f anything) → #f (short-circuit)
        (and #t #t #t) → #t
        """
        if len(args) == 0:
            return AIFPLBoolean(True)

        # Check each argument
        folded_args = []
        for arg in args:
            # If we hit a false constant, short-circuit
            if isinstance(arg, AIFPLBoolean) and not arg.value:
                return AIFPLBoolean(False)

            # If it's a true constant, we can skip it
            if isinstance(arg, AIFPLBoolean) and arg.value:
                continue

            # Non-constant argument - can't fully fold
            folded_args.append(arg)

        # If all args were true constants, result is true
        if len(folded_args) == 0:
            return AIFPLBoolean(True)

        # If only one arg remains, return it
        if len(folded_args) == 1:
            return AIFPLList((AIFPLSymbol('and'), folded_args[0]))

        # Multiple non-constant args remain
        return AIFPLList((AIFPLSymbol('and'),) + tuple(folded_args))

    def _fold_or(self, args: List[AIFPLValue]) -> AIFPLValue:
        """
        Fold 'or' with short-circuit evaluation.

        (or) → #f
        (or #t anything) → #t (short-circuit)
        (or #f #f #f) → #f
        """
        if len(args) == 0:
            return AIFPLBoolean(False)

        # Check each argument
        folded_args = []
        for arg in args:
            # If we hit a true constant, short-circuit
            if isinstance(arg, AIFPLBoolean) and arg.value:
                return AIFPLBoolean(True)

            # If it's a false constant, we can skip it
            if isinstance(arg, AIFPLBoolean) and not arg.value:
                continue

            # Non-constant argument - can't fully fold
            folded_args.append(arg)

        # If all args were false constants, result is false
        if len(folded_args) == 0:
            return AIFPLBoolean(False)

        # If only one arg remains, return it
        if len(folded_args) == 1:
            return AIFPLList((AIFPLSymbol('or'), folded_args[0]))

        # Multiple non-constant args remain
        return AIFPLList((AIFPLSymbol('or'),) + tuple(folded_args))

    def _is_constant(self, expr: AIFPLValue) -> bool:
        """
        Check if expression is a compile-time constant.

        Constants are:
        - Numeric literals (integer, float, complex)
        - String literals
        - Boolean literals
        - Empty lists
        - Lists where all elements are constants

        NOT constants:
        - Symbols (variables)
        - Function calls (even with constant args - we fold those separately)
        """
        if isinstance(expr, AIFPLSymbol):
            return False

        # Lists with all constant elements are constants (for quote, etc.)
        # But we don't consider function calls as constants here
        if isinstance(expr, AIFPLList):
            # Empty list is a constant
            if expr.is_empty():
                return True

            # If first element is a symbol, it's a function call, not a constant
            if isinstance(expr.first(), AIFPLSymbol):
                return False

            # Otherwise check if all elements are constants
            return all(self._is_constant(elem) for elem in expr.elements)

        # Other types are constants
        return True

    def _fold_add(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold addition: (+ a b c ...) → sum"""
        if len(args) == 0:
            return AIFPLInteger(0)

        result = self._to_python_number(args[0])
        for arg in args[1:]:
            result = result + self._to_python_number(arg)

        return self._from_python_number(result)

    def _fold_subtract(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold subtraction: (- a b c ...) → difference"""
        if len(args) == 0:
            return None

        if len(args) == 1:
            # Unary negation
            result = -self._to_python_number(args[0])
            return self._from_python_number(result)

        result = self._to_python_number(args[0])
        for arg in args[1:]:
            result = result - self._to_python_number(arg)

        return self._from_python_number(result)

    def _fold_multiply(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold multiplication: (* a b c ...) → product"""
        if len(args) == 0:
            return AIFPLInteger(1)

        result = self._to_python_number(args[0])
        for arg in args[1:]:
            result = result * self._to_python_number(arg)

        return self._from_python_number(result)

    def _fold_divide(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold division: (/ a b c ...) → quotient"""
        if len(args) < 2:
            return None

        result = self._to_python_number(args[0])
        for arg in args[1:]:
            divisor = self._to_python_number(arg)
            if divisor == 0:
                # Division by zero - don't fold, let runtime handle it
                return None

            result = result / divisor

        return self._from_python_number(result)

    def _fold_floor_divide(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold floor division: (// a b) → floor quotient"""
        if len(args) != 2:
            return None

        a = self._to_python_number(args[0])
        b = self._to_python_number(args[1])

        if b == 0:
            return None  # Division by zero

        # Floor division only works on real numbers
        if isinstance(a, complex) or isinstance(b, complex):
            return None

        result = a // b
        return self._from_python_number(result)

    def _fold_modulo(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold modulo: (% a b) → remainder"""
        if len(args) != 2:
            return None

        a = self._to_python_number(args[0])
        b = self._to_python_number(args[1])

        if b == 0:
            return None  # Division by zero

        # Modulo only works on real numbers
        if isinstance(a, complex) or isinstance(b, complex):
            return None

        result = a % b
        return self._from_python_number(result)

    def _fold_power(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold power: (** a b) → a^b"""
        if len(args) != 2:
            return None

        base = self._to_python_number(args[0])
        exponent = self._to_python_number(args[1])

        result = base ** exponent
        return self._from_python_number(result)

    # Comparison operations

    def _fold_equal(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold equality: (= a b c ...) → boolean"""
        if len(args) < 2:
            return None

        # Check if all arguments are equal
        first_val = self._to_python_number(args[0])
        for arg in args[1:]:
            if self._to_python_number(arg) != first_val:
                return AIFPLBoolean(False)

        return AIFPLBoolean(True)

    def _fold_not_equal(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold inequality: (!= a b) → boolean"""
        if len(args) != 2:
            return None

        a = self._to_python_number(args[0])
        b = self._to_python_number(args[1])

        return AIFPLBoolean(a != b)

    def _fold_less_than(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold less than: (< a b c ...) → boolean"""
        if len(args) < 2:
            return None

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
                return AIFPLBoolean(False)

            prev = curr

        return AIFPLBoolean(True)

    def _fold_greater_than(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold greater than: (> a b c ...) → boolean"""
        if len(args) < 2:
            return None

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
                return AIFPLBoolean(False)

            prev = curr

        return AIFPLBoolean(True)

    def _fold_less_equal(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold less than or equal: (<= a b c ...) → boolean"""
        if len(args) < 2:
            return None

        prev = self._to_python_number(args[0])

        # Complex numbers don't support ordering - can't fold
        if isinstance(prev, complex):
            return None

        for arg in args[1:]:
            curr = self._to_python_number(arg)
            if isinstance(curr, complex):
                return None

            if prev > curr:
                return AIFPLBoolean(False)

            prev = curr

        return AIFPLBoolean(True)

    def _fold_greater_equal(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold greater than or equal: (>= a b c ...) → boolean"""
        if len(args) < 2:
            return None

        prev = self._to_python_number(args[0])

        # Complex numbers don't support ordering - can't fold
        if isinstance(prev, complex):
            return None

        for arg in args[1:]:
            curr = self._to_python_number(arg)
            if isinstance(curr, complex):
                return None

            if prev < curr:
                return AIFPLBoolean(False)

            prev = curr

        return AIFPLBoolean(True)

    def _fold_not(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold not: (not a) → boolean"""
        if len(args) != 1:
            return None

        if not isinstance(args[0], AIFPLBoolean):
            return None

        return AIFPLBoolean(not args[0].value)

    def _fold_sqrt(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold sqrt: (sqrt a) → square root"""
        if len(args) != 1:
            return None

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

    def _fold_abs(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold abs: (abs a) → absolute value"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])
        result = abs(val)

        return self._from_python_number(result)

    def _fold_min(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold min: (min a b c ...) → minimum"""
        if len(args) == 0:
            return None

        vals = [self._to_python_number(arg) for arg in args]

        # min/max don't work with complex numbers - can't fold
        if any(isinstance(v, complex) for v in vals):
            return None

        # Type narrowing: we've excluded complex, so only int | float remain
        # Cast to help mypy understand this
        real_vals = [v for v in vals if not isinstance(v, complex)]
        result = min(real_vals)

        return self._from_python_number(result)

    def _fold_max(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold max: (max a b c ...) → maximum"""
        if len(args) == 0:
            return None

        vals = [self._to_python_number(arg) for arg in args]

        # min/max don't work with complex numbers - can't fold
        if any(isinstance(v, complex) for v in vals):
            return None

        # Type narrowing: we've excluded complex, so only int | float remain
        # Cast to help mypy understand this
        real_vals = [v for v in vals if not isinstance(v, complex)]
        result = max(real_vals)

        return self._from_python_number(result)

    def _fold_pow(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold pow: (pow a b) → a^b"""
        if len(args) != 2:
            return None

        base = self._to_python_number(args[0])
        exponent = self._to_python_number(args[1])

        result = base ** exponent
        return self._from_python_number(result)

    def _fold_sin(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold sin: (sin a) → sine"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.sin(val)

        else:
            result = math.sin(val)

        return self._from_python_number(result)

    def _fold_cos(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold cos: (cos a) → cosine"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.cos(val)

        else:
            result = math.cos(val)

        return self._from_python_number(result)

    def _fold_tan(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold tan: (tan a) → tangent"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.tan(val)

        else:
            result = math.tan(val)

        return self._from_python_number(result)

    def _fold_log(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold log: (log a) → natural logarithm"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex) or val <= 0:
            result = cmath.log(val)

        else:
            result = math.log(val)

        return self._from_python_number(result)

    def _fold_log10(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold log10: (log10 a) → base-10 logarithm"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex) or val <= 0:
            result = cmath.log10(val)

        else:
            result = math.log10(val)

        return self._from_python_number(result)

    def _fold_exp(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold exp: (exp a) → e^a"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            result = cmath.exp(val)

        else:
            result = math.exp(val)

        return self._from_python_number(result)

    def _fold_round(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold round: (round a) → rounded integer"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return None  # Can't round complex numbers

        result = round(val)
        return AIFPLInteger(int(result))

    def _fold_floor(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold floor: (floor a) → floor integer"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return None  # Can't floor complex numbers

        result = math.floor(val)
        return AIFPLInteger(int(result))

    def _fold_ceil(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold ceil: (ceil a) → ceiling integer"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return None  # Can't ceil complex numbers

        result = math.ceil(val)
        return AIFPLInteger(int(result))

    def _fold_real(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold real: (real a) → real part"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return AIFPLFloat(val.real)

        # Convert to float to match runtime behavior
        return AIFPLFloat(float(val))

    def _fold_imag(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold imag: (imag a) → imaginary part"""
        if len(args) != 1:
            return None

        val = self._to_python_number(args[0])

        if isinstance(val, complex):
            return AIFPLFloat(val.imag)

        # Real numbers have 0.0 imaginary part (float, not int)
        return AIFPLFloat(0.0)

    def _fold_complex(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold complex: (complex real imag) → complex number"""
        if len(args) != 2:
            return None

        real = self._to_python_number(args[0])
        imag = self._to_python_number(args[1])

        # Don't fold if arguments are complex - runtime will raise error
        if isinstance(real, complex):
            return None

        if isinstance(imag, complex):
            return None

        result = complex(real, imag)
        return AIFPLComplex(result)

    def _fold_bit_or(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold bit-or: (bit-or a b ...) → bitwise OR"""
        if len(args) < 2:
            return None

        # All args must be integers
        if not all(isinstance(arg, AIFPLInteger) for arg in args):
            return None

        # Extract first arg with type narrowing
        first_arg = args[0]
        assert isinstance(first_arg, AIFPLInteger)
        result = first_arg.value
        for arg in args[1:]:
            # Type already checked above, but help mypy
            assert isinstance(arg, AIFPLInteger)
            result = result | arg.value

        return AIFPLInteger(result)

    def _fold_bit_and(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold bit-and: (bit-and a b ...) → bitwise AND"""
        if len(args) < 2:
            return None

        if not all(isinstance(arg, AIFPLInteger) for arg in args):
            return None

        # Extract first arg with type narrowing
        first_arg = args[0]
        assert isinstance(first_arg, AIFPLInteger)
        result = first_arg.value
        for arg in args[1:]:
            assert isinstance(arg, AIFPLInteger)
            result = result & arg.value

        return AIFPLInteger(result)

    def _fold_bit_xor(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold bit-xor: (bit-xor a b ...) → bitwise XOR"""
        if len(args) < 2:
            return None

        if not all(isinstance(arg, AIFPLInteger) for arg in args):
            return None

        # Extract first arg with type narrowing
        first_arg = args[0]
        assert isinstance(first_arg, AIFPLInteger)
        result = first_arg.value
        for arg in args[1:]:
            assert isinstance(arg, AIFPLInteger)
            result = result ^ arg.value

        return AIFPLInteger(result)

    def _fold_bit_not(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold bit-not: (bit-not a) → bitwise NOT"""
        if len(args) != 1:
            return None

        arg = args[0]
        if not isinstance(arg, AIFPLInteger):
            return None

        # Type narrowed by isinstance check above
        result = ~arg.value
        return AIFPLInteger(result)

    def _fold_bit_shift_left(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold bit-shift-left: (bit-shift-left a b) → a << b"""
        if len(args) != 2:
            return None

        if not all(isinstance(arg, AIFPLInteger) for arg in args):
            return None

        # Type narrowing for mypy
        arg0 = args[0]
        arg1 = args[1]
        assert isinstance(arg0, AIFPLInteger) and isinstance(arg1, AIFPLInteger)
        result = arg0.value << arg1.value
        return AIFPLInteger(result)

    def _fold_bit_shift_right(self, args: List[AIFPLValue]) -> AIFPLValue | None:
        """Fold bit-shift-right: (bit-shift-right a b) → a >> b"""
        if len(args) != 2:
            return None

        if not all(isinstance(arg, AIFPLInteger) for arg in args):
            return None

        # Type narrowing for mypy
        arg0 = args[0]
        arg1 = args[1]
        assert isinstance(arg0, AIFPLInteger) and isinstance(arg1, AIFPLInteger)
        result = arg0.value >> arg1.value
        return AIFPLInteger(result)

    def _to_python_number(self, value: AIFPLValue) -> int | float | complex:
        """Convert AIFPL numeric value to Python number."""
        if isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            return value.value

        raise ValueError(f"Expected numeric value, got {type(value).__name__}")

    def _from_python_number(self, value: int | float | complex) -> AIFPLValue:
        """Convert Python number to AIFPL value with proper type."""
        if isinstance(value, bool):
            # bool is a subclass of int in Python, handle it first
            return AIFPLBoolean(value)

        if isinstance(value, int):
            return AIFPLInteger(value)

        if isinstance(value, float):
            return AIFPLFloat(value)

        if isinstance(value, complex):
            return AIFPLComplex(value)

        raise ValueError(f"Unsupported numeric type: {type(value)}")


class ASTOptimizer:
    """
    Orchestrates AST optimization passes.

    This class manages multiple optimization passes and applies them in sequence
    to transform the AST before bytecode compilation.
    """

    def __init__(self, enable_passes: List[str] | None = None):
        """
        Initialize with optional pass selection.

        Args:
            enable_passes: List of pass names to enable, or None for all passes
        """
        all_passes = {
            'constant_folding': ConstantFoldingPass(),
            # Future passes can be added here:
            # 'dead_code_elimination': DeadCodeEliminationPass(),
            # 'common_subexpression_elimination': CSEPass(),
        }

        if enable_passes is None:
            # Enable all passes by default
            self.passes = list(all_passes.values())

        else:
            # Enable only specified passes
            self.passes = [all_passes[name] for name in enable_passes if name in all_passes]

    def optimize(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Run all enabled optimization passes in sequence.

        Args:
            expr: Input AST expression

        Returns:
            Optimized AST expression
        """
        optimized = expr
        for pass_instance in self.passes:
            optimized = pass_instance.optimize(optimized)

        return optimized
