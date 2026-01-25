"""Main AIFPL (AI Functional Programming Language) class with enhanced error messages."""

import math
from typing import Union, Dict

from aifpl.aifpl_value import AIFPLFunction, AIFPLNumber, AIFPLBoolean, AIFPLValue
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM


class AIFPL:
    """
    AIFPL (AI Functional Programming Language) calculator with LISP-like syntax and enhanced error messages.

    This version provides comprehensive error reporting with:
    - Clear explanations of what went wrong
    - Context showing the problematic input
    - Suggestions for how to fix the problem
    - Examples of correct usage
    - Position information where helpful

    Designed specifically to help LLMs understand and self-correct errors.
    """

    # AIFPL implementations of higher-order functions
    _PRELUDE_SOURCE = {
        'map': """(lambda (f lst)
                    (let ((helper (lambda (f lst acc)
                                    (if (null? lst) (reverse acc)
                                        (helper f (rest lst) (cons (f (first lst)) acc))))))
                    (helper f lst (list))))""",
        'filter': """(lambda (pred lst)
                    (let ((helper (lambda (pred lst acc)
                                    (if (null? lst) (reverse acc)
                                        (if (pred (first lst))
                                            (helper pred (rest lst) (cons (first lst) acc))
                                            (helper pred (rest lst) acc))))))
                        (helper pred lst (list))))""",
        'fold': """(lambda (f init lst)
                    (let ((helper (lambda (f acc lst)
                                    (if (null? lst) acc
                                        (helper f (f acc (first lst)) (rest lst))))))
                    (helper f init lst)))""",
        'find': """(lambda (pred lst)
                    (let ((find (lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) (first lst) (find pred (rest lst)))))))
                    (find pred lst)))""",
        'any?': """(lambda (pred lst)
                    (let ((any? (lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) #t (any? pred (rest lst)))))))
                    (any? pred lst)))""",
        'all?': """(lambda (pred lst)
                    (let ((all? (lambda (pred lst) (if (null? lst) #t (if (pred (first lst)) (all? pred (rest lst)) #f)))))
                    (all? pred lst)))""",
    }

    # Mathematical constants
    CONSTANTS = {
        'pi': AIFPLNumber(math.pi),
        'e': AIFPLNumber(math.e),
        'j': AIFPLNumber(1j),
        'true': AIFPLBoolean(True),
        'false': AIFPLBoolean(False),
    }

    # Class-level caches for prelude functions
    _prelude_evaluator_cache = None  # For evaluator mode
    _prelude_bytecode_cache = None   # For VM mode

    @classmethod
    def _load_prelude_for_evaluator(cls) -> dict[str, AIFPLFunction]:
        """Load prelude as evaluated AIFPLFunction objects (cached)."""
        if cls._prelude_evaluator_cache is not None:
            return cls._prelude_evaluator_cache

        evaluator = AIFPLEvaluator()

        prelude_funcs = {}
        for name, source_code in cls._PRELUDE_SOURCE.items():
            tokenizer = AIFPLTokenizer()
            tokens = tokenizer.tokenize(source_code)
            parser = AIFPLParser(tokens, source_code)
            expr = parser.parse()
            result = evaluator.evaluate(expr, cls.CONSTANTS, None)
            if isinstance(result, AIFPLFunction):
                prelude_funcs[name] = result

        cls._prelude_evaluator_cache = prelude_funcs
        return prelude_funcs

    @classmethod
    def _load_prelude_for_vm(
        cls,
        compiler: AIFPLCompiler,
        vm: AIFPLVM,
        constants: Dict[str, AIFPLValue]
    ) -> Dict[str, AIFPLFunction]:
        """Load prelude as bytecode AIFPLFunction objects (cached)."""
        if cls._prelude_bytecode_cache is not None:
            return cls._prelude_bytecode_cache

        bytecode_prelude: dict[str, AIFPLFunction] = {}
        for name, source_code in cls._PRELUDE_SOURCE.items():
            tokenizer = AIFPLTokenizer()
            tokens = tokenizer.tokenize(source_code)
            parser = AIFPLParser(tokens, source_code)
            expr = parser.parse()
            bytecode = compiler.compile(expr, name=f"<prelude:{name}>")
            vm.set_globals(constants, {})
            func = vm.execute(bytecode)
            if isinstance(func, AIFPLFunction):
                bytecode_prelude[name] = func

        cls._prelude_bytecode_cache = bytecode_prelude
        return bytecode_prelude

    def __init__(self, max_depth: int = 1000, floating_point_tolerance: float = 1e-10, use_bytecode: bool = False):
        """
        Initialize enhanced AIFPL calculator.

        Args:
            max_depth: Maximum recursion depth for expression evaluation
            floating_point_tolerance: Tolerance for floating point comparisons and simplifications
            use_bytecode: If True, use bytecode compiler and VM instead of tree-walking interpreter
        """
        self.max_depth = max_depth
        self.floating_point_tolerance = floating_point_tolerance
        self.use_bytecode = use_bytecode

        # Initialize bytecode components if needed
        if use_bytecode:
            self.compiler = AIFPLCompiler()
            self.vm = AIFPLVM()

    def evaluate(self, expression: str) -> Union[int, float, complex, str, bool, list, AIFPLFunction]:
        """
        Evaluate an AIFPL expression with comprehensive enhanced error reporting.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression converted to Python types

        Raises:
            AIFPLTokenError: If tokenization fails (with detailed context and suggestions)
            AIFPLParseError: If parsing fails (with detailed context and suggestions)
            AIFPLEvalError: If evaluation fails (with detailed context and suggestions)
        """
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize(expression)

        parser = AIFPLParser(tokens, expression)
        parsed_expr = parser.parse()

        if self.use_bytecode:
            # Load prelude and compile main expression
            bytecode_prelude = self._load_prelude_for_vm(self.compiler, self.vm, self.CONSTANTS)
            code = self.compiler.compile(parsed_expr)
            self.vm.set_globals(self.CONSTANTS, bytecode_prelude)
            result = self.vm.execute(code)

            # VM returns AIFPLValue, convert to Python
            return result.to_python()

        evaluator = AIFPLEvaluator(
            max_depth=self.max_depth,
            floating_point_tolerance=self.floating_point_tolerance
        )

        # Set expression context for error reporting
        evaluator.set_expression_context(expression)

        # Load prelude and evaluate
        prelude_funcs = self._load_prelude_for_evaluator()
        result = evaluator.evaluate(parsed_expr, self.CONSTANTS, prelude_funcs)

        # Simplify the result
        simplified = evaluator.simplify_result(result)
        # Convert to Python types for backward compatibility
        return simplified.to_python()

    def evaluate_and_format(self, expression: str) -> str:
        """
        Evaluate an AIFPL expression and return formatted result with comprehensive enhanced error reporting.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            String representation of the result using LISP conventions

        Raises:
            AIFPLTokenError: If tokenization fails (with detailed context and suggestions)
            AIFPLParseError: If parsing fails (with detailed context and suggestions)
            AIFPLEvalError: If evaluation fails (with detailed context and suggestions)
        """
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize(expression)

        parser = AIFPLParser(tokens, expression)
        parsed_expr = parser.parse()

        if self.use_bytecode:
            # Load prelude and compile main expression
            bytecode_prelude = self._load_prelude_for_vm(self.compiler, self.vm, self.CONSTANTS)
            code = self.compiler.compile(parsed_expr)
            self.vm.set_globals(self.CONSTANTS, bytecode_prelude)
            result = self.vm.execute(code)

            # VM returns AIFPLValue, format it
            return self.vm.format_result(result)

        evaluator = AIFPLEvaluator(
            max_depth=self.max_depth,
            floating_point_tolerance=self.floating_point_tolerance
        )

        # Set expression context for error reporting
        evaluator.set_expression_context(expression)

        # Load prelude and evaluate
        prelude_funcs = self._load_prelude_for_evaluator()
        result = evaluator.evaluate(parsed_expr, self.CONSTANTS, prelude_funcs)

        # Simplify and format the result
        simplified = evaluator.simplify_result(result)
        return evaluator.format_result(simplified)
