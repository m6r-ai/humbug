"""Main AIFPL (AI Functional Programming Language) class with enhanced error messages."""

from typing import Union
from pathlib import Path
from aifpl.aifpl_value import AIFPLFunction, AIFPLAList
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

    # Class-level cache for prelude functions
    _prelude_functions = None

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
        'find': """(lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) (first lst) (find pred (rest lst)))))""",
        'any?': """(lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) #t (any? pred (rest lst)))))""",
        'all?': """(lambda (pred lst) (if (null? lst) #t (if (pred (first lst)) (all? pred (rest lst)) #f)))""",
    }

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

    @classmethod
    def _load_prelude(cls) -> dict[str, AIFPLFunction]:
        """Load prelude functions from prelude.aifpl file (cached at class level)."""
        # Return cached version if already loaded
        if cls._prelude_functions is not None:
            return cls._prelude_functions

        try:
            # Use evaluator to parse and evaluate each function (creates AIFPLFunction with AST body)
            from aifpl.aifpl_evaluator import AIFPLEvaluator
            evaluator = AIFPLEvaluator()

            # Create environment with constants and builtins
            from aifpl.aifpl_environment import AIFPLEnvironment
            env = AIFPLEnvironment()
            env = env.define_many({**evaluator.CONSTANTS, **evaluator._builtin_functions})

            prelude_funcs = {}
            for name, code in cls._PRELUDE_SOURCE.items():
                tokenizer = AIFPLTokenizer()
                tokens = tokenizer.tokenize(code)
                parser = AIFPLParser(tokens, code)
                expr = parser.parse()

                # Evaluate to get the lambda function
                result = evaluator._evaluate_expression(expr, env, 0)

                if isinstance(result, AIFPLFunction):
                    prelude_funcs[name] = result

            cls._prelude_functions = prelude_funcs
            return prelude_funcs

        except Exception as e:
            import traceback
            traceback.print_exc()
            print(f"Warning: Failed to load prelude: {e}")
            cls._prelude_functions = {}
            return {}

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

        evaluator = AIFPLEvaluator(
            max_depth=self.max_depth,
            floating_point_tolerance=self.floating_point_tolerance
        )

        # Set expression context for error reporting
        evaluator.set_expression_context(expression)

        # Load prelude functions (cached at class level)
        prelude_funcs = self._load_prelude()

        if self.use_bytecode:
            # Compile and execute with VM
            code = self.compiler.compile(parsed_expr)

            # Set up globals (builtins and constants)
            # Only pass constants (pi, e, j, true, false) for variable lookup
            globals_dict = evaluator.CONSTANTS
            self.vm.set_globals(globals_dict, prelude_funcs)

            # Execute
            result = self.vm.execute(code)
        else:
            # Use tree-walking interpreter
            result = evaluator.evaluate(parsed_expr, prelude_funcs)

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

        evaluator = AIFPLEvaluator(
            max_depth=self.max_depth,
            floating_point_tolerance=self.floating_point_tolerance
        )

        # Set expression context for error reporting
        evaluator.set_expression_context(expression)

        if self.use_bytecode:
            # Compile and execute with VM
            code = self.compiler.compile(parsed_expr)

            # Set up globals (builtins and constants)
            globals_dict = evaluator.CONSTANTS
            self.vm.set_globals(globals_dict)

            # Execute
            result = self.vm.execute(code)
        else:
            # Use tree-walking interpreter
            result = evaluator.evaluate(parsed_expr)

        # Simplify and format the result
        simplified = evaluator.simplify_result(result)
        return evaluator.format_result(simplified)
