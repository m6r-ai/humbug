"""Main AIFPL (AI Functional Programming Language) class with enhanced error messages."""

import math
from typing import Union, Dict

from aifpl.aifpl_value import AIFPLFunction, AIFPLFloat, AIFPLBoolean, AIFPLValue
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer
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

    Execution Model:
    - Uses bytecode compiler and VM for all evaluation
    - Tail-call optimized for recursive functions
    - High performance through bytecode compilation and optimized VM
    """

    # AIFPL implementations of higher-order functions
    _PRELUDE_SOURCE = {
        'map': """(lambda (f lst)
                    (letrec ((helper (lambda (f lst acc)
                                       (if (null? lst) (reverse acc)
                                           (helper f (rest lst) (cons (f (first lst)) acc))))))
                    (helper f lst (list))))""",
        'filter': """(lambda (pred lst)
                    (letrec ((helper (lambda (pred lst acc)
                                       (if (null? lst) (reverse acc)
                                           (if (pred (first lst))
                                               (helper pred (rest lst) (cons (first lst) acc))
                                               (helper pred (rest lst) acc))))))
                        (helper pred lst (list))))""",
        'fold': """(lambda (f init lst)
                    (letrec ((helper (lambda (f acc lst)
                                       (if (null? lst) acc
                                           (helper f (f acc (first lst)) (rest lst))))))
                    (helper f init lst)))""",
        'find': """(lambda (pred lst)
                    (letrec ((find (lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) (first lst) (find pred (rest lst)))))))
                    (find pred lst)))""",
        'any?': """(lambda (pred lst)
                    (letrec ((any? (lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) #t (any? pred (rest lst)))))))
                    (any? pred lst)))""",
        'all?': """(lambda (pred lst)
                    (letrec ((all? (lambda (pred lst) (if (null? lst) #t (if (pred (first lst)) (all? pred (rest lst)) #f)))))
                    (all? pred lst)))""",
    }

    # Mathematical constants
    CONSTANTS = {
        'pi': AIFPLFloat(math.pi),
        'e': AIFPLFloat(math.e),
        'true': AIFPLBoolean(True),
        'false': AIFPLBoolean(False),
    }

    # Class-level cache for prelude functions
    _prelude_cache = None

    @classmethod
    def _load_prelude(
        cls,
        compiler: AIFPLCompiler,
        vm: AIFPLVM
    ) -> Dict[str, AIFPLFunction]:
        """Load prelude as bytecode AIFPLFunction objects (cached)."""
        if cls._prelude_cache is not None:
            return cls._prelude_cache

        bytecode_prelude: dict[str, AIFPLFunction] = {}
        for name, source_code in cls._PRELUDE_SOURCE.items():
            lexer = AIFPLLexer()
            tokens = lexer.lex(source_code)
            parser = AIFPLParser(tokens, source_code)
            expr = parser.parse()
            # Analyze semantics before compilation
            analyzer = AIFPLSemanticAnalyzer()
            expr = analyzer.analyze(expr)
            bytecode = compiler.compile(expr, name=f"<prelude:{name}>")
            func = vm.execute(bytecode, cls.CONSTANTS, {})
            if isinstance(func, AIFPLFunction):
                bytecode_prelude[name] = func

        cls._prelude_cache = bytecode_prelude
        return bytecode_prelude

    def __init__(self, max_depth: int = 1000):
        """
        Initialize AIFPL calculator.

        Args:
            max_depth: Maximum recursion depth (kept for compatibility, may be removed in future)
                      Note: VM uses tail-call optimization, so deep recursion is supported
        """
        self.max_depth = max_depth

        # VM components (always used)
        self.analyzer = AIFPLSemanticAnalyzer()
        self.compiler = AIFPLCompiler()
        self.vm = AIFPLVM()

        # Load prelude once at initialization
        self._prelude = self._load_prelude(self.compiler, self.vm)

    def _evaluate_raw(self, expression: str) -> 'AIFPLValue':
        """
        Evaluate an AIFPL expression without error handling.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression as AIFPLValue
        """
        # Lex
        lexer = AIFPLLexer()
        tokens = lexer.lex(expression)

        # Parse
        parser = AIFPLParser(tokens, expression)
        parsed_expr = parser.parse()

        # Semantic analysis
        parsed_expr = self.analyzer.analyze(parsed_expr)

        # Compile
        code = self.compiler.compile(parsed_expr)

        # Execute
        result = self.vm.execute(code, self.CONSTANTS, self._prelude)

        return result

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
        result = self._evaluate_raw(expression)
        return result.to_python()

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
        result = self._evaluate_raw(expression)
        return result.describe()
