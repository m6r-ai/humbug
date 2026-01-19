"""Main AIFPL (AI Functional Programming Language) class with enhanced error messages."""

from typing import Union
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_value import AIFPLFunction
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
            self.vm = None  # Will be created with evaluator reference

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

        if self.use_bytecode:
            # Compile and execute with VM
            code = self.compiler.compile(parsed_expr)
            
            # Create VM with evaluator reference
            if self.vm is None:
                self.vm = AIFPLVM(evaluator)
            
            # Set up globals (builtins and constants)
            # Pass constants for variable lookup
            globals_dict = evaluator.CONSTANTS  
            self.vm.set_globals(globals_dict)
            
            # Pass builtins separately for higher-order function use
            self.vm.set_builtins(evaluator._builtin_functions)
            
            # Execute
            result = self.vm.execute(code)
        else:
            # Use tree-walking interpreter
            result = evaluator.evaluate(parsed_expr)

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
            
            # Create VM with evaluator reference
            if self.vm is None:
                self.vm = AIFPLVM(evaluator)
            
            # Set up globals (builtins and constants)
            globals_dict = evaluator.CONSTANTS
            self.vm.set_globals(globals_dict)
            
            # Pass builtins separately for higher-order function use
            self.vm.set_builtins(evaluator._builtin_functions)
            
            # Execute
            result = self.vm.execute(code)
        else:
            # Use tree-walking interpreter
            result = evaluator.evaluate(parsed_expr)

        # Simplify and format the result
        simplified = evaluator.simplify_result(result)
        return evaluator.format_result(simplified)
