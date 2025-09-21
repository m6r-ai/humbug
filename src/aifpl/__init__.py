"""AIFPL (AI Functional Programming Language) package."""

# Main API
from aifpl.aifpl import AIFPL

# Exceptions (for error handling)
from aifpl.aifpl_error import AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError

# Value types (new hierarchy)
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList, AIFPLRecursivePlaceholder,
    python_to_aifpl_value, aifpl_value_to_python
)

# Lower-level components (for advanced usage)
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import (
    AIFPLParser, AIFPLParsedExpression, AIFPLLambdaExpr, AIFPLLetExpr, AIFPLFunctionCall
)
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_environment import AIFPLEnvironment, AIFPLFunction, AIFPLTailCall, AIFPLCallStack
from aifpl.aifpl_dependency_analyzer import DependencyAnalyzer, BindingGroup


__all__ = [
    # Main API
    "AIFPL",

    # Exceptions
    "AIFPLError", "AIFPLTokenError", "AIFPLParseError", "AIFPLEvalError",

    # Value types
    "AIFPLValue", "AIFPLNumber", "AIFPLString", "AIFPLBoolean", "AIFPLSymbol", "AIFPLList", "AIFPLRecursivePlaceholder",
    "python_to_aifpl_value", "aifpl_value_to_python",

    # Lower-level components
    "AIFPLToken", "AIFPLTokenType", 
    "AIFPLTokenizer", "AIFPLParser", "AIFPLParsedExpression", "AIFPLLambdaExpr",
    "AIFPLLetExpr", "AIFPLFunctionCall",
    "AIFPLEvaluator", "AIFPLEnvironment", "AIFPLFunction", "AIFPLTailCall", "AIFPLCallStack",
    "DependencyAnalyzer", "BindingGroup"
]
