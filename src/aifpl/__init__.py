"""AIFPL (AI Functional Programming Language) package."""

# Main API
from aifpl.aifpl import AIFPL

# Exceptions (for error handling)
from aifpl.aifpl_error import AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError

# Lower-level components (for advanced usage)
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import (
    AIFPLParser, AIFPLParsedExpression, AIFPLLambdaExpr, AIFPLLetExpr, AIFPLFunctionCall, AIFPLStringLiteral
)
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_environment import AIFPLEnvironment, AIFPLLambdaFunction, AIFPLTailCall, AIFPLCallStack


__all__ = [
    "AIFPL",
    "AIFPLError", "AIFPLTokenError", "AIFPLParseError", "AIFPLEvalError",
    "AIFPLToken", "AIFPLTokenType", 
    "AIFPLTokenizer", "AIFPLParser", "AIFPLParsedExpression", "AIFPLLambdaExpr",
    "AIFPLLetExpr", "AIFPLFunctionCall", "AIFPLStringLiteral",
    "AIFPLEvaluator", "AIFPLEnvironment", "AIFPLLambdaFunction", "AIFPLTailCall", "AIFPLCallStack"
]
