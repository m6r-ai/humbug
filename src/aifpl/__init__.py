"""AIFPL (AI Functional Programming Language) package."""

# Main API
from aifpl.aifpl import AIFPL

# Exceptions (for error handling)
from aifpl.aifpl_error import AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError

# Lower-level components (for advanced usage)
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser, ParsedExpression, LambdaExpr, LetExpr, FunctionCall, StringLiteral
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_environment import Environment, LambdaFunction, TailCall, CallStack


__all__ = [
    "AIFPL",
    "AIFPLError", "AIFPLTokenError", "AIFPLParseError", "AIFPLEvalError",
    "AIFPLToken", "AIFPLTokenType", 
    "AIFPLTokenizer", "AIFPLParser", "ParsedExpression", "LambdaExpr", "LetExpr", "FunctionCall", "StringLiteral",
    "AIFPLEvaluator", "Environment", "LambdaFunction", "TailCall", "CallStack"
]
