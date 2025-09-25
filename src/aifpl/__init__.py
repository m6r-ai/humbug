"""AIFPL (AI Functional Programming Language) package with enhanced error messages."""

# Main API
from aifpl.aifpl import AIFPL

# Exceptions (for error handling) - now with enhanced detailed versions
from aifpl.aifpl_error import AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError
from aifpl.aifpl_detailed_error import (
    AIFPLDetailedError, AIFPLDetailedTokenError, AIFPLDetailedParseError, AIFPLDetailedEvalError,
    ErrorContext, ErrorMessageBuilder
)

# Value types (new hierarchy)
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList, AIFPLRecursivePlaceholder, AIFPLFunction,
    AIFPLBuiltinFunction
)

# Lower-level components (for advanced usage)
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_evaluator import AIFPLEvaluator
from aifpl.aifpl_environment import AIFPLEnvironment


__all__ = [
    # Main API
    "AIFPL",

    # Exceptions (both original and enhanced)
    "AIFPLError", "AIFPLTokenError", "AIFPLParseError", "AIFPLEvalError",
    "AIFPLDetailedError", "AIFPLDetailedTokenError", "AIFPLDetailedParseError", "AIFPLDetailedEvalError",
    "ErrorContext", "ErrorMessageBuilder",

    # Value types
    "AIFPLValue", "AIFPLNumber", "AIFPLString", "AIFPLBoolean", "AIFPLSymbol", "AIFPLList",
    "AIFPLRecursivePlaceholder", "AIFPLFunction", "AIFPLBuiltinFunction",

    # Lower-level components
    "AIFPLToken", "AIFPLTokenType", "AIFPLTokenizer", "AIFPLParser", "AIFPLEvaluator", "AIFPLEnvironment"
]