"""AIFPL (AI Functional Programming Language) package with enhanced error messages."""

# Main API
from aifpl.aifpl import AIFPL

# Exceptions (enhanced with detailed context)
from aifpl.aifpl_error import AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError, ErrorMessageBuilder

# Value types
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList, AIFPLAList,
    AIFPLRecursivePlaceholder, AIFPLFunction
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

    # Exceptions (enhanced with detailed context)
    "AIFPLError", "AIFPLTokenError", "AIFPLParseError", "AIFPLEvalError", "ErrorMessageBuilder",

    # Value types
    "AIFPLValue", "AIFPLNumber", "AIFPLString", "AIFPLBoolean", "AIFPLSymbol", "AIFPLList", "AIFPLAList",
    "AIFPLRecursivePlaceholder", "AIFPLFunction",

    # Lower-level components
    "AIFPLToken", "AIFPLTokenType", "AIFPLTokenizer", "AIFPLParser", "AIFPLEvaluator", "AIFPLEnvironment"
]
