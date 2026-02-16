"""AIFPL (AI Functional Programming Language) package with enhanced error messages."""

# Main API
from aifpl.aifpl import AIFPL

# Exceptions (enhanced with detailed context)
from aifpl.aifpl_error import (
    AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError,
    AIFPLCancelledException
)

# AST types
from aifpl.aifpl_ast import (
    AIFPLASTNode, AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex,
    AIFPLASTString, AIFPLASTBoolean, AIFPLASTSymbol, AIFPLASTList
)

# Value types
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex,
    AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList, AIFPLAList, AIFPLFunction
)

# Lower-level components (for advanced usage)
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser

# Trace watchers (for debugging)
from aifpl.aifpl_vm import AIFPLTraceWatcher
from aifpl.aifpl_trace import (
    AIFPLStdoutTraceWatcher, AIFPLFileTraceWatcher, AIFPLBufferingTraceWatcher
)

__all__ = [
    # Main API
    "AIFPL",

    # Exceptions (enhanced with detailed context)
    "AIFPLError", "AIFPLTokenError", "AIFPLParseError", "AIFPLEvalError", "AIFPLCancelledException",

    # AST node types
    "AIFPLASTNode", "AIFPLASTInteger", "AIFPLASTFloat", "AIFPLASTComplex",
    "AIFPLASTString", "AIFPLASTBoolean", "AIFPLASTSymbol", "AIFPLASTList",

    # Value types
    "AIFPLValue", "AIFPLInteger", "AIFPLFloat", "AIFPLComplex",
    "AIFPLString", "AIFPLBoolean", "AIFPLSymbol", "AIFPLList", "AIFPLAList", "AIFPLFunction",

    # Lower-level components
    "AIFPLToken", "AIFPLTokenType", "AIFPLLexer", "AIFPLParser",

    # Trace watchers
    "AIFPLTraceWatcher", "AIFPLStdoutTraceWatcher",
    "AIFPLFileTraceWatcher", "AIFPLBufferingTraceWatcher",
]
