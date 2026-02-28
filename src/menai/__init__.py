"""Menai (AI Functional Programming Language) package with enhanced error messages."""

# Main API
from menai.menai import Menai

# Exceptions (enhanced with detailed context)
from menai.menai_error import (
    MenaiError, MenaiTokenError, MenaiParseError, MenaiEvalError,
    MenaiCancelledException
)

# AST types
from menai.menai_ast import (
    MenaiASTNode, MenaiASTInteger, MenaiASTFloat, MenaiASTComplex,
    MenaiASTString, MenaiASTBoolean, MenaiASTSymbol, MenaiASTList
)

# Value types
from menai.menai_value import (
    MenaiValue, MenaiInteger, MenaiFloat, MenaiComplex,
    MenaiString, MenaiBoolean, MenaiSymbol, MenaiList, MenaiDict, MenaiFunction
)

# Lower-level components (for advanced usage)
from menai.menai_token import MenaiToken, MenaiTokenType
from menai.menai_lexer import MenaiLexer
from menai.menai_parser import MenaiParser

# Trace watchers (for debugging)
from menai.menai_vm import MenaiTraceWatcher
from menai.menai_trace import (
    MenaiStdoutTraceWatcher, MenaiFileTraceWatcher, MenaiBufferingTraceWatcher
)

__all__ = [
    # Main API
    "Menai",

    # Exceptions (enhanced with detailed context)
    "MenaiError", "MenaiTokenError", "MenaiParseError", "MenaiEvalError", "MenaiCancelledException",

    # AST node types
    "MenaiASTNode", "MenaiASTInteger", "MenaiASTFloat", "MenaiASTComplex",
    "MenaiASTString", "MenaiASTBoolean", "MenaiASTSymbol", "MenaiASTList",

    # Value types
    "MenaiValue", "MenaiInteger", "MenaiFloat", "MenaiComplex",
    "MenaiString", "MenaiBoolean", "MenaiSymbol", "MenaiList", "MenaiDict", "MenaiFunction",

    # Lower-level components
    "MenaiToken", "MenaiTokenType", "MenaiLexer", "MenaiParser",

    # Trace watchers
    "MenaiTraceWatcher", "MenaiStdoutTraceWatcher",
    "MenaiFileTraceWatcher", "MenaiBufferingTraceWatcher",
]
