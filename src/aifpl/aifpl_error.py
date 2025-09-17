"""Exception classes for AIFPL (AI Functional Programming Language)."""


class AIFPLError(Exception):
    """Base exception for AIFPL errors."""


class AIFPLTokenError(AIFPLError):
    """Tokenization errors."""


class AIFPLParseError(AIFPLError):
    """Parsing errors."""


class AIFPLEvalError(AIFPLError):
    """Evaluation errors."""
