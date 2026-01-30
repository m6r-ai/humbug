"""
AIFPL AST optimization pass
"""

from aifpl.aifpl_value import (
    AIFPLValue
)


class AIFPLOptimizationPass:
    """Base class for AST optimization passes."""

    def optimize(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Transform AST, returning optimized version.

        Args:
            expr: Input AST expression

        Returns:
            Optimized AST expression
        """
        raise NotImplementedError
