"""
AIFPL AST optimization pass
"""

from aifpl.aifpl_ast import (AIFPLASTNode)


class AIFPLOptimizationPass:
    """Base class for AST optimization passes."""

    def optimize(self, expr: AIFPLASTNode) -> AIFPLASTNode:
        """
        Transform AST, returning optimized version.

        Args:
            expr: Input AST expression

        Returns:
            Optimized AST expression
        """
        raise NotImplementedError
