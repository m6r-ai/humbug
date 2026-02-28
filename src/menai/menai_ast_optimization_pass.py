"""
Menai AST optimization pass
"""

from menai.menai_ast import (MenaiASTNode)


class MenaiASTOptimizationPass:
    """Base class for AST optimization passes."""

    def optimize(self, expr: MenaiASTNode) -> MenaiASTNode:
        """
        Transform AST, returning optimized version.

        Args:
            expr: Input AST expression

        Returns:
            Optimized AST expression
        """
        raise NotImplementedError
