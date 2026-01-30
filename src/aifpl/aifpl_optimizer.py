"""
AIFPL AST Optimizer - Optimizes AST before bytecode compilation.

This module provides a framework for AST optimization passes that run after
desugaring but before bytecode compilation. Optimizations transform the AST
while preserving runtime semantics.
"""

from typing import List

from aifpl.aifpl_constant_folding_pass import AIFPLConstantFoldingPass
from aifpl.aifpl_value import AIFPLValue


class AIFPLOptimizer:
    """
    Orchestrates AST optimization passes.

    This class manages multiple optimization passes and applies them in sequence
    to transform the AST before bytecode compilation.
    """

    def __init__(self, enable_passes: List[str] | None = None):
        """
        Initialize with optional pass selection.

        Args:
            enable_passes: List of pass names to enable, or None for all passes
        """
        all_passes = {
            'constant_folding': AIFPLConstantFoldingPass(),
            # Future passes can be added here:
            # 'dead_code_elimination': DeadCodeEliminationPass(),
            # 'common_subexpression_elimination': CSEPass(),
        }

        if enable_passes is None:
            # Enable all passes by default
            self.passes = list(all_passes.values())

        else:
            # Enable only specified passes
            self.passes = [all_passes[name] for name in enable_passes if name in all_passes]

    def optimize(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Run all enabled optimization passes in sequence.

        Args:
            expr: Input AST expression

        Returns:
            Optimized AST expression
        """
        optimized = expr
        for pass_instance in self.passes:
            optimized = pass_instance.optimize(optimized)

        return optimized
