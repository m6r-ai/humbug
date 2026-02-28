"""
Menai IR optimization pass base class.

Mirrors MenaiOptimizationPass at the AST level, but operates on the IR tree.
Each pass is self-contained: it performs whatever analysis it needs internally
(e.g. use counting) and returns a new IR tree plus a flag indicating whether
any changes were made.  The pass manager uses that flag to drive fixed-point
iteration.
"""

from menai.menai_ir import MenaiIRExpr


class MenaiIROptimizationPass:
    """Base class for IR optimization passes."""

    def optimize(self, ir: MenaiIRExpr) -> tuple[MenaiIRExpr, bool]:
        """
        Transform the IR tree, returning an optimized version.

        Each pass is responsible for any analysis it requires (e.g. use
        counting).  The pass must not mutate the input tree.

        Args:
            ir: Root IR node to optimize.

        Returns:
            A tuple of (new_ir, changed) where changed is True if the pass
            made at least one transformation.  Returning the original node
            unchanged with changed=False signals the pass manager that this
            pass has reached a fixed point and need not be re-run.
        """
        raise NotImplementedError
