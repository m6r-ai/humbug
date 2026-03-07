"""
Menai CFG optimization pass base class.

Mirrors MenaiIROptimizationPass at the IR level, but operates on the CFG.
Each pass is self-contained and returns a (possibly new) MenaiCFGFunction
together with a flag indicating whether any changes were made.  The pass
manager uses that flag to drive fixed-point iteration.
"""

from menai.menai_cfg import MenaiCFGFunction, MenaiCFGMakeClosureInstr


class MenaiCFGOptimizationPass:
    """Base class for CFG optimization passes."""

    def optimize(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        """
        Transform `func` and all nested lambda functions it contains.

        Subclasses implement `_optimize_function` to apply their transformation
        to a single flat function.  This method handles recursion into nested
        lambdas embedded in MenaiCFGMakeClosureInstr instructions automatically.

        Args:
            func: The CFG function to optimize.

        Returns:
            A tuple of (new_func, changed) where changed is True if the pass
            made at least one transformation anywhere in the function tree.
        """
        func, changed = self._optimize_function(func)
        func, nested_changed = self._optimize_nested(func)
        return func, changed or nested_changed

    def _optimize_function(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        """
        Transform a single flat CFG function, returning an optimized version.

        The pass must not assume it is the only pass being run; the pass
        manager may run multiple passes to a fixed point.

        Args:
            func: The CFG function to optimize.

        Returns:
            A tuple of (new_func, changed) where changed is True if the pass
            made at least one transformation.  Returning the original function
            unchanged with changed=False signals the pass manager that this
            pass has reached a fixed point and need not be re-run.
        """
        raise NotImplementedError

    def _optimize_nested(self, func: MenaiCFGFunction) -> tuple[MenaiCFGFunction, bool]:
        """
        Recursively optimize all MenaiCFGFunction objects embedded in
        MenaiCFGMakeClosureInstr instructions anywhere in `func`.

        When a nested lambda is optimized and returns a new function object,
        the MakeClosure instruction is updated in place.

        Returns `func` (possibly with mutated MakeClosure instructions) and
        a flag indicating whether any nested function was changed.
        """
        changed = False
        for block in func.blocks:
            for i, instr in enumerate(block.instrs):
                if isinstance(instr, MenaiCFGMakeClosureInstr):
                    new_child, child_changed = self.optimize(instr.function)
                    if child_changed:
                        block.instrs[i] = MenaiCFGMakeClosureInstr(
                            result=instr.result,
                            function=new_child,
                            captures=instr.captures,
                            needs_patching=instr.needs_patching,
                        )
                        changed = True
        return func, changed
