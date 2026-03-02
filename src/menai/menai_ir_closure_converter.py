"""
Menai IR Closure Converter — Step 4.

This pass walks a fully-classified and addressed IR tree and makes closure
capture explicit in the IR representation, preparing for lambda lifting
(Step 5).

VM slot layout
--------------
The Menai VM separates call-site arguments from captured values:

    ENTER n        — pops n args from the call stack into locals[0..n-1]
    MAKE_CLOSURE   — pops M captured values from the stack and stores them
                     into locals[param_count..param_count+M-1] at call time

So for a lambda with param_count=N and len(free_vars)=M:

    locals[0..N-1]     — call-site arguments  (populated by ENTER)
    locals[N..N+M-1]   — captured free vars   (populated by MAKE_CLOSURE)

param_count on the CodeObject controls ENTER and must equal the number of
explicit call-site arguments.  It must NOT be changed by this pass.

MenaiIRLambda.params vs param_count
-------------------------------------
MenaiIRLambda.params is the list of named parameters used by the addresser
to build the lambda's scope dict:

    params[i]       → slot i      (call-site args, populated by ENTER)
    free_vars[j]    → slot N+j    (captured values, populated by MAKE_CLOSURE)

After the first addresser run, body references to free vars are already
resolved to depth=0, index=N+j — they are in the lambda's own frame.
The free_var_plans (which load the values from the enclosing frame before
MAKE_CLOSURE) have depth>0.

What this pass does
-------------------
The pass is a pure tree rewrite that recurses into every IR node and
returns a structurally identical tree.  It does NOT change free_vars,
params, param_count, or max_locals on any lambda — the MAKE_CLOSURE
mechanism already makes capture explicit at the IR level, and changing
param_count would break the ENTER instruction.

The pass serves as:
  1. The insertion point in the pipeline for future lambda-lifting logic
     (Step 5), which will rewrite params, free_vars, and call sites.
  2. A tree-rewriting skeleton that recurses into all node types
     (including free_var_plans and parent_ref_plans, which are evaluated
     in the enclosing frame and may themselves contain lambdas).
  3. A verified no-op: the re-run of MenaiIRAddresser after this pass
     confirms that all body references are correctly resolved to depth=0.

parent_refs are not touched — they are recursive back-edges handled
separately by LOAD_PARENT_VAR.

Pipeline position
-----------------
    MenaiIRParentRefClassifier
        ↓
    MenaiIRAddresser             (first run — resolves addresses)
        ↓
    MenaiIRClosureConverter      ← THIS PASS
        ↓
    MenaiIRAddresser             (second run — re-verifies after conversion)
        ↓
    IR optimization passes
        ↓
    MenaiCodeGen

Usage
-----
    converter = MenaiIRClosureConverter()
    new_ir = converter.convert(ir)
"""

from __future__ import annotations

from typing import List

from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIREmptyList,
    MenaiIRError,
    MenaiIRExpr,
    MenaiIRIf,
    MenaiIRLambda,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRQuote,
    MenaiIRReturn,
    MenaiIRTrace,
    MenaiIRVariable,
)


class MenaiIRClosureConverter:
    """
    Makes closure capture explicit in the IR tree (Step 4).

    Currently a tree-rewriting identity pass that recurses into every node
    (including free_var_plans and parent_ref_plans) without changing the
    lambda structure.  Serves as the pipeline insertion point for the
    lambda-lifting transformation (Step 5).

    The pass is stateless between calls.

    Usage::

        new_ir = MenaiIRClosureConverter().convert(ir)
    """

    def convert(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """
        Walk *ir* and return a new tree with all closure captures made
        explicit.

        Args:
            ir: IR tree produced by MenaiIRParentRefClassifier and addressed
                by MenaiIRAddresser.

        Returns:
            New IR tree (structurally equivalent; ready for addresser re-run).
        """
        return self._walk(ir)

    # ------------------------------------------------------------------
    # Main recursive walk
    # ------------------------------------------------------------------

    def _walk(self, ir: MenaiIRExpr) -> MenaiIRExpr:
        """Recursively walk and rewrite *ir*."""

        if isinstance(ir, MenaiIRLambda):
            return self._convert_lambda(ir)

        if isinstance(ir, MenaiIRLet):
            return self._walk_let(ir)

        if isinstance(ir, MenaiIRLetrec):
            return self._walk_letrec(ir)

        if isinstance(ir, MenaiIRIf):
            return MenaiIRIf(
                condition_plan=self._walk(ir.condition_plan),
                then_plan=self._walk(ir.then_plan),
                else_plan=self._walk(ir.else_plan),
                in_tail_position=ir.in_tail_position,
            )

        if isinstance(ir, MenaiIRCall):
            return self._walk_call(ir)

        if isinstance(ir, MenaiIRReturn):
            return MenaiIRReturn(value_plan=self._walk(ir.value_plan))

        if isinstance(ir, MenaiIRTrace):
            return MenaiIRTrace(
                message_plans=[self._walk(m) for m in ir.message_plans],
                value_plan=self._walk(ir.value_plan),
            )

        if isinstance(ir, (MenaiIRVariable, MenaiIRConstant, MenaiIRQuote,
                           MenaiIREmptyList, MenaiIRError)):
            # Leaf nodes — return unchanged.
            return ir

        raise TypeError(
            f"MenaiIRClosureConverter: unhandled IR node type {type(ir).__name__}"
        )

    # ------------------------------------------------------------------
    # Node-specific handlers
    # ------------------------------------------------------------------

    def _convert_lambda(self, ir: MenaiIRLambda) -> MenaiIRLambda:
        """
        Convert a lambda node.

        Recurses into the body and into free_var_plans (evaluated in the
        enclosing frame and may contain nested lambdas that need converting).
        (parent_ref_plans removed — letrec siblings are now regular free_vars)

        The lambda's params, sibling_free_vars, outer_free_vars, param_count, and
        max_locals are preserved: the MAKE_CLOSURE mechanism already makes
        capture explicit at the bytecode level, and param_count must not
        change (it controls how many arguments ENTER pops from the call
        stack).
        """
        new_body = self._walk(ir.body_plan)

        # Recurse into sibling/outer free_var_plans: evaluated in the enclosing frame and
        # may themselves contain lambdas.
        new_sibling_free_var_plans: List[MenaiIRExpr] = [
            self._walk(p) for p in ir.sibling_free_var_plans
        ]
        new_outer_free_var_plans: List[MenaiIRExpr] = [
            self._walk(p) for p in ir.outer_free_var_plans
        ]

        return MenaiIRLambda(
            params=ir.params,
            body_plan=new_body,
            sibling_free_vars=ir.sibling_free_vars,
            sibling_free_var_plans=new_sibling_free_var_plans,
            outer_free_vars=ir.outer_free_vars,
            outer_free_var_plans=new_outer_free_var_plans,
            param_count=ir.param_count,
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            max_locals=ir.max_locals,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )

    def _walk_let(self, ir: MenaiIRLet) -> MenaiIRLet:
        """Walk a let node, recursing into binding values and body."""
        new_bindings: List[tuple] = [
            (name, self._walk(value_plan), var_index)
            for name, value_plan, var_index in ir.bindings
        ]
        return MenaiIRLet(
            bindings=new_bindings,
            body_plan=self._walk(ir.body_plan),
            in_tail_position=ir.in_tail_position,
        )

    def _walk_letrec(self, ir: MenaiIRLetrec) -> MenaiIRLetrec:
        """Walk a letrec node, recursing into binding values and body."""
        new_bindings: List[tuple] = [
            (name, self._walk(value_plan), var_index)
            for name, value_plan, var_index in ir.bindings
        ]
        return MenaiIRLetrec(
            bindings=new_bindings,
            body_plan=self._walk(ir.body_plan),
            in_tail_position=ir.in_tail_position,
        )

    def _walk_call(self, ir: MenaiIRCall) -> MenaiIRCall:
        """Walk a call node."""
        new_args = [self._walk(a) for a in ir.arg_plans]

        return MenaiIRCall(
            func_plan=self._walk(ir.func_plan),
            arg_plans=new_args,
            is_tail_call=ir.is_tail_call,
            is_builtin=ir.is_builtin,
            builtin_name=ir.builtin_name,
        )
