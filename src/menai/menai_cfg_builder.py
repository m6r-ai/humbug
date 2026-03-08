"""
CFG builder for the Menai compiler.

Translates a symbolic MenaiIR tree into a MenaiCFGFunction in SSA form.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

from menai.menai_cfg import (
    MenaiCFGApplyInstr,
    MenaiCFGBlock,
    MenaiCFGBranchTerm,
    MenaiCFGBuiltinInstr,
    MenaiCFGCallInstr,
    MenaiCFGConstInstr,
    MenaiCFGFreeVarInstr,
    MenaiCFGFunction,
    MenaiCFGGlobalInstr,
    MenaiCFGJumpTerm,
    MenaiCFGMakeClosureInstr,
    MenaiCFGPatchClosureInstr,
    MenaiCFGParamInstr,
    MenaiCFGPhiInstr,
    MenaiCFGRaiseTerm,
    MenaiCFGReturnTerm,
    MenaiCFGSelfLoopTerm,
    MenaiCFGTailApplyTerm,
    MenaiCFGTailCallTerm,
    MenaiCFGTraceInstr,
    MenaiCFGValue,
)
from menai.menai_ir import (
    MenaiIRCall,
    MenaiIRConstant,
    MenaiIREmptyList,
    MenaiIRBuildList,
    MenaiIRBuildDict,
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
from menai.menai_value import MenaiDict, MenaiList


@dataclass
class MenaiCFGScope:
    """
    A single lexical scope frame mapping variable names to SSA values.

    Frames are chained via `parent`; lookup walks innermost-first.
    """
    bindings: Dict[str, MenaiCFGValue] = field(default_factory=dict)
    parent: Optional['MenaiCFGScope'] = None

    def lookup(self, name: str) -> Optional[MenaiCFGValue]:
        """Search this frame and all ancestors for `name`."""
        frame: Optional[MenaiCFGScope] = self
        while frame is not None:
            if name in frame.bindings:
                return frame.bindings[name]
            frame = frame.parent
        return None

    def bind(self, name: str, value: MenaiCFGValue) -> None:
        """Add a binding to this (innermost) frame."""
        self.bindings[name] = value

    def child(self) -> 'MenaiCFGScope':
        """Return a new child scope whose parent is this frame."""
        return MenaiCFGScope(parent=self)


@dataclass
class _FunctionState:
    """
    Mutable state threaded through the build of a single MenaiCFGFunction.

    Isolated per lambda so that nested lambdas get their own counters and
    block lists.
    """
    function: MenaiCFGFunction
    value_counter: int = 0
    block_counter: int = 0
    self_value: 'MenaiCFGValue | None' = None  # SSA value of the function's own self-capture
                                                # free var, set for letrec-bound lambdas only.

    def new_value(self, hint: str = "") -> MenaiCFGValue:
        """Allocate a new SSA value with an optional hint for debugging."""
        v = MenaiCFGValue(id=self.value_counter, hint=hint)
        self.value_counter += 1
        return v

    def new_block(self, label: str) -> MenaiCFGBlock:
        """Allocate a new CFG block with the given label."""
        b = MenaiCFGBlock(id=self.block_counter, label=label)
        self.block_counter += 1
        self.function.blocks.append(b)
        return b


class MenaiCFGBuilder:
    """
    Builds a MenaiCFGFunction from a (symbolic, unaddressed) MenaiIR tree.

    Usage::

        cfg = MenaiCFGBuilder().build(ir_expr)
    """

    def __init__(self) -> None:
        # Instance-level letrec sibling context.  Set to the sibling-name set
        # during Phase 2b of _build_letrec (evaluating non-lambda binding RHS
        # expressions) so that _build_lambda_expr can detect lambdas that have
        # sibling captures and need PATCH_CLOSURE treatment.  None at all other
        # times, including during normal lambda-only letrec Phase 1.
        self._letrec_sibling_names: Optional[set] = None

        # Collected during Phase 2b: (closure_val, ir_lambda) pairs for lambdas
        # embedded in non-lambda letrec binding RHS expressions that have at
        # least one sibling capture.  Processed (patched) after Phase 2b.
        self._letrec_deferred_patches: Optional[List[Tuple[MenaiCFGValue, MenaiIRLambda]]] = None

    def build(self, ir: MenaiIRExpr) -> MenaiCFGFunction:
        """
        Build the top-level MenaiCFGFunction from an IR expression.

        Args:
            ir: Root of the optimised, symbolic IR tree (output of the IR
                optimisation passes).

        Returns:
            MenaiCFGFunction for the top-level module body.
        """
        func = MenaiCFGFunction(
            params=[],
            free_vars=[],
            is_variadic=False,
            binding_name=None,
        )
        state = _FunctionState(function=func)
        entry = state.new_block("entry")
        scope = MenaiCFGScope()

        result_val, current_block = self._build_expr(
            ir, entry, scope, state, tail=True
        )

        # If the expression returned a value rather than terminating the block,
        # wrap it in a return.
        if current_block.terminator is None:
            current_block.terminator = MenaiCFGReturnTerm(value=result_val)

        self._link_predecessors(func)
        return func

    def _build_expr(
        self,
        ir: MenaiIRExpr,
        block: MenaiCFGBlock,
        scope: MenaiCFGScope,
        state: _FunctionState,
        tail: bool,
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        """
        Emit instructions for `ir` into `block` (and successor blocks as
        needed), returning the SSA value that holds the result and the
        (possibly new) current block after emission.

        When `tail` is True the expression is in tail position: calls become
        tail-call terminators and the caller will not emit a further return.

        Args:
            ir:    IR node to lower.
            block: Current basic block to emit into.
            scope: Current lexical scope.
            state: Per-function mutable state (counters, block list).
            tail:  True if this expression is in tail position.

        Returns:
            (result_value, current_block) — the SSA value produced and the
            block that is "current" after emission (may differ from `block`
            if new blocks were created, e.g. for if-expressions).
        """
        if isinstance(ir, MenaiIRConstant):
            return self._build_constant(ir, block, state)

        if isinstance(ir, MenaiIREmptyList):
            return self._build_empty_list(block, state)

        if isinstance(ir, MenaiIRQuote):
            return self._build_quote(ir, block, state)

        if isinstance(ir, MenaiIRVariable):
            return self._build_variable(ir, block, scope, state)

        if isinstance(ir, MenaiIRIf):
            return self._build_if(ir, block, scope, state, tail)

        if isinstance(ir, MenaiIRLet):
            return self._build_let(ir, block, scope, state, tail)

        if isinstance(ir, MenaiIRLetrec):
            return self._build_letrec(ir, block, scope, state, tail)

        if isinstance(ir, MenaiIRLambda):
            return self._build_lambda_expr(ir, block, scope, state)

        if isinstance(ir, MenaiIRCall):
            return self._build_call(ir, block, scope, state, tail)

        if isinstance(ir, MenaiIRBuildList):
            return self._build_list(ir, block, scope, state)

        if isinstance(ir, MenaiIRBuildDict):
            return self._build_dict(ir, block, scope, state)

        if isinstance(ir, MenaiIRReturn):
            # MenaiIRReturn is the IR tree's explicit return wrapper.
            # We honour tail=True here since the IR already marked this.
            return self._build_expr(ir.value_plan, block, scope, state, tail=True)

        if isinstance(ir, MenaiIRTrace):
            return self._build_trace(ir, block, scope, state)

        if isinstance(ir, MenaiIRError):
            return self._build_error(ir, block, state)

        raise TypeError(f"MenaiCFGBuilder: unhandled IR node {type(ir).__name__}")

    def _build_constant(
        self, ir: MenaiIRConstant, block: MenaiCFGBlock, state: _FunctionState
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        result = state.new_value("const")
        block.instrs.append(MenaiCFGConstInstr(result=result, value=ir.value))
        return result, block

    def _build_empty_list(
        self, block: MenaiCFGBlock, state: _FunctionState
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        result = state.new_value("empty_list")
        block.instrs.append(MenaiCFGConstInstr(result=result, value=MenaiList()))
        return result, block

    def _build_quote(
        self, ir: MenaiIRQuote, block: MenaiCFGBlock, state: _FunctionState
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        result = state.new_value("quoted")
        block.instrs.append(MenaiCFGConstInstr(result=result, value=ir.quoted_value))
        return result, block

    def _build_variable(
        self, ir: MenaiIRVariable, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        if ir.var_type == 'global':
            result = state.new_value(ir.name)
            block.instrs.append(MenaiCFGGlobalInstr(result=result, name=ir.name))
            return result, block

        # Local variable — must be in scope.
        val = scope.lookup(ir.name)
        assert val is not None, (
            f"MenaiCFGBuilder: unresolved local variable {ir.name!r}"
        )
        return val, block

    def _build_error(
        self, ir: MenaiIRError, block: MenaiCFGBlock, state: _FunctionState
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        # error terminates the block; the returned value is a placeholder
        # that will never be used (the block has no successors).
        block.terminator = MenaiCFGRaiseTerm(message=ir.message)
        placeholder = state.new_value("error")
        return placeholder, block

    def _build_if(
        self, ir: MenaiIRIf, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState, tail: bool
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        # Emit condition into the current block.
        cond_val, block = self._build_expr(
            ir.condition_plan, block, scope, state, tail=False
        )

        # Create the three successor blocks.
        then_block = state.new_block("then")
        else_block = state.new_block("else")
        join_block = state.new_block("join")

        block.terminator = MenaiCFGBranchTerm(
            cond=cond_val,
            true_block=then_block,
            false_block=else_block,
        )

        # Build then branch.
        then_val, then_exit = self._build_expr(
            ir.then_plan, then_block, scope, state, tail=tail
        )
        # If the then branch didn't terminate (e.g. it's not a tail call),
        # jump to the join block.
        if then_exit.terminator is None:
            then_exit.terminator = MenaiCFGJumpTerm(target=join_block)

        # Build else branch.
        else_val, else_exit = self._build_expr(
            ir.else_plan, else_block, scope, state, tail=tail
        )
        if else_exit.terminator is None:
            else_exit.terminator = MenaiCFGJumpTerm(target=join_block)

        # Determine whether the join block is reachable.  It is reachable iff
        # at least one branch has a JumpTerm pointing to it (i.e. the branch
        # did not terminate with a tail-call/return/self-loop).
        then_jumps_to_join = isinstance(then_exit.terminator, MenaiCFGJumpTerm) and then_exit.terminator.target is join_block
        else_jumps_to_join = isinstance(else_exit.terminator, MenaiCFGJumpTerm) and else_exit.terminator.target is join_block
        join_reachable = then_jumps_to_join or else_jumps_to_join

        if not join_reachable:
            # Both branches are tail-terminated; join is unreachable.
            # Return a placeholder — the caller will not emit a further return.
            placeholder = state.new_value("if_result")
            return placeholder, join_block

        # Join block is reachable.  If only one branch reaches it, the join
        # value is unambiguous — no phi needed.  The join block will be empty
        # and MenaiCFGBypassEmptyBlocks will eliminate it.
        # If both branches reach it, emit a phi to merge the two values.
        if then_jumps_to_join and not else_jumps_to_join:
            return then_val, join_block

        if else_jumps_to_join and not then_jumps_to_join:
            return else_val, join_block

        phi_result = state.new_value("if_result")
        join_block.instrs.append(MenaiCFGPhiInstr(
            result=phi_result,
            incoming=[(then_val, then_exit), (else_val, else_exit)],
        ))
        return phi_result, join_block

    def _build_let(
        self, ir: MenaiIRLet, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState, tail: bool
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        # Binding values are evaluated in the outer scope (parallel let).
        binding_vals: List[Tuple[str, MenaiCFGValue]] = []
        for name, value_plan in ir.bindings:
            val, block = self._build_expr(value_plan, block, scope, state, tail=False)
            binding_vals.append((name, val))

        # Body is evaluated in a child scope that contains all binding names.
        body_scope = scope.child()
        for name, val in binding_vals:
            body_scope.bind(name, val)

        return self._build_expr(ir.body_plan, block, body_scope, state, tail=tail)

    def _build_letrec(
        self, ir: MenaiIRLetrec, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState, tail: bool
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        """
        Build a letrec in three phases.

        Phase 1: For lambda bindings, build child CFGs and emit
                 MenaiCFGMakeClosureInstr with only OUTER (non-sibling)
                 captures.  Sibling captures cannot be loaded yet because the
                 sibling closures haven't been created.  The VM pre-allocates
                 None slots for the full free_vars list; PATCH_CLOSURE fills
                 in siblings.  Non-lambda bindings are deferred to Phase 2b.

        Phase 2: Register all closure SSA values in letrec_scope so that
                 sibling references resolve.

        Phase 2b: Evaluate non-lambda binding values now that all sibling
                 closure SSA values are in letrec_scope.  Any lambdas nested
                 inside the value expression that capture sibling names are
                 built with needs_patching=True and their sibling captures are
                 collected for Phase 3.

        Phase 3: Emit MenaiCFGPatchClosureInstr for every sibling capture of
                 every lambda binding, using the full capture_index from the
                 child function's free_vars list.
        """
        sibling_names = {name for name, _ in ir.bindings}
        # Build a child scope for letrec-bound names (populated in Phase 2).
        letrec_scope = scope.child()

        # Phase 1: build each lambda's CFG and emit MAKE_CLOSURE with only
        # outer captures (those not in sibling_names).
        binding_vals: Dict[str, MenaiCFGValue] = {}

        for name, value_plan in ir.bindings:
            if not isinstance(value_plan, MenaiIRLambda):
                # Non-lambda binding (e.g. a list or call expression whose RHS
                # contains a nested lambda closing over this binding's name).
                # Allocate a placeholder SSA value now so sibling lambdas can
                # reference this name; the real value is computed in Phase 2b.
                placeholder = state.new_value(name)
                binding_vals[name] = placeholder
                continue

            child_func = self._build_lambda_function(value_plan)

            # Collect outer captures only — evaluate them in the current scope.
            # Sibling captures are deferred to Phase 3 (PATCH_CLOSURE).
            outer_captures: List[MenaiCFGValue] = []
            for fv_plan, fv_name in zip(
                value_plan.sibling_free_var_plans + value_plan.outer_free_var_plans,
                value_plan.sibling_free_vars + value_plan.outer_free_vars,
            ):
                if fv_name not in sibling_names:
                    fv_val, block = self._build_expr(fv_plan, block, scope, state, tail=False)
                    outer_captures.append(fv_val)

            has_sibling_captures = bool(value_plan.sibling_free_vars)
            closure_val = state.new_value(name)
            make_instr = MenaiCFGMakeClosureInstr(
                result=closure_val,
                function=child_func,
                captures=outer_captures,
                needs_patching=has_sibling_captures,
            )
            block.instrs.append(make_instr)
            binding_vals[name] = closure_val

        # Phase 2: register all closure values in letrec_scope so sibling
        # references resolve during Phase 3.
        for name, closure_val in binding_vals.items():
            letrec_scope.bind(name, closure_val)

        # Phase 2b: evaluate non-lambda binding values now that all sibling
        # closure SSA values are in letrec_scope.  Any nested lambdas in the
        # value expression that have sibling captures are intercepted by
        # _build_lambda_expr (which checks self._letrec_sibling_names) and
        # recorded in self._letrec_deferred_patches for Phase 3b below.
        prev_sibling_names = self._letrec_sibling_names
        prev_deferred_patches = self._letrec_deferred_patches
        self._letrec_sibling_names = sibling_names
        self._letrec_deferred_patches = []
        for name, value_plan in ir.bindings:
            if isinstance(value_plan, MenaiIRLambda):
                continue
            real_val, block = self._build_expr(value_plan, block, letrec_scope, state, tail=False)
            letrec_scope.bind(name, real_val)

        deferred_non_lambda_patches = self._letrec_deferred_patches
        self._letrec_sibling_names = prev_sibling_names
        self._letrec_deferred_patches = prev_deferred_patches

        # Phase 3: emit PATCH_CLOSURE for every sibling capture of every lambda.
        # capture_index is the position in the child function's full free_vars
        # list (sibling_free_vars + outer_free_vars), which is what the VM uses.
        # Appended to block.instrs (not patch_instrs) so they execute before
        # the letrec body, which is also built into the same block's instrs.
        for name, value_plan in ir.bindings:
            if not isinstance(value_plan, MenaiIRLambda):
                continue  # Non-lambda bindings have no closure captures to patch
            closure_val = binding_vals[name]

            for capture_index, fv_name in enumerate(
                value_plan.sibling_free_vars + value_plan.outer_free_vars
            ):
                if fv_name in {n for n, _ in ir.bindings}:
                    patch_val = letrec_scope.lookup(fv_name)
                    assert patch_val is not None
                    block.instrs.append(MenaiCFGPatchClosureInstr(
                        closure=closure_val,
                        capture_index=capture_index,
                        value=patch_val,
                    ))

        # Phase 3b: emit PATCH_CLOSURE for lambdas embedded in non-lambda
        # binding RHS expressions (collected during Phase 2b).  After Phase 2b,
        # all non-lambda binding names are in letrec_scope with their real
        # values, so we can patch the sibling captures now.
        for closure_val, lambda_ir in deferred_non_lambda_patches:
            for capture_index, fv_name in enumerate(
                lambda_ir.sibling_free_vars + lambda_ir.outer_free_vars
            ):
                if fv_name in sibling_names:
                    patch_val = letrec_scope.lookup(fv_name)
                    assert patch_val is not None, (
                        f"MenaiCFGBuilder: sibling free var {fv_name!r} not in letrec_scope"
                    )
                    block.instrs.append(MenaiCFGPatchClosureInstr(
                        closure=closure_val,
                        capture_index=capture_index,
                        value=patch_val,
                    ))

        # Build the body with all letrec names in scope.
        return self._build_expr(ir.body_plan, block, letrec_scope, state, tail=tail)

    def _build_lambda_expr(
        self, ir: MenaiIRLambda, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState,
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        """
        Build a lambda that appears as a value expression (not inside letrec).

        Recursively builds the child MenaiCFGFunction, then emits a
        MenaiCFGMakeClosureInstr in the parent block.

        Captures are loaded by evaluating each free_var_plan as an IR
        expression.  After copy propagation these may be constants or other
        non-variable expressions, not necessarily MenaiIRVariable nodes, so
        we dispatch through _build_expr rather than doing a scope lookup by name.

        Letrec Phase 2b interception: if self._letrec_sibling_names is set (we
        are inside the evaluation of a non-lambda letrec binding's RHS) and
        this lambda has sibling captures, we treat it like a letrec lambda:
        emit MAKE_CLOSURE with needs_patching=True and record it in
        self._letrec_deferred_patches so Phase 3b can PATCH_CLOSURE the
        sibling captures after all non-lambda binding values are available.
        """
        child_func = self._build_lambda_function(ir)

        # Check for letrec Phase 2b interception.
        sibling_names = self._letrec_sibling_names
        has_sibling_captures = bool(ir.sibling_free_vars) and sibling_names is not None
        if has_sibling_captures:
            assert sibling_names is not None
            # Evaluate only outer (non-sibling) captures now.
            outer_captures: List[MenaiCFGValue] = []
            for fv_plan, fv_name in zip(
                ir.sibling_free_var_plans + ir.outer_free_var_plans,
                ir.sibling_free_vars + ir.outer_free_vars,
            ):
                if fv_name not in sibling_names:
                    fv_val, block = self._build_expr(fv_plan, block, scope, state, tail=False)
                    outer_captures.append(fv_val)

            result = state.new_value(ir.binding_name or "lambda")
            block.instrs.append(MenaiCFGMakeClosureInstr(
                result=result,
                function=child_func,
                captures=outer_captures,
                needs_patching=True,
            ))
            assert self._letrec_deferred_patches is not None
            self._letrec_deferred_patches.append((result, ir))
            return result, block

        # Evaluate each capture plan in the current block and scope.
        captures: List[MenaiCFGValue] = []
        for fv_plan in ir.sibling_free_var_plans + ir.outer_free_var_plans:
            fv_val, block = self._build_expr(fv_plan, block, scope, state, tail=False)
            captures.append(fv_val)

        result = state.new_value(ir.binding_name or "lambda")
        block.instrs.append(MenaiCFGMakeClosureInstr(
            result=result,
            function=child_func,
            captures=captures,
        ))
        return result, block

    def _build_lambda_function(self, ir: MenaiIRLambda) -> MenaiCFGFunction:
        """
        Recursively build a MenaiCFGFunction for a MenaiIRLambda node.

        The child function has its own block list, value counter, and block
        counter.  The enclosing scope is NOT accessible from within the child
        (all captures are explicit in ir.sibling_free_vars / outer_free_vars).

        Args:
            ir: The lambda IR node.

        Returns:
            A fully-built MenaiCFGFunction for this lambda.
        """
        func = MenaiCFGFunction(
            params=list(ir.params),
            free_vars=list(ir.sibling_free_vars + ir.outer_free_vars),
            is_variadic=ir.is_variadic,
            binding_name=ir.binding_name,
            source_line=ir.source_line,
            source_file=ir.source_file,
        )
        state = _FunctionState(function=func)
        entry = state.new_block("entry")

        # Build the lambda's own scope: params first, then captured free vars.
        lambda_scope = MenaiCFGScope()

        for idx, param_name in enumerate(ir.params):
            param_val = state.new_value(param_name)
            entry.instrs.append(MenaiCFGParamInstr(
                result=param_val,
                index=idx,
                param_name=param_name,
            ))
            lambda_scope.bind(param_name, param_val)

        free_var_names = ir.sibling_free_vars + ir.outer_free_vars
        for idx, fv_name in enumerate(free_var_names):
            fv_val = state.new_value(fv_name)
            entry.instrs.append(MenaiCFGFreeVarInstr(
                result=fv_val,
                index=idx,
                var_name=fv_name,
            ))
            lambda_scope.bind(fv_name, fv_val)
            if fv_name == ir.binding_name:
                state.self_value = fv_val

        # Build the body.
        result_val, current_block = self._build_expr(
            ir.body_plan, entry, lambda_scope, state, tail=True
        )

        if current_block.terminator is None:
            current_block.terminator = MenaiCFGReturnTerm(value=result_val)

        self._link_predecessors(func)
        return func

    def _build_call(
        self, ir: MenaiIRCall, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState, tail: bool
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        if ir.is_builtin:
            return self._build_builtin_call(ir, block, scope, state, tail)

        # Evaluate arguments.
        arg_vals: List[MenaiCFGValue] = []
        for arg_plan in ir.arg_plans:
            arg_val, block = self._build_expr(arg_plan, block, scope, state, tail=False)
            arg_vals.append(arg_val)

        # Evaluate the function expression.
        func_val, block = self._build_expr(ir.func_plan, block, scope, state, tail=False)

        if tail:
            # Detect direct self-recursive tail call.
            if (isinstance(ir.func_plan, MenaiIRVariable)
                    and ir.func_plan.var_type == 'local'
                    and ir.func_plan.name == state.function.binding_name
                    and state.self_value is not None
                    and func_val is state.self_value):
                block.terminator = MenaiCFGSelfLoopTerm(args=arg_vals)
                placeholder = state.new_value("self_loop")
                return placeholder, block

            block.terminator = MenaiCFGTailCallTerm(func=func_val, args=arg_vals)
            placeholder = state.new_value("tail_call")
            return placeholder, block

        result = state.new_value("call_result")
        block.instrs.append(MenaiCFGCallInstr(
            result=result,
            func=func_val,
            args=arg_vals,
        ))
        return result, block

    def _build_list(
        self, ir: MenaiIRBuildList, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState,
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        """
        Build a list literal iteratively.

        Emits LOAD_EMPTY_LIST into an accumulator slot, then for each element
        emits a LIST_APPEND builtin op into the same accumulator slot,
        reusing the slot each time.  This is O(N) in instructions and O(1)
        in register slots regardless of list size.
        """
        # Seed: empty list into a fresh accumulator slot.
        acc_val = state.new_value("list_acc")
        block.instrs.append(MenaiCFGConstInstr(result=acc_val, value=MenaiList()))

        for elem_plan in ir.element_plans:
            elem_val, block = self._build_expr(elem_plan, block, scope, state, tail=False)
            new_acc = state.new_value("list_acc")
            block.instrs.append(MenaiCFGBuiltinInstr(
                result=new_acc,
                op='list-append',
                args=[acc_val, elem_val],
            ))
            acc_val = new_acc

        return acc_val, block

    def _build_dict(
        self,
        ir: MenaiIRBuildDict,
        block: MenaiCFGBlock,
        scope: MenaiCFGScope,
        state: _FunctionState,
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        """
        Build a dict literal iteratively.

        Emits LOAD_EMPTY_DICT into an accumulator slot, then for each
        (key, value) pair emits a DICT_SET builtin op into the accumulator.
        """
        acc_val = state.new_value("dict_acc")
        block.instrs.append(MenaiCFGConstInstr(result=acc_val, value=MenaiDict()))

        for key_plan, val_plan in ir.pair_plans:
            key_val, block = self._build_expr(key_plan, block, scope, state, tail=False)
            val_val, block = self._build_expr(val_plan, block, scope, state, tail=False)
            new_acc = state.new_value("dict_acc")
            block.instrs.append(MenaiCFGBuiltinInstr(
                result=new_acc,
                op='dict-set',
                args=[acc_val, key_val, val_val],
            ))
            acc_val = new_acc

        return acc_val, block

    def _build_builtin_call(
        self,
        ir: MenaiIRCall,
        block: MenaiCFGBlock,
        scope: MenaiCFGScope,
        state: _FunctionState,
        tail: bool = False,
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        """
        Emit a builtin call.  Most builtins are never in tail position (they
        are opcode-backed primitives that return a value inline).

        The 'apply' builtin is special-cased: in non-tail position it emits
        MenaiCFGApplyInstr (lowered to APPLY); in tail position it emits a
        MenaiCFGTailApplyTerm (lowered to TAIL_APPLY), which is essential for
        tail-recursive functions that recurse via apply.
        """
        assert ir.builtin_name is not None

        # Evaluate all argument plans.
        arg_vals: List[MenaiCFGValue] = []
        for arg_plan in ir.arg_plans:
            arg_val, block = self._build_expr(arg_plan, block, scope, state, tail=False)
            arg_vals.append(arg_val)

        if ir.builtin_name == 'apply':
            # apply is always (apply func arg-list) — two args.
            assert len(arg_vals) == 2
            if tail:
                block.terminator = MenaiCFGTailApplyTerm(
                    func=arg_vals[0],
                    arg_list=arg_vals[1],
                )
                placeholder = state.new_value("tail_apply")
                return placeholder, block

            result = state.new_value("apply_result")
            block.instrs.append(MenaiCFGApplyInstr(
                result=result,
                func=arg_vals[0],
                arg_list=arg_vals[1],
            ))
            return result, block

        result = state.new_value(ir.builtin_name)
        block.instrs.append(MenaiCFGBuiltinInstr(
            result=result,
            op=ir.builtin_name,
            args=arg_vals,
        ))
        return result, block

    def _build_trace(
        self, ir: MenaiIRTrace, block: MenaiCFGBlock, scope: MenaiCFGScope, state: _FunctionState,
    ) -> Tuple[MenaiCFGValue, MenaiCFGBlock]:
        msg_vals: List[MenaiCFGValue] = []
        for msg_plan in ir.message_plans:
            msg_val, block = self._build_expr(msg_plan, block, scope, state, tail=False)
            msg_vals.append(msg_val)

        value_val, block = self._build_expr(ir.value_plan, block, scope, state, tail=False)

        result = state.new_value("trace_result")
        block.instrs.append(MenaiCFGTraceInstr(
            result=result,
            messages=msg_vals,
            value=value_val,
        ))
        return result, block

    def _link_predecessors(self, func: MenaiCFGFunction) -> None:
        """
        Populate the `predecessors` list on every block in `func`.

        Called once after all blocks in a function have been created and
        their terminators set.
        """
        for block in func.blocks:
            term = block.terminator
            if isinstance(term, MenaiCFGJumpTerm):
                term.target.predecessors.append(block)

            elif isinstance(term, MenaiCFGBranchTerm):
                term.true_block.predecessors.append(block)
                term.false_block.predecessors.append(block)

            # ReturnTerm, TailCallTerm, TailApplyTerm, SelfLoopTerm,
            # RaiseTerm have no successors.
