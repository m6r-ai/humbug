"""Menai IR builder - compiles AST to IR."""

from typing import List, Dict, Tuple, Set, cast
from dataclasses import dataclass, field

from menai.menai_builtin_registry import MenaiBuiltinRegistry
from menai.menai_error import MenaiEvalError
from menai.menai_ir import (
    MenaiIRExpr, MenaiIRConstant, MenaiIRVariable, MenaiIRIf, MenaiIRLet, MenaiIRLetrec,
    MenaiIRLambda, MenaiIRCall, MenaiIRQuote, MenaiIRError, MenaiIREmptyList,
    MenaiIRReturn, MenaiIRTrace
)
from menai.menai_ast import (
    MenaiASTNode, MenaiASTInteger, MenaiASTFloat, MenaiASTComplex,
    MenaiASTString, MenaiASTBoolean, MenaiASTNone, MenaiASTSymbol, MenaiASTList
)


@dataclass
class CompilationScope:
    """
    Tracks variable bindings in a lexical scope.

    Maps variable names to their index within the scope.
    """
    bindings: Dict[str, int] = field(default_factory=dict)

    def add_binding(self, name: str, index: int) -> None:
        """
        Add a binding with the given index.

        Note: If a binding with the same name already exists, it will be
        overwritten with a new index. This is intentional to support shadowing.

        Args:
            name: Variable name
            index: Global index for this variable
        """
        self.bindings[name] = index

    def get_binding(self, name: str) -> int | None:
        """Get binding index, or None if not found."""
        return self.bindings.get(name)


@dataclass
class AnalysisContext:
    """
    Analysis context for Phase 1 - tracks scopes for variable resolution.

    This context does NOT emit bytecode or track constants/names.
    It only performs analysis and builds the compilation plan.
    """
    scopes: List[CompilationScope] = field(default_factory=list)
    parent_ctx: 'AnalysisContext | None' = None
    next_local_index: int = 0
    max_locals: int = 0
    current_binding_name: str | None = None  # Name of the binding currently being analysed.
                                              # Used for lambda binding_name (debuggability).
    letrec_bound_names: Set[str] = field(default_factory=set)  # Names bound by any enclosing letrec
    current_letrec_names: Set[str] = field(default_factory=set)  # Names in the *immediately* enclosing
                                                                  # letrec group only (not accumulated).
                                                                  # Used to partition sibling vs outer captures.
    # Track names for global resolution (we need to know what's a global vs local)
    # but we don't assign indices — that's for codegen.
    names: Set[str] = field(default_factory=set)

    def push_scope(self) -> None:
        """Enter a new lexical scope."""
        self.scopes.append(CompilationScope())
        # next_local_index continues from parent scope

    def pop_scope(self) -> CompilationScope:
        """Exit current lexical scope and reclaim local variable slots."""
        popped = self.scopes.pop()

        # Reset next_local_index to reclaim slots from the popped scope
        # Find the highest index still in use across remaining scopes
        if self.scopes:
            max_used = max((max(scope.bindings.values()) for scope in self.scopes if scope.bindings), default=-1)
            self.next_local_index = max_used + 1

        else:
            self.next_local_index = 0

        return popped

    def update_max_locals(self) -> None:
        """Update max locals based on current scope depth."""
        self.max_locals = max(self.max_locals, self.next_local_index)

    def allocate_local_index(self) -> int:
        """Allocate a new local variable index from the global counter."""
        index = self.next_local_index
        self.next_local_index += 1
        return index

    def current_scope(self) -> CompilationScope:
        """Get current scope."""
        return self.scopes[-1]

    def resolve_variable(self, name: str) -> Tuple[str, int, int]:
        """
        Resolve variable to (type, depth, index).

        Depth is the number of context boundaries (lambda frames) to cross.
        This is used for LOAD_PARENT_VAR to walk the parent frame chain.

        Returns:
            ('local', depth, index) for local variables (depth = context nesting level)
            ('global', 0, 0) for global variables (index assigned during codegen)
        """
        # Search from innermost to outermost scope in current context
        for scope in reversed(self.scopes):
            index = scope.get_binding(name)
            if index is not None:
                return ('local', 0, index)

        # Search parent contexts (each parent context is one frame boundary)
        if self.parent_ctx is not None:
            var_type, parent_depth, index = self.parent_ctx.resolve_variable(name)
            if var_type == 'local':
                # Found in parent context - increment depth
                return ('local', parent_depth + 1, index)
            return (var_type, parent_depth, index)

        # Not found in local scopes, must be global
        self.names.add(name)
        return ('global', 0, 0)

    def create_child_context(self) -> 'AnalysisContext':
        """Create a child context for nested lambda analysis."""
        child = AnalysisContext()
        child.parent_ctx = self
        child.letrec_bound_names = self.letrec_bound_names.copy()  # Inherit enclosing letrec names
        # current_letrec_names is NOT inherited — it is specific to the immediately enclosing letrec.
        return child


class MenaiIRBuilder:
    """
    Builds intermediate representation (IR) from AST.

    This performs semantic analysis and builds an IR tree that can be
    passed to the code generator. This is a pure transformation - no
    desugaring or optimization happens here (those are separate passes).
    """

    def __init__(self) -> None:
        """
        Initialize IR builder.
        """
        # All known builtin names — used to recognise builtin calls and mark
        # them is_builtin=True so the codegen emits direct opcodes instead of
        # a global lookup + CALL (which would recurse into the prelude lambda).
        self._builtin_names: frozenset = frozenset(MenaiBuiltinRegistry.BUILTIN_OPCODE_ARITIES.keys())

    def build(self, expr: MenaiASTNode) -> MenaiIRExpr:
        """
        Build IR from an AST expression.

        Args:
            expr: AST expression (should already be desugared and optimized)

        Returns:
            IR tree ready for code generation
        """
        analysis_ctx = AnalysisContext()
        plan = self._analyze_expression(expr, analysis_ctx, in_tail_position=True)

        # Wrap the top-level expression in a MenaiIRReturn if needed
        if self._needs_return_wrapper(plan):
            plan = MenaiIRReturn(value_plan=plan)

        return plan

    def _analyze_expression(self, expr: MenaiASTNode, ctx: AnalysisContext, in_tail_position: bool = False) -> MenaiIRExpr:
        """Analyze an expression and return a compilation plan (Phase 1)."""

        # Cache the type - check once instead of multiple isinstance() calls
        expr_type = type(expr)

        # Self-evaluating values (constants)
        if expr_type in (MenaiASTInteger, MenaiASTFloat, MenaiASTComplex, MenaiASTString):
            return MenaiIRConstant(value=expr.to_runtime_value())

        if expr_type is MenaiASTBoolean:
            return MenaiIRConstant(value=expr.to_runtime_value())

        if expr_type is MenaiASTNone:
            return MenaiIRConstant(value=expr.to_runtime_value())

        # Symbol (variable reference)
        if expr_type is MenaiASTSymbol:
            return self._analyze_variable(cast(MenaiASTSymbol, expr).name, ctx)

        # List (function call or special form)
        if expr_type is MenaiASTList:
            return self._analyze_list(cast(MenaiASTList, expr), ctx, in_tail_position)

        raise MenaiEvalError(
            message=f"Cannot analyze expression of type {type(expr).__name__}",
            received=str(expr)
        )

    def _analyze_variable(self, name: str, ctx: AnalysisContext) -> MenaiIRVariable:
        """Analyze a variable reference."""
        # resolve_variable() determines var_type and whether the name crosses
        # a lambda boundary (depth > 0).  depth and index are left as -1 —
        # MenaiIRAddresser fills them in before codegen.
        # is_parent_ref: crosses a lambda boundary AND is bound by an enclosing letrec.
        var_type, depth, _index = ctx.resolve_variable(name)
        is_parent_ref = (depth > 0) and (name in ctx.letrec_bound_names)
        return MenaiIRVariable(
            name=name,
            var_type=var_type,
            is_parent_ref=is_parent_ref,
        )

    def _analyze_list(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRExpr:
        """Analyze a list expression (function call or special form)."""
        if expr.is_empty():
            return MenaiIREmptyList()

        first = expr.first()
        first_type = type(first)

        # Check for special forms
        if first_type is MenaiASTSymbol:
            name = cast(MenaiASTSymbol, first).name

            if name == 'if':
                return self._analyze_if(expr, ctx, in_tail_position)

            if name == 'let':
                return self._analyze_let(expr, ctx, in_tail_position)

            if name == 'letrec':
                return self._analyze_letrec(expr, ctx, in_tail_position)

            if name == 'lambda':
                return self._analyze_lambda(expr, ctx)

            if name == 'quote':
                return self._analyze_quote(expr)

            if name == 'error':
                return self._analyze_error(expr)

            if name == 'trace':
                return self._analyze_trace(expr, ctx, in_tail_position)

            if name == 'apply':
                return self._analyze_apply(expr, ctx, in_tail_position)

        # Regular function call
        return self._analyze_function_call(expr, ctx, in_tail_position)

    def _analyze_quote(self, expr: MenaiASTList) -> MenaiIRQuote:
        """Analyze a quote expression."""
        assert len(expr.elements) == 2, "Quote expression should have exactly 2 elements"
        quoted = expr.elements[1].to_runtime_value()
        return MenaiIRQuote(quoted_value=quoted)

    def _analyze_error(self, expr: MenaiASTList) -> MenaiIRError:
        """Analyze an error expression."""
        assert len(expr.elements) == 2, "Error expression should have exactly 2 elements"
        message = expr.elements[1].to_runtime_value()
        return MenaiIRError(message=message)

    def _analyze_trace(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRTrace:
        """
        Analyze a trace expression.

        (trace msg1 msg2 ... msgN expr)
        """
        assert len(expr.elements) >= 3, "Trace expression should have at least 3 elements (trace msg expr)"

        # All elements except first (trace) and last (return expr) are messages
        messages = expr.elements[1:-1]
        return_expr = expr.elements[-1]

        # Analyze message expressions and return expression
        message_plans = [self._analyze_expression(msg, ctx, in_tail_position=False) for msg in messages]
        value_plan = self._analyze_expression(return_expr, ctx, in_tail_position)

        return MenaiIRTrace(message_plans=message_plans, value_plan=value_plan)

    def _analyze_apply(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRCall:
        """Analyze an apply expression: (apply f args)"""
        assert len(expr.elements) == 3, "Apply expression should have exactly 3 elements"

        _, func_expr, args_expr = expr.elements

        func_plan = self._analyze_expression(func_expr, ctx, in_tail_position=False)
        args_plan = self._analyze_expression(args_expr, ctx, in_tail_position=False)

        # arg_plans carries [func, arglist] in the order the codegen expects:
        # the apply codegen case iterates arg_plans to push both onto the stack.
        return MenaiIRCall(
            func_plan=func_plan,  # unused for builtins but kept for IR consistency
            arg_plans=[func_plan, args_plan],
            is_tail_call=in_tail_position,
            is_builtin=True,
            builtin_name='apply'
        )

    def _needs_return_wrapper(self, plan: MenaiIRExpr) -> bool:
        """
        Check if a plan needs to be wrapped in MenaiIRReturn.
        
        Returns False if the plan already handles returns (e.g., is a tail call
        or is an if expression that already wrapped its branches).
        """
        # Tail calls don't need return wrappers
        if isinstance(plan, MenaiIRCall) and plan.is_tail_call:
            return False

        # If expressions in tail position already wrap their branches
        if isinstance(plan, MenaiIRIf) and plan.in_tail_position:
            return False

        # Let/letrec: the body is what actually returns, so defer to the body
        if isinstance(plan, (MenaiIRLet, MenaiIRLetrec)):
            return self._needs_return_wrapper(plan.body_plan)

        # MenaiIRReturn already has a return
        if isinstance(plan, MenaiIRReturn):
            return False

        return True

    def _analyze_if(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRIf:
        """Analyze an if expression."""
        assert len(expr.elements) == 4, "If expression should have exactly 4 elements"

        _, condition, then_expr, else_expr = expr.elements

        # Negation elimination: (if (boolean-not cond) then else) → (if cond else then)
        #
        # If the condition is a call to boolean-not, we can drop the negation and
        # swap the branches instead, saving one BOOLEAN_NOT opcode per such branch.
        # We detect this at the AST level before analysis so the transformation is
        # free and requires no IR changes.
        if (isinstance(condition, MenaiASTList)
                and len(condition.elements) == 2
                and isinstance(condition.elements[0], MenaiASTSymbol)
                and condition.elements[0].name == 'boolean-not'):
            condition = condition.elements[1]
            then_expr, else_expr = else_expr, then_expr

        # Analyze all three sub-expressions
        condition_plan = self._analyze_expression(condition, ctx, in_tail_position=False)
        then_plan = self._analyze_expression(then_expr, ctx, in_tail_position=in_tail_position)
        else_plan = self._analyze_expression(else_expr, ctx, in_tail_position=in_tail_position)

        # Wrap branches in MenaiIRReturn when in tail position
        # (unless they're already tail calls that don't need a return)
        if in_tail_position:
            if self._needs_return_wrapper(then_plan):
                then_plan = MenaiIRReturn(value_plan=then_plan)

            if self._needs_return_wrapper(else_plan):
                else_plan = MenaiIRReturn(value_plan=else_plan)

        return MenaiIRIf(
            condition_plan=condition_plan,
            then_plan=then_plan,
            else_plan=else_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_let(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRLet:
        """
        Analyze a let expression with parallel binding semantics.

        In parallel let, all binding values are evaluated BEFORE any bindings
        are added to the scope. This means bindings cannot reference each other.
        For sequential binding semantics, use let*.
        """
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, MenaiASTList), "Binding list should be a list"

        # Push new scope
        ctx.push_scope()

        # PHASE 1: Analyze all binding values WITHOUT allocating indices
        # This implements parallel binding semantics - values see outer scope
        # Track the maximum next_local_index after analyzing each binding value
        # Sibling expressions can reuse indices, so we need the MAX, not the sum
        binding_start_index = ctx.next_local_index
        max_locals_after_values = ctx.max_locals

        analyzed_bindings = []
        for binding in bindings_list.elements:
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, MenaiASTSymbol)

            name = name_expr.name

            # Reset next_local_index to binding_start_index before each sibling
            # This allows siblings to reuse the same index range
            ctx.next_local_index = binding_start_index
            # Also reset max_locals to allow nested expressions to allocate from binding_start_index
            ctx.max_locals = binding_start_index

            # Set binding name so nested lambdas get a meaningful name for
            # debuggability and tail-recursion detection.
            old_binding_name = ctx.current_binding_name
            ctx.current_binding_name = name
            value_plan = self._analyze_expression(value_expr, ctx, in_tail_position=False)
            ctx.current_binding_name = old_binding_name

            analyzed_bindings.append((name, value_plan))

            # Track the maximum max_locals reached by any sibling
            max_locals_after_values = max(max_locals_after_values, ctx.max_locals)

        # PHASE 2: NOW allocate indices and add bindings to scope
        # Start allocating after the maximum index used by any binding value
        binding_start_index = max_locals_after_values

        # Restore max_locals to reflect the maximum across all siblings
        ctx.max_locals = max_locals_after_values
        binding_plans = []
        for i, (name, value_plan) in enumerate(analyzed_bindings):
            var_index = binding_start_index + i
            binding_plans.append((name, value_plan, var_index))
            ctx.current_scope().add_binding(name, var_index)

        # Update next_local_index to reflect the indices we just used
        ctx.next_local_index = binding_start_index + len(analyzed_bindings)
        ctx.update_max_locals()

        # Analyze body
        body_plan = self._analyze_expression(body, ctx, in_tail_position=in_tail_position)

        # Pop scope
        ctx.pop_scope()

        return MenaiIRLet(
            bindings=binding_plans,
            body_plan=body_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_letrec(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRLetrec:
        """Analyze a letrec expression.

        After letrec splitting in the desugarer, every letrec arriving here is
        a single fully-mutually-recursive group of lambdas.  All bindings are
        therefore recursive siblings of each other — no dependency analysis is
        needed.
        """
        assert len(expr.elements) == 3, "Letrec expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, MenaiASTList), "Binding list should be a list"

        # Push new scope
        ctx.push_scope()

        # First pass: allocate slots and add all names to scope so that
        # recursive references within binding values resolve correctly.
        binding_pairs = []
        for binding in bindings_list.elements:
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, MenaiASTSymbol)

            name = name_expr.name
            var_index = ctx.allocate_local_index()
            ctx.current_scope().add_binding(name, var_index)
            binding_pairs.append((name, value_expr, var_index))

        ctx.update_max_locals()

        # Register all binding names as letrec-bound so that nested lambdas
        # can identify recursive back-edges via letrec_bound_names.
        all_names = [name for name, _, _ in binding_pairs]
        ctx.letrec_bound_names.update(all_names)

        # Record the immediate letrec group so lambdas can partition sibling vs outer captures.
        ctx.current_letrec_names = set(all_names)

        # Second pass: analyze each binding value with full letrec context.
        binding_plans = []
        for name, value_expr, var_index in binding_pairs:
            old_binding_name = ctx.current_binding_name
            ctx.current_binding_name = name
            value_plan = self._analyze_expression(value_expr, ctx, in_tail_position=False)
            ctx.current_binding_name = old_binding_name
            binding_plans.append((name, value_plan, var_index))

        # Analyze body
        body_plan = self._analyze_expression(body, ctx, in_tail_position=in_tail_position)

        # Clear current_letrec_names — we have left the immediate letrec scope.
        ctx.current_letrec_names = set()

        # Pop scope
        ctx.pop_scope()

        return MenaiIRLetrec(
            bindings=binding_plans,
            body_plan=body_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_lambda(self, expr: MenaiASTList, ctx: AnalysisContext) -> MenaiIRLambda:
        """Analyze a lambda expression."""
        assert len(expr.elements) == 3, "Lambda expression should have exactly 3 elements"

        _, params_list, body = expr.elements
        assert isinstance(params_list, MenaiASTList), "Parameter list should be a list"

        # Extract parameter names, stripping the dot sentinel.
        # The semantic analyser guarantees the dot (if present) is second-to-last,
        # followed by exactly one rest parameter symbol.
        param_names = []
        is_variadic = False
        for param in params_list.elements:
            assert isinstance(param, MenaiASTSymbol)
            if param.name == '.':
                is_variadic = True
                continue
            param_names.append(param.name)

        # Find free variables
        bound_vars = set(param_names)
        free_vars = self._find_free_variables(body, bound_vars, ctx)

        # Partition free variables into sibling captures (from the immediately
        # enclosing letrec group) and outer captures (everything else).
        # This distinction is used by the lambda lifter to implement the
        # known-call optimisation for letrec groups.
        sibling_names = ctx.current_letrec_names
        sibling_free_vars: List[str] = [fv for fv in free_vars if fv in sibling_names]
        outer_free_vars:   List[str] = [fv for fv in free_vars if fv not in sibling_names]

        sibling_free_var_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(name=fv, var_type='local', is_parent_ref=False)
            for fv in sibling_free_vars
        ]
        outer_free_var_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(name=fv, var_type='local', is_parent_ref=False)
            for fv in outer_free_vars
        ]

        # Create child context for lambda body analysis
        lambda_ctx = ctx.create_child_context()
        lambda_ctx.push_scope()

        # Add parameters to lambda scope
        for param_name in param_names:
            param_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(param_name, param_index)

        # Add all free vars to lambda scope as captured closure slots
        # Sibling captures first (slots N..N+S-1), then outer (slots N+S..N+S+O-1).
        for free_var in sibling_free_vars + outer_free_vars:
            free_var_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(free_var, free_var_index)

        lambda_ctx.update_max_locals()

        # Analyze lambda body (in tail position)
        body_plan = self._analyze_expression(body, lambda_ctx, in_tail_position=True)

        # Wrap the lambda body in a MenaiIRReturn if needed
        if self._needs_return_wrapper(body_plan):
            body_plan = MenaiIRReturn(value_plan=body_plan)

        lambda_ctx.pop_scope()

        return MenaiIRLambda(
            params=param_names,
            body_plan=body_plan,
            sibling_free_vars=sibling_free_vars,
            sibling_free_var_plans=sibling_free_var_plans,
            outer_free_vars=outer_free_vars,
            outer_free_var_plans=outer_free_var_plans,
            param_count=len(param_names),
            is_variadic=is_variadic,
            binding_name=ctx.current_binding_name,
            max_locals=lambda_ctx.max_locals,
            source_line=expr.line if (hasattr(expr, 'line') and expr.line is not None) else 0,
            source_file=expr.source_file if (hasattr(expr, 'source_file') and expr.source_file) else ""
        )

    def _analyze_function_call(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRCall:
        """Analyze a function call."""
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])

        func_type = type(func_expr)

        # Check if calling a known builtin
        if func_type is MenaiASTSymbol and cast(MenaiASTSymbol, func_expr).name in self._builtin_names:
            builtin_name = cast(MenaiASTSymbol, func_expr).name

            # Analyze arguments
            arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]

            return MenaiIRCall(
                func_plan=MenaiIRVariable(name=builtin_name, var_type='global', depth=0, index=0),
                arg_plans=arg_plans,
                is_tail_call=False,
                is_builtin=True,
                builtin_name=builtin_name
            )

        # Analyze function and arguments — always use the real callee reference.
        # Self-recursive tail call optimisation (JUMP 0) is detected later in
        # the codegen, after all structural IR transformations are complete.
        func_plan: MenaiIRExpr = self._analyze_expression(func_expr, ctx, in_tail_position=False)
        arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]

        return MenaiIRCall(
            func_plan=func_plan,
            arg_plans=arg_plans,
            is_tail_call=in_tail_position,
            is_builtin=False,
            builtin_name=None
        )

    def _find_free_variables(self, expr: MenaiASTNode, bound_vars: Set[str], parent_ctx: AnalysisContext) -> List[str]:
        """
        Find free variables in an expression.

        Free variables are those that are:
        - Referenced in the expression
        - Not in bound_vars (parameters)
        - Not globals
        - Defined in parent scopes
        """
        free: List[str] = []
        self._collect_free_vars(expr, bound_vars, parent_ctx, free, set())
        return free

    def _collect_free_vars(
        self,
        expr: MenaiASTNode,
        bound_vars: Set[str],
        parent_ctx: AnalysisContext,
        free: List[str],
        seen: Set[str]
    ) -> None:
        """Recursively collect free variables."""
        expr_type = type(expr)

        if expr_type is MenaiASTSymbol:
            name = cast(MenaiASTSymbol, expr).name
            if name in seen or name in bound_vars:
                return

            # Check if it's defined in parent scopes
            var_type, _, _ = parent_ctx.resolve_variable(name)
            if var_type == 'local' and name not in seen:
                free.append(name)
                seen.add(name)

        elif expr_type is MenaiASTList:
            if cast(MenaiASTList, expr).is_empty():
                return

            first = cast(MenaiASTList, expr).first()
            first_type = type(first)

            # Handle special forms that bind variables
            if first_type is MenaiASTSymbol:
                if cast(MenaiASTSymbol, first).name == 'lambda':
                    # Nested lambda: we need to find what free variables it uses
                    # that come from outer scopes, so the parent lambda can capture them.
                    # The nested lambda will then capture them from the parent.
                    if len(cast(MenaiASTList, expr).elements) >= 3:
                        nested_params = cast(MenaiASTList, expr).elements[1]
                        nested_body = cast(MenaiASTList, expr).elements[2]

                        # Get parameter names from nested lambda
                        nested_bound = bound_vars.copy()
                        if isinstance(nested_params, MenaiASTList):
                            for param in nested_params.elements:
                                if isinstance(param, MenaiASTSymbol):
                                    nested_bound.add(param.name)

                        # Find free variables in nested lambda's body
                        # These are variables the nested lambda needs, which might come
                        # from the parent lambda or from even outer scopes
                        self._collect_free_vars(nested_body, nested_bound, parent_ctx, free, seen)

                    # Don't recurse into the lambda's parameters or other parts
                    return

                if cast(MenaiASTSymbol, first).name == 'let':
                    # Let bindings create new bound variables
                    # Extract binding names and recurse with updated bound_vars
                    if len(cast(MenaiASTList, expr).elements) >= 3:
                        bindings_list = cast(MenaiASTList, expr).elements[1]
                        body = cast(MenaiASTList, expr).elements[2]

                        # Collect let binding names
                        new_bound = bound_vars.copy()
                        if isinstance(bindings_list, MenaiASTList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, MenaiASTList) and len(binding.elements) >= 2:
                                    name_expr = binding.elements[0]
                                    if isinstance(name_expr, MenaiASTSymbol):
                                        new_bound.add(name_expr.name)

                        # Recurse into binding values first (to find free vars in lambda definitions)
                        if isinstance(bindings_list, MenaiASTList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, MenaiASTList) and len(binding.elements) >= 2:
                                    value_expr = binding.elements[1]
                                    # Use original bound_vars, not new_bound, because bindings can't reference each other yet
                                    self._collect_free_vars(value_expr, bound_vars, parent_ctx, free, seen)

                        # Recurse into let body with new bound variables
                        self._collect_free_vars(body, new_bound, parent_ctx, free, seen)
                    return

                # Note: We don't need special handling for 'letrec' here.
                # Unlike 'let', letrec bindings are already in scope when we analyze them
                # (they're added to the parent context before analyzing binding values).
                # So when we recurse into letrec bodies, resolve_variable() will correctly
                # find letrec bindings as locals in the parent context, and they won't be
                # added to the free variables list.

            # Recursively check all elements
            for elem in cast(MenaiASTList, expr).elements:
                self._collect_free_vars(elem, bound_vars, parent_ctx, free, seen)
