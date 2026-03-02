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
        """Add a binding to the current scope."""
        self.bindings[name] = index

    def get_binding(self, name: str) -> int | None:
        """Get the index of a binding, or None if not found."""
        return self.bindings.get(name)


@dataclass
class AnalysisContext:
    """
    Analysis context for IR building — tracks scopes for variable resolution.

    Slot indices are no longer allocated here; that is the addresser's job.
    The scope chain is used only to determine var_type ('local' vs 'global')
    and is_parent_ref for each variable reference.
    """
    scopes: List[CompilationScope] = field(default_factory=list)
    parent_ctx: 'AnalysisContext | None' = None
    current_binding_name: str | None = None  # Name of the binding currently being analysed.
    letrec_bound_names: Set[str] = field(default_factory=set)  # Names bound by any enclosing letrec
    current_letrec_names: Set[str] = field(default_factory=set)  # Names in the immediately enclosing
                                                                   # letrec group only.
    names: Set[str] = field(default_factory=set)

    def push_scope(self) -> None:
        """Enter a new lexical scope."""
        self.scopes.append(CompilationScope())

    def pop_scope(self) -> CompilationScope:
        """Exit current lexical scope."""
        return self.scopes.pop()

    def current_scope(self) -> CompilationScope:
        """Get current scope."""
        return self.scopes[-1]

    def add_name_to_scope(self, name: str) -> None:
        """
        Add a name to the current scope with a placeholder index of 0.

        The actual slot index is assigned by MenaiIRAddresser.  We only need
        the name to be present so that resolve_variable() can classify it as
        'local' rather than 'global'.
        """
        self.scopes[-1].add_binding(name, 0)

    def resolve_variable(self, name: str) -> Tuple[str, int, int]:
        """
        Resolve variable to (type, depth, index).

        depth is the number of lambda-frame boundaries crossed.
        index is a placeholder (0) — the real slot is assigned by the addresser.

        Returns:
            ('local', depth, 0) for local variables
            ('global', 0, 0) for global variables
        """
        for scope in reversed(self.scopes):
            if scope.get_binding(name) is not None:
                return ('local', 0, 0)

        if self.parent_ctx is not None:
            var_type, parent_depth, index = self.parent_ctx.resolve_variable(name)
            if var_type == 'local':
                return ('local', parent_depth + 1, 0)

            return (var_type, parent_depth, index)

        self.names.add(name)
        return ('global', 0, 0)

    def create_child_context(self) -> 'AnalysisContext':
        """Create a child context for nested lambda analysis."""
        child = AnalysisContext()
        child.parent_ctx = self
        child.letrec_bound_names = self.letrec_bound_names.copy()
        return child


class MenaiIRBuilder:
    """
    Builds intermediate representation (IR) from AST.

    Emits MenaiIRVariable nodes with depth=-1, index=-1 (unresolved sentinels).
    Slot allocation and max_locals computation are deferred entirely to
    MenaiIRAddresser, which runs once as the final pre-codegen step.
    """

    def __init__(self) -> None:
        """Initialize IR builder."""
        self._builtin_names: frozenset = frozenset(MenaiBuiltinRegistry.BUILTIN_OPCODE_ARITIES.keys())

    def build(self, expr: MenaiASTNode) -> MenaiIRExpr:
        """
        Build IR from an AST expression.

        Args:
            expr: AST expression (should already be desugared and optimized)

        Returns:
            IR tree ready for transformation passes and code generation
        """
        analysis_ctx = AnalysisContext()
        plan = self._analyze_expression(expr, analysis_ctx, in_tail_position=True)

        if self._needs_return_wrapper(plan):
            plan = MenaiIRReturn(value_plan=plan)

        return plan

    def _analyze_expression(self, expr: MenaiASTNode, ctx: AnalysisContext, in_tail_position: bool = False) -> MenaiIRExpr:
        """Analyze an expression and return a compilation plan."""

        expr_type = type(expr)

        if expr_type in (MenaiASTInteger, MenaiASTFloat, MenaiASTComplex, MenaiASTString):
            return MenaiIRConstant(value=expr.to_runtime_value())

        if expr_type is MenaiASTBoolean:
            return MenaiIRConstant(value=expr.to_runtime_value())

        if expr_type is MenaiASTNone:
            return MenaiIRConstant(value=expr.to_runtime_value())

        if expr_type is MenaiASTSymbol:
            return self._analyze_variable(cast(MenaiASTSymbol, expr).name, ctx)

        if expr_type is MenaiASTList:
            return self._analyze_list(cast(MenaiASTList, expr), ctx, in_tail_position)

        raise MenaiEvalError(
            message=f"Cannot analyze expression of type {type(expr).__name__}",
            received=str(expr)
        )

    def _analyze_variable(self, name: str, ctx: AnalysisContext) -> MenaiIRVariable:
        """Analyze a variable reference."""
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
        """Analyze a trace expression."""
        assert len(expr.elements) >= 3, "Trace expression should have at least 3 elements"
        messages = expr.elements[1:-1]
        return_expr = expr.elements[-1]
        message_plans = [self._analyze_expression(msg, ctx, in_tail_position=False) for msg in messages]
        value_plan = self._analyze_expression(return_expr, ctx, in_tail_position)
        return MenaiIRTrace(message_plans=message_plans, value_plan=value_plan)

    def _analyze_apply(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRCall:
        """Analyze an apply expression."""
        assert len(expr.elements) == 3, "Apply expression should have exactly 3 elements"
        _, func_expr, args_expr = expr.elements
        func_plan = self._analyze_expression(func_expr, ctx, in_tail_position=False)
        args_plan = self._analyze_expression(args_expr, ctx, in_tail_position=False)
        return MenaiIRCall(
            func_plan=func_plan,
            arg_plans=[func_plan, args_plan],
            is_tail_call=in_tail_position,
            is_builtin=True,
            builtin_name='apply'
        )

    def _needs_return_wrapper(self, plan: MenaiIRExpr) -> bool:
        """Check if a plan needs to be wrapped in a MenaiIRReturn."""
        if isinstance(plan, MenaiIRCall) and plan.is_tail_call:
            return False

        if isinstance(plan, MenaiIRIf) and plan.in_tail_position:
            return False

        if isinstance(plan, (MenaiIRLet, MenaiIRLetrec)):
            return self._needs_return_wrapper(plan.body_plan)

        if isinstance(plan, MenaiIRReturn):
            return False

        return True

    def _analyze_if(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRIf:
        """Analyze an if expression."""
        assert len(expr.elements) == 4, "If expression should have exactly 4 elements"

        _, condition, then_expr, else_expr = expr.elements

        # Negation elimination: (if (boolean-not cond) then else) → (if cond else then)
        if (isinstance(condition, MenaiASTList)
                and len(condition.elements) == 2
                and isinstance(condition.elements[0], MenaiASTSymbol)
                and condition.elements[0].name == 'boolean-not'):
            condition = condition.elements[1]
            then_expr, else_expr = else_expr, then_expr

        condition_plan = self._analyze_expression(condition, ctx, in_tail_position=False)
        then_plan = self._analyze_expression(then_expr, ctx, in_tail_position=in_tail_position)
        else_plan = self._analyze_expression(else_expr, ctx, in_tail_position=in_tail_position)

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

        Slot indices are not allocated here — MenaiIRAddresser handles that.
        We only add names to the scope so that resolve_variable() can classify
        them as 'local' for the body.
        """
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, MenaiASTList)

        ctx.push_scope()

        # Analyze all binding values in the outer scope (parallel let semantics).
        analyzed_bindings: List[Tuple[str, MenaiIRExpr]] = []
        for binding in bindings_list.elements:
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, MenaiASTSymbol)
            name = name_expr.name

            old_binding_name = ctx.current_binding_name
            ctx.current_binding_name = name
            value_plan = self._analyze_expression(value_expr, ctx, in_tail_position=False)
            ctx.current_binding_name = old_binding_name

            analyzed_bindings.append((name, value_plan))

        # Add all binding names to scope for the body.
        for name, _ in analyzed_bindings:
            ctx.add_name_to_scope(name)

        body_plan = self._analyze_expression(body, ctx, in_tail_position=in_tail_position)

        ctx.pop_scope()

        return MenaiIRLet(
            bindings=analyzed_bindings,
            body_plan=body_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_letrec(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRLetrec:
        """
        Analyze a letrec expression.

        All binding names are added to scope before analyzing binding values
        (mutual recursion).  Slot indices are not allocated here.
        """
        assert len(expr.elements) == 3, "Letrec expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, MenaiASTList)

        ctx.push_scope()

        # First pass: add all names to scope (so recursive references resolve).
        binding_pairs: List[Tuple[str, MenaiASTNode]] = []
        for binding in bindings_list.elements:
            assert isinstance(binding, MenaiASTList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, MenaiASTSymbol)
            name = name_expr.name
            ctx.add_name_to_scope(name)
            binding_pairs.append((name, value_expr))

        # Register all binding names as letrec-bound.
        all_names = [name for name, _ in binding_pairs]
        ctx.letrec_bound_names.update(all_names)
        ctx.current_letrec_names = set(all_names)

        # Second pass: analyze each binding value with full letrec context.
        binding_plans: List[Tuple[str, MenaiIRExpr]] = []
        for name, value_expr in binding_pairs:
            old_binding_name = ctx.current_binding_name
            ctx.current_binding_name = name
            value_plan = self._analyze_expression(value_expr, ctx, in_tail_position=False)
            ctx.current_binding_name = old_binding_name
            binding_plans.append((name, value_plan))

        body_plan = self._analyze_expression(body, ctx, in_tail_position=in_tail_position)

        ctx.current_letrec_names = set()
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
        assert isinstance(params_list, MenaiASTList)

        param_names = []
        is_variadic = False
        for param in params_list.elements:
            assert isinstance(param, MenaiASTSymbol)
            if param.name == '.':
                is_variadic = True
                continue
            param_names.append(param.name)

        bound_vars = set(param_names)
        free_vars = self._find_free_variables(body, bound_vars, ctx)

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

        lambda_ctx = ctx.create_child_context()
        lambda_ctx.push_scope()

        for param_name in param_names:
            lambda_ctx.add_name_to_scope(param_name)

        for free_var in sibling_free_vars + outer_free_vars:
            lambda_ctx.add_name_to_scope(free_var)

        body_plan = self._analyze_expression(body, lambda_ctx, in_tail_position=True)

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
            source_line=expr.line if (hasattr(expr, 'line') and expr.line is not None) else 0,
            source_file=expr.source_file if (hasattr(expr, 'source_file') and expr.source_file) else ""
        )

    def _analyze_function_call(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRCall:
        """Analyze a function call."""
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])

        func_type = type(func_expr)

        if func_type is MenaiASTSymbol and cast(MenaiASTSymbol, func_expr).name in self._builtin_names:
            builtin_name = cast(MenaiASTSymbol, func_expr).name
            arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]
            return MenaiIRCall(
                func_plan=MenaiIRVariable(name=builtin_name, var_type='global', depth=0, index=0),
                arg_plans=arg_plans,
                is_tail_call=False,
                is_builtin=True,
                builtin_name=builtin_name
            )

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
        """Find free variables in an expression."""
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

            var_type, _, _ = parent_ctx.resolve_variable(name)
            if var_type == 'local' and name not in seen:
                free.append(name)
                seen.add(name)

        elif expr_type is MenaiASTList:
            if cast(MenaiASTList, expr).is_empty():
                return

            first = cast(MenaiASTList, expr).first()
            first_type = type(first)

            if first_type is MenaiASTSymbol:
                if cast(MenaiASTSymbol, first).name == 'lambda':
                    if len(cast(MenaiASTList, expr).elements) >= 3:
                        nested_params = cast(MenaiASTList, expr).elements[1]
                        nested_body = cast(MenaiASTList, expr).elements[2]
                        nested_bound = bound_vars.copy()
                        if isinstance(nested_params, MenaiASTList):
                            for param in nested_params.elements:
                                if isinstance(param, MenaiASTSymbol):
                                    nested_bound.add(param.name)

                        self._collect_free_vars(nested_body, nested_bound, parent_ctx, free, seen)

                    return

                if cast(MenaiASTSymbol, first).name == 'let':
                    if len(cast(MenaiASTList, expr).elements) >= 3:
                        bindings_list = cast(MenaiASTList, expr).elements[1]
                        body = cast(MenaiASTList, expr).elements[2]
                        new_bound = bound_vars.copy()
                        if isinstance(bindings_list, MenaiASTList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, MenaiASTList) and len(binding.elements) >= 2:
                                    name_expr = binding.elements[0]
                                    if isinstance(name_expr, MenaiASTSymbol):
                                        new_bound.add(name_expr.name)

                        if isinstance(bindings_list, MenaiASTList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, MenaiASTList) and len(binding.elements) >= 2:
                                    value_expr = binding.elements[1]
                                    self._collect_free_vars(value_expr, bound_vars, parent_ctx, free, seen)

                        self._collect_free_vars(body, new_bound, parent_ctx, free, seen)

                    return

            for elem in cast(MenaiASTList, expr).elements:
                self._collect_free_vars(elem, bound_vars, parent_ctx, free, seen)
