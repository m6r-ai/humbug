"""Menai IR builder - compiles AST to IR."""

from typing import List, Dict, Tuple, Set, cast
from dataclasses import dataclass, field

from menai.menai_bytecode import BUILTIN_OPCODE_MAP
from menai.menai_error import MenaiEvalError
from menai.menai_ir import (
    MenaiIRExpr, MenaiIRConstant, MenaiIRVariable, MenaiIRIf, MenaiIRLet, MenaiIRLetrec,
    MenaiIRLambda, MenaiIRCall, MenaiIRQuote, MenaiIRError, MenaiIREmptyList,
    MenaiIRReturn, MenaiIRTrace, MenaiIRBuildList, MenaiIRBuildDict, MenaiIRBuildSet,
    MenaiIRBuildStruct
)
from menai.menai_ast import (
    MenaiASTNode, MenaiASTInteger, MenaiASTFloat, MenaiASTComplex,
    MenaiASTString, MenaiASTBoolean, MenaiASTNone, MenaiASTSymbol, MenaiASTList, MenaiASTListLiteral,
    MenaiASTDict, MenaiASTSet, MenaiASTStruct
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

    The scope chain is used only to determine var_type ('local' vs 'global')
    for each variable reference.
    """
    scopes: List[CompilationScope] = field(default_factory=list)
    parent_ctx: 'AnalysisContext | None' = None
    current_binding_name: str | None = None  # Name of the binding currently being analysed.
    current_letrec_names: Set[str] = field(default_factory=set)  # Names in the immediately enclosing
                                                                   # letrec group only.
    names: Set[str] = field(default_factory=set)
    free_vars: List[str] = field(default_factory=list)   # Free variables discovered during analysis,
                                                          # in order of first encounter.
    free_vars_seen: Set[str] = field(default_factory=set) # Companion set for O(1) duplicate checks.

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

        We only need the name to be present so that resolve_variable() can classify it as
        'local' rather than 'global'.
        """
        self.scopes[-1].add_binding(name, 0)

    def resolve_variable(self, name: str) -> str:
        """
        Resolve variable name to its type: 'local' or 'global'.

        Returns:
            'local' if the name is bound in any enclosing scope.
            'global' if not found locally (resolved at runtime from the environment).
        """
        for scope in reversed(self.scopes):
            if scope.get_binding(name) is not None:
                return 'local'

        if self.parent_ctx is not None:
            result = self.parent_ctx.resolve_variable(name)
            if result == 'local' and name not in self.free_vars_seen:
                self.free_vars.append(name)
                self.free_vars_seen.add(name)
            return result

        self.names.add(name)
        return 'global'

    def create_child_context(self) -> 'AnalysisContext':
        """Create a child context for nested lambda analysis."""
        child = AnalysisContext()
        child.parent_ctx = self
        return child


class MenaiIRBuilder:
    """
    Builds intermediate representation (IR) from AST.

    Emits MenaiIRVariable nodes carrying only name and var_type ('local' or
    'global').  Slot allocation is handled downstream by MenaiCFGBuilder.
    """

    def __init__(self) -> None:
        """Initialize IR builder."""
        # Only $-prefixed names are treated as opcode-backed builtins by the IR
        # builder.  Public names (integer+, float=?, etc.) are prelude functions
        # and resolve as globals.
        # Exceptions: 'list', 'dict', and 'set' are variadic BUILD_OPs intercepted here
        # to emit MenaiIRBuildList / MenaiIRBuildDict / MenaiIRBuildSet flat nodes.  They are not
        # in BUILTIN_OPCODE_MAP (no fixed arity) and cannot be $-prefixed.
        self._builtin_names: frozenset = frozenset('$' + name for name in BUILTIN_OPCODE_MAP)
        self._builtin_names |= frozenset({'list', 'dict', 'set'})

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
            list_expr = cast(MenaiASTList, expr)
            return self._analyze_list(list_expr, ctx, in_tail_position)

        if expr_type is MenaiASTListLiteral:
            return MenaiIRConstant(value=cast(MenaiASTListLiteral, expr).to_runtime_value())

        if expr_type is MenaiASTSet:
            return MenaiIRConstant(value=cast(MenaiASTSet, expr).to_runtime_value())

        if expr_type is MenaiASTDict:
            return MenaiIRConstant(value=cast(MenaiASTDict, expr).to_runtime_value())

        if expr_type is MenaiASTStruct:
            return MenaiIRConstant(value=cast(MenaiASTStruct, expr).to_runtime_value())

        raise MenaiEvalError(
            message=f"Cannot analyze expression of type {type(expr).__name__}",
            received=str(expr)
        )

    def _analyze_variable(self, name: str, ctx: AnalysisContext) -> MenaiIRVariable:
        """Analyze a variable reference."""
        return MenaiIRVariable(name=name, var_type=ctx.resolve_variable(name))

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
                return self._analyze_error(expr, ctx)

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

    def _analyze_error(self, expr: MenaiASTList, ctx: AnalysisContext) -> MenaiIRError:
        """Analyze an error expression."""
        assert len(expr.elements) == 2, "Error expression should have exactly 2 elements"
        message_plan = self._analyze_expression(expr.elements[1], ctx, in_tail_position=False)
        return MenaiIRError(message=message_plan)

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

        if isinstance(plan, MenaiIRBuildList):
            return True

        if isinstance(plan, MenaiIRBuildDict):
            return True

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

        lambda_ctx = ctx.create_child_context()
        lambda_ctx.push_scope()

        for param_name in param_names:
            lambda_ctx.add_name_to_scope(param_name)

        body_plan = self._analyze_expression(body, lambda_ctx, in_tail_position=True)

        if self._needs_return_wrapper(body_plan):
            body_plan = MenaiIRReturn(value_plan=body_plan)

        lambda_ctx.pop_scope()

        sibling_names = ctx.current_letrec_names
        free_vars = lambda_ctx.free_vars
        sibling_free_vars: List[str] = [fv for fv in free_vars if fv in sibling_names]
        outer_free_vars:   List[str] = [fv for fv in free_vars if fv not in sibling_names]

        sibling_free_var_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(name=fv, var_type='local')
            for fv in sibling_free_vars
        ]
        outer_free_var_plans: List[MenaiIRExpr] = [
            MenaiIRVariable(name=fv, var_type='local')
            for fv in outer_free_vars
        ]

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

    def _analyze_function_call(self, expr: MenaiASTList, ctx: AnalysisContext, in_tail_position: bool) -> MenaiIRExpr:
        """Analyze a function call."""
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])

        func_type = type(func_expr)

        # (MenaiASTStruct field1 field2 ...) — struct constructor call.
        # The desugarer places the MenaiASTStruct node directly in function position.
        if func_type is MenaiASTStruct:
            struct_node = cast(MenaiASTStruct, func_expr)
            field_plans = [
                self._analyze_expression(arg, ctx, in_tail_position=False)
                for arg in arg_exprs
            ]
            return MenaiIRBuildStruct(struct_type=struct_node.to_runtime_value(), field_plans=field_plans)

        if func_type is MenaiASTSymbol and cast(MenaiASTSymbol, func_expr).name in self._builtin_names:
            dollar_name = cast(MenaiASTSymbol, func_expr).name
            # (list e1 ... eN) — emit a flat MenaiIRBuildList node.
            if dollar_name == 'list':
                element_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]
                return MenaiIRBuildList(element_plans=element_plans)

            # (dict k1 v1 k2 v2 ...) — emit a flat MenaiIRBuildDict node.
            # The semantic analyser guarantees an even argument count.
            if dollar_name == 'dict':
                assert len(arg_exprs) % 2 == 0, "dict: odd arg count should have been caught by semantic analyser"
                pair_plans = [
                    (self._analyze_expression(arg_exprs[i], ctx, in_tail_position=False),
                     self._analyze_expression(arg_exprs[i + 1], ctx, in_tail_position=False))
                    for i in range(0, len(arg_exprs), 2)
                ]
                return MenaiIRBuildDict(pair_plans=pair_plans)

            # (set e1 ... eN) — emit a flat MenaiIRBuildSet node.
            if dollar_name == 'set':
                element_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]
                return MenaiIRBuildSet(element_plans=element_plans)

            # Strip the $ prefix — the rest of the pipeline (codegen etc.)
            # uses the bare opcode name.
            # 'list', 'dict', and 'set' are plain builtin names (no $ prefix).
            builtin_name = dollar_name[1:] if dollar_name.startswith('$') else dollar_name
            arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]
            return MenaiIRCall(
                func_plan=MenaiIRVariable(name=dollar_name, var_type='global'),
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
