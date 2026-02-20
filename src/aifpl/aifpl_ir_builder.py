"""AIFPL IR builder - compiles AST to IR."""

from typing import List, Dict, Tuple, Set, cast
from dataclasses import dataclass, field

from aifpl.aifpl_builtin_registry import AIFPLBuiltinRegistry
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_ir import (
    AIFPLIRExpr, AIFPLIRConstant, AIFPLIRVariable, AIFPLIRIf, AIFPLIRLet, AIFPLIRLetrec,
    AIFPLIRLambda, AIFPLIRCall, AIFPLIRQuote, AIFPLIRError, AIFPLIREmptyList,
    AIFPLIRAnd, AIFPLIROr, AIFPLIRReturn, AIFPLIRTrace
)
from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer
from aifpl.aifpl_ast import (
    AIFPLASTNode, AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex,
    AIFPLASTString, AIFPLASTBoolean, AIFPLASTSymbol, AIFPLASTList
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
    current_function_name: str | None = None
    current_binding_name: str | None = None  # Name of the binding being analyzed (for lambda naming)
    sibling_bindings: List[str] = field(default_factory=list)
    # Track names for global resolution (we need to know what's a global vs local)
    # But we don't assign indices - that's for codegen
    names: Set[str] = field(default_factory=set)
    parent_ref_names: Set[str] = field(default_factory=set)  # Names that are parent references (recursive bindings)

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
        child.sibling_bindings = []
        child.parent_ref_names = self.parent_ref_names.copy()  # Inherit parent refs
        return child


class AIFPLIRBuilder:
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
        # a global lookup + CALL_FUNCTION (which would recurse into the prelude lambda).
        self._builtin_names: frozenset = frozenset(AIFPLBuiltinRegistry.ARITY_TABLE.keys())

    def build(self, expr: AIFPLASTNode) -> AIFPLIRExpr:
        """
        Build IR from an AST expression.

        Args:
            expr: AST expression (should already be desugared and optimized)

        Returns:
            IR tree ready for code generation
        """
        analysis_ctx = AnalysisContext()
        plan = self._analyze_expression(expr, analysis_ctx, in_tail_position=True)

        # Wrap the top-level expression in a AIFPLIRReturn if needed
        if self._needs_return_wrapper(plan):
            plan = AIFPLIRReturn(value_plan=plan)

        return plan

    def _analyze_expression(self, expr: AIFPLASTNode, ctx: AnalysisContext, in_tail_position: bool = False) -> AIFPLIRExpr:
        """Analyze an expression and return a compilation plan (Phase 1)."""

        # Cache the type - check once instead of multiple isinstance() calls
        expr_type = type(expr)

        # Self-evaluating values (constants)
        if expr_type in (AIFPLASTInteger, AIFPLASTFloat, AIFPLASTComplex, AIFPLASTString):
            return AIFPLIRConstant(value=expr.to_runtime_value())

        if expr_type is AIFPLASTBoolean:
            return AIFPLIRConstant(value=expr.to_runtime_value())

        # Symbol (variable reference)
        if expr_type is AIFPLASTSymbol:
            return self._analyze_variable(cast(AIFPLASTSymbol, expr).name, ctx)

        # List (function call or special form)
        if expr_type is AIFPLASTList:
            return self._analyze_list(cast(AIFPLASTList, expr), ctx, in_tail_position)

        raise AIFPLEvalError(
            message=f"Cannot analyze expression of type {type(expr).__name__}",
            received=str(expr)
        )

    def _analyze_variable(self, name: str, ctx: AnalysisContext) -> AIFPLIRVariable:
        """Analyze a variable reference."""
        var_type, depth, index = ctx.resolve_variable(name)
        # Check if this is a parent reference (recursive binding)
        # Only use parent ref if it's from a parent context (depth > 0) and is a recursive binding
        is_parent_ref = (depth > 0) and (name in ctx.parent_ref_names)
        return AIFPLIRVariable(
            name=name,
            var_type=var_type,
            depth=depth,
            index=index,
            is_parent_ref=is_parent_ref
        )

    def _analyze_list(self, expr: AIFPLASTList, ctx: AnalysisContext, in_tail_position: bool) -> AIFPLIRExpr:
        """Analyze a list expression (function call or special form)."""
        if expr.is_empty():
            return AIFPLIREmptyList()

        first = expr.first()
        first_type = type(first)

        # Check for special forms
        if first_type is AIFPLASTSymbol:
            name = cast(AIFPLASTSymbol, first).name

            if name == 'if':
                return self._analyze_if(expr, ctx, in_tail_position)

            if name == 'let':
                return self._analyze_let(expr, ctx, in_tail_position)

            if name == 'letrec':
                return self._analyze_letrec(expr, ctx, in_tail_position)

            if name == 'lambda':
                return self._analyze_lambda(expr, ctx)

            if name == 'and':
                return self._analyze_and(expr, ctx)

            if name == 'or':
                return self._analyze_or(expr, ctx)

            if name == 'quote':
                return self._analyze_quote(expr)

            if name == 'error':
                return self._analyze_error(expr)

            if name == 'trace':
                return self._analyze_trace(expr, ctx, in_tail_position)

        # Regular function call
        return self._analyze_function_call(expr, ctx, in_tail_position)

    def _analyze_quote(self, expr: AIFPLASTList) -> AIFPLIRQuote:
        """Analyze a quote expression."""
        assert len(expr.elements) == 2, "Quote expression should have exactly 2 elements"
        quoted = expr.elements[1].to_runtime_value()
        return AIFPLIRQuote(quoted_value=quoted)

    def _analyze_error(self, expr: AIFPLASTList) -> AIFPLIRError:
        """Analyze an error expression."""
        assert len(expr.elements) == 2, "Error expression should have exactly 2 elements"
        message = expr.elements[1].to_runtime_value()
        return AIFPLIRError(message=message)

    def _analyze_trace(self, expr: AIFPLASTList, ctx: AnalysisContext, in_tail_position: bool) -> AIFPLIRTrace:
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

        return AIFPLIRTrace(message_plans=message_plans, value_plan=value_plan)

    def _needs_return_wrapper(self, plan: AIFPLIRExpr) -> bool:
        """
        Check if a plan needs to be wrapped in AIFPLIRReturn.
        
        Returns False if the plan already handles returns (e.g., is a tail call
        or is an if expression that already wrapped its branches).
        """
        # Tail calls don't need return wrappers
        if isinstance(plan, AIFPLIRCall) and plan.is_tail_call:
            return False

        # If expressions in tail position already wrap their branches
        if isinstance(plan, AIFPLIRIf) and plan.in_tail_position:
            return False

        return True

    def _analyze_if(self, expr: AIFPLASTList, ctx: AnalysisContext, in_tail_position: bool) -> AIFPLIRIf:
        """Analyze an if expression."""
        assert len(expr.elements) == 4, "If expression should have exactly 4 elements"

        _, condition, then_expr, else_expr = expr.elements

        # Analyze all three sub-expressions
        condition_plan = self._analyze_expression(condition, ctx, in_tail_position=False)
        then_plan = self._analyze_expression(then_expr, ctx, in_tail_position=in_tail_position)
        else_plan = self._analyze_expression(else_expr, ctx, in_tail_position=in_tail_position)

        # Wrap branches in AIFPLIRReturn when in tail position
        # (unless they're already tail calls that don't need a return)
        if in_tail_position:
            if self._needs_return_wrapper(then_plan):
                then_plan = AIFPLIRReturn(value_plan=then_plan)

            if self._needs_return_wrapper(else_plan):
                else_plan = AIFPLIRReturn(value_plan=else_plan)

        return AIFPLIRIf(
            condition_plan=condition_plan,
            then_plan=then_plan,
            else_plan=else_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_and(self, expr: AIFPLASTList, ctx: AnalysisContext) -> AIFPLIRAnd:
        """Analyze an and expression."""
        args = list(expr.elements[1:])
        arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in args]
        return AIFPLIRAnd(arg_plans=arg_plans)

    def _analyze_or(self, expr: AIFPLASTList, ctx: AnalysisContext) -> AIFPLIROr:
        """Analyze an or expression."""
        args = list(expr.elements[1:])
        arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in args]
        return AIFPLIROr(arg_plans=arg_plans)

    def _analyze_let(self, expr: AIFPLASTList, ctx: AnalysisContext, in_tail_position: bool) -> AIFPLIRLet:
        """
        Analyze a let expression with parallel binding semantics.

        In parallel let, all binding values are evaluated BEFORE any bindings
        are added to the scope. This means bindings cannot reference each other.
        For sequential binding semantics, use let*.
        """
        assert len(expr.elements) == 3, "Let expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, AIFPLASTList), "Binding list should be a list"

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
            assert isinstance(binding, AIFPLASTList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, AIFPLASTSymbol)

            name = name_expr.name

            # Reset next_local_index to binding_start_index before each sibling
            # This allows siblings to reuse the same index range
            ctx.next_local_index = binding_start_index
            # Also reset max_locals to allow nested expressions to allocate from binding_start_index
            ctx.max_locals = binding_start_index

            # Set binding name for better debugging/disassembly
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

        return AIFPLIRLet(
            bindings=binding_plans,
            body_plan=body_plan,
            in_tail_position=in_tail_position
        )

    def _analyze_letrec(self, expr: AIFPLASTList, ctx: AnalysisContext, in_tail_position: bool) -> AIFPLIRLetrec:
        """Analyze a letrec expression."""
        assert len(expr.elements) == 3, "Letrec expression should have exactly 3 elements"

        _, bindings_list, body = expr.elements
        assert isinstance(bindings_list, AIFPLASTList), "Binding list should be a list"

        # Push new scope
        ctx.push_scope()

        # First pass: Add all binding names to scope (for recursive references)
        binding_pairs = []
        for binding in bindings_list.elements:
            assert isinstance(binding, AIFPLASTList) and len(binding.elements) == 2
            name_expr, value_expr = binding.elements
            assert isinstance(name_expr, AIFPLASTSymbol)

            name = name_expr.name
            var_index = ctx.allocate_local_index()
            ctx.current_scope().add_binding(name, var_index)
            binding_pairs.append((name, value_expr, var_index))

        ctx.update_max_locals()

        # Analyze dependencies
        analyzer = AIFPLDependencyAnalyzer()
        binding_info = [(name, value_expr) for name, value_expr, _ in binding_pairs]
        binding_groups = analyzer.analyze_letrec_bindings(binding_info)

        # Determine which bindings are recursive
        recursive_bindings = set()
        for group in binding_groups:
            if group.is_recursive:
                recursive_bindings.update(group.names)

        # Add recursive bindings to parent_ref_names so nested lambdas know about them
        ctx.parent_ref_names.update(recursive_bindings)

        # Second pass: Analyze binding values in topological order
        # The binding_groups are already in topological order from the analyzer
        binding_plans = []

        # Create a map from name to (value_expr, var_index)
        binding_map = {name: (value_expr, var_index) for name, value_expr, var_index in binding_pairs}

        # Process groups in topological order
        for group in binding_groups:
            for name in group.names:
                value_expr, var_index = binding_map[name]

                # Set context for recursive bindings
                # Always set binding name for better debugging/disassembly
                old_binding_name = ctx.current_binding_name
                old_sibling_bindings = ctx.sibling_bindings

                ctx.current_binding_name = name
                if name in recursive_bindings:
                    ctx.sibling_bindings = list(group.names)

                value_plan = self._analyze_expression(value_expr, ctx, in_tail_position=False)

                # Restore context
                ctx.current_binding_name = old_binding_name
                ctx.sibling_bindings = old_sibling_bindings

                binding_plans.append((name, value_plan, var_index))

        # Analyze body
        body_plan = self._analyze_expression(body, ctx, in_tail_position=in_tail_position)

        # Pop scope
        ctx.pop_scope()

        return AIFPLIRLetrec(
            bindings=binding_plans,
            body_plan=body_plan,
            binding_groups=binding_groups,
            recursive_bindings=recursive_bindings,
            in_tail_position=in_tail_position
        )

    def _analyze_lambda(self, expr: AIFPLASTList, ctx: AnalysisContext) -> AIFPLIRLambda:
        """Analyze a lambda expression."""
        assert len(expr.elements) == 3, "Lambda expression should have exactly 3 elements"

        _, params_list, body = expr.elements
        assert isinstance(params_list, AIFPLASTList), "Parameter list should be a list"

        # Extract parameter names, stripping the dot sentinel.
        # The semantic analyser guarantees the dot (if present) is second-to-last,
        # followed by exactly one rest parameter symbol.
        param_names = []
        is_variadic = False
        for param in params_list.elements:
            assert isinstance(param, AIFPLASTSymbol)
            if param.name == '.':
                is_variadic = True
                continue
            param_names.append(param.name)

        # Find free variables
        bound_vars = set(param_names)
        free_vars = self._find_free_variables(body, bound_vars, ctx)

        # Separate free variables into captured (from outer scopes) and parent references (recursive bindings)
        captured_vars = []
        free_var_plans = []
        parent_refs = []
        parent_ref_plans = []

        current_binding = ctx.current_binding_name
        current_siblings = ctx.sibling_bindings

        for free_var in free_vars:
            # Create a plan for loading this free variable from parent scope
            var_type, depth, index = ctx.resolve_variable(free_var)

            # Check if this is a self-reference or sibling (parent reference)
            is_parent_ref = False
            if current_binding and free_var == current_binding:
                is_parent_ref = True

            elif current_siblings and free_var in current_siblings:
                is_parent_ref = True

            elif free_var in ctx.parent_ref_names:
                # Also check if it's a recursive binding from a parent letrec
                is_parent_ref = True

            if is_parent_ref:
                # Parent reference - will use LOAD_PARENT_VAR
                parent_refs.append(free_var)
                parent_ref_plans.append(AIFPLIRVariable(
                    name=free_var, var_type=var_type, depth=depth, index=index, is_parent_ref=True
                ))

            else:
                # Regular free variable - will be captured
                captured_vars.append(free_var)
                free_var_plans.append(AIFPLIRVariable(
                    name=free_var, var_type=var_type, depth=depth, index=index, is_parent_ref=False
                ))

        # Create child context for lambda body analysis
        lambda_ctx = ctx.create_child_context()
        lambda_ctx.push_scope()

        # Add parameters to lambda scope
        for param_name in param_names:
            param_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(param_name, param_index)

        # Add captured free variables to lambda scope
        for free_var in captured_vars:
            free_var_index = lambda_ctx.allocate_local_index()
            lambda_ctx.current_scope().add_binding(free_var, free_var_index)

        # Set function name for tail recursion detection
        lambda_ctx.current_function_name = ctx.current_binding_name

        # Mark parent refs so they can be identified during body analysis
        lambda_ctx.parent_ref_names = set(parent_refs)
        lambda_ctx.update_max_locals()

        # Analyze lambda body (in tail position)
        body_plan = self._analyze_expression(body, lambda_ctx, in_tail_position=True)

        # Wrap the lambda body in a AIFPLIRReturn if needed
        if self._needs_return_wrapper(body_plan):
            body_plan = AIFPLIRReturn(value_plan=body_plan)

        lambda_ctx.pop_scope()

        return AIFPLIRLambda(
            params=param_names,
            body_plan=body_plan,
            free_vars=captured_vars,
            free_var_plans=free_var_plans,
            parent_refs=parent_refs,
            parent_ref_plans=parent_ref_plans,
            param_count=len(param_names),
            is_variadic=is_variadic,
            binding_name=ctx.current_binding_name,
            sibling_bindings=ctx.sibling_bindings,
            max_locals=lambda_ctx.max_locals,
            source_line=expr.line if (hasattr(expr, 'line') and expr.line is not None) else 0,
            source_file=expr.source_file if (hasattr(expr, 'source_file') and expr.source_file) else ""
        )

    def _analyze_function_call(self, expr: AIFPLASTList, ctx: AnalysisContext, in_tail_position: bool) -> AIFPLIRCall:
        """Analyze a function call."""
        func_expr = expr.first()
        arg_exprs = list(expr.elements[1:])

        func_type = type(func_expr)

        # Check if calling a known builtin
        if func_type is AIFPLASTSymbol and cast(AIFPLASTSymbol, func_expr).name in self._builtin_names:
            builtin_name = cast(AIFPLASTSymbol, func_expr).name

            # Analyze arguments
            arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]

            return AIFPLIRCall(
                func_plan=AIFPLIRVariable(name=builtin_name, var_type='global', depth=0, index=0),
                arg_plans=arg_plans,
                is_tail_call=False,  # Builtins are never tail-called
                is_tail_recursive=False,
                is_builtin=True,
                builtin_name=builtin_name
            )

        # Check for tail-recursive call
        is_tail_recursive = (
            in_tail_position and
            ctx.current_function_name is not None and
            func_type is AIFPLASTSymbol and
            cast(AIFPLASTSymbol, func_expr).name == ctx.current_function_name
        )

        # Analyze function and arguments
        func_plan: AIFPLIRExpr
        if is_tail_recursive:
            func_plan = AIFPLIRVariable(name='<tail-recursive>', var_type='local', depth=0, index=0)

        else:
            func_plan = self._analyze_expression(func_expr, ctx, in_tail_position=False)

        arg_plans = [self._analyze_expression(arg, ctx, in_tail_position=False) for arg in arg_exprs]

        return AIFPLIRCall(
            func_plan=func_plan,
            arg_plans=arg_plans,
            is_tail_call=in_tail_position,
            is_tail_recursive=is_tail_recursive,
            is_builtin=False,
            builtin_name=None
        )

    def _find_free_variables(self, expr: AIFPLASTNode, bound_vars: Set[str], parent_ctx: AnalysisContext) -> List[str]:
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
        expr: AIFPLASTNode,
        bound_vars: Set[str],
        parent_ctx: AnalysisContext,
        free: List[str],
        seen: Set[str]
    ) -> None:
        """Recursively collect free variables."""
        expr_type = type(expr)

        if expr_type is AIFPLASTSymbol:
            name = cast(AIFPLASTSymbol, expr).name
            if name in seen or name in bound_vars:
                return

            # Check if it's defined in parent scopes
            var_type, _, _ = parent_ctx.resolve_variable(name)
            if var_type == 'local' and name not in seen:
                free.append(name)
                seen.add(name)

        elif expr_type is AIFPLASTList:
            if cast(AIFPLASTList, expr).is_empty():
                return

            first = cast(AIFPLASTList, expr).first()
            first_type = type(first)

            # Handle special forms that bind variables
            if first_type is AIFPLASTSymbol:
                if cast(AIFPLASTSymbol, first).name == 'lambda':
                    # Nested lambda: we need to find what free variables it uses
                    # that come from outer scopes, so the parent lambda can capture them.
                    # The nested lambda will then capture them from the parent.
                    if len(cast(AIFPLASTList, expr).elements) >= 3:
                        nested_params = cast(AIFPLASTList, expr).elements[1]
                        nested_body = cast(AIFPLASTList, expr).elements[2]

                        # Get parameter names from nested lambda
                        nested_bound = bound_vars.copy()
                        if isinstance(nested_params, AIFPLASTList):
                            for param in nested_params.elements:
                                if isinstance(param, AIFPLASTSymbol):
                                    nested_bound.add(param.name)

                        # Find free variables in nested lambda's body
                        # These are variables the nested lambda needs, which might come
                        # from the parent lambda or from even outer scopes
                        self._collect_free_vars(nested_body, nested_bound, parent_ctx, free, seen)

                    # Don't recurse into the lambda's parameters or other parts
                    return

                if cast(AIFPLASTSymbol, first).name == 'let':
                    # Let bindings create new bound variables
                    # Extract binding names and recurse with updated bound_vars
                    if len(cast(AIFPLASTList, expr).elements) >= 3:
                        bindings_list = cast(AIFPLASTList, expr).elements[1]
                        body = cast(AIFPLASTList, expr).elements[2]

                        # Collect let binding names
                        new_bound = bound_vars.copy()
                        if isinstance(bindings_list, AIFPLASTList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLASTList) and len(binding.elements) >= 2:
                                    name_expr = binding.elements[0]
                                    if isinstance(name_expr, AIFPLASTSymbol):
                                        new_bound.add(name_expr.name)

                        # Recurse into binding values first (to find free vars in lambda definitions)
                        if isinstance(bindings_list, AIFPLASTList):
                            for binding in bindings_list.elements:
                                if isinstance(binding, AIFPLASTList) and len(binding.elements) >= 2:
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
            for elem in cast(AIFPLASTList, expr).elements:
                self._collect_free_vars(elem, bound_vars, parent_ctx, free, seen)
