"""
AIFPL Type Flow Analyzer — AST optimization pass.

This pass runs after AIFPLConstantFolder and before AIFPLIRBuilder. It infers
static types for expressions and uses that information to fold type-predicate
calls whose results are statically known.

The primary motivation is eliminating the redundant type-guard opcodes that the
desugarer emits for literal match arms:

    ; Desugared match arm for integer literal:
    (if (if (integer? tmp) (integer=? tmp 42) #f)
        "matched"
        <next-arm>)

When `tmp` is bound to a known integer, `(integer? tmp)` folds to #t, the
inner `if` reduces to `(integer=? tmp 42)`, and the outer match arm compiles
to a single equality check — no type-guard opcode.

Design decisions:
  - Conservative: UNKNOWN is the safe default; the pass never changes semantics.
  - Flat lattice: no union types, no negation narrowing.
  - Inline constant folding: when a type predicate folds to a boolean literal,
    the pass immediately simplifies any enclosing (if #t/f ...) or desugared-and
    (if cond then #f) without requiring a second constant-folder pass.
  - Lexically scoped type environment: immutable dicts threaded through recursion.
"""

from enum import Enum
from typing import Dict, Optional, Tuple

from aifpl.aifpl_ast import (
    AIFPLASTNode, AIFPLASTBoolean, AIFPLASTInteger, AIFPLASTFloat,
    AIFPLASTComplex, AIFPLASTString, AIFPLASTNone, AIFPLASTSymbol,
    AIFPLASTList,
)
from aifpl.aifpl_optimization_pass import AIFPLOptimizationPass


class AIFPLInferredType(Enum):
    """
    Flat type lattice used by the type flow analyzer.

    UNKNOWN is the top element — it means "could be anything" and is the safe
    conservative default.  The analyzer never needs to be unsound: if it cannot
    determine the type it returns UNKNOWN and emits no optimization.
    """
    UNKNOWN  = "unknown"
    NONE     = "none"
    BOOLEAN  = "boolean"
    INTEGER  = "integer"
    FLOAT    = "float"
    COMPLEX  = "complex"
    STRING   = "string"
    LIST     = "list"
    DICT     = "dict"
    FUNCTION = "function"
    SYMBOL   = "symbol"


# Shorthand aliases used throughout this module
_U = AIFPLInferredType.UNKNOWN
_NONE = AIFPLInferredType.NONE
_BOOL = AIFPLInferredType.BOOLEAN
_INT  = AIFPLInferredType.INTEGER
_FLT  = AIFPLInferredType.FLOAT
_CPX  = AIFPLInferredType.COMPLEX
_STR  = AIFPLInferredType.STRING
_LIST = AIFPLInferredType.LIST
_DICT = AIFPLInferredType.DICT
_FUN  = AIFPLInferredType.FUNCTION
_SYM  = AIFPLInferredType.SYMBOL

# Type environment: maps variable name → inferred type
TypeEnv = Dict[str, AIFPLInferredType]


# Builtins whose return type is always the same regardless of arguments.
# Builtins that return UNKNOWN (e.g. dict-get, list-ref) are simply absent.
_BUILTIN_RETURN_TYPES: Dict[str, AIFPLInferredType] = {
    # Type predicates — always return boolean
    'none?':              _BOOL,
    'boolean?':           _BOOL,
    'integer?':           _BOOL,
    'float?':             _BOOL,
    'complex?':           _BOOL,
    'string?':            _BOOL,
    'list?':              _BOOL,
    'dict?':              _BOOL,
    'function?':          _BOOL,
    'symbol?':            _BOOL,
    # Boolean operations
    'boolean=?':          _BOOL,
    'boolean!=?':         _BOOL,
    'boolean-not':        _BOOL,
    # Integer comparisons
    'integer=?':          _BOOL,
    'integer!=?':         _BOOL,
    'integer<?':          _BOOL,
    'integer>?':          _BOOL,
    'integer<=?':         _BOOL,
    'integer>=?':         _BOOL,
    # Float comparisons
    'float=?':            _BOOL,
    'float!=?':           _BOOL,
    'float<?':            _BOOL,
    'float>?':            _BOOL,
    'float<=?':           _BOOL,
    'float>=?':           _BOOL,
    # String comparisons
    'string=?':           _BOOL,
    'string!=?':          _BOOL,
    'string<?':           _BOOL,
    'string>?':           _BOOL,
    'string<=?':          _BOOL,
    'string>=?':          _BOOL,
    # List / dict comparisons and predicates
    'list=?':             _BOOL,
    'list!=?':            _BOOL,
    'dict=?':             _BOOL,
    'dict!=?':            _BOOL,
    'list-null?':         _BOOL,
    'list-member?':       _BOOL,
    'dict-has?':          _BOOL,
    # Function predicates
    'function=?':         _BOOL,
    'function!=?':        _BOOL,
    'function-variadic?': _BOOL,
    'function-accepts?':  _BOOL,
    # Symbol comparisons
    'symbol=?':           _BOOL,
    'symbol!=?':          _BOOL,
    # String predicates
    'string-prefix?':     _BOOL,
    'string-suffix?':     _BOOL,

    # Integer arithmetic — always return integer
    'integer+':                 _INT,
    'integer-':                 _INT,
    'integer*':                 _INT,
    'integer/':                 _INT,
    'integer%':                 _INT,
    'integer-neg':              _INT,
    'integer-abs':              _INT,
    'integer-expn':             _INT,
    'integer-min':              _INT,
    'integer-max':              _INT,
    'integer-bit-or':           _INT,
    'integer-bit-and':          _INT,
    'integer-bit-xor':          _INT,
    'integer-bit-not':          _INT,
    'integer-bit-shift-left':   _INT,
    'integer-bit-shift-right':  _INT,
    'string-length':            _INT,
    'list-length':              _INT,
    'dict-length':              _INT,
    'float->integer':           _INT,
    'function-min-arity':       _INT,

    # Float arithmetic — always return float
    'float+':       _FLT,
    'float-':       _FLT,
    'float*':       _FLT,
    'float/':       _FLT,
    'float//':      _FLT,
    'float%':       _FLT,
    'float-neg':    _FLT,
    'float-abs':    _FLT,
    'float-floor':  _FLT,
    'float-ceil':   _FLT,
    'float-round':  _FLT,
    'float-exp':    _FLT,
    'float-expn':   _FLT,
    'float-log':    _FLT,
    'float-log2':   _FLT,
    'float-log10':  _FLT,
    'float-logn':   _FLT,
    'float-sin':    _FLT,
    'float-cos':    _FLT,
    'float-tan':    _FLT,
    'float-sqrt':   _FLT,
    'float-min':    _FLT,
    'float-max':    _FLT,
    'integer->float':  _FLT,
    'complex-abs':     _FLT,
    'complex-real':    _FLT,
    'complex-imag':    _FLT,

    # Complex arithmetic — always return complex
    'complex+':        _CPX,
    'complex-':        _CPX,
    'complex*':        _CPX,
    'complex/':        _CPX,
    'complex-neg':     _CPX,
    'complex-exp':     _CPX,
    'complex-expn':    _CPX,
    'complex-log':     _CPX,
    'complex-log10':   _CPX,
    'complex-logn':    _CPX,
    'complex-sin':     _CPX,
    'complex-cos':     _CPX,
    'complex-tan':     _CPX,
    'complex-sqrt':    _CPX,
    'integer->complex': _CPX,
    'float->complex':   _CPX,

    # String operations — always return string
    'string-concat':      _STR,
    'string-ref':         _STR,
    'string-upcase':      _STR,
    'string-downcase':    _STR,
    'string-trim':        _STR,
    'string-trim-left':   _STR,
    'string-trim-right':  _STR,
    'string-replace':     _STR,
    'string-slice':       _STR,
    'integer->string':    _STR,
    'float->string':      _STR,
    'complex->string':    _STR,
    'symbol->string':     _STR,

    # List operations — always return list
    'list':          _LIST,
    'list-prepend':  _LIST,
    'list-append':   _LIST,
    'list-concat':   _LIST,
    'list-reverse':  _LIST,
    'list-rest':     _LIST,
    'list-slice':    _LIST,
    'list-remove':   _LIST,
    'list-map':      _LIST,
    'list-filter':   _LIST,
    'list-zip':      _LIST,
    'list-unzip':    _LIST,
    'list-sort':     _LIST,
    'dict-keys':     _LIST,
    'dict-values':   _LIST,
    'string->list':  _LIST,
    'range':         _LIST,

    # Dict operations — always return dict
    'dict':        _DICT,
    'dict-set':    _DICT,
    'dict-remove': _DICT,
    'dict-merge':  _DICT,
    'dict-map':    _DICT,
    'dict-filter': _DICT,
}

# Maps type-predicate name → the type it tests for
_TYPE_PREDICATE_MAP: Dict[str, AIFPLInferredType] = {
    'none?':     _NONE,
    'boolean?':  _BOOL,
    'integer?':  _INT,
    'float?':    _FLT,
    'complex?':  _CPX,
    'string?':   _STR,
    'list?':     _LIST,
    'dict?':     _DICT,
    'function?': _FUN,
    'symbol?':   _SYM,
}


def _bool_node(value: bool) -> AIFPLASTBoolean:
    return AIFPLASTBoolean(value)


class AIFPLTypeFlowAnalyzer(AIFPLOptimizationPass):
    """
    Type-flow analysis and type-predicate folding pass.

    Traverses the desugared, constant-folded AST, infers static types for
    sub-expressions, and folds type-predicate calls whose results are
    statically determined.  Performs inline constant folding of `if`
    expressions whose conditions have been reduced to boolean literals, so
    that no second constant-folder pass is required.

    Correctness invariant: the pass is conservative.  It may only replace a
    type-predicate call with #t or #f when it is certain.  If there is any
    doubt it leaves the expression unchanged.  A wrong fold would silently
    change program semantics; an un-folded expression is merely suboptimal.
    """
    def optimize(self, expr: AIFPLASTNode) -> AIFPLASTNode:
        """Entry point — optimize with an empty type environment."""
        return self._optimize(expr, {})

    def _optimize(self, expr: AIFPLASTNode, env: TypeEnv) -> AIFPLASTNode:
        """
        Recursively optimize *expr* given type environment *env*.

        Returns an optimized AST node that is semantically equivalent to
        the input.
        """
        # Non-list nodes are atomic — nothing to optimize
        if not isinstance(expr, AIFPLASTList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()
        if not isinstance(first, AIFPLASTSymbol):
            # A list whose head is not a symbol is unusual (data list or quoted
            # form).  Recursively optimize elements conservatively.
            return self._optimize_elements(expr, env)

        name = first.name

        # Handle special forms first, since they have non-standard semantics
        if name == 'if':
            return self._optimize_if(expr, env)

        if name in ('let', 'letrec'):
            return self._optimize_let(expr, env)

        if name == 'lambda':
            return self._optimize_lambda(expr, env)

        if name in ('quote', 'error', 'trace'):
            # quote: contents are data, not code — leave untouched.
            # error/trace: optimize sub-expressions but no type folding needed.
            return self._optimize_elements(expr, env)

        return self._optimize_call(expr, env)

    def _optimize_elements(self, expr: AIFPLASTList, env: TypeEnv) -> AIFPLASTList:
        """Recursively optimize all elements of *expr* and return a new list."""
        optimized = tuple(self._optimize(elem, env) for elem in expr.elements)
        return AIFPLASTList(optimized, line=expr.line, column=expr.column, source_file=expr.source_file)

    def _optimize_if(self, expr: AIFPLASTList, env: TypeEnv) -> AIFPLASTNode:
        """
        Optimize (if condition then else).

        1. Optimize the condition.
        2. If the condition reduced to a boolean literal, inline-fold the branch.
        3. Otherwise narrow the type environment for each branch and optimize them.
        """
        assert len(expr.elements) == 4
        _, condition, then_expr, else_expr = expr.elements

        opt_condition = self._optimize(condition, env)

        # Inline constant folding: condition is now a known boolean
        if isinstance(opt_condition, AIFPLASTBoolean):
            if opt_condition.value:
                return self._optimize(then_expr, env)
            return self._optimize(else_expr, env)

        # Narrow environments for each branch
        then_env, else_env = self._narrow_from_condition(opt_condition, env)

        opt_then = self._optimize(then_expr, then_env)
        opt_else = self._optimize(else_expr, else_env)

        return AIFPLASTList(
            (first_sym(expr), opt_condition, opt_then, opt_else),
            line=expr.line, column=expr.column, source_file=expr.source_file
        )

    def _optimize_let(self, expr: AIFPLASTList, env: TypeEnv) -> AIFPLASTNode:
        """
        Optimize (let ((var val) ...) body) or (letrec ...).

        For each binding, infer the type of the value expression (in the
        current env — letrec bindings are mutually recursive but we conservatively
        treat them the same way), extend the environment, then optimize the body.
        """
        assert len(expr.elements) == 3
        form_sym, bindings_list, body = expr.elements
        assert isinstance(bindings_list, AIFPLASTList)

        extended_env: TypeEnv = dict(env)
        opt_bindings = []

        for binding in bindings_list.elements:
            assert isinstance(binding, AIFPLASTList) and len(binding.elements) == 2
            var_sym, val_expr = binding.elements

            opt_val = self._optimize(val_expr, env)
            inferred = self._infer_type(opt_val, env)

            assert isinstance(var_sym, AIFPLASTSymbol)
            extended_env[var_sym.name] = inferred

            opt_bindings.append(AIFPLASTList(
                (var_sym, opt_val),
                line=binding.line, column=binding.column, source_file=binding.source_file
            ))

        opt_body = self._optimize(body, extended_env)

        return AIFPLASTList(
            (form_sym,
             AIFPLASTList(tuple(opt_bindings),
                          line=bindings_list.line, column=bindings_list.column,
                          source_file=bindings_list.source_file),
             opt_body),
            line=expr.line, column=expr.column, source_file=expr.source_file
        )

    def _optimize_lambda(self, expr: AIFPLASTList, env: TypeEnv) -> AIFPLASTNode:
        """
        Optimize (lambda (params) body).

        Lambda parameters have UNKNOWN type — we do not do interprocedural
        analysis.  The body is optimized with params removed from the env
        (they shadow any outer bindings with UNKNOWN).
        """
        assert len(expr.elements) == 3
        lambda_sym, params, body = expr.elements
        assert isinstance(params, AIFPLASTList)

        # Shadow all parameter names with UNKNOWN in the body environment
        body_env: TypeEnv = dict(env)
        for param in params.elements:
            if isinstance(param, AIFPLASTSymbol):
                body_env[param.name] = _U

        opt_body = self._optimize(body, body_env)

        return AIFPLASTList(
            (lambda_sym, params, opt_body),
            line=expr.line, column=expr.column, source_file=expr.source_file
        )

    def _optimize_call(self, expr: AIFPLASTList, env: TypeEnv) -> AIFPLASTNode:
        """
        Optimize a function call expression.

        If the callee is a type predicate and the argument's type is statically
        known, replace the call with the appropriate boolean literal.  Otherwise
        optimize arguments recursively.
        """
        first = expr.first()
        assert isinstance(first, AIFPLASTSymbol)
        name = first.name

        # Optimize all arguments first
        opt_args = tuple(self._optimize(arg, env) for arg in expr.elements[1:])

        # Attempt to fold a type predicate call
        if name in _TYPE_PREDICATE_MAP and len(opt_args) == 1:
            arg_type = self._infer_type(opt_args[0], env)
            result = self._fold_type_predicate(name, arg_type)
            if result is not None:
                return _bool_node(result)

        return AIFPLASTList(
            (first,) + opt_args,
            line=expr.line, column=expr.column, source_file=expr.source_file
        )

    def _infer_type(self, expr: AIFPLASTNode, env: TypeEnv) -> AIFPLInferredType:
        """
        Return the statically inferred type of *expr* given *env*.

        Returns UNKNOWN whenever the type cannot be determined with certainty.
        """
        # Literal nodes — type is always known
        if isinstance(expr, AIFPLASTNone):
            return _NONE

        if isinstance(expr, AIFPLASTBoolean):
            return _BOOL

        if isinstance(expr, AIFPLASTInteger):
            return _INT

        if isinstance(expr, AIFPLASTFloat):
            return _FLT

        if isinstance(expr, AIFPLASTComplex):
            return _CPX

        if isinstance(expr, AIFPLASTString):
            return _STR

        # Variable — look up in environment
        if isinstance(expr, AIFPLASTSymbol):
            return env.get(expr.name, _U)

        # List node — could be a call or a special form
        if isinstance(expr, AIFPLASTList):
            if expr.is_empty():
                return _U  # Empty list is unusual at this stage

            first = expr.first()
            if not isinstance(first, AIFPLASTSymbol):
                return _U

            name = first.name

            # Lambda always produces a function
            if name == 'lambda':
                return _FUN

            # if: join the two branch types
            if name == 'if' and len(expr.elements) == 4:
                _, _, then_expr, else_expr = expr.elements
                t_then = self._infer_type(then_expr, env)
                t_else = self._infer_type(else_expr, env)
                return t_then if t_then == t_else else _U

            # let/letrec: infer type of the body in an extended env
            if name in ('let', 'letrec') and len(expr.elements) == 3:
                _, bindings_list, body = expr.elements
                if isinstance(bindings_list, AIFPLASTList):
                    extended: TypeEnv = dict(env)
                    for binding in bindings_list.elements:
                        if (isinstance(binding, AIFPLASTList)
                                and len(binding.elements) == 2
                                and isinstance(binding.elements[0], AIFPLASTSymbol)):
                            var_name = binding.elements[0].name
                            extended[var_name] = self._infer_type(binding.elements[1], env)

                    return self._infer_type(body, extended)

            # Known-return builtin
            if name in _BUILTIN_RETURN_TYPES:
                return _BUILTIN_RETURN_TYPES[name]

        return _U

    # ------------------------------------------------------------------
    # Type narrowing
    # ------------------------------------------------------------------

    def _narrow_from_condition(
        self, condition: AIFPLASTNode, env: TypeEnv
    ) -> Tuple[TypeEnv, TypeEnv]:
        """
        Given a condition expression, return (then_env, else_env).

        then_env has types narrowed by knowing the condition is true.
        else_env is always the unchanged env (we do not track type negation).

        Recognised patterns (all in post-desugaring form):

        1. (T? x)  — direct type predicate on a variable
           then_env: x → T

        2. (if (T? x) rest #f)  — desugared `and` whose first operand is a
           type predicate on a variable (produced by _make_and in the desugarer)
           then_env: x → T  (and any further narrowing from `rest`)

        Anything else: both envs are unchanged.
        """
        # Pattern 1: (T? x) — direct type predicate
        narrowed = self._try_narrow_type_predicate(condition, env)
        if narrowed is not None:
            return narrowed

        # Pattern 2: (if (T? x) rest #f) — desugared `and`
        # The desugarer lowers (and A B ...) to (if A (if B ... #f) #f).
        # We recognise the outermost (if cond then #f) shape and use `cond`
        # for narrowing, then continue narrowing into `then` recursively.
        if (isinstance(condition, AIFPLASTList)
                and len(condition.elements) == 4
                and isinstance(condition.elements[0], AIFPLASTSymbol)
                and condition.elements[0].name == 'if'
                and isinstance(condition.elements[3], AIFPLASTBoolean)
                and not condition.elements[3].value):
            # condition = (if sub_cond then_part #f)
            sub_cond = condition.elements[1]
            then_part = condition.elements[2]

            # Try to narrow from sub_cond
            sub_narrowed = self._try_narrow_type_predicate(sub_cond, env)
            if sub_narrowed is not None:
                # sub_cond is (T? x): narrow x → T, then continue into then_part
                then_env_after_sub, _ = sub_narrowed
                # Recursively narrow from then_part using the already-narrowed env
                then_env_final, _ = self._narrow_from_condition(then_part, then_env_after_sub)
                return then_env_final, env

        return env, env

    def _try_narrow_type_predicate(
        self, expr: AIFPLASTNode, env: TypeEnv
    ) -> Optional[Tuple[TypeEnv, TypeEnv]]:
        """
        If *expr* is `(T? var)` where T? is a type predicate and var is a symbol,
        return (then_env, else_env) with var narrowed to T in then_env.
        Otherwise return None.
        """
        if not isinstance(expr, AIFPLASTList):
            return None
        if len(expr.elements) != 2:
            return None

        pred = expr.elements[0]
        arg = expr.elements[1]

        if not isinstance(pred, AIFPLASTSymbol):
            return None

        if pred.name not in _TYPE_PREDICATE_MAP:
            return None

        if not isinstance(arg, AIFPLASTSymbol):
            return None

        narrowed_type = _TYPE_PREDICATE_MAP[pred.name]
        then_env: TypeEnv = {**env, arg.name: narrowed_type}
        return then_env, env

    def _fold_type_predicate(
        self, builtin_name: str, arg_type: AIFPLInferredType
    ) -> Optional[bool]:
        """
        If *builtin_name* is a type predicate and *arg_type* is known (not
        UNKNOWN), return True or False.  Return None if the result cannot be
        determined statically.
        """
        if arg_type is _U:
            return None

        expected = _TYPE_PREDICATE_MAP.get(builtin_name)
        if expected is None:
            return None

        return arg_type == expected


def first_sym(expr: AIFPLASTList) -> AIFPLASTSymbol:
    """Return the first element of *expr* as an AIFPLASTSymbol (asserted)."""
    sym = expr.first()
    assert isinstance(sym, AIFPLASTSymbol)
    return sym
