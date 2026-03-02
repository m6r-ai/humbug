"""
Tests for MenaiFreeVarAnalyzer.

Two test layers:

1. Unit tests — construct desugared AST fragments directly and assert the
   FreeVarInfo annotation.  These are precise and fast; they don't go through
   the full pipeline.

2. Agreement tests — compile real Menai source through the full pipeline and
   assert that for every MenaiIRLambda in the IR tree, the free_vars list
   reported by the IR builder matches the frozenset reported by
   MenaiFreeVarAnalyzer for the corresponding source lambda.

   Agreement tests are the key regression guard before Step 2 (deferred
   depth resolution), because they prove the standalone analyzer produces
   exactly the same information the IR builder currently derives inline.

AST construction helpers
------------------------
All helpers produce MenaiASTList / MenaiASTSymbol nodes with no source
location metadata (line=None, column=None, source_file="").  The analyzer
does not require location info.
"""

from __future__ import annotations

import pytest

from menai.menai_ast import (
    MenaiASTBoolean,
    MenaiASTInteger,
    MenaiASTList,
    MenaiASTString,
    MenaiASTSymbol,
)
from menai.menai_free_var_analyzer import FreeVarInfo, MenaiFreeVarAnalyzer
from menai.menai_ir import (
    MenaiIRLambda,
    MenaiIRExpr,
    MenaiIRLet,
    MenaiIRCall,
    MenaiIRLetrec,
    MenaiIRIf,
    MenaiIRReturn,
    MenaiIRTrace,
)


# ---------------------------------------------------------------------------
# AST construction helpers
# ---------------------------------------------------------------------------

def sym(name: str) -> MenaiASTSymbol:
    return MenaiASTSymbol(name)


def lst(*elements) -> MenaiASTList:
    return MenaiASTList(tuple(elements))


def integer(n: int) -> MenaiASTInteger:
    return MenaiASTInteger(n)


def boolean(b: bool) -> MenaiASTBoolean:
    return MenaiASTBoolean(b)


def string(s: str) -> MenaiASTString:
    return MenaiASTString(s)


def make_lambda(params: list[str], body) -> MenaiASTList:
    """(lambda (p1 p2 ...) body)"""
    return lst(sym('lambda'), lst(*[sym(p) for p in params]), body)


def make_let(bindings: list[tuple[str, object]], body) -> MenaiASTList:
    """(let ((name val) ...) body)"""
    binding_nodes = [lst(sym(name), val) for name, val in bindings]
    return lst(sym('let'), lst(*binding_nodes), body)


def make_letrec(bindings: list[tuple[str, object]], body) -> MenaiASTList:
    """(letrec ((name val) ...) body)"""
    binding_nodes = [lst(sym(name), val) for name, val in bindings]
    return lst(sym('letrec'), lst(*binding_nodes), body)


def make_if(cond, then, else_) -> MenaiASTList:
    return lst(sym('if'), cond, then, else_)


def make_call(func, *args) -> MenaiASTList:
    return lst(func, *args)


# ---------------------------------------------------------------------------
# Helper: collect all MenaiIRLambda nodes from an IR tree
# ---------------------------------------------------------------------------

def _collect_ir_lambdas(ir: MenaiIRExpr) -> list[MenaiIRLambda]:
    """Walk an IR tree and return all MenaiIRLambda nodes."""
    result: list[MenaiIRLambda] = []
    _collect_ir_lambdas_rec(ir, result)
    return result


def _collect_ir_lambdas_rec(ir: MenaiIRExpr, result: list[MenaiIRLambda]) -> None:
    if isinstance(ir, MenaiIRLambda):
        result.append(ir)
        _collect_ir_lambdas_rec(ir.body_plan, result)
        for plan in ir.sibling_free_var_plans + ir.outer_free_var_plans:
            _collect_ir_lambdas_rec(plan, result)

    elif isinstance(ir, (MenaiIRLet, MenaiIRLetrec)):
        for _, val_plan, _ in ir.bindings:
            _collect_ir_lambdas_rec(val_plan, result)
        _collect_ir_lambdas_rec(ir.body_plan, result)

    elif isinstance(ir, MenaiIRIf):
        _collect_ir_lambdas_rec(ir.condition_plan, result)
        _collect_ir_lambdas_rec(ir.then_plan, result)
        _collect_ir_lambdas_rec(ir.else_plan, result)

    elif isinstance(ir, MenaiIRReturn):
        _collect_ir_lambdas_rec(ir.value_plan, result)

    elif isinstance(ir, MenaiIRTrace):
        for msg in ir.message_plans:
            _collect_ir_lambdas_rec(msg, result)
        _collect_ir_lambdas_rec(ir.value_plan, result)

    elif isinstance(ir, MenaiIRCall):
        _collect_ir_lambdas_rec(ir.func_plan, result)
        for plan in ir.arg_plans:
            _collect_ir_lambdas_rec(plan, result)



# ---------------------------------------------------------------------------
# Helper: compile source to IR (for agreement tests)
# ---------------------------------------------------------------------------

def _compile_to_ir(source: str) -> MenaiIRExpr:
    from menai.menai_lexer import MenaiLexer
    from menai.menai_parser import MenaiParser
    from menai.menai_semantic_analyzer import MenaiSemanticAnalyzer
    from menai.menai_desugarer import MenaiDesugarer
    from menai.menai_ir_builder import MenaiIRBuilder

    tokens = MenaiLexer().lex(source)
    ast = MenaiParser().parse(tokens, source)
    ast = MenaiSemanticAnalyzer().analyze(ast, source)
    desugared = MenaiDesugarer().desugar(ast)
    return MenaiIRBuilder().build(desugared)


def _compile_to_desugared(source: str):
    from menai.menai_lexer import MenaiLexer
    from menai.menai_parser import MenaiParser
    from menai.menai_semantic_analyzer import MenaiSemanticAnalyzer
    from menai.menai_desugarer import MenaiDesugarer

    tokens = MenaiLexer().lex(source)
    ast = MenaiParser().parse(tokens, source)
    ast = MenaiSemanticAnalyzer().analyze(ast, source)
    return MenaiDesugarer().desugar(ast)


# ===========================================================================
# 1. Unit tests
# ===========================================================================

class TestFreeVarAnalyzerUnit:
    """Unit tests using hand-constructed desugared AST fragments."""

    def _analyze(self, expr) -> FreeVarInfo:
        return MenaiFreeVarAnalyzer().analyze(expr)

    # --- No free variables ---

    def test_lambda_no_free_vars_constant_body(self):
        """(lambda (x) 42) — body is a constant, no free vars."""
        lam = make_lambda(['x'], integer(42))
        info = self._analyze(lam)
        assert info.get(lam) == frozenset()

    def test_lambda_no_free_vars_param_used(self):
        """(lambda (x) x) — x is a parameter, not free."""
        lam = make_lambda(['x'], sym('x'))
        info = self._analyze(lam)
        assert info.get(lam) == frozenset()

    def test_lambda_no_free_vars_two_params(self):
        """(lambda (x y) (integer+ x y)) — both params used, none free."""
        body = make_call(sym('integer+'), sym('x'), sym('y'))
        lam = make_lambda(['x', 'y'], body)
        info = self._analyze(lam)
        assert info.get(lam) == frozenset()

    # --- Single free variable ---

    def test_lambda_one_free_var(self):
        """
        (let ((z 1))
          (lambda (x) (integer+ x z)))
        z is bound in the let, so it is a local in the enclosing scope and
        therefore free in the lambda.
        """
        body = make_call(sym('integer+'), sym('x'), sym('z'))
        lam = make_lambda(['x'], body)
        expr = make_let([('z', integer(1))], lam)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'z'})

    def test_lambda_captures_outer_let_binding(self):
        """
        (let ((a 10))
          (let ((b 20))
            (lambda () (integer+ a b))))
        Both a and b are free in the lambda.
        """
        body = make_call(sym('integer+'), sym('a'), sym('b'))
        lam = make_lambda([], body)
        inner = make_let([('b', integer(20))], lam)
        expr = make_let([('a', integer(10))], inner)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'a', 'b'})

    # --- Globals are not free variables ---

    def test_global_builtin_not_free(self):
        """
        (lambda (x) (integer+ x 1))
        integer+ is a global builtin, not in any local scope — not free.
        """
        body = make_call(sym('integer+'), sym('x'), integer(1))
        lam = make_lambda(['x'], body)
        info = self._analyze(lam)
        assert info.get(lam) == frozenset()

    def test_unbound_name_at_top_level_not_free(self):
        """
        (lambda (x) (foo x))
        foo is not in any local scope — treated as global, not free.
        """
        body = make_call(sym('foo'), sym('x'))
        lam = make_lambda(['x'], body)
        info = self._analyze(lam)
        assert info.get(lam) == frozenset()

    # --- Shadowing ---

    def test_inner_let_shadows_outer(self):
        """
        (let ((x 1))
          (lambda (y)
            (let ((x 2))   ; shadows outer x
              (integer+ x y))))
        x is shadowed by the inner let, so only y is a param (not free).
        The inner let's x is bound within the lambda body, so it's not free.
        The outer x is shadowed and never referenced.
        Result: no free vars.
        """
        inner_body = make_call(sym('integer+'), sym('x'), sym('y'))
        inner_let = make_let([('x', integer(2))], inner_body)
        lam = make_lambda(['y'], inner_let)
        expr = make_let([('x', integer(1))], lam)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset()

    def test_param_shadows_outer_binding(self):
        """
        (let ((x 99))
          (lambda (x) x))
        x is a parameter of the lambda, so the outer x is shadowed.
        No free vars.
        """
        lam = make_lambda(['x'], sym('x'))
        expr = make_let([('x', integer(99))], lam)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset()

    # --- let binding semantics ---

    def test_let_binding_value_sees_outer_scope(self):
        """
        (let ((a 1))
          (let ((b a))         ; b's value references a from outer scope
            (lambda () b)))    ; b is free in the lambda
        The lambda's free var is b (from the inner let), not a.
        """
        lam = make_lambda([], sym('b'))
        inner_let = make_let([('b', sym('a'))], lam)
        expr = make_let([('a', integer(1))], inner_let)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'b'})

    def test_let_bindings_cannot_see_siblings(self):
        """
        (let ((a 1))
          (let ((b 2)
                (c b))          ; c's value references b — but b is a sibling
            (lambda () c)))     ; in parallel let, b is NOT in scope for c's value
        c is free in the lambda.  b is NOT free (the lambda doesn't reference b).
        """
        lam = make_lambda([], sym('c'))
        # Parallel let: b and c are siblings, c's value 'b' is evaluated in outer scope
        inner_let = make_let([('b', integer(2)), ('c', sym('b'))], lam)
        expr = make_let([('a', integer(1))], inner_let)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'c'})

    # --- letrec binding semantics ---

    def test_letrec_names_in_scope_for_values(self):
        """
        (letrec ((f (lambda (n) (f n))))  ; f is free in the lambda? No — f is a
          f)                              ; letrec binding, so it IS in scope.
        The lambda references f.  f is bound by the enclosing letrec scope, which
        means it IS a local in scope_stack when we analyse the lambda.  The
        analyzer therefore correctly reports f as free in the lambda.
        The IR builder also sees f as free — it classifies it as a parent_ref
        (recursive back-edge) rather than a captured free var, but it is free.
        """
        lam = make_lambda(['n'], make_call(sym('f'), sym('n')))
        expr = make_letrec([('f', lam)], sym('f'))
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'f'})

    def test_letrec_sibling_not_free(self):
        """
        (letrec ((even? (lambda (n) (odd? n)))
                 (odd?  (lambda (n) (even? n))))
          even?)
        even? and odd? are letrec siblings.  Both names are in the letrec scope,
        which is in scope_stack when the lambdas are analysed.  So odd? IS free
        in even?'s lambda, and even? IS free in odd?'s lambda.
        The IR builder also sees them as free (as parent_refs).
        """
        lam_even = make_lambda(['n'], make_call(sym('odd?'), sym('n')))
        lam_odd = make_lambda(['n'], make_call(sym('even?'), sym('n')))
        expr = make_letrec([('even?', lam_even), ('odd?', lam_odd)], sym('even?'))
        info = self._analyze(expr)
        assert info.get(lam_even) == frozenset({'odd?'})
        assert info.get(lam_odd) == frozenset({'even?'})

    def test_letrec_captures_outer_let(self):
        """
        (let ((base 10))
          (letrec ((f (lambda (n) (integer+ n base))))
            (f 5)))
        base is bound in the outer let — it IS free in the lambda.
        """
        body = make_call(sym('integer+'), sym('n'), sym('base'))
        lam = make_lambda(['n'], body)
        letrec = make_letrec([('f', lam)], make_call(sym('f'), integer(5)))
        expr = make_let([('base', integer(10))], letrec)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'base'})

    # --- Nested lambdas ---

    def test_nested_lambda_outer_captures_nothing(self):
        """
        (let ((x 1))
          (lambda ()           ; outer lambda — captures x
            (lambda () x)))    ; inner lambda — also captures x
        The outer lambda is free in x.
        The inner lambda is also free in x (x is not bound by outer's params).
        """
        inner_lam = make_lambda([], sym('x'))
        outer_lam = make_lambda([], inner_lam)
        expr = make_let([('x', integer(1))], outer_lam)
        info = self._analyze(expr)
        assert info.get(outer_lam) == frozenset({'x'})
        assert info.get(inner_lam) == frozenset({'x'})

    def test_nested_lambda_inner_captures_outer_param(self):
        """
        (let ((z 0))
          (lambda (x)          ; outer lambda — captures z? No, doesn't use z.
            (lambda (y)        ; inner lambda — captures x from outer param
              (integer+ x y))))
        Outer lambda: no free vars (x is a param, inner lambda is just a value).
        Inner lambda: x is free (it's a param of the outer lambda, which is a
                      local scope for the inner lambda).
        """
        inner_body = make_call(sym('integer+'), sym('x'), sym('y'))
        inner_lam = make_lambda(['y'], inner_body)
        outer_lam = make_lambda(['x'], inner_lam)
        expr = make_let([('z', integer(0))], outer_lam)
        info = self._analyze(expr)
        assert info.get(outer_lam) == frozenset()
        assert info.get(inner_lam) == frozenset({'x'})

    def test_nested_lambda_outer_must_capture_for_inner(self):
        """
        (let ((a 1))
          (lambda ()           ; outer lambda — must capture a for inner
            (lambda () a)))    ; inner lambda — uses a
        Both outer and inner are free in a.
        """
        inner_lam = make_lambda([], sym('a'))
        outer_lam = make_lambda([], inner_lam)
        expr = make_let([('a', integer(1))], outer_lam)
        info = self._analyze(expr)
        assert info.get(outer_lam) == frozenset({'a'})
        assert info.get(inner_lam) == frozenset({'a'})

    # --- if expression ---

    def test_if_branches_see_outer_scope(self):
        """
        (let ((flag #t) (val 42))
          (lambda () (if flag val 0)))
        flag and val are free in the lambda.
        """
        body = make_if(sym('flag'), sym('val'), integer(0))
        lam = make_lambda([], body)
        expr = make_let([('flag', boolean(True)), ('val', integer(42))], lam)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'flag', 'val'})

    # --- quote ---

    def test_quote_has_no_free_vars(self):
        """
        (lambda () (quote (a b c)))
        Quoted symbols are data, not variable references.
        """
        body = lst(sym('quote'), lst(sym('a'), sym('b'), sym('c')))
        lam = make_lambda([], body)
        info = self._analyze(lam)
        assert info.get(lam) == frozenset()

    # --- variadic lambda ---

    def test_variadic_lambda_dot_not_counted_as_param(self):
        """
        (let ((x 1))
          (lambda (a . rest) (integer+ a x)))
        x is free; a and rest are params (dot is a sentinel, not a param name).
        """
        body = make_call(sym('integer+'), sym('a'), sym('x'))
        # Variadic lambda: (lambda (a . rest) body)
        params = lst(sym('a'), sym('.'), sym('rest'))
        lam = MenaiASTList((sym('lambda'), params, body))
        expr = make_let([('x', integer(1))], lam)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'x'})

    # --- Multiple lambdas at the same level ---

    def test_two_sibling_lambdas_independent(self):
        """
        (let ((a 1) (b 2))
          (let ((f (lambda () a))
                (g (lambda () b)))
            (list f g)))
        f is free in a; g is free in b.  They don't interfere.
        """
        lam_f = make_lambda([], sym('a'))
        lam_g = make_lambda([], sym('b'))
        inner = make_let([('f', lam_f), ('g', lam_g)],
                         make_call(sym('list'), sym('f'), sym('g')))
        expr = make_let([('a', integer(1)), ('b', integer(2))], inner)
        info = self._analyze(expr)
        assert info.get(lam_f) == frozenset({'a'})
        assert info.get(lam_g) == frozenset({'b'})

    # --- trace form ---

    def test_trace_body_free_vars_collected(self):
        """
        (let ((x 1))
          (lambda () (trace "msg" x)))
        x is free in the lambda.
        """
        body = lst(sym('trace'), string("msg"), sym('x'))
        lam = make_lambda([], body)
        expr = make_let([('x', integer(1))], lam)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'x'})

    # --- Non-lambda top-level expressions ---

    def test_no_lambdas_returns_empty_info(self):
        """A tree with no lambdas produces an empty FreeVarInfo."""
        expr = make_call(sym('integer+'), integer(1), integer(2))
        info = self._analyze(expr)
        assert info.all_lambdas() == []

    def test_literal_root_returns_empty_info(self):
        """A literal at the root produces an empty FreeVarInfo."""
        info = self._analyze(integer(99))
        assert info.all_lambdas() == []

    # --- Let binding value with a lambda ---

    def test_lambda_in_let_binding_value(self):
        """
        (let ((a 5))
          (let ((f (lambda (x) (integer+ x a))))
            (f 3)))
        The lambda is in a let binding value; a is free in it.
        """
        body = make_call(sym('integer+'), sym('x'), sym('a'))
        lam = make_lambda(['x'], body)
        inner = make_let([('f', lam)], make_call(sym('f'), integer(3)))
        expr = make_let([('a', integer(5))], inner)
        info = self._analyze(expr)
        assert info.get(lam) == frozenset({'a'})

    # --- Deep nesting ---

    def test_three_level_nesting(self):
        """
        (let ((a 1))
          (lambda (b)
            (lambda (c)
              (lambda (d)
                (integer+ a (integer+ b (integer+ c d)))))))
        Outer lambda (params b): free in a
        Middle lambda (params c): free in a, b
        Inner lambda (params d): free in a, b, c
        """
        innermost_body = make_call(
            sym('integer+'), sym('a'),
            make_call(sym('integer+'), sym('b'),
                      make_call(sym('integer+'), sym('c'), sym('d')))
        )
        inner_lam = make_lambda(['d'], innermost_body)
        mid_lam = make_lambda(['c'], inner_lam)
        outer_lam = make_lambda(['b'], mid_lam)
        expr = make_let([('a', integer(1))], outer_lam)
        info = self._analyze(expr)
        assert info.get(outer_lam) == frozenset({'a'})
        assert info.get(mid_lam) == frozenset({'a', 'b'})
        assert info.get(inner_lam) == frozenset({'a', 'b', 'c'})


# ===========================================================================
# 2. Agreement tests
# ===========================================================================

class TestFreeVarAnalyzerAgreement:
    """
    Agreement tests: verify that MenaiFreeVarAnalyzer produces results
    consistent with the IR builder's free_vars + parent_refs for every
    lambda in the compiled IR.

    After the PATCH_CLOSURE refactor, letrec siblings are regular free_vars
    (no parent_refs distinction).  The agreement invariant is simply:

        analyzer_free_vars(lambda) == frozenset(ir_lambda.sibling_free_vars + ir_lambda.outer_free_vars)

    We check this for every MenaiIRLambda in the IR tree.
    """

    def _check_agreement(self, source: str) -> None:
        """
        Compile source, run the analyzer on the desugared AST, collect all
        IR lambdas, and assert the agreement invariant for each.
        """
        desugared = _compile_to_desugared(source)
        info = MenaiFreeVarAnalyzer().analyze(desugared)
        ir = _compile_to_ir(source)

        ir_lambdas = _collect_ir_lambdas(ir)

        # The total number of IR lambdas must equal the number of lambdas
        # the analyzer annotated.  This catches cases where the analyzer
        # misses a lambda entirely.
        assert len(ir_lambdas) == len(info.all_lambdas()), (
            f"IR has {len(ir_lambdas)} lambdas but analyzer annotated "
            f"{len(info.all_lambdas())}"
        )

        # For each IR lambda, check that the union of free_vars + parent_refs
        # equals the analyzer's frozenset.
        # We match by position in the IR tree (depth-first order), which
        # corresponds to the order lambdas appear in the source.
        analyzer_sets = sorted(
            [fv for _, fv in info.all_lambdas()],
            key=lambda s: sorted(s)
        )
        ir_sets = sorted(
            [frozenset(lam.sibling_free_vars + lam.outer_free_vars) for lam in ir_lambdas],
            key=lambda s: sorted(s)
        )
        assert analyzer_sets == ir_sets, (
            f"Free var sets disagree.\n"
            f"  Analyzer: {analyzer_sets}\n"
            f"  IR builder: {ir_sets}"
        )

    def test_agreement_no_closures(self):
        """Simple expression with no closures."""
        self._check_agreement('(integer+ 1 2)')

    def test_agreement_simple_lambda_no_free_vars(self):
        """Lambda with no free variables."""
        self._check_agreement('(lambda (x) (integer+ x 1))')

    def test_agreement_simple_closure(self):
        """Lambda capturing one variable from enclosing let."""
        self._check_agreement('''
        (let ((a 10))
          (lambda (x) (integer+ x a)))
        ''')

    def test_agreement_multi_var_closure(self):
        """Lambda capturing multiple variables."""
        self._check_agreement('''
        (let ((a 1) (b 2) (c 3))
          (lambda () (integer+ a (integer+ b c))))
        ''')

    def test_agreement_self_recursive_letrec(self):
        """Self-recursive letrec lambda."""
        self._check_agreement('''
        (letrec ((fact (lambda (n)
                         (if (integer<=? n 1)
                             1
                             (integer* n (fact (integer- n 1)))))))
          (fact 5))
        ''')

    def test_agreement_mutual_recursion(self):
        """Mutually recursive letrec lambdas."""
        self._check_agreement('''
        (letrec ((even? (lambda (n) (if (integer=? n 0) #t (odd?  (integer- n 1)))))
                 (odd?  (lambda (n) (if (integer=? n 0) #f (even? (integer- n 1))))))
          (even? 10))
        ''')

    def test_agreement_letrec_with_non_lambda_binding(self):
        """Mixed letrec: non-lambda + recursive lambda (desugared to let+letrec)."""
        self._check_agreement('''
        (letrec ((base 10)
                 (fact (lambda (n)
                          (if (integer<=? n 1) base
                              (integer* n (fact (integer- n 1)))))))
          (fact 5))
        ''')

    def test_agreement_nested_lambdas(self):
        """Nested lambdas where inner captures outer param."""
        self._check_agreement('''
        (lambda (x) (lambda (y) (integer+ x y)))
        ''')

    def test_agreement_deeply_nested_closures(self):
        """Three levels of nesting, each capturing from the level above."""
        self._check_agreement('''
        (let ((a 1))
          (lambda (b)
            (lambda (c)
              (integer+ a (integer+ b c)))))
        ''')

    def test_agreement_closure_in_let_binding(self):
        """Lambda in a let binding value that captures an outer binding."""
        self._check_agreement('''
        (let ((offset 100))
          (let ((add-offset (lambda (x) (integer+ x offset))))
            (add-offset 5)))
        ''')

    def test_agreement_higher_order_function(self):
        """Lambda passed to a higher-order function, capturing from outer scope."""
        self._check_agreement('''
        (let ((factor 3))
          (map-list (lambda (x) (integer* x factor)) (list 1 2 3)))
        ''')

    def test_agreement_closure_over_closure(self):
        """Closure that returns a closure (currying pattern)."""
        self._check_agreement('''
        (lambda (x)
          (lambda (y)
            (lambda (z)
              (integer+ x (integer+ y z)))))
        ''')

    def test_agreement_match_desugared_form(self):
        """match desugars to if/let; closures inside match arms."""
        self._check_agreement('''
        (let ((base 10))
          (lambda (n)
            (match n
              (0 base)
              (_ (integer+ n base)))))
        ''')

    def test_agreement_letrec_captures_outer_let(self):
        """Letrec lambda that captures from an enclosing let."""
        self._check_agreement('''
        (let ((limit 0))
          (letrec ((count-down (lambda (n)
                                 (if (integer<=? n limit)
                                     n
                                     (count-down (integer- n 1))))))
            (count-down 10)))
        ''')

    def test_agreement_mutual_recursion_with_shared_capture(self):
        """Mutually recursive lambdas both capturing the same outer variable."""
        self._check_agreement('''
        (let ((zero 0))
          (letrec ((even? (lambda (n) (if (integer=? n zero) #t (odd?  (integer- n 1)))))
                   (odd?  (lambda (n) (if (integer=? n zero) #f (even? (integer- n 1))))))
            (even? 10)))
        ''')

    def test_agreement_lambda_in_if_branches(self):
        """Lambdas appearing in both branches of an if."""
        self._check_agreement('''
        (let ((a 1) (b 2))
          (if #t
              (lambda () a)
              (lambda () b)))
        ''')

    def test_agreement_trace_form(self):
        """trace form with a closure inside."""
        self._check_agreement('''
        (let ((x 42))
          (trace "value" (lambda () x)))
        ''')

    def test_agreement_variadic_lambda(self):
        """Variadic lambda capturing an outer binding."""
        self._check_agreement('''
        (let ((prefix "hello"))
          (lambda (a . rest) (string-concat prefix a)))
        ''')

    def test_agreement_letrec_non_recursive_lambda(self):
        """Non-recursive lambda in letrec (desugared to let)."""
        self._check_agreement('''
        (letrec ((square (lambda (x) (integer* x x))))
          (square 6))
        ''')

    def test_agreement_complex_rubiks_like_pattern(self):
        """
        Pattern similar to the rubiks_cube.menai motivating case:
        non-lambda binding (a list) captured by lambdas.
        """
        self._check_agreement('''
        (letrec ((all-moves (list "U" "D" "F" "B"))
                 (get-candidates (lambda (last-move)
                                   (filter-list
                                     (lambda (m) (string!=? m last-move))
                                     all-moves))))
          (get-candidates "U"))
        ''')
