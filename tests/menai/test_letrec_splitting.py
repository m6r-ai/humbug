"""
Tests for letrec splitting in the desugarer.

The desugarer's _desugar_letrec pass splits mixed letrec forms into nested
let/letrec forms in topological order:

  - Non-recursive binding (lambda or non-lambda) -> let
  - Recursive group (always lambdas after splitting) -> letrec

This eliminates the mixed-binding problem that previously prevented lambda
lifting: after splitting, every remaining letrec contains only a genuine
mutually-recursive group of lambdas.

Test organisation:
  - Splitting correctness: the right structure is produced
  - Semantic equivalence: split forms evaluate identically to the originals
  - Existing letrec patterns still work unchanged
  - Edge cases: empty bindings, all non-recursive, all recursive, etc.
"""

import pytest


class TestLetrecSplittingSemantics:
    """
    Verify that split letrec forms evaluate identically to the originals.

    These are end-to-end tests through the full pipeline.  They do not
    inspect the intermediate AST structure; they only check that the
    runtime result is correct.
    """

    def test_non_lambda_binding_before_lambda(self, menai, helpers):
        """
        Non-lambda binding followed by a lambda that captures it.

        This is the canonical mixed-binding case from the design doc:
        all-moves is a list value, get-candidates is a lambda that uses it.
        """
        src = '''
        (letrec ((all-moves (list "U" "D" "F"))
                 (get-candidates (lambda (last-move)
                                   (filter-list (lambda (m) (string!=? m last-move))
                                                all-moves))))
          (get-candidates "U"))
        '''
        helpers.assert_evaluates_to(menai, src, '("D" "F")')

    def test_non_lambda_binding_used_by_two_lambdas(self, menai, helpers):
        """
        Single non-lambda binding captured by two independent lambdas.
        Both lambdas should see the same value.
        """
        src = '''
        (letrec ((base 10)
                 (add-base (lambda (x) (integer+ x base)))
                 (mul-base (lambda (x) (integer* x base))))
          (list (add-base 5) (mul-base 3)))
        '''
        helpers.assert_evaluates_to(menai, src, '(15 30)')

    def test_non_lambda_before_mutually_recursive_lambdas(self, menai, helpers):
        """
        Non-lambda binding followed by a mutually recursive lambda group.
        The recursive group must stay as a letrec; the non-lambda becomes a let.
        """
        src = '''
        (letrec ((limit 0)
                 (even? (lambda (n) (if (integer=? n limit) #t (odd?  (integer- n 1)))))
                 (odd?  (lambda (n) (if (integer=? n limit) #f (even? (integer- n 1))))))
          (even? 10))
        '''
        helpers.assert_evaluates_to(menai, src, '#t')

    def test_multiple_non_lambda_bindings(self, menai, helpers):
        """
        Multiple non-lambda bindings each become their own let.
        """
        src = '''
        (letrec ((a 1)
                 (b 2)
                 (c 3)
                 (sum (lambda () (integer+ a (integer+ b c)))))
          (sum))
        '''
        helpers.assert_evaluates_to(menai, src, '6')

    def test_non_lambda_binding_after_lambda(self, menai, helpers):
        """
        Non-lambda binding that appears after a lambda in source order.
        Topological sort must still place the non-lambda before the lambda
        that depends on it (or independently if there is no dependency).
        """
        src = '''
        (letrec ((double (lambda (x) (integer* x factor)))
                 (factor 2))
          (double 7))
        '''
        helpers.assert_evaluates_to(menai, src, '14')

    def test_chain_of_non_lambda_dependencies(self, menai, helpers):
        """
        Non-lambda bindings that depend on each other form a chain.
        Each should become a separate let in the correct order.
        """
        src = '''
        (letrec ((a 3)
                 (b (integer* a 2))
                 (c (integer+ a b))
                 (show (lambda () c)))
          (show))
        '''
        helpers.assert_evaluates_to(menai, src, '9')

    def test_non_recursive_lambda_becomes_let(self, menai, helpers):
        """
        A lambda binding that is not self- or mutually-recursive should
        become a let, not a letrec.  It must still be callable.
        """
        src = '''
        (letrec ((square (lambda (x) (integer* x x))))
          (square 6))
        '''
        helpers.assert_evaluates_to(menai, src, '36')

    def test_self_recursive_lambda_stays_letrec(self, menai, helpers):
        """
        A self-recursive lambda must remain a letrec so it can reference itself.
        """
        src = '''
        (letrec ((fact (lambda (n)
                         (if (integer<=? n 1) 1 (integer* n (fact (integer- n 1)))))))
          (fact 6))
        '''
        helpers.assert_evaluates_to(menai, src, '720')

    def test_mutually_recursive_lambdas_stay_letrec(self, menai, helpers):
        """
        Mutually recursive lambdas must remain in a single letrec group.
        """
        src = '''
        (letrec ((even? (lambda (n) (if (integer=? n 0) #t (odd?  (integer- n 1)))))
                 (odd?  (lambda (n) (if (integer=? n 0) #f (even? (integer- n 1))))))
          (even? 100))
        '''
        helpers.assert_evaluates_to(menai, src, '#t')

    def test_mixed_self_recursive_and_non_lambda(self, menai, helpers):
        """
        Mix of a self-recursive lambda and a non-lambda binding.
        The non-lambda becomes a let; the recursive lambda stays letrec.
        """
        src = '''
        (letrec ((base 10)
                 (fact (lambda (n)
                         (if (integer<=? n 1) base (integer* n (fact (integer- n 1)))))))
          (fact 5))
        '''
        # fact(5) = 5*4*3*2*1*base = 120 * 10 ... wait: base replaces the 1 base case
        # fact(1) = base = 10, fact(2) = 2*10 = 20, fact(3) = 3*20 = 60,
        # fact(4) = 4*60 = 240, fact(5) = 5*240 = 1200
        helpers.assert_evaluates_to(menai, src, '1200')

    def test_non_lambda_captured_inside_nested_lambda(self, menai, helpers):
        """
        Non-lambda binding captured by a lambda that is itself nested inside
        another lambda (closure over closure).
        """
        src = '''
        (letrec ((offset 100)
                 (make-adder (lambda (x) (lambda (y) (integer+ y (integer+ x offset))))))
          ((make-adder 5) 3))
        '''
        helpers.assert_evaluates_to(menai, src, '108')

    def test_non_lambda_binding_is_a_list(self, menai, helpers):
        """
        Non-lambda binding that is a list literal (the motivating rubiks_cube case).
        """
        src = '''
        (letrec ((moves (list "U" "D" "F" "B" "L" "R"))
                 (count-moves (lambda () (list-length moves))))
          (count-moves))
        '''
        helpers.assert_evaluates_to(menai, src, '6')

    def test_non_lambda_binding_is_a_call(self, menai, helpers):
        """
        Non-lambda binding whose value is a function call result.
        """
        src = '''
        (letrec ((doubled (integer* 6 7))
                 (show (lambda () doubled)))
          (show))
        '''
        helpers.assert_evaluates_to(menai, src, '42')

    def test_purely_non_recursive_letrec_becomes_nested_lets(self, menai, helpers):
        """
        A letrec where no binding is recursive at all becomes nested lets.
        All bindings are independent; order is topological.
        """
        src = '''
        (letrec ((x 1)
                 (y 2)
                 (z 3))
          (integer+ x (integer+ y z)))
        '''
        helpers.assert_evaluates_to(menai, src, '6')

    def test_letrec_body_in_tail_position(self, menai, helpers):
        """
        The body of the split form is still in tail position; TCO must work.
        """
        src = '''
        (letrec ((limit 100000)
                 (loop (lambda (n)
                          (if (integer>=? n limit) n (loop (integer+ n 1))))))
          (loop 0))
        '''
        helpers.assert_evaluates_to(menai, src, '100000')

    def test_large_mutual_recursion_with_non_lambda_binding(self, menai, helpers):
        """
        TCO still works for the mutually recursive group after splitting.
        """
        src = '''
        (letrec ((zero 0)
                 (even? (lambda (n) (if (integer=? n zero) #t (odd?  (integer- n 1)))))
                 (odd?  (lambda (n) (if (integer=? n zero) #f (even? (integer- n 1))))))
          (even? 100000))
        '''
        helpers.assert_evaluates_to(menai, src, '#t')


class TestLetrecSplittingStructure:
    """
    Verify the desugarer produces the correct AST structure after splitting.

    These tests inspect the desugared AST directly rather than evaluating it,
    so they catch structural regressions independently of the IR/codegen.
    """

    def _desugar(self, source: str):
        """Helper: lex, parse, analyse, then desugar the source."""
        from menai.menai_lexer import MenaiLexer
        from menai.menai_ast_builder import MenaiASTBuilder
        from menai.menai_semantic_analyzer import MenaiSemanticAnalyzer
        from menai.menai_desugarer import MenaiDesugarer
        from menai.menai_ast import MenaiASTList, MenaiASTSymbol

        tokens = MenaiLexer().lex(source)
        ast = MenaiASTBuilder().build(tokens, source)
        ast = MenaiSemanticAnalyzer().analyze(ast, source)
        return MenaiDesugarer().desugar(ast)

    def _head_symbol(self, node) -> str:
        """Return the name of the first element of a list node."""
        from menai.menai_ast import MenaiASTList, MenaiASTSymbol
        assert isinstance(node, MenaiASTList)
        assert isinstance(node.elements[0], MenaiASTSymbol)
        return node.elements[0].name

    def test_pure_recursive_letrec_unchanged(self):
        """
        A letrec with only mutually recursive lambdas must remain a letrec.
        """
        from menai.menai_ast import MenaiASTList, MenaiASTSymbol

        result = self._desugar('''
        (letrec ((even? (lambda (n) (if (integer=? n 0) #t (odd?  (integer- n 1)))))
                 (odd?  (lambda (n) (if (integer=? n 0) #f (even? (integer- n 1))))))
          (even? 4))
        ''')
        # Top-level form must be letrec
        assert self._head_symbol(result) == 'letrec'

    def test_non_recursive_lambda_becomes_let(self):
        """
        A single non-recursive lambda binding must become a let.
        """
        from menai.menai_ast import MenaiASTList, MenaiASTSymbol

        result = self._desugar('''
        (letrec ((square (lambda (x) (integer* x x))))
          (square 5))
        ''')
        # Non-recursive lambda -> let
        assert self._head_symbol(result) == 'let'

    def test_non_lambda_binding_becomes_let(self):
        """
        A non-lambda binding must become a let.
        """
        from menai.menai_ast import MenaiASTList

        result = self._desugar('''
        (letrec ((x 42))
          x)
        ''')
        assert self._head_symbol(result) == 'let'

    def test_mixed_produces_let_wrapping_letrec(self):
        """
        A non-lambda binding followed by a recursive lambda group must produce
        (let ((non-lambda ...)) (letrec ((lambda ...)) body)).
        """
        from menai.menai_ast import MenaiASTList, MenaiASTSymbol

        result = self._desugar('''
        (letrec ((base 10)
                 (fact (lambda (n)
                          (if (integer<=? n 1) 1 (integer* n (fact (integer- n 1)))))))
          (fact 5))
        ''')
        # Outer form: let (for base)
        assert self._head_symbol(result) == 'let'

        # Inner form (body of the let): letrec (for fact)
        assert isinstance(result, MenaiASTList)
        inner = result.elements[2]
        assert self._head_symbol(inner) == 'letrec'

    def test_two_non_lambda_bindings_produce_nested_lets(self):
        """
        Two independent non-lambda bindings must produce two nested lets.
        """
        from menai.menai_ast import MenaiASTList

        result = self._desugar('''
        (letrec ((a 1)
                 (b 2))
          (integer+ a b))
        ''')
        # Outer: let
        assert self._head_symbol(result) == 'let'
        # Inner body: let
        assert isinstance(result, MenaiASTList)
        inner = result.elements[2]
        assert self._head_symbol(inner) == 'let'

    def test_non_lambda_then_mutual_recursion(self):
        """
        Non-lambda binding + mutually recursive group -> let wrapping letrec
        where the letrec has both lambda bindings.
        """
        from menai.menai_ast import MenaiASTList, MenaiASTSymbol

        result = self._desugar('''
        (letrec ((zero 0)
                 (even? (lambda (n) (if (integer=? n zero) #t (odd?  (integer- n 1)))))
                 (odd?  (lambda (n) (if (integer=? n zero) #f (even? (integer- n 1))))))
          (even? 4))
        ''')
        # Outer: let (for zero)
        assert self._head_symbol(result) == 'let'

        # Inner: letrec (for even? and odd?)
        assert isinstance(result, MenaiASTList)
        inner = result.elements[2]
        assert self._head_symbol(inner) == 'letrec'

        # The letrec must contain both even? and odd?
        assert isinstance(inner, MenaiASTList)
        letrec_bindings = inner.elements[1]
        assert isinstance(letrec_bindings, MenaiASTList)
        assert len(letrec_bindings.elements) == 2

        bound_names = set()
        for b in letrec_bindings.elements:
            assert isinstance(b, MenaiASTList)
            assert isinstance(b.elements[0], MenaiASTSymbol)
            bound_names.add(b.elements[0].name)
        assert bound_names == {'even?', 'odd?'}


class TestLetrecSplittingEdgeCases:
    """Edge cases and boundary conditions for letrec splitting."""

    def test_single_non_lambda_binding(self, menai, helpers):
        """Single non-lambda binding in letrec."""
        helpers.assert_evaluates_to(menai, '(letrec ((x 99)) x)', '99')

    def test_single_recursive_lambda(self, menai, helpers):
        """Single self-recursive lambda — must stay letrec."""
        src = '(letrec ((f (lambda (n) (if (integer<=? n 0) 0 (f (integer- n 1)))))) (f 5))'
        helpers.assert_evaluates_to(menai, src, '0')

    def test_single_non_recursive_lambda(self, menai, helpers):
        """Single non-recursive lambda — becomes let."""
        src = '(letrec ((f (lambda (x) (integer+ x 1)))) (f 41))'
        helpers.assert_evaluates_to(menai, src, '42')

    def test_binding_that_references_earlier_non_lambda(self, menai, helpers):
        """
        A non-lambda binding that references another non-lambda binding
        defined earlier in the same letrec.
        """
        src = '''
        (letrec ((a 5)
                 (b (integer* a 2))
                 (f (lambda () b)))
          (f))
        '''
        helpers.assert_evaluates_to(menai, src, '10')

    def test_lambda_referencing_sibling_non_recursive_lambda(self, menai, helpers):
        """
        A lambda that calls another lambda, neither being recursive.
        Both become lets; the inner one must be in scope when the outer is called.
        """
        src = '''
        (letrec ((double (lambda (x) (integer* x 2)))
                 (quad   (lambda (x) (double (double x)))))
          (quad 3))
        '''
        helpers.assert_evaluates_to(menai, src, '12')

    def test_non_lambda_binding_is_boolean(self, menai, helpers):
        """Non-lambda binding that is a boolean literal."""
        src = '(letrec ((flag #t) (f (lambda () flag))) (f))'
        helpers.assert_evaluates_to(menai, src, '#t')

    def test_non_lambda_binding_is_string(self, menai, helpers):
        """Non-lambda binding that is a string literal."""
        src = '(letrec ((greeting "hello") (f (lambda () greeting))) (f))'
        helpers.assert_evaluates_to(menai, src, '"hello"')

    def test_non_lambda_binding_is_empty_list(self, menai, helpers):
        """Non-lambda binding that is an empty list."""
        src = '(letrec ((empty (list)) (f (lambda () empty))) (f))'
        helpers.assert_evaluates_to(menai, src, '()')

    def test_letrec_inside_letrec(self, menai, helpers):
        """
        Nested letrec forms — both should be split independently.
        """
        src = '''
        (letrec ((outer-val 10)
                 (outer-f (lambda (x)
                            (letrec ((inner-val 5)
                                     (inner-f (lambda (y) (integer+ y inner-val))))
                              (integer+ x (inner-f outer-val))))))
          (outer-f 3))
        '''
        # inner-f(outer-val) = 5 + 10 = 15; outer-f(3) = 3 + 15 = 18
        helpers.assert_evaluates_to(menai, src, '18')

    def test_letrec_inside_let_body(self, menai, helpers):
        """
        A letrec inside a let body is also split correctly.
        """
        src = '''
        (let ((base 100))
          (letrec ((offset 7)
                   (f (lambda (x) (integer+ x (integer+ base offset)))))
            (f 0)))
        '''
        helpers.assert_evaluates_to(menai, src, '107')

    def test_shadow_outer_binding(self, menai, helpers):
        """
        A letrec binding that shadows an outer let binding.
        The inner binding must take precedence inside the letrec body.
        """
        src = '''
        (let ((x 1))
          (letrec ((x 2)
                   (f (lambda () x)))
            (f)))
        '''
        helpers.assert_evaluates_to(menai, src, '2')

    def test_source_location_preserved(self, menai, helpers):
        """
        Splitting must not break compilation even when source location
        metadata is present (regression guard).
        """
        src = '''
        (letrec ((a 1)
                 (b (integer+ a 1))
                 (f (lambda (n) (integer+ n b))))
          (f a))
        '''
        helpers.assert_evaluates_to(menai, src, '3')
