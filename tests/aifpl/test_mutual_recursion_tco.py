"""
Tests for Tail Call Optimization (TCO) with Mutual Recursion.

These tests verify that AIFPL's tail call optimization works correctly
for mutually recursive functions (functions that call each other).
"""

import pytest

from aifpl import AIFPLEvalError


class TestMutualRecursionBasic:
    """Basic mutual recursion tests - the canonical even?/odd? example."""

    def test_simple_mutual_recursion_small_value(self, aifpl, helpers):
        """
        Simple mutual recursion works for small values.

        This should work even without TCO optimization.
        """
        even_odd = '''
        (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
          (even? 10))
        '''
        helpers.assert_evaluates_to(aifpl, even_odd, '#t')

    def test_simple_mutual_recursion_medium_value(self, aifpl, helpers):
        """
        Simple mutual recursion works for medium values (100).

        This should work even without TCO optimization.
        """
        even_odd = '''
        (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
          (even? 100))
        '''
        helpers.assert_evaluates_to(aifpl, even_odd, '#t')

    def test_simple_mutual_recursion_large_value(self, aifpl, helpers):
        """
        Simple mutual recursion works for large values (10000).

        This REQUIRES TCO to work - will fail without it.
        """
        even_odd = '''
        (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
          (even? 10000))
        '''
        helpers.assert_evaluates_to(aifpl, even_odd, '#t')

    def test_simple_mutual_recursion_very_large_value(self, aifpl, helpers):
        """
        Simple mutual recursion works for very large values (100000).

        This is the stress test from the issue documentation.
        """
        even_odd = '''
        (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
          (even? 100000))
        '''
        helpers.assert_evaluates_to(aifpl, even_odd, '#t')

    def test_odd_check_large_value(self, aifpl, helpers):
        """
        Test odd? function with large value.

        Verifies that both directions of mutual recursion work.
        """
        even_odd = '''
        (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
          (odd? 10001))
        '''
        helpers.assert_evaluates_to(aifpl, even_odd, '#t')


class TestMutualRecursionWithAccumulators:
    """Mutual recursion with accumulator parameters."""

    def test_mutual_recursion_with_accumulator(self, aifpl, helpers):
        """
        Mutual recursion with accumulator parameter works.

        Tests that TCO works when mutually recursive functions carry state.
        """
        mutual_with_acc = '''
        (letrec ((count-down-a (lambda (n acc)
                                 (if (<= n 0)
                                     acc
                                     (count-down-b (- n 1) (+ acc 1)))))
                 (count-down-b (lambda (n acc)
                                 (if (<= n 0)
                                     acc
                                     (count-down-a (- n 1) (+ acc 2))))))
          (count-down-a 5000 0))
        '''
        result = aifpl.evaluate(mutual_with_acc)
        assert result is not None
        assert isinstance(result, int)

    def test_mutual_recursion_list_processing(self, aifpl, helpers):
        """
        Mutual recursion for list processing works.

        Tests a practical use case: processing lists with alternating logic.
        """
        list_processor = '''
        (letrec ((process-evens (lambda (lst acc)
                                  (if (null? lst)
                                      acc
                                      (process-odds (rest lst) (cons (first lst) acc)))))
                 (process-odds (lambda (lst acc)
                                 (if (null? lst)
                                     acc
                                     (process-evens (rest lst) acc)))))
          (process-evens (range 1 10001) (list)))
        '''
        result = aifpl.evaluate(list_processor)
        assert result is not None


class TestMutualRecursionMultipleFunctions:
    """Mutual recursion with more than two functions."""

    def test_three_way_mutual_recursion(self, aifpl, helpers):
        """
        Three mutually recursive functions work.

        Tests that TCO works for more complex mutual recursion patterns.
        """
        three_way = '''
        (letrec ((func-a (lambda (n) (if (<= n 0) "a" (func-b (- n 1)))))
                 (func-b (lambda (n) (if (<= n 0) "b" (func-c (- n 1)))))
                 (func-c (lambda (n) (if (<= n 0) "c" (func-a (- n 1))))))
          (func-a 10000))
        '''
        result = aifpl.evaluate_and_format(three_way)
        assert result in ['"a"', '"b"', '"c"']

    def test_four_way_mutual_recursion(self, aifpl, helpers):
        """
        Four mutually recursive functions work.

        Tests even more complex mutual recursion patterns.
        """
        four_way = '''
        (letrec ((func-a (lambda (n) (if (<= n 0) 1 (func-b (- n 1)))))
                 (func-b (lambda (n) (if (<= n 0) 2 (func-c (- n 1)))))
                 (func-c (lambda (n) (if (<= n 0) 3 (func-d (- n 1)))))
                 (func-d (lambda (n) (if (<= n 0) 4 (func-a (- n 1))))))
          (func-a 10000))
        '''
        result = aifpl.evaluate(four_way)
        assert result in [1, 2, 3, 4]


class TestMutualRecursionWithLet:
    """Mutual recursion combined with let bindings."""

    def test_mutual_recursion_with_let_bindings(self, aifpl, helpers):
        """
        Mutual recursion with let bindings works.

        Combines two features: mutual recursion and let-based tail calls.
        """
        mutual_with_let = '''
        (letrec ((even? (lambda (n)
                          (if (= n 0)
                              #t
                              (let ((next (- n 1)))
                                (odd? next)))))
                 (odd? (lambda (n)
                         (if (= n 0)
                             #f
                             (let ((next (- n 1)))
                               (even? next))))))
          (even? 10000))
        '''
        helpers.assert_evaluates_to(aifpl, mutual_with_let, '#t')

    def test_mutual_recursion_with_multiple_let_bindings(self, aifpl, helpers):
        """
        Mutual recursion with multiple let bindings works.
        """
        mutual_complex = '''
        (letrec ((func-a (lambda (n acc)
                          (if (<= n 0)
                              acc
                              (let ((next-n (- n 1))
                                    (next-acc (+ acc 1)))
                                (func-b next-n next-acc)))))
                 (func-b (lambda (n acc)
                          (if (<= n 0)
                              acc
                              (let ((next-n (- n 1))
                                    (next-acc (* acc 2)))
                                (func-a next-n next-acc))))))
          (func-a 1000 1))
        '''
        result = aifpl.evaluate(mutual_complex)
        assert result is not None


class TestMutualRecursionEdgeCases:
    """Edge cases for mutual recursion TCO."""

    def test_mutual_recursion_different_arities_small(self, aifpl, helpers):
        """
        Mutual recursion with different arities works for small values.

        Tests that functions with different parameter counts can be mutually recursive.
        This should work even without TCO.
        """
        different_arities = '''
        (letrec ((func-one (lambda (n) (if (<= n 0) "done" (func-two n 0))))
                 (func-two (lambda (n acc) (if (<= n 0) acc (func-one (- n 1))))))
          (func-one 10))
        '''
        result = aifpl.evaluate_and_format(different_arities)
        assert result in ['"done"', '0']

    def test_mutual_recursion_different_arities_large(self, aifpl, helpers):
        """
        Mutual recursion with different arities works for large values.

        Tests that TCO handles functions with different parameter counts.
        """
        different_arities = '''
        (letrec ((func-one (lambda (n) (if (<= n 0) "done" (func-two n 0))))
                 (func-two (lambda (n acc) (if (<= n 0) acc (func-one (- n 1))))))
          (func-one 10000))
        '''
        result = aifpl.evaluate_and_format(different_arities)
        assert result in ['"done"', '0']

    def test_mutual_recursion_with_conditionals(self, aifpl, helpers):
        """
        Mutual recursion with complex conditionals works.

        Tests that TCO works with multiple conditional branches.
        """
        with_conditionals = '''
        (letrec ((process-a (lambda (n mode)
                             (if (<= n 0)
                                 mode
                                 (if (= mode 0)
                                     (process-b (- n 1) 1)
                                     (process-b (- n 1) 0)))))
                 (process-b (lambda (n mode)
                             (if (<= n 0)
                                 mode
                                 (if (= mode 0)
                                     (process-a (- n 1) 1)
                                     (process-a (- n 1) 0))))))
          (process-a 10000 0))
        '''
        result = aifpl.evaluate(with_conditionals)
        assert result in [0, 1]

    def test_self_and_mutual_recursion_mixed_small(self, aifpl, helpers):
        """
        Mix of self-recursion and mutual recursion works for small values.

        Tests that both patterns can coexist in the same letrec.
        """
        mixed = '''
        (letrec ((self-rec (lambda (n) (if (<= n 0) 0 (self-rec (- n 1)))))
                 (mutual-a (lambda (n) (if (<= n 0) 1 (mutual-b (- n 1)))))
                 (mutual-b (lambda (n) (if (<= n 0) 2 (mutual-a (- n 1))))))
          (list (self-rec 10) (mutual-a 10)))
        '''
        helpers.assert_evaluates_to(aifpl, mixed, '(0 1)')

    def test_self_and_mutual_recursion_mixed_large(self, aifpl, helpers):
        """
        Mix of self-recursion and mutual recursion works for large values.

        Self-recursion should use existing TCO, mutual recursion should use new TCO.
        """
        mixed = '''
        (letrec ((self-rec (lambda (n) (if (<= n 0) 0 (self-rec (- n 1)))))
                 (mutual-a (lambda (n) (if (<= n 0) 1 (mutual-b (- n 1)))))
                 (mutual-b (lambda (n) (if (<= n 0) 2 (mutual-a (- n 1))))))
          (list (self-rec 10000) (mutual-a 10000)))
        '''
        helpers.assert_evaluates_to(aifpl, mixed, '(0 1)')


class TestMutualRecursionPracticalExamples:
    """Practical examples that benefit from mutual recursion TCO."""

    def test_state_machine_parser(self, aifpl, helpers):
        """
        State machine parser using mutual recursion.

        Practical example: parsing with different states represented as functions.
        """
        state_machine = '''
        (letrec ((state-normal (lambda (chars acc)
                                 (if (null? chars)
                                     acc
                                     (let ((c (first chars)))
                                       (if (string=? c "<")
                                           (state-tag (rest chars) (string-append acc c))
                                           (state-normal (rest chars) (string-append acc c)))))))
                 (state-tag (lambda (chars acc)
                             (if (null? chars)
                                 acc
                                 (let ((c (first chars)))
                                   (if (string=? c ">")
                                       (state-normal (rest chars) (string-append acc c))
                                       (state-tag (rest chars) (string-append acc c))))))))
          (state-normal (string->list "This <is> a <test> string") ""))
        '''
        result = aifpl.evaluate_and_format(state_machine)
        assert "test" in result

    def test_alternating_list_processor(self, aifpl, helpers):
        """
        Alternating list processor using mutual recursion.

        Practical example: processing list elements with alternating logic.
        """
        alternating = '''
        (letrec ((process-pos (lambda (lst pos-acc neg-acc)
                               (if (null? lst)
                                   (list pos-acc neg-acc)
                                   (let ((val (first lst)))
                                     (process-neg (rest lst) (+ pos-acc val) neg-acc)))))
                 (process-neg (lambda (lst pos-acc neg-acc)
                               (if (null? lst)
                                   (list pos-acc neg-acc)
                                   (let ((val (first lst)))
                                     (process-pos (rest lst) pos-acc (+ neg-acc val)))))))
          (process-pos (range 1 1001) 0 0))
        '''
        result = aifpl.evaluate(alternating)
        assert result is not None


class TestMutualRecursionPerformance:
    """Performance tests to verify TCO eliminates stack overflow."""

    def test_very_deep_mutual_recursion(self, aifpl):
        """
        Very deep mutual recursion (100000+ iterations) works.

        This is the ultimate test - the value from the issue documentation.
        """
        deep = '''
        (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
          (even? 100000))
        '''

        assert aifpl.evaluate_and_format(deep) == '#t'

    def test_comparison_self_vs_mutual_recursion(self, aifpl):
        """
        Compare performance of self-recursion vs mutual recursion.

        Both should handle the same depth without stack overflow.
        """
        # Self-recursion (already works)
        self_rec = '''
        (letrec ((countdown (lambda (n) (if (<= n 0) "done" (countdown (- n 1))))))
          (countdown 100000))
        '''
        assert aifpl.evaluate_and_format(self_rec) == '"done"'

        # Mutual recursion (now works!)
        mutual_rec = '''
        (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
          (even? 100000))
        '''
        assert aifpl.evaluate_and_format(mutual_rec) == '#t'


class TestMutualRecursionCorrectness:
    """Tests to ensure mutual recursion produces correct results."""

    def test_even_odd_correctness_small_values(self, aifpl, helpers):
        """
        Verify even?/odd? produces correct results for small values.

        This validates the logic is correct before testing TCO.
        """
        tests = [
            ('(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 0))', '#t'),
            ('(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (odd? 0))', '#f'),
            ('(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 1))', '#f'),
            ('(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (odd? 1))', '#t'),
            ('(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 4))', '#t'),
            ('(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (odd? 5))', '#t'),
        ]

        for expr, expected in tests:
            helpers.assert_evaluates_to(aifpl, expr, expected)

    def test_mutual_recursion_returns_correct_values(self, aifpl, helpers):
        """
        Verify mutual recursion returns correct values for large inputs.

        This ensures TCO doesn't break correctness.
        """
        # Test even numbers
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10000))',
            '#t'
        )

        # Test odd numbers
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (odd? 10001))',
            '#t'
        )
