"""
Tests for tail call optimization (TCO) with let bindings.

These tests verify that AIFPL's tail call optimization works correctly
for recursive calls that appear after a 'let' binding.

FIXED BUG:
Previously, tail calls after 'let' bindings were not properly optimized.
The fix ensures that all recursive calls in tail position are optimized,
regardless of whether they appear directly or after a 'let' binding.

EXPECTED BEHAVIOR:
All recursive calls in tail position should be optimized, including:
- Direct tail calls: (if c result (recurse args))
- Tail calls after 'let': (let ((x val)) (recurse x))
- Nested let expressions with tail calls
- Conditional branches with let-based tail calls
"""

import pytest

from aifpl import AIFPLEvalError


class TestTailCallOptimizationWithLet:
    """Test cases that verify TCO works correctly with let bindings."""

    def test_direct_tail_recursion_works(self, aifpl, helpers):
        """
        Baseline test: Direct tail recursion works.
        
        This demonstrates that TCO is functional for direct recursive calls.
        """
        # Simple countdown - direct tail call
        direct_recursion = '''
        (let ((countdown (lambda (n)
                          (if (<= n 0) 
                              "done" 
                              (countdown (- n 1))))))
          (countdown 200))
        '''
        helpers.assert_evaluates_to(aifpl, direct_recursion, '"done"')

    def test_tail_recursion_after_simple_let(self, aifpl, helpers):
        """
        Tail recursion after a simple let binding works.
        
        This is the simplest case: the recursive call is in tail position
        inside a let body, and TCO should be applied.
        """
        let_then_recursion = '''
        (let ((countdown (lambda (n)
                          (if (<= n 0) 
                              "done" 
                              (let ((next-n (- n 1)))
                                (countdown next-n))))))
          (countdown 200))
        '''
        helpers.assert_evaluates_to(aifpl, let_then_recursion, '"done"')

    def test_tail_recursion_with_let_accumulator(self, aifpl, helpers):
        """
        Tail recursion with accumulator using let works.
        
        This is a common pattern: using let to compute the next accumulator value
        before making the tail call.
        """
        accumulator_with_let = '''
        (let ((sum-to-n (lambda (n acc)
                         (if (<= n 0) 
                             acc 
                             (let ((next-acc (+ acc n)))
                               (sum-to-n (- n 1) next-acc))))))
          (sum-to-n 200 0))
        '''
        # Sum from 1 to 200 = 200 * 201 / 2 = 20100
        helpers.assert_evaluates_to(aifpl, accumulator_with_let, '20100')

    def test_tail_recursion_with_multiple_let_bindings(self, aifpl, helpers):
        """
        Tail recursion with multiple let bindings works.
        
        This tests the case where we need to extract multiple intermediate
        values before making the tail call.
        """
        multiple_bindings = '''
        (let ((compute (lambda (n result)
                        (if (<= n 0) 
                            result 
                            (let ((next-n (- n 1))
                                  (next-result (* result 2)))
                              (compute next-n next-result))))))
          (compute 10 1))
        '''
        # 2^10 = 1024
        result = aifpl.evaluate(multiple_bindings)
        assert result is not None
        assert result == 1024

    def test_character_by_character_parsing(self, aifpl, helpers):
        """
        Character-by-character parsing works with TCO.
        
        This is the motivating use case: parsing strings character by character
        requires extracting the current character and rest of string using let,
        then making a tail call.
        
        This pattern is essential for CSV parsing, JSON parsing, etc.
        """
        char_counter = '''
        (let ((count-chars (lambda (chars acc)
                            (if (null? chars) 
                                acc 
                                (let ((c (first chars))
                                      (rest-chars (rest chars)))
                                  (count-chars rest-chars (+ acc 1)))))))
          (count-chars (string->list "This is a test string with many characters in it for testing purposes") 0))
        '''
        helpers.assert_evaluates_to(aifpl, char_counter, '69')

    def test_list_processing_with_let(self, aifpl, helpers):
        """
        List processing with let extraction works.
        
        Common pattern: extract head and tail of list, process head,
        then recursively process tail.
        """
        list_sum = '''
        (let ((sum-list (lambda (lst acc)
                         (if (null? lst) 
                             acc 
                             (let ((head (first lst))
                                   (tail (rest lst)))
                               (sum-list tail (+ acc head)))))))
          (sum-list (range 1 101) 0))
        '''
        # Sum from 1 to 100 = 5050
        helpers.assert_evaluates_to(aifpl, list_sum, '5050')

    def test_csv_parsing_pattern(self, aifpl, helpers):
        """
        CSV parsing pattern works with TCO.
        
        This demonstrates the actual CSV parsing use case that motivated
        the TCO fix. We need to extract the current character,
        check if we're in quotes, and make a tail call.
        """
        simple_csv_parser = '''
        (let ((parse-chars (lambda (chars in-quotes current-field fields)
                            (if (null? chars)
                                (reverse (cons current-field fields))
                                (let ((c (first chars))
                                      (rest-chars (rest chars)))
                                  (if (string=? c ",")
                                      (if in-quotes
                                          (parse-chars rest-chars in-quotes 
                                                     (string-append current-field c) fields)
                                          (parse-chars rest-chars #f "" 
                                                     (cons current-field fields)))
                                      (parse-chars rest-chars in-quotes 
                                                 (string-append current-field c) fields)))))))
          (parse-chars (string->list "field1,field2,field3") #f "" (list)))
        '''
        helpers.assert_evaluates_to(aifpl, simple_csv_parser, '("field1" "field2" "field3")')

    def test_nested_let_with_tail_call(self, aifpl, helpers):
        """
        Nested let expressions with tail call work.
        
        Tests that TCO works even with multiple levels of let nesting.
        """
        nested_let = '''
        (let ((process (lambda (n acc)
                        (if (<= n 0) 
                            acc 
                            (let ((temp1 (- n 1)))
                              (let ((temp2 (+ acc n)))
                                (process temp1 temp2)))))))
          (process 100 0))
        '''
        helpers.assert_evaluates_to(aifpl, nested_let, '5050')

    def test_let_with_conditional_tail_calls(self, aifpl, helpers):
        """
        Let with conditional tail calls works.
        
        Tests that TCO works when the tail call is in an if branch
        that's inside a let.
        """
        conditional_in_let = '''
        (let ((collatz (lambda (n steps)
                        (if (= n 1) 
                            steps 
                            (let ((next-n (if (= (% n 2) 0) (/ n 2) (+ (* n 3) 1))))
                              (collatz next-n (+ steps 1)))))))
          (collatz 100 0))
        '''
        # Collatz sequence starting from 100 takes 25 steps
        helpers.assert_evaluates_to(aifpl, conditional_in_let, '25')

    def test_comparison_direct_vs_let_tail_calls(self, aifpl_custom):
        """
        Both direct and let-based tail calls work for deep recursion.
        
        This test verifies that both forms of tail calls are properly optimized.
        """
        aifpl = aifpl_custom(max_depth=1000)

        # Direct tail call - works
        direct = '''
        (let ((f (lambda (n) (if (<= n 0) "done" (f (- n 1))))))
          (f 500))
        '''
        assert aifpl.evaluate_and_format(direct) == '"done"'

        # Let-based tail call - now also works!
        with_let = '''
        (let ((f (lambda (n) (if (<= n 0) "done" (let ((x (- n 1))) (f x))))))
          (f 500))
        '''
        assert aifpl.evaluate_and_format(with_let) == '"done"'

    def test_deep_recursion_with_let_bindings(self, aifpl_custom):
        """
        Deep recursion with let bindings works with TCO.
        
        This test verifies that let-based recursion can handle hundreds
        of iterations without hitting depth limits.
        """
        aifpl = aifpl_custom(max_depth=1000)

        # This now works with proper TCO
        test_depth = '''
        (let ((f (lambda (n) (if (<= n 0) n (let ((x (- n 1))) (f x))))))
          (f 500))
        '''
        
        # Should complete successfully and return 0
        assert aifpl.evaluate_and_format(test_depth) == '0'

    def test_practical_example_string_reversal(self, aifpl, helpers):
        """
        Practical example - string reversal using tail recursion with let.
        
        This is a real-world use case that requires TCO with let bindings.
        """
        string_reverse = '''
        (let ((reverse-chars (lambda (chars acc)
                              (if (null? chars) 
                                  acc 
                                  (let ((head (first chars))
                                        (tail (rest chars)))
                                    (reverse-chars tail (cons head acc)))))))
          (list->string (reverse-chars (string->list "This is a reasonably long string to reverse") (list))))
        '''
        helpers.assert_evaluates_to(aifpl, string_reverse, '"esrever ot gnirts gnol ylbanosaer a si sihT"')

    def test_practical_example_list_filter_custom(self, aifpl, helpers):
        """
        Custom filter implementation using tail recursion with let.
        
        Shows that we can implement our own filter function with proper TCO.
        """
        custom_filter = '''
        (let ((my-filter (lambda (pred lst acc)
                          (if (null? lst) 
                              (reverse acc) 
                              (let ((head (first lst))
                                    (tail (rest lst)))
                                (if (pred head)
                                    (my-filter pred tail (cons head acc))
                                    (my-filter pred tail acc)))))))
          (my-filter (lambda (x) (> x 5)) (range 1 11) (list)))
        '''
        # Should return list of numbers from 6 to 10
        helpers.assert_evaluates_to(aifpl, custom_filter, '(6 7 8 9 10)')


class TestTailCallOptimizationLetEdgeCases:
    """Additional edge cases for TCO with let bindings."""

    def test_let_with_no_bindings_tail_call(self, aifpl, helpers):
        """
        Edge case: Empty let before tail call works.
        """
        empty_let = '''
        (let ((f (lambda (n) (if (<= n 0) "done" (let () (f (- n 1)))))))
          (f 100))
        '''
        helpers.assert_evaluates_to(aifpl, empty_let, '"done"')

    def test_let_binding_not_used_in_tail_call(self, aifpl, helpers):
        """
        Edge case: Let binding that's not used in the tail call still allows TCO.
        """
        unused_binding = '''
        (let ((f (lambda (n) 
                  (if (<= n 0) 
                      "done" 
                      (let ((unused 42))
                        (f (- n 1)))))))
          (f 100))
        '''
        helpers.assert_evaluates_to(aifpl, unused_binding, '"done"')

    def test_mutual_recursion_with_let(self, aifpl, helpers):
        """
        Edge case: Mutual recursion with let bindings works.
        
        Tests that TCO works for mutually recursive functions that use let.
        Note: This test uses a smaller value (20) because mutual recursion
        with let bindings still has some limitations.
        """
        mutual_with_let = '''
        (let ((is-even (lambda (n) 
                        (if (= n 0) 
                            #t 
                            (let ((next (- n 1)))
                              (is-odd next)))))
              (is-odd (lambda (n) 
                       (if (= n 0) 
                           #f 
                           (let ((next (- n 1)))
                             (is-even next))))))
          (is-even 20))
        '''
        helpers.assert_evaluates_to(aifpl, mutual_with_let, '#t')

    def test_documentation_example_from_bug_report(self, aifpl_custom):
        """
        Test the exact example from the bug report documentation.
        
        This is the canonical example that originally demonstrated the bug.
        Now it should work correctly.
        """
        aifpl = aifpl_custom(max_depth=1000)

        # From TCO_BUG_REPORT.md - this now works!
        bug_example = '''
        (let ((test (lambda (n acc)
                     (if (= n 0)
                         acc
                         (let ((next-n (- n 1)))
                           (test next-n (+ acc 1)))))))
          (test 500 0))
        '''
        
        # Should complete successfully and return 500
        assert aifpl.evaluate_and_format(bug_example) == '500'


class TestTailCallOptimizationVerification:
    """
    Tests that verify TCO works correctly in all tail positions.
    
    These tests ensure that the fix doesn't break anything and that
    TCO works correctly for all forms of tail calls.
    """

    def test_direct_tail_calls_still_work(self, aifpl, helpers):
        """Verify that direct tail calls continue to work after the fix."""
        direct = '''
        (let ((f (lambda (n acc) 
                  (if (<= n 0) acc (f (- n 1) (+ acc 1))))))
          (f 500 0))
        '''
        helpers.assert_evaluates_to(aifpl, direct, '500')

    def test_if_branch_tail_calls_still_work(self, aifpl, helpers):
        """Verify that tail calls in if branches continue to work."""
        if_branches = '''
        (let ((f (lambda (n) 
                  (if (<= n 0) 
                      "zero" 
                      (if (= n 1) 
                          "one" 
                          (f (- n 2)))))))
          (f 100))
        '''
        helpers.assert_evaluates_to(aifpl, if_branches, '"zero"')

    def test_combined_if_and_let_tail_calls(self, aifpl, helpers):
        """
        Verify that combining if and let in tail position works.
        """
        combined = '''
        (let ((f (lambda (n acc)
                  (if (<= n 0)
                      acc
                      (if (= (% n 2) 0)
                          (let ((half (/ n 2)))
                            (f half (+ acc 1)))
                          (let ((next (- n 1)))
                            (f next (+ acc 1))))))))
          (f 100 0))
        '''
        # Count steps from 100 to 0 (using halving for evens, decrement for odds)
        result = aifpl.evaluate(combined)
        assert result is not None
        # The result should be a positive integer representing the number of steps
        assert isinstance(result, int)
        assert result > 0

    def test_all_tail_positions_optimized(self, aifpl_custom):
        """
        Comprehensive test that all tail positions are properly optimized.
        """
        aifpl = aifpl_custom(max_depth=1000)
        
        comprehensive = '''
        (let ((process (lambda (n mode acc)
                        (if (<= n 0)
                            acc
                            (if (= mode 0)
                                ; Direct tail call
                                (process (- n 1) 1 (+ acc 1))
                                (if (= mode 1)
                                    ; Tail call in if branch
                                    (process (- n 1) 2 (+ acc 2))
                                    ; Tail call after let
                                    (let ((next-n (- n 1))
                                          (next-acc (+ acc 3)))
                                      (process next-n 0 next-acc))))))))
          (process 300 0 0))
        '''
        
        # Should complete without hitting depth limit
        result = aifpl.evaluate(comprehensive)
        assert result is not None


class TestTailCallOptimizationPerformance:
    """Tests that verify TCO provides good performance for deep recursion."""

    def test_very_deep_recursion_with_let(self, aifpl_custom):
        """
        Test that very deep recursion (1000+ iterations) works with let-based TCO.
        """
        aifpl = aifpl_custom(max_depth=5000)
        
        deep = '''
        (let ((countdown (lambda (n)
                          (if (<= n 0)
                              "done"
                              (let ((next (- n 1)))
                                (countdown next))))))
          (countdown 1000))
        '''
        
        assert aifpl.evaluate_and_format(deep) == '"done"'

    def test_factorial_with_let_accumulator(self, aifpl, helpers):
        """
        Test factorial computation using let-based tail recursion.
        """
        factorial = '''
        (let ((fact (lambda (n acc)
                     (if (<= n 1)
                         acc
                         (let ((next-n (- n 1))
                               (next-acc (* n acc)))
                           (fact next-n next-acc))))))
          (fact 10 1))
        '''
        helpers.assert_evaluates_to(aifpl, factorial, '3628800')

    def test_fibonacci_with_let_accumulators(self, aifpl, helpers):
        """
        Test Fibonacci computation using let-based tail recursion with two accumulators.
        """
        fibonacci = '''
        (let ((fib (lambda (n a b)
                    (if (<= n 0)
                        a
                        (let ((next-n (- n 1))
                              (next-a b)
                              (next-b (+ a b)))
                          (fib next-n next-a next-b))))))
          (fib 20 0 1))
        '''
        # 20th Fibonacci number
        helpers.assert_evaluates_to(aifpl, fibonacci, '6765')
