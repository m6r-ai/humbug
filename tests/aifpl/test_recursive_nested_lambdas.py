"""
Tests for recursive functions calling themselves from within nested lambdas.

This test suite covers the bug where recursive functions failed when calling
themselves from within nested lambdas (e.g., inside map/filter/fold).

The bug was fixed by:
1. Making closures store a reference to their parent environment
2. Making LOAD_NAME traverse the environment chain to find recursive bindings

These tests ensure that recursive bindings are properly accessible from nested
lambda contexts, which is essential for patterns like recursive graph traversal.
"""

import pytest


class TestRecursiveNestedLambdas:
    """Test recursive functions calling themselves from within nested lambdas."""

    def test_simple_direct_recursion(self, aifpl, helpers):
        """Test that simple direct recursion still works (baseline)."""
        # Basic factorial
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((factorial (lambda (n)
                                 (if (<= n 1)
                                     1
                                     (* n (factorial (- n 1)))))))
              (factorial 5))''',
            '120'
        )

        # Two bindings with recursion
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((x 10)
                  (factorial (lambda (n)
                              (if (<= n 1)
                                  1
                                  (* n (factorial (- n 1)))))))
              (+ x (factorial 5)))''',
            '130'
        )

    def test_nested_lambda_recursion(self, aifpl, helpers):
        """Test recursion where outer lambda returns inner lambda that calls outer."""
        # Lambda factory pattern
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((make-factorial
                       (lambda ()
                         (lambda (n)
                           (if (<= n 1)
                               1
                               (* n ((make-factorial) (- n 1))))))))
              ((make-factorial) 5))''',
            '120'
        )

    def test_recursion_inside_map(self, aifpl, helpers):
        """Test recursive function calling itself from within map."""
        # Simple case: recursive call inside map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((visit (lambda (id path)
                          (if (member? id path)
                              (list)
                              (let ((new-path (cons id path)))
                                (if (> id 1)
                                    (fold append (list)
                                          (map (lambda (next-id) (visit next-id new-path))
                                               (list (- id 1))))
                                    (list id)))))))
              (visit 3 (list)))''',
            '(1)'
        )

    def test_simplified_dfs_visit(self, aifpl, helpers):
        """Test simplified DFS-like structure with recursion."""
        # Simplified version without map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((all-ids (list 1 2 3))
                  (visit (lambda (id path)
                          (if (member? id path)
                              (list id)
                              (let ((new-path (cons id path)))
                                (if (> id 1)
                                    (visit (- id 1) new-path)
                                    (list id)))))))
              (visit 3 (list)))''',
            '(1)'
        )

    def test_actual_dfs_visit_structure(self, aifpl, helpers):
        """Test actual DFS visit structure from detect-cycles."""
        # Full DFS structure with map and fold
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((all-task-ids (list "T001" "T002" "T003"))
                  (get-successors (lambda (task-id)
                                   (if (string=? task-id "T001")
                                       (list "T002")
                                       (list))))
                  (dfs-visit
                    (lambda (task-id path visited-in-path)
                      (if (member? task-id visited-in-path)
                          (list (list "cycle" task-id))
                          (let ((successors (get-successors task-id)))
                            (let ((new-path (append path (list task-id)))
                                  (new-visited (cons task-id visited-in-path)))
                              (fold append (list)
                                    (map (lambda (succ) (dfs-visit succ new-path new-visited))
                                         successors))))))))
              (fold append (list)
                    (map (lambda (task-id) (dfs-visit task-id (list) (list)))
                         all-task-ids)))''',
            '()'  # No cycles detected
        )

    def test_recursion_in_filter(self, aifpl, helpers):
        """Test recursive function calling itself from within filter."""
        # Recursive predicate in filter
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((is-valid (lambda (n)
                                 (if (<= n 1)
                                     #t
                                     (and (> n 0) (is-valid (- n 1)))))))
              (filter (lambda (x) (is-valid x)) (list 1 2 3)))''',
            '(1 2 3)'
        )

    def test_recursion_in_fold(self, aifpl, helpers):
        """Test recursive function calling itself from within fold."""
        # Recursive function used in fold accumulator
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((sum-to (lambda (n)
                               (if (<= n 0)
                                   0
                                   (+ n (sum-to (- n 1)))))))
              (fold + 0
                    (map (lambda (x) (sum-to x)) (list 1 2 3))))''',
            '10'  # sum-to(1)=1, sum-to(2)=3, sum-to(3)=6, total=10
        )

    def test_mutual_recursion_in_map(self, aifpl, helpers):
        """Test mutual recursion where functions call each other from within map."""
        # Even/odd mutual recursion used in map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((is-even (lambda (n) (if (= n 0) #t (is-odd (- n 1)))))
                  (is-odd (lambda (n) (if (= n 0) #f (is-even (- n 1))))))
              (map (lambda (x) (is-even x)) (list 0 1 2 3 4)))''',
            '(#t #f #t #f #t)'
        )

    def test_nested_recursion_multiple_levels(self, aifpl, helpers):
        """Test recursion through multiple levels of nested lambdas."""
        # Three levels: outer recursive function -> map -> inner lambda -> recursive call
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((process (lambda (n)
                               (if (<= n 0)
                                   (list)
                                   (fold append
                                         (list n)
                                         (map (lambda (x) (process (- x 1)))
                                              (list n)))))))
              (process 3))''',
            '(3 2 1)'
        )

    def test_recursion_with_any_all(self, aifpl, helpers):
        """Test recursive functions used with any? and all? predicates."""
        # Recursive predicate in any?
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((has-path (lambda (n)
                                 (if (<= n 1)
                                     #t
                                     (has-path (- n 1))))))
              (any? (lambda (x) (has-path x)) (list 1 2 3)))''',
            '#t'
        )

        # Recursive predicate in all?
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((is-positive-chain (lambda (n)
                                           (if (<= n 1)
                                               #t
                                               (and (> n 0) (is-positive-chain (- n 1)))))))
              (all? (lambda (x) (is-positive-chain x)) (list 1 2 3)))''',
            '#t'
        )

    def test_recursion_with_find(self, aifpl, helpers):
        """Test recursive function used as predicate in find."""
        # Recursive predicate in find
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((reaches-target (lambda (n target)
                                        (if (= n target)
                                           #t
                                           (if (< n target)
                                               #f
                                               (reaches-target (- n 1) target))))))
              (find (lambda (x) (reaches-target x 5)) (list 3 5 7)))''',
            '5'
        )

    def test_complex_recursive_graph_traversal(self, aifpl, helpers):
        """Test complex recursive graph traversal pattern."""
        # Simulated graph traversal with adjacency list
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((get-neighbors (lambda (node)
                                       (if (= node 1)
                                           (list 2 3)
                                           (if (= node 2)
                                               (list 4)
                                               (list)))))
                  (visit-all (lambda (node visited)
                              (if (member? node visited)
                                  visited
                                  (let ((new-visited (cons node visited)))
                                    (fold (lambda (acc neighbor)
                                           (visit-all neighbor acc))
                                          new-visited
                                          (get-neighbors node)))))))
              (length (visit-all 1 (list))))''',
            '4'  # Visits nodes 1, 2, 3, 4
        )

    def test_recursion_through_let_in_map(self, aifpl, helpers):
        """Test recursion where recursive call is inside a let within map."""
        # Recursive call inside let inside map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((compute (lambda (n)
                               (if (<= n 1)
                                   (list n)
                                   (fold append
                                         (list n)
                                         (map (lambda (x)
                                               (let ((result (compute (- x 1))))
                                                 result))
                                              (list n)))))))
              (compute 3))''',
            '(3 2 1)'
        )

    def test_recursion_with_conditional_in_map(self, aifpl, helpers):
        """Test recursion with conditional logic inside map."""
        # Recursive call in conditional inside map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((process (lambda (n)
                               (if (<= n 0)
                                   0
                                   (fold +
                                         n
                                         (map (lambda (x)
                                               (if (> x 1)
                                                   (process (- x 1))
                                                   0))
                                              (list n)))))))
              (process 3))''',
            '6'  # 3 + process(2) = 3 + (2 + process(1)) = 3 + (2 + 1) = 6
        )

    def test_deeply_nested_recursive_calls(self, aifpl, helpers):
        """Test recursive calls through deeply nested lambda structures."""
        # Four levels of nesting
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((outer (lambda (n)
                             (if (<= n 0)
                                 0
                                 (fold +
                                       0
                                       (map (lambda (a)
                                             (fold +
                                                   0
                                                   (map (lambda (b)
                                                         (if (> b 0)
                                                             (outer (- b 1))
                                                             b))
                                                        (list a))))
                                            (list n)))))))
              (outer 2))''',
            '0'
        )

    def test_recursion_with_multiple_recursive_calls_in_map(self, aifpl, helpers):
        """Test function making multiple recursive calls within map."""
        # Multiple recursive calls in the same map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((fib (lambda (n)
                           (if (<= n 1)
                               n
                               (fold +
                                     0
                                     (map (lambda (x) (fib x))
                                          (list (- n 1) (- n 2))))))))
              (fib 6))''',
            '8'  # Fibonacci(6) = 8
        )

    def test_recursion_closure_capture(self, aifpl, helpers):
        """Test that recursive functions properly capture their environment."""
        # Recursive function capturing outer variables
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((base 10)
                  (compute (lambda (n)
                            (if (<= n 0)
                                base
                                (fold +
                                      0
                                      (map (lambda (x) (compute (- x 1)))
                                           (list n)))))))
              (compute 2))''',
            '10'  # compute(2) -> map over (list 2) -> compute(1) -> base = 10
        )

    def test_performance_reasonable_recursion_depth(self, aifpl, helpers):
        """Test that recursive calls through map don't cause excessive overhead."""
        # Moderate recursion depth (should complete quickly)
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((count-down (lambda (n acc)
                                   (if (<= n 0)
                                       acc
                                       (fold +
                                             0
                                             (map (lambda (x) (count-down (- x 1) (+ acc 1)))
                                                   (list n)))))))
              (count-down 10 0))''',
            '10'
        )


class TestRecursiveNestedLambdasBytecode:
    """Test recursive nested lambdas specifically in bytecode mode."""

    def test_bytecode_simple_recursion_in_map(self, aifpl_bytecode, helpers):
        """Test bytecode compilation of recursive calls in map."""
        helpers.assert_evaluates_to(
            aifpl_bytecode,
            '''(letrec ((visit (lambda (n)
                             (if (<= n 1)
                                 (list n)
                                 (fold append
                                       (list n)
                                       (map (lambda (x) (visit (- x 1)))
                                            (list n)))))))
              (visit 4))''',
            '(4 3 2 1)'
        )

    def test_bytecode_nested_lambda_factory(self, aifpl_bytecode, helpers):
        """Test bytecode compilation of nested lambda factory pattern."""
        helpers.assert_evaluates_to(
            aifpl_bytecode,
            '''(letrec ((make-counter
                       (lambda (start)
                         (lambda (n)
                           (if (<= n 0)
                               start
                               ((make-counter (+ start 1)) (- n 1)))))))
              ((make-counter 0) 5))''',
            '0'  # Returns start value after 5 recursive calls
        )

    def test_bytecode_mutual_recursion_in_map(self, aifpl_bytecode, helpers):
        """Test bytecode compilation of mutual recursion in map."""
        helpers.assert_evaluates_to(
            aifpl_bytecode,
            '''(letrec ((ping (lambda (n) (if (<= n 0) 0 (+ 1 (pong (- n 1))))))
                  (pong (lambda (n) (if (<= n 0) 0 (+ 1 (ping (- n 1)))))))
              (map (lambda (x) (ping x)) (list 1 2 3)))''',
            '(1 2 3)'
        )

    def test_bytecode_environment_chain_lookup(self, aifpl_bytecode, helpers):
        """Test that bytecode properly traverses environment chain for recursive bindings."""
        helpers.assert_evaluates_to(
            aifpl_bytecode,
            '''(letrec ((outer 100)
                  (recurse (lambda (n)
                            (if (<= n 0)
                                outer
                                (fold +
                                      0
                                      (map (lambda (x) (recurse (- x 1)))
                                           (list n)))))))
              (recurse 3))''',
            '100'  # recurse(3) -> map over (list 3) -> recurse(2) -> ... -> recurse(0) -> outer = 100
        )
