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
                                 (if (integer<=? n 1)
                                     1
                                     (integer* n (factorial (integer- n 1)))))))
              (factorial 5))''',
            '120'
        )

        # Two bindings with recursion
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((x 10)
                  (factorial (lambda (n)
                              (if (integer<=? n 1)
                                  1
                                  (integer* n (factorial (integer- n 1)))))))
              (integer+ x (factorial 5)))''',
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
                           (if (integer<=? n 1)
                               1
                               (integer* n ((make-factorial) (integer- n 1))))))))
              ((make-factorial) 5))''',
            '120'
        )

    def test_recursion_inside_map(self, aifpl, helpers):
        """Test recursive function calling itself from within map."""
        # Simple case: recursive call inside map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((visit (lambda (id path)
                          (if (list-member? id path)
                              (list)
                              (let ((new-path (list-cons id path)))
                                (if (integer>? id 1)
                                    (fold append (list)
                                          (map (lambda (next-id) (visit next-id new-path))
                                               (list (integer- id 1))))
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
                          (if (list-member? id path)
                              (list id)
                              (let ((new-path (list-cons id path)))
                                (if (integer>? id 1)
                                    (visit (integer- id 1) new-path)
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
                      (if (list-member? task-id visited-in-path)
                          (list (list "cycle" task-id))
                          (let ((successors (get-successors task-id)))
                            (let ((new-path (append path (list task-id)))
                                  (new-visited (list-cons task-id visited-in-path)))
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
                                 (if (integer<=? n 1)
                                     #t
                                     (and (integer>? n 0) (is-valid (integer- n 1)))))))
              (filter (lambda (x) (is-valid x)) (list 1 2 3)))''',
            '(1 2 3)'
        )

    def test_recursion_in_fold(self, aifpl, helpers):
        """Test recursive function calling itself from within fold."""
        # Recursive function used in fold accumulator
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((sum-to (lambda (n)
                               (if (integer<=? n 0)
                                   0
                                   (integer+ n (sum-to (integer- n 1)))))))
              (fold integer+ 0
                    (map (lambda (x) (sum-to x)) (list 1 2 3))))''',
            '10'  # sum-to(1)=1, sum-to(2)=3, sum-to(3)=6, total=10
        )

    def test_mutual_recursion_in_map(self, aifpl, helpers):
        """Test mutual recursion where functions call each other from within map."""
        # Even/odd mutual recursion used in map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((is-even (lambda (n) (if (integer=? n 0) #t (is-odd (integer- n 1)))))
                  (is-odd (lambda (n) (if (integer=? n 0) #f (is-even (integer- n 1))))))
              (map (lambda (x) (is-even x)) (list 0 1 2 3 4)))''',
            '(#t #f #t #f #t)'
        )

    def test_nested_recursion_multiple_levels(self, aifpl, helpers):
        """Test recursion through multiple levels of nested lambdas."""
        # Three levels: outer recursive function -> map -> inner lambda -> recursive call
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((process (lambda (n)
                               (if (integer<=? n 0)
                                   (list)
                                   (fold append
                                         (list n)
                                         (map (lambda (x) (process (integer- x 1)))
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
                                 (if (integer<=? n 1)
                                     #t
                                     (has-path (integer- n 1))))))
              (any? (lambda (x) (has-path x)) (list 1 2 3)))''',
            '#t'
        )

        # Recursive predicate in all?
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((is-positive-chain (lambda (n)
                                           (if (integer<=? n 1)
                                               #t
                                               (and (integer>? n 0) (is-positive-chain (integer- n 1)))))))
              (all? (lambda (x) (is-positive-chain x)) (list 1 2 3)))''',
            '#t'
        )

    def test_recursion_with_find(self, aifpl, helpers):
        """Test recursive function used as predicate in find."""
        # Recursive predicate in find
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((reaches-target (lambda (n target)
                                        (if (integer=? n target)
                                           #t
                                           (if (integer<? n target)
                                               #f
                                               (reaches-target (integer- n 1) target))))))
              (find (lambda (x) (reaches-target x 5)) (list 3 5 7)))''',
            '5'
        )

    def test_complex_recursive_graph_traversal(self, aifpl, helpers):
        """Test complex recursive graph traversal pattern."""
        # Simulated graph traversal with adjacency list
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((get-neighbors (lambda (node)
                                       (if (integer=? node 1)
                                           (list 2 3)
                                           (if (integer=? node 2)
                                               (list 4)
                                               (list)))))
                  (visit-all (lambda (node visited)
                              (if (list-member? node visited)
                                  visited
                                  (let ((new-visited (list-cons node visited)))
                                    (fold (lambda (acc neighbor)
                                           (visit-all neighbor acc))
                                          new-visited
                                          (get-neighbors node)))))))
              (list-length (visit-all 1 (list))))''',
            '4'  # Visits nodes 1, 2, 3, 4
        )

    def test_recursion_through_let_in_map(self, aifpl, helpers):
        """Test recursion where recursive call is inside a let within map."""
        # Recursive call inside let inside map
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((compute (lambda (n)
                               (if (integer<=? n 1)
                                   (list n)
                                   (fold append
                                         (list n)
                                         (map (lambda (x)
                                               (let ((result (compute (integer- x 1))))
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
                               (if (integer<=? n 0)
                                   0
                                   (fold integer+
                                         n
                                         (map (lambda (x)
                                               (if (integer>? x 1)
                                                   (process (integer- x 1))
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
                             (if (integer<=? n 0)
                                 0
                                 (fold integer+
                                       0
                                       (map (lambda (a)
                                             (fold integer+
                                                   0
                                                   (map (lambda (b)
                                                         (if (integer>? b 0)
                                                             (outer (integer- b 1))
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
                           (if (integer<=? n 1)
                               n
                               (fold integer+
                                     0
                                     (map (lambda (x) (fib x))
                                          (list (integer- n 1) (integer- n 2))))))))
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
                            (if (integer<=? n 0)
                                base
                                (fold integer+
                                      0
                                      (map (lambda (x) (compute (integer- x 1)))
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
                                   (if (integer<=? n 0)
                                       acc
                                       (fold integer+
                                             0
                                             (map (lambda (x) (count-down (integer- x 1) (integer+ acc 1)))
                                                   (list n)))))))
              (count-down 10 0))''',
            '10'
        )


class TestRecursiveNestedLambdasBytecode:
    """Test recursive nested lambdas specifically in bytecode mode."""

    def test_bytecode_simple_recursion_in_map(self, aifpl, helpers):
        """Test bytecode compilation of recursive calls in map."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((visit (lambda (n)
                             (if (integer<=? n 1)
                                 (list n)
                                 (fold append
                                       (list n)
                                       (map (lambda (x) (visit (integer- x 1)))
                                            (list n)))))))
              (visit 4))''',
            '(4 3 2 1)'
        )

    def test_bytecode_nested_lambda_factory(self, aifpl, helpers):
        """Test bytecode compilation of nested lambda factory pattern."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((make-counter
                       (lambda (start)
                         (lambda (n)
                           (if (integer<=? n 0)
                               start
                               ((make-counter (integer+ start 1)) (integer- n 1)))))))
              ((make-counter 0) 5))''',
            '0'  # Returns start value after 5 recursive calls
        )

    def test_bytecode_mutual_recursion_in_map(self, aifpl, helpers):
        """Test bytecode compilation of mutual recursion in map."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((ping (lambda (n) (if (integer<=? n 0) 0 (integer+ 1 (pong (integer- n 1))))))
                  (pong (lambda (n) (if (integer<=? n 0) 0 (integer+ 1 (ping (integer- n 1)))))))
              (map (lambda (x) (ping x)) (list 1 2 3)))''',
            '(1 2 3)'
        )

    def test_bytecode_environment_chain_lookup(self, aifpl, helpers):
        """Test that bytecode properly traverses environment chain for recursive bindings."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((outer 100)
                  (recurse (lambda (n)
                            (if (integer<=? n 0)
                                outer
                                (fold integer+
                                      0
                                      (map (lambda (x) (recurse (integer- x 1)))
                                           (list n)))))))
              (recurse 3))''',
            '100'  # recurse(3) -> map over (list 3) -> recurse(2) -> ... -> recurse(0) -> outer = 100
        )



class TestLetrecLambdasInDataStructures:
    """
    Test lambdas nested in data structures that reference their letrec binding.

    This test suite covers the bug where lambdas nested inside data structures
    (like lists, cons cells, or returned from conditionals) couldn't properly
    reference their letrec binding variable.

    The bug was fixed by:
    1. Adding current_letrec_binding context to CompilationContext
    2. Making _compile_lambda check context when binding_name not provided
    3. Adding local_names dict to Frame to map variable names to local indices
    4. Making PATCH_CLOSURE_SELF register names in frame.local_names
    5. Making LOAD_NAME check parent frame local_names before failing

    These tests ensure that self-referential lambdas work correctly even when
    nested inside data structures, which is essential for patterns like creating
    recursive data structures or returning recursive functions from conditionals.
    """

    def test_lambda_in_list_simple(self, aifpl, helpers):
        """Test lambda nested in list can reference its binding."""
        # Lambda in list that references its binding
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x)))) x)',
            '(<lambda ()>)'  # Returns a list containing the lambda
        )

    def test_lambda_in_list_called_once(self, aifpl, helpers):
        """Test calling a lambda extracted from a list."""
        # Extract lambda from list and call it
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x)))) ((list-first x)))',
            '(<lambda ()>)'  # Calling lambda returns x (the list)
        )

    def test_lambda_in_list_called_multiple_times(self, aifpl, helpers):
        """Test calling lambda multiple times in a chain."""
        # Call lambda, get result, extract lambda, call again
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x)))) (list-first ((list-first x))))',
            '<lambda ()>'  # Returns the lambda itself
        )

    def test_lambda_in_list_deep_recursion(self, aifpl, helpers):
        """Test the original bug report case - deep recursive calls."""
        # Original test case: ((list-first ((list-first ((list-first ((list-first x))))))))
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x)))) ((list-first ((list-first ((list-first ((list-first x)))))))))',
            '(<lambda ()>)'  # Returns x after multiple recursive calls
        )

    def test_lambda_in_cons(self, aifpl, helpers):
        """Test lambda in cons cell can reference its binding."""
        # Lambda in cons cell
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list-cons (lambda () x) (list)))) (list-first x))',
            '<lambda ()>'  # Returns the lambda
        )

    def test_lambda_in_cons_called(self, aifpl, helpers):
        """Test calling lambda from cons cell."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list-cons (lambda () x) (list)))) ((list-first x)))',
            '(<lambda ()>)'  # Returns x (the cons cell)
        )

    def test_lambda_from_if_true_branch(self, aifpl, helpers):
        """Test lambda returned from if true branch."""
        # Lambda from if expression (true branch)
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (if #t (lambda () x) (lambda () 42)))) (x))',
            '<lambda ()>'  # Calling lambda returns itself
        )

    def test_lambda_from_if_false_branch(self, aifpl, helpers):
        """Test lambda returned from if false branch."""
        # Lambda from if expression (false branch)
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (if #f (lambda () 42) (lambda () x)))) (x))',
            '<lambda ()>'  # Calling lambda returns itself
        )

    def test_lambda_in_nested_list(self, aifpl, helpers):
        """Test lambda in nested list structure."""
        # Lambda in nested list
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (list (lambda () x))))) (list-first (list-first x)))',
            '<lambda ()>'  # Returns the lambda
        )

    def test_lambda_in_nested_list_called(self, aifpl, helpers):
        """Test calling lambda from nested list."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (list (lambda () x))))) ((list-first (list-first x))))',
            '((<lambda ()>))'  # Calling lambda returns x
        )

    def test_multiple_lambdas_in_list(self, aifpl, helpers):
        """Test multiple self-referential lambdas in same list."""
        # Multiple lambdas in list, each referencing x
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x) (lambda () x)))) (list-length x))',
            '2'  # List has two lambdas
        )

    def test_multiple_lambdas_in_list_called(self, aifpl, helpers):
        """Test calling different lambdas from list."""
        # Call first lambda
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x) (lambda () x)))) (list-length ((list-first x))))',
            '2'  # First lambda returns x which has length 2
        )

        # Call second lambda
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x) (lambda () x)))) (list-length ((list-first (list-rest x)))))',
            '2'  # Second lambda also returns x which has length 2
        )

    def test_lambda_with_parameters_in_list(self, aifpl, helpers):
        """Test lambda with parameters nested in list."""
        # Lambda with params that references its binding
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda (n) (if (integer<=? n 0) x (list n)))))) ((list-first x) 0))',
            '(<lambda (param0)>)'  # Calling with 0 returns x
        )

        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda (n) (if (integer<=? n 0) x (list n)))))) ((list-first x) 5))',
            '(5)'  # Calling with 5 returns (list 5)
        )

    def test_lambda_in_list_with_mutual_recursion(self, aifpl, helpers):
        """Test mutually recursive lambdas in data structures."""
        # Two mutually recursive functions in lists
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((x (list (lambda (n) (if (integer<=? n 0) 0 (integer+ 1 ((list-first y) (integer- n 1)))))))
                        (y (list (lambda (n) (if (integer<=? n 0) 0 (integer+ 1 ((list-first x) (integer- n 1))))))))
                  ((list-first x) 4))''',
            '4'  # Mutual recursion works
        )
    def test_lambda_in_alist(self, aifpl, helpers):
        """Test lambda nested in alist."""
        # Lambda as value in alist
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (alist (list "func" (lambda () x))))) (alist-get x "func"))',
            '<lambda ()>'  # Returns the lambda
        )

    def test_lambda_in_alist_called(self, aifpl, helpers):
        """Test calling lambda from alist."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (alist (list "func" (lambda () x))))) ((alist-get x "func")))',
            '{("func" <lambda ()>)}'  # Calling lambda returns x (the alist)
        )

    def test_non_self_referential_lambda_in_list(self, aifpl, helpers):
        """Test that non-self-referential lambdas in lists still work."""
        # Lambda that doesn't reference x
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () 42)))) ((list-first x)))',
            '42'  # Simple lambda works
        )

    def test_lambda_referencing_other_binding_in_list(self, aifpl, helpers):
        """Test lambda in list referencing different letrec binding."""
        # Lambda references y, not x
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () y))) (y 100)) ((list-first x)))',
            '100'  # Lambda returns y
        )


class TestLetrecLambdasInDataStructuresBytecode:
    """Test lambdas in data structures specifically in bytecode mode."""

    def test_bytecode_lambda_in_list_simple(self, aifpl, helpers):
        """Test bytecode compilation of lambda in list."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x)))) x)',
            '(<lambda ()>)'
        )

    def test_bytecode_lambda_in_list_called(self, aifpl, helpers):
        """Test bytecode execution of calling lambda from list."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x)))) ((list-first x)))',
            '(<lambda ()>)'
        )

    def test_bytecode_deep_recursion(self, aifpl, helpers):
        """Test bytecode handles deep recursive calls."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x)))) ((list-first ((list-first ((list-first ((list-first x)))))))))',
            '(<lambda ()>)'
        )

    def test_bytecode_lambda_from_if(self, aifpl, helpers):
        """Test bytecode compilation of lambda from if expression."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (if #t (lambda () x) (lambda () 42)))) (x))',
            '<lambda ()>'
        )

    def test_bytecode_multiple_lambdas(self, aifpl, helpers):
        """Test bytecode handles multiple lambdas in data structure."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (list (lambda () x) (lambda () x)))) (list-length ((list-first x))))',
            '2'
        )

    def test_bytecode_mutual_recursion_in_lists(self, aifpl, helpers):
        """Test bytecode compilation of mutual recursion in lists."""
        helpers.assert_evaluates_to(
            aifpl,
            '''(letrec ((x (list (lambda (n) (if (integer<=? n 0) 0 (integer+ 1 ((list-first y) (integer- n 1)))))))
                        (y (list (lambda (n) (if (integer<=? n 0) 0 (integer+ 1 ((list-first x) (integer- n 1))))))))
                  ((list-first x) 4))''',
            '4'
        )

    def test_bytecode_lambda_in_alist(self, aifpl, helpers):
        """Test bytecode compilation of lambda in alist."""
        helpers.assert_evaluates_to(
            aifpl,
            '(letrec ((x (alist (list "func" (lambda () x))))) ((alist-get x "func")))',
            '{("func" <lambda ()>)}'
        )
