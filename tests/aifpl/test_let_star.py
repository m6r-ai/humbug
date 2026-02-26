"""Tests for let and let* binding semantics."""

import pytest

from aifpl import AIFPLEvalError


class TestLetStarSemantics:
    """Test let* sequential binding semantics and let parallel binding semantics."""

    def test_let_star_sequential_bindings(self, aifpl, helpers):
        """Test that let* allows sequential bindings where later bindings reference earlier ones."""
        # Basic sequential binding
        helpers.assert_evaluates_to(aifpl, "(let* ((x 5) (y (integer* x 2))) (integer+ x y))", "15")

        # Multiple sequential bindings
        helpers.assert_evaluates_to(aifpl, "(let* ((a 1) (b (integer+ a 1)) (c (integer+ b 1))) (integer+ a b c))", "6")

        # Nested computation
        helpers.assert_evaluates_to(aifpl, "(let* ((x 10) (y (integer+ x 5)) (z (integer* y 2))) z)", "30")

    def test_let_star_empty_bindings(self, aifpl, helpers):
        """Test let* with no bindings."""
        helpers.assert_evaluates_to(aifpl, "(let* () 42)", "42")
        helpers.assert_evaluates_to(aifpl, "(let* () (integer+ 1 2))", "3")

    def test_let_star_single_binding(self, aifpl, helpers):
        """Test let* with a single binding."""
        helpers.assert_evaluates_to(aifpl, "(let* ((x 10)) (integer* x x))", "100")
        helpers.assert_evaluates_to(aifpl, "(let* ((s \"hello\")) (string-upcase s))", '"HELLO"')

    def test_let_star_shadowing(self, aifpl, helpers):
        """Test that let* allows shadowing of variables."""
        # Shadowing outer variable
        helpers.assert_evaluates_to(
            aifpl,
            "(let ((x 5)) (let* ((x 10) (y (integer* x 2))) y))",
            "20"
        )

        # Shadowing within let*
        helpers.assert_evaluates_to(
            aifpl,
            "(let* ((x 1) (x (integer+ x 10))) x)",
            "11"
        )

    def test_let_star_with_lambdas(self, aifpl, helpers):
        """Test let* with lambda expressions."""
        helpers.assert_evaluates_to(
            aifpl,
            "(let* ((f (lambda (x) (integer* x 2))) (result (f 5))) result)",
            "10"
        )

        # Lambda referencing earlier binding
        helpers.assert_evaluates_to(
            aifpl,
            "(let* ((x 10) (f (lambda (y) (integer+ x y)))) (f 5))",
            "15"
        )

    def test_let_star_desugars_to_nested_lets(self, aifpl, helpers):
        """Test that let* behavior is equivalent to nested lets."""
        # let* version
        let_star_result = aifpl.evaluate("(let* ((x 1) (y 2) (z 3)) (integer+ x y z))")

        # Equivalent nested let version
        nested_let_result = aifpl.evaluate(
            "(let ((x 1)) (let ((y 2)) (let ((z 3)) (integer+ x y z))))"
        )

        assert let_star_result == nested_let_result

    def test_let_parallel_bindings_cannot_reference_each_other(self, aifpl):
        """Test that parallel let bindings cannot reference each other."""
        # This should raise an error because y tries to reference x
        # which is not yet in scope during parallel binding evaluation
        with pytest.raises(AIFPLEvalError, match="Undefined variable: 'x'"):
            aifpl.evaluate("(let ((x 5) (y (integer* x 2))) (integer+ x y))")

    def test_let_parallel_bindings_all_use_outer_scope(self, aifpl, helpers):
        """Test that parallel let bindings all use the outer scope."""
        # Both bindings should see the outer x
        helpers.assert_evaluates_to(
            aifpl,
            "(let ((x 10)) (let ((x 1) (y x)) (integer+ x y)))",
            "11"  # x=1, y=10 (from outer scope)
        )

    def test_let_parallel_allows_independent_bindings(self, aifpl, helpers):
        """Test that parallel let works with independent bindings."""
        helpers.assert_evaluates_to(aifpl, "(let ((x 5) (y 10)) (integer+ x y))", "15")
        helpers.assert_evaluates_to(aifpl, "(let ((a 1) (b 2) (c 3)) (integer+ a b c))", "6")

    def test_let_vs_let_star_difference(self, aifpl):
        """Test the key difference between let and let*."""
        # Set up an outer x
        outer_x = 100

        # In let*, y sees the NEW x (5)
        let_star_result = aifpl.evaluate(
            f"(let ((x {outer_x})) (let* ((x 5) (y x)) y))"
        )
        assert let_star_result == 5

        # In let, y sees the OLD x (100)
        let_result = aifpl.evaluate(
            f"(let ((x {outer_x})) (let ((x 5) (y x)) y))"
        )
        assert let_result == outer_x

    def test_let_star_error_messages(self, aifpl):
        """Test that let* provides good error messages."""
        # Wrong number of elements
        with pytest.raises(AIFPLEvalError, match="Let\\* expression structure is incorrect"):
            aifpl.evaluate("(let* ((x 1)))")  # Missing body

        # Invalid binding structure
        with pytest.raises(AIFPLEvalError, match="Let\\* binding list must be a list"):
            aifpl.evaluate("(let* x 42)")

        # Binding not a list
        with pytest.raises(AIFPLEvalError, match="Let\\* binding .* must be a list"):
            aifpl.evaluate("(let* (x) 42)")

    def test_let_star_with_complex_expressions(self, aifpl, helpers):
        """Test let* with complex nested expressions."""
        helpers.assert_evaluates_to(
            aifpl,
            """(let* ((nums (list 1 2 3 4 5))
                      (doubled (list-map (lambda (x) (integer* x 2)) nums))
                      (sum (list-fold integer+ 0 doubled)))
                sum)""",
            "30"
        )

    def test_let_star_in_recursion(self, aifpl, helpers):
        """Test let* used within recursive functions."""
        helpers.assert_evaluates_to(
            aifpl,
            """(letrec ((factorial
                       (lambda (n)
                         (let* ((is-base (integer<=? n 1))
                                (result (if is-base 1 (integer* n (factorial (integer- n 1))))))
                           result))))
                 (factorial 5))""",
            "120"
        )

    def test_nested_let_and_let_star(self, aifpl, helpers):
        """Test mixing let and let* in nested expressions."""
        # let* inside let
        helpers.assert_evaluates_to(
            aifpl,
            "(let ((x 10)) (let* ((y x) (z (integer* y 2))) z))",
            "20"
        )
        # let inside let*
        helpers.assert_evaluates_to(
            aifpl,
            "(let* ((x 10)) (let ((y 5) (z 3)) (integer+ x y z)))",
            "18"
        )
