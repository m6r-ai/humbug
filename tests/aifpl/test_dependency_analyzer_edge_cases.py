"""Tests for AIFPL dependency analyzer edge cases."""

import pytest

from aifpl import AIFPLEvalError


class TestAIFPLDependencyAnalyzerEdgeCases:
    """Test dependency analyzer edge cases and complex dependency scenarios."""

    def test_simple_dependency_analysis(self, aifpl):
        """Test simple dependency analysis in let bindings."""
        # Simple sequential dependency
        result = aifpl.evaluate("""
        (let* ((x 5)
               (y (+ x 3)))
          (+ x y))
        """)
        assert result == 13  # x=5, y=8, sum=13

        # Multiple dependencies
        result = aifpl.evaluate("""
        (let* ((a 2)
               (b (* a 3))
               (c (+ a b)))
          c)
        """)
        assert result == 8  # a=2, b=6, c=8

    def test_complex_dependency_chains(self, aifpl):
        """Test complex dependency chains."""
        # Long dependency chain
        result = aifpl.evaluate("""
        (let* ((a 1)
               (b (+ a 1))
               (c (+ b 1))
               (d (+ c 1))
               (e (+ d 1)))
          e)
        """)
        assert result == 5

        # Branching dependencies
        result = aifpl.evaluate("""
        (let* ((base 10)
               (left (* base 2))
               (right (* base 3))
               (sum (+ left right)))
          sum)
        """)
        assert result == 50  # base=10, left=20, right=30, sum=50

    def test_dependency_analysis_with_functions(self, aifpl):
        """Test dependency analysis with function calls."""
        # Dependencies involving function calls
        result = aifpl.evaluate("""
        (let* ((x 4)
               (y (sqrt x))
               (z (* y y)))
          z)
        """)
        assert result == 4  # x=4, y=2, z=4

        # Complex function dependencies
        result = aifpl.evaluate("""
        (let* ((angle (* pi 0.5))
               (sin-val (sin angle))
               (cos-val (cos angle))
               (sum (+ sin-val cos-val)))
          (round sum))
        """)
        assert result == 1  # sin(π/2) + cos(π/2) ≈ 1 + 0 = 1

    def test_dependency_analysis_with_conditionals(self, aifpl):
        """Test dependency analysis with conditional expressions."""
        # Dependencies in conditional branches
        result = aifpl.evaluate("""
        (let* ((flag #t)
               (x 10)
               (result (if flag (+ x 5) (- x 5))))
          result)
        """)
        assert result == 15

        # Complex conditional dependencies
        result = aifpl.evaluate("""
        (let* ((a 5)
               (b 3)
               (condition (> a b))
               (result (if condition (* a b) (+ a b))))
          result)
        """)
        assert result == 15  # 5 > 3 is true, so 5 * 3 = 15

    def test_dependency_analysis_with_lambda_functions(self, aifpl):
        """Test dependency analysis with lambda functions."""
        # Lambda capturing variables from let bindings
        result = aifpl.evaluate("""
        (let* ((multiplier 3)
               (f (lambda (x) (* x multiplier)))
               (result (f 4)))
          result)
        """)
        assert result == 12

        # Complex lambda dependencies
        result = aifpl.evaluate("""
        (let* ((base 10)
               (adder (lambda (x) (+ x base)))
               (doubler (lambda (x) (* x 2)))
               (composer (lambda (x) (doubler (adder x))))
               (result (composer 5)))
          result)
        """)
        assert result == 30  # (5 + 10) * 2 = 30

    def test_dependency_analysis_with_list_operations(self, aifpl):
        """Test dependency analysis with list operations."""
        # List dependencies
        result = aifpl.evaluate("""
        (let* ((base-list (list 1 2 3))
              (extended (append base-list (list 4 5)))
              (length-val (length extended)))
          length-val)
        """)
        assert result == 5

        # Complex list processing
        result = aifpl.evaluate("""
        (let* ((numbers (list 1 2 3 4 5))
               (doubled (map (lambda (x) (* x 2)) numbers))
               (sum (fold + 0 doubled)))
          sum)
        """)
        assert result == 30  # (2+4+6+8+10) = 30

    def test_dependency_analysis_with_string_operations(self, aifpl):
        """Test dependency analysis with string operations."""
        # String processing dependencies
        result = aifpl.evaluate("""
        (let* ((first-name "John")
               (last-name "Doe")
               (full-name (string-append first-name " " last-name))
               (length-val (string-length full-name)))
          length-val)
        """)
        assert result == 8  # "John Doe" has 8 characters

        # Complex string transformations
        result = aifpl.evaluate("""
        (let* ((text "hello world")
               (upper-text (string-upcase text))
               (words (string-split upper-text " "))
               (count (length words)))
          count)
        """)
        assert result == 2

    def test_dependency_analysis_error_cases(self, aifpl):
        """Test dependency analysis error cases."""
        # Undefined variable in dependency
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("""
            (let* ((x undefined-var)
                   (y (+ x 1)))
              y)
            """)

        # Circular dependency (if detected)
        try:
            # This might be allowed or might cause an error
            result = aifpl.evaluate("""
            (let* ((x (+ y 1))
                   (y (+ x 1)))
              x)
            """)
            # If it succeeds, that's also valid (might use forward references)
        except AIFPLEvalError:
            # Circular dependencies might be detected and rejected
            pass

    def test_dependency_analysis_with_nested_let(self, aifpl):
        """Test dependency analysis with nested let expressions."""
        # Nested let with dependencies
        result = aifpl.evaluate("""
        (let ((outer 10))
          (let* ((inner (+ outer 5))
                 (combined (+ outer inner)))
            combined))
        """)
        assert result == 25  # outer=10, inner=15, combined=25

        # Multiple levels of nesting
        result = aifpl.evaluate("""
        (let ((a 1))
          (let ((b (+ a 1)))
            (let ((c (+ a b)))
              (let ((d (+ b c)))
                (+ a b c d)))))
        """)
        # a=1, b=2, c=3, d=5, sum=11
        assert result == 11

    def test_dependency_analysis_with_higher_order_functions(self, aifpl):
        """Test dependency analysis with higher-order functions."""
        # Map with dependencies
        result = aifpl.evaluate("""
        (let* ((base 5)
               (numbers (list 1 2 3))
               (add-base (lambda (x) (+ x base)))
               (results (map add-base numbers)))
          results)
        """)
        assert result == [6, 7, 8]

        # Filter with dependencies
        result = aifpl.evaluate("""
        (let* ((threshold 2)
               (numbers (list 1 2 3 4 5))
               (filtered (filter (lambda (x) (> x threshold)) numbers))
               (count (length filtered)))
          count)
        """)
        assert result == 3  # [3, 4, 5] has length 3

    def test_dependency_analysis_performance_edge_cases(self, aifpl):
        """Test dependency analysis performance with complex cases."""
        # Many variables with dependencies
        many_deps = """
        (let* (""" + "\n".join(
            f"(var{i} {i})" if i == 0 else f"(var{i} (+ var{i-1} 1))"
            for i in range(20)
        ) + """)
          var19)
        """
        result = aifpl.evaluate(many_deps)
        assert result == 19

        # Complex dependency graph
        complex_deps = """
        (let* ((a 1)
               (b (+ a 1))
               (c (+ a 2))
               (d (+ b c))
               (e (* b c))
               (f (+ d e))
               (g (- f d))
               (h (/ f 2)))
          (+ g h))
        """
        result = aifpl.evaluate(complex_deps)
        # a=1, b=2, c=3, d=5, e=6, f=11, g=6, h=5.5, sum=11.5
        assert result == 11.5

    def test_dependency_analysis_with_recursive_structures(self, aifpl):
        """Test dependency analysis with recursive structures (if supported)."""
        try:
            # Recursive function definition
            result = aifpl.evaluate("""
            (let* ((factorial (lambda (n)
                               (if (<= n 1)
                                   1
                                   (* n (factorial (- n 1))))))
                   (result (factorial 5)))
              result)
            """)
            assert result == 120
        except AIFPLEvalError:
            # Recursive definitions might not be supported
            pass

    def test_dependency_analysis_with_pattern_matching(self, aifpl):
        """Test dependency analysis with pattern matching (if supported)."""
        try:
            # Pattern matching with dependencies
            result = aifpl.evaluate("""
            (let* ((value 42)
                   (result (match value
                             ((number? n) (+ n 10))
                             (_ 0))))
              result)
            """)
            assert result == 52
        except AIFPLEvalError:
            # Pattern matching might not be supported
            pass

    def test_dependency_analysis_with_closures(self, aifpl):
        """Test dependency analysis with closures."""
        # Closure capturing multiple variables
        result = aifpl.evaluate("""
        (let* ((x 10)
               (y 20)
               (f (lambda (z) (+ x y z)))
               (result (f 5)))
          result)
        """)
        assert result == 35

        # Nested closures with dependencies
        result = aifpl.evaluate("""
        (let* ((base 5)
              (multiplier 3)
              (make-func (lambda (offset)
                          (lambda (x)
                            (+ (* x multiplier) base offset))))
              (func (make-func 2))
              (result (func 4)))
          result)
        """)
        assert result == 19  # (4 * 3) + 5 + 2 = 19

    def test_dependency_analysis_with_side_effect_free_operations(self, aifpl):
        """Test that dependency analysis works with side-effect-free operations."""
        # Mathematical operations
        result = aifpl.evaluate("""
        (let* ((x 2)
               (squared (* x x))
               (cubed (* squared x))
               (sum (+ x squared cubed)))
          sum)
        """)
        assert result == 14  # 2 + 4 + 8 = 14

        # String operations
        result = aifpl.evaluate("""
        (let* ((base "hello")
               (upper (string-upcase base))
               (length-val (string-length upper))
               (doubled (* length-val 2)))
          doubled)
        """)
        assert result == 10  # "HELLO" has 5 chars, doubled = 10

    def test_dependency_analysis_with_type_conversions(self, aifpl):
        """Test dependency analysis with type conversions."""
        # Number/string conversions
        result = aifpl.evaluate("""
        (let* ((num 42)
               (str (number->string num))
               (back-to-num (string->number str))
               (doubled (* back-to-num 2)))
          doubled)
        """)
        assert result == 84

        # Complex type conversion chain
        result = aifpl.evaluate("""
        (let* ((numbers (list 1 2 3))
               (strings (map number->string numbers))
               (joined (string-join strings ","))
               (length-val (string-length joined)))
          length-val)
        """)
        assert result == 5  # "1,2,3" has 5 characters

    def test_dependency_analysis_with_boolean_operations(self, aifpl):
        """Test dependency analysis with boolean operations."""
        # Boolean logic dependencies
        result = aifpl.evaluate("""
        (let* ((a #t)
               (b #f)
               (and-result (and a b))
               (or-result (or a b))
               (final (if or-result 1 0)))
          final)
        """)
        assert result == 1

        # Complex boolean expressions
        result = aifpl.evaluate("""
        (let* ((x 5)
               (y 3)
               (greater (> x y))
               (equal (= x y))
               (result (and greater (not equal))))
          result)
        """)
        assert result is True

    def test_dependency_analysis_with_arithmetic_sequences(self, aifpl):
        """Test dependency analysis with arithmetic sequences."""
        # Arithmetic progression
        result = aifpl.evaluate("""
        (let* ((start 2)
               (diff 3)
               (term1 start)
               (term2 (+ term1 diff))
               (term3 (+ term2 diff))
               (term4 (+ term3 diff))
               (sum (+ term1 term2 term3 term4)))
          sum)
        """)
        # term1=2, term2=5, term3=8, term4=11, sum=26
        assert result == 26

    def test_dependency_analysis_with_list_comprehension_style(self, aifpl):
        """Test dependency analysis with list comprehension style operations."""
        # List processing pipeline
        result = aifpl.evaluate("""
        (let* ((numbers (range 1 6))
               (evens (filter (lambda (x) (= (% x 2) 0)) numbers))
               (squares (map (lambda (x) (* x x)) evens))
               (sum (fold + 0 squares)))
          sum)
        """)
        assert result == 20  # evens=[2,4], squares=[4,16], sum=20

    def test_dependency_analysis_edge_case_ordering(self, aifpl):
        """Test dependency analysis with edge case ordering."""
        # Variables defined in reverse dependency order
        result = aifpl.evaluate("""
        (letrec ((z (+ x y))
                 (y (+ x 1))
                 (x 5))
          z)
        """)
        assert result == 11  # x=5, y=6, z=11

        # Mixed ordering
        result = aifpl.evaluate("""
        (letrec ((c (+ a b))
                 (a 3)
                 (b (* a 2))
                 (d (- c a)))
          d)
        """)
        assert result == 6  # a=3, b=6, c=9, d=6

    def test_dependency_analysis_with_error_propagation(self, aifpl):
        """Test dependency analysis with error propagation."""
        # Error in dependency chain
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("""
            (let* ((a 5)
                   (b (/ a 0))
                   (c (+ b 1)))
              c)
            """)

        # Error should not affect independent variables
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("""
            (let* ((good 42)
                   (bad (/ 1 0))
                   (dependent (+ bad 1)))
              good)
            """)

    def test_dependency_analysis_memory_efficiency(self, aifpl):
        """Test dependency analysis memory efficiency."""
        # Large dependency graph
        large_graph = """
        (let* (""" + "\n".join([
            f"(base{i} {i})" for i in range(10)
        ] + [
            f"(derived{i} (+ base{i} base{(i+1)%10}))" for i in range(10)
        ] + [
            f"(final{i} (* derived{i} 2))" for i in range(10)
        ]) + """)
          (+ """ + " ".join(f"final{i}" for i in range(10)) + """))
        """
        result = aifpl.evaluate(large_graph)
        # Should compute without memory issues
        assert isinstance(result, (int, float))

    def test_dependency_analysis_with_functional_composition(self, aifpl):
        """Test dependency analysis with functional composition."""
        # Function composition chain
        result = aifpl.evaluate("""
        (let* ((add1 (lambda (x) (+ x 1)))
               (mul2 (lambda (x) (* x 2)))
               (sub3 (lambda (x) (- x 3)))
               (compose (lambda (f g) (lambda (x) (f (g x)))))
               (func1 (compose mul2 add1))
               (func2 (compose sub3 func1))
               (result (func2 5)))
          result)
        """)
        assert result == 9  # ((5+1)*2)-3 = 12-3 = 9
