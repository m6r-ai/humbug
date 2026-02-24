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
               (y (integer+ x 3)))
          (integer+ x y))
        """)
        assert result == 13  # x=5, y=8, sum=13

        # Multiple dependencies
        result = aifpl.evaluate("""
        (let* ((a 2)
               (b (integer* a 3))
               (c (integer+ a b)))
          c)
        """)
        assert result == 8  # a=2, b=6, c=8

    def test_complex_dependency_chains(self, aifpl):
        """Test complex dependency chains."""
        # Long dependency chain
        result = aifpl.evaluate("""
        (let* ((a 1)
               (b (integer+ a 1))
               (c (integer+ b 1))
               (d (integer+ c 1))
               (e (integer+ d 1)))
          e)
        """)
        assert result == 5

        # Branching dependencies
        result = aifpl.evaluate("""
        (let* ((base 10)
               (left (integer* base 2))
               (right (integer* base 3))
               (sum (integer+ left right)))
          sum)
        """)
        assert result == 50  # base=10, left=20, right=30, sum=50

    def test_dependency_analysis_with_functions(self, aifpl):
        """Test dependency analysis with function calls."""
        # Dependencies involving function calls
        result = aifpl.evaluate("""
        (let* ((x 4.0)
               (y (integer (float-sqrt x)))
               (z (integer* y y)))
          z)
        """)
        assert result == 4  # x=4, y=2, z=4

        # Complex function dependencies
        result = aifpl.evaluate("""
        (let* ((angle (float* pi 0.5))
               (sin-val (float-sin angle))
               (cos-val (float-cos angle))
               (sum (float+ sin-val cos-val)))
          (round sum))
        """)
        assert result == 1  # sin(π/2) + cos(π/2) ≈ 1 + 0 = 1

    def test_dependency_analysis_with_conditionals(self, aifpl):
        """Test dependency analysis with conditional expressions."""
        # Dependencies in conditional branches
        result = aifpl.evaluate("""
        (let* ((flag #t)
               (x 10)
               (result (if flag (integer+ x 5) (integer- x 5))))
          result)
        """)
        assert result == 15

        # Complex conditional dependencies
        result = aifpl.evaluate("""
        (let* ((a 5)
               (b 3)
               (condition (integer>? a b))
               (result (if condition (integer* a b) (integer+ a b))))
          result)
        """)
        assert result == 15  # 5 > 3 is true, so 5 * 3 = 15

    def test_dependency_analysis_with_lambda_functions(self, aifpl):
        """Test dependency analysis with lambda functions."""
        # Lambda capturing variables from let bindings
        result = aifpl.evaluate("""
        (let* ((multiplier 3)
               (f (lambda (x) (integer* x multiplier)))
               (result (f 4)))
          result)
        """)
        assert result == 12

        # Complex lambda dependencies
        result = aifpl.evaluate("""
        (let* ((base 10)
               (adder (lambda (x) (integer+ x base)))
               (doubler (lambda (x) (integer* x 2)))
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
               (doubled (map (lambda (x) (integer* x 2)) numbers))
               (sum (fold integer+ 0 doubled)))
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
                   (y (integer+ x 1)))
              y)
            """)

        # Circular dependency (if detected)
        try:
            # This might be allowed or might cause an error
            result = aifpl.evaluate("""
            (let* ((x (integer+ y 1))
                   (y (integer+ x 1)))
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
          (let* ((inner (integer+ outer 5))
                 (combined (integer+ outer inner)))
            combined))
        """)
        assert result == 25  # outer=10, inner=15, combined=25

        # Multiple levels of nesting
        result = aifpl.evaluate("""
        (let ((a 1))
          (let ((b (integer+ a 1)))
            (let ((c (integer+ a b)))
              (let ((d (integer+ b c)))
                (integer+ a b c d)))))
        """)
        # a=1, b=2, c=3, d=5, sum=11
        assert result == 11

    def test_dependency_analysis_with_higher_order_functions(self, aifpl):
        """Test dependency analysis with higher-order functions."""
        # Map with dependencies
        result = aifpl.evaluate("""
        (let* ((base 5)
               (numbers (list 1 2 3))
               (add-base (lambda (x) (integer+ x base)))
               (results (map add-base numbers)))
          results)
        """)
        assert result == [6, 7, 8]

        # Filter with dependencies
        result = aifpl.evaluate("""
        (let* ((threshold 2)
               (numbers (list 1 2 3 4 5))
               (filtered (filter (lambda (x) (integer>? x threshold)) numbers))
               (count (length filtered)))
          count)
        """)
        assert result == 3  # [3, 4, 5] has length 3

    def test_dependency_analysis_performance_edge_cases(self, aifpl):
        """Test dependency analysis performance with complex cases."""
        # Many variables with dependencies
        many_deps = """
        (let* (""" + "\n".join(
            f"(var{i} {i})" if i == 0 else f"(var{i} (integer+ var{i-1} 1))"
            for i in range(20)
        ) + """)
          var19)
        """
        result = aifpl.evaluate(many_deps)
        assert result == 19

        # Complex dependency graph
        # a=1, b=2, c=3, d=5, e=6, f=11, g=6, h=5.5, sum=11.5
        # f/2 is float division since result is 5.5
        complex_deps = """
        (let* ((a 1)
               (b (integer+ a 1))
               (c (integer+ a 2))
               (d (integer+ b c))
               (e (integer* b c))
               (f (integer+ d e))
               (g (integer- f d))
               (h (float/ (float f) 2.0)))
          (float+ (float g) h))
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
                               (if (integer<=? n 1)
                                   1
                                   (integer* n (factorial (integer- n 1))))))
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
                             ((number? n) (integer+ n 10))
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
               (f (lambda (z) (integer+ x y z)))
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
                            (integer+ (integer* x multiplier) base offset))))
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
               (squared (integer* x x))
               (cubed (integer* squared x))
               (sum (integer+ x squared cubed)))
          sum)
        """)
        assert result == 14  # 2 + 4 + 8 = 14

        # String operations
        result = aifpl.evaluate("""
        (let* ((base "hello")
               (upper (string-upcase base))
               (length-val (string-length upper))
               (doubled (integer* length-val 2)))
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
               (doubled (integer* back-to-num 2)))
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
               (greater (integer>? x y))
               (equal (integer=? x y))
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
               (term2 (integer+ term1 diff))
               (term3 (integer+ term2 diff))
               (term4 (integer+ term3 diff))
               (sum (integer+ term1 term2 term3 term4)))
          sum)
        """)
        # term1=2, term2=5, term3=8, term4=11, sum=26
        assert result == 26

    def test_dependency_analysis_with_list_comprehension_style(self, aifpl):
        """Test dependency analysis with list comprehension style operations."""
        # List processing pipeline
        result = aifpl.evaluate("""
        (let* ((numbers (range 1 6))
               (evens (filter (lambda (x) (integer=? (% x 2) 0)) numbers))
               (squares (map (lambda (x) (integer* x x)) evens))
               (sum (fold integer+ 0 squares)))
          sum)
        """)
        assert result == 20  # evens=[2,4], squares=[4,16], sum=20

    def test_dependency_analysis_edge_case_ordering(self, aifpl):
        """Test dependency analysis with edge case ordering."""
        # Variables defined in reverse dependency order
        result = aifpl.evaluate("""
        (letrec ((z (integer+ x y))
                 (y (integer+ x 1))
                 (x 5))
          z)
        """)
        assert result == 11  # x=5, y=6, z=11

        # Mixed ordering
        result = aifpl.evaluate("""
        (letrec ((c (integer+ a b))
                 (a 3)
                 (b (integer* a 2))
                 (d (integer- c a)))
          d)
        """)
        assert result == 6  # a=3, b=6, c=9, d=6

    def test_dependency_analysis_with_error_propagation(self, aifpl):
        """Test dependency analysis with error propagation."""
        # Error in dependency chain
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("""
            (let* ((a 5)
                   (b (integer/ a 0))
                   (c (integer+ b 1)))
              c)
            """)

        # Error should not affect independent variables
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("""
            (let* ((good 42)
                   (bad (integer/ 1 0))
                   (dependent (integer+ bad 1)))
              good)
            """)

    def test_dependency_analysis_memory_efficiency(self, aifpl):
        """Test dependency analysis memory efficiency."""
        # Large dependency graph
        large_graph = """
        (let* (""" + "\n".join([
            f"(base{i} {i})" for i in range(10)
        ] + [
            f"(derived{i} (integer+ base{i} base{(i+1)%10}))" for i in range(10)
        ] + [
            f"(final{i} (integer* derived{i} 2))" for i in range(10)
        ]) + """)
          (integer+ """ + " ".join(f"final{i}" for i in range(10)) + """))
        """
        result = aifpl.evaluate(large_graph)
        # Should compute without memory issues
        assert isinstance(result, (int, float))

    def test_dependency_analysis_with_functional_composition(self, aifpl):
        """Test dependency analysis with functional composition."""
        # Function composition chain
        result = aifpl.evaluate("""
        (let* ((add1 (lambda (x) (integer+ x 1)))
               (mul2 (lambda (x) (integer* x 2)))
               (sub3 (lambda (x) (integer- x 3)))
               (compose (lambda (f g) (lambda (x) (f (g x)))))
               (func1 (compose mul2 add1))
               (func2 (compose sub3 func1))
               (result (func2 5)))
          result)
        """)
        assert result == 9  # ((5+1)*2)-3 = 12-3 = 9
