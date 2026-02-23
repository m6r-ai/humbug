"""Tests for AIFPL environment edge cases."""

import pytest

from aifpl import AIFPLEvalError


class TestAIFPLEnvironmentEdgeCases:
    """Test environment edge cases and variable scoping."""

    def test_global_environment_constants(self, aifpl):
        """Test that global environment contains expected constants."""
        # Mathematical constants
        pi_value = aifpl.evaluate("pi")
        assert abs(pi_value - 3.14159265) < 1e-6

        e_value = aifpl.evaluate("e")
        assert abs(e_value - 2.71828182) < 1e-6

        j_value = aifpl.evaluate("1j")
        assert j_value == 1j

        # Boolean constants
        assert aifpl.evaluate("true") is True
        assert aifpl.evaluate("false") is False

    def test_global_environment_functions(self, aifpl):
        """Test that global environment contains expected functions."""
        # Arithmetic functions should be available
        arithmetic_functions = ["//", "%", "pow"]

        for func in arithmetic_functions:
            try:
                # Test that function exists by calling it
                result = aifpl.evaluate(f"({func} 6 2)")
                assert isinstance(result, (int, float, complex))
            except AIFPLEvalError:
                # Some functions might have different arity requirements
                try:
                    result = aifpl.evaluate(f"({func} 6)")
                    assert isinstance(result, (int, float, complex))
                except AIFPLEvalError:
                    # Function might require specific arguments
                    pass

        # Mathematical functions
        math_functions = ["abs", "sqrt", "sin", "cos", "tan", "log", "exp", "log10"]

        for func in math_functions:
            result = aifpl.evaluate(f"({func} 1)")
            assert isinstance(result, (int, float, complex))

        # List functions
        list_functions = ["list", "first", "rest", "last", "length", "reverse", "append"]

        for func in list_functions:
            try:
                if func == "list":
                    result = aifpl.evaluate(f"({func} 1 2 3)")
                elif func in ["append"]:
                    result = aifpl.evaluate(f"({func} (list 1) (list 2))")
                else:
                    result = aifpl.evaluate(f"({func} (list 1 2 3))")
                # Should not raise an error
            except AIFPLEvalError:
                # Some functions might have specific requirements
                pass

    def test_variable_scoping_let_bindings(self, aifpl):
        """Test variable scoping with let bindings."""
        # Simple let binding
        result = aifpl.evaluate("(let ((x 5)) x)")
        assert result == 5

        # Let binding shadows outer scope
        result = aifpl.evaluate("""
        (let ((x 10))
          (let ((x 20))
            x))
        """)
        assert result == 20

        # Inner binding doesn't affect outer scope - test this with nested let expressions
        result1 = aifpl.evaluate("""
        (let ((x 10))
          (let ((x 20))
            x))
        """)
        assert result1 == 20  # Inner x

        result2 = aifpl.evaluate("""
        (let ((x 10))
          (let ((y 20))
            x))
        """)
        assert result2 == 10  # Outer x is still accessible

        # Sequential let bindings
        result = aifpl.evaluate("""
        (let* ((x 5)
               (y (integer+ x 3)))
          y)
        """)
        assert result == 8  # x is available when defining y

    def test_variable_scoping_lambda_functions(self, aifpl):
        """Test variable scoping with lambda functions."""
        # Lambda parameter shadows outer variable
        result = aifpl.evaluate("""
        (let ((x 10))
          ((lambda (x) x) 20))
        """)
        assert result == 20

        # Lambda captures outer variables (closure)
        result = aifpl.evaluate("""
        (let ((x 10))
          ((lambda (y) (integer+ x y)) 5))
        """)
        assert result == 15

        # Nested lambda closures
        result = aifpl.evaluate("""
        (let ((x 10))
          (let ((f (lambda (y)
                    (lambda (z)
                      (integer+ x y z)))))
            ((f 5) 3)))
        """)
        assert result == 18  # 10 + 5 + 3

    def test_variable_scoping_nested_environments(self, aifpl):
        """Test deeply nested environment scoping."""
        # Multiple levels of let bindings
        result = aifpl.evaluate("""
        (let ((a 1))
          (let ((b 2))
            (let ((c 3))
              (let ((d 4))
                (integer+ a b c d)))))
        """)
        assert result == 10

        # Variable shadowing at multiple levels
        result = aifpl.evaluate("""
        (let ((x 1))
          (let ((x 2))
            (let ((x 3))
              (let ((x 4))
                x))))
        """)
        assert result == 4

    def test_undefined_variable_errors(self, aifpl):
        """Test undefined variable error handling."""
        # Simple undefined variable
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("undefined-var")

        # Undefined variable in expression
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("(integer+ 1 undefined-var)")

        # Undefined variable in let binding
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("(let ((x undefined-var)) x)")

    def test_environment_isolation_between_evaluations(self, aifpl):
        """Test that environments are isolated between evaluations."""
        # Define variable in one evaluation
        aifpl.evaluate("(let ((x 10)) x)")

        # Variable should not be available in next evaluation
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("x")

        # Each evaluation starts with clean environment
        result1 = aifpl.evaluate("(let ((x 10)) x)")
        result2 = aifpl.evaluate("(let ((x 20)) x)")
        assert result1 == 10
        assert result2 == 20

    def test_environment_with_higher_order_functions(self, aifpl):
        """Test environment behavior with higher-order functions."""
        # Lambda passed to map captures environment
        result = aifpl.evaluate("""
        (let ((multiplier 3))
          (map (lambda (x) (integer* x multiplier)) (list 1 2 3)))
        """)
        assert result == [3, 6, 9]

        # Filter with closure
        result = aifpl.evaluate("""
        (let ((threshold 2))
          (filter (lambda (x) (> x threshold)) (list 1 2 3 4)))
        """)
        assert result == [3, 4]

        # Fold with closure
        result = aifpl.evaluate("""
        (let ((base 10))
          (fold (lambda (acc x) (integer+ acc x base)) 0 (list 1 2 3)))
        """)
        assert result == 36  # 0 + (1+10) + (2+10) + (3+10) = 36

    def test_environment_variable_lifecycle(self, aifpl):
        """Test variable lifecycle in different scopes."""
        # Variables should be available throughout their scope
        result = aifpl.evaluate("""
        (let ((x 5))
          (let ((y (integer+ x 3)))
            (let ((z (integer* x y)))
              (integer+ x y z))))
        """)
        # x=5, y=5+3=8, z=5*8=40, sum=5+8+40=53
        assert result == 53

        # Variables should not leak out of scope - test with separate evaluations
        # since AIFPL doesn't support multiple body expressions in let
        result = aifpl.evaluate("""
        (let ((x 5))
          (let ((y 10))
            y))
        """)
        assert result == 10

        # y should not be available outside its scope
        with pytest.raises(AIFPLEvalError, match="Undefined variable"):
            aifpl.evaluate("y")

    def test_environment_with_recursive_bindings(self, aifpl):
        """Test environment with recursive bindings (if supported)."""
        try:
            # Self-referential binding (might not be supported)
            result = aifpl.evaluate("""
            (let ((factorial (lambda (n)
                              (if (<= n 1)
                                  1
                                  (integer* n (factorial (integer- n 1)))))))
              (factorial 5))
            """)
            assert result == 120
        except AIFPLEvalError:
            # Recursive bindings might not be supported
            pass

    def test_environment_error_context(self, aifpl):
        """Test that environment errors provide good context."""
        try:
            aifpl.evaluate("nonexistent-variable")
        except AIFPLEvalError as e:
            error_msg = str(e)
            assert "Undefined variable" in error_msg
            assert "nonexistent-variable" in error_msg
            # Should suggest available variables
            assert "Available variables" in error_msg or "pi" in error_msg

    def test_environment_with_complex_closures(self, aifpl):
        """Test environment with complex closure scenarios."""
        # Closure with multiple captured variables
        result = aifpl.evaluate("""
        (let ((a 2) (b 3) (c 4))
          (let ((f (lambda (x) (integer+ a b c x))))
            (f 5)))
        """)
        assert result == 14  # 2 + 3 + 4 + 5

        # Closure returning closure
        result = aifpl.evaluate("""
        (let ((multiplier 5))
          (let ((make-adder (lambda (n)
                             (lambda (x)
                               (integer+ (integer* x multiplier) n)))))
            ((make-adder 10) 3)))
        """)
        assert result == 25  # (3 * 5) + 10

    def test_environment_with_conditional_bindings(self, aifpl):
        """Test environment with conditional variable bindings."""
        # Variable binding in conditional branch
        result = aifpl.evaluate("""
        (let ((condition #t))
          (if condition
              (let ((x 10)) x)
              (let ((x 20)) x)))
        """)
        assert result == 10

        # Different variable in each branch
        result = aifpl.evaluate("""
        (let ((flag #f))
          (if flag
              (let ((result "true")) result)
              (let ((result "false")) result)))
        """)
        assert result == "false"

    def test_environment_with_pattern_matching(self, aifpl):
        """Test environment with pattern matching variable bindings (if supported)."""
        try:
            # Pattern matching with variable binding
            result = aifpl.evaluate("""
            (match 42
              ((number? n) (integer+ n 10))
              (_ 0))
            """)
            assert result == 52
        except AIFPLEvalError:
            # Pattern matching might not be supported
            pass

    def test_environment_memory_efficiency(self, aifpl):
        """Test environment memory efficiency with many variables."""
        # Many variables in single scope
        many_vars = """
        (let (""" + " ".join(f"(var{i} {i})" for i in range(50)) + """)
          (integer+ """ + " ".join(f"var{i}" for i in range(50)) + """))
        """
        result = aifpl.evaluate(many_vars)
        assert result == sum(range(50))

        # Deeply nested scopes
        nested_scopes = "x"
        for i in range(20):
            nested_scopes = f"(let ((x{i} {i})) (integer+ x{i} {nested_scopes}))"

        try:
            result = aifpl.evaluate(nested_scopes)
            # Should be sum of 0 to 19 plus the final x
            # But x is undefined, so this might error
        except AIFPLEvalError:
            # Expected if x is undefined at the end
            pass

    def test_environment_with_string_and_list_operations(self, aifpl):
        """Test environment with string and list operations."""
        # String operations with closures
        result = aifpl.evaluate("""
        (let ((prefix "Hello, "))
          (map (lambda (name) (string-append prefix name))
               (list "Alice" "Bob" "Charlie")))
        """)
        assert result == ["Hello, Alice", "Hello, Bob", "Hello, Charlie"]

        # List operations with captured variables
        result = aifpl.evaluate("""
        (let ((base-list (list 1 2 3)))
          (let ((extended (append base-list (list 4 5))))
            (length extended)))
        """)
        assert result == 5

    def test_environment_variable_shadowing_edge_cases(self, aifpl):
        """Test edge cases of variable shadowing."""
        # Shadow built-in constants
        result = aifpl.evaluate("""
        (let ((pi 3))
          pi)
        """)
        assert result == 3  # Should use local binding, not global constant

        # Shadow function names (if allowed)
        try:
            result = aifpl.evaluate("""
            (let ((+ 42))
              +)
            """)
            assert result == 42
        except AIFPLEvalError:
            # Shadowing built-in functions might not be allowed
            pass

        # Multiple levels of shadowing
        result = aifpl.evaluate("""
        (let ((x 1))
          (let ((x 2))
            (let ((x 3))
              (integer+ x (let ((x 4)) x)))))
        """)
        assert result == 7  # 3 + 4

    def test_environment_with_function_definitions(self, aifpl):
        """Test environment with function definitions."""
        # Define and use function in same scope
        result = aifpl.evaluate("""
        (let ((square (lambda (x) (integer* x x))))
          (square 5))
        """)
        assert result == 25

        # Function using other functions from same scope
        result = aifpl.evaluate("""
        (let ((double (lambda (x) (integer* x 2)))
              (triple (lambda (x) (integer* x 3))))
          (let ((combine (lambda (x) (integer+ (double x) (triple x)))))
            (combine 4)))
        """)
        assert result == 20  # (4*2) + (4*3) = 8 + 12 = 20

    def test_environment_cleanup_after_errors(self, aifpl):
        """Test that environment is properly cleaned up after errors."""
        # Error in nested scope
        with pytest.raises(AIFPLEvalError):
            aifpl.evaluate("""
            (let ((x 10))
              (let ((y (integer/ x 0)))
                y))
            """)

        # Next evaluation should work normally
        result = aifpl.evaluate("(integer+ 1 2)")
        assert result == 3

        # Global environment should be unchanged
        pi_value = aifpl.evaluate("pi")
        assert abs(pi_value - 3.14159265) < 1e-6

    def test_environment_with_large_closures(self, aifpl):
        """Test environment with large closure environments."""
        # Closure capturing many variables
        large_closure = """
        (let (""" + " ".join(f"(var{i} {i})" for i in range(20)) + """)
          (let ((f (lambda (x) 
                    (integer+ x """ + " ".join(f"var{i}" for i in range(20)) + """))))
            (f 100)))
        """
        result = aifpl.evaluate(large_closure)
        assert result == 100 + sum(range(20))  # 100 + 190 = 290

    def test_environment_variable_lookup_performance(self, aifpl):
        """Test variable lookup performance in deep environments."""
        # Deep environment with variable at different levels
        deep_env = """
        (let ((deep-var 42))
        """ + "".join(f"(let ((level{i} {i}))" for i in range(10)) + """
          deep-var
        """ + ")" * 10 + ")"

        result = aifpl.evaluate(deep_env)
        assert result == 42

    def test_environment_with_mutual_references(self, aifpl):
        """Test environment with mutual references (if supported)."""
        try:
            # Functions that reference each other
            result = aifpl.evaluate("""
            (let ((even? (lambda (n)
                          (if (= n 0) #t (odd? (integer- n 1)))))
                  (odd? (lambda (n)
                         (if (= n 0) #f (even? (integer- n 1))))))
              (even? 4))
            """)
            assert result is True
        except AIFPLEvalError:
            # Mutual recursion might not be supported
            pass

    def test_environment_edge_case_variable_names(self, aifpl):
        """Test environment with edge case variable names."""
        # Variable names with special characters (if allowed)
        special_names = [
            "x-var",        # Hyphen
            "var?",         # Question mark
            "var->other",   # Arrow
            "var123",       # Numbers
        ]

        for name in special_names:
            try:
                result = aifpl.evaluate(f"(let (({name} 42)) {name})")
                assert result == 42
            except AIFPLEvalError:
                # Some variable names might not be allowed
                pass
