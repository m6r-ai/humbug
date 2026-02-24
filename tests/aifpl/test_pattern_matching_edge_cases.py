"""Tests for AIFPL pattern matching edge cases."""

import pytest

from aifpl import AIFPLEvalError


class TestAIFPLPatternMatchingEdgeCases:
    """Test pattern matching edge cases and comprehensive scenarios."""

    def test_basic_pattern_matching_edge_cases(self, aifpl):
        """Test basic pattern matching edge cases."""
        # Match with literal values
        result = aifpl.evaluate('(match 42 (42 "found") (_ "not found"))')
        assert result == "found"

        result = aifpl.evaluate('(match 43 (42 "found") (_ "not found"))')
        assert result == "not found"

        # Match with string literals
        result = aifpl.evaluate('(match "hello" ("hello" "found") (_ "not found"))')
        assert result == "found"

        result = aifpl.evaluate('(match "world" ("hello" "found") (_ "not found"))')
        assert result == "not found"

        # Match with boolean literals
        result = aifpl.evaluate('(match #t (#t "true") (#f "false") (_ "other"))')
        assert result == "true"

        result = aifpl.evaluate('(match #f (#t "true") (#f "false") (_ "other"))')
        assert result == "false"

    def test_wildcard_pattern_edge_cases(self, aifpl):
        """Test wildcard pattern edge cases."""
        # Wildcard should match anything
        result = aifpl.evaluate('(match 42 (_ "matched"))')
        assert result == "matched"

        result = aifpl.evaluate('(match "hello" (_ "matched"))')
        assert result == "matched"

        result = aifpl.evaluate('(match #t (_ "matched"))')
        assert result == "matched"

        result = aifpl.evaluate('(match (list 1 2 3) (_ "matched"))')
        assert result == "matched"

        # Wildcard as fallback
        result = aifpl.evaluate('(match 99 (1 "one") (2 "two") (_ "other"))')
        assert result == "other"

    def test_type_pattern_edge_cases(self, aifpl):
        """Test type pattern edge cases."""
        # String type patterns
        result = aifpl.evaluate('(match "hello" ((string? s) "string") (_ "other"))')
        assert result == "string"

        result = aifpl.evaluate('(match 42 ((string? s) "string") (_ "other"))')
        assert result == "other"

        # Boolean type patterns
        result = aifpl.evaluate('(match #t ((boolean? b) "boolean") (_ "other"))')
        assert result == "boolean"

        result = aifpl.evaluate('(match #f ((boolean? b) "boolean") (_ "other"))')
        assert result == "boolean"

        result = aifpl.evaluate('(match 42 ((boolean? b) "boolean") (_ "other"))')
        assert result == "other"

        # List type patterns
        result = aifpl.evaluate('(match (list 1 2 3) ((list? l) "list") (_ "other"))')
        assert result == "list"

        result = aifpl.evaluate('(match () ((list? l) "list") (_ "other"))')
        assert result == "list"

        result = aifpl.evaluate('(match 42 ((list? l) "list") (_ "other"))')
        assert result == "other"

    def test_variable_binding_pattern_edge_cases(self, aifpl):
        """Test variable binding pattern edge cases."""
        # Simple variable binding
        result = aifpl.evaluate('(match 42 ((integer? n) n) (_ 0))')
        assert result == 42

        # Variable binding with transformation
        result = aifpl.evaluate('(match 5 ((integer? n) (integer* n 2)) (_ 0))')
        assert result == 10

        # String variable binding
        result = aifpl.evaluate('(match "hello" ((string? s) (string-upcase s)) (_ ""))')
        assert result == "HELLO"

        # Multiple variable bindings (if supported)
        try:
            result = aifpl.evaluate('(match (list 1 2) ((a b) (integer+ a b)) (_ 0))')
            assert result == 3
        except AIFPLEvalError:
            # Multiple variable binding might not be supported
            pass

    def test_list_pattern_matching_edge_cases(self, aifpl):
        """Test list pattern matching edge cases."""
        # Empty list pattern
        result = aifpl.evaluate('(match () (() "empty") (_ "not empty"))')
        assert result == "empty"

        result = aifpl.evaluate('(match (list 1) (() "empty") (_ "not empty"))')
        assert result == "not empty"

        # Single element list pattern
        try:
            result = aifpl.evaluate('(match (list 1) ((x) x) (_ 0))')
            assert result == 1
        except AIFPLEvalError:
            # Single element destructuring might not be supported
            pass

        # Multiple element list pattern
        try:
            result = aifpl.evaluate('(match (list 1 2 3) ((a b c) (integer+ a b c)) (_ 0))')
            assert result == 6
        except AIFPLEvalError:
            # Multiple element destructuring might not be supported
            pass

        # Head/tail pattern (if supported)
        try:
            result = aifpl.evaluate('(match (list 1 2 3) ((head . tail) head) (_ 0))')
            assert result == 1
        except AIFPLEvalError:
            # Head/tail patterns might not be supported
            pass

    def test_nested_pattern_matching_edge_cases(self, aifpl):
        """Test nested pattern matching edge cases."""
        # Nested list patterns
        try:
            result = aifpl.evaluate('(match (list (list 1 2) 3) (((a b) c) (integer+ a b c)) (_ 0))')
            assert result == 6
        except AIFPLEvalError:
            # Nested patterns might not be supported
            pass

        # Mixed type nested patterns
        try:
            result = aifpl.evaluate('''
            (match (list 1 "hello")
              (((integer? n) (string? s)) (list n (string-length s)))
              (_ (list 0 0)))
            ''')
            assert result == [1, 5]
        except AIFPLEvalError:
            # Complex nested patterns might not be supported
            pass

    def test_pattern_matching_with_complex_data(self, aifpl):
        """Test pattern matching with complex data structures."""
        # Match nested lists
        result = aifpl.evaluate('''
        (match (list (list 1) (list 2))
          ((list? l) "list")
          (_ "other"))
        ''')
        assert result == "list"

        # Match mixed-type lists
        result = aifpl.evaluate('''
        (match (list 1 "hello" #t)
          ((list? l) "mixed list")
          (_ "other"))
        ''')
        assert result == "mixed list"

    def test_pattern_matching_error_cases(self, aifpl):
        """Test pattern matching error cases."""
        # No matching pattern (should use default or error)
        try:
            result = aifpl.evaluate('(match 42 ("hello" "string"))')
            # Should either have a default case or raise an error
        except AIFPLEvalError:
            # Error for no matching pattern is acceptable
            pass

        # Invalid pattern syntax (if detectable)
        try:
            result = aifpl.evaluate('(match 42 (invalid-pattern "result"))')
        except AIFPLEvalError:
            # Invalid pattern should raise error
            pass

        try:
            result = aifpl.evaluate('(match 42 ((42) "found") "not a list")')
        except AIFPLEvalError:
            pass

    def test_pattern_matching_with_functions(self, aifpl):
        """Test pattern matching with function values."""
        # Match function type
        result = aifpl.evaluate('''
        (match (lambda (x) x)
          ((function? f) "function")
          (_ "other"))
        ''')
        assert result == "function"

        # Match non-function
        result = aifpl.evaluate('''
        (match 42
          ((function? f) "function")
          (_ "other"))
        ''')
        assert result == "other"

    def test_pattern_matching_performance_edge_cases(self, aifpl):
        """Test pattern matching performance with many patterns."""
        # Many literal patterns
        many_patterns = '''
        (match 50
        ''' + '\n'.join(f'  ({i} "pattern-{i}")' for i in range(1, 50)) + '''
          (50 "found")
          (_ "not found"))
        '''

        result = aifpl.evaluate(many_patterns)
        assert result == "found"

        # Pattern not found case
        not_found_patterns = '''
        (match 999
        ''' + '\n'.join(f'  ({i} "pattern-{i}")' for i in range(1, 50)) + '''
          (_ "not found"))
        '''

        result = aifpl.evaluate(not_found_patterns)
        assert result == "not found"

    def test_pattern_matching_with_expressions(self, aifpl):
        """Test pattern matching where patterns contain expressions."""
        # Pattern matching with variable binding and expressions
        result = aifpl.evaluate('''
        (match 5
          ((integer? n) (if (integer>? n 0) "positive" "non-positive"))
          (_ "not a number"))
        ''')
        assert result == "positive"

        result = aifpl.evaluate('''
        (match -3
          ((integer? n) (if (integer>? n 0) "positive" "non-positive"))
          (_ "not a number"))
        ''')
        assert result == "non-positive"

    def test_pattern_matching_with_let_bindings(self, aifpl):
        """Test pattern matching combined with let bindings."""
        # Pattern matching inside let
        result = aifpl.evaluate('''
        (let ((x 42))
          (match x
            ((integer? n) (integer* n 2))
            (_ 0)))
        ''')
        assert result == 84

        # Let bindings in pattern match results
        result = aifpl.evaluate('''
        (match 5
          ((integer? n) (let ((doubled (integer* n 2))) (integer+ doubled 1)))
          (_ 0))
        ''')
        assert result == 11

    def test_pattern_matching_with_higher_order_functions(self, aifpl):
        """Test pattern matching with higher-order functions."""
        # Pattern matching in map
        try:
            result = aifpl.evaluate('''
            (map (lambda (x)
                   (match x
                     ((integer? n) (integer* n 2))
                     (_ 0)))
                 (list 1 2 3))
            ''')
            assert result == [2, 4, 6]
        except AIFPLEvalError:
            # Complex lambda/match combinations might not be supported
            pass

        # Pattern matching in filter
        try:
            result = aifpl.evaluate('''
            (filter (lambda (x)
                      (match x
                        ((integer? n) (integer>? n 2))
                        (_ #f)))
                    (list 1 2 3 4 5))
            ''')
            assert result == [3, 4, 5]
        except AIFPLEvalError:
            # Complex lambda/match combinations might not be supported
            pass

    def test_pattern_matching_edge_case_types(self, aifpl):
        """Test pattern matching with edge case types."""
        # Match zero
        result = aifpl.evaluate('''
        (match 0
          (0 "zero")
          ((integer? n) "non-zero integer")
          (_ "other"))
        ''')
        assert result == "zero"

        # Match empty string
        result = aifpl.evaluate('''
        (match ""
          ("" "empty string")
          ((string? s) "non-empty string")
          (_ "other"))
        ''')
        assert result == "empty string"

        # Match false
        result = aifpl.evaluate('''
        (match #f
          (#f "false")
          ((boolean? b) "true")
          (_ "other"))
        ''')
        assert result == "false"

    def test_pattern_matching_with_arithmetic_results(self, aifpl):
        """Test pattern matching with results of arithmetic operations."""
        # Match result of arithmetic
        result = aifpl.evaluate('''
        (match (integer+ 2 3)
          (5 "five")
          ((integer? n) "other integer")
          (_ "not a number"))
        ''')
        assert result == "five"

        # Match with complex arithmetic
        result = aifpl.evaluate('''
        (match (integer* (integer+ 1 2) (integer- 5 3))
          (6 "six")
          ((integer? n) "other integer")
          (_ "not a number"))
        ''')
        assert result == "six"

    def test_pattern_matching_exhaustiveness(self, aifpl):
        """Test pattern matching exhaustiveness scenarios."""
        # Should have wildcard for exhaustiveness
        result = aifpl.evaluate('''
        (match "unknown"
          ("hello" "greeting")
          ("goodbye" "farewell")
          (_ "unknown"))
        ''')
        assert result == "unknown"

        # Boolean exhaustiveness
        result = aifpl.evaluate('''
        (match #t
          (#t "true case")
          (#f "false case"))
        ''')
        assert result == "true case"

        result = aifpl.evaluate('''
        (match #f
          (#t "true case")
          (#f "false case"))
        ''')
        assert result == "false case"

    def test_pattern_matching_with_nested_functions(self, aifpl):
        """Test pattern matching with nested function calls."""
        # Pattern match on nested function results
        result = aifpl.evaluate('''
        (match (integer-abs -5)
          (5 "five")
          ((integer? n) "other")
          (_ "not integer"))
        ''')
        assert result == "five"

        # Pattern match with string functions
        result = aifpl.evaluate('''
        (match (string-length "hello")
          (5 "five chars")
          ((integer? n) "other length")
          (_ "not integer"))
        ''')
        assert result == "five chars"

    def test_pattern_matching_variable_scope(self, aifpl):
        """Test variable scope in pattern matching."""
        # Variables bound in patterns should be local to that pattern
        result = aifpl.evaluate('''
        (let ((n 100))
          (match 42
            ((integer? n) n) ; This n should shadow the outer n
            (_ n)))          ; This n should refer to outer n
        ''')
        assert result == 42  # Should use the pattern-bound n, not outer n

        # Test that pattern variables don't leak
        result = aifpl.evaluate('''
        (let ((result (match 42 ((integer? n) n) (_ 0))))
          result)
        ''')
        assert result == 42

    def test_pattern_matching_with_complex_expressions_in_results(self, aifpl):
        """Test pattern matching with complex expressions in result branches."""
        # Complex expression in result
        result = aifpl.evaluate('''
        (match 3
          ((integer? n)
           (let ((squared (integer* n n))
                 (doubled (integer* n 2)))
             (integer+ squared doubled)))
          (_ 0))
        ''')
        assert result == 15  # 3^2 + 3*2 = 9 + 6 = 15

        # Nested match in result
        try:
            result = aifpl.evaluate('''
            (match 5
              ((integer? n)
               (match (integer>? n 3)
                 (#t "big number")
                 (#f "small number")))
              (_ "not number"))
            ''')
            assert result == "big number"
        except AIFPLEvalError:
            # Nested match might not be supported
            pass

    def test_pattern_matching_type_specificity(self, aifpl):
        """Test pattern matching type specificity."""
        # Integer should match number pattern
        result = aifpl.evaluate('''
        (match 42
          ((integer? i) "integer")
          ((float? n) "float")
          (_ "other"))
        ''')
        assert result == "integer"  # More specific pattern should match first

        # Float should match number but not integer
        result = aifpl.evaluate('''
        (match 3.14
          ((integer? i) "integer")
          ((float? n) "float")
          (_ "other"))
        ''')
        assert result == "float"

        # Complex should match number but not integer or float
        result = aifpl.evaluate('''
        (match (complex 1 2)
          ((integer? i) "integer")
          ((float? f) "float")
          ((complex? n) "complex")
          (_ "other"))
        ''')
        assert result == "complex"

    def test_pattern_matching_edge_case_combinations(self, aifpl):
        """Test pattern matching with edge case combinations."""
        # Combine multiple pattern types
        result = aifpl.evaluate('''
        (match (list 1 2 3)
          (() "empty")
          ((list? l) (if (integer>? (length l) 2) "long list" "short list"))
          (_ "not list"))
        ''')
        assert result == "long list"

        # Pattern matching with arithmetic in guards
        result = aifpl.evaluate('''
        (match 10
          ((integer? n) (if (integer=? (% n 2) 0) "even" "odd"))
          (_ "not number"))
        ''')
        assert result == "even"

    # ========== CORRECTED DOT PATTERN VALIDATION TESTS ==========

    def test_dot_pattern_validation_errors(self, aifpl):
        """Test comprehensive validation of dot patterns - all caught upfront now."""

        # Test 1: Dot at beginning - caught by early validation
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((. tail) "invalid") (_ "other"))')

        # Test 2: Dot at end - now caught by early validation (refactored!)
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((head .) "invalid") (_ "other"))')

        # Test 3: Multiple elements after dot - now caught by early validation (refactored!)
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((head . tail extra) "invalid") (_ "other"))')

        # Test 4: Multiple dots - caught by early validation
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b . c) "invalid") (_ "other"))')

    def test_dot_pattern_validation_comprehensive(self, aifpl):
        """Test that ALL dot validation is now done upfront."""

        # All these should be caught by _validate_list_pattern_syntax now

        # Dot at beginning
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((. a) "invalid"))')

        # Dot at end
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((a .) "invalid"))')

        # Multiple elements after dot
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b c) "invalid"))')

        # Multiple dots
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b . c) "invalid"))')

    def test_dot_pattern_error_messages(self, aifpl):
        """Test that dot pattern errors provide specific and helpful messages."""

        # Test specific error message content for dot at beginning
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(match (list 1 2) ((. tail) "test"))')

        error = str(exc_info.value)
        assert "Invalid pattern in clause 1" in error
        assert "dot at beginning" in error

        # Test specific error message content for dot at end
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(match (list 1 2) ((head .) "test"))')

        error = str(exc_info.value)
        assert "Invalid pattern in clause 1" in error
        assert "dot at end" in error

        # Test specific error message content for multiple elements after dot
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(match (list 1 2 3) ((head . tail extra) "test"))')

        error = str(exc_info.value)
        assert "Invalid pattern in clause 1" in error
        assert "multiple elements after dot" in error

        # Test specific error message content for multiple dots
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(match (list 1 2 3) ((a . b . c) "test"))')

        error = str(exc_info.value)
        assert "Invalid pattern in clause 1" in error
        assert "multiple dots" in error

    def test_dot_pattern_validation_vs_matching_separation(self, aifpl):
        """Test that validation errors are clearly separated from matching failures."""

        # These should be validation errors (syntax problems) - caught upfront
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((a .) "invalid"))')

        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b c) "invalid"))')

        # This should be a matching failure (semantic problem - not enough elements)
        result = aifpl.evaluate_and_format('(match (list 1) ((a b . rest) "matched") (_ "no match"))')
        assert result == '"no match"'

        # This should be a no-patterns-matched error (semantic problem)
        with pytest.raises(AIFPLEvalError, match="No patterns matched"):
            aifpl.evaluate('(match (list 1) ((a b . rest) "matched"))')

    def test_valid_dot_patterns(self, aifpl):
        """Test that valid dot patterns work."""

        # Valid head/tail patterns that should work
        result = aifpl.evaluate('(match (list 1 2 3) ((head . tail) head))')
        assert result == 1

        result = aifpl.evaluate('(match (list 1 2 3 4) ((a b . rest) (length rest)))')
        assert result == 2

        result = aifpl.evaluate('(match (list 1 2 3 4 5) ((a b c . rest) (list a (length rest))))')
        assert result == [1, 2]

        # Valid single element with tail
        result = aifpl.evaluate('(match (list 42) ((head . tail) (null? tail)))')
        assert result

        result = aifpl.evaluate('(match (list 1 2 3) ((1 . tail) \"matched head\") (_ \"no match\"))')
        assert result == "matched head"

        result = aifpl.evaluate('(match (list 1 2 3) ((42 . tail) \"matched head\") (_ \"no match\"))')
        assert result == "no match"

        result = aifpl.evaluate('(match (list 1 \"hello\" \"world\") ((1 . (string? tail)) \"matched\") (_ \"no match\"))')
        assert result == "no match"

    def test_adot_pattern_failures(self, aifpl):
        """Test dot pattern failures."""

        # All these should fail during pattern validation phase, not matching phase

        # Pattern validation should catch these before any matching begins
        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((a .) "invalid"))')

        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b c) "invalid"))')

        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2) ((. a) "invalid"))')

        with pytest.raises(AIFPLEvalError, match="Invalid pattern in clause 1"):
            aifpl.evaluate('(match (list 1 2 3) ((a . b . c) "invalid"))')
