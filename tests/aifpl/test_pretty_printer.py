"""Tests for the AIFPL pretty-printer."""

import pytest

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter, FormatOptions


class TestPrettyPrinterBasic:
    """Test basic pretty-printer functionality."""

    def test_simple_atoms(self):
        """Test formatting of simple atomic values."""
        printer = AIFPLPrettyPrinter()

        assert printer.format("42") == "42\n"
        assert printer.format("3.14") == "3.14\n"
        assert printer.format("#t") == "#t\n"
        assert printer.format("#f") == "#f\n"
        assert printer.format('"hello"') == '"hello"\n'
        assert printer.format("symbol") == "symbol\n"

    def test_empty_list(self):
        """Test formatting of empty list."""
        printer = AIFPLPrettyPrinter()
        assert printer.format("()") == "()\n"

    def test_compact_list(self):
        """Test that short lists stay compact."""
        printer = AIFPLPrettyPrinter()

        # Simple arithmetic
        assert printer.format("(+ 1 2 3)") == "(+ 1 2 3)\n"

        # Short nested lists
        assert printer.format("(* n (factorial (- n 1)))") == "(* n (factorial (- n 1)))\n"

    def test_multiline_list(self):
        """Test that long lists are formatted multi-line."""
        printer = AIFPLPrettyPrinter()

        # Very long list should be multi-line
        code = "(very-long-function-name-that-exceeds-threshold arg1 arg2 arg3 arg4 arg5)"
        result = printer.format(code)
        assert "\n" in result
        assert result.count("\n") > 1  # More than just trailing newline


class TestPrettyPrinterAlignment:
    """Test traditional Lisp-style alignment."""

    def test_multiline_list_alignment(self):
        """Test that multi-line lists use traditional Lisp alignment."""
        # Force multi-line with low threshold
        options = FormatOptions(compact_threshold=20)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1 2 3 4 5 6 7 8 9 10)"
        result = printer.format(code)

        # Should have first argument on same line as function
        lines = result.split("\n")
        assert lines[0] == "(+ 1"

        # Subsequent arguments should align under first argument
        # First argument starts at position 3 (after "(+ ")
        assert lines[1] == "   2"
        assert lines[2] == "   3"

        # All argument lines should have same indentation
        for i in range(1, 10):
            assert lines[i].startswith("   ")


class TestPrettyPrinterLetForms:
    """Test formatting of let/let*/letrec forms."""

    def test_simple_let(self):
        """Test formatting of simple let expression."""
        printer = AIFPLPrettyPrinter()
        code = "(let ((x 5)(y 10)) (+ x y))"
        result = printer.format(code)

        expected = "(let ((x 5)\n      (y 10))\n  (+ x y))\n"
        assert result == expected

    def test_let_star(self):
        """Test formatting of let* expression."""
        printer = AIFPLPrettyPrinter()
        code = "(let* ((x 5)(y (* x 2))) (+ x y))"
        result = printer.format(code)

        expected = "(let* ((x 5)\n       (y (* x 2)))\n  (+ x y))\n"
        assert result == expected

    def test_letrec_with_lambda(self):
        """Test formatting of letrec with lambda."""
        printer = AIFPLPrettyPrinter()
        code = "(letrec ((factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))) (factorial 5))"
        result = printer.format(code)

        # Should have proper indentation
        assert "letrec" in result
        assert "lambda" in result
        assert "(* n (factorial (- n 1)))" in result  # Should be compact

    def test_letrec_multiple_functions(self):
        """Test formatting of letrec with multiple function definitions."""
        printer = AIFPLPrettyPrinter()
        code = "(letrec ((f (lambda (x) x))(g (lambda (y) y))) (f 5))"
        result = printer.format(code)

        # Should have blank line between function definitions
        lines = result.split("\n")
        # Find the line with first function's closing paren
        # and check there's a blank line before the second function
        assert any(line.strip() == "" for line in lines)


class TestPrettyPrinterLambda:
    """Test formatting of lambda expressions."""

    def test_simple_lambda(self):
        """Test formatting of simple lambda."""
        printer = AIFPLPrettyPrinter()
        code = "(lambda (x y) (* x y))"
        result = printer.format(code)

        expected = "(lambda (x y)\n  (* x y))\n"
        assert result == expected

    def test_lambda_with_complex_body(self):
        """Test formatting of lambda with complex body."""
        printer = AIFPLPrettyPrinter()
        code = "(lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"
        result = printer.format(code)

        # Should have proper indentation
        assert "lambda" in result
        assert "if" in result
        assert result.count("\n") > 2  # Multiple lines


class TestPrettyPrinterConditionals:
    """Test formatting of conditional expressions."""

    def test_if_expression(self):
        """Test formatting of if expression."""
        printer = AIFPLPrettyPrinter()
        code = "(if (> x 5) (+ x 10) (- x 5))"
        result = printer.format(code)

        expected = "(if (> x 5)\n  (+ x 10)\n  (- x 5))\n"
        assert result == expected

    def test_nested_if(self):
        """Test formatting of nested if expressions."""
        printer = AIFPLPrettyPrinter()
        code = "(if (> x 0) (if (< x 10) 1 2) 3)"
        result = printer.format(code)

        # Should have proper indentation for nested if
        assert "if" in result
        assert result.count("\n") > 3


class TestPrettyPrinterComments:
    """Test comment preservation and formatting."""

    def test_end_of_line_comment(self):
        """Test that end-of-line comments are preserved."""
        printer = AIFPLPrettyPrinter()
        code = "(let ((x 5)  ; initial value\n      (y 10))  ; second value\n  (+ x y))"
        result = printer.format(code)

        assert "; initial value" in result
        assert "; second value" in result

    def test_standalone_comment_before_code(self):
        """Test standalone comment before code."""
        printer = AIFPLPrettyPrinter()
        code = "; This is a comment\n(+ 1 2)"
        result = printer.format(code)

        assert "; This is a comment" in result
        assert result.startswith("; This is a comment")

    def test_multiple_adjacent_comments(self):
        """Test multiple adjacent comments stay together."""
        printer = AIFPLPrettyPrinter()
        code = "; Comment 1\n; Comment 2\n; Comment 3\n(+ 1 2)"
        result = printer.format(code)

        lines = result.split("\n")
        # First three lines should be comments with no blank lines between
        assert lines[0] == "; Comment 1"
        assert lines[1] == "; Comment 2"
        assert lines[2] == "; Comment 3"

    def test_blank_line_between_comments_preserved(self):
        """Test that blank lines between comments are preserved."""
        printer = AIFPLPrettyPrinter()
        code = "; First comment\n\n; Second comment\n(+ 1 2)"
        result = printer.format(code)

        lines = result.split("\n")
        # Should have blank line between comments
        assert lines[0] == "; First comment"
        assert lines[1] == ""
        assert lines[2] == "; Second comment"

    def test_comment_after_code_gets_blank_line(self):
        """Test that comment after code gets a blank line for readability."""
        printer = AIFPLPrettyPrinter()
        code = "(+ 1 2)\n; Comment after code\n(+ 3 4)"
        result = printer.format(code)

        lines = result.split("\n")
        # Should have blank line before comment
        assert "(+ 1 2)" in lines[0]
        assert lines[1] == ""
        assert "; Comment after code" in lines[2]


class TestPrettyPrinterIndentation:
    """Test indentation correctness."""

    def test_let_binding_indentation(self):
        """Test that let bindings are properly indented."""
        printer = AIFPLPrettyPrinter()
        code = "(let ((x 5)(y 10)) (+ x y))"
        result = printer.format(code)

        lines = result.split("\n")
        # Check that bindings are properly indented
        assert "(let ((x 5)" in lines[0]
        assert lines[1].strip().startswith("(y 10)")

    def test_letrec_binding_value_indentation(self):
        """Test that letrec binding values are indented correctly."""
        printer = AIFPLPrettyPrinter()
        code = "(letrec ((factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))) (factorial 5))"
        result = printer.format(code)

        # Lambda body should be indented relative to lambda keyword
        # not relative to the start of the line
        lines = result.split("\n")
        # Find the line with "if"
        if_line = [line for line in lines if "(if" in line][0]
        # Should have significant indentation
        assert if_line.startswith(" " * 20)  # At least 20 spaces

    def test_nested_expression_indentation(self):
        """Test that nested expressions maintain proper indentation."""
        printer = AIFPLPrettyPrinter()
        code = "(let ((x (let ((y 5)) (+ y 1)))) (+ x 10))"
        result = printer.format(code)

        # Inner let should be indented
        assert "let" in result
        lines = result.split("\n")
        # Should have multiple indentation levels
        indents = [len(line) - len(line.lstrip()) for line in lines if line.strip()]
        assert len(set(indents)) > 1  # Multiple different indentation levels


class TestPrettyPrinterOptions:
    """Test formatting options."""

    def test_custom_indent_size(self):
        """Test custom indentation size."""
        options = FormatOptions(indent_size=4)
        printer = AIFPLPrettyPrinter(options)
        code = "(let ((x 5)) (+ x 10))"
        result = printer.format(code)

        lines = result.split("\n")
        # Body should be indented by 4 spaces
        body_line = [line for line in lines if "(+ x 10)" in line][0]
        assert body_line.startswith("    ")  # 4 spaces

    def test_custom_comment_spacing(self):
        """Test custom comment spacing."""
        options = FormatOptions(comment_spacing=4)
        printer = AIFPLPrettyPrinter(options)
        code = "(let ((x 5)  ; comment\n      (y 10))\n  (+ x y))"
        result = printer.format(code)

        # Comment should have 4 spaces before it
        assert "    ; comment" in result

    def test_custom_compact_threshold(self):
        """Test custom compact threshold."""
        options = FormatOptions(compact_threshold=20)
        printer = AIFPLPrettyPrinter(options)
        code = "(+ 1 2 3 4 5 6 7 8 9 10)"
        result = printer.format(code)

        # Should be multi-line due to low threshold
        assert result.count("\n") > 1


class TestPrettyPrinterEdgeCases:
    """Test edge cases and special scenarios."""

    def test_quoted_expressions(self):
        """Test formatting of quoted expressions."""
        printer = AIFPLPrettyPrinter()
        code = "'(a b c)"
        result = printer.format(code)

        assert result == "'(a b c)\n"

    def test_deeply_nested_lists(self):
        """Test formatting of deeply nested lists."""
        printer = AIFPLPrettyPrinter()
        code = "(+ 1 (+ 2 (+ 3 (+ 4 5))))"
        result = printer.format(code)

        # Should stay compact since it's under threshold
        assert "(+ 1 (+ 2 (+ 3 (+ 4 5))))" in result

    def test_empty_let_bindings(self):
        """Test formatting of let with no bindings."""
        printer = AIFPLPrettyPrinter()
        code = "(let () 42)"
        result = printer.format(code)

        assert "let" in result
        assert "42" in result

    def test_match_expression(self):
        """Test formatting of match expression."""
        printer = AIFPLPrettyPrinter()
        code = "(match x (1 'one) (2 'two) (_ 'other))"
        result = printer.format(code)

        # Match clauses should be indented
        assert "match" in result
        assert result.count("\n") > 2

    def test_idempotence(self):
        """Test that formatting is idempotent."""
        printer = AIFPLPrettyPrinter()
        code = "(let ((x 5)(y 10)) (+ x y))"

        # Format once
        result1 = printer.format(code)

        # Format the result again
        result2 = printer.format(result1)

        # Should be identical
        assert result1 == result2

    def test_preserves_string_escapes(self):
        """Test that string escape sequences are preserved."""
        printer = AIFPLPrettyPrinter()
        code = '"hello\\nworld"'
        result = printer.format(code)

        assert '"hello\\nworld"' in result

    def test_complex_numbers(self):
        """Test formatting of complex numbers."""
        printer = AIFPLPrettyPrinter()
        code = "(+ 3+4j 5j)"
        result = printer.format(code)

        assert "3+4j" in result or "(3+4j)" in result
        assert "5j" in result


class TestPrettyPrinterRealWorldExamples:
    """Test with real-world code examples."""

    def test_factorial_function(self):
        """Test formatting of factorial function."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec ((factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))) (factorial 5))"""
        result = printer.format(code)

        # Should be properly formatted
        assert "letrec" in result
        assert "factorial" in result
        assert "lambda" in result
        assert "(* n (factorial (- n 1)))" in result  # Should be compact

    def test_mutual_recursion(self):
        """Test formatting of mutually recursive functions."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec ((even? (lambda (n) (or (= n 0) (odd? (- n 1))))) (odd? (lambda (n) (and (!= n 0) (even? (- n 1)))))) (even? 10))"""
        result = printer.format(code)

        # Should have blank line between function definitions
        assert "even?" in result
        assert "odd?" in result
        # Check for blank line between definitions
        lines = result.split("\n")
        blank_lines = [i for i, line in enumerate(lines) if line.strip() == ""]
        assert len(blank_lines) > 0

    def test_map_with_lambda(self):
        """Test formatting of map with lambda."""
        printer = AIFPLPrettyPrinter()
        code = "(map (lambda (x) (* x 2)) (list 1 2 3 4 5))"
        result = printer.format(code)

        # Should stay compact
        assert "(map (lambda (x) (* x 2)) (list 1 2 3 4 5))" in result
