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

        expected = "(let ((x 5) (y 10)) (+ x y))\n"
        assert result == expected

    def test_let_star(self):
        """Test formatting of let* expression."""
        printer = AIFPLPrettyPrinter()
        code = "(let* ((x 5)(y (* x 2))) (+ x y))"
        result = printer.format(code)

        expected = "(let* ((x 5) (y (* x 2))) (+ x y))\n"
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


class TestPrettyPrinterLambda:
    """Test formatting of lambda expressions."""

    def test_simple_lambda(self):
        """Test formatting of simple lambda."""
        printer = AIFPLPrettyPrinter()
        code = "(lambda (x y) (* x y))"
        result = printer.format(code)

        expected = "(lambda (x y) (* x y))\n"
        assert result == expected

    def test_lambda_with_complex_body(self):
        """Test formatting of lambda with complex body."""
        printer = AIFPLPrettyPrinter()
        code = "(lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"
        result = printer.format(code)

        # Should be compact since it fits on one line
        assert "lambda" in result
        assert "if" in result


class TestPrettyPrinterConditionals:
    """Test formatting of conditional expressions."""

    def test_if_expression(self):
        """Test formatting of if expression."""
        printer = AIFPLPrettyPrinter()
        code = "(if (> x 5) (+ x 10) (- x 5))"
        result = printer.format(code)

        expected = "(if (> x 5) (+ x 10) (- x 5))\n"
        assert result == expected

    def test_nested_if(self):
        """Test formatting of nested if expressions."""
        printer = AIFPLPrettyPrinter()
        code = "(if (> x 0) (if (< x 10) 1 2) 3)"
        result = printer.format(code)

        # Should be compact since it fits on one line
        assert "if" in result
        # Compact: (if (> x 0) (if (< x 10) 1 2) 3)


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

        # Now uses compact format since it fits on one line
        expected = "(let ((x 5) (y 10)) (+ x y))\n"
        assert result == expected

    def test_letrec_binding_value_indentation(self):
        """Test that letrec binding values are indented correctly."""
        printer = AIFPLPrettyPrinter()
        code = "(letrec ((factorial (lambda (n) (if (integer=? n 0) 1 (integer* n (factorial (integer- n 1))))))) (factorial 5))"
        result = printer.format(code)

        # Now uses expanded format with bindings on next line
        lines = result.split("\n")
        # Check structure: letrec on first line, bindings on next line
        assert lines[0].strip() == "(letrec"
        # Bindings list should be indented
        assert lines[1].strip().startswith("((factorial")
        # Body should be on its own line
        factorial_call_line = [line for line in lines if "(factorial 5)" in line][0]
        assert factorial_call_line.strip() == "(factorial 5))"

    def test_nested_expression_indentation(self):
        """Test that nested expressions maintain proper indentation."""
        printer = AIFPLPrettyPrinter()
        code = "(let ((x (let ((y 5)) (+ y 1)))) (+ x 10))"
        result = printer.format(code)

        # Now uses compact format since it fits on one line
        # But inner let has its own formatting
        assert "let" in result
        expected = "(let ((x (let ((y 5)) (+ y 1)))) (+ x 10))\n"
        assert result == expected


class TestPrettyPrinterOptions:
    """Test formatting options."""

    def test_custom_indent_size(self):
        """Test custom indentation size."""
        options = FormatOptions(indent_size=4)
        printer = AIFPLPrettyPrinter(options)
        code = "(let ((x 5)) (+ x 10))"
        result = printer.format(code)

        # Uses compact format, so all on one line
        assert result == "(let ((x 5)) (+ x 10))\n"

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

        # Now uses compact format since it fits on one line
        assert "match" in result
        assert result == "(match x (1 'one) (2 'two) (_ 'other))\n"

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
        """Test formatting of map with lambda."""
        printer = AIFPLPrettyPrinter()
        code = "(map (lambda (x) (* x 2)) (list 1 2 3 4 5))"
        result = printer.format(code)

        # Should stay compact
        assert "(map (lambda (x) (* x 2)) (list 1 2 3 4 5))" in result



class TestPrettyPrinterNestedLambdaIndentation:
    """Test proper indentation of nested lambdas in multiline lists."""

    def test_nested_lambda_body_indentation(self):
        """Test that nested lambda bodies are indented correctly relative to their position."""
        # Use a low compact threshold to force multiline formatting
        options = FormatOptions(compact_threshold=40)
        printer = AIFPLPrettyPrinter(options)

        # This pattern from validation.aifpl exposed a bug where the inner lambda
        # body was indented relative to the wrong base position
        code = '(map (lambda (dep) (alist-get dep "to-task")) (filter (lambda (dep) (string=? (alist-get dep "from-task") task-id)) dependencies))'
        result = printer.format(code)

        lines = result.split('\n')

        # Find the line with the inner lambda (inside filter)
        filter_lambda_line = None
        string_eq_line = None
        for i, line in enumerate(lines):
            if 'filter (lambda' in line:
                filter_lambda_line = i
            if '(string=?' in line and string_eq_line is None:
                string_eq_line = i

        assert filter_lambda_line is not None, "Should find filter lambda line"
        assert string_eq_line is not None, "Should find string=? line"

        # The string=? should be indented 2 spaces from where (lambda starts
        lambda_pos = lines[filter_lambda_line].index('(lambda')
        string_eq_indent = len(lines[string_eq_line]) - len(lines[string_eq_line].lstrip())
        expected_indent = lambda_pos + 2

        assert string_eq_indent == expected_indent, \
            f"Lambda body at line {string_eq_line} should be indented {expected_indent} spaces " \
            f"(lambda at column {lambda_pos} + 2), but got {string_eq_indent} spaces.\\n" \
            f"Filter lambda line: {repr(lines[filter_lambda_line])}\\n" \
            f"String=? line: {repr(lines[string_eq_line])}"


class TestPrettyPrinterCommentsInSpecialForms:
    """Test comment handling in special forms (lambda, if, let, match)."""

    def test_lambda_with_comment_before_body(self):
        """Test lambda with comment before body expression."""
        printer = AIFPLPrettyPrinter()
        code = """(lambda (x)
  ; This is the body
  (* x 2))"""
        result = printer.format(code)

        # Comment should be preserved and body should follow
        assert "; This is the body" in result
        assert "(* x 2)" in result
        lines = result.split("\n")
        # Find comment line and body line
        comment_idx = next(i for i, line in enumerate(lines) if "; This is the body" in line)
        body_idx = next(i for i, line in enumerate(lines) if "(* x 2)" in line)
        # Body should come after comment
        assert body_idx > comment_idx

    def test_lambda_with_multiple_comments_before_body(self):
        """Test lambda with multiple comments before body."""
        printer = AIFPLPrettyPrinter()
        code = """(lambda (x)
  ; Comment 1
  ; Comment 2
  (* x 2))"""
        result = printer.format(code)

        assert "; Comment 1" in result
        assert "; Comment 2" in result
        assert "(* x 2)" in result

    def test_if_with_comment_before_then_branch(self):
        """Test if with comment before then branch."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  ; Positive case
  (+ x 1)
  (- x 1))"""
        result = printer.format(code)

        assert "; Positive case" in result
        assert "(+ x 1)" in result
        assert "(- x 1)" in result

    def test_if_with_comment_before_else_branch(self):
        """Test if with comment before else branch."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  (+ x 1)
  ; Negative case
  (- x 1))"""
        result = printer.format(code)

        assert "(+ x 1)" in result
        assert "; Negative case" in result
        assert "(- x 1)" in result
        lines = result.split("\n")
        # Find indices
        then_idx = next(i for i, line in enumerate(lines) if "(+ x 1)" in line)
        comment_idx = next(i for i, line in enumerate(lines) if "; Negative case" in line)
        else_idx = next(i for i, line in enumerate(lines) if "(- x 1)" in line)
        # Order should be: then, comment, else
        assert then_idx < comment_idx < else_idx

    def test_if_with_comments_before_both_branches(self):
        """Test if with comments before both then and else branches."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  ; Positive case
  (+ x 1)
  ; Negative case
  (- x 1))"""
        result = printer.format(code)

        assert "; Positive case" in result
        assert "; Negative case" in result
        assert "(+ x 1)" in result
        assert "(- x 1)" in result

    def test_nested_if_with_comments(self):
        """Test nested if expressions with comments."""
        printer = AIFPLPrettyPrinter()
        code = """(if (> x 0)
  ; Check if we've seen this task in current path (cycle!)
  (if (member? task-id visited-in-path)
    ; Found a cycle - extract the cycle from path
    (let ((cycle-start-pos (position task-id path)))
      (if (!= cycle-start-pos #f)
        (list (drop cycle-start-pos (append path (list task-id))))
        (list)))
    ; Not a cycle yet, continue DFS
    (let* ((successors (get-successors task-id dependencies))
           (new-path (append path (list task-id)))
           (new-visited (cons task-id visited-in-path)))
      (fold append (list)
            (map (lambda (succ) (dfs-visit succ new-path new-visited))
                 successors))))
  ; Else branch
  42)"""
        result = printer.format(code)

        # All comments should be preserved
        assert "; Check if we've seen this task in current path (cycle!)" in result
        assert "; Found a cycle - extract the cycle from path" in result
        assert "; Not a cycle yet, continue DFS" in result
        assert "; Else branch" in result

        # All code should be present
        assert "member?" in result
        assert "get-successors" in result
        assert "dfs-visit" in result

    def test_let_with_comment_before_body(self):
        """Test let with comment before body."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5)
      (y 10))
  ; Calculate sum
  (+ x y))"""
        result = printer.format(code)

        assert "; Calculate sum" in result
        assert "(+ x y)" in result

    def test_let_with_multiple_comments_before_body(self):
        """Test let with multiple comments before body."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5))
  ; Comment 1
  ; Comment 2
  (+ x 1))"""
        result = printer.format(code)

        assert "; Comment 1" in result
        assert "; Comment 2" in result
        assert "(+ x 1)" in result

    def test_match_with_comments_between_clauses(self):
        """Test match with comments between clauses."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  ; First case
  (1 "one")
  ; Second case
  (2 "two")
  ; Default case
  (_ "other"))"""
        result = printer.format(code)

        assert "; First case" in result
        assert "; Second case" in result
        assert "; Default case" in result
        assert '(1 "one")' in result
        assert '(2 "two")' in result
        assert '(_ "other")' in result

    def test_match_with_end_of_line_comments(self):
        """Test match with end-of-line comments."""
        printer = AIFPLPrettyPrinter()
        code = """(match x
  (1 "one")  ; First
  (2 "two")  ; Second
  (_ "other"))  ; Default"""
        result = printer.format(code)

        assert "; First" in result
        assert "; Second" in result
        assert "; Default" in result

    def test_letrec_with_comments_in_lambda_body(self):
        """Test letrec with comments inside lambda bodies."""
        printer = AIFPLPrettyPrinter()
        code = """(letrec ((factorial (lambda (n)
                      ; Base case
                      (if (<= n 1)
                        1
                        ; Recursive case
                        (* n (factorial (- n 1)))))))
  (factorial 5))"""
        result = printer.format(code)

        assert "; Base case" in result
        assert "; Recursive case" in result
        assert "factorial" in result
        assert "(* n (factorial (- n 1)))" in result

    def test_complex_nested_comments(self):
        """Test complex nesting with comments at multiple levels."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5))
  ; Start of computation
  (let ((y 10))
    ; Inner let
    (if (> x 0)
      ; Positive branch
      (+ x y)
      ; Negative branch
      (- x y))))"""
        result = printer.format(code)

        assert "; Start of computation" in result
        assert "; Inner let" in result
        assert "; Positive branch" in result
        assert "; Negative branch" in result
        assert "(+ x y)" in result
        assert "(- x y)" in result

    def test_comment_indentation_in_nested_structures(self):
        """Test that comments are properly indented in nested structures."""
        printer = AIFPLPrettyPrinter()
        code = """(lambda (x)
  ; Outer comment
  (if (> x 0)
    ; Inner comment
    (+ x 1)
    (- x 1)))"""
        result = printer.format(code)

        lines = result.split("\n")
        outer_comment_line = next(line for line in lines if "; Outer comment" in line)
        inner_comment_line = next(line for line in lines if "; Inner comment" in line)

        # Inner comment should be more indented than outer
        outer_indent = len(outer_comment_line) - len(outer_comment_line.lstrip())
        inner_indent = len(inner_comment_line) - len(inner_comment_line.lstrip())
        assert inner_indent > outer_indent

    def test_end_of_line_comment_after_closing_paren(self):
        """Test end-of-line comment after closing paren."""
        printer = AIFPLPrettyPrinter()
        code = """(let ((x 5)
      (y 10))  ; End of bindings
  (+ x y))"""
        result = printer.format(code)

        assert "; End of bindings" in result
        # Comment should be on same line as closing paren
        assert "))  ; End of bindings" in result

    def test_validation_aifpl_style_comments(self):
        """Test the style of comments found in validation.aifpl."""
        printer = AIFPLPrettyPrinter()
        # Simplified version of the detect-cycles function structure
        code = """(lambda (task-id path visited-in-path)
  ; Check if we've seen this task in current path (cycle!)
  (if (member? task-id visited-in-path)
    ; Found a cycle - extract the cycle from path
    (let ((cycle-start-pos (position task-id path)))
      (if (!= cycle-start-pos #f)
        (list (drop cycle-start-pos (append path (list task-id))))
        (list)))
    ; Not a cycle yet, continue DFS
    (let* ((successors (get-successors task-id dependencies))
           (new-path (append path (list task-id)))
           (new-visited (cons task-id visited-in-path)))
      (fold append (list)
            (map (lambda (succ) (dfs-visit succ new-path new-visited))
                 successors)))))"""
        result = printer.format(code)

        # All three major comments should be present
        assert "; Check if we've seen this task in current path (cycle!)" in result
        assert "; Found a cycle - extract the cycle from path" in result
        assert "; Not a cycle yet, continue DFS" in result

        # The if expression should have both branches
        assert "member?" in result
        assert "get-successors" in result
        assert "dfs-visit" in result

        # Structure should be maintained
        lines = result.split("\n")
        # Find key lines to verify structure
        member_idx = next(i for i, line in enumerate(lines) if "member?" in line)
        cycle_comment_idx = next(i for i, line in enumerate(lines) if "; Found a cycle" in line)
        dfs_comment_idx = next(i for i, line in enumerate(lines) if "; Not a cycle yet" in line)

        # Comments should be in the right order
        assert member_idx < cycle_comment_idx < dfs_comment_idx
