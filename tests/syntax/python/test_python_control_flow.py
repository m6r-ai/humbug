"""
Tests for Python control flow statement tokenization.
"""
import pytest

from syntax.python.python_lexer import PythonLexer


class TestPythonControlFlow:
    """Test Python control flow statement tokenization."""

    def test_if_statement(self):
        """Test if statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'if x > 0:')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1, "Should have 'if' keyword"
        assert keyword_tokens[0].value == 'if'

    def test_if_else_statement(self):
        """Test if-else statement."""
        lines = ['if x > 0:', 'else:']
        keywords_found = []

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            keywords = [t.value for t in tokens if t.type.name == 'KEYWORD']
            keywords_found.extend(keywords)

        assert 'if' in keywords_found
        assert 'else' in keywords_found

    def test_if_elif_else_statement(self):
        """Test if-elif-else statement."""
        lines = ['if x > 0:', 'elif x < 0:', 'else:']
        keywords_found = []

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            keywords = [t.value for t in tokens if t.type.name == 'KEYWORD']
            keywords_found.extend(keywords)

        assert 'if' in keywords_found
        assert 'elif' in keywords_found
        assert 'else' in keywords_found

    def test_while_loop(self):
        """Test while loop."""
        lexer = PythonLexer()
        lexer.lex(None, 'while x < 10:')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1, "Should have 'while' keyword"
        assert keyword_tokens[0].value == 'while'

    def test_for_loop(self):
        """Test for loop."""
        lexer = PythonLexer()
        lexer.lex(None, 'for i in range(10):')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'for' in keyword_values
        assert 'in' in keyword_values

    def test_for_loop_with_enumerate(self):
        """Test for loop with enumerate."""
        lexer = PythonLexer()
        lexer.lex(None, 'for i, item in enumerate(items):')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'for' in keyword_values
        assert 'in' in keyword_values

    def test_break_statement(self):
        """Test break statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'break')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'
        assert tokens[0].value == 'break'

    def test_continue_statement(self):
        """Test continue statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'continue')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'
        assert tokens[0].value == 'continue'

    def test_pass_statement(self):
        """Test pass statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'pass')

        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type.name == 'KEYWORD'
        assert tokens[0].value == 'pass'

    def test_return_statement(self):
        """Test return statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'return x')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'return'

    def test_return_statement_with_expression(self):
        """Test return statement with complex expression."""
        lexer = PythonLexer()
        lexer.lex(None, 'return x + y * 2')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'return'

    def test_return_none(self):
        """Test return None statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'return None')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'return' in keyword_values
        assert 'None' in keyword_values

    def test_yield_statement(self):
        """Test yield statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'yield x')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'yield'

    def test_yield_from_statement(self):
        """Test yield from statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'yield from iterable')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'yield' in keyword_values
        assert 'from' in keyword_values

    def test_try_except_statement(self):
        """Test try-except statement."""
        lines = ['try:', 'except Exception:']
        keywords_found = []

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            keywords = [t.value for t in tokens if t.type.name == 'KEYWORD']
            keywords_found.extend(keywords)

        assert 'try' in keywords_found
        assert 'except' in keywords_found

    def test_try_except_finally_statement(self):
        """Test try-except-finally statement."""
        lines = ['try:', 'except Exception:', 'finally:']
        keywords_found = []

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            keywords = [t.value for t in tokens if t.type.name == 'KEYWORD']
            keywords_found.extend(keywords)

        assert 'try' in keywords_found
        assert 'except' in keywords_found
        assert 'finally' in keywords_found

    def test_raise_statement(self):
        """Test raise statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'raise ValueError("error")')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'raise'

    def test_assert_statement(self):
        """Test assert statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'assert x > 0')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'assert'

    def test_assert_with_message(self):
        """Test assert statement with message."""
        lexer = PythonLexer()
        lexer.lex(None, 'assert x > 0, "x must be positive"')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'assert'

    def test_with_statement(self):
        """Test with statement (context manager)."""
        lexer = PythonLexer()
        lexer.lex(None, 'with open("file.txt") as f:')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'with' in keyword_values
        assert 'as' in keyword_values

    def test_multiple_with_items(self):
        """Test with statement with multiple items."""
        lexer = PythonLexer()
        lexer.lex(None, 'with open("a") as f1, open("b") as f2:')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'with' in keyword_values
        assert keyword_values.count('as') == 2

    def test_nested_if_statements(self):
        """Test nested if statements (indentation)."""
        lines = [
            'if x > 0:',
            '    if y > 0:',
        ]

        for line in lines:
            lexer = PythonLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) == 1
            assert keyword_tokens[0].value == 'if'

    def test_ternary_operator(self):
        """Test ternary conditional operator."""
        lexer = PythonLexer()
        lexer.lex(None, 'x if condition else y')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'if' in keyword_values
        assert 'else' in keyword_values

    def test_match_statement(self):
        """Test match statement (Python 3.10+)."""
        # Note: match/case are not in the keyword list, so they'll be identifiers
        lexer = PythonLexer()
        lexer.lex(None, 'match value:')

        tokens = list(lexer._tokens)
        # match might not be recognized as keyword in older implementations
        identifier_or_keyword = [t for t in tokens if t.value == 'match']
        assert len(identifier_or_keyword) >= 1, "Should have 'match' token"

    def test_async_for_loop(self):
        """Test async for loop."""
        lexer = PythonLexer()
        lexer.lex(None, 'async for item in async_iter:')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'async' in keyword_values
        assert 'for' in keyword_values
        assert 'in' in keyword_values

    def test_async_with_statement(self):
        """Test async with statement."""
        lexer = PythonLexer()
        lexer.lex(None, 'async with resource as r:')

        tokens = list(lexer._tokens)
        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keyword_tokens]
        assert 'async' in keyword_values
        assert 'with' in keyword_values
        assert 'as' in keyword_values
