"""
Tests for the Java lexer.
"""
import pytest

from syntax.java.java_lexer import JavaLexer, JavaLexerState
from syntax.lexer import TokenType


class TestJavaLexerBasics:
    """Test basic Java lexer functionality."""

    def test_empty_input(self):
        """Test lexing empty input."""
        lexer = JavaLexer()
        state = lexer.lex(None, '')

        tokens = list(lexer._tokens)
        assert len(tokens) == 0

    def test_whitespace_only(self):
        """Test lexing whitespace-only input produces no tokens."""
        for code in [' ', '   ', '\t', '  \t  ']:
            lexer = JavaLexer()
            lexer.lex(None, code)
            assert len(lexer._tokens) == 0

    def test_returns_java_lexer_state(self):
        """Test that lex returns a JavaLexerState."""
        lexer = JavaLexer()
        state = lexer.lex(None, 'int x;')
        assert isinstance(state, JavaLexerState)

    def test_initial_state_flags_false(self):
        """Test that initial state has all flags False."""
        lexer = JavaLexer()
        state = lexer.lex(None, '')
        assert state.in_block_comment is False
        assert state.in_javadoc is False
        assert state.in_text_block is False
        assert state.in_annotation is False

    def test_token_positions_are_sequential(self):
        """Test that token start positions are non-decreasing."""
        lexer = JavaLexer()
        lexer.lex(None, 'int x = 42;')
        tokens = list(lexer._tokens)
        for i in range(len(tokens) - 1):
            assert tokens[i].start <= tokens[i + 1].start

    def test_token_values_match_input(self):
        """Test that token values match the input at their start positions."""
        input_str = 'int x = 42;'
        lexer = JavaLexer()
        lexer.lex(None, input_str)
        for token in lexer._tokens:
            assert input_str[token.start:token.start + len(token.value)] == token.value


class TestJavaKeywords:
    """Test Java keyword tokenisation."""

    def test_primitive_type_keywords(self):
        """Test primitive type keywords are tokenised as KEYWORD."""
        for kw in ['boolean', 'byte', 'char', 'double', 'float', 'int', 'long', 'short', 'void']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{kw}' should produce one token"
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"
            assert tokens[0].value == kw

    def test_control_flow_keywords(self):
        """Test control flow keywords."""
        for kw in ['if', 'else', 'for', 'while', 'do', 'switch', 'case', 'default',
                   'break', 'continue', 'return', 'goto']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"

    def test_class_and_object_keywords(self):
        """Test class and object-related keywords."""
        for kw in ['class', 'interface', 'enum', 'extends', 'implements', 'new',
                   'this', 'super', 'instanceof']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"

    def test_modifier_keywords(self):
        """Test access modifier and other modifier keywords."""
        for kw in ['public', 'private', 'protected', 'static', 'final', 'abstract',
                   'synchronized', 'volatile', 'transient', 'native', 'strictfp']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"

    def test_exception_keywords(self):
        """Test exception-related keywords."""
        for kw in ['try', 'catch', 'finally', 'throw', 'throws', 'assert']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"

    def test_import_package_keywords(self):
        """Test import and package keywords."""
        for kw in ['import', 'package']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD

    def test_literal_value_keywords(self):
        """Test literal value keywords."""
        for kw in ['true', 'false', 'null']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"

    def test_module_system_keywords(self):
        """Test Java 9+ module system keywords."""
        for kw in ['module', 'requires', 'exports', 'opens', 'uses', 'provides',
                   'with', 'to', 'transitive']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"

    def test_modern_java_keywords(self):
        """Test modern Java keywords (Java 10+)."""
        for kw in ['var', 'yield', 'record', 'sealed', 'permits']:
            lexer = JavaLexer()
            lexer.lex(None, kw)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.KEYWORD, f"'{kw}' should be KEYWORD"

    def test_keywords_are_case_sensitive(self):
        """Test that keywords are case-sensitive (Java is case-sensitive)."""
        for word in ['Int', 'INT', 'Class', 'CLASS', 'Public', 'PUBLIC', 'True', 'FALSE']:
            lexer = JavaLexer()
            lexer.lex(None, word)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER, f"'{word}' should be IDENTIFIER not KEYWORD"

    def test_keyword_not_prefix_matched(self):
        """Test that keywords are not matched as prefixes of identifiers."""
        for word in ['integer', 'interface_', 'classname', 'finals', 'returns']:
            lexer = JavaLexer()
            lexer.lex(None, word)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER, f"'{word}' should be IDENTIFIER"


class TestJavaIdentifiers:
    """Test Java identifier tokenisation."""

    def test_simple_identifiers(self):
        """Test simple identifiers."""
        for ident in ['x', 'myVar', 'MyClass', '_private', '__init', 'var123', 'camelCase']:
            lexer = JavaLexer()
            lexer.lex(None, ident)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER
            assert tokens[0].value == ident

    def test_identifier_starting_with_underscore(self):
        """Test identifiers starting with underscore."""
        for ident in ['_', '__', '_field', '__field__']:
            lexer = JavaLexer()
            lexer.lex(None, ident)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.IDENTIFIER


class TestJavaOperators:
    """Test Java operator tokenisation."""

    def test_arithmetic_operators(self):
        """Test arithmetic operators."""
        for op in ['+', '-', '*', '/', '%']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_comparison_operators(self):
        """Test comparison operators."""
        for op in ['==', '!=', '<', '>', '<=', '>=']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_logical_operators(self):
        """Test logical operators."""
        for op in ['&&', '||', '!']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_bitwise_operators(self):
        """Test bitwise operators."""
        for op in ['&', '|', '^', '~', '<<', '>>', '>>>']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_assignment_operators(self):
        """Test assignment operators."""
        for op in ['=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=', '>>>=']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_increment_decrement(self):
        """Test increment and decrement operators."""
        for op in ['++', '--']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_member_access_and_special_operators(self):
        """Test member access, method reference, lambda, and varargs operators."""
        for op in ['.', '::', '->', '...']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{op}' should produce one token"
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_ternary_and_colon(self):
        """Test ternary operator components."""
        for op in ['?', ':']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_brackets_parens_braces(self):
        """Test brackets, parentheses, and braces."""
        for op in ['(', ')', '[', ']', '{', '}']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.OPERATOR
            assert tokens[0].value == op

    def test_punctuation(self):
        """Test punctuation."""
        for op in [';', ',', '@']:
            lexer = JavaLexer()
            lexer.lex(None, op)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1

    def test_maximum_munch_shift_operators(self):
        """Test that >>> and >> are greedily matched before >."""
        lexer = JavaLexer()
        lexer.lex(None, '>>>=')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].value == '>>>='

        lexer = JavaLexer()
        lexer.lex(None, '>>>')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].value == '>>>'

        lexer = JavaLexer()
        lexer.lex(None, '>>=')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].value == '>>='


class TestJavaStringLiterals:
    """Test Java string literal tokenisation."""

    def test_simple_string(self):
        """Test simple double-quoted string."""
        lexer = JavaLexer()
        lexer.lex(None, '"hello"')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"hello"'

    def test_empty_string(self):
        """Test empty string literal."""
        lexer = JavaLexer()
        lexer.lex(None, '""')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '""'

    def test_string_with_escape_sequences(self):
        """Test string with escape sequences."""
        lexer = JavaLexer()
        lexer.lex(None, r'"hello\nworld"')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_string_with_escaped_quote(self):
        """Test string with escaped double quote inside."""
        lexer = JavaLexer()
        lexer.lex(None, r'"say \"hi\""')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_character_literal(self):
        """Test character literal."""
        lexer = JavaLexer()
        lexer.lex(None, "'a'")
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_character_literal_escape(self):
        """Test escaped character literal."""
        lexer = JavaLexer()
        lexer.lex(None, r"'\n'")
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_text_block_single_line(self):
        """Test text block (triple-quoted string) on a single line."""
        lexer = JavaLexer()
        lexer.lex(None, '"""hello"""')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == '"""hello"""'

    def test_text_block_empty(self):
        """Test empty text block."""
        lexer = JavaLexer()
        lexer.lex(None, '""""""')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_text_block_continuation_start(self):
        """Test that an unclosed text block sets in_text_block state."""
        lexer = JavaLexer()
        state = lexer.lex(None, '"""hello')
        assert state.in_text_block is True

    def test_text_block_continuation_end(self):
        """Test that a text block continued from previous line closes correctly."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '"""hello')
        assert state1.in_text_block is True

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, 'world"""')
        assert state2.in_text_block is False
        tokens = list(lexer2._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING

    def test_text_block_continuation_middle(self):
        """Test text block continuation line with no closing quotes."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '"""line one')

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, 'line two')
        assert state2.in_text_block is True

    def test_string_keywords_not_tokenised_as_keywords(self):
        """Test that keywords inside strings are not separately tokenised."""
        lexer = JavaLexer()
        lexer.lex(None, '"int class public"')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.STRING


class TestJavaNumericLiterals:
    """Test Java numeric literal tokenisation."""

    def test_integer_literals(self):
        """Test integer literals."""
        for num in ['0', '1', '42', '1000', '2147483647']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.NUMBER
            assert tokens[0].value == num

    def test_long_suffix(self):
        """Test long integer literals with L suffix."""
        for num in ['42L', '42l', '1000L']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.NUMBER
            assert tokens[0].value == num

    def test_float_literals(self):
        """Test float literals."""
        for num in ['3.14', '0.5', '1.0']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.NUMBER

    def test_float_suffix(self):
        """Test float literals with F/D suffix."""
        for num in ['3.14F', '3.14f', '3.14D', '3.14d']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.NUMBER
            assert tokens[0].value == num

    def test_scientific_notation(self):
        """Test scientific notation literals."""
        for num in ['1e10', '1E10', '1.5e-3', '2.0E+4']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{num}' should produce one token"
            assert tokens[0].type == TokenType.NUMBER

    def test_hex_literals(self):
        """Test hexadecimal integer literals."""
        for num in ['0x0', '0xFF', '0xDEAD', '0xBEEF', '0X1A2B']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.NUMBER
            assert tokens[0].value == num

    def test_binary_literals(self):
        """Test binary integer literals (Java 7+)."""
        for num in ['0b0', '0b1', '0b1010', '0B1111']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.NUMBER
            assert tokens[0].value == num

    def test_underscore_separators(self):
        """Test numeric literals with underscore separators (Java 7+)."""
        for num in ['1_000_000', '0xFF_FF', '0b1010_0101']:
            lexer = JavaLexer()
            lexer.lex(None, num)
            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type == TokenType.NUMBER
            assert tokens[0].value == num

    def test_dot_float_literal(self):
        """Test float literal starting with a dot."""
        lexer = JavaLexer()
        lexer.lex(None, '.5')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == '.5'


class TestJavaComments:
    """Test Java comment tokenisation."""

    def test_single_line_comment(self):
        """Test single-line comment."""
        lexer = JavaLexer()
        lexer.lex(None, '// this is a comment')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == '// this is a comment'

    def test_single_line_comment_empty(self):
        """Test empty single-line comment."""
        lexer = JavaLexer()
        lexer.lex(None, '//')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_single_line_comment_with_code_before(self):
        """Test single-line comment after code."""
        lexer = JavaLexer()
        lexer.lex(None, 'int x; // declare x')
        tokens = list(lexer._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1
        assert comment_tokens[0].value == '// declare x'

    def test_block_comment_single_line(self):
        """Test block comment on a single line."""
        lexer = JavaLexer()
        lexer.lex(None, '/* a comment */')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT
        assert tokens[0].value == '/* a comment */'

    def test_block_comment_start(self):
        """Test that unclosed block comment sets in_block_comment state."""
        lexer = JavaLexer()
        state = lexer.lex(None, '/* start')
        assert state.in_block_comment is True

    def test_block_comment_continuation(self):
        """Test block comment continuation across lines."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '/* start')
        assert state1.in_block_comment is True

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, 'middle')
        assert state2.in_block_comment is True

    def test_block_comment_end(self):
        """Test block comment ending on a continuation line."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '/* start')

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, 'end */')
        assert state2.in_block_comment is False

        tokens2 = list(lexer2._tokens)
        assert len(tokens2) == 1
        assert tokens2[0].type == TokenType.COMMENT

    def test_block_comment_end_with_code_after(self):
        """Test that code after a block comment close is tokenised."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '/* comment')

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, 'end */ int x;')
        assert state2.in_block_comment is False

        tokens2 = list(lexer2._tokens)
        comment_tokens = [t for t in tokens2 if t.type == TokenType.COMMENT]
        keyword_tokens = [t for t in tokens2 if t.type == TokenType.KEYWORD]
        assert len(comment_tokens) == 1
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'int'

    def test_javadoc_comment_single_line(self):
        """Test JavaDoc comment on a single line."""
        lexer = JavaLexer()
        lexer.lex(None, '/** JavaDoc comment */')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT

    def test_javadoc_comment_start(self):
        """Test that unclosed JavaDoc comment sets in_javadoc state."""
        lexer = JavaLexer()
        state = lexer.lex(None, '/** start')
        assert state.in_javadoc is True
        assert state.in_block_comment is True

    def test_javadoc_comment_end(self):
        """Test JavaDoc comment ending on a continuation line."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '/** start')

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, ' * end */')
        assert state2.in_javadoc is False
        assert state2.in_block_comment is False

    def test_keywords_in_comments_not_tokenised(self):
        """Test that keywords inside comments are not separately tokenised."""
        lexer = JavaLexer()
        lexer.lex(None, '// int class public')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.COMMENT


class TestJavaAnnotations:
    """Test Java annotation tokenisation."""

    def test_simple_annotation(self):
        """Test simple annotation."""
        lexer = JavaLexer()
        lexer.lex(None, '@Override')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.ANNOTATION
        assert tokens[0].value == '@Override'

    def test_annotation_with_package(self):
        """Test annotation with package path."""
        lexer = JavaLexer()
        lexer.lex(None, '@java.lang.Override')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.ANNOTATION
        assert tokens[0].value == '@java.lang.Override'

    def test_annotation_suppress_warnings(self):
        """Test SuppressWarnings annotation."""
        lexer = JavaLexer()
        lexer.lex(None, '@SuppressWarnings')
        tokens = list(lexer._tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.ANNOTATION

    def test_annotation_followed_by_class(self):
        """Test annotation followed by class declaration."""
        lexer = JavaLexer()
        lexer.lex(None, '@Override public')
        tokens = list(lexer._tokens)
        annotation_tokens = [t for t in tokens if t.type == TokenType.ANNOTATION]
        keyword_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(annotation_tokens) == 1
        assert len(keyword_tokens) == 1


class TestJavaLexerStatePersistence:
    """Test that lexer state is correctly persisted and restored."""

    def test_state_type_assertion(self):
        """Test that passing wrong state type raises AssertionError."""
        from syntax.lexer import LexerState
        lexer = JavaLexer()
        with pytest.raises(AssertionError):
            lexer.lex(LexerState(), 'int x;')

    def test_block_comment_state_persists(self):
        """Test block comment state across three lines."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '/*')
        assert state1.in_block_comment is True

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, ' * middle line')
        assert state2.in_block_comment is True

        lexer3 = JavaLexer()
        state3 = lexer3.lex(state2, ' */')
        assert state3.in_block_comment is False

    def test_text_block_state_persists(self):
        """Test text block state across multiple lines."""
        lexer1 = JavaLexer()
        state1 = lexer1.lex(None, '"""')
        assert state1.in_text_block is True

        lexer2 = JavaLexer()
        state2 = lexer2.lex(state1, '    line one')
        assert state2.in_text_block is True

        lexer3 = JavaLexer()
        state3 = lexer3.lex(state2, '    """')
        assert state3.in_text_block is False

    def test_normal_state_after_complete_statement(self):
        """Test that state is clean after a complete statement."""
        lexer = JavaLexer()
        state = lexer.lex(None, 'int x = 42;')
        assert state.in_block_comment is False
        assert state.in_javadoc is False
        assert state.in_text_block is False
