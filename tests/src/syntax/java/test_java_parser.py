"""
Tests for the Java parser.
"""
import pytest

from syntax.java.java_parser import JavaParser, JavaParserState
from syntax.lexer import TokenType


class TestJavaParserBasics:
    """Test basic Java parser functionality."""

    def test_parse_empty_input(self):
        """Test parsing empty input."""
        parser = JavaParser()
        state = parser.parse(None, '')
        assert isinstance(state, JavaParserState)
        assert state.parsing_continuation is False
        assert state.continuation_state == 0
        assert state.in_element is False
        assert state.in_generic is False
        assert state.generic_depth == 0

    def test_parse_simple_declaration(self):
        """Test parsing a simple variable declaration."""
        parser = JavaParser()
        state = parser.parse(None, 'int x = 42;')
        assert isinstance(state, JavaParserState)
        assert state.parsing_continuation is False

    def test_parse_returns_java_parser_state(self):
        """Test that parse returns a JavaParserState."""
        parser = JavaParser()
        state = parser.parse(None, 'int x;')
        assert isinstance(state, JavaParserState)

    def test_state_type_assertion(self):
        """Test that passing wrong state type raises AssertionError."""
        from syntax.parser import ParserState
        parser = JavaParser()
        with pytest.raises(AssertionError):
            parser.parse(ParserState(), 'int x;')

    def test_keywords_remain_keywords(self):
        """Test that keywords are not reclassified by the parser."""
        parser = JavaParser()
        parser.parse(None, 'if (condition)')
        tokens = list(parser._tokens)
        if_tokens = [t for t in tokens if t.value == 'if']
        assert len(if_tokens) == 1
        assert if_tokens[0].type == TokenType.KEYWORD

    def test_numbers_not_reclassified(self):
        """Test that numbers are not reclassified by the parser."""
        parser = JavaParser()
        parser.parse(None, '42 + 3.14')
        tokens = list(parser._tokens)
        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 2

    def test_strings_not_reclassified(self):
        """Test that strings are not reclassified by the parser."""
        parser = JavaParser()
        parser.parse(None, '"hello"')
        tokens = list(parser._tokens)
        string_tokens = [t for t in tokens if t.type == TokenType.STRING]
        assert len(string_tokens) == 1

    def test_comments_not_reclassified(self):
        """Test that comments are not reclassified by the parser."""
        parser = JavaParser()
        parser.parse(None, '// a comment')
        tokens = list(parser._tokens)
        comment_tokens = [t for t in tokens if t.type == TokenType.COMMENT]
        assert len(comment_tokens) == 1


class TestJavaParserFunctionAndMethod:
    """Test function/method call identification."""

    def test_function_call(self):
        """Test that an identifier followed by ( is marked as FUNCTION_OR_METHOD."""
        parser = JavaParser()
        parser.parse(None, 'println("hello")')
        tokens = list(parser._tokens)
        fn_tokens = [t for t in tokens if t.value == 'println']
        assert len(fn_tokens) == 1
        assert fn_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_method_call_on_object(self):
        """Test method call on an object."""
        parser = JavaParser()
        parser.parse(None, 'obj.method()')
        tokens = list(parser._tokens)
        method_tokens = [t for t in tokens if t.value == 'method']
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_plain_identifier_not_function(self):
        """Test that a plain identifier not followed by ( stays IDENTIFIER."""
        parser = JavaParser()
        parser.parse(None, 'int x;')
        tokens = list(parser._tokens)
        x_tokens = [t for t in tokens if t.value == 'x']
        assert len(x_tokens) == 1
        assert x_tokens[0].type == TokenType.IDENTIFIER

    def test_multiple_method_calls(self):
        """Test multiple method calls on one line."""
        parser = JavaParser()
        parser.parse(None, 'foo(); bar();')
        tokens = list(parser._tokens)
        fn_tokens = [t for t in tokens if t.type == TokenType.FUNCTION_OR_METHOD]
        assert len(fn_tokens) == 2

    def test_nested_function_calls(self):
        """Test nested function calls."""
        parser = JavaParser()
        parser.parse(None, 'outer(inner())')
        tokens = list(parser._tokens)
        fn_tokens = [t for t in tokens if t.type == TokenType.FUNCTION_OR_METHOD]
        assert len(fn_tokens) == 2

    def test_constructor_call(self):
        """Test constructor call with new."""
        parser = JavaParser()
        parser.parse(None, 'new ArrayList()')
        tokens = list(parser._tokens)
        fn_tokens = [t for t in tokens if t.value == 'ArrayList']
        assert len(fn_tokens) == 1
        assert fn_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_keyword_before_paren_not_function(self):
        """Test that keywords followed by ( are not reclassified as FUNCTION_OR_METHOD."""
        parser = JavaParser()
        parser.parse(None, 'if (x > 0)')
        tokens = list(parser._tokens)
        if_tokens = [t for t in tokens if t.value == 'if']
        assert len(if_tokens) == 1
        assert if_tokens[0].type == TokenType.KEYWORD


class TestJavaParserElementAccess:
    """Test element/field access identification."""

    def test_field_access(self):
        """Test that an identifier after . is marked as ELEMENT."""
        parser = JavaParser()
        parser.parse(None, 'obj.field')
        tokens = list(parser._tokens)
        field_tokens = [t for t in tokens if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_chained_field_access(self):
        """Test chained field access."""
        parser = JavaParser()
        parser.parse(None, 'a.b.c')
        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type == TokenType.ELEMENT]
        assert len(element_tokens) == 2
        assert element_tokens[0].value == 'b'
        assert element_tokens[1].value == 'c'

    def test_method_after_dot_is_function(self):
        """Test that a method call after . is FUNCTION_OR_METHOD, not ELEMENT."""
        parser = JavaParser()
        parser.parse(None, 'obj.method()')
        tokens = list(parser._tokens)
        method_tokens = [t for t in tokens if t.value == 'method']
        assert len(method_tokens) == 1
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_chained_method_and_field(self):
        """Test chained method call followed by field access."""
        parser = JavaParser()
        parser.parse(None, 'obj.method().field')
        tokens = list(parser._tokens)
        method_tokens = [t for t in tokens if t.value == 'method']
        field_tokens = [t for t in tokens if t.value == 'field']
        assert method_tokens[0].type == TokenType.FUNCTION_OR_METHOD
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_in_element_state_set_after_dot(self):
        """Test that in_element state is True after a trailing dot."""
        parser = JavaParser()
        state = parser.parse(None, 'obj.')
        assert state.in_element is True

    def test_in_element_state_used_on_next_line(self):
        """Test that in_element state from previous line is used."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, 'obj.')
        assert state1.in_element is True

        parser2 = JavaParser()
        state2 = parser2.parse(state1, 'field')
        tokens2 = list(parser2._tokens)
        field_tokens = [t for t in tokens2 if t.value == 'field']
        assert len(field_tokens) == 1
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_in_element_reset_by_non_dot_operator(self):
        """Test that in_element is reset when a non-dot operator is encountered."""
        parser = JavaParser()
        state = parser.parse(None, 'a.b + c')
        tokens = list(parser._tokens)
        # c comes after +, so in_element should be reset; c should be plain IDENTIFIER
        c_tokens = [t for t in tokens if t.value == 'c']
        assert len(c_tokens) == 1
        assert c_tokens[0].type == TokenType.IDENTIFIER

    def test_in_element_false_after_complete_access(self):
        """Test that in_element is False after a complete member access."""
        parser = JavaParser()
        state = parser.parse(None, 'obj.field')
        assert state.in_element is False


class TestJavaParserGenerics:
    """Test generic type parameter identification."""

    def test_simple_generic_type(self):
        """Test simple generic type like List<String>."""
        parser = JavaParser()
        parser.parse(None, 'List<String>')
        tokens = list(parser._tokens)

        generic_start = [t for t in tokens if t.type == TokenType.GENERIC_START]
        generic_end = [t for t in tokens if t.type == TokenType.GENERIC_END]
        generic_type = [t for t in tokens if t.type == TokenType.GENERIC_TYPE]

        assert len(generic_start) == 1
        assert len(generic_end) == 1
        assert len(generic_type) == 1
        assert generic_type[0].value == 'String'

    def test_generic_with_two_type_params(self):
        """Test generic with two type parameters like Map<String, Integer>."""
        parser = JavaParser()
        parser.parse(None, 'Map<String, Integer>')
        tokens = list(parser._tokens)

        generic_type = [t for t in tokens if t.type == TokenType.GENERIC_TYPE]
        assert len(generic_type) == 2
        values = {t.value for t in generic_type}
        assert 'String' in values
        assert 'Integer' in values

    def test_nested_generics(self):
        """Test nested generics like Map<String, List<Integer>>."""
        parser = JavaParser()
        parser.parse(None, 'Map<String, List<Integer>>')
        tokens = list(parser._tokens)

        generic_start = [t for t in tokens if t.type == TokenType.GENERIC_START]
        generic_end = [t for t in tokens if t.type == TokenType.GENERIC_END]

        assert len(generic_start) == 2
        # The lexer tokenises '>>' as a single token, so one GENERIC_END token
        # closes both generic levels.
        assert len(generic_end) == 1
        assert generic_end[0].value == '>>'

    def test_nested_generics_closed_by_double_gt(self):
        """Test that nested generics closed by >> are handled correctly.

        In Java, Map<String, List<Integer>> uses >> which the lexer tokenises
        as a single '>>' operator token. The parser must handle this by closing
        two generic levels when it sees '>>'.
        """
        parser = JavaParser()
        state = parser.parse(None, 'Map<String, List<Integer>>')
        # After parsing, generic state should be fully closed
        assert state.in_generic is False
        assert state.generic_depth == 0

    def test_generic_state_cleared_after_close(self):
        """Test that generic state is cleared after the closing >."""
        parser = JavaParser()
        state = parser.parse(None, 'List<String>')
        assert state.in_generic is False
        assert state.generic_depth == 0

    def test_generic_state_open_across_lines(self):
        """Test that generic state persists across lines when not closed."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, 'Map<String,')
        assert state1.in_generic is True
        assert state1.generic_depth == 1

        parser2 = JavaParser()
        state2 = parser2.parse(state1, 'Integer>')
        assert state2.in_generic is False
        assert state2.generic_depth == 0

    def test_wildcard_generic(self):
        """Test wildcard generic type parameter ?."""
        parser = JavaParser()
        parser.parse(None, 'List<?>')
        tokens = list(parser._tokens)
        generic_start = [t for t in tokens if t.type == TokenType.GENERIC_START]
        assert len(generic_start) == 1

    def test_bounded_type_parameter(self):
        """Test bounded type parameter with extends."""
        parser = JavaParser()
        parser.parse(None, '<T extends Comparable>')
        tokens = list(parser._tokens)

        type_param_tokens = [t for t in tokens if t.type == TokenType.TYPE_PARAMETER]
        assert len(type_param_tokens) == 1
        assert type_param_tokens[0].value == 'T'

    def test_less_than_not_generic(self):
        """Test that < in a comparison expression is not treated as a generic start."""
        parser = JavaParser()
        parser.parse(None, 'a < b')
        tokens = list(parser._tokens)

        # Should be a plain OPERATOR, not GENERIC_START
        generic_start = [t for t in tokens if t.type == TokenType.GENERIC_START]
        assert len(generic_start) == 0

        op_tokens = [t for t in tokens if t.type == TokenType.OPERATOR and t.value == '<']
        assert len(op_tokens) == 1

    def test_generic_method_call(self):
        """Test generic method call like Collections.<String>emptyList()."""
        parser = JavaParser()
        parser.parse(None, 'Collections.<String>emptyList()')
        tokens = list(parser._tokens)
        # emptyList should be FUNCTION_OR_METHOD
        fn_tokens = [t for t in tokens if t.value == 'emptyList']
        assert len(fn_tokens) == 1
        assert fn_tokens[0].type == TokenType.FUNCTION_OR_METHOD


class TestJavaParserMethodReferences:
    """Test method reference tokenisation."""

    def test_method_reference_operator(self):
        """Test that :: is tokenised as METHOD_REFERENCE_OPERATOR."""
        parser = JavaParser()
        parser.parse(None, 'String::new')
        tokens = list(parser._tokens)

        mr_op_tokens = [t for t in tokens if t.type == TokenType.METHOD_REFERENCE_OPERATOR]
        assert len(mr_op_tokens) == 1
        assert mr_op_tokens[0].value == '::'

    def test_method_reference_on_class(self):
        """Test method reference on a class name."""
        parser = JavaParser()
        parser.parse(None, 'String::valueOf')
        tokens = list(parser._tokens)

        mr_op_tokens = [t for t in tokens if t.type == TokenType.METHOD_REFERENCE_OPERATOR]
        assert len(mr_op_tokens) == 1

    def test_method_reference_on_instance(self):
        """Test method reference on an instance via field access."""
        parser = JavaParser()
        parser.parse(None, 'System.out::println')
        tokens = list(parser._tokens)

        mr_op_tokens = [t for t in tokens if t.type == TokenType.METHOD_REFERENCE_OPERATOR]
        assert len(mr_op_tokens) == 1

    def test_method_reference_does_not_mark_lhs_as_element(self):
        """Test that the identifier before :: is not marked as ELEMENT.

        'String' in 'String::new' should remain IDENTIFIER, not ELEMENT,
        because :: is not the same as the . operator.
        """
        parser = JavaParser()
        parser.parse(None, 'String::new')
        tokens = list(parser._tokens)

        string_tokens = [t for t in tokens if t.value == 'String']
        assert len(string_tokens) == 1
        assert string_tokens[0].type == TokenType.IDENTIFIER

    def test_in_element_reset_after_method_reference(self):
        """Test that in_element is reset after :: operator.

        After 'obj.field::method', the identifier after :: should not be
        treated as an element access even though in_element may have been
        set by the preceding dot.
        """
        parser = JavaParser()
        state = parser.parse(None, 'obj.field::')
        # in_element should be False after ::
        assert state.in_element is False


class TestJavaParserImportAndPackage:
    """Test that import and package statements suppress identifier reclassification."""

    def test_import_identifiers_not_reclassified(self):
        """Test that identifiers in import statements stay as IDENTIFIER."""
        parser = JavaParser()
        parser.parse(None, 'import java.util.ArrayList;')
        tokens = list(parser._tokens)

        # None of java, util, ArrayList should be FUNCTION_OR_METHOD or ELEMENT
        # even though they follow dots
        ident_tokens = [t for t in tokens
                        if t.value in ('java', 'util', 'ArrayList')]
        for tok in ident_tokens:
            assert tok.type == TokenType.IDENTIFIER, \
                f"'{tok.value}' in import should stay IDENTIFIER, got {tok.type.name}"

    def test_package_identifiers_not_reclassified(self):
        """Test that identifiers in package statements stay as IDENTIFIER."""
        parser = JavaParser()
        parser.parse(None, 'package com.example.myapp;')
        tokens = list(parser._tokens)

        ident_tokens = [t for t in tokens
                        if t.value in ('com', 'example', 'myapp')]
        for tok in ident_tokens:
            assert tok.type == TokenType.IDENTIFIER, \
                f"'{tok.value}' in package should stay IDENTIFIER, got {tok.type.name}"


class TestJavaParserBlockCommentContinuation:
    """Test parser state for block comment continuation."""

    def test_block_comment_start_sets_continuation(self):
        """Test that an unclosed block comment sets parsing_continuation."""
        parser = JavaParser()
        state = parser.parse(None, '/* start of comment')
        assert state.parsing_continuation is True
        assert state.continuation_state == 1

    def test_block_comment_continuation(self):
        """Test parsing a continuation line of a block comment."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, '/* start')

        parser2 = JavaParser()
        state2 = parser2.parse(state1, ' * middle')
        assert state2.parsing_continuation is True
        assert state2.continuation_state == 1

    def test_block_comment_end_clears_continuation(self):
        """Test that closing a block comment clears parsing_continuation."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, '/* start')

        parser2 = JavaParser()
        state2 = parser2.parse(state1, ' * end */')
        assert state2.parsing_continuation is False
        assert state2.continuation_state == 0

    def test_javadoc_start_sets_continuation(self):
        """Test that an unclosed JavaDoc comment sets parsing_continuation."""
        parser = JavaParser()
        state = parser.parse(None, '/** JavaDoc start')
        assert state.parsing_continuation is True
        assert state.continuation_state == 1

    def test_no_continuation_for_complete_statement(self):
        """Test that a complete statement does not set continuation."""
        parser = JavaParser()
        state = parser.parse(None, 'int x = 42;')
        assert state.parsing_continuation is False
        assert state.continuation_state == 0


class TestJavaParserStatePreservation:
    """Test that parser state is correctly preserved across parse calls."""

    def test_in_element_preserved(self):
        """Test that in_element state is carried to the next parse call."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, 'obj.')
        assert state1.in_element is True

        parser2 = JavaParser()
        state2 = parser2.parse(state1, 'field')
        tokens2 = list(parser2._tokens)
        field_tokens = [t for t in tokens2 if t.value == 'field']
        assert field_tokens[0].type == TokenType.ELEMENT

    def test_in_generic_preserved(self):
        """Test that in_generic state is carried to the next parse call."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, 'Map<String,')
        assert state1.in_generic is True

        parser2 = JavaParser()
        state2 = parser2.parse(state1, 'Integer>')
        assert state2.in_generic is False

    def test_generic_depth_preserved(self):
        """Test that generic_depth is carried to the next parse call."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, 'Map<String,')
        assert state1.generic_depth == 1

        parser2 = JavaParser()
        state2 = parser2.parse(state1, 'List<Integer>')
        # After seeing List<Integer> with depth already 1, depth goes to 2 then back to 1
        assert state2.generic_depth == 1

    def test_lexer_state_preserved(self):
        """Test that lexer state is preserved in parser state."""
        parser = JavaParser()
        state = parser.parse(None, '/* comment')
        assert state.lexer_state is not None
        assert state.lexer_state.in_block_comment is True


class TestJavaParserAnnotations:
    """Test annotation handling in the parser."""

    def test_annotation_token_preserved(self):
        """Test that annotation tokens are passed through unchanged."""
        parser = JavaParser()
        parser.parse(None, '@Override')
        tokens = list(parser._tokens)
        annotation_tokens = [t for t in tokens if t.type == TokenType.ANNOTATION]
        assert len(annotation_tokens) == 1
        assert annotation_tokens[0].value == '@Override'

    def test_annotation_before_method(self):
        """Test annotation before a method declaration."""
        parser = JavaParser()
        parser.parse(None, '@Override public void method()')
        tokens = list(parser._tokens)
        annotation_tokens = [t for t in tokens if t.type == TokenType.ANNOTATION]
        fn_tokens = [t for t in tokens if t.value == 'method']
        assert len(annotation_tokens) == 1
        assert fn_tokens[0].type == TokenType.FUNCTION_OR_METHOD


class TestJavaParserComplexExpressions:
    """Test parsing of complex Java expressions."""

    def test_chained_calls(self):
        """Test chained method calls like builder.setName("x").build()."""
        parser = JavaParser()
        parser.parse(None, 'builder.setName("x").build()')
        tokens = list(parser._tokens)

        set_name_tokens = [t for t in tokens if t.value == 'setName']
        build_tokens = [t for t in tokens if t.value == 'build']

        assert set_name_tokens[0].type == TokenType.FUNCTION_OR_METHOD
        assert build_tokens[0].type == TokenType.FUNCTION_OR_METHOD

    def test_static_method_call(self):
        """Test static method call like System.out.println()."""
        parser = JavaParser()
        parser.parse(None, 'System.out.println("hello")')
        tokens = list(parser._tokens)

        println_tokens = [t for t in tokens if t.value == 'println']
        assert len(println_tokens) == 1
        assert println_tokens[0].type == TokenType.FUNCTION_OR_METHOD

        out_tokens = [t for t in tokens if t.value == 'out']
        assert len(out_tokens) == 1
        assert out_tokens[0].type == TokenType.ELEMENT

    def test_lambda_expression(self):
        """Test lambda expression arrow operator."""
        parser = JavaParser()
        parser.parse(None, 'x -> x * 2')
        tokens = list(parser._tokens)
        arrow_tokens = [t for t in tokens if t.value == '->']
        assert len(arrow_tokens) == 1
        assert arrow_tokens[0].type == TokenType.OPERATOR

    def test_ternary_expression(self):
        """Test ternary expression."""
        parser = JavaParser()
        parser.parse(None, 'x > 0 ? x : -x')
        tokens = list(parser._tokens)
        assert len(tokens) > 0

    def test_instanceof_check(self):
        """Test instanceof expression."""
        parser = JavaParser()
        parser.parse(None, 'obj instanceof String')
        tokens = list(parser._tokens)
        instanceof_tokens = [t for t in tokens if t.value == 'instanceof']
        assert len(instanceof_tokens) == 1
        assert instanceof_tokens[0].type == TokenType.KEYWORD

    def test_array_access_does_not_mark_as_function(self):
        """Test that arr[0] does not mark arr as FUNCTION_OR_METHOD."""
        parser = JavaParser()
        parser.parse(None, 'arr[0]')
        tokens = list(parser._tokens)
        arr_tokens = [t for t in tokens if t.value == 'arr']
        assert len(arr_tokens) == 1
        assert arr_tokens[0].type == TokenType.IDENTIFIER

    def test_cast_expression(self):
        """Test cast expression."""
        parser = JavaParser()
        parser.parse(None, '(String) obj')
        tokens = list(parser._tokens)
        string_tokens = [t for t in tokens if t.value == 'String']
        assert len(string_tokens) == 1
        # String here is used as a type cast, should be IDENTIFIER (not ELEMENT or FUNCTION)
        assert string_tokens[0].type == TokenType.IDENTIFIER

    def test_multiline_method_chain(self):
        """Test a method chain split across lines."""
        parser1 = JavaParser()
        state1 = parser1.parse(None, 'stream')

        parser2 = JavaParser()
        state2 = parser2.parse(state1, '.filter(x -> x > 0)')
        tokens2 = list(parser2._tokens)

        filter_tokens = [t for t in tokens2 if t.value == 'filter']
        assert len(filter_tokens) == 1
        assert filter_tokens[0].type == TokenType.FUNCTION_OR_METHOD
