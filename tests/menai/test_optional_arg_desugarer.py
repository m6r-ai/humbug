"""Tests for desugaring of builtins with optional arguments.

Several builtins have optional trailing arguments whose opcodes always require
the full argument count.  The desugarer synthesises the missing default so the
$-prefixed opcode call is always fully saturated:

  Constant defaults (1-arg min, 2-arg opcode):
    (integer->complex n)   → ($integer->complex n 0)
    (integer->string  n)   → ($integer->string  n 10)
    (float->complex   x)   → ($float->complex   x 0.0)
    (string->integer  s)   → ($string->integer  s 10)
    (string->list     s)   → ($string->list     s "")
    (list->string     l)   → ($list->string     l "")

  Constant defaults (2-arg min, 3-arg opcode):
    (dict-get  d k)        → ($dict-get   d k #none)
    (range start end)      → ($range      start end 1)

  Computed defaults (2-arg min, 3-arg opcode):
    (string-slice s start) → (let ((#:t s)) ($string-slice #:t start ($string-length #:t)))
    (list-slice   l start) → (let ((#:t l)) ($list-slice   #:t start ($list-length   #:t)))

Tests are organised into three layers:
  1. Desugarer structure — verify the AST shape produced by the desugarer.
  2. End-to-end evaluation — verify correct results via Menai.evaluate.
  3. Error cases — verify that ill-formed calls are rejected appropriately.
"""

import pytest

from menai import Menai, MenaiEvalError
from menai.menai_ast import (
    MenaiASTFloat,
    MenaiASTInteger,
    MenaiASTList,
    MenaiASTNone,
    MenaiASTNode,
    MenaiASTString,
    MenaiASTSymbol,
)
from menai.menai_ast_builder import MenaiASTBuilder
from menai.menai_ast_desugarer import MenaiASTDesugarer
from menai.menai_ast_semantic_analyzer import MenaiASTSemanticAnalyzer
from menai.menai_lexer import MenaiLexer


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def parse(source: str) -> MenaiASTNode:
    """Lex, parse, and semantically-analyse *source*, returning the AST."""
    tokens = MenaiLexer().lex(source)
    ast = MenaiASTBuilder().build(tokens, source)
    return MenaiASTSemanticAnalyzer().analyze(ast)


def desugar(source: str) -> MenaiASTNode:
    """Parse and desugar *source*, returning the desugared AST."""
    return MenaiASTDesugarer().desugar(parse(source))


def op_name(node: MenaiASTNode) -> str:
    """Return the operator name of a list node."""
    assert isinstance(node, MenaiASTList)
    first = node.first()
    assert isinstance(first, MenaiASTSymbol)
    return first.name


# ---------------------------------------------------------------------------
# 1. Desugarer structure tests
# ---------------------------------------------------------------------------

class TestDictGetDesugarerStructure:
    """Verify the AST shape produced for dict-get calls."""

    def test_two_arg_becomes_dollar_dict_get_with_none_default(self):
        """(dict-get d k) → ($dict-get d k #none)"""
        result = desugar('(dict-get (dict) "key")')
        assert op_name(result) == '$dict-get'
        assert isinstance(result, MenaiASTList)
        assert len(result.elements) == 4
        assert isinstance(result.elements[3], MenaiASTNone)

    def test_three_arg_becomes_dollar_dict_get_with_supplied_default(self):
        """(dict-get d k defval) → ($dict-get d k defval)"""
        result = desugar('(dict-get (dict) "key" 42)')
        assert op_name(result) == '$dict-get'
        assert isinstance(result, MenaiASTList)
        assert len(result.elements) == 4
        assert isinstance(result.elements[3], MenaiASTInteger)
        assert result.elements[3].value == 42

    def test_synthesised_none_carries_source_location(self):
        """The synthesised #none default inherits the call-site source location."""
        result = desugar('(dict-get (dict) "key")')
        assert isinstance(result, MenaiASTList)
        default_node = result.elements[3]
        assert isinstance(default_node, MenaiASTNone)
        assert default_node.line >= 0
        assert default_node.column >= 0


class TestRangeDesugarerStructure:
    """Verify the AST shape produced for range calls."""

    def test_two_arg_becomes_dollar_range_with_step_one(self):
        """(range start end) → ($range start end 1)"""
        result = desugar('(range 0 10)')
        assert op_name(result) == '$range'
        assert isinstance(result, MenaiASTList)
        assert len(result.elements) == 4
        assert isinstance(result.elements[3], MenaiASTInteger)
        assert result.elements[3].value == 1

    def test_three_arg_becomes_dollar_range_with_supplied_step(self):
        """(range start end step) → ($range start end step)"""
        result = desugar('(range 0 10 2)')
        assert op_name(result) == '$range'
        assert isinstance(result, MenaiASTList)
        assert len(result.elements) == 4
        assert isinstance(result.elements[3], MenaiASTInteger)
        assert result.elements[3].value == 2

    def test_synthesised_step_carries_source_location(self):
        """The synthesised step=1 inherits the call-site source location."""
        result = desugar('(range 0 5)')
        assert isinstance(result, MenaiASTList)
        step_node = result.elements[3]
        assert isinstance(step_node, MenaiASTInteger)
        assert step_node.line >= 0
        assert step_node.column >= 0


class TestIntegerToComplexDesugarerStructure:
    """Verify the AST shape produced for integer->complex calls."""

    def test_one_arg_becomes_dollar_form_with_zero_imag(self):
        """(integer->complex n) → ($integer->complex n 0)"""
        result = desugar('(integer->complex 3)')
        assert op_name(result) == '$integer->complex'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTInteger)
        assert result.elements[2].value == 0

    def test_two_arg_passes_through(self):
        """(integer->complex n imag) → ($integer->complex n imag)"""
        result = desugar('(integer->complex 3 4)')
        assert op_name(result) == '$integer->complex'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTInteger)
        assert result.elements[2].value == 4


class TestIntegerToStringDesugarerStructure:
    """Verify the AST shape produced for integer->string calls."""

    def test_one_arg_becomes_dollar_form_with_base_ten(self):
        """(integer->string n) → ($integer->string n 10)"""
        result = desugar('(integer->string 255)')
        assert op_name(result) == '$integer->string'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTInteger)
        assert result.elements[2].value == 10

    def test_two_arg_passes_through(self):
        """(integer->string n base) → ($integer->string n base)"""
        result = desugar('(integer->string 255 16)')
        assert op_name(result) == '$integer->string'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTInteger)
        assert result.elements[2].value == 16


class TestFloatToComplexDesugarerStructure:
    """Verify the AST shape produced for float->complex calls."""

    def test_one_arg_becomes_dollar_form_with_zero_imag(self):
        """(float->complex x) → ($float->complex x 0.0)"""
        result = desugar('(float->complex 1.5)')
        assert op_name(result) == '$float->complex'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTFloat)
        assert result.elements[2].value == 0.0

    def test_two_arg_passes_through(self):
        """(float->complex x y) → ($float->complex x y)"""
        result = desugar('(float->complex 1.5 2.5)')
        assert op_name(result) == '$float->complex'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTFloat)
        assert result.elements[2].value == 2.5


class TestStringToIntegerDesugarerStructure:
    """Verify the AST shape produced for string->integer calls."""

    def test_one_arg_becomes_dollar_form_with_base_ten(self):
        """(string->integer s) → ($string->integer s 10)"""
        result = desugar('(string->integer "42")')
        assert op_name(result) == '$string->integer'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTInteger)
        assert result.elements[2].value == 10

    def test_two_arg_passes_through(self):
        """(string->integer s base) → ($string->integer s base)"""
        result = desugar('(string->integer "ff" 16)')
        assert op_name(result) == '$string->integer'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTInteger)
        assert result.elements[2].value == 16


class TestStringToListDesugarerStructure:
    """Verify the AST shape produced for string->list calls."""

    def test_one_arg_becomes_dollar_form_with_empty_sep(self):
        """(string->list s) → ($string->list s "")"""
        result = desugar('(string->list "hello")')
        assert op_name(result) == '$string->list'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTString)
        assert result.elements[2].value == ""

    def test_two_arg_passes_through(self):
        """(string->list s sep) → ($string->list s sep)"""
        result = desugar('(string->list "a,b,c" ",")')
        assert op_name(result) == '$string->list'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTString)
        assert result.elements[2].value == ","


class TestListToStringDesugarerStructure:
    """Verify the AST shape produced for list->string calls."""

    def test_one_arg_becomes_dollar_form_with_empty_sep(self):
        """(list->string l) → ($list->string l "")"""
        result = desugar('(list->string (list "a" "b"))')
        assert op_name(result) == '$list->string'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTString)
        assert result.elements[2].value == ""

    def test_two_arg_passes_through(self):
        """(list->string l sep) → ($list->string l sep)"""
        result = desugar('(list->string (list "a" "b") ", ")')
        assert op_name(result) == '$list->string'
        assert len(result.elements) == 3
        assert isinstance(result.elements[2], MenaiASTString)
        assert result.elements[2].value == ", "


class TestStringSliceDesugarerStructure:
    """Verify the AST shape produced for string-slice calls."""

    def test_two_arg_wraps_in_let_with_length_default(self):
        """(string-slice s start) → (let ((#:t s)) ($string-slice #:t start ($string-length #:t)))"""
        result = desugar('(string-slice "hello" 1)')
        # Outer form is a let
        assert op_name(result) == 'let'
        assert isinstance(result, MenaiASTList)
        bindings = result.elements[1]
        assert isinstance(bindings, MenaiASTList)
        assert len(bindings.elements) == 1
        # Binding value is the desugared collection expression
        binding = bindings.elements[0]
        assert isinstance(binding, MenaiASTList)
        temp_name = binding.elements[0].name
        assert temp_name.startswith('#:match-tmp-')
        # Body is ($string-slice temp start ($string-length temp))
        body = result.elements[2]
        assert op_name(body) == '$string-slice'
        assert len(body.elements) == 4
        # First arg is the temp
        assert body.elements[1].name == temp_name
        # Third arg is ($string-length temp)
        end_expr = body.elements[3]
        assert op_name(end_expr) == '$string-length'
        assert end_expr.elements[1].name == temp_name

    def test_three_arg_becomes_dollar_string_slice(self):
        """(string-slice s start end) → ($string-slice s start end)"""
        result = desugar('(string-slice "hello" 1 3)')
        assert op_name(result) == '$string-slice'
        assert len(result.elements) == 4

    def test_collection_not_double_evaluated(self):
        """The collection expression appears only once in the desugared output."""
        result = desugar('(string-slice "hello" 1)')
        # The string literal "hello" should appear exactly once (in the let binding),
        # not also inside the body.
        assert op_name(result) == 'let'
        body = result.elements[2]
        # body is ($string-slice temp ...) — temp is a symbol, not the literal
        assert isinstance(body.elements[1], MenaiASTSymbol)


class TestListSliceDesugarerStructure:
    """Verify the AST shape produced for list-slice calls."""

    def test_two_arg_wraps_in_let_with_length_default(self):
        """(list-slice l start) → (let ((#:t l)) ($list-slice #:t start ($list-length #:t)))"""
        result = desugar('(list-slice (list 1 2 3) 1)')
        assert op_name(result) == 'let'
        assert isinstance(result, MenaiASTList)
        bindings = result.elements[1]
        binding = bindings.elements[0]
        temp_name = binding.elements[0].name
        assert temp_name.startswith('#:match-tmp-')
        body = result.elements[2]
        assert op_name(body) == '$list-slice'
        assert len(body.elements) == 4
        assert body.elements[1].name == temp_name
        end_expr = body.elements[3]
        assert op_name(end_expr) == '$list-length'
        assert end_expr.elements[1].name == temp_name

    def test_three_arg_becomes_dollar_list_slice(self):
        """(list-slice l start end) → ($list-slice l start end)"""
        result = desugar('(list-slice (list 1 2 3) 1 2)')
        assert op_name(result) == '$list-slice'
        assert len(result.elements) == 4

    def test_collection_not_double_evaluated(self):
        """The collection expression appears only once in the desugared output."""
        result = desugar('(list-slice (list 1 2 3) 1)')
        assert op_name(result) == 'let'
        body = result.elements[2]
        assert isinstance(body.elements[1], MenaiASTSymbol)


# ---------------------------------------------------------------------------
# 2. End-to-end evaluation tests
# ---------------------------------------------------------------------------

@pytest.fixture
def menai():
    return Menai()


class TestDictGetEval:
    """dict-get evaluated end-to-end, covering both 2- and 3-argument forms."""

    def test_two_arg_key_found(self, menai):
        result = menai.evaluate('(dict-get (dict (list "a" 1) (list "b" 2)) "a")')
        assert result == 1

    def test_two_arg_key_missing_returns_none(self, menai):
        result = menai.evaluate('(dict-get (dict (list "a" 1)) "missing")')
        assert result is None

    def test_three_arg_key_found(self, menai):
        result = menai.evaluate('(dict-get (dict (list "a" 1)) "a" 99)')
        assert result == 1

    def test_three_arg_key_missing_returns_default(self, menai):
        result = menai.evaluate('(dict-get (dict (list "a" 1)) "missing" 99)')
        assert result == 99

    def test_two_arg_none_default_usable_as_sentinel(self, menai):
        result = menai.evaluate('(none? (dict-get (dict (list "a" 1)) "missing"))')
        assert result is True

    def test_two_arg_in_nested_lookup(self, menai):
        result = menai.evaluate(
            '(let ((outer (dict (list "inner" (dict (list "x" 42))))))'
            '  (dict-get (dict-get outer "inner") "x"))'
        )
        assert result == 42


class TestRangeEval:
    """range evaluated end-to-end, covering both 2- and 3-argument forms."""

    def test_two_arg_basic(self, menai):
        assert menai.evaluate('(range 0 5)') == [0, 1, 2, 3, 4]

    def test_two_arg_empty(self, menai):
        assert menai.evaluate('(range 3 3)') == []

    def test_two_arg_single_element(self, menai):
        assert menai.evaluate('(range 7 8)') == [7]

    def test_three_arg_step_two(self, menai):
        assert menai.evaluate('(range 0 10 2)') == [0, 2, 4, 6, 8]

    def test_three_arg_negative_step(self, menai):
        assert menai.evaluate('(range 5 0 -1)') == [5, 4, 3, 2, 1]

    def test_two_arg_length_matches_end_minus_start(self, menai):
        assert menai.evaluate('(list-length (range 0 10))') == 10


class TestIntegerToComplexEval:
    """integer->complex evaluated end-to-end."""

    def test_one_arg_real_only(self, menai):
        result = menai.evaluate_and_format('(integer->complex 3)')
        assert result == '3+0j'

    def test_two_arg_real_and_imag(self, menai):
        result = menai.evaluate_and_format('(integer->complex 3 4)')
        assert result == '3+4j'


class TestIntegerToStringEval:
    """integer->string evaluated end-to-end."""

    def test_one_arg_decimal(self, menai):
        assert menai.evaluate('(integer->string 42)') == '42'

    def test_two_arg_hex(self, menai):
        assert menai.evaluate('(integer->string 255 16)') == 'ff'

    def test_two_arg_binary(self, menai):
        assert menai.evaluate('(integer->string 10 2)') == '1010'


class TestFloatToComplexEval:
    """float->complex evaluated end-to-end."""

    def test_one_arg_real_only(self, menai):
        result = menai.evaluate_and_format('(float->complex 1.5)')
        assert result == '1.5+0j'

    def test_two_arg_real_and_imag(self, menai):
        result = menai.evaluate_and_format('(float->complex 1.5 2.5)')
        assert result == '1.5+2.5j'


class TestStringToIntegerEval:
    """string->integer evaluated end-to-end."""

    def test_one_arg_decimal(self, menai):
        assert menai.evaluate('(string->integer "42")') == 42

    def test_two_arg_hex(self, menai):
        assert menai.evaluate('(string->integer "ff" 16)') == 255

    def test_two_arg_binary(self, menai):
        assert menai.evaluate('(string->integer "1010" 2)') == 10


class TestStringToListEval:
    """string->list evaluated end-to-end."""

    def test_one_arg_splits_to_chars(self, menai):
        assert menai.evaluate('(string->list "abc")') == ['a', 'b', 'c']

    def test_two_arg_splits_on_separator(self, menai):
        assert menai.evaluate('(string->list "a,b,c" ",")') == ['a', 'b', 'c']

    def test_one_arg_empty_string(self, menai):
        assert menai.evaluate('(string->list "")') == []


class TestListToStringEval:
    """list->string evaluated end-to-end."""

    def test_one_arg_no_separator(self, menai):
        assert menai.evaluate('(list->string (list "a" "b" "c"))') == 'abc'

    def test_two_arg_with_separator(self, menai):
        assert menai.evaluate('(list->string (list "a" "b" "c") ", ")') == 'a, b, c'

    def test_one_arg_empty_list(self, menai):
        assert menai.evaluate('(list->string (list))') == ''


class TestStringSliceEval:
    """string-slice evaluated end-to-end."""

    def test_two_arg_slices_to_end(self, menai):
        assert menai.evaluate('(string-slice "hello" 1)') == 'ello'

    def test_two_arg_from_zero(self, menai):
        assert menai.evaluate('(string-slice "hello" 0)') == 'hello'

    def test_two_arg_from_end(self, menai):
        assert menai.evaluate('(string-slice "hello" 5)') == ''

    def test_three_arg_explicit_end(self, menai):
        assert menai.evaluate('(string-slice "hello" 1 3)') == 'el'

    def test_two_arg_result_length(self, menai):
        result = menai.evaluate('(string-length (string-slice "hello" 2))')
        assert result == 3


class TestListSliceEval:
    """list-slice evaluated end-to-end."""

    def test_two_arg_slices_to_end(self, menai):
        assert menai.evaluate('(list-slice (list 1 2 3 4 5) 2)') == [3, 4, 5]

    def test_two_arg_from_zero(self, menai):
        assert menai.evaluate('(list-slice (list 1 2 3) 0)') == [1, 2, 3]

    def test_two_arg_from_end(self, menai):
        assert menai.evaluate('(list-slice (list 1 2 3) 3)') == []

    def test_three_arg_explicit_end(self, menai):
        assert menai.evaluate('(list-slice (list 1 2 3 4 5) 1 3)') == [2, 3]

    def test_two_arg_result_length(self, menai):
        result = menai.evaluate('(list-length (list-slice (list 1 2 3 4 5) 2))')
        assert result == 3


# ---------------------------------------------------------------------------
# 3. Error cases
# ---------------------------------------------------------------------------

class TestDictGetErrors:
    def test_too_few_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(dict-get (dict (list "a" 1)))')

    def test_too_many_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(dict-get (dict (list "a" 1)) "a" 0 "extra")')

    def test_non_dict_first_arg_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="requires dict argument"):
            menai.evaluate('(dict-get (list 1 2 3) "key")')


class TestRangeErrors:
    def test_too_few_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(range 5)')

    def test_too_many_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(range 0 10 2 99)')

    def test_zero_step_raises(self, menai):
        with pytest.raises(MenaiEvalError, match="[Ss]tep cannot be zero"):
            menai.evaluate('(range 0 10 0)')


class TestIntegerToComplexErrors:
    def test_too_few_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer->complex)')

    def test_too_many_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer->complex 1 2 3)')


class TestIntegerToStringErrors:
    def test_too_few_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer->string)')

    def test_too_many_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(integer->string 42 10 "extra")')


class TestStringSliceErrors:
    def test_too_few_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-slice "hello")')

    def test_too_many_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(string-slice "hello" 1 3 99)')


class TestListSliceErrors:
    def test_too_few_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(list-slice (list 1 2 3))')

    def test_too_many_args_rejected(self, menai):
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(list-slice (list 1 2 3) 1 2 99)')
