"""
Tests for the MAKE_LIST, MAKE_SET, and MAKE_DICT opcodes.

These opcodes are emitted when building a list, set, or dict whose elements
are not all compile-time constants (so the constant folder cannot fold the
entire expression into a single literal).  Each test verifies both that the
correct opcode is present in the compiled bytecode and that the VM produces
the correct result.
"""

import pytest
from menai import Menai
from menai.menai_bytecode import Opcode, unpack_instruction
from menai.menai_compiler import MenaiCompiler


def _opcodes_in(source: str) -> set:
    """Return the set of Opcode values present in the compiled bytecode for source."""
    compiler = MenaiCompiler()
    code = compiler.compile(source)

    def collect(co):
        result = set()
        for word in co.instructions:
            result.add(Opcode(unpack_instruction(word).opcode))
        for child in co.code_objects:
            result |= collect(child)
        return result

    return collect(code)


@pytest.fixture
def menai():
    return Menai()


class TestMakeList:
    """MAKE_LIST is emitted for (list ...) with at least one dynamic element."""

    def test_make_list_opcode_emitted(self):
        """Verify MAKE_LIST appears in the bytecode for a dynamic list."""
        opcodes = _opcodes_in("(lambda (a b c) (list a b c))")
        assert Opcode.MAKE_LIST in opcodes

    def test_make_list_not_emitted_for_constants(self):
        """Constant-only (list ...) is folded — MAKE_LIST must NOT appear."""
        opcodes = _opcodes_in("(list 1 2 3)")
        assert Opcode.MAKE_LIST not in opcodes

    def test_dynamic_list_correct_result(self, menai):
        assert menai.evaluate("(let ((f (lambda (a b c) (list a b c)))) (f 1 2 3))") == [1, 2, 3]

    def test_dynamic_list_single_element(self, menai):
        assert menai.evaluate("(let ((x 42)) (list x))") == [42]

    def test_dynamic_list_mixed_constant_and_dynamic(self, menai):
        assert menai.evaluate("(let ((x 2)) (list 1 x 3))") == [1, 2, 3]

    def test_dynamic_list_nine_elements(self, menai):
        """Exercise the case that motivated the optimisation."""
        result = menai.evaluate("""
            (let ((f (lambda (face)
                       (list
                         (integer* face 1)
                         (integer* face 2)
                         (integer* face 3)
                         (integer* face 4)
                         (integer* face 5)
                         (integer* face 6)
                         (integer* face 7)
                         (integer* face 8)
                         (integer* face 9)))))
              (f 1))
        """)
        assert result == [1, 2, 3, 4, 5, 6, 7, 8, 9]


class TestMakeSet:
    """MAKE_SET is emitted for (set ...) with at least one dynamic element."""

    def test_make_set_opcode_emitted(self):
        """Verify MAKE_SET appears in the bytecode for a dynamic set."""
        opcodes = _opcodes_in("(lambda (a b c) (set a b c))")
        assert Opcode.MAKE_SET in opcodes

    def test_make_set_not_emitted_for_constants(self):
        """Constant-only (set ...) is folded — MAKE_SET must NOT appear."""
        opcodes = _opcodes_in("(set 1 2 3)")
        assert Opcode.MAKE_SET not in opcodes

    def test_dynamic_set_correct_result(self, menai):
        result = menai.evaluate("(let ((f (lambda (a b c) (set a b c)))) (f 10 20 30))")
        assert result == {10, 20, 30}

    def test_dynamic_set_single_element(self, menai):
        result = menai.evaluate("(let ((x 42)) (set x))")
        assert result == {42}

    def test_dynamic_set_deduplicates(self, menai):
        result = menai.evaluate("(let ((x 1)) (set x x 2))")
        assert result == {1, 2}

    def test_dynamic_set_mixed_constant_and_dynamic(self, menai):
        result = menai.evaluate("(let ((x 2)) (set 1 x 3))")
        assert result == {1, 2, 3}


class TestMakeDict:
    """MAKE_DICT is emitted for (dict ...) with at least one dynamic element."""

    def test_make_dict_opcode_emitted(self):
        """Verify MAKE_DICT appears in the bytecode for a dynamic dict."""
        opcodes = _opcodes_in('(lambda (a b) (dict "x" a "y" b))')
        assert Opcode.MAKE_DICT in opcodes

    def test_make_dict_not_emitted_for_constants(self):
        """Constant-only (dict ...) is folded — MAKE_DICT must NOT appear."""
        opcodes = _opcodes_in('(dict "a" 1 "b" 2)')
        assert Opcode.MAKE_DICT not in opcodes

    def test_dynamic_dict_correct_result(self, menai):
        result = menai.evaluate('(let ((f (lambda (a b) (dict "x" a "y" b)))) (f 1 2))')
        assert result == {"x": 1, "y": 2}

    def test_dynamic_dict_single_pair(self, menai):
        result = menai.evaluate('(let ((v 99)) (dict "k" v))')
        assert result == {"k": 99}

    def test_dynamic_dict_mixed_constant_and_dynamic(self, menai):
        result = menai.evaluate('(let ((v 42)) (dict "a" 1 "b" v))')
        assert result == {"a": 1, "b": 42}

    def test_dynamic_dict_dynamic_key(self, menai):
        result = menai.evaluate('(let ((k "name")) (dict k "Alice"))')
        assert result == {"name": "Alice"}
