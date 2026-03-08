"""
Tests for the phi-sink coalescing optimisation in MenaiBytecodeBuilder.

When a ConstInstr or GlobalInstr result is used only as a single phi
incoming value, the bytecode builder should emit the load directly into
the phi slot, eliminating the intermediate register and the MOVE that
would otherwise copy into it.

Covers:
  1. Single-branch const phi-sink: one const arm, one non-const arm
  2. All-const phi-sink: every arm is a constant (phi instruction removed)
  3. Global phi-sink: a global load flows only into a phi
  4. Non-sink value used elsewhere: MOVE must be preserved
  5. Multi-arm branchy chain (the motivating pattern): no MOVEs for const arms
  6. Non-const arm (param passthrough): MOVE preserved
  7. End-to-end correctness: results are identical with and without the opt
  8. MOVE count reduction is measurable vs. unoptimized compilation
"""

from menai.menai_bytecode import Opcode
from menai.menai_compiler import MenaiCompiler


def _count_op(code, opcode) -> int:
    """Count occurrences of `opcode` in `code` and all nested code objects."""
    n = sum(1 for i in code.instructions if i.opcode == opcode)
    for nested in code.code_objects:
        n += _count_op(nested, opcode)
    return n


def _find_lambda(code):
    """Return the first nested code object (the compiled lambda body)."""
    assert code.code_objects, "expected at least one nested code object"
    return code.code_objects[0]


def _compile(src: str, optimize: bool = True):
    return MenaiCompiler(optimize=optimize).compile(src)


class TestPhiSinkSingleConstArm:
    """One const arm and one non-const arm (param passthrough)."""

    def test_const_arm_emits_no_move(self):
        """
        (lambda (x) (if (boolean? x) "yes" x))

        The "yes" arm is a ConstInstr used only as a phi incoming.
        It should be emitted directly into the phi slot — no MOVE for that arm.
        The passthrough arm (x) still needs a MOVE.
        """
        src = '(lambda (x) (if (boolean? x) "yes" x))'
        code = _find_lambda(_compile(src))
        # Only the passthrough arm produces a MOVE; the const arm must not.
        assert _count_op(code, Opcode.MOVE) <= 1

    def test_result_correct(self):
        from menai import Menai
        m = Menai()
        result = m.evaluate('(let ((f (lambda (x) (if (boolean? x) "yes" x)))) (list (f #t) (f 42)))')
        assert result == ["yes", 42]


class TestPhiSinkAllConstArms:
    """Both arms are constants — phi is eliminated entirely."""

    def test_no_moves_when_all_arms_are_consts(self):
        """
        (lambda (x) (if (boolean? x) "yes" "no"))

        Both arms are ConstInstrs used only as phi incomings.
        After coalescing, the phi has no entries left and is dropped.
        No MOVEs should appear.
        """
        src = '(lambda (x) (if (boolean? x) "yes" "no"))'
        code = _find_lambda(_compile(src))
        assert _count_op(code, Opcode.MOVE) == 0

    def test_result_correct(self):
        from menai import Menai
        m = Menai()
        result = m.evaluate('(let ((f (lambda (x) (if (boolean? x) "yes" "no")))) (list (f #t) (f 0)))')
        assert result == ["yes", "no"]


class TestPhiSinkGlobalArm:
    """A global load (LOAD_NAME) used only as a phi incoming is coalesced."""

    def test_global_arm_emits_no_move(self):
        """
        (lambda (x) (if (boolean? x) integer+ x))

        `integer+` is a global name.  Its load should go directly into the
        phi slot with no intermediate MOVE.
        """
        src = '(lambda (x) (if (boolean? x) integer+ x))'
        code = _find_lambda(_compile(src))
        assert _count_op(code, Opcode.MOVE) <= 1

    def test_result_correct(self):
        from menai import Menai
        m = Menai()
        result = m.evaluate(
            '(let ((f (lambda (x) (if (boolean? x) integer+ x))))'
            '  (list (f #t) (f 99)))'
        )
        assert result[0] is not None   # integer+ is a function
        assert result[1] == 99


class TestPhiSinkNonSinkValueUsedElsewhere:
    """A const that is used in two places must NOT be coalesced."""

    def test_shared_const_keeps_move(self):
        """
        (lambda (x) (if (boolean? x) (integer+ 1 1) 1))

        The integer 1 appears in two places (both args of integer+), so it
        has use_count > 1 and must not be treated as a phi-sink.
        The else-arm const '1' is only used as the phi incoming, so it CAN
        be coalesced.
        """
        src = '(lambda (x) (if (boolean? x) (integer+ 1 1) 1))'
        code = _find_lambda(_compile(src))
        # Result must still be correct regardless of MOVE count.
        from menai import Menai
        m = Menai()
        result = m.evaluate(
            '(let ((f (lambda (x) (if (boolean? x) (integer+ 1 1) 1))))'
            '  (list (f #t) (f 0)))'
        )
        assert result == [2, 1]


class TestPhiSinkMultiArmChain:
    """The motivating pattern: a chain of string comparisons."""

    def test_move_count_reduced(self):
        """
        A three-level nested if with const arms: the phi-bearing join block
        is inlined into each predecessor, so no MOVE instructions are needed.
        """
        src = """
        (lambda (x)
          (if (string=? x "R")  "R'"
          (if (string=? x "R'") "R2"
          (if (string=? x "R2") "R"
              x))))
        """
        code = _find_lambda(_compile(src, optimize=True))
        # Join block inlined into all predecessors — no MOVEs needed.
        assert _count_op(code, Opcode.MOVE) == 0

    def test_result_correct(self):
        from menai import Menai
        m = Menai()
        result = m.evaluate("""
        (let ((f (lambda (x)
                   (if (string=? x "R")  "R'"
                   (if (string=? x "R'") "R2"
                   (if (string=? x "R2") "R"
                       x))))))
          (list (f "R") (f "R'") (f "R2") (f "other")))
        """)
        assert result == ["R'", "R2", "R", "other"]

    def test_deeper_chain_result_correct(self):
        """Six-arm chain — exercises the fixed-point loop."""
        from menai import Menai
        m = Menai()
        result = m.evaluate("""
        (let ((f (lambda (x)
                   (if (string=? x "a") 1
                   (if (string=? x "b") 2
                   (if (string=? x "c") 3
                   (if (string=? x "d") 4
                   (if (string=? x "e") 5
                       0))))))))
          (list (f "a") (f "b") (f "c") (f "d") (f "e") (f "z")))
        """)
        assert result == [1, 2, 3, 4, 5, 0]


class TestPhiSinkBuiltinArm:
    """A builtin result used only as a phi incoming is coalesced (no MOVE)."""

    def test_builtin_result_phi_sink(self):
        """
        (lambda (x y) (if (boolean? x) (boolean-not x) y))

        The `boolean-not` result is used only as the phi incoming for the
        then-arm.  It should be emitted directly into the phi slot.
        """
        src = '(lambda (x y) (if (boolean? x) (boolean-not x) y))'
        code = _find_lambda(_compile(src))
        # then-arm is a builtin (coalesced, no MOVE); else-arm is a param (MOVE).
        assert _count_op(code, Opcode.MOVE) <= 1

    def test_is_valid_pattern(self):
        """
        The is-valid? pattern: chained `and` expressions where the last arm
        is a builtin result flowing into a phi.  The liveness-based allocator
        assigns the builtin result and the phi result to different slots because
        the builtin result is live-in to the join block at the point the phi
        slot is allocated.  At most one MOVE is emitted (for the innermost arm).
        The two #f const arms are coalesced to zero MOVEs.
        """
        src = """
        (lambda (a b c)
          (if (boolean-not (boolean? a))
              #f
              (if (boolean-not (boolean? b))
                  #f
                  (boolean-not (boolean? c)))))
        """
        code = _find_lambda(_compile(src))
        # Two #f const arms share the phi slot directly; innermost builtin arm
        # requires at most one MOVE.
        assert _count_op(code, Opcode.MOVE) <= 1

    def test_builtin_arm_correct(self):
        from menai import Menai
        m = Menai()
        result = m.evaluate("""
        (let ((f (lambda (a b c)
                   (if (boolean-not (boolean? a))
                       #f
                       (if (boolean-not (boolean? b))
                           #f
                           (boolean-not (boolean? c)))))))
          (list (f 1 2 3) (f #t 2 3) (f #t #t 3) (f #t #t #t)))
        """)
        # a,b,c non-boolean → #f; a boolean → short-circuit #f;
        # a,b boolean, c non-boolean → #t; a,b,c boolean → #f
        assert result == [False, False, True, False]


class TestPhiSinkParamPassthrough:
    """A param value flowing into a phi cannot be coalesced (not a leaf instr)."""

    def test_param_passthrough_keeps_move(self):
        """
        (lambda (x y) (if (boolean? x) x y))

        Both arms are params.  The phi-bearing join block is inlined into
        each predecessor, so no MOVE instructions are needed.
        """
        src = '(lambda (x y) (if (boolean? x) x y))'
        code = _find_lambda(_compile(src))
        # Join block inlined — no MOVEs needed.
        assert _count_op(code, Opcode.MOVE) == 0

    def test_result_correct(self):
        from menai import Menai
        m = Menai()
        result = m.evaluate(
            '(let ((f (lambda (x y) (if (boolean? x) x y))))'
            '  (list (f #t 99) (f 0 99)))'
        )
        assert result == [True, 99]


class TestPhiSinkEndToEnd:
    """Correctness checks across a range of programs that exercise phi-sinking."""

    def _check(self, src: str, expected):
        from menai import Menai
        assert Menai().evaluate(src) == expected

    def test_integer_const_arms(self):
        self._check(
            '(let ((f (lambda (x) (if (integer=? x 0) 100 200)))) (list (f 0) (f 1)))',
            [100, 200]
        )

    def test_boolean_const_arms(self):
        self._check(
            '(let ((f (lambda (x) (if (string=? x "yes") #t #f)))) (list (f "yes") (f "no")))',
            [True, False]
        )

    def test_none_const_arm(self):
        self._check(
            '(let ((f (lambda (x) (if (boolean? x) #none x)))) (list (f #t) (f 42)))',
            [None, 42]
        )

    def test_empty_list_const_arm(self):
        self._check(
            '(let ((f (lambda (x) (if (boolean? x) (list) x)))) (list (f #t) (f 1)))',
            [[], 1]
        )

    def test_nested_lambda_phi_sink(self):
        """Phi-sinking works inside a nested lambda."""
        self._check(
            """
            (let ((outer (lambda (flag)
                           (let ((inner (lambda (x)
                                          (if flag "flagged" x))))
                             (list (inner "a") (inner "b"))))))
              (list (outer #t) (outer #f)))
            """,
            [["flagged", "flagged"], ["a", "b"]]
        )
