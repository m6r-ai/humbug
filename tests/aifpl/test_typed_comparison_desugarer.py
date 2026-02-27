"""Desugarer structure tests for variadic type-specific ordered comparisons.

Verifies the AST shape produced by AIFPLDesugarer for:
    integer<?  integer>?  integer<=?  integer>=?
    float<?    float>?    float<=?    float>=?
    string<?   string>?   string<=?   string>=?

The desugarer routes all twelve through _desugar_comparison_chain, which:
  - 2-arg: emits a direct binary call (no let*, no and)
  - 3-arg: let*-binds all three args, builds (if (op t0 t1) (op t1 t2) #f)
  - 4-arg: let*-binds all four args, builds (if (op t0 t1) (if (op t1 t2) (op t2 t3) #f) #f)

Each argument appears in the desugared AST as a temp-variable reference
(i.e. evaluated exactly once by the let* binding).
"""

import pytest

from aifpl.aifpl_ast import (
    AIFPLASTList,
    AIFPLASTNode,
    AIFPLASTSymbol,
    AIFPLASTBoolean,
)
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def parse(source: str) -> AIFPLASTNode:
    tokens = AIFPLLexer().lex(source)
    ast = AIFPLParser().parse(tokens, source)
    return AIFPLSemanticAnalyzer().analyze(ast)


def desugar(source: str) -> AIFPLASTNode:
    return AIFPLDesugarer().desugar(parse(source))


def sym(node: AIFPLASTNode) -> str:
    """Return the leading symbol name of a list node."""
    assert isinstance(node, AIFPLASTList), f"Expected AIFPLASTList, got {type(node)}"
    first = node.first()
    assert isinstance(first, AIFPLASTSymbol), f"Expected AIFPLASTSymbol, got {type(first)}"
    return first.name


def is_temp(node: AIFPLASTNode) -> bool:
    """Return True if node is a temp variable reference (#:match-tmp-N)."""
    return isinstance(node, AIFPLASTSymbol) and node.name.startswith('#:match-tmp-')


# ---------------------------------------------------------------------------
# 2-arg: direct binary call, no wrapping
# ---------------------------------------------------------------------------

ALL_OPS = [
    'integer<?', 'integer>?', 'integer<=?', 'integer>=?',
    'float<?',   'float>?',   'float<=?',   'float>=?',
    'string<?',  'string>?',  'string<=?',  'string>=?',
]

INTEGER_OPS = ['integer<?', 'integer>?', 'integer<=?', 'integer>=?']
FLOAT_OPS   = ['float<?',   'float>?',   'float<=?',   'float>=?']
STRING_OPS  = ['string<?',  'string>?',  'string<=?',  'string>=?']


def _two_arg_expr(op: str) -> str:
    if op.startswith('integer'):
        return f'({op} 1 2)'
    if op.startswith('float'):
        return f'({op} 1.0 2.0)'
    return f'({op} "a" "b")'


def _three_arg_expr(op: str) -> str:
    if op.startswith('integer'):
        return f'({op} 1 2 3)'
    if op.startswith('float'):
        return f'({op} 1.0 2.0 3.0)'
    return f'({op} "a" "b" "c")'


def _four_arg_expr(op: str) -> str:
    if op.startswith('integer'):
        return f'({op} 1 2 3 4)'
    if op.startswith('float'):
        return f'({op} 1.0 2.0 3.0 4.0)'
    return f'({op} "a" "b" "c" "d")'


class TestTwoArgPassThrough:
    """2-arg calls produce a direct binary call node — no let*, no and."""

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_two_arg_is_direct_binary_call(self, op):
        result = desugar(_two_arg_expr(op))
        # Top-level node is the operator itself
        assert sym(result) == op, f"Expected {op!r}, got {sym(result)!r}"
        assert isinstance(result, AIFPLASTList)
        # Exactly (op arg0 arg1)
        assert len(result.elements) == 3, (
            f"Expected 3 elements (op + 2 args), got {len(result.elements)}"
        )

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_two_arg_no_temp_bindings(self, op):
        result = desugar(_two_arg_expr(op))
        # No temp variables — the two arguments are the original literals
        assert sym(result) == op
        for arg in result.elements[1:]:
            assert not is_temp(arg), f"Unexpected temp variable in 2-arg result: {arg}"


# ---------------------------------------------------------------------------
# 3-arg: let*-bound temps + (and (op t0 t1) (op t1 t2))
# ---------------------------------------------------------------------------

def _unwrap_let_star_bindings(node: AIFPLASTNode):
    """
    Peel off a chain of single-binding let* nodes, collecting bound names.

    The desugarer fully reduces let* to let, so we accept both forms.

    Returns (bindings, body) where bindings is a list of (name, value) pairs
    and body is the innermost non-let* expression.
    """
    bindings = []
    while isinstance(node, AIFPLASTList) and sym(node) in ('let', 'let*'):
        # (let* ((name val)) body)
        binding_list = node.elements[1]
        assert isinstance(binding_list, AIFPLASTList)
        # Single-binding let (produced by desugaring let* with one binding)
        assert len(binding_list.elements) == 1
        single = binding_list.elements[0]
        assert isinstance(single, AIFPLASTList) and len(single.elements) == 2
        name_node, val_node = single.elements
        assert isinstance(name_node, AIFPLASTSymbol)
        bindings.append((name_node.name, val_node))
        node = node.elements[2]
    return bindings, node


class TestThreeArgStructure:
    """3-arg calls produce let*-bound temps chained with an if-chain."""

    # After full desugaring, let* is reduced to let; accept either.
    @pytest.mark.parametrize("op", ALL_OPS)
    def test_three_arg_top_level_is_let_star(self, op):
        result = desugar(_three_arg_expr(op))
        assert sym(result) in ('let', 'let*'), (
            f"Expected let/let* at top level for {op!r}, got {sym(result)!r}"
        )

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_three_arg_has_three_temp_bindings(self, op):
        result = desugar(_three_arg_expr(op))
        bindings, body = _unwrap_let_star_bindings(result)
        # Three args → three temp bindings (one per arg)
        assert len(bindings) == 3, (
            f"Expected 3 temp bindings for {op!r}, got {len(bindings)}: {bindings}"
        )
        # All bound names are temp variables
        for name, _ in bindings:
            assert name.startswith('#:match-tmp-'), (
                f"Expected temp name, got {name!r}"
            )

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_three_arg_body_is_if_chain(self, op):
        result = desugar(_three_arg_expr(op))
        _, body = _unwrap_let_star_bindings(result)
        # (and A B) lowered to (if A B #f)
        assert sym(body) == 'if', (
            f"Expected 'if' body for {op!r}, got {sym(body)!r}"
        )
        # else-branch must be #f
        assert isinstance(body.elements[3], AIFPLASTBoolean)
        assert body.elements[3].value is False

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_three_arg_pairs_use_correct_operator(self, op):
        result = desugar(_three_arg_expr(op))
        _, body = _unwrap_let_star_bindings(result)
        # body = (if pair0 pair1 #f)
        pair0 = body.elements[1]
        pair1 = body.elements[2]
        assert sym(pair0) == op, f"pair0 operator: expected {op!r}, got {sym(pair0)!r}"
        assert sym(pair1) == op, f"pair1 operator: expected {op!r}, got {sym(pair1)!r}"

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_three_arg_pairs_share_middle_temp(self, op):
        """The middle argument (t1) appears as the RHS of pair0 and LHS of pair1."""
        result = desugar(_three_arg_expr(op))
        bindings, body = _unwrap_let_star_bindings(result)
        t0_name, t1_name, t2_name = [name for name, _ in bindings]

        # body = (if pair0 pair1 #f)
        pair0 = body.elements[1]   # condition
        pair1 = body.elements[2]   # then-branch

        # pair0 = (op t0 t1)
        assert isinstance(pair0, AIFPLASTList) and len(pair0.elements) == 3
        lhs0 = pair0.elements[1]
        rhs0 = pair0.elements[2]
        assert isinstance(lhs0, AIFPLASTSymbol) and lhs0.name == t0_name
        assert isinstance(rhs0, AIFPLASTSymbol) and rhs0.name == t1_name

        # pair1 = (op t1 t2)
        assert isinstance(pair1, AIFPLASTList) and len(pair1.elements) == 3
        lhs1 = pair1.elements[1]
        rhs1 = pair1.elements[2]
        assert isinstance(lhs1, AIFPLASTSymbol) and lhs1.name == t1_name
        assert isinstance(rhs1, AIFPLASTSymbol) and rhs1.name == t2_name


# ---------------------------------------------------------------------------
# 4-arg: let*-bound temps + (if (op t0 t1) (if (op t1 t2) (op t2 t3) #f) #f)
# ---------------------------------------------------------------------------

class TestFourArgStructure:
    """4-arg calls produce let*-bound temps chained with a nested if-chain."""

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_four_arg_has_four_temp_bindings(self, op):
        result = desugar(_four_arg_expr(op))
        bindings, body = _unwrap_let_star_bindings(result)
        assert len(bindings) == 4, (
            f"Expected 4 temp bindings for {op!r}, got {len(bindings)}"
        )

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_four_arg_body_is_nested_if_chain(self, op):
        result = desugar(_four_arg_expr(op))
        _, body = _unwrap_let_star_bindings(result)
        # (if p0 (if p1 p2 #f) #f)
        assert sym(body) == 'if', f"Expected 'if', got {sym(body)!r}"
        assert isinstance(body.elements[3], AIFPLASTBoolean) and not body.elements[3].value
        inner = body.elements[2]
        assert sym(inner) == 'if', f"Expected inner 'if', got {sym(inner)!r}"
        assert isinstance(inner.elements[3], AIFPLASTBoolean) and not inner.elements[3].value

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_four_arg_all_pairs_use_correct_operator(self, op):
        result = desugar(_four_arg_expr(op))
        _, body = _unwrap_let_star_bindings(result)
        # body = (if p0 (if p1 p2 #f) #f)
        p0 = body.elements[1]
        inner = body.elements[2]
        p1 = inner.elements[1]
        p2 = inner.elements[2]
        for i, pair in enumerate([p0, p1, p2]):
            assert sym(pair) == op, f"pair{i}: expected {op!r}, got {sym(pair)!r}"

    @pytest.mark.parametrize("op", ALL_OPS)
    def test_four_arg_adjacent_pairs_share_temps(self, op):
        """Each consecutive pair shares a temp: t0-t1, t1-t2, t2-t3."""
        result = desugar(_four_arg_expr(op))
        bindings, body = _unwrap_let_star_bindings(result)
        names = [name for name, _ in bindings]

        # body = (if p0 (if p1 p2 #f) #f)
        inner = body.elements[2]
        pairs = [body.elements[1], inner.elements[1], inner.elements[2]]
        for i, pair in enumerate(pairs):
            assert isinstance(pair, AIFPLASTList) and len(pair.elements) == 3
            lhs = pair.elements[1]
            rhs = pair.elements[2]
            assert isinstance(lhs, AIFPLASTSymbol) and lhs.name == names[i], (
                f"pair{i} LHS: expected {names[i]!r}, got {lhs.name!r}"
            )
            assert isinstance(rhs, AIFPLASTSymbol) and rhs.name == names[i + 1], (
                f"pair{i} RHS: expected {names[i+1]!r}, got {rhs.name!r}"
            )


# ---------------------------------------------------------------------------
# No interaction with the existing generic < > <= >= desugaring
# ---------------------------------------------------------------------------

class TestNoInterferenceWithGenericOps:
    """Typed comparison operators desugar independently of each other."""

    def test_integer_lt_two_arg_is_direct(self):
        """integer<? with 2 args produces a direct binary call, no wrapping."""
        result = desugar('(integer<? 1 2)')
        assert sym(result) == 'integer<?'
        assert len(result.elements) == 3

