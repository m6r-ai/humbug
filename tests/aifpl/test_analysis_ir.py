"""Tests for Analysis IR (Intermediate Representation).

These tests verify that IR classes can be constructed and have the expected properties.
"""

import pytest
from aifpl.aifpl_analysis_ir import (
    AnalyzedExpression,
    AnalyzedLiteral,
    AnalyzedVariable,
    AnalyzedIf,
    AnalyzedLet,
    AnalyzedLambda,
    AnalyzedCall,
    AnalyzedAnd,
    AnalyzedOr,
    AnalyzedMatch,
    AnalyzedMatchClause,
    AnalyzedPattern,
    AnalyzedLiteralPattern,
    AnalyzedVariablePattern,
    AnalyzedWildcardPattern,
    AnalyzedTypePattern,
    AnalyzedListPattern,
    AnalyzedConsPattern,
    AnalyzedQuote,
    AnalyzedMakeList,
    dump_analyzed_ir,
)
from aifpl.aifpl_value import AIFPLNumber, AIFPLString, AIFPLBoolean, AIFPLSymbol, AIFPLList
from aifpl.aifpl_dependency_analyzer import AIFPLBindingGroup


class TestAnalyzedLiteral:
    """Test AnalyzedLiteral IR node."""
    
    def test_create_number_literal(self):
        """Test creating analyzed number literal."""
        num = AIFPLNumber(42)
        analyzed = AnalyzedLiteral(
            expr_type='literal',
            source_expr=num,
            value=num,
            const_index=0,
            instruction_count=1
        )
        
        assert analyzed.expr_type == 'literal'
        assert analyzed.value == num
        assert analyzed.const_index == 0
        assert analyzed.instruction_count == 1
    
    def test_create_string_literal(self):
        """Test creating analyzed string literal."""
        s = AIFPLString("hello")
        analyzed = AnalyzedLiteral(
            expr_type='literal',
            source_expr=s,
            value=s,
            const_index=1,
            instruction_count=1
        )
        
        assert analyzed.value == s
        assert analyzed.const_index == 1
    
    def test_create_boolean_literal(self):
        """Test creating analyzed boolean literal."""
        b = AIFPLBoolean(True)
        analyzed = AnalyzedLiteral(
            expr_type='literal',
            source_expr=b,
            value=b,
            const_index=2,
            instruction_count=1
        )
        
        assert analyzed.value == b
        assert analyzed.const_index == 2
    
    def test_repr(self):
        """Test string representation."""
        num = AIFPLNumber(42)
        analyzed = AnalyzedLiteral(
            expr_type='literal',
            source_expr=num,
            value=num,
            const_index=0,
            instruction_count=1
        )
        
        repr_str = repr(analyzed)
        assert "AnalyzedLiteral" in repr_str
        assert "42" in repr_str


class TestAnalyzedVariable:
    """Test AnalyzedVariable IR node."""
    
    def test_create_local_variable(self):
        """Test creating local variable reference."""
        sym = AIFPLSymbol("x")
        analyzed = AnalyzedVariable(
            expr_type='variable',
            source_expr=sym,
            name="x",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        assert analyzed.name == "x"
        assert analyzed.var_type == 'local'
        assert analyzed.depth == 0
        assert analyzed.index == 0
    
    def test_create_global_variable(self):
        """Test creating global variable reference."""
        sym = AIFPLSymbol("pi")
        analyzed = AnalyzedVariable(
            expr_type='variable',
            source_expr=sym,
            name="pi",
            var_type='global',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        assert analyzed.name == "pi"
        assert analyzed.var_type == 'global'
    
    def test_create_builtin_variable(self):
        """Test creating builtin function reference."""
        sym = AIFPLSymbol("+")
        analyzed = AnalyzedVariable(
            expr_type='variable',
            source_expr=sym,
            name="+",
            var_type='builtin',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        assert analyzed.name == "+"
        assert analyzed.var_type == 'builtin'
    
    def test_repr_local(self):
        """Test string representation for local variable."""
        sym = AIFPLSymbol("x")
        analyzed = AnalyzedVariable(
            expr_type='variable',
            source_expr=sym,
            name="x",
            var_type='local',
            depth=1,
            index=2,
            instruction_count=1
        )
        
        repr_str = repr(analyzed)
        assert "x" in repr_str
        assert "local" in repr_str
        assert "[1]" in repr_str
        assert "[2]" in repr_str


class TestAnalyzedIf:
    """Test AnalyzedIf IR node."""
    
    def test_create_if_expression(self):
        """Test creating analyzed if expression."""
        # Create simple condition, then, else
        condition = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("x"),
            name="x",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        then_branch = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(1),
            value=AIFPLNumber(1),
            const_index=0,
            instruction_count=1
        )
        
        else_branch = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(2),
            value=AIFPLNumber(2),
            const_index=1,
            instruction_count=1
        )
        
        analyzed = AnalyzedIf(
            expr_type='if',
            source_expr=AIFPLList(()),
            condition=condition,
            then_branch=then_branch,
            else_branch=else_branch,
            jump_to_else_offset=3,
            jump_past_else_offset=5,
            instruction_count=5
        )
        
        assert analyzed.condition == condition
        assert analyzed.then_branch == then_branch
        assert analyzed.else_branch == else_branch
        assert analyzed.jump_to_else_offset == 3
        assert analyzed.jump_past_else_offset == 5


class TestAnalyzedLet:
    """Test AnalyzedLet IR node."""
    
    def test_create_simple_let(self):
        """Test creating analyzed let expression."""
        # (let ((x 5)) x)
        binding_value = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(5),
            value=AIFPLNumber(5),
            const_index=0,
            instruction_count=1
        )
        
        body = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("x"),
            name="x",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        analyzed = AnalyzedLet(
            expr_type='let',
            source_expr=AIFPLList(()),
            bindings=[("x", binding_value, 0)],
            body=body,
            binding_groups=[],
            recursive_bindings=set(),
            sibling_groups=[],
            instruction_count=3
        )
        
        assert len(analyzed.bindings) == 1
        assert analyzed.bindings[0][0] == "x"
        assert analyzed.bindings[0][2] == 0  # var_index
        assert len(analyzed.recursive_bindings) == 0
    
    def test_create_recursive_let(self):
        """Test creating analyzed let with recursive binding."""
        # (let ((fact (lambda (n) ...))) ...)
        lambda_body = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(1),
            value=AIFPLNumber(1),
            const_index=0,
            instruction_count=1
        )
        
        lambda_value = AnalyzedLambda(
            expr_type='lambda',
            source_expr=AIFPLList(()),
            params=["n"],
            body=lambda_body,
            free_vars=["fact"],
            is_recursive=True,
            recursive_siblings=[],
            code_index=0,
            instruction_count=1
        )
        
        body = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("fact"),
            name="fact",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        analyzed = AnalyzedLet(
            expr_type='let',
            source_expr=AIFPLList(()),
            bindings=[("fact", lambda_value, 0)],
            body=body,
            binding_groups=[],
            recursive_bindings={"fact"},
            sibling_groups=[],
            instruction_count=3
        )
        
        assert "fact" in analyzed.recursive_bindings
        assert analyzed.bindings[0][1].is_recursive


class TestAnalyzedLambda:
    """Test AnalyzedLambda IR node."""
    
    def test_create_simple_lambda(self):
        """Test creating analyzed lambda."""
        # (lambda (x) x)
        body = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("x"),
            name="x",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        analyzed = AnalyzedLambda(
            expr_type='lambda',
            source_expr=AIFPLList(()),
            params=["x"],
            body=body,
            free_vars=[],
            is_recursive=False,
            recursive_siblings=[],
            code_index=0,
            instruction_count=1
        )
        
        assert analyzed.params == ["x"]
        assert len(analyzed.free_vars) == 0
        assert not analyzed.is_recursive
    
    def test_create_closure_lambda(self):
        """Test creating lambda with free variables."""
        # (lambda (x) (+ x y))  where y is free
        body = AnalyzedCall(
            expr_type='call',
            source_expr=AIFPLList(()),
            func=AnalyzedVariable(
                expr_type='variable',
                source_expr=AIFPLSymbol("+"),
                name="+",
                var_type='builtin',
                depth=0,
                index=0,
                instruction_count=1
            ),
            args=[],
            is_tail_call=False,
            is_builtin=True,
            builtin_index=0,
            instruction_count=1
        )
        
        analyzed = AnalyzedLambda(
            expr_type='lambda',
            source_expr=AIFPLList(()),
            params=["x"],
            body=body,
            free_vars=["y"],
            is_recursive=False,
            recursive_siblings=[],
            code_index=0,
            instruction_count=1
        )
        
        assert "y" in analyzed.free_vars
        assert "x" not in analyzed.free_vars


class TestAnalyzedCall:
    """Test AnalyzedCall IR node."""
    
    def test_create_builtin_call(self):
        """Test creating analyzed builtin call."""
        # (+ 1 2)
        func = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("+"),
            name="+",
            var_type='builtin',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        arg1 = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(1),
            value=AIFPLNumber(1),
            const_index=0,
            instruction_count=1
        )
        
        arg2 = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(2),
            value=AIFPLNumber(2),
            const_index=1,
            instruction_count=1
        )
        
        analyzed = AnalyzedCall(
            expr_type='call',
            source_expr=AIFPLList(()),
            func=func,
            args=[arg1, arg2],
            is_tail_call=False,
            is_builtin=True,
            builtin_index=0,
            instruction_count=4
        )
        
        assert analyzed.is_builtin
        assert analyzed.builtin_index == 0
        assert len(analyzed.args) == 2
    
    def test_create_tail_call(self):
        """Test creating analyzed tail call."""
        func = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("f"),
            name="f",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        analyzed = AnalyzedCall(
            expr_type='call',
            source_expr=AIFPLList(()),
            func=func,
            args=[],
            is_tail_call=True,
            is_builtin=False,
            instruction_count=2
        )
        
        assert analyzed.is_tail_call
        assert not analyzed.is_builtin


class TestAnalyzedAndOr:
    """Test AnalyzedAnd and AnalyzedOr IR nodes."""
    
    def test_create_and(self):
        """Test creating analyzed 'and' expression."""
        arg1 = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("x"),
            name="x",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        arg2 = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("y"),
            name="y",
            var_type='local',
            depth=0,
            index=1,
            instruction_count=1
        )
        
        analyzed = AnalyzedAnd(
            expr_type='and',
            source_expr=AIFPLList(()),
            args=[arg1, arg2],
            jump_to_false_offsets=[1, 3],
            jump_to_end_offset=5,
            instruction_count=6
        )
        
        assert len(analyzed.args) == 2
        assert len(analyzed.jump_to_false_offsets) == 2
    
    def test_create_or(self):
        """Test creating analyzed 'or' expression."""
        arg1 = AnalyzedVariable(
            expr_type='variable',
            source_expr=AIFPLSymbol("x"),
            name="x",
            var_type='local',
            depth=0,
            index=0,
            instruction_count=1
        )
        
        analyzed = AnalyzedOr(
            expr_type='or',
            source_expr=AIFPLList(()),
            args=[arg1],
            jump_to_true_offsets=[1],
            jump_to_end_offset=3,
            instruction_count=4
        )
        
        assert len(analyzed.args) == 1


class TestAnalyzedPatterns:
    """Test pattern IR nodes."""
    
    def test_literal_pattern(self):
        """Test literal pattern."""
        pattern = AnalyzedLiteralPattern(
            pattern_type='literal',
            source_expr=AIFPLNumber(42),
            value=AIFPLNumber(42),
            const_index=0,
            instruction_count=3
        )
        
        assert pattern.pattern_type == 'literal'
        assert pattern.const_index == 0
    
    def test_variable_pattern(self):
        """Test variable pattern."""
        pattern = AnalyzedVariablePattern(
            pattern_type='variable',
            source_expr=AIFPLSymbol("x"),
            name="x",
            var_index=0,
            instruction_count=2
        )
        
        assert pattern.name == "x"
        assert pattern.var_index == 0
    
    def test_wildcard_pattern(self):
        """Test wildcard pattern."""
        pattern = AnalyzedWildcardPattern(
            pattern_type='wildcard',
            source_expr=AIFPLSymbol("_"),
            instruction_count=1
        )
        
        assert pattern.pattern_type == 'wildcard'
    
    def test_type_pattern(self):
        """Test type pattern."""
        pattern = AnalyzedTypePattern(
            pattern_type='type',
            source_expr=AIFPLList(()),
            type_predicate='number?',
            var_name="x",
            var_index=0,
            instruction_count=5
        )
        
        assert pattern.type_predicate == 'number?'
        assert pattern.var_name == "x"


class TestDumpIR:
    """Test IR dumping for debugging."""
    
    def test_dump_literal(self):
        """Test dumping literal IR."""
        analyzed = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(42),
            value=AIFPLNumber(42),
            const_index=0,
            instruction_count=1
        )
        
        dump = dump_analyzed_ir(analyzed)
        assert "AnalyzedLiteral" in dump
        assert "42" in dump
    
    def test_dump_if(self):
        """Test dumping if IR."""
        condition = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLBoolean(True),
            value=AIFPLBoolean(True),
            const_index=0,
            instruction_count=1
        )
        
        then_branch = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(1),
            value=AIFPLNumber(1),
            const_index=1,
            instruction_count=1
        )
        
        else_branch = AnalyzedLiteral(
            expr_type='literal',
            source_expr=AIFPLNumber(2),
            value=AIFPLNumber(2),
            const_index=2,
            instruction_count=1
        )
        
        analyzed = AnalyzedIf(
            expr_type='if',
            source_expr=AIFPLList(()),
            condition=condition,
            then_branch=then_branch,
            else_branch=else_branch,
            jump_to_else_offset=3,
            jump_past_else_offset=5,
            instruction_count=5
        )
        
        dump = dump_analyzed_ir(analyzed)
        assert "AnalyzedIf" in dump
        assert "condition:" in dump
        assert "then:" in dump
        assert "else:" in dump
    
    def test_dump_nested_structure(self):
        """Test dumping nested IR structure."""
        # Create nested if: (if x (if y 1 2) 3)
        inner_if = AnalyzedIf(
            expr_type='if',
            source_expr=AIFPLList(()),
            condition=AnalyzedVariable(
                expr_type='variable',
                source_expr=AIFPLSymbol("y"),
                name="y",
                var_type='local',
                depth=0,
                index=1,
                instruction_count=1
            ),
            then_branch=AnalyzedLiteral(
                expr_type='literal',
                source_expr=AIFPLNumber(1),
                value=AIFPLNumber(1),
                const_index=0,
                instruction_count=1
            ),
            else_branch=AnalyzedLiteral(
                expr_type='literal',
                source_expr=AIFPLNumber(2),
                value=AIFPLNumber(2),
                const_index=1,
                instruction_count=1
            ),
            jump_to_else_offset=3,
            jump_past_else_offset=5,
            instruction_count=5
        )
        
        outer_if = AnalyzedIf(
            expr_type='if',
            source_expr=AIFPLList(()),
            condition=AnalyzedVariable(
                expr_type='variable',
                source_expr=AIFPLSymbol("x"),
                name="x",
                var_type='local',
                depth=0,
                index=0,
                instruction_count=1
            ),
            then_branch=inner_if,
            else_branch=AnalyzedLiteral(
                expr_type='literal',
                source_expr=AIFPLNumber(3),
                value=AIFPLNumber(3),
                const_index=2,
                instruction_count=1
            ),
            jump_to_else_offset=7,
            jump_past_else_offset=9,
            instruction_count=9
        )
        
        dump = dump_analyzed_ir(outer_if)
        assert dump.count("AnalyzedIf") == 2
        assert dump.count("condition:") == 2
        assert "x" in dump
        assert "y" in dump
