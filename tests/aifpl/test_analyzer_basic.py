"""Tests for AIFPL Analyzer - Basic functionality.

These tests verify the analyzer skeleton and basic expression analysis
(literals, variables, quote, list construction).
"""

import pytest
from aifpl.aifpl_analyzer import AIFPLAnalyzer
from aifpl.aifpl_analysis_ir import (
    AnalyzedLiteral,
    AnalyzedVariable,
    AnalyzedQuote,
    AnalyzedMakeList,
)
from aifpl.aifpl_value import (
    AIFPLNumber,
    AIFPLString,
    AIFPLBoolean,
    AIFPLSymbol,
    AIFPLList,
)
from aifpl.aifpl_error import AIFPLEvalError


class TestAnalyzerCreation:
    """Test analyzer creation and initialization."""
    
    def test_create_analyzer(self):
        """Test creating an analyzer."""
        analyzer = AIFPLAnalyzer()
        
        assert analyzer.symbol_table is not None
        assert len(analyzer.constants) == 0
        assert len(analyzer.names) == 0
    
    def test_create_with_globals(self):
        """Test creating analyzer with global variables."""
        globals_dict = {
            "pi": AIFPLNumber(3.14159),
            "e": AIFPLNumber(2.71828),
        }
        
        analyzer = AIFPLAnalyzer(globals_dict)
        
        # Globals should be in symbol table
        pi_info = analyzer.symbol_table.resolve("pi")
        e_info = analyzer.symbol_table.resolve("e")
        
        assert pi_info is not None
        assert pi_info.symbol_type == "global"
        assert e_info is not None
        assert e_info.symbol_type == "global"
    
    def test_builtins_in_symbol_table(self):
        """Test that builtins are added to symbol table."""
        analyzer = AIFPLAnalyzer()
        
        # Check a few builtins
        plus_info = analyzer.symbol_table.resolve("+")
        list_info = analyzer.symbol_table.resolve("list")
        
        assert plus_info is not None
        assert plus_info.symbol_type == "builtin"
        assert list_info is not None
        assert list_info.symbol_type == "builtin"


class TestAnalyzeLiterals:
    """Test analyzing literal values."""
    
    def test_analyze_number(self):
        """Test analyzing a number literal."""
        analyzer = AIFPLAnalyzer()
        num = AIFPLNumber(42)
        
        result = analyzer.analyze(num)
        
        assert isinstance(result, AnalyzedLiteral)
        assert result.value == num
        assert result.const_index == 0
        assert result.instruction_count == 1
    
    def test_analyze_string(self):
        """Test analyzing a string literal."""
        analyzer = AIFPLAnalyzer()
        s = AIFPLString("hello")
        
        result = analyzer.analyze(s)
        
        assert isinstance(result, AnalyzedLiteral)
        assert result.value == s
        assert result.const_index == 0
        assert result.instruction_count == 1
    
    def test_analyze_boolean_true(self):
        """Test analyzing boolean true."""
        analyzer = AIFPLAnalyzer()
        b = AIFPLBoolean(True)
        
        result = analyzer.analyze(b)
        
        assert isinstance(result, AnalyzedLiteral)
        assert result.value == b
        assert result.instruction_count == 1
    
    def test_analyze_boolean_false(self):
        """Test analyzing boolean false."""
        analyzer = AIFPLAnalyzer()
        b = AIFPLBoolean(False)
        
        result = analyzer.analyze(b)
        
        assert isinstance(result, AnalyzedLiteral)
        assert result.value == b
        assert result.instruction_count == 1
    
    def test_constants_deduplicated(self):
        """Test that constants are deduplicated in the pool."""
        analyzer = AIFPLAnalyzer()
        
        # Analyze same number twice
        num1 = AIFPLNumber(42)
        num2 = AIFPLNumber(42)
        
        result1 = analyzer.analyze(num1)
        result2 = analyzer.analyze(num2)
        
        # Should have same const_index
        assert result1.const_index == result2.const_index
        assert len(analyzer.constants) == 1


class TestAnalyzeVariables:
    """Test analyzing variable references."""
    
    def test_analyze_global_variable(self):
        """Test analyzing a global variable."""
        globals_dict = {"pi": AIFPLNumber(3.14159)}
        analyzer = AIFPLAnalyzer(globals_dict)
        
        sym = AIFPLSymbol("pi")
        result = analyzer.analyze(sym)
        
        assert isinstance(result, AnalyzedVariable)
        assert result.name == "pi"
        assert result.var_type == "global"
        assert result.instruction_count == 1
    
    def test_analyze_builtin_variable(self):
        """Test analyzing a builtin function reference."""
        analyzer = AIFPLAnalyzer()
        
        sym = AIFPLSymbol("+")
        result = analyzer.analyze(sym)
        
        assert isinstance(result, AnalyzedVariable)
        assert result.name == "+"
        assert result.var_type == "builtin"
        assert result.instruction_count == 1
    
    def test_analyze_unknown_variable(self):
        """Test analyzing an unknown variable (forward reference)."""
        analyzer = AIFPLAnalyzer()
        
        sym = AIFPLSymbol("unknown")
        result = analyzer.analyze(sym)
        
        # Should assume it's a global
        assert isinstance(result, AnalyzedVariable)
        assert result.name == "unknown"
        assert result.var_type == "global"
    
    def test_names_deduplicated(self):
        """Test that names are deduplicated in the pool."""
        analyzer = AIFPLAnalyzer()
        
        # Analyze same variable twice
        sym1 = AIFPLSymbol("x")
        sym2 = AIFPLSymbol("x")
        
        result1 = analyzer.analyze(sym1)
        result2 = analyzer.analyze(sym2)
        
        # Should have same name index
        assert result1.index == result2.index
        assert len(analyzer.names) == 1


class TestAnalyzeQuote:
    """Test analyzing quote expressions."""
    
    def test_analyze_quote_number(self):
        """Test analyzing (quote 42)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            AIFPLNumber(42)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedQuote)
        assert result.quoted_value == AIFPLNumber(42)
        assert result.instruction_count == 1
    
    def test_analyze_quote_symbol(self):
        """Test analyzing (quote x)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            AIFPLSymbol("x")
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedQuote)
        assert isinstance(result.quoted_value, AIFPLSymbol)
        assert result.quoted_value.name == "x"
    
    def test_analyze_quote_list(self):
        """Test analyzing (quote (1 2 3))."""
        analyzer = AIFPLAnalyzer()
        
        quoted_list = AIFPLList((
            AIFPLNumber(1),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            quoted_list
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedQuote)
        assert result.quoted_value == quoted_list
    
    def test_quote_wrong_arg_count(self):
        """Test quote with wrong number of arguments."""
        analyzer = AIFPLAnalyzer()
        
        # No arguments
        expr = AIFPLList((AIFPLSymbol("quote"),))
        
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)
        
        # Too many arguments
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            AIFPLNumber(1),
            AIFPLNumber(2)
        ))
        
        with pytest.raises(AIFPLEvalError, match="wrong number of arguments"):
            analyzer.analyze(expr)


class TestAnalyzeMakeList:
    """Test analyzing list construction."""
    
    def test_analyze_empty_list(self):
        """Test analyzing ()."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList(())
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedLiteral)
        assert result.value == AIFPLList(())
        assert result.instruction_count == 1
    
    def test_analyze_list_construction(self):
        """Test analyzing (list 1 2 3)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("list"),
            AIFPLNumber(1),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedMakeList)
        assert len(result.elements) == 3
        assert all(isinstance(e, AnalyzedLiteral) for e in result.elements)
        # 3 LOAD_CONST + 1 MAKE_LIST = 4 instructions
        assert result.instruction_count == 4
    
    def test_analyze_list_with_variables(self):
        """Test analyzing (list x y z)."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("list"),
            AIFPLSymbol("x"),
            AIFPLSymbol("y"),
            AIFPLSymbol("z")
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedMakeList)
        assert len(result.elements) == 3
        assert all(isinstance(e, AnalyzedVariable) for e in result.elements)
    
    def test_analyze_nested_list(self):
        """Test analyzing (list 1 (list 2 3) 4)."""
        analyzer = AIFPLAnalyzer()
        
        inner_list = AIFPLList((
            AIFPLSymbol("list"),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        
        expr = AIFPLList((
            AIFPLSymbol("list"),
            AIFPLNumber(1),
            inner_list,
            AIFPLNumber(4)
        ))
        
        result = analyzer.analyze(expr)
        
        assert isinstance(result, AnalyzedMakeList)
        assert len(result.elements) == 3
        assert isinstance(result.elements[1], AnalyzedMakeList)


class TestAnalyzerNotImplemented:
    """Test that unimplemented features raise NotImplementedError."""
    
    def test_if_not_implemented(self):
        """Test that 'if' analysis is now implemented."""
        # This test was removed because _analyze_if is now implemented
        # See test_analyzer_if.py for comprehensive if analysis tests
        pass
    
    def test_let_not_implemented(self):
        """Test that 'let' analysis is now implemented."""
        # This test was removed because _analyze_let is now implemented
        # See test_analyzer_let.py for comprehensive let analysis tests
        pass
    
    def test_lambda_not_implemented(self):
        """Test that 'lambda' analysis is not yet implemented."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("lambda"),
            AIFPLList((AIFPLSymbol("x"),)),
            AIFPLSymbol("x")
        ))
        
        with pytest.raises(NotImplementedError, match="_analyze_lambda"):
            analyzer.analyze(expr)
    
    def test_call_not_implemented(self):
        """Test that function call analysis is now implemented."""
        # This test was removed because _analyze_call is now implemented
        # See test_analyzer_calls.py for comprehensive call analysis tests
        pass
    
    def test_and_not_implemented(self):
        """Test that 'and' analysis is not yet implemented."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("and"),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        with pytest.raises(NotImplementedError, match="_analyze_and"):
            analyzer.analyze(expr)
    
    def test_or_not_implemented(self):
        """Test that 'or' analysis is not yet implemented."""
        analyzer = AIFPLAnalyzer()
        
        expr = AIFPLList((
            AIFPLSymbol("or"),
            AIFPLBoolean(True),
            AIFPLBoolean(False)
        ))
        
        with pytest.raises(NotImplementedError, match="_analyze_or"):
            analyzer.analyze(expr)


class TestInstructionCounting:
    """Test that instruction counts are calculated correctly."""
    
    def test_literal_instruction_count(self):
        """Test literal has 1 instruction."""
        analyzer = AIFPLAnalyzer()
        result = analyzer.analyze(AIFPLNumber(42))
        
        assert result.instruction_count == 1
    
    def test_variable_instruction_count(self):
        """Test variable reference has 1 instruction."""
        analyzer = AIFPLAnalyzer()
        result = analyzer.analyze(AIFPLSymbol("x"))
        
        assert result.instruction_count == 1
    
    def test_quote_instruction_count(self):
        """Test quote has 1 instruction."""
        analyzer = AIFPLAnalyzer()
        expr = AIFPLList((
            AIFPLSymbol("quote"),
            AIFPLNumber(42)
        ))
        result = analyzer.analyze(expr)
        
        assert result.instruction_count == 1
    
    def test_list_instruction_count(self):
        """Test list construction instruction count."""
        analyzer = AIFPLAnalyzer()
        
        # (list 1 2 3) = 3 LOAD_CONST + 1 MAKE_LIST = 4
        expr = AIFPLList((
            AIFPLSymbol("list"),
            AIFPLNumber(1),
            AIFPLNumber(2),
            AIFPLNumber(3)
        ))
        result = analyzer.analyze(expr)
        
        assert result.instruction_count == 4
