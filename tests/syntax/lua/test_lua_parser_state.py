"""
Tests for Lua parser state continuation across lines.
These tests ensure the parser correctly handles state preservation.
"""
import pytest

from syntax.lua.lua_parser import LuaParser


class TestLuaParserStateContinuation:
    """Test parser state continuation across multiple lines."""

    def test_parser_with_previous_state(self):
        """Test parser called with previous parser state."""
        parser1 = LuaParser()
        state1 = parser1.parse(None, 'local x = {')
        
        # Get tokens from first line
        tokens1 = []
        while True:
            token = parser1.get_next_token()
            if token is None:
                break
            tokens1.append(token)
        
        # Parse second line with previous state
        parser2 = LuaParser()
        state2 = parser2.parse(state1, '    a = 1,')
        
        # Get tokens from second line
        tokens2 = []
        while True:
            token = parser2.get_next_token()
            if token is None:
                break
            tokens2.append(token)
        
        # Should have tokens from both lines
        assert len(tokens1) >= 3, "First line should have tokens"
        assert len(tokens2) >= 3, "Second line should have tokens"
        
        # State should be preserved
        assert state2 is not None, "Should return state"

    def test_parser_multiline_table(self):
        """Test parser with table across multiple lines."""
        lines = [
            'local t = {',
            '    x = 1,',
            '    y = 2,',
            '}',
        ]
        
        state = None
        all_tokens = []
        
        for line in lines:
            parser = LuaParser()
            state = parser.parse(state, line)
            
            # Collect tokens from this line
            while True:
                token = parser.get_next_token()
                if token is None:
                    break
                all_tokens.append(token)
        
        # Should have collected tokens from all lines
        assert len(all_tokens) >= 10, "Should have tokens from all lines"
        
        # Check for expected tokens
        keywords = [t for t in all_tokens if t.type.name == 'KEYWORD']
        assert len(keywords) >= 1, "Should have 'local' keyword"

    def test_parser_multiline_function(self):
        """Test parser with function definition across multiple lines."""
        lines = [
            'function test(a, b)',
            '    return a + b',
            'end',
        ]
        
        state = None
        
        for line in lines:
            parser = LuaParser()
            state = parser.parse(state, line)
            
            # Should not crash
            assert state is not None or line == lines[-1], f"State should exist for line: {line}"

    def test_parser_table_access_state(self):
        """Test parser with table access (dot notation) state."""
        lines = [
            'local obj = {}',
            'obj.field = 42',
            'obj.method()',
        ]
        
        state = None
        
        for line in lines:
            parser = LuaParser()
            state = parser.parse(state, line)
            
            tokens = []
            while True:
                token = parser.get_next_token()
                if token is None:
                    break
                tokens.append(token)
            
            # Should have tokens
            assert len(tokens) >= 1, f"Line '{line}' should produce tokens"

    def test_parser_multiline_comment_continuation(self):
        """Test parser with multiline comment state."""
        lines = [
            'local x = 1 --[[comment',
            'continues here',
            'end]]',
            'local y = 2',
        ]
        
        state = None
        
        for i, line in enumerate(lines):
            parser = LuaParser()
            state = parser.parse(state, line)
            
            # Should handle all lines
            assert state is not None or i == len(lines) - 1, f"State should exist for line {i}"

    def test_parser_multiline_string_continuation(self):
        """Test parser with multiline string state."""
        lines = [
            'local s = [[first line',
            'second line',
            'third line]]',
        ]
        
        state = None
        
        for line in lines:
            parser = LuaParser()
            state = parser.parse(state, line)
            
            # Should not crash
            assert state is not None or line == lines[-1]

    def test_parser_empty_previous_state(self):
        """Test parser with None as previous state (first line)."""
        parser = LuaParser()
        state = parser.parse(None, 'local x = 42')
        
        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break
            tokens.append(token)
        
        # Should work normally
        assert len(tokens) >= 4, "Should have tokens"
        assert state is not None, "Should return state"

    def test_parser_state_type_validation(self):
        """Test that parser validates state type."""
        from syntax.lua.lua_parser import LuaParserState
        
        parser1 = LuaParser()
        state1 = parser1.parse(None, 'local x = 1')
        
        # State should be LuaParserState
        assert isinstance(state1, LuaParserState), "Should return LuaParserState"
        
        # Use it in next parser
        parser2 = LuaParser()
        state2 = parser2.parse(state1, 'local y = 2')
        
        # Should work without error
        assert isinstance(state2, LuaParserState), "Should return LuaParserState"

    def test_parser_preserves_table_access_state(self):
        """Test that parser preserves table access state across lines."""
        # Line with table access
        parser1 = LuaParser()
        state1 = parser1.parse(None, 'obj.field')
        
        # Check if state has table access info
        assert hasattr(state1, 'in_table_access'), "State should have in_table_access attribute"
        
        # Continue with next line
        parser2 = LuaParser()
        state2 = parser2.parse(state1, '.another_field')
        
        # Should work
        assert state2 is not None

    def test_parser_complex_multiline_code(self):
        """Test parser with complex multiline code."""
        lines = [
            'function fibonacci(n)',
            '    if n <= 1 then',
            '        return n',
            '    else',
            '        return fibonacci(n-1) + fibonacci(n-2)',
            '    end',
            'end',
        ]
        
        state = None
        total_tokens = 0
        
        for line in lines:
            parser = LuaParser()
            state = parser.parse(state, line)
            
            # Count tokens
            line_tokens = 0
            while True:
                token = parser.get_next_token()
                if token is None:
                    break
                line_tokens += 1
                total_tokens += 1
            
            assert line_tokens >= 1, f"Line '{line}' should produce tokens"
        
        # Should have many tokens from all lines
        assert total_tokens >= 20, "Should have collected many tokens"
