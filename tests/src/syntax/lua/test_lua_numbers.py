"""
Tests for Lua number tokenization.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer


class TestLuaNumbers:
    """Test Lua number tokenization."""

    def test_decimal_integers(self):
        """Test decimal integer numbers."""
        test_cases = ['0', '42', '123', '999999']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Number '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Number '{num}' should be NUMBER type"

    def test_decimal_floats(self):
        """Test decimal floating point numbers."""
        test_cases = ['3.14', '0.5', '123.456', '.5', '0.0']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Float '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Float '{num}' should be NUMBER type"

    def test_hexadecimal_integers(self):
        """Test hexadecimal integers."""
        test_cases = ['0x0', '0xFF', '0x2A', '0xDEADBEEF', '0xabc123']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Hex '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Hex '{num}' should be NUMBER type"

    def test_hexadecimal_floats(self):
        """Test hexadecimal floating point numbers (Lua 5.2+)."""
        test_cases = ['0x1.fp3', '0x0.8p0', '0x1.ABp2']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Hex float '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Hex float '{num}' should be NUMBER type"

    def test_scientific_notation(self):
        """Test scientific notation numbers."""
        test_cases = ['1e10', '1.5e-3', '0.5e+10', '1E-5']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Scientific '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Scientific '{num}' should be NUMBER type"

    def test_hex_scientific(self):
        """Test hexadecimal scientific notation."""
        test_cases = ['0x1p4', '0x10P2', '0xFFp-3']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"Hex scientific '{num}' should produce one token"
            assert tokens[0].type.name == 'NUMBER', f"Hex scientific '{num}' should be NUMBER type"

    def test_negative_numbers(self):
        """Test negative numbers."""
        test_cases = ['-42', '-3.14', '-0xFF', '-1e10']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert tokens[0].type.name == 'OPERATOR' and tokens[0].value == '-', "First token should be minus operator"
            assert tokens[1].type.name == 'NUMBER', f"Number should be NUMBER type"

    def test_leading_zeros(self):
        """Test numbers with leading zeros."""
        test_cases = ['007', '000123', '0.000', '0x00FF']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert tokens[0].type.name == 'NUMBER', f"Number with leading zeros should be NUMBER"

    def test_underscore_numbers(self):
        """Test that underscores are not part of numbers."""
        lexer = LuaLexer()
        lexer.lex(None, '1_000')  # Not valid in Lua

        tokens = list(lexer._tokens)
        assert len(tokens) >= 2, "Should split into multiple tokens"

    def test_large_numbers(self):
        """Test large numbers."""
        test_cases = ['999999999', '1.7976931348623157e308']
        for num in test_cases:
            lexer = LuaLexer()
            lexer.lex(None, num)

            tokens = list(lexer._tokens)
            assert tokens[0].type.name == 'NUMBER', f"Large number should be NUMBER"
