"""Tests for TerminalLine."""

import pytest

from terminal.terminal_line import TerminalLine, TerminalCharacterAttributes


class TestTerminalLineBasics:
    """Basic character storage and retrieval."""

    def test_initial_state_is_spaces(self):
        line = TerminalLine(10)
        for col in range(10):
            char, attrs, fg, bg = line.get_character(col)
            assert char == '\x00' or char == ' ' or ord(char) == 0

    def test_set_and_get_character(self):
        line = TerminalLine(10)
        line.set_character(3, 'A', TerminalCharacterAttributes.NONE, None, None)
        char, attrs, fg, bg = line.get_character(3)
        assert char == 'A'
        assert attrs == TerminalCharacterAttributes.NONE
        assert fg is None
        assert bg is None

    def test_set_character_with_attributes(self):
        line = TerminalLine(10)
        attrs = TerminalCharacterAttributes.BOLD | TerminalCharacterAttributes.UNDERLINE
        line.set_character(0, 'X', attrs, 0xFF0000, 0x0000FF)
        char, got_attrs, fg, bg = line.get_character(0)
        assert char == 'X'
        assert got_attrs == attrs
        assert fg == 0xFF0000
        assert bg == 0x0000FF

    def test_out_of_bounds_get_returns_default(self):
        line = TerminalLine(5)
        char, attrs, fg, bg = line.get_character(10)
        assert char == ' '
        assert attrs == TerminalCharacterAttributes.NONE
        assert fg is None
        assert bg is None

    def test_out_of_bounds_set_is_ignored(self):
        line = TerminalLine(5)
        line.set_character(10, 'Z', TerminalCharacterAttributes.NONE, None, None)
        # Should not raise; nothing written
        char, _, _, _ = line.get_character(4)
        assert char != 'Z'

    def test_width_stored_correctly(self):
        line = TerminalLine(42)
        assert line.width == 42

    def test_overwrite_character(self):
        line = TerminalLine(10)
        line.set_character(5, 'A', TerminalCharacterAttributes.NONE, None, None)
        line.set_character(5, 'B', TerminalCharacterAttributes.BOLD, 0x123456, None)
        char, attrs, fg, _ = line.get_character(5)
        assert char == 'B'
        assert attrs == TerminalCharacterAttributes.BOLD
        assert fg == 0x123456

    def test_all_attribute_flags(self):
        line = TerminalLine(1)
        all_attrs = (
            TerminalCharacterAttributes.BOLD |
            TerminalCharacterAttributes.ITALIC |
            TerminalCharacterAttributes.UNDERLINE |
            TerminalCharacterAttributes.STRIKE |
            TerminalCharacterAttributes.HIDDEN |
            TerminalCharacterAttributes.BLINK |
            TerminalCharacterAttributes.INVERSE |
            TerminalCharacterAttributes.DIM
        )
        line.set_character(0, 'T', all_attrs, None, None)
        _, got_attrs, _, _ = line.get_character(0)
        assert got_attrs == all_attrs

    def test_unicode_character(self):
        line = TerminalLine(5)
        line.set_character(2, '€', TerminalCharacterAttributes.NONE, None, None)
        char, _, _, _ = line.get_character(2)
        assert char == '€'
