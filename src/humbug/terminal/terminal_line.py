"""Terminal line class implementation."""

import array
from enum import Flag, auto
from typing import Optional, Tuple


class CharacterAttributes(Flag):
    """Bit flags for character attributes."""
    NONE = 0
    BOLD = auto()
    ITALIC = auto()
    UNDERLINE = auto()
    STRIKE = auto()
    HIDDEN = auto()
    BLINK = auto()
    INVERSE = auto()
    DIM = auto()
    CUSTOM_FG = auto()
    CUSTOM_BG = auto()


class TerminalLine:
    """Fixed-width line of terminal characters."""
    def __init__(self, width: int) -> None:
        """Initialize empty line with given width."""
        self.width = width
        # For each character cell we store:
        # - Unicode codepoint (4 bytes)
        # - Attributes flags (4 bytes)
        # - FG color (4 bytes)
        # - BG color (4 bytes)
        self.data = array.array('L', [0] * (width * 4))

    def set_character(
        self,
        index: int,
        char: str,
        attributes: CharacterAttributes,
        fg_color: Optional[int],
        bg_color: Optional[int]
    ) -> None:
        """Set character and attributes at position."""
        if 0 <= index < self.width:
            base = index * 4
            self.data[base] = ord(char)
            self.data[base + 1] = attributes.value
            self.data[base + 2] = fg_color if fg_color is not None else 0
            self.data[base + 3] = bg_color if bg_color is not None else 0

    def get_character(self, index: int) -> Tuple[str, CharacterAttributes, Optional[int], Optional[int]]:
        """Get character and attributes at position."""
        if 0 <= index < self.width:
            base = index * 4
            char = chr(self.data[base])
            attributes = CharacterAttributes(self.data[base + 1])
            fg_color = self.data[base + 2] if self.data[base + 2] != 0 else None
            bg_color = self.data[base + 3] if self.data[base + 3] != 0 else None
            return (char, attributes, fg_color, bg_color)
        return (' ', CharacterAttributes.NONE, None, None)
