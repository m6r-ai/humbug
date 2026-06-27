"""ColorTheme enumeration representing the user's full theme selection."""

from enum import Enum, auto


class ColorTheme(Enum):
    """
    The user's chosen colour theme.

    LIGHT, DARK, and SYSTEM select the built-in default palette with the
    corresponding light/dark axis.  COLOR_BLIND selects the fixed
    colour-blind-friendly palette (always dark).  CUSTOM applies user
    overrides on top of the default palette.

    OCEAN_LIGHT selects a fixed light palette with a blue-tinted
    "ocean" aesthetic for a professional look.
    """
    LIGHT = auto()
    DARK = auto()
    SYSTEM = auto()
    COLOR_BLIND = auto()
    OCEAN_LIGHT = auto()
    CUSTOM = auto()
