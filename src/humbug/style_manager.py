"""Style manager for handling application-wide style and zoom settings.

Implements a singleton pattern to maintain consistent styling across components.
Provides signals for style changes and utilities for scaled size calculations.
"""

from enum import Enum, auto
import os
from pathlib import Path
from typing import Dict, List

from PySide6.QtCore import QObject, Signal, QOperatingSystemVersion, Qt
from PySide6.QtGui import QTextCharFormat, QFontDatabase, QColor, QFontMetricsF, QFont, QPixmap

from syntax import TokenType

from humbug.color_role import ColorRole


class ColorMode(Enum):
    """Enumeration for color theme modes."""
    LIGHT = auto()
    DARK = auto()


class StyleManager(QObject):
    """
    Singleton manager for application-wide style settings.

    Handles zoom factor management and style updates across the application.
    Emits signals when zoom level changes to notify dependent components.

    Attributes:
        style_changed (Signal): Emitted when style changes, passing new zoom factor
        _instance (StyleManager): Singleton instance
        _zoom_factor (float): Current zoom scaling factor
        _initialized (bool): Tracks initialization state of QObject base
    """

    style_changed = Signal()
    _instance = None

    def __new__(cls) -> 'StyleManager':
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(StyleManager, cls).__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialize QObject base class if not already done."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._zoom_factor = 1.0
            self._base_font_size = self._determine_base_font_size()
            self._user_font_size: float | None = None
            self._initialized = True
            self._color_mode = ColorMode.DARK  # Default to dark mode
            self._colors: Dict[ColorRole, Dict[ColorMode, str]] = self._initialize_colors()
            self._highlights: Dict[TokenType, QTextCharFormat] = {}
            self._proportional_highlights: Dict[TokenType, QTextCharFormat] = {}

            self._code_font_families = ["Menlo", "Consolas", "Monaco", "monospace"]
            self._initialize_highlights()
            self._initialize_proportional_highlights()
            self._create_theme_icons()

    def _initialize_colors(self) -> Dict[ColorRole, Dict[ColorMode, str]]:
        """Initialize the application colours for both light and dark modes."""
        return {
            # Background colours
            ColorRole.BACKGROUND_PRIMARY: {
                ColorMode.DARK: "#060606",
                ColorMode.LIGHT: "#fcfcfc"
            },
            ColorRole.BACKGROUND_SECONDARY: {
                ColorMode.DARK: "#141414",
                ColorMode.LIGHT: "#ececec"
            },
            ColorRole.BACKGROUND_TERTIARY: {
                ColorMode.DARK: "#080808",
                ColorMode.LIGHT: "#f8f8f8"
            },
            ColorRole.BACKGROUND_TERTIARY_HOVER: {
                ColorMode.DARK: "#303030",
                ColorMode.LIGHT: "#e0e0e0"
            },
            ColorRole.BACKGROUND_TERTIARY_PRESSED: {
                ColorMode.DARK: "#505050",
                ColorMode.LIGHT: "#c8c8c8"
            },
            ColorRole.BACKGROUND_DIALOG: {
                ColorMode.DARK: "#282828",
                ColorMode.LIGHT: "#d0d0d0"
            },

            # Text colours
            ColorRole.TEXT_PRIMARY: {
                ColorMode.DARK: "#d8d8d8",
                ColorMode.LIGHT: "#202020"
            },
            ColorRole.TEXT_BRIGHT: {
                ColorMode.DARK: "#ffffff",
                ColorMode.LIGHT: "#000000"
            },
            ColorRole.TEXT_HEADING: {
                ColorMode.DARK: "#ffe0a0",
                ColorMode.LIGHT: "#204080"
            },
            ColorRole.TEXT_HEADING_BRIGHT: {
                ColorMode.DARK: "#ffe8a0",
                ColorMode.LIGHT: "#203880"
            },
            ColorRole.TEXT_DISABLED: {
                ColorMode.DARK: "#707070",
                ColorMode.LIGHT: "#909090"
            },
            ColorRole.TEXT_SELECTED: {
                ColorMode.DARK: "#404058",
                ColorMode.LIGHT: "#c8c8dc"
            },
            ColorRole.TEXT_FOUND: {
                ColorMode.DARK: "#885050",
                ColorMode.LIGHT: "#e0b4b4"
            },
            ColorRole.TEXT_FOUND_DIM: {
                ColorMode.DARK: "#583838",
                ColorMode.LIGHT: "#f4d8d8"
            },
            ColorRole.TEXT_RECOMMENDED: {
                ColorMode.DARK: "#ffffff",
                ColorMode.LIGHT: "#ffffff"
            },
            ColorRole.TEXT_LINK: {
                ColorMode.DARK: "#80a0ff",
                ColorMode.LIGHT: "#0000ff"
            },
            ColorRole.TEXT_INACTIVE: {
                ColorMode.DARK: "#909090",
                ColorMode.LIGHT: "#707070"
            },
            ColorRole.TEXT_EPHEMERAL: {
                ColorMode.DARK: "#e0a080",
                ColorMode.LIGHT: "#a0785c"
            },
            ColorRole.TEXT_EPHEMERAL_INACTIVE: {
                ColorMode.DARK: "#a06040",
                ColorMode.LIGHT: "#c09070"
            },
            ColorRole.TEXT_ERROR: {
                ColorMode.DARK: "#e03020",
                ColorMode.LIGHT: "#f04030"
            },
            ColorRole.TEXT_ERROR_INACTIVE: {
                ColorMode.DARK: "#e06050",
                ColorMode.LIGHT: "#f07060"
            },

            # Edit box colours
            ColorRole.EDIT_BOX_BORDER: {
                ColorMode.DARK: "#6060c0",
                ColorMode.LIGHT: "#404080"
            },
            ColorRole.EDIT_BOX_BACKGROUND: {
                ColorMode.DARK: "#242454",
                ColorMode.LIGHT: "#b8b8f8"
            },
            ColorRole.EDIT_BOX_ERROR: {
                ColorMode.DARK: "#c03020",
                ColorMode.LIGHT: "#d04030"
            },

            # Mindspace colours
            ColorRole.MINDSPACE_BACKGROUND: {
                ColorMode.DARK: "#101010",
                ColorMode.LIGHT: "#fafafa"
            },
            ColorRole.MINDSPACE_NAME_BACKGROUND: {
                ColorMode.DARK: "#383838",
                ColorMode.LIGHT: "#d0d0d0"
            },
            ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER: {
                ColorMode.DARK: "#585858",
                ColorMode.LIGHT: "#b0b0b0"
            },
            ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED: {
                ColorMode.DARK: "#707070",
                ColorMode.LIGHT: "#909090"
            },
            ColorRole.MINDSPACE_HEADING: {
                ColorMode.DARK: "#242424",
                ColorMode.LIGHT: "#e0e0e0"
            },

            # Tab colours
            ColorRole.TAB_BAR_BACKGROUND: {
                ColorMode.DARK: "#383838",
                ColorMode.LIGHT: "#d0d0d0"
            },
            ColorRole.TAB_BACKGROUND_ACTIVE: {
                ColorMode.DARK: "#000000",
                ColorMode.LIGHT: "#ffffff"
            },
            ColorRole.TAB_BACKGROUND_INACTIVE: {
                ColorMode.DARK: "#202020",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.TAB_BACKGROUND_HOVER: {
                ColorMode.DARK: "#242454",
                ColorMode.LIGHT: "#b8b8f8"
            },
            ColorRole.TAB_BACKGROUND_UPDATED: {
                ColorMode.DARK: "#3c2054",
                ColorMode.LIGHT: "#f0d0f8"
            },
            ColorRole.TAB_BORDER_ACTIVE: {
                ColorMode.DARK: "#a0d0ff",
                ColorMode.LIGHT: "#ff3018"
            },

            # Button colours
            ColorRole.BUTTON_BACKGROUND: {
                ColorMode.DARK: "#0c0c0c",
                ColorMode.LIGHT: "#e8e8e8"
            },
            ColorRole.BUTTON_BACKGROUND_PRESSED: {
                ColorMode.DARK: "#505050",
                ColorMode.LIGHT: "#b0b0b0"
            },
            ColorRole.BUTTON_BACKGROUND_HOVER: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#c0c0c0"
            },
            ColorRole.BUTTON_SECONDARY_BACKGROUND: {
                ColorMode.DARK: "#2c2c2c",
                ColorMode.LIGHT: "#d8d8d8"
            },
            ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED: {
                ColorMode.DARK: "#505050",
                ColorMode.LIGHT: "#b0b0b0"
            },
            ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#c0c0c0"
            },
            ColorRole.BUTTON_BACKGROUND_RECOMMENDED: {
                ColorMode.DARK: "#2050c0",
                ColorMode.LIGHT: "#3060d0"
            },
            ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED: {
                ColorMode.DARK: "#4070e0",
                ColorMode.LIGHT: "#1040b0"
            },
            ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER: {
                ColorMode.DARK: "#3060d0",
                ColorMode.LIGHT: "#2050c0"
            },
            ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE: {
                ColorMode.DARK: "#c03020",
                ColorMode.LIGHT: "#d04030"
            },
            ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED: {
                ColorMode.DARK: "#e05040",
                ColorMode.LIGHT: "#b02010"
            },
            ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER: {
                ColorMode.DARK: "#d04030",
                ColorMode.LIGHT: "#c03020"
            },
            ColorRole.BUTTON_BACKGROUND_DISABLED: {
                ColorMode.DARK: "#202020",
                ColorMode.LIGHT: "#d8d8d8"
            },

            # Menu elements
            ColorRole.MENU_BACKGROUND: {
                ColorMode.DARK: "#2d2d2d",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.MENU_HOVER: {
                ColorMode.DARK: "#3060d0",
                ColorMode.LIGHT: "#5090e0"
            },
            ColorRole.MENU_BORDER: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#b0b0b0"
            },

            # Splitter bars
            ColorRole.SPLITTER: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#b0b0b0"
            },
            ColorRole.TAB_SPLITTER: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#d0d0d0"
            },

            # Scroll bar elements
            ColorRole.SCROLLBAR_BACKGROUND: {
                ColorMode.DARK: "#2d2d2d",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.SCROLLBAR_HANDLE: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#c0c0c0"
            },

            # Code block border
            ColorRole.CODE_BORDER: {
                ColorMode.DARK: "#383838",
                ColorMode.LIGHT: "#d0d0d0"
            },

            # Table elements
            ColorRole.TABLE_BORDER: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#a0a0a0"
            },
            ColorRole.TABLE_HEADER_BACKGROUND: {
                ColorMode.DARK: "#484838",
                ColorMode.LIGHT: "#d0d0e0"
            },

            # Message colours
            ColorRole.MESSAGE_BACKGROUND: {
                ColorMode.DARK: "#181818",
                ColorMode.LIGHT: "#f2f2f2"
            },
            ColorRole.MESSAGE_BACKGROUND_HOVER: {
                ColorMode.DARK: "#383838",
                ColorMode.LIGHT: "#d8d8d8"
            },
            ColorRole.MESSAGE_BACKGROUND_PRESSED: {
                ColorMode.DARK: "#505050",
                ColorMode.LIGHT: "#c0c0c0"
            },
            ColorRole.MESSAGE_USER_BACKGROUND: {
                ColorMode.DARK: "#282828",
                ColorMode.LIGHT: "#e0e0e0"
            },
            ColorRole.MESSAGE_USER_BACKGROUND_HOVER: {
                ColorMode.DARK: "#484848",
                ColorMode.LIGHT: "#c0c0c0"
            },
            ColorRole.MESSAGE_USER_BACKGROUND_PRESSED: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#a0a0a0"
            },
            ColorRole.MESSAGE_BORDER: {
                ColorMode.DARK: "#303030",
                ColorMode.LIGHT: "#e2e2e2"
            },
            ColorRole.MESSAGE_USER_BORDER: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#d0d0d0"
            },
            ColorRole.MESSAGE_FOCUSED: {
                ColorMode.DARK: "#586878",
                ColorMode.LIGHT: "#98a8b8"
            },
            ColorRole.MESSAGE_USER: {
                ColorMode.DARK: "#7090e0",
                ColorMode.LIGHT: "#5068a0"
            },
            ColorRole.MESSAGE_AI: {
                ColorMode.DARK: "#80c080",
                ColorMode.LIGHT: "#208020"
            },
            ColorRole.MESSAGE_REASONING: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#808080"
            },
            ColorRole.MESSAGE_TOOL_CALL: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#808080"
            },
            ColorRole.MESSAGE_TOOL_RESULT: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#808080"
            },
            ColorRole.MESSAGE_USER_QUEUED: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#808080"
            },
            ColorRole.MESSAGE_SYSTEM_ERROR: {
                ColorMode.DARK: "#c08080",
                ColorMode.LIGHT: "#a04040"
            },
            ColorRole.MESSAGE_SYSTEM_SUCCESS: {
                ColorMode.DARK: "#80c080",
                ColorMode.LIGHT: "#40a040"
            },
            ColorRole.MESSAGE_LANGUAGE: {
                ColorMode.DARK: "#a07850",
                ColorMode.LIGHT: "#806040"
            },
            ColorRole.MESSAGE_STREAMING: {
                ColorMode.DARK: "#c0a080",
                ColorMode.LIGHT: "#a07050"
            },
            ColorRole.MESSAGE_TRACE: {
                ColorMode.DARK: "#a0a0a0",
                ColorMode.LIGHT: "#606060"
            },
            ColorRole.MESSAGE_INFORMATION: {
                ColorMode.DARK: "#80b0f0",
                ColorMode.LIGHT: "#0060c0"
            },
            ColorRole.MESSAGE_WARNING: {
                ColorMode.DARK: "#f0c040",
                ColorMode.LIGHT: "#c0a020"
            },
            ColorRole.MESSAGE_ERROR: {
                ColorMode.DARK: "#ff6060",
                ColorMode.LIGHT: "#c03030"
            },

            # Status bar elements
            ColorRole.STATUS_BAR_BACKGROUND: {
                ColorMode.DARK: "#121212",
                ColorMode.LIGHT: "#e8e8e8"
            },
            ColorRole.CANARY_BACKGROUND: {
                ColorMode.DARK: "#802020",
                ColorMode.LIGHT: "#ff8080"
            },

            # Close button states
            ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER: {
                ColorMode.DARK: "#d04030",
                ColorMode.LIGHT: "#c03020"
            },

            # Line numbers
            ColorRole.LINE_NUMBER: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#c0c0c0"
            },

            # Syntax highlighting
            ColorRole.SYNTAX_ERROR: {
                ColorMode.DARK: "#ff0000",
                ColorMode.LIGHT: "#ff0000"
            },
            ColorRole.SYNTAX_01: {
                ColorMode.DARK: "#80e0d0",
                ColorMode.LIGHT: "#007070"
            },
            ColorRole.SYNTAX_02: {
                ColorMode.DARK: "#f0f0f0",
                ColorMode.LIGHT: "#202020"
            },
            ColorRole.SYNTAX_03: {
                ColorMode.DARK: "#68b068",
                ColorMode.LIGHT: "#407040"
            },
            ColorRole.SYNTAX_04: {
                ColorMode.DARK: "#ffa0eb",
                ColorMode.LIGHT: "#c000a0"
            },
            ColorRole.SYNTAX_05: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#606060"
            },
            ColorRole.SYNTAX_06: {
                ColorMode.DARK: "#90e0e8",
                ColorMode.LIGHT: "#0080a0"
            },
            ColorRole.SYNTAX_07: {
                ColorMode.DARK: "#e0e080",
                ColorMode.LIGHT: "#a0a000"
            },
            ColorRole.SYNTAX_08: {
                ColorMode.DARK: "#b090f0",
                ColorMode.LIGHT: "#5040c0"
            },
            ColorRole.SYNTAX_09: {
                ColorMode.DARK: "#90e0e8",
                ColorMode.LIGHT: "#0080a0"
            },
            ColorRole.SYNTAX_10: {
                ColorMode.DARK: "#d070d0",
                ColorMode.LIGHT: "#a000a0"
            },
            ColorRole.SYNTAX_11: {
                ColorMode.DARK: "#80b0f0",
                ColorMode.LIGHT: "#0060c0"
            },
            ColorRole.SYNTAX_12: {
                ColorMode.DARK: "#f08080",
                ColorMode.LIGHT: "#c03030"
            },
            ColorRole.SYNTAX_13: {
                ColorMode.DARK: "#ffc0eb",
                ColorMode.LIGHT: "#c080a0"
            },
            ColorRole.SYNTAX_14: {
                ColorMode.DARK: "#f0e060",
                ColorMode.LIGHT: "#a09040"
            },
            ColorRole.SYNTAX_15: {
                ColorMode.DARK: "#70e0e8",
                ColorMode.LIGHT: "#2090a0"
            },
            ColorRole.SYNTAX_16: {
                ColorMode.DARK: "#88d048",
                ColorMode.LIGHT: "#508020"
            },
            ColorRole.SYNTAX_17: {
                ColorMode.DARK: "#c0c0c0",
                ColorMode.LIGHT: "#404040"
            },
            ColorRole.SYNTAX_18: {
                ColorMode.DARK: "#80b080",
                ColorMode.LIGHT: "#609060"
            },
            ColorRole.SYNTAX_19: {
                ColorMode.DARK: "#c87050",
                ColorMode.LIGHT: "#a04020"
            },
            ColorRole.SYNTAX_20: {
                ColorMode.DARK: "#c05040",
                ColorMode.LIGHT: "#803828"
            },
            ColorRole.SYNTAX_21: {
                ColorMode.DARK: "#30c090",
                ColorMode.LIGHT: "#24906c"
            },

            # Terminal basic colors
            ColorRole.TERM_BLACK: {
                ColorMode.DARK: "#000000",
                ColorMode.LIGHT: "#000000"
            },
            ColorRole.TERM_RED: {
                ColorMode.DARK: "#cd0000",
                ColorMode.LIGHT: "#cd0000"
            },
            ColorRole.TERM_GREEN: {
                ColorMode.DARK: "#00cd00",
                ColorMode.LIGHT: "#00cd00"
            },
            ColorRole.TERM_YELLOW: {
                ColorMode.DARK: "#cdcd00",
                ColorMode.LIGHT: "#cdcd00"
            },
            ColorRole.TERM_BLUE: {
                ColorMode.DARK: "#0000ee",
                ColorMode.LIGHT: "#0000ee"
            },
            ColorRole.TERM_MAGENTA: {
                ColorMode.DARK: "#cd00cd",
                ColorMode.LIGHT: "#cd00cd"
            },
            ColorRole.TERM_CYAN: {
                ColorMode.DARK: "#00cdcd",
                ColorMode.LIGHT: "#00cdcd"
            },
            ColorRole.TERM_WHITE: {
                ColorMode.DARK: "#e5e5e5",
                ColorMode.LIGHT: "#e5e5e5"
            },

            # Terminal bright colors
            ColorRole.TERM_BRIGHT_BLACK: {
                ColorMode.DARK: "#7f7f7f",
                ColorMode.LIGHT: "#7f7f7f"
            },
            ColorRole.TERM_BRIGHT_RED: {
                ColorMode.DARK: "#ff0000",
                ColorMode.LIGHT: "#ff0000"
            },
            ColorRole.TERM_BRIGHT_GREEN: {
                ColorMode.DARK: "#00ff00",
                ColorMode.LIGHT: "#00ff00"
            },
            ColorRole.TERM_BRIGHT_YELLOW: {
                ColorMode.DARK: "#ffff00",
                ColorMode.LIGHT: "#ffff00"
            },
            ColorRole.TERM_BRIGHT_BLUE: {
                ColorMode.DARK: "#5c5cff",
                ColorMode.LIGHT: "#5c5cff"
            },
            ColorRole.TERM_BRIGHT_MAGENTA: {
                ColorMode.DARK: "#ff00ff",
                ColorMode.LIGHT: "#ff00ff"
            },
            ColorRole.TERM_BRIGHT_CYAN: {
                ColorMode.DARK: "#00ffff",
                ColorMode.LIGHT: "#00ffff"
            },
            ColorRole.TERM_BRIGHT_WHITE: {
                ColorMode.DARK: "#ffffff",
                ColorMode.LIGHT: "#ffffff"
            }
        }

    def _initialize_highlights(self) -> None:
        # Mapping from token type to colour
        colour_mapping = {
            TokenType.ADDRESS: ColorRole.SYNTAX_01,
            TokenType.ANNOTATION: ColorRole.SYNTAX_08,
            TokenType.ARGUMENT: ColorRole.SYNTAX_11,
            TokenType.ATTRIBUTE: ColorRole.SYNTAX_08,
            TokenType.BACKTICK: ColorRole.SYNTAX_12,
            TokenType.BOOLEAN: ColorRole.SYNTAX_16,
            TokenType.BOLD: ColorRole.SYNTAX_21,
            TokenType.BOLD_END: ColorRole.SYNTAX_07,
            TokenType.BOLD_START: ColorRole.SYNTAX_07,
            TokenType.BLOCKQUOTE: ColorRole.SYNTAX_13,
            TokenType.CHARACTER: ColorRole.SYNTAX_20,
            TokenType.CODE: ColorRole. SYNTAX_02,
            TokenType.COMMAND: ColorRole.SYNTAX_07,
            TokenType.COMMENT: ColorRole.SYNTAX_03,
            TokenType.CSS_AT_RULE: ColorRole.SYNTAX_04,
            TokenType.DIMENSION: ColorRole.SYNTAX_16,
            TokenType.DOC_COMMENT: ColorRole.SYNTAX_03,
            TokenType.DOCTYPE: ColorRole.SYNTAX_05,
            TokenType.DIRECTIVE: ColorRole.SYNTAX_18,
            TokenType.ELEMENT: ColorRole.SYNTAX_06,
            TokenType.ERROR: ColorRole.SYNTAX_ERROR,
            TokenType.FENCE: ColorRole.SYNTAX_14,
            TokenType.FENCE_END: ColorRole.SYNTAX_14,
            TokenType.FENCE_START: ColorRole.SYNTAX_14,
            TokenType.FUNCTION_OR_METHOD: ColorRole.SYNTAX_07,
            TokenType.GENERIC_END: ColorRole.SYNTAX_17,
            TokenType.GENERIC_START: ColorRole.SYNTAX_17,
            TokenType.GENERIC_TYPE: ColorRole.SYNTAX_21,
            TokenType.HASH: ColorRole.SYNTAX_11,
            TokenType.HEADING: ColorRole.SYNTAX_08,
            TokenType.HEX: ColorRole.SYNTAX_16,
            TokenType.HORIZONTAL_RULE: ColorRole.SYNTAX_21,
            TokenType.HTML_ATTRIBUTE: ColorRole.SYNTAX_09,
            TokenType.HTML_TAG: ColorRole.SYNTAX_10,
            TokenType.IDENTIFIER: ColorRole.SYNTAX_11,
            TokenType.IMAGE_ALT_END: ColorRole.SYNTAX_07,
            TokenType.IMAGE_ALT_TEXT: ColorRole.SYNTAX_16,
            TokenType.IMAGE_END: ColorRole.SYNTAX_07,
            TokenType.IMAGE_START: ColorRole.SYNTAX_07,
            TokenType.IMAGE_URL: ColorRole.SYNTAX_20,
            TokenType.INLINE_CODE: ColorRole.SYNTAX_12,
            TokenType.INLINE_CODE_END: ColorRole.SYNTAX_07,
            TokenType.INLINE_CODE_START: ColorRole.SYNTAX_07,
            TokenType.ITALIC: ColorRole.SYNTAX_09,
            TokenType.ITALIC_END: ColorRole.SYNTAX_07,
            TokenType.ITALIC_START: ColorRole.SYNTAX_07,
            TokenType.JSON_KEY: ColorRole.SYNTAX_07,
            TokenType.KEYWORD: ColorRole.SYNTAX_13,
            TokenType.LANGUAGE: ColorRole.SYNTAX_14,
            TokenType.LIFETIME: ColorRole.SYNTAX_17,
            TokenType.LINK_END: ColorRole.SYNTAX_07,
            TokenType.LINK_START: ColorRole.SYNTAX_07,
            TokenType.LINK_TEXT: ColorRole.SYNTAX_16,
            TokenType.LINK_TEXT_END: ColorRole.SYNTAX_07,
            TokenType.LINK_URL: ColorRole.SYNTAX_20,
            TokenType.LIST_MARKER: ColorRole.SYNTAX_15,
            TokenType.LPAREN: ColorRole.SYNTAX_17,
            TokenType.NUMBER: ColorRole.SYNTAX_16,
            TokenType.OPERATOR: ColorRole.SYNTAX_17,
            TokenType.OPTION: ColorRole.SYNTAX_10,
            TokenType.OPTION_VALUE: ColorRole.SYNTAX_08,
            TokenType.PREPROCESSOR: ColorRole.SYNTAX_05,
            TokenType.QUOTE: ColorRole.SYNTAX_13,
            TokenType.REGEXP: ColorRole.SYNTAX_19,
            TokenType.RPAREN: ColorRole.SYNTAX_17,
            TokenType.RUNE: ColorRole.SYNTAX_20,
            TokenType.STRING: ColorRole.SYNTAX_20,
            TokenType.TABLE: ColorRole.SYNTAX_03,
            TokenType.TEXT: ColorRole.SYNTAX_17,
            TokenType.TYPE: ColorRole.SYNTAX_21,
            TokenType.XML_DOC: ColorRole.SYNTAX_03
        }

        for token_type, role in colour_mapping.items():
            text_format = self._create_highlight(role)
            self._highlights[token_type] = text_format

        self._error_highlight = self._create_highlight(ColorRole.SYNTAX_ERROR)

    def _create_highlight(self, role: ColorRole) -> QTextCharFormat:
        text_highlight = QTextCharFormat()
        text_highlight.setFontFamilies(self._code_font_families)
        text_highlight.setFontFixedPitch(True)
        text_highlight.setForeground(QColor(self._colors[role][self._color_mode]))

        return text_highlight

    def _initialize_proportional_highlights(self) -> None:
        # Mapping from token type to colour
        colour_mapping = {
            TokenType.BOLD: ColorRole.SYNTAX_21,
            TokenType.BOLD_END: ColorRole.SYNTAX_07,
            TokenType.BOLD_START: ColorRole.SYNTAX_07,
            TokenType.BLOCKQUOTE: ColorRole.SYNTAX_13,
            TokenType.FENCE: ColorRole.SYNTAX_14,
            TokenType.HEADING: ColorRole.SYNTAX_08,
            TokenType.IMAGE_ALT_END: ColorRole.SYNTAX_07,
            TokenType.IMAGE_ALT_TEXT: ColorRole.SYNTAX_16,
            TokenType.IMAGE_END: ColorRole.SYNTAX_07,
            TokenType.IMAGE_START: ColorRole.SYNTAX_07,
            TokenType.IMAGE_URL: ColorRole.SYNTAX_20,
            TokenType.INLINE_CODE_END: ColorRole.SYNTAX_07,
            TokenType.INLINE_CODE_START: ColorRole.SYNTAX_07,
            TokenType.ITALIC: ColorRole.SYNTAX_09,
            TokenType.ITALIC_END: ColorRole.SYNTAX_07,
            TokenType.ITALIC_START: ColorRole.SYNTAX_07,
            TokenType.LINK_END: ColorRole.SYNTAX_07,
            TokenType.LINK_START: ColorRole.SYNTAX_07,
            TokenType.LINK_TEXT: ColorRole.SYNTAX_16,
            TokenType.LINK_TEXT_END: ColorRole.SYNTAX_07,
            TokenType.LINK_URL: ColorRole.SYNTAX_20,
            TokenType.LIST_MARKER: ColorRole.SYNTAX_15,
            TokenType.TABLE: ColorRole.SYNTAX_03,
            TokenType.TEXT: ColorRole.TEXT_PRIMARY
        }

        for token_type, role in colour_mapping.items():
            text_format = self._create_proportional_highlight(role)
            self._proportional_highlights[token_type] = text_format

        self._proportional_highlights[TokenType.INLINE_CODE] = self._create_highlight(ColorRole.SYNTAX_12)
        self._error_proportional_highlight = self._create_proportional_highlight(ColorRole.SYNTAX_ERROR)

    def _create_proportional_highlight(self, role: ColorRole) -> QTextCharFormat:
        text_highlight = QTextCharFormat()
        text_highlight.setForeground(QColor(self._colors[role][self._color_mode]))

        return text_highlight

    def _create_app_icon_svg(self, bg_color: str, text_color: str) -> str:
        """
        Create an application icon SVG with specified colors.

        Args:
            bg_color: Background color in hex format
            text_color: Text color in hex format

        Returns:
            str: SVG markup for the application icon
        """
        return f'''
            <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="512" zoomAndPan="magnify" viewBox="0 0 384 384" height="512" preserveAspectRatio="xMidYMid meet" version="1.0"><defs><g/></defs>
                <rect x="-38.4" width="460.8" fill="{text_color}" y="-38.399999" height="460.799983" fill-opacity="1"/>
                <rect x="-38.4" width="460.8" fill="{bg_color}" y="-38.399999" height="460.799983" fill-opacity="1"/>
                <g fill="{text_color}" fill-opacity="1">
                    <g transform="translate(94.464843, 294.75626)">
                        <g>
                            <path d="M 63.484375 -84.734375 L 63.484375 0 L 15.359375 0 L 15.359375 -211.203125 L 63.484375 -211.203125
                                L 63.484375 -126.96875 L 131.578125 -126.96875 L 131.578125 -211.203125 L 179.71875 -211.203125 L 179.71875 0
                                L 131.578125 0 L 131.578125 -84.734375 Z M 63.484375 -84.734375 "/>
                        </g>
                    </g>
                </g>
            </svg>
        '''

    def _write_icon(self, name: str, svg_data: str) -> None:
        icon_dir = os.path.expanduser("~/.humbug/icons")
        with open(os.path.join(icon_dir, name), 'w', encoding='utf-8') as f:
            f.write(svg_data)

    def _create_active_inactive_theme_icons(self, active: bool, suffix: str, color: str) -> None:
        prefix = "" if active else "inactive-"

        # Close button - visible version
        self._write_icon(f'{prefix}close-{suffix}.svg', f'''
            <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                <path stroke="{color}" stroke-width="6" fill="none"
                    d="M16,16 L48,48 M48,16 L16,48"/>
            </svg>
        ''')

        # Conversation tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}conversation-{suffix}.svg', f'''
            <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M8 10H8.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M12 10H12.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M16 10H16.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M21 13V7C21 5.11438 21 4.17157 20.4142 3.58579C19.8284 3 18.8856 3 17 3H7C5.11438 3
                    4.17157 3 3.58579 3.58579C3 4.17157 3 5.11438 3 7V13C3 14.8856 3 15.8284 3.58579 16.4142C4.17157
                    17 5.11438 17 7 17H7.5C7.77614 17 8 17.2239 8 17.5V20V20.1499C8 20.5037 8.40137 20.7081
                    8.6875 20.5L13.0956 17.2941C13.3584 17.103 13.675 17 14 17H17C18.8856 17 19.8284 17 20.4142
                    16.4142C21 15.8284 21 14.8856 21 13Z" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            </svg>
        ''')

        # Editor tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}editor-{suffix}.svg', f'''
            <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M13 21H21" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M20.0651 7.39423L7.09967 20.4114C6.72438 20.7882 6.21446 21 5.68265 21H4.00383C3.44943
                    21 3 20.5466 3 19.9922V18.2987C3 17.7696 3.20962 17.2621 3.58297 16.8873L16.5517 3.86681C19.5632
                    1.34721 22.5747 4.87462 20.0651 7.39423Z" stroke="{color}" stroke-width="2"
                    stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M15.3097 5.30981L18.7274 8.72755" stroke="{color}" stroke-width="2"
                    stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
        ''')

        # Log tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}log-{suffix}.svg', f'''
            <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M5 12H7.75044C7.89947 12 8.03179 11.9046 8.07892 11.7632V11.7632L9.875 6.375V6.375C9.91626
                    6.25122 10.0918 6.25238 10.1364 6.375V6.375L13.875 16.6562L13.885 16.6837C13.9253 16.7946 14.0812
                    16.797 14.125 16.6875V16.6875L15.8841 12.2898V12.2898C15.9541 12.1148 16.1236 12 16.3122 12H19"
                    stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
        ''')

        # Shell tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}shell-{suffix}.svg', f'''
            <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M3 7C3 5.11438 3 4.17157 3.58579 3.58579C4.17157 3 5.11438 3 7 3H12H17C18.8856 3 19.8284
                    3 20.4142 3.58579C21 4.17157 21 5.11438 21 7V10V13C21 14.8856 21 15.8284 20.4142 16.4142C19.8284
                    17 18.8856 17 17 17H12H7C5.11438 17 4.17157 17 3.58579 16.4142C3 15.8284 3 14.8856 3 13V10V7Z"
                    stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
                <path d="M7 21H17" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M12 17V21" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
            </svg>
        ''')

        # Terminal tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}terminal-{suffix}.svg', f'''
            <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M13 15H16" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
                <path d="M8 15L10.5 12.5V12.5C10.7761 12.2239 10.7761 11.7761 10.5 11.5V11.5L8 9"
                    stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M3 8C3 6.11438 3 5.17157 3.58579 4.58579C4.17157 4 5.11438 4 7 4H12H17C18.8856 4
                    19.8284 4 20.4142 4.58579C21 5.17157 21 6.11438 21 8V12V16C21 17.8856 21 18.8284 20.4142
                    19.4142C19.8284 20 18.8856 20 17 20H12H7C5.11438 20 4.17157 20 3.58579 19.4142C3 18.8284
                    3 17.8856 3 16V12V8Z" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            </svg>
        ''')

        # Preview tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}preview-{suffix}.svg', f'''
            <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M17.8284 6.82843C18.4065 7.40649 18.6955 7.69552 18.8478 8.06306C19 8.4306 19 8.83935
                    19 9.65685L19 17C19 18.8856 19 19.8284 18.4142 20.4142C17.8284 21 16.8856 21 15 21H9C7.11438
                    21 6.17157 21 5.58579 20.4142C5 19.8284 5 18.8856 5 17L5 7C5 5.11438 5 4.17157 5.58579
                    3.58579C6.17157 3 7.11438 3 9 3H12.3431C13.1606 3 13.5694 3 13.9369 3.15224C14.3045 3.30448
                    14.5935 3.59351 15.1716 4.17157L17.8284 6.82843Z" stroke="{color}"
                    stroke-width="2" stroke-linejoin="round"/>
                <path d="M9 6L11 6" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M10 9L12 9" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M9 12L11 12" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M10 15L12 15" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
        ''')

    def _create_theme_icons(self) -> None:
        """Create theme-specific icons in the user's .humbug directory."""
        icon_dir = os.path.expanduser("~/.humbug/icons")
        os.makedirs(icon_dir, exist_ok=True)

        # Create collapsed and expanded arrows for both themes
        for mode in ColorMode:
            color = self._colors[ColorRole.TEXT_PRIMARY][mode]
            inactive_color = self._colors[ColorRole.TEXT_INACTIVE][mode]
            suffix = mode.name.lower()

            # Right-pointing arrow
            self._write_icon(f'arrow-right-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M24,16 L40,32 L24,48"/>
                </svg>
            ''')

            # Left-pointing arrow
            self._write_icon(f'arrow-left-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M40,16 L24,32 L40,48"/>
                </svg>
            ''')

            # Up-pointing arrow
            self._write_icon(f'arrow-up-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M16,40 L32,24 L48,40"/>
                </svg>
            ''')

            # Down-pointing arrow
            self._write_icon(f'arrow-down-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M16,24 L32,40 L48,24"/>
                </svg>
            ''')

            # Close button - visible version
            self._write_icon(f'close-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M16,16 L48,48 M48,16 L16,48"/>
                </svg>
            ''')

            # Checkbox check mark
            self._write_icon(f'check-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M16,32 L28,44 L48,20" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
            ''')

            # Right-pointing expand
            self._write_icon(f'expand-right-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M20,8 L44,32 L20,56"/>
                </svg>
            ''')

            # Left-pointing expand
            self._write_icon(f'expand-left-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M44,8 L20,32 L44,56"/>
                </svg>
            ''')

            # Down-pointing expand
            self._write_icon(f'expand-down-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M8,20 L32,44 L56,20"/>
                </svg>
            ''')

            # Bulb on icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
            self._write_icon(f'info-{suffix}.svg', f'''
                <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path d="M12 7C9.23858 7 7 9.23858 7 12C7 13.3613 7.54402 14.5955 8.42651 15.4972C8.77025 15.8484 9.05281 16.2663
                        9.14923 16.7482L9.67833 19.3924C9.86537 20.3272 10.6862 21 11.6395 21H12.3605C13.3138 21 14.1346 20.3272
                        14.3217 19.3924L14.8508 16.7482C14.9472 16.2663 15.2297 15.8484 15.5735 15.4972C16.456 14.5955 17
                        13.3613 17 12C17 9.23858 14.7614 7 12 7Z" stroke="{color}" stroke-width="2"/>
                    <path d="M12 4V3" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M18 6L19 5" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M20 12H21" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M4 12H3" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M5 5L6 6" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M10 17H14" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
            ''')

            # Alert triangle icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
            self._write_icon(f'warning-{suffix}.svg', f'''
                <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path d="M12 10V13" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
                    <path d="M12 16V15.9888" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
                    <path d="M10.2518 5.147L3.6508 17.0287C2.91021 18.3618 3.87415 20 5.39912 20H18.6011C20.126 20 21.09
                        18.3618 20.3494 17.0287L13.7484 5.147C12.9864 3.77538 11.0138 3.77538 10.2518 5.147Z"
                        stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
            ''')

            # Alert hexagon icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
            self._write_icon(f'critical-{suffix}.svg', f'''
                <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path d="M3 9.22843V14.7716C3 15.302 3.21071 15.8107 3.58579 16.1858L7.81421 20.4142C8.18929 20.7893 8.69799
                        21 9.22843 21H14.7716C15.302 21 15.8107 20.7893 16.1858 20.4142L20.4142 16.1858C20.7893 15.8107 21
                        15.302 21 14.7716V9.22843C21 8.69799 20.7893 8.18929 20.4142 7.81421L16.1858 3.58579C15.8107 3.21071
                        15.302 3 14.7716 3H9.22843C8.69799 3 8.18929 3.21071 7.81421 3.58579L3.58579 7.81421C3.21071 8.18929
                        3 8.69799 3 9.22843Z" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M12 8V13" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
                    <path d="M12 16V15.9888" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
                </svg>
            ''')

            # Help circle icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
            self._write_icon(f'question-{suffix}.svg', f'''
                <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path d="M21 12C21 16.9706 16.9706 21 12 21C7.02944 21 3 16.9706 3 12C3 7.02944 7.02944 3 12
                        3C16.9706 3 21 7.02944 21 12Z" stroke="{color}" stroke-width="2"/>
                    <path d="M10.5 8.67709C10.8665 8.26188 11.4027 8 12 8C13.1046 8 14 8.89543 14 10C14 10.9337
                        13.3601 11.718 12.4949 11.9383C12.2273 12.0064 12 12.2239 12 12.5V12.5V13"
                        stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                    <path d="M12 16H12.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
            ''')

            self._write_icon(f'save-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M8,40, L8,56 L56,56 L56,40"/>
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M32,8, L32,40"/>
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M20,28, L32,40, L44,28"/>
                </svg>
            ''')

            self._write_icon(f'cog-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="4" fill="none" stroke-linejoin="miter"
                        d="M24,16 L31,14 L33,6 L44,9 L42,17 L47,22 L55,20 L58,31 L50,33 L48,40 L54,46 L46,54
                        L40,48 L33,50 L31,58 L20,55 L22,47 L17,42 L9,44 L6,33 L14,31 L16,24 L10,18 L18,10 Z
                        M40,32 A8,8 0 1,1 24,32 A8,8 0 1,1 40,32 Z"/>
                </svg>
            ''')

            self._write_icon(f'copy-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <rect x="22" y="10" width="32" height="32" stroke="{color}" stroke-width="6" fill="none"/>
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M22,22, L10,22 L10,54 L42,54 L42,42"/>
                </svg>
            ''')

            self._write_icon(f'fork-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M32,8 L32,32 M32,32 L8,56 L8,40 M8,56 L24,56 M32,32 L56,56 L56,40 M56,56 L40,56"/>
                </svg>
            ''')

            self._write_icon(f'delete-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M12.5,20 L12.5,56 L51.5,56 L51.5,20 M4,20 L60,20 M18,20 L24,8 L40,8 L46,20"/>
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M25.5,28 L25.5,46 M38.5,28 L38.5,46"/>
                </svg>
            ''')

            self._write_icon(f'edit-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none"
                        d="M8,56 L56,56 M40,12 L52,24 L24,52 L8,56 L12,40 L40,12 Z"/>
                </svg>
            ''')

            self._write_icon(f'submit-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path stroke="{color}" stroke-width="6" fill="none" d="M8,32 L32,8 L56,32"/>
                    <path stroke="{color}" stroke-width="6" fill="none" d="M32,56 L32,8"/>
                </svg>
            ''')

            self._write_icon(f'stop-{suffix}.svg', f'''
                <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <circle cx="32" cy="32" r="28" fill="none" stroke="{color}" stroke-width="6"/>
                    <rect x="20" y="20" width="24" height="24" fill="{color}" stroke="none"/>
                </svg>
            ''')

            self._create_active_inactive_theme_icons(True, suffix, color)
            self._create_active_inactive_theme_icons(False, suffix, inactive_color)

        # Create the standard application icon for about dialog
        self._write_icon('app-icon.svg', self._create_app_icon_svg('#4040c0', '#ffffff'))

        # Create light mode disabled version
        self._write_icon('app-icon-disabled-light.svg', self._create_app_icon_svg('#c0c0c0', '#e0e0e0'))

        # Create dark mode disabled version
        self._write_icon('app-icon-disabled-dark.svg', self._create_app_icon_svg('#202020', '#404040'))

    def get_icon_path(self, name: str) -> str:
        """
        Get the path to a theme-appropriate icon.

        Args:
            name: Base name of the icon (without theme suffix or extension)

        Returns:
            Full path to the icon file
        """
        icon_dir = os.path.expanduser("~/.humbug/icons")
        theme = "dark" if self._color_mode == ColorMode.DARK else "light"
        return Path(os.path.join(icon_dir, f"{name}-{theme}.svg")).as_posix()

    def scale_icon(self, icon_path: str, target_size: int) -> QPixmap:
        """
        Load and scale an icon to the appropriate size.

        Args:
            icon_path: Path to the icon file
            target_size: Desired size in pixels

        Returns:
            Scaled QPixmap of the icon
        """
        pixmap = QPixmap(icon_path)
        scaled_size = int(target_size * self._zoom_factor)
        return pixmap.scaled(
            scaled_size,
            scaled_size,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        )

    def get_color(self, role: ColorRole) -> QColor:
        """
        Get a color for a specific role.

        Args:
            role: The ColorRole to look up

        Returns:
            QColor: The color for the specified role

        Raises:
            KeyError: If no color is defined for the role
        """
        return QColor(self._colors[role][self._color_mode])

    def get_color_str(self, role: ColorRole) -> str:
        """
        Get a color string for a specific role.

        Args:
            role: The ColorRole to look up

        Returns:
            str: The color string (hex format) for the specified role

        Raises:
            KeyError: If no color is defined for the role
        """
        return self._colors[role][self._color_mode]

    def get_highlight(self, token_type: TokenType) -> QTextCharFormat:
        """
        Get the highlight format for a specific token type.

        Args:
            token_type: The TokenType to look up

        Returns:
            QTextCharFormat: The highlight format for the specified token type
        """
        if token_type not in self._highlights:
            print(f"token type {token_type} not mapped")

        return self._highlights.get(token_type, self._error_highlight)

    def get_proportional_highlight(self, token_type: TokenType) -> QTextCharFormat:
        """
        Get the proportionally-spaced highlight format for a specific token type.

        Args:
            token_type: The TokenType to look up

        Returns:
            QTextCharFormat: The highlight format for the specified token type
        """
        if token_type not in self._proportional_highlights:
            print(f"token type {token_type} not mapped")

        return self._proportional_highlights.get(token_type, self._error_proportional_highlight)

    def _determine_base_font_size(self) -> float:
        """
        Determine the default system font size based on the operating system.

        Returns:
            int: Base font size in points.
        """
        # Get current OS
        os_type = QOperatingSystemVersion.current()

        # Get system default font
        system_font = QFontDatabase.systemFont(QFontDatabase.SystemFont.GeneralFont)
        system_size = system_font.pointSizeF()

        # Apply OS-specific adjustments if system detection fails
        if system_size > 0:
            return system_size

        if os_type.type() == QOperatingSystemVersion.OSType.MacOS:  # type: ignore
            # macOS typically uses 13pt as default
            return 13

        if os_type.type() == QOperatingSystemVersion.OSType.Windows:  # type: ignore
            # Windows typically uses 9pt as default
            return 9

        # Linux typically uses 10pt as default
        return 10

    def base_font_size(self) -> float:
        """Get the base font size for the current system."""
        return self._user_font_size or self._base_font_size

    def set_user_font_size(self, size: float | None) -> None:
        """Set user-specific font size override."""
        if size != self._user_font_size:
            self._user_font_size = size

            if size:
                self.style_changed.emit()

    def color_mode(self) -> ColorMode:
        """Get the current color mode."""
        return self._color_mode

    def set_color_mode(self, mode: ColorMode) -> None:
        """
        Set the color mode and update application styles.

        Args:
            mode: The ColorMode to switch to
        """
        if mode != self._color_mode:
            self._color_mode = mode
            self._initialize_highlights()
            self._initialize_proportional_highlights()
            self.style_changed.emit()

    def zoom_factor(self) -> float:
        """Current zoom scaling factor."""
        return self._zoom_factor

    def set_zoom(self, factor: float) -> None:
        """
        Set new zoom factor and update application styles.

        Args:
            factor: New zoom factor to apply (clamped between 0.5 and 2.0)
        """
        new_factor = max(0.5, min(2.0, factor))
        if new_factor != self._zoom_factor:
            self._zoom_factor = new_factor
            self.style_changed.emit()

    def get_space_width(self) -> float:
        """Get the width of a space character"""
        font = QFont(self._code_font_families)
        font.setPointSizeF(self.base_font_size() * self._zoom_factor)
        font_metrics = QFontMetricsF(font)
        space_width = font_metrics.horizontalAdvance('        ') / 8
        return space_width

    def monospace_font_families(self) -> List[str]:
        """Get the standard monospace font family fallback sequence."""
        return self._code_font_families

    def message_bubble_spacing(self) -> float:
        """Get the number of pixels to use in message bubble spacing."""
        return 10.0

    def get_dialog_stylesheet(self) -> str:
        """
        Get a complete stylesheet for dialog windows.

        Returns:
            A stylesheet string with styling for all common dialog components
        """
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()

        return f"""
            QDialog {{
                background-color: {self.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                font-size: {base_font_size * zoom_factor}pt;
            }}

            QScrollArea {{
                background-color: {self.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                border: none;
            }}

            QScrollArea > QWidget > QWidget {{
                background-color: {self.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                border: none;
            }}

            /* Labels */
            QLabel {{
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {self.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                border: none;
                border-radius: 4px;
                padding: 0px;
                margin: 0px;
                font-size: {base_font_size * zoom_factor}pt;
            }}

            /* Text inputs */
            QLineEdit {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {int(base_font_size * zoom_factor)}pt;
            }}
            QLineEdit:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}

            /* Checkboxes */
            QCheckBox {{
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 0px;
                margin: 0px;
            }}
            QCheckBox::indicator {{
                width: {int(18 * zoom_factor)}px;
                height: {int(18 * zoom_factor)}px;
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            QCheckBox::indicator:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
            }}
            QCheckBox::indicator:checked {{
                image: url({self.get_icon_path('check')});
            }}
            QCheckBox:disabled {{
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}

            /* Combo boxes */
            QComboBox {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 6px;
                margin: 0px;
                font-size: {int(base_font_size * zoom_factor)}pt;
            }}
            QComboBox:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QComboBox::drop-down {{
                border: none;
                width: 20px;
            }}
            QComboBox::down-arrow {{
                image: url({self.get_icon_path("arrow-down")});
                width: 12px;
                height: 12px;
            }}
            QComboBox::down-arrow:on {{
                image: url({self.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QComboBox::down-arrow:disabled {{
                image: none;
            }}
            QComboBox QAbstractItemView::item:selected {{
                border: none;
                background-color: {self.get_color_str(ColorRole.TEXT_SELECTED)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QComboBox QListView {{
                background-color: {self.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}

            /* Spin boxes */
            QSpinBox, QDoubleSpinBox {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QSpinBox:disabled, QDoubleSpinBox:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QSpinBox::up-button, QSpinBox::down-button,
            QDoubleSpinBox::up-button, QDoubleSpinBox::down-button {{
                border: none;
                width: 20px;
            }}
            QSpinBox::up-arrow, QDoubleSpinBox::up-arrow {{
                image: url({self.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QSpinBox::up-arrow:disabled, QSpinBox::up-arrow:off,
            QDoubleSpinBox::up-arrow:disabled, QDoubleSpinBox::up-arrow:off {{
                image: none;
            }}
            QSpinBox::down-arrow, QDoubleSpinBox::down-arrow {{
                image: url({self.get_icon_path('arrow-down')});
                width: 12px;
                height: 12px;
            }}
            QSpinBox::down-arrow:disabled, QSpinBox::down-arrow:off,
            QDoubleSpinBox::down-arrow:disabled, QDoubleSpinBox::down-arrow:off {{
                image: none;
            }}

            /* Buttons */
            QPushButton {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QPushButton:hover {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QPushButton:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}

            /* Recommended (primary) buttons */
            QPushButton[recommended="true"] {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {self.get_color_str(ColorRole.TEXT_RECOMMENDED)};
            }}
            QPushButton[recommended="true"]:hover {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            QPushButton[recommended="true"]:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
            QPushButton[recommended="false"] {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)};
                color: {self.get_color_str(ColorRole.TEXT_RECOMMENDED)};
            }}
            QPushButton[recommended="false"]:hover {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER)};
            }}
            QPushButton[recommended="false"]:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED)};
            }}

            QScrollBar:vertical {{
                background-color: {self.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {self.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """
