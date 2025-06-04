"""Style manager for handling application-wide style and zoom settings.

Implements a singleton pattern to maintain consistent styling across components.
Provides signals for style changes and utilities for scaled size calculations.
"""

from enum import Enum, auto
import os
from pathlib import Path
from typing import Dict, List

from PySide6.QtCore import QObject, Signal, QOperatingSystemVersion, Qt
from PySide6.QtGui import (
    QTextCharFormat, QFontDatabase, QColor, QFontMetricsF, QFont, QPixmap
)

from humbug.gui.color_role import ColorRole
from humbug.syntax.lexer import TokenType


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
                ColorMode.DARK: "#080808",
                ColorMode.LIGHT: "#e0e0e0"
            },
            ColorRole.BACKGROUND_SECONDARY: {
                ColorMode.DARK: "#141414",
                ColorMode.LIGHT: "#f0f0f0"
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

            # Tab colours
            ColorRole.TAB_BACKGROUND_ACTIVE: {
                ColorMode.DARK: "#1f1f1f",
                ColorMode.LIGHT: "#ffffff"
            },
            ColorRole.TAB_INACTIVE: {
                ColorMode.DARK: "#909090",
                ColorMode.LIGHT: "#707070"
            },
            ColorRole.TAB_BACKGROUND_INACTIVE: {
                ColorMode.DARK: "#141414",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.TAB_BACKGROUND_HOVER: {
                ColorMode.DARK: "#242454",
                ColorMode.LIGHT: "#c8c8f8"
            },
            ColorRole.TAB_BORDER_ACTIVE: {
                ColorMode.DARK: "#8080ff",
                ColorMode.LIGHT: "#8080ff"
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
                ColorMode.DARK: "#3d3d3d",
                ColorMode.LIGHT: "#e0e0e0"
            },

            # Splitter bars
            ColorRole.SPLITTER: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#c0c0c0"
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
                ColorMode.DARK: "#020202",
                ColorMode.LIGHT: "#f4f4f4"
            },
            ColorRole.MESSAGE_USER_BACKGROUND: {
                ColorMode.DARK: "#141414",
                ColorMode.LIGHT: "#e4e4e4"
            },
            ColorRole.MESSAGE_FOCUSED: {
                ColorMode.DARK: "#6a7684",
                ColorMode.LIGHT: "#7a8694"
            },
            ColorRole.MESSAGE_USER: {
                ColorMode.DARK: "#8080c0",
                ColorMode.LIGHT: "#6060a0"
            },
            ColorRole.MESSAGE_AI: {
                ColorMode.DARK: "#80c080",
                ColorMode.LIGHT: "#40a040"
            },
            ColorRole.MESSAGE_REASONING: {
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
            ColorRole.MESSAGE_BOOKMARK: {
                ColorMode.DARK: "#ffdf00",
                ColorMode.LIGHT: "#806000"
            },
            ColorRole.MESSAGE_STREAMING: {
                ColorMode.DARK: "#c0a080",
                ColorMode.LIGHT: "#a07050"
            },

            # Status bar elements
            ColorRole.STATUS_BAR_BACKGROUND: {
                ColorMode.DARK: "#121212",
                ColorMode.LIGHT: "#e8e8e8"
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
                ColorMode.LIGHT: "#806000"
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
                ColorMode.DARK: "#CD0000",
                ColorMode.LIGHT: "#CD0000"
            },
            ColorRole.TERM_GREEN: {
                ColorMode.DARK: "#00CD00",
                ColorMode.LIGHT: "#00CD00"
            },
            ColorRole.TERM_YELLOW: {
                ColorMode.DARK: "#CDCD00",
                ColorMode.LIGHT: "#CDCD00"
            },
            ColorRole.TERM_BLUE: {
                ColorMode.DARK: "#0000EE",
                ColorMode.LIGHT: "#0000EE"
            },
            ColorRole.TERM_MAGENTA: {
                ColorMode.DARK: "#CD00CD",
                ColorMode.LIGHT: "#CD00CD"
            },
            ColorRole.TERM_CYAN: {
                ColorMode.DARK: "#00CDCD",
                ColorMode.LIGHT: "#00CDCD"
            },
            ColorRole.TERM_WHITE: {
                ColorMode.DARK: "#E5E5E5",
                ColorMode.LIGHT: "#E5E5E5"
            },

            # Terminal bright colors
            ColorRole.TERM_BRIGHT_BLACK: {
                ColorMode.DARK: "#7F7F7F",
                ColorMode.LIGHT: "#7F7F7F"
            },
            ColorRole.TERM_BRIGHT_RED: {
                ColorMode.DARK: "#FF0000",
                ColorMode.LIGHT: "#FF0000"
            },
            ColorRole.TERM_BRIGHT_GREEN: {
                ColorMode.DARK: "#00FF00",
                ColorMode.LIGHT: "#00FF00"
            },
            ColorRole.TERM_BRIGHT_YELLOW: {
                ColorMode.DARK: "#FFFF00",
                ColorMode.LIGHT: "#FFFF00"
            },
            ColorRole.TERM_BRIGHT_BLUE: {
                ColorMode.DARK: "#5C5CFF",
                ColorMode.LIGHT: "#5C5CFF"
            },
            ColorRole.TERM_BRIGHT_MAGENTA: {
                ColorMode.DARK: "#FF00FF",
                ColorMode.LIGHT: "#FF00FF"
            },
            ColorRole.TERM_BRIGHT_CYAN: {
                ColorMode.DARK: "#00FFFF",
                ColorMode.LIGHT: "#00FFFF"
            },
            ColorRole.TERM_BRIGHT_WHITE: {
                ColorMode.DARK: "#FFFFFF",
                ColorMode.LIGHT: "#FFFFFF"
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
            TokenType.PREPROCESSOR: ColorRole.SYNTAX_18,
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
            TokenType.TEXT: ColorRole.SYNTAX_17
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
            <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" viewBox="0 0 384 384">
                <rect x="0" y="0" width="384" height="384" fill="{bg_color}"/>
                <g fill="{text_color}">
                    <g transform="translate(38.400004, 250.960113)">
                        <path d="M 97.234375 0
                            L 92.125 -64.078125
                            L 91.46875 -64.078125
                            L 69.328125-0.65625
                            L 54.640625 -0.65625
                            L 32.5 -64.078125
                            L 31.84375 -64.078125
                            L 26.734375 0
                            L 2.09375 0
                            L 11.796875 -108.109375
                            L 38 -108.109375
                            L 61.984375 -45.734375
                            L 85.96875 -108.109375
                            L 112.171875 -108.109375
                            L 121.875 0
                            Z"/>
                    </g>
                    <g transform="translate(162.225522, 250.960113)">
                        <path d="M 76 -108.109375
                            L 44.15625 -71.03125
                            C 46 -72.070312 48.578125 -72.59375 51.890625 -72.59375
                            C 55.734375 -72.59375 59.617188 -71.785156 63.546875 -70.171875
                            C 67.484375 -68.554688 71.175781 -66.238281 74.625 -63.21875
                            C 78.082031 -60.207031 80.898438 -56.210938 83.078125 -51.234375
                            C 85.265625 -46.253906 86.359375 -40.753906 86.359375 -34.734375
                            C 86.359375 -28.609375 85.203125 -23.125 82.890625 -18.28125
                            C 80.578125 -13.4375 77.472656 -9.59375 73.578125 -6.75
                            C 69.691406 -3.90625 65.429688 -1.738281 60.796875 -0.25
                            C 56.171875 1.226562 51.328125 1.96875 46.265625 1.96875
                            C 34.117188 1.96875 23.894531 -1.566406 15.59375 -8.640625
                            C 7.289062 -15.722656 3.140625 -24.421875 3.140625 -34.734375
                            C 3.140625 -43.460938 4.363281 -50.644531 6.8125 -56.28125
                            C 9.257812 -61.914062 13.71875 -69.101562 20.1875 -77.84375
                            L 45.609375 -108.109375
                            Z
                            M 28.703125 -34.734375
                            C 28.703125 -30.015625 30.226562 -26.101562 33.28125 -23
                            C 36.34375 -19.894531 40.273438 -18.34375 45.078125 -18.34375
                            C 49.878906 -18.34375 53.851562 -19.894531 57 -23
                            C 60.144531 -26.101562 61.71875 -30.015625 61.71875 -34.734375
                            C 61.71875 -39.535156 60.125 -43.53125 56.9375 -46.71875
                            C 53.75 -49.90625 49.796875 -51.5 45.078125 -51.5
                            C 40.535156 -51.5 36.671875 -49.90625 33.484375 -46.71875
                            C 30.296875 -43.53125 28.703125 -39.535156 28.703125 -34.734375
                            Z"/>
                    </g>
                    <g transform="translate(249.066902, 250.960113)">
                        <path d="M 7.859375 -108.109375
                            L 46.515625 -108.109375
                            C 56.910156 -108.109375 65.382812 -105.332031 71.9375 -99.78125
                            C 78.488281 -94.238281 81.765625 -86.3125 81.765625 -76
                            C 81.765625 -68.050781 79.773438 -61.390625 75.796875 -56.015625
                            C 71.828125 -50.648438 66.4375 -46.875 59.625 -44.6875
                            L 96.84375 0
                            L 65.515625 0
                            L 32.5 -42.71875
                            L 32.5 0
                            L 7.859375 0
                            Z
                            M 32.5 -60.15625
                            L 35.375 -60.15625
                            C 37.65625 -60.15625 39.578125 -60.195312 41.140625 -60.28125
                            C 42.710938 -60.363281 44.503906 -60.664062 46.515625 -61.1875
                            C 48.523438 -61.71875 50.140625 -62.460938 51.359375 -63.421875
                            C 52.585938 -64.390625 53.640625 -65.789062 54.515625 -67.625
                            C 55.390625 -69.457031 55.828125 -71.679688 55.828125 -74.296875
                            C 55.828125 -76.921875 55.390625 -79.148438 54.515625 -80.984375
                            C 53.640625 -82.816406 52.585938 -84.210938 51.359375 -85.171875
                            C 50.140625 -86.140625 48.523438 -86.882812 46.515625 -87.40625
                            C 44.503906 -87.925781 42.710938 -88.226562 41.140625 -88.3125
                            C 39.578125 -88.40625 37.65625 -88.453125 35.375 -88.453125
                            L 32.5 -88.453125
                            Z"/>
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

        # System tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}system-{suffix}.svg', f'''
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

        # Wiki tab icon (from https://www.svgrepo.com/collection/scarlab-oval-line-icons/)
        self._write_icon(f'{prefix}wiki-{suffix}.svg', f'''
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
            inactive_color = self._colors[ColorRole.TEXT_DISABLED][mode]
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

            # Message box icons
            self._write_icon(f'info-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <circle cx="32" cy="32" r="28" stroke="{color}" stroke-width="4" fill="none"/>
                    <text x="32" y="40" text-anchor="middle"
                        font-size="36" fill="{color}" font-family="sans-serif">i</text>
                </svg>
            ''')

            self._write_icon(f'warning-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <path d="M32 4 L60 56 L4 56 Z" stroke="{color}" stroke-width="4" fill="none"/>
                    <text x="32" y="48" text-anchor="middle"
                        font-size="36" fill="{color}" font-family="sans-serif">!</text>
                </svg>
            ''')

            self._write_icon(f'critical-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <circle cx="32" cy="32" r="28" stroke="{color}" stroke-width="4" fill="none"/>
                    <path stroke="{color}" stroke-width="4" fill="none"
                        d="M20,20 L44,44 M44,20 L20,44"/>
                </svg>
            ''')

            self._write_icon(f'question-{suffix}.svg', f'''
                <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
                    <circle cx="32" cy="32" r="28" stroke="{color}" stroke-width="4" fill="none"/>
                    <text x="32" y="44" text-anchor="middle"
                        font-size="36" fill="{color}" font-family="sans-serif">?</text>
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

        if os_type.type() == QOperatingSystemVersion.OSType.MacOS:
            # macOS typically uses 13pt as default
            return 13

        if os_type.type() == QOperatingSystemVersion.OSType.Windows:
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
            self._initialize_highlights()  # Reinitialize highlights with new colors
            self._initialize_proportional_highlights()
            self.style_changed.emit()  # Trigger style update

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
        return 8.0

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
                padding: 0;
                margin: 0;
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
                padding: 0;
                margin: 0;
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
                margin: 0;
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
