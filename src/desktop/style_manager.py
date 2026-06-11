"""
Style manager for handling application-wide style and zoom settings.

Implements a singleton pattern to maintain consistent styling across components.
Provides signals for style changes and utilities for scaled size calculations.
"""

from enum import Enum, auto
import os
from pathlib import Path
import shutil
import sys
from typing import Dict, List

from PySide6.QtCore import QObject, Signal, QOperatingSystemVersion, Qt
from PySide6.QtGui import QTextCharFormat, QFontDatabase, QColor, QFontMetricsF, QFont, QPixmap, QGuiApplication
from PySide6.QtGui import QPainter
from PySide6.QtWidgets import QMenu, QWidget
from PySide6.QtSvg import QSvgRenderer

from syntax import TokenType

from desktop.color_role import ColorRole
from desktop.icons.icon_pack import active_inactive_icon_names, app_icon_svg, theme_icon_svg, update_icon_svg


class ColorMode(Enum):
    """Enumeration for color theme modes."""
    LIGHT = auto()
    DARK = auto()
    SYSTEM = auto()


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
            self._font_ligatures: bool = True
            self._color_mode = ColorMode.SYSTEM  # Default to system mode
            self._colors: Dict[ColorRole, Dict[ColorMode, str]] = self._initialize_colors()
            self._highlights: Dict[TokenType, QTextCharFormat] = {}
            self._proportional_highlights: Dict[TokenType, QTextCharFormat] = {}

            # Cache SVG-rendered icons by (name, scaled_size)
            self._scaled_icon_cache: Dict[tuple[str, int], QPixmap] = {}

            self._code_font_families = ["JetBrains Mono", "Noto Sans Arabic"]
            self._proportional_font_families = ["Noto Sans", "Noto Sans Arabic"]
            self._initialize_highlights()
            self._initialize_proportional_highlights()
            self._create_theme_icons()

            # Connect to OS-level colour scheme changes for SYSTEM mode
            hints = QGuiApplication.styleHints()
            hints.colorSchemeChanged.connect(self._on_system_color_scheme_changed)

    def _initialize_colors(self) -> Dict[ColorRole, Dict[ColorMode, str]]:
        """Initialize the application colours for both light and dark modes."""
        return {
            # Brand colours
            ColorRole.BRAND_PRIMARY: {
                ColorMode.DARK: "#9b87f5",
                ColorMode.LIGHT: "#6248e8"
            },
            ColorRole.BRAND_GRADIENT_START: {
                ColorMode.DARK: "#29c5ff",
                ColorMode.LIGHT: "#29c5ff"
            },
            ColorRole.BRAND_GRADIENT_END: {
                ColorMode.DARK: "#c050ff",
                ColorMode.LIGHT: "#c050ff"
            },
            ColorRole.BRAND_ICON_BG_START: {
                ColorMode.DARK: "#1e0e50",
                ColorMode.LIGHT: "#1e0e50"
            },
            ColorRole.BRAND_ICON_BG_END: {
                ColorMode.DARK: "#060612",
                ColorMode.LIGHT: "#060612"
            },

            # Background colours
            ColorRole.BACKGROUND_PRIMARY: {
                ColorMode.DARK: "#080808",
                ColorMode.LIGHT: "#f4f4f4"
            },
            ColorRole.BACKGROUND_SECONDARY: {
                ColorMode.DARK: "#282828",
                ColorMode.LIGHT: "#e4e4e4"
            },
            ColorRole.BACKGROUND_TERTIARY: {
                ColorMode.DARK: "#060606",
                ColorMode.LIGHT: "#fefefe"
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
                ColorMode.DARK: "#1e1e1e",
                ColorMode.LIGHT: "#d8d8d8"
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
                ColorMode.DARK: "#5080ff",
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
            ColorRole.TEXT_SUCCESS: {
                ColorMode.DARK: "#4ade80",
                ColorMode.LIGHT: "#16a34a"
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

            ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND: {
                ColorMode.DARK: "#181818",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.MINDSPACE_FOLDER: {
                ColorMode.DARK: "#4B89DC",
                ColorMode.LIGHT: "#4B89DC"
            },
            ColorRole.MINDSPACE_FOLDER_BREADCRUMB: {
                ColorMode.DARK: "#F0C0C8",
                ColorMode.LIGHT: "#C08090"
            },

            # Tab colours
            ColorRole.TAB_BAR_BACKGROUND: {
                ColorMode.DARK: "#1c1c1c",
                ColorMode.LIGHT: "#ececec"
            },
            ColorRole.TAB_BACKGROUND_ACTIVE: {
                ColorMode.DARK: "#000000",
                ColorMode.LIGHT: "#ffffff"
            },
            ColorRole.TAB_BACKGROUND_INACTIVE: {
                ColorMode.DARK: "#303030",
                ColorMode.LIGHT: "#d4d4d4"
            },
            ColorRole.TAB_BACKGROUND_HOVER: {
                ColorMode.DARK: "#242454",
                ColorMode.LIGHT: "#c8c8ff"
            },
            ColorRole.TAB_BACKGROUND_UPDATED: {
                ColorMode.DARK: "#3c2054",
                ColorMode.LIGHT: "#f0d0f8"
            },
            ColorRole.TAB_BORDER_ACTIVE: {
                ColorMode.DARK: "#e03826",
                ColorMode.LIGHT: "#ff4030"
            },

            # Button colours
            ColorRole.BUTTON_BACKGROUND: {
                ColorMode.DARK: "#303030",
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
                ColorMode.LIGHT: "#6080e0"
            },
            ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED: {
                ColorMode.DARK: "#4070e0",
                ColorMode.LIGHT: "#4060c0"
            },
            ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER: {
                ColorMode.DARK: "#3060d0",
                ColorMode.LIGHT: "#5070d0"
            },
            ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE: {
                ColorMode.DARK: "#c03020",
                ColorMode.LIGHT: "#e06048"
            },
            ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED: {
                ColorMode.DARK: "#e05040",
                ColorMode.LIGHT: "#c04030"
            },
            ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER: {
                ColorMode.DARK: "#d04030",
                ColorMode.LIGHT: "#d0503c"
            },
            ColorRole.BUTTON_BACKGROUND_EDIT: {
                ColorMode.DARK: "#b07010",
                ColorMode.LIGHT: "#a06008"
            },
            ColorRole.BUTTON_BACKGROUND_EDIT_PRESSED: {
                ColorMode.DARK: "#906000",
                ColorMode.LIGHT: "#804800"
            },
            ColorRole.BUTTON_BACKGROUND_EDIT_HOVER: {
                ColorMode.DARK: "#c08020",
                ColorMode.LIGHT: "#b07018"
            },
            ColorRole.BUTTON_BACKGROUND_DISABLED: {
                ColorMode.DARK: "#282828",
                ColorMode.LIGHT: "#e0e0e0"
            },

            # Switch colours
            ColorRole.SWITCH_TRACK_ON: {
                ColorMode.DARK: "#5a6893",
                ColorMode.LIGHT: "#90a0e0"
            },
            ColorRole.SWITCH_TRACK_OFF: {
                ColorMode.DARK: "#303030",
                ColorMode.LIGHT: "#e0e0e0"
            },
            ColorRole.SWITCH_TRACK_DISABLED_ON: {
                ColorMode.DARK: "#3d4670",
                ColorMode.LIGHT: "#c0d2f0"
            },
            ColorRole.SWITCH_TRACK_BORDER: {
                ColorMode.DARK: "#505050",
                ColorMode.LIGHT: "#b0b0b0"
            },
            ColorRole.SWITCH_KNOB: {
                ColorMode.DARK: "#d8d8d8",
                ColorMode.LIGHT: "#202020"
            },
            ColorRole.SWITCH_KNOB_DISABLED: {
                ColorMode.DARK: "#909090",
                ColorMode.LIGHT: "#707070"
            },

            # Menu elements
            ColorRole.MENU_BACKGROUND: {
                ColorMode.DARK: "#2d2d2d",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.MENU_HOVER: {
                ColorMode.DARK: "#3060d0",
                ColorMode.LIGHT: "#70a0f0"
            },
            ColorRole.MENU_BORDER: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#b0b0b0"
            },
            ColorRole.COMBO_ITEM_HOVER: {
                ColorMode.DARK: "#484848",
                ColorMode.LIGHT: "#d0d0d0"
            },

            # Splitter bars
            ColorRole.SPLITTER: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#b0b0b0"
            },
            ColorRole.TAB_SPLITTER: {
                ColorMode.DARK: "#040404",
                ColorMode.LIGHT: "#fcfcfc"
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
                ColorMode.DARK: "#282828",
                ColorMode.LIGHT: "#e0e0e0"
            },

            # Table elements
            ColorRole.TABLE_BORDER: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#a0a0a0"
            },

            # Blockquote elements
            ColorRole.BLOCKQUOTE_BORDER: {
                ColorMode.DARK: "#5a5a48",
                ColorMode.LIGHT: "#c0c0d2"
            },
            ColorRole.TABLE_HEADER_BACKGROUND: {
                ColorMode.DARK: "#484838",
                ColorMode.LIGHT: "#d0d0e0"
            },

            # Message colours
            ColorRole.MESSAGE_BACKGROUND: {
                ColorMode.DARK: "#121212",
                ColorMode.LIGHT: "#f8f8f8"
            },
            ColorRole.MESSAGE_BACKGROUND_HOVER: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#c4c4c4"
            },
            ColorRole.MESSAGE_BACKGROUND_PRESSED: {
                ColorMode.DARK: "#585858",
                ColorMode.LIGHT: "#b0b0b0"
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

            # The input background has an alpha channel so it allows a small amount of what's underneath it to show through.
            ColorRole.MESSAGE_INPUT_BACKGROUND: {
                ColorMode.DARK: "#ea383838",
                ColorMode.LIGHT: "#ead0d0d0"
            },
            ColorRole.MESSAGE_ATTACHMENT_BACKGROUND: {
                ColorMode.DARK: "#383838",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.MESSAGE_BORDER: {
                ColorMode.DARK: "#282828",
                ColorMode.LIGHT: "#e4e4e4"
            },
            ColorRole.MESSAGE_USER_BORDER: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#d0d0d0"
            },
            ColorRole.MESSAGE_INPUT_BORDER: {
                ColorMode.DARK: "#707070",
                ColorMode.LIGHT: "#909090"
            },
            ColorRole.MESSAGE_SPOTLIGHTED: {
                ColorMode.DARK: "#788ca0",
                ColorMode.LIGHT: "#607488"
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
                ColorMode.DARK: "#a0a0a0",
                ColorMode.LIGHT: "#606060"
            },
            ColorRole.MESSAGE_SYSTEM_ERROR: {
                ColorMode.DARK: "#c08080",
                ColorMode.LIGHT: "#a04040"
            },
            ColorRole.MESSAGE_SYSTEM_SUCCESS: {
                ColorMode.DARK: "#80c080",
                ColorMode.LIGHT: "#40a040"
            },
            ColorRole.MESSAGE_SYNTAX: {
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
                ColorMode.DARK: "#e03030",
                ColorMode.LIGHT: "#ff7070"
            },

            # Line numbers
            ColorRole.DROP_TARGET_HIGHLIGHT: {
                ColorMode.DARK: "#142454",
                ColorMode.LIGHT: "#a8b8f8"
            },
            ColorRole.DROP_TARGET_SEPARATOR_HIGHLIGHT: {
                ColorMode.DARK: "#a8b8f8",
                ColorMode.LIGHT: "#142454"
            },

            # Line numbers
            ColorRole.LINE_NUMBER: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#c0c0c0"
            },

            # Diff view colours
            ColorRole.DIFF_REMOVED_BACKGROUND: {
                ColorMode.DARK: "#402020",
                ColorMode.LIGHT: "#f0c8c8"
            },
            ColorRole.DIFF_ADDED_BACKGROUND: {
                ColorMode.DARK: "#204020",
                ColorMode.LIGHT: "#c8f0c8"
            },
            ColorRole.DIFF_CHANGED_BACKGROUND: {
                ColorMode.DARK: "#202050",
                ColorMode.LIGHT: "#c8c8f0"
            },
            ColorRole.DIFF_HUNK_LINE_NUMBER: {
                ColorMode.DARK: "#e0e0e0",
                ColorMode.LIGHT: "#202020"
            },

            # VCS status colours
            ColorRole.VCS_MODIFIED: {
                ColorMode.DARK: "#f0c040",
                ColorMode.LIGHT: "#c0a020"
            },
            ColorRole.VCS_ADDED: {
                ColorMode.DARK: "#68b068",
                ColorMode.LIGHT: "#207020"
            },
            ColorRole.VCS_DELETED: {
                ColorMode.DARK: "#f08080",
                ColorMode.LIGHT: "#c03030"
            },
            ColorRole.VCS_RENAMED: {
                ColorMode.DARK: "#8080c0",
                ColorMode.LIGHT: "#4040a0"
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
                ColorMode.DARK: "#68c068",
                ColorMode.LIGHT: "#40a040"
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
                ColorMode.DARK: "#80a0f0",
                ColorMode.LIGHT: "#0040c0"
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
                ColorMode.LIGHT: "#b03828"
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
            TokenType.DIFF_ADDED: ColorRole.SYNTAX_16,
            TokenType.DIFF_CHANGED: ColorRole.SYNTAX_14,
            TokenType.DIFF_HEADING: ColorRole.SYNTAX_07,
            TokenType.DIFF_METADATA: ColorRole.SYNTAX_11,
            TokenType.DIFF_REMOVED: ColorRole.SYNTAX_12,
            TokenType.DOCTYPE: ColorRole.SYNTAX_05,
            TokenType.DOT: ColorRole.SYNTAX_13,
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
            TokenType.STRIKETHROUGH: ColorRole.SYNTAX_10,
            TokenType.STRIKETHROUGH_END: ColorRole.SYNTAX_07,
            TokenType.STRIKETHROUGH_START: ColorRole.SYNTAX_07,
            TokenType.TYPE: ColorRole.SYNTAX_21,
            TokenType.VECTOR_START: ColorRole.SYNTAX_08,
            TokenType.XML_DOC: ColorRole.SYNTAX_03
        }

        for token_type, role in colour_mapping.items():
            text_format = self._create_highlight(role)
            self._highlights[token_type] = text_format

        self._error_highlight = self._create_highlight(ColorRole.SYNTAX_ERROR)

    def _create_highlight(self, role: ColorRole) -> QTextCharFormat:
        text_highlight = QTextCharFormat()
        text_highlight.setFontFamilies(self._code_font_families)
        if not self._font_ligatures:
            text_highlight.setFontStyleStrategy(QFont.StyleStrategy.PreferNoShaping)

        text_highlight.setFontFixedPitch(True)
        text_highlight.setForeground(QColor(self._colors[role][self._resolve_color_mode()]))

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
            TokenType.STRIKETHROUGH: ColorRole.SYNTAX_10,
            TokenType.STRIKETHROUGH_END: ColorRole.SYNTAX_07,
            TokenType.STRIKETHROUGH_START: ColorRole.SYNTAX_07,
            TokenType.TEXT: ColorRole.TEXT_PRIMARY
        }

        for token_type, role in colour_mapping.items():
            text_format = self._create_proportional_highlight(role)
            self._proportional_highlights[token_type] = text_format

        self._proportional_highlights[TokenType.INLINE_CODE] = self._create_highlight(ColorRole.SYNTAX_12)
        self._error_proportional_highlight = self._create_proportional_highlight(ColorRole.SYNTAX_ERROR)

    def _create_proportional_highlight(self, role: ColorRole) -> QTextCharFormat:
        text_highlight = QTextCharFormat()
        text_highlight.setForeground(QColor(self._colors[role][self._resolve_color_mode()]))

        return text_highlight

    def _write_icon(self, name: str, svg_data: str) -> None:
        icon_dir = os.path.expanduser("~/.humbug/icons")
        with open(os.path.join(icon_dir, name), 'w', encoding='utf-8') as f:
            f.write(svg_data)

    def _create_active_inactive_theme_icons(self, active: bool, suffix: str, color: str, prefix: str | None = None) -> None:
        if prefix is None:
            prefix = "" if active else "inactive-"

        for icon_name in active_inactive_icon_names():
            self._write_icon(f'{prefix}{icon_name}-{suffix}.svg', theme_icon_svg(icon_name, color))

    def _create_theme_icons(self) -> None:
        """Create theme-specific icons in the user's .humbug directory."""
        icon_dir = os.path.expanduser("~/.humbug/icons")
        os.makedirs(icon_dir, exist_ok=True)

        # Create collapsed and expanded arrows for both themes
        for mode in (ColorMode.LIGHT, ColorMode.DARK):
            color = self._colors[ColorRole.TEXT_PRIMARY][mode]
            inactive_color = self._colors[ColorRole.TEXT_INACTIVE][mode]
            bright_color = self._colors[ColorRole.TEXT_BRIGHT][mode]
            update_color = self._colors[ColorRole.BUTTON_BACKGROUND_RECOMMENDED][mode]
            suffix = mode.name.lower()

            for icon_name in (
                "arrow-right", "arrow-left", "arrow-up", "arrow-down", "close", "check",
                "expand-right", "expand-left", "expand-down", "info", "warning", "critical",
                "question", "save", "cog", "copy", "fork", "delete", "edit", "submit",
                "stop", "paperclip", "minimize", "maximize", "restore"
            ):
                self._write_icon(f'{icon_name}-{suffix}.svg', theme_icon_svg(icon_name, color))

            self._write_icon(f'inactive-expand-right-{suffix}.svg', theme_icon_svg("expand-right", inactive_color))
            self._write_icon(f'inactive-expand-left-{suffix}.svg', theme_icon_svg("expand-left", inactive_color))
            self._write_icon(f'bright-expand-right-{suffix}.svg', theme_icon_svg("expand-right", bright_color))
            self._write_icon(f'bright-expand-left-{suffix}.svg', theme_icon_svg("expand-left", bright_color))
            self._write_icon(f'inactive-cog-{suffix}.svg', theme_icon_svg("cog", inactive_color))
            self._write_icon(f'bright-cog-{suffix}.svg', theme_icon_svg("cog", bright_color))

            self._create_active_inactive_theme_icons(True, suffix, color)
            self._create_active_inactive_theme_icons(False, suffix, inactive_color)
            self._create_active_inactive_theme_icons(True, suffix, bright_color, prefix="bright-")

            self._write_icon(f'update-{suffix}.svg', update_icon_svg(update_color))
            for icon_name in ("find-match-case", "find-whole-word", "find-regexp", "find-hidden"):
                self._write_icon(f'{icon_name}-{suffix}.svg', theme_icon_svg(icon_name, color))

        # SVG fallback for when the PNG is unavailable — gradient container, brand H
        brand_color = self._colors[ColorRole.BRAND_PRIMARY][ColorMode.DARK]
        bg_start = self._colors[ColorRole.BRAND_ICON_BG_START][ColorMode.DARK]
        bg_end = self._colors[ColorRole.BRAND_ICON_BG_END][ColorMode.DARK]
        self._write_icon('app-icon.svg', app_icon_svg(brand_color, bg_start, bg_end))

        # Composite the transparent source logo onto themed backgrounds
        if getattr(sys, 'frozen', False):
            resources_base = Path(getattr(sys, '_MEIPASS'))

        else:
            resources_base = Path(__file__).parent.parent.parent

        # Copy app-icon.png from bundled resources to ~/.humbug/icons
        src = Path(resources_base / 'resources' / 'icons' / 'app-icon.png')
        if src.exists():
            shutil.copy2(str(src), os.path.join(icon_dir, 'app-icon.png'))

        # Copy app-logo.png from bundled resources to ~/.humbug/logos
        logo_dir = os.path.expanduser("~/.humbug/logos")
        os.makedirs(logo_dir, exist_ok=True)
        src = Path(resources_base / 'resources' / 'logos' / 'app-logo.png')
        if src.exists():
            shutil.copy2(str(src), os.path.join(logo_dir, 'app-logo.png'))

    def get_icon_path(self, name: str) -> str:
        """
        Get the path to a theme-appropriate icon.

        Args:
            name: Base name of the icon (without theme suffix or extension)

        Returns:
            Full path to the icon file
        """
        icon_dir = os.path.expanduser("~/.humbug/icons")
        theme = "dark" if self._resolve_color_mode() == ColorMode.DARK else "light"
        return Path(os.path.join(icon_dir, f"{name}-{theme}.svg")).as_posix()

    def get_app_icon_path(self) -> str:
        """Return the app icon PNG path, falling back to SVG."""
        icon_dir = os.path.expanduser("~/.humbug/icons")
        png = os.path.join(icon_dir, "app-icon.png")
        if os.path.exists(png):
            return Path(png).as_posix()

        return Path(os.path.join(icon_dir, "app-icon.svg")).as_posix()

    def get_app_logo_path(self) -> str:
        """Return the app logo PNG path, falling back to SVG."""
        icon_dir = os.path.expanduser("~/.humbug/logos")
        png = os.path.join(icon_dir, "app-logo.png")
        if os.path.exists(png):
            return Path(png).as_posix()

        return Path(os.path.join(icon_dir, "app-icon.svg")).as_posix()

    def scale_icon(self, icon_name: str, target_size: int) -> QPixmap:
        """
        Load and scale an icon to the appropriate size

        Args:
            icon_name: Name of the icon we want
            target_size: Desired size in pixels

        Returns:
            Scaled QPixmap of the icon
        """
        scaled_size = int(target_size * self._zoom_factor)
        cache_key = (icon_name, scaled_size)

        if cache_key in self._scaled_icon_cache:
            return self._scaled_icon_cache[cache_key]

        renderer = QSvgRenderer(self.get_icon_path(icon_name))
        pixmap = QPixmap(scaled_size, scaled_size)
        pixmap.fill(Qt.GlobalColor.transparent)
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        renderer.render(painter)
        painter.end()

        self._scaled_icon_cache[cache_key] = pixmap
        return pixmap

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
        return QColor(self._colors[role][self._resolve_color_mode()])

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
        return self._colors[role][self._resolve_color_mode()]

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

    def font_ligatures(self) -> bool:
        """Get whether font ligatures are enabled for monospace text."""
        return self._font_ligatures

    def set_font_ligatures(self, enabled: bool) -> None:
        """Set whether font ligatures are enabled for monospace text."""
        if enabled != self._font_ligatures:
            self._font_ligatures = enabled
            self._initialize_highlights()
            self.style_changed.emit()

    def color_mode(self) -> ColorMode:
        """Get the resolved (effective) color mode, always LIGHT or DARK."""
        return self._resolve_color_mode()

    def _resolve_color_mode(self) -> ColorMode:
        """
        Resolve the effective (LIGHT or DARK) color mode.

        When the user preference is SYSTEM, the OS color scheme is queried.
        Qt.ColorScheme.Unknown is treated as LIGHT.

        Returns:
            ColorMode.LIGHT or ColorMode.DARK
        """
        if self._color_mode != ColorMode.SYSTEM:
            return self._color_mode

        scheme = QGuiApplication.styleHints().colorScheme()
        if scheme == Qt.ColorScheme.Dark:
            return ColorMode.DARK

        return ColorMode.LIGHT

    def _on_system_color_scheme_changed(self) -> None:
        """Handle OS-level color scheme changes when in SYSTEM mode."""
        if self._color_mode == ColorMode.SYSTEM:
            self._initialize_highlights()
            self._initialize_proportional_highlights()
            self._scaled_icon_cache.clear()
            self.style_changed.emit()

    def user_color_mode(self) -> ColorMode:
        """Get the user's color mode preference, which may be SYSTEM, LIGHT, or DARK."""
        return self._color_mode

    def set_color_mode(self, mode: ColorMode) -> None:
        """
        Set the color mode and update application styles.

        Clears both icon caches since icon paths include the theme name
        (e.g., 'arrow-dark.svg' vs 'arrow-light.svg').

        Args:
            mode: The ColorMode to switch to
        """
        if mode != self._color_mode:
            self._color_mode = mode
            self._initialize_highlights()
            self._initialize_proportional_highlights()
            self._scaled_icon_cache.clear()
            self.style_changed.emit()

    def zoom_factor(self) -> float:
        """Current zoom scaling factor."""
        return self._zoom_factor

    def set_zoom(self, factor: float) -> None:
        """
        Set new zoom factor and update application styles.

        Clears the scaled icon cache since all icons need to be re-rendered
        at the new zoom level.

        Args:
            factor: New zoom factor to apply (clamped between 0.5 and 2.0)
        """
        new_factor = max(0.5, min(2.0, factor))
        if new_factor != self._zoom_factor:
            self._zoom_factor = new_factor
            print(f"zoom_factor changed to {new_factor}")
            self._scaled_icon_cache.clear()  # Invalidate scaled icons
            self.style_changed.emit()

    def get_space_width(self) -> float:
        """Get the width of a space character"""
        font = QFont(self._code_font_families)
        if not self._font_ligatures:
            font.setStyleStrategy(QFont.StyleStrategy.PreferNoShaping)

        font.setPointSizeF(self.base_font_size() * self._zoom_factor)
        font_metrics = QFontMetricsF(font)
        space_width = font_metrics.horizontalAdvance('        ') / 8
        return space_width

    def monospace_font_families(self) -> List[str]:
        """Get the standard monospace font family fallback sequence."""
        return self._code_font_families

    def make_monospace_font(self) -> QFont:
        """
        Create a monospace QFont configured with current style settings.

        Applies PreferNoShaping when font ligatures are disabled so that all
        monospace widgets get consistent ligature behaviour from a single place.
        """
        font = QFont()
        font.setFamilies(self._code_font_families)
        font.setFixedPitch(True)
        font.setPointSizeF(self.base_font_size() * self._zoom_factor)
        if not self._font_ligatures:
            font.setStyleStrategy(QFont.StyleStrategy.PreferNoShaping)

        return font

    def make_monospace_font_no_ligatures(self) -> QFont:
        """
        Create a monospace QFont with ligatures unconditionally disabled.

        Used by the terminal, which is a character-cell renderer where ligatures
        would break glyph alignment regardless of the user preference.
        """
        font = QFont()
        font.setFamilies(self._code_font_families)
        font.setFixedPitch(True)
        font.setPointSizeF(self.base_font_size() * self._zoom_factor)
        font.setStyleStrategy(QFont.StyleStrategy.PreferNoShaping)
        return font

    def proportional_font_families(self) -> List[str]:
        """Get the standard proportional font family fallback sequence."""
        return self._proportional_font_families

    def message_bubble_spacing(self) -> float:
        """Get the number of pixels to use in message bubble spacing."""
        return 10.0

    def nice_tab_width(self) -> float:
        """Get the ideal width for tabs to balance information density and readability."""
        return 1024.0

    def scale(self, value: int | float) -> int:
        """Scale a UI metric by the current zoom factor."""
        return round(value * self._zoom_factor)

    def spacing(self, units: int = 1) -> int:
        """Get a standard spacing value."""
        return self.scale(4 * units)

    def radius(self, size: str = "control") -> int:
        """Get a standard border radius."""
        match size:
            case "panel":
                return self.scale(8)

            case "surface":
                return self.scale(6)

            case _:
                return self.scale(4)

    def control_height(self) -> int:
        """Get the standard interactive control height."""
        return self.scale(32)

    def row_height(self) -> int:
        """Get the standard list/tree row height."""
        return self.scale(28)

    def tab_icon_size(self) -> int:
        """Get the standard tab icon size."""
        return self.scale(16)

    def tab_padding(self) -> int:
        """Get the standard horizontal tab padding."""
        return self.spacing(2)

    def tab_spacing(self) -> int:
        """Get the standard spacing between tab label elements."""
        return self.spacing(2)

    def tab_scroller_width(self) -> int:
        """Get the reserved width for tab scroll controls."""
        return self.scale(64)

    def tab_scroller_button_size(self) -> int:
        """Get the tab scroll control button size."""
        return self.control_height()

    def tool_button_size(self) -> int:
        """Get the standard square tool button size."""
        return self.scale(20)

    def switch_height(self) -> int:
        """Get the compact switch height."""
        return max(20, self.scale(20))

    def switch_width(self) -> int:
        """Get the compact switch width."""
        return round(self.switch_height() * 2.1)

    def switch_knob_inset(self) -> int:
        """Get the compact switch knob inset."""
        return max(2, self.scale(3))

    def splitter_width(self) -> int:
        """Get the standard splitter handle width."""
        return max(1, self.scale(1))

    def active_indicator_width(self) -> int:
        """Get the standard active/focus indicator width."""
        return max(2, self.scale(3))

    def compact_active_indicator_width(self) -> int:
        """Get the compact active/focus indicator width."""
        return max(1, self.scale(2))

    def panel_margin(self) -> int:
        """Get the standard margin around framed panes."""
        return self.scale(4)

    def get_button_stylesheet(self, selector: str = "QPushButton") -> str:
        """Get shared push-button styling."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()

        return f"""
            {selector} {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            {selector}:hover {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            {selector}:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            {selector}:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            {selector}[recommended="true"] {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {self.get_color_str(ColorRole.TEXT_RECOMMENDED)};
            }}
            {selector}[recommended="true"]:hover {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            {selector}[recommended="true"]:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
            {selector}[recommended="false"] {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)};
                color: {self.get_color_str(ColorRole.TEXT_RECOMMENDED)};
            }}
            {selector}[recommended="false"]:hover {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER)};
            }}
            {selector}[recommended="false"]:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED)};
            }}
        """

    def get_tool_button_stylesheet(self, selector: str = "QToolButton") -> str:
        """Get shared icon/tool-button styling."""
        size = self.tool_button_size()
        radius = self.radius()
        padding = self.spacing(1)

        return f"""
            {selector} {{
                background-color: transparent;
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: {radius}px;
                padding: {padding}px;
                min-width: {size}px;
                min-height: {size}px;
            }}
            {selector}:hover {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            {selector}:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            {selector}:disabled {{
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
                background-color: transparent;
            }}
            {selector}:checked {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {self.get_color_str(ColorRole.TEXT_RECOMMENDED)};
            }}
        """

    def get_toggle_button_stylesheet(self, selector: str = "QToolButton#toggleButton") -> str:
        """Get shared compact toggle-button styling."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()
        size = self.tool_button_size()
        radius = self.radius()
        padding = self.spacing(1)

        return f"""
            {selector} {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: {radius}px;
                padding: {padding}px;
                min-width: {size}px;
                min-height: {size}px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            {selector}:hover {{
                background-color: {self.get_color_str(ColorRole.TEXT_FOUND_DIM)};
            }}
            {selector}:pressed {{
                background-color: {self.get_color_str(ColorRole.TEXT_FOUND)};
            }}
            {selector}:checked {{
                background-color: {self.get_color_str(ColorRole.TEXT_FOUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            {selector}:checked:hover {{
                background-color: {self.get_color_str(ColorRole.TEXT_FOUND_DIM)};
            }}
            {selector}:checked:pressed {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            {selector}:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
        """

    def get_text_input_stylesheet(self, selector: str) -> str:
        """Get shared text input styling."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()

        return f"""
            {selector} {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {int(base_font_size * zoom_factor)}pt;
            }}
            {selector}:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
        """

    def get_checkbox_stylesheet(self, selector: str = "QCheckBox") -> str:
        """Get shared checkbox styling."""
        zoom_factor = self.zoom_factor()

        return f"""
            {selector} {{
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 0px;
                margin: 0px;
            }}
            {selector}::indicator {{
                width: {int(18 * zoom_factor)}px;
                height: {int(18 * zoom_factor)}px;
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            }}
            {selector}::indicator:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
            }}
            {selector}::indicator:checked {{
                image: url({self.get_icon_path('check')});
            }}
            {selector}:disabled {{
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
        """

    def get_combo_box_stylesheet(self, selector: str = "QComboBox") -> str:
        """Get shared combo-box styling (MUI outlined variant)."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()
        font_pt = int(base_font_size * zoom_factor)
        arrow_size = max(10, int(14 * zoom_factor))
        drop_width = max(28, int(36 * zoom_factor))
        pad_v = max(6, int(8 * zoom_factor))
        pad_h = max(10, int(12 * zoom_factor))
        is_button_combo = "QPushButton" in selector
        is_search_button = "SearchButton" in selector
        is_rtl_button = "ButtonRtl" in selector
        trigger_icon_pad = max(22, int(26 * zoom_factor))
        trigger_text_pad = max(6, int(8 * zoom_factor))
        left_pad = (
            trigger_text_pad if is_search_button else
            trigger_icon_pad if is_rtl_button else
            trigger_text_pad if is_button_combo else
            pad_h
        )
        right_pad = (
            left_pad if is_search_button else
            trigger_text_pad if is_rtl_button else
            trigger_icon_pad if is_button_combo else
            drop_width
        )
        item_pad_v = max(4, int(6 * zoom_factor))
        item_pad_h = max(10, int(12 * zoom_factor))

        border_normal = self.get_color_str(ColorRole.MENU_BORDER)
        border_hover  = self.get_color_str(ColorRole.TEXT_INACTIVE)
        border_active = self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)
        bg            = self.get_color_str(ColorRole.BACKGROUND_DIALOG)
        bg_disabled   = self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)
        text          = self.get_color_str(ColorRole.TEXT_PRIMARY)
        text_disabled = self.get_color_str(ColorRole.TEXT_DISABLED)
        popup_bg      = self.get_color_str(ColorRole.MENU_BACKGROUND)
        popup_border  = self.get_color_str(ColorRole.MENU_BORDER)
        item_hover    = self.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        item_selected = self.get_color_str(ColorRole.TEXT_SELECTED)

        return f"""
            {selector} {{
                background-color: {bg};
                color: {text};
                border: 1px solid {border_normal};
                border-radius: 6px;
                padding: {pad_v}px {right_pad}px {pad_v}px {left_pad}px;
                margin: 0px;
                font-size: {font_pt}pt;
                text-align: left;
                selection-background-color: transparent;
                selection-color: {text};
            }}
            {selector}:hover {{
                border-color: {border_hover};
                background-color: {popup_bg};
            }}
            {selector}:on {{
                border-color: {border_active};
                border-width: 2px;
                padding: {pad_v - 1}px {right_pad - 1}px {pad_v - 1}px {left_pad - 1}px;
            }}
            {selector}:disabled {{
                background-color: {bg_disabled};
                color: {text_disabled};
                border-color: {border_normal};
            }}
            {selector} QLineEdit {{
                background-color: transparent;
                border: none;
                padding: 0px;
                margin: 0px;
                color: {text};
                selection-background-color: {item_selected};
            }}
            {selector}::drop-down {{
                subcontrol-origin: border;
                subcontrol-position: right center;
                border: none;
                width: {drop_width}px;
            }}
            {selector}::down-arrow {{
                image: url({self.get_icon_path("arrow-down")});
                width: {arrow_size}px;
                height: {arrow_size}px;
            }}
            {selector}::down-arrow:on {{
                image: url({self.get_icon_path("arrow-up")});
            }}
            {selector}::down-arrow:disabled {{
                image: none;
            }}
            {selector} QAbstractItemView {{
                background-color: {popup_bg};
                color: {text};
                border: 1px solid {popup_border};
                border-radius: 6px;
                padding: 0px;
                margin: 0px;
                outline: none;
                show-decoration-selected: 1;
            }}
            {selector} QAbstractItemView::item {{
                padding: {item_pad_v}px {item_pad_h}px;
                border: none;
                color: {text};
            }}
            {selector} QAbstractItemView::item:hover {{
                background-color: {item_hover};
            }}
            {selector} QAbstractItemView::item:selected {{
                background-color: {item_selected};
                color: {text};
            }}
            {selector} QAbstractItemView::item:disabled {{
                color: {text_disabled};
                background-color: transparent;
                padding-top: {max(3, item_pad_v // 2)}px;
                padding-bottom: {max(3, item_pad_v // 2)}px;
            }}
        """

    def get_combo_popup_stylesheet(self) -> str:
        """Stylesheet for the QCompleter popup (a top-level QListView)."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()
        font_pt = int(base_font_size * zoom_factor)
        search_pad_v = max(5, int(6 * zoom_factor))
        item_pad_h = max(10, int(12 * zoom_factor))

        popup_bg      = self.get_color_str(ColorRole.MENU_BACKGROUND)
        popup_border  = self.get_color_str(ColorRole.MENU_BORDER)
        surface_bg    = self.get_color_str(ColorRole.BACKGROUND_TERTIARY)
        text          = self.get_color_str(ColorRole.TEXT_PRIMARY)
        text_disabled = self.get_color_str(ColorRole.TEXT_DISABLED)
        item_hover    = self.get_color_str(ColorRole.COMBO_ITEM_HOVER)
        item_selected = self.get_color_str(ColorRole.TEXT_SELECTED)
        focus_border  = self.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)
        scrollbar_bg  = self.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)
        scrollbar_h   = self.get_color_str(ColorRole.SCROLLBAR_HANDLE)

        return f"""
            QLineEdit#SettingsComboPopupSearch {{
                background-color: {surface_bg};
                color: {text};
                border: 1px solid {popup_border};
                border-radius: 5px;
                padding: {search_pad_v}px {item_pad_h}px;
                margin: 0px;
                font-size: {font_pt}pt;
                selection-background-color: {item_selected};
            }}
            QLineEdit#SettingsComboPopupSearch:focus {{
                border-color: {focus_border};
            }}
            QListView, QListWidget#SettingsComboPopupList {{
                background-color: {popup_bg};
                color: {text};
                border: none;
                border-radius: 0px;
                padding: 0px;
                margin: 0px;
                outline: none;
                font-size: {font_pt}pt;
            }}
            QListView::item, QListWidget#SettingsComboPopupList::item {{
                padding: 0px {item_pad_h}px;
                border: none;
                border-radius: 4px;
                color: {text};
            }}
            QListView::item:hover, QListWidget#SettingsComboPopupList::item:hover {{
                background-color: {item_hover};
            }}
            QListView::item:selected, QListWidget#SettingsComboPopupList::item:selected {{
                background-color: {item_selected};
                color: {text};
            }}
            QListView::item:disabled, QListWidget#SettingsComboPopupList::item:disabled {{
                color: {text_disabled};
                background-color: transparent;
            }}
            QScrollBar:vertical {{
                background-color: {scrollbar_bg};
                width: 8px;
                border-radius: 4px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {scrollbar_h};
                min-height: 20px;
                border-radius: 4px;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
        """

    def get_spin_box_stylesheet(self) -> str:
        """Get shared integer spin-box styling."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()

        return f"""
            QSpinBox {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QSpinBox:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QSpinBox::up-button, QSpinBox::down-button {{
                border: none;
                width: 20px;
            }}
            QSpinBox::up-arrow {{
                image: url({self.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QSpinBox::up-arrow:disabled, QSpinBox::up-arrow:off {{
                image: none;
            }}
            QSpinBox::down-arrow {{
                image: url({self.get_icon_path('arrow-down')});
                width: 12px;
                height: 12px;
            }}
            QSpinBox::down-arrow:disabled, QSpinBox::down-arrow:off {{
                image: none;
            }}
        """

    def get_double_spin_box_stylesheet(self) -> str:
        """Get shared double spin-box styling."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()

        return f"""
            QDoubleSpinBox {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QDoubleSpinBox:disabled {{
                background-color: {self.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QDoubleSpinBox::up-button, QDoubleSpinBox::down-button {{
                border: none;
                width: 20px;
            }}
            QDoubleSpinBox::up-arrow {{
                image: url({self.get_icon_path('arrow-up')});
                width: 12px;
                height: 12px;
            }}
            QDoubleSpinBox::up-arrow:disabled, QDoubleSpinBox::up-arrow:off {{
                image: none;
            }}
            QDoubleSpinBox::down-arrow {{
                image: url({self.get_icon_path('arrow-down')});
                width: 12px;
                height: 12px;
            }}
            QDoubleSpinBox::down-arrow:disabled, QDoubleSpinBox::down-arrow:off {{
                image: none;
            }}
        """

    def create_menu(self, parent: QWidget) -> QMenu:
        """Create a styled QMenu with correct attributes for all platforms."""
        menu = QMenu(parent)
        menu.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        menu.setWindowFlag(Qt.WindowType.FramelessWindowHint, True)
        menu.setStyleSheet(self.get_menu_stylesheet())
        return menu

    def get_menu_stylesheet(self) -> str:
        """Apply styling to a specific menu."""
        zoom_factor = self.zoom_factor()
        base_font_size = self.base_font_size()
        return f"""
            QMenu {{
                background-color: {self.get_color_str(ColorRole.MENU_BACKGROUND)};
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-color: {self.get_color_str(ColorRole.MENU_BORDER)};
                border-width: 1px;
                border-style: solid;
                border-radius: 8px;
                margin: 0px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QMenu::item {{
                margin: 2px;
                padding: 4px 8px 4px 8px;
                border-radius: 4px;
            }}
            QMenu::item:disabled {{
                color: {self.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QMenu::item:selected {{
                background-color: {self.get_color_str(ColorRole.MENU_HOVER)}
            }}
        """

    def get_scrollbar_size(self) -> int:
        """Get the standard scrollbar thickness in pixels."""
        return 10

    def get_scrollbar_stylesheet(self, selector: str = "QScrollBar") -> str:
        """Get the shared scrollbar stylesheet for the given selector prefix."""
        scrollbar_size = self.get_scrollbar_size()
        handle_minimum = 18

        return f"""
            {selector}:vertical {{
                background-color: {self.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: {scrollbar_size}px;
            }}
            {selector}:horizontal {{
                background-color: {self.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                height: {scrollbar_size}px;
            }}
            {selector}::handle:vertical {{
                background-color: {self.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: {handle_minimum}px;
            }}
            {selector}::handle:horizontal {{
                background-color: {self.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: {handle_minimum}px;
            }}
            {selector}::add-page:vertical, {selector}::sub-page:vertical,
            {selector}::add-page:horizontal, {selector}::sub-page:horizontal {{
                background: none;
            }}
            {selector}::add-line:vertical, {selector}::sub-line:vertical {{
                height: 0px;
            }}
            {selector}::add-line:horizontal, {selector}::sub-line:horizontal {{
                width: 0px;
            }}
        """

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
                background-color: transparent;
                border: none;
            }}

            QScrollArea > QWidget > QWidget {{
                background-color: transparent;
                border: none;
            }}

            /* Labels */
            QLabel {{
                color: {self.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                border-radius: 4px;
                padding: 0px;
                margin: 0px;
                font-size: {base_font_size * zoom_factor}pt;
            }}

            {self.get_text_input_stylesheet("QLineEdit")}
            {self.get_text_input_stylesheet("#SettingsTextArea")}
            {self.get_checkbox_stylesheet()}
            {self.get_combo_box_stylesheet()}
            {self.get_spin_box_stylesheet()}
            {self.get_double_spin_box_stylesheet()}
            {self.get_button_stylesheet()}

            QFrame#SettingsSeparator {{
                background-color: {self.get_color_str(ColorRole.BACKGROUND_TERTIARY)};
                border: none;
                max-height: 1px;
                min-height: 1px;
            }}

            {self.get_scrollbar_stylesheet()}
        """
