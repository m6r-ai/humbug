"""Style manager for handling application-wide style and zoom settings.

Implements a singleton pattern to maintain consistent styling across components.
Provides signals for style changes and utilities for scaled size calculations.
"""

from enum import Enum, auto
from typing import Dict

from PySide6.QtCore import QObject, Signal, QOperatingSystemVersion
from PySide6.QtGui import (
    QTextCharFormat, QFontDatabase, QGuiApplication, QColor, QFontMetricsF, QFont
)

from humbug.gui.color_role import ColorRole


class ColorMode(Enum):
    """Enumeration for color theme modes."""
    LIGHT = auto()
    DARK = auto()


class StyleManager(QObject):
    """Singleton manager for application-wide style settings.

    Handles zoom factor management and style updates across the application.
    Emits signals when zoom level changes to notify dependent components.

    Attributes:
        style_changed (Signal): Emitted when style changes, passing new zoom factor
        _instance (StyleManager): Singleton instance
        _zoom_factor (float): Current zoom scaling factor
        _initialized (bool): Tracks initialization state of QObject base
    """

    style_changed = Signal(float)
    _instance = None

    def __new__(cls):
        """Create or return singleton instance."""
        if cls._instance is None:
            cls._instance = super(StyleManager, cls).__new__(cls)
        return cls._instance

    def __init__(self):
        """Initialize QObject base class if not already done."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._zoom_factor = 1.0
            self._base_font_size = self._determine_base_font_size()
            self._initialized = True
            self._color_mode = ColorMode.DARK  # Default to dark mode
            self._colors: Dict[ColorRole, Dict[ColorMode, str]] = self._initialize_colors()
            self._highlights: Dict[str, QTextCharFormat] = {}

            self._code_font_families = ["Menlo", "Monaco", "Courier New", "monospace"]
            self._initialize_highlights()

    def _initialize_colors(self) -> Dict[ColorRole, Dict[ColorMode, str]]:
        """Initialize the application colors for both light and dark modes."""
        return {
            # Background colors
            ColorRole.BACKGROUND_PRIMARY: {
                ColorMode.DARK: "#080808",
                ColorMode.LIGHT: "#ffffff"
            },
            ColorRole.BACKGROUND_SECONDARY: {
                ColorMode.DARK: "#2d2d2d",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.BACKGROUND_INPUT: {
                ColorMode.DARK: "#202020",
                ColorMode.LIGHT: "#ffffff"
            },

            # Message backgrounds
            ColorRole.MESSAGE_USER: {
                ColorMode.DARK: "#303030",
                ColorMode.LIGHT: "#e8e8e8"
            },
            ColorRole.MESSAGE_AI: {
                ColorMode.DARK: "#181818",
                ColorMode.LIGHT: "#f8f8f8"
            },
            ColorRole.MESSAGE_SYSTEM: {
                ColorMode.DARK: "#1a3a1a",
                ColorMode.LIGHT: "#e8ffe8"
            },
            ColorRole.MESSAGE_ERROR: {
                ColorMode.DARK: "#3a1a1a",
                ColorMode.LIGHT: "#ffe8e8"
            },
            ColorRole.MESSAGE_HEADER: {
                ColorMode.DARK: "#2a3544",
                ColorMode.LIGHT: "#edf2f7"
            },

            # UI elements
            ColorRole.TAB_ACTIVE: {
                ColorMode.DARK: "#242424",
                ColorMode.LIGHT: "#ffffff"
            },
            ColorRole.TAB_INACTIVE: {
                ColorMode.DARK: "#1c1c1c",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.TAB_HOVER: {
                ColorMode.DARK: "#242424",
                ColorMode.LIGHT: "#f8f8f8"
            },
            ColorRole.MENU_BACKGROUND: {
                ColorMode.DARK: "#2d2d2d",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.MENU_HOVER: {
                ColorMode.DARK: "#3d3d3d",
                ColorMode.LIGHT: "#e0e0e0"
            },
            ColorRole.SCROLLBAR_BACKGROUND: {
                ColorMode.DARK: "#2d2d2d",
                ColorMode.LIGHT: "#f0f0f0"
            },
            ColorRole.SCROLLBAR_HANDLE: {
                ColorMode.DARK: "#404040",
                ColorMode.LIGHT: "#c0c0c0"
            },
            ColorRole.STATUS_BAR: {
                ColorMode.DARK: "#d3d3d3",
                ColorMode.LIGHT: "#404040"
            },

            # Text colors
            ColorRole.TEXT_PRIMARY: {
                ColorMode.DARK: "#ffffff",
                ColorMode.LIGHT: "#000000"
            },
            ColorRole.DISABLED_TEXT: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#a0a0a0"
            },
            ColorRole.SELECTED_TEXT: {
                ColorMode.DARK: "#606060",
                ColorMode.LIGHT: "#e0e0e0"
            },

            # Close button states
            ColorRole.CLOSE_BUTTON_HOVER: {
                ColorMode.DARK: "#ff4444",
                ColorMode.LIGHT: "#ff4444"
            },

            # Syntax highlighting
            ColorRole.CODE_BLOCK_BACKGROUND: {
                ColorMode.DARK: "#141414",
                ColorMode.LIGHT: "#f8f8f8"
            },
            ColorRole.SYNTAX_CODE: {
                ColorMode.DARK: "#804040",
                ColorMode.LIGHT: "#c04040"
            },
            ColorRole.SYNTAX_COMMENT: {
                ColorMode.DARK: "#68d068",
                ColorMode.LIGHT: "#408040"
            },
            ColorRole.SYNTAX_CSS_AT_RULE: {
                ColorMode.DARK: "#ffc0eb",
                ColorMode.LIGHT: "#c000a0"
            },
            ColorRole.SYNTAX_ELEMENT: {
                ColorMode.DARK: "#90e0e8",
                ColorMode.LIGHT: "#0080a0"
            },
            ColorRole.SYNTAX_ERROR: {
                ColorMode.DARK: "#ff0000",
                ColorMode.LIGHT: "#ff0000"
            },
            ColorRole.SYNTAX_FUNCTION_OR_METHOD: {
                ColorMode.DARK: "#e0e080",
                ColorMode.LIGHT: "#806000"
            },
            ColorRole.SYNTAX_HEADING: {
                ColorMode.DARK: "#f06060",
                ColorMode.LIGHT: "#c04040"
            },
            ColorRole.SYNTAX_HTML_ATTRIBUTE: {
                ColorMode.DARK: "#90e0e8",
                ColorMode.LIGHT: "#0080a0"
            },
            ColorRole.SYNTAX_HTML_DOCTYPE: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#606060"
            },
            ColorRole.SYNTAX_HTML_TAG: {
                ColorMode.DARK: "#ffc0eb",
                ColorMode.LIGHT: "#c000a0"
            },
            ColorRole.SYNTAX_IDENTIFIER: {
                ColorMode.DARK: "#80b0f0",
                ColorMode.LIGHT: "#0060c0"
            },
            ColorRole.SYNTAX_KEYWORD: {
                ColorMode.DARK: "#ffc0eb",
                ColorMode.LIGHT: "#c000a0"
            },
            ColorRole.SYNTAX_NUMBER: {
                ColorMode.DARK: "#c08040",
                ColorMode.LIGHT: "#804000"
            },
            ColorRole.SYNTAX_OPERATOR: {
                ColorMode.DARK: "#c0c0c0",
                ColorMode.LIGHT: "#404040"
            },
            ColorRole.SYNTAX_PREPROCESSOR: {
                ColorMode.DARK: "#808080",
                ColorMode.LIGHT: "#606060"
            },
            ColorRole.SYNTAX_REGEXP: {
                ColorMode.DARK: "#c87050",
                ColorMode.LIGHT: "#a04020"
            },
            ColorRole.SYNTAX_STRING: {
                ColorMode.DARK: "#f06060",
                ColorMode.LIGHT: "#c04040"
            },
            ColorRole.SYNTAX_TEXT: {
                ColorMode.DARK: "#d0d0d0",
                ColorMode.LIGHT: "#404040"
            }
        }

    def _initialize_highlights(self):
        # Mapping from token type to colour
        colour_mapping = {
            "COMMENT": ColorRole.SYNTAX_COMMENT,
            "CSS_AT_RULE": ColorRole.SYNTAX_CSS_AT_RULE,
            "ELEMENT": ColorRole.SYNTAX_ELEMENT,
            "ERROR": ColorRole.SYNTAX_ERROR,
            "FUNCTION_OR_METHOD": ColorRole.SYNTAX_FUNCTION_OR_METHOD,
            "HEADING": ColorRole.SYNTAX_HEADING,
            "HTML_ATTRIBUTE": ColorRole.SYNTAX_HTML_ATTRIBUTE,
            "HTML_DOCTYPE": ColorRole.SYNTAX_HTML_DOCTYPE,
            "HTML_TAG": ColorRole.SYNTAX_HTML_TAG,
            "IDENTIFIER": ColorRole.SYNTAX_IDENTIFIER,
            "KEYWORD": ColorRole.SYNTAX_KEYWORD,
            "NUMBER": ColorRole.SYNTAX_NUMBER,
            "OPERATOR": ColorRole.SYNTAX_OPERATOR,
            "PREPROCESSOR": ColorRole.SYNTAX_PREPROCESSOR,
            "REGEXP": ColorRole.SYNTAX_REGEXP,
            "STRING": ColorRole.SYNTAX_STRING,
            "TEXT": ColorRole.SYNTAX_TEXT,
            "WHITESPACE": ColorRole.SELECTED_TEXT
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

    def get_color(self, role: ColorRole) -> QColor:
        """Get a color for a specific role.

        Args:
            role: The ColorRole to look up

        Returns:
            QColor: The color for the specified role

        Raises:
            KeyError: If no color is defined for the role
        """
        return QColor(self._colors[role][self._color_mode])

    def get_color_str(self, role: ColorRole) -> str:
        """Get a color string for a specific role.

        Args:
            role: The ColorRole to look up

        Returns:
            str: The color string (hex format) for the specified role

        Raises:
            KeyError: If no color is defined for the role
        """
        return self._colors[role][self._color_mode]

    def get_highlight(self, token_type: str) -> QTextCharFormat:
        if token_type not in self._highlights:
            print(f"token type {token_type} not mapped")

        return self._highlights.get(token_type, self._error_highlight)

    def _determine_base_font_size(self) -> float:
        """Determine the default system font size based on the operating system.

        Returns:
            int: Base font size in points.
        """
        # Get current OS
        os_type = QOperatingSystemVersion.current()

        # Get system default font
        system_font = QFontDatabase.systemFont(QFontDatabase.GeneralFont)
        system_size = system_font.pointSize()

        # Apply OS-specific adjustments if system detection fails
        if system_size > 0:
            return system_size

        if os_type.type() == QOperatingSystemVersion.MacOS:
            # macOS typically uses 13pt as default
            return 13

        if os_type.type() == QOperatingSystemVersion.Windows:
            # Windows typically uses 9pt as default
            return 9

        # Linux typically uses 10pt as default
        return 10

    @property
    def base_font_size(self) -> float:
        """Get the base font size for the current system."""
        return self._base_font_size

    @property
    def color_mode(self) -> ColorMode:
        """Get the current color mode."""
        return self._color_mode

    def set_color_mode(self, mode: ColorMode):
        """Set the color mode and update application styles.

        Args:
            mode: The ColorMode to switch to
        """
        if mode != self._color_mode:
            self._color_mode = mode
            self._initialize_highlights()  # Reinitialize highlights with new colors
            self.style_changed.emit(self._zoom_factor)  # Trigger style update

    @property
    def zoom_factor(self) -> float:
        """Current zoom scaling factor."""
        return self._zoom_factor

    def set_zoom(self, factor: float):
        """Set new zoom factor and update application styles.

        Args:
            factor: New zoom factor to apply (clamped between 0.5 and 2.0)
        """
        new_factor = max(0.5, min(2.0, factor))
        if new_factor != self._zoom_factor:
            self._zoom_factor = new_factor
            self.style_changed.emit(self._zoom_factor)

    def get_scaled_size(self, base_size: float) -> float:
        """Calculate scaled size based on current zoom factor.

        Args:
            base_size: Original size to scale

        Returns:
            Scaled size adjusted for current zoom factor
        """
        return base_size * self._zoom_factor

    def get_space_width(self) -> float:
        font = QFont(["Menlo", "Monaco", "Courier New", "monospace"])
        font.setPointSizeF(self._base_font_size * self._zoom_factor)
        font_metrics = QFontMetricsF(font)
        space_width = font_metrics.horizontalAdvance('        ') / 8
        return space_width

    def points_to_pixels(self, points: float) -> float:
        """Convert point size to pixels based on device pixel ratio.

        Args:
            points: Font size in points

        Returns:
            Font size in pixels
        """
        # Get the primary screen's logical DPI
        screen = QGuiApplication.primaryScreen()
        if not screen:
            return points * 1.333333  # Fallback to standard 96 DPI (72 * 4/3)

        # Convert points to pixels using the screen's logical DPI
        logical_dpi = screen.logicalDotsPerInchY()
        return (points * logical_dpi) / 72.0

    def pixels_to_points(self, pixels: float) -> float:
        """Convert pixel size to points based on device pixel ratio.

        Args:
            pixels: Size in pixels

        Returns:
            Size in points
        """
        screen = QGuiApplication.primaryScreen()
        if not screen:
            return pixels * 0.75  # Fallback to standard 96 DPI (72/96)

        logical_dpi = screen.logicalDotsPerInch()
        return (pixels * 72.0) / logical_dpi
