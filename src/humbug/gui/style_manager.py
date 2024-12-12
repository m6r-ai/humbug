"""Style manager for handling application-wide style and zoom settings.

Implements a singleton pattern to maintain consistent styling across components.
Provides signals for style changes and utilities for scaled size calculations.
"""

from typing import Dict

from PySide6.QtCore import QObject, Signal, QOperatingSystemVersion
from PySide6.QtGui import QFontDatabase, QGuiApplication, QColor

from humbug.gui.color_role import ColorRole


class StyleManager(QObject):
    """Singleton manager for application-wide style settings.

    Handles zoom factor management and style updates across the application.
    Emits signals when zoom level changes to notify dependent components.

    Attributes:
        zoom_changed (Signal): Emitted when zoom factor changes, passing new factor
        _instance (StyleManager): Singleton instance
        _zoom_factor (float): Current zoom scaling factor
        _initialized (bool): Tracks initialization state of QObject base
    """

    zoom_changed = Signal(float)
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
            self._colors: Dict[ColorRole, str] = self._initialize_colors()

    def _initialize_colors(self) -> Dict[ColorRole, str]:
        """Initialize the application colors."""
        return {
            # Background colors
            ColorRole.BACKGROUND_PRIMARY: "#080808",
            ColorRole.BACKGROUND_SECONDARY: "#2d2d2d",
            ColorRole.BACKGROUND_INPUT: "#202020",

            # Message backgrounds
            ColorRole.MESSAGE_USER: "#303030",
            ColorRole.MESSAGE_AI: "#181818",
            ColorRole.MESSAGE_SYSTEM: "#1a3a1a",
            ColorRole.MESSAGE_ERROR: "#3a1a1a",
            ColorRole.MESSAGE_HEADER: "#2a3544",

            # Code blocks
            ColorRole.CODE_BLOCK_BACKGROUND: "#2d2d2d",

            # UI elements
            ColorRole.TAB_ACTIVE: "#242424",
            ColorRole.TAB_INACTIVE: "#1c1c1c",
            ColorRole.TAB_HOVER: "#242424",
            ColorRole.MENU_BACKGROUND: "#2d2d2d",
            ColorRole.MENU_HOVER: "#3d3d3d",
            ColorRole.SCROLLBAR_BACKGROUND: "#2d2d2d",
            ColorRole.SCROLLBAR_HANDLE: "#404040",
            ColorRole.STATUS_BAR: "#d3d3d3",

            # Text colors
            ColorRole.TEXT_PRIMARY: "#ffffff",
            ColorRole.SELECTED_TEXT: "#606060",

            # Close button states
            ColorRole.CLOSE_BUTTON_NORMAL: "#242424",
            ColorRole.CLOSE_BUTTON_HOVER: "#ff4444",

            # Syntax highlighting
            ColorRole.SYNTAX_CODE: "#804040"
        }

    def get_color(self, role: ColorRole) -> QColor:
        """Get a color for a specific role.

        Args:
            role: The ColorRole to look up

        Returns:
            QColor: The color for the specified role

        Raises:
            KeyError: If no color is defined for the role
        """
        return QColor(self._colors[role])

    def get_color_str(self, role: ColorRole) -> str:
        """Get a color string for a specific role.

        Args:
            role: The ColorRole to look up

        Returns:
            str: The color string (hex format) for the specified role

        Raises:
            KeyError: If no color is defined for the role
        """
        return self._colors[role]

    def _determine_base_font_size(self) -> int:
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
    def base_font_size(self) -> int:
        """Get the base font size for the current system."""
        return self._base_font_size

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
            self.zoom_changed.emit(self._zoom_factor)

    def get_scaled_size(self, base_size: int) -> int:
        """Calculate scaled size based on current zoom factor.

        Args:
            base_size: Original size to scale

        Returns:
            Scaled size adjusted for current zoom factor
        """
        return int(base_size * self._zoom_factor)

    def points_to_pixels(self, points: float) -> int:
        """Convert point size to pixels based on device pixel ratio.

        Args:
            points: Font size in points

        Returns:
            int: Font size in pixels
        """
        # Get the primary screen's logical DPI
        screen = QGuiApplication.primaryScreen()
        if not screen:
            return int(points * 1.333333)  # Fallback to standard 96 DPI (72 * 4/3)

        # Convert points to pixels using the screen's logical DPI
        logical_dpi = screen.logicalDotsPerInchY()
        return int((points * logical_dpi) / 72.0)

    def pixels_to_points(self, pixels: float) -> float:
        """Convert pixel size to points based on device pixel ratio.

        Args:
            pixels: Size in pixels

        Returns:
            float: Size in points
        """
        screen = QGuiApplication.primaryScreen()
        if not screen:
            return pixels * 0.75  # Fallback to standard 96 DPI (72/96)

        logical_dpi = screen.logicalDotsPerInch()
        return (pixels * 72.0) / logical_dpi
