"""Style manager for handling application-wide style and zoom settings.

Implements a singleton pattern to maintain consistent styling across components.
Provides signals for style changes and utilities for scaled size calculations.
"""

from PySide6.QtCore import QObject, Signal, QOperatingSystemVersion
from PySide6.QtGui import QFontDatabase


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
