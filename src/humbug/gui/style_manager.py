"""Style manager for handling application-wide style and zoom settings.

Implements a singleton pattern to maintain consistent styling across components.
Provides signals for style changes and utilities for scaled size calculations.
"""

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication


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
            self._initialized = True
    
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
