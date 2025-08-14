"""Updated file tree icon provider with theme-aware colors."""

import os
from typing import Dict, Tuple

from PySide6.QtCore import Qt, QFileInfo
from PySide6.QtGui import QIcon, QPainter, QPixmap, QColor
from PySide6.QtWidgets import QFileIconProvider
from PySide6.QtSvg import QSvgRenderer

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager, ColorMode


class MindspaceTreeIconProvider(QFileIconProvider):
    """Custom file icon provider with theme-aware scalable SVG icons."""

    def __init__(self) -> None:
        """Initialize the icon provider."""
        super().__init__()
        self._style_manager = StyleManager()
        self._cached_icons: Dict[str, QIcon] = {}

        # Initialize icon cache
        self._initialize_icons()

    def _initialize_icons(self) -> None:
        """Create and cache standard icons."""
        # Create path shapes for each icon type in SVG format
        self._svg_paths = {
            "folder": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M10 25 C10 25 35 25 40 25 C45 25 47 15 50 15 C53 15 90 15 90 15 L90 85 L10 85 L10 25"
                        fill="#4B89DC" stroke="none"/>
                </svg>
            ''',
            "file": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M20 10 L60 10 C60 10 60 30 60 30 C60 30 80 30 80 30 L80 90 L20 90 Z"
                        fill="none" stroke="currentColor" stroke-width="5"/>
                    <path d="M60 10 L80 30" stroke="currentColor" stroke-width="5" fill="none"/>
                </svg>
            ''',
            "conversation": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M15 15 C15 15 85 15 85 15 C85 15 85 55 85 55 C85 55 70 55 70 55 L50 80 L50 55 C50 55 15 55 15 55 Z"
                        fill="none" stroke="currentColor" stroke-width="5"/>
                </svg>
            ''',
            "metaphor": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M75 85 L71 45 L70.5 45 L55 84.5 L45 84.5 L29.5 45 L29 45 L25 85
                            L10 85 L16 20 L33 20 L50 60 L67 20 L84 20 L90 85 Z"
                        fill="currentColor"/>
                </svg>
            ''',
            "code": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M20 10 L60 10 C60 10 60 30 60 30 C60 30 80 30 80 30 L80 90 L20 90 Z"
                        fill="none" stroke="currentColor" stroke-width="5"/>
                    <path d="M60 10 L80 30" stroke="currentColor" stroke-width="5" fill="none"/>
                    <path d="M30 30 L50 30" stroke="accentColor" stroke-width="5"/>
                    <path d="M30 50 L70 50" stroke="accentColor" stroke-width="5"/>
                    <path d="M30 70 L70 70" stroke="accentColor" stroke-width="5"/>
                </svg>
            ''',
            "text": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M20 10 L60 10 C60 10 60 30 60 30 C60 30 80 30 80 30 L80 90 L20 90 Z"
                        fill="none" stroke="currentColor" stroke-width="5"/>
                    <path d="M60 10 L80 30" stroke="currentColor" stroke-width="5" fill="none"/>
                    <path d="M30 30 L50 30" stroke="accentColor" stroke-width="5"/>
                    <path d="M30 50 L70 50" stroke="accentColor" stroke-width="5"/>
                    <path d="M30 70 L70 70" stroke="accentColor" stroke-width="5"/>
                </svg>
            '''
        }

        # Map file extensions to icon types and accent colors
        self._extension_map = {
            '.c': ('code', '#3572A5'),    # Python blue
            '.cc': ('code', '#f34b7d'),   # C++ pink
            '.cpp': ('code', '#f34b7d'),  # C++ pink
            '.css': ('code', '#563d7c'),  # CSS purple
            '.cxx': ('code', '#f34b7d'),  # C++ pink
            '.h': ('code', '#3572A5'),    # Python blue
            '.hh': ('code', '#f34b7d'),   # C++ pink
            '.hpp': ('code', '#f34b7d'),  # C++ pink
            '.html': ('code', '#e34c26'), # HTML orange
            '.htm': ('code', '#e34c26'),  # HTML orange
            '.hxx': ('code', '#f34b7d'),  # C++ pink
            '.java': ('code', None),      # Java files
            '.js': ('code', '#f1e05a'),   # JavaScript yellow
            '.jsx': ('code', '#f1e05a'),  # JavaScript yellow
            '.kt': ('code', '#f120aa'),   # Kotlin pink
            '.kts': ('code', '#f120aa'),  # Kotlin pink
            '.m6r': ('metaphor', None),   # Metaphor files
            '.md': ('text', None),        # Markdown files
            '.py': ('code', '#3572A5'),   # Python blue
            '.pyw': ('code', '#3572A5'),  # Python blue
            '.pyi': ('code', '#3572A5'),  # Python blue
            '.ts': ('code', '#3178c6'),   # TypeScript blue
            '.tsx': ('code', '#3178c6'),  # TypeScript blue
            '.txt': ('text', None),       # Text files
            '.conv': ('conversation', None),  # Conversation files
        }

        self._clear_cache()

    def _clear_cache(self) -> None:
        """Clear the icon cache to force regeneration."""
        self._cached_icons.clear()

    def _get_theme_colors(self) -> Tuple[QColor, QColor]:
        """Get appropriate colors for current theme.

        Returns:
            Tuple of (base_color, accent_color) for the current theme
        """
        if self._style_manager.color_mode() == ColorMode.DARK:
            base_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        else:
            base_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)

        # We'll use button background hover for accent if no specific accent
        accent_color = self._style_manager.get_color(ColorRole.BUTTON_BACKGROUND_HOVER)
        return base_color, accent_color

    def _create_svg_icon(self, svg_data: str, accent_color: str = "") -> QIcon:
        """Create an icon from SVG data with theme-aware colors.

        Args:
            svg_data: SVG markup defining the icon
            accent_color: Optional hex color for language-specific accents

        Returns:
            QIcon instance with the rendered SVG at multiple sizes
        """
        icon = QIcon()

        # Get colors for current theme
        base_color, default_accent = self._get_theme_colors()

        # Use provided accent color if available, otherwise use theme default
        accent = QColor(accent_color) if accent_color else default_accent

        # Replace placeholder colors in SVG
        svg_data = svg_data.replace('currentColor', base_color.name())
        svg_data = svg_data.replace('accentColor', accent.name())

        # Create renderer from SVG data
        renderer = QSvgRenderer()
        renderer.load(svg_data.encode('utf-8'))

        # Create icon at base size scaled by zoom factor
        base_size = 16
        scaled_size = int(base_size * self._style_manager.zoom_factor())

        # Create transparent pixmap
        pixmap = QPixmap(scaled_size, scaled_size)
        pixmap.fill(Qt.GlobalColor.transparent)

        # Paint the SVG onto the pixmap
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        renderer.render(painter)
        painter.end()

        icon.addPixmap(pixmap)
        return icon

    def update_icons(self) -> None:
        """Update icons when theme or zoom changes."""
        self._clear_cache()

    def icon(self, arg: QFileIconProvider.IconType | QFileInfo) -> QIcon:  # type: ignore[override]
        """Get the appropriate icon for a file or standard type.

        Args:
            arg: Either a QFileIconProvider.IconType or QFileInfo

        Returns:
            QIcon instance for the requested type

        Raises:
            ValueError: If an unknown icon type is requested
        """
        # Handle standard icon types
        if isinstance(arg, QFileIconProvider.IconType):
            # For standard icons, delegate to parent class
            return super().icon(arg)

        # Handle QFileInfo
        info = arg

        # Check if it's a directory first
        if info.isDir():
            cache_key = 'folder'
            if cache_key not in self._cached_icons:
                self._cached_icons[cache_key] = self._create_svg_icon(self._svg_paths['folder'])

            return self._cached_icons[cache_key]

        # Get file extension and map to icon type
        ext = os.path.splitext(info.fileName())[1].lower()
        icon_type, accent_color = self._extension_map.get(ext, ('file', None))

        # Create cache key from type, accent color, and theme mode
        theme_suffix = 'dark' if self._style_manager.color_mode() == ColorMode.DARK else 'light'
        cache_key = f"{icon_type}_{accent_color}_{theme_suffix}"

        # Create icon if not in cache
        if cache_key not in self._cached_icons:
            svg_data = self._svg_paths[icon_type]
            self._cached_icons[cache_key] = self._create_svg_icon(svg_data, accent_color if accent_color is not None else "")

        return self._cached_icons[cache_key]
