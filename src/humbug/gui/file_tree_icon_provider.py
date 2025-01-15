"""Workspace file tree icon management with scalable SVG icons."""

import os
from typing import Dict, Optional

from PySide6.QtCore import Qt, QFileInfo
from PySide6.QtGui import QIcon, QPainter, QPixmap
from PySide6.QtWidgets import QFileIconProvider
from PySide6.QtSvg import QSvgRenderer

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class FileTreeIconProvider(QFileIconProvider):
    """Custom file icon provider with scalable SVG icons."""

    def __init__(self):
        """Initialize the icon provider."""
        super().__init__()
        self._style_manager = StyleManager()
        self._cached_icons: Dict[str, QIcon] = {}

        # Initialize icon cache
        self._initialize_icons()

    def _initialize_icons(self):
        """Create and cache standard icons."""
        # Create path shapes for each icon type in SVG format
        self._svg_paths = {
            "folder": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M10 25 L40 25 L50 15 L90 15 L90 85 L10 85 Z"/>
                </svg>
            ''',
            "folder_open": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M10 25 L40 25 L50 15 L90 15 L90 35 L80 85 L5 85 L15 35 Z"/>
                </svg>
            ''',
            "file": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M20 10 L60 10 L80 30 L80 90 L20 90 Z"/>
                    <path d="M60 10 L60 30 L80 30"/>
                </svg>
            ''',
            "conversation": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M10 10 Q10 10 90 10 Q90 10 90 60 Q90 60 70 60 L50 85 L50 60 Q10 60 10 60 Q10 60 10 10"/>
                </svg>
            ''',
            "metaphor": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M10 90 L10 10 L50 50 L90 10 L90 90"/>
                </svg>
            ''',
            "code": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M20 10 L60 10 L80 30 L80 90 L20 90 Z"/>
                    <path d="M60 10 L60 30 L80 30"/>
                    <circle cx="50" cy="60" r="15"/>
                </svg>
            ''',
            "text": '''
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
                    <path d="M20 10 L80 10 L80 90 L20 90 Z"/>
                    <path d="M30 30 L70 30"/>
                    <path d="M30 50 L70 50"/>
                    <path d="M30 70 L70 70"/>
                </svg>
            '''
        }

        # Map file extensions to icon types
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
            '.js': ('code', '#f1e05a'),   # JavaScript yellow
            '.jsx': ('code', '#f1e05a'),  # JavaScript yellow
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

    def _clear_cache(self):
        """Clear the icon cache to force regeneration."""
        self._cached_icons.clear()

    def _create_svg_icon(self, svg_data: str, accent_color: Optional[str] = None) -> QIcon:
        """Create an icon from SVG data with optional accent color.

        Args:
            svg_data: SVG markup defining the icon
            accent_color: Optional hex color for language-specific accents

        Returns:
            QIcon instance with the rendered SVG at multiple sizes
        """
        icon = QIcon()
        base_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)

        # Replace placeholder colors in SVG
        svg_data = svg_data.replace('currentColor', base_color.name())
        if accent_color:
            svg_data = svg_data.replace('accentColor', accent_color)

        # Create renderer from SVG data
        renderer = QSvgRenderer()
        renderer.load(svg_data.encode('utf-8'))

        # We use a base size of 16px, which will be scaled by the zoom factor.
        # The zoom factor ranges from 0.5 to 2.0, giving us an effective size range of 8px to 32px
        base_size = 16
        scaled_size = int(base_size * self._style_manager.zoom_factor)

        # Create transparent pixmap
        pixmap = QPixmap(scaled_size, scaled_size)
        pixmap.fill(Qt.transparent)

        # Paint the SVG onto the pixmap
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.Antialiasing)
        renderer.render(painter)
        painter.end()
        icon.addPixmap(pixmap)

        return icon

    def icon(self, info: QFileInfo) -> QIcon:
        """Get the appropriate icon for a file type.

        Args:
            info: File information to get icon for

        Returns:
            QIcon instance for the file type
        """
        # Check if it's a directory first
        if info.isDir():
            cache_key = 'folder'
            if cache_key not in self._cached_icons:
                self._cached_icons[cache_key] = self._create_svg_icon(self._svg_paths['folder'])
            return self._cached_icons[cache_key]

        # Get file extension and map to icon type
        ext = os.path.splitext(info.fileName())[1].lower()
        icon_type, accent_color = self._extension_map.get(ext, ('file', None))

        # Create cache key from type and accent color
        cache_key = f"{icon_type}_{accent_color}" if accent_color else icon_type

        # Create icon if not in cache
        if cache_key not in self._cached_icons:
            svg_data = self._svg_paths[icon_type]
            self._cached_icons[cache_key] = self._create_svg_icon(svg_data, accent_color)

        return self._cached_icons[cache_key]

    def update_icons(self):
        """Update icons when style or zoom changes."""
        self._clear_cache()
