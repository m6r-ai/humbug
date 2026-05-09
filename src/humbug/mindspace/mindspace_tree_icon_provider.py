"""Updated file tree icon provider with theme-aware colors."""

import os
from typing import Dict, Tuple

from PySide6.QtCore import Qt, QFileInfo
from PySide6.QtGui import QIcon, QPainter, QPixmap, QColor
from PySide6.QtWidgets import QFileIconProvider
from PySide6.QtSvg import QSvgRenderer

from humbug.color_role import ColorRole
from humbug.icons.icon_pack import MINDSPACE_ICON_PACK
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
        self._svg_paths = MINDSPACE_ICON_PACK

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
            '.m6r': ('code', None),       # Metaphor files
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

    def _create_svg_icon(self, svg_data: str, accent_color: str = "", folder_color: str = "") -> QIcon:
        """Create an icon from SVG data with theme-aware colors.

        Args:
            svg_data: SVG markup defining the icon
            accent_color: Optional hex color for language-specific accents
            folder_color: Optional hex color replacing the folderColor placeholder

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
        if folder_color:
            svg_data = svg_data.replace('folderColor', folder_color)

        # Create renderer from SVG data
        renderer = QSvgRenderer()
        renderer.load(svg_data.encode('utf-8'))

        # Create icon at base size scaled by zoom factor
        base_size = 16
        scaled_size = round(base_size * self._style_manager.zoom_factor())

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

    def breadcrumb_folder_icon(self) -> QIcon:
        """Return the hollow tinted folder icon for use in the breadcrumb bar.

        Returns:
            QIcon instance with the hollow breadcrumb folder rendered at the current zoom
        """
        theme_suffix = 'dark' if self._style_manager.color_mode() == ColorMode.DARK else 'light'
        color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER_BREADCRUMB)
        cache_key = f"folder_breadcrumb_{theme_suffix}_{color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_breadcrumb'], folder_color=color
            )

        return self._cached_icons[cache_key]

    def open_folder_breadcrumb_icon(self) -> QIcon:
        """Return the open folder icon in the breadcrumb tint colour.

        Returns:
            QIcon instance with the open folder rendered in MINDSPACE_FOLDER_BREADCRUMB colour
        """
        theme_suffix = 'dark' if self._style_manager.color_mode() == ColorMode.DARK else 'light'
        color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER_BREADCRUMB)
        cache_key = f"folder_open_breadcrumb_{theme_suffix}_{color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_open'], folder_color=color
            )

        return self._cached_icons[cache_key]

    def open_folder_icon(self) -> QIcon:
        """Return the open folder icon for expanded tree directories."""
        theme_suffix = 'dark' if self._style_manager.color_mode() == ColorMode.DARK else 'light'
        color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER)
        cache_key = f"folder_open_{theme_suffix}_{color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_open'], folder_color=color
            )

        return self._cached_icons[cache_key]

    def root_folder_icon(self) -> QIcon:
        """Return the root folder icon using breadcrumb folder colors."""
        theme_suffix = 'dark' if self._style_manager.color_mode() == ColorMode.DARK else 'light'
        folder_color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER_BREADCRUMB)
        cache_key = f"folder_root_{theme_suffix}_{folder_color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_root'],
                folder_color=folder_color
            )

        return self._cached_icons[cache_key]

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
            theme_suffix = 'dark' if self._style_manager.color_mode() == ColorMode.DARK else 'light'
            color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER)
            cache_key = f"folder_{theme_suffix}_{color}"
            if cache_key not in self._cached_icons:
                self._cached_icons[cache_key] = self._create_svg_icon(
                    self._svg_paths['folder'], folder_color=color
                )

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
