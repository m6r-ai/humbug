"""Updated file tree icon provider with theme-aware colors."""

import os

from PySide6.QtCore import Qt, QFileInfo
from PySide6.QtGui import QIcon, QPainter, QPixmap, QColor
from PySide6.QtWidgets import QFileIconProvider
from PySide6.QtSvg import QSvgRenderer

from desktop.color_role import ColorRole
from desktop.icons.icon_pack import MINDSPACE_ICON_PACK
from desktop.style_manager import StyleManager


class SidebarTreeIconProvider(QFileIconProvider):
    """Custom file icon provider with theme-aware scalable SVG icons."""

    def __init__(self) -> None:
        """Initialize the icon provider."""
        super().__init__()
        self._style_manager = StyleManager()
        self._cached_icons: dict[str, QIcon] = {}

        self._initialize_icons()

    def _initialize_icons(self) -> None:
        """Create and cache standard icons."""
        self._svg_paths = MINDSPACE_ICON_PACK

        self._filename_map = {
            'blueprint.md': ('blueprint', None),
        }

        self._extension_map = {
            '.c': ('code', '#3572A5'),
            '.cc': ('code', '#f34b7d'),
            '.cpp': ('code', '#f34b7d'),
            '.css': ('code', '#563d7c'),
            '.cxx': ('code', '#f34b7d'),
            '.h': ('code', '#3572A5'),
            '.hh': ('code', '#f34b7d'),
            '.hpp': ('code', '#f34b7d'),
            '.html': ('code', '#e34c26'),
            '.htm': ('code', '#e34c26'),
            '.hxx': ('code', '#f34b7d'),
            '.java': ('code', None),
            '.js': ('code', '#f1e05a'),
            '.jsx': ('code', '#f1e05a'),
            '.kt': ('code', '#f120aa'),
            '.kts': ('code', '#f120aa'),
            '.m6r': ('code', None),
            '.md': ('text', None),
            '.py': ('code', '#3572A5'),
            '.pyw': ('code', '#3572A5'),
            '.pyi': ('code', '#3572A5'),
            '.ts': ('code', '#3178c6'),
            '.tsx': ('code', '#3178c6'),
            '.txt': ('text', None),
            '.conv': ('conversation', None),
        }

        self._clear_cache()

    def _clear_cache(self) -> None:
        """Clear the icon cache to force regeneration."""
        self._cached_icons.clear()

    def _get_theme_colors(self) -> tuple[QColor, QColor]:
        """Get appropriate colors for current theme."""
        base_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        accent_color = self._style_manager.get_color(ColorRole.BUTTON_BACKGROUND_HOVER)
        return base_color, accent_color

    def _create_svg_icon(self, svg_data: str, accent_color: str = "", folder_color: str = "") -> QIcon:
        """Create an icon from SVG data with theme-aware colors."""
        icon = QIcon()

        base_color, default_accent = self._get_theme_colors()
        accent = QColor(accent_color) if accent_color else default_accent

        svg_data = svg_data.replace('currentColor', base_color.name())
        svg_data = svg_data.replace('accentColor', accent.name())
        if folder_color:
            svg_data = svg_data.replace('folderColor', folder_color)

        renderer = QSvgRenderer()
        renderer.load(svg_data.encode('utf-8'))

        base_size = 16
        scaled_size = round(base_size * self._style_manager.zoom_factor())

        pixmap = QPixmap(scaled_size, scaled_size)
        pixmap.fill(Qt.GlobalColor.transparent)

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
        """Return the hollow tinted folder icon for use in the breadcrumb bar."""
        color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER_BREADCRUMB)
        cache_key = f"folder_breadcrumb_{color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_breadcrumb'], folder_color=color
            )

        return self._cached_icons[cache_key]

    def open_folder_breadcrumb_icon(self) -> QIcon:
        """Return the open folder icon in the breadcrumb tint colour."""
        color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER_BREADCRUMB)
        cache_key = f"folder_open_breadcrumb_{color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_open'], folder_color=color
            )

        return self._cached_icons[cache_key]

    def open_folder_icon(self) -> QIcon:
        """Return the open folder icon for expanded tree directories."""
        color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER)
        cache_key = f"folder_open_{color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_open'], folder_color=color
            )

        return self._cached_icons[cache_key]

    def root_folder_icon(self) -> QIcon:
        """Return the root folder icon using breadcrumb folder colors."""
        folder_color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER_BREADCRUMB)
        cache_key = f"folder_root_{folder_color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(
                self._svg_paths['folder_root'],
                folder_color=folder_color
            )

        return self._cached_icons[cache_key]

    def icon(self, arg: QFileIconProvider.IconType | QFileInfo) -> QIcon:  # type: ignore[override]
        """Get the appropriate icon for a file or standard type."""
        if isinstance(arg, QFileIconProvider.IconType):
            return super().icon(arg)

        info = arg

        if info.isDir():
            color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER)
            cache_key = f"folder_{color}"
            if cache_key not in self._cached_icons:
                self._cached_icons[cache_key] = self._create_svg_icon(
                    self._svg_paths['folder'], folder_color=color
                )

            return self._cached_icons[cache_key]

        ext = os.path.splitext(info.fileName())[1].lower()
        filename = info.fileName().lower()
        icon_type, accent_color = self._filename_map.get(
            filename,
            self._extension_map.get(ext, ('file', None))
        )

        text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        cache_key = f"{icon_type}_{accent_color}_{text_color}"

        if cache_key not in self._cached_icons:
            svg_data = self._svg_paths[icon_type]
            self._cached_icons[cache_key] = self._create_svg_icon(svg_data, accent_color if accent_color is not None else "")

        return self._cached_icons[cache_key]
