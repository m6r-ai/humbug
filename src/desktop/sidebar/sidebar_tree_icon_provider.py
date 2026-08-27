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

        # Exact filenames (matched before extension) - in strict lexographic order
        self._filename_map = {
            '.gitattributes': ('git', '#f1502f'),
            '.gitignore': ('git', '#f1502f'),
            '.gitkeep': ('git', '#f1502f'),
            '.gitlab-ci.yml': ('gear', '#e24329'),
            '.gitmodules': ('git', '#f1502f'),
            'azure-pipelines.yml': ('gear', '#0078d7'),
            'bitrise.yml': ('gear', '#7e30b0'),
            'blueprint.md': ('blueprint', None),
            'build.gradle': ('code', '#02303a'),
            'cargo.lock': ('lock', '#dea584'),
            'codeowners': ('text', None),
            'composer.lock': ('lock', '#4F5D95'),
            'compose.yaml': ('config', '#2496ED'),
            'compose.yml': ('config', '#2496ED'),
            'docker-compose.yml': ('config', '#2496ED'),
            'docker-compose.yaml': ('config', '#2496ED'),
            'gemfile.lock': ('lock', '#701516'),
            'package-lock.json': ('lock', '#e8a33d'),
            'pnpm-lock.yaml': ('lock', '#f9ad00'),
            'podfile.lock': ('lock', '#e8a33d'),
            'pom.xml': ('code', '#b07219'),
            'settings.gradle': ('code', '#02303a'),
            'yarn.lock': ('lock', '#2c8ebb'),
        }

        # Basename (stem without extension), matched after filename, so that e.g.
        # README.md, LICENSE, Dockerfile.dev all resolve - in strict lexographic order
        self._basename_map = {
            'authors': ('text', None),
            'changelog': ('book', '#519aba'),
            'cmakelists': ('gear', '#064f8c'),
            'containerfile': ('package', '#2496ED'),
            'contributing': ('book', '#519aba'),
            'dockerfile': ('package', '#2496ED'),
            'jenkinsfile': ('gear', '#d33833'),
            'kubeconfig': ('config', '#326ce5'),
            'license': ('certificate', '#cb9800'),
            'licence': ('certificate', '#cb9800'),
            'makefile': ('gear', '#6d8086'),
            'notice': ('text', None),
            'readme': ('book', '#519aba'),
        }

        # File extensions, matched after filename and basename - in strict lexographic order
        self._extension_map = {
            '.3gp': ('video', '#f06292'),
            '.7z': ('archive', '#f6b73c'),
            '.aab': ('binary', '#3ddc84'),
            '.aac': ('audio', '#ff8a65'),
            '.adoc': ('text', None),
            '.ai': ('design', '#ff9a00'),
            '.apk': ('binary', '#3ddc84'),
            '.app': ('binary', '#4c8bf5'),
            '.asc': ('key', '#ffca28'),
            '.astro': ('code', '#ff5d01'),
            '.avi': ('video', '#f06292'),
            '.avif': ('image', '#a074c4'),
            '.avro': ('database', '#50abdf'),
            '.bash': ('terminal', '#4EAA25'),
            '.bat': ('terminal', '#C1F12E'),
            '.bin': ('binary', '#8a8a8a'),
            '.blend': ('design', '#ea7600'),
            '.bmp': ('image', '#a074c4'),
            '.bz2': ('archive', '#f6b73c'),
            '.c': ('lang_c', None),
            '.cc': ('lang_cpp', None),
            '.cer': ('certificate', '#4caf50'),
            '.cfg': ('config', '#6d8086'),
            '.class': ('binary', '#b07219'),
            '.clj': ('code', '#db5855'),
            '.cjs': ('lang_js', None),
            '.cmd': ('terminal', '#C1F12E'),
            '.conf': ('config', '#6d8086'),
            '.conv': ('conversation', None),
            '.cpp': ('lang_cpp', None),
            '.crt': ('certificate', '#4caf50'),
            '.csr': ('certificate', '#4caf50'),
            '.css': ('lang_css', None),
            '.csv': ('table', '#217346'),
            '.cxx': ('lang_cpp', None),
            '.dart': ('code', '#00B4AB'),
            '.db': ('database', '#dea584'),
            '.dll': ('binary', '#8a8a8a'),
            '.doc': ('document', '#2b579a'),
            '.docx': ('document', '#2b579a'),
            '.dylib': ('binary', '#8a8a8a'),
            '.ear': ('binary', '#b07219'),
            '.env': ('config', '#edd534'),
            '.eot': ('font', '#f24e1e'),
            '.erl': ('code', '#B83998'),
            '.ex': ('code', '#6e4a7e'),
            '.exe': ('binary', '#4c8bf5'),
            '.exs': ('code', '#6e4a7e'),
            '.fig': ('design', '#a259ff'),
            '.fish': ('terminal', '#4EAA25'),
            '.flac': ('audio', '#ff8a65'),
            '.flv': ('video', '#f06292'),
            '.gif': ('image', '#a074c4'),
            '.go': ('lang_go', None),
            '.groovy': ('code', '#4298b8'),
            '.gz': ('archive', '#f6b73c'),
            '.h': ('lang_c', None),
            '.heic': ('image', '#a074c4'),
            '.helm': ('config', '#0f1689'),
            '.hh': ('lang_cpp', None),
            '.hpp': ('lang_cpp', None),
            '.htm': ('lang_html', None),
            '.html': ('lang_html', None),
            '.hxx': ('lang_cpp', None),
            '.ico': ('image', '#a074c4'),
            '.ini': ('config', '#6d8086'),
            '.ipa': ('binary', '#4c8bf5'),
            '.ipynb': ('lang_jupyter', None),
            '.jar': ('binary', '#b07219'),
            '.java': ('lang_java', None),
            '.jks': ('key', '#ffca28'),
            '.jpeg': ('image', '#a074c4'),
            '.jpg': ('image', '#a074c4'),
            '.js': ('lang_js', None),
            '.json': ('config', '#cbcb41'),
            '.jsonc': ('config', '#cbcb41'),
            '.jsx': ('lang_react', None),
            '.key': ('key', '#ffca28'),
            '.keystore': ('key', '#ffca28'),
            '.kt': ('lang_kotlin', None),
            '.kts': ('lang_kotlin', None),
            '.less': ('lang_css', None),
            '.lock': ('lock', '#e8a33d'),
            '.lua': ('code', '#000080'),
            '.m6r': ('code', None),
            '.m': ('lang_c', None),
            '.m4a': ('audio', '#ff8a65'),
            '.md': ('text', None),
            '.mkv': ('video', '#f06292'),
            '.mjs': ('lang_js', None),
            '.mm': ('lang_cpp', None),
            '.mov': ('video', '#f06292'),
            '.mp3': ('audio', '#ff8a65'),
            '.mp4': ('video', '#f06292'),
            '.mpeg': ('video', '#f06292'),
            '.o': ('binary', '#8a8a8a'),
            '.obj': ('binary', '#8a8a8a'),
            '.ods': ('table', '#217346'),
            '.odt': ('document', '#2b579a'),
            '.ogg': ('audio', '#ff8a65'),
            '.orc': ('database', '#50abdf'),
            '.otf': ('font', '#f24e1e'),
            '.p12': ('key', '#ffca28'),
            '.pages': ('document', '#f7a600'),
            '.parquet': ('database', '#50abdf'),
            '.pdf': ('pdf', '#e04a3f'),
            '.pem': ('certificate', '#4caf50'),
            '.php': ('lang_php', None),
            '.pl': ('code', '#0298c3'),
            '.png': ('image', '#a074c4'),
            '.properties': ('config', '#6d8086'),
            '.ps1': ('terminal', '#012456'),
            '.psd': ('design', '#31a8ff'),
            '.py': ('lang_python', None),
            '.pyi': ('lang_python', None),
            '.pyw': ('lang_python', None),
            '.r': ('code', '#198CE7'),
            '.rar': ('archive', '#f6b73c'),
            '.rb': ('lang_ruby', None),
            '.rs': ('lang_rust', None),
            '.rst': ('text', None),
            '.rtf': ('document', '#4285F4'),
            '.sass': ('lang_sass', None),
            '.scala': ('code', '#c22d40'),
            '.scss': ('lang_sass', None),
            '.sh': ('terminal', '#4EAA25'),
            '.sig': ('key', '#ffca28'),
            '.sketch': ('design', '#fdad00'),
            '.so': ('binary', '#8a8a8a'),
            '.sql': ('database', '#e38c00'),
            '.sqlite': ('database', '#003b57'),
            '.sqlite3': ('database', '#003b57'),
            '.svelte': ('lang_svelte', None),
            '.svg': ('image', '#ffb13b'),
            '.swift': ('lang_swift', None),
            '.tar': ('archive', '#f6b73c'),
            '.tf': ('config', '#7B42BC'),
            '.tfvars': ('config', '#7B42BC'),
            '.tgz': ('archive', '#f6b73c'),
            '.tiff': ('image', '#a074c4'),
            '.toml': ('config', '#9c4221'),
            '.ts': ('lang_ts', None),
            '.tsv': ('table', '#217346'),
            '.tsx': ('lang_react', None),
            '.ttf': ('font', '#f24e1e'),
            '.txt': ('text', None),
            '.vue': ('lang_vue', None),
            '.war': ('binary', '#b07219'),
            '.wav': ('audio', '#ff8a65'),
            '.webp': ('image', '#a074c4'),
            '.webm': ('video', '#f06292'),
            '.woff': ('font', '#f24e1e'),
            '.woff2': ('font', '#f24e1e'),
            '.xd': ('design', '#ff61f6'),
            '.xls': ('table', '#217346'),
            '.xlsx': ('table', '#217346'),
            '.xml': ('code', '#e37933'),
            '.xz': ('archive', '#f6b73c'),
            '.yaml': ('config', '#cb171e'),
            '.yml': ('config', '#cb171e'),
            '.zig': ('code', '#ec915c'),
            '.zip': ('archive', '#f6b73c'),
            '.zsh': ('terminal', '#4EAA25'),
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

    def _resolve_icon(self, filename: str, stem: str, ext: str) -> tuple[str, str | None]:
        """
        Resolve a file to an (icon_name, accent_color) pair.

        Resolution order: exact filename, then basename (README/LICENSE/etc.),
        then test/spec markers, then extension, then a generic file fallback.

        Args:
            filename: The lowercased file name.
            stem: The name without its final extension.
            ext: The final extension including the leading dot.

        Returns:
            An (icon_name, accent_color) tuple.
        """
        if filename in self._filename_map:
            return self._filename_map[filename]

        if stem in self._basename_map:
            return self._basename_map[stem]

        # e.g. foo.test.js / bar.spec.ts / baz.snap
        if stem.endswith((".test", ".spec")) or ext == ".snap":
            return ("beaker", "#8bc34a")

        return self._extension_map.get(ext, ("file", None))

    def icon(self, arg: QFileIconProvider.IconType | QFileInfo) -> QIcon:  # type: ignore[override]
        """Get the appropriate icon for a file or standard type."""
        if isinstance(arg, QFileIconProvider.IconType):
            return super().icon(arg)

        info = arg

        if info.isSymLink():
            text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
            cache_key = f"symlink_#4eaa25_{text_color}"
            if cache_key not in self._cached_icons:
                self._cached_icons[cache_key] = self._create_svg_icon(
                    self._svg_paths['symlink'], "#4eaa25"
                )

            return self._cached_icons[cache_key]

        if info.isDir():
            color = self._style_manager.get_color_str(ColorRole.MINDSPACE_FOLDER)
            cache_key = f"folder_{color}"
            if cache_key not in self._cached_icons:
                self._cached_icons[cache_key] = self._create_svg_icon(
                    self._svg_paths['folder'], folder_color=color
                )

            return self._cached_icons[cache_key]

        filename = info.fileName().lower()
        stem, ext = os.path.splitext(filename)
        icon_type, accent_color = self._resolve_icon(filename, stem, ext)

        text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        cache_key = f"{icon_type}_{accent_color}_{text_color}"

        if cache_key not in self._cached_icons:
            svg_data = self._svg_paths[icon_type]
            self._cached_icons[cache_key] = self._create_svg_icon(svg_data, accent_color if accent_color is not None else "")

        return self._cached_icons[cache_key]

    def pinned_section_icon(self) -> QIcon:
        """Return the plain pin icon used for the Pinned section header."""
        text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        cache_key = f"pinned_section_{text_color}"
        if cache_key not in self._cached_icons:
            self._cached_icons[cache_key] = self._create_svg_icon(self._svg_paths['pin'])

        return self._cached_icons[cache_key]
