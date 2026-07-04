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

        # Exact filenames (matched before extension).
        self._filename_map = {
            'blueprint.md': ('blueprint', None),
            '.gitignore': ('git', '#f1502f'),
            '.gitattributes': ('git', '#f1502f'),
            '.gitmodules': ('git', '#f1502f'),
            '.gitkeep': ('git', '#f1502f'),
            'package-lock.json': ('lock', '#e8a33d'),
            'yarn.lock': ('lock', '#2c8ebb'),
            'pnpm-lock.yaml': ('lock', '#f9ad00'),
            'cargo.lock': ('lock', '#dea584'),
            'podfile.lock': ('lock', '#e8a33d'),
            'gemfile.lock': ('lock', '#701516'),
            'composer.lock': ('lock', '#4F5D95'),
            'docker-compose.yml': ('config', '#2496ED'),
            'docker-compose.yaml': ('config', '#2496ED'),
            'compose.yml': ('config', '#2496ED'),
            'compose.yaml': ('config', '#2496ED'),
            '.gitlab-ci.yml': ('gear', '#e24329'),
            'azure-pipelines.yml': ('gear', '#0078d7'),
            'bitrise.yml': ('gear', '#7e30b0'),
            'pom.xml': ('code', '#b07219'),
            'build.gradle': ('code', '#02303a'),
            'settings.gradle': ('code', '#02303a'),
            'codeowners': ('text', None),
        }

        # Basename (stem without extension), matched after filename, so that e.g.
        # README.md, LICENSE, Dockerfile.dev all resolve.
        self._basename_map = {
            'readme': ('book', '#519aba'),
            'license': ('certificate', '#cb9800'),
            'licence': ('certificate', '#cb9800'),
            'changelog': ('book', '#519aba'),
            'contributing': ('book', '#519aba'),
            'authors': ('text', None),
            'notice': ('text', None),
            'makefile': ('gear', '#6d8086'),
            'cmakelists': ('gear', '#064f8c'),
            'dockerfile': ('package', '#2496ED'),
            'containerfile': ('package', '#2496ED'),
            'jenkinsfile': ('gear', '#d33833'),
            'kubeconfig': ('config', '#326ce5'),
        }

        self._extension_map = {
            # Documents
            '.txt': ('text', None), '.md': ('text', None), '.rst': ('text', None),
            '.adoc': ('text', None), '.rtf': ('document', '#4285F4'),
            '.doc': ('document', '#2b579a'), '.docx': ('document', '#2b579a'),
            '.pdf': ('pdf', '#e04a3f'), '.odt': ('document', '#2b579a'), '.pages': ('document', '#f7a600'),
            # Web / JS / TS (brand logos)
            '.js': ('lang_js', None), '.mjs': ('lang_js', None), '.cjs': ('lang_js', None),
            '.jsx': ('lang_react', None), '.ts': ('lang_ts', None), '.tsx': ('lang_react', None),
            '.vue': ('lang_vue', None), '.svelte': ('lang_svelte', None), '.astro': ('code', '#ff5d01'),
            '.html': ('lang_html', None), '.htm': ('lang_html', None), '.css': ('lang_css', None),
            '.scss': ('lang_sass', None), '.sass': ('lang_sass', None), '.less': ('lang_css', None),
            # Languages (brand logos)
            '.java': ('lang_java', None), '.kt': ('lang_kotlin', None), '.kts': ('lang_kotlin', None),
            '.groovy': ('code', '#4298b8'), '.scala': ('code', '#c22d40'), '.clj': ('code', '#db5855'),
            '.py': ('lang_python', None), '.pyw': ('lang_python', None), '.pyi': ('lang_python', None),
            '.ipynb': ('lang_jupyter', None), '.rb': ('lang_ruby', None), '.php': ('lang_php', None),
            '.go': ('lang_go', None), '.rs': ('lang_rust', None), '.swift': ('lang_swift', None),
            '.m': ('lang_c', None), '.mm': ('lang_cpp', None), '.h': ('lang_c', None),
            '.c': ('lang_c', None), '.cpp': ('lang_cpp', None), '.cc': ('lang_cpp', None),
            '.cxx': ('lang_cpp', None), '.hpp': ('lang_cpp', None), '.hh': ('lang_cpp', None),
            '.hxx': ('lang_cpp', None), '.dart': ('code', '#00B4AB'), '.lua': ('code', '#000080'),
            '.r': ('code', '#198CE7'), '.pl': ('code', '#0298c3'), '.erl': ('code', '#B83998'),
            '.ex': ('code', '#6e4a7e'), '.exs': ('code', '#6e4a7e'), '.zig': ('code', '#ec915c'),
            '.m6r': ('code', None),
            # Shells
            '.sh': ('terminal', '#4EAA25'), '.bash': ('terminal', '#4EAA25'), '.zsh': ('terminal', '#4EAA25'),
            '.fish': ('terminal', '#4EAA25'), '.ps1': ('terminal', '#012456'),
            '.bat': ('terminal', '#C1F12E'), '.cmd': ('terminal', '#C1F12E'),
            # Config
            '.json': ('config', '#cbcb41'), '.jsonc': ('config', '#cbcb41'), '.yaml': ('config', '#cb171e'),
            '.yml': ('config', '#cb171e'), '.toml': ('config', '#9c4221'), '.ini': ('config', '#6d8086'),
            '.cfg': ('config', '#6d8086'), '.conf': ('config', '#6d8086'), '.properties': ('config', '#6d8086'),
            '.env': ('config', '#edd534'), '.xml': ('code', '#e37933'),
            # Data / databases
            '.sql': ('database', '#e38c00'), '.db': ('database', '#dea584'), '.sqlite': ('database', '#003b57'),
            '.sqlite3': ('database', '#003b57'), '.parquet': ('database', '#50abdf'), '.avro': ('database', '#50abdf'),
            '.orc': ('database', '#50abdf'), '.csv': ('table', '#217346'), '.tsv': ('table', '#217346'),
            '.xls': ('table', '#217346'), '.xlsx': ('table', '#217346'), '.ods': ('table', '#217346'),
            # Images
            '.png': ('image', '#a074c4'), '.jpg': ('image', '#a074c4'), '.jpeg': ('image', '#a074c4'),
            '.gif': ('image', '#a074c4'), '.svg': ('image', '#ffb13b'), '.webp': ('image', '#a074c4'),
            '.avif': ('image', '#a074c4'), '.bmp': ('image', '#a074c4'), '.ico': ('image', '#a074c4'),
            '.tiff': ('image', '#a074c4'), '.heic': ('image', '#a074c4'),
            # Video
            '.mp4': ('video', '#f06292'), '.mov': ('video', '#f06292'), '.avi': ('video', '#f06292'),
            '.mkv': ('video', '#f06292'), '.webm': ('video', '#f06292'), '.flv': ('video', '#f06292'),
            '.mpeg': ('video', '#f06292'), '.3gp': ('video', '#f06292'),
            # Audio
            '.mp3': ('audio', '#ff8a65'), '.wav': ('audio', '#ff8a65'), '.aac': ('audio', '#ff8a65'),
            '.ogg': ('audio', '#ff8a65'), '.flac': ('audio', '#ff8a65'), '.m4a': ('audio', '#ff8a65'),
            # Archives
            '.zip': ('archive', '#f6b73c'), '.rar': ('archive', '#f6b73c'), '.7z': ('archive', '#f6b73c'),
            '.tar': ('archive', '#f6b73c'), '.gz': ('archive', '#f6b73c'), '.tgz': ('archive', '#f6b73c'),
            '.bz2': ('archive', '#f6b73c'), '.xz': ('archive', '#f6b73c'),
            # Executables / binaries
            '.exe': ('binary', '#4c8bf5'), '.dll': ('binary', '#8a8a8a'), '.so': ('binary', '#8a8a8a'),
            '.dylib': ('binary', '#8a8a8a'), '.bin': ('binary', '#8a8a8a'), '.app': ('binary', '#4c8bf5'),
            '.apk': ('binary', '#3ddc84'), '.aab': ('binary', '#3ddc84'), '.ipa': ('binary', '#4c8bf5'),
            '.jar': ('binary', '#b07219'), '.war': ('binary', '#b07219'), '.ear': ('binary', '#b07219'),
            '.class': ('binary', '#b07219'), '.o': ('binary', '#8a8a8a'), '.obj': ('binary', '#8a8a8a'),
            # Fonts
            '.ttf': ('font', '#f24e1e'), '.otf': ('font', '#f24e1e'), '.woff': ('font', '#f24e1e'),
            '.woff2': ('font', '#f24e1e'), '.eot': ('font', '#f24e1e'),
            # Design
            '.fig': ('design', '#a259ff'), '.sketch': ('design', '#fdad00'), '.xd': ('design', '#ff61f6'),
            '.psd': ('design', '#31a8ff'), '.ai': ('design', '#ff9a00'), '.blend': ('design', '#ea7600'),
            # Certificates / keys
            '.pem': ('certificate', '#4caf50'), '.crt': ('certificate', '#4caf50'), '.cer': ('certificate', '#4caf50'),
            '.csr': ('certificate', '#4caf50'), '.key': ('key', '#ffca28'), '.p12': ('key', '#ffca28'),
            '.jks': ('key', '#ffca28'), '.keystore': ('key', '#ffca28'), '.asc': ('key', '#ffca28'),
            '.sig': ('key', '#ffca28'),
            # Infrastructure
            '.tf': ('config', '#7B42BC'), '.tfvars': ('config', '#7B42BC'), '.helm': ('config', '#0f1689'),
            # Lockfiles by extension
            '.lock': ('lock', '#e8a33d'),
            # Humbug conversations
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
