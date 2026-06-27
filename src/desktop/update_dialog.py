"""Dialog for displaying version and update information."""

from PySide6.QtCore import Qt
from PySide6.QtGui import QPixmap
from PySide6.QtWidgets import QDialog, QLabel, QPushButton, QVBoxLayout, QWidget

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.style_manager import StyleManager
from desktop.url_opener import open_url
from desktop.widgets.gradient_label import GradientBorderLabel


class UpdateDialog(QDialog):
    """
    Dialog shown when the user selects 'Check for Updates'.

    Displays the current version and the latest available version from GitHub,
    or a 'you are up to date' message if no newer version was found.
    Handles three states:
      - Checking (spinner label while the async check is in progress)
      - Up to date
      - Update available (with a clickable link to the release page)
    """

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the dialog in the 'checking' state."""
        super().__init__(parent)

        self._language_manager = LanguageManager()
        self._style_manager = StyleManager()
        strings = self._language_manager.strings()
        self._link_color = self._style_manager.get_color_str(ColorRole.TEXT_LINK)

        self.setWindowTitle(strings.check_for_updates)
        self.setMinimumWidth(420)
        self.setModal(True)

        layout = QVBoxLayout()
        layout.setSpacing(8)
        layout.addSpacing(24)

        logo_label = GradientBorderLabel(
            self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_START),
            self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_END),
            radius=16.0,
            border_width=2.0,
            fill_color=self._style_manager.get_color_str(ColorRole.LOGO_BACKGROUND),
        )
        logo_pixmap = QPixmap(self._style_manager.get_app_logo_path())
        zoom_factor = self._style_manager.zoom_factor()
        scaled_size = int(160 * zoom_factor)
        logo_label.setPixmap(logo_pixmap.scaled(
            scaled_size, scaled_size,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation,
        ))
        logo_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(logo_label)
        layout.addSpacing(8)

        self._current_version_label = QLabel()
        self._current_version_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self._current_version_label)

        self._status_label = QLabel()
        self._status_label.setObjectName("statusLabel")
        self._status_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._status_label.setWordWrap(True)
        self._status_label.linkActivated.connect(open_url)
        layout.addWidget(self._status_label)

        layout.addSpacing(16)

        min_button_width = int(90 * zoom_factor)

        self._close_button = QPushButton(strings.close_button)
        self._close_button.clicked.connect(self.accept)
        self._close_button.setMinimumWidth(min_button_width)
        self._close_button.setContentsMargins(8, 8, 8, 8)
        layout.addWidget(self._close_button, alignment=Qt.AlignmentFlag.AlignCenter)
        layout.addSpacing(16)

        self.setLayout(layout)
        self._apply_style()
        self.set_checking()

    def set_checking(self) -> None:
        """Show the 'checking' state."""
        strings = self._language_manager.strings()
        self._current_version_label.setText(
            strings.update_current_version.format(strings.update_checking)
        )
        self._status_label.setText("")

    def set_result(self, current: str, latest: str | None, release_url: str | None) -> None:
        """
        Update the dialog with the check result.

        Args:
            current: Current version string, e.g. "v46".
            latest: Latest version tag from GitHub, e.g. "v47", or None if check failed.
            release_url: URL to the GitHub release page, or None.
        """
        strings = self._language_manager.strings()
        self._current_version_label.setText(strings.update_current_version.format(current))

        if latest is None:
            self._status_label.setText(strings.update_check_failed)
            return

        try:
            current_int = int(current.lstrip("vV"))
            latest_int = int(latest.lstrip("vV"))
            is_newer = latest_int > current_int

        except ValueError:
            is_newer = False

        if is_newer:
            link_text = f"<a href='{release_url}' style='color: {self._link_color}; text-decoration: none;'>{latest}</a>"
            self._status_label.setText(strings.update_available_message.format(link_text))

        else:
            self._status_label.setText(strings.update_up_to_date)

    def _apply_style(self) -> None:
        """Apply dialog styling."""
        style_manager = self._style_manager
        base_font_size = style_manager.base_font_size()
        zoom_factor = style_manager.zoom_factor()

        self.setStyleSheet(f"""
            QDialog {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QLabel#statusLabel {{
                font-size: {base_font_size * zoom_factor}pt;
                margin: 4px 16px;
            }}
            QPushButton {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED)};
                border: none;
                border-radius: 4px;
                padding: 6px;
                min-width: 80px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QPushButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
        """)
