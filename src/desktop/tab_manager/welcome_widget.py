"""Welcome message widget implementation."""

from PySide6.QtWidgets import QVBoxLayout, QLabel, QFrame, QWidget, QPushButton
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QDragEnterEvent, QDragLeaveEvent, QDropEvent, QPainter, QPaintEvent, QPixmap

from desktop.color_role import ColorRole
from desktop.gradient_label import GradientBorderLabel, GradientLabel
from desktop.style_manager import StyleManager
from desktop.language.language_manager import LanguageManager
from desktop.user.user_settings import UserSettings
from desktop.version import CURRENT_VERSION


class WelcomeWidget(QFrame):
    """Widget showing welcome message when no tabs are open."""
    path_dropped = Signal(str, str)  # source_type, path
    user_settings_requested = Signal()  # Emitted when user wants to open settings

    def __init__(self, parent: QWidget | None = None):
        """Initialize welcome widget."""
        super().__init__(parent)

        self._has_ai_configured = False
        self._drag_active = False

        self.setAcceptDrops(True)

        # Create layout
        layout = QVBoxLayout()
        self.setLayout(layout)

        # Get language manager and connect to changes
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Style manager created early so gradient colours are available immediately
        self._style_manager = StyleManager()

        # Add application logo with gradient border
        self._logo_label = GradientBorderLabel(
            self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_START),
            self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_END),
            radius=16.0,
            border_width=2.0,
            fill_color=self._style_manager.get_color_str(ColorRole.LOGO_BACKGROUND),
        )
        self._logo_label.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignBottom)
        layout.addStretch()
        layout.addSpacing(30)
        layout.addWidget(self._logo_label)
        layout.addSpacing(30)

        # Application name + version — gradient matches the logo colours
        self._title_label = GradientLabel(
            f"Humbug v{CURRENT_VERSION}",
            self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_START),
            self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_END),
        )
        self._title_label.setObjectName("titleLabel")
        self._title_label.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignBottom)

        # Add message and button for AI configuration
        self._message_label = QLabel()
        self._message_label.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignTop)
        self._message_label.setWordWrap(True)
        self._message_label.setVisible(False)

        self._settings_button = QPushButton()
        self._settings_button.setVisible(False)
        self._settings_button.clicked.connect(self._on_settings_button_clicked)

        # Create a container for the button to center it
        button_container = QWidget()
        button_layout = QVBoxLayout(button_container)
        button_layout.setContentsMargins(0, 0, 0, 0)
        button_layout.addWidget(self._settings_button, alignment=Qt.AlignmentFlag.AlignHCenter)

        # Add widgets to layout
        layout.addWidget(self._title_label)
        layout.addSpacing(30)
        layout.addWidget(self._message_label)
        layout.addSpacing(20)
        layout.addWidget(button_container)
        layout.addStretch()

        # Set margins for dialog-style spacing
        layout.setContentsMargins(20, 20, 20, 20)

        self.apply_style()
        self._on_language_changed()

    def set_user_settings(self, settings: UserSettings) -> None:
        """
        Update the welcome widget based on user settings.

        Args:
            settings: Current user settings
        """
        self._has_ai_configured = self._check_ai_configured(settings)
        self._update_message()

    def _check_ai_configured(self, settings: UserSettings) -> bool:
        """
        Check if user has at least one AI backend configured.

        Args:
            settings: User settings to check

        Returns:
            True if at least one AI backend is configured
        """
        for backend_id, backend in settings.ai_backends.items():
            if backend.enabled:
                # Local backends (ollama, vllm) can work with defaults
                if backend_id in ("ollama", "vllm"):
                    return True

                # Other backends need credentials
                if backend.api_key or backend.url:
                    return True

        return False

    def _update_message(self) -> None:
        """Update the message and button visibility based on AI configuration status."""
        strings = self._language_manager.strings()

        if not self._has_ai_configured:
            self._message_label.setText(strings.welcome_message)
            self._settings_button.setText(strings.welcome_button)

        visible = not self._has_ai_configured
        self._message_label.setVisible(visible)
        self._settings_button.setVisible(visible)

    def _on_language_changed(self) -> None:
        """Update text when language changes."""
        # Update button text
        strings = self._language_manager.strings()
        self._settings_button.setText(strings.welcome_button)
        # Update message text
        self._update_message()

    def _on_settings_button_clicked(self) -> None:
        """Handle settings button click."""
        self.user_settings_requested.emit()

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """
        Accept file drops.

        Args:
            event: The drag enter event
        """
        if event.mimeData().hasFormat("application/x-humbug-path"):
            event.acceptProposedAction()
            self._drag_active = True
            self.update()
            return

        event.ignore()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Clear the highlight when the drag leaves."""
        self._drag_active = False
        self.update()
        super().dragLeaveEvent(event)

    def dropEvent(self, event: QDropEvent) -> None:
        """
        Handle file drops by emitting signal.

        Args:
            event: The drop event

        Raises:
            UnicodeDecodeError: If the path data cannot be decoded as UTF-8
        """
        self._drag_active = False
        self.update()

        if event.mimeData().hasFormat("application/x-humbug-path"):
            mime_data = event.mimeData().data("application/x-humbug-path").data()

            # Convert to bytes first if it's not already bytes
            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            path = mime_data.decode()

            # Extract source type if available
            source_type = None
            if event.mimeData().hasFormat("application/x-humbug-source"):
                source_data = event.mimeData().data("application/x-humbug-source").data()

                # Convert to bytes first if it's not already bytes
                if not isinstance(source_data, bytes):
                    source_data = bytes(source_data)

                source_type = source_data.decode()

            self.path_dropped.emit(source_type, path)
            event.acceptProposedAction()
            return

        event.ignore()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Paint a highlight overlay when a drag is active over this widget."""
        super().paintEvent(event)
        if not self._drag_active:
            return

        painter = QPainter(self)
        color = self._style_manager.get_color(ColorRole.DROP_TARGET_HIGHLIGHT)
        painter.fillRect(self.rect(), color)
        painter.end()

    def apply_style(self) -> None:
        """Apply current style settings."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Keep gradient colours in sync with theme
        grad_start = self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_START)
        grad_end = self._style_manager.get_color_str(ColorRole.BRAND_GRADIENT_END)
        self._logo_label.update_colors(grad_start, grad_end)
        self._title_label.update_colors(grad_start, grad_end)
        self._logo_label.update_fill_color(
            self._style_manager.get_color_str(ColorRole.LOGO_BACKGROUND))

        # Update logo
        logo_pixmap = QPixmap(self._style_manager.get_app_logo_path())
        scaled_size = int(300 * zoom_factor)
        self._logo_label.setPixmap(logo_pixmap.scaled(
            scaled_size, scaled_size,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        ))

        # Update button style
        self._settings_button.setStyleSheet(f"""
            QPushButton {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 10px 20px;
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QPushButton:hover {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
        """)

        # Update colors and frame style
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
                border: none;
                border-radius: {4 * zoom_factor}px;
            }}
            QFrame > QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background: none;
                font-size: {base_font_size}pt;
            }}
            QLabel#titleLabel {{
                background: none;
                font-size: {base_font_size * 1.8}pt;
                font-weight: bold;
            }}
            #message_label {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                font-size: {base_font_size}pt;
                font-weight: normal;
            }}
        """)

        # Set object name for the message label to apply specific styling
        self._message_label.setObjectName("message_label")
