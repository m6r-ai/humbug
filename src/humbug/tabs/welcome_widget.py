"""Welcome message widget implementation."""

from PySide6.QtWidgets import QVBoxLayout, QLabel, QFrame, QWidget, QPushButton
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QPixmap, QDragEnterEvent, QDropEvent

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.user.user_settings import UserSettings


class WelcomeWidget(QFrame):
    """Widget showing welcome message when no tabs are open."""
    path_dropped = Signal(str, str)  # source_type, path
    user_settings_requested = Signal()  # Emitted when user wants to open settings

    def __init__(self, parent: QWidget | None = None):
        """Initialize welcome widget."""
        super().__init__(parent)

        self._has_ai_configured = False

        self.setAcceptDrops(True)

        # Create layout
        layout = QVBoxLayout()
        self.setLayout(layout)

        # Add application icon
        self._icon_label = QLabel()
        self._icon_label.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignBottom)
        layout.addStretch()
        layout.addWidget(self._icon_label)
        layout.addSpacing(20)  # Space between icon and title

        # Application name
        self._title_label = QLabel("Humbug v38")
        self._title_label.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignBottom)

        # Add message and button for AI configuration
        self._message_label = QLabel()
        self._message_label.setAlignment(Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignTop)
        self._message_label.setWordWrap(True)
        self._message_label.setVisible(False)

        self._settings_button = QPushButton("Open Preferences")
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

        # Get style manager and connect to changes
        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

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
        if not self._has_ai_configured:
            self._message_label.setText(
                "Welcome to Humbug!  It looks like you don't have any AIs configured yet?\n\n"
                "To start using AI features, please open the application \"Preferences\", scroll to\n"
                "the \"AI Backend Configuration\" section and set up at least one AI backend."
            )
            self._message_label.setVisible(True)
            self._settings_button.setVisible(True)
        else:
            self._message_label.setVisible(False)
            self._settings_button.setVisible(False)

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
            return

        event.ignore()

    def dropEvent(self, event: QDropEvent) -> None:
        """
        Handle file drops by emitting signal.

        Args:
            event: The drop event

        Raises:
            UnicodeDecodeError: If the path data cannot be decoded as UTF-8
        """
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

    def _on_style_changed(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Update icon size
        icon_path = self._style_manager.get_icon_path("app-icon-disabled")
        icon_pixmap = QPixmap(icon_path)
        scaled_size = int(160 * zoom_factor)  # 160px base size for welcome screen
        self._icon_label.setPixmap(icon_pixmap.scaled(
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
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                border: none;
                border-radius: {4 * zoom_factor}px;
            }}
            QFrame > QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background: none;
                font-size: {base_font_size * 1.5}pt;
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
