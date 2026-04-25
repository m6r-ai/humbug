"""Main mindspace view widget containing files, conversations, and preview views."""

import os

from PySide6.QtCore import Qt, Signal, QSize, QEvent, QObject
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import (
    QButtonGroup,
    QHBoxLayout,
    QPushButton,
    QSizePolicy,
    QStackedWidget,
    QToolButton,
    QVBoxLayout,
    QWidget,
)

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.conversations.mindspace_conversations_view import MindspaceConversationsView
from humbug.mindspace.files.mindspace_files_view import MindspaceFilesView
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.mindspace.preview.mindspace_preview_view import MindspacePreviewView
from humbug.mindspace.vcs.mindspace_vcs_view import MindspaceVCSView
from humbug.style_manager import StyleManager


class MindspaceView(QWidget):
    """Main mindspace view widget containing the sidebar rail and active pane."""

    open_mindspace_requested = Signal()
    file_clicked = Signal(MindspaceViewType, str, bool)
    toggle_requested = Signal()
    file_deleted = Signal(str)
    file_renamed = Signal(str, str)
    file_moved = Signal(str, str)
    file_edited = Signal(str, bool)
    file_opened_in_preview = Signal(str, bool)
    file_opened_in_diff = Signal(str, bool)
    new_conversation_requested = Signal(str)
    settings_requested = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the mindspace view widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._mindspace_manager = MindspaceManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._active_view_type = MindspaceViewType.CONVERSATIONS
        self._vcs_available = False
        self._sidebar_collapsed = False
        self._expanded_sidebar_width = 320
        self._rail_collapsed_width = 48
        self._content_min_width = 240
        self._view_buttons: dict[MindspaceViewType, QToolButton] = {}
        self._view_widgets: dict[MindspaceViewType, QWidget] = {}

        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._rail_widget = QWidget(self)
        self._rail_widget.setObjectName("_rail_widget")
        self._rail_widget.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Expanding)
        rail_layout = QVBoxLayout(self._rail_widget)
        rail_layout.setContentsMargins(0, 0, 0, 0)
        rail_layout.setSpacing(0)

        self._view_button_group = QButtonGroup(self)
        self._view_button_group.setExclusive(True)

        self._sidebar_toggle_button = QToolButton(self._rail_widget)
        self._sidebar_toggle_button.setObjectName("_sidebar_toggle_button")
        self._sidebar_toggle_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        self._sidebar_toggle_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._sidebar_toggle_button.clicked.connect(self._toggle_sidebar)
        self._sidebar_toggle_button.setProperty("icon_name", "expand-right")
        self._sidebar_toggle_button.installEventFilter(self)
        rail_layout.addWidget(self._sidebar_toggle_button)

        self._conversations_button = self._create_view_button(MindspaceViewType.CONVERSATIONS, "conversation")
        rail_layout.addWidget(self._conversations_button)

        self._files_button = self._create_view_button(MindspaceViewType.FILES, "files")
        rail_layout.addWidget(self._files_button)

        self._preview_button = self._create_view_button(MindspaceViewType.PREVIEW, "preview")
        rail_layout.addWidget(self._preview_button)

        self._vcs_button = self._create_view_button(MindspaceViewType.VCS, "diff")
        self._vcs_button.hide()
        rail_layout.addWidget(self._vcs_button)

        rail_layout.addStretch()

        self._settings_button = QToolButton(self._rail_widget)
        self._settings_button.setObjectName("_settings_button")
        self._settings_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        self._settings_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._settings_button.clicked.connect(self._on_settings_button_clicked)
        self._settings_button.setProperty("icon_name", "cog")
        self._settings_button.installEventFilter(self)
        self._settings_button.hide()
        rail_layout.addWidget(self._settings_button)

        layout.addWidget(self._rail_widget)

        self._content_widget = QWidget(self)
        self._content_widget.setObjectName("_content_widget")
        self._content_widget.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Expanding)
        content_layout = QVBoxLayout(self._content_widget)
        content_layout.setContentsMargins(0, 0, 0, 0)
        content_layout.setSpacing(0)

        self._header_widget = QWidget(self._content_widget)
        self._header_widget.setObjectName("_header_widget")
        header_layout = QHBoxLayout(self._header_widget)
        header_layout.setContentsMargins(14, 10, 10, 10)
        header_layout.setSpacing(8)

        self._mindspace_button = QPushButton(self._header_widget)
        self._mindspace_button.setObjectName("_mindspace_button")
        self._mindspace_button.clicked.connect(self.open_mindspace_requested.emit)
        header_layout.addWidget(self._mindspace_button)
        header_layout.addStretch()

        content_layout.addWidget(self._header_widget)

        self._pane_stack = QStackedWidget(self._content_widget)
        self._pane_stack.setObjectName("_pane_stack")
        content_layout.addWidget(self._pane_stack)

        layout.addWidget(self._content_widget, 1)

        self._conversations_view = MindspaceConversationsView()
        self._files_view = MindspaceFilesView()
        self._preview_view = MindspacePreviewView()
        self._vcs_view = MindspaceVCSView()

        self._register_view(MindspaceViewType.CONVERSATIONS, self._conversations_view)
        self._register_view(MindspaceViewType.FILES, self._files_view)
        self._register_view(MindspaceViewType.PREVIEW, self._preview_view)
        self._register_view(MindspaceViewType.VCS, self._vcs_view)

        self._files_view.file_clicked.connect(self.file_clicked.emit)
        self._files_view.file_deleted.connect(self.file_deleted.emit)
        self._files_view.file_renamed.connect(self.file_renamed.emit)
        self._files_view.file_moved.connect(self.file_moved.emit)
        self._files_view.file_edited.connect(self.file_edited.emit)
        self._files_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)
        self._files_view.file_opened_in_diff.connect(self.file_opened_in_diff.emit)

        self._conversations_view.file_clicked.connect(self.file_clicked.emit)
        self._conversations_view.file_deleted.connect(self.file_deleted.emit)
        self._conversations_view.file_renamed.connect(self.file_renamed.emit)
        self._conversations_view.file_moved.connect(self.file_moved.emit)
        self._conversations_view.file_edited.connect(self.file_edited.emit)
        self._conversations_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)
        self._conversations_view.new_conversation_requested.connect(self.new_conversation_requested.emit)

        self._vcs_view.file_opened_in_diff.connect(self.file_opened_in_diff.emit)
        self._vcs_view.file_clicked.connect(self.file_clicked.emit)
        self._vcs_view.file_deleted.connect(self.file_deleted.emit)
        self._vcs_view.file_edited.connect(self.file_edited.emit)
        self._vcs_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)
        self._vcs_view.repo_available.connect(self._on_vcs_repo_available)

        self._preview_view.file_clicked.connect(self.file_clicked.emit)
        self._preview_view.file_deleted.connect(self.file_deleted.emit)
        self._preview_view.file_renamed.connect(self.file_renamed.emit)
        self._preview_view.file_moved.connect(self.file_moved.emit)
        self._preview_view.file_edited.connect(self.file_edited.emit)
        self._preview_view.file_opened_in_preview.connect(self.file_opened_in_preview.emit)

        self._mindspace_button.setText(self._language_manager.strings().mindspace_label_none)
        self._set_active_view(MindspaceViewType.CONVERSATIONS)
        self._on_language_changed()

    def _create_view_button(self, view_type: MindspaceViewType, icon_name: str) -> QToolButton:
        """Create a left-rail button for a view."""
        button = QToolButton(self._rail_widget)
        button.setObjectName(f"_view_button_{view_type.name.lower()}")
        button.setProperty("view_type", view_type.name.lower())
        button.setProperty("icon_name", icon_name)
        button.setCheckable(True)
        button.setAutoRaise(False)
        button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        button.clicked.connect(lambda checked, vt=view_type: self._on_view_button_clicked(vt, checked))
        button.installEventFilter(self)
        self._view_button_group.addButton(button)
        self._view_buttons[view_type] = button
        return button

    def _register_view(self, view_type: MindspaceViewType, view: QWidget) -> None:
        """Register a view in the stacked content area."""
        self._view_widgets[view_type] = view
        self._pane_stack.addWidget(view)

    def _on_view_button_clicked(self, view_type: MindspaceViewType, checked: bool) -> None:
        """Switch active pane from rail selection."""
        if checked:
            self._set_active_view(view_type)

    def _set_active_view(self, view_type: MindspaceViewType) -> None:
        """Set the active view in the stacked pane."""
        if view_type == MindspaceViewType.VCS and not self._vcs_available:
            view_type = MindspaceViewType.CONVERSATIONS

        widget = self._view_widgets[view_type]
        self._pane_stack.setCurrentWidget(widget)
        self._active_view_type = view_type

        button = self._view_buttons[view_type]
        if not button.isChecked():
            button.setChecked(True)

        self._update_button_styling()

    def _toggle_sidebar(self) -> None:
        """Toggle the mindspace sidebar between expanded and collapsed states."""
        self.toggle_requested.emit()

    def set_collapsed(self, collapsed: bool) -> None:
        """Set the collapsed state, called by the splitter after it has moved."""
        self._sidebar_collapsed = collapsed
        self._update_button_styling()
        self.apply_style()

    @property
    def rail_width(self) -> int:
        """Return the fixed width of the icon rail."""
        return self._rail_collapsed_width

    def _on_vcs_repo_available(self, has_repo: bool) -> None:
        """Show or hide the VCS rail button when repository state changes."""
        self._vcs_available = has_repo
        self._vcs_button.setVisible(has_repo)

        if not has_repo and self._active_view_type == MindspaceViewType.VCS:
            self._set_active_view(MindspaceViewType.CONVERSATIONS)
        else:
            self._update_button_styling()

    def reveal_and_select_file(self, view_type: MindspaceViewType, file_path: str) -> None:
        """
        Reveal and select a file in the appropriate view.

        Args:
            file_path: Absolute path to the file to reveal and select
        """
        if not file_path or not self._mindspace_manager.has_mindspace():
            return

        match view_type:
            case MindspaceViewType.CONVERSATIONS:
                self._set_active_view(MindspaceViewType.CONVERSATIONS)
                self._conversations_view.reveal_and_select_file(file_path)

            case MindspaceViewType.FILES:
                self._set_active_view(MindspaceViewType.FILES)
                self._files_view.reveal_and_select_file(file_path)

            case MindspaceViewType.PREVIEW:
                self._set_active_view(MindspaceViewType.PREVIEW)
                self._preview_view.reveal_and_select_file(file_path)

            case MindspaceViewType.VCS:
                if self._vcs_available:
                    self._set_active_view(MindspaceViewType.VCS)

    def set_mindspace(self, path: str) -> None:
        """
        Set the mindspace root directory.

        Args:
            path: Path to the mindspace directory, or empty string to clear
        """
        if not path:
            self._mindspace_button.setText(self._language_manager.strings().mindspace_label_none)
            self._settings_button.hide()
        else:
            self._mindspace_button.setText(os.path.basename(path.rstrip("\\/")))
            self._settings_button.show()

        self._files_view.set_mindspace(path)
        self._conversations_view.set_mindspace(path)
        self._vcs_view.set_mindspace(path)
        self._preview_view.set_mindspace(path)

    def _on_settings_button_clicked(self) -> None:
        """Handle settings button click."""
        self.settings_requested.emit()

    def _on_language_changed(self) -> None:
        """Update when the language changes."""
        current_text = self._mindspace_button.text()
        none_text = self._language_manager.strings().mindspace_label_none
        if current_text == none_text or not current_text:
            self._mindspace_button.setText(none_text)

        strings = self._language_manager.strings()
        self._mindspace_button.setToolTip(strings.mindspace_name_tooltip)
        self._settings_button.setToolTip(strings.mindspace_settings)
        self._conversations_button.setToolTip(strings.mindspace_conversations)
        self._files_button.setToolTip(strings.mindspace_files)
        self._preview_button.setToolTip(strings.mindspace_preview)
        self._vcs_button.setToolTip(strings.mindspace_vcs)
        self._sidebar_toggle_button.setToolTip(
            strings.mindspace_collapse_sidebar if not self._sidebar_collapsed else strings.mindspace_expand_sidebar
        )

        self.apply_style()

    def _update_button_styling(self) -> None:
        """Update button icons and sizes."""
        zoom_factor = self._style_manager.zoom_factor()
        icon_base_size = 22
        icon_size = QSize(round(icon_base_size * zoom_factor), round(icon_base_size * zoom_factor))

        if self.layoutDirection() == Qt.LayoutDirection.LeftToRight:
            collapse_icon = "expand-right" if self._sidebar_collapsed else "expand-left"
        else:
            collapse_icon = "expand-left" if self._sidebar_collapsed else "expand-right"

        toggle_icon_size = round(20 * zoom_factor)
        self._sidebar_toggle_button.setIcon(QIcon(self._style_manager.scale_icon(f"inactive-{collapse_icon}", 20)))
        self._sidebar_toggle_button.setIconSize(QSize(toggle_icon_size, toggle_icon_size))
        self._sidebar_toggle_button.setProperty("icon_name", collapse_icon)

        for view_type, button in self._view_buttons.items():
            icon_name = button.property("icon_name")
            assert isinstance(icon_name, str)

            is_checked = view_type == self._active_view_type
            if is_checked:
                button.setIcon(QIcon(self._style_manager.scale_icon(f"bright-{icon_name}", icon_base_size)))

            else:
                button.setIcon(QIcon(self._style_manager.scale_icon(f"inactive-{icon_name}", icon_base_size)))

            button.setIconSize(icon_size)
            button.setChecked(is_checked)

        settings_icon_size = round(20 * zoom_factor)
        self._settings_button.setIcon(QIcon(self._style_manager.scale_icon("inactive-cog", 20)))
        self._settings_button.setIconSize(QSize(settings_icon_size, settings_icon_size))

    def _set_button_hover_icon(self, button: QToolButton, hovered: bool) -> None:
        """Update a rail button's icon to reflect hover state."""
        zoom_factor = self._style_manager.zoom_factor()
        icon_name = button.property("icon_name")
        if not isinstance(icon_name, str):
            return

        is_checked = button.isChecked()
        if hovered:
            size = 20 if button in (self._sidebar_toggle_button, self._settings_button) else 22
            button.setIcon(QIcon(self._style_manager.scale_icon(icon_name, size)))

        elif is_checked:
            button.setIcon(QIcon(self._style_manager.scale_icon(f"bright-{icon_name}", 22)))

        else:
            size = 20 if button in (self._sidebar_toggle_button, self._settings_button) else 22
            button.setIcon(QIcon(self._style_manager.scale_icon(f"inactive-{icon_name}", size)))

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """Handle hover events on rail buttons to update icon brightness."""
        if isinstance(obj, QToolButton) and obj.parent() is self._rail_widget:
            if event.type() == QEvent.Type.Enter:
                self._set_button_hover_icon(obj, hovered=True)

            elif event.type() == QEvent.Type.Leave:
                self._set_button_hover_icon(obj, hovered=False)

        return super().eventFilter(obj, event)

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        self._rail_collapsed_width = round(48 * zoom_factor)
        self._content_min_width = round(230 * zoom_factor)

        header_font = self._mindspace_button.font()
        header_font.setPointSizeF(base_font_size * zoom_factor)
        header_font.setBold(True)
        self._mindspace_button.setFont(header_font)

        rail_button_font = self.font()
        rail_button_font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(rail_button_font)

        self._update_button_styling()

        rail_width = self._rail_collapsed_width
        rail_button_height = round(48 * zoom_factor)
        rail_padding = round(6 * zoom_factor)
        rail_indicator = round(2 * zoom_factor)
        content_radius = round(8 * zoom_factor)
        header_bottom_border = self._style_manager.get_color_str(ColorRole.MENU_BORDER)
        panel_background = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        rail_background = self._style_manager.get_color_str(ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND)
        rail_hover = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        selected_background = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)
        header_background = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)
        header_hover = self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER)
        header_pressed = self._style_manager.get_color_str(ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED)
        text_color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        disabled_color = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        subtle_text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        border_color = self._style_manager.get_color_str(ColorRole.MENU_BORDER)
        accent_color = self._style_manager.get_color_str(ColorRole.TAB_BORDER_ACTIVE)
        content_surface = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)

        indicator_side = "border-left" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "border-right"

        self._rail_widget.setFixedWidth(rail_width)
        self.setMinimumWidth(rail_width)
        self._sidebar_toggle_button.setFixedHeight(rail_button_height)
        for button in self._view_buttons.values():
            button.setFixedHeight(rail_button_height)

        self._settings_button.setFixedHeight(rail_button_height)

        self.setStyleSheet(f"""
            {self._style_manager.get_menu_stylesheet()}
            {self._style_manager.get_scrollbar_stylesheet()}

            MindspaceView {{
                background-color: {panel_background};
            }}

            QWidget#_rail_widget {{
                background-color: {rail_background};
            }}

            QWidget#_content_widget {{
                background-color: {panel_background};
                border-right: 1px solid {border_color};
            }}

            QWidget#_header_widget {{
                background-color: {header_background};
                border-bottom: 1px solid {header_bottom_border};
            }}

            QWidget#_pane_stack {{
                background-color: {panel_background};
                border: none;
            }}

            QPushButton#_mindspace_button {{
                color: {text_color};
                background-color: transparent;
                border: none;
                margin: 0px;
                padding: 4px 6px;
                text-align: left;
            }}

            QPushButton#_mindspace_button:hover {{
                background-color: {header_hover};
            }}

            QPushButton#_mindspace_button:pressed {{
                background-color: {header_pressed};
            }}

            QPushButton#_mindspace_button:disabled {{
                color: {disabled_color};
            }}

            QToolButton#_sidebar_toggle_button {{
                color: {subtle_text};
                background-color: transparent;
                border: none;
                padding: {rail_padding}px;
                margin: 2px 0px;
            }}

            QToolButton#_settings_button,
            QToolButton[view_type] {{
                color: {text_color};
                background-color: transparent;
                border: none;
                padding: {rail_padding}px;
                margin: 2px 0px;
            }}

            QToolButton[view_type]:checked {{
                {indicator_side}: {rail_indicator}px solid {accent_color};
            }}

            QToolButton[view_type]:disabled,
            QToolButton#_settings_button:disabled {{
                color: {disabled_color};
            }}

            QWidget#_content_widget MindspaceConversationsView,
            QWidget#_content_widget MindspaceFilesView,
            QWidget#_content_widget MindspacePreviewView,
            QWidget#_content_widget MindspaceVCSView {{
                background-color: {content_surface};
                border: none;
                border-radius: {content_radius}px;
            }}

            QToolTip {{
                background-color: {rail_hover};
                color: {text_color};
                border: 1px solid {border_color};
                padding: 2px 4px;
            }}
        """)

        self._files_view.apply_style()
        self._conversations_view.apply_style()
        self._vcs_view.apply_style()
        self._preview_view.apply_style()
        self._update_button_styling()
