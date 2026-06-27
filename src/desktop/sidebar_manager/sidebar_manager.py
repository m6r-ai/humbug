import os
from typing import Callable

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

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.style_manager import StyleManager
from desktop.url_opener import open_url


class SidebarManager(QWidget):
    """
    Sidebar container: manages the icon rail and the active panel pane.

    The manager is generic — it has no knowledge of individual panel types.
    Panels are registered via ``register_panel`` after construction.
    ``main_window.py`` is the wiring point.
    """

    open_mindspace_requested = Signal()
    update_check_requested = Signal()
    file_clicked = Signal(str, str, bool)           # panel_id, path, ephemeral
    toggle_requested = Signal()
    file_deleted = Signal(str)
    file_renamed = Signal(str, str)
    file_moved = Signal(str, str)
    file_opened_in_editor = Signal(str, bool)
    file_opened_in_preview = Signal(str)
    file_opened_in_diff = Signal(str, bool)
    new_conversation_requested = Signal(str)
    settings_requested = Signal()
    tab_overview_requested = Signal()
    tab_carousel_requested = Signal()

    # panel_id, path, ephemeral, query, case_sensitive, regexp, line_number, message_id
    search_result_activated = Signal(str, str, bool, str, bool, bool, object, object)
    search_highlights_cleared = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the sidebar manager."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._mindspace_manager = MindspaceManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        self._active_panel_id: str = ""
        self._sidebar_collapsed = False
        self._update_release_url: str | None = None
        self._expanded_sidebar_width = 320
        self._rail_collapsed_width = 48
        self._content_min_width = 240

        # Keyed by panel_id
        self._panel_buttons: dict[str, QToolButton] = {}
        self._panel_widgets: dict[str, SidebarBase] = {}
        self._panel_insert_index: int = 1  # index in rail_layout after toggle button

        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._rail_widget = QWidget(self)
        self._rail_widget.setObjectName("_rail_widget")
        self._rail_widget.setSizePolicy(QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Expanding)
        self._rail_layout = QVBoxLayout(self._rail_widget)
        self._rail_layout.setContentsMargins(0, 0, 0, 0)
        self._rail_layout.setSpacing(0)

        self._view_button_group = QButtonGroup(self)
        self._view_button_group.setExclusive(True)

        self._sidebar_toggle_button = QToolButton(self._rail_widget)
        self._sidebar_toggle_button.setObjectName("_sidebar_toggle_button")
        self._sidebar_toggle_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        self._sidebar_toggle_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._sidebar_toggle_button.clicked.connect(self._toggle_sidebar)
        self._sidebar_toggle_button.setProperty("icon_name", "expand-right")
        self._sidebar_toggle_button.installEventFilter(self)
        self._rail_layout.addWidget(self._sidebar_toggle_button)

        self._content_widget = QWidget(self)
        self._content_widget.setObjectName("_content_widget")
        self._content_widget.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Expanding)
        content_layout = QVBoxLayout(self._content_widget)
        content_layout.setContentsMargins(0, 0, 0, 0)
        content_layout.setSpacing(0)

        self._header_widget = QPushButton(self._content_widget)
        self._header_widget.setObjectName("_header_widget")
        self._header_widget.clicked.connect(self.open_mindspace_requested.emit)
        content_layout.addWidget(self._header_widget)

        self._pane_stack = QStackedWidget(self._content_widget)
        self._pane_stack.setObjectName("_pane_stack")
        content_layout.addWidget(self._pane_stack)

        self._panel_class_names: list[str] = []
        self._rail_layout.addStretch()

        self._update_button = QToolButton(self._rail_widget)
        self._update_button.setObjectName("_update_button")
        self._update_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        self._update_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._update_button.clicked.connect(self._on_update_button_clicked)
        self._update_button.setProperty("icon_name", "update")
        self._update_button.installEventFilter(self)
        self._update_button.hide()
        self._rail_layout.addWidget(self._update_button)

        self._tab_overview_button = QToolButton(self._rail_widget)
        self._tab_overview_button.setObjectName("_tab_overview_button")
        self._tab_overview_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        self._tab_overview_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._tab_overview_button.clicked.connect(self._on_tab_overview_button_clicked)
        self._tab_overview_button.setProperty("icon_name", "tab-overview")
        self._tab_overview_button.installEventFilter(self)
        self._rail_layout.addWidget(self._tab_overview_button)

        self._tab_carousel_button = QToolButton(self._rail_widget)
        self._tab_carousel_button.setObjectName("_tab_carousel_button")
        self._tab_carousel_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        self._tab_carousel_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._tab_carousel_button.clicked.connect(self._on_tab_carousel_button_clicked)
        self._tab_carousel_button.setProperty("icon_name", "tab-carousel")
        self._tab_carousel_button.installEventFilter(self)
        self._rail_layout.addWidget(self._tab_carousel_button)

        self._settings_button = QToolButton(self._rail_widget)
        self._settings_button.setObjectName("_settings_button")
        self._settings_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        self._settings_button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._settings_button.clicked.connect(self._on_settings_button_clicked)
        self._settings_button.setProperty("icon_name", "cog")
        self._settings_button.installEventFilter(self)
        self._rail_layout.addWidget(self._settings_button)

        layout.addWidget(self._rail_widget)
        layout.addWidget(self._content_widget, 1)

        self._header_widget.setText(self._language_manager.strings().mindspace_label_none)
        self._on_language_changed()

    def register_panel(
        self,
        panel_id: str,
        icon_name: str,
        factory: "Callable[[QWidget], SidebarBase]",
        wire_signals: "Callable[[SidebarBase, SidebarManager], None]",
        visibility_signal: str | None = None,
        on_activated: "Callable[[SidebarBase], None] | None" = None,
    ) -> None:
        """
        Register a sidebar panel.

        Must be called before the sidebar is shown.  Panels appear in the rail
        in registration order.

        Args:
            panel_id: Unique string identifier for this panel (e.g. ``"files"``).
            icon_name: Base icon name for the rail button.
            factory: Callable that constructs the panel widget given a parent.
            wire_signals: Callable that connects panel-specific signals to this
                manager after the panel is constructed.
            visibility_signal: Optional name of a ``bool``-carrying signal on the
                panel that controls rail button visibility.  The button starts
                hidden when this is set.
            on_activated: Optional callback invoked each time this panel's rail
                button is clicked and the panel becomes active.
        """
        panel = factory(self)
        assert isinstance(panel, SidebarBase)

        button = self._create_panel_button(panel_id, icon_name)

        if visibility_signal is not None:
            sig = getattr(panel, visibility_signal, None)
            if sig is not None:
                sig.connect(lambda visible, pid=panel_id: self._on_panel_visibility_changed(pid, visible))

            button.hide()

        if on_activated is not None:
            on_act = on_activated
            button.clicked.connect(
                lambda checked, pid=panel_id, cb=on_act: self._on_panel_button_clicked(pid, checked, cb)
            )

        else:
            button.clicked.connect(
                lambda checked, pid=panel_id: self._on_panel_button_clicked(pid, checked, None)
            )

        self._rail_layout.insertWidget(self._panel_insert_index, button)
        self._panel_insert_index += 1

        self._panel_buttons[panel_id] = button
        self._panel_widgets[panel_id] = panel
        self._pane_stack.addWidget(panel)
        self._panel_class_names.append(type(panel).__name__)

        wire_signals(panel, self)

        if len(self._panel_widgets) == 1:
            self._set_active_panel(panel_id)

    def _create_panel_button(self, panel_id: str, icon_name: str) -> QToolButton:
        """
        Create a rail button for a panel without connecting its click signal.

        Args:
            panel_id: The panel identifier string.
            icon_name: Base icon name for the button.
        """
        button = QToolButton(self._rail_widget)
        button.setObjectName(f"_panel_button_{panel_id}")
        button.setProperty("panel_id", panel_id)
        button.setProperty("icon_name", icon_name)
        button.setCheckable(True)
        button.setAutoRaise(False)
        button.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        button.installEventFilter(self)
        self._view_button_group.addButton(button)
        return button

    def _on_panel_button_clicked(
        self,
        panel_id: str,
        checked: bool,
        on_activated: "Callable[[SidebarBase], None] | None",
    ) -> None:
        """
        Handle rail button click: switch active panel and expand if collapsed.

        Args:
            panel_id: The panel to activate.
            checked: Whether the button is now checked.
            on_activated: Optional callback to invoke on the panel after activation.
        """
        if not checked:
            return

        self._set_active_panel(panel_id)
        if self._sidebar_collapsed:
            self._toggle_sidebar()

        if on_activated is not None:
            panel = self._panel_widgets.get(panel_id)
            if panel is not None:
                on_activated(panel)

    def _set_active_panel(self, panel_id: str) -> None:
        """
        Make the given panel current in the stacked pane.

        Args:
            panel_id: ID of the panel to activate.
        """
        widget = self._panel_widgets.get(panel_id)
        if widget is None:
            return

        self._pane_stack.setCurrentWidget(widget)
        self._active_panel_id = panel_id

        button = self._panel_buttons.get(panel_id)
        if button is not None and not button.isChecked():
            button.setChecked(True)

        self._update_button_styling()

    def _on_panel_visibility_changed(self, panel_id: str, visible: bool) -> None:
        """
        Show or hide a panel's rail button based on its visibility signal.

        If the panel being hidden is currently active, fall back to the first
        visible panel.

        Args:
            panel_id: The panel whose visibility changed.
            visible: Whether the panel should now be visible.
        """
        button = self._panel_buttons.get(panel_id)
        if button is None:
            return

        button.setVisible(visible)

        if not visible and self._active_panel_id == panel_id:
            for pid, btn in self._panel_buttons.items():
                if btn.isVisible() and pid != panel_id:
                    self._set_active_panel(pid)
                    break

        else:
            self._update_button_styling()

    def _toggle_sidebar(self) -> None:
        """Emit toggle_requested to let the splitter collapse/expand the sidebar."""
        self.toggle_requested.emit()

    def set_collapsed(self, collapsed: bool) -> None:
        """
        Record the collapsed state after the splitter has moved.

        Args:
            collapsed: Whether the sidebar content pane is now hidden.
        """
        self._sidebar_collapsed = collapsed
        self._update_button_styling()
        self.apply_style()

    def rail_width(self) -> int:
        """Return the fixed pixel width of the icon rail."""
        return self._rail_collapsed_width

    def sidebar_collapsed(self) -> bool:
        """Return whether the sidebar is currently collapsed."""
        return self._sidebar_collapsed

    def expanded_sidebar_width(self) -> int:
        """Return the last recorded expanded width of the sidebar."""
        return self._expanded_sidebar_width

    def set_expanded_sidebar_width(self, width: int) -> None:
        """
        Record the expanded sidebar width.

        Args:
            width: Width in pixels.
        """
        self._expanded_sidebar_width = width

    def content_min_width(self) -> int:
        """Return the minimum width of the sidebar content pane."""
        return self._content_min_width

    def reveal_and_select_file(self, panel_id: str, file_path: str) -> None:
        """
        Reveal and select a file in the named panel.

        The panel must already be the active panel — this method never switches
        the sidebar view.  Callers that want to force a panel switch first should
        call show_panel() explicitly before calling this method.

        Args:
            panel_id: ID of the panel that should handle the reveal.
            file_path: Absolute path to the file to reveal and select.
        """
        if not file_path or not self._mindspace_manager.has_mindspace():
            return

        panel = self._panel_widgets.get(panel_id)
        if panel is None:
            return

        if self._active_panel_id != panel_id:
            return

        panel.reveal_and_select_file(file_path)

    def show_panel(self, panel_id: str) -> None:
        """
        Activate a panel by ID and expand the sidebar if collapsed.

        Args:
            panel_id: ID of the panel to show.
        """
        self._set_active_panel(panel_id)
        if self._sidebar_collapsed:
            self.toggle_requested.emit()

    def set_mindspace(self, path: str) -> None:
        """
        Propagate a mindspace change to all registered panels.

        Args:
            path: Absolute path to the new mindspace root, or empty string to clear.
        """
        if not path:
            self._header_widget.setText(self._language_manager.strings().mindspace_label_none)

        else:
            self._header_widget.setText(os.path.basename(path.rstrip("\\/")))

        for panel in self._panel_widgets.values():
            panel.set_mindspace(path)

    def show_update_available(self, version: str, release_url: str) -> None:
        """
        Show the update rail button.

        Args:
            version: Latest version string, e.g. ``"v49"``.
            release_url: URL to the release page.
        """
        self._update_release_url = release_url
        strings = self._language_manager.strings()
        self._update_button.setToolTip(strings.update_tooltip.format(version))
        self._update_button.show()
        self._update_button_styling()

    def _on_update_button_clicked(self) -> None:
        """Open the release page in the system browser."""
        if self._update_release_url:
            open_url(self._update_release_url)

    def _on_settings_button_clicked(self) -> None:
        """Forward settings button click."""
        self.settings_requested.emit()

    def _on_tab_overview_button_clicked(self) -> None:
        """Forward tab overview button click."""
        self.tab_overview_requested.emit()

    def _on_tab_carousel_button_clicked(self) -> None:
        """Forward tab carousel button click."""
        self.tab_carousel_requested.emit()

    def _on_language_changed(self) -> None:
        """Refresh localised strings on all rail controls."""
        current_text = self._header_widget.text()
        none_text = self._language_manager.strings().mindspace_label_none
        if current_text == none_text or not current_text:
            self._header_widget.setText(none_text)

        strings = self._language_manager.strings()
        self._header_widget.setToolTip(strings.mindspace_name_tooltip)
        self._settings_button.setToolTip(strings.mindspace_settings)
        self._settings_button.setToolTip(strings.settings)
        self._tab_overview_button.setToolTip(strings.tab_overview_tooltip)
        self._tab_carousel_button.setToolTip(strings.tab_carousel_tooltip)
        self._sidebar_toggle_button.setToolTip(
            strings.mindspace_collapse_sidebar if not self._sidebar_collapsed else strings.mindspace_expand_sidebar
        )

        self.apply_style()

    def _update_button_styling(self) -> None:
        """Refresh icons and sizes on all rail buttons."""
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

        for panel_id, button in self._panel_buttons.items():
            icon_name = button.property("icon_name")
            assert isinstance(icon_name, str)
            is_checked = panel_id == self._active_panel_id
            if is_checked:
                button.setIcon(QIcon(self._style_manager.scale_icon(f"bright-{icon_name}", icon_base_size)))

            else:
                button.setIcon(QIcon(self._style_manager.scale_icon(f"inactive-{icon_name}", icon_base_size)))

            button.setIconSize(icon_size)
            button.setChecked(is_checked)

        settings_icon_size = round(20 * zoom_factor)
        self._settings_button.setIcon(QIcon(self._style_manager.scale_icon("inactive-cog", 20)))
        self._settings_button.setIconSize(QSize(settings_icon_size, settings_icon_size))

        tab_button_icon_size = round(20 * zoom_factor)
        self._tab_overview_button.setIcon(QIcon(self._style_manager.scale_icon("inactive-tab-overview", 20)))
        self._tab_overview_button.setIconSize(QSize(tab_button_icon_size, tab_button_icon_size))
        self._tab_carousel_button.setIcon(QIcon(self._style_manager.scale_icon("inactive-tab-carousel", 20)))
        self._tab_carousel_button.setIconSize(QSize(tab_button_icon_size, tab_button_icon_size))

        if self._update_button.isVisible():
            update_icon_size = round(20 * zoom_factor)
            self._update_button.setIcon(QIcon(self._style_manager.scale_icon("update", 20)))
            self._update_button.setIconSize(QSize(update_icon_size, update_icon_size))

    def _set_button_hover_icon(self, button: QToolButton, hovered: bool) -> None:
        """
        Update a rail button's icon for hover state.

        Args:
            button: The button whose icon should change.
            hovered: Whether the cursor is currently over the button.
        """
        icon_name = button.property("icon_name")
        if not isinstance(icon_name, str):
            return

        is_checked = button.isChecked()
        is_update_button = button is self._update_button
        small_buttons = (
            self._sidebar_toggle_button, self._settings_button, self._update_button,
            self._tab_overview_button, self._tab_carousel_button,
        )
        if hovered:
            size = 20 if button in small_buttons else 22
            button.setIcon(QIcon(self._style_manager.scale_icon(icon_name, size)))

        elif is_checked:
            button.setIcon(QIcon(self._style_manager.scale_icon(f"bright-{icon_name}", 22)))

        else:
            size = 20 if button in small_buttons else 22
            prefix = "" if is_update_button else "inactive-"
            button.setIcon(QIcon(self._style_manager.scale_icon(f"{prefix}{icon_name}", size)))

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """Update rail button icons on hover enter/leave."""
        if isinstance(obj, QToolButton) and obj.parent() is self._rail_widget:
            if event.type() == QEvent.Type.Enter:
                self._set_button_hover_icon(obj, hovered=True)

            elif event.type() == QEvent.Type.Leave:
                self._set_button_hover_icon(obj, hovered=False)

        return super().eventFilter(obj, event)

    def apply_style(self) -> None:
        """Apply current application style to the manager and all panels."""
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor()
        base_font_size = style_manager.base_font_size()

        self._rail_collapsed_width = round(48 * zoom_factor)
        self._content_min_width = round(230 * zoom_factor)

        header_font = self._header_widget.font()
        header_font.setPointSizeF(base_font_size * zoom_factor)
        header_font.setBold(True)
        self._header_widget.setFont(header_font)

        rail_button_font = self.font()
        rail_button_font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(rail_button_font)

        self._update_button_styling()

        rail_width = self._rail_collapsed_width
        rail_button_height = round(48 * zoom_factor)
        rail_padding = round(6 * zoom_factor)
        rail_indicator = 2
        content_radius = round(8 * zoom_factor)
        panel_background = style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        rail_background = style_manager.get_color_str(ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND)
        rail_hover = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        text_color = style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        disabled_color = style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        subtle_text = style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        border_color = style_manager.get_color_str(ColorRole.MENU_BORDER)
        accent_color = style_manager.get_color_str(ColorRole.TAB_BORDER_ACTIVE)
        content_surface = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)

        indicator_side = "border-left" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "border-right"
        indicator_padding_side = "padding-left" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "padding-right"

        self._rail_widget.setFixedWidth(rail_width)
        self.setMinimumWidth(rail_width)
        self._sidebar_toggle_button.setFixedHeight(rail_button_height)
        for button in self._panel_buttons.values():
            button.setFixedHeight(rail_button_height)

        self._settings_button.setFixedHeight(rail_button_height)
        self._tab_overview_button.setFixedHeight(rail_button_height)
        self._tab_carousel_button.setFixedHeight(rail_button_height)

        # Build a QSS selector that matches all registered panel widget class names
        panel_selectors_rule = ""
        if self._panel_class_names:
            panel_selectors = ",\n            ".join(
                f"QWidget#_content_widget {name}" for name in self._panel_class_names
            )
            panel_selectors_rule = f"""
            {panel_selectors} {{
                background-color: {content_surface};
                border: none;
                border-radius: {content_radius}px;
                margin: {round(4 * zoom_factor)}px;
            }}"""

        self.setStyleSheet(f"""
            {self._style_manager.get_scrollbar_stylesheet()}

            QWidget#_rail_widget {{
                background-color: {rail_background};
            }}

            QWidget#_content_widget {{
                background-color: {panel_background};
            }}

            QPushButton#_header_widget {{
                background-color: {panel_background};
                border: none;
                color: {subtle_text};
                padding: 10px 8px 6px 8px;
                text-align: left;
                text-transform: uppercase;
            }}

            QPushButton#_header_widget:hover {{
                color: {text_color};
            }}

            QWidget#_pane_stack {{
                background-color: {panel_background};
                border: none;
            }}

            QToolButton#_sidebar_toggle_button {{
                color: {subtle_text};
                background-color: transparent;
                border: none;
                padding: {rail_padding}px;
                margin: 2px 0px;
            }}

            QToolButton#_settings_button,
            QToolButton#_update_button,
            QToolButton#_tab_overview_button,
            QToolButton#_tab_carousel_button,
            QToolButton[panel_id] {{
                color: {text_color};
                background-color: transparent;
                border: none;
                padding: {rail_padding}px;
                margin: 2px 0px;
            }}

            QToolButton[panel_id]:checked {{
                {indicator_side}: {rail_indicator}px solid {accent_color};
                {indicator_padding_side}: {rail_padding - rail_indicator}px;
            }}

            QToolButton[panel_id]:disabled,
            QToolButton#_settings_button:disabled {{
                color: {disabled_color};
            }}

            {panel_selectors_rule}

            QToolTip {{
                background-color: {rail_hover};
                color: {text_color};
                border: 1px solid {border_color};
                padding: 2px 4px;
            }}
        """)

        for panel in self._panel_widgets.values():
            panel.apply_style()
