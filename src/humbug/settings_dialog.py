"""Unified settings dialog combining user and mindspace settings."""

import logging
from typing import Dict, cast

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QScrollArea,
    QWidget, QFrame, QListWidget, QListWidgetItem, QStackedWidget, QSplitter
)
from PySide6.QtCore import Signal, Qt
from PySide6.QtGui import QFont

from ai import AIBackendSettings, AIConversationSettings, AIManager, AIReasoningCapability
from ai_tool import AIToolManager

from humbug.language.language_code import LanguageCode
from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.settings.settings_checkbox import SettingsCheckbox
from humbug.settings.settings_container import SettingsContainer
from humbug.settings.settings_factory import SettingsFactory
from humbug.settings.settings_section import SettingsSection
from humbug.settings.settings_text_field import SettingsTextField
from humbug.style_manager import StyleManager, ColorMode
from humbug.user.user_file_sort_order import UserFileSortOrder
from humbug.user.user_manager import UserManager
from humbug.user.user_settings import UserSettings


# Section identifier constants
SECTION_DISPLAY = "display"
SECTION_FILE_ACCESS = "file_access"
SECTION_AI_BACKENDS = "ai_backends"
SECTION_AI_MODEL = "ai_model"
SECTION_AI_TOOLS = "ai_tools"
SECTION_EDITOR = "editor"
SECTION_TERMINAL = "terminal"

_ALL_MINDSPACES_SECTIONS = [SECTION_DISPLAY, SECTION_FILE_ACCESS, SECTION_AI_BACKENDS]
_THIS_MINDSPACE_SECTIONS = [SECTION_AI_MODEL, SECTION_AI_TOOLS, SECTION_EDITOR, SECTION_TERMINAL]


class SettingsDialog(QDialog):
    """Unified settings dialog for both user and mindspace settings."""

    user_settings_changed = Signal(UserSettings)
    mindspace_settings_changed = Signal(MindspaceSettings)

    def __init__(self, parent: QWidget | None = None) -> None:
        """
        Initialize the unified settings dialog.

        Args:
            parent: Parent widget, typically the main window.
        """
        super().__init__(parent)

        self._logger = logging.getLogger(__name__)
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._style_manager = StyleManager()
        self._ai_manager = AIManager()
        self._tool_manager = AIToolManager()
        self._user_manager = UserManager()

        self._initial_user_settings: UserSettings | None = None
        self._current_user_settings: UserSettings | None = None
        self._initial_mindspace_settings: MindspaceSettings | None = None
        self._current_mindspace_settings: MindspaceSettings | None = None
        self._has_mindspace = False

        self._ai_backend_controls: Dict[str, Dict[str, QWidget]] = {}
        self._tool_checkboxes: Dict[str, QWidget] = {}

        # Map section id -> (list item, stack page widget)
        self._section_items: Dict[str, QListWidgetItem] = {}
        self._section_pages: Dict[str, QWidget] = {}

        strings = self._language_manager.strings()
        self.setWindowTitle(strings.settings)
        self.setMinimumWidth(900)
        self.setMinimumHeight(700)
        self.setModal(True)

        self._build_ui()

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

    def _build_ui(self) -> None:
        """Construct the dialog layout."""
        strings = self._language_manager.strings()
        zoom_factor = self._style_manager.zoom_factor()

        main_layout = QVBoxLayout()
        main_layout.setSpacing(0)
        main_layout.setContentsMargins(0, 0, 0, 0)

        # --- Splitter: nav list on left, content stack on right ---
        self._splitter = QSplitter(Qt.Orientation.Horizontal)
        self._splitter.setHandleWidth(1)
        self._splitter.setChildrenCollapsible(False)

        # Left navigation list
        self._nav_list = QListWidget()
        self._nav_list.setObjectName("SettingsNavList")
        self._nav_list.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self._nav_list.setFixedWidth(int(200 * zoom_factor))
        self._nav_list.currentItemChanged.connect(self._on_nav_item_changed)

        # Right stacked content
        self._stack = QStackedWidget()

        self._splitter.addWidget(self._nav_list)
        self._splitter.addWidget(self._stack)
        self._splitter.setStretchFactor(0, 0)
        self._splitter.setStretchFactor(1, 1)
        self._splitter.setSizes([int(200 * zoom_factor), 700])

        main_layout.addWidget(self._splitter, 1)

        # Separator line above buttons
        separator = QFrame()
        separator.setFrameShape(QFrame.Shape.HLine)
        separator.setObjectName("SettingsSeparator")
        main_layout.addWidget(separator)

        # Button row
        spacing = int(self._style_manager.message_bubble_spacing() * zoom_factor)
        button_layout = QHBoxLayout()
        button_layout.setSpacing(spacing)
        button_layout.setContentsMargins(20, 12, 20, 12)
        button_layout.addStretch()

        min_button_width = int(90 * zoom_factor)
        min_button_height = 40

        self.ok_button = QPushButton(strings.ok)
        self.ok_button.setProperty("recommended", True)
        self.ok_button.clicked.connect(self._on_ok_clicked)

        self.apply_button = QPushButton(strings.apply)
        self.apply_button.clicked.connect(self._on_apply_clicked)
        self.apply_button.setEnabled(False)

        self.cancel_button = QPushButton(strings.cancel)
        self.cancel_button.clicked.connect(self.reject)

        for button in [self.ok_button, self.apply_button, self.cancel_button]:
            button.setMinimumWidth(min_button_width)
            button.setMinimumHeight(min_button_height)
            button.setContentsMargins(8, 8, 8, 8)
            button_layout.addWidget(button)

        button_layout.addStretch()
        main_layout.addLayout(button_layout)
        self.setLayout(main_layout)

        # Build nav groups and section pages
        self._add_nav_group(strings.settings_all_mindspaces)
        self._build_display_page()
        self._build_file_access_page()
        self._build_ai_backends_page()

        self._mindspace_group_item = self._add_nav_group(strings.settings_this_mindspace)
        self._build_ai_model_page()
        self._build_tools_page()
        self._build_editor_page()
        self._build_terminal_page()

    def _add_nav_group(self, label: str) -> QListWidgetItem:
        """Add a non-selectable group header to the nav list."""
        item = QListWidgetItem(label)
        item.setFlags(Qt.ItemFlag.NoItemFlags)
        item.setData(Qt.ItemDataRole.UserRole, None)
        font = item.font()
        font.setBold(True)
        item.setFont(font)
        self._nav_list.addItem(item)
        return item

    def _add_nav_section(self, section_id: str, label: str) -> QListWidgetItem:
        """Add a selectable section item to the nav list."""
        item = QListWidgetItem("  " + label)
        item.setData(Qt.ItemDataRole.UserRole, section_id)
        self._nav_list.addItem(item)
        self._section_items[section_id] = item
        return item

    def _make_scroll_page(self, container: SettingsContainer) -> QWidget:
        """Wrap a SettingsContainer in a scroll area page."""
        spacing = int(self._style_manager.message_bubble_spacing())
        container.setContentsMargins(spacing, spacing, spacing, spacing)

        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setWidget(container)
        return scroll

    def _build_display_page(self) -> None:
        """Build the Display settings page."""
        strings = self._language_manager.strings()
        container = SettingsContainer()

        self._display_heading = SettingsFactory.create_page_heading(strings.display_settings)
        container.add_setting(self._display_heading)

        self._language_combo = SettingsFactory.create_combo(strings.select_language)
        container.add_setting(self._language_combo)

        language_names = {
            LanguageCode.EN: "English",
            LanguageCode.FR: "Français",
            LanguageCode.AR: "العربية"
        }
        self._language_combo.set_items([(language_names[c], c) for c in LanguageCode])
        self._language_combo.set_value(self._language_manager.current_language())

        self._font_size_spin = SettingsFactory.create_double_spinbox(
            strings.font_size, 8.0, 24.0, 0.5, 1
        )
        container.add_setting(self._font_size_spin)

        self._theme_combo = SettingsFactory.create_combo(strings.display_theme)
        container.add_setting(self._theme_combo)
        self._theme_combo.set_items([
            (strings.theme_system, ColorMode.SYSTEM),
            (strings.theme_light, ColorMode.LIGHT),
            (strings.theme_dark, ColorMode.DARK),
        ])

        self._file_sort_combo = SettingsFactory.create_combo(strings.file_sort_order)
        container.add_setting(self._file_sort_combo)
        self._file_sort_combo.set_items([
            (strings.sort_directories_first, UserFileSortOrder.DIRECTORIES_FIRST),
            (strings.sort_alphabetical, UserFileSortOrder.ALPHABETICAL),
        ])

        container.add_stretch()
        container.value_changed.connect(self._on_value_changed)

        self._display_container = container
        page = self._make_scroll_page(container)
        self._stack.addWidget(page)
        self._section_pages[SECTION_DISPLAY] = page
        self._add_nav_section(SECTION_DISPLAY, strings.settings_display)

    def _build_file_access_page(self) -> None:
        """Build the File Access settings page."""
        strings = self._language_manager.strings()
        container = SettingsContainer()

        self._file_access_heading = SettingsFactory.create_page_heading(strings.external_file_access)
        container.add_setting(self._file_access_heading)

        self._allow_external_access_checkbox = SettingsFactory.create_checkbox(
            strings.allow_external_file_access
        )
        container.add_setting(self._allow_external_access_checkbox)

        self._external_allowlist_area = SettingsFactory.create_text_area(strings.external_file_allowlist)
        container.add_setting(self._external_allowlist_area)

        self._external_denylist_area = SettingsFactory.create_text_area(strings.external_file_denylist)
        container.add_setting(self._external_denylist_area)

        self._allow_external_access_checkbox.value_changed.connect(self._handle_external_access_enabled)

        container.add_stretch()
        container.value_changed.connect(self._on_value_changed)

        self._file_access_container = container
        page = self._make_scroll_page(container)
        self._stack.addWidget(page)
        self._section_pages[SECTION_FILE_ACCESS] = page
        self._add_nav_section(SECTION_FILE_ACCESS, strings.settings_file_access)

    def _build_ai_backends_page(self) -> None:
        """Build the AI Backends settings page."""
        strings = self._language_manager.strings()
        container = SettingsContainer()

        self._ai_backends_heading = SettingsFactory.create_page_heading(strings.ai_backend_config)
        container.add_setting(self._ai_backends_heading)

        ai_backend_mapping = [
            ("anthropic", strings.anthropic_backend),
            ("deepseek", strings.deepseek_backend),
            ("google", strings.google_backend),
            ("mistral", strings.mistral_backend),
            ("ollama", strings.ollama_backend),
            ("openai", strings.openai_backend),
            ("vllm", strings.vllm_backend),
            ("xai", strings.xai_backend),
            ("zai", strings.zai_backend),
        ]

        for backend_id, backend_name in ai_backend_mapping:
            backend_title = SettingsFactory.create_section(backend_name)
            container.add_setting(backend_title)

            enable_checkbox = SettingsFactory.create_checkbox(strings.enable_backend)
            container.add_setting(enable_checkbox)

            api_key_field = SettingsFactory.create_text_field(strings.api_key)
            container.add_setting(api_key_field)

            default_url = self._ai_manager.get_default_url(backend_id)
            url_field = SettingsFactory.create_text_field(strings.api_url, placeholder=default_url)
            container.add_setting(url_field)

            self._ai_backend_controls[backend_id] = {
                "enable": enable_checkbox,
                "key": api_key_field,
                "url": url_field,
                "title": backend_title,
            }

            enable_checkbox.value_changed.connect(
                lambda _checked=None, bid=backend_id: self._handle_backend_enabled(bid)
            )

            spacer = SettingsFactory.create_spacer(24)
            container.add_setting(spacer)

        container.add_stretch()
        container.value_changed.connect(self._on_value_changed)

        self._ai_backends_container = container
        page = self._make_scroll_page(container)
        self._stack.addWidget(page)
        self._section_pages[SECTION_AI_BACKENDS] = page
        self._add_nav_section(SECTION_AI_BACKENDS, strings.settings_ai_backends)

    def _build_ai_model_page(self) -> None:
        """Build the AI Model settings page."""
        strings = self._language_manager.strings()
        container = SettingsContainer()

        self._ai_model_heading = SettingsFactory.create_page_heading(strings.model_settings)
        container.add_setting(self._ai_model_heading)

        self._model_combo = SettingsFactory.create_combo(strings.settings_model_label)
        container.add_setting(self._model_combo)

        self._temp_spin = SettingsFactory.create_double_spinbox(
            strings.settings_temp_label, 0.0, 1.0, 0.1, 1
        )
        container.add_setting(self._temp_spin)

        self._reasoning_combo = SettingsFactory.create_combo(strings.settings_reasoning_label)
        container.add_setting(self._reasoning_combo)

        self._model_combo.value_changed.connect(self._on_model_value_changed)

        container.add_stretch()
        container.value_changed.connect(self._on_value_changed)

        self._ai_model_container = container
        page = self._make_scroll_page(container)
        self._stack.addWidget(page)
        self._section_pages[SECTION_AI_MODEL] = page
        self._add_nav_section(SECTION_AI_MODEL, strings.settings_ai_model)

    def _build_tools_page(self) -> None:
        """Build the Tools settings page."""
        strings = self._language_manager.strings()
        container = SettingsContainer()

        self._tools_heading = SettingsFactory.create_page_heading(strings.tool_settings)
        container.add_setting(self._tools_heading)

        tool_configs = self._tool_manager.get_all_tool_configs()
        for config in tool_configs:
            checkbox = SettingsFactory.create_checkbox(config.display_name)
            self._tool_checkboxes[config.name] = checkbox
            container.add_setting(checkbox)

        container.add_stretch()
        container.value_changed.connect(self._on_value_changed)

        self._tools_container = container
        page = self._make_scroll_page(container)
        self._stack.addWidget(page)
        self._section_pages[SECTION_AI_TOOLS] = page
        self._add_nav_section(SECTION_AI_TOOLS, strings.settings_ai_tools)

    def _build_editor_page(self) -> None:
        """Build the Editor settings page."""
        strings = self._language_manager.strings()
        container = SettingsContainer()

        self._editor_heading = SettingsFactory.create_page_heading(strings.editor_settings)
        container.add_setting(self._editor_heading)

        tabs_section = SettingsFactory.create_section(strings.settings_tabs)
        container.add_setting(tabs_section)
        self._editor_tabs_section = tabs_section

        self._soft_tabs_check = SettingsFactory.create_checkbox(strings.use_soft_tabs)
        container.add_setting(self._soft_tabs_check)

        self._tab_size_spin = SettingsFactory.create_spinbox(strings.tab_size, 1, 8, 1)
        container.add_setting(self._tab_size_spin)

        spacer = SettingsFactory.create_spacer(24)
        container.add_setting(spacer)

        backup_section = SettingsFactory.create_section(strings.backup_settings)
        container.add_setting(backup_section)

        self._auto_backup_check = SettingsFactory.create_checkbox(strings.auto_backup)
        container.add_setting(self._auto_backup_check)

        self._backup_interval_spin = SettingsFactory.create_spinbox(
            strings.backup_interval, 60, 3600, 60
        )
        container.add_setting(self._backup_interval_spin)

        self._auto_backup_check.value_changed.connect(self._on_auto_backup_changed)

        container.add_stretch()
        container.value_changed.connect(self._on_value_changed)

        self._editor_container = container
        page = self._make_scroll_page(container)
        self._stack.addWidget(page)
        self._section_pages[SECTION_EDITOR] = page
        self._add_nav_section(SECTION_EDITOR, strings.settings_editor)

    def _build_terminal_page(self) -> None:
        """Build the Terminal settings page."""
        strings = self._language_manager.strings()
        container = SettingsContainer()

        self._terminal_heading = SettingsFactory.create_page_heading(strings.terminal_settings)
        container.add_setting(self._terminal_heading)

        self._terminal_fixed_width_check = SettingsFactory.create_checkbox(
            strings.terminal_fixed_width_enabled
        )
        container.add_setting(self._terminal_fixed_width_check)

        self._terminal_fixed_width_spin = SettingsFactory.create_spinbox(
            strings.terminal_fixed_width, 40, 200, 2
        )
        container.add_setting(self._terminal_fixed_width_spin)

        self._terminal_scrollback_check = SettingsFactory.create_checkbox(
            strings.terminal_scrollback_enabled
        )
        container.add_setting(self._terminal_scrollback_check)

        self._terminal_scrollback_spin = SettingsFactory.create_spinbox(
            strings.terminal_scrollback_lines, 500, 10000, 100
        )
        container.add_setting(self._terminal_scrollback_spin)

        self._terminal_close_on_exit_check = SettingsFactory.create_checkbox(
            strings.terminal_close_on_exit
        )
        container.add_setting(self._terminal_close_on_exit_check)

        self._terminal_fixed_width_check.value_changed.connect(self._on_terminal_fixed_width_changed)
        self._terminal_scrollback_check.value_changed.connect(self._on_terminal_scrollback_changed)

        container.add_stretch()
        container.value_changed.connect(self._on_value_changed)

        self._terminal_container = container
        page = self._make_scroll_page(container)
        self._stack.addWidget(page)
        self._section_pages[SECTION_TERMINAL] = page
        self._add_nav_section(SECTION_TERMINAL, strings.settings_terminal)

    def set_settings(
        self,
        user_settings: UserSettings,
        mindspace_settings: MindspaceSettings | None,
        initial_section: str | None = None,
    ) -> None:
        """
        Populate the dialog with current settings.

        Args:
            user_settings: Current user settings (always provided).
            mindspace_settings: Current mindspace settings, or None if no mindspace is open.
            initial_section: Section identifier to show initially. Defaults to SECTION_DISPLAY.
        """
        self._has_mindspace = mindspace_settings is not None

        self._initial_user_settings = user_settings
        self._current_user_settings = self._copy_user_settings(user_settings)

        self._initial_mindspace_settings = mindspace_settings
        self._current_mindspace_settings = (
            self._copy_mindspace_settings(mindspace_settings)
            if mindspace_settings is not None
            else None
        )

        self._populate_user_settings(user_settings)
        self._populate_mindspace_settings(mindspace_settings)
        self._update_mindspace_sections_enabled()

        self._reset_modified_state()
        self.apply_button.setEnabled(False)

        # Navigate to the requested section
        target = initial_section if initial_section else SECTION_DISPLAY
        self._select_section(target)

    def get_user_settings(self) -> UserSettings:
        """Read current user settings from the dialog controls."""
        ai_backends = {}
        for backend_id, controls in self._ai_backend_controls.items():
            enabled = cast(SettingsCheckbox, controls["enable"]).get_value()
            api_key = cast(SettingsTextField, controls["key"]).get_value()
            url = cast(SettingsTextField, controls["url"]).get_value()
            ai_backends[backend_id] = AIBackendSettings(
                enabled=enabled, api_key=api_key, url=url
            )

        return UserSettings(
            ai_backends=ai_backends,
            language=self._language_combo.get_value(),
            font_size=self._font_size_spin.get_value(),
            theme=self._theme_combo.get_value(),
            file_sort_order=self._file_sort_combo.get_value(),
            allow_external_file_access=self._allow_external_access_checkbox.get_value(),
            external_file_allowlist=self._external_allowlist_area.get_value(),
            external_file_denylist=self._external_denylist_area.get_value(),
        )

    def get_mindspace_settings(self) -> MindspaceSettings | None:
        """Read current mindspace settings from the dialog controls, or None if no mindspace."""
        if not self._has_mindspace:
            return None

        enabled_tools = {
            name: cast(SettingsCheckbox, cb).get_value()
            for name, cb in self._tool_checkboxes.items()
        }

        return MindspaceSettings(
            use_soft_tabs=self._soft_tabs_check.get_value(),
            tab_size=self._tab_size_spin.get_value(),
            auto_backup=self._auto_backup_check.get_value(),
            auto_backup_interval=self._backup_interval_spin.get_value(),
            terminal_fixed_width_enabled=self._terminal_fixed_width_check.get_value(),
            terminal_fixed_width=self._terminal_fixed_width_spin.get_value(),
            terminal_scrollback_enabled=self._terminal_scrollback_check.get_value(),
            terminal_scrollback_lines=self._terminal_scrollback_spin.get_value(),
            terminal_close_on_exit=self._terminal_close_on_exit_check.get_value(),
            model=self._model_combo.get_text(),
            temperature=self._temp_spin.get_value(),
            reasoning=self._reasoning_combo.get_value(),
            enabled_tools=enabled_tools,
        )

    def _populate_user_settings(self, settings: UserSettings) -> None:
        """Load user settings into the dialog controls."""
        # Display
        self._language_combo.set_value(settings.language)
        self._font_size_spin.set_value(
            settings.font_size
            if settings.font_size is not None
            else self._style_manager.base_font_size()
        )
        self._theme_combo.set_value(settings.theme)
        self._file_sort_combo.set_value(settings.file_sort_order)

        # File access
        self._allow_external_access_checkbox.set_value(settings.allow_external_file_access)
        self._external_allowlist_area.set_value(settings.external_file_allowlist)
        self._external_denylist_area.set_value(settings.external_file_denylist)
        self._external_allowlist_area.set_enabled(settings.allow_external_file_access)

        # AI backends
        for backend_id, controls in self._ai_backend_controls.items():
            backend = settings.ai_backends.get(backend_id, AIBackendSettings())
            cast(SettingsCheckbox, controls["enable"]).set_value(backend.enabled)
            cast(SettingsTextField, controls["key"]).set_value(backend.api_key)
            cast(SettingsTextField, controls["url"]).set_value(backend.url)
            cast(SettingsTextField, controls["key"]).set_enabled(backend.enabled)
            cast(SettingsTextField, controls["url"]).set_enabled(backend.enabled)

    def _populate_mindspace_settings(self, settings: MindspaceSettings | None) -> None:
        """Load mindspace settings into the dialog controls."""
        if settings is None:
            return

        # AI model
        ai_backends = self._user_manager.get_ai_backends()
        models = [(m, m) for m in AIConversationSettings.iter_models_by_backends(ai_backends)]
        self._model_combo.set_items(models)
        self._model_combo.set_value(settings.model)
        self._temp_spin.set_value(settings.temperature)
        self._update_model_capabilities(settings.model)
        self._reasoning_combo.set_value(settings.reasoning)

        # Tools
        for tool_name, checkbox in self._tool_checkboxes.items():
            enabled = settings.enabled_tools.get(tool_name, True)
            cast(SettingsCheckbox, checkbox).set_value(enabled)

        # Editor
        self._soft_tabs_check.set_value(settings.use_soft_tabs)
        self._tab_size_spin.set_value(settings.tab_size)

        # Backup
        self._auto_backup_check.set_value(settings.auto_backup)
        self._backup_interval_spin.set_value(settings.auto_backup_interval)
        self._backup_interval_spin.set_enabled(settings.auto_backup)

        # Terminal
        self._terminal_fixed_width_check.set_value(settings.terminal_fixed_width_enabled)
        self._terminal_fixed_width_spin.set_value(settings.terminal_fixed_width)
        self._terminal_fixed_width_spin.set_enabled(settings.terminal_fixed_width_enabled)
        self._terminal_scrollback_check.set_value(settings.terminal_scrollback_enabled)
        self._terminal_scrollback_spin.set_value(settings.terminal_scrollback_lines)
        self._terminal_scrollback_spin.set_enabled(settings.terminal_scrollback_enabled)
        self._terminal_close_on_exit_check.set_value(settings.terminal_close_on_exit)

    def _update_mindspace_sections_enabled(self) -> None:
        """Enable or disable 'This Mindspace' nav items based on whether a mindspace is open."""
        for section_id in _THIS_MINDSPACE_SECTIONS:
            item = self._section_items.get(section_id)
            if item is None:
                continue

            if self._has_mindspace:
                item.setFlags(Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable)

            else:
                item.setFlags(Qt.ItemFlag.NoItemFlags)

    def _select_section(self, section_id: str) -> None:
        """Switch the nav list and stack to the given section."""
        item = self._section_items.get(section_id)
        page = self._section_pages.get(section_id)
        if item is None or page is None:
            return

        self._nav_list.setCurrentItem(item)
        self._stack.setCurrentWidget(page)

    def _on_nav_item_changed(self, current: QListWidgetItem, _previous: QListWidgetItem) -> None:
        """Switch the content stack when a nav item is selected."""
        if current is None:
            return

        section_id = current.data(Qt.ItemDataRole.UserRole)
        if section_id is None:
            return

        page = self._section_pages.get(section_id)
        if page is not None:
            self._stack.setCurrentWidget(page)

    def _on_value_changed(self) -> None:
        """Enable Apply when any setting has been modified."""
        if self._current_user_settings is None:
            return

        modified = any(
            c.is_modified()
            for c in [
                self._display_container,
                self._file_access_container,
                self._ai_backends_container,
                self._ai_model_container,
                self._tools_container,
                self._editor_container,
                self._terminal_container,
            ]
        )
        self.apply_button.setEnabled(modified)

    def _handle_external_access_enabled(self) -> None:
        """Enable or disable external file access text areas."""
        enabled = self._allow_external_access_checkbox.get_value()
        self._external_allowlist_area.set_enabled(enabled)

    def _handle_backend_enabled(self, backend_id: str) -> None:
        """Enable or disable key/URL fields for a backend."""
        controls = self._ai_backend_controls[backend_id]
        enabled = cast(SettingsCheckbox, controls["enable"]).get_value()
        cast(SettingsTextField, controls["key"]).set_enabled(enabled)
        cast(SettingsTextField, controls["url"]).set_enabled(enabled)

    def _on_model_value_changed(self) -> None:
        """Update capability controls when the model selection changes."""
        self._update_model_capabilities(self._model_combo.get_text())

    def _update_model_capabilities(self, model: str) -> None:
        """Refresh reasoning combo and temperature enable state for a model."""
        strings = self._language_manager.strings()
        capabilities = AIConversationSettings.get_reasoning_capability(model)

        items = []
        if capabilities & AIReasoningCapability.NO_REASONING:
            items.append((strings.settings_no_reasoning, AIReasoningCapability.NO_REASONING))
        if capabilities & AIReasoningCapability.HIDDEN_REASONING:
            items.append((strings.settings_hidden_reasoning, AIReasoningCapability.HIDDEN_REASONING))
        if capabilities & AIReasoningCapability.VISIBLE_REASONING:
            items.append((strings.settings_visible_reasoning, AIReasoningCapability.VISIBLE_REASONING))

        self._reasoning_combo.set_items(items)
        self._reasoning_combo.setEnabled(len(items) > 1)
        self._temp_spin.set_enabled(AIConversationSettings.supports_temperature(model))

    def _on_auto_backup_changed(self) -> None:
        """Enable or disable backup interval spin based on auto backup checkbox."""
        self._backup_interval_spin.set_enabled(self._auto_backup_check.get_value())

    def _on_terminal_fixed_width_changed(self) -> None:
        """Enable or disable fixed width spin based on checkbox."""
        self._terminal_fixed_width_spin.set_enabled(self._terminal_fixed_width_check.get_value())

    def _on_terminal_scrollback_changed(self) -> None:
        """Enable or disable scrollback lines spin based on checkbox."""
        self._terminal_scrollback_spin.set_enabled(self._terminal_scrollback_check.get_value())

    def _on_apply_clicked(self) -> None:
        """Apply all settings changes to both managers."""
        user_settings = self.get_user_settings()
        self._current_user_settings = user_settings
        self.user_settings_changed.emit(user_settings)

        mindspace_settings = self.get_mindspace_settings()
        if mindspace_settings is not None:
            self._current_mindspace_settings = mindspace_settings
            self._tool_manager.set_tool_enabled_states(mindspace_settings.enabled_tools)
            self.mindspace_settings_changed.emit(mindspace_settings)

        self._reset_modified_state()
        self.apply_button.setEnabled(False)

    def _on_ok_clicked(self) -> None:
        """Apply settings then close."""
        self._on_apply_clicked()
        self.accept()

    def reject(self) -> None:
        """Revert both settings to their initial values then close."""
        if self._initial_user_settings and self._current_user_settings != self._initial_user_settings:
            self.user_settings_changed.emit(self._initial_user_settings)

        if (
            self._initial_mindspace_settings is not None
            and self._current_mindspace_settings != self._initial_mindspace_settings
        ):
            self._tool_manager.set_tool_enabled_states(
                self._initial_mindspace_settings.enabled_tools
            )
            self.mindspace_settings_changed.emit(self._initial_mindspace_settings)

        super().reject()

    def _on_language_changed(self) -> None:
        """Update all dialog text when the language changes."""
        strings = self._language_manager.strings()
        self.setWindowTitle(strings.settings)

        self.ok_button.setText(strings.ok)
        self.apply_button.setText(strings.apply)
        self.cancel_button.setText(strings.cancel)

        # Update nav group labels (non-selectable items)
        for i in range(self._nav_list.count()):
            item = self._nav_list.item(i)
            section_id = item.data(Qt.ItemDataRole.UserRole)
            if section_id is None:
                # Group header — identify by position
                pass  # Rebuilt on next open; labels are set at build time

        # Update section nav labels
        label_map = {
            SECTION_DISPLAY: strings.settings_display,
            SECTION_FILE_ACCESS: strings.settings_file_access,
            SECTION_AI_BACKENDS: strings.settings_ai_backends,
            SECTION_AI_MODEL: strings.settings_ai_model,
            SECTION_AI_TOOLS: strings.settings_ai_tools,
            SECTION_EDITOR: strings.settings_editor,
            SECTION_TERMINAL: strings.settings_terminal,
        }
        for section_id, item in self._section_items.items():
            item.setText("  " + label_map.get(section_id, section_id))

        # Update page headings
        self._display_heading.set_label(strings.display_settings)
        self._file_access_heading.set_label(strings.external_file_access)
        self._ai_backends_heading.set_label(strings.ai_backend_config)
        self._ai_model_heading.set_label(strings.model_settings)
        self._tools_heading.set_label(strings.tool_settings)
        self._editor_heading.set_label(strings.editor_settings)
        self._terminal_heading.set_label(strings.terminal_settings)

        # Update Display page controls
        current_lang = self._language_combo.get_value()
        language_names = {
            LanguageCode.EN: "English",
            LanguageCode.FR: "Français",
            LanguageCode.AR: "العربية",
        }
        self._language_combo.set_label(strings.select_language)
        self._language_combo.set_items([(language_names[c], c) for c in LanguageCode])
        self._language_combo.set_value(current_lang)

        self._font_size_spin.set_label(strings.font_size)

        current_theme = self._theme_combo.get_value()
        self._theme_combo.set_label(strings.display_theme)
        self._theme_combo.set_items([
            (strings.theme_system, ColorMode.SYSTEM),
            (strings.theme_light, ColorMode.LIGHT),
            (strings.theme_dark, ColorMode.DARK),
        ])
        self._theme_combo.set_value(current_theme)

        current_sort = self._file_sort_combo.get_value()
        self._file_sort_combo.set_label(strings.file_sort_order)
        self._file_sort_combo.set_items([
            (strings.sort_directories_first, UserFileSortOrder.DIRECTORIES_FIRST),
            (strings.sort_alphabetical, UserFileSortOrder.ALPHABETICAL),
        ])
        self._file_sort_combo.set_value(current_sort)

        # Update File Access page controls
        self._allow_external_access_checkbox.set_label(strings.allow_external_file_access)
        self._external_allowlist_area.set_label(strings.external_file_allowlist)
        self._external_denylist_area.set_label(strings.external_file_denylist)

        # Update AI Backends page controls
        backend_name_map = {
            "anthropic": strings.anthropic_backend,
            "deepseek": strings.deepseek_backend,
            "google": strings.google_backend,
            "mistral": strings.mistral_backend,
            "ollama": strings.ollama_backend,
            "openai": strings.openai_backend,
            "vllm": strings.vllm_backend,
            "xai": strings.xai_backend,
            "zai": strings.zai_backend,
        }
        for backend_id, controls in self._ai_backend_controls.items():
            cast(SettingsSection, controls["title"]).set_label(backend_name_map[backend_id])
            cast(SettingsCheckbox, controls["enable"]).set_label(strings.enable_backend)
            cast(SettingsTextField, controls["key"]).set_label(strings.api_key)
            cast(SettingsTextField, controls["url"]).set_label(strings.api_url)

        # Update AI Model page controls
        self._model_combo.set_label(strings.settings_model_label)
        self._temp_spin.set_label(strings.settings_temp_label)
        current_model = self._model_combo.get_text()
        self._update_model_capabilities(current_model)
        self._reasoning_combo.set_label(strings.settings_reasoning_label)

        # Update Editor page controls
        self._soft_tabs_check.set_label(strings.use_soft_tabs)
        self._tab_size_spin.set_label(strings.tab_size)
        self._editor_tabs_section.set_label(strings.settings_tabs)

        # Update Backup page controls
        self._auto_backup_check.set_label(strings.auto_backup)
        self._backup_interval_spin.set_label(strings.backup_interval)

        # Update Terminal page controls
        self._terminal_fixed_width_check.set_label(strings.terminal_fixed_width_enabled)
        self._terminal_fixed_width_spin.set_label(strings.terminal_fixed_width)
        self._terminal_scrollback_check.set_label(strings.terminal_scrollback_enabled)
        self._terminal_scrollback_spin.set_label(strings.terminal_scrollback_lines)
        self._terminal_close_on_exit_check.set_label(strings.terminal_close_on_exit)

        self.adjustSize()

    def _on_style_changed(self) -> None:
        """Update dialog styling when the application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        nav_bg = self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        nav_text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        nav_disabled = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        nav_selected_bg = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)
        nav_hover_bg = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        splitter_color = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)
        font_pt = int(base_font_size * zoom_factor)

        self.setStyleSheet(self._style_manager.get_dialog_stylesheet() + f"""
            QListWidget#SettingsNavList {{
                background-color: {nav_bg};
                border: none;
                border-radius: 0px;
                padding: 8px 0px;
                font-size: {font_pt}pt;
                outline: none;
            }}
            QListWidget#SettingsNavList::item {{
                color: {nav_text};
                padding: 6px 12px;
                border: none;
            }}
            QListWidget#SettingsNavList::item:selected {{
                background-color: {nav_selected_bg};
                color: {nav_text};
            }}
            QListWidget#SettingsNavList::item:hover:!selected {{
                background-color: {nav_hover_bg};
            }}
            QListWidget#SettingsNavList::item:disabled {{
                color: {nav_disabled};
            }}
            QSplitter::handle {{
                background-color: {splitter_color};
                width: 1px;
            }}
        """)

        # Update nav list font sizes for group headers
        bold_font = QFont()
        bold_font.setBold(True)
        bold_font.setPointSizeF(base_font_size * zoom_factor * 0.85)

        section_font = QFont()
        section_font.setPointSizeF(base_font_size * zoom_factor)

        for i in range(self._nav_list.count()):
            item = self._nav_list.item(i)
            section_id = item.data(Qt.ItemDataRole.UserRole)
            if section_id is None:
                item.setFont(bold_font)

            else:
                item.setFont(section_font)

        # Update nav list width constraints
        self._nav_list.setFixedWidth(int(200 * zoom_factor))
        self._splitter.setSizes([int(200 * zoom_factor), self._stack.width()])

    def _reset_modified_state(self) -> None:
        """Reset modified tracking on all containers."""
        for container in [
            self._display_container,
            self._file_access_container,
            self._ai_backends_container,
            self._ai_model_container,
            self._tools_container,
            self._editor_container,
            self._terminal_container,
        ]:
            container.reset_modified_state()

    @staticmethod
    def _copy_user_settings(settings: UserSettings) -> UserSettings:
        """Return a shallow-enough copy of UserSettings for change detection."""
        return UserSettings(
            ai_backends={
                k: AIBackendSettings(enabled=v.enabled, api_key=v.api_key, url=v.url)
                for k, v in settings.ai_backends.items()
            },
            language=settings.language,
            font_size=settings.font_size,
            theme=settings.theme,
            file_sort_order=settings.file_sort_order,
            allow_external_file_access=settings.allow_external_file_access,
            external_file_allowlist=list(settings.external_file_allowlist),
            external_file_denylist=list(settings.external_file_denylist),
        )

    @staticmethod
    def _copy_mindspace_settings(settings: MindspaceSettings) -> MindspaceSettings:
        """Return a copy of MindspaceSettings for change detection."""
        return MindspaceSettings(
            use_soft_tabs=settings.use_soft_tabs,
            tab_size=settings.tab_size,
            auto_backup=settings.auto_backup,
            auto_backup_interval=settings.auto_backup_interval,
            terminal_fixed_width_enabled=settings.terminal_fixed_width_enabled,
            terminal_fixed_width=settings.terminal_fixed_width,
            terminal_scrollback_enabled=settings.terminal_scrollback_enabled,
            terminal_scrollback_lines=settings.terminal_scrollback_lines,
            terminal_close_on_exit=settings.terminal_close_on_exit,
            model=settings.model,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            enabled_tools=settings.enabled_tools.copy(),
        )
