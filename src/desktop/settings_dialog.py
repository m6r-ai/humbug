"""Unified settings dialog combining user and mindspace settings."""

import asyncio
import logging
import os
from typing import Dict, List, cast

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QScrollArea,
    QWidget, QFrame, QListWidget, QListWidgetItem, QStackedWidget, QSplitter,
    QStyledItemDelegate, QStyleOptionViewItem, QLabel
)
from PySide6.QtCore import QModelIndex, QPersistentModelIndex, QSize, Signal, Qt
from PySide6.QtGui import QFont

from ai import AIBackendSettings, AIConversationSettings, AIManager, AIReasoningCapability
from ai.ai_model import AIReasoningEffort
from ai.ollama.ollama_backend import OllamaBackend
from ai_tool import AIToolManager
from mindspace.mindspace_settings import MindspaceSettings

from desktop.ai_backend_display import get_all_backend_display_names, get_backend_display_name
from desktop.color_picker_dialog import ThemeColorPickerDialog
from desktop.language.language_code import LanguageCode
from desktop.color_role import ColorRole
from desktop.fetch_error import fetch_error_message as _fetch_error_message
from desktop.fetch_error import pull_error_message as _pull_error_message
from desktop.language.language_manager import LanguageManager
from desktop.settings.settings_accordion import SettingsAccordion
from desktop.settings.settings_action_row import SettingsActionRow
from desktop.settings.settings_container import SettingsContainer
from desktop.settings.settings_combo import SettingsCombo
from desktop.settings.settings_double_spinbox import SettingsDoubleSpinBox
from desktop.settings.settings_factory import SettingsFactory
from desktop.settings.settings_page_heading import SettingsPageHeading
from desktop.settings.settings_section import SettingsSection
from desktop.settings.settings_spinbox import SettingsSpinBox
from desktop.settings.settings_switch import SettingsSwitch
from desktop.settings.settings_text_area import SettingsTextArea
from desktop.settings.settings_text_field import SettingsTextField
from desktop.style_manager import StyleManager
from desktop.color_theme import ColorTheme
from desktop.user.user_file_sort_order import UserFileSortOrder
from desktop.user.user_settings import UserSettings

_FETCHED_MODELS_CACHE = os.path.join(os.path.expanduser("~"), ".humbug", "fetched-models.json")


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


class _NavItemDelegate(QStyledItemDelegate):
    """Item delegate for the settings nav list that controls row height cross-platform."""

    def __init__(self, style_manager: StyleManager, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = style_manager

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        row_height = fm.height() + round(12 * zoom)
        return QSize(super().sizeHint(option, index).width(), row_height)


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
        self.setWindowModality(Qt.WindowModality.WindowModal)

        self._logger = logging.getLogger(__name__)
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._style_manager = StyleManager()
        self._ai_manager = AIManager()
        self._tool_manager = AIToolManager()

        self._initial_user_settings: UserSettings | None = None
        self._current_user_settings: UserSettings | None = None
        self._initial_mindspace_settings: MindspaceSettings | None = None
        self._current_mindspace_settings: MindspaceSettings | None = None
        self._has_mindspace = False

        self._ai_backend_controls: Dict[str, Dict[str, QWidget | None]] = {}
        self._tool_switches: Dict[str, QWidget] = {}
        self._pending_custom_colors: Dict[str, Dict[str, str]] = {}
        self._pending_saved_color_themes: Dict[str, Dict[str, Dict[str, str]]] = {}
        self._pending_active_custom_theme_name: str | None = None
        self._fetched_models_cache_path = _FETCHED_MODELS_CACHE

        # Map section id -> (list item, stack page widget)
        self._section_items: Dict[str, QListWidgetItem] = {}
        self._section_pages: Dict[str, QWidget] = {}

        self._display_heading: SettingsPageHeading
        self._language_combo: SettingsCombo
        self._font_size_spin: SettingsDoubleSpinBox
        self._font_ligatures_check: SettingsSwitch
        self._theme_combo: SettingsCombo
        self._customize_colors_row: SettingsActionRow
        self._file_sort_combo: SettingsCombo
        self._check_for_updates_check: SettingsSwitch
        self._display_container: SettingsContainer

        self._file_access_heading: SettingsPageHeading
        self._allow_external_access_switch: SettingsSwitch
        self._external_allowlist_area: SettingsTextArea
        self._external_denylist_area: SettingsTextArea
        self._file_access_container: SettingsContainer

        self._ai_backends_heading: SettingsPageHeading
        self._ai_backends_container: SettingsContainer

        self._ai_model_heading: SettingsPageHeading
        self._model_filter_combo: SettingsCombo
        self._model_combo: SettingsCombo
        self._temp_spin: SettingsDoubleSpinBox
        self._reasoning_combo: SettingsCombo
        self._effort_combo: SettingsCombo
        self._ai_model_container: SettingsContainer

        self._tools_heading: SettingsPageHeading
        self._tools_container: SettingsContainer

        self._editor_heading: SettingsPageHeading
        self._editor_tabs_section: SettingsSection
        self._soft_tabs_check: SettingsSwitch
        self._tab_size_spin: SettingsSpinBox
        self._auto_backup_check: SettingsSwitch
        self._backup_interval_spin: SettingsSpinBox
        self._editor_container: SettingsContainer

        self._terminal_heading: SettingsPageHeading
        self._terminal_fixed_width_check: SettingsSwitch
        self._terminal_fixed_width_spin: SettingsSpinBox
        self._terminal_scrollback_check: SettingsSwitch
        self._terminal_scrollback_spin: SettingsSpinBox
        self._terminal_close_on_exit_check: SettingsSwitch
        self._terminal_container: SettingsContainer

        strings = self._language_manager.strings()
        self.setWindowTitle(strings.settings)
        self.setMinimumWidth(900)
        self.setMinimumHeight(700)
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
        self._nav_list.setItemDelegate(_NavItemDelegate(self._style_manager, self._nav_list))

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

        self._font_ligatures_check = SettingsFactory.create_switch(strings.font_ligatures)
        container.add_setting(self._font_ligatures_check)

        self._theme_combo = SettingsFactory.create_combo(strings.display_theme)
        container.add_setting(self._theme_combo)
        self._theme_combo.set_items([
            (strings.theme_system, ColorTheme.SYSTEM),
            (strings.theme_light, ColorTheme.LIGHT),
            (strings.theme_dark, ColorTheme.DARK),
            (strings.theme_color_blind, ColorTheme.COLOR_BLIND),
            (strings.theme_ocean_light, ColorTheme.OCEAN_LIGHT),
            (strings.theme_custom, ColorTheme.CUSTOM),
        ])

        self._customize_colors_row = SettingsActionRow(strings.customize_colors)
        self._customize_colors_row.button().clicked.connect(self._on_customize_colors)
        container.add_setting(self._customize_colors_row)

        self._file_sort_combo = SettingsFactory.create_combo(strings.file_sort_order)
        container.add_setting(self._file_sort_combo)
        self._file_sort_combo.set_items([
            (strings.sort_directories_first, UserFileSortOrder.DIRECTORIES_FIRST),
            (strings.sort_alphabetical, UserFileSortOrder.ALPHABETICAL),
        ])

        self._check_for_updates_check = SettingsFactory.create_switch(strings.check_for_updates_setting)
        container.add_setting(self._check_for_updates_check)

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

        self._allow_external_access_switch = SettingsFactory.create_switch(
            strings.allow_external_file_access
        )
        container.add_setting(self._allow_external_access_switch)

        self._external_allowlist_area = SettingsFactory.create_text_area(strings.external_file_allowlist)
        container.add_setting(self._external_allowlist_area)

        self._external_denylist_area = SettingsFactory.create_text_area(strings.external_file_denylist)
        container.add_setting(self._external_denylist_area)

        self._allow_external_access_switch.value_changed.connect(self._handle_external_access_enabled)

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

        ai_backend_mapping = list(get_all_backend_display_names(strings).items())

        for backend_id, backend_name in ai_backend_mapping:
            accordion = SettingsAccordion(backend_name, expanded=False)
            container.add_setting(accordion)

            enable_switch = SettingsFactory.create_switch(strings.enable_backend)
            accordion.add_content(enable_switch)

            api_key_field = SettingsFactory.create_text_field(strings.api_key)
            accordion.add_content(api_key_field)

            default_url = self._ai_manager.get_default_url(backend_id)
            url_label = strings.api_url
            url_placeholder = default_url
            url_field = SettingsFactory.create_text_field(url_label, placeholder=url_placeholder)
            accordion.add_content(url_field)

            is_ollama = backend_id in ("ollama", "ollama-cloud")

            fetch_label = strings.ollama_update_local_models if is_ollama else "Update Models"
            fetch_row = SettingsActionRow(fetch_label)
            accordion.add_content(fetch_row)
            fetch_row.button().setEnabled(False)

            manage_row = SettingsActionRow("Remove Fetched Models…")
            accordion.add_content(manage_row)
            manage_row.button().setEnabled(False)

            if is_ollama:
                pull_name_field = SettingsFactory.create_text_field(
                    strings.ollama_pull_label,
                    placeholder=strings.ollama_pull_placeholder,
                )
                accordion.add_content(pull_name_field)

                pull_row = SettingsActionRow(strings.ollama_pull_button)
                accordion.add_content(pull_row)
                pull_row.button().setEnabled(False)

            else:
                pull_name_field = None
                pull_row = None

            self._ai_backend_controls[backend_id] = {
                "enable": enable_switch,
                "key": api_key_field,
                "url": url_field,
                "title": accordion,
                "fetch_row": fetch_row,
                "manage_row": manage_row,
                "pull_name": pull_name_field,
                "pull_row": pull_row,
            }

            enable_switch.value_changed.connect(
                lambda _checked=None, bid=backend_id: self._handle_backend_enabled(bid)
            )
            api_key_field.value_changed.connect(
                lambda _v=None, bid=backend_id: self._update_fetch_button_state(bid)
            )
            fetch_row.button().clicked.connect(
                lambda _checked=False, bid=backend_id: self._on_fetch_models_clicked(bid)
            )
            manage_row.button().clicked.connect(
                lambda _checked=False, bid=backend_id: self._on_manage_models_clicked(bid)
            )
            if is_ollama and pull_name_field and pull_row:
                pull_name_field.value_changed.connect(
                    lambda _v=None, bid=backend_id: self._update_pull_button_state(bid)
                )
                pull_row.button().clicked.connect(
                    lambda _checked=False, bid=backend_id: self._on_pull_model_clicked(bid)
                )

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

        self._model_filter_combo = SettingsFactory.create_combo("Provider")
        container.add_setting(self._model_filter_combo)

        self._model_combo = SettingsFactory.create_combo(strings.settings_model_label)
        self._model_combo.set_searchable(True)
        container.add_setting(self._model_combo)

        self._reasoning_combo = SettingsFactory.create_combo(strings.settings_reasoning_label)
        container.add_setting(self._reasoning_combo)

        self._effort_combo = SettingsFactory.create_combo(strings.settings_reasoning_effort_label)
        container.add_setting(self._effort_combo)

        self._temp_spin = SettingsFactory.create_double_spinbox(
            strings.settings_temp_label, 0.0, 1.0, 0.1, 1
        )
        container.add_setting(self._temp_spin)

        self._model_filter_combo.value_changed.connect(self._on_model_filter_changed)
        self._model_combo.value_changed.connect(self._on_model_value_changed)
        self._effort_combo.value_changed.connect(self._on_effort_value_changed)

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
            switch = SettingsFactory.create_switch(config.display_name)
            self._tool_switches[config.name] = switch
            container.add_setting(switch)

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

        self._soft_tabs_check = SettingsFactory.create_switch(strings.use_soft_tabs)
        container.add_setting(self._soft_tabs_check)

        self._tab_size_spin = SettingsFactory.create_spinbox(strings.tab_size, 1, 8, 1)
        container.add_setting(self._tab_size_spin)

        spacer = SettingsFactory.create_spacer(24)
        container.add_setting(spacer)

        backup_section = SettingsFactory.create_section(strings.backup_settings)
        container.add_setting(backup_section)

        self._auto_backup_check = SettingsFactory.create_switch(strings.auto_backup)
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

        self._terminal_fixed_width_check = SettingsFactory.create_switch(
            strings.terminal_fixed_width_enabled
        )
        container.add_setting(self._terminal_fixed_width_check)

        self._terminal_fixed_width_spin = SettingsFactory.create_spinbox(
            strings.terminal_fixed_width, 40, 200, 2
        )
        container.add_setting(self._terminal_fixed_width_spin)

        self._terminal_scrollback_check = SettingsFactory.create_switch(
            strings.terminal_scrollback_enabled
        )
        container.add_setting(self._terminal_scrollback_check)

        self._terminal_scrollback_spin = SettingsFactory.create_spinbox(
            strings.terminal_scrollback_lines, 500, 10000, 100
        )
        container.add_setting(self._terminal_scrollback_spin)

        self._terminal_close_on_exit_check = SettingsFactory.create_switch(
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
            enabled = cast(SettingsSwitch, controls["enable"]).get_value()
            api_key = cast(SettingsTextField, controls["key"]).get_value()
            url = cast(SettingsTextField, controls["url"]).get_value()
            ai_backends[backend_id] = AIBackendSettings(
                enabled=enabled, api_key=api_key, url=url
            )

        return UserSettings(
            ai_backends=ai_backends,
            language=self._language_combo.get_value(),
            font_size=self._font_size_spin.get_value(),
            font_ligatures=self._font_ligatures_check.get_value(),
            theme=self._theme_combo.get_value(),
            custom_colors=dict(self._pending_custom_colors),
            saved_color_themes={k: dict(v) for k, v in self._pending_saved_color_themes.items()},
            active_custom_theme_name=self._pending_active_custom_theme_name,
            file_sort_order=self._file_sort_combo.get_value(),
            allow_external_file_access=self._allow_external_access_switch.get_value(),
            external_file_allowlist=self._external_allowlist_area.get_value(),
            external_file_denylist=self._external_denylist_area.get_value(),
            check_for_updates=self._check_for_updates_check.get_value(),
        )

    def get_mindspace_settings(self) -> MindspaceSettings | None:
        """Read current mindspace settings from the dialog controls, or None if no mindspace."""
        if not self._has_mindspace:
            return None

        enabled_tools = {
            name: cast(SettingsSwitch, cb).get_value()
            for name, cb in self._tool_switches.items()
        }

        current_model = str(self._model_combo.get_value() or "")
        key = self._model_combo.get_value()
        current_model, current_provider = key if isinstance(key, tuple) else ("", "")
        reasoning_options = AIConversationSettings.get_supported_reasoning_efforts(current_model, current_provider)
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
            model=current_model,
            provider=current_provider,
            temperature=self._temp_spin.get_value(),
            reasoning=self._reasoning_combo.get_value(),
            reasoning_effort=self._effort_combo.get_value() if reasoning_options else None,
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
        self._font_ligatures_check.set_value(settings.font_ligatures)
        self._file_sort_combo.set_value(settings.file_sort_order)
        self._check_for_updates_check.set_value(settings.check_for_updates)
        self._pending_custom_colors = dict(settings.custom_colors)
        self._pending_saved_color_themes = {k: dict(v) for k, v in settings.saved_color_themes.items()}
        self._pending_active_custom_theme_name = settings.active_custom_theme_name

        # File access
        self._allow_external_access_switch.set_value(settings.allow_external_file_access)
        self._external_allowlist_area.set_value(settings.external_file_allowlist)
        self._external_denylist_area.set_value(settings.external_file_denylist)
        self._external_allowlist_area.set_enabled(settings.allow_external_file_access)

        # AI backends
        for backend_id, controls in self._ai_backend_controls.items():
            backend = settings.ai_backends.get(backend_id, AIBackendSettings())
            cast(SettingsSwitch, controls["enable"]).set_value(backend.enabled)
            cast(SettingsTextField, controls["key"]).set_value(backend.api_key)
            cast(SettingsTextField, controls["url"]).set_value(backend.url)
            cast(SettingsTextField, controls["key"]).set_enabled(backend.enabled)
            cast(SettingsTextField, controls["url"]).set_enabled(
                backend.enabled or backend_id in ("ollama", "ollama-cloud")
            )
            self._update_fetch_button_state(backend_id)

    def _populate_mindspace_settings(self, settings: MindspaceSettings | None) -> None:
        """Load mindspace settings into the dialog controls."""
        if settings is None:
            return

        # AI model
        ai_backends = self._ai_manager.get_backends()
        self._populate_model_filter_combo(ai_backends)
        self._populate_model_combo(ai_backends, filter_provider=None)
        self._model_combo.set_value((settings.model, settings.provider))
        self._temp_spin.set_value(settings.temperature)
        self._update_model_capabilities(settings.model, settings.provider)
        if settings.reasoning_effort is not None:
            self._effort_combo.set_value(settings.reasoning_effort)

        self._reasoning_combo.set_value(settings.reasoning)

        # Tools
        for tool_name, switch in self._tool_switches.items():
            enabled = settings.enabled_tools.get(tool_name, True)
            cast(SettingsSwitch, switch).set_value(enabled)

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
        enabled = self._allow_external_access_switch.get_value()
        self._external_allowlist_area.set_enabled(enabled)

    def _handle_backend_enabled(self, backend_id: str) -> None:
        """Enable or disable key/URL fields for a backend."""
        controls = self._ai_backend_controls[backend_id]
        enabled = cast(SettingsSwitch, controls["enable"]).get_value()
        cast(SettingsTextField, controls["key"]).set_enabled(enabled)
        # Ollama URL is always editable so the user can set the host before enabling.
        url_always_on = backend_id in ("ollama", "ollama-cloud")
        cast(SettingsTextField, controls["url"]).set_enabled(enabled or url_always_on)
        self._update_fetch_button_state(backend_id)
        self._update_pull_button_state(backend_id)

    def _on_model_value_changed(self) -> None:
        """Update capability controls when the model selection changes."""
        key = self._model_combo.get_value()
        model, provider = key if isinstance(key, tuple) else ("", "")
        self._update_model_capabilities(model, provider)

    def _on_effort_value_changed(self) -> None:
        """Update temperature enable state when reasoning effort changes."""
        key = self._model_combo.get_value()
        model, provider = key if isinstance(key, tuple) else ("", "")
        effort = self._effort_combo.get_value() if AIConversationSettings.get_supported_reasoning_efforts(model, provider) else None
        self._temp_spin.set_enabled(AIConversationSettings.supports_temperature(model, provider, effort))

    def _update_model_capabilities(self, model: str, provider: str) -> None:
        """Refresh reasoning combo and temperature enable state for a model."""
        strings = self._language_manager.strings()
        capabilities = AIConversationSettings.get_reasoning_capability(model, provider)

        items = []
        if capabilities & AIReasoningCapability.NO_REASONING:
            items.append((strings.settings_no_reasoning, AIReasoningCapability.NO_REASONING))

        if capabilities & AIReasoningCapability.HIDDEN_REASONING:
            items.append((strings.settings_hidden_reasoning, AIReasoningCapability.HIDDEN_REASONING))

        if capabilities & AIReasoningCapability.VISIBLE_REASONING:
            items.append((strings.settings_visible_reasoning, AIReasoningCapability.VISIBLE_REASONING))

        self._reasoning_combo.set_items(items)
        self._reasoning_combo.setEnabled(len(items) > 1)

        effort_labels = {
            AIReasoningEffort.NONE: strings.settings_effort_none,
            AIReasoningEffort.MINIMAL: strings.settings_effort_minimal,
            AIReasoningEffort.LOW: strings.settings_effort_low,
            AIReasoningEffort.MEDIUM: strings.settings_effort_medium,
            AIReasoningEffort.HIGH: strings.settings_effort_high,
            AIReasoningEffort.XHIGH: strings.settings_effort_xhigh,
            AIReasoningEffort.MAX: strings.settings_effort_max,
        }
        efforts = AIConversationSettings.get_supported_reasoning_efforts(model, provider)
        if efforts:
            effort_items = [(effort_labels.get(e, e), e) for e in efforts]
            self._effort_combo.set_items(effort_items)
            self._effort_combo.setEnabled(len(effort_items) > 1)
            self._effort_combo.setVisible(True)

        else:
            self._effort_combo.set_items([])
            self._effort_combo.setEnabled(False)
            self._effort_combo.setVisible(False)

        effort = self._effort_combo.get_value() if AIConversationSettings.get_supported_reasoning_efforts(model, provider) else None
        self._temp_spin.set_enabled(AIConversationSettings.supports_temperature(model, provider, effort))

    def _get_provider_display_names(self) -> Dict[str, str]:
        """Return a mapping from provider ID to a human-readable display name."""
        return get_all_backend_display_names(self._language_manager.strings())

    def _populate_model_filter_combo(self, ai_backends: Dict) -> None:
        """Populate the provider filter combo with all providers that have models."""
        provider_names = self._get_provider_display_names()
        providers_with_models = set(
            provider
            for (_, provider) in AIConversationSettings.iter_models_by_backends(ai_backends)
        )
        items: List[tuple] = [("All Providers", None)]
        for provider_id, display in provider_names.items():
            if provider_id in providers_with_models:
                items.append((display, provider_id))
        self._model_filter_combo.set_items(items)

    def _populate_model_combo(self, ai_backends: Dict, filter_provider: str | None) -> None:
        """Populate the model combo grouped by provider, optionally filtered."""
        provider_names = self._get_provider_display_names()
        grouped: Dict[str, List[tuple]] = {}
        for (model_name, provider) in AIConversationSettings.iter_models_by_backends(ai_backends):
            if filter_provider and provider != filter_provider:
                continue

            display = AIConversationSettings.get_display_name(model_name, provider)
            grouped.setdefault(provider, []).append((display, (model_name, provider)))

        if filter_provider:
            items = [item for entries in grouped.values() for item in entries]
            self._model_combo.set_items(items)

        else:
            groups = [
                (provider_names.get(provider, provider), entries)
                for provider, entries in grouped.items()
            ]
            self._model_combo.set_grouped_items(groups)

    def _refresh_model_combo(self) -> None:
        """Re-populate the model combo after new models have been registered."""
        ai_backends = self._ai_manager.get_backends()
        filter_provider = self._model_filter_combo.get_value()
        self._populate_model_filter_combo(ai_backends)
        self._populate_model_combo(ai_backends, filter_provider)

    def _on_model_filter_changed(self) -> None:
        """Repopulate model list when the provider filter changes."""
        ai_backends = self._ai_manager.get_backends()
        filter_provider = self._model_filter_combo.get_value()
        self._populate_model_combo(ai_backends, filter_provider)

    def _update_fetch_button_state(self, backend_id: str) -> None:
        """Enable "Update Models" only when backend is on and (for non-Ollama) has a key."""
        controls = self._ai_backend_controls[backend_id]
        enabled = cast(SettingsSwitch, controls["enable"]).get_value()
        api_key = cast(SettingsTextField, controls["key"]).get_value().strip()
        needs_key = backend_id not in ("ollama", "vllm")
        can_fetch = enabled and (api_key or not needs_key)
        cast(SettingsActionRow, controls["fetch_row"]).button().setEnabled(bool(can_fetch))
        self._update_manage_button_state(backend_id)

    def _update_manage_button_state(self, backend_id: str) -> None:
        """Enable "Remove Fetched Models" only when there are fetched models for this provider."""
        controls = self._ai_backend_controls[backend_id]
        has_fetched = bool(AIConversationSettings.get_fetched_models_by_provider(backend_id))
        cast(SettingsActionRow, controls["manage_row"]).button().setEnabled(has_fetched)

    def _update_pull_button_state(self, backend_id: str) -> None:
        """Enable Pull only when Ollama is on and a model name is entered."""
        controls = self._ai_backend_controls[backend_id]
        pull_row = controls.get("pull_row")
        if pull_row is None:
            return
        enabled = cast(SettingsSwitch, controls["enable"]).get_value()
        name = cast(SettingsTextField, controls["pull_name"]).get_value().strip()
        cast(SettingsActionRow, pull_row).button().setEnabled(bool(enabled and name))

    def _on_pull_model_clicked(self, backend_id: str) -> None:
        """Pull a model from the Ollama registry by name."""
        controls = self._ai_backend_controls[backend_id]
        pull_row = cast(SettingsActionRow, controls["pull_row"])
        pull_name_field = cast(SettingsTextField, controls["pull_name"])
        model_name = pull_name_field.get_value().strip()
        if not model_name:
            return

        strings = self._language_manager.strings()
        pull_row.button().setEnabled(False)
        pull_row.set_status(strings.ollama_pull_pulling.format(model_name))

        url = cast(SettingsTextField, controls["url"]).get_value().strip()
        backend_class = self._ai_manager.get_backend_class(backend_id)
        if backend_class is None:
            return
        api_url = url or self._ai_manager.get_default_url(backend_id)
        backend = backend_class(api_key="", api_url=api_url)

        async def _do_pull() -> None:
            try:
                def on_progress(status: str) -> None:
                    pull_row.set_status(status)

                await cast(OllamaBackend, backend).pull_model(model_name, on_progress)
                AIConversationSettings.register_fetched_models([model_name], backend_id)
                AIConversationSettings.save_fetched_models_cache(
                    self._fetched_models_cache_path
                )
                self._refresh_model_combo()
                self._update_manage_button_state(backend_id)
                pull_row.set_success(strings.ollama_pull_success.format(model_name))
            except Exception as exc:  # pylint: disable=broad-except
                pull_row.set_error(_pull_error_message(exc))
                self._logger.warning("pull_model failed for %s: %s", model_name, exc)
            finally:
                self._update_pull_button_state(backend_id)

        asyncio.get_event_loop().create_task(_do_pull())

    def _on_fetch_models_clicked(self, backend_id: str) -> None:
        """Kick off an async model-list fetch for the given backend."""
        controls = self._ai_backend_controls[backend_id]
        fetch_row = cast(SettingsActionRow, controls["fetch_row"])
        fetch_row.button().setEnabled(False)
        fetch_row.set_status("Fetching…")  # clears any previous error colour

        api_key = cast(SettingsTextField, controls["key"]).get_value().strip()
        url = cast(SettingsTextField, controls["url"]).get_value().strip()

        backend_class = self._ai_manager.get_backend_class(backend_id)
        if backend_class is None:
            fetch_row.set_status("Unknown provider.")
            return
        api_url = url or self._ai_manager.get_default_url(backend_id)
        backend = backend_class(api_key=api_key, api_url=api_url)

        async def _do_fetch() -> None:
            try:
                model_ids = await backend.fetch_models()

            except Exception as exc:  # pylint: disable=broad-except
                fetch_row.set_error(_fetch_error_message(exc, backend_id))
                self._logger.warning("fetch_models failed for %s: %s", backend_id, exc)

            else:
                if backend_id in ("ollama", "ollama-cloud"):
                    self._register_ollama_models(model_ids, fetch_row, backend_id)

                else:
                    newly_added, _ = AIConversationSettings.register_fetched_models(
                        model_ids, backend_id
                    )
                    AIConversationSettings.save_fetched_models_cache(
                        self._fetched_models_cache_path
                    )
                    if newly_added:
                        self._refresh_model_combo()
                        fetch_row.set_success(f"Added {len(newly_added)} new model(s).")

                    else:
                        fetch_row.set_status("List already up to date.")

            finally:
                self._update_fetch_button_state(backend_id)

        asyncio.get_event_loop().create_task(_do_fetch())

    def _on_manage_models_clicked(self, backend_id: str) -> None:
        """Open the fetched-model manager dialog for a provider."""
        fetched = AIConversationSettings.get_fetched_models_by_provider(backend_id)
        title = self._get_provider_display_names().get(backend_id, backend_id)
        # Pass (model, provider) tuples; dialog shows model names and removes by key
        dlg = _FetchedModelManagerDialog(fetched, backend_id, title, self)
        dlg.exec()

        # After dialog closes, persist and refresh
        AIConversationSettings.save_fetched_models_cache(self._fetched_models_cache_path)
        self._update_manage_button_state(backend_id)
        self._refresh_model_combo()

    def _register_ollama_models(
        self, model_ids: List[str], fetch_row: SettingsActionRow, backend_id: str
    ) -> None:
        """Register installed Ollama models returned by /api/tags."""
        if not model_ids:
            fetch_row.set_status("No installed models found.")
            return

        newly_added, _ = AIConversationSettings.register_fetched_models(
            model_ids, backend_id
        )
        AIConversationSettings.save_fetched_models_cache(
            self._fetched_models_cache_path
        )
        self._refresh_model_combo()
        self._update_manage_button_state(backend_id)
        if newly_added:
            fetch_row.set_success(f"Added {len(newly_added)} model(s).")
        else:
            fetch_row.set_status("Local model list already up to date.")

    def _on_auto_backup_changed(self) -> None:
        """Enable or disable backup interval spin based on auto backup switch."""
        self._backup_interval_spin.set_enabled(self._auto_backup_check.get_value())

    def _on_terminal_fixed_width_changed(self) -> None:
        """Enable or disable fixed width spin based on switch."""
        self._terminal_fixed_width_spin.set_enabled(self._terminal_fixed_width_check.get_value())

    def _on_terminal_scrollback_changed(self) -> None:
        """Enable or disable scrollback lines spin based on switch."""
        self._terminal_scrollback_spin.set_enabled(self._terminal_scrollback_check.get_value())

    def _on_customize_colors(self) -> None:
        """Open the color picker dialog and apply returned theme settings."""
        current_mode: ColorTheme = self._theme_combo.get_value()
        dialog = ThemeColorPickerDialog(initial_mode=current_mode, parent=self)
        dialog.theme_settings_changed.connect(self._on_color_picker_applied)
        dialog.saved_color_themes_changed.connect(self._on_saved_color_themes_changed)
        dialog.exec()

    def _on_color_picker_applied(
        self,
        mode: ColorTheme,
        custom_colors: Dict[str, Dict[str, str]],
        active_custom_theme_name: str | None,
    ) -> None:
        """Receive theme mode + custom colors + active saved-theme name from the color picker dialog."""
        self._theme_combo.set_value(mode)
        # Only the live ("Manually") custom set is stored in custom_colors. When a saved theme is
        # selected unchanged, leave the manual set intact so it stays distinct from the saved theme.
        if active_custom_theme_name is None:
            self._pending_custom_colors = custom_colors

        self._pending_active_custom_theme_name = active_custom_theme_name
        self.apply_button.setEnabled(True)

    def _on_saved_color_themes_changed(
        self,
        saved_themes: Dict[str, Dict[str, Dict[str, str]]],
        theme: ColorTheme,
        active_custom_theme_name: str | None,
    ) -> None:
        """
        Receive the updated set of saved (named) custom themes from the color picker dialog.

        Saving or deleting a theme persists immediately so saved themes show up in the View
        menu right away, and deleting the active theme reverts the app to the default theme.
        """
        self._pending_saved_color_themes = saved_themes
        self._pending_active_custom_theme_name = active_custom_theme_name
        self._theme_combo.set_value(theme)

        user_settings = self.get_user_settings()
        self._current_user_settings = user_settings
        self.user_settings_changed.emit(user_settings)
        self.apply_button.setEnabled(False)

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

        self._font_ligatures_check.set_label(strings.font_ligatures)

        current_theme = self._theme_combo.get_value()
        self._theme_combo.set_label(strings.display_theme)
        self._theme_combo.set_items([
            (strings.theme_system, ColorTheme.SYSTEM),
            (strings.theme_light, ColorTheme.LIGHT),
            (strings.theme_dark, ColorTheme.DARK),
            (strings.theme_color_blind, ColorTheme.COLOR_BLIND),
            (strings.theme_ocean_light, ColorTheme.OCEAN_LIGHT),
            (strings.theme_custom, ColorTheme.CUSTOM),
        ])
        self._theme_combo.set_value(current_theme)
        self._customize_colors_row.set_button_text(strings.customize_colors)

        current_sort = self._file_sort_combo.get_value()
        self._file_sort_combo.set_label(strings.file_sort_order)
        self._file_sort_combo.set_items([
            (strings.sort_directories_first, UserFileSortOrder.DIRECTORIES_FIRST),
            (strings.sort_alphabetical, UserFileSortOrder.ALPHABETICAL),
        ])
        self._file_sort_combo.set_value(current_sort)

        self._check_for_updates_check.set_label(strings.check_for_updates_setting)

        # Update File Access page controls
        self._allow_external_access_switch.set_label(strings.allow_external_file_access)
        self._external_allowlist_area.set_label(strings.external_file_allowlist)
        self._external_denylist_area.set_label(strings.external_file_denylist)

        # Update AI Backends page controls
        for backend_id, controls in self._ai_backend_controls.items():
            cast(SettingsAccordion, controls["title"]).set_label(get_backend_display_name(backend_id, strings))
            cast(SettingsSwitch, controls["enable"]).set_label(strings.enable_backend)
            cast(SettingsTextField, controls["key"]).set_label(strings.api_key)
            cast(SettingsTextField, controls["url"]).set_label(strings.api_url)

        # Update AI Model page controls
        self._model_filter_combo.set_label("Provider")
        self._model_combo.set_label(strings.settings_model_label)
        self._temp_spin.set_label(strings.settings_temp_label)
        key = self._model_combo.get_value()
        model, provider = key if isinstance(key, tuple) else ("", "")
        self._update_model_capabilities(model, provider)
        self._reasoning_combo.set_label(strings.settings_reasoning_label)
        self._effort_combo.set_label(strings.settings_reasoning_effort_label)

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
            font_ligatures=settings.font_ligatures,
            theme=settings.theme,
            custom_colors=dict(settings.custom_colors),
            saved_color_themes={k: dict(v) for k, v in settings.saved_color_themes.items()},
            active_custom_theme_name=settings.active_custom_theme_name,
            file_sort_order=settings.file_sort_order,
            allow_external_file_access=settings.allow_external_file_access,
            external_file_allowlist=list(settings.external_file_allowlist),
            external_file_denylist=list(settings.external_file_denylist),
            check_for_updates=settings.check_for_updates,
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
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning=settings.reasoning,
            enabled_tools=settings.enabled_tools.copy(),
        )


class _FetchedModelManagerDialog(QDialog):
    """Dialog for viewing and permanently removing fetched (non-built-in) models."""

    def __init__(
        self, model_keys: List[tuple], backend_id: str, provider_label: str, parent: QWidget | None = None
    ) -> None:
        super().__init__(parent)
        self.setWindowTitle(f"Fetched Models — {provider_label}")
        self.setModal(True)
        self.setMinimumWidth(460)

        outer = QVBoxLayout()
        outer.setSpacing(0)
        outer.setContentsMargins(0, 0, 0, 0)

        # Scrollable model list
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setMinimumHeight(120)
        scroll.setMaximumHeight(400)

        list_widget = QWidget()
        self._list_layout = QVBoxLayout(list_widget)
        self._list_layout.setSpacing(2)
        self._list_layout.setContentsMargins(16, 12, 16, 12)

        self._empty_label = QLabel("No fetched models for this provider.")
        self._list_layout.addWidget(self._empty_label)
        self._list_layout.addStretch()

        self._rows: dict = {}  # model_name -> row_widget
        self._backend_id = backend_id
        for (model_name, _provider) in sorted(model_keys):
            self._add_model_row(model_name)

        self._update_empty_label()
        scroll.setWidget(list_widget)
        outer.addWidget(scroll)

        sep = QFrame()
        sep.setFrameShape(QFrame.Shape.HLine)
        sep.setObjectName("SettingsSeparator")
        outer.addWidget(sep)

        btn_layout = QHBoxLayout()
        btn_layout.setContentsMargins(16, 10, 16, 10)
        btn_layout.addStretch()
        close_btn = QPushButton("Close")
        close_btn.setMinimumWidth(90)
        close_btn.setMinimumHeight(36)
        close_btn.setProperty("recommended", True)
        close_btn.clicked.connect(self.accept)
        btn_layout.addWidget(close_btn)
        btn_layout.addStretch()
        outer.addLayout(btn_layout)

        self.setLayout(outer)

        style_manager = StyleManager()
        self.setStyleSheet(style_manager.get_dialog_stylesheet())

    def _add_model_row(self, model_id: str) -> None:
        row = QWidget()
        row_layout = QHBoxLayout(row)
        row_layout.setContentsMargins(0, 2, 0, 2)
        row_layout.setSpacing(8)

        label = QLabel(model_id)
        label.setSizePolicy(label.sizePolicy().horizontalPolicy(), label.sizePolicy().verticalPolicy())
        row_layout.addWidget(label, 1)

        remove_btn = QPushButton("Remove")
        remove_btn.setFixedWidth(80)
        remove_btn.setMinimumHeight(28)
        remove_btn.clicked.connect(lambda _checked=False, mid=model_id: self._remove_model(mid, self._backend_id))
        row_layout.addWidget(remove_btn)

        # Insert before the stretch (last item)
        insert_at = self._list_layout.count() - 1
        self._list_layout.insertWidget(insert_at, row)
        self._rows[model_id] = row

    def _remove_model(self, model_id: str, provider: str) -> None:
        AIConversationSettings.remove_fetched_model(model_id, provider)
        row = self._rows.pop(model_id, None)
        if row:
            self._list_layout.removeWidget(row)
            row.deleteLater()
        self._update_empty_label()

    def _update_empty_label(self) -> None:
        self._empty_label.setVisible(not self._rows)
