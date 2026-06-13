"""Main window implementation for Humbug application."""

import asyncio
from datetime import datetime, timezone
import json
import logging
import os
import sys
from pathlib import Path
from typing import cast, Dict, Tuple

from PySide6.QtWidgets import (
    QMainWindow, QWidget, QVBoxLayout, QMenuBar, QFileDialog,
    QLabel, QApplication, QDialog, QMenu, QStatusBar
)
from PySide6.QtCore import Qt, QTimer, QEvent, QPoint
from PySide6.QtGui import QKeyEvent, QMouseEvent, QAction, QKeySequence, QActionGroup

from ai.ai_conversation_settings import AIConversationSettings
from ai_tool import AIToolManager
from ai_transcript_conversation import AITranscriptConversation
from clock_ai_tool.clock_ai_tool import ClockAITool
from context.context_info import ContextInfo
from context.context_registry import ContextRegistry
from conversation_ai_tool.conversation_ai_tool import ConversationAITool
from delegate_ai_tool import DelegateAITool
from document_converter_ai_tool.document_converter_ai_tool import DocumentConverterAITool
from editor_ai_tool.editor_ai_tool import EditorAITool
from filesystem_ai_tool.filesystem_ai_tool import FileSystemAITool
from filesystem_ai_tool.filesystem_access_settings import FilesystemAccessSettings
from help_ai_tool.help_ai_tool import HelpAITool
from menai_ai_tool.menai_ai_tool import MenaiAITool
from mindspace.mindspace_error import MindspaceError, MindspaceExistsError
from mindspace.mindspace_log_level import MindspaceLogLevel
from mindspace.mindspace_settings import MindspaceSettings
from preview_ai_tool.preview_ai_tool import PreviewAITool
from terminal_ai_tool.terminal_ai_tool import TerminalAITool

from desktop.about_dialog import AboutDialog
from desktop.color_role import ColorRole
from desktop.conversation_sidebar.conversation_sidebar import ConversationSidebar
from desktop.conversation_tab.conversation_tab import ConversationTab
from desktop.diff_tab.diff_tab import DiffTab
from desktop.editor_tab.editor_tab import EditorTab
from desktop.exception_notifier import get_exception_notifier
from desktop.file_sidebar.file_sidebar import FileSidebar
from desktop.language.language_manager import LanguageManager
from desktop.log_tab.log_tab import LogTab
from desktop.message_box import MessageBox, MessageBoxType
from desktop.mindspace.mindspace_folders_dialog import MindspaceFoldersDialog
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.main_window_splitter import MainWindowSplitter
from desktop.preview_sidebar.preview_sidebar import PreviewSidebar
from desktop.preview_tab.preview_tab import PreviewTab
from desktop.search_sidebar.search_sidebar import SearchSidebar
from desktop.settings_dialog import SettingsDialog, SECTION_AI_BACKENDS
from desktop.shell_tab.commands.shell_command_cat import ShellCommandCat
from desktop.shell_tab.commands.shell_command_clear import ShellCommandClear
from desktop.shell_tab.commands.shell_command_conversation import ShellCommandConversation
from desktop.shell_tab.commands.shell_command_diff import ShellCommandDiff
from desktop.shell_tab.commands.shell_command_edit import ShellCommandEdit
from desktop.shell_tab.commands.shell_command_help import ShellCommandHelp
from desktop.shell_tab.commands.shell_command_log import ShellCommandLog
from desktop.shell_tab.commands.shell_command_preview import ShellCommandPreview
from desktop.shell_tab.commands.shell_command_terminal import ShellCommandTerminal
from desktop.shell_tab.shell_command_registry import ShellCommandRegistry
from desktop.shell_tab.shell_tab import ShellTab
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar_manager import SidebarManager
from desktop.style_manager import StyleManager, ColorMode
from desktop.status_message import StatusMessage
from desktop.system_ai_tool import SystemAITool
from desktop.tab_manager import TabManager, TabManagerError
from desktop.terminal_tab.terminal_tab import TerminalTab
from desktop.title_bar import MenuBarDragFilter, WindowControlsWidget
from desktop.update_checker import UpdateChecker
from desktop.update_dialog import UpdateDialog
from desktop.user.user_manager import UserManager, UserError
from desktop.user.user_settings import UserSettings
from desktop.vcs_sidebar.vcs_sidebar import VCSSidebar


def _create_conversation_tab(
    info: ContextInfo, registry: ContextRegistry, parent: QWidget
) -> ConversationTab:
    """Context factory for ConversationTab."""
    transcript = registry.get_model(info.context_id, AITranscriptConversation)
    if transcript is not None:
        tab = ConversationTab(
            info.context_id, transcript.path(), parent,
            ai_transcript_conversation=transcript,
        )

    else:
        tab = ConversationTab(info.context_id, info.path, parent)

    return tab


def _create_editor_tab(
    info: ContextInfo, registry: ContextRegistry, parent: QWidget
) -> EditorTab:
    """Context factory for EditorTab."""
    tab = EditorTab(info.context_id, info.path, None, parent)
    goto = registry.get_model(info.context_id, tuple)
    if goto is not None:
        QTimer.singleShot(0, lambda: tab.goto_line(goto[0], goto[1]))

    return tab


def _create_terminal_tab(
    info: ContextInfo, _registry: ContextRegistry, parent: QWidget
) -> TerminalTab:
    """Context factory for TerminalTab."""
    return TerminalTab(info.context_id, None, parent)


def _create_preview_tab(
    info: ContextInfo, _registry: ContextRegistry, parent: QWidget
) -> PreviewTab:
    """Context factory for PreviewTab."""
    return PreviewTab(info.context_id, info.path, parent)


def _create_diff_tab(
    info: ContextInfo, _registry: ContextRegistry, parent: QWidget
) -> DiffTab:
    """Context factory for DiffTab."""
    return DiffTab(info.context_id, info.path, parent)


def _create_log_tab(
    info: ContextInfo, _registry: ContextRegistry, parent: QWidget
) -> LogTab:
    """Context factory for LogTab."""
    return LogTab(info.context_id, parent)


def _create_shell_tab(
    info: ContextInfo, _registry: ContextRegistry, parent: QWidget
) -> ShellTab:
    """Context factory for ShellTab."""
    return ShellTab(info.context_id, parent)


def _wire_conversation_sidebar(panel: SidebarBase, mgr: SidebarManager) -> None:
    """Wire ConversationSidebar signals to SidebarManager."""
    assert isinstance(panel, ConversationSidebar)
    panel.file_clicked.connect(mgr.file_clicked)
    panel.file_deleted.connect(mgr.file_deleted)
    panel.file_renamed.connect(mgr.file_renamed)
    panel.file_moved.connect(mgr.file_moved)
    panel.file_opened_in_editor.connect(mgr.file_opened_in_editor)
    panel.file_opened_in_preview.connect(mgr.file_opened_in_preview)
    panel.new_conversation_requested.connect(mgr.new_conversation_requested)


def _wire_vcs_sidebar(panel: SidebarBase, mgr: SidebarManager) -> None:
    """Wire VCSSidebar signals to SidebarManager."""
    assert isinstance(panel, VCSSidebar)
    panel.file_clicked.connect(mgr.file_clicked)
    panel.file_deleted.connect(mgr.file_deleted)
    panel.file_opened_in_editor.connect(mgr.file_opened_in_editor)
    panel.file_opened_in_preview.connect(mgr.file_opened_in_preview)
    panel.file_opened_in_diff.connect(mgr.file_opened_in_diff)


def _wire_file_sidebar(panel: SidebarBase, mgr: SidebarManager) -> None:
    """Wire FileSidebar signals to SidebarManager."""
    assert isinstance(panel, FileSidebar)
    panel.file_clicked.connect(mgr.file_clicked)
    panel.file_deleted.connect(mgr.file_deleted)
    panel.file_renamed.connect(mgr.file_renamed)
    panel.file_moved.connect(mgr.file_moved)
    panel.file_opened_in_editor.connect(mgr.file_opened_in_editor)
    panel.file_opened_in_preview.connect(mgr.file_opened_in_preview)
    panel.file_opened_in_diff.connect(mgr.file_opened_in_diff)


def _wire_preview_sidebar(panel: SidebarBase, mgr: SidebarManager) -> None:
    """Wire PreviewSidebar signals to SidebarManager."""
    assert isinstance(panel, PreviewSidebar)
    panel.file_clicked.connect(mgr.file_clicked)
    panel.file_deleted.connect(mgr.file_deleted)
    panel.file_renamed.connect(mgr.file_renamed)
    panel.file_moved.connect(mgr.file_moved)
    panel.file_opened_in_editor.connect(mgr.file_opened_in_editor)
    panel.file_opened_in_preview.connect(mgr.file_opened_in_preview)
    panel.file_opened_in_diff.connect(mgr.file_opened_in_diff)


def _wire_search_sidebar(panel: SidebarBase, mgr: SidebarManager) -> None:
    """Wire SearchSidebar signals to SidebarManager."""
    assert isinstance(panel, SearchSidebar)
    panel.file_clicked.connect(mgr.file_clicked)
    panel.result_activated.connect(mgr.search_result_activated)
    panel.highlights_cleared.connect(mgr.search_highlights_cleared)


def _activate_search_sidebar(panel: SidebarBase) -> None:
    """Focus the search input when the search panel is activated."""
    assert isinstance(panel, SearchSidebar)
    panel.focus_search()


class MainWindow(QMainWindow):
    """Main window for the Humbug application."""

    _RESIZE_ZONE = 6

    # Maps tool_name() to the sidebar panel that should reveal the tab's file.
    # Tab types not listed here default to "files".
    _TAB_PANEL_MAP = {
        "conversation": "conversations",
        "preview": "preview",
        "diff": "vcs",
    }

    def __init__(self) -> None:
        """Initialize the main window."""
        super().__init__()

        self._logger = logging.getLogger("MainWindow")

        self._use_custom_title_bar = sys.platform != "darwin"
        self._window_controls: WindowControlsWidget | None = None
        self._settings_dialog: SettingsDialog | None = None
        self._resize_drag_active = False
        self._resize_direction: tuple[int, int] = (0, 0)
        self._resize_start_pos: QPoint | None = None
        self._resize_start_geometry = self.geometry()
        self._zoom_levels: list[float] = []
        self._zoom_base_index: int = 0

        if self._use_custom_title_bar:
            self.setWindowFlag(Qt.WindowType.FramelessWindowHint, True)
            self.setMouseTracking(True)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        strings = self._language_manager.strings()

        # Humbug menu actions
        self._about_action = QAction(strings.about_humbug, self)
        self._about_action.setMenuRole(QAction.MenuRole.AboutRole)
        self._about_action.triggered.connect(self._on_show_about_dialog)

        self._check_for_updates_action = QAction(strings.check_for_updates, self)
        self._check_for_updates_action.setMenuRole(QAction.MenuRole.ApplicationSpecificRole)
        self._check_for_updates_action.triggered.connect(self._on_check_for_updates)

        self._settings_action = QAction(strings.settings, self)
        self._settings_action.setMenuRole(QAction.MenuRole.PreferencesRole)
        self._settings_action.setShortcut(QKeySequence("Ctrl+,"))
        self._settings_action.setText(strings.settings)
        self._settings_action.triggered.connect(self._on_show_settings_dialog)

        self._quit_action = QAction(strings.quit_humbug, self)
        self._quit_action.setMenuRole(QAction.MenuRole.QuitRole)
        self._quit_action.setShortcut(QKeySequence("Ctrl+Q"))
        self._quit_action.triggered.connect(self.close)

        # File menu actions
        self._new_mindspace_action = QAction(strings.new_mindspace, self)
        self._new_mindspace_action.setShortcut(QKeySequence("Ctrl+Alt+N"))
        self._new_mindspace_action.triggered.connect(self._on_new_mindspace)

        self._new_conv_action = QAction(strings.new_conversation, self)
        self._new_conv_action.setShortcut(QKeySequence("Ctrl+Shift+N"))
        self._new_conv_action.triggered.connect(self._on_new_conversation)

        self._new_file_action = QAction(strings.new_file, self)
        self._new_file_action.setShortcut(QKeySequence.StandardKey.New)
        self._new_file_action.triggered.connect(self._on_new_file)

        self._new_terminal_action = QAction(strings.new_terminal, self)
        self._new_terminal_action.setShortcut(QKeySequence("Ctrl+Alt+T"))
        self._new_terminal_action.triggered.connect(self._on_new_terminal)

        self._open_mindspace_action = QAction(strings.open_mindspace, self)
        self._open_mindspace_action.setShortcut(QKeySequence("Ctrl+Alt+O"))
        self._open_mindspace_action.triggered.connect(self._on_open_mindspace)

        self._open_preview_action = QAction(strings.open_preview, self)
        self._open_preview_action.setShortcut(QKeySequence("Ctrl+Shift+W"))
        self._open_preview_action.triggered.connect(self._on_open_preview)

        self._open_diff_action = QAction(strings.open_diff, self)
        self._open_diff_action.setShortcut(QKeySequence("Ctrl+Shift+D"))
        self._open_diff_action.triggered.connect(self._on_open_diff)

        self._open_conv_action = QAction(strings.open_conversation, self)
        self._open_conv_action.setShortcut(QKeySequence("Ctrl+Shift+O"))
        self._open_conv_action.triggered.connect(self._on_open_conversation)

        self._open_file_action = QAction(strings.open_file, self)
        self._open_file_action.setShortcut(QKeySequence.StandardKey.Open)
        self._open_file_action.triggered.connect(self._on_open_file)

        self._save_action = QAction(strings.save, self)
        self._save_action.setShortcut(QKeySequence.StandardKey.Save)
        self._save_action.triggered.connect(self._on_save_file)

        self._save_as_action = QAction(strings.save_as, self)
        self._save_as_action.setShortcut(QKeySequence.StandardKey.SaveAs)
        self._save_as_action.triggered.connect(self._on_save_file_as)

        self._close_tab_action = QAction(strings.close_tab, self)
        self._close_tab_action.setShortcut(QKeySequence("Ctrl+W"))
        self._close_tab_action.triggered.connect(self._on_close_tab)

        self._close_mindspace_action = QAction(strings.close_mindspace, self)
        self._close_mindspace_action.setShortcut(QKeySequence("Ctrl+Alt+W"))
        self._close_mindspace_action.triggered.connect(self._on_close_mindspace)

        # Edit menu actions
        self._submit_message_action = QAction(strings.submit_message, self)
        self._submit_message_action.setShortcut(QKeySequence("Ctrl+Enter"))
        self._submit_message_action.triggered.connect(self._on_submit_message)

        self._undo_action = QAction(strings.undo, self)
        self._undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self._undo_action.triggered.connect(self._undo)

        self._redo_action = QAction(strings.redo, self)
        self._redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self._redo_action.triggered.connect(self._redo)

        self._cut_action = QAction(strings.cut, self)
        self._cut_action.setShortcut(QKeySequence("Ctrl+X"))
        self._cut_action.triggered.connect(self._cut)

        self._copy_action = QAction(strings.copy, self)
        self._copy_action.setShortcut(QKeySequence("Ctrl+C"))
        self._copy_action.triggered.connect(self._copy)

        self._paste_action = QAction(strings.paste, self)
        self._paste_action.setShortcut(QKeySequence("Ctrl+V"))
        self._paste_action.triggered.connect(self._paste)

        self._find_action = QAction(strings.find, self)
        self._find_action.setShortcut(QKeySequence.StandardKey.Find)
        self._find_action.triggered.connect(self._find)

        self._find_replace_action = QAction(strings.find_replace, self)
        find_replace_shortcut = "Ctrl+Alt+F" if sys.platform == "darwin" else "Ctrl+H"
        self._find_replace_action.setShortcut(QKeySequence(find_replace_shortcut))
        self._find_replace_action.triggered.connect(self._find_replace)

        self._goto_line_action = QAction(strings.goto_line, self)
        goto_line_shortcut = "Meta+G" if sys.platform == "darwin" else "Ctrl+G"
        self._goto_line_action.setShortcut(QKeySequence(goto_line_shortcut))
        self._goto_line_action.triggered.connect(self._goto_line)

        self._global_search_action = QAction(strings.mindspace_search, self)
        self._global_search_action.setShortcut(QKeySequence("Ctrl+Shift+F"))
        self._global_search_action.triggered.connect(self._show_global_search)

        self._mindspace_settings_action = QAction(strings.mindspace_settings, self)
        self._mindspace_settings_action.setShortcut(QKeySequence("Ctrl+Alt+,"))
        self._mindspace_settings_action.triggered.connect(self._on_show_settings_dialog)

        self._conv_settings_action = QAction(strings.conversation_settings, self)
        self._conv_settings_action.setShortcut(QKeySequence("Ctrl+Shift+,"))
        self._conv_settings_action.triggered.connect(self._on_show_conversation_settings_dialog)

        # View menu actions - Theme menu will be created in _on_language_changed
        self._theme_menu: QMenu | None = None
        self._theme_actions: Dict[ColorMode, QAction] = {}

        self._zoom_in_action = QAction(strings.zoom_in, self)
        self._zoom_in_action.setShortcut(QKeySequence("Ctrl+="))
        self._zoom_in_action.triggered.connect(lambda: self._handle_zoom(1.04427379))

        self._zoom_out_action = QAction(strings.zoom_out, self)
        self._zoom_out_action.setShortcut(QKeySequence("Ctrl+-"))
        self._zoom_out_action.triggered.connect(lambda: self._handle_zoom(1/1.04427379))

        self._reset_zoom_action = QAction(strings.reset_zoom, self)
        self._reset_zoom_action.setShortcut(QKeySequence("Ctrl+0"))
        self._reset_zoom_action.triggered.connect(lambda: self._set_zoom(1.0))

        self._show_system_log_action = QAction(strings.show_system_log, self)
        self._show_system_log_action.setShortcut(QKeySequence("Ctrl+Shift+L"))
        self._show_system_log_action.triggered.connect(self._on_show_system_log)

        self._show_system_shell_action = QAction(strings.show_system_shell, self)
        self._show_system_shell_action.setShortcut(QKeySequence("Ctrl+Shift+Y"))
        self._show_system_shell_action.triggered.connect(self._on_show_system_shell)

        self._show_tab_overview_action = QAction(strings.show_tab_overview, self)
        self._show_tab_overview_action.setShortcut(QKeySequence("Ctrl+Shift+E"))
        self._show_tab_overview_action.triggered.connect(self._on_show_tab_overview)

        self._show_tab_carousel_action = QAction(strings.show_tab_carousel, self)
        self._show_tab_carousel_action.setShortcut(QKeySequence("Ctrl+Shift+T"))
        self._show_tab_carousel_action.triggered.connect(self._on_show_tab_carousel)

        self._show_all_columns_action = QAction(strings.show_all_columns, self)
        self._show_all_columns_action.setShortcut(QKeySequence("Ctrl+\\"))
        self._show_all_columns_action.triggered.connect(self._on_show_all_columns)

        self._split_column_left_action = QAction(strings.split_column_left, self)
        self._split_column_left_action.setShortcut(QKeySequence("Ctrl+Shift+["))

        self._split_column_right_action = QAction(strings.split_column_right, self)
        self._split_column_right_action.setShortcut(QKeySequence("Ctrl+Shift+]"))

        self._merge_column_left_action = QAction(strings.merge_column_left, self)
        self._merge_column_left_action.setShortcut(QKeySequence("Ctrl+["))

        self._merge_column_right_action = QAction(strings.merge_column_right, self)
        self._merge_column_right_action.setShortcut(QKeySequence("Ctrl+]"))

        self._swap_column_left_action = QAction(strings.swap_column_left, self)
        self._swap_column_left_action.setShortcut(QKeySequence("Ctrl+Alt+["))

        self._swap_column_right_action = QAction(strings.swap_column_right, self)
        self._swap_column_right_action.setShortcut(QKeySequence("Ctrl+Alt+]"))

        self._next_message_action = QAction(strings.next_message, self)
        self._next_message_action.setShortcut(QKeySequence("Alt+Down"))
        self._next_message_action.triggered.connect(self._on_navigate_next_message)

        self._previous_message_action = QAction(strings.previous_message, self)
        self._previous_message_action.setShortcut(QKeySequence("Alt+Up"))
        self._previous_message_action.triggered.connect(self._on_navigate_previous_message)

        self._menu_bar = QMenuBar(self)
        self.setMenuBar(self._menu_bar)

        # Humbug menu
        self._humbug_menu = self._menu_bar.addMenu(strings.humbug_menu)
        self._humbug_menu.addAction(self._about_action)
        self._humbug_menu.addAction(self._check_for_updates_action)
        self._humbug_menu.addSeparator()
        self._humbug_menu.addAction(self._settings_action)
        self._humbug_menu.addSeparator()
        self._humbug_menu.addAction(self._quit_action)

        # File menu
        self._file_menu = self._menu_bar.addMenu(strings.file_menu)
        self._file_menu.addAction(self._new_mindspace_action)
        self._file_menu.addAction(self._new_conv_action)
        self._file_menu.addAction(self._new_file_action)
        self._file_menu.addAction(self._new_terminal_action)
        self._file_menu.addSeparator()
        self._file_menu.addAction(self._open_mindspace_action)
        self._file_menu.addAction(self._open_preview_action)
        self._file_menu.addAction(self._open_diff_action)
        self._file_menu.addAction(self._open_conv_action)
        self._file_menu.addAction(self._open_file_action)
        self._file_menu.addSeparator()
        self._file_menu.addAction(self._save_action)
        self._file_menu.addAction(self._save_as_action)
        self._file_menu.addSeparator()
        self._file_menu.addAction(self._close_mindspace_action)
        self._file_menu.addAction(self._close_tab_action)

        # Edit menu
        self._edit_menu = self._menu_bar.addMenu(strings.edit_menu)
        self._edit_menu.addAction(self._submit_message_action)
        self._edit_menu.addSeparator()
        self._edit_menu.addAction(self._undo_action)
        self._edit_menu.addAction(self._redo_action)
        self._edit_menu.addSeparator()
        self._edit_menu.addAction(self._cut_action)
        self._edit_menu.addAction(self._copy_action)
        self._edit_menu.addAction(self._paste_action)
        self._edit_menu.addSeparator()
        self._edit_menu.addAction(self._find_action)
        self._edit_menu.addAction(self._find_replace_action)
        self._edit_menu.addAction(self._goto_line_action)
        self._edit_menu.addAction(self._global_search_action)
        self._edit_menu.addSeparator()
        self._edit_menu.addAction(self._conv_settings_action)

        # View menu
        self._view_menu = self._menu_bar.addMenu(strings.view_menu)

        # Theme menu will be added when language changes
        self._view_menu.addSeparator()
        self._view_menu.addAction(self._zoom_in_action)
        self._view_menu.addAction(self._zoom_out_action)
        self._view_menu.addAction(self._reset_zoom_action)
        self._view_menu.addSeparator()
        self._view_menu.addAction(self._show_system_log_action)
        self._view_menu.addAction(self._show_system_shell_action)
        self._view_menu.addSeparator()
        self._view_menu.addAction(self._show_tab_overview_action)
        self._view_menu.addAction(self._show_tab_carousel_action)
        self._view_menu.addSeparator()
        self._view_menu.addAction(self._show_all_columns_action)
        self._view_menu.addAction(self._split_column_left_action)
        self._view_menu.addAction(self._split_column_right_action)
        self._view_menu.addAction(self._merge_column_left_action)
        self._view_menu.addAction(self._merge_column_right_action)
        self._view_menu.addAction(self._swap_column_left_action)
        self._view_menu.addAction(self._swap_column_right_action)
        self._view_menu.addSeparator()
        self._view_menu.addAction(self._next_message_action)
        self._view_menu.addAction(self._previous_message_action)

        self.setWindowTitle("Humbug")
        self.setMinimumSize(1280, 800)

        if self._use_custom_title_bar:
            self._window_controls = WindowControlsWidget(self)
            self._menu_bar.setCornerWidget(self._window_controls)
            MenuBarDragFilter(self._menu_bar)

        # Main widget and layout
        main_widget = QWidget()
        self.setCentralWidget(main_widget)
        layout = QVBoxLayout(main_widget)
        layout.setSpacing(0)
        layout.setContentsMargins(0, 0, 0, 0)

        # Create splitter
        self._splitter = MainWindowSplitter(Qt.Orientation.Horizontal)
        layout.addWidget(self._splitter)

        self._sidebar_manager = SidebarManager(self)
        self._sidebar_manager.register_panel("conversations", "conversation", ConversationSidebar, _wire_conversation_sidebar)
        self._sidebar_manager.register_panel("vcs", "diff", VCSSidebar, _wire_vcs_sidebar, visibility_signal="repo_available")
        self._sidebar_manager.register_panel("files", "files", FileSidebar, _wire_file_sidebar)
        self._sidebar_manager.register_panel("preview", "preview", PreviewSidebar, _wire_preview_sidebar)
        self._sidebar_manager.register_panel(
            "search", "search", SearchSidebar, _wire_search_sidebar, on_activated=_activate_search_sidebar
        )
        self._sidebar_manager.file_clicked.connect(self._on_sidebar_file_clicked)
        self._sidebar_manager.file_deleted.connect(self._on_sidebar_file_deleted)
        self._sidebar_manager.file_renamed.connect(self._on_sidebar_file_renamed)
        self._sidebar_manager.file_moved.connect(self._on_sidebar_file_moved)
        self._sidebar_manager.file_opened_in_editor.connect(self._on_sidebar_file_opened_in_editor)
        self._sidebar_manager.file_opened_in_preview.connect(self._on_sidebar_file_opened_in_preview)
        self._sidebar_manager.file_opened_in_diff.connect(self._on_sidebar_file_opened_in_diff)
        self._sidebar_manager.search_result_activated.connect(self._on_mindspace_search_result_activated)
        self._sidebar_manager.search_highlights_cleared.connect(self._clear_global_search_highlights)
        self._sidebar_manager.open_mindspace_requested.connect(self._on_open_mindspace)
        self._sidebar_manager.settings_requested.connect(self._on_show_settings_dialog)
        self._sidebar_manager.new_conversation_requested.connect(self._on_sidebar_new_conversation_in_folder)
        self._sidebar_manager.toggle_requested.connect(self._splitter.toggle_mindspace)
        self._splitter.addWidget(self._sidebar_manager)

        # Create tab manager in splitter
        self._tab_manager = TabManager(self._open_path_from_drop, self)
        self._tab_manager.tab_changed.connect(self._on_tab_manager_tab_changed)
        self._tab_manager.user_settings_requested.connect(self._on_show_settings_dialog_ai_backends)
        self._tab_manager.tab_closed.connect(self._on_tab_manager_tab_closed)
        self._tab_manager.new_tab_requested.connect(self._on_tab_bar_new_tab_requested)
        self._splitter.addWidget(self._tab_manager)

        # Register tab factories for session restore and context-open events.
        tab_manager = self._tab_manager
        tab_manager.register_tab_factory("conversation", ConversationTab.restore_from_state)
        tab_manager.register_tab_factory("editor", EditorTab.restore_from_state)
        tab_manager.register_tab_factory("log", LogTab.restore_from_state)
        tab_manager.register_tab_factory("shell", ShellTab.restore_from_state)
        tab_manager.register_tab_factory("terminal", TerminalTab.restore_from_state)
        tab_manager.register_tab_factory("preview", PreviewTab.restore_from_state)
        tab_manager.register_tab_factory("diff", DiffTab.restore_from_state)

        tab_manager.register_context_factory("conversation", _create_conversation_tab)
        tab_manager.register_context_factory("editor", _create_editor_tab)
        tab_manager.register_context_factory("terminal", _create_terminal_tab)
        tab_manager.register_context_factory("preview", _create_preview_tab)
        tab_manager.register_context_factory("diff", _create_diff_tab)
        tab_manager.register_context_factory("log", _create_log_tab)
        tab_manager.register_context_factory("shell", _create_shell_tab)

        # Set initial sidebar width
        self._splitter.setSizes([300, self.width() - 300])

        # Set the stretch factors: 0 for sidebar (no stretch) and 1 for tabs (stretch to fill)
        self._splitter.setStretchFactor(0, 0)
        self._splitter.setStretchFactor(1, 1)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._on_style_changed)

        # Create a timer that fires every 50ms to keep our menu states correct
        self._menu_timer = QTimer()
        self._menu_timer.setInterval(50)
        self._menu_timer.timeout.connect(self._update_menu_state)
        self._menu_timer.start()

        # Create status bar
        self._status_bar = QStatusBar()
        self._status_message_label = QLabel()
        self._status_bar.addPermanentWidget(self._status_message_label)

        self.setStatusBar(self._status_bar)
        self._tab_manager.status_message.connect(self._on_tab_manager_status_message)

        # Connect to exception notifier for canary functionality
        self._canary_active = False
        exception_notifier = get_exception_notifier()
        exception_notifier.exception_occurred.connect(self._on_exception_occurred)

        self._on_language_changed()

        self._user_manager = UserManager()
        user_settings = self._user_manager.settings()
        self._style_manager.set_user_font_size(user_settings.font_size)
        self._build_zoom_levels()
        self._style_manager.set_font_ligatures(user_settings.font_ligatures)
        self._language_manager.set_language(user_settings.language)

        # Set theme from user settings
        self._style_manager.set_color_mode(user_settings.theme)
        self._update_theme_menu()

        # Update welcome widget with initial user settings
        self._tab_manager.update_welcome_widget(user_settings)

        self._mindspace_manager = MindspaceManager()
        mindspace = self._mindspace_manager.mindspace()

        # Initialize command registry and register commands
        self._command_registry = ShellCommandRegistry()

        def _clear_shell_history() -> None:
            for tab in self._tab_manager.get_all_tabs():
                if tab.tool_name() == "shell":
                    tab.clear_history()
                    return

        self._command_registry.register_command(ShellCommandCat())
        self._command_registry.register_command(ShellCommandClear(_clear_shell_history))
        self._command_registry.register_command(ShellCommandConversation())
        self._command_registry.register_command(ShellCommandDiff())
        self._command_registry.register_command(ShellCommandEdit())
        self._command_registry.register_command(ShellCommandLog())
        self._command_registry.register_command(ShellCommandPreview())
        self._command_registry.register_command(ShellCommandTerminal())

        # Register help command last so it can see all other commands
        self._command_registry.register_command(ShellCommandHelp(self._command_registry))

        self._ai_tool_manager = AIToolManager()

        # Store Menai tool instance so we can update its module path when mindspace changes
        self._menai_tool = MenaiAITool()
        self._menai_tool.set_mindspace(mindspace)
        self._ai_tool_manager.register_tool(
            self._menai_tool, "Menai: evaluates expressions using AI Functional Programming Language syntax"
        )
        self._ai_tool_manager.register_tool(ClockAITool(), "Clock: gets the current time and date")
        self._ai_tool_manager.register_tool(
            DelegateAITool(mindspace), "Delegate: delegates tasks to specialized AI instances"
        )
        self._ai_tool_manager.register_tool(
            FileSystemAITool(
                self._resolve_mindspace_path,
                self._get_filesystem_access_settings,
                mindspace
            ),
            "FileSystem: handles file operations in the current mindspace"
        )
        self._ai_tool_manager.register_tool(
            DocumentConverterAITool(self._resolve_mindspace_path, mindspace),
            "DocumentConverter: converts documents between supported formats in the current mindspace"
        )
        self._ai_tool_manager.register_tool(
            SystemAITool(self._tab_manager, mindspace),
            "System: manages UI tab lifecycle operations (create, open, close, organize tabs)"
        )
        self._ai_tool_manager.register_tool(
            EditorAITool(mindspace), "Editor: operations for interacting with editor tabs"
        )
        self._ai_tool_manager.register_tool(
            TerminalAITool(mindspace), "Terminal: operations for interacting with terminal tabs"
        )
        self._ai_tool_manager.register_tool(
            ConversationAITool(mindspace), "Conversation: operations for interacting with conversation tabs"
        )
        self._ai_tool_manager.register_tool(
            PreviewAITool(mindspace), "Preview: operations for interacting with preview tabs"
        )
        self._ai_tool_manager.register_tool(
            HelpAITool(self._ai_tool_manager), "Help: provides detailed documentation for AI tools and operations"
        )

        QTimer.singleShot(0, self._restore_last_mindspace)
        QTimer.singleShot(0, self._load_user_ai_config)

        self._update_checker = UpdateChecker(self)
        self._update_checker.update_available.connect(self._on_update_available)
        QTimer.singleShot(5000, self._run_startup_update_check)

    def changeEvent(self, event: QEvent) -> None:
        """Handle change events."""
        super().changeEvent(event)
        if event.type() == QEvent.Type.WindowStateChange:
            self._tab_manager.apply_style()
            if self._window_controls is not None:
                QTimer.singleShot(0, self._update_window_controls_state)

    def _update_window_controls_state(self) -> None:
        """Update the window controls maximised state after the event loop settles."""
        if self._window_controls is not None:
            state = self.windowState()
            is_maximised = bool(state & (Qt.WindowState.WindowMaximized | Qt.WindowState.WindowFullScreen))
            self._window_controls.set_maximised(is_maximised)


    def _on_exception_occurred(self) -> None:
        """Handle uncaught exception notification by activating canary."""
        self._logger.debug("Uncaught exception detected, activating canary")
        self._canary_active = True
        self._on_style_changed()  # Refresh styles to apply canary background

    def _update_menu_state(self) -> None:
        """Update enabled/disabled state of menu items."""
        # Update mindspace-specific actions
        has_mindspace = self._mindspace_manager.has_mindspace()
        self._close_mindspace_action.setEnabled(has_mindspace)
        self._new_conv_action.setEnabled(has_mindspace)
        self._new_file_action.setEnabled(has_mindspace)
        self._open_preview_action.setEnabled(has_mindspace)
        self._open_diff_action.setEnabled(has_mindspace)
        self._open_conv_action.setEnabled(has_mindspace)
        self._open_file_action.setEnabled(has_mindspace)
        self._new_terminal_action.setEnabled(has_mindspace)

        # Update tab-specific actions
        tab_manager = self._tab_manager
        tab = tab_manager.get_current_tab()
        self._save_action.setEnabled(tab is not None and tab.can_save())
        self._save_as_action.setEnabled(tab is not None and tab.can_save_as())
        self._close_tab_action.setEnabled(tab is not None)
        self._undo_action.setEnabled(tab is not None and tab.can_undo())
        self._redo_action.setEnabled(tab is not None and tab.can_redo())
        self._cut_action.setEnabled(tab is not None and tab.can_cut())
        self._copy_action.setEnabled(tab is not None and tab.can_copy())
        self._paste_action.setEnabled(tab is not None and tab.can_paste())
        self._find_action.setEnabled(tab is not None)
        self._find_replace_action.setEnabled(tab is not None and tab.can_show_find_replace())
        self._goto_line_action.setEnabled(tab is not None and tab.can_show_goto_line())
        self._global_search_action.setEnabled(has_mindspace)
        self._submit_message_action.setEnabled(tab is not None and tab.can_submit())
        self._conv_settings_action.setEnabled(tab is not None and tab.can_show_conversation_settings_dialog())

        # Update view actions
        current_zoom = self._style_manager.zoom_factor()
        current_zoom_index = self._zoom_levels.index(current_zoom) if current_zoom in self._zoom_levels else self._zoom_base_index
        left_to_right = self._language_manager.left_to_right()
        self._zoom_in_action.setEnabled(current_zoom_index < len(self._zoom_levels) - 1)
        self._zoom_out_action.setEnabled(current_zoom_index > 0)
        self._show_system_log_action.setEnabled(has_mindspace)
        self._show_system_shell_action.setEnabled(has_mindspace)
        self._show_all_columns_action.setEnabled(tab_manager.can_show_all_columns())
        self._show_tab_overview_action.setEnabled(tab is not None)
        self._show_tab_carousel_action.setEnabled(tab is not None)
        self._split_column_left_action.setEnabled(tab_manager.can_split_column())
        self._split_column_right_action.setEnabled(tab_manager.can_split_column())
        self._merge_column_left_action.setEnabled(tab_manager.can_merge_column(left_to_right))
        self._merge_column_right_action.setEnabled(tab_manager.can_merge_column(not left_to_right))
        self._swap_column_left_action.setEnabled(tab_manager.can_swap_column(left_to_right))
        self._swap_column_right_action.setEnabled(tab_manager.can_swap_column(not left_to_right))
        self._next_message_action.setEnabled(tab is not None and tab.can_navigate_next_message())
        self._previous_message_action.setEnabled(tab is not None and tab.can_navigate_previous_message())
        self._update_navigation_action_text()

    def _on_language_changed(self) -> None:
        """Update UI text when language changes."""
        self._update_navigation_action_text()

        app = cast(QApplication, QApplication.instance())
        left_to_right = self._language_manager.left_to_right()
        if left_to_right:
            app.setLayoutDirection(Qt.LayoutDirection.LeftToRight)

        else:
            app.setLayoutDirection(Qt.LayoutDirection.RightToLeft)

        strings = self._language_manager.strings()

        # Update menu titles
        self._humbug_menu.setTitle(strings.humbug_menu)
        self._edit_menu.setTitle(strings.edit_menu)
        self._file_menu.setTitle(strings.file_menu)
        self._view_menu.setTitle(strings.view_menu)

        # Update action texts
        self._about_action.setText(strings.about_humbug)
        self._check_for_updates_action.setText(strings.check_for_updates)
        self._settings_action.setText(strings.settings)
        self._quit_action.setText(strings.quit_humbug)
        self._new_mindspace_action.setText(strings.new_mindspace)
        self._new_conv_action.setText(strings.new_conversation)
        self._new_file_action.setText(strings.new_file)
        self._open_mindspace_action.setText(strings.open_mindspace)
        self._open_preview_action.setText(strings.open_preview)
        self._open_diff_action.setText(strings.open_diff)
        self._open_conv_action.setText(strings.open_conversation)
        self._open_file_action.setText(strings.open_file)
        self._save_action.setText(strings.save)
        self._save_as_action.setText(strings.save_as)
        self._close_tab_action.setText(strings.close_tab)
        self._close_mindspace_action.setText(strings.close_mindspace)
        self._submit_message_action.setText(strings.submit_message)
        self._undo_action.setText(strings.undo)
        self._redo_action.setText(strings.redo)
        self._cut_action.setText(strings.cut)
        self._copy_action.setText(strings.copy)
        self._paste_action.setText(strings.paste)
        self._find_action.setText(strings.find)
        self._find_replace_action.setText(strings.find_replace)
        self._goto_line_action.setText(strings.goto_line)
        self._global_search_action.setText(strings.mindspace_search)
        self._mindspace_settings_action.setText(strings.mindspace_settings)
        self._conv_settings_action.setText(strings.conversation_settings)

        # Recreate the theme menu with updated language strings
        if self._theme_menu is not None:
            self._view_menu.removeAction(self._theme_menu.menuAction())

        self._theme_menu = self._create_theme_menu()
        self._view_menu.insertMenu(self._zoom_in_action, self._theme_menu)

        self._zoom_in_action.setText(strings.zoom_in)
        self._zoom_out_action.setText(strings.zoom_out)
        self._reset_zoom_action.setText(strings.reset_zoom)
        self._show_system_shell_action.setText(strings.show_system_shell)
        self._show_tab_overview_action.setText(strings.show_tab_overview)
        self._show_tab_carousel_action.setText(strings.show_tab_carousel)
        self._show_all_columns_action.setText(strings.show_all_columns)
        self._split_column_left_action.setText(strings.split_column_left)
        self._split_column_right_action.setText(strings.split_column_right)
        self._merge_column_left_action.setText(strings.merge_column_left)
        self._merge_column_right_action.setText(strings.merge_column_right)
        self._swap_column_left_action.setText(strings.swap_column_left)
        self._swap_column_right_action.setText(strings.swap_column_right)

        # Our logic for left and right reverses for right-to-left languages
        left_to_right = self._language_manager.left_to_right()
        self._split_column_left_action.triggered.disconnect()
        self._split_column_left_action.triggered.connect(lambda: self._on_split_column(left_to_right))
        self._split_column_right_action.triggered.disconnect()
        self._split_column_right_action.triggered.connect(lambda: self._on_split_column(not left_to_right))
        self._merge_column_left_action.triggered.disconnect()
        self._merge_column_left_action.triggered.connect(lambda: self._on_merge_column(left_to_right))
        self._merge_column_right_action.triggered.disconnect()
        self._merge_column_right_action.triggered.connect(lambda: self._on_merge_column(not left_to_right))
        self._swap_column_left_action.triggered.disconnect()
        self._swap_column_left_action.triggered.connect(lambda: self._on_swap_column(left_to_right))
        self._swap_column_right_action.triggered.disconnect()
        self._swap_column_right_action.triggered.connect(lambda: self._on_swap_column(not left_to_right))

        self._on_style_changed()

    def _update_navigation_action_text(self) -> None:
        """Set the next/previous action labels to match the current tab type."""
        strings = self._language_manager.strings()
        tab = self._tab_manager.get_current_tab()
        if tab is not None and tab.is_navigating_as_hunks():
            self._next_message_action.setText(strings.next_hunk)
            self._previous_message_action.setText(strings.previous_hunk)

        else:
            self._next_message_action.setText(strings.next_message)
            self._previous_message_action.setText(strings.previous_message)

    def _create_theme_menu(self) -> QMenu:
        """
        Create a display theme submenu with available themes.

        Returns:
            QMenu: The theme submenu
        """
        strings = self._language_manager.strings()

        # Create the theme menu
        theme_menu = self._style_manager.create_menu(self)
        theme_menu.setTitle(strings.display_theme)

        # Create an action group so only one theme can be selected at a time
        theme_action_group = QActionGroup(self)
        theme_action_group.setExclusive(True)

        # Create the theme actions dictionary to store references
        self._theme_actions = {}

        # Add Automatic (system) theme action
        system_action = QAction(strings.theme_system, self)
        system_action.setCheckable(True)
        system_action.setChecked(self._style_manager.user_color_mode() == ColorMode.SYSTEM)
        system_action.triggered.connect(lambda: self._set_color_mode(ColorMode.SYSTEM))
        theme_action_group.addAction(system_action)
        theme_menu.addAction(system_action)
        self._theme_actions[ColorMode.SYSTEM] = system_action

        # Add Light theme action
        light_action = QAction(strings.theme_light, self)
        light_action.setCheckable(True)
        light_action.setChecked(self._style_manager.user_color_mode() == ColorMode.LIGHT)
        light_action.triggered.connect(lambda: self._set_color_mode(ColorMode.LIGHT))
        theme_action_group.addAction(light_action)
        theme_menu.addAction(light_action)
        self._theme_actions[ColorMode.LIGHT] = light_action

        # Add Dark theme action
        dark_action = QAction(strings.theme_dark, self)
        dark_action.setCheckable(True)
        dark_action.setChecked(self._style_manager.user_color_mode() == ColorMode.DARK)
        dark_action.triggered.connect(lambda: self._set_color_mode(ColorMode.DARK))
        theme_action_group.addAction(dark_action)
        theme_menu.addAction(dark_action)
        self._theme_actions[ColorMode.DARK] = dark_action

        return theme_menu

    def _update_theme_menu(self) -> None:
        """Update the theme menu to reflect the current selected theme."""
        # Set the checked state for the appropriate theme action
        for theme, action in self._theme_actions.items():
            action.setChecked(theme == self._style_manager.user_color_mode())

    def _set_color_mode(self, theme: ColorMode) -> None:
        """
        Set the color mode (theme) for the application.

        Args:
            theme: The new theme to apply
        """
        self._style_manager.set_color_mode(theme)

    def _on_tab_manager_tab_changed(self) -> None:
        """Handle tab change events."""
        current_tab = self._tab_manager.get_current_tab()
        if current_tab is None:
            return

        path = current_tab.path()
        if path is None:
            return

        panel = self._TAB_PANEL_MAP.get(current_tab.tool_name(), "files")
        self._sidebar_manager.reveal_and_select_file(panel, path)

    def _on_tab_manager_status_message(self, message: StatusMessage) -> None:
        """Update status bar with new message."""
        self._status_message_label.setText(message.text)
        if message.timeout:
            QTimer.singleShot(message.timeout, self._status_message_label.clear)

    def _restore_last_mindspace(self) -> None:
        """Restore last mindspace on startup if available."""
        try:
            with open(os.path.expanduser("~/.humbug/mindspace.json"), encoding='utf-8') as f:
                data = json.load(f)
                mindspace_path = data.get("lastMindspace")
                if mindspace_path and os.path.exists(mindspace_path):
                    try:
                        self._mindspace_manager.open_mindspace(mindspace_path)
                        self._sidebar_manager.set_mindspace(mindspace_path)
                        self._restore_mindspace_state()

                    except MindspaceError as e:
                        self._logger.error("Failed to restore mindspace: %s", str(e))
                        # Don't show error dialog on startup, just log it

        except (FileNotFoundError, json.JSONDecodeError):
            pass

    def _load_user_ai_config(self) -> None:
        """Load user-defined AI model config from ~/.humbug/user-ai-config.json."""
        path = os.path.expanduser("~/.humbug/user-ai-config.json")
        errors = AIConversationSettings.load_user_config(path)
        if errors:
            detail = "\n".join(f"• {e}" for e in errors)
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "User AI Config Error",
                f"Failed to load user AI configuration:\n\n{detail}"
            )

    def _on_new_mindspace(self) -> None:
        """Show folder selection dialog and create new mindspace."""
        self._menu_timer.stop()
        strings = self._language_manager.strings()
        dir_path = QFileDialog.getExistingDirectory(self, strings.file_dialog_new_mindspace, os.path.expanduser("~"))
        self._menu_timer.start()
        if not dir_path:
            return

        if self._mindspace_manager.is_already_mindspace(dir_path):
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.mindspace_exists_error
            )
            return

        # Show folder configuration dialog
        dialog = MindspaceFoldersDialog(dir_path, self)
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return

        try:
            # Create mindspace with selected folders
            self._mindspace_manager.create_mindspace(dir_path, dialog.get_selected_folders())

        except MindspaceExistsError:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.mindspace_exists_error
            )
            return

        except MindspaceError as e:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_creating_mindspace.format(str(e))
            )
            return

        self._open_mindspace_path(dir_path)

    def _on_open_mindspace(self) -> None:
        """Open a new mindspace."""
        self._menu_timer.stop()
        strings = self._language_manager.strings()
        dir_path = QFileDialog.getExistingDirectory(self, strings.file_dialog_open_mindspace, os.path.expanduser("~"))
        self._menu_timer.start()
        if not dir_path:
            return

        self._open_mindspace_path(dir_path)

    def _open_mindspace_path(self, path: str) -> None:
        # Before we do anything, check if the new location is a mindspace
        if not self._mindspace_manager.check_mindspace(path):
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_opening_mindspace.format(path)
            )
            return

        # If we're switching mindspaces, save the current one first
        if self._mindspace_manager.has_mindspace():
            self._save_mindspace_state()
            if not self._close_all_tabs():
                return

            self._mindspace_manager.close_mindspace()

        # Open the new mindspace
        try:
            self._mindspace_manager.open_mindspace(path)
            self._sidebar_manager.set_mindspace(path)

        except MindspaceError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_opening_mindspace.format(str(e))
            )
            return

        # Restore the state of the newly opened mindspace
        self._restore_mindspace_state()

    def _on_close_mindspace(self) -> None:
        if not self._mindspace_manager.has_mindspace():
            self._logger.error("No mindspace active, cannot close")
            return

        self._save_mindspace_state()
        if not self._close_all_tabs():
            return

        self._sidebar_manager.set_mindspace("")

        # Clear Menai module path and cache when closing mindspace
        self._menai_tool.set_module_path([])
        self._mindspace_manager.close_mindspace()

    def _save_mindspace_state(self) -> None:
        """Save current mindspace state."""
        if not self._mindspace_manager.has_mindspace():
            self._logger.error("No mindspace active, cannot save")
            return

        try:
            mindspace_state = self._tab_manager.save_state()
            self._mindspace_manager.save_mindspace_state(mindspace_state)

        except MindspaceError as e:
            self._logger.error("Failed to save mindspace state: %s", str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_saving_mindspace.format(str(e))
            )

    def _restore_mindspace_state(self) -> None:
        """Restore previously open tabs from mindspace state."""
        saved_state = self._mindspace_manager.load_mindspace_state()
        if not saved_state:
            self._logger.debug("No saved states found")
            return

        try:
            self._tab_manager.restore_state(saved_state)

        except MindspaceError as e:
            self._logger.error("Failed to restore mindspace state: %s", str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_restoring_mindspace.format(str(e))
            )

        # Update Menai module path to use the new mindspace's menai_modules directory
        mindspace_path = self._mindspace_manager.mindspace_path()
        self._menai_tool.set_module_path([mindspace_path])

    def _close_all_tabs(self) -> bool:
        return self._tab_manager.close_all_tabs()

    def _undo(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.undo()

    def _redo(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.redo()

    def _cut(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.cut()

    def _copy(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.copy()

    def _paste(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.paste()

    def _find(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.show_find()

    def _find_replace(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.show_find_replace()

    def _goto_line(self) -> None:
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.show_goto_line()

    def _show_global_search(self) -> None:
        """Open the mindspace global-search pane."""
        if not self._mindspace_manager.has_mindspace():
            return

        self._sidebar_manager.show_panel("search")

    def _on_show_about_dialog(self) -> None:
        """Show the About dialog."""
        dialog = AboutDialog(self)
        dialog.exec()

    def _run_startup_update_check(self) -> None:
        """Schedule the automatic startup update check if enabled in user settings."""
        user_settings = self._user_manager.settings()
        if not user_settings.check_for_updates:
            return

        asyncio.get_event_loop().create_task(self._update_checker.check(manual=False))

    def _on_update_available(self, version: str, release_url: str) -> None:
        """Show the update button in the mindspace rail when a new version is found."""
        self._sidebar_manager.show_update_available(version, release_url)

    def _on_check_for_updates(self) -> None:
        """Handle 'Check for Updates' menu action."""
        dialog = UpdateDialog(self)
        dialog.show()

        async def _run() -> None:
            await self._update_checker.check(manual=True)
            dialog.set_result(
                self._update_checker.current_version_str(),
                self._update_checker.latest_version(),
                self._update_checker.latest_release_url(),
            )

        asyncio.get_event_loop().create_task(_run())

    def _on_new_terminal(self) -> None:
        """Create a new terminal tab."""
        if not self._mindspace_manager.has_mindspace():
            return

        self._mindspace_manager.mindspace().contexts().open(
            context_type="terminal",
            title="Terminal",
        )

    def _on_new_file(self) -> None:
        """Create a new empty editor tab."""
        if not self._mindspace_manager.has_mindspace():
            return

        self._mindspace_manager.mindspace().contexts().open(
            context_type="editor",
            path="",
            title=EditorTab.next_untitled_title(),
        )

    def _on_sidebar_file_opened_in_preview(self, path: str) -> None:
        """Handle click of a preview link from the sidbar."""
        contexts = self._mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(path, "preview")
        if existing:
            contexts.focus(existing.context_id)

        else:
            context_id = contexts.open(
                context_type="preview",
                path=path,
                title=os.path.basename(path),
            )
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"User opened preview: '{path}'\ntab ID: {context_id}"
            )

    def _on_sidebar_file_opened_in_diff(self, path: str, ephemeral: bool) -> None:
        """Handle request to open a file diff from the sidebar."""
        contexts = self._mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(path, "diff")
        if existing:
            if not ephemeral:
                self._tab_manager.make_tab_permanent(existing.context_id)
            contexts.focus(existing.context_id)

        else:
            context_id = contexts.open(
                context_type="diff",
                path=path,
                title=os.path.basename(path),
                is_ephemeral=ephemeral,
            )
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"User opened diff: '{path}'\ntab ID: {context_id}"
            )

    def _on_sidebar_file_clicked(self, source: str, path: str, ephemeral: bool) -> None:
        """Handle click of a file from the sidebar."""
        self._open_by_panel_id(source, path, ephemeral)

    def _on_mindspace_search_result_activated(
        self,
        source: str,
        path: str,
        ephemeral: bool,
        search_text: str,
        case_sensitive: bool,
        regexp: bool,
        line_number: int | None,
        message_id: str | None,
    ) -> None:
        """Open a search result and apply the same highlight without changing local find UI state."""
        context_id = self._open_by_panel_id(source, path, ephemeral)
        if context_id is not None:
            tab = self._tab_manager.get_tab_by_id(context_id)
            if tab is not None:
                tab.navigate_to_search_match(search_text, line_number, message_id, case_sensitive=case_sensitive, regexp=regexp)

    def _open_path_from_drop(self, source_type: str, path: str) -> str | None:
        """
        Adapter for TabManager's open_path callable: maps drag-drop source strings to panel IDs.

        Args:
            source_type: Source view type string from mime data ('conversations', 'vcs', 'preview', 'files').
            path: File path to open.

        Returns:
            The context_id of the opened or focused tab, or None if skipped/failed.
        """
        return self._open_by_panel_id(source_type, path, ephemeral=False)

    def _open_by_panel_id(self, panel_id: str, path: str, ephemeral: bool) -> str | None:
        """
        Open a file with the appropriate tab type based on panel ID.

        Returns the context_id of the opened or focused tab, or None if skipped/failed.
        """
        if os.path.isdir(path) and panel_id != "preview":
            return None

        if panel_id == "vcs":
            context_type = "diff"

        elif panel_id == "conversations":
            context_type = "conversation"

        elif panel_id == "preview":
            context_type = "preview"

        else:
            context_type = "editor"

        try:
            contexts = self._mindspace_manager.mindspace().contexts()
            existing = contexts.get_by_path_and_type(path, context_type)
            if existing:
                if not ephemeral:
                    self._tab_manager.make_tab_permanent(existing.context_id)

                contexts.focus(existing.context_id)
                return existing.context_id

            context_id = contexts.open(
                context_type=context_type,
                path=path,
                title=os.path.basename(path),
                is_ephemeral=ephemeral,
            )
            return context_id

        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_opening_file_title,
                str(e)
            )
            return None

    def _clear_global_search_highlights(self) -> None:
        """Clear transient highlights that were applied from global search."""
        for tab in self._tab_manager.get_all_tabs():
            tab.clear_search_highlight()

    def _on_sidebar_file_deleted(self, path: str) -> None:
        """
        Handle deletion of a file by closing any open tab.

        Args:
            path: Path of file being deleted
        """
        tabs = [t for t in self._tab_manager.get_all_tabs() if t.path() == path]
        self._tab_manager.close_deleted_file(path)
        for tab in tabs:
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Deleted '{path}' - closed {tab.tool_name()} tab\ntab ID: {tab.tab_id()}"
            )

    def _on_sidebar_file_renamed(self, old_path: str, new_path: str) -> None:
        """
        Handle renaming of files.

        Args:
            old_path: Original path of renamed file
            new_path: New path after renaming
        """
        self._tab_manager.handle_file_rename(old_path, new_path)
        if self._mindspace_manager.has_mindspace():
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Human renamed file: '{old_path}' -> '{new_path}'"
            )

    def _on_sidebar_file_moved(self, old_path: str, new_path: str) -> None:
        """
        Handle moving of files.

        Args:
            old_path: Original path of moved file
            new_path: New path after moving
        """
        self._tab_manager.handle_file_rename(old_path, new_path)
        if self._mindspace_manager.has_mindspace():
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Human moved file: '{old_path}' -> '{new_path}'"
            )

    def _on_sidebar_file_opened_in_editor(self, path: str, ephemeral: bool) -> None:
        """Handle file opened in editor event from the sidebar."""
        self._open_file_path(path, ephemeral)

    def _on_open_file(self) -> None:
        """Show open file dialog and create editor tab."""
        self._menu_timer.stop()
        strings = self._language_manager.strings()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            strings.file_dialog_open_file,
            self._mindspace_manager.file_dialog_directory()
        )
        self._menu_timer.start()

        if not file_path:
            return

        self._mindspace_manager.update_file_dialog_directory(file_path)
        self._open_file_path(file_path, False)

    def _open_file_path(self, path: str, ephemeral: bool) -> None:
        """Open file in editor tab."""
        try:
            contexts = self._mindspace_manager.mindspace().contexts()
            existing = contexts.get_by_path_and_type(path, "editor")
            if existing:
                if not ephemeral:
                    self._tab_manager.make_tab_permanent(existing.context_id)

                contexts.focus(existing.context_id)
                context_id = existing.context_id

            else:
                context_id = contexts.open(
                    context_type="editor",
                    path=path,
                    title=os.path.basename(path),
                )

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"User opened editor for file: '{path}'\nTab ID: {context_id}"
            )

        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_opening_file_title,
                strings.could_not_open.format(path, str(e))
            )

    def _on_save_file(self) -> None:
        """Save the current file."""
        try:
            tab = self._tab_manager.get_current_tab()
            if tab is None or not tab.can_save():
                return

            tab.save()
            path = tab.path()
            self._sidebar_manager.reveal_and_select_file("files", path)
            if self._mindspace_manager.has_mindspace():
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"Human saved file: '{path}'"
                )

        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_saving_file_title,
                str(e)
            )

    def _on_save_file_as(self) -> None:
        """Save the current file with a new name."""
        try:
            tab = self._tab_manager.get_current_tab()
            if tab is None or not tab.can_save_as():
                return
            tab.save_as()
            path = tab.path()
            self._sidebar_manager.reveal_and_select_file("files", path)
            if self._mindspace_manager.has_mindspace():
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"Human saved file as: '{path}'"
                )

        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_saving_file_title,
                str(e)
            )

    def _on_open_preview(self) -> None:
        """Open the preview page in a new tab."""
        preview_path = self._mindspace_manager.get_absolute_path(".")
        contexts = self._mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(preview_path, "preview")
        if existing:
            contexts.focus(existing.context_id)
            return

        mindspace_name = os.path.basename(self._mindspace_manager.mindspace_path())
        contexts.open(
            context_type="preview",
            path=preview_path,
            title=f"[{mindspace_name.upper()}]",
        )

    def _on_open_diff(self) -> None:
        """Show open file dialog and open a diff tab for the selected file."""
        if not self._mindspace_manager.has_mindspace():
            return

        self._menu_timer.stop()
        strings = self._language_manager.strings()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            strings.open_diff,
            self._mindspace_manager.file_dialog_directory()
        )
        self._menu_timer.start()

        if not file_path:
            return

        self._mindspace_manager.update_file_dialog_directory(file_path)
        contexts = self._mindspace_manager.mindspace().contexts()
        existing = contexts.get_by_path_and_type(file_path, "diff")
        if existing:
            contexts.focus(existing.context_id)

        else:
            context_id = contexts.open(
                context_type="diff",
                path=file_path,
                title=os.path.basename(file_path),
            )
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"User opened diff: '{file_path}'\ntab ID: {context_id}"
            )

    def _on_show_system_log(self) -> None:
        """Show the log tab."""
        contexts = self._mindspace_manager.mindspace().contexts()
        existing = next((i for i in contexts.list_all() if i.context_type == "log"), None)
        if existing:
            contexts.focus(existing.context_id)
            return

        contexts.open(context_type="log", title="Mindspace Log")

    def _on_show_system_shell(self) -> None:
        """Show the shell tab."""
        contexts = self._mindspace_manager.mindspace().contexts()
        existing = next((i for i in contexts.list_all() if i.context_type == "shell"), None)
        if existing:
            contexts.focus(existing.context_id)
            return

        contexts.open(context_type="shell", title="Humbug Shell")

    def _on_show_tab_overview(self) -> None:
        """Toggle the open-tabs overview overlay."""
        self._tab_manager.toggle_tab_overview()

    def _on_show_tab_carousel(self) -> None:
        """Toggle the open-tabs carousel overlay."""
        self._tab_manager.toggle_tab_carousel()

    def _on_show_all_columns(self) -> None:
        """Show all columns equally."""
        self._tab_manager.show_all_columns()

    def _on_split_column(self, split_left: bool) -> None:
        """Split the current column."""
        self._tab_manager.split_column(split_left)

    def _on_merge_column(self, merge_left: bool) -> None:
        """Merge the current column."""
        self._tab_manager.merge_column(merge_left)

    def _on_swap_column(self, swap_left: bool) -> None:
        """Swap the current column."""
        self._tab_manager.swap_column(swap_left)

    def _on_style_changed(self) -> None:
        """Handle style changes by updating all styled widgets."""
        # Apply styles to individual top-level widgets
        self._apply_menubar_style()
        self._apply_statusbar_style()
        if self._window_controls is not None:
            self._window_controls.apply_style()

        self._apply_splitter_style()

        # Apply styles to the sidebar manager and column manager
        self._sidebar_manager.apply_style()
        self._tab_manager.apply_style()

    def _apply_menubar_style(self) -> None:
        """Apply styling to menu bar."""
        style_manager = self._style_manager
        base_font_size = style_manager.base_font_size()

        new_stylesheet = f"""
            QMenuBar {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_BACKGROUND)};
                padding: 4px;
                font-size: {base_font_size}pt;
            }}
            QMenuBar::item {{
                background-color: transparent;
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-radius: 4px;
                padding: 4px 8px 4px 8px;
            }}
            QMenuBar::item:selected {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_HOVER)};
            }}
        """
        if new_stylesheet != self._menu_bar.styleSheet():
            self._menu_bar.setStyleSheet(new_stylesheet)

    def _apply_statusbar_style(self) -> None:
        """Apply styling to status bar."""
        style_manager = self._style_manager
        base_font_size = style_manager.base_font_size()

        # Determine background based on canary state
        status_bg_color = (
            style_manager.get_color_str(ColorRole.CANARY_BACKGROUND)
            if self._canary_active
            else style_manager.get_color_str(ColorRole.STATUS_BAR_BACKGROUND)
        )

        new_stylesheet = f"""
            QStatusBar {{
                background-color: {status_bg_color};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-top: 1px solid {style_manager.get_color_str(ColorRole.SPLITTER)};
            }}
            QStatusBar::item {{
                border: 0;
                padding: 0;
            }}
            QStatusBar QLabel {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                margin: 0px;
            }}
        """
        if new_stylesheet != self._status_bar.styleSheet():
            self._status_bar.setStyleSheet(new_stylesheet)

        # Update status bar font
        zoom_factor = style_manager.zoom_factor()
        status_font = self.font()
        status_font.setPointSizeF(base_font_size * zoom_factor)
        padding = int(2 * zoom_factor)
        self._status_message_label.setContentsMargins(padding, padding, padding, padding)
        self._status_bar.setFont(status_font)
        self._status_message_label.setFont(status_font)

    def _apply_splitter_style(self) -> None:
        """Trigger repaint of main window splitter handle."""
        # The main window splitter uses custom painted handles, so we just need to trigger a repaint
        self._splitter.update()

    def _generate_conversation_path(self, relative_folder: str | None = None) -> tuple[str, str]:
        """
        Generate a timestamped title and absolute path for a new conversation file.

        Args:
            relative_folder: Optional folder path relative to mindspace. Defaults to 'conversations'.

        Returns:
            Tuple of (title, absolute_path).
        """
        timestamp = datetime.now(timezone.utc)
        title = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
        folder = relative_folder if relative_folder is not None else "conversations"
        filename = os.path.join(folder, f"{title}.conv")
        return title, self._mindspace_manager.get_absolute_path(filename)

    def _on_new_conversation(self) -> None:
        """Create new conversation tab."""
        self._create_new_conversation()

    def _on_tab_bar_new_tab_requested(self, column_index: int, insert_index: int) -> None:
        """
        Create a new conversation tab at a specific position in a column.

        Args:
            column_index: Index of the column that should receive the new tab
            insert_index: Position within the column's tab bar
        """
        context_id = self._create_new_conversation()
        if context_id is None:
            return

        try:
            self._tab_manager.move_tab_to_column(context_id, column_index)

        except TabManagerError:
            return

        self._tab_manager.reposition_tab(context_id, column_index, insert_index)

    def _create_new_conversation(self) -> str | None:
        """Create a new conversation tab and return its context ID, or None on failure."""
        if not self._mindspace_manager.has_mindspace():
            return None

        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")

        except MindspaceError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_opening_conversation.format(str(e))
            )
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.ERROR,
                f"User failed to create new conversation: {str(e)}"
            )
            return None

        try:
            title, full_path = self._generate_conversation_path()
            context_id = self._mindspace_manager.mindspace().contexts().open(
                context_type="conversation",
                path=full_path,
                title=title,
            )
        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_opening_conversation.format(str(e))
            )
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.ERROR,
                f"User failed to create new conversation: {str(e)}"
            )
            return None

        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User created new conversation\ntab ID: {context_id}"
        )
        return context_id

    def _on_sidebar_new_conversation_in_folder(self, folder_path: str) -> None:
        """Create a new conversation in a specific folder."""
        if not self._mindspace_manager.has_mindspace():
            return

        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")
            relative_folder = self._mindspace_manager.get_relative_path(folder_path)
            title, full_path = self._generate_conversation_path(relative_folder)
            context_id = self._mindspace_manager.mindspace().contexts().open(
                context_type="conversation",
                path=full_path,
                title=title,
            )
        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_creating_conversation.format(str(e))
            )
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.ERROR,
                f"User failed to create new conversation in folder: {str(e)}"
            )
            return

        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User created new conversation in folder '{folder_path}'\ntab ID: {context_id}"
        )

        self._sidebar_manager.reveal_and_select_file("conversations", full_path)

    def _get_canonical_mindspace_path(self, path: str) -> str | None:
        """Get the canonical path of the current mindspace."""
        if not self._mindspace_manager.has_mindspace():
            return None

        # Get the absolute path of the current mindspace
        return self._mindspace_manager.get_relative_path(path)

    def _resolve_mindspace_path(self, path: str) -> Tuple[Path, str]:
        if not self._mindspace_manager.has_mindspace():
            raise ValueError("No mindspace open")

        # If the path is absolute and already resolves within the mindspace, use it directly.
        # Otherwise, if it starts with a separator, treat it as mindspace-root-relative by
        # stripping the leading separator before joining to the mindspace root.
        if os.path.isabs(path):
            abs_path = os.path.abspath(path)

        elif path.startswith(os.sep):
            abs_path = self._mindspace_manager.get_absolute_path(path[1:])

        else:
            abs_path = self._mindspace_manager.get_absolute_path(path)

        resolved_path = Path(abs_path).resolve()

        # Verify the resolved path is still within mindspace
        relative_path = self._mindspace_manager.get_mindspace_relative_path(str(resolved_path))
        if relative_path is None:
            raise ValueError(f"Path is outside mindspace boundaries: {path}")

        return resolved_path, relative_path

    def _get_filesystem_access_settings(self) -> FilesystemAccessSettings:
        """Get filesystem access settings from user settings."""
        user_settings = self._user_manager.settings()
        return FilesystemAccessSettings(
            allow_external_access=user_settings.allow_external_file_access,
            external_allowlist=user_settings.external_file_allowlist,
            external_denylist=user_settings.external_file_denylist
        )

    def _on_open_conversation(self) -> None:
        """Show open conversation dialog and create conversation tab."""
        self._menu_timer.stop()
        strings = self._language_manager.strings()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            strings.file_dialog_open_conversation,
            self._mindspace_manager.conversations_directory(),
            f"{strings.file_filter_conversation};;{strings.file_filter_all}"
        )
        self._menu_timer.start()

        if not file_path:
            return

        self._mindspace_manager.update_conversations_directory(file_path)
        self._open_conversation_path(file_path)

    def _open_conversation_path(self, path: str) -> None:
        """Open an existing conversation file."""
        try:
            contexts = self._mindspace_manager.mindspace().contexts()
            existing = contexts.get_by_path_and_type(path, "conversation")
            if existing:
                contexts.focus(existing.context_id)
                return

            title = os.path.splitext(os.path.basename(path))[0]
            contexts.open(
                context_type="conversation",
                path=path,
                title=title,
            )
        except Exception as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_opening_conversation.format(path, str(e))
            )

    def _on_close_tab(self) -> None:
        """Close the current tab."""
        tab = self._tab_manager.get_current_tab()
        if tab is None:
            self._logger.error("No current tab to close")
            return

        self._tab_manager.close_tab_by_id(tab.tab_id())
        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User closed tab\nTab ID: {tab.tab_id()}"
        )

    def _on_submit_message(self) -> None:
        """Handle message submission."""
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.submit()

    def _on_tab_manager_tab_closed(self, tab_id: str) -> None:
        """Handle a tab closed via the tab bar close button."""
        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User closed tab\ntab ID: {tab_id}"
        )

    def _on_navigate_next_message(self) -> None:
        """Navigate to the next message in conversation."""
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.navigate_next_message()

    def _on_navigate_previous_message(self) -> None:
        """Navigate to the previous message in conversation."""
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.navigate_previous_message()

    def _on_show_settings_dialog(self, initial_section: str | None = None) -> None:
        """Show the unified settings dialog."""
        if self._settings_dialog and self._settings_dialog.isVisible():
            self._settings_dialog.raise_()
            self._settings_dialog.activateWindow()
            return

        has_mindspace = self._mindspace_manager.has_mindspace()
        mindspace_settings = (
            cast(MindspaceSettings, self._mindspace_manager.settings())
            if has_mindspace
            else None
        )

        dialog = SettingsDialog(self)
        self._settings_dialog = dialog

        def _on_user_settings_changed(new_settings: UserSettings) -> None:
            try:
                self._user_manager.update_settings(new_settings)
                self._style_manager.set_user_font_size(new_settings.font_size)
                self._build_zoom_levels()
                self._style_manager.set_font_ligatures(new_settings.font_ligatures)
                self._language_manager.set_language(new_settings.language)

                new_theme = new_settings.theme
                if new_theme != self._style_manager.user_color_mode():
                    self._style_manager.set_color_mode(new_theme)
                    self._update_theme_menu()

                self._tab_manager.update_welcome_widget(new_settings)
                self._logger.info("User settings saved successfully")

                if self._mindspace_manager.has_mindspace():
                    self._mindspace_manager.add_interaction(
                        MindspaceLogLevel.INFO,
                        f"User settings changed\n"
                        f"external file access: {new_settings.allow_external_file_access}\n"
                        f"allowlist entries: {len(new_settings.external_file_allowlist)}\n"
                        f"denylist entries: {len(new_settings.external_file_denylist)}"
                    )

            except UserError as e:
                self._logger.error("Failed to save user settings: %s", str(e))
                strings = self._language_manager.strings()
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.settings_error_title,
                    strings.error_saving_user_settings.format(str(e))
                )

        def _on_mindspace_settings_changed(new_settings: MindspaceSettings) -> None:
            try:
                self._mindspace_manager.update_settings(new_settings)
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"Mindspace settings changed\n"
                    f"enabled tools: {', '.join(k for k, v in new_settings.enabled_tools.items() if v)}\n"
                    f"disabled tools: {', '.join(k for k, v in new_settings.enabled_tools.items() if not v)}"
                )

            except MindspaceError as e:
                self._logger.error("Failed to save mindspace settings: %s", str(e))
                strings = self._language_manager.strings()
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.settings_error_title,
                    strings.error_saving_mindspace_settings.format(str(e))
                )

        def _on_dialog_finished(_result: int) -> None:
            self._settings_dialog = None

        dialog.user_settings_changed.connect(_on_user_settings_changed)
        dialog.mindspace_settings_changed.connect(_on_mindspace_settings_changed)
        dialog.finished.connect(_on_dialog_finished)
        dialog.set_settings(self._user_manager.settings(), mindspace_settings, initial_section)
        dialog.show()
        dialog.raise_()
        dialog.activateWindow()

    def _on_show_settings_dialog_ai_backends(self) -> None:
        """Show the unified settings dialog opened to the AI Backends section."""
        self._on_show_settings_dialog(SECTION_AI_BACKENDS)

    def _on_show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        tab = self._tab_manager.get_current_tab()
        if tab is not None:
            tab.show_conversation_settings_dialog()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle global key events."""
        if event.key() == Qt.Key.Key_Escape:
            tab = self._tab_manager.get_current_tab()
            if tab is not None and tab.handle_esc_key():
                event.accept()
                return

        super().keyPressEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """Begin an edge-resize drag when the cursor is in the resize zone."""
        if not self._use_custom_title_bar or self.isMaximized():
            super().mousePressEvent(event)
            return

        if event.button() == Qt.MouseButton.LeftButton:
            direction = self._resize_direction_at(event.pos())
            if direction != (0, 0):
                self._resize_drag_active = True
                self._resize_direction = direction
                self._resize_start_pos = event.globalPosition().toPoint()
                self._resize_start_geometry = self.geometry()
                event.accept()
                return

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        """Update cursor shape and perform edge-resize dragging."""
        if not self._use_custom_title_bar:
            super().mouseMoveEvent(event)
            return

        if self._resize_drag_active and self._resize_start_pos is not None:
            delta = event.globalPosition().toPoint() - self._resize_start_pos
            geo = self._resize_start_geometry
            dx, dy = self._resize_direction
            new_x = geo.x()
            new_y = geo.y()
            new_w = geo.width()
            new_h = geo.height()
            min_w = self.minimumWidth()
            min_h = self.minimumHeight()

            if dx < 0:
                new_w = max(min_w, geo.width() - delta.x())
                new_x = geo.right() - new_w + 1

            elif dx > 0:
                new_w = max(min_w, geo.width() + delta.x())

            if dy < 0:
                new_h = max(min_h, geo.height() - delta.y())
                new_y = geo.bottom() - new_h + 1

            elif dy > 0:
                new_h = max(min_h, geo.height() + delta.y())

            self.setGeometry(new_x, new_y, new_w, new_h)
            event.accept()
            return

        if not self.isMaximized():
            self._update_resize_cursor(event.pos())

        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        """End an edge-resize drag."""
        if self._resize_drag_active and event.button() == Qt.MouseButton.LeftButton:
            self._resize_drag_active = False
            self._resize_start_pos = None
            event.accept()
            return

        super().mouseReleaseEvent(event)

    def leaveEvent(self, event: QEvent) -> None:
        """Clear any resize cursor when the mouse leaves the window."""
        if self._use_custom_title_bar:
            self.unsetCursor()

        super().leaveEvent(event)

    def _resize_direction_at(self, pos: QPoint) -> tuple[int, int]:
        """
        Return a (dx, dy) direction tuple for the resize zone at pos.

        Each component is -1 (left/top edge), 0 (no resize on that axis), or
        +1 (right/bottom edge).  Returns (0, 0) when pos is not in any resize zone.

        Args:
            pos: Mouse position in window-local coordinates.
        """
        zone = self._RESIZE_ZONE
        w = self.width()
        h = self.height()
        x = pos.x()
        y = pos.y()
        dx = -1 if x < zone else (1 if x >= w - zone else 0)
        dy = -1 if y < zone else (1 if y >= h - zone else 0)
        return (dx, dy)

    def _update_resize_cursor(self, pos: QPoint) -> None:
        """Set the cursor shape appropriate for the resize zone at pos."""
        dx, dy = self._resize_direction_at(pos)
        if (dx, dy) == (0, 0):
            self.unsetCursor()
            return

        child = self.childAt(pos)
        if child is not None and child is not self:
            self.unsetCursor()
            return

        if dx != 0 and dy != 0:
            if (dx < 0 and dy < 0) or (dx > 0 and dy > 0):
                self.setCursor(Qt.CursorShape.SizeFDiagCursor)

            else:
                self.setCursor(Qt.CursorShape.SizeBDiagCursor)

        elif dx != 0:
            self.setCursor(Qt.CursorShape.SizeHorCursor)

        else:
            self.setCursor(Qt.CursorShape.SizeVerCursor)

    def _build_zoom_levels(self) -> None:
        """Build the list of valid zoom factors from the current base font size.

        Zoom levels are chosen so that base_font_size * zoom is always an integer
        number of points.  Steps are ~10% of the current size (minimum 1pt), built
        symmetrically downward and upward from the base, between 0.5x and 2.0x.
        """
        base = self._style_manager.base_font_size()
        base_pt = round(base)
        min_pt = max(1, round(base * 0.5))
        max_pt = round(base * 2.0)

        levels: list[float] = []

        pt = base_pt
        while pt >= min_pt:
            if pt / base >= 0.5:
                levels.append(pt / base)

            step = max(1, round(pt * 0.1))
            pt -= step

        levels.reverse()

        pt = base_pt + max(1, round(base_pt * 0.1))
        while pt <= max_pt:
            levels.append(pt / base)
            step = max(1, round(pt * 0.1))
            pt += step

        self._zoom_levels = levels
        self._zoom_base_index = levels.index(base_pt / base)

    def _handle_zoom(self, factor: float) -> None:
        """Handle zoom in/out requests."""
        current_zoom = self._style_manager.zoom_factor()
        if current_zoom in self._zoom_levels:
            index = self._zoom_levels.index(current_zoom)

        else:
            index = self._zoom_base_index

        if factor > 1.0:
            index = min(len(self._zoom_levels) - 1, index + 1)

        else:
            index = max(0, index - 1)

        self._set_zoom(self._zoom_levels[index])

    def _set_zoom(self, zoom_level: float) -> None:
        """Set zoom level for the application."""
        self._style_manager.set_zoom(zoom_level)

    def closeEvent(self, event: QEvent) -> None:
        """Handle application close request."""
        self._save_mindspace_state()
        if not self._close_all_tabs():
            event.ignore()
            return

        event.accept()
