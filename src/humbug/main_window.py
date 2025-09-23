"""Main window implementation for Humbug application."""

import json
import logging
import os
from pathlib import Path
from typing import cast, Dict, Tuple

from PySide6.QtWidgets import (
    QMainWindow, QWidget, QVBoxLayout, QMenuBar, QFileDialog,
    QSplitter, QLabel, QApplication, QDialog, QMenu, QStatusBar
)
from PySide6.QtCore import Qt, QTimer, QEvent
from PySide6.QtGui import QKeyEvent, QAction, QKeySequence, QActionGroup

from metaphor import (
    MetaphorASTBuilder, MetaphorASTBuilderError, MetaphorFormatVisitor, MetaphorASTRootNode,
    format_errors, format_preamble
)

from ai_tool import AIToolManager
from ai_tool.aifpl.aifpl_ai_tool import AIFPLAITool
from ai_tool.clock.clock_ai_tool import ClockAITool
from ai_tool.filesystem.filesystem_ai_tool import FileSystemAITool

from humbug.about_dialog import AboutDialog
from humbug.color_role import ColorRole
from humbug.delegate_ai_tool import DelegateAITool
from humbug.exception_notifier import get_exception_notifier
from humbug.message_box import MessageBox, MessageBoxType
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_error import MindspaceError, MindspaceExistsError
from humbug.mindspace.mindspace_folders_dialog import MindspaceFoldersDialog
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.mindspace.mindspace_settings_dialog import MindspaceSettingsDialog
from humbug.mindspace.mindspace_view import MindspaceView
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.style_manager import StyleManager, ColorMode
from humbug.status_message import StatusMessage
from humbug.system_ai_tool import SystemAITool
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.conversation.conversation_error import ConversationError
from humbug.tabs.conversation.conversation_tab import ConversationTab
from humbug.tabs.shell.commands.shell_command_clear import ShellCommandClear
from humbug.tabs.shell.commands.shell_command_conversation import ShellCommandConversation
from humbug.tabs.shell.commands.shell_command_edit import ShellCommandEdit
from humbug.tabs.shell.commands.shell_command_help import ShellCommandHelp
from humbug.tabs.shell.commands.shell_command_log import ShellCommandLog
from humbug.tabs.shell.commands.shell_command_m6rc import ShellCommandM6rc
from humbug.tabs.shell.commands.shell_command_terminal import ShellCommandTerminal
from humbug.tabs.shell.commands.shell_command_wiki import ShellCommandWiki
from humbug.tabs.shell.shell_command_registry import ShellCommandRegistry
from humbug.tabs.tab_base import TabBase
from humbug.tabs.wiki.wiki_error import WikiError
from humbug.tabs.wiki.wiki_tab import WikiTab
from humbug.user.user_manager import UserManager, UserError
from humbug.user.user_settings import UserSettings
from humbug.user.user_settings_dialog import UserSettingsDialog


class MainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self) -> None:
        """Initialize the main window."""
        super().__init__()

        self._logger = logging.getLogger("MainWindow")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        strings = self._language_manager.strings()

        # Humbug menu actions
        self._about_action = QAction(strings.about_humbug, self)
        self._about_action.setMenuRole(QAction.MenuRole.AboutRole)
        self._about_action.triggered.connect(self._show_about_dialog)

        self._user_settings_action = QAction(strings.user_settings, self)
        self._user_settings_action.setMenuRole(QAction.MenuRole.PreferencesRole)
        self._user_settings_action.setShortcut(QKeySequence("Ctrl+,"))
        self._user_settings_action.triggered.connect(self._show_user_settings_dialog)

        self._quit_action = QAction(strings.quit_humbug, self)
        self._quit_action.setMenuRole(QAction.MenuRole.QuitRole)
        self._quit_action.setShortcut(QKeySequence("Ctrl+Q"))
        self._quit_action.triggered.connect(self.close)

        # File menu actions
        self._new_mindspace_action = QAction(strings.new_mindspace, self)
        self._new_mindspace_action.setShortcut(QKeySequence("Ctrl+Alt+N"))
        self._new_mindspace_action.triggered.connect(self._new_mindspace)

        self._new_conv_action = QAction(strings.new_conversation, self)
        self._new_conv_action.setShortcut(QKeySequence("Ctrl+Shift+N"))
        self._new_conv_action.triggered.connect(self._new_conversation)

        self._new_metaphor_conv_action = QAction(strings.new_metaphor_conversation, self)
        self._new_metaphor_conv_action.setShortcut(QKeySequence("Ctrl+Shift+M"))
        self._new_metaphor_conv_action.triggered.connect(self._new_metaphor_conversation)

        self._new_file_action = QAction(strings.new_file, self)
        self._new_file_action.setShortcut(QKeySequence.StandardKey.New)
        self._new_file_action.triggered.connect(self._new_file)

        self._new_terminal_action = QAction(strings.new_terminal, self)
        self._new_terminal_action.setShortcut(QKeySequence("Ctrl+Alt+T"))
        self._new_terminal_action.triggered.connect(self._new_terminal)

        self._open_mindspace_action = QAction(strings.open_mindspace, self)
        self._open_mindspace_action.setShortcut(QKeySequence("Ctrl+Alt+O"))
        self._open_mindspace_action.triggered.connect(self._open_mindspace)

        self._open_wiki_action = QAction(strings.open_wiki, self)
        self._open_wiki_action.setShortcut(QKeySequence("Ctrl+Shift+W"))
        self._open_wiki_action.triggered.connect(self._open_wiki)

        self._open_conv_action = QAction(strings.open_conversation, self)
        self._open_conv_action.setShortcut(QKeySequence("Ctrl+Shift+O"))
        self._open_conv_action.triggered.connect(self._open_conversation)

        self._open_file_action = QAction(strings.open_file, self)
        self._open_file_action.setShortcut(QKeySequence.StandardKey.Open)
        self._open_file_action.triggered.connect(self._open_file)

        self._fork_conv_action = QAction(strings.fork_conversation, self)
        self._fork_conv_action.setShortcut(QKeySequence("Ctrl+Shift+F"))
        self._fork_conv_action.triggered.connect(self._fork_conversation)

        self._save_action = QAction(strings.save, self)
        self._save_action.setShortcut(QKeySequence.StandardKey.Save)
        self._save_action.triggered.connect(self._save_file)

        self._save_as_action = QAction(strings.save_as, self)
        self._save_as_action.setShortcut(QKeySequence.StandardKey.SaveAs)
        self._save_as_action.triggered.connect(self._save_file_as)

        self._close_tab_action = QAction(strings.close_tab, self)
        self._close_tab_action.setShortcut(QKeySequence("Ctrl+W"))
        self._close_tab_action.triggered.connect(self._close_tab)

        self._close_mindspace_action = QAction(strings.close_mindspace, self)
        self._close_mindspace_action.setShortcut(QKeySequence("Ctrl+Alt+W"))
        self._close_mindspace_action.triggered.connect(self._close_mindspace)

        # Edit menu actions
        self._submit_message_action = QAction(strings.submit_message, self)
        self._submit_message_action.setShortcut(QKeySequence("Ctrl+J"))
        self._submit_message_action.triggered.connect(self._submit_message)

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

        self._mindspace_settings_action = QAction(strings.mindspace_settings, self)
        self._mindspace_settings_action.setShortcut(QKeySequence("Ctrl+Alt+,"))
        self._mindspace_settings_action.triggered.connect(self._show_mindspace_settings_dialog)

        self._conv_settings_action = QAction(strings.conversation_settings, self)
        self._conv_settings_action.setShortcut(QKeySequence("Ctrl+Shift+,"))
        self._conv_settings_action.triggered.connect(self._show_conversation_settings_dialog)

        # View menu actions - Theme menu will be created in _on_language_changed
        self._theme_menu: QMenu | None = None
        self._theme_actions: Dict[ColorMode, QAction] = {}

        self._zoom_in_action = QAction(strings.zoom_in, self)
        self._zoom_in_action.setShortcut(QKeySequence("Ctrl+="))
        self._zoom_in_action.triggered.connect(lambda: self._handle_zoom(1.0800597))

        self._zoom_out_action = QAction(strings.zoom_out, self)
        self._zoom_out_action.setShortcut(QKeySequence("Ctrl+-"))
        self._zoom_out_action.triggered.connect(lambda: self._handle_zoom(1/1.0800597))

        self._reset_zoom_action = QAction(strings.reset_zoom, self)
        self._reset_zoom_action.setShortcut(QKeySequence("Ctrl+0"))
        self._reset_zoom_action.triggered.connect(lambda: self._set_zoom(1.0))

        self._show_system_log_action = QAction(strings.show_system_log, self)
        self._show_system_log_action.setShortcut(QKeySequence("Ctrl+Shift+L"))
        self._show_system_log_action.triggered.connect(self._show_system_log)

        self._show_system_shell_action = QAction(strings.show_system_shell, self)
        self._show_system_shell_action.setShortcut(QKeySequence("Ctrl+Shift+Y"))
        self._show_system_shell_action.triggered.connect(self._show_system_shell)

        self._show_all_columns_action = QAction(strings.show_all_columns, self)
        self._show_all_columns_action.setShortcut(QKeySequence("Ctrl+\\"))
        self._show_all_columns_action.triggered.connect(self._show_all_columns)

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
        self._next_message_action.triggered.connect(self._navigate_next_message)

        self._previous_message_action = QAction(strings.previous_message, self)
        self._previous_message_action.setShortcut(QKeySequence("Alt+Up"))
        self._previous_message_action.triggered.connect(self._navigate_previous_message)

        self._menu_bar = QMenuBar(self)
        self.setMenuBar(self._menu_bar)

        # Humbug menu
        self._humbug_menu = self._menu_bar.addMenu(strings.humbug_menu)
        self._humbug_menu.addAction(self._about_action)
        self._humbug_menu.addSeparator()
        self._humbug_menu.addAction(self._user_settings_action)
        self._humbug_menu.addSeparator()
        self._humbug_menu.addAction(self._quit_action)

        # File menu
        self._file_menu = self._menu_bar.addMenu(strings.file_menu)
        self._file_menu.addAction(self._new_mindspace_action)
        self._file_menu.addAction(self._new_conv_action)
        self._file_menu.addAction(self._new_metaphor_conv_action)
        self._file_menu.addAction(self._new_file_action)
        self._file_menu.addAction(self._new_terminal_action)
        self._file_menu.addSeparator()
        self._file_menu.addAction(self._open_mindspace_action)
        self._file_menu.addAction(self._open_wiki_action)
        self._file_menu.addAction(self._open_conv_action)
        self._file_menu.addAction(self._open_file_action)
        self._file_menu.addSeparator()
        self._file_menu.addAction(self._fork_conv_action)
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
        self._edit_menu.addSeparator()
        self._edit_menu.addAction(self._mindspace_settings_action)
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

        # Main widget and layout
        main_widget = QWidget()
        self.setCentralWidget(main_widget)
        layout = QVBoxLayout(main_widget)
        layout.setSpacing(0)
        layout.setContentsMargins(0, 0, 0, 0)

        # Create splitter
        self._splitter = QSplitter(Qt.Orientation.Horizontal)
        layout.addWidget(self._splitter)

        # Create and add mindspace view
        self._mindspace_view = MindspaceView(self)
        self._mindspace_view.file_clicked.connect(self._on_mindspace_view_file_clicked)
        self._mindspace_view.file_deleted.connect(self._on_mindspace_view_file_deleted)
        self._mindspace_view.file_renamed.connect(self._on_mindspace_view_file_renamed)
        self._mindspace_view.file_edited.connect(self._on_mindspace_view_file_edited)
        self._mindspace_view.file_opened_in_wiki.connect(self._on_mindspace_view_file_opened_in_wiki)
        self._splitter.addWidget(self._mindspace_view)

        # Create tab manager in splitter
        self._column_manager = ColumnManager(self)
        self._column_manager.tab_changed.connect(self._on_column_manager_tab_changed)
        self._column_manager.fork_requested.connect(self._on_column_manager_fork_requested)
        self._column_manager.fork_from_index_requested.connect(self._on_column_manager_fork_from_index_requested)
        self._column_manager.open_wiki_link_requested.connect(self._on_column_manager_open_wiki_link_requested)
        self._column_manager.edit_file_requested.connect(self._on_column_manager_edit_file_requested)
        self._splitter.addWidget(self._column_manager)

        # Set initial mindspace view width
        self._splitter.setSizes([300, self.width() - 300])

        # Set the stretch factors: 0 for mindspace view (no stretch) and 1 for column manager (stretch to fill)
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
        self._column_manager.status_message.connect(self._on_column_manager_status_message)

        # Connect to exception notifier for canary functionality
        self._canary_active = False
        exception_notifier = get_exception_notifier()
        exception_notifier.exception_occurred.connect(self._on_exception_occurred)

        self._on_language_changed()

        self._user_manager = UserManager()
        user_settings = self._user_manager.settings()
        self._style_manager.set_user_font_size(user_settings.font_size)
        self._language_manager.set_language(user_settings.language)

        # Set theme from user settings
        self._style_manager.set_color_mode(user_settings.theme)
        self._update_theme_menu()

        self._mindspace_manager = MindspaceManager()

        # Initialize command registry and register commands
        self._command_registry = ShellCommandRegistry()
        self._command_registry.register_command(ShellCommandClear(self._column_manager))
        self._command_registry.register_command(ShellCommandConversation(self._column_manager))
        self._command_registry.register_command(ShellCommandEdit(self._column_manager))
        self._command_registry.register_command(ShellCommandLog(self._column_manager))
        self._command_registry.register_command(ShellCommandM6rc(self._column_manager))
        self._command_registry.register_command(ShellCommandTerminal(self._column_manager))
        self._command_registry.register_command(ShellCommandWiki(self._column_manager))

        # Register help command last so it can see all other commands
        self._command_registry.register_command(ShellCommandHelp(self._command_registry))

        self._ai_tool_manager = AIToolManager()
        self._ai_tool_manager.register_tool(
            AIFPLAITool(), "AIFPL: evaluates expressions using AI Functional Programming Language syntax"
        )
        self._ai_tool_manager.register_tool(ClockAITool(), "Clock: gets the current time and date")
        self._ai_tool_manager.register_tool(
            DelegateAITool(self._column_manager), "Delegate: delegates tasks to specialized AI instances"
        )
        self._ai_tool_manager.register_tool(
            FileSystemAITool(self._resolve_mindspace_path), "FileSystem: handles file operations in the current mindspace"
        )
        self._ai_tool_manager.register_tool(SystemAITool(self._column_manager), "System: implements UI automations within Humbug")

        QTimer.singleShot(0, self._restore_last_mindspace)

    def changeEvent(self, event: QEvent) -> None:
        """Handle change events."""
        # If our window state changes then update the column manager's style
        if event.type() == QEvent.Type.WindowStateChange:
            self._column_manager.apply_style()

        return super().changeEvent(event)

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
        self._new_metaphor_conv_action.setEnabled(has_mindspace)
        self._new_file_action.setEnabled(has_mindspace)
        self._open_wiki_action.setEnabled(has_mindspace)
        self._open_conv_action.setEnabled(has_mindspace)
        self._open_file_action.setEnabled(has_mindspace)
        self._new_terminal_action.setEnabled(has_mindspace)
        self._mindspace_settings_action.setEnabled(has_mindspace)

        # Update tab-specific actions
        column_manager = self._column_manager
        self._fork_conv_action.setEnabled(column_manager.can_fork_conversation())
        self._save_action.setEnabled(column_manager.can_save_file())
        self._save_as_action.setEnabled(column_manager.can_save_file_as())
        self._close_tab_action.setEnabled(column_manager.can_close_tab())
        self._undo_action.setEnabled(column_manager.can_undo())
        self._redo_action.setEnabled(column_manager.can_redo())
        self._cut_action.setEnabled(column_manager.can_cut())
        self._copy_action.setEnabled(column_manager.can_copy())
        self._paste_action.setEnabled(column_manager.can_paste())
        self._find_action.setEnabled(column_manager.can_show_find())
        self._submit_message_action.setEnabled(column_manager.can_submit_message())
        self._conv_settings_action.setEnabled(column_manager.can_show_conversation_settings_dialog())

        # Update view actions
        current_zoom = self._style_manager.zoom_factor()
        left_to_right = self._language_manager.left_to_right()
        self._zoom_in_action.setEnabled(current_zoom < 2.0)
        self._zoom_out_action.setEnabled(current_zoom > 0.5)
        self._show_system_log_action.setEnabled(has_mindspace)
        self._show_system_shell_action.setEnabled(has_mindspace)
        self._show_all_columns_action.setEnabled(column_manager.can_show_all_columns())
        self._split_column_left_action.setEnabled(column_manager.can_split_column())
        self._split_column_right_action.setEnabled(column_manager.can_split_column())
        self._merge_column_left_action.setEnabled(column_manager.can_merge_column(left_to_right))
        self._merge_column_right_action.setEnabled(column_manager.can_merge_column(not left_to_right))
        self._swap_column_left_action.setEnabled(column_manager.can_swap_column(left_to_right))
        self._swap_column_right_action.setEnabled(column_manager.can_swap_column(not left_to_right))
        self._next_message_action.setEnabled(column_manager.can_navigate_next_message())
        self._previous_message_action.setEnabled(column_manager.can_navigate_previous_message())

    def _on_language_changed(self) -> None:
        """Update UI text when language changes."""
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
        self._user_settings_action.setText(strings.user_settings)
        self._quit_action.setText(strings.quit_humbug)
        self._new_mindspace_action.setText(strings.new_mindspace)
        self._new_conv_action.setText(strings.new_conversation)
        self._new_metaphor_conv_action.setText(strings.new_metaphor_conversation)
        self._new_file_action.setText(strings.new_file)
        self._open_mindspace_action.setText(strings.open_mindspace)
        self._open_wiki_action.setText(strings.open_wiki)
        self._open_conv_action.setText(strings.open_conversation)
        self._open_file_action.setText(strings.open_file)
        self._fork_conv_action.setText(strings.fork_conversation)
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
        self._show_all_columns_action.setText(strings.show_all_columns)
        self._split_column_left_action.setText(strings.split_column_left)
        self._split_column_right_action.setText(strings.split_column_right)
        self._merge_column_left_action.setText(strings.merge_column_left)
        self._merge_column_right_action.setText(strings.merge_column_right)
        self._swap_column_left_action.setText(strings.swap_column_left)
        self._swap_column_right_action.setText(strings.swap_column_right)
        self._next_message_action.setText(strings.next_message)
        self._previous_message_action.setText(strings.previous_message)

        # Our logic for left and right reverses for right-to-left languages
        left_to_right = self._language_manager.left_to_right()
        self._split_column_left_action.triggered.disconnect()
        self._split_column_left_action.triggered.connect(lambda: self._split_column(left_to_right))
        self._split_column_right_action.triggered.disconnect()
        self._split_column_right_action.triggered.connect(lambda: self._split_column(not left_to_right))
        self._merge_column_left_action.triggered.disconnect()
        self._merge_column_left_action.triggered.connect(lambda: self._merge_column(left_to_right))
        self._merge_column_right_action.triggered.disconnect()
        self._merge_column_right_action.triggered.connect(lambda: self._merge_column(not left_to_right))
        self._swap_column_left_action.triggered.disconnect()
        self._swap_column_left_action.triggered.connect(lambda: self._swap_column(left_to_right))
        self._swap_column_right_action.triggered.disconnect()
        self._swap_column_right_action.triggered.connect(lambda: self._swap_column(not left_to_right))

        self._on_style_changed()

    def _create_theme_menu(self) -> QMenu:
        """
        Create a display theme submenu with available themes.

        Returns:
            QMenu: The theme submenu
        """
        strings = self._language_manager.strings()

        # Create the theme menu
        theme_menu = QMenu(strings.display_theme, self)

        # Create an action group so only one theme can be selected at a time
        theme_action_group = QActionGroup(self)
        theme_action_group.setExclusive(True)

        # Create the theme actions dictionary to store references
        self._theme_actions = {}

        # Add Light theme action
        light_action = QAction(strings.theme_light, self)
        light_action.setCheckable(True)
        light_action.setChecked(self._style_manager.color_mode() == ColorMode.LIGHT)
        light_action.triggered.connect(lambda: self._set_color_mode(ColorMode.LIGHT))
        theme_action_group.addAction(light_action)
        theme_menu.addAction(light_action)
        self._theme_actions[ColorMode.LIGHT] = light_action

        # Add Dark theme action
        dark_action = QAction(strings.theme_dark, self)
        dark_action.setCheckable(True)
        dark_action.setChecked(self._style_manager.color_mode() == ColorMode.DARK)
        dark_action.triggered.connect(lambda: self._set_color_mode(ColorMode.DARK))
        theme_action_group.addAction(dark_action)
        theme_menu.addAction(dark_action)
        self._theme_actions[ColorMode.DARK] = dark_action

        return theme_menu

    def _update_theme_menu(self) -> None:
        """Update the theme menu to reflect the current selected theme."""
        # Set the checked state for the appropriate theme action
        for theme, action in self._theme_actions.items():
            action.setChecked(theme == self._style_manager.color_mode())

    def _set_color_mode(self, theme: ColorMode) -> None:
        """
        Set the color mode (theme) for the application.

        Args:
            theme: The new theme to apply
        """
        self._style_manager.set_color_mode(theme)

    def _map_tab_to_mindspace_view(self, tab: TabBase) -> MindspaceViewType:
        """Map a tab to its corresponding mindspace view type."""
        if isinstance(tab, ConversationTab):
            return MindspaceViewType.CONVERSATIONS

        if isinstance(tab, WikiTab):
            return MindspaceViewType.WIKI

        return MindspaceViewType.FILES

    def _on_column_manager_tab_changed(self) -> None:
        """Handle tab change events."""
        current_tab = self._column_manager.get_current_tab()
        if current_tab is None:
            return

        path = current_tab.path()
        if path is None:
            return

        view_type = self._map_tab_to_mindspace_view(current_tab)
        self._mindspace_view.reveal_and_select_file(view_type, path)

    def _on_column_manager_status_message(self, message: StatusMessage) -> None:
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
                        self._mindspace_view.set_mindspace(mindspace_path)
                        self._restore_mindspace_state()

                    except MindspaceError as e:
                        self._logger.error("Failed to restore mindspace: %s", str(e))
                        # Don't show error dialog on startup, just log it

        except (FileNotFoundError, json.JSONDecodeError):
            pass

    def _new_mindspace(self) -> None:
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

    def _open_mindspace(self) -> None:
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
            self._mindspace_view.set_mindspace(path)

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

    def _close_mindspace(self) -> None:
        if not self._mindspace_manager.has_mindspace():
            self._logger.error("No mindspace active, cannot close")
            return

        self._save_mindspace_state()
        if not self._close_all_tabs():
            return

        self._mindspace_view.set_mindspace("")
        self._mindspace_manager.close_mindspace()

    def _save_mindspace_state(self) -> None:
        """Save current mindspace state."""
        if not self._mindspace_manager.has_mindspace():
            self._logger.error("No mindspace active, cannot save")
            return

        try:
            mindspace_state = self._column_manager.save_state()
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
            self._column_manager.restore_state(saved_state)

        except MindspaceError as e:
            self._logger.error("Failed to restore mindspace state: %s", str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_restoring_mindspace.format(str(e))
            )

    def _close_all_tabs(self) -> bool:
        return self._column_manager.close_all_tabs()

    def _undo(self) -> None:
        self._column_manager.undo()

    def _redo(self) -> None:
        self._column_manager.redo()

    def _cut(self) -> None:
        self._column_manager.cut()

    def _copy(self) -> None:
        self._column_manager.copy()

    def _paste(self) -> None:
        self._column_manager.paste()

    def _find(self) -> None:
        self._column_manager.show_find()

    def _show_about_dialog(self) -> None:
        """Show the About dialog."""
        dialog = AboutDialog(self)
        dialog.exec()

    def _new_terminal(self) -> None:
        """Create a new terminal tab."""
        if not self._mindspace_manager.has_mindspace():
            return

        self._column_manager.new_terminal()

    def _new_file(self) -> None:
        """Create a new empty editor tab."""
        if not self._mindspace_manager.has_mindspace():
            return

        self._column_manager.new_file()

    def _on_mindspace_view_file_opened_in_wiki(self, path: str, ephemeral: bool) -> None:
        """Handle click of a wiki link from the mindspace view."""
        wiki_tab = self._column_manager.open_wiki_page(path, ephemeral)
        if wiki_tab is None:
            return

        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User opened wiki: '{path}'\ntab ID: {wiki_tab.tab_id()}"
        )

    def _on_mindspace_view_file_clicked(self, source: MindspaceViewType, path: str, ephemeral: bool) -> None:
        """Handle click of a file from the mindspace view."""
        self._column_manager.open_file_by_mindspace_view_type(source, path, ephemeral)

    def _on_mindspace_view_file_deleted(self, path: str) -> None:
        """Handle deletion of a file by closing any open tab.

        Args:
            path: Path of file being deleted
        """
        self._column_manager.close_deleted_file(path)

    def _on_mindspace_view_file_renamed(self, old_path: str, new_path: str) -> None:
        """Handle renaming of files.

        Args:
            old_path: Original path of renamed file
            new_path: New path after renaming
        """
        self._column_manager.handle_file_rename(old_path, new_path)

    def _on_mindspace_view_file_edited(self, path: str, ephemeral: bool) -> None:
        """Handle file edited event from the mindspace view."""
        self._open_file_path(path, ephemeral)

    def _open_file(self) -> None:
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
            editor_tab = self._column_manager.open_file(path, ephemeral)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"User opened editor for file: '{path}'\nTab ID: {editor_tab.tab_id()}"
            )

        except OSError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_opening_file_title,
                strings.could_not_open.format(path, str(e))
            )

    def _save_file(self) -> None:
        """Save the current file."""
        path = self._column_manager.save_file()
        self._mindspace_view.reveal_and_select_file(MindspaceViewType.FILES, path)

    def _save_file_as(self) -> None:
        """Save the current file with a new name."""
        path = self._column_manager.save_file_as()
        self._mindspace_view.reveal_and_select_file(MindspaceViewType.FILES, path)

    def _open_wiki(self) -> None:
        """Open the wiki page in a new tab."""
        self._column_manager.open_wiki_page(self._mindspace_manager.get_absolute_path("."), False)

    def _show_system_log(self) -> None:
        """Show the log tab."""
        self._column_manager.show_system_log()

    def _show_system_shell(self) -> None:
        """Show the shell tab."""
        self._column_manager.show_system_shell()

    def _show_all_columns(self) -> None:
        """Show all columns equally."""
        self._column_manager.show_all_columns()

    def _split_column(self, split_left: bool) -> None:
        """Split the current column."""
        self._column_manager.split_column(split_left)

    def _merge_column(self, merge_left: bool) -> None:
        """Merge the current column."""
        self._column_manager.merge_column(merge_left)

    def _swap_column(self, swap_left: bool) -> None:
        """Swap the current column."""
        self._column_manager.swap_column(swap_left)

    def _on_style_changed(self) -> None:
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor()
        base_font_size = style_manager.base_font_size()

        # Determine status bar background color based on canary state
        status_bg_color = (
            style_manager.get_color_str(ColorRole.CANARY_BACKGROUND)
            if self._canary_active
            else style_manager.get_color_str(ColorRole.STATUS_BAR_BACKGROUND)
        )

        self.setStyleSheet(f"""
            QMainWindow {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}

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

            QMenu {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-color: {style_manager.get_color_str(ColorRole.MENU_BORDER)};
                border-width: 1px;
                border-style: solid;
                border-radius: 8px;
                margin: 0px;
            }}
            QMenu::item {{
                margin: 2px;
                padding: 4px 8px 4px 8px;
                border-radius: 4px;
            }}
            QMenu::item:disabled {{
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QMenu::item:selected {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_HOVER)}
            }}

            QStatusBar {{
                background-color: {status_bg_color};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: {2 * zoom_factor}px;
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.SPLITTER)};
            }}
            QStatusBar::item {{
                border: 0;
                padding: 0;
            }}

            QStatusBar QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 0;
            }}

            QSplitter::handle {{
                background-color: {style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                width: 1px;
            }}
        """)

        # Update status bar font
        status_font = self.font()
        status_font.setPointSizeF(base_font_size * zoom_factor)
        self._status_bar.setFont(status_font)
        self._status_message_label.setFont(status_font)

        # Apply styles to the mindspace view and column manager
        self._mindspace_view.apply_style()
        self._column_manager.apply_style()

    def _new_conversation(self) -> ConversationTab | None:
        """Create new conversation tab."""
        if not self._mindspace_manager.has_mindspace():
            return None

        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")
            conversation_tab = self._column_manager.new_conversation()

        except MindspaceError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_opening_conversation.format(str(e))
            )
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.ERROR,
                f"User failed to create new conversation: {str(e)}"
            )
            return None

        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User created new conversation\ntab ID: {conversation_tab.tab_id()}"
        )
        return conversation_tab

    def _get_canonical_mindspace_path(self, path: str) -> str | None:
        """Get the canonical path of the current mindspace."""
        if not self._mindspace_manager.has_mindspace():
            return None

        # Get the absolute path of the current mindspace
        return self._mindspace_manager.get_relative_path(path)

    def _resolve_mindspace_path(self, path: str) -> Tuple[Path, str]:
        # Check if our path starts with a separator.  If it does we'll assume it's for the root of the mindspace.
        if path.startswith(os.sep):
            path = path[1:]

        # Convert to absolute path via mindspace manager
        abs_path = self._mindspace_manager.get_absolute_path(path)
        resolved_path = Path(abs_path).resolve()

        # Verify the resolved path is still within mindspace
        relative_path = self._mindspace_manager.get_mindspace_relative_path(str(resolved_path))
        if relative_path is None:
            raise ValueError(f"Path is outside mindspace boundaries: {path}")

        return resolved_path, relative_path

    def _new_metaphor_conversation(self) -> None:
        """Create new conversation from Metaphor file."""
        # Show file dialog
        self._menu_timer.stop()
        strings = self._language_manager.strings()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            strings.file_dialog_open_metaphor,
            self._mindspace_manager.file_dialog_directory(),
            f"{strings.file_filter_metaphor};;{strings.file_filter_all}"
        )
        self._menu_timer.start()

        if not file_path:
            return

        self._mindspace_manager.update_file_dialog_directory(file_path)
        search_path = self._mindspace_manager.mindspace_path()

        metaphor_ast_builder = MetaphorASTBuilder(self._get_canonical_mindspace_path)
        try:
            syntax_tree = MetaphorASTRootNode()
            metaphor_ast_builder.build_ast_from_file(syntax_tree, file_path, [search_path], search_path, [file_path])
            formatter = MetaphorFormatVisitor()
            prompt = format_preamble() + formatter.format(syntax_tree)

        except MetaphorASTBuilderError as e:
            self._column_manager.show_system_shell()
            error = f"Metaphor compiler error prevented new Metaphor conversation:\n\n{format_errors(e.errors)}"
            self._mindspace_manager.add_interaction(MindspaceLogLevel.WARN, error)
            self._column_manager.show_system_log()
            return

        conversation_tab = self._new_conversation()
        if conversation_tab is None:
            return

        # Set input text
        conversation_tab.set_input_text(prompt)

    def _open_conversation(self) -> None:
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
        self._open_conversation_path(file_path, False)

    def _open_conversation_path(self, path: str, ephemeral: bool) -> ConversationTab | None:
        """Open an existing conversation file."""
        try:
            tab = self._column_manager.open_conversation(path, ephemeral)
            return tab

        except ConversationError as e:
            self._logger.error("Error opening conversation: %s: %s", path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_opening_conversation.format(path, str(e))
            )
            return None

    def _fork_conversation(self) -> None:
        """Create a new conversation tab with the history of the current conversation."""
        # Create task to fork conversation
        try:
            self._column_manager.fork_conversation_from_index(None)

        except ConversationError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_forking_conversation.format(str(e))
            )

    def _on_column_manager_fork_requested(self) -> None:
        """Handle fork conversation requests."""
        self._fork_conversation()

    def _on_column_manager_fork_from_index_requested(self, index: int) -> None:
        """Handle fork conversation requests from a specific index."""
        try:
            self._column_manager.fork_conversation_from_index(index)

        except ConversationError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_forking_conversation.format(str(e))
            )

    def _on_column_manager_open_wiki_link_requested(self, path: str) -> None:
        """Handle requests to open a wiki page."""
        try:
            self._column_manager.open_wiki_page(path, True)

        except WikiError as e:
            self._logger.info("Error opening wiki page: %s: %s", path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.wiki_error_title,
                strings.could_not_open.format(path, str(e))
            )

    def _on_column_manager_edit_file_requested(self, path: str) -> None:
        """Handle requests to open a file in the editor."""
        self._open_file_path(path, False)

    def _close_tab(self) -> None:
        """Close the current tab."""
        tab = self._column_manager.get_current_tab()
        if tab is None:
            self._logger.error("No current tab to close")
            return

        self._column_manager.close_tab()
        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"User closed tab\nTab ID: {tab.tab_id()}"
        )

    def _submit_message(self) -> None:
        """Handle message submission."""
        self._column_manager.submit_message()

    def _navigate_next_message(self) -> None:
        """Navigate to the next message in conversation."""
        self._column_manager.navigate_next_message()

    def _navigate_previous_message(self) -> None:
        """Navigate to the previous message in conversation."""
        self._column_manager.navigate_previous_message()

    def _show_user_settings_dialog(self) -> None:
        """Show the user settings dialog."""
        dialog = UserSettingsDialog(self)
        dialog.set_settings(self._user_manager.settings())

        def _on_settings_changed(new_settings: UserSettings) -> None:
            try:
                self._user_manager.update_settings(new_settings)
                self._style_manager.set_user_font_size(new_settings.font_size)
                self._language_manager.set_language(new_settings.language)

                # Update theme from settings if it changed
                new_theme = new_settings.theme
                if new_theme != self._style_manager.color_mode():
                    self._style_manager.set_color_mode(new_theme)
                    self._update_theme_menu()

                self._logger.info("User settings saved successfully")

            except UserError as e:
                self._logger.error("Failed to save user settings: %s", str(e))
                strings = self._language_manager.strings()
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.settings_error_title,
                    strings.error_saving_user_settings.format(str(e))
                )

        dialog.settings_changed.connect(_on_settings_changed)
        dialog.exec()

    def _show_mindspace_settings_dialog(self) -> None:
        """Show the mindspace settings dialog."""
        if not self._mindspace_manager.has_mindspace():
            return

        settings = cast(MindspaceSettings, self._mindspace_manager.settings())
        dialog = MindspaceSettingsDialog(self)
        dialog.set_settings(settings)

        def _on_settings_changed(new_settings: MindspaceSettings) -> None:
            try:
                self._mindspace_manager.update_settings(new_settings)

            except OSError as e:
                self._logger.error("Failed to save mindspace settings: %s", str(e))
                strings = self._language_manager.strings()
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    strings.settings_error_title,
                    strings.error_saving_mindspace_settings.format(str(e))
                )

        dialog.settings_changed.connect(_on_settings_changed)
        dialog.exec()

    def _show_conversation_settings_dialog(self) -> None:
        """Show the conversation settings dialog."""
        self._column_manager.show_conversation_settings_dialog()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle global key events."""
        if event.key() == Qt.Key.Key_Escape:
            if self._column_manager.handle_esc_key():
                return

        super().keyPressEvent(event)

    def _handle_zoom(self, factor: float) -> None:
        """Handle zoom in/out requests."""
        new_zoom = self._style_manager.zoom_factor() * factor
        self._set_zoom(new_zoom)

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
