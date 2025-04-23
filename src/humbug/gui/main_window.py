"""Main window implementation for Humbug application."""

import asyncio
import json
import logging
import os
from typing import cast, Dict, List

from PySide6.QtWidgets import (
    QMainWindow, QWidget, QVBoxLayout, QMenuBar, QFileDialog,
    QSplitter, QLabel, QApplication, QDialog, QMenu, QStatusBar
)
from PySide6.QtCore import Qt, QTimer, QEvent
from PySide6.QtGui import QKeyEvent, QAction, QKeySequence, QActionGroup

from humbug.gui.about_dialog import AboutDialog
from humbug.gui.color_role import ColorRole
from humbug.gui.column_manager import ColumnManager
from humbug.gui.commands.conversation_command import ConversationCommand
from humbug.gui.commands.edit_command import EditCommand
from humbug.gui.commands.help_command import HelpCommand
from humbug.gui.commands.m6rc_command import M6rcCommand
from humbug.gui.commands.terminal_command import TerminalCommand
from humbug.gui.message_box import MessageBox, MessageBoxType
from humbug.gui.mindspace.mindspace_folders_dialog import MindspaceFoldersDialog
from humbug.gui.mindspace.mindspace_settings_dialog import MindspaceSettingsDialog
from humbug.gui.mindspace.mindspace_file_tree import MindspaceFileTree
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager, ColorMode
from humbug.gui.tab.conversation.conversation_error import ConversationError
from humbug.gui.user_settings_dialog import UserSettingsDialog
from humbug.language.language_manager import LanguageManager
from humbug.metaphor import (
    MetaphorParser, MetaphorParserError, MetaphorFormatVisitor, MetaphorRootNode,
    format_errors, format_preamble
)
from humbug.mindspace.mindspace_error import MindspaceError, MindspaceExistsError
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_settings import MindspaceSettings
from humbug.mindspace.system.system_command_registry import SystemCommandRegistry
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.user.user_manager import UserManager, UserError
from humbug.user.user_settings import UserSettings


class MainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self) -> None:
        """Initialize the main window."""
        super().__init__()

        self._logger = logging.getLogger("MainWindow")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._handle_language_changed)
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

        # View menu actions - Theme menu will be created in _handle_language_changed
        self._theme_menu: QMenu | None = None
        self._theme_actions: Dict[ColorMode, QAction] = {}

        self._zoom_in_action = QAction(strings.zoom_in, self)
        self._zoom_in_action.setShortcut(QKeySequence("Ctrl+="))
        self._zoom_in_action.triggered.connect(lambda: self._handle_zoom(1.189027))

        self._zoom_out_action = QAction(strings.zoom_out, self)
        self._zoom_out_action.setShortcut(QKeySequence("Ctrl+-"))
        self._zoom_out_action.triggered.connect(lambda: self._handle_zoom(1/1.189027))

        self._reset_zoom_action = QAction(strings.reset_zoom, self)
        self._reset_zoom_action.setShortcut(QKeySequence("Ctrl+0"))
        self._reset_zoom_action.triggered.connect(lambda: self._set_zoom(1.0))

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

        self._toggle_bookmark_action = QAction(strings.bookmark_section, self)
        self._toggle_bookmark_action.setCheckable(True)
        self._toggle_bookmark_action.setShortcut(QKeySequence("Ctrl+B"))
        self._toggle_bookmark_action.triggered.connect(self._toggle_bookmark)

        self._next_bookmark_action = QAction(strings.next_bookmark, self)
        self._next_bookmark_action.setShortcut(QKeySequence("Ctrl+Shift+N"))
        self._next_bookmark_action.triggered.connect(self._navigate_next_bookmark)

        self._previous_bookmark_action = QAction(strings.previous_bookmark, self)
        self._previous_bookmark_action.setShortcut(QKeySequence("Ctrl+Shift+P"))
        self._previous_bookmark_action.triggered.connect(self._navigate_previous_bookmark)

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
        self._view_menu.addSeparator()
        self._view_menu.addAction(self._toggle_bookmark_action)
        self._view_menu.addAction(self._next_bookmark_action)
        self._view_menu.addAction(self._previous_bookmark_action)

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

        # Create and add file tree
        self._file_tree = MindspaceFileTree(self)
        self._file_tree.file_activated.connect(self._handle_file_activation)
        self._file_tree.file_deleted.connect(self._handle_file_deletion)
        self._file_tree.file_renamed.connect(self._handle_file_rename)
        self._splitter.addWidget(self._file_tree)

        # Create tab manager in splitter
        self._column_manager = ColumnManager(self)
        self._splitter.addWidget(self._column_manager)

        # Set initial file tree width
        self._splitter.setSizes([240, self.width() - 240])

        # Set the stretch factors: 0 for file tree (no stretch) and 1 for column manager (stretch to fill)
        self._splitter.setStretchFactor(0, 0)
        self._splitter.setStretchFactor(1, 1)

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)

        # Create a timer that fires every 50ms to keep our menu states correct
        self._menu_timer = QTimer()
        self._menu_timer.setInterval(50)
        self._menu_timer.timeout.connect(self._update_menu_state)
        self._menu_timer.start()

        # Create status bar with left and right widgets
        self._status_bar = QStatusBar()
        self._status_left = QWidget()
        self._status_right = QWidget()

        # Set up left widget (empty for now)
        self._status_bar.addWidget(self._status_left, 1)

        # Set up right widget for status messages
        self._status_right_layout = QVBoxLayout(self._status_right)
        self._status_right_layout.setContentsMargins(0, 0, 0, 0)
        self._status_message_label = QLabel()
        self._status_right_layout.addWidget(self._status_message_label)
        self._status_bar.addPermanentWidget(self._status_right)

        self.setStatusBar(self._status_bar)
        self._column_manager.status_message.connect(self._handle_status_message)

        self._handle_language_changed()

        self._user_manager = UserManager()
        user_settings = self._user_manager.settings()
        self._style_manager.set_user_font_size(user_settings.font_size)
        self._language_manager.set_language(user_settings.language)

        # Set theme from user settings
        self._style_manager.set_color_mode(user_settings.theme)
        self._update_theme_menu()

        self._mindspace_manager = MindspaceManager()

        # Initialize command registry and register commands
        self._command_registry = SystemCommandRegistry()

        # Create and register commands
        conversation_command = ConversationCommand(self._process_conversation_command)
        self._command_registry.register_command(conversation_command)

        edit_command = EditCommand(self._process_edit_command)
        self._command_registry.register_command(edit_command)

        m6rc_command = M6rcCommand(self._process_m6rc_command)
        self._command_registry.register_command(m6rc_command)

        terminal_command = TerminalCommand(self._process_terminal_command)
        self._command_registry.register_command(terminal_command)

        # Register help command last so it can see all other commands
        help_command = HelpCommand(self._command_registry)
        self._command_registry.register_command(help_command)

        QTimer.singleShot(0, self._restore_last_mindspace)

    def _update_menu_state(self) -> None:
        """Update enabled/disabled state of menu items."""
        # Update mindspace-specific actions
        has_mindspace = self._mindspace_manager.has_mindspace()
        self._close_mindspace_action.setEnabled(has_mindspace)
        self._new_conv_action.setEnabled(has_mindspace)
        self._new_metaphor_conv_action.setEnabled(has_mindspace)
        self._new_file_action.setEnabled(has_mindspace)
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
        self._toggle_bookmark_action.setEnabled(column_manager.can_toggle_bookmark())
        self._next_bookmark_action.setEnabled(column_manager.can_navigate_next_bookmark())
        self._previous_bookmark_action.setEnabled(column_manager.can_navigate_previous_bookmark())

    def _handle_language_changed(self) -> None:
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
        self._toggle_bookmark_action.setText(strings.bookmark_section)
        self._next_bookmark_action.setText(strings.next_bookmark)
        self._previous_bookmark_action.setText(strings.previous_bookmark)

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

        self._handle_style_changed()

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
        light_action.triggered.connect(lambda: self._handle_theme_change(ColorMode.LIGHT))
        theme_action_group.addAction(light_action)
        theme_menu.addAction(light_action)
        self._theme_actions[ColorMode.LIGHT] = light_action

        # Add Dark theme action
        dark_action = QAction(strings.theme_dark, self)
        dark_action.setCheckable(True)
        dark_action.setChecked(self._style_manager.color_mode() == ColorMode.DARK)
        dark_action.triggered.connect(lambda: self._handle_theme_change(ColorMode.DARK))
        theme_action_group.addAction(dark_action)
        theme_menu.addAction(dark_action)
        self._theme_actions[ColorMode.DARK] = dark_action

        return theme_menu

    def _update_theme_menu(self) -> None:
        """Update the theme menu to reflect the current selected theme."""
        # Set the checked state for the appropriate theme action
        for theme, action in self._theme_actions.items():
            action.setChecked(theme == self._style_manager.color_mode())

    def _handle_theme_change(self, theme: ColorMode) -> None:
        """
        Handle theme change requests.

        Args:
            theme: The new theme to apply
        """
        self._style_manager.set_color_mode(theme)

    def _handle_status_message(self, message: StatusMessage) -> None:
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
                        self._file_tree.set_mindspace(mindspace_path)
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
        dir_path = QFileDialog.getExistingDirectory(
            self, strings.file_dialog_new_mindspace
        )
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
        dir_path = QFileDialog.getExistingDirectory(self, strings.file_dialog_open_mindspace)
        self._menu_timer.start()
        if not dir_path:
            return

        self._open_mindspace_path(dir_path)

    def _open_mindspace_path(self, path: str) -> None:
        # If we're switching mindspaces, save the current one first
        if self._mindspace_manager.has_mindspace():
            self._save_mindspace_state()
            self._close_all_tabs()
            self._mindspace_manager.close_mindspace()

        # Open the new mindspace
        try:
            self._mindspace_manager.open_mindspace(path)
            self._file_tree.set_mindspace(path)

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
        self._close_all_tabs()
        self._file_tree.set_mindspace("")
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

    def _close_all_tabs(self) -> None:
        self._column_manager.close_all_tabs()

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

    def _handle_file_activation(self, path: str) -> None:
        """Handle file activation from the file tree."""
        # Are we opening a conversation or a file?
        ext = os.path.splitext(path)[1].lower()
        if ext == ".conv":
            self._open_conversation_path(path)
            return

        self._open_file_path(path)

    def _handle_file_deletion(self, path: str) -> None:
        """Handle deletion of a file by closing any open tab.

        Args:
            path: Path of file being deleted
        """
        self._column_manager.close_deleted_file(path)

    def _handle_file_rename(self, old_path: str, new_path: str) -> None:
        """Handle renaming of files.

        Args:
            old_path: Original path of renamed file
            new_path: New path after renaming
        """
        self._column_manager.handle_file_rename(old_path, new_path)

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
        self._open_file_path(file_path)

    def _open_file_path(self, path: str) -> None:
        """Open file in editor tab."""
        try:
            self._column_manager.open_file(path)

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
        self._column_manager.save_file()

    def _save_file_as(self) -> None:
        """Save the current file with a new name."""
        self._column_manager.save_file_as()

    def _show_system_shell(self) -> None:
        """Show the system tab."""
        self._column_manager.show_system()

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

    def _handle_style_changed(self) -> None:
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor()
        base_font_size = style_manager.base_font_size()

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
                border-color: {style_manager.get_color_str(ColorRole.MENU_HOVER)};
                border-width: 1px;
                border-style: solid;
                border-radius: 4px;
            }}
            QMenu::item {{
                margin: 3px 5px;
                padding: 4px 4px 4px 4px;
            }}
            QMenu::item:disabled {{
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
            QMenu::item:selected {{
                background-color: {style_manager.get_color_str(ColorRole.MENU_HOVER)}
            }}
        """)

        # Update status bar font
        status_font = self.font()
        status_font.setPointSizeF(base_font_size * zoom_factor)
        self._status_bar.setFont(status_font)
        self._status_message_label.setFont(status_font)

        self._status_bar.setStyleSheet(f"""
            QStatusBar {{
                background-color: {self._style_manager.get_color_str(ColorRole.STATUS_BAR_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: {2 * zoom_factor}px;
                border-top: 1px solid {self._style_manager.get_color_str(ColorRole.SPLITTER)};
            }}
            QStatusBar::item {{
                border: 0;
                padding: 0;
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                padding: 0;
            }}
        """)

        self._splitter.setStyleSheet(f"""
            QSplitter::handle {{
                background-color: {style_manager.get_color_str(ColorRole.SPLITTER)};
                margin: 0;
                width: 1px;
            }}
        """)

    def _new_conversation(self) -> str | None:
        """Create new conversation tab."""
        if not self._mindspace_manager.has_mindspace():
            return None

        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")
            return self._column_manager.new_conversation(
                self._mindspace_manager.mindspace_path()
            )

        except MindspaceError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.mindspace_error_title,
                strings.error_opening_conversation.format(str(e))
            )
            return None

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

        metaphor_parser = MetaphorParser()
        try:
            syntax_tree = MetaphorRootNode()
            metaphor_parser.parse_file(syntax_tree, file_path, [search_path], search_path, [file_path])
            formatter = MetaphorFormatVisitor()
            prompt = format_preamble() + formatter.format(syntax_tree)

        except MetaphorParserError as e:
            self._column_manager.show_system()
            strings = self._language_manager.strings()
            error = f"{strings.metaphor_error_title}\n```\n{format_errors(e.errors)}\n```"
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                error
            )
            return

        conversation_id = self._new_conversation()
        if conversation_id is None:
            return

        # Get the tab and set input text
        conversation_tab = self._column_manager.find_conversation_tab_by_id(conversation_id)
        if conversation_tab is None:
            return

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
        self._open_conversation_path(file_path)

    def _open_conversation_path(self, path: str) -> None:
        """Open an existing conversation file."""
        try:
            self._column_manager.open_conversation(path)

        except ConversationError as e:
            self._logger.error("Error opening conversation: %s: %s", path, str(e))
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_opening_conversation.format(path, str(e))
            )

    async def _fork_conversation_async(self) -> None:
        """Async helper for forking."""
        try:
            await self._column_manager.fork_conversation()

        except ConversationError as e:
            strings = self._language_manager.strings()
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.conversation_error_title,
                strings.error_forking_conversation.format(str(e))
            )

    def _fork_conversation(self) -> None:
        """Create a new conversation tab with the history of the current conversation."""
        # Create task to fork conversation
        asyncio.create_task(self._fork_conversation_async())

    def _close_tab(self) -> None:
        """Close the current tab."""
        self._column_manager.close_tab()

    def _submit_message(self) -> None:
        """Handle message submission."""
        self._column_manager.submit_message()

    def _navigate_next_message(self) -> None:
        """Navigate to the next message in conversation."""
        self._column_manager.navigate_next_message()

    def _navigate_previous_message(self) -> None:
        """Navigate to the previous message in conversation."""
        self._column_manager.navigate_previous_message()

    def _toggle_bookmark(self) -> None:
        """Handle toggling a bookmark."""
        self._column_manager.toggle_bookmark()

    def _navigate_next_bookmark(self) -> None:
        """Move to the next bookmark."""
        self._column_manager.navigate_next_bookmark()

    def _navigate_previous_bookmark(self) -> None:
        """Move to the previous bookmark."""
        self._column_manager.navigate_previous_bookmark()

    def _show_user_settings_dialog(self) -> None:
        """Show the user settings dialog."""
        dialog = UserSettingsDialog(self)
        dialog.set_settings(self._user_manager.settings())

        def handle_settings_changed(new_settings: UserSettings) -> None:
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

        dialog.settings_changed.connect(handle_settings_changed)
        dialog.exec()

    def _show_mindspace_settings_dialog(self) -> None:
        """Show the mindspace settings dialog."""
        if not self._mindspace_manager.has_mindspace():
            return

        settings = cast(MindspaceSettings, self._mindspace_manager.settings())
        dialog = MindspaceSettingsDialog(self)
        dialog.set_settings(settings)

        def handle_settings_changed(new_settings: MindspaceSettings) -> None:
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

        dialog.settings_changed.connect(handle_settings_changed)
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
        if not self._column_manager.can_close_all_tabs():
            event.ignore()
            return

        self._save_mindspace_state()
        self._close_all_tabs()
        event.accept()

    def _process_conversation_command(self, model: str | None, temperature: float | None) -> bool:
        """Process the conversation command."""
        self._column_manager.protect_system_tab(True)
        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")
            self._column_manager.new_conversation(
                self._mindspace_manager.mindspace_path(), model, temperature
            )

        except MindspaceError as e:
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR, f"Failed to create conversation: {str(e)}"
            )
            return False

        finally:
            self._column_manager.protect_system_tab(False)

        return True

    def _process_edit_command(self, file_path: str) -> bool:
        """Process the edit command."""
        self._column_manager.protect_system_tab(True)
        self._column_manager.open_file(file_path)
        self._column_manager.protect_system_tab(False)
        return True

    def _process_m6rc_command(self, file_path: str, args: List[str], model: str | None, temperature: float | None) -> bool:
        """Process the m6rc command."""
        search_path = self._mindspace_manager.mindspace_path()

        metaphor_parser = MetaphorParser()
        try:
            syntax_tree = MetaphorRootNode()
            metaphor_parser.parse_file(syntax_tree, file_path, [search_path], search_path, args)
            formatter = MetaphorFormatVisitor()
            prompt = format_preamble() + formatter.format(syntax_tree)

        except FileNotFoundError:
            error = f"File not found: {file_path}"
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR, error
            )
            return False

        except MetaphorParserError as e:
            strings = self._language_manager.strings()
            error = f"{strings.metaphor_error_title}\n{format_errors(e.errors)}"
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR, error
            )
            return False

        self._column_manager.protect_system_tab(True)
        conversation_id: str | None = None
        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")
            conversation_id = self._column_manager.new_conversation(
                self._mindspace_manager.mindspace_path(), model, temperature
            )

        except MindspaceError as e:
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR, f"Failed to create conversation: {str(e)}"
            )
            return False

        self._column_manager.protect_system_tab(False)
        if conversation_id is None:
            return False

        conversation_tab = self._column_manager.find_conversation_tab_by_id(conversation_id)
        if conversation_tab is None:
            return False

        conversation_tab.set_input_text(prompt)
        conversation_tab.submit()
        return True

    def _process_terminal_command(self) -> bool:
        """Process the terminal command."""
        self._column_manager.protect_system_tab(True)
        self._column_manager.new_terminal()
        self._column_manager.protect_system_tab(False)
        return True
