"""Main window implementation for Humbug application."""

import asyncio
import json
import logging
import os
from typing import Dict, Optional

from m6rclib import (
    MetaphorParser, MetaphorParserError, format_ast, format_errors
)

from PySide6.QtWidgets import (
    QMainWindow, QDialog, QWidget, QVBoxLayout, QMenuBar, QFileDialog, QSplitter, QLabel
)
from PySide6.QtCore import Qt, QTimer, Slot
from PySide6.QtGui import QKeyEvent, QAction, QKeySequence
from PySide6.QtWidgets import QStatusBar

from humbug.ai.ai_backend import AIBackend
from humbug.gui.about_dialog import AboutDialog
from humbug.gui.conversation_error import ConversationError
from humbug.gui.conversation_settings_dialog import ConversationSettingsDialog
from humbug.gui.conversation_tab import ConversationTab
from humbug.gui.color_role import ColorRole
from humbug.gui.editor_tab import EditorTab
from humbug.gui.message_box import MessageBox, MessageBoxType
from humbug.gui.status_message import StatusMessage
from humbug.gui.style_manager import StyleManager, ColorMode
from humbug.gui.tab_manager import TabManager
from humbug.gui.workspace_settings_dialog import WorkspaceSettingsDialog
from humbug.gui.workspace_file_tree import WorkspaceFileTree
from humbug.workspace.workspace_manager import WorkspaceManager
from humbug.workspace.workspace_error import WorkspaceError, WorkspaceExistsError


class MainWindow(QMainWindow):
    """Main window for the Humbug application."""

    def __init__(self, ai_backends: Dict[str, AIBackend]):
        """Initialize the main window."""
        super().__init__()
        self._ai_backends = ai_backends
        self._logger = logging.getLogger("MainWindow")
        self._dark_mode = True

        # Humbug menu actions
        self._about_action = QAction("About Humbug", self)
        self._about_action.triggered.connect(self._show_about_dialog)

        self._quit_action = QAction("Quit Humbug", self)
        self._quit_action.setShortcut(QKeySequence("Ctrl+Q"))
        self._quit_action.triggered.connect(self.close)

        # File menu actions
        self._new_workspace_action = QAction("New Workspace", self)
        self._new_workspace_action.setShortcut(QKeySequence("Ctrl+Alt+N"))
        self._new_workspace_action.triggered.connect(self._new_workspace)

        self._new_conv_action = QAction("New Conversation", self)
        self._new_conv_action.setShortcut(QKeySequence("Ctrl+Shift+N"))
        self._new_conv_action.triggered.connect(self._new_conversation)

        self._new_metaphor_conv_action = QAction("New Metaphor Conversation...", self)
        self._new_metaphor_conv_action.setShortcut(QKeySequence("Ctrl+Shift+M"))
        self._new_metaphor_conv_action.triggered.connect(self._new_metaphor_conversation)

        self._new_file_action = QAction("New File", self)
        self._new_file_action.setShortcut(QKeySequence.New)
        self._new_file_action.triggered.connect(self._new_file)

        self._open_workspace_action = QAction("Open Workspace", self)
        self._open_workspace_action.setShortcut(QKeySequence("Ctrl+Alt+O"))
        self._open_workspace_action.triggered.connect(self._open_workspace)

        self._open_conv_action = QAction("Open Conversation...", self)
        self._open_conv_action.setShortcut(QKeySequence("Ctrl+Shift+O"))
        self._open_conv_action.triggered.connect(self._open_conversation)

        self._open_file_action = QAction("Open File...", self)
        self._open_file_action.setShortcut(QKeySequence.Open)
        self._open_file_action.triggered.connect(self._open_file)

        self._fork_conv_action = QAction("Fork Conversation", self)
        self._fork_conv_action.setShortcut(QKeySequence("Ctrl+Shift+F"))
        self._fork_conv_action.triggered.connect(self._fork_conversation)

        self._save_action = QAction("Save", self)
        self._save_action.setShortcut(QKeySequence.Save)
        self._save_action.triggered.connect(self._save_file)

        self._save_as_action = QAction("Save As...", self)
        self._save_as_action.setShortcut(QKeySequence.SaveAs)
        self._save_as_action.triggered.connect(self._save_file_as)

        self._close_tab_action = QAction("Close Tab", self)
        self._close_tab_action.setShortcut(QKeySequence("Ctrl+W"))
        self._close_tab_action.triggered.connect(self._close_current_tab)

        self._close_workspace_action = QAction("Close Workspace", self)
        self._close_workspace_action.setShortcut(QKeySequence("Ctrl+Alt+W"))
        self._close_workspace_action.triggered.connect(self._close_workspace)

        # Edit menu actions
        self._submit_action = QAction("Submit", self)
        self._submit_action.setShortcut(QKeySequence("Ctrl+J"))
        self._submit_action.triggered.connect(self._submit_message)

        self._undo_action = QAction("Undo", self)
        self._undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self._undo_action.triggered.connect(self._undo)

        self._redo_action = QAction("Redo", self)
        self._redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self._redo_action.triggered.connect(self._redo)

        self._cut_action = QAction("Cut", self)
        self._cut_action.setShortcut(QKeySequence("Ctrl+X"))
        self._cut_action.triggered.connect(self._cut)

        self._copy_action = QAction("Copy", self)
        self._copy_action.setShortcut(QKeySequence("Ctrl+C"))
        self._copy_action.triggered.connect(self._copy)

        self._paste_action = QAction("Paste", self)
        self._paste_action.setShortcut(QKeySequence("Ctrl+V"))
        self._paste_action.triggered.connect(self._paste)

        self._workspace_settings_action = QAction("Workspace Settings", self)
        self._workspace_settings_action.setShortcut(QKeySequence("Ctrl+Alt+,"))
        self._workspace_settings_action.triggered.connect(self._show_workspace_settings_dialog)

        self._conv_settings_action = QAction("Conversation Settings", self)
        self._conv_settings_action.setShortcut(QKeySequence("Ctrl+,"))
        self._conv_settings_action.triggered.connect(self._show_conversation_settings_dialog)

        # View menu actions
        self._dark_mode_action = QAction("&Dark Mode", self)
        self._dark_mode_action.setCheckable(True)
        self._dark_mode_action.setChecked(True)
        self._dark_mode_action.triggered.connect(self._handle_dark_mode)

        self._zoom_in_action = QAction("Zoom In", self)
        self._zoom_in_action.setShortcut(QKeySequence("Ctrl+="))
        self._zoom_in_action.triggered.connect(lambda: self._handle_zoom(1.189027))

        self._zoom_out_action = QAction("Zoom Out", self)
        self._zoom_out_action.setShortcut(QKeySequence("Ctrl+-"))
        self._zoom_out_action.triggered.connect(lambda: self._handle_zoom(1/1.189027))

        self._reset_zoom_action = QAction("Reset Zoom", self)
        self._reset_zoom_action.setShortcut(QKeySequence("Ctrl+0"))
        self._reset_zoom_action.triggered.connect(lambda: self._set_zoom(1.0))

        self._split_view_action = QAction("Split View", self)
        self._split_view_action.setCheckable(True)
        self._split_view_action.setChecked(False)
        self._split_view_action.triggered.connect(self._handle_column_mode)

        self._menu_bar = QMenuBar(self)
        self.setMenuBar(self._menu_bar)

        # Humbug menu
        humbug_menu = self._menu_bar.addMenu("&Humbug")
        humbug_menu.addAction(self._about_action)
        humbug_menu.addSeparator()
        humbug_menu.addAction(self._quit_action)

        # File menu
        file_menu = self._menu_bar.addMenu("&File")
        file_menu.addAction(self._new_workspace_action)
        file_menu.addAction(self._new_conv_action)
        file_menu.addAction(self._new_metaphor_conv_action)
        file_menu.addAction(self._new_file_action)
        file_menu.addSeparator()
        file_menu.addAction(self._open_workspace_action)
        file_menu.addAction(self._open_conv_action)
        file_menu.addAction(self._open_file_action)
        file_menu.addSeparator()
        file_menu.addAction(self._fork_conv_action)
        file_menu.addSeparator()
        file_menu.addAction(self._save_action)
        file_menu.addAction(self._save_as_action)
        file_menu.addSeparator()
        file_menu.addAction(self._close_workspace_action)
        file_menu.addAction(self._close_tab_action)

        # Edit menu
        edit_menu = self._menu_bar.addMenu("&Edit")
        edit_menu.addAction(self._submit_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self._undo_action)
        edit_menu.addAction(self._redo_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self._cut_action)
        edit_menu.addAction(self._copy_action)
        edit_menu.addAction(self._paste_action)
        edit_menu.addSeparator()
        edit_menu.addAction(self._workspace_settings_action)
        edit_menu.addAction(self._conv_settings_action)

        # View menu
        view_menu = self._menu_bar.addMenu("&View")
        view_menu.addAction(self._dark_mode_action)
        view_menu.addSeparator()
        view_menu.addAction(self._zoom_in_action)
        view_menu.addAction(self._zoom_out_action)
        view_menu.addAction(self._reset_zoom_action)
        view_menu.addSeparator()
        view_menu.addAction(self._split_view_action)

        self.setWindowTitle("Humbug")
        self.setMinimumSize(800, 600)

        # Main widget and layout
        main_widget = QWidget()
        self.setCentralWidget(main_widget)
        layout = QVBoxLayout(main_widget)
        layout.setSpacing(0)
        layout.setContentsMargins(0, 0, 0, 0)

        # Create splitter
        self._splitter = QSplitter(Qt.Horizontal)
        layout.addWidget(self._splitter)

        # Create and add file tree
        self._file_tree = WorkspaceFileTree(self)
        self._file_tree.file_activated.connect(self._handle_file_activation)
        self._splitter.addWidget(self._file_tree)

        # Create tab manager in splitter
        self._tab_manager = TabManager(ai_backends, self)
        self._splitter.addWidget(self._tab_manager)

        # Set initial file tree width
        self._splitter.setSizes([240, self.width() - 240])

        # Set the stretch factors: 0 for file tree (no stretch) and 1 for tab manager (stretch to fill)
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
        self._tab_manager.current_tab_changed.connect(self._handle_tab_changed)
        self._tab_manager.column_state_changed.connect(self._handle_column_state_changed)

        self._handle_style_changed()

        self._workspace_manager = WorkspaceManager()
        self._restore_last_workspace()

    def _handle_column_state_changed(self, has_two_columns: bool):
        """Handle column state changes from tab manager."""
        self._split_view_action.setChecked(has_two_columns)

        # Save workspace state when column configuration changes
        self._save_workspace_state()

    def _handle_column_mode(self, checked: bool) -> None:
        """Handle column mode changes.

        Args:
            checked: True if two columns should be enabled, False for single column
        """
        if checked:
            self._tab_manager.switch_to_double_column()
        else:
            self._tab_manager.switch_to_single_column()

        # Update menu state
        self._split_view_action.setChecked(checked)

        # Save state
        self._save_workspace_state()

    def _handle_tab_changed(self) -> None:
        """Handle tab change by connecting status message signal."""
        current_tab = self._tab_manager.get_current_tab()
        if current_tab:
            # Disconnect any existing connections to avoid duplicates
            try:
                current_tab.status_message.disconnect()
            except RuntimeError:
                pass  # No existing connections

            current_tab.status_message.connect(self._handle_status_message)
            current_tab.update_status()

    def _handle_status_message(self, message: StatusMessage) -> None:
        """Update status bar with new message."""
        self._status_message_label.setText(message.text)
        if message.timeout:
            QTimer.singleShot(message.timeout, self._status_message_label.clear)

    def _restore_last_workspace(self):
        """Restore last workspace on startup if available."""
        try:
            with open(os.path.expanduser("~/.humbug/workspace.json"), encoding='utf-8') as f:
                data = json.load(f)
                workspace_path = data.get("lastWorkspace")
                if workspace_path and os.path.exists(workspace_path):
                    try:
                        self._workspace_manager.open_workspace(workspace_path)
                        self._file_tree.set_workspace(workspace_path)
                        self._style_manager.set_workspace_font_size(self._workspace_manager.settings.font_size)
                        self._restore_workspace_state()
                    except WorkspaceError as e:
                        self._logger.error("Failed to restore workspace: %s", str(e))
                        # Don't show error dialog on startup, just log it
        except (FileNotFoundError, json.JSONDecodeError):
            pass

    def _new_workspace(self):
        self._menu_timer.stop()
        dir_path = QFileDialog.getExistingDirectory(
            self, "Create New Workspace"
        )
        self._menu_timer.start()
        if not dir_path:
            return

        try:
            self._workspace_manager.create_workspace(dir_path)
        except WorkspaceExistsError:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Workspace Error",
                "Workspace already exists in selected directory."
            )
            return
        except WorkspaceError as e:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Workspace Error",
                f"Failed to create workspace: {str(e)}"
            )
            return

        self._open_workspace_path(dir_path)

    def _open_workspace(self):
        """Open a new workspace."""
        self._menu_timer.stop()
        dir_path = QFileDialog.getExistingDirectory(self, "Open Workspace")
        self._menu_timer.start()
        if not dir_path:
            return

        self._open_workspace_path(dir_path)

    def _open_workspace_path(self, path: str) -> None:
        # If we're switching workspaces, save the current one first
        if self._workspace_manager.has_workspace:
            self._save_workspace_state()
            self._close_all_tabs()
            self._style_manager.set_workspace_font_size(None)
            self._workspace_manager.close_workspace()

        # Open the new workspace
        try:
            self._workspace_manager.open_workspace(path)
            self._file_tree.set_workspace(path)
            self._style_manager.set_workspace_font_size(self._workspace_manager.settings.font_size)
        except WorkspaceError as e:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Workspace Error",
                f"Failed to open workspace: {str(e)}"
            )
            return

        # Restore the state of the newly opened workspace
        self._restore_workspace_state()

    def _close_workspace(self):
        if not self._workspace_manager.has_workspace:
            self._logger.error("No workspace active, cannot close")
            return

        self._save_workspace_state()
        self._close_all_tabs()
        self._file_tree.set_workspace(None)
        self._style_manager.set_workspace_font_size(None)
        self._workspace_manager.close_workspace()

    def _save_workspace_state(self):
        """Save current workspace state."""
        if not self._workspace_manager.has_workspace:
            self._logger.error("No workspace active, cannot save")
            return

        try:
            workspace_state = self._tab_manager.save_state()
            self._workspace_manager.save_workspace_state(workspace_state)
        except WorkspaceError as e:
            self._logger.error("Failed to save workspace state: %s", str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Workspace Error",
                f"Failed to save workspace state: {str(e)}"
            )

    def _restore_workspace_state(self):
        """Restore previously open tabs from workspace state."""
        saved_state = self._workspace_manager.load_workspace_state()
        if not saved_state:
            self._logger.debug("No saved states found")
            return

        try:
            self._tab_manager.restore_state(saved_state)
        except WorkspaceError as e:
            self._logger.error("Failed to restore workspace state: %s", str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Workspace Error",
                f"Failed to restore workspace state: {str(e)}"
            )

    def _close_all_tabs(self):
        for tab in self._tab_manager.get_all_tabs():
            self._tab_manager.close_tab(tab.tab_id)

    def _undo(self):
        self._tab_manager.get_current_tab().undo()

    def _redo(self):
        self._tab_manager.get_current_tab().redo()

    def _cut(self):
        self._tab_manager.get_current_tab().cut()

    def _copy(self):
        self._tab_manager.get_current_tab().copy()

    def _paste(self):
        self._tab_manager.get_current_tab().paste()

    def _show_about_dialog(self):
        """Show the About dialog."""
        dialog = AboutDialog(self)
        dialog.exec()

    def _new_file(self):
        """Create a new empty editor tab."""
        if not self._workspace_manager.has_workspace:
            return

        self._tab_manager.new_file()

    def _handle_file_activation(self, path: str):
        """Handle file activation from the file tree."""
        # Are we opening a conversation or a file?
        ext = os.path.splitext(path)[1].lower()
        if ext == ".conv":
            self._open_conversation_path(path)
            return

        self._open_file_path(path)

    def _open_file(self):
        """Show open file dialog and create editor tab."""
        self._menu_timer.stop()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Open File",
            self._workspace_manager.workspace_path
        )
        self._menu_timer.start()

        if not file_path:
            return

        self._open_file_path(file_path)

    def _open_file_path(self, path: str) -> None:
        """Open file in editor tab."""
        try:
            self._tab_manager.open_file(path)
        except OSError as e:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Error Opening File",
                f"Could not open {path}: {str(e)}"
            )

    def _save_file(self):
        """Save the current file."""
        current_tab = self._tab_manager.get_current_tab()
        if isinstance(current_tab, EditorTab):
            current_tab.save()

    def _save_file_as(self):
        """Save the current file with a new name."""
        current_tab = self._tab_manager.get_current_tab()
        if isinstance(current_tab, EditorTab):
            current_tab.save_as()

    @Slot()
    def _update_menu_state(self):
        """Update enabled/disabled state of menu items."""
        current_tab = self._tab_manager.get_current_tab()

        has_workspace = self._workspace_manager.has_workspace

        # Update workspace-specific actions
        self._close_workspace_action.setEnabled(has_workspace)
        self._new_conv_action.setEnabled(has_workspace)
        self._new_metaphor_conv_action.setEnabled(has_workspace)
        self._new_file_action.setEnabled(has_workspace)
        self._open_conv_action.setEnabled(has_workspace)
        self._open_file_action.setEnabled(has_workspace)
        self._workspace_settings_action.setEnabled(has_workspace)

        # Disable all actions by default
        self._fork_conv_action.setEnabled(False)
        self._save_action.setEnabled(False)
        self._save_as_action.setEnabled(False)
        self._close_tab_action.setEnabled(False)
        self._undo_action.setEnabled(False)
        self._redo_action.setEnabled(False)
        self._cut_action.setEnabled(False)
        self._copy_action.setEnabled(False)
        self._paste_action.setEnabled(False)
        self._submit_action.setEnabled(False)
        self._conv_settings_action.setEnabled(False)

        if not current_tab:
            return

        # Enable common edit operations based on tab state
        self._save_action.setEnabled(current_tab.can_save())
        self._save_as_action.setEnabled(current_tab.can_save_as())
        self._close_tab_action.setEnabled(True)
        self._undo_action.setEnabled(current_tab.can_undo())
        self._redo_action.setEnabled(current_tab.can_redo())
        self._cut_action.setEnabled(current_tab.can_cut())
        self._copy_action.setEnabled(current_tab.can_copy())
        self._paste_action.setEnabled(current_tab.can_paste())
        self._submit_action.setEnabled(current_tab.can_submit())

        # Enable conversation-specific operations for conversation tabs
        if isinstance(current_tab, ConversationTab):
            self._fork_conv_action.setEnabled(True)
            self._conv_settings_action.setEnabled(True)

        # Update zoom actions
        current_zoom = self._style_manager.zoom_factor
        self._zoom_in_action.setEnabled(current_zoom < 2.0)
        self._zoom_out_action.setEnabled(current_zoom > 0.5)

    def _handle_style_changed(self) -> None:
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor
        base_font_size = style_manager.base_font_size

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

    def _new_conversation(self) -> Optional[str]:
        """Create new conversation tab."""
        if not self._workspace_manager.has_workspace:
            return None

        try:
            self._workspace_manager.ensure_workspace_dir("conversations")
            return self._tab_manager.new_conversation(
                self._workspace_manager.workspace_path
            )
        except WorkspaceError as e:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Workspace Error",
                f"Failed to create conversation: {str(e)}"
            )
            return None

    def _new_metaphor_conversation(self):
        """Create new conversation from Metaphor file."""
        if not self._workspace_manager.has_workspace:
            MessageBox.show_message(
                self,
                MessageBoxType.WARNING,
                "Workspace Required",
                "Please open a workspace before creating a Metaphor conversation."
            )
            return

        # Show file dialog
        self._menu_timer.stop()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Open Metaphor File",
            self._workspace_manager.workspace_path,
            "Metaphor Files (*.m6r);;All Files (*.*)"
        )
        self._menu_timer.start()

        if not file_path:
            return

        search_paths = [self._workspace_manager.workspace_path]

        metaphor_parser = MetaphorParser()
        try:
            syntax_tree = metaphor_parser.parse_file(file_path, search_paths)
            prompt = format_ast(syntax_tree)

            # Create conversation with prompt
            conversation_id = self._new_conversation()
            if conversation_id:
                # Get the tab and set input text
                conversation_tab = self._tab_manager.find_conversation_tab_by_id(conversation_id)
                if conversation_tab:
                    conversation_tab.set_input_text(prompt)
        except MetaphorParserError as e:
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Metaphor Processing Error",
                f"Failed to process Metaphor file:\n\n{format_errors(e.errors)}"
            )

    def _open_conversation(self):
        """Show open conversation dialog and create conversation tab."""
        self._menu_timer.stop()
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Open Conversation",
            self._workspace_manager.workspace_path,
            "Conversation Files (*.conv);;All Files (*.*)"
        )
        self._menu_timer.start()

        if not file_path:
            return

        self._open_conversation_path(file_path)

    def _open_conversation_path(self, path: str) -> None:
        """Open an existing conversation file."""
        try:
            self._tab_manager.open_conversation(path)
        except ConversationError as e:
            self._logger.error("Error opening conversation: %s: %s", path, str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                "Error Loading Conversation",
                f"Could not load {path}: {str(e)}"
            )

    def _fork_conversation(self):
        """Create a new conversation tab with the history of the current conversation."""
        current_tab = self._tab_manager.get_current_tab()
        if not isinstance(current_tab, ConversationTab):
            return

        async def fork_and_handle_errors():
            try:
                await self._tab_manager.fork_conversation(current_tab)
            except ConversationError as e:
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    "Error Forking Conversation",
                    f"Could not fork conversation: {str(e)}"
                )

        # Create task to fork conversation
        asyncio.create_task(fork_and_handle_errors())

    def _close_current_tab(self):
        """Close the current conversation tab."""
        conversation_tab = self._tab_manager.get_current_tab()
        if not conversation_tab:
            return

        self._tab_manager.close_tab(conversation_tab.tab_id)

    def _submit_message(self):
        """Handle message submission."""
        conversation_tab = self._tab_manager.get_current_tab()
        if not conversation_tab or not conversation_tab.can_submit():
            return

        conversation_tab.submit()

    def _show_workspace_settings_dialog(self):
        """Show the workspace settings dialog."""
        if not self._workspace_manager.has_workspace:
            return

        dialog = WorkspaceSettingsDialog(self)
        dialog.set_settings(self._workspace_manager.settings)

        def handle_settings_changed(new_settings):
            try:
                self._workspace_manager.update_settings(new_settings)
                self._style_manager.set_workspace_font_size(new_settings.font_size)
            except OSError as e:
                self._logger.error("Failed to save workspace settings: %s", str(e))
                MessageBox.show_message(
                    self,
                    MessageBoxType.CRITICAL,
                    "Settings Error",
                    f"Failed to save workspace settings: {str(e)}"
                )

        dialog.settings_changed.connect(handle_settings_changed)
        dialog.exec()

    def _show_conversation_settings_dialog(self):
        """Show the conversation settings dialog."""
        conversation_tab = self._tab_manager.get_current_tab()
        if not conversation_tab:
            return

        dialog = ConversationSettingsDialog(self, self._ai_backends)
        dialog.set_settings(conversation_tab.get_settings())

        if dialog.exec() == QDialog.Accepted:
            conversation_tab.update_conversation_settings(dialog.get_settings())

    def keyPressEvent(self, event: QKeyEvent):
        """Handle global key events."""
        if event.key() == Qt.Key_Escape:
            conversation_tab = self._tab_manager.get_current_tab()
            if conversation_tab and isinstance(conversation_tab, ConversationTab):
                conversation_tab.cancel_current_tasks()
            return

        super().keyPressEvent(event)

    def _handle_dark_mode(self, _):
        """Handle dark mode enable/disable requests."""
        self._dark_mode = not self._dark_mode
        self._dark_mode_action.setChecked(self._dark_mode)
        self._style_manager.set_color_mode(ColorMode.DARK if self._dark_mode else ColorMode.LIGHT)

    def _handle_zoom(self, factor: float):
        """Handle zoom in/out requests."""
        new_zoom = self._style_manager.zoom_factor * factor
        self._set_zoom(new_zoom)

    def _set_zoom(self, zoom_level: float):
        """Set zoom level for the application."""
        self._style_manager.set_zoom(zoom_level)

    def closeEvent(self, event):
        """Handle application close request."""
        # Check each tab in turn
        for tab in self._tab_manager.get_all_tabs():
            if tab.is_modified and not tab.can_close():
                event.ignore()
                return

        self._save_workspace_state()
        event.accept()
