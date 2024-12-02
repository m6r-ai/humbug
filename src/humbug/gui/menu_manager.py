"""Menu implementation for Humbug application."""

from PySide6.QtWidgets import (QMainWindow, QMenuBar, QMenu, QDialog,
                             QVBoxLayout, QLabel, QPushButton)
from PySide6.QtGui import QAction, QKeySequence
from PySide6.QtCore import Qt, QSize
import platform

from humbug import format_version

class AboutDialog(QDialog):
    """About dialog for Humbug application."""

    def __init__(self, parent=None):
        """Initialize the About dialog."""
        super().__init__(parent)
        self.setWindowTitle("About Humbug")
        self.setFixedSize(QSize(400, 200))
        self.setup_ui()

    def setup_ui(self):
        """Set up the About dialog UI."""
        layout = QVBoxLayout()

        # Title with version
        title_label = QLabel(f"Humbug v{format_version()}")
        title_label.setAlignment(Qt.AlignCenter)
        title_label.setStyleSheet("font-size: 18px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)

        # Description
        desc_label = QLabel(
            "Humbug is a GUI-based application that allows users to "
            "interact with AI backends through a simple chat interface."
        )
        desc_label.setWordWrap(True)
        desc_label.setAlignment(Qt.AlignCenter)
        desc_label.setStyleSheet("margin: 10px;")
        layout.addWidget(desc_label)

        # Close button
        close_button = QPushButton("Close")
        close_button.clicked.connect(self.accept)
        close_button.setStyleSheet("margin: 10px;")
        layout.addWidget(close_button)

        layout.addStretch()
        self.setLayout(layout)


class MenuManager:
    """Manages menu creation and handling for the main window."""

    def __init__(self, main_window: QMainWindow):
        """Initialize menu manager with main window reference."""
        self.window = main_window
        self.menu_bar = QMenuBar()
        self.setup_menus()

    def setup_menus(self):
        """Set up all application menus."""
        self.setup_humbug_menu()
        self.setup_edit_menu()
        self.window.setMenuBar(self.menu_bar)

    def setup_humbug_menu(self):
        """Set up the Humbug menu."""
        humbug_menu = self.menu_bar.addMenu("Humbug")

        # About action
        about_action = QAction("About Humbug", self.window)
        about_action.triggered.connect(self.show_about_dialog)
        humbug_menu.addAction(about_action)

        # Separator
        humbug_menu.addSeparator()

        # Quit action
        quit_action = QAction("Quit Humbug", self.window)
        quit_action.triggered.connect(self.window.close)
        if platform.system() == "Darwin":  # macOS
            quit_action.setShortcut(QKeySequence("Cmd+Q"))
        else:  # Windows/Linux
            quit_action.setShortcut(QKeySequence("Ctrl+Q"))
        humbug_menu.addAction(quit_action)

    def setup_edit_menu(self):
        """Set up the Edit menu."""
        edit_menu = self.menu_bar.addMenu("Edit")

        # Submit action
        self.submit_action = QAction("Submit", self.window)
        if platform.system() == "Darwin":  # macOS
            self.submit_action.setShortcut(QKeySequence("Cmd+J"))
        else:  # Windows/Linux
            self.submit_action.setShortcut(QKeySequence("Ctrl+J"))
        self.submit_action.triggered.connect(self.window.submit_message)
        edit_menu.addAction(self.submit_action)

        edit_menu.addSeparator()

        # Undo action
        self.undo_action = QAction("Undo", self.window)
        if platform.system() == "Darwin":  # macOS
            self.undo_action.setShortcut(QKeySequence("Cmd+Z"))
        else:  # Windows/Linux
            self.undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self.undo_action.triggered.connect(lambda: self.window.input_area.undo())
        edit_menu.addAction(self.undo_action)

        # Redo action
        self.redo_action = QAction("Redo", self.window)
        if platform.system() == "Darwin":  # macOS
            self.redo_action.setShortcut(QKeySequence("Cmd+Shift+Z"))
        else:  # Windows/Linux
            self.redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self.redo_action.triggered.connect(lambda: self.window.input_area.redo())
        edit_menu.addAction(self.redo_action)

        edit_menu.addSeparator()

        # Cut action
        self.cut_action = QAction("Cut", self.window)
        if platform.system() == "Darwin":  # macOS
            self.cut_action.setShortcut(QKeySequence("Cmd+X"))
        else:  # Windows/Linux
            self.cut_action.setShortcut(QKeySequence("Ctrl+X"))
        self.cut_action.triggered.connect(lambda: self.window.input_area.cut())
        edit_menu.addAction(self.cut_action)

        # Copy action
        self.copy_action = QAction("Copy", self.window)
        if platform.system() == "Darwin":  # macOS
            self.copy_action.setShortcut(QKeySequence("Cmd+C"))
        else:  # Windows/Linux
            self.copy_action.setShortcut(QKeySequence("Ctrl+C"))
        self.copy_action.triggered.connect(lambda: self.window.input_area.copy())
        edit_menu.addAction(self.copy_action)

        # Paste action
        self.paste_action = QAction("Paste", self.window)
        if platform.system() == "Darwin":  # macOS
            self.paste_action.setShortcut(QKeySequence("Cmd+V"))
        else:  # Windows/Linux
            self.paste_action.setShortcut(QKeySequence("Ctrl+V"))
        self.paste_action.triggered.connect(lambda: self.window.input_area.paste())
        edit_menu.addAction(self.paste_action)

    def show_about_dialog(self):
        """Show the About dialog."""
        dialog = AboutDialog(self.window)
        dialog.exec()

    def update_menu_states(self):
        """Update enabled/disabled state of menu items."""
        # Get text editor state
        has_selection = self.window.input_area.textCursor().hasSelection()
        has_text = bool(self.window.input_area.toPlainText())
        can_undo = self.window.input_area.document().isUndoAvailable()
        can_redo = self.window.input_area.document().isRedoAvailable()

        # Update Edit menu actions
        self.submit_action.setEnabled(has_text)
        self.undo_action.setEnabled(can_undo)
        self.redo_action.setEnabled(can_redo)
        self.cut_action.setEnabled(has_selection)
        self.copy_action.setEnabled(has_selection)
        # Paste is always enabled as clipboard state is not easily accessible
