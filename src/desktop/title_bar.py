"""Window control buttons widget for frameless windows on Linux and Windows."""

import sys

from PySide6.QtWidgets import QWidget, QHBoxLayout, QToolButton, QSizePolicy, QApplication
from PySide6.QtCore import Qt, QSize, QPoint, QEvent
from PySide6.QtGui import QMouseEvent, QIcon

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


class _WindowControlButton(QToolButton):
    """A square tool button used for window controls (minimise, maximise, close)."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the window control button."""
        super().__init__(parent)
        self.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.setCursor(Qt.CursorShape.ArrowCursor)
        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)


class WindowControlsWidget(QWidget):
    """
    Window control buttons (minimise, maximise/restore, close) for frameless windows.

    Intended to be set as the corner widget of a QMenuBar so the controls appear
    on the same row as the menu items. Used on Linux and Windows where the OS title
    bar has been removed via Qt.WindowType.FramelessWindowHint.
    """

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the window controls widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._is_maximised = False

        self.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)

        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(0, 2, 8, 0)
        self._layout.setSpacing(10)

        self._minimise_button = _WindowControlButton(self)
        self._minimise_button.clicked.connect(self._on_minimise)
        self._layout.addWidget(self._minimise_button)

        self._maximise_button = _WindowControlButton(self)
        self._maximise_button.clicked.connect(self._on_maximise_restore)
        self._layout.addWidget(self._maximise_button)

        self._close_button = _WindowControlButton(self)
        self._close_button.clicked.connect(self._on_close)
        self._layout.addWidget(self._close_button)

        self.apply_style()

    def apply_style(self) -> None:
        """Apply current style settings to the buttons."""
        style_manager = self._style_manager
        btn_size = 22

        is_rtl = self.layoutDirection() == Qt.LayoutDirection.RightToLeft
        left_margin = 8 if is_rtl else 0
        right_margin = 0 if is_rtl else 8
        self._layout.setContentsMargins(left_margin, 2, right_margin, 0)

        self._minimise_button.setFixedSize(btn_size, btn_size)
        self._maximise_button.setFixedSize(btn_size, btn_size)
        self._close_button.setFixedSize(btn_size, btn_size)

        icon_px = max(1, int(btn_size * 0.7))
        self._minimise_button.setIconSize(QSize(icon_px, icon_px))
        self._maximise_button.setIconSize(QSize(icon_px, icon_px))
        self._close_button.setIconSize(QSize(icon_px, icon_px))

        bg = style_manager.get_color_str(ColorRole.MENU_BACKGROUND)
        hover = style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        pressed = style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)
        close_hover = style_manager.get_color_str(ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER)

        shared_style = f"""
            QToolButton {{
                border: none;
                outline: none;
                padding: 0px;
                margin: 0px;
                background-color: {bg};
            }}
            QToolButton:hover {{
                background-color: {hover};
            }}
            QToolButton:pressed {{
                background-color: {pressed};
            }}
        """
        close_style = f"""
            QToolButton {{
                border: none;
                outline: none;
                padding: 0px;
                margin: 0px;
                background-color: {bg};
            }}
            QToolButton:hover {{
                background-color: {close_hover};
            }}
            QToolButton:pressed {{
                background-color: {pressed};
            }}
        """

        self._minimise_button.setStyleSheet(shared_style)
        self._maximise_button.setStyleSheet(shared_style)
        self._close_button.setStyleSheet(close_style)

        self._update_button_icons()

    def _update_button_icons(self) -> None:
        """Redraw the painted icons for minimise, maximise/restore, and close."""
        style_manager = self._style_manager
        icon_px = 16
        minimize_pixmap = style_manager.scale_icon("minimize", icon_px)
        minimize_icon = QIcon()
        minimize_icon.addPixmap(minimize_pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        self._minimise_button.setIcon(minimize_icon)

        if self._is_maximised:
            restore_pixmap = style_manager.scale_icon("restore", icon_px)
            restore_icon = QIcon()
            restore_icon.addPixmap(restore_pixmap, QIcon.Mode.Normal, QIcon.State.Off)
            self._maximise_button.setIcon(restore_icon)

        else:
            maximize_pixmap = style_manager.scale_icon("maximize", icon_px)
            maximize_icon = QIcon()
            maximize_icon.addPixmap(maximize_pixmap, QIcon.Mode.Normal, QIcon.State.Off)
            self._maximise_button.setIcon(maximize_icon)

        close_pixmap = style_manager.scale_icon("close", 16)
        close_icon = QIcon()
        close_icon.addPixmap(close_pixmap, QIcon.Mode.Normal, QIcon.State.Off)
        self._close_button.setIcon(close_icon)

    def set_maximised(self, maximised: bool) -> None:
        """
        Update the maximise/restore button to reflect the current window state.

        Args:
            maximised: True if the window is currently maximised.
        """
        if maximised != self._is_maximised:
            self._is_maximised = maximised
            self._update_button_icons()
            if not maximised:
                self._maximise_button.setAttribute(Qt.WidgetAttribute.WA_UnderMouse, False)
                QApplication.sendEvent(self._maximise_button, QEvent(QEvent.Type.Leave))

    def _on_minimise(self) -> None:
        """Minimise the parent window."""
        window = self.window()
        if window:
            window.showMinimized()

    def _on_maximise_restore(self) -> None:
        """Toggle maximise/restore on the parent window."""
        window = self.window()
        if window:
            state = window.windowState()
            if state & (Qt.WindowState.WindowMaximized | Qt.WindowState.WindowFullScreen):
                window.showNormal()

            else:
                # On Windows, hide the window around showMaximized() to prevent a brief
                # compositor glitch caused by the window moving to (0, 0) before Qt has
                # finished repainting the widget hierarchy at the new size.
                if sys.platform == "win32":
                    window.hide()

                window.showMaximized()
                if sys.platform == "win32":
                    window.show()

    def _on_close(self) -> None:
        """Close the parent window."""
        window = self.window()
        if window:
            window.close()


class MenuBarDragFilter(QWidget):
    """
    Transparent event filter widget that enables window dragging via the menu bar.

    Installed as a child of the menu bar; intercepts mouse events on the menu bar
    background (i.e. not on menu items) to move the frameless window.
    """

    def __init__(self, menu_bar: QWidget) -> None:
        """
        Initialise the drag filter.

        Args:
            menu_bar: The QMenuBar this filter is attached to.
        """
        super().__init__(menu_bar)
        self._menu_bar = menu_bar
        self._drag_pos: QPoint | None = None
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        menu_bar.installEventFilter(self)

    def eventFilter(self, watched: object, event: object) -> bool:
        """Forward relevant mouse events from the menu bar for drag handling."""
        if watched is not self._menu_bar:
            return False

        if not isinstance(event, QMouseEvent):
            return False

        if event.type() == QMouseEvent.Type.MouseButtonPress:
            return self._handle_press(event)

        if event.type() == QMouseEvent.Type.MouseMove:
            return self._handle_move(event)

        if event.type() == QMouseEvent.Type.MouseButtonRelease:
            self._drag_pos = None
            return False

        if event.type() == QMouseEvent.Type.MouseButtonDblClick:
            return self._handle_double_click(event)

        return False

    def _handle_press(self, event: QMouseEvent) -> bool:
        """Start a window drag if the press is on the menu bar background."""
        if event.button() != Qt.MouseButton.LeftButton:
            return False

        window = self._menu_bar.window()
        if not window:
            return False

        state = window.windowState()
        if bool(state & (Qt.WindowState.WindowMaximized | Qt.WindowState.WindowFullScreen)):
            return False

        child = self._menu_bar.childAt(event.pos())
        if child is not None and child is not self._menu_bar:
            return False

        self._drag_pos = event.globalPosition().toPoint() - window.frameGeometry().topLeft()
        return False

    def _handle_move(self, event: QMouseEvent) -> bool:
        """Move the window if a drag is in progress."""
        if self._drag_pos is None:
            return False

        if not event.buttons() & Qt.MouseButton.LeftButton:
            self._drag_pos = None
            return False

        window = self._menu_bar.window()
        if window:
            window.move(event.globalPosition().toPoint() - self._drag_pos)

        return False

    def _handle_double_click(self, event: QMouseEvent) -> bool:
        """Toggle maximise/restore on double-click on the menu bar background."""
        if event.button() != Qt.MouseButton.LeftButton:
            return False

        child = self._menu_bar.childAt(event.pos())
        if child is not None and child is not self._menu_bar:
            return False

        window = self._menu_bar.window()
        if not window:
            return False

        state = window.windowState()
        if state & (Qt.WindowState.WindowMaximized | Qt.WindowState.WindowFullScreen):
            window.showNormal()

        else:
            # On Windows, hide the window around showMaximized() to prevent a brief
            # compositor glitch caused by the window moving to (0, 0) before Qt has
            # finished repainting the widget hierarchy at the new size.
            if sys.platform == "win32":
                window.hide()

            window.showMaximized()
            if sys.platform == "win32":
                window.show()

        return False
