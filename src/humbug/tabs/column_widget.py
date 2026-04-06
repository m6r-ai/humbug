from PySide6.QtWidgets import QTabWidget, QWidget, QToolButton, QHBoxLayout, QSizePolicy
from PySide6.QtCore import Signal, QEvent, QObject, Qt, QSize
from PySide6.QtGui import QDragEnterEvent, QDragMoveEvent, QDropEvent, QIcon

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.tab_bar import TabBar


class ColumnWidget(QTabWidget):
    """Enhanced QTabWidget for use in columns with drag and drop support."""

    column_activated = Signal(QTabWidget)
    tab_dropped = Signal(str, QTabWidget, int)  # tab_id, target_column, target_index
    path_dropped = Signal(str, str, QTabWidget, int)  # source_type, path, target_column, target_index

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the tab widget."""
        super().__init__(parent)
        self.setMovable(True)
        self.setDocumentMode(True)

        # Enable drag and drop
        self.setAcceptDrops(True)

        # Configure tab bar
        self.setTabBar(TabBar(self))
        tab_bar = self.tabBar()
        tab_bar.setDrawBase(False)
        tab_bar.installEventFilter(self)

        # Corner nav widget — keeps scroll buttons out of the tab area
        self._style_manager = StyleManager()
        nav = QWidget()
        nav.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        nav_layout = QHBoxLayout(nav)
        nav_layout.setContentsMargins(2, 2, 4, 2)
        nav_layout.setSpacing(6)

        self._scroll_left_btn = QToolButton()
        self._scroll_left_btn.setArrowType(Qt.ArrowType.NoArrow)
        self._scroll_left_btn.setCursor(Qt.CursorShape.PointingHandCursor)
        self._scroll_left_btn.setAutoRepeat(True)
        self._scroll_left_btn.setAutoRepeatDelay(200)
        self._scroll_left_btn.setAutoRepeatInterval(50)
        self._scroll_left_btn.clicked.connect(lambda: self._on_nav_scroll(-1))
        nav_layout.addWidget(self._scroll_left_btn)

        self._scroll_right_btn = QToolButton()
        self._scroll_right_btn.setArrowType(Qt.ArrowType.NoArrow)
        self._scroll_right_btn.setCursor(Qt.CursorShape.PointingHandCursor)
        self._scroll_right_btn.setAutoRepeat(True)
        self._scroll_right_btn.setAutoRepeatDelay(200)
        self._scroll_right_btn.setAutoRepeatInterval(50)
        self._scroll_right_btn.clicked.connect(lambda: self._on_nav_scroll(1))
        nav_layout.addWidget(self._scroll_right_btn)

        self.setCornerWidget(nav, Qt.Corner.TopRightCorner)
        self._style_manager.style_changed.connect(self._apply_nav_style)
        self._apply_nav_style()

    def _apply_nav_style(self) -> None:
        """Style the corner nav scroll buttons to match the copy/download action buttons."""
        sm = self._style_manager
        zoom = sm.zoom_factor()
        button_size = max(24, int(30 * zoom))
        icon_base_size = 16
        icon_scaled_size = int(icon_base_size * zoom)
        radius = max(10, int(button_size / 2.4))
        surface = sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND)
        left_icon_name = "arrow-left" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-right"
        right_icon_name = "arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left"
        btn_style = f"""
            QToolButton {{
                background-color: {surface};
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: 1px solid {sm.get_color_str(ColorRole.MENU_BORDER)};
                padding: 0px;
                margin: 0px;
                border-radius: {radius}px;
                min-width: {button_size}px;
                min-height: {button_size}px;
                max-width: {button_size}px;
                max-height: {button_size}px;
            }}
            QToolButton:hover {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER)};
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
                border-color: {sm.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            QToolButton:pressed {{
                background-color: {sm.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED)};
                border-color: {sm.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            QToolButton:disabled {{
                background-color: {surface};
                color: {sm.get_color_str(ColorRole.TEXT_DISABLED)};
                border-color: {sm.get_color_str(ColorRole.MENU_BORDER)};
            }}
        """
        icon_size = QSize(icon_scaled_size, icon_scaled_size)
        self._scroll_left_btn.setIcon(QIcon(sm.scale_icon(left_icon_name, icon_base_size)))
        self._scroll_left_btn.setIconSize(icon_size)
        self._scroll_left_btn.setStyleSheet(btn_style)
        self._scroll_right_btn.setIcon(QIcon(sm.scale_icon(right_icon_name, icon_base_size)))
        self._scroll_right_btn.setIconSize(icon_size)
        self._scroll_right_btn.setStyleSheet(btn_style)

    def _on_nav_scroll(self, direction: int) -> None:
        """Forward scroll request to the tab bar."""
        tab_bar = self.tabBar()
        assert isinstance(tab_bar, TabBar)
        tab_bar.scroll_tabs(direction)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """Handle window activation and mouse events to detect active column."""
        if event.type() in (QEvent.Type.MouseButtonPress, QEvent.Type.FocusIn):
            # Emit activation on mouse press or focus
            self.column_activated.emit(self)
            return False  # Don't consume the event

        return super().eventFilter(watched, event)

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """
        Handle drag enter events for tab drops.

        Args:
            event: The drag enter event

        Raises:
            None
        """
        if event.mimeData().hasFormat("application/x-humbug-tab"):
            event.acceptProposedAction()
            return

        if event.mimeData().hasFormat("application/x-humbug-path"):
            event.acceptProposedAction()
            return

        event.ignore()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """
        Handle drag move events to show insertion position.

        Args:
            event: The drag move event

        Raises:
            None
        """
        if (event.mimeData().hasFormat("application/x-humbug-tab") or
                event.mimeData().hasFormat("application/x-humbug-path")):
            event.acceptProposedAction()

            # Map cursor position to the tab bar to find insertion position
            pos = self.tabBar().mapFromParent(event.pos())
            _index = self.tabBar().tabAt(pos)
            return
            # Note: Could add visual indicator of insertion position here if desired

        event.ignore()

    def dropEvent(self, event: QDropEvent) -> None:
        """
        Handle drop events for tab movement and file drops.

        Args:
            event: The drop event

        Raises:
            None
        """
        if event.mimeData().hasFormat("application/x-humbug-tab"):
            # Extract tab ID from mime data
            mime_data = event.mimeData().data("application/x-humbug-tab").data()

            # Convert to bytes first if it's not already bytes
            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            tab_id = mime_data.decode()

            # Map the drop position to the tab bar
            pos = self.tabBar().mapFromParent(event.pos())
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with drop info for tab manager to handle
            self.tab_dropped.emit(tab_id, self, target_index)
            event.acceptProposedAction()
            return

        if event.mimeData().hasFormat("application/x-humbug-path"):
            # Extract path from mime data
            mime_data = event.mimeData().data("application/x-humbug-path").data()

            # Convert to bytes first if it's not already bytes
            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            path = mime_data.decode()

            # Extract source type if available
            source_type = None
            if event.mimeData().hasFormat("application/x-humbug-source"):
                source_data = event.mimeData().data("application/x-humbug-source").data()

                # Convert to bytes first if it's not already bytes
                if not isinstance(source_data, bytes):
                    source_data = bytes(source_data)

                source_type = source_data.decode()

            # Map the drop position to the tab bar
            pos = self.tabBar().mapFromParent(event.pos())
            target_index = self.tabBar().tabAt(pos)

            # If dropped past the last tab, append
            if target_index == -1:
                target_index = self.count()

            # Emit signal with path info for column manager to handle
            self.path_dropped.emit(source_type, path, self, target_index)
            event.acceptProposedAction()
            return

        event.ignore()
