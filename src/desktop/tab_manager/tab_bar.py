from dataclasses import dataclass, field
import math

from PySide6.QtCore import QEvent, QMimeData, QObject, QPoint, QRect, QSize, Qt, Signal
from PySide6.QtGui import (
    QContextMenuEvent, QDrag, QFont, QFontMetricsF, QPainter, QPaintEvent, QMouseEvent, QPixmap, QWheelEvent
)
from PySide6.QtWidgets import QApplication, QTabBar, QToolButton, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


@dataclass
class _TabData:
    """All display state for a single tab label."""

    tab_id: str
    icon_name: str
    text: str
    tool_tip: str
    is_current: bool = False
    is_active_column: bool = False
    is_updated: bool = False
    is_ephemeral: bool = False
    is_file_missing: bool = False


@dataclass
class _TabGeometry:
    """Pre-computed paint geometry for a single tab."""

    icon_rect: QRect
    text_rect: QRect
    close_rect: QRect


@dataclass
class _PaintResources:
    """Cached pixmaps and font used across paint calls."""

    font: QFont = field(default_factory=QFont)
    icon_pixmaps: dict[str, QPixmap] = field(default_factory=dict)
    inactive_icon_pixmaps: dict[str, QPixmap] = field(default_factory=dict)
    close_pixmap: QPixmap = field(default_factory=QPixmap)
    inactive_close_pixmap: QPixmap = field(default_factory=QPixmap)


class TabBar(QTabBar):
    """
    Fully custom-painted tab bar.

    All label content (type icon, text, close button) is drawn directly in
    paintEvent so that the layout can be centred within each tab rect without
    the constraints imposed by setTabButton.  Mouse interaction (hover, close,
    double-click, drag) is handled via the standard mouse event overrides.

    Tab data is keyed by tab_id (a stable string).  Each Qt tab slot stores its
    tab_id via setTabData so we can always resolve index -> tab_id -> _TabData
    without maintaining any parallel index mapping.
    """

    close_clicked = Signal(str)
    double_clicked = Signal(str)
    context_menu_requested = Signal(str, QPoint)

    def __init__(self, parent: QWidget | None = None) -> None:
        self._style_manager = StyleManager()
        self._tab_data: dict[str, _TabData] = {}
        self._resources = _PaintResources()

        self._current_hovered_tab = -1
        self._close_hovered = False
        self._drag_start_pos: QPoint | None = None
        self._drag_tab_index: int = -1

        self._scroll_pixel_accumulator = 0
        self._scroll_pixels_per_step = 32
        self._drop_index = -1

        super().__init__(parent)
        self.setExpanding(False)
        self.setDocumentMode(True)
        self.setMouseTracking(True)
        self._rebuild_resources()

        self.installEventFilter(self)

    def add_tab_data(
        self,
        index: int,
        tab_id: str,
        icon_name: str,
        text: str,
        tool_tip: str,
        is_ephemeral: bool = False,
        is_file_missing: bool = False,
    ) -> None:
        """Store display data for a newly added tab and bind it to the Qt slot."""
        self._tab_data[tab_id] = _TabData(
            tab_id=tab_id,
            icon_name=icon_name,
            text=text,
            tool_tip=tool_tip,
            is_ephemeral=is_ephemeral,
            is_file_missing=is_file_missing,
        )
        self.setTabData(index, tab_id)
        self.adjustSize()

    def remove_tab_data(self, tab_id: str) -> None:
        """Remove display data for a tab that has been removed."""
        self._tab_data.pop(tab_id, None)

    def get_tab_text(self, index: int) -> str:
        """Return the current display text for the tab at index."""
        data = self._data_for_index(index)
        return data.text if data else ""

    def set_tab_text(self, index: int, text: str) -> None:
        """Update the display text for a tab and trigger a repaint."""
        data = self._data_for_index(index)
        if data:
            data.text = text
            self.adjustSize()
            self.updateGeometry()

    def set_tab_state(
        self,
        index: int,
        is_current: bool,
        is_updated: bool,
        is_active_column: bool,
    ) -> None:
        """Update the current/updated/active-column state for a tab."""
        data = self._data_for_index(index)
        if data:
            data.is_current = is_current
            data.is_updated = is_updated
            data.is_active_column = is_active_column
            self.update(self.tabRect(index))

    def set_tab_ephemeral(self, index: int, is_ephemeral: bool) -> None:
        """Update the ephemeral state for a tab."""
        data = self._data_for_index(index)
        if data:
            data.is_ephemeral = is_ephemeral
            self.adjustSize()
            self.updateGeometry()

    def set_tab_file_missing(self, index: int, is_file_missing: bool) -> None:
        """Update the file-missing state for a tab."""
        data = self._data_for_index(index)
        if data:
            data.is_file_missing = is_file_missing
            self.adjustSize()
            self.updateGeometry()

    def index_for_tab_id(self, tab_id: str) -> int:
        """Return the tab index for the given tab_id, or -1 if not found."""
        for i in range(self.count()):
            if self.tabData(i) == tab_id:
                return i

        return -1

    def set_drop_index(self, index: int) -> None:
        """Set the insertion index for an in-progress drag and trigger a repaint."""
        if index != self._drop_index:
            self._drop_index = index
            self.update()

    def clear_drop_index(self) -> None:
        """Clear the drop insertion indicator and trigger a repaint."""
        if self._drop_index != -1:
            self._drop_index = -1
            self.update()

    def handle_style_changed(self) -> None:
        """Rebuild cached resources and repaint when the style changes."""
        self._rebuild_resources()
        self.adjustSize()
        self.updateGeometry()

    def tabSizeHint(self, index: int) -> QSize:
        """Return a size based on painted content rather than Qt's default layout."""
        data = self._data_for_index(index)
        text = data.text if data else ""

        sm = self._style_manager
        icon_size = sm.tab_icon_size()
        spacing = sm.tab_spacing()
        padding = sm.tab_padding()

        font = QFont(self._resources.font)
        if data is not None:
            font.setItalic(data.is_ephemeral)
            font.setStrikeOut(data.is_file_missing)

        fm = QFontMetricsF(font)
        text_width = math.ceil(fm.horizontalAdvance(text))

        content_width = icon_size + spacing + text_width + spacing + icon_size
        width = padding + content_width + padding
        height = icon_size + padding * 2

        return QSize(width, height)

    def minimumTabSizeHint(self, index: int) -> QSize:
        return self.tabSizeHint(index)

    def paintEvent(self, event: QPaintEvent) -> None:  # type: ignore[override]
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        border_px = self._top_border_px()
        is_ltr = self.layoutDirection() == Qt.LayoutDirection.LeftToRight
        outermost_edge = 0 if is_ltr else self.width()
        prev_is_current = False

        for index in range(self.count()):
            data = self._data_for_index(index)
            tab_rect = self.tabRect(index)

            if not tab_rect.intersects(event.rect()):
                prev_is_current = data.is_current if data else False
                continue

            if is_ltr:
                outermost_edge = max(outermost_edge, tab_rect.right() + 1)

            else:
                outermost_edge = min(outermost_edge, tab_rect.left())

            is_current = data.is_current if data else False
            is_updated = data.is_updated if data else False
            is_active_column = data.is_active_column if data else False
            is_hovered = index == self._current_hovered_tab

            bg_role = self._background_role(is_active_column, is_current, is_updated, is_hovered)
            bg_color = self._style_manager.get_color(bg_role)
            painter.fillRect(tab_rect, bg_color)

            if index > 0:
                left_border_color = self._style_manager.get_color(
                    ColorRole.SPLITTER if is_current or prev_is_current else ColorRole.TAB_SPLITTER
                )
                if is_ltr:
                    painter.fillRect(tab_rect.adjusted(0, 0, -tab_rect.width() + 1, 0), left_border_color)

                else:
                    painter.fillRect(tab_rect.adjusted(tab_rect.width() - 1, 0, 0, 0), left_border_color)

            if is_current:
                border_role = ColorRole.TAB_BORDER_ACTIVE if is_active_column else ColorRole.SPLITTER
                painter.fillRect(
                    tab_rect.adjusted(0, 0, 0, -tab_rect.height() + border_px),
                    self._style_manager.get_color(border_role),
                )

            else:
                painter.fillRect(
                    tab_rect.adjusted(0, tab_rect.height() - 1, 0, 0),
                    self._style_manager.get_color(ColorRole.SPLITTER),
                )

            if data:
                self._paint_label(painter, index, data, tab_rect, is_current, is_active_column, is_hovered)

            prev_is_current = is_current

        self._paint_trailing_area(painter, outermost_edge, is_ltr, prev_is_current)
        painter.end()

        self._paint_drop_indicator()

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            index = self.tabAt(event.pos())
            if index != -1:
                self._drag_start_pos = event.pos()
                self._drag_tab_index = index

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        pos = event.pos()
        index = self.tabAt(pos)

        old_hovered = self._current_hovered_tab
        old_close_hovered = self._close_hovered

        self._current_hovered_tab = index
        self._close_hovered = index != -1 and self._close_rect_for(index).contains(pos)

        if self._current_hovered_tab != old_hovered or self._close_hovered != old_close_hovered:
            if old_hovered != -1:
                self.update(self.tabRect(old_hovered))

            if index != -1:
                self.update(self.tabRect(index))

        if self._close_hovered:
            self.setCursor(Qt.CursorShape.PointingHandCursor)

        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)

        if index != -1:
            data = self._data_for_index(index)
            self.setToolTip((data.tool_tip or data.text) if data else "")

        else:
            self.setToolTip("")

        if (
            self._drag_start_pos is not None
            and self._drag_tab_index != -1
            and (pos - self._drag_start_pos).manhattanLength() >= QApplication.startDragDistance()
        ):
            self._start_drag(self._drag_tab_index)
            return

        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            index = self.tabAt(event.pos())
            if index != -1 and self._close_rect_for(index).contains(event.pos()):
                data = self._data_for_index(index)
                if data:
                    self.close_clicked.emit(data.tab_id)
                    self._drag_start_pos = None
                    self._drag_tab_index = -1
                    return

        self._drag_start_pos = None
        self._drag_tab_index = -1
        super().mouseReleaseEvent(event)

    def mouseDoubleClickEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            index = self.tabAt(event.pos())
            if index != -1:
                data = self._data_for_index(index)
                if data:
                    self.double_clicked.emit(data.tab_id)

        super().mouseDoubleClickEvent(event)

    def contextMenuEvent(self, event: QContextMenuEvent) -> None:
        index = self.tabAt(event.pos())
        if index != -1:
            data = self._data_for_index(index)
            if data:
                self.context_menu_requested.emit(data.tab_id, event.globalPos())
                event.accept()
                return

        super().contextMenuEvent(event)

    def wheelEvent(self, event: QWheelEvent) -> None:
        """Use wheel or trackpad gestures to scroll the tab strip."""
        pixel_delta = event.pixelDelta()
        angle_delta = event.angleDelta()

        if pixel_delta.x() != 0 or pixel_delta.y() != 0:
            px = pixel_delta.x() if pixel_delta.x() != 0 else pixel_delta.y()
            self._scroll_pixel_accumulator += px
            step_count = int(self._scroll_pixel_accumulator / self._scroll_pixels_per_step)
            if step_count != 0:
                self._scroll_pixel_accumulator -= step_count * self._scroll_pixels_per_step
                step_count = max(-4, min(4, step_count))
                if self._scroll_tabs_by_steps(-step_count):
                    event.accept()
                    return

            else:
                event.accept()
                return

        delta = angle_delta.x()
        if delta == 0:
            delta = angle_delta.y()

        if delta != 0:
            step_count = max(1, min(3, abs(delta) // 120))
            if self._scroll_tabs_by_steps(step_count if delta < 0 else -step_count):
                event.accept()
                return

        super().wheelEvent(event)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        if event.type() == QEvent.Type.Leave:
            if self._current_hovered_tab != -1:
                old = self._current_hovered_tab
                self._current_hovered_tab = -1
                self._close_hovered = False
                self.setCursor(Qt.CursorShape.ArrowCursor)
                self.update(self.tabRect(old))

        return super().eventFilter(watched, event)

    def _rebuild_resources(self) -> None:
        sm = self._style_manager

        font = QFont()
        font.setFamilies(sm.proportional_font_families())
        font.setPointSizeF(sm.base_font_size() * sm.zoom_factor())
        self._resources.font = font

        self._resources.icon_pixmaps.clear()
        self._resources.inactive_icon_pixmaps.clear()

        for icon_name in ("conversation", "editor", "log", "shell", "terminal", "preview", "diff", "usage"):
            self._resources.icon_pixmaps[icon_name] = sm.scale_icon(icon_name, 16)
            self._resources.inactive_icon_pixmaps[icon_name] = sm.scale_icon(f"inactive-{icon_name}", 16)

        self._resources.close_pixmap = sm.scale_icon("close", 16)
        self._resources.inactive_close_pixmap = sm.scale_icon("inactive-close", 16)

    def _data_for_index(self, index: int) -> _TabData | None:
        """Resolve a Qt tab index to its _TabData via the stored tab_id."""
        tab_id = self.tabData(index)
        if not isinstance(tab_id, str):
            return None

        return self._tab_data.get(tab_id)

    def _tab_geometry(self, tab_rect: QRect, index: int) -> _TabGeometry:
        """Compute the icon, text and close rects centred within tab_rect."""
        sm = self._style_manager
        icon_size = sm.tab_icon_size()
        spacing = sm.tab_spacing()

        data = self._data_for_index(index)
        text = data.text if data else ""

        font = QFont(self._resources.font)
        if data is not None:
            font.setItalic(data.is_ephemeral)
            font.setStrikeOut(data.is_file_missing)

        fm = QFontMetricsF(font)
        text_width = math.ceil(fm.horizontalAdvance(text))

        content_width = icon_size + spacing + text_width + spacing + icon_size
        cx = tab_rect.left() + (tab_rect.width() - content_width) // 2
        cy = tab_rect.top() + (tab_rect.height() - icon_size) // 2

        if self.layoutDirection() == Qt.LayoutDirection.LeftToRight:
            icon_rect = QRect(cx, cy, icon_size, icon_size)
            text_x = cx + icon_size + spacing
            text_rect = QRect(text_x, tab_rect.top(), text_width, tab_rect.height())
            close_x = text_x + text_width + spacing
            close_rect = QRect(close_x, cy, icon_size, icon_size)

        else:
            close_rect = QRect(cx, cy, icon_size, icon_size)
            text_x = cx + icon_size + spacing
            text_rect = QRect(text_x, tab_rect.top(), text_width, tab_rect.height())
            icon_rect = QRect(text_x + text_width + spacing, cy, icon_size, icon_size)

        return _TabGeometry(icon_rect=icon_rect, text_rect=text_rect, close_rect=close_rect)

    def _close_rect_for(self, index: int) -> QRect:
        """Return the close-button hit rect for the tab at index."""
        return self._tab_geometry(self.tabRect(index), index).close_rect

    def _paint_label(
        self,
        painter: QPainter,
        index: int,
        data: _TabData,
        tab_rect: QRect,
        is_current: bool,
        is_active_column: bool,
        is_hovered: bool,
    ) -> None:
        geom = self._tab_geometry(tab_rect, index)
        show_active = is_current and is_active_column
        visible = is_current or is_hovered

        icon_map = self._resources.icon_pixmaps if show_active else self._resources.inactive_icon_pixmaps
        icon_pixmap = icon_map.get(data.icon_name)
        if icon_pixmap:
            painter.drawPixmap(geom.icon_rect.topLeft(), icon_pixmap)

        font = QFont(self._resources.font)
        font.setItalic(data.is_ephemeral)
        font.setStrikeOut(data.is_file_missing)
        painter.setFont(font)

        if data.is_file_missing:
            text_role = ColorRole.TEXT_ERROR if show_active else ColorRole.TEXT_ERROR_INACTIVE

        elif data.is_ephemeral:
            text_role = ColorRole.TEXT_EPHEMERAL if show_active else ColorRole.TEXT_EPHEMERAL_INACTIVE

        else:
            text_role = ColorRole.TEXT_PRIMARY if show_active else ColorRole.TEXT_INACTIVE

        painter.setPen(self._style_manager.get_color(text_role))
        painter.drawText(geom.text_rect, Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft, data.text)

        if visible:
            close_hovered = self._close_hovered and index == self._current_hovered_tab
            if close_hovered:
                hover_color = self._style_manager.get_color(ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER)
                radius = self._style_manager.radius()
                painter.setBrush(hover_color)
                painter.setPen(Qt.PenStyle.NoPen)
                painter.drawRoundedRect(geom.close_rect, radius, radius)

            close_pixmap = (
                self._resources.close_pixmap if show_active else self._resources.inactive_close_pixmap
            )
            painter.drawPixmap(geom.close_rect.topLeft(), close_pixmap)

    def _paint_trailing_area(self, painter: QPainter, outermost_edge: int, is_ltr: bool, prev_is_current: bool) -> None:
        """Paint the outer edge and bottom border of the empty space after all tabs."""
        if self.count() == 0:
            return

        splitter_color = self._style_manager.get_color(
            ColorRole.SPLITTER if prev_is_current else ColorRole.TAB_SPLITTER
        )

        if is_ltr:
            if outermost_edge >= self.width():
                return

            edge_rect = self.rect()
            edge_rect.setLeft(outermost_edge)
            edge_rect.setWidth(1)
            painter.fillRect(edge_rect, splitter_color)
            if outermost_edge + 1 < self.width():
                bottom_strip = self.rect()
                bottom_strip.setLeft(outermost_edge)
                bottom_strip.setTop(self.height() - 1)
                bottom_strip.setHeight(1)
                painter.fillRect(bottom_strip, self._style_manager.get_color(ColorRole.SPLITTER))

        else:
            if outermost_edge <= 0:
                return

            edge_rect = self.rect()
            edge_rect.setRight(outermost_edge)
            edge_rect.setLeft(outermost_edge - 1)
            painter.fillRect(edge_rect, splitter_color)
            if outermost_edge - 1 > 0:
                bottom_strip = self.rect()
                bottom_strip.setRight(outermost_edge)
                bottom_strip.setTop(self.height() - 1)
                bottom_strip.setHeight(1)
                painter.fillRect(bottom_strip, self._style_manager.get_color(ColorRole.SPLITTER))

    def _paint_drop_indicator(self) -> None:
        """Paint the drag-and-drop insertion bar on top of everything else."""
        if self._drop_index == -1:
            return

        bar_color = self._style_manager.get_color(ColorRole.DROP_TARGET_SEPARATOR_HIGHLIGHT)
        bar_width = 2

        is_ltr = self.layoutDirection() == Qt.LayoutDirection.LeftToRight
        if is_ltr:
            if self._drop_index < self.count():
                bar_x = self.tabRect(self._drop_index).left()

            elif self.count() > 0:
                bar_x = self.tabRect(self.count() - 1).right() + 1

            else:
                bar_x = 0

        else:
            if self._drop_index < self.count():
                bar_x = self.tabRect(self._drop_index).right() - bar_width + 1

            elif self.count() > 0:
                bar_x = self.tabRect(self.count() - 1).left() - bar_width

            else:
                bar_x = self.width() - bar_width

        painter = QPainter(self)
        painter.fillRect(QRect(bar_x, 0, bar_width, self.height()), bar_color)
        painter.end()

    def _top_border_px(self) -> int:
        """Return the top-border thickness (1px in full-screen, 2px otherwise)."""
        parent = self.parentWidget()
        while parent:
            if parent.isFullScreen():
                return 1

            parent = parent.parentWidget()

        return 2

    def _background_role(
        self,
        is_active_column: bool,
        is_current: bool,
        is_updated: bool,
        is_hovered: bool,
    ) -> ColorRole:
        if is_hovered and (not is_current or not is_active_column):
            return ColorRole.TAB_BACKGROUND_HOVER

        if is_updated:
            return ColorRole.TAB_BACKGROUND_UPDATED

        if is_current:
            return ColorRole.TAB_BACKGROUND_ACTIVE

        return ColorRole.TAB_BACKGROUND_INACTIVE

    def _start_drag(self, index: int) -> None:
        """Initiate a QDrag operation for the tab at index."""
        data = self._data_for_index(index)
        if not data:
            return

        drag_start_pos = self._drag_start_pos

        self._drag_start_pos = None
        self._drag_tab_index = -1

        drag = QDrag(self)
        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-tab", data.tab_id.encode())
        drag.setMimeData(mime_data)

        pixmap = self._render_tab_pixmap(index)
        drag.setPixmap(pixmap)
        tab_rect = self.tabRect(index)
        if drag_start_pos is not None:
            hot_spot = drag_start_pos - tab_rect.topLeft()

        else:
            hot_spot = tab_rect.center() - tab_rect.topLeft()

        drag.setHotSpot(hot_spot)

        drag.exec_(Qt.DropAction.MoveAction)

    def _render_tab_pixmap(self, index: int) -> QPixmap:
        """Render a single tab into a pixmap for drag feedback."""
        tab_rect = self.tabRect(index)
        pixmap = QPixmap(tab_rect.size())
        pixmap.fill(self._style_manager.get_color(ColorRole.TAB_BACKGROUND_ACTIVE))

        data = self._data_for_index(index)
        if data:
            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.RenderHint.Antialiasing)
            local_rect = QRect(QPoint(0, 0), tab_rect.size())
            self._paint_label(painter, index, data, local_rect, True, True, False)
            painter.end()

        return pixmap

    def _scroll_tabs(self, direction: int) -> bool:
        """Scroll the tab strip by clicking the hidden native arrow buttons."""
        arrow_type = Qt.ArrowType.RightArrow if direction > 0 else Qt.ArrowType.LeftArrow
        for button in self.findChildren(QToolButton):
            if button.arrowType() != arrow_type:
                continue

            if not button.isEnabled():
                continue

            button.click()
            return True

        return False

    def _scroll_tabs_by_steps(self, steps: int) -> bool:
        """Scroll the tab strip by the requested number of native steps."""
        if steps == 0:
            return False

        direction = 1 if steps > 0 else -1
        moved = False
        for _ in range(abs(steps)):
            if not self._scroll_tabs(direction):
                break

            moved = True

        return moved
