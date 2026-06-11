"""Overview ("recents screen") overlay showing all open tabs as swipeable cards.

This module is purely presentational.  It knows nothing about tabs, contexts
or the mindspace: it consumes a list of TabOverviewEntry display records and
emits signals when the user activates or dismisses a card.  The TabManager is
responsible for supplying the data and acting on the signals.
"""

from dataclasses import dataclass
import math
from typing import Dict, List

from PySide6.QtCore import (
    QEasingCurve, QEvent, QPoint, QPropertyAnimation, QRect, QRectF, QSize, Qt, Signal
)
from PySide6.QtGui import (
    QColor, QEnterEvent, QFont, QFontMetricsF, QKeyEvent, QMouseEvent, QPainter, QPainterPath,
    QPaintEvent, QPen, QPixmap, QResizeEvent
)
from PySide6.QtWidgets import QApplication, QGraphicsOpacityEffect, QScrollArea, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


def aspect_fill_source_rect(thumbnail: QPixmap, target: QRect) -> QRect:
    """
    Compute the source crop (in pixmap device pixels) that fills target's aspect ratio.

    Working in the pixmap's own pixel space and drawing rect-to-rect sidesteps
    device-pixel-ratio mismatches between grabbed thumbnails and the screen.
    """
    pw = thumbnail.width()
    ph = thumbnail.height()
    if pw <= 0 or ph <= 0 or target.width() <= 0 or target.height() <= 0:
        return QRect()

    target_aspect = target.width() / target.height()
    source_aspect = pw / ph

    if source_aspect > target_aspect:
        crop_width = max(1, int(ph * target_aspect))
        return QRect((pw - crop_width) // 2, 0, crop_width, ph)

    crop_height = max(1, int(pw / target_aspect))
    return QRect(0, 0, pw, crop_height)


@dataclass
class TabOverviewEntry:
    """Display-only record describing one open tab."""

    tab_id: str
    icon_name: str
    title: str
    thumbnail: QPixmap
    is_current: bool = False


class TabOverviewCard(QWidget):
    """A single tab card: thumbnail plus title bar, closable with an upward swipe."""

    activated = Signal(str)
    close_requested = Signal(str)

    def __init__(self, entry: TabOverviewEntry, parent: QWidget) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._entry = entry
        self._is_hovered = False
        self._is_selected = False
        self._close_hovered = False
        self._close_pressed = False

        self._press_pos: QPoint | None = None
        self._home_pos = QPoint()
        self._is_swiping = False

        self._opacity_effect = QGraphicsOpacityEffect(self)
        self._opacity_effect.setOpacity(1.0)
        self.setGraphicsEffect(self._opacity_effect)

        self._animation: QPropertyAnimation | None = None

        self.setMouseTracking(True)
        self.setCursor(Qt.CursorShape.PointingHandCursor)

    def tab_id(self) -> str:
        """Return the ID of the tab this card represents."""
        return self._entry.tab_id

    def set_selected(self, selected: bool) -> None:
        """Set whether this card carries the keyboard selection highlight."""
        if self._is_selected != selected:
            self._is_selected = selected
            self.update()

    def _close_rect(self) -> QRect:
        """Return the hit rect of the close button in the card header."""
        sm = self._style_manager
        icon_size = sm.tab_icon_size()
        spacing = sm.tab_spacing()
        return QRect(
            self.width() - spacing - icon_size,
            (self._header_height() - icon_size) // 2,
            icon_size,
            icon_size,
        )

    def set_home_pos(self, pos: QPoint) -> None:
        """Record the card's resting position within the grid."""
        self._home_pos = pos

    def restore(self) -> None:
        """Animate the card back to its grid position at full opacity."""
        self._opacity_effect.setOpacity(1.0)
        self._animate_to(self._home_pos)

    def _header_height(self) -> int:
        return self._style_manager.scale(28)

    def enterEvent(self, event: QEnterEvent) -> None:
        self._is_hovered = True
        self.update()
        super().enterEvent(event)

    def leaveEvent(self, event: QEvent) -> None:
        self._is_hovered = False
        self._close_hovered = False
        self.update()
        super().leaveEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            if self._close_rect().contains(event.pos()):
                self._close_pressed = True
                return

            self._press_pos = event.pos()
            self._is_swiping = False

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        close_hovered = self._close_rect().contains(event.pos())
        if close_hovered != self._close_hovered:
            self._close_hovered = close_hovered
            self.update()

        if self._press_pos is None:
            super().mouseMoveEvent(event)
            return

        delta = event.pos() - self._press_pos
        if not self._is_swiping:
            if delta.manhattanLength() < QApplication.startDragDistance():
                return

            # Only a predominantly upward drag starts a swipe
            if delta.y() >= 0 or abs(delta.x()) > abs(delta.y()):
                return

            self._is_swiping = True

        offset_y = min(0, delta.y())
        self.move(self._home_pos + QPoint(0, offset_y))
        progress = min(1.0, -offset_y / max(1, self.height()))
        self._opacity_effect.setOpacity(max(0.15, 1.0 - progress))

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        if event.button() != Qt.MouseButton.LeftButton:
            super().mouseReleaseEvent(event)
            return

        if self._close_pressed:
            self._close_pressed = False
            if self._close_rect().contains(event.pos()):
                self._emit_close()

            return

        press_pos = self._press_pos
        self._press_pos = None

        if not self._is_swiping:
            if press_pos is not None and self.rect().contains(event.pos()):
                self.activated.emit(self._entry.tab_id)

            return

        self._is_swiping = False
        dragged = self._home_pos.y() - self.pos().y()
        if dragged > self.height() * 0.35:
            self._animate_to(QPoint(self._home_pos.x(), -self.height()), on_done=self._emit_close)

        else:
            self._opacity_effect.setOpacity(1.0)
            self._animate_to(self._home_pos)

    def _emit_close(self) -> None:
        self.close_requested.emit(self._entry.tab_id)

    def _animate_to(self, target: QPoint, on_done: object = None) -> None:
        animation = QPropertyAnimation(self, b"pos", self)
        animation.setDuration(150)
        animation.setEasingCurve(QEasingCurve.Type.OutCubic)
        animation.setStartValue(self.pos())
        animation.setEndValue(target)
        if callable(on_done):
            animation.finished.connect(on_done)

        self._animation = animation
        animation.start()

    def paintEvent(self, _event: QPaintEvent) -> None:
        sm = self._style_manager
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        radius = sm.radius("panel")
        card_rect = self.rect().adjusted(0, 0, -1, -1)

        path = QPainterPath()
        path.addRoundedRect(card_rect, radius, radius)
        painter.setClipPath(path)

        painter.fillRect(card_rect, sm.get_color(ColorRole.BACKGROUND_SECONDARY))

        header_height = self._header_height()
        header_rect = QRect(0, 0, card_rect.width(), header_height)
        painter.fillRect(header_rect, sm.get_color(ColorRole.BACKGROUND_TERTIARY))

        # Type icon and elided title in the header
        icon_size = sm.tab_icon_size()
        spacing = sm.tab_spacing()
        icon_pixmap = sm.scale_icon(self._entry.icon_name, 16)
        icon_y = (header_height - icon_size) // 2
        painter.drawPixmap(QPoint(spacing, icon_y), icon_pixmap)

        font = QFont()
        font.setFamilies(sm.proportional_font_families())
        font.setPointSizeF(sm.base_font_size() * sm.zoom_factor())
        painter.setFont(font)
        painter.setPen(sm.get_color(ColorRole.TEXT_PRIMARY))

        close_rect = self._close_rect()
        text_x = spacing + icon_size + spacing
        text_rect = QRect(text_x, 0, close_rect.left() - spacing - text_x, header_height)
        fm = QFontMetricsF(font)
        elided = fm.elidedText(self._entry.title, Qt.TextElideMode.ElideMiddle, text_rect.width())
        painter.drawText(text_rect, Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft, elided)

        # Close button
        if self._close_hovered:
            hover_color = sm.get_color(ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER)
            painter.setBrush(hover_color)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.drawRoundedRect(close_rect, sm.radius(), sm.radius())

        painter.drawPixmap(close_rect.topLeft(), sm.scale_icon("close", 16))

        # Thumbnail fills the body exactly; rect-to-rect drawing keeps this
        # correct regardless of the thumbnail's device pixel ratio
        body_rect = QRect(0, header_height, card_rect.width(), card_rect.height() - header_height)
        thumbnail = self._entry.thumbnail
        if not thumbnail.isNull() and body_rect.height() > 0:
            painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)
            source = aspect_fill_source_rect(thumbnail, body_rect)
            if not source.isEmpty():
                painter.drawPixmap(body_rect, thumbnail, source)

        painter.setClipping(False)

        # Border: highlighted when hovered or selected.  A centred pen stroke
        # covers the clip boundary so no seam shows at the rounded corners.
        if self._is_hovered or self._is_selected:
            border_color = sm.get_color(ColorRole.BRAND_PRIMARY)
            border_width = 2.0

        else:
            border_color = sm.get_color(ColorRole.SPLITTER)
            border_width = 1.0

        pen = QPen(border_color)
        pen.setWidthF(border_width)
        painter.setPen(pen)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        inset = border_width / 2
        painter.drawRoundedRect(
            QRectF(card_rect).adjusted(inset, inset, -inset, -inset), radius, radius
        )

class TabOverviewWidget(QWidget):
    """Full-area overlay presenting all open tabs as a scrollable grid of cards."""

    tab_activated = Signal(str)
    tab_close_requested = Signal(str)
    dismissed = Signal()

    def __init__(self, parent: QWidget) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._cards: Dict[str, TabOverviewCard] = {}
        self._card_order: List[str] = []
        self._selected_id: str | None = None

        self._scroll_area = QScrollArea(self)
        self._scroll_area.setWidgetResizable(False)
        self._scroll_area.setFrameShape(QScrollArea.Shape.NoFrame)
        self._scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._scroll_area.viewport().setAutoFillBackground(False)
        self._scroll_area.setStyleSheet("QScrollArea { background: transparent; }")

        self._content = QWidget()
        self._content.setAutoFillBackground(False)
        self._content.setStyleSheet("background: transparent;")
        self._scroll_area.setWidget(self._content)

        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

    def set_entries(self, entries: List[TabOverviewEntry]) -> None:
        """Replace the displayed cards with one card per entry."""
        for card in self._cards.values():
            card.deleteLater()

        self._cards.clear()
        self._card_order = []

        self._selected_id = None
        for entry in entries:
            card = TabOverviewCard(entry, self._content)
            card.activated.connect(self.tab_activated)
            card.close_requested.connect(self.tab_close_requested)
            card.show()
            self._cards[entry.tab_id] = card
            self._card_order.append(entry.tab_id)
            if entry.is_current:
                self._selected_id = entry.tab_id

        if self._selected_id is None and self._card_order:
            self._selected_id = self._card_order[0]

        self._apply_selection()
        self._relayout()

    def remove_card(self, tab_id: str) -> None:
        """Remove the card for a tab that has been closed."""
        card = self._cards.pop(tab_id, None)
        if card is None:
            return

        if tab_id in self._card_order:
            self._card_order.remove(tab_id)

        card.deleteLater()
        if self._selected_id == tab_id:
            self._selected_id = self._card_order[0] if self._card_order else None
            self._apply_selection()

        self._relayout()

    def cycle_selection(self, step: int = 1) -> None:
        """Move the selection highlight by step cards, wrapping at either end."""
        if not self._card_order:
            return

        if self._selected_id in self._card_order:
            index = (self._card_order.index(self._selected_id) + step) % len(self._card_order)

        else:
            index = 0

        self._selected_id = self._card_order[index]
        self._apply_selection()
        self._scroll_area.ensureWidgetVisible(self._cards[self._selected_id])

    def selected_tab_id(self) -> str | None:
        """Return the tab ID of the currently selected card, if any."""
        return self._selected_id

    def _apply_selection(self) -> None:
        for tab_id, card in self._cards.items():
            card.set_selected(tab_id == self._selected_id)

    def restore_card(self, tab_id: str) -> None:
        """Return a swiped-away card to its grid position (close was vetoed)."""
        card = self._cards.get(tab_id)
        if card is not None:
            card.restore()

    def card_count(self) -> int:
        """Return the number of cards currently displayed."""
        return len(self._cards)

    def _card_size(self) -> QSize:
        sm = self._style_manager
        return QSize(sm.scale(340), sm.scale(250))

    def _relayout(self) -> None:
        """Position all cards in a grid sized to the available width."""
        margin = self._style_manager.scale(24)
        spacing = self._style_manager.scale(16)
        card_size = self._card_size()

        available = max(card_size.width(), self.width() - margin * 2)
        columns = max(1, (available + spacing) // (card_size.width() + spacing))
        columns = min(columns, max(1, len(self._card_order)))

        rows = math.ceil(len(self._card_order) / columns) if self._card_order else 0
        content_width = max(self.width(), card_size.width() + margin * 2)
        content_height = margin * 2 + rows * card_size.height() + max(0, rows - 1) * spacing
        self._content.resize(content_width, max(content_height, self.height()))

        grid_width = columns * card_size.width() + (columns - 1) * spacing
        x0 = max(margin, (content_width - grid_width) // 2)

        for i, tab_id in enumerate(self._card_order):
            row, col = divmod(i, columns)
            pos = QPoint(
                x0 + col * (card_size.width() + spacing),
                margin + row * (card_size.height() + spacing),
            )
            card = self._cards[tab_id]
            card.resize(card_size)
            card.set_home_pos(pos)
            card.move(pos)

    def paintEvent(self, _event: QPaintEvent) -> None:
        painter = QPainter(self)
        dim = QColor(self._style_manager.get_color(ColorRole.BACKGROUND_PRIMARY))
        dim.setAlpha(230)
        painter.fillRect(self.rect(), dim)

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self._scroll_area.setGeometry(self.rect())
        self._relayout()

    def focusNextPrevChild(self, next: bool) -> bool:  # pylint: disable=redefined-builtin
        """Cycle the card selection on Tab/Shift+Tab instead of moving focus."""
        self.cycle_selection(1 if next else -1)
        return True

    def keyPressEvent(self, event: QKeyEvent) -> None:
        if event.key() == Qt.Key.Key_Escape:
            self.dismissed.emit()
            return

        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            if self._selected_id is not None:
                self.tab_activated.emit(self._selected_id)

            return

        if event.key() in (Qt.Key.Key_Right, Qt.Key.Key_Tab):
            self.cycle_selection()
            return

        if event.key() in (Qt.Key.Key_Left, Qt.Key.Key_Backtab):
            self.cycle_selection(-1)
            return

        super().keyPressEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        # A press that reaches the overlay itself (not a card) dismisses it
        self.dismissed.emit()
        super().mousePressEvent(event)
