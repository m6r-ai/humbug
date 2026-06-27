"""
Carousel overlay showing open tabs as a horizontally scrollable card strip.

Like tab_overview, this module is purely presentational: it consumes
TabOverviewEntry display records and emits signals when the user activates,
closes or dismisses.  The TabManager supplies the data and acts on the signals.
"""


from PySide6.QtCore import QEasingCurve, QEvent, QPoint, QRect, QRectF, Qt, QVariantAnimation, Signal
from PySide6.QtGui import (
    QColor, QFont, QFontMetricsF, QKeyEvent, QMouseEvent, QPainter, QPainterPath, QPaintEvent,
    QPen, QWheelEvent
)
from PySide6.QtWidgets import QApplication, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager
from desktop.tab_manager.tab_overview import TabOverviewEntry, aspect_fill_source_rect


class TabCarouselWidget(QWidget):
    """Full-area overlay presenting open tabs as a centred, scrollable carousel."""

    tab_activated = Signal(str)
    tab_close_requested = Signal(str)
    dismissed = Signal()

    def __init__(self, parent: QWidget) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._entries: list[TabOverviewEntry] = []

        self._scroll_pos = 0.0
        self._target_index = 0
        self._animation: QVariantAnimation | None = None
        self._wheel_accumulator = 0

        self._press_pos: QPoint | None = None
        self._press_scroll_pos = 0.0
        self._press_card_index: int | None = None
        self._is_dragging = False

        self._swipe_index: int | None = None
        self._swipe_offset_y = 0.0
        self._swipe_animation: QVariantAnimation | None = None
        self._close_hover_index: int | None = None

        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setMouseTracking(True)

    def set_entries(self, entries: list[TabOverviewEntry]) -> None:
        """Replace the displayed entries, centring on the current tab."""
        self._entries = list(entries)
        current = next((i for i, e in enumerate(entries) if e.is_current), 0)
        self._target_index = current
        self._scroll_pos = float(current)
        self._stop_animation()
        self.update()

    def remove_entry(self, tab_id: str) -> None:
        """Remove the card for a tab that has been closed."""
        index = next((i for i, e in enumerate(self._entries) if e.tab_id == tab_id), -1)
        if index == -1:
            return

        del self._entries[index]
        if not self._entries:
            self.update()
            return

        if self._target_index >= index:
            self._target_index = max(0, self._target_index - 1)

        self._target_index = min(self._target_index, len(self._entries) - 1)
        self._scroll_pos = min(self._scroll_pos, float(len(self._entries) - 1))
        self._go_to(self._target_index)

    def entry_count(self) -> int:
        """Return the number of entries currently displayed."""
        return len(self._entries)

    def current_tab_id(self) -> str | None:
        """Return the tab ID of the centred card, if any."""
        if not self._entries:
            return None

        return self._entries[self._target_index].tab_id

    def select_next(self) -> None:
        """Advance the carousel to the next card, wrapping at the end."""
        if not self._entries:
            return

        self._go_to((self._target_index + 1) % len(self._entries))

    def _go_to(self, index: int) -> None:
        """Animate the carousel so that the card at index is centred."""
        index = max(0, min(index, len(self._entries) - 1))
        self._target_index = index
        self._stop_animation()

        animation = QVariantAnimation(self)
        animation.setDuration(280)
        animation.setEasingCurve(QEasingCurve.Type.OutQuart)
        animation.setStartValue(self._scroll_pos)
        animation.setEndValue(float(index))
        animation.valueChanged.connect(self._on_animation_value)
        self._animation = animation
        animation.start()

    def _stop_animation(self) -> None:
        if self._animation is not None:
            self._animation.stop()
            self._animation = None

    def _on_animation_value(self, value: object) -> None:
        self._scroll_pos = float(value)  # type: ignore[arg-type]
        self.update()

    def _header_height(self) -> int:
        return self._style_manager.scale(28)

    def _card_spacing(self) -> int:
        """Horizontal distance between adjacent card centres."""
        return 480

    def _card_offset_x(self, offset: float) -> int:
        """
        Convert a fractional card offset to a pixel displacement from the strip centre.

        Spacing shrinks with distance so outer cards sit closer together than
        inner ones.  A separate, steeper falloff coefficient is used here so the
        spacing can compress more aggressively than the card size scale, keeping
        the outer cards visually tight without shrinking them too small.
        """
        spacing = self._card_spacing()
        sign = 1 if offset >= 0 else -1
        steps = abs(offset)
        whole = int(steps)
        frac = steps - whole
        px = sum(spacing * max(0.3125, 1.0 - 0.4583 * (i + 0.5)) for i in range(whole))
        px += frac * spacing * max(0.3125, 1.0 - 0.4583 * (whole + frac * 0.5))
        return sign * int(px)

    def _card_rect(self, offset: float, index: int | None = None) -> QRect:
        """
        Compute the rect for a card at the given fractional offset from centre.

        When an entry index is given the card adopts the aspect ratio of that
        tab's thumbnail, so the rendered content and its container line up
        exactly instead of cropping or letterboxing.
        """
        max_width = int(self.width() * 0.52)
        max_height = int(self.height() * 0.66)
        header_height = self._header_height()

        width = max_width
        height = max_height

        if index is not None and 0 <= index < len(self._entries):
            thumbnail = self._entries[index].thumbnail
            if not thumbnail.isNull() and thumbnail.height() > 0:
                aspect = thumbnail.width() / thumbnail.height()
                body_height = max(1, max_height - header_height)
                body_width = min(max_width, int(body_height * aspect))
                body_height = max(1, int(body_width / aspect))
                width = body_width
                height = body_height + header_height

        scale = max(0.60, 1.0 - 0.25 * abs(offset))
        width = int(width * scale)
        height = int(height * scale)

        center_x = self.width() // 2 + self._card_offset_x(offset)
        center_y = self.height() // 2
        return QRect(center_x - width // 2, center_y - height // 2, width, height)

    def _close_rect(self, card_rect: QRect) -> QRect:
        sm = self._style_manager
        icon_size = sm.tab_icon_size()
        spacing = sm.tab_spacing()
        return QRect(
            card_rect.right() - spacing - icon_size,
            card_rect.top() + (self._header_height() - icon_size) // 2,
            icon_size,
            icon_size,
        )

    def _visible_indices(self) -> list[int]:
        """Return entry indices near enough to the centre to be drawn."""
        return [
            i for i in range(len(self._entries))
            if abs(i - self._scroll_pos) <= 2.6
        ]

    def paintEvent(self, _event: QPaintEvent) -> None:
        sm = self._style_manager
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        dim = QColor(sm.get_color(ColorRole.BACKGROUND_PRIMARY))
        dim.setAlpha(230)
        painter.fillRect(self.rect(), dim)

        # Draw far cards first so nearer cards overlap them
        for index in sorted(self._visible_indices(), key=lambda i: -abs(i - self._scroll_pos)):
            self._paint_card(painter, index)

    def _paint_card(self, painter: QPainter, index: int) -> None:
        sm = self._style_manager
        entry = self._entries[index]
        offset = index - self._scroll_pos
        card_rect = self._card_rect(offset, index)
        is_focused = abs(offset) < 0.5
        radius = sm.radius("panel")

        is_swiping = index == self._swipe_index and self._swipe_offset_y < 0
        if is_swiping:
            card_rect = card_rect.translated(0, int(self._swipe_offset_y))
            progress = min(1.0, -self._swipe_offset_y / max(1, card_rect.height()))
            painter.setOpacity(max(0.15, 1.0 - progress))

        painter.save()
        path = QPainterPath()
        path.addRoundedRect(card_rect, radius, radius)
        painter.setClipPath(path)

        painter.fillRect(card_rect, sm.get_color(ColorRole.BACKGROUND_SECONDARY))

        header_height = self._header_height()
        header_rect = QRect(card_rect.left(), card_rect.top(), card_rect.width(), header_height)
        painter.fillRect(header_rect, sm.get_color(ColorRole.BACKGROUND_TERTIARY))

        icon_size = sm.tab_icon_size()
        spacing = sm.tab_spacing()
        icon_y = card_rect.top() + (header_height - icon_size) // 2
        painter.drawPixmap(QPoint(card_rect.left() + spacing, icon_y), sm.scale_icon(entry.icon_name, 16))

        font = QFont()
        font.setFamilies(sm.proportional_font_families())
        font.setPointSizeF(sm.base_font_size() * sm.zoom_factor())
        painter.setFont(font)
        painter.setPen(sm.get_color(ColorRole.TEXT_PRIMARY))

        text_x = card_rect.left() + spacing + icon_size + spacing
        text_right = self._close_rect(card_rect).left() - spacing if is_focused else card_rect.right() - spacing
        text_rect = QRect(text_x, card_rect.top(), max(0, text_right - text_x), header_height)
        fm = QFontMetricsF(font)
        elided = fm.elidedText(entry.title, Qt.TextElideMode.ElideMiddle, text_rect.width())
        painter.drawText(text_rect, Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft, elided)

        if is_focused:
            close_rect = self._close_rect(card_rect)
            if index == self._close_hover_index:
                hover_color = sm.get_color(ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER)
                painter.setBrush(hover_color)
                painter.setPen(Qt.PenStyle.NoPen)
                painter.drawRoundedRect(close_rect, sm.radius(), sm.radius())

            painter.drawPixmap(close_rect.topLeft(), sm.scale_icon("close", 16))

        body_rect = QRect(
            card_rect.left(),
            card_rect.top() + header_height,
            card_rect.width(),
            card_rect.height() - header_height,
        )
        thumbnail = entry.thumbnail
        if not thumbnail.isNull() and body_rect.height() > 0:
            # Rect-to-rect drawing fills the body exactly regardless of the
            # thumbnail's device pixel ratio
            painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)
            source = aspect_fill_source_rect(thumbnail, body_rect)
            if not source.isEmpty():
                painter.drawPixmap(body_rect, thumbnail, source)

        # Dim non-focused cards so the centred one stands out
        if not is_focused:
            shade = QColor(0, 0, 0)
            shade.setAlpha(min(120, int(90 * abs(offset))))
            painter.fillRect(card_rect, shade)

        painter.restore()

        painter.setBrush(Qt.BrushStyle.NoBrush)
        if is_focused:
            # Soft halo around the focused card instead of a hard border
            glow_base = sm.get_color(ColorRole.BRAND_PRIMARY)
            for ring in range(5, 0, -1):
                glow = QColor(glow_base)
                glow.setAlpha(int(70 / ring))
                pen = QPen(glow)
                pen.setWidthF(2.0)
                painter.setPen(pen)
                painter.drawRoundedRect(
                    QRectF(card_rect).adjusted(0.5 - ring, 0.5 - ring, ring - 0.5, ring - 0.5),
                    radius + ring,
                    radius + ring,
                )

            # Fine edge line, stroked over the clip boundary so no background
            # seam shows through at the rounded corners
            edge = QColor(glow_base)
            edge.setAlpha(190)
            pen = QPen(edge)
            pen.setWidthF(1.5)
            painter.setPen(pen)
            painter.drawRoundedRect(
                QRectF(card_rect).adjusted(0.75, 0.75, -0.75, -0.75), radius, radius
            )

        else:
            pen = QPen(sm.get_color(ColorRole.SPLITTER))
            pen.setWidthF(1.0)
            painter.setPen(pen)
            painter.drawRoundedRect(
                QRectF(card_rect).adjusted(0.5, 0.5, -0.5, -0.5), radius, radius
            )

        if is_swiping:
            painter.setOpacity(1.0)

    def leaveEvent(self, event: QEvent) -> None:
        if self._close_hover_index is not None:
            self._close_hover_index = None
            self.update()

        super().leaveEvent(event)

    def wheelEvent(self, event: QWheelEvent) -> None:
        delta = event.angleDelta().x()
        if delta == 0:
            delta = event.angleDelta().y()

        self._wheel_accumulator += delta
        steps = int(self._wheel_accumulator / 120)
        if steps != 0:
            self._wheel_accumulator -= steps * 120
            self._go_to(self._target_index - steps)

        event.accept()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        if event.key() == Qt.Key.Key_Escape:
            self.dismissed.emit()
            return

        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            tab_id = self.current_tab_id()
            if tab_id is not None:
                self.tab_activated.emit(tab_id)

            return

        if event.key() == Qt.Key.Key_Right:
            self._go_to(self._target_index + 1)
            return

        if event.key() == Qt.Key.Key_Left:
            self._go_to(self._target_index - 1)
            return

        super().keyPressEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        # Defer all click handling to release so a drag can scroll the strip
        self._stop_animation()
        self._stop_swipe_animation()
        self._press_pos = event.pos()
        self._press_scroll_pos = self._scroll_pos
        self._press_card_index = self._card_index_at(event.pos())
        self._is_dragging = False

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        if not self._entries:
            super().mouseMoveEvent(event)
            return

        if self._press_pos is None:
            close_hover_index: int | None = None
            focused_index = self._target_index
            card_rect = self._card_rect(0.0, focused_index)
            if self._close_rect(card_rect).contains(event.pos()):
                close_hover_index = focused_index

            if close_hover_index != self._close_hover_index:
                self._close_hover_index = close_hover_index
                self.update()

            super().mouseMoveEvent(event)
            return

        delta = event.pos() - self._press_pos
        if self._close_hover_index is not None:
            self._close_hover_index = None
            self.update()

        if not self._is_dragging and self._swipe_index is None:
            if delta.manhattanLength() < QApplication.startDragDistance():
                return

            # Pick a gesture axis once: an upward drag on a card swipes it
            # closed, anything else scrolls the strip
            if (
                self._press_card_index is not None
                and delta.y() < 0
                and abs(delta.y()) > abs(delta.x())
            ):
                self._swipe_index = self._press_card_index

            else:
                self._is_dragging = True

        if self._swipe_index is not None:
            self._swipe_offset_y = min(0.0, float(delta.y()))
            self.update()
            return

        position = self._press_scroll_pos - delta.x() / max(1, self._card_spacing())
        self._scroll_pos = max(0.0, min(position, float(len(self._entries) - 1)))
        self.update()

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        if event.button() != Qt.MouseButton.LeftButton:
            super().mouseReleaseEvent(event)
            return

        press_pos = self._press_pos
        self._press_pos = None

        if self._swipe_index is not None:
            self._finish_swipe()
            return

        if self._is_dragging:
            # Snap after a drag; never treat it as a click.  A short but deliberate
            # drag pages one card in its direction rather than springing back.
            self._is_dragging = False
            target = round(self._scroll_pos)
            delta = self._scroll_pos - self._press_scroll_pos
            if target == round(self._press_scroll_pos) and abs(delta) > 0.08:
                target += 1 if delta > 0 else -1

            self._go_to(target)
            return

        if press_pos is not None:
            self._handle_click(event.pos())

    def _card_index_at(self, pos: QPoint) -> int | None:
        """Return the index of the card under pos, testing nearest-to-centre first."""
        for index in sorted(self._visible_indices(), key=lambda i: abs(i - self._scroll_pos)):
            if self._card_rect(index - self._scroll_pos, index).contains(pos):
                return index

        return None

    def _finish_swipe(self) -> None:
        """Complete an upward swipe: close past the threshold, else spring back."""
        index = self._swipe_index
        if index is None or index >= len(self._entries):
            self._reset_swipe()
            return

        card_rect = self._card_rect(index - self._scroll_pos, index)
        if -self._swipe_offset_y > card_rect.height() * 0.35:
            tab_id = self._entries[index].tab_id
            target = -float(card_rect.bottom() + 20)
            self._animate_swipe(target, on_done=lambda: self._complete_swipe_close(tab_id))

        else:
            self._animate_swipe(0.0, on_done=self._reset_swipe)

    def _complete_swipe_close(self, tab_id: str) -> None:
        # Reset before emitting: the close may remove the entry (re-indexing the
        # list), or be vetoed, in which case the card simply springs back
        self._reset_swipe()
        self.tab_close_requested.emit(tab_id)

    def _reset_swipe(self) -> None:
        self._swipe_index = None
        self._swipe_offset_y = 0.0
        self.update()

    def _animate_swipe(self, target: float, on_done: object = None) -> None:
        self._stop_swipe_animation()
        animation = QVariantAnimation(self)
        animation.setDuration(160)
        animation.setEasingCurve(QEasingCurve.Type.OutCubic)
        animation.setStartValue(self._swipe_offset_y)
        animation.setEndValue(target)
        animation.valueChanged.connect(self._on_swipe_animation_value)
        if callable(on_done):
            animation.finished.connect(on_done)

        self._swipe_animation = animation
        animation.start()

    def _stop_swipe_animation(self) -> None:
        if self._swipe_animation is not None:
            self._swipe_animation.stop()
            self._swipe_animation = None

    def _on_swipe_animation_value(self, value: object) -> None:
        self._swipe_offset_y = float(value)  # type: ignore[arg-type]
        self.update()

    def _handle_click(self, pos: QPoint) -> None:
        """Activate, close, centre or dismiss based on where a clean click landed."""
        # Test cards nearest the centre first since they are drawn on top
        for index in sorted(self._visible_indices(), key=lambda i: abs(i - self._scroll_pos)):
            offset = index - self._scroll_pos
            card_rect = self._card_rect(offset, index)
            if not card_rect.contains(pos):
                continue

            if abs(offset) < 0.5:
                if self._close_rect(card_rect).contains(pos):
                    self.tab_close_requested.emit(self._entries[index].tab_id)

                else:
                    self.tab_activated.emit(self._entries[index].tab_id)

            else:
                self._go_to(index)

            return

        self.dismissed.emit()
