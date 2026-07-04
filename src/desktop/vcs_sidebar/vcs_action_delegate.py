"""Item delegate for the VCS trees with inline hover actions."""

from PySide6.QtCore import QModelIndex, QPersistentModelIndex, QRect, QSize, Qt
from PySide6.QtGui import QBrush, QColor, QIcon, QPainter
from PySide6.QtWidgets import QStyle, QStyledItemDelegate, QStyleOptionViewItem, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


# Extra item-data roles carrying the status badge and its colour.
BADGE_ROLE = Qt.ItemDataRole.UserRole + 2
BADGE_COLOR_ROLE = Qt.ItemDataRole.UserRole + 3
KIND_ROLE = Qt.ItemDataRole.UserRole + 1

# Glyphs for the inline actions.
_ACTION_GLYPHS = {
    "stage": "＋",
    "unstage": "－",
    "discard": "↩",
}


class VCSActionDelegate(QStyledItemDelegate):
    """Draws icon + name + status badge, revealing inline actions on hover."""

    def __init__(
        self,
        is_staged: bool,
        style_manager: StyleManager,
        parent: QWidget | None = None,
        show_actions: bool = True,
    ) -> None:
        super().__init__(parent)
        self._is_staged = is_staged
        self._style_manager = style_manager
        self._show_actions = show_actions

    # -- geometry ------------------------------------------------------------

    def _zoom(self) -> float:
        """Return the current UI zoom factor."""
        return self._style_manager.zoom_factor()

    def _action_width(self) -> int:
        """Pixel width of a single action glyph column."""
        return max(18, round(22 * self._zoom()))

    def _reserve_width(self) -> int:
        """Width reserved at the trailing edge for badge/actions (stable on hover)."""
        # Two actions on the Changes tree (discard + stage), one on Staged.
        return self._action_width() * (2 if not self._is_staged else 1)

    def actions_for(self, kind: str) -> list[str]:
        """Return the ordered action ids shown for a row of the given kind."""
        if not self._show_actions:
            return []

        if self._is_staged:
            return ["unstage"]

        return ["discard", "stage"] if kind == "file" else ["stage"]

    def action_rects(self, rect: QRect, kind: str) -> list[tuple[str, QRect]]:
        """Return (action_id, rect) pairs for the trailing action glyphs."""
        actions = self.actions_for(kind)
        width = self._action_width()
        start = rect.right() - width * len(actions)
        return [
            (action, QRect(start + i * width, rect.top(), width, rect.height()))
            for i, action in enumerate(actions)
        ]

    # -- painting ------------------------------------------------------------

    def paint(
        self,
        painter: QPainter,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> None:
        painter.save()

        rect: QRect = option.rect  # type: ignore
        state = QStyle.StateFlag(option.state)  # type: ignore
        is_selected = bool(state & QStyle.StateFlag.State_Selected)
        is_hovered = bool(state & QStyle.StateFlag.State_MouseOver)
        kind = index.data(KIND_ROLE) or "file"

        if is_selected:
            bg = self._style_manager.get_color(ColorRole.TEXT_SELECTED)

        elif is_hovered:
            bg = self._style_manager.get_color(ColorRole.BACKGROUND_TERTIARY_HOVER)

        else:
            bg = self._style_manager.get_color(ColorRole.MINDSPACE_BACKGROUND)

        painter.fillRect(rect, bg)
        painter.setFont(option.font)  # type: ignore

        reserve = self._reserve_width()
        pad = round(4 * self._zoom())
        icon_size = round(16 * self._zoom())

        # Icon.
        x = rect.left() + pad
        icon = index.data(Qt.ItemDataRole.DecorationRole)
        if isinstance(icon, QIcon) and not icon.isNull():
            icon_rect = QRect(x, rect.top() + (rect.height() - icon_size) // 2, icon_size, icon_size)
            icon.paint(painter, icon_rect, Qt.AlignmentFlag.AlignCenter)
            x += icon_size + pad

        # Name (elided within the space left of the reserved trailing strip).
        text = index.data(Qt.ItemDataRole.DisplayRole) or ""
        text_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        foreground = index.data(Qt.ItemDataRole.ForegroundRole)
        if isinstance(foreground, QBrush):
            text_color = foreground.color()

        elif isinstance(foreground, QColor):
            text_color = foreground

        text_rect = QRect(x, rect.top(), rect.right() - reserve - x, rect.height())
        painter.setPen(text_color)
        fm = painter.fontMetrics()
        elided = fm.elidedText(text, Qt.TextElideMode.ElideRight, text_rect.width())
        painter.drawText(
            text_rect, Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft, elided
        )

        # Trailing strip: inline actions on hover, otherwise the status badge.
        if is_hovered:
            self._paint_actions(painter, rect, kind)

        else:
            self._paint_badge(painter, rect, index)

        painter.restore()

    def _paint_actions(self, painter: QPainter, rect: QRect, kind: str) -> None:
        """Paint the inline action glyphs at the trailing edge."""
        for action, arect in self.action_rects(rect, kind):
            if action == "stage":
                color = self._style_manager.get_color(ColorRole.VCS_ADDED)

            elif action == "discard":
                color = self._style_manager.get_color(ColorRole.VCS_DELETED)

            else:
                color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)

            painter.setPen(color)
            painter.drawText(arect, Qt.AlignmentFlag.AlignCenter, _ACTION_GLYPHS[action])

    def _paint_badge(
        self, painter: QPainter, rect: QRect, index: QModelIndex | QPersistentModelIndex
    ) -> None:
        """Paint the single-character status badge at the trailing edge."""
        badge = index.data(BADGE_ROLE)
        if not badge:
            return

        color = index.data(BADGE_COLOR_ROLE)
        if not isinstance(color, QColor):
            color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)

        badge_rect = QRect(
            rect.right() - self._action_width(), rect.top(), self._action_width(), rect.height()
        )
        painter.setPen(color)
        painter.drawText(badge_rect, Qt.AlignmentFlag.AlignCenter, badge)

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        zoom = self._zoom()
        fm = option.fontMetrics  # type: ignore
        row_height = max(fm.height() + round(8 * zoom), round(24 * zoom))
        return QSize(super().sizeHint(option, index).width(), row_height)
