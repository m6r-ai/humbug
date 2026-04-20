"""Item delegate for the VCS status list."""

from PySide6.QtCore import QModelIndex, QPersistentModelIndex, QRect, QSize, Qt
from PySide6.QtGui import QBrush, QColor, QPainter
from PySide6.QtWidgets import QStyle, QStyledItemDelegate, QStyleOptionViewItem, QWidget

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class MindspaceVCSDelegate(QStyledItemDelegate):
    """
    Delegate that renders each VCS status entry as two visual columns.

    The left column shows the single-character status badge in the status
    colour for that entry.  The right column shows the file path in the
    normal text colour.

    The badge column width is fixed at a small multiple of the current
    character width so it remains proportional across zoom levels.
    """

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()

    def _badge_width(self) -> int:
        """Return the pixel width reserved for the badge column."""
        return round(self._style_manager.get_space_width() * 3)

    def paint(
        self,
        painter: QPainter,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> None:
        """Paint the background then the badge and path as separate columns."""
        painter.save()

        # rect and state are valid properties but PySide6 does not expose them correctly
        rect: QRect = option.rect  # type: ignore
        state = QStyle.StateFlag(option.state)  # type: ignore

        # Draw background manually so we never call super().paint() which
        # would also render the display-role text and cause double drawing.
        is_selected = bool(state & QStyle.StateFlag.State_Selected)
        is_hovered = bool(state & QStyle.StateFlag.State_MouseOver)

        if is_selected:
            bg = self._style_manager.get_color(ColorRole.TEXT_SELECTED)

        elif is_hovered:
            bg = self._style_manager.get_color(ColorRole.TAB_BACKGROUND_HOVER)

        else:
            bg = self._style_manager.get_color(ColorRole.MINDSPACE_BACKGROUND)

        painter.fillRect(rect, bg)

        badge_width = self._badge_width()

        badge_rect = QRect(
            rect.left(),
            rect.top(),
            badge_width,
            rect.height(),
        )
        path_rect = QRect(
            rect.left() + badge_width,
            rect.top(),
            rect.width() - badge_width,
            rect.height(),
        )

        # Retrieve the display text stored as "B  path/to/file".
        display_text: str = index.data(Qt.ItemDataRole.DisplayRole) or ""
        if "  " in display_text:
            badge_char, _, path_text = display_text.partition("  ")

        else:
            badge_char = display_text[:1]
            path_text = display_text[2:]

        # Retrieve the badge colour from ForegroundRole, stored as a QBrush
        # by item.setForeground().
        badge_color: QColor = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        foreground = index.data(Qt.ItemDataRole.ForegroundRole)
        if isinstance(foreground, QBrush):
            badge_color = foreground.color()
        elif isinstance(foreground, QColor):
            badge_color = foreground

        path_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)

        painter.setFont(option.font)  # type: ignore

        # Draw badge in status colour.
        painter.setPen(badge_color)
        painter.drawText(
            badge_rect,
            Qt.AlignmentFlag.AlignCenter,
            badge_char,
        )

        # Draw path in normal text colour, elided if too long.
        painter.setPen(path_color)
        painter.drawText(
            path_rect,
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            path_text,
        )

        painter.restore()

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        """Return a size hint with appropriate row height."""
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        line_height = fm.height()
        row_height = max(line_height + round(8 * zoom), round(24 * zoom))
        return QSize(super().sizeHint(option, index).width(), row_height)
