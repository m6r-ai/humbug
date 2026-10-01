"""
Quick Switcher overlay: fuzzy-filter files, conversations, and open tabs by name.

This module is purely presentational, in keeping with other Humbug overlays
(see tab_manager/tab_overview.py): it knows nothing about the mindspace,
contexts, or tabs. It is handed a flat list of QuickSwitcherEntry records to
display and filter, and emits a signal carrying the chosen entry's ID when the
user picks one. The caller is responsible for gathering entries and acting on
the activated signal.
"""

from dataclasses import dataclass
from typing import cast

from PySide6.QtCore import QEvent, QObject, Qt, Signal
from PySide6.QtGui import QIcon, QKeyEvent, QMouseEvent, QPainter, QPaintEvent, QResizeEvent
from PySide6.QtWidgets import QLineEdit, QListWidget, QListWidgetItem, QVBoxLayout, QWidget

from desktop.color_role import ColorRole
from desktop.quick_switcher.quick_switcher_match import fuzzy_match_score
from desktop.style_manager import StyleManager

_MAX_VISIBLE_RESULTS = 50
_TITLE_MATCH_BONUS = 1000


@dataclass(slots=True)
class QuickSwitcherEntry:
    """One candidate the Quick Switcher can filter to and activate."""

    entry_id: str
    kind: str  # "file", "conversation", or "tab"
    title: str
    subtitle: str
    icon_name: str


class QuickSwitcherWidget(QWidget):
    """Full-area overlay: a centred filter box and result list over a dimmed background."""

    entry_activated = Signal(str)
    dismissed = Signal()

    def __init__(self, parent: QWidget) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._entries: list[QuickSwitcherEntry] = []

        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

        self._panel = QWidget(self)
        self._panel.setObjectName("QuickSwitcherPanel")
        self._panel.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        panel_layout = QVBoxLayout(self._panel)
        panel_layout.setContentsMargins(12, 12, 12, 12)
        panel_layout.setSpacing(8)

        self._input = QLineEdit(self._panel)
        self._input.setObjectName("QuickSwitcherInput")
        self._input.installEventFilter(self)
        self._input.textChanged.connect(self._refilter)
        panel_layout.addWidget(self._input)

        self._list = QListWidget(self._panel)
        self._list.setObjectName("QuickSwitcherList")
        self._list.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self._list.itemClicked.connect(self._activate_item)
        panel_layout.addWidget(self._list)

        self.refresh_style()

    def set_placeholder(self, text: str) -> None:
        """Set the placeholder text shown in the empty filter box."""
        self._input.setPlaceholderText(text)

    def set_entries(self, entries: list[QuickSwitcherEntry]) -> None:
        """Replace the full candidate list, clear the filter box, and reset the view."""
        self._entries = entries
        self._input.blockSignals(True)
        self._input.clear()
        self._input.blockSignals(False)
        self._populate([(entry, 0) for entry in entries])

    def focus_input(self) -> None:
        """Give keyboard focus to the filter box, selecting any prior text."""
        self._input.setFocus()
        self._input.selectAll()

    def selected_entry_id(self) -> str | None:
        """Return the entry_id of the currently highlighted row, if any."""
        item = self._list.currentItem()
        if item is None:
            return None

        return cast(str, item.data(Qt.ItemDataRole.UserRole))

    def refresh_style(self) -> None:
        """Rebuild the panel's stylesheet from the current theme, sizes, and zoom."""
        sm = self._style_manager
        radius = sm.radius("panel")
        font_pt = sm.base_font_size() * sm.zoom_factor()

        self._panel.setStyleSheet(f"""
            QWidget#QuickSwitcherPanel {{
                background-color: {sm.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                border: 1px solid {sm.get_color_str(ColorRole.SPLITTER)};
                border-radius: {radius}px;
            }}
            {sm.get_text_input_stylesheet("QLineEdit#QuickSwitcherInput")}
            QListWidget#QuickSwitcherList {{
                background-color: transparent;
                border: none;
                outline: none;
                font-size: {font_pt}pt;
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QListWidget#QuickSwitcherList::item {{
                padding: {sm.spacing(1)}px {sm.spacing(2)}px;
                border-radius: {sm.radius()}px;
            }}
            QListWidget#QuickSwitcherList::item:selected {{
                background-color: {sm.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QListWidget#QuickSwitcherList::item:hover:!selected {{
                background-color: {sm.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)};
            }}
            {sm.get_scrollbar_stylesheet("QListWidget#QuickSwitcherList QScrollBar")}
        """)

    def _refilter(self, query: str) -> None:
        """Re-rank and redisplay entries against the current filter text."""
        if not query:
            self._populate([(entry, 0) for entry in self._entries])
            return

        scored: list[tuple[QuickSwitcherEntry, int]] = []
        for entry in self._entries:
            title_score = fuzzy_match_score(query, entry.title)
            if title_score is not None:
                scored.append((entry, title_score + _TITLE_MATCH_BONUS))
                continue

            subtitle_score = fuzzy_match_score(query, entry.subtitle)
            if subtitle_score is not None:
                scored.append((entry, subtitle_score))

        scored.sort(key=lambda pair: pair[1], reverse=True)
        self._populate(scored)

    def _populate(self, scored_entries: list[tuple[QuickSwitcherEntry, int]]) -> None:
        """Rebuild the list widget from a ranked list of (entry, score) pairs."""
        self._list.clear()
        icon_size = self._style_manager.tab_icon_size()
        for entry, _score in scored_entries[:_MAX_VISIBLE_RESULTS]:
            icon = QIcon(self._style_manager.scale_icon(entry.icon_name, icon_size))
            label = f"{entry.title}   {entry.subtitle}" if entry.subtitle else entry.title
            item = QListWidgetItem(icon, label)
            item.setData(Qt.ItemDataRole.UserRole, entry.entry_id)
            self._list.addItem(item)

        if self._list.count() > 0:
            self._list.setCurrentRow(0)

    def _activate_item(self, item: QListWidgetItem) -> None:
        """Emit entry_activated for the given row, if it carries an entry ID."""
        entry_id = item.data(Qt.ItemDataRole.UserRole)
        if entry_id is not None:
            self.entry_activated.emit(entry_id)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        if watched is self._input and event.type() == QEvent.Type.KeyPress:
            key_event = cast(QKeyEvent, event)
            key = key_event.key()

            if key == Qt.Key.Key_Escape:
                self.dismissed.emit()
                return True

            if key in (Qt.Key.Key_Down, Qt.Key.Key_Up):
                if self._list.count() > 0:
                    step = 1 if key == Qt.Key.Key_Down else -1
                    row = max(0, min(self._list.currentRow() + step, self._list.count() - 1))
                    self._list.setCurrentRow(row)

                return True

            if key in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
                item = self._list.currentItem()
                if item is not None:
                    self._activate_item(item)

                return True

        return super().eventFilter(watched, event)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        if event.key() == Qt.Key.Key_Escape:
            self.dismissed.emit()
            return

        super().keyPressEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        # A press that reaches the overlay itself (not the panel) dismisses it
        self.dismissed.emit()
        super().mousePressEvent(event)

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        sm = self._style_manager
        width = min(sm.scale(640), max(sm.scale(280), self.width() - sm.scale(80)))
        height = min(sm.scale(420), max(sm.scale(200), self.height() - sm.scale(160)))
        x = (self.width() - width) // 2
        y = (self.height() - height) // 3
        self._panel.setGeometry(x, y, width, height)

    def paintEvent(self, _event: QPaintEvent) -> None:
        painter = QPainter(self)
        dim = self._style_manager.get_color(ColorRole.BACKGROUND_PRIMARY)
        dim.setAlpha(180)
        painter.fillRect(self.rect(), dim)
