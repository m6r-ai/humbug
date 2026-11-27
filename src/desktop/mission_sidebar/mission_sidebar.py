"""The Missions panel of the sidebar."""

from collections.abc import Callable
from dataclasses import dataclass
import os

from PySide6.QtCore import QFileInfo, QModelIndex, QPersistentModelIndex, QPoint, QRect, QRectF, QSize, Qt, Signal
from PySide6.QtGui import QPainter
from PySide6.QtWidgets import (
    QFrame, QLabel, QLineEdit, QPushButton, QStyle, QStyleOptionViewItem, QTreeWidget, QTreeWidgetItem,
    QVBoxLayout, QWidget
)

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar.sidebar_pane_style import build_tree_pane_stylesheet
from desktop.sidebar.sidebar_row_spacing_delegate import SidebarRowSpacingDelegate
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider
from desktop.sidebar.sidebar_tree_style import SidebarTreeStyle
from desktop.style_manager import StyleManager


_MISSION_ROLE = Qt.ItemDataRole.UserRole + 1
_PATH_ROLE = Qt.ItemDataRole.UserRole + 2

# With more missions than this, a search box helps find one.
_SEARCH_FROM_MISSIONS = 4


@dataclass(frozen=True)
class MissionSidebarEntry:
    """
    A mission as the Missions panel lists it.

    Attributes:
        mission_id: The mission
        title: What the mission is called
        status_text: What the mission is doing, in a word or two
        status_role: The colour its status is shown in
        lead_path: Absolute path of the mission's lead conversation
        archived: Whether the user has put the mission away
        waiting: How many requests from the mission are waiting for the user
        search_text: What a search matches the mission against
    """
    mission_id: str
    title: str
    status_text: str
    status_role: ColorRole
    lead_path: str
    archived: bool = False
    waiting: int = 0
    search_text: str = ""


class _MissionRowDelegate(SidebarRowSpacingDelegate):
    """Draws a mission row as a status dot, its title, and its status at the trailing edge."""

    def __init__(self, tree: QTreeWidget, style_manager: StyleManager) -> None:
        """Initialise the delegate for the missions tree."""
        super().__init__(tree, style_manager)
        self._tree = tree

    def paint(self, painter: QPainter, option: QStyleOptionViewItem, index: QModelIndex | QPersistentModelIndex) -> None:
        """Paint a mission row by hand, and anything else the usual way."""
        mission = index.data(_MISSION_ROLE)
        if not isinstance(mission, MissionSidebarEntry):
            super().paint(painter, option, index)
            return

        sm = self._style_manager
        rect: QRect = option.rect  # type: ignore[attr-defined]
        state = option.state  # type: ignore[attr-defined]
        style = self._tree.style()
        background = QStyleOptionViewItem(option)
        self.initStyleOption(background, index)
        background.text = ""  # type: ignore[attr-defined]
        style.drawPrimitive(QStyle.PrimitiveElement.PE_PanelItemViewItem, background, painter, self._tree)

        painter.save()
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        right_to_left = self._tree.layoutDirection() == Qt.LayoutDirection.RightToLeft
        gap = sm.scale(8)
        dot = sm.scale(8)
        left, right = rect.left() + sm.scale(4), rect.right() - sm.scale(8)

        status_color = sm.get_color(mission.status_role)
        dot_x = right - dot if right_to_left else left
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(status_color)
        painter.drawEllipse(QRectF(dot_x, rect.center().y() - dot / 2 + 0.5, dot, dot))

        font = option.font  # type: ignore[attr-defined]
        metrics = option.fontMetrics  # type: ignore[attr-defined]
        status_font = painter.font()
        status_font.setPointSizeF(font.pointSizeF() * 0.85)
        painter.setFont(status_font)
        status_width = painter.fontMetrics().horizontalAdvance(mission.status_text)
        text_left = left + dot + gap
        text_right = right
        if right_to_left:
            text_left, text_right = left, right - dot - gap

        status_rect = (
            QRect(text_left, rect.top(), status_width, rect.height()) if right_to_left
            else QRect(text_right - status_width, rect.top(), status_width, rect.height())
        )
        painter.setPen(status_color)
        painter.drawText(status_rect, Qt.AlignmentFlag.AlignVCenter, mission.status_text)

        if mission.waiting:
            # Requests waiting for the user are counted in a pill, so they stand out in a long list.
            count = str(mission.waiting)
            pill_height = painter.fontMetrics().height() + sm.scale(2)
            pill_width = max(pill_height, painter.fontMetrics().horizontalAdvance(count) + sm.scale(10))
            pill_left = status_rect.right() + gap if right_to_left else status_rect.left() - gap - pill_width
            pill = QRectF(pill_left, rect.center().y() - pill_height / 2 + 0.5, pill_width, pill_height)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(sm.get_color(ColorRole.MESSAGE_WARNING))
            painter.drawRoundedRect(pill, pill_height / 2, pill_height / 2)
            painter.setPen(sm.get_color(ColorRole.MINDSPACE_BACKGROUND))
            painter.drawText(pill, Qt.AlignmentFlag.AlignCenter, count)
            status_rect = (
                QRect(status_rect.left(), rect.top(), round(pill.right()) - status_rect.left(), rect.height())
                if right_to_left
                else QRect(round(pill.left()), rect.top(), status_rect.right() - round(pill.left()), rect.height())
            )

        painter.setFont(font)
        title_rect = (
            QRect(status_rect.right() + gap, rect.top(), text_right - status_rect.right() - gap, rect.height())
            if right_to_left
            else QRect(text_left, rect.top(), status_rect.left() - gap - text_left, rect.height())
        )
        selected = bool(state & QStyle.StateFlag.State_Selected)
        title_role = ColorRole.TEXT_INACTIVE if mission.archived and not selected else ColorRole.TEXT_PRIMARY
        painter.setPen(sm.get_color(title_role))
        title = metrics.elidedText(mission.title, Qt.TextElideMode.ElideRight, max(0, title_rect.width()))
        painter.drawText(
            title_rect,
            Qt.AlignmentFlag.AlignVCenter | (Qt.AlignmentFlag.AlignRight if right_to_left else Qt.AlignmentFlag.AlignLeft),
            title,
        )
        painter.restore()


class MissionSidebar(SidebarBase):
    """
    Lists the mindspace's missions, each with the conversations working on it.

    A mission unfolds to its lead conversation, with the helpers the lead delegated to
    beneath it.  Archived missions are left out until asked for, and a search box appears
    once there are enough missions to need one.  The panel only shows what it is given and
    asks for anything to be done to a mission; whoever follows the missions does it.
    """

    file_clicked = Signal(str, str, bool)  # panel_id, path, ephemeral
    mission_activated = Signal(str, bool)  # mission_id, ephemeral
    new_mission_requested = Signal()
    mission_rename_requested = Signal(str)  # mission_id
    mission_rerun_requested = Signal(str)  # mission_id
    mission_archive_requested = Signal(str, bool)  # mission_id, archived
    mission_delete_requested = Signal(str)  # mission_id

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialise the panel, empty."""
        super().__init__(parent)
        self.setObjectName("MissionSidebar")
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._icon_provider = SidebarTreeIconProvider()
        self._missions: list[MissionSidebarEntry] = []
        self._children_of: Callable[[str], list[str]] = lambda _path: []
        self._file_menu_providers: list[Callable[[str], list[tuple[str, Callable[[], None]]]]] = []

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        strings = self._language_manager.strings()
        self._header = SidebarSectionHeader(strings.mindspace_missions, self)
        self._new_mission_button = self._header.add_action_button(
            "plus", strings.mission_new, self.new_mission_requested.emit
        )
        self._new_mission_button.setEnabled(False)
        layout.addWidget(self._header)

        self._search = QLineEdit(self)
        self._search.setObjectName("MissionSidebarSearch")
        self._search.setClearButtonEnabled(True)
        self._search.textChanged.connect(self.refresh)
        self._search.hide()
        layout.addWidget(self._search)

        self._empty_label = QLabel(strings.mindspace_missions_empty, self)
        self._empty_label.setObjectName("_empty_label")
        self._empty_label.setWordWrap(True)
        self._empty_label.setAlignment(Qt.AlignmentFlag.AlignTop)
        layout.addWidget(self._empty_label, 1)

        self._tree = QTreeWidget(self)
        self._tree.setObjectName("MissionSidebarTree")
        self._tree_style = SidebarTreeStyle()
        self._tree.setStyle(self._tree_style)
        self._tree.setFrameShape(QFrame.Shape.NoFrame)
        self._tree.setHeaderHidden(True)
        self._tree.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tree.customContextMenuRequested.connect(self._show_context_menu)
        self._delegate = _MissionRowDelegate(self._tree, self._style_manager)
        self._tree.setItemDelegate(self._delegate)
        self._tree.itemClicked.connect(lambda item: self._activate(item, True))
        self._tree.itemDoubleClicked.connect(lambda item: self._activate(item, False))
        self._tree.hide()
        layout.addWidget(self._tree, 1)

        self._archived_toggle = QPushButton(self)
        self._archived_toggle.setObjectName("MissionSidebarArchived")
        self._archived_toggle.setCheckable(True)
        self._archived_toggle.setCursor(Qt.CursorShape.PointingHandCursor)
        self._archived_toggle.toggled.connect(self.refresh)
        self._archived_toggle.hide()
        layout.addWidget(self._archived_toggle)

        self._language_manager.language_changed.connect(self._on_language_changed)
        self._on_language_changed()

    def set_mindspace(self, path: str) -> None:
        """Allow starting a mission while a mindspace is open."""
        self._new_mission_button.setEnabled(bool(path))
        if not path:
            self.set_missions([], lambda _path: [])

    def add_file_menu_provider(self, provider: Callable[[str], list[tuple[str, Callable[[], None]]]]) -> None:
        """
        Offer more actions for a mission's conversation when it is right-clicked.

        Args:
            provider: Returns (label, callback) pairs for a conversation's absolute path
        """
        self._file_menu_providers.append(provider)

    def reveal_and_select_file(self, file_path: str) -> None:
        """Select a conversation's row: its mission's row when it leads one, otherwise its own."""
        if not self.select_path(file_path):
            self._tree.setCurrentIndex(QModelIndex())
            self._tree.clearSelection()

    def set_missions(self, missions: list[MissionSidebarEntry], children_of: Callable[[str], list[str]]) -> None:
        """
        Show these missions, keeping what was unfolded and selected.

        Args:
            missions: The missions, in the order to list them
            children_of: Returns the conversations a conversation delegated to
        """
        self._missions = list(missions)
        self._children_of = children_of
        self.refresh()

    def refresh(self) -> None:
        """Rebuild the rows, for instance once a lead has delegated to a new helper."""
        expanded = {self._key(item) for item in self._rows() if item.isExpanded()}
        current = self._tree.currentItem()
        selected = self._key(current) if current is not None else ""

        strings = self._language_manager.strings()
        archived = sum(1 for mission in self._missions if mission.archived)
        if not archived and self._archived_toggle.isChecked():
            self._archived_toggle.setChecked(False)
            return

        self._archived_toggle.setVisible(bool(archived))
        self._archived_toggle.setText(strings.mission_archived_count.format(archived))
        self._search.setVisible(len(self._missions) > _SEARCH_FROM_MISSIONS)
        shown = self._shown_missions()

        self._tree.clear()
        self._tree.setVisible(bool(shown))
        self._empty_label.setVisible(not shown)
        self._empty_label.setText(strings.mission_search_none if self._missions else strings.mindspace_missions_empty)

        restore: list[QTreeWidgetItem] = []
        for mission in shown:
            item = QTreeWidgetItem()
            item.setData(0, _MISSION_ROLE, mission)
            item.setToolTip(0, f"{mission.title}\n{mission.status_text}")
            self._tree.addTopLevelItem(item)
            restore.append(item)
            if mission.lead_path:
                self._add_conversation(item, mission.lead_path, set(), restore)

        for item in restore:
            key = self._key(item)
            if key in expanded:
                item.setExpanded(True)

            if key == selected:
                self._tree.setCurrentItem(item)

    def _shown_missions(self) -> list[MissionSidebarEntry]:
        """Return the missions matching the search, without archived ones unless the user asked to see them."""
        words = self._search.text().casefold().split() if self._search.isVisible() else []
        show_archived = self._archived_toggle.isChecked()
        return [
            mission for mission in self._missions
            if (show_archived or not mission.archived)
            and all(word in (mission.search_text or mission.title).casefold() for word in words)
        ]

    def select_path(self, path: str) -> bool:
        """
        Select the row for a conversation: its mission's row when it leads one, otherwise its own.

        Args:
            path: Absolute path of the conversation

        Returns:
            True if the conversation is listed here
        """
        path = os.path.normpath(path)
        found = None
        for item in self._rows():
            mission = item.data(0, _MISSION_ROLE)
            row_path = mission.lead_path if isinstance(mission, MissionSidebarEntry) else item.data(0, _PATH_ROLE)
            if row_path and os.path.normpath(row_path) == path:
                found = item
                break

        if found is None:
            return False

        parent = found.parent()
        while parent is not None:
            parent.setExpanded(True)
            parent = parent.parent()

        self._tree.setCurrentItem(found)
        self._tree.scrollToItem(found)
        return True

    def _rows(self) -> list[QTreeWidgetItem]:
        """Return every row, each before the rows beneath it."""
        rows = []
        pending = [self._tree.topLevelItem(i) for i in range(self._tree.topLevelItemCount())]
        while pending:
            item = pending.pop(0)
            if item is None:
                continue

            rows.append(item)
            pending.extend(item.child(i) for i in range(item.childCount()))

        return rows

    def _add_conversation(
        self, parent: QTreeWidgetItem, path: str, seen: set[str], restore: list[QTreeWidgetItem]
    ) -> None:
        """Add a conversation under a row, with the conversations it delegated to beneath it."""
        if path in seen:
            return

        seen.add(path)
        item = QTreeWidgetItem([os.path.splitext(os.path.basename(path))[0]])
        item.setData(0, _PATH_ROLE, path)
        item.setToolTip(0, path)
        item.setIcon(0, self._icon_provider.icon(QFileInfo(path)))
        parent.addChild(item)
        restore.append(item)
        for child in sorted(self._children_of(path)):
            self._add_conversation(item, child, seen, restore)

    @staticmethod
    def _key(item: QTreeWidgetItem) -> str:
        """Return what identifies a row from one rebuild to the next."""
        mission = item.data(0, _MISSION_ROLE)
        if isinstance(mission, MissionSidebarEntry):
            return f"mission:{mission.mission_id}"

        return f"path:{item.data(0, _PATH_ROLE)}"

    def _activate(self, item: QTreeWidgetItem, ephemeral: bool) -> None:
        """
        Open what a row stands for, the mission or the conversation, in a tab of its own.

        A single click opens a temporary tab that the next one replaces, as in the Conversations
        panel; a double click opens one that stays.
        """
        mission = item.data(0, _MISSION_ROLE)
        if isinstance(mission, MissionSidebarEntry):
            self.mission_activated.emit(mission.mission_id, ephemeral)
            return

        path = item.data(0, _PATH_ROLE)
        if path:
            self.file_clicked.emit("conversations", path, ephemeral)

    def _show_context_menu(self, position: QPoint) -> None:
        """Offer what can be done to a mission, or to one of its conversations."""
        item = self._tree.itemAt(position)
        if item is None:
            return

        strings = self._language_manager.strings()
        menu = self._style_manager.create_menu(self)
        mission = item.data(0, _MISSION_ROLE)
        if isinstance(mission, MissionSidebarEntry):
            mission_id = mission.mission_id
            path = mission.lead_path
            menu.addAction(strings.mindspace_mission_open).triggered.connect(
                lambda: self.mission_activated.emit(mission_id, False)
            )
            menu.addAction(strings.mission_rename).triggered.connect(
                lambda: self.mission_rename_requested.emit(mission_id)
            )
            menu.addAction(strings.mission_run_again).triggered.connect(
                lambda: self.mission_rerun_requested.emit(mission_id)
            )
            menu.addAction(
                strings.mission_unarchive if mission.archived else strings.mission_archive
            ).triggered.connect(lambda: self.mission_archive_requested.emit(mission_id, not mission.archived))
            menu.addSeparator()
            menu.addAction(strings.mission_delete).triggered.connect(
                lambda: self.mission_delete_requested.emit(mission_id)
            )

        else:
            path = item.data(0, _PATH_ROLE)

        entries = [entry for provider in self._file_menu_providers for entry in provider(path)] if path else []
        if entries and not menu.isEmpty():
            menu.addSeparator()

        for label, callback in entries:
            menu.addAction(label).triggered.connect(callback)

        if menu.isEmpty():
            return

        menu.exec(self._tree.viewport().mapToGlobal(position))

    def _on_language_changed(self) -> None:
        """Show the panel's words in the new language."""
        strings = self._language_manager.strings()
        self._header.set_title(strings.mindspace_missions)
        self._new_mission_button.setToolTip(strings.mission_new)
        self._search.setPlaceholderText(strings.mission_search)
        self._search.setAccessibleName(strings.mission_search)
        self.apply_style()

    def apply_style(self) -> None:
        """Style the panel to match the rest of the sidebar."""
        sm = self._style_manager
        zoom_factor = sm.zoom_factor()
        font_size = sm.base_font_size() * zoom_factor
        self._header.apply_style()
        font = self.font()
        font.setPointSizeF(font_size)
        self._tree.setFont(font)
        self._empty_label.setFont(font)
        self._search.setFont(font)
        self._archived_toggle.setFont(font)
        icon_size = round(16 * zoom_factor)
        self._tree.setIconSize(QSize(icon_size, icon_size))
        self._icon_provider.update_icons()
        subtle = sm.get_color_str(ColorRole.TEXT_INACTIVE)
        inset = sm.scale(SidebarSectionHeader.TITLE_INSET)
        self.setStyleSheet(
            build_tree_pane_stylesheet(
                sm, "MissionSidebar", "QTreeWidget#MissionSidebarTree",
                self.layoutDirection(), zoom_factor,
            )
            + f"""
            QLabel#_empty_label {{
                color: {subtle};
                padding: {round(8 * zoom_factor)}px {inset}px;
                background-color: {sm.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
            }}
            QLineEdit#MissionSidebarSearch {{
                margin: {round(4 * zoom_factor)}px {inset}px;
                padding: {round(3 * zoom_factor)}px {round(6 * zoom_factor)}px;
                background-color: {sm.get_color_str(ColorRole.EDIT_BOX_BACKGROUND)};
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: 1px solid {sm.get_color_str(ColorRole.EDIT_BOX_BORDER)};
                border-radius: {round(4 * zoom_factor)}px;
            }}
            QPushButton#MissionSidebarArchived {{
                text-align: left;
                padding: {round(6 * zoom_factor)}px {inset}px;
                border: none;
                color: {subtle};
                background-color: {sm.get_color_str(ColorRole.MINDSPACE_BACKGROUND)};
            }}
            QPushButton#MissionSidebarArchived:hover {{
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            QPushButton#MissionSidebarArchived:checked {{
                color: {sm.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            """
        )
        self.refresh()
